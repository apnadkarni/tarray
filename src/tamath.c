/*
 * Copyright (c) 2015, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file license.terms for license
 */

#include "tarray.h"

#ifdef TA_MT_ENABLE
/*
 * Thresholds for multithreading.
 * TBD - need to benchmark and set. Likely to depend on compiler.
 */
int ta_math_mt_threshold = TA_MT_THRESHOLD_DEFAULT;
#endif

static const char *ta_math_op_names[] = {
    "+", "-", "*", "/", "&", "|", NULL
};
enum ta_math_op_e {
    TAM_OP_PLUS,TAM_OP_MINUS,TAM_OP_MUL,TAM_OP_DIV,TAM_OP_BITAND,TAM_OP_BITOR,
};


struct ta_math_operand {
    thdr_t *thdr_operand;
    ta_value_t scalar_operand; /* Only valid if thdr_operand is NULL */
};

struct thdr_math_mt_context {
    thdr_t  *thdr;              /* Will hold result (SHARED among threads)
                                   thdr->header must NOT be modified except
                                   by main thread!!!
                                 */
    struct ta_math_operand  *poperands;        /* Operands */
    int      noperands;         /* Number of operands */
    int      start;             /* Starting position in source thdr
                                   for this thread */
    int      count;             /* Number of elements in thdr to examine */
    TCL_RESULT res;             /* Status of thread */
    enum ta_math_op_e op;                /* Operation */
};    

static double ta_math_double_from_operand(struct ta_math_operand *poperand,
                                          int thdr_index)
{
    thdr_t *thdr = poperand->thdr_operand;
    if (thdr) {
        TA_ASSERT(thdr->used > thdr_index);
        switch (thdr->type) {
        case TA_BYTE:
            return *THDRELEMPTR(thdr, unsigned char, thdr_index);
        case TA_INT:
            return *THDRELEMPTR(thdr, int, thdr_index);
        case TA_UINT:
            return *THDRELEMPTR(thdr, unsigned int, thdr_index);
        case TA_WIDE:
            return (double) *THDRELEMPTR(thdr, Tcl_WideInt, thdr_index);
        case TA_DOUBLE:
            return *THDRELEMPTR(thdr, double, thdr_index);
        default:
            ta_type_panic(poperand->thdr_operand->type);
            return 0;           /* To keep compiler happy */
        }
    } else {
        TA_ASSERT(poperand->scalar_operand.type == TA_DOUBLE || poperand->scalar_operand.type == TA_WIDE);
        if (poperand->scalar_operand.type == TA_DOUBLE)
            return poperand->scalar_operand.dval;
        else
            return (double) poperand->scalar_operand.wval;
    }
}

static Tcl_WideInt ta_math_wide_from_operand(struct ta_math_operand *poperand,
                                          int thdr_index)
{
    thdr_t *thdr = poperand->thdr_operand;
    if (thdr) {
        TA_ASSERT(thdr->used > thdr_index);
        switch (thdr->type) {
        case TA_BYTE:
            return *THDRELEMPTR(thdr, unsigned char, thdr_index);
        case TA_INT:
            return *THDRELEMPTR(thdr, int, thdr_index);
        case TA_UINT:
            return *THDRELEMPTR(thdr, unsigned int, thdr_index);
        case TA_WIDE:
            return *THDRELEMPTR(thdr, Tcl_WideInt, thdr_index);
        case TA_DOUBLE:
            /* Any double operands would have forced result type to be
               TA_DOUBLE, not TA_WIDE, so fallthru to panic */
        default:
            ta_type_panic(poperand->thdr_operand->type);
            return 0;           /* To keep compiler happy */
        }
    } else {
        /* All scalar non-double operands must have been promoted to wide */
        TA_ASSERT(poperand->scalar_operand.type == TA_WIDE);
        return poperand->scalar_operand.wval;
    }
}

static double ta_math_double_operation(enum ta_math_op_e op, double accumulator, ta_value_t *poperand)
{
    double operand;

    TA_ASSERT(op != TAM_OP_BITAND && op != TAM_OP_BITOR);
    TA_ASSERT(poperand->type == TA_DOUBLE || poperand->type == TA_WIDE);

    if (poperand->type == TA_DOUBLE)
        operand = poperand->dval;
    else
        operand = (double) poperand->wval;

    switch (op) {
    case TAM_OP_PLUS: return accumulator + operand;
    case TAM_OP_MINUS: return accumulator - operand;
    case TAM_OP_MUL: return accumulator * operand;
    case TAM_OP_DIV: return accumulator / operand; /* Check for div-by-0 ? */
    }
    return 0.0;                 /* To keep compiler happy */
}    

static Tcl_WideInt ta_math_wide_operation(enum ta_math_op_e op, Tcl_WideInt accumulator, Tcl_WideInt operand)
{
    switch (op) {
    case TAM_OP_PLUS: return accumulator + operand;
    case TAM_OP_MINUS: return accumulator - operand;
    case TAM_OP_MUL: return accumulator * operand;
    case TAM_OP_DIV: return accumulator / operand; /* Check for div-by-0 ? */
    }
    return 0;                   /* To keep compiler happy */
}

static void thdr_math_mt_worker(struct thdr_math_mt_context *pctx)
{
    struct ta_math_operand *poperands;
    int noperands;
    int start, end;
    unsigned char type;

    TA_ASSERT(pctx->count > 0);
    TA_ASSERT((pctx->start + pctx->count) <= pctx->thdr->usable);

    type = pctx->thdr->type;
    poperands = pctx->poperands;
    noperands = pctx->noperands;
    start = pctx->start;
    end = start + pctx->count;

    TA_ASSERT(noperands > 0);

#define DOUBLELOOP(op_)                                               \
    do {                                                                \
        int i, j;                                                       \
        for (i = start; i < end; ++i) {                                 \
            double accum = ta_math_double_from_operand(&poperands[0], i); \
            for (j = 1; j < noperands; ++j) {                           \
                accum op_ ta_math_double_from_operand(&poperands[j], i); \
            }                                                           \
            *THDRELEMPTR(pctx->thdr, double, i) = accum;                \
        }                                                               \
    } while (0)

#define INTEGERLOOP(op_, type_)                                       \
    do {                                                                \
        int i, j;                                                       \
        for (i = start; i < end; ++i) {                                 \
            Tcl_WideInt accum = ta_math_wide_from_operand(&poperands[0], i); \
            for (j = 1; j < noperands; ++j) {                           \
                accum op_ ta_math_wide_from_operand(&poperands[j], i); \
            }                                                           \
            *THDRELEMPTR(pctx->thdr, type_, i) = (type_) accum;         \
        }                                                               \
    } while (0)


    switch (pctx->op) {
    case TAM_OP_PLUS:
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(+=, unsigned char); break;
        case TA_INT: INTEGERLOOP(+=, int); break;
        case TA_UINT: INTEGERLOOP(+=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(+=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(+=); break;
        }
        break;
    case TAM_OP_MINUS:
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(-=, unsigned char); break;
        case TA_INT: INTEGERLOOP(-=, int); break;
        case TA_UINT: INTEGERLOOP(-=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(-=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(-=); break;
        }
        break;
    case TAM_OP_MUL:
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(*=, unsigned char); break;
        case TA_INT: INTEGERLOOP(*=, int); break;
        case TA_UINT: INTEGERLOOP(*=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(*=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(*=); break;
        }
        break;
    case TAM_OP_DIV:
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(/=, unsigned char); break;
        case TA_INT: INTEGERLOOP(/=, int); break;
        case TA_UINT: INTEGERLOOP(/=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(/=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(/=); break;
        }
        break;
    case TAM_OP_BITAND:
        TA_ASSERT(pctx->thdr->type != TA_DOUBLE);
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(&=, unsigned char); break;
        case TA_INT: INTEGERLOOP(&=, int); break;
        case TA_UINT: INTEGERLOOP(&=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(&=, Tcl_WideInt); break;
        }
        break;
    case TAM_OP_BITOR:
        TA_ASSERT(pctx->thdr->type != TA_DOUBLE);
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(|=, unsigned char); break;
        case TA_INT: INTEGERLOOP(|=, int); break;
        case TA_UINT: INTEGERLOOP(|=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(|=, Tcl_WideInt); break;
        }
        break;
    }
}


TCL_RESULT tcol_math_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    struct ta_math_operand operands[2];
    struct ta_math_operand *poperands;
    int i, j, ncontexts, noperands;
    struct thdr_math_mt_context mt_context[4];
    thdr_t *result_thdr = NULL;
    TCL_RESULT status;
    int result_type;
    int mt_sizes[4];
    int thdr_size = 0;
    int op;
    int only_scalars = 1;

    if (objc < 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "operation tarray ?tarray...?");
	return TCL_ERROR;
    }
    
    if ((status = Tcl_GetIndexFromObj(ip, objv[1], ta_math_op_names, "operation", 0, &op)) != TCL_OK)
        return status;
    
    noperands = objc - 2;    

    /*
     * Common case of two operands, save on allocation.
     * Note noperands is the *maximum* number of entries that will be needed.
     */
    if (noperands > ARRAYSIZE(operands))
        poperands = TA_ALLOCMEM(noperands * sizeof(*poperands));
    else
        poperands = operands;

    /*
     * The loop does three related things:
     *
     * - ensure compatibility of each operand
     * - figure out type promotion
     * - collect the operands
     */
    result_type = TA_BYTE;      /* Assume smallest width */
    for (i = 0, j = 2; j < objc; ++i, ++j) {
        if (tcol_convert(NULL, objv[j]) == TCL_OK) {
            /* Check if size is consistent with previous thdrs */
            thdr_t *thdr = tcol_thdr(objv[j]);
            if (thdr_size) {
                if (thdr->used != thdr_size) {
                    status = ta_column_lengths_error(ip);
                    goto vamoose;
                }
            } else
                thdr_size = thdr->used; /* Init expected size of column */

            /* Column. Check if permitted type */
            switch (tcol_type(objv[j])) {
            case TA_BYTE:
                break;
            case TA_INT:
                if (result_type == TA_BYTE)
                    result_type = TA_INT;
                break;
            case TA_UINT: /* Fall thru */
            case TA_WIDE:
                if (result_type != TA_DOUBLE)
                    result_type = TA_WIDE;
                break;
            case TA_DOUBLE:
                result_type = TA_DOUBLE;
                break;
            default:
                status = ta_bad_type_error(ip, tcol_thdr(objv[j]));
                goto vamoose;
            }
            poperands[i].thdr_operand = tcol_thdr(objv[j]);
            only_scalars = 0;
        } else {
            /* Check if an integer, wide or double */
            ta_value_t *ptav = &poperands[i].scalar_operand;
            if (Tcl_GetWideIntFromObj(NULL, objv[j], &ptav->wval) == TCL_OK) {
                /* Note integers are also stored as wides during computation */
                ptav->type = TA_WIDE;
                if (ptav->wval >= INT_MIN && ptav->wval <= INT_MAX) {
                    if (result_type == TA_BYTE)
                        result_type = TA_INT;
                } else if (result_type != TA_DOUBLE)
                    result_type = TA_WIDE;
            } else {
                status = Tcl_GetDoubleFromObj(ip, objv[j], &ptav->dval);
                if (status == TCL_OK) {
                    ptav->type = TA_DOUBLE;
                    result_type = TA_DOUBLE;
                } else
                    goto vamoose;
            }
            poperands[i].thdr_operand = NULL; /* Indicate scalar_operand is valid */
        }
    }
    
    if (result_type == TA_DOUBLE &&
        (op == TAM_OP_BITAND || op == TAM_OP_BITOR)) {
        Tcl_SetObjResult(ip, Tcl_NewStringObj("Bit operations not valid for type double", -1));
        status = TCL_ERROR;
        goto vamoose;
    }

    /* If we are only passed scalars, compute and return the result */
    if (only_scalars) {
        if (result_type == TA_DOUBLE) {
            double dresult;

            if (poperands[0].scalar_operand.type == TA_DOUBLE)
                dresult = poperands[0].scalar_operand.dval;
            else
                dresult = (double) poperands[0].scalar_operand.wval;
            
            for (j = 1 ; j < noperands; ++j) {
                TA_ASSERT(poperands[j].scalar_operand.type == TA_DOUBLE || poperands[j].scalar_operand.type == TA_WIDE );
                dresult = ta_math_double_operation(op, dresult, &poperands[j].scalar_operand);
            }
            Tcl_SetObjResult(ip, Tcl_NewDoubleObj(dresult));
        } else {
            Tcl_WideInt wresult = poperands[0].scalar_operand.wval;
            for (j = 1 ; j < noperands; ++j) {
                TA_ASSERT(poperands[j].scalar_operand.type == TA_WIDE);
                wresult = ta_math_wide_operation(op, wresult, poperands[j].scalar_operand.wval);
            }
            Tcl_SetObjResult(ip, Tcl_NewWideIntObj(wresult));
        }
        status = TCL_OK;
        goto vamoose;           /* yea yea gotos are bad */
    }

    if (thdr_size == 0) {
        /* Empty columns. Return an empty column */
        Tcl_SetObjResult(ip, tcol_new(thdr_alloc(ip, result_type, 0)));
        status = TCL_OK;
        goto vamoose;
    }

    /* We have at least one column and with at least one element */

#if !defined(TA_MT_ENABLE)
    ncontexts = 1;
    mt_sizes[0] = thdr_size;
#else
    ncontexts = thdr_calc_mt_split_ex(result_type, 0, thdr_size, 
                                      ta_math_mt_threshold, 
                                      ARRAYSIZE(mt_sizes), mt_sizes);
#   if defined(TA_ENABLE_ASSERT)
    {
        int total = 0;
        for (j = 0; j < ncontexts; ++j) {
            total += mt_sizes[j];
        }
        TA_ASSERT(total == thdr_size);
    }
#   endif
    
#endif

    /* Allocate the result thdr based on the type we want to return */
    result_thdr = thdr_alloc(ip, result_type, thdr_size);
    if (result_thdr == NULL) {
        status = TCL_ERROR;
        goto vamoose;
    }

    mt_context[0].thdr = result_thdr;
    mt_context[0].poperands = poperands;
    mt_context[0].noperands = noperands;
    mt_context[0].res = TCL_OK;
    mt_context[0].op = op;
    mt_context[0].start = 0;
    mt_context[0].count = mt_sizes[0];
    
    if (ncontexts == 1) {
        thdr_math_mt_worker(&mt_context[0]);
    }
#if defined(TA_MT_ENABLE)
    else {
        ta_mt_group_t grp;
        for (j = 1; j < ncontexts; ++j) {
            mt_context[j].thdr = mt_context[0].thdr;
            mt_context[j].poperands = poperands;
            mt_context[j].noperands = noperands;
            mt_context[j].res = TCL_OK;
            mt_context[j].op = op;
            mt_context[j].count = mt_sizes[j];
            mt_context[j].start = mt_context[j-1].start + mt_context[j-1].count;
        }
        
        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* Fire off other threads. Context 0 we will handle ourselves */
        for (j = 1; j < ncontexts; ++j) {
            /* TBD - check return code */
            ta_mt_group_async_f(grp, &mt_context[j], thdr_math_mt_worker);
        }
        thdr_math_mt_worker(&mt_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);
    }
#endif /* TA_MT_ENABLE */

    /* First verify all threads ran successfully */
    status = TCL_OK;
    for (j = 0; j < ncontexts; ++j) {
       if (mt_context[j].res != TCL_OK) {
            status = TCL_ERROR;
            break;
        }
    }

    if (status == TCL_OK) {
        result_thdr->used = thdr_size;
        Tcl_SetObjResult(ip, tcol_new(result_thdr));
        result_thdr = NULL;     /* So it does not get freed below */
    }


vamoose:
    if (poperands != operands)
        TA_FREEMEM(poperands);
    if (result_thdr != NULL) {
        /* May be because of pure scalar result or error */
        thdr_decr_refs(result_thdr);
    }

    return status;
}
