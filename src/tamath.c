/*
 * Copyright (c) 2012-2015, Ashok P. Nadkarni
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


struct thdr_math_mt_context {
    thdr_t  *thdr;              /* Will hold result (SHARED among threads)
                                   thdr->header must NOT be modified except
                                   by main thread!!!
                                 */
    thdr_t  **poperands;        /* Operands */
    ta_value_t scalar;          /* Scalar total */
    int      noperands;         /* Number of operands */
    int      start;             /* Starting position in source thdr
                                   for this thread */
    int      count;             /* Number of elements in thdr to examine */
    TCL_RESULT res;             /* Status of thread */
    enum ta_math_op_e op;                /* Operation */
};    


static void thdr_math_mt_worker(struct thdr_math_mt_context *pctx)
{
    TA_ASSERT((pctx->start + pctx->count) <= pctx->thdr->usable);
    TA_ASSERT((pctx->start + pctx->count) <= pctx->poperands[0]->used);

#define OPLOOP_ (op_, initval_, accumulator_type_, srctype_) 
    do {
        int i, j, end;
        accumulator_type_ accumulator = initval_;
        for (i = pctx->start, end = pctx->start + pctx->count; i < end; ++i) {
            accumulator = (initval_);
            for (j = 0; j < pctx->noperands;);
        }
    } while (0)

    switch (pctx->op) {
    case TAM_OP_PLUS:
        switch (pctx->thdr->type) {
        case TA_BYTE:
        case TA_INT:
        case TA_UINT:
        case TA_WIDE:
        case TA_DOUBLE:{
            
        }
            
        }
        break;
    case TAM_OP_MINUS:
        break;
    case TAM_OP_MUL:
        break;
    case TAM_OP_DIV:
        break;
    case TAM_OP_BITAND:
        break;
    case TAM_OP_BITOR:
        break;
    }
}

TCL_RESULT tcol_math_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    int nscalars, nthdrs;
    int op;
    thdr_t *thdr;
    thdr_t *thdrs[2], **pthdrs;
    ta_value_t scalars[2], *pscalars;
    ta_value_t scalar_result;
    TCL_RESULT status;
    int result_type;
    struct thdr_math_mt_context mt_context[4];
    int j, ncontexts, noperands;
    int mt_sizes[4];
    int thdr_size = 0;
    thdr_t *first_operand_thdr = NULL;
    ta_value_t first_operand_scalar;

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
    if (noperands > ARRAYSIZE(thdrs)) {
        pthdrs = TA_ALLOCMEM(noperands * sizeof(*pthdrs));
        pscalars = TA_ALLOCMEM(noperands * sizeof(*pscalars));
    } else {
        pthdrs = thdrs;
        pscalars = scalars;
    }

    /*
     * The loop does three related things:
     *
     * - ensure compatibility of each operand
     * - figure out type promotion
     * - collect the scalars and columns
     */
    result_type = TA_BYTE;      /* Assume smallest width */
    nthdrs =  0;
    nscalars = 0;
    for (j = 2; j < objc; ++j) {
        if (tcol_convert(NULL, objv[j]) == TCL_OK) {
            /* Check if size is consistent with previous thdrs */
            thdr = tcol_thdr(objv[j]);
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
            pthdrs[nthdrs] = tcol_thdr(objv[j]);
            if (j == 2) {
                /*
                 * The first operand is treated differently for operations
                 * like minus. Remember what it was
                 */
                TA_ASSERT(nthdrs == 0);
                first_operand_thdr = pthdrs[0];
            }
            ++nthdrs;
        } else {
            /* Check if an integer, wide or double */
            if (Tcl_GetWideIntFromObj(NULL, objv[j], &pscalars[nscalars].wval)
                == TCL_OK) {
                pscalars[nscalars].type = TA_WIDE;
                /* Note integers are also stored as wides during computation */
                if (pscalars[nscalars].wval >= INT_MIN &&
                    pscalars[nscalars].wval <= INT_MAX) {
                    if (result_type == TA_BYTE)
                        result_type = TA_INT;
                } else if (result_type != TA_DOUBLE)
                        result_type = TA_DOUBLE;
            } else {
                status = Tcl_GetDoubleFromObj(ip, objv[j], &pscalars[nscalars].dval);
                if (status == TCL_OK) {
                    pscalars[nscalars].type = TA_DOUBLE;
                    result_type = TA_DOUBLE;
                } else
                    goto vamoose;
            }
            if (j == 2) {
                TA_ASSERT(nscalars == 0);
                first_operand_scalar = pscalars[0];
            }
            ++nscalars;
        }
    }
    
    

    /* Add up (or whatever) the scalar values. Note that all integer
       values are stored as wides irrespective of their actual width */
    if (result_type == TA_DOUBLE) {
        if (op == TAM_OP_BITAND || op == TAM_OP_BITOR) {
            Tcl_SetObjResult(interp, Tcl_NewStringObject("Bit operations not valid for type double", -1));
            status = TCL_ERROR;
            goto vamoose;
        }

        /* When combining scalars, remember that the first operand
         * is treated differently (consider subtraction for instance).
         * If the first operand was a scalar, initialize with it else
         * initialize based on the operation.
         */
        if (first_operand_thdr == NULL) {
            TBD;
        } else {
            switch (op) {
            case TAM_OP_PLUS:
            }
        }

        scalar_result.type = TA_DOUBLE;
        scalar_result.dval = 0.0;
        for (j = 0 ; j < nscalars; ++j) {
            TA_ASSERT(pscalars[j].type == TA_WIDE || pscalars[j].type == TA_DOUBLE);
            if (pscalars[j].type == TA_DOUBLE)
                scalar_result.dval += pscalars[j].dval;
            else
                scalar_result.dval += (double) pscalars[j].wval;
        }
    } else {
        scalar_result.wval = 0;
        for (j = 0 ; j < nscalars; ++j) {
            TA_ASSERT(pscalars[j].type == TA_WIDE);
            scalar_result.wval += pscalars[j].wval;
        }
    }

    /* If no columns specified, just return the scalar */
    if (nthdrs == 0) {
        switch (result_type) {
        case TA_BYTE:
            scalar_result.ucval = (unsigned char) scalar_result.wval;
            break;
        case TA_INT:
            scalar_result.ival = (int) scalar_result.wval;
            break;
        }
        Tcl_SetObjResult(ip, ta_value_to_obj(&scalar_result));
        return TCL_OK;
    }
        
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
    mt_context[0].thdr = thdr_alloc(ip, result_type, thdr_size);
    if (mt_context[0].thdr == NULL)
        return TCL_ERROR;

    mt_context[0].poperands = pthdrs;
    mt_context[0].noperands = nthdrs;
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
            mt_context[j].poperands = pthdrs;
            mt_context[j].noperands = nthdrs;
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

    

vamoose:
    if (pthdrs != thdrs)
        TA_FREEMEM(pthdrs);
    if (pscalars != scalars)
        TA_FREEMEM(pscalars);

    return status;
}
