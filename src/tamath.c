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
    "+", "-", "*", "/", "&&", "||", "^^", "&", "|", "^", NULL
};
enum ta_math_op_e {
    TAM_OP_PLUS,TAM_OP_MINUS,TAM_OP_MUL,TAM_OP_DIV,
    TAM_OP_AND,TAM_OP_OR, TAM_OP_XOR,
    TAM_OP_BITAND,TAM_OP_BITOR, TAM_OP_BITXOR,
};

TA_INLINE is_logical_op(enum ta_math_op_e op) {
    return (op == TAM_OP_AND || op == TAM_OP_OR || op == TAM_OP_XOR);
}

TA_INLINE is_bit_op(enum ta_math_op_e op) {
    return (op == TAM_OP_BITAND || op == TAM_OP_BITOR || op == TAM_OP_BITXOR);
}

struct ta_math_operand {
    int     span_start; /* Where in thdr_operand the logical column starts */
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
    int      error_code;        /* If non-0, one of the error codes below */
#define TAM_DIV0  1
    enum ta_math_op_e op;                /* Operation */
};    

static double ta_math_double_from_operand(struct ta_math_operand *poperand,
                                          int thdr_index)
{
    thdr_t *thdr = poperand->thdr_operand;
    if (thdr) {
        thdr_index += poperand->span_start;
        TA_ASSERT(thdr->used > thdr_index);
        switch (thdr->type) {
        case TA_BOOLEAN:
            return ba_get(THDRELEMPTR(thdr, ba_t, 0), thdr_index);
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
        thdr_index += poperand->span_start;
        TA_ASSERT(thdr->used > thdr_index);
        switch (thdr->type) {
        case TA_BOOLEAN:
            return ba_get(THDRELEMPTR(thdr, ba_t, 0), thdr_index);
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

    TA_ASSERT(! is_bit_op(op));
    TA_ASSERT(poperand->type == TA_DOUBLE || poperand->type == TA_WIDE);

    if (poperand->type == TA_DOUBLE)
        operand = poperand->dval;
    else
        operand = (double) poperand->wval; /* TBD - overflow check? */

    switch (op) {
    case TAM_OP_PLUS: return accumulator + operand;
    case TAM_OP_MINUS: return accumulator - operand;
    case TAM_OP_MUL: return accumulator * operand;
    case TAM_OP_DIV: return accumulator / operand; /* Check for div-by-0 ? */
    case TAM_OP_AND: return accumulator && operand;
    case TAM_OP_OR: return accumulator || operand;
    case TAM_OP_XOR:
        if ((accumulator && operand) ||
            (accumulator == 0 && operand == 0))
            return 0;
        else 
            return 1;
    case TAM_OP_BITAND: /* Keep gcc happy */
    case TAM_OP_BITOR:  /* ditto */
    case TAM_OP_BITXOR: /* ditto */
        Tcl_Panic("Invalid math double operand");
    }
    return 0.0;                 /* To keep compiler happy */
}    

static Tcl_WideInt ta_math_wide_operation(enum ta_math_op_e op, Tcl_WideInt accumulator, Tcl_WideInt operand)
{
    Tcl_WideInt result;
    switch (op) {
    case TAM_OP_PLUS: return accumulator + operand;
    case TAM_OP_MINUS: return accumulator - operand;
    case TAM_OP_MUL: return accumulator * operand;
    case TAM_OP_DIV:
        TA_ASSERT(operand != 0); /* Caller should have ensured */
        /* See INST_DIV in tclExecute.c in Tcl sources */
        result = accumulator / operand;
        if (((result < 0) || ((result == 0) &&
                              ((accumulator < 0 && operand > 0) ||
                               (accumulator > 0 && operand < 0)))) &&
            ((result * operand) != accumulator)) {
            result -= 1;
        }
        return result;
    case TAM_OP_AND: return accumulator && operand;
    case TAM_OP_OR: return accumulator || operand;
    case TAM_OP_XOR:
        if ((accumulator && operand) ||
            (accumulator == 0 && operand == 0))
            return 0;
        else 
            return 1;
    case TAM_OP_BITOR: return accumulator | operand;
    case TAM_OP_BITAND: return accumulator & operand;
    case TAM_OP_BITXOR: return accumulator ^ operand;
    }
    return 0;                   /* To keep compiler happy */
}

static void thdr_math_mt_worker(void *pv)
{
    struct thdr_math_mt_context *pctx = pv;
    struct ta_math_operand *poperands;
    int noperands;
    int start, end;
    unsigned char type;

    TA_ASSERT(pctx->count > 0);
    TA_ASSERT((pctx->start + pctx->count) <= pctx->thdr->usable);

    type = pctx->thdr->type;
    TA_ASSERT(type == TA_BYTE || type == TA_INT || type == TA_UINT || type == TA_DOUBLE || type == TA_WIDE);
    poperands = pctx->poperands;
    noperands = pctx->noperands;
    start = pctx->start;
    end = start + pctx->count;

    TA_ASSERT(noperands > 0);

    /*
     * The loops below use the supplied indices as is without adjusting
     * for the column span because the ta_math_xxx_from_operand already
     * account for the span starting offset.
     */
#define DOUBLELOOP(op_)                                               \
    do {                                                                \
        int i, j;                                                       \
        double *p = THDRELEMPTR(pctx->thdr, double, 0); \
        for (i = start; i < end; ++i) {                                 \
            double accum = ta_math_double_from_operand(&poperands[0], i); \
            for (j = 1; j < noperands; ++j) {                           \
                accum op_ ta_math_double_from_operand(&poperands[j], i); \
            }                                                           \
            p[i] = accum;                \
        }                                                               \
    } while (0)

#define INTEGERLOOP(op_, type_)                                       \
    do {                                                                \
        int i, j;                                                       \
        type_ *p = THDRELEMPTR(pctx->thdr, type_, 0);                   \
        for (i = start; i < end; ++i) {                                 \
            Tcl_WideInt accum = ta_math_wide_from_operand(&poperands[0], i); \
            for (j = 1; j < noperands; ++j) {                           \
                accum op_ ta_math_wide_from_operand(&poperands[j], i); \
            }                                                           \
            p[i] = (type_) accum;         \
        }                                                               \
    } while (0)
    
    /* Integer div. See INST_DIV in tclExecute.c in Tcl sources for semantics */
#define DIVLOOP(type_)                                                  \
    do {                                                                \
        int i, j;                                                       \
        type_ *p = THDRELEMPTR(pctx->thdr, type_, 0);                   \
        for (i = start; i < end; ++i) {                                 \
            Tcl_WideInt accum = ta_math_wide_from_operand(&poperands[0], i); \
            for (j = 1; j < noperands; ++j) {                           \
                Tcl_WideInt wresult;                                    \
                Tcl_WideInt operand = ta_math_wide_from_operand(&poperands[j], i); \
                if (operand == 0) {                                     \
                    pctx->error_code = TAM_DIV0;                        \
                    i = end; /* To break outer loop */                  \
                    break;                                              \
                }                                                       \
                wresult = accum / operand;                              \
                if (((wresult < 0) || ((wresult == 0) &&                \
                                       ((accum < 0 && operand > 0) ||   \
                                        (accum > 0 && operand < 0)))) && \
                    ((wresult * operand) != accum)) {                   \
                    wresult -= 1;                                       \
                }                                                       \
                accum = wresult;                                        \
            }                                                           \
            p[i] = (type_) accum;         \
        }                                                               \
    } while (0)

    switch (pctx->op) {
    case TAM_OP_PLUS:
        switch (type) {
        case TA_BYTE: INTEGERLOOP(+=, unsigned char); break;
        case TA_INT: INTEGERLOOP(+=, int); break;
        case TA_UINT: INTEGERLOOP(+=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(+=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(+=); break;
        }
        break;
    case TAM_OP_MINUS:
        switch (type) {
        case TA_BYTE: INTEGERLOOP(-=, unsigned char); break;
        case TA_INT: INTEGERLOOP(-=, int); break;
        case TA_UINT: INTEGERLOOP(-=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(-=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(-=); break;
        }
        break;
    case TAM_OP_MUL:
        switch (type) {
        case TA_BYTE: INTEGERLOOP(*=, unsigned char); break;
        case TA_INT: INTEGERLOOP(*=, int); break;
        case TA_UINT: INTEGERLOOP(*=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(*=, Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(*=); break;
        }
        break;
    case TAM_OP_DIV:
        switch (type) {
        case TA_BYTE: DIVLOOP(unsigned char); break;
        case TA_INT: DIVLOOP(int); break;
        case TA_UINT: DIVLOOP(unsigned int); break;
        case TA_WIDE: DIVLOOP(Tcl_WideInt); break;
        case TA_DOUBLE: DOUBLELOOP(/=); break;
        }
        break;
    case TAM_OP_BITAND:
        TA_ASSERT(type != TA_DOUBLE);
        switch (type) {
        case TA_BYTE: INTEGERLOOP(&=, unsigned char); break;
        case TA_INT: INTEGERLOOP(&=, int); break;
        case TA_UINT: INTEGERLOOP(&=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(&=, Tcl_WideInt); break;
        }
        break;
    case TAM_OP_BITOR:
        TA_ASSERT(type != TA_DOUBLE);
        switch (type) {
        case TA_BYTE: INTEGERLOOP(|=, unsigned char); break;
        case TA_INT: INTEGERLOOP(|=, int); break;
        case TA_UINT: INTEGERLOOP(|=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(|=, Tcl_WideInt); break;
        }
        break;
    case TAM_OP_BITXOR:
        TA_ASSERT(type != TA_DOUBLE);
        switch (type) {
        case TA_BYTE: INTEGERLOOP(^=, unsigned char); break;
        case TA_INT: INTEGERLOOP(^=, int); break;
        case TA_UINT: INTEGERLOOP(^=, unsigned int); break;
        case TA_WIDE: INTEGERLOOP(^=, Tcl_WideInt); break;
        }
        break;
    }
}


/* Called for bit operations on pure boolean values. No column
   should be anything other than boolean. Scalars must be TA_WIDE
   and are treated as booleans using the low bit.
*/
static TCL_RESULT ta_math_boolean_result(
    Tcl_Interp *ip, int op,
    struct ta_math_operand *poperands, /* Must have at least one column */
    int noperands,
    int size /* Every col in poperands expected to be of this size */
)
{
    thdr_t *thdr;
    ba_t *baP;
    int opindex;
    int need_complement;
    thdr_t *thdr2 = NULL;

    TA_ASSERT(noperands > 0);
    
    if (! (is_bit_op(op) || is_logical_op(op)))
        return ta_invalid_op_for_type(ip, TA_BOOLEAN);

    thdr = thdr_alloc(ip, TA_BOOLEAN, size);
    if (thdr == NULL)
        return TCL_ERROR;
    baP = THDRELEMPTR(thdr, ba_t, 0);
    
    if (size == 0)
        goto done;

    /*
     * Each operand is expected to be a boolean column or a TA_WIDE
     * scalar value converted from a boolean 0/1/true/false etc.
     * Optimize by going through all scalar operands first. So
     * For | if any scalar is 1, just return a columns of 1's etc.
     * For & if any scalar is 0, just return a column of 0's etc.
     * For ^, if computed value is 1, complement at the end.
     * We may save unnecessary computations.
     */
    need_complement = 0;
    for (opindex = 0; opindex < noperands; ++opindex) {
        if (poperands[opindex].thdr_operand == NULL) {
            /* Scalar operand */
            struct ta_math_operand *poper = &poperands[opindex];
            int ival;
            TA_ASSERT(poper->scalar_operand.type == TA_WIDE);
            if (is_bit_op(op))
                ival = (poper->scalar_operand.wval & 1);
            else
                ival = (poper->scalar_operand.wval != 0);
            if (ival == 0) {
                if (op == TAM_OP_BITAND || op == TAM_OP_AND) {
                    ba_fill(baP, 0, size, 0);
                    thdr->used = size;
                    goto done;
                }
            } else {
                if (op == TAM_OP_BITOR || op == TAM_OP_OR) {
                    ba_fill(baP, 0, size, 1);
                    thdr->used = size;
                    goto done;
                }
                if (op == TAM_OP_BITXOR || op == TAM_OP_XOR) {
                    need_complement ^= 1; /* Complement result */
                }
            }
        }
    }

    /* TBD - optimize for case where all operands are columns and
       use ba_conjunct etc.
    */
    /* TBD - optimize for case of a single column. Just return the
       same column unless need to complement */
    

    /* Initialize the result vector based on the first column operand */
    if (is_bit_op(op)) {
        /* Bitwise operations. Columns must be boolean */
        for (opindex = 0; opindex < noperands; ++opindex) {
            if (poperands[opindex].thdr_operand)
                break;              /* Found a column */
        }
        TA_ASSERT(opindex < noperands); /* Must be at least one column */
        TA_ASSERT(poperands[opindex].thdr_operand->type == TA_BOOLEAN);
        thdr_copy(thdr, 0, poperands[opindex].thdr_operand, 
                  poperands[opindex].span_start, size, 0);
        if (need_complement)
            ba_complement(baP, 0, size);
        opindex += 1;           /* Since we have taken this col into account */
    } else {
        /*
         * Logical operations. Columns need not be boolean. Just initialize
         * to identity values. Not worth optimizing for each column type.
         */
        opindex = 0;
        if (op == TAM_OP_AND) 
            ba_fill(baP, 0, size, 1);
        else
            ba_fill(baP, 0, size, 0); /* OR and XOR */
        thdr->used = size;
    }
    
    /* Now handle remaining operands, skipping scalars */ 
    /* TBD - optimize by combining two columns at a time recursively */
    
    /* Loop for bitwise operations */
    if (is_bit_op(op)) {
        for ( ; opindex < noperands; ++opindex) {
            struct ta_math_operand *poper = &poperands[opindex];
            ba_t *ba_oper;
            if (poper->thdr_operand == NULL)
                continue;           /* Skip scalar */
            /* Column operand */
            TA_ASSERT(poper->thdr_operand->type == TA_BOOLEAN);
            TA_ASSERT(poper->thdr_operand->used >= poper->span_start+size);
            ba_oper = THDRELEMPTR(poper->thdr_operand, ba_t, 0);
            switch (op) {
            case TAM_OP_BITAND: 
                ba_conjunct(baP, 0, ba_oper, poper->span_start, size);
                break;
            case TAM_OP_BITOR: 
                ba_disjunct(baP, 0, ba_oper, poper->span_start, size);
                break;
            case TAM_OP_BITXOR:
                ba_xdisjunct(baP, 0, ba_oper, poper->span_start, size);
                break;
            }
        }
    } else {
        /* Loop for logical operations. We convert each non-boolean column
         * to boolean and then do the operation. That is faster than
         * individually converting values at the expense of memory
         */
        for ( ; opindex < noperands; ++opindex) {
            struct ta_math_operand *poper = &poperands[opindex];
            ba_t *ba_oper;
            int start;
            if (poper->thdr_operand == NULL)
                continue;           /* Skip scalar */
            /* Column operand */
            TA_ASSERT(poper->thdr_operand->used >= poper->span_start+size);
            if (poper->thdr_operand->type == TA_BOOLEAN) {
                ba_oper = THDRELEMPTR(poper->thdr_operand, ba_t, 0);
                start = poper->span_start;
            } else {
                /* Not a boolean column. Convert first. */
                if (thdr2 == NULL)
                    thdr2 = thdr_alloc(ip, TA_BOOLEAN, size);
                if (thdr2 == NULL) {
                    thdr_decr_refs(thdr);
                    return TCL_ERROR;
                }
                if (thdr_copy_cast(ip, thdr2, 0, poper->thdr_operand, poper->span_start, size, 0) != TCL_OK) {
                    thdr_decr_refs(thdr2);
                    thdr_decr_refs(thdr);
                    return TCL_ERROR;
                }
                ba_oper = THDRELEMPTR(thdr2, ba_t, 0);
                start = 0;
            }
            switch (op) {
            case TAM_OP_AND: 
            case TAM_OP_BITAND: 
                ba_conjunct(baP, 0, ba_oper, start, size);
                break;
            case TAM_OP_OR: 
            case TAM_OP_BITOR: 
                ba_disjunct(baP, 0, ba_oper, start, size);
                break;
            case TAM_OP_XOR:
            case TAM_OP_BITXOR:
                ba_xdisjunct(baP, 0, ba_oper, start, size);
                break;
            }
        }
    }
        
done:
    if (thdr2)
        thdr_decr_refs(thdr2);
    Tcl_SetObjResult(ip, tcol_new(thdr));
    return TCL_OK;
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
    unsigned char coltype = TA_NONE;
    int have_boolean_col = 0;

    if (objc < 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "operation tarray ?tarray...?");
	return TCL_ERROR;
    }
    
    if ((status = ta_opt_from_obj(ip, objv[1], ta_math_op_names, "operation", 0, &op)) != TCL_OK)
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

    /* TBD - optimizations
       if scalar is identity for the operation, skip that operand.
       If scalar is the "zero" for the operation return the zero column
    */
    
    /*
     * The loop does three related things:
     *
     * - ensure compatibility of each operand
     * - figure out type promotion
     * - collect the operands
     */
    result_type = TA_NONE;
    for (i = 0, j = 2; j < objc; ++i, ++j) {
        if (tcol_convert(NULL, objv[j]) == TCL_OK) {
            Tcl_Obj *tcol = objv[j];
            span_t *span;
            
            /* Check if size is consistent with previous thdrs */
            if (thdr_size) {
                if (tcol_occupancy(tcol) != thdr_size) {
                    status = ta_column_lengths_error(ip);
                    goto vamoose;
                }
            } else
                thdr_size = tcol_occupancy(tcol); /* Init expected size of column */
            coltype = tcol_type(tcol);
            switch (coltype) {
            case TA_BOOLEAN:
                have_boolean_col = 1; /* So we do not multithread */
                if (result_type == TA_NONE)
                    result_type = TA_BOOLEAN;
                break;
            case TA_BYTE:
                if (result_type == TA_NONE || result_type == TA_BOOLEAN)
                    result_type = TA_BYTE;
                break;
            case TA_INT:
                if (result_type == TA_NONE || result_type == TA_BOOLEAN || result_type == TA_BYTE)
                    result_type = TA_INT;
                break;
            case TA_UINT:
                if (result_type != TA_DOUBLE && result_type != TA_WIDE)
                    result_type = TA_UINT;
                break;
            case TA_WIDE:
                if (result_type != TA_DOUBLE)
                    result_type = TA_WIDE;
                break;
            case TA_DOUBLE:
                result_type = TA_DOUBLE;
                break;
            default:
                status = ta_invalid_op_for_type(ip, coltype);
                goto vamoose;
            }
            poperands[i].thdr_operand = tcol_thdr(tcol);
            span = tcol_span(tcol);
            poperands[i].span_start = span ? span->first : 0;
            only_scalars = 0;
        } else {
            /* Scalar operand. Check if an integer, wide or double.
               If we have already seen a boolean column, do NOT change
               the expected result type.
             */
            ta_value_t *ptav = &poperands[i].scalar_operand;
            if (Tcl_GetWideIntFromObj(NULL, objv[j], &ptav->wval) == TCL_OK) {
                /* Note integers are also stored as wides during computation */
                ptav->type = TA_WIDE;

                if (result_type != TA_DOUBLE 
                    && result_type != TA_WIDE) {
                    /* If value fits in a byte, it fits in any type so
                       no need to change. Else figure out whether result
                       type needs promotion */
                    if (ptav->wval < 0 || ptav->wval > UCHAR_MAX) {
                        if (ptav->wval < INT_MIN || ptav->wval > UINT_MAX)
                            result_type = TA_WIDE;
                        else {
                            /* Conflict between UINT and INT is resolved
                               by choosing UINT as in C */
                            if (result_type != TA_UINT) {
                                if (ptav->wval > INT_MAX)
                                    result_type = TA_UINT;
                                else 
                                    result_type = TA_INT;
                            }
                        }
                    } else {
                        if (result_type == TA_NONE ||
                            result_type == TA_BOOLEAN) {
                            if (ptav->wval == 0 || ptav->wval == 1)
                                result_type = TA_BOOLEAN;
                            else
                                result_type = TA_BYTE;
                        }
                    }
                }
            } else {
                status = Tcl_GetDoubleFromObj(NULL, objv[j], &ptav->dval);
                if (status == TCL_OK) {
                    ptav->type = TA_DOUBLE;
                    result_type = TA_DOUBLE;
                } else {
                    /* Scalar is not a numeric. See if it might be a boolean
                       (true or false). These are NOT treated as numeric
                       and thus result_type is set to TA_BOOLEAN.
                    */
                    int ival;
                    status = Tcl_GetBooleanFromObj(NULL, objv[j], &ival);
                    if (status != TCL_OK) {
                        ta_invalid_operand_error(ip, objv[j]);
                        goto vamoose;
                    }
                    /* "true" etc. can only be used in logical operations */
                    if (! is_logical_op(op)) {
                        status = ta_invalid_op_for_type(ip, TA_BOOLEAN);
                        goto vamoose;
                    }
                    if (result_type == TA_NONE)
                        result_type = TA_BOOLEAN;
                    /* All non-doubles are stored as wides */
                    ptav->type = TA_WIDE;
                    ptav->wval = ival;
                }
            }
            poperands[i].thdr_operand = NULL; /* Indicate scalar_operand is valid */
        }
    }
    
    TA_ASSERT(result_type != TA_NONE && result_type != TA_STRING && result_type != TA_ANY);
    
    if (result_type == TA_DOUBLE && is_bit_op(op)) {
        status = ta_invalid_op_for_type(ip, TA_DOUBLE);
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
            if (is_logical_op(op)) 
                Tcl_SetObjResult(ip, Tcl_NewIntObj(dresult ? 1 : 0));
            else
                Tcl_SetObjResult(ip, Tcl_NewDoubleObj(dresult));
        } else {
            Tcl_WideInt wresult = poperands[0].scalar_operand.wval;
            for (j = 1 ; j < noperands; ++j) {
                TA_ASSERT(poperands[j].scalar_operand.type == TA_WIDE);
                if (op == TAM_OP_DIV &&
                    poperands[j].scalar_operand.wval == 0) {
                    Tcl_SetResult(ip, "divide by zero", TCL_STATIC);
                    status = TCL_ERROR;
                    goto vamoose;
                }
                wresult = ta_math_wide_operation(op, wresult, poperands[j].scalar_operand.wval);
            }
            Tcl_SetObjResult(ip, Tcl_NewWideIntObj(wresult));
        }
        status = TCL_OK;
        goto vamoose;           /* yea yea gotos are bad */
    }

    /* Irrespective of type promotion, column types for logical operations
     * is always TA_BOOLEAN
     */
    if (is_logical_op(op))
        result_type = TA_BOOLEAN;

    /* Boolean types need special treatment. If the operator is an
     * arithmetic operator, the result type needs to be changed to
     * integer. Otherwise, call the boolean type-specific code
     * as opposed to the general purpose code below.
     */
    if (result_type == TA_BOOLEAN) {
        if (is_bit_op(op) || is_logical_op(op)) {
            status = ta_math_boolean_result(ip, op, poperands, noperands, thdr_size);
            goto vamoose;
        } else
            result_type = TA_INT;
    }
    if (thdr_size == 0) {
        /* Empty columns. Return an empty column of the appropriate type */
        Tcl_SetObjResult(ip, tcol_new(thdr_alloc(ip, result_type, 0)));
        status = TCL_OK;
        goto vamoose;
    }

    /* We have at least one column and with at least one element */

#if !defined(TA_MT_ENABLE)
    ncontexts = 1;
    mt_sizes[0] = thdr_size;
#else
    /*
     * Note about multithreading - the different column operands
     * may have different alignments and span offsets. From a MT
     * perspective, this does not matter because they are only
     * read from. We need only be concerned with the thdr
     * that is being written to as far as alignment issues are
     * concerned.
     */
    if (have_boolean_col) {
        /* If boolean columns involved, turn off MT. It
         * will probably work for operands since they are
         * read-only but not if result is also boolean.
         * For now, just disable MT in general if booleans
         * are involved in any fashion.
         */
        ncontexts = 1;
        mt_sizes[0] = thdr_size;
    } else {
        ncontexts = thdr_calc_mt_split_ex(result_type, 0, thdr_size, 
                                          ta_math_mt_threshold, 
                                          ARRAYSIZE(mt_sizes), mt_sizes);
    }
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
    mt_context[0].error_code = 0;
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
            mt_context[j].error_code = 0;
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
       if (mt_context[j].error_code != 0) {
           char *msg;
           switch (mt_context[j].error_code) {
           case TAM_DIV0:
               msg = "divide by zero";
               break;
           default:
               msg = "error in math operation";
               break;
           }
           Tcl_SetResult(ip, msg, TCL_STATIC);
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

/* Parses a Tcl_Obj as an int, wide or double. Error otherwise */
static TCL_RESULT parse_series_operand(Tcl_Interp *ip, Tcl_Obj *o, ta_value_t *ptav)
{
    if (ta_value_from_obj(NULL, o, TA_INT, ptav) == TCL_OK ||
        ta_value_from_obj(NULL, o, TA_WIDE, ptav) == TCL_OK ||
        ta_value_from_obj(ip, o, TA_DOUBLE, ptav) == TCL_OK)
        return TCL_OK;
    return TCL_ERROR;
}

/* Only converts specific type combinations needed by tcol_series_cmd! */
static void convert_series_operand(ta_value_t *ptav, unsigned char tatype) {
    if (ptav->type == tatype)
        return;
    switch (tatype) {
    case TA_WIDE:
        TA_ASSERT(ptav->type == TA_INT);
        ptav->wval = ptav->ival;
        break;
    case TA_DOUBLE:
        TA_ASSERT(ptav->type == TA_INT || ptav->type == TA_WIDE);
        if (ptav->type == TA_INT)
            ptav->dval = ptav->ival;
        else 
            ptav->dval = (double) ptav->wval;
        break;
    default:
        ta_type_panic(tatype);
    }
    ptav->type = tatype;
}

static thdr_t* init_double_series(Tcl_Interp *ip, double start, double limit, double step)
{
    thdr_t *thdr = NULL;
    int nmax;
    double   dbl, dmax, *pdbl;
    
    if (step == 0 ||
        step > 0 && start > limit ||
        step < 0 && start < limit) {
        ta_invalid_operand_error(ip, NULL);
        return NULL;
    }

    if (limit == start) 
        return thdr_alloc(ip, TA_DOUBLE, 0);
    
    /* Below can probably be condensed but it reflects my thought
       process regarding over/under flows
    */
    
    if (start >= 0 && limit >= 0 ||
        start < 0 && limit < 0) {
        /* Both have same sign, so limit-start cannot overflow */
        dmax = ceil((limit - start)/step);
        TA_ASSERT(ta_finite_double(dmax));
        TA_ASSERT(dmax >= 0); /* Since limit < start => step < 0 */
    } else {
        /* limit and start have different signs. limit-start may overflow
           so compute number of steps from 0 separately.
           Note the two cases below work out to be the same! But keeping
           them separate as it's clearer in my mind that way.
        */
        if (limit >= 0) {
            TA_ASSERT(start < 0);
            TA_ASSERT(step > 0); /* Since limit > start */
            dmax = ceil(limit / step);
            TA_ASSERT(ta_finite_double(dmax));
            TA_ASSERT(dmax >= 0); /* Since limit > 0, step > 0 */
            dbl = ceil(-start / step);
            TA_ASSERT(ta_finite_double(dbl));
            TA_ASSERT(dbl >= 0); /* Since start < 0, step > 0 */
            dmax += dbl;
        } else {
            TA_ASSERT(start >= 0);
            TA_ASSERT(step < 0); /* Since limit < start */
            dmax = ceil(limit / step);
            TA_ASSERT(ta_finite_double(dmax));
            TA_ASSERT(dmax >= 0); /* Since limit < 0, step < 0 */
            dbl = start / -step;
            TA_ASSERT(ta_finite_double(dbl));
            TA_ASSERT(dbl >= 0); /* Since start > 0, step < 0 */
            dmax += dbl;
        }
    }

    /* Note because of float rounding, dmax is only an estimate so in the
     * loop below we will reallocate if necessary. To reduce chances of 
     * reallocation, add some margin to nmax
     */
    if (dmax >= (INT_MAX-10))
        goto memory_limit_error;

    nmax = 10 + (int) dmax;
    
    thdr = thdr_alloc(ip, TA_DOUBLE, (int) nmax);
    if (thdr) {
        int i;
        thdr_t *thdr2;
        pdbl = THDRELEMPTR(thdr, double, 0);

        if (step > 0) {
            for (i = 0, dbl = start; dbl < limit; dbl += step, ++i) {
                if (i == nmax) {
                    if (nmax > (INT_MAX-10))
                        goto memory_limit_error;
                    nmax += 10; /* TBD - compute limit-double/step ? */
                    thdr2 = thdr_realloc(ip, thdr, nmax);
                    if (thdr2 == NULL)
                        goto error_exit;
                    thdr = thdr2;
                    pdbl = THDRELEMPTR(thdr, double, i);
                }
                *pdbl++ = dbl;
            }
        } else {
            for (i = 0, dbl = start; dbl > limit; dbl += step, ++i) {
                if (i == nmax) {
                    if (nmax > (INT_MAX-10))
                        goto memory_limit_error;
                    nmax += 10; /* TBD - compute limit-double/step ? */
                    thdr2 = thdr_realloc(ip, thdr, nmax);
                    if (thdr2 == NULL)
                        goto error_exit;
                    thdr = thdr2;
                    pdbl = THDRELEMPTR(thdr, double, i);
                }
                *pdbl++ = dbl;
            }
        }
        thdr->used = (pdbl - THDRELEMPTR(thdr, double, 0));
        TA_ASSERT(thdr->used <= nmax);
    }
    
    return thdr;

memory_limit_error:
    Tcl_SetResult(ip, "Request exceeds max size.", TCL_STATIC);
error_exit:
    if (thdr)
        thdr_free(thdr);
    return NULL;
}
    
static thdr_t* init_wide_series(Tcl_Interp *ip, Tcl_WideInt start, Tcl_WideInt limit, Tcl_WideInt step)
{
    thdr_t *thdr;
    Tcl_WideInt wide, *pwide;
    uint64_t nmax;

    if (step == 0 ||
        step > 0 && start > limit ||
        step < 0 && start < limit) {
        ta_invalid_operand_error(ip, NULL);
        return NULL;
    }

    if (limit == start) 
        return thdr_alloc(ip, TA_WIDE, 0);
    
    if (start >= 0 && limit >= 0 ||
        start < 0 && limit < 0) {
        /* Both have same sign, so limit-start cannot overflow */
        TA_ASSERT(((limit - start)/step) >= 0); /* limit < start => step < 0 */
        nmax = ((limit - start)/step) + 1;
    } else {
        /* limit and start have different signs. limit-start may overflow
           so compute number of steps from 0 separately as *unsigned* 
           numbers and check for overflow on the unsigned number.
           Note the two cases below work out to be the same! But keeping
           them separate as it's clearer in my mind that way.
        */
        if (limit >= 0) {
            TA_ASSERT(start < 0);
            TA_ASSERT(step > 0); /* Since limit > start */
            nmax = 1 + (limit / step);
            nmax += 1 + (-start / step);
        } else {
            TA_ASSERT(start >= 0);
            TA_ASSERT(step < 0); /* Since limit < start */
            nmax = 1 + (start / -step);
            nmax += 1 + (limit / step);
        }
        /* Note nmax may be an overestimate but no matter */
    }
    
    if (nmax >= INT_MAX) {
        ta_limit_error(ip, nmax);
        return NULL;
    }
    
    thdr = thdr_alloc(ip, TA_WIDE, (int) nmax);
    if (thdr) {
        pwide = THDRELEMPTR(thdr, Tcl_WideInt, 0);

        if (step > 0) {
            for (wide = start; wide < limit; wide += step)
                *pwide++ = wide;
        } else {
            for (wide = start; wide > limit; wide += step)
                *pwide++ = wide;
        }
        thdr->used = (pwide - THDRELEMPTR(thdr, Tcl_WideInt, 0));
        TA_ASSERT(thdr->used <= nmax);
    }
    
    return thdr;
}
    
static thdr_t* init_int_series(Tcl_Interp *ip, int start, int limit, int step)
{
    thdr_t *thdr;
    int i, *pi;
    Tcl_WideInt nmax;

    if (step == 0 ||
        step > 0 && start > limit ||
        step < 0 && start < limit) {
        ta_invalid_operand_error(ip, NULL);
        return NULL;
    }

    if (limit == start) 
        return thdr_alloc(ip, TA_INT, 0);
    
    /*
     * Need to be careful of overflows so just convert to wide and check.
     * Note because of checks above, nmax is +ve irrespective of
     * sign of step.
     */
    nmax = (((Tcl_WideInt) limit - (Tcl_WideInt) start) / step) + 1;
    TA_ASSERT(nmax > 0);
    if (nmax >= INT_MAX) {
        ta_limit_error(ip, nmax);
        return NULL;
    }
    
    thdr = thdr_alloc(ip, TA_INT, (int) nmax);
    if (thdr) {
        pi = THDRELEMPTR(thdr, int, 0);

        if (step > 0) {
            for (i = start; i < limit; i += step)
                *pi++ = i;
        } else {
            for (i = start; i > limit; i += step)
                *pi++ = i;
        }
        thdr->used = (pi - THDRELEMPTR(thdr, int, 0));
        TA_ASSERT(thdr->used == nmax || thdr->used == (nmax-1));
    }
    
    return thdr;
}

TCL_RESULT tcol_series_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    ta_value_t start, limit, step;
    TCL_RESULT status;
    thdr_t *thdr = NULL;
    
    if (objc < 2 || objc > 4) {
	Tcl_WrongNumArgs(ip, 1, objv, "?START? LIMIT ?STEP?");
	return TCL_ERROR;
    }

    /*
     * series LIMIT
     * series LIMIT STEP
     * series START LIMIT STEP
     */
    status = parse_series_operand(ip, objv[objc == 4 ? 2 : 1], &limit);
    if (status == TCL_ERROR)
        return status;
    if (objc == 2) {
        step.type = TA_INT;
        step.ival = 1;
        start.type = TA_INT;
        start.ival = 0;
    } else {
        status = parse_series_operand(ip, objv[objc == 4 ? 3 : 2], &step);
        if (status == TCL_ERROR)
            return status;
        if (objc == 3) {
            start.type = TA_INT;
            start.ival = 0;
        } else {
            status = parse_series_operand(ip, objv[1], &start);
            if (status == TCL_ERROR)
                return status;
        }
    }
    
    /* Figure out the type of the result column. */
    if (start.type == TA_DOUBLE || limit.type == TA_DOUBLE || step.type == TA_DOUBLE) {
        convert_series_operand(&start, TA_DOUBLE);
        convert_series_operand(&limit, TA_DOUBLE);
        convert_series_operand(&step, TA_DOUBLE);
        thdr = init_double_series(ip, start.dval, limit.dval, step.dval);
    } 
    else if (start.type == TA_WIDE || limit.type == TA_WIDE || step.type == TA_WIDE) {
        convert_series_operand(&start, TA_WIDE);
        convert_series_operand(&limit, TA_WIDE);
        convert_series_operand(&step, TA_WIDE);
        thdr = init_wide_series(ip, start.wval, limit.wval, step.wval);
    } 
    else {
        TA_ASSERT(start.type == TA_INT);
        TA_ASSERT(limit.type == TA_INT);
        TA_ASSERT(step.type == TA_INT);
        thdr = init_int_series(ip, start.ival, limit.ival, step.ival);
    } 
            
    /* TBD - what if start==limit? Return single value or error or empty? */

    if (thdr) {
        TA_ASSERT(thdr->used <= thdr->usable);
        Tcl_SetObjResult(ip, tcol_new(thdr));
        return TCL_OK;
    } else
        return TCL_ERROR;
}
