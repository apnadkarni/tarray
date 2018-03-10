/*
 * Copyright (c) 2015-2016, Ashok P. Nadkarni
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
    "+", "-", "*", "/",
    "&&", "||", "^^",
    "&", "|", "^",
    "==", "!=", "<", "<=", ">", ">=",
    NULL
};
enum ta_math_op_e {
    TAM_OP_PLUS, TAM_OP_MINUS, TAM_OP_MUL, TAM_OP_DIV,
    TAM_OP_AND, TAM_OP_OR, TAM_OP_XOR,
    TAM_OP_BITAND, TAM_OP_BITOR, TAM_OP_BITXOR,
    TAM_OP_EQ, TAM_OP_NE, TAM_OP_LT, TAM_OP_LE, TAM_OP_GT, TAM_OP_GE,
};

static int is_logical_op(enum ta_math_op_e op)
{
    return (op == TAM_OP_AND || op == TAM_OP_OR || op == TAM_OP_XOR);
}

static int is_bit_op(enum ta_math_op_e op)
{
    return (op == TAM_OP_BITAND || op == TAM_OP_BITOR || op == TAM_OP_BITXOR);
}

static int is_compare_op(enum ta_math_op_e op)
{
    switch (op) {
    case TAM_OP_EQ: case TAM_OP_NE: case TAM_OP_LT:
    case TAM_OP_LE: case TAM_OP_GT: case TAM_OP_GE:
        return 1;
    default:
        return 0;
    }
}

struct ta_math_operand {
    thdr_t *thdr_operand;
    ta_value_t scalar_operand; /* Only valid if thdr_operand is NULL */
    Tcl_Obj *obj;              /* Original Tcl_Obj for operand. Only
                                  valid if thdr_operand is NULL */
    int     span_start; /* Where in thdr_operand the logical column starts.
                           Only valid if thdr_operand is not NULL */
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

/* NOTE: will panic for non-numeric values */
static double ta_math_double_from_operand(struct ta_math_operand *poperand,
                                          int thdr_index)
{
    thdr_t *thdr = poperand->thdr_operand;
    if (poperand->thdr_operand) {
        return thdr_index_double(poperand->thdr_operand,
                                 thdr_index + poperand->span_start);
    } else {
        TA_ASSERT(poperand->scalar_operand.type == TA_DOUBLE || poperand->scalar_operand.type == TA_WIDE);
        if (poperand->scalar_operand.type == TA_DOUBLE)
            return poperand->scalar_operand.dval;
        else
            return (double) poperand->scalar_operand.wval;
    }
}

/* NOTE: will panic for non-numeric values */
static Tcl_WideInt ta_math_wide_from_operand(struct ta_math_operand *poperand,
                                          int thdr_index)
{
    if (poperand->thdr_operand) {
        return thdr_index_wide(poperand->thdr_operand, 
                               thdr_index + poperand->span_start);
    } else {
        /* All scalar non-double operands must have been promoted to wide */
        TA_ASSERT(poperand->scalar_operand.type == TA_WIDE);
        return poperand->scalar_operand.wval;
    }
}

static char *ta_math_string_from_operand(struct ta_math_operand *poperand,
                                         int thdr_index, char buf[40])
{
    if (poperand->thdr_operand) {
        return thdr_index_string(poperand->thdr_operand, 
                                 thdr_index + poperand->span_start,
                                 buf);
    } else {
        TA_ASSERT(poperand->obj != NULL);
        return Tcl_GetString(poperand->obj);
    }
}

static double ta_math_double_operation(enum ta_math_op_e op, double dbl, ta_value_t *poperand)
{
    double operand;

    TA_ASSERT(! is_bit_op(op));
    TA_ASSERT(poperand->type == TA_DOUBLE || poperand->type == TA_WIDE);

    if (poperand->type == TA_DOUBLE)
        operand = poperand->dval;
    else
        operand = (double) poperand->wval; /* TBD - overflow check? */

    switch (op) {
    case TAM_OP_PLUS: return dbl + operand;
    case TAM_OP_MINUS: return dbl - operand;
    case TAM_OP_MUL: return dbl * operand;
    case TAM_OP_DIV: return dbl / operand; /* Check for div-by-0 ? */
    case TAM_OP_AND: return dbl && operand;
    case TAM_OP_OR: return dbl || operand;
    case TAM_OP_XOR:
        if ((dbl && operand) ||
            (dbl == 0 && operand == 0))
            return 0;
        else 
            return 1;
    case TAM_OP_EQ: return dbl == operand;
    case TAM_OP_NE: return dbl != operand;
    case TAM_OP_LT: return dbl < operand;
    case TAM_OP_LE: return dbl <= operand;
    case TAM_OP_GT: return dbl > operand;
    case TAM_OP_GE: return dbl >= operand;
    case TAM_OP_BITAND: /* Keep gcc happy */
    case TAM_OP_BITOR:  /* ditto */
    case TAM_OP_BITXOR: /* ditto */
        Tcl_Panic("Invalid math double operand");
    }
    return 0.0;                 /* To keep compiler happy */
}    

static Tcl_WideInt ta_math_wide_operation(enum ta_math_op_e op, Tcl_WideInt wide, Tcl_WideInt operand)
{
    Tcl_WideInt result;
    switch (op) {
    case TAM_OP_PLUS: return wide + operand;
    case TAM_OP_MINUS: return wide - operand;
    case TAM_OP_MUL: return wide * operand;
    case TAM_OP_DIV:
        TA_ASSERT(operand != 0); /* Caller should have ensured */
        /* See INST_DIV in tclExecute.c in Tcl sources */
        result = wide / operand;
        if (((result < 0) || ((result == 0) &&
                              ((wide < 0 && operand > 0) ||
                               (wide > 0 && operand < 0)))) &&
            ((result * operand) != wide)) {
            result -= 1;
        }
        return result;
    case TAM_OP_AND: return wide && operand;
    case TAM_OP_OR: return wide || operand;
    case TAM_OP_XOR:
        if ((wide && operand) ||
            (wide == 0 && operand == 0))
            return 0;
        else 
            return 1;
    case TAM_OP_EQ: return wide == operand;
    case TAM_OP_NE: return wide != operand;
    case TAM_OP_LT: return wide < operand;
    case TAM_OP_LE: return wide <= operand;
    case TAM_OP_GT: return wide > operand;
    case TAM_OP_GE: return wide >= operand;
    case TAM_OP_BITOR: return wide | operand;
    case TAM_OP_BITAND: return wide & operand;
    case TAM_OP_BITXOR: return wide ^ operand;
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
    TA_ASSERT(is_bit_op(op) || is_logical_op(op));

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
            if (is_bit_op(op)) {
                /* 
                 * Note bitwise operations involving scalars only come into
                 * this function when the scalar is a string boolean (true)
                 * and all columns are booleans. The scalar is passed then
                 * as a wide (as all integer values are).
                 * Else they would go into the mainstream worker code which
                 * handles wider types.
                 */
                TA_ASSERT(poper->scalar_operand.type == TA_WIDE);
                ival = (poper->scalar_operand.wval & 1);
            }
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
        if (op == TAM_OP_AND || need_complement) 
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

TCL_RESULT ta_math_compare_operation(
    Tcl_Interp *ip, int op, int promoted_type,
    struct ta_math_operand *poperands, /* At least one column */
    int noperands,
    int size)
{
    int i, j;
    thdr_t *thdr;
    ba_t *baP;
    TCL_RESULT status = TCL_OK;

    TA_ASSERT(is_compare_op(op));
    thdr = thdr_alloc(ip, TA_BOOLEAN, size);
    if (thdr == NULL)
        return TCL_ERROR;
    baP = THDRELEMPTR(thdr, ba_t, 0);
    
    if (size == 0)
        goto done;
    
    if (noperands <= 1) {
        ba_fill(baP, 0, size, 1);
        goto done;
    }

    if (promoted_type == TA_ANY) {
        for (i = 0; i < size; ++i) {
            int result = 1;
            for (j = 0 ; j < (noperands-1); ++j) {
                char *s1, *s2;
                char buf1[40], buf2[40];
                int comparison;
                s1 = ta_math_string_from_operand(&poperands[j], i, buf1);
                s2 = ta_math_string_from_operand(&poperands[j+1], i, buf2);
                comparison = ta_utf8_compare(s1, s2, 0);
                switch (op) {
                case TAM_OP_EQ: result = (comparison == 0); break;
                case TAM_OP_NE: result = (comparison != 0); break;
                case TAM_OP_LT: result = (comparison < 0); break;
                case TAM_OP_LE: result = (comparison <= 0); break;
                case TAM_OP_GT: result = (comparison > 0); break;
                case TAM_OP_GE: result = (comparison >= 0); break;
                }
                if (result == 0)
                    break;
            }
            ba_put(baP, i, result);
        }
    } else if (promoted_type == TA_DOUBLE) {
        for (i = 0; i < size; ++i) {
            int result = 1;
            for (j = 0; j < (noperands-1); ++j) {
                double dbl1, dbl2;
                dbl1 = ta_math_double_from_operand(&poperands[j], i);
                dbl2 = ta_math_double_from_operand(&poperands[j+1], i);
                switch (op) {
                case TAM_OP_EQ: result = (dbl1 == dbl2); break;
                case TAM_OP_NE: result = (dbl1 != dbl2); break;
                case TAM_OP_LT: result = (dbl1 < dbl2); break;
                case TAM_OP_LE: result = (dbl1 <= dbl2); break;
                case TAM_OP_GT: result = (dbl1 > dbl2); break;
                case TAM_OP_GE: result = (dbl1 >= dbl2); break;
                }
                if (result == 0)
                    break;
            }
            ba_put(baP, i, result);
        }
    } else {
        for (i = 0; i < size; ++i) {
            int result = 1;
            for (j = 0; j < (noperands-1); ++j) {
                Tcl_WideInt wide1, wide2;
                wide1 = ta_math_wide_from_operand(&poperands[j], i);
                wide2 = ta_math_wide_from_operand(&poperands[j+1], i);
                switch (op) {
                case TAM_OP_EQ: result = (wide1 == wide2); break;
                case TAM_OP_NE: result = (wide1 != wide2); break;
                case TAM_OP_LT: result = (wide1 < wide2); break;
                case TAM_OP_LE: result = (wide1 <= wide2); break;
                case TAM_OP_GT: result = (wide1 > wide2); break;
                case TAM_OP_GE: result = (wide1 >= wide2); break;
                }
                if (result == 0)
                    break;
            }
            ba_put(baP, i, result);
        }
    }

done:
    thdr->used = size;
    Tcl_SetObjResult(ip, tcol_new(thdr));
    return TCL_OK;
}

TCL_RESULT ta_math_scalar_operation(
    Tcl_Interp *ip, int op, int promoted_type,
    struct ta_math_operand *poperands, /* Must be all scalars */
    int noperands)
{
    int j;
    TCL_RESULT status = TCL_OK;

    TA_ASSERT(promoted_type != TA_STRING);
    if (is_compare_op(op)) {
        int result = 1;

        /* Note: For 0/1 operands, we return 1 just like Tcl */
        if (noperands > 1) {
            if (promoted_type == TA_ANY) {
                for (j = 0; j < (noperands-1); ++j) {
                    Tcl_Obj *obj1 = poperands[j].obj;
                    Tcl_Obj *obj2 = poperands[j+1].obj;
                    int nocase = 0; /* TBD - support case-insensitivity later */
                    TA_ASSERT(obj1 != NULL);
                    TA_ASSERT(obj2 != NULL);
                    switch (op) {
                    case TAM_OP_EQ:
                        result = ta_obj_equal(obj1, obj2, nocase);
                        break;
                    case TAM_OP_NE:
                        result = !ta_obj_equal(obj1, obj2, nocase);
                        break;
                    case TAM_OP_LT:
                        result = ta_obj_compare(obj1, obj2, nocase) < 0;
                        break;
                    case TAM_OP_LE:
                        result = ta_obj_compare(obj1, obj2, nocase) <= 0;
                        break;
                    case TAM_OP_GT:
                        result = ta_obj_compare(obj1, obj2, nocase) > 0;
                        break;
                    case TAM_OP_GE:
                        result = ta_obj_compare(obj1, obj2, nocase) >= 0;
                        break;
                    }
                    if (result == 0)
                        break;
                }
            } else if (promoted_type == TA_DOUBLE) {
                for (j = 0; j < (noperands-1); ++j) {
                    double dbl1, dbl2;
                    TA_ASSERT(poperands[j].scalar_operand.type == TA_DOUBLE || poperands[j].scalar_operand.type == TA_WIDE);
                    TA_ASSERT(poperands[j+1].scalar_operand.type == TA_DOUBLE || poperands[j+1].scalar_operand.type == TA_WIDE);
                    if (poperands[j].scalar_operand.type == TA_DOUBLE)
                        dbl1 = poperands[j].scalar_operand.dval;
                    else
                        dbl1 = (double) poperands[j].scalar_operand.wval;
                    if (poperands[j+1].scalar_operand.type == TA_DOUBLE)
                        dbl2 = poperands[j+1].scalar_operand.dval;
                    else
                        dbl2 = (double) poperands[j+1].scalar_operand.wval;
                    switch (op) {
                    case TAM_OP_EQ: result = (dbl1 == dbl2); break;
                    case TAM_OP_NE: result = (dbl1 != dbl2); break;
                    case TAM_OP_LT: result = (dbl1 < dbl2); break;
                    case TAM_OP_LE: result = (dbl1 <= dbl2); break;
                    case TAM_OP_GT: result = (dbl1 > dbl2); break;
                    case TAM_OP_GE: result = (dbl1 >= dbl2); break;
                    }
                    if (result == 0)
                        break;
                }
            } else {
                for (j = 0; j < (noperands-1); ++j) {
                    Tcl_WideInt wide1, wide2;
                    TA_ASSERT(poperands[j].scalar_operand.type == TA_WIDE);
                    TA_ASSERT(poperands[j+1].scalar_operand.type == TA_WIDE);
                    wide1 = poperands[j].scalar_operand.wval;
                    wide2 = poperands[j+1].scalar_operand.wval;
                    switch (op) {
                    case TAM_OP_EQ: result = (wide1 == wide2); break;
                    case TAM_OP_NE: result = (wide1 != wide2); break;
                    case TAM_OP_LT: result = (wide1 < wide2); break;
                    case TAM_OP_LE: result = (wide1 <= wide2); break;
                    case TAM_OP_GT: result = (wide1 > wide2); break;
                    case TAM_OP_GE: result = (wide1 >= wide2); break;
                    }
                    if (result == 0)
                        break;
                }
            }
        }
        Tcl_SetObjResult(ip, Tcl_NewIntObj(result));
        
    } else if (promoted_type == TA_DOUBLE) {
        /* Operations on doubles other than logical operations */

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
    } else if (promoted_type != TA_ANY) {
        /* Operations on non-doubles other than logical operations */

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
    } else {
        ta_invalid_op_for_type(ip, TA_ANY);
    }

vamoose:
    return status;
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

    if (noperands != 2 && op == TAM_OP_NE) {
        return ta_invalid_argcount(ip);
    }

    /* Common case, save on allocation. */
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
                if (result_type != TA_DOUBLE && result_type != TA_WIDE && result_type != TA_ANY)
                    result_type = TA_UINT;
                break;
            case TA_WIDE:
                if (result_type != TA_DOUBLE && result_type != TA_ANY)
                    result_type = TA_WIDE;
                break;
            case TA_DOUBLE:
                if (result_type != TA_ANY)
                    result_type = TA_DOUBLE;
                break;
            case TA_STRING: /* FALLTHRU */
            case TA_ANY:
                if (! is_compare_op(op)) {
                    status = ta_invalid_op_for_type(ip, coltype);
                    goto vamoose;
                }
                result_type = TA_ANY;
                break;
            default:
                ta_type_panic(coltype);
            }
            poperands[i].thdr_operand = tcol_thdr(tcol);
            span = tcol_span(tcol);
            if (span) {
                poperands[i].span_start = span->first;
            } else {
                poperands[i].span_start = 0;
            }
            only_scalars = 0;
            poperands[i].obj = NULL; /* Only used for scalars */
        } else {
            /* Scalar operand. Check if an integer, wide or double.
               If we have already seen a boolean column, do NOT change
               the expected result type.
             */
            ta_value_t *ptav = &poperands[i].scalar_operand;
            /* Avoid shimmering tables. */
            if (table_affirm(objv[j])) {
                status = ta_invalid_op_for_table(ip);
                goto vamoose;
            }
            if (Tcl_GetWideIntFromObj(NULL, objv[j], &ptav->wval) == TCL_OK) {
                /* Note integers are also stored as wides during computation */
                ptav->type = TA_WIDE;

                if (result_type != TA_DOUBLE 
                    && result_type != TA_WIDE && result_type != TA_ANY) {
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
                /* Not a integer */
                status = Tcl_GetDoubleFromObj(NULL, objv[j], &ptav->dval);
                if (status == TCL_OK) {
                    ptav->type = TA_DOUBLE;
                    if (result_type != TA_ANY)
                        result_type = TA_DOUBLE;
                } else {
                    /* Scalar is not a numeric. See if it might be a boolean
                       (true or false). These are NOT treated as numeric
                       and thus result_type is set to TA_BOOLEAN.
                    */
                    int ival;
                    status = Tcl_GetBooleanFromObj(NULL, objv[j], &ival);
                    if (status == TCL_OK && is_logical_op(op)) {
                        if (result_type == TA_NONE)
                            result_type = TA_BOOLEAN;
                        /* All non-doubles are stored as wides */
                        ptav->type = TA_WIDE;
                        ptav->wval = ival;
                    } else {
                        if (!is_compare_op(op)) {
                            status = ta_invalid_op_for_type(ip, TA_ANY);
                            goto vamoose;
                        }
                        result_type = TA_ANY;
                        ptav->type = TA_ANY;
                        ptav->oval = objv[j];
                    }
                }
            }
            poperands[i].thdr_operand = NULL; /* Indicate scalar_operand is valid */
            poperands[i].obj = objv[j];
        }
    }
    
    TA_ASSERT(result_type != TA_NONE && result_type != TA_STRING);
    
    if (result_type == TA_DOUBLE && is_bit_op(op)) {
        status = ta_invalid_op_for_type(ip, TA_DOUBLE);
        goto vamoose;
    }

    /* If we are only passed scalars, compute and return the result */
    if (only_scalars) {
        status = ta_math_scalar_operation(ip, op, result_type, poperands, noperands);
        goto vamoose;           /* yea yea gotos are bad */
    }

    if (is_compare_op(op)) {
        status = ta_math_compare_operation(ip, op, result_type, poperands, noperands, thdr_size);
        goto vamoose;
    }

    TA_ASSERT(result_type != TA_ANY && result_type != TA_STRING);
        
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

/*
 * Compares two columns for equality. They are equal if every element value is
 * equal. If strict is true, column types must also be the same, else
 * comparison does conversion if required.
 * Returns 1 if equal, 0 if unequal.
 * Caller should have verified cola and colb are columns.
 */
int tcol_equality_test(Tcl_Interp *ip, Tcl_Obj *cola, Tcl_Obj *colb, int strict)
{
    thdr_t *thdra, *thdrb;
    span_t *spana, *spanb;
    int counta, countb, starta, startb;

    if (cola == colb)
        return 1;

    TA_NOFAIL(tcol_convert(ip, cola), 1);
    TA_NOFAIL(tcol_convert(ip, colb), 1);

    thdra = OBJTHDR(cola);
    spana = OBJTHDRSPAN(cola);
    starta = thdr_start_and_count(thdra, spana, &counta);

    thdrb = OBJTHDR(colb);
    spanb = OBJTHDRSPAN(colb);
    startb = thdr_start_and_count(thdrb, spanb, &countb);

    if (counta != countb)
        return 0;

    /* Check if thdrs are same AND spans also point to same area */
    if (thdra == thdrb && starta == startb) {
        return 1;
    }

#define CMPLOOP_(type_)                         \
    do {                                        \
        type_ *a, *b;                           \
        int i;                                  \
        a = THDRELEMPTR(thdra, type_, starta);  \
        b = THDRELEMPTR(thdrb, type_, startb);  \
        for (i = 0; i < counta; ++i) {          \
            if (a[i] != b[i])                   \
                return 0;                       \
        }                                       \
        return 1;                               \
    } while (0)

    if (thdra->type == thdrb->type) {
        if (counta == 0)
            return 1;           /* Both empty columns of same type */
        switch (thdra->type) {
        case TA_BOOLEAN:
            return ba_equal(THDRELEMPTR(thdra, ba_t, 0), starta,
                            THDRELEMPTR(thdrb, ba_t, 0), startb,
                            counta);
        case TA_BYTE: CMPLOOP_(unsigned char);
        case TA_INT: CMPLOOP_(int);
        case TA_UINT: CMPLOOP_(unsigned int);
        case TA_WIDE: CMPLOOP_(Tcl_WideInt);
        case TA_DOUBLE: CMPLOOP_(double);
        case TA_STRING:
            {
                int i;
                tas_t **a, **b;
                a = THDRELEMPTR(thdra, tas_t *, starta);
                b = THDRELEMPTR(thdrb, tas_t *, startb);
                for (i = 0; i < counta; ++i) {
                    if (! tas_equal(a[i], b[i], 0))
                        return 0;
                }
                return 1;
            }

        case TA_ANY:
            {
                int i;
                Tcl_Obj **a, **b;
                a = THDRELEMPTR(thdra, Tcl_Obj *, starta);
                b = THDRELEMPTR(thdrb, Tcl_Obj *, startb);
                for (i = 0; i < counta; ++i) {
                    if (! ta_obj_equal(a[i], b[i], 0))
                        return 0;
                }
                return 1;
            }
        }
    }

    if (strict)
        return 0;               /* Different types */

    if (counta == 0)
        return 1;           /* Both empty columns */

    /* 
     * Types diff so we will have to promote to a common type.
     */
    if (thdra->type == TA_ANY || thdra->type == TA_STRING ||
        thdrb->type == TA_ANY || thdrb->type == TA_STRING) {
        char bufa[40], bufb[40]; /* Enough to hold string rep of wides */
        char *sa, *sb;
        int i;
        
        for (i = 0; i < counta; ++i) {
            sa = thdr_index_string(thdra, starta+i, bufa);
            sb = thdr_index_string(thdrb, startb+i, bufb);
            if (! ta_utf8_equal(sa, sb, 0))
                return 0;
        }
    } 
    else if (thdra->type == TA_DOUBLE || thdrb->type == TA_DOUBLE) {
        double a, b;
        int i;
        for (i = 0; i < counta; ++i) {
            a = thdr_index_double(thdra, starta+i);
            b = thdr_index_double(thdrb, startb+i);
            if (a != b)
                return 0;
        }
    }
    else {
        Tcl_WideInt a, b;
        int i;
        for (i = 0; i < counta; ++i) {
            a = thdr_index_wide(thdra, starta+i);
            b = thdr_index_wide(thdrb, startb+i);
            if (a != b)
                return 0;
        }
    }    

    return 1;
}
