/*
 * Copyright (c) 2024, Ashok P. Nadkarni
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
Tcl_Size ta_fold_mt_threshold = TA_MT_THRESHOLD_DEFAULT;
#endif

static const char *ta_fold_op_names[] = {
    "+", NULL
};
enum ta_fold_op_e {
    TAF_OP_SUM,
};

struct thdr_fold_mt_context {
    ta_value_t accumulator;     /* Holds result of folding */
    thdr_t  *thdr;              /* thdr to be folded */
    Tcl_Size start;             /* Starting position in source thdr
                                   for this thread */
    Tcl_Size count;             /* Number of elements in thdr to examine */
    TCL_RESULT status;             /* Status of thread */
    enum ta_fold_op_e op;                /* Operation */
};


static void thdr_fold_mt_worker(void *pv)
{
    struct thdr_fold_mt_context *pctx = pv;
    double daccumulator, *dstart, *dend;

#define INTEGERLOOP(type_)                                   \
    do {                                                     \
        type_ *start, *end;                                  \
        Tcl_WideInt accum;                                   \
        start = THDRELEMPTR(pctx->thdr, type_, pctx->start); \
        end   = start + pctx->count;                         \
        accum = 0;                                           \
        while (start < end)                                  \
            accum += *start++;                               \
        pctx->accumulator.wval = accum;                      \
    } while (0)

    switch (pctx->op) {
    case TAF_OP_SUM:
        switch (pctx->thdr->type) {
        case TA_BYTE: INTEGERLOOP(unsigned char) ; break;
        case TA_INT: INTEGERLOOP(int) ; break;
        case TA_UINT: INTEGERLOOP(unsigned int) ; break;
        case TA_WIDE: INTEGERLOOP(Tcl_WideInt) ; break;
        case TA_DOUBLE:
            dstart = THDRELEMPTR(pctx->thdr, double, pctx->start);
            dend   = dstart + pctx->count;
            daccumulator = 0.0;
            while (dstart < dend)
                daccumulator += *dstart++;
            pctx->accumulator.dval = daccumulator;
            break;
        default:
            pctx->status = TCL_ERROR; /* Should never happen */
        }
        break;
    default:
        pctx->status = TCL_ERROR; /* Should never happen if options checked */
        break;
    }
}

TCL_RESULT tcol_fold_cmd(ClientData clientdata, Tcl_Interp *ip,
                         int objc, Tcl_Obj *const objv[])
{
    int j, ncontexts;
    thdr_t *thdr;
    struct thdr_fold_mt_context mt_context[4];
    Tcl_Size mt_sizes[4];
    Tcl_Size start, nelems;
    int op;
    TCL_RESULT status;
    span_t *span;

    if (objc < 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "operation tarray");
	return TCL_ERROR;
    }

    if ((status = ta_opt_from_obj(ip, objv[1], ta_fold_op_names, "operation", 0, &op)) != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, objv[2])) != TCL_OK)
        return status;

    thdr = OBJTHDR(objv[2]);

    switch (thdr->type) {
    case TA_BYTE:
    case TA_INT:
    case TA_UINT:
    case TA_WIDE:
    case TA_DOUBLE:
        break;
    default:
        return ta_bad_type_error(ip, thdr);
    }

    span = OBJTHDRSPAN(objv[2]);
    if (span) {
        start = span->first;
        nelems = span->count;
    } else {
        start = 0;
        nelems = thdr->used;
    }

#if !defined(TA_MT_ENABLE)
    ncontexts = 1;
    mt_sizes[0] = nelems;
#else
    ncontexts = thdr_calc_mt_split_ex(thdr->type, start, nelems,
                                      ta_fold_mt_threshold,
                                      ARRAYSIZE(mt_sizes), mt_sizes);
#   if defined(TA_ENABLE_ASSERT)
    {
        int total = 0;
        for (j = 0; j < ncontexts; ++j) {
            total += mt_sizes[j];
        }
        TA_ASSERT(total == nelems);
    }
#   endif

#endif

    for (j = 0; j < ncontexts; ++j) {
        mt_context[j].thdr = thdr;
        if (thdr->type == TA_DOUBLE) {
            mt_context[j].accumulator.type = TA_DOUBLE;
            mt_context[j].accumulator.dval = 0.0;
        } else {
            mt_context[j].accumulator.type = TA_WIDE;
            mt_context[j].accumulator.wval = 0;
        }

        mt_context[j].status = TCL_OK;
        mt_context[j].op = op;
        mt_context[j].start = start;
        mt_context[j].count = mt_sizes[j];
        start += mt_sizes[j];
    }

    if (ncontexts == 1) {
        thdr_fold_mt_worker(&mt_context[0]);
    }
#if defined(TA_MT_ENABLE)
    else {
        ta_mt_group_t grp;

        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* Fire off other threads. Context 0 we will handle ourselves */
        for (j = 1; j < ncontexts; ++j) {
            /* TBD - check return code */
            ta_mt_group_async_f(grp, &mt_context[j], thdr_fold_mt_worker);
        }
        thdr_fold_mt_worker(&mt_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);
    }
#endif /* TA_MT_ENABLE */

    /* First verify all threads ran successfully */
    status = TCL_OK;
    for (j = 0; j < ncontexts; ++j) {
       if (mt_context[j].status != TCL_OK) {
           Tcl_SetResult(ip, "Error during fold operation", TCL_STATIC);
           status = TCL_ERROR;
           break;
        }
    }

    if (status == TCL_OK) {
        if (mt_context[0].accumulator.type == TA_DOUBLE) {
            double dval = mt_context[0].accumulator.dval;
            for (j = 1; j < ncontexts; ++j) {
                dval += mt_context[j].accumulator.dval;
            }
            Tcl_SetObjResult(ip, Tcl_NewDoubleObj(dval));
        } else {
            Tcl_WideInt wval = mt_context[0].accumulator.wval;
            for (j = 1; j < ncontexts; ++j) {
                wval += mt_context[j].accumulator.wval;
            }
            Tcl_SetObjResult(ip, Tcl_NewWideIntObj(wval));
        }
    }

    return status;
}
