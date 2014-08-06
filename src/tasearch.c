/*
 * Copyright (c) 2012-2014, Ashok P. Nadkarni
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
int ta_search_mt_threshold = TA_MT_THRESHOLD_DEFAULT;
#endif

/*
 * Options for 'tarray search'
 */
static const char *ta_search_switches_e[] = {
    "-all", "-inline", "-not", "-range", "-eq", "-gt", "-lt", "-pat", "-re", "-nocase", "-among", NULL
};
enum ta_search_switches_e {
    TA_SEARCH_OPT_ALL, TA_SEARCH_OPT_INLINE, TA_SEARCH_OPT_INVERT, TA_SEARCH_OPT_RANGE, TA_SEARCH_OPT_EQ, TA_SEARCH_OPT_GT, TA_SEARCH_OPT_LT, TA_SEARCH_OPT_PAT, TA_SEARCH_OPT_RE, TA_SEARCH_OPT_NOCASE, TA_SEARCH_OPT_AMONG
};
/* Search flags */
#define TA_SEARCH_INLINE 1  /* Return values, not indices */
#define TA_SEARCH_INVERT 2  /* Invert matching expression */
#define TA_SEARCH_ALL    4  /* Return all matches */
#define TA_SEARCH_NOCASE 8  /* Ignore case */

/* Contains search criteria */
typedef struct ta_search_s {
    thdr_t *indices;            /* List of indices to examine, may be NULL */
    int lower;                  /* Start of range to search */
    int upper;                  /* End of range to search */
    int flags;                  /* Search attributes */
    int cur;                    /* Cur position to check. If indices is NULL,
                                   this is the index into the haystack. If
                                   indices is not-NULL, this is the index
                                   into indices
                                */
    enum ta_search_switches_e op;    /* Search operation */
} ta_search_t;

TCL_RESULT ta_search_op_error(Tcl_Interp *ip, int op)
{
    if (ip) {
        const char *ops = NULL;
        if (op < (sizeof(ta_search_switches_e)/sizeof(ta_search_switches_e[0])))
            ops = ta_search_switches_e[op];
        if (ops == NULL)
            Tcl_SetObjResult(ip, Tcl_ObjPrintf("Unknown or invalid search operator (%d).", op));
        else
            Tcl_SetObjResult(ip, Tcl_ObjPrintf("Unknown or invalid search operator (%s).", ops));
        Tcl_SetErrorCode(ip, "TARRAY", "SEARCH", "OPER", NULL);
    }
    return TCL_ERROR;
}

/* Helper to figure out next slot to test when search indices are specified */
static int ta_search_calc_slot_indices(ta_search_t *psearch)
{
    int slot;

    TA_ASSERT(psearch->indices != NULL);
    while (psearch->cur < psearch->indices->used) {
        slot = *THDRELEMPTR(psearch->indices, int, psearch->cur);
        if (slot >= psearch->lower && slot <= psearch->upper) {
            /* This index within range. Loop to try next */
            return slot;
        }
        /* Not in range, try next */
        psearch->cur += 1;
    }
    return -1;                  /* No more slots to check */
}

/* Helper to figure out next slot to test */
static TA_INLINE int ta_search_calc_slot(ta_search_t *psearch)
{
    if (psearch->indices)
        return ta_search_calc_slot_indices(psearch);
    else {
        /* When indices is NULL, cur itself is slot */
        return psearch->cur <= psearch->upper ?  psearch->cur : -1;
    }
}

static TCL_RESULT thdr_search_boolean(Tcl_Interp *ip, thdr_t * haystackP,
                                      Tcl_Obj *needleObj, ta_search_t *psearch)
{
    int bval;
    ba_t *baP;
    int pos;
    Tcl_Obj *oresult;
    thdr_t *thdr;
    thdr_t *newP;

    TA_ASSERT(haystackP->type == TA_BOOLEAN);

    if (psearch->op != TA_SEARCH_OPT_EQ)
        return ta_search_op_error(ip, psearch->op);

    if (Tcl_GetBooleanFromObj(ip, needleObj, &bval) != TCL_OK)
        return TCL_ERROR;
    
    if (psearch->flags & TA_SEARCH_INVERT)
        bval = !bval;

    baP = THDRELEMPTR(haystackP, ba_t, 0);

    if (psearch->indices == NULL) {
        /* Search a range */
        if (psearch->flags & TA_SEARCH_ALL) {
            thdr = thdr_alloc(ip, 
                              psearch->flags & TA_SEARCH_INLINE ? TA_BOOLEAN : TA_INT,
                              10);                /* Assume 10 hits */
            if (thdr == NULL)
                return TCL_ERROR;
            pos = psearch->lower;
            while ((pos = ba_find(baP, bval, pos, haystackP->used)) != -1
                   && pos <= psearch->upper) {
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->usable) {
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                    if (newP)
                        thdr = newP;
                    else {
                        thdr_decr_refs(thdr);
                        return TCL_ERROR;
                    }
                }
                if (psearch->flags & TA_SEARCH_INLINE)
                    ba_put(THDRELEMPTR(thdr, ba_t, 0), thdr->used, bval);
                else
                    *THDRELEMPTR(thdr, int, thdr->used) = pos;
                thdr->used++;
                ++pos;
            }
            if ((psearch->flags & TA_SEARCH_INLINE) == 0)
                thdr->sort_order = THDR_SORTED_ASCENDING; /* indices are naturally sorted */
            oresult = tcol_new(thdr);
        } else {
            /* Return first found element */
            pos = ba_find(baP, bval, psearch->lower, haystackP->used);
            if (pos > psearch->upper)
                pos = -1;
            if (psearch->flags & TA_SEARCH_INLINE)
                oresult = pos == -1 ? Tcl_NewObj() : Tcl_NewIntObj(bval);
            else
                oresult = Tcl_NewIntObj(pos);
        }
    } else {
        /* We have to look only in specific position given by indices */

        if (psearch->flags & TA_SEARCH_ALL) {

            thdr = thdr_alloc(ip, 
                              psearch->flags & TA_SEARCH_INLINE ? TA_BOOLEAN : TA_INT,
                              10);                /* Assume 10 hits */
            if (thdr == NULL)
                return TCL_ERROR;

            while (1) {
                pos = ta_search_calc_slot_indices(psearch);
                if (pos == -1)
                    break;

                TA_ASSERT(pos < haystackP->used);
                if (ba_get(baP, pos) == bval) {
                    /* Ensure enough space in target array */
                    if (thdr->used >= thdr->usable) {
                        newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                        if (newP)
                            thdr = newP;
                        else {
                            thdr_decr_refs(thdr);
                            return TCL_ERROR;
                        }
                    }
                    if (psearch->flags & TA_SEARCH_INLINE)
                        ba_put(THDRELEMPTR(thdr, ba_t, 0), thdr->used, bval);
                    else
                        *THDRELEMPTR(thdr, int, thdr->used) = pos;
                    thdr->used++;
                }
                psearch->cur += 1;  /* Next index to check */
            }

            oresult = tcol_new(thdr);

        } else {
            /* Return first found element among given indices */
            while (1) {
                pos = ta_search_calc_slot(psearch);
                if (pos == -1)
                    break;
                TA_ASSERT(pos < haystackP->used);

                if (ba_get(baP, pos) == bval)
                    break;
                psearch->cur += 1;  /* Next index to check */
            }
            if (psearch->flags & TA_SEARCH_INLINE)
                oresult = pos == -1 ? Tcl_NewObj() : Tcl_NewIntObj(bval);
            else
                oresult = Tcl_NewIntObj(pos);
        }
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}
                        
struct thdr_search_mt_context {
    thdr_t *haystack;      /* MT read access, must NOT be modified */
    ta_value_t needle;
    thdr_t *thdr;              /* Will contain values or indices */
    struct thdr_search_mt_context *context0; /* Ptr to first context - used
                                                in assertion checking */
    ta_value_t first_value;           /* Value of first match */
    int first_match;           /* Index of first match */
    int start;                  /* Starting position to look in haystack */
    int count;                  /* Number of elements to examine */
    int flags;                  /* TA_SEARCH_* flags */
    TCL_RESULT res;
    enum ta_search_switches_e op; /* Search operation */
};

static void thdr_basic_search_mt_worker(struct thdr_search_mt_context *pctx)
{
    int compare_wanted;
    thdr_t *thdr = NULL;
    unsigned char type = pctx->haystack->type;
    
    TA_ASSERT((pctx->start + pctx->count) <= pctx->haystack->used);
    /* If regexp operation, must not multithread */
    TA_ASSERT(pctx->op != TA_SEARCH_OPT_RE || pctx == pctx->context0);
    /* If inline+all search for STRING or ANY, must not multithread.
       TBD - we could perhaps change this if we postpone refcounting until
       we return to the main thread. But error handling becomes harder.
    */
    TA_ASSERT(pctx == pctx->context0 ||
              ((pctx->flags & (TA_SEARCH_INLINE|TA_SEARCH_ALL)) != (TA_SEARCH_INLINE|TA_SEARCH_ALL)) ||
              (type != TA_ANY && type != TA_STRING));

    compare_wanted = pctx->flags & TA_SEARCH_INVERT ? 0 : 1;

    pctx->thdr = NULL;
    pctx->first_match = -1;
    if (pctx->flags & TA_SEARCH_ALL) {
        thdr = thdr_alloc(NULL,
                          pctx->flags & TA_SEARCH_INLINE ? type : TA_INT,
                          10);                /* Assume 10 hits TBD */
        if (thdr == NULL) {
            pctx->res = TCL_ERROR;
            return;
        }
    } else
        pctx->first_value.type = type;

#define SEARCHLOOP_(type_, value_) \
    do {                                                                \
        type_ *p, *end;                                                 \
        type_ needle = value_;                                          \
        int pos = pctx->start;                                          \
        p = THDRELEMPTR(pctx->haystack, type_, pctx->start);            \
        end = p + pctx->count;                                          \
        while (p < end) {                                               \
            int compare_result;                                         \
            switch (pctx->op) {                                         \
            case TA_SEARCH_OPT_GT: compare_result = (*p > needle); break; \
            case TA_SEARCH_OPT_LT: compare_result = (*p < needle); break; \
            case TA_SEARCH_OPT_EQ: compare_result = (*p == needle); break; \
            default: goto op_panic;                                     \
            }                                                           \
                                                                        \
            if (compare_result == compare_wanted) {                     \
                /* Have a match, add it to found items */               \
                if (pctx->flags & TA_SEARCH_ALL) {                      \
                    if (thdr->used >= thdr->usable) {                   \
                        thdr_t *pnew;                                   \
                        pnew = thdr_realloc(NULL, thdr, thdr->used + TA_EXTRA(thdr->used)); \
                        if (pnew)                                       \
                            thdr = pnew;                                \
                        else {                                          \
                            thdr_decr_refs(thdr);                       \
                            pctx->res = TCL_ERROR;                      \
                            return;                                     \
                        }                                               \
                    }                                                   \
                    if (pctx->flags & TA_SEARCH_INLINE)                 \
                        *THDRELEMPTR(thdr, type_, thdr->used) = *p;     \
                    else                                                \
                        *THDRELEMPTR(thdr, int, thdr->used) = pos;      \
                    thdr->used++;                                       \
                } else {                                                \
                    pctx->first_match = pos;                            \
                    break;                                              \
                }                                                       \
            }                                                           \
            ++pos;                                                      \
            ++p;                                                        \
        }                                                               \
    } while (0)

    switch (type) {
    case TA_INT:
        SEARCHLOOP_(int, pctx->needle.ival);
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.ival = *THDRELEMPTR(pctx->haystack, int, pctx->first_match);
        break;
    case TA_UINT:
        SEARCHLOOP_(unsigned int, pctx->needle.uival);
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.uival = *THDRELEMPTR(pctx->haystack, unsigned int, pctx->first_match);
        break;
    case TA_WIDE:
        SEARCHLOOP_(Tcl_WideInt, pctx->needle.wval);
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.wval = *THDRELEMPTR(pctx->haystack, Tcl_WideInt, pctx->first_match);
        break;
    case TA_DOUBLE:
        SEARCHLOOP_(double, pctx->needle.dval);
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.dval = *THDRELEMPTR(pctx->haystack, double, pctx->first_match);
        break;
    case TA_BYTE:
        SEARCHLOOP_(unsigned char, pctx->needle.ucval);
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.ucval = *THDRELEMPTR(pctx->haystack, unsigned char, pctx->first_match);
        break;
    case TA_ANY:
        do {
            Tcl_Obj **p, **end;
            Tcl_Obj *needleObj = pctx->needle.oval; 
            int pos = pctx->start;
            int nocase = pctx->flags & TA_SEARCH_NOCASE;
            Tcl_RegExp re;

            /* NOTE: regexp code is not thread safe. It is up to the caller
               to ensure only the interp thread calls this routine for
               regexp matching */
            if (pctx->op == TA_SEARCH_OPT_RE) {
                /* Following lsearch implementation, get the regexp before any
                   shimmering can take place, and try to compile for the efficient
                   NOSUB case
                */
                re = Tcl_GetRegExpFromObj(NULL, needleObj,
                                          TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0)|TCL_REG_NOSUB );
                if (re == NULL) {
                    /* That failed, so try without the NOSUB flag */
                    re = Tcl_GetRegExpFromObj(NULL, needleObj,
                                              TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0));
                    if (re == NULL) {
                        pctx->res = TCL_ERROR;
                        return;
                    }
                }
            }

            if (nocase)
                nocase = TCL_MATCH_NOCASE;

            p = THDRELEMPTR(pctx->haystack, Tcl_Obj *, pctx->start);
            end = p + pctx->count;

            while (p < end) {
                int compare_result;
                switch (pctx->op) {
                case TA_SEARCH_OPT_GT:
                    compare_result = ta_obj_compare(*p, needleObj, nocase) > 0;
                    break;
                case TA_SEARCH_OPT_LT: 
                    compare_result = ta_obj_compare(*p, needleObj, nocase) < 0;
                    break;
                case TA_SEARCH_OPT_EQ:
                    compare_result = ta_obj_equal(*p, needleObj, nocase);
                    break;
                case TA_SEARCH_OPT_PAT:
                    compare_result = Tcl_StringCaseMatch(Tcl_GetString(*p),
                                                         Tcl_GetString(needleObj),
                                                         nocase);
                    break;
                case TA_SEARCH_OPT_RE:
                    compare_result = Tcl_RegExpExecObj(NULL, re, *p, 0, 0, 0);
                    if (compare_result < 0) {
                        thdr_decr_refs(thdr); /* Note this unrefs embedded Tcl_Objs if needed */
                        pctx->res = TCL_ERROR;
                        return;
                    }
                    break;
                default:
                    goto op_panic;
                }

                if (compare_result == compare_wanted) {
                    /* Have a match, add it to found items */
                    if (pctx->flags & TA_SEARCH_ALL) {
                        if (thdr->used >= thdr->usable) {
                            thdr_t *pnew;
                            pnew = thdr_realloc(NULL, thdr, thdr->used + TA_EXTRA(thdr->used));
                            if (pnew)
                                thdr = pnew;
                            else {
                                thdr_decr_refs(thdr);
                                pctx->res = TCL_ERROR;
                                return;
                            }
                        }
                        if (pctx->flags & TA_SEARCH_INLINE) {
                            /* Note this operation is not thread-safe. The
                               assumption is caller is using only the interp
                               thread */
                            Tcl_IncrRefCount(*p);
                            *THDRELEMPTR(thdr, Tcl_Obj *, thdr->used) = *p;
                        } else
                            *THDRELEMPTR(thdr, int, thdr->used) = pos;
                        thdr->used++;
                    } else {
                        pctx->first_match = pos;
                        break;
                    }
                }
                ++pos;
                ++p;
            }
        } while (0);
        /* Note here we do not incr ref count on the object as that will
           done in the "master" object when returning the value */
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.oval = *THDRELEMPTR(pctx->haystack, Tcl_Obj *, pctx->first_match);
        break;

    case TA_STRING:
#define SEARCHLOOPSTRING_(CMP_)                                         \
        do {                                                            \
            while (p < end) {                                           \
            int compare_result;                                         \
            compare_result = CMP_;                                      \
            if (compare_result < 0)                                     \
                goto cmp_error;                                         \
            if (compare_result == compare_wanted) {                     \
                /* Have a match, add it to found items */               \
                if (pctx->flags & TA_SEARCH_ALL) {                      \
                    if (thdr->used >= thdr->usable) {                   \
                        thdr_t *pnew;                                   \
                        pnew = thdr_realloc(NULL, thdr, thdr->used + TA_EXTRA(thdr->used)); \
                        if (pnew)                                       \
                            thdr = pnew;                                \
                        else                                            \
                            goto cmp_error;                             \
                    }                                                   \
                    if (pctx->flags & TA_SEARCH_INLINE) {               \
                        /* Note this operation is not thread-safe. The  \
                           assumption is caller is using only the interp \
                           thread so ok to do this*/                    \
                        *THDRELEMPTR(thdr, tas_t *, thdr->used) = tas_ref(*p); \
                    } else                                              \
                        *THDRELEMPTR(thdr, int, thdr->used) = pos;      \
                    thdr->used++;                                       \
                } else {                                                \
                    pctx->first_match = pos;                            \
                    break;                                              \
                }                                                       \
            }                                                           \
            ++pos;                                                      \
            ++p;                                                        \
        }                                                               \
    } while (0)

        do {
            tas_t **p, **end;
            tas_t *needle = pctx->needle.ptas;
            int pos = pctx->start;
            int nocase = pctx->flags & TA_SEARCH_NOCASE;
            Tcl_RegExp re;

            /* NOTE: regexp code is not thread safe. It is up to the caller
               to ensure only the interp thread calls this routine for
               regexp matching */
            if (pctx->op == TA_SEARCH_OPT_RE) {
                if (! nocase)
                    re = Tcl_RegExpCompile(NULL, needle->s);
                else {
                    /* Tcl_RegExpCompile does not have flags for
                     * case-insensitive comparisons so prefix the option
                    */
                    /* Note sizeof includes the terminating null */
                    char *nocase_s = TA_ALLOCMEM(sizeof("(?i)")+strlen(needle->s));
                    strcpy(nocase_s, "(?i)");
                    strcpy(nocase_s + sizeof("(?i)")-1, needle->s);
                    re = Tcl_RegExpCompile(NULL, nocase_s);
                    TA_FREEMEM(nocase_s);
                }
                if (re == NULL) {
                    pctx->res = TCL_ERROR;
                    return;
                }
            }

            if (nocase)
                nocase = TCL_MATCH_NOCASE;
            p = THDRELEMPTR(pctx->haystack, tas_t *, pctx->start);
            end = p + pctx->count;

            switch (pctx->op) {
            case TA_SEARCH_OPT_GT:
                SEARCHLOOPSTRING_((tas_compare(*p, needle, nocase) > 0));
                break;
            case TA_SEARCH_OPT_LT:
                SEARCHLOOPSTRING_((tas_compare(*p, needle, nocase) < 0));
                break;
            case TA_SEARCH_OPT_EQ:
                SEARCHLOOPSTRING_((tas_equal(*p, needle, nocase)));
                break;
            case TA_SEARCH_OPT_PAT:
                SEARCHLOOPSTRING_((Tcl_StringCaseMatch((*p)->s, needle->s, nocase)));
                break;
            case TA_SEARCH_OPT_RE:
                SEARCHLOOPSTRING_((Tcl_RegExpExec(NULL, re, (*p)->s, (*p)->s)));
                break;
            default:
                goto op_panic;
            }
        } while (0);

        /* NOTE:
         * Do not use tas_ref because that is not MT-safe and this point
         * in the code is reached even for operations that allow MT.
         * Correspondingly caller should not use ta_value_clear
         * on pctx->first_value 
         */
        if ((pctx->flags & TA_SEARCH_ALL) == 0 && pctx->first_match >= 0)
            pctx->first_value.ptas = *THDRELEMPTR(pctx->haystack, tas_t *, pctx->first_match);
        break;


    default:
        ta_type_panic(type);
    }

    if (pctx->flags & TA_SEARCH_ALL) {
        if ((pctx->flags & TA_SEARCH_INLINE) == 0)
            thdr->sort_order = THDR_SORTED_ASCENDING; /* indices are naturally sorted */
        pctx->thdr = thdr;
    }

    pctx->res = TCL_OK;
    return;

op_panic:
    ta_operator_panic(pctx->op);
    return;                     /* To keep compiler happy */

cmp_error:
    thdr_decr_refs(thdr);
    pctx->res = TCL_ERROR;
    return;
}

/* Returns TCL_OK, if search completed, TCL_CONTINUE if mt search is not possible and TCL_ERROR for errors */
static TCL_RESULT thdr_basic_search_mt(Tcl_Interp *ip, thdr_t * haystackP,
                                       Tcl_Obj *needleObj, ta_search_t *psearch)
{
    TCL_RESULT res;
    struct thdr_search_mt_context mt_context[2];
    int i, ncontexts, count;

    TA_ASSERT(haystackP->type != TA_BOOLEAN);

    /* First check that the search amenable to multithreading. */
    switch (psearch->op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT:
    case TA_SEARCH_OPT_EQ:
    case TA_SEARCH_OPT_PAT:
    case TA_SEARCH_OPT_RE:
        break;
    default: return TCL_CONTINUE;
    }

    /* Verify we are not searching with indices (-among option) */
    if (psearch->indices)
        return TCL_CONTINUE;

    /* Empty column/range. Format of return value depends on other options,
       e.g. whether inline or not etc.
       Instead of repeating here, let slow path handle it */
    if (psearch->upper <= psearch->lower)
        return TCL_CONTINUE;
    count = psearch->upper - psearch->lower + 1;

    res = ta_value_from_obj(ip, needleObj, haystackP->type, &mt_context[0].needle);
    if (res != TCL_OK)
        return res;

    /* OK, looks like we can do a MT search */

    if (haystackP->type == TA_ANY)
        thdr_ensure_obj_strings(haystackP);

    mt_context[0].haystack = haystackP;
    mt_context[0].start = psearch->lower;
    mt_context[0].flags = psearch->flags;
    mt_context[0].op = psearch->op;
    mt_context[0].context0 = mt_context;
    
#ifdef TA_MT_ENABLE
    /* TBD - see if this can be reworked to avoid call to thdr_calc_mt_split
       if we are not going to multi thread for other reasons */
    mt_context[0].count =
        thdr_calc_mt_split(haystackP->type, psearch->lower, count, &mt_context[1].count);
    TA_ASSERT((mt_context[0].count + mt_context[1].count) == count);
    
    /* Note that TA_ANY/TA+STRING requires manipulation of ref counts
       which is not thread-safe so we use only one thread for those
       cases if INLINE+ALL search. This is a REQUIREMENT.

       In addition if TA_SEARCH_ALL is not set, we do not use MT.
       This is NOT a REQUIREMENT. Rather it is to avoid both unnecessary
       work as well as holding up the first thread if the match is found
       in the first thread itself.
    */
    /* Use single thread if regexp matching as regexp engine is not thread
       safe (stores Tcl_Obj* in compiled regexps and potentially updates them)
    */
    if (count < ta_search_mt_threshold ||
        psearch->op == TA_SEARCH_OPT_RE ||
        ((psearch->flags & TA_SEARCH_ALL) == 0) ||
        mt_context[1].count == 0 ||
        ((haystackP->type == TA_ANY || haystackP->type == TA_STRING) &&
         ((psearch->flags & (TA_SEARCH_INLINE|TA_SEARCH_ALL)) == (TA_SEARCH_INLINE|TA_SEARCH_ALL)))) {
        mt_context[0].count = count;
        thdr_basic_search_mt_worker(&mt_context[0]);
        ncontexts = 1;
    } else {
        ta_mt_group_t grp;

        mt_context[1].haystack = haystackP;
        mt_context[1].start = psearch->lower + mt_context[0].count;
        mt_context[1].flags = psearch->flags;
        mt_context[1].op = psearch->op;
        mt_context[1].needle = mt_context[0].needle;
        mt_context[1].context0 = mt_context;

        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* TBD - check return code */ ta_mt_group_async_f(grp, &mt_context[1], thdr_basic_search_mt_worker);
        thdr_basic_search_mt_worker(&mt_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);

        ncontexts = 2;
    }

#else

    mt_context[0].count = count;
    thdr_basic_search_mt_worker(&mt_context[0]);
    ncontexts = 1;

#endif

    /* Note if set, mt_context[1].needle is SAME so do NOT clear it as well
       else you will have double freeing */
    ta_value_clear(&mt_context[0].needle);

    /*
     * If TA_SEARCH_ALL is true, the thdr fields of the context are valid
     * Else the first_match/first_value fields are valid.
     */
    
    /* First verify all threads ran successfully */
    res = TCL_OK;
    for (i = 0; i < ncontexts; ++i) {
        if (mt_context[i].res != TCL_OK) {
            res = TCL_ERROR;
            break;
        }
    }

    if (res == TCL_OK) {
        if (psearch->flags & TA_SEARCH_ALL) {
            thdr_t *result_thdr;
            /* Based on above checks that all contexts that exist have status OK */
            /* Context 0 always exists. See if it has any matches */
            result_thdr = mt_context[0].thdr; /* Assume */
            if (mt_context[0].thdr->used > 0) {
                /* Have something here. See if second context also does. */
                if (ncontexts > 1) {
                    if (mt_context[1].thdr->used > 0) {
                        int total = mt_context[0].thdr->used + mt_context[1].thdr->used;
                        if (result_thdr->usable < total) {
                            result_thdr = thdr_realloc(ip, result_thdr, total);
                            if (result_thdr == NULL) {
                                thdr_decr_refs(mt_context[0].thdr);
                                thdr_decr_refs(mt_context[1].thdr);
                                return TCL_ERROR;
                            }
                            mt_context[0].thdr = result_thdr;
                        }
                        thdr_copy(result_thdr,
                                  result_thdr->used,
                                  mt_context[1].thdr,
                                  0,
                                  mt_context[1].thdr->used,
                                  0);
                    }
                    thdr_decr_refs(mt_context[1].thdr);
                }
            } else {
                /* First context had no match. Check second if any */
                if (ncontexts > 1) {
                    thdr_decr_refs(mt_context[0].thdr);
                    result_thdr = mt_context[1].thdr; /* May also be empty */
                }
            }
            Tcl_SetObjResult(ip, tcol_new(result_thdr));
        } else {
            /* Single value to be returned */
            Tcl_Obj *resultObj;
            int matching_context;
            if (mt_context[0].first_match >= 0)
                matching_context = 0;
            else if (ncontexts > 1 && mt_context[1].first_match >= 0)
                matching_context = 1;
            else
                matching_context = -1;
            if (psearch->flags & TA_SEARCH_INLINE) {
                if (matching_context >= 0)
                    resultObj = ta_value_to_obj(&mt_context[matching_context].first_value);
                else
                    resultObj = Tcl_NewObj();
            } else {
                if (matching_context >= 0)
                    resultObj = Tcl_NewIntObj(mt_context[matching_context].first_match);
                else
                    resultObj = Tcl_NewIntObj(-1);
            }
            /*
             * NOTE: Do NOT call ta_value_clear on mt_context[].first_value
             * since ref counts would not have been updated in the worker
             * threads.
             */
            Tcl_SetObjResult(ip, resultObj);
        }
    } else {
        /* Free up thdrs (only allocated with TA_SEARCH_ALL) */
        if (psearch->flags & TA_SEARCH_ALL) {
            for (i = 0; i < ncontexts; ++i) {
                if (mt_context[i].res == TCL_OK && mt_context[i].thdr != NULL)
                    thdr_decr_refs(mt_context[i].thdr);
            }
        }
        Tcl_SetResult(ip, "Error during column search (out of memory or invalid regexp?)", TCL_STATIC);
    }

    return res;
}


static TCL_RESULT thdr_search_entier(Tcl_Interp *ip, thdr_t * haystackP,
                                     Tcl_Obj *needleObj, ta_search_t *psearch)
{
    int slot;
    Tcl_Obj *oresult;
    Tcl_WideInt needle, elem, min_val, max_val;
    int compare_result;
    int compare_wanted;
    void *p;
    TCL_RESULT res;

    res = thdr_basic_search_mt(ip, haystackP, needleObj, psearch);
    if (res != TCL_CONTINUE)
        return res;

    /* TBD - The code below is supposed to handle all cases including the ones
       handled by thdr_basic_search_mt above. Should we change that to
       only do the remaining case(s) ? I think the only such case is
       when psearch->indices is not NULL (-among option)
    */

    switch (psearch->op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return ta_search_op_error(ip, psearch->op);
    }

    if (Tcl_GetWideIntFromObj(ip, needleObj, &needle) != TCL_OK)
        return TCL_ERROR;

    switch (haystackP->type) {
    case TA_INT:
        max_val = INT_MAX;
        min_val = INT_MIN;
        break;
    case TA_UINT:
        max_val = UINT_MAX;
        min_val = 0;
        break;
    case TA_WIDE:
        max_val = needle; /* No-op */
        min_val = needle;
        break;
    case TA_BYTE:
        max_val = UCHAR_MAX;
        min_val = 0;
        break;
    default:
        ta_type_panic(haystackP->type);
    }

    if (needle > max_val || needle < min_val) {
        return ta_value_type_error(ip, needleObj, haystackP->type);
    }

    compare_wanted = psearch->flags & TA_SEARCH_INVERT ? 0 : 1;

    p = THDRELEMPTR(haystackP, char, 0);

    if (psearch->flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            psearch->flags & TA_SEARCH_INLINE ? haystackP->type : TA_INT,
                            10);                /* Assume 10 hits TBD */
        if (thdr == NULL)
            return TCL_ERROR;

        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1)
                break;

            switch (haystackP->type) {
            case TA_INT:  elem = *(slot + (int *)p); break;
            case TA_UINT: elem = *(slot + (unsigned int *)p); break;
            case TA_WIDE: elem = *(slot + (Tcl_WideInt *)p); break;
            case TA_BYTE: elem = *(slot + (unsigned char *)p); break;
            }
            switch (psearch->op) {
            case TA_SEARCH_OPT_GT: compare_result = (elem > needle); break;
            case TA_SEARCH_OPT_LT: compare_result = (elem < needle); break;
            case TA_SEARCH_OPT_EQ:
            default: compare_result = (elem == needle); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->usable) {
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                    if (newP)
                        thdr = newP;
                    else {
                        thdr_decr_refs(thdr);
                        return TCL_ERROR;
                    }
                }
                if (psearch->flags & TA_SEARCH_INLINE) {
                    switch (thdr->type) {
                    case TA_INT:  *THDRELEMPTR(thdr, int, thdr->used) = (int) elem; break;
                    case TA_UINT: *THDRELEMPTR(thdr, unsigned int, thdr->used) = (unsigned int) elem; break;
                    case TA_WIDE: *THDRELEMPTR(thdr, Tcl_WideInt, thdr->used) = elem; break;
                    case TA_BYTE:  *THDRELEMPTR(thdr, unsigned char, thdr->used) = (unsigned char) elem; break;
                    }
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = slot;
                }
                thdr->used++;
            }

            psearch->cur += 1;  /* Next slot or index */
        } /* end while (1) */

        if (psearch->indices == NULL &&
            ((psearch->flags & TA_SEARCH_INLINE) == 0))
            thdr->sort_order = THDR_SORTED_ASCENDING; /* indices are naturally sorted */

        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */
        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1)
                break;
            switch (haystackP->type) {
            case TA_INT:  elem = *(slot + (int *)p); break;
            case TA_UINT: elem = *(slot + (unsigned int *)p); break;
            case TA_WIDE: elem = *(slot + (Tcl_WideInt *)p); break;
            case TA_BYTE: elem = *(slot + (unsigned char *)p); break;
            }
            switch (psearch->op) {
            case TA_SEARCH_OPT_GT: compare_result = (elem > needle); break;
            case TA_SEARCH_OPT_LT: compare_result = (elem < needle); break;
            case TA_SEARCH_OPT_EQ:
            default: compare_result = (elem == needle); break;
            }
            if (compare_result == compare_wanted)
                break;
            psearch->cur += 1;  /* Try next slot */
        }
        if (psearch->flags & TA_SEARCH_INLINE)
            oresult = slot == -1 ? Tcl_NewObj() : Tcl_NewWideIntObj(elem);
        else
            oresult = Tcl_NewIntObj(slot);
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}


static TCL_RESULT thdr_search_double(Tcl_Interp *ip, thdr_t * haystackP,
                                     Tcl_Obj *needleObj, ta_search_t *psearch)
{
    int slot;
    Tcl_Obj *oresult;
    double dneedle, dbl;
    int compare_result;
    int compare_wanted;
    TCL_RESULT res;

    TA_ASSERT(haystackP->type == TA_DOUBLE);

    res = thdr_basic_search_mt(ip, haystackP, needleObj, psearch);
    if (res != TCL_CONTINUE)
        return res;

    /* TBD - The code below is supposed to handle all cases including the ones
       handled by thdr_basic_search_mt above. Should we change that to
       only do the remaining case(s) ? I think the only such case is
       when psearch->indices is not NULL (-among option)
    */

    switch (psearch->op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return ta_search_op_error(ip, psearch->op);
    }

    if ((res = Tcl_GetDoubleFromObj(ip, needleObj, &dneedle)) != TCL_OK)
        return res;
    
    compare_wanted = psearch->flags & TA_SEARCH_INVERT ? 0 : 1;

    if (psearch->flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            psearch->flags & TA_SEARCH_INLINE ? TA_DOUBLE : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdr == NULL)
            return TCL_ERROR;

        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1)
                break;

            dbl = *THDRELEMPTR(haystackP, double, slot);
            switch (psearch->op) {
            case TA_SEARCH_OPT_GT: compare_result = (dbl > dneedle); break;
            case TA_SEARCH_OPT_LT: compare_result = (dbl < dneedle); break;
            case TA_SEARCH_OPT_EQ: compare_result = (dbl == dneedle); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->usable) {
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                    if (newP)
                        thdr = newP;
                    else {
                        thdr_decr_refs(thdr);
                        return TCL_ERROR;
                    }
                }
                if (psearch->flags & TA_SEARCH_INLINE) {
                    *THDRELEMPTR(thdr, double, thdr->used) = dbl;
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = slot;
                }
                thdr->used++;
            }

            psearch->cur += 1;  /* Next slot or index */
        } /* end while (1) */

        if (psearch->indices == NULL &&
            ((psearch->flags & TA_SEARCH_INLINE) == 0))
            thdr->sort_order = THDR_SORTED_ASCENDING; /* indices are naturally sorted */

        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */
        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1)
                break;
            dbl = *THDRELEMPTR(haystackP, double, slot);
            switch (psearch->op) {
            case TA_SEARCH_OPT_GT: compare_result = (dbl > dneedle); break;
            case TA_SEARCH_OPT_LT: compare_result = (dbl < dneedle); break;
            case TA_SEARCH_OPT_EQ: compare_result = (dbl == dneedle); break;
            }
            if (compare_result == compare_wanted)
                break;
            psearch->cur += 1;  /* Try next slot */
        }
        if (psearch->flags & TA_SEARCH_INLINE)
            oresult = slot == -1 ? Tcl_NewObj() : Tcl_NewDoubleObj(dbl);
        else
            oresult = Tcl_NewIntObj(slot);
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}

static TCL_RESULT thdr_search_obj(Tcl_Interp *ip, thdr_t * haystackP,
                                  Tcl_Obj *needleObj, ta_search_t *psearch)
{
    int slot;
    Tcl_Obj *pobj;
    Tcl_Obj *oresult;
    int compare_result;
    int compare_wanted;
    int nocase;
    Tcl_RegExp re;
    TCL_RESULT res;

    /* TBD - do we need to increment the haystacP ref to guard against shimmering */
    TA_ASSERT(haystackP->type == TA_ANY);
    
    res = thdr_basic_search_mt(ip, haystackP, needleObj, psearch);
    if (res != TCL_CONTINUE)
        return res;

    compare_wanted = psearch->flags & TA_SEARCH_INVERT ? 0 : 1;
    nocase = psearch->flags & TA_SEARCH_NOCASE;

    switch (psearch->op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
    case TA_SEARCH_OPT_PAT:
        break;
    case TA_SEARCH_OPT_RE:
        /* Following lsearch implementation, get the regexp before any
           shimmering can take place, and try to compile for the efficient
           NOSUB case
        */
        re = Tcl_GetRegExpFromObj(NULL, needleObj,
                                  TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0)|TCL_REG_NOSUB );
        if (re == NULL) {
            /* That failed, so try without the NOSUB flag */
            re = Tcl_GetRegExpFromObj(ip, needleObj,
                                      TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0));
            if (re == NULL)
                return TCL_ERROR;
        }
        break;
    default:
        return ta_search_op_error(ip, psearch->op);
    }

    if (psearch->flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            psearch->flags & TA_SEARCH_INLINE ? TA_ANY : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdr == NULL)
            return TCL_ERROR;


        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1)
                break;

            pobj = *THDRELEMPTR(haystackP, Tcl_Obj *, slot);
            switch (psearch->op) {
            case TA_SEARCH_OPT_GT:
                compare_result = ta_obj_compare(pobj, needleObj, nocase) > 0;
                break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(pobj, needleObj, nocase) < 0;
                break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_equal(pobj, needleObj, nocase);
                break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(pobj),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExecObj(ip, re, pobj,
                                                   0, 0, 0);
                if (compare_result < 0) {
                    thdr_decr_refs(thdr); /* Note this unrefs embedded Tcl_Objs if needed */
                    return TCL_ERROR;
                }
                break;
            }
            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->usable) {
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                    if (newP)
                        thdr = newP;
                    else {
                        thdr_decr_refs(thdr);
                        return TCL_ERROR;
                    }
                }
                if (psearch->flags & TA_SEARCH_INLINE) {
                    Tcl_IncrRefCount(pobj);
                    *THDRELEMPTR(thdr, Tcl_Obj *, thdr->used) = pobj;
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = slot;
                }
                thdr->used++;
            }

            psearch->cur += 1;  /* Next slot or index */
        } /* end while (1) */

        if (psearch->indices == NULL &&
            ((psearch->flags & TA_SEARCH_INLINE) == 0))
            thdr->sort_order = THDR_SORTED_ASCENDING; /* indices are naturally sorted */
        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */

        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1) {
                pobj = NULL;
                break;
            }
            pobj = *THDRELEMPTR(haystackP, Tcl_Obj *, slot);

            switch (psearch->op) {
            case TA_SEARCH_OPT_GT:
                compare_result = ta_obj_compare(pobj, needleObj, nocase) > 0;
                break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(pobj, needleObj, nocase) < 0;
                break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_equal(pobj, needleObj, nocase);
                break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(pobj),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExecObj(ip, re, pobj, 0, 0, 0);
                if (compare_result < 0)
                    return TCL_ERROR;
                break;
            }
            if (compare_result == compare_wanted)
                break;

            psearch->cur += 1;  /* Next slot or index */
        }

        TA_ASSERT(pobj == NULL || slot >= 0);
        TA_ASSERT(pobj || slot < 0);

        if (psearch->flags & TA_SEARCH_INLINE) {
            /* No need to incr ref for pobj, the SetObjResult does it */
            oresult = pobj ? pobj : Tcl_NewObj();
        } else
            oresult = Tcl_NewIntObj(slot);
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}


static TCL_RESULT thdr_search_string(Tcl_Interp *ip, thdr_t *haystackP,
                                     Tcl_Obj *needleObj, ta_search_t *psearch)
{
    int slot;
    tas_t *ptas, *needle;
    Tcl_Obj *oresult;
    int compare_result;
    int compare_wanted;
    int nocase;
    Tcl_RegExp re;
    TCL_RESULT res;

    /* TBD - do we need to increment the haystacP ref to guard against shimmering */
    TA_ASSERT(haystackP->type == TA_STRING);
    
    res = thdr_basic_search_mt(ip, haystackP, needleObj, psearch);
    if (res != TCL_CONTINUE)
        return res;

    compare_wanted = psearch->flags & TA_SEARCH_INVERT ? 0 : 1;
    nocase = psearch->flags & TA_SEARCH_NOCASE;

    switch (psearch->op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
    case TA_SEARCH_OPT_PAT:
        break;
    case TA_SEARCH_OPT_RE:
        /* Following lsearch implementation, get the regexp before any
           shimmering can take place, and try to compile for the efficient
           NOSUB case
        */
        re = Tcl_GetRegExpFromObj(NULL, needleObj,
                                  TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0)|TCL_REG_NOSUB );
        if (re == NULL) {
            /* That failed, so try without the NOSUB flag */
            re = Tcl_GetRegExpFromObj(ip, needleObj,
                                      TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0));
            if (re == NULL)
                return TCL_ERROR;
        }
        break;
    default:
        return ta_search_op_error(ip, psearch->op);
    }

    if (nocase && psearch->op == TA_SEARCH_OPT_PAT)
        nocase = TCL_MATCH_NOCASE;
    needle = tas_from_obj(needleObj); /* Must be UNREF'ed ! */
    if (psearch->flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            psearch->flags & TA_SEARCH_INLINE ? TA_STRING : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdr == NULL)
            goto free_needle_and_return_error;

        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1)
                break;

            ptas = *THDRELEMPTR(haystackP, tas_t *, slot);
            switch (psearch->op) {
            case TA_SEARCH_OPT_GT:
                compare_result = tas_compare(ptas, needle, nocase) > 0;
                break;
            case TA_SEARCH_OPT_LT: 
                compare_result = tas_compare(ptas, needle, nocase) < 0;
                break;
            case TA_SEARCH_OPT_EQ:
                compare_result = tas_equal(ptas, needle, nocase);
                break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(ptas->s, needle->s,
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExec(NULL, re, ptas->s, ptas->s);
                if (compare_result < 0) {
                    thdr_decr_refs(thdr); /* Note this unrefs embedded elements  if needed */
                    goto free_needle_and_return_error;
                }
                break;
            }
            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->usable) {
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                    if (newP)
                        thdr = newP;
                    else {
                        thdr_decr_refs(thdr);
                        goto free_needle_and_return_error;
                    }
                }
                if (psearch->flags & TA_SEARCH_INLINE) {
                    *THDRELEMPTR(thdr, tas_t *, thdr->used) = tas_ref(ptas);
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = slot;
                }
                thdr->used++;
            }

            psearch->cur += 1;  /* Next slot or index */
        } /* end while (1) */

        if (psearch->indices == NULL &&
            ((psearch->flags & TA_SEARCH_INLINE) == 0))
            thdr->sort_order = THDR_SORTED_ASCENDING; /* indices are naturally sorted */
        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */

        while (1) {
            slot = ta_search_calc_slot(psearch);
            if (slot == -1) {
                ptas = NULL;
                break;
            }
            ptas = *THDRELEMPTR(haystackP, tas_t *, slot);

            switch (psearch->op) {
            case TA_SEARCH_OPT_GT:
                compare_result = tas_compare(ptas, needle, nocase) > 0;
                break;
            case TA_SEARCH_OPT_LT: 
                compare_result = tas_compare(ptas, needle, nocase) < 0;
                break;
            case TA_SEARCH_OPT_EQ:
                compare_result = tas_equal(ptas, needle, nocase);
                break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(ptas->s, needle->s, nocase);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExec(NULL, re, ptas->s, ptas->s);
                if (compare_result < 0)
                    goto free_needle_and_return_error;
                break;
            }
            if (compare_result == compare_wanted)
                break;

            psearch->cur += 1;  /* Next slot or index */
        }

        TA_ASSERT(ptas == NULL || slot >= 0);
        TA_ASSERT(ptas || slot < 0);

        if (psearch->flags & TA_SEARCH_INLINE) {
            /* No need to incr ref for pobj, the SetObjResult does it */
            oresult = ptas ? tas_to_obj(ptas) : Tcl_NewObj();
        } else
            oresult = Tcl_NewIntObj(slot);
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;

free_needle_and_return_error:
    if (needle)
        tas_unref(needle);
    return TCL_ERROR;
}



TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    int i, n, opt, endval;
    thdr_t *haystackP;
    ta_search_t search;
    Tcl_Obj *orange;
    Tcl_Obj **range;
    TCL_RESULT status;
    TCL_RESULT (*searchfn)(Tcl_Interp *, thdr_t *, Tcl_Obj *, ta_search_t *);

    if (objc < 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "?options? tarray pattern");
	return TCL_ERROR;
    }

    if (tcol_convert(ip, objv[objc-2]) != TCL_OK)
        return TCL_ERROR;

    haystackP = tcol_thdr(objv[objc-2]);
    search.indices = NULL;
    search.flags = 0;
    search.lower = 0;
    search.upper = haystackP->used - 1;
    search.op = TA_SEARCH_OPT_EQ;
    for (i = 1; i < objc-2; ++i) {
	status = Tcl_GetIndexFromObj(ip, objv[i], ta_search_switches_e, "option", 0, &opt);
        if (status != TCL_OK)
            goto vamoose;

        switch ((enum ta_search_switches_e) opt) {
        case TA_SEARCH_OPT_ALL: search.flags |= TA_SEARCH_ALL; break;
        case TA_SEARCH_OPT_INLINE: search.flags |= TA_SEARCH_INLINE; break;
        case TA_SEARCH_OPT_INVERT: search.flags |= TA_SEARCH_INVERT; break;
        case TA_SEARCH_OPT_NOCASE: search.flags |= TA_SEARCH_NOCASE; break;
        case TA_SEARCH_OPT_AMONG:
            if (search.indices) {
                thdr_decr_refs(search.indices);
                search.indices = NULL;
            }
            if (i > objc-4) {
                status = ta_missing_arg_error(ip, "-indices");
                goto vamoose;
            }
            ++i;
            switch (ta_obj_to_indices(ip, objv[i], 0, 0, &search.indices, NULL)) {
            case TA_INDEX_TYPE_ERROR:
                status = TCL_ERROR;
                goto vamoose;
            case TA_INDEX_TYPE_INT:
                Tcl_Panic("ta_obj_to_indices returned TA_INDEX_TYPE_INT when passed NULL pointer");
                break;
            case TA_INDEX_TYPE_THDR:
                break;
            }
            break;
        case TA_SEARCH_OPT_RANGE:
            if (i > objc-4) {
                status = ta_missing_arg_error(ip, "-range");
                goto vamoose;
            }
            ++i;
            /*
             * To prevent shimmering, check if the index object is same
             * as tarray object.
             */
            /* TBD - see if we can replace with ta_parse_range_option_value */
            if (objv[i] == objv[objc-2])
                orange = Tcl_DuplicateObj(objv[i]);
            else {
                orange = objv[i];
                Tcl_IncrRefCount(orange);
            }
            status = Tcl_ListObjGetElements(ip, orange, &n, &range);
            if (status != TCL_OK)
                goto vamoose;
            /* Note if # elements (n) is 0, the defaults are used */
            endval = haystackP->used - 1;
            if (n > 2
                || (n > 0
                    &&
                    (ta_convert_index(ip, range[0], &search.lower, endval, INT_MIN, INT_MAX) != TCL_OK
                     || (n > 1
                         && ta_convert_index(ip, range[1], &search.upper, endval, INT_MIN, INT_MAX) != TCL_OK)))) {
                status = ta_invalid_range_error(ip, orange);
                Tcl_DecrRefCount(orange);
                goto vamoose;
            }
            Tcl_DecrRefCount(orange);
            break;
        case TA_SEARCH_OPT_EQ:
        case TA_SEARCH_OPT_GT:
        case TA_SEARCH_OPT_LT:
        case TA_SEARCH_OPT_PAT:
        case TA_SEARCH_OPT_RE:
            search.op = (enum ta_search_switches_e) opt;
        }
    }

    if (search.lower < 0)
        search.lower = 0;
    if (search.upper >= haystackP->used)
        search.upper = haystackP->used - 1;
    if (search.indices)
        search.cur = 0;
    else
        search.cur = search.lower;

    switch (haystackP->type) {
    case TA_BOOLEAN: searchfn = thdr_search_boolean; break;
    case TA_INT:
    case TA_UINT:
    case TA_BYTE:
    case TA_WIDE:   searchfn = thdr_search_entier; break;
    case TA_DOUBLE: searchfn = thdr_search_double; break;
    case TA_ANY:    searchfn = thdr_search_obj; break;
    case TA_STRING:    searchfn = thdr_search_string; break;
    default:
        ta_type_panic(haystackP->type);
        return TCL_ERROR;       /* To avoid compiler warning about return */
    }
    status = (*searchfn)(ip, haystackP, objv[objc-1], &search);

vamoose:
    if (search.indices)
        thdr_decr_refs(search.indices);

    return status;
}


TCL_RESULT tcol_lookup_cmd(ClientData clientdata, Tcl_Interp *ip,
                           int objc, Tcl_Obj *const objv[])
{
    thdr_t *thdr;
    tas_t *ptas, *pkey;
    ClientData value;
    int pos, delete_pos;

    if (objc < 2 || objc > 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "column ?key?");
	return TCL_ERROR;
    }
    
    if (tcol_convert(ip, objv[1]) != TCL_OK)
        return TCL_ERROR;
    thdr = tcol_thdr(objv[1]);
    if (thdr->type != TA_STRING)
        return ta_bad_type_error(ip, thdr);

    if (objc == 2) {
        thdr_lookup_build(thdr);
        return TCL_OK;
    }

    if (thdr->lookup == TAS_LOOKUP_INVALID_HANDLE)
        thdr_lookup_init(thdr);

    pkey = tas_from_obj(objv[2]);
    pos = delete_pos = -1;

    if (tas_lookup_entry(thdr->lookup, pkey, &value)) {
        pos = (int) value;
        /* Found it. See if it is still valid */
        if (pos >= thdr->used)
            pos = -1;
        else {
            /* See if the element actually matches */
            ptas = *THDRELEMPTR(thdr, tas_t *, pos);
            if (!tas_equal(ptas, pkey, 0)) {
                delete_pos = pos;
                pos = -1;
            }
        }
    }

    tas_unref(pkey);

    if (pos == -1) {
        /* Do a search */
        ta_search_t search;
        search.indices = NULL;
        search.lower = 0;
        search.upper = thdr->used-1;
        search.flags = 0;
        search.cur = 0;
        search.op = TA_SEARCH_OPT_EQ;
        if (thdr_search_string(ip, thdr, objv[2], &search) == TCL_OK) {
            /* TBD - clean up search interface */
            Tcl_Obj *resultObj = Tcl_GetObjResult(ip);
            TA_NOFAIL(Tcl_GetIntFromObj(NULL, resultObj, &pos), TCL_OK);
        }
        if (pos >= 0)
            thdr_lookup_add(thdr, pos);
        else {
            /* Not in table. Remove from lookup if it necessary */
            pos = -1;
            if (delete_pos > 0)
                thdr_lookup_delete(thdr, delete_pos);
        }
    }

    Tcl_SetObjResult(ip, Tcl_NewIntObj(pos));
    return TCL_OK;
}
