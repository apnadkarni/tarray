/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include "tarray.h"

/*
 * Options for 'tarray search'
 */
static const char *ta_search_switches_e[] = {
    "-all", "-inline", "-not", "-range", "-eq", "-gt", "-lt", "-pat", "-re", "-nocase", NULL
};
enum ta_search_switches_e {
    TA_SEARCH_OPT_ALL, TA_SEARCH_OPT_INLINE, TA_SEARCH_OPT_INVERT, TA_SEARCH_OPT_RANGE, TA_SEARCH_OPT_EQ, TA_SEARCH_OPT_GT, TA_SEARCH_OPT_LT, TA_SEARCH_OPT_PAT, TA_SEARCH_OPT_RE, TA_SEARCH_OPT_NOCASE
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
TA_INLINE int ta_search_calc_slot(ta_search_t *psearch)
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
                        
/* TBD - see how much performance is gained by separating this search function into
   type-specific functions */
static TCL_RESULT thdr_search_entier(Tcl_Interp *ip, thdr_t * haystackP,
                                     Tcl_Obj *needleObj, ta_search_t *psearch)
{
    int slot;
    Tcl_Obj *oresult;
    Tcl_WideInt needle, elem, min_val, max_val;
    int compare_result;
    int compare_wanted;
    void *p;

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

    TA_ASSERT(haystackP->type == TA_DOUBLE);

    switch (psearch->op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return ta_search_op_error(ip, psearch->op);
    }

    if (Tcl_GetDoubleFromObj(ip, needleObj, &dneedle) != TCL_OK)
        return TCL_ERROR;
    
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

    /* TBD - do we need to increment the haystacP ref to guard against shimmering */
    TA_ASSERT(haystackP->type == TA_ANY);
    
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
                compare_result = ta_obj_compare(pobj, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(pobj, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_compare(pobj, needleObj, nocase) == 0; break;
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
                compare_result = ta_obj_compare(pobj, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(pobj, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_compare(pobj, needleObj, nocase) == 0; break;
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

TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    int i, n, opt, endval;
    thdr_t *haystackP;
    ta_search_t search;
    Tcl_Obj *orange;
    Tcl_Obj **range;

    if (objc < 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "?options? tarray pattern");
	return TCL_ERROR;
    }

    if (tcol_convert(ip, objv[objc-2]) != TCL_OK)
        return TCL_ERROR;

    haystackP = TARRAYHDR(objv[objc-2]);
    search.indices = NULL;
    search.flags = 0;
    search.lower = 0;
    search.upper = haystackP->used - 1;
    search.op = TA_SEARCH_OPT_EQ;
    for (i = 1; i < objc-2; ++i) {
	if (Tcl_GetIndexFromObj(ip, objv[i], ta_search_switches_e, "option", 0, &opt)
            != TCL_OK) {
            return TCL_ERROR;
	}
        switch ((enum ta_search_switches_e) opt) {
        case TA_SEARCH_OPT_ALL: search.flags |= TA_SEARCH_ALL; break;
        case TA_SEARCH_OPT_INLINE: search.flags |= TA_SEARCH_INLINE; break;
        case TA_SEARCH_OPT_INVERT: search.flags |= TA_SEARCH_INVERT; break;
        case TA_SEARCH_OPT_NOCASE: search.flags |= TA_SEARCH_NOCASE; break;
        case TA_SEARCH_OPT_RANGE:
            if (i > objc-4)
                return ta_missing_arg_error(ip, "-start");
            ++i;
            /*
             * To prevent shimmering, check if the index object is same
             * as tarray object.
             */
            if (objv[i] == objv[objc-2])
                orange = Tcl_DuplicateObj(objv[i]);
            else {
                orange = objv[i];
                Tcl_IncrRefCount(orange);
            }
            if (Tcl_ListObjGetElements(ip, orange, &n, &range) != TCL_OK)
                return TCL_ERROR;
            /* Note if # elements (n) is 0, the defaults are used */
            endval = haystackP->used - 1;
            if (n > 2
                || (n > 0
                    &&
                    (ta_convert_index(ip, range[0], &search.lower, endval, INT_MIN, INT_MAX) != TCL_OK
                     || (n > 1
                         && ta_convert_index(ip, range[1], &search.upper, endval, INT_MIN, INT_MAX) != TCL_OK)))) {
                ta_invalid_range_error(ip, orange);
                Tcl_DecrRefCount(orange);
                return TCL_ERROR;
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
    case TA_BOOLEAN:
        return thdr_search_boolean(ip, haystackP, objv[objc-1], &search);
    case TA_INT:
    case TA_UINT:
    case TA_BYTE:
    case TA_WIDE:
        return thdr_search_entier(ip, haystackP, objv[objc-1], &search);
    case TA_DOUBLE:
        return thdr_search_double(ip, haystackP, objv[objc-1], &search);
    case TA_ANY:
        return thdr_search_obj(ip, haystackP, objv[objc-1], &search);
    default:
        ta_type_panic(haystackP->type);
        return TCL_ERROR;       /* To avoid compiler warning about return */
    }
}
