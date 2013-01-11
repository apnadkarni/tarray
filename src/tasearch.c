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
    "-all", "-inline", "-not", "-start", "-eq", "-gt", "-lt", "-pat", "-re", "-nocase", NULL
};
enum ta_search_switches_e {
    TA_SEARCH_OPT_ALL, TA_SEARCH_OPT_INLINE, TA_SEARCH_OPT_INVERT, TA_SEARCH_OPT_START, TA_SEARCH_OPT_EQ, TA_SEARCH_OPT_GT, TA_SEARCH_OPT_LT, TA_SEARCH_OPT_PAT, TA_SEARCH_OPT_RE, TA_SEARCH_OPT_NOCASE
};
/* Search flags */
#define TA_SEARCH_INLINE 1  /* Return values, not indices */
#define TA_SEARCH_INVERT 2  /* Invert matching expression */
#define TA_SEARCH_ALL    4  /* Return all matches */
#define TA_SEARCH_NOCASE 8  /* Ignore case */

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

static TCL_RESULT thdr_search_boolean(Tcl_Interp *ip, thdr_t * haystackP,
                                      Tcl_Obj *needleObj, int start,
                                      enum ta_search_switches_e op, int flags)
{
    int bval;
    ba_t *baP;
    int pos;
    Tcl_Obj *oresult;

    TA_ASSERT(haystackP->type == TA_BOOLEAN);

    if (op != TA_SEARCH_OPT_EQ)
        return ta_search_op_error(ip, op);

    if (Tcl_GetBooleanFromObj(ip, needleObj, &bval) != TCL_OK)
        return TCL_ERROR;
    
    if (flags & TA_SEARCH_INVERT)
        bval = !bval;

    /* First locate the starting point for the search */
    baP = THDRELEMPTR(haystackP, ba_t, 0);

    if (flags & TA_SEARCH_ALL) {
        thdr_t *thdr;
        thdr_t *newP;
        thdr = thdr_alloc(ip, 
                            flags & TA_SEARCH_INLINE ? TA_BOOLEAN : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdr == NULL)
            return TCL_ERROR;
        pos = start;
        while ((pos = ba_find(baP, bval, pos, thdr->used)) != -1) {
            /* Ensure enough space in target array */
            if (thdr->used >= thdr->allocated)
                newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
            if (newP)
                thdr = newP;
            else {
                thdr_decr_refs(thdr);
                return TCL_ERROR;
            }
            if (flags & TA_SEARCH_INLINE)
                ba_put(THDRELEMPTR(thdr, ba_t, 0), thdr->used, bval);
            else
                *THDRELEMPTR(thdr, int, thdr->used) = pos;
            thdr->used++;
            ++pos;
        }
        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdr); /* indices are naturally sorted */
        oresult = tcol_new(thdr);
    } else {
        /* Return first found element */
        pos = ba_find(baP, bval, start, haystackP->used);
        oresult = pos == -1 ?
            Tcl_NewObj() :
            Tcl_NewIntObj((flags & TA_SEARCH_INLINE) ? bval : pos);
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}
                        
/* TBD - see how much performance is gained by separating this search function into
   type-specific functions */
static TCL_RESULT thdr_search_entier(Tcl_Interp *ip, thdr_t * haystackP,
                                     Tcl_Obj *needleObj, int start, enum ta_search_switches_e op, int flags)
{
    int offset;
    Tcl_Obj *oresult;
    Tcl_WideInt needle, elem, min_val, max_val;
    int compare_result;
    int compare_wanted;
    int elem_size;
    char *p;

    switch (op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return ta_search_op_error(ip, op);
    }

    if (Tcl_GetWideIntFromObj(ip, needleObj, &needle) != TCL_OK)
        return TCL_ERROR;

    p = THDRELEMPTR(haystackP, char, 0);
    switch (haystackP->type) {
    case TA_INT:
        max_val = INT_MAX;
        min_val = INT_MIN;
        p += start * sizeof(int);
        elem_size = sizeof(int);
        break;
    case TA_UINT:
        max_val = UINT_MAX;
        min_val = 0;
        p += start * sizeof(unsigned int);
        elem_size = sizeof(unsigned int);
        break;
    case TA_WIDE:
        max_val = needle; /* No-op */
        min_val = needle;
        p += start * sizeof(Tcl_WideInt);
        elem_size = sizeof(Tcl_WideInt);
        break;
    case TA_BYTE:
        max_val = UCHAR_MAX;
        min_val = 0;
        p += start * sizeof(unsigned char);
        elem_size = sizeof(unsigned char);
        break;
    default:
        ta_type_panic(haystackP->type);
    }

    if (needle > max_val || needle < min_val) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Integer \"%s\" type mismatch for typearray (type %d).", Tcl_GetString(needleObj), haystackP->type));
        return TCL_ERROR;
    }

    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;

    if (flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            flags & TA_SEARCH_INLINE ? haystackP->type : TA_INT,
                            10);                /* Assume 10 hits TBD */
        if (thdr == NULL)
            return TCL_ERROR;

        for (offset = start; offset < haystackP->used; ++offset, p += elem_size) {
            switch (haystackP->type) {
            case TA_INT:  elem = *(int *)p; break;
            case TA_UINT: elem = *(unsigned int *)p; break;
            case TA_WIDE: elem = *(Tcl_WideInt *)p; break;
            case TA_BYTE: elem = *(unsigned char *)p; break;
            }
            switch (op) {
            case TA_SEARCH_OPT_GT: compare_result = (elem > needle); break;
            case TA_SEARCH_OPT_LT: compare_result = (elem < needle); break;
            case TA_SEARCH_OPT_EQ:
            default: compare_result = (elem == needle); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->allocated)
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                if (newP)
                    thdr = newP;
                else {
                    thdr_decr_refs(thdr);
                    return TCL_ERROR;
                }
                if (flags & TA_SEARCH_INLINE) {
                    switch (thdr->type) {
                    case TA_INT:  *THDRELEMPTR(thdr, int, thdr->used) = (int) elem; break;
                    case TA_UINT: *THDRELEMPTR(thdr, unsigned int, thdr->used) = (unsigned int) elem; break;
                    case TA_WIDE: *THDRELEMPTR(thdr, Tcl_WideInt, thdr->used) = elem; break;
                    case TA_BYTE:  *THDRELEMPTR(thdr, unsigned char, thdr->used) = (unsigned char) elem; break;
                    }
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = offset;
                }
                thdr->used++;
            }
        }

        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdr); /* indices are naturally sorted */
        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, p += elem_size) {
            switch (haystackP->type) {
            case TA_INT:  elem = *(int *)p; break;
            case TA_UINT: elem = *(unsigned int *)p; break;
            case TA_WIDE: elem = *(Tcl_WideInt *)p; break;
            case TA_BYTE: elem = *(unsigned char *)p; break;
            }
            switch (op) {
            case TA_SEARCH_OPT_GT: compare_result = (elem > needle); break;
            case TA_SEARCH_OPT_LT: compare_result = (elem < needle); break;
            case TA_SEARCH_OPT_EQ:
            default: compare_result = (elem == needle); break;
            }
            if (compare_result == compare_wanted)
                break;
        }
        if (offset >= haystackP->used) {
            /* No match */
            oresult = Tcl_NewObj();
        } else {
            if (flags & TA_SEARCH_INLINE)
                oresult = Tcl_NewWideIntObj(elem);
            else
                oresult = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}


static TCL_RESULT thdr_search_double(Tcl_Interp *ip, thdr_t * haystackP,
                                          Tcl_Obj *needleObj, int start, enum ta_search_switches_e op, int flags)
{
    int offset;
    Tcl_Obj *oresult;
    double dval, *pdbl;
    int compare_result;
    int compare_wanted;

    TA_ASSERT(haystackP->type == TA_DOUBLE);

    switch (op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return ta_search_op_error(ip, op);
    }

    if (Tcl_GetDoubleFromObj(ip, needleObj, &dval) != TCL_OK)
        return TCL_ERROR;
    
    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;

    /* First locate the starting point for the search */
    pdbl = THDRELEMPTR(haystackP, double, start);

    if (flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            flags & TA_SEARCH_INLINE ? TA_DOUBLE : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdr == NULL)
            return TCL_ERROR;

        for (offset = start; offset < haystackP->used; ++offset, ++pdbl) {
            switch (op) {
            case TA_SEARCH_OPT_GT: compare_result = (*pdbl > dval); break;
            case TA_SEARCH_OPT_LT: compare_result = (*pdbl < dval); break;
            case TA_SEARCH_OPT_EQ: compare_result = (*pdbl == dval); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdr->used >= thdr->allocated)
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                if (newP)
                    thdr = newP;
                else {
                    thdr_decr_refs(thdr);
                    return TCL_ERROR;
                }
                if (flags & TA_SEARCH_INLINE) {
                    *THDRELEMPTR(thdr, double, thdr->used) = *pdbl;
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = offset;
                }
                thdr->used++;
            }
        }

        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdr); /* indices are naturally sorted */
        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++pdbl) {
            switch (op) {
            case TA_SEARCH_OPT_GT: compare_result = (*pdbl > dval); break;
            case TA_SEARCH_OPT_LT: compare_result = (*pdbl < dval); break;
            case TA_SEARCH_OPT_EQ: compare_result = (*pdbl == dval); break;
            }
            if (compare_result == compare_wanted)
                break;
        }
        if (offset >= haystackP->used) {
            /* No match */
            oresult = Tcl_NewObj();
        } else {
            if (flags & TA_SEARCH_INLINE)
                oresult = Tcl_NewDoubleObj(*pdbl);
            else
                oresult = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}

static TCL_RESULT thdr_search_obj(Tcl_Interp *ip, thdr_t * haystackP,
                                  Tcl_Obj *needleObj, int start, enum ta_search_switches_e op, int flags)
{
    int offset;
    Tcl_Obj **pobjs;
    Tcl_Obj *oresult;
    int compare_result;
    int compare_wanted;
    int nocase;
    Tcl_RegExp re;

    /* TBD - do we need to increment the haystacP ref to guard against shimmering */
    TA_ASSERT(haystackP->type == TA_OBJ);
    
    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;
    nocase = flags & TA_SEARCH_NOCASE;

    switch (op) {
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
        return ta_search_op_error(ip, op);
    }

    /* First locate the starting point for the search */
    pobjs = THDRELEMPTR(haystackP, Tcl_Obj *, start);


    if (flags & TA_SEARCH_ALL) {
        thdr_t *thdr, *newP;

        thdr = thdr_alloc(ip,
                            flags & TA_SEARCH_INLINE ? TA_OBJ : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdr == NULL)
            return TCL_ERROR;

        for (offset = start; offset < haystackP->used; ++offset, ++pobjs) {
            switch (op) {
            case TA_SEARCH_OPT_GT:
                compare_result = ta_obj_compare(*pobjs, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(*pobjs, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_compare(*pobjs, needleObj, nocase) == 0; break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(*pobjs),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExecObj(ip, re, *pobjs,
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
                if (thdr->used >= thdr->allocated)
                    newP = thdr_realloc(ip, thdr, thdr->used + TA_EXTRA(thdr->used));
                if (newP)
                    thdr = newP;
                else {
                    thdr_decr_refs(thdr);
                    return TCL_ERROR;
                }
                if (flags & TA_SEARCH_INLINE) {
                    Tcl_IncrRefCount(*pobjs);
                    *THDRELEMPTR(thdr, Tcl_Obj *, thdr->used) = *pobjs;
                } else {
                    *THDRELEMPTR(thdr, int, thdr->used) = offset;
                }
                thdr->used++;
            }
        }

        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdr); /* indices are naturally sorted */
        oresult = tcol_new(thdr);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++pobjs) {
            switch (op) {
            case TA_SEARCH_OPT_GT:
                compare_result = ta_obj_compare(*pobjs, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(*pobjs, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_compare(*pobjs, needleObj, nocase) == 0; break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(*pobjs),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExecObj(ip, re, *pobjs,
                                                   0, 0, 0);
                if (compare_result < 0)
                    return TCL_ERROR;
                break;
            }
            if (compare_result == compare_wanted)
                break;
        }
        if (offset >= haystackP->used) {
            /* No match */
            oresult = Tcl_NewObj();
        } else {
            if (flags & TA_SEARCH_INLINE)
                oresult = *pobjs; /* No need to incr ref, the SetObjResult does it */
            else
                oresult = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(ip, oresult);
    return TCL_OK;
}

TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    int flags;
    int start_index;
    int i, n, opt;
    thdr_t *haystackP;
    enum ta_search_switches_e op;

    if (objc < 3) {
	Tcl_WrongNumArgs(ip, 1, objv, "?options? tarray pattern");
	return TCL_ERROR;
    }

    if (tcol_convert(ip, objv[objc-2]) != TCL_OK)
        return TCL_ERROR;
    haystackP = TARRAYHDR(objv[objc-2]);
    flags = 0;
    start_index = 0;
    op = TA_SEARCH_OPT_EQ;
    for (i = 1; i < objc-2; ++i) {
	if (Tcl_GetIndexFromObj(ip, objv[i], ta_search_switches_e, "option", 0, &opt)
            != TCL_OK) {
            return TCL_ERROR;
	}
        switch ((enum ta_search_switches_e) opt) {
        case TA_SEARCH_OPT_ALL: flags |= TA_SEARCH_ALL; break;
        case TA_SEARCH_OPT_INLINE: flags |= TA_SEARCH_INLINE; break;
        case TA_SEARCH_OPT_INVERT: flags |= TA_SEARCH_INVERT; break;
        case TA_SEARCH_OPT_NOCASE: flags |= TA_SEARCH_NOCASE; break;
        case TA_SEARCH_OPT_START:
            if (i > objc-4)
                return ta_missing_arg_error(ip, "-start");
            ++i;
            /*
             * To prevent shimmering, check if the index object is same
             * as tarray object.
             */
            if (objv[i] == objv[objc-2]) {
                Tcl_Obj *dupObj = Tcl_DuplicateObj(objv[i]);
                n = Tcl_GetIntFromObj(ip, dupObj, &start_index);
                Tcl_DecrRefCount(dupObj);
                if (n != TCL_OK)
                    return TCL_ERROR;
            } else {
                if (Tcl_GetIntFromObj(ip, objv[i], &start_index) != TCL_OK)
                    return TCL_ERROR;
            }
            break;
        case TA_SEARCH_OPT_EQ:
        case TA_SEARCH_OPT_GT:
        case TA_SEARCH_OPT_LT:
        case TA_SEARCH_OPT_PAT:
        case TA_SEARCH_OPT_RE:
            op = (enum ta_search_switches_e) opt;
        }
    }

    switch (haystackP->type) {
    case TA_BOOLEAN:
        return thdr_search_boolean(ip, haystackP, objv[objc-1], start_index,op,flags);
    case TA_INT:
    case TA_UINT:
    case TA_BYTE:
    case TA_WIDE:
        return thdr_search_entier(ip, haystackP, objv[objc-1], start_index, op, flags);
    case TA_DOUBLE:
        return thdr_search_double(ip, haystackP, objv[objc-1], start_index, op, flags);
    case TA_OBJ:
        return thdr_search_obj(ip, haystackP, objv[objc-1], start_index, op, flags);
    default:
        Tcl_SetResult(ip, "Not implemented", TCL_STATIC);
        return TCL_ERROR;
    }

}
