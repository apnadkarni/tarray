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

TCL_RESULT ta_search_op_error(Tcl_Interp *interp, int op)
{
    if (interp) {
        const char *ops = NULL;
        if (op < (sizeof(ta_search_switches_e)/sizeof(ta_search_switches_e[0])))
            ops = ta_search_switches_e[op];
        if (ops == NULL)
            Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unknown or invalid search operator (%d).", op));
        else
            Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unknown or invalid search operator (%s).", ops));
        Tcl_SetErrorCode(interp, "TARRAY", "SEARCH", "OPER", NULL);
    }
    return TCL_ERROR;
}

static TCL_RESULT thdr_search_boolean(Tcl_Interp *interp, TAHdr * haystackP,
                                      Tcl_Obj *needleObj, int start,
                                      enum ta_search_switches_e op, int flags)
{
    int bval;
    ba_t *baP;
    int pos;
    Tcl_Obj *resultObj;

    TA_ASSERT(haystackP->type == TA_BOOLEAN);

    if (op != TA_SEARCH_OPT_EQ)
        return ta_search_op_error(interp, op);

    if (Tcl_GetBooleanFromObj(interp, needleObj, &bval) != TCL_OK)
        return TCL_ERROR;
    
    if (flags & TA_SEARCH_INVERT)
        bval = !bval;

    /* First locate the starting point for the search */
    baP = TAHDRELEMPTR(haystackP, ba_t, 0);

    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP;
        TAHdr *newP;
        thdrP = thdr_alloc(interp, 
                            flags & TA_SEARCH_INLINE ? TA_BOOLEAN : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdrP == NULL)
            return TCL_ERROR;
        pos = start;
        while ((pos = ba_find(baP, bval, pos, thdrP->used)) != -1) {
            /* Ensure enough space in target array */
            if (thdrP->used >= thdrP->allocated)
                newP = thdr_realloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
            if (newP)
                thdrP = newP;
            else {
                TAHDR_DECRREF(thdrP);
                return TCL_ERROR;
            }
            if (flags & TA_SEARCH_INLINE)
                ba_put(TAHDRELEMPTR(thdrP, ba_t, 0), thdrP->used, bval);
            else
                *TAHDRELEMPTR(thdrP, int, thdrP->used) = pos;
            thdrP->used++;
            ++pos;
        }
        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdrP); /* indices are naturally sorted */
        resultObj = tcol_new(thdrP);
    } else {
        /* Return first found element */
        pos = ba_find(baP, bval, start, haystackP->used);
        resultObj = pos == -1 ?
            Tcl_NewObj() :
            Tcl_NewIntObj((flags & TA_SEARCH_INLINE) ? bval : pos);
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}
                        
/* TBD - see how much performance is gained by separating this search function into
   type-specific functions */
static TCL_RESULT thdr_search_entier(Tcl_Interp *interp, TAHdr * haystackP,
                                     Tcl_Obj *needleObj, int start, enum ta_search_switches_e op, int flags)
{
    int offset;
    Tcl_Obj *resultObj;
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
        return ta_search_op_error(interp, op);
    }

    if (Tcl_GetWideIntFromObj(interp, needleObj, &needle) != TCL_OK)
        return TCL_ERROR;

    p = TAHDRELEMPTR(haystackP, char, 0);
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
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Integer \"%s\" type mismatch for typearray (type %d).", Tcl_GetString(needleObj), haystackP->type));
        return TCL_ERROR;
    }

    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;

    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP, *newP;

        thdrP = thdr_alloc(interp,
                            flags & TA_SEARCH_INLINE ? haystackP->type : TA_INT,
                            10);                /* Assume 10 hits TBD */
        if (thdrP == NULL)
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
                if (thdrP->used >= thdrP->allocated)
                    newP = thdr_realloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
                if (newP)
                    thdrP = newP;
                else {
                    TAHDR_DECRREF(thdrP);
                    return TCL_ERROR;
                }
                if (flags & TA_SEARCH_INLINE) {
                    switch (thdrP->type) {
                    case TA_INT:  *TAHDRELEMPTR(thdrP, int, thdrP->used) = (int) elem; break;
                    case TA_UINT: *TAHDRELEMPTR(thdrP, unsigned int, thdrP->used) = (unsigned int) elem; break;
                    case TA_WIDE: *TAHDRELEMPTR(thdrP, Tcl_WideInt, thdrP->used) = elem; break;
                    case TA_BYTE:  *TAHDRELEMPTR(thdrP, unsigned char, thdrP->used) = (unsigned char) elem; break;
                    }
                } else {
                    *TAHDRELEMPTR(thdrP, int, thdrP->used) = offset;
                }
                thdrP->used++;
            }
        }

        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdrP); /* indices are naturally sorted */
        resultObj = tcol_new(thdrP);

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
            resultObj = Tcl_NewObj();
        } else {
            if (flags & TA_SEARCH_INLINE)
                resultObj = Tcl_NewWideIntObj(elem);
            else
                resultObj = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}


static TCL_RESULT thdr_search_double(Tcl_Interp *interp, TAHdr * haystackP,
                                          Tcl_Obj *needleObj, int start, enum ta_search_switches_e op, int flags)
{
    int offset;
    Tcl_Obj *resultObj;
    double dval, *dvalP;
    int compare_result;
    int compare_wanted;

    TA_ASSERT(haystackP->type == TA_DOUBLE);

    switch (op) {
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return ta_search_op_error(interp, op);
    }

    if (Tcl_GetDoubleFromObj(interp, needleObj, &dval) != TCL_OK)
        return TCL_ERROR;
    
    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;

    /* First locate the starting point for the search */
    dvalP = TAHDRELEMPTR(haystackP, double, start);

    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP, *newP;

        thdrP = thdr_alloc(interp,
                            flags & TA_SEARCH_INLINE ? TA_DOUBLE : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdrP == NULL)
            return TCL_ERROR;

        for (offset = start; offset < haystackP->used; ++offset, ++dvalP) {
            switch (op) {
            case TA_SEARCH_OPT_GT: compare_result = (*dvalP > dval); break;
            case TA_SEARCH_OPT_LT: compare_result = (*dvalP < dval); break;
            case TA_SEARCH_OPT_EQ: compare_result = (*dvalP == dval); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdrP->used >= thdrP->allocated)
                    newP = thdr_realloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
                if (newP)
                    thdrP = newP;
                else {
                    TAHDR_DECRREF(thdrP);
                    return TCL_ERROR;
                }
                if (flags & TA_SEARCH_INLINE) {
                    *TAHDRELEMPTR(thdrP, double, thdrP->used) = *dvalP;
                } else {
                    *TAHDRELEMPTR(thdrP, int, thdrP->used) = offset;
                }
                thdrP->used++;
            }
        }

        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdrP); /* indices are naturally sorted */
        resultObj = tcol_new(thdrP);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++dvalP) {
            switch (op) {
            case TA_SEARCH_OPT_GT: compare_result = (*dvalP > dval); break;
            case TA_SEARCH_OPT_LT: compare_result = (*dvalP < dval); break;
            case TA_SEARCH_OPT_EQ: compare_result = (*dvalP == dval); break;
            }
            if (compare_result == compare_wanted)
                break;
        }
        if (offset >= haystackP->used) {
            /* No match */
            resultObj = Tcl_NewObj();
        } else {
            if (flags & TA_SEARCH_INLINE)
                resultObj = Tcl_NewDoubleObj(*dvalP);
            else
                resultObj = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}

static TCL_RESULT thdr_search_obj(Tcl_Interp *interp, TAHdr * haystackP,
                                  Tcl_Obj *needleObj, int start, enum ta_search_switches_e op, int flags)
{
    int offset;
    Tcl_Obj **objPP;
    Tcl_Obj *resultObj;
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
            re = Tcl_GetRegExpFromObj(interp, needleObj,
                                      TCL_REG_ADVANCED|(nocase ? TCL_REG_NOCASE : 0));
            if (re == NULL)
                return TCL_ERROR;
        }
        break;
    default:
        return ta_search_op_error(interp, op);
    }

    /* First locate the starting point for the search */
    objPP = TAHDRELEMPTR(haystackP, Tcl_Obj *, start);


    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP, *newP;

        thdrP = thdr_alloc(interp,
                            flags & TA_SEARCH_INLINE ? TA_OBJ : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdrP == NULL)
            return TCL_ERROR;

        for (offset = start; offset < haystackP->used; ++offset, ++objPP) {
            switch (op) {
            case TA_SEARCH_OPT_GT:
                compare_result = ta_obj_compare(*objPP, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(*objPP, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_compare(*objPP, needleObj, nocase) == 0; break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(*objPP),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExecObj(interp, re, *objPP,
                                                   0, 0, 0);
                if (compare_result < 0) {
                    TAHDR_DECRREF(thdrP); /* Note this unrefs embedded Tcl_Objs if needed */
                    return TCL_ERROR;
                }
                break;
            }
            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdrP->used >= thdrP->allocated)
                    newP = thdr_realloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
                if (newP)
                    thdrP = newP;
                else {
                    TAHDR_DECRREF(thdrP);
                    return TCL_ERROR;
                }
                if (flags & TA_SEARCH_INLINE) {
                    Tcl_IncrRefCount(*objPP);
                    *TAHDRELEMPTR(thdrP, Tcl_Obj *, thdrP->used) = *objPP;
                } else {
                    *TAHDRELEMPTR(thdrP, int, thdrP->used) = offset;
                }
                thdrP->used++;
            }
        }

        if ((flags & TA_SEARCH_INLINE) == 0)
            thdr_mark_sorted_ascending(thdrP); /* indices are naturally sorted */
        resultObj = tcol_new(thdrP);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++objPP) {
            switch (op) {
            case TA_SEARCH_OPT_GT:
                compare_result = ta_obj_compare(*objPP, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = ta_obj_compare(*objPP, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = ta_obj_compare(*objPP, needleObj, nocase) == 0; break;
            case TA_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(*objPP),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TA_SEARCH_OPT_RE:
                compare_result = Tcl_RegExpExecObj(interp, re, *objPP,
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
            resultObj = Tcl_NewObj();
        } else {
            if (flags & TA_SEARCH_INLINE)
                resultObj = *objPP; /* No need to incr ref, the SetObjResult does it */
            else
                resultObj = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}

TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *interp,
                              int objc, Tcl_Obj *const objv[])
{
    int flags;
    int start_index;
    int i, n, opt;
    TAHdr *haystackP;
    enum ta_search_switches_e op;

    if (objc < 3) {
	Tcl_WrongNumArgs(interp, 1, objv, "?options? tarray pattern");
	return TCL_ERROR;
    }

    if (tcol_convert(interp, objv[objc-2]) != TCL_OK)
        return TCL_ERROR;
    haystackP = TARRAYHDR(objv[objc-2]);
    flags = 0;
    start_index = 0;
    op = TA_SEARCH_OPT_EQ;
    for (i = 1; i < objc-2; ++i) {
	if (Tcl_GetIndexFromObj(interp, objv[i], ta_search_switches_e, "option", 0, &opt)
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
                return ta_missing_arg_error(interp, "-start");
            ++i;
            /*
             * To prevent shimmering, check if the index object is same
             * as tarray object.
             */
            if (objv[i] == objv[objc-2]) {
                Tcl_Obj *dupObj = Tcl_DuplicateObj(objv[i]);
                n = Tcl_GetIntFromObj(interp, dupObj, &start_index);
                Tcl_DecrRefCount(dupObj);
                if (n != TCL_OK)
                    return TCL_ERROR;
            } else {
                if (Tcl_GetIntFromObj(interp, objv[i], &start_index) != TCL_OK)
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
        return thdr_search_boolean(interp, haystackP, objv[objc-1], start_index,op,flags);
    case TA_INT:
    case TA_UINT:
    case TA_BYTE:
    case TA_WIDE:
        return thdr_search_entier(interp, haystackP, objv[objc-1], start_index, op, flags);
    case TA_DOUBLE:
        return thdr_search_double(interp, haystackP, objv[objc-1], start_index, op, flags);
    case TA_OBJ:
        return thdr_search_obj(interp, haystackP, objv[objc-1], start_index, op, flags);
    default:
        Tcl_SetResult(interp, "Not implemented", TCL_STATIC);
        return TCL_ERROR;
    }

}
