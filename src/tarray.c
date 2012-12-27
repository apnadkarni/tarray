/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include <string.h>
#include "tcl.h"
#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#define TARRAY_ENABLE_ASSERT 1
#include "tarray.h"

/*
 * TArray is a Tcl "type" used for densely storing arrays of elements
 * of a specific type.
 */
static void TArrayTypeDupObj(Tcl_Obj *srcP, Tcl_Obj *dstP);
static void TArrayTypeFreeRep(Tcl_Obj *objP);
static void TArrayTypeUpdateStringRep(Tcl_Obj *objP);
struct Tcl_ObjType gTArrayType = {
    "TArray",
    TArrayTypeFreeRep,
    TArrayTypeDupObj,
    TArrayTypeUpdateStringRep,
    NULL,     /* jenglish advises to keep this NULL */
};



/* Must match definitions in tarray.h ! */
const char *gTArrayTypeTokens[] = {
    "boolean",
    "int",
    "uint",
    "wide",
    "double",
    "byte",
    "tclobj",
    NULL
};    

/*
 * Options for 'tarray search'
 */
static const char *TArraySearchSwitches[] = {
    "-all", "-inline", "-not", "-start", "-eq", "-gt", "-lt", "-pat", "-re", "-nocase", NULL
};
enum TArraySearchSwitches {
    TARRAY_SEARCH_OPT_ALL, TARRAY_SEARCH_OPT_INLINE, TARRAY_SEARCH_OPT_INVERT, TARRAY_SEARCH_OPT_START, TARRAY_SEARCH_OPT_EQ, TARRAY_SEARCH_OPT_GT, TARRAY_SEARCH_OPT_LT, TARRAY_SEARCH_OPT_PAT, TARRAY_SEARCH_OPT_RE, TARRAY_SEARCH_OPT_NOCASE
};
/* Search flags */
#define TARRAY_SEARCH_INLINE 1  /* Return values, not indices */
#define TARRAY_SEARCH_INVERT 2  /* Invert matching expression */
#define TARRAY_SEARCH_ALL    4  /* Return all matches */
#define TARRAY_SEARCH_NOCASE 8  /* Ignore case */

void TArrayStringOverflowPanic(const char *where)
{
    Tcl_Panic("Max size for a Tcl value (%d bytes) exceeded in %s", INT_MAX, where ? where : "unknown function");
}

void TArrayTypePanic(unsigned char tatype)
{
    Tcl_Panic("Unknown or unexpected tarray type %d", tatype);
}

void TArraySharedPanic(const char *where)
{
    Tcl_Panic("Shared TAHdr passed for modification to %s.", where);
}

void TArrayTooSmallPanic(TAHdr *thdrP, const char *where)
{
    Tcl_Panic("Insufficient space in TAHdr (allocated %d) in %s.", thdrP->allocated, where);
}

TCL_RESULT TArrayBadArgError(Tcl_Interp *interp, const char *optname)
{
    if (interp) {
        Tcl_SetObjResult(interp, Tcl_ObjPrintf("Missing or invalid argument to option '%s'", optname));
        Tcl_SetErrorCode(interp, "TARRAY", "ARGUMENT", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayNotTArrayError(Tcl_Interp *interp)
{
    if (interp) {
        Tcl_SetResult(interp, "Object is not a TArray", TCL_STATIC);
        Tcl_SetErrorCode(interp, "TARRAY", "OBJTYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayBadSearchOpError(Tcl_Interp *interp, int op)
{
    if (interp) {
        const char *ops = NULL;
        if (op < (sizeof(TArraySearchSwitches)/sizeof(TArraySearchSwitches[0])))
            ops = TArraySearchSwitches[op];
        if (ops == NULL)
            Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unknown or invalid search operator (%d).", op));
        else
            Tcl_SetObjResult(interp, Tcl_ObjPrintf("Unknown or invalid search operator (%s).", ops));
        Tcl_SetErrorCode(interp, "TARRAY", "SEARCH", "OPER", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayIndexRangeError(Tcl_Interp *interp, Tcl_Obj *indexObj)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("tarray index %s out of bounds", Tcl_GetString(indexObj)));
        Tcl_SetErrorCode(interp, "TARRAY", "INDEX", "RANGE", NULL);
    }
    return TCL_ERROR;
}

/*
 * Map numeric or string index to numeric integer index.
 * Does NOT check for out of bounds errors, caller must do that
 */
TCL_RESULT IndexToInt(Tcl_Interp *interp, Tcl_Obj *objP, int *indexP, int end_value)
{
    char *s;

    if (Tcl_GetIntFromObj(NULL, objP, indexP) != TCL_OK) {
        s = Tcl_GetString(objP);
        if (strcmp(s, "end")) {
            if (interp != NULL) {
                Tcl_SetObjResult(interp, Tcl_ObjPrintf(
                                     "bad index '%s': must be 'end' or an integer value", s));
                Tcl_SetErrorCode(interp, "TARRAY", "VALUE", "INDEX", NULL);
            }
            return TCL_ERROR;
        }
        *indexP = end_value;
    }
    return TCL_OK;
}

TCL_RESULT RationalizeRangeIndices(Tcl_Interp *interp, TAHdr *thdrP, Tcl_Obj *lowObj, Tcl_Obj *highObj, int *lowP, int *countP)
{
    int low, high;

    if (IndexToInt(interp, lowObj, &low, thdrP->used-1) != TCL_OK)
        return TCL_ERROR;

    if (IndexToInt(interp, highObj, &high, thdrP->used-1) != TCL_OK)
        return TCL_ERROR;

    /* We allow low index to be 1 greater than last element. Caller should
     * check for this if appropriate. High index can be any greater value
     * than lower range.
     */
    if (low < 0 || low > thdrP->used || high < low) {
        Tcl_SetResult(interp, "tarray index out of range.", TCL_STATIC);
        return TCL_ERROR;
    }

    *lowP = low;
    *countP = high - low + 1;
    return TCL_OK;
}

TCL_RESULT TArrayValueFromObj(Tcl_Interp *interp, Tcl_Obj *objP,
                              unsigned char tatype, TArrayValue *tavP)
{
    int i;
    switch (tatype) {
    case TARRAY_BOOLEAN:
        if (Tcl_GetBooleanFromObj(interp, objP, &i) != TCL_OK)
            return TCL_ERROR;
        tavP->bval = (i != 0);
        break;
    case TARRAY_BYTE:
    case TARRAY_INT:
        if (Tcl_GetIntFromObj(interp, objP, &tavP->ival) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TARRAY_INT)
            break;
        if (tavP->ival > 255 || tavP->ival < 0) {
            if (interp)
                Tcl_SetObjResult(interp,
                                 Tcl_ObjPrintf("Integer \"%d\" does not fit type \"byte\" typearray.", tavP->ival));
            return TCL_ERROR;
        }
        tavP->ucval = (unsigned char) tavP->ival;
        break;
    case TARRAY_UINT:
    case TARRAY_WIDE:
        if (Tcl_GetWideIntFromObj(interp, objP, &tavP->wval) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TARRAY_WIDE)
            break;
        if (tavP->wval < 0 || tavP->wval > 0xFFFFFFFF) {
            if (interp)
                Tcl_SetObjResult(interp,
                                 Tcl_ObjPrintf("Integer \"%s\" too large for type \"uint\" typearray.", Tcl_GetString(objP)));
            return TCL_ERROR;
        }
        tavP->uival = (unsigned int) tavP->wval;
        break;
    case TARRAY_DOUBLE:
        if (Tcl_GetDoubleFromObj(interp, objP, &tavP->dval) != TCL_OK)
            return TCL_ERROR;
        break;
    case TARRAY_OBJ:
        tavP->oval = objP;
        break;
    default:
        TArrayTypePanic(tatype);
    }

    tavP->type = tatype;
    return TCL_OK;
}

/*
 * Set the value of an element range at a position in a TAHdr.
 * See the asserts below for conditions under which this can be called
 */
void TAHdrFill(Tcl_Interp *interp, TAHdr *thdrP,
                   const TArrayValue *tavP, int pos, int count)
{
    int i;

    TARRAY_ASSERT(! TAHDR_SHARED(thdrP));
    TARRAY_ASSERT((pos+count) <= thdrP->allocated);
    TARRAY_ASSERT(pos <= thdrP->used);
    TARRAY_ASSERT(thdrP->type == tavP->type);

    switch (thdrP->type) {
    case TARRAY_BOOLEAN:
        ba_fill(TAHDRELEMPTR(thdrP, ba_t, 0), pos, count, tavP->bval);
        break;
    case TARRAY_INT:
    case TARRAY_UINT:
        if (tavP->ival == 0) {
            memset(TAHDRELEMPTR(thdrP, int, pos), 0, count*sizeof(int));
        } else {
            int *iP;
            iP = TAHDRELEMPTR(thdrP, int, pos);
            for (i = 0; i < count; ++i, ++iP)
                *iP = tavP->ival;
        }
        break;
        
    case TARRAY_BYTE:
        memset(TAHDRELEMPTR(thdrP, unsigned char, pos), tavP->ucval, count);
        break;

    case TARRAY_WIDE:
        if (tavP->wval == 0) {
            memset(TAHDRELEMPTR(thdrP, Tcl_WideInt, pos), 0, count*sizeof(Tcl_WideInt));
        } else {
            Tcl_WideInt *wideP;
            wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, pos);
            for (i = 0; i < count; ++i, ++wideP)
                *wideP = tavP->wval;
        }
        break;
    case TARRAY_DOUBLE:
        {
            double *dvalP;
            dvalP = TAHDRELEMPTR(thdrP, double, pos);
            for (i = 0; i < count; ++i, ++dvalP)
                *dvalP = tavP->dval;
        }
        break;
    case TARRAY_OBJ:
        {
            Tcl_Obj **objPP;
            int n;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts.
             */
            /* First loop overwriting existing elements */
            n = pos + count;
            if (n > thdrP->used)
                n = thdrP->used;
            objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, pos);
            for (i = pos; i < n; ++i) {
                /* Be careful of the order */
                Tcl_IncrRefCount(tavP->oval);
                Tcl_DecrRefCount(*objPP);
                *objPP = tavP->oval;
            }

            /* Now loop over new elements being appended */
            for (; i < pos+count; ++i) {
                Tcl_IncrRefCount(tavP->oval);
                *objPP = tavP->oval;
            }
        }
        break;
    default:
        TArrayTypePanic(thdrP->type);
    }

    if ((pos + count) > thdrP->used)
        thdrP->used = pos + count;
}


/* Increments the ref counts of Tcl_Objs in a tarray making sure not
   to run past end of array */
void TArrayIncrObjRefs(TAHdr *thdrP, int first, int count)
{
    register int i;
    register Tcl_Obj **objPP;

    if (thdrP->type == TARRAY_OBJ) {
        if ((first + count) > thdrP->used)
            count = thdrP->used - first;
        if (count <= 0)
            return;
        objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, first);
        for (i = 0; i < count; ++i, ++objPP) {
            Tcl_IncrRefCount(*objPP);
        }
    }
}

/* Decrements the ref counts of Tcl_Objs in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void TArrayDecrObjRefs(TAHdr *thdrP, int first, int count)
{
    register int i;
    register Tcl_Obj **objPP;

    if (thdrP->type == TARRAY_OBJ) {
        if ((first + count) > thdrP->used)
            count = thdrP->used - first;
        if (count <= 0)
            return;
        objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, first);
        for (i = 0; i < count; ++i, ++objPP) {
            Tcl_DecrRefCount(*objPP);
        }
    }
}

void TAHdrFree(TAHdr *thdrP)
{
    if (thdrP->type == TARRAY_OBJ) {
        TArrayDecrObjRefs(thdrP, 0, thdrP->used);
    }
    TARRAY_FREEMEM(thdrP);
}


static void TArrayTypeFreeRep(Tcl_Obj *objP)
{
    TAHdr *thdrP;

    TARRAY_ASSERT(objP->typePtr == &gTArrayType);

    thdrP = TARRAYHDR(objP); 
    TARRAY_ASSERT(thdrP);

    TAHDR_DECRREF(thdrP);
    TARRAYHDR(objP) = NULL;
    objP->typePtr = NULL;
}

static void TArrayTypeDupObj(Tcl_Obj *srcObj, Tcl_Obj *dstObj)
{
    TARRAY_ASSERT(srcObj->typePtr == &gTArrayType);
    TARRAY_ASSERT(TARRAYHDR(srcObj) != NULL);
        
    TARRAY_OBJ_SETREP(dstObj, TARRAYHDR(srcObj));
}


/* Called to generate a string implementation from an array of Tcl_Obj */
static void UpdateStringForObjType(Tcl_Obj *objP)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
    Tcl_Obj **objv;
    int objc;
    TAHdr *tahP;
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    int i, length, bytesNeeded = 0;
    const char *elem;
    char *dst;

    tahP = TARRAYHDR(objP);
    objv = TAHDRELEMPTR(tahP, Tcl_Obj *, 0);
    objc = tahP->used;

    /*
     * Pass 1: estimate space, gather flags.
     */

    if (objc <= LOCAL_SIZE) {
        flagPtr = localFlags;
    } else {
        /*
         * We know objc <= TARRAY_MAX_OBJC, so this is safe.
         */

        flagPtr = (int *) ckalloc(objc * sizeof(int));
    }

    bytesNeeded +=
        sizeof("tarray ") - 1 /* -1 to exclude the null */
        + sizeof(" {") - 1 /* Start of list minus trailing null */
        + 1;               /* Trailing "}" */
    bytesNeeded += strlen(gTArrayTypeTokens[TARRAY_OBJ]);
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = TCL_DONT_QUOTE_HASH;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if (bytesNeeded < 0)
            TArrayStringOverflowPanic("UpdateStringForObjType");
    }
    if (bytesNeeded > INT_MAX - objc + 1)
        TArrayStringOverflowPanic("UpdateStringForObjType");

    bytesNeeded += objc;        /* For separators and terminating null */

    /*
     * Pass 2: copy into string rep buffer.
     */

    objP->bytes = ckalloc(bytesNeeded);
    dst = objP->bytes;
    memcpy(dst, "tarray ", sizeof("tarray ")-1);
    dst += sizeof("tarray ") - 1;
    strcpy(dst, gTArrayTypeTokens[TARRAY_OBJ]);
    dst += strlen(gTArrayTypeTokens[TARRAY_OBJ]);
    *dst++ = ' ';
    *dst++ = '{';
    /* TBD - handle objc==0 case */
    if (objc) {
        for (i = 0; i < objc; i++) {
            flagPtr[i] |= (i ? TCL_DONT_QUOTE_HASH : 0);
            elem = Tcl_GetStringFromObj(objv[i], &length);
            dst += Tcl_ConvertCountedElement(elem, length, dst, flagPtr[i]);
            *dst++ = ' ';
            /* Assert <, not <= because need to add terminating "}" */
            TARRAY_ASSERT((dst-objP->bytes) < bytesNeeded);
        }
        dst[-1] = '}';
    } else
        *dst++ = '}';
    *dst = '\0';
    TARRAY_ASSERT((dst-objP->bytes) < bytesNeeded);
    objP->length = dst - objP->bytes;

    if (flagPtr != localFlags) {
        ckfree((char *) flagPtr);
    }
}


static void TArrayTypeUpdateStringRep(Tcl_Obj *objP)
{
    int i, n, count;
    int allocated, unused;
    char *cP;
    int max_elem_space;  /* Max space to print one element including
                            either terminating null or space */
    TAHdr *thdrP;
        
    TARRAY_ASSERT(objP->typePtr == &gTArrayType);

    objP->bytes = NULL;

    count = TARRAYELEMCOUNT(objP);
    if (count == 0) {
        objP->bytes = ckalloc(sizeof(objP->bytes[0]));
        objP->bytes[0] = 0;
        objP->length = 0;
        return;
    }

    thdrP = TARRAYHDR(objP);

    /* Code below based on count > 0 else terminating \0 will blow memory */

    /*
     * Special case Boolean since we know exactly how many chars will
     * be required 
     */

    /*
     * When output size cannot be calculated exactly, we allocate using
     * some estimate based on the type.
     */
        
    switch (TARRAYTYPE(objP)) {
    case TARRAY_BOOLEAN:
        {
            ba_t *baP = TAHDRELEMPTR(thdrP, ba_t, 0);
            register ba_t ba = *baP;
            register ba_t ba_mask;

            /* For BOOLEANS, we know how long a buffer needs to be */
            cP = ckalloc(2*count);
            objP->bytes = cP;
            n = count / BA_UNIT_SIZE;
            for (i = 0; i < n; ++i, ++baP) {
                for (ba_mask = BITPOSMASK(0); ba_mask ; ba_mask = BITMASKNEXT(ba_mask)) {
                    *cP++ = (ba & ba_mask) ? '1' : '0';
                    *cP++ = ' ';
                }
            }
            n = count - n*BA_UNIT_SIZE;    /* Left over bits in last byte */
            if (n) {
                ba = *baP;
                for (i = 0, ba_mask = BITPOSMASK(0); i < n; ++i, ba_mask = BITMASKNEXT(ba_mask)) {
                    *cP++ = (ba & ba_mask) ? '1' : '0';
                    *cP++ = ' ';
                }
            }
            cP[-1] = 0;         /* Overwrite last space with terminating \0 */
            objP->length = 2*count - 1;
        }
        return;
                
    case TARRAY_OBJ:
        UpdateStringForObjType(objP);
        return;
                
    case TARRAY_UINT:
    case TARRAY_INT:
        TARRAY_ASSERT(sizeof(int) == 4); /* So max string space needed is 11 */
        max_elem_space = 11+1;
        break;
    case TARRAY_WIDE:
        max_elem_space = TCL_INTEGER_SPACE+1;
        break;
    case TARRAY_DOUBLE:
        max_elem_space = TCL_DOUBLE_SPACE+1;
        break;
    case TARRAY_BYTE:
        max_elem_space = 3+1;
        break;
    default:
        TArrayTypePanic(thdrP->type);
    }
            
    allocated = 0;
    unused = 0;
    objP->bytes= NULL;
    /* TBD - do Nested loop for efficiency reasons to avoid switch on every iter */
    for (i = 0; i < count; ++i) {
        if (unused < max_elem_space) {
            n = allocated - unused; /* Used space */
            /* Increase assuming remaining take half max space on average */
            allocated += ((max_elem_space + 1)/2)*(count - i);
            objP->bytes = ckrealloc(objP->bytes, allocated);
            cP = n + (char *) objP->bytes;
            unused = allocated - n;
        }
        switch (thdrP->type) {
        case TARRAY_UINT:
            _snprintf(cP, unused, "%u", *TAHDRELEMPTR(thdrP, unsigned int, i));
            break;
        case TARRAY_INT:
            _snprintf(cP, unused, "%d", *TAHDRELEMPTR(thdrP, int, i));
            break;
        case TARRAY_WIDE:
            _snprintf(cP, unused, "%" TCL_LL_MODIFIER "d", *TAHDRELEMPTR(thdrP, Tcl_WideInt, i));
            break;
        case TARRAY_DOUBLE:
            /* Do not use _snprintf because of slight difference
               it does not include decimal point for whole ints. For
               consistency with Tcl, use Tcl_PrintDouble instead */
            Tcl_PrintDouble(NULL, *TAHDRELEMPTR(thdrP, double, i), cP);
            break;
        case TARRAY_BYTE:
            _snprintf(cP, unused, "%u", *TAHDRELEMPTR(thdrP, unsigned char, i));
            break;
        }
        n = strlen(cP);
        cP += n;
        *cP++ = ' ';
        unused -= n+1;
    }

    cP[-1] = 0;         /* Overwrite last space with terminating \0 */
    objP->length = allocated - unused - 1; /* Terminating null not included in length */
            
    /* Only shrink array if unused space is comparatively too large */
    if (unused > (allocated / 8) && unused > 10)
        objP->bytes = ckrealloc(objP->bytes, allocated - unused);
    return;
}

Tcl_Obj *TArrayNewObj(TAHdr *thdrP)
{
    Tcl_Obj *objP = Tcl_NewObj();
    Tcl_InvalidateStringRep(objP);
    TARRAY_OBJ_SETREP(objP, thdrP);
    return objP;
}
    
/* thdrP must NOT be shared and must have enough slots */
/* interp may be NULL (only used for errors) */
TCL_RESULT TAHdrSetFromObjs(Tcl_Interp *interp, TAHdr *thdrP,
                                 int first, int nelems,
                                 Tcl_Obj * const elems[])
{
    int i, ival;
    Tcl_WideInt wide;
    double dval;

    TARRAY_ASSERT(thdrP->nrefs < 2);

    if ((first + nelems) > thdrP->allocated) {
        /* Should really panic but not a fatal error (ie. no memory
         * corruption etc.). Most likely some code path did not check
         * size and allocate space accordingly.
         */
        if (interp)
            Tcl_SetResult(interp, "Internal error: TArray too small.", TCL_STATIC);
        return TCL_ERROR;
    }

    /*
     * In case of conversion errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors.
     *
     * As a special optimization, when appending to the end, we do
     * not need to first check. We directly store the values and in case
     * of errors, simply do not update size.
     *
     * Also for TARRAY_OBJ there is no question of conversion and hence
     * no question of conversion errors.
     *
     */

    if (first < thdrP->used && thdrP->type != TARRAY_OBJ) {
        /* Not appending, need to verify conversion */
        switch (thdrP->type) {
        case TARRAY_BOOLEAN:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetBooleanFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TARRAY_UINT:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
                if (wide < 0 || wide > 0xFFFFFFFF) {
                    if (interp)
                        Tcl_SetObjResult(interp,
                                         Tcl_ObjPrintf("Integer \"%s\" too large for type \"uint\" typearray.", Tcl_GetString(elems[i])));
                    goto convert_error;
                }
            }
            break;

        case TARRAY_INT:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TARRAY_WIDE:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TARRAY_DOUBLE:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetDoubleFromObj(interp, elems[i], &dval) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TARRAY_BYTE:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival > 255 || ival < 0) {
                    if (interp)
                        Tcl_SetObjResult(interp,
                                         Tcl_ObjPrintf("Integer \"%d\" does not fit type \"byte\" typearray.", ival));
                    goto convert_error;
                }
            }
            break;
        default:
            TArrayTypePanic(thdrP->type);
        }
    }

    /*
     * Now actually store the values. Note we still have to check
     * status on conversion since we did not do checks when we are appending
     * to the end.
     */

    switch (thdrP->type) {
    case TARRAY_BOOLEAN:
        {
            register ba_t *baP;
            ba_t ba, ba_mask;
            int off;

            /* Take care of the initial condition where the first bit
               may not be aligned on a boundary */
            baP = TAHDRELEMPTR(thdrP, ba_t, first / BA_UNIT_SIZE);
            off = first % BA_UNIT_SIZE; /* Offset of bit within a char */
            ba_mask = BITPOSMASK(off); /* The bit pos corresponding to 'first' */
            if (off != 0) {
                /*
                 * Offset is off within a ba_t. Get the ba_t at that location
                 * preserving the preceding bits within the char.
                 */
                ba = *baP & BITPOSMASKLT(off);
            } else {
                ba = 0;
            }
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetBooleanFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival)
                    ba |= ba_mask;
                ba_mask = BITMASKNEXT(ba_mask);
                if (ba_mask == 0) {
                    *baP++ = ba;
                    ba = 0;
                    ba_mask = BITPOSMASK(0);
                }
            }
            if (ba_mask != BITPOSMASK(0)) {
                /* We have some leftover bits in ba that need to be stored.
                 * We need to *merge* these into the corresponding word
                 * keeping the existing high index bits.
                 * Note the bit indicated by ba_mask also has to be preserved,
                 * not overwritten.
                 */
                *baP = ba | (*baP & BITMASKGE(ba_mask));
            }
        }
        break;

    case TARRAY_UINT:
        {
            register unsigned int *uintP;
            uintP = TAHDRELEMPTR(thdrP, unsigned int, first);
            for (i = 0; i < nelems; ++i, ++uintP) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
                if (wide < 0 || wide > 0xFFFFFFFF) {
                    if (interp)
                        Tcl_SetObjResult(interp,
                                         Tcl_ObjPrintf("Integer \"%s\" too large for type \"uint\" typearray.", Tcl_GetString(elems[i])));
                    goto convert_error;
                }
                *uintP = (unsigned int) wide;
            }
        }
        break;
    case TARRAY_INT:
        {
            register int *intP;
            intP = TAHDRELEMPTR(thdrP, int, first);
            for (i = 0; i < nelems; ++i, ++intP) {
                if (Tcl_GetIntFromObj(interp, elems[i], intP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TARRAY_WIDE:
        {
            register Tcl_WideInt *wideP;
            wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, first);
            for (i = 0; i < nelems; ++i, ++wideP) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], wideP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TARRAY_DOUBLE:
        {
            register double *dblP;
            dblP = TAHDRELEMPTR(thdrP, double, first);
            for (i = 0; i < nelems; ++i, ++dblP) {
                if (Tcl_GetDoubleFromObj(interp, elems[i], dblP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TARRAY_OBJ:
        {
            register Tcl_Obj **objPP;
            objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, first);
            for (i = 0; i < nelems; ++i, ++objPP) {
                /* Careful about the order here! */
                Tcl_IncrRefCount(elems[i]);
                if ((first + i) < thdrP->used) {
                    /* Deref what was originally in that slot */
                    Tcl_DecrRefCount(*objPP);
                }
                *objPP = elems[i];
            }
        }
        break;

    case TARRAY_BYTE:
        {
            register unsigned char *byteP;
            byteP = TAHDRELEMPTR(thdrP, unsigned char, first);
            for (i = 0; i < nelems; ++i, ++byteP) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival > 255 || ival < 0) {
                    if (interp)
                        Tcl_SetObjResult(interp,
                                         Tcl_ObjPrintf("Integer \"%d\" does not fit type \"byte\" typearray.", ival));
                    goto convert_error;
                }
                *byteP = (unsigned char) ival;
            }
        }
        break;

    default:
        TArrayTypePanic(thdrP->type);
    }

    if ((first + nelems) > thdrP->used)
        thdrP->used = first + nelems;

    return TCL_OK;

convert_error:                  /* Interp should already contain errors */
    TARRAY_ASSERT(thdrP->type != TARRAY_OBJ); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}

/* interp may be NULL (only used for errors) */
/* See asserts in code for prerequisite conditions */
TCL_RESULT TAHdrGridSetFromObjs(Tcl_Interp *interp,
                                    TAHdr * const thdrs[], int nthdrs,
                                    Tcl_Obj *tuples, int first)
{
    int t, r, ival;
    TAHdr *thdrP;
    Tcl_WideInt wide;
    double dval;
    Tcl_Obj **rows;
    int nrows;
    int have_obj_cols;
    int have_other_cols;
    int need_data_validation;
    Tcl_Obj *valObj;

    TARRAY_ASSERT(nthdrs > 0);

    if (Tcl_ListObjGetElements(interp, tuples, &nrows, &rows) != TCL_OK)
        return TCL_ERROR;

    if (nrows == 0)
        return TCL_OK;          /* Nought to do */

    for (t = 0, have_obj_cols = 0, have_other_cols = 0; t < nthdrs; ++t) {
        TARRAY_ASSERT(thdrs[t]->nrefs < 2); /* Unshared */
        TARRAY_ASSERT(thdrs[t]->allocated >= (first + nrows)); /* 'Nuff space */
        TARRAY_ASSERT(thdrs[t]->used == thdrs[0]->used); /* All same size */

        if (thdrs[t]->type == TARRAY_OBJ)
            have_obj_cols = 1;
        else
            have_other_cols = 1;

    }

    /*
     * In case of errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors
     * or to hold new Tcl_Values so conversion does not need to be repeated.
     *
     * There are two kinds of errors - data type errors (e.g. attempt
     * to store a non-integer into an integer field) and structural
     * errors (e.g. a row not having enough elements).
     *
     * As a special optimization, when appending to the end, we do
     * not need to first check. We directly store the values and in case
     * of errors, simply not update the old size.
     *
     * TARRAY_OBJ add a complication. They do not need a type check
     * but because their reference counts have to be managed, it is more
     * complicated to back track on errors when we skip the validation
     * checks in the pure append case. So we update these columns
     * only after everything else has been updated.
     */

    if (! have_other_cols) {
        /* Only TARRAY_OBJ columns, data validation is a no-op */
        need_data_validation = 0;
    } else if (first >= thdrs[0]->used) {
        /*
         * Pure append, not overwriting so rollback becomes easy and
         * no need for prevalidation step.
         */
        need_data_validation = 0;
    } else
        need_data_validation = 1;
       
    /* We could either iterate vertically or horizontally
     *   for (per thdr)
     *     switch (thdr->type)
     *       for (per row)
     *         field <- Tcl_ListObjIndex
     *         validate field
     * or
     *   for (per row)
     *     fields <- Tcl_ListObjGetElements
     *     per field
     *       switch thdr->type
     *         validate field
     *
     * Not clear which will perform better - first case inner loop has
     * a call (Tcl_ListObjIndex). Second case inner loop has a switch
     * (probably faster than a call). On the other hand, when actually
     * writing out to the array, cache effects might make the former
     * faster (writing consecutive locations).
     *
     * As it turns out, we use the first method for a different reason -
     * when we are strictly appending without overwriting, we do not
     * validate since rollback is easy. The complication is that if
     * any column is of type TARRAY_OBJ, when an error occurs we have to
     * rollback that column's Tcl_Obj reference counts. Keeping track
     * of this is more involved using the second scheme and much simpler
     * with the first scheme. Hence we go with that.
     */

    if (need_data_validation) {
        for (r = 0; r < nrows; ++r) {
            Tcl_Obj **fields;
            int nfields;
        
            if (Tcl_ListObjGetElements(interp, rows[r], &nfields, &fields)
                != TCL_OK)
                goto error_return;

            /* Must have sufficient fields, more is ok */
            if (nfields < nthdrs)
                goto width_error;

            for (t = 0; t < nthdrs; ++t) {
                switch (thdrs[t]->type) {
                case TARRAY_BOOLEAN:
                    if (Tcl_GetBooleanFromObj(interp, fields[t], &ival) != TCL_OK)
                        goto error_return;
                    break;
                case TARRAY_UINT:
                    if (Tcl_GetWideIntFromObj(interp, fields[t], &wide) != TCL_OK)
                        goto error_return;
                    if (wide < 0 || wide > 0xFFFFFFFF) {
                        if (interp)
                            Tcl_SetObjResult(interp,
                                             Tcl_ObjPrintf("Integer \"%s\" too large for type \"uint\" typearray.", Tcl_GetString(fields[t])));
                        goto error_return;
                    }
                    break;
                case TARRAY_INT:
                    if (Tcl_GetIntFromObj(interp, fields[t], &ival) != TCL_OK)
                        goto error_return;
                    break;
                case TARRAY_WIDE:
                    if (Tcl_GetWideIntFromObj(interp, fields[t], &wide) != TCL_OK)
                        goto error_return;
                    break;
                case TARRAY_DOUBLE:
                    if (Tcl_GetDoubleFromObj(interp, fields[t], &dval) != TCL_OK)
                        goto error_return;
                    break;
                case TARRAY_BYTE:
                    if (Tcl_GetIntFromObj(interp, fields[t], &ival) != TCL_OK)
                        goto error_return;
                    if (ival > 255 || ival < 0) {
                        if (interp)
                            Tcl_SetObjResult(interp,
                                             Tcl_ObjPrintf("Integer \"%d\" does not fit type \"byte\" typearray.", ival));
                        goto error_return;
                    }
                    break;
                case TARRAY_OBJ:
                    break;      /* No validation */
                default:
                    TArrayTypePanic(thdrP->type);
                }
            }
        }
    } else {
        /* We are not validating data but then validate row widths */
        /* We are doing this to simplify error rollback for TARRAY_OBJ */
        for (r = 0; r < nrows; ++r) {
            if (Tcl_ListObjLength(interp, rows[r], &ival) == TCL_ERROR)
                goto error_return;
            /* Width of row must not be too short, longer is ok */
            if (ival < nthdrs)
                goto width_error;
        }
    }

    /*
     * Now actually store the values. Note we still have to check
     * status on conversion in case we did not do checks when we are appending
     * to the end, and we have to store TARRAY_OBJ last to facilitate
     * rollback on errors as discussed earlier.
     */
    if (have_other_cols) {
        for (t=0; t < nthdrs; ++t) {
            /* Skip TARRAY_OBJ on this round, until all other data is stored */
            if (thdrs[t]->type == TARRAY_OBJ)
                continue;
            switch (thdrP->type) {
            case TARRAY_BOOLEAN:
                {
                    register ba_t *baP;
                    ba_t ba, ba_mask;
                    int off;

                    /* Take care of the initial condition where the first bit
                       may not be aligned on a boundary */
                    baP = TAHDRELEMPTR(thdrP, ba_t, first / BA_UNIT_SIZE);
                    off = first % BA_UNIT_SIZE; /* Offset of bit within a char */
                    ba_mask = BITPOSMASK(off); /* The bit pos corresponding to 'first' */
                    if (off != 0) {
                        /*
                         * Offset is off within a ba_t. Get the ba_t at that location
                         * preserving the preceding bits within the char.
                         */
                        ba = *baP & BITPOSMASKLT(off);
                    } else
                        ba = 0;
                    for (r = 0; r < nrows; ++r) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TARRAY_ASSERT(valObj);
                        if (Tcl_GetBooleanFromObj(interp, valObj, &ival) != TCL_OK)
                            goto error_return;
                        if (ival)
                            ba |= ba_mask;
                        ba_mask = BITMASKNEXT(ba_mask);
                        if (ba_mask == 0) {
                            *baP++ = ba;
                            ba = 0;
                            ba_mask = BITPOSMASK(0);
                        }
                    }
                    if (ba_mask != BITPOSMASK(0)) {
                        /* We have some leftover bits in ba that need
                        to be stored.  * We need to *merge* these into
                        the corresponding word * keeping the existing
                        high index bits.  * Note the bit indicated by
                        ba_mask also has to be preserved, * not
                        overwritten.  */
                        *baP = ba | (*baP & BITMASKGE(ba_mask));
                    }
                }
                break;

            case TARRAY_UINT:
                {
                    register unsigned int *uintP;
                    uintP = TAHDRELEMPTR(thdrP, unsigned int, first);
                    for (r = 0; r < nrows; ++r, ++uintP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TARRAY_ASSERT(valObj);
                        if (Tcl_GetWideIntFromObj(interp, valObj, &wide) != TCL_OK)
                            goto error_return;
                        if (wide < 0 || wide > 0xFFFFFFFF) {
                            if (interp)
                                Tcl_SetObjResult(interp,
                                                 Tcl_ObjPrintf("Integer \"%s\" too large for type \"uint\" typearray.", Tcl_GetString(rows[r])));
                            goto error_return;
                        }
                        *uintP = (unsigned int) wide;
                    }
                }
                break;
            case TARRAY_INT:
                {
                    register int *intP;
                    intP = TAHDRELEMPTR(thdrP, int, first);
                    for (r = 0; r < nrows; ++r, ++intP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TARRAY_ASSERT(valObj);
                        if (Tcl_GetIntFromObj(interp, valObj, intP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TARRAY_WIDE:
                {
                    register Tcl_WideInt *wideP;
                    wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, first);
                    for (r = 0; r < nrows; ++r, ++wideP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TARRAY_ASSERT(valObj);
                        if (Tcl_GetWideIntFromObj(interp, valObj, wideP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TARRAY_DOUBLE:
                {
                    register double *dblP;
                    dblP = TAHDRELEMPTR(thdrP, double, first);
                    for (r = 0; r < nrows; ++r, ++dblP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TARRAY_ASSERT(valObj);
                        if (Tcl_GetDoubleFromObj(interp, valObj, dblP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TARRAY_BYTE:
                {
                    register unsigned char *byteP;
                    byteP = TAHDRELEMPTR(thdrP, unsigned char, first);
                    for (r = 0; r < nrows; ++r, ++byteP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TARRAY_ASSERT(valObj);
                        if (Tcl_GetIntFromObj(interp, valObj, &ival) != TCL_OK)
                            goto error_return;
                        if (ival > 255 || ival < 0) {
                            if (interp)
                                Tcl_SetObjResult(interp,
                                                 Tcl_ObjPrintf("Integer \"%d\" does not fit type \"byte\" typearray.", ival));
                            goto error_return;
                        }
                        *byteP = (unsigned char) ival;
                    }
                }
                break;
            default:
                TArrayTypePanic(thdrP->type);
            }
        }
    }

    /* Now that no errors are possible, update the TARRAY_OBJ columns */
    for (t=0; t < nthdrs; ++t) {
        register Tcl_Obj **objPP;
        if (thdrs[t]->type != TARRAY_OBJ)
            continue;
        objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, first);
        for (r = 0; r < nrows ; ++r, ++objPP) {
            Tcl_ListObjIndex(interp, rows[r], t, &valObj);
            TARRAY_ASSERT(valObj);
            /* Careful about the order here! */
            Tcl_IncrRefCount(valObj);
            if ((first + r) < thdrP->used) {
                /* Deref what was originally in that slot */
                Tcl_DecrRefCount(*objPP);
            }
            *objPP = valObj;
        }
    }

    /* Now finally, update all the counts */
    for (t=0; t < nthdrs; ++t) {
        if ((first + nrows) > thdrs[t]->used)
            thdrs[t]->used = first + nrows;
    }

    return TCL_OK;

width_error:
    if (interp)
        Tcl_SetResult(interp, "Not enough elements in row", TCL_STATIC);

error_return:                  /* Interp should already contain errors */
    return TCL_ERROR;
}


int TArrayCalcSize(unsigned char tatype, int count)
{
    int space;

    switch (tatype) {
    case TARRAY_BOOLEAN:
        space = BA_BYTES_NEEDED(0, count);
        break;
    case TARRAY_UINT:
    case TARRAY_INT:
        space = count * sizeof(int);
        break;
    case TARRAY_WIDE:
        space = count * sizeof(Tcl_WideInt);
        break;
    case TARRAY_DOUBLE:
        space = count * sizeof(double);
        break;
    case TARRAY_OBJ:
        space = count * sizeof(Tcl_Obj *);
        break;
    case TARRAY_BYTE:
        space = count * sizeof(unsigned char);
        break;
    default:
        TArrayTypePanic(tatype);
    }

    return sizeof(TAHdr) + space;
}

TAHdr *TArrayRealloc(TAHdr *oldP, int new_count)
{
    TAHdr *thdrP;

    TARRAY_ASSERT(oldP->nrefs < 2);
    TARRAY_ASSERT(oldP->used <= new_count);

    thdrP = (TAHdr *) TARRAY_REALLOCMEM((char *) oldP, TArrayCalcSize(oldP->type, new_count));
    thdrP->allocated = new_count;
    return thdrP;
}

TAHdr * TArrayAlloc(unsigned char tatype, int count)
{
    unsigned char nbits;
    TAHdr *thdrP;

    if (count == 0)
            count = TARRAY_DEFAULT_NSLOTS;
    thdrP = (TAHdr *) TARRAY_ALLOCMEM(TArrayCalcSize(tatype, count));
    thdrP->nrefs = 0;
    thdrP->allocated = count;
    thdrP->used = 0;
    thdrP->type = tatype;
    switch (tatype) {
    case TARRAY_BOOLEAN: nbits = 1; break;
    case TARRAY_UINT: nbits = sizeof(unsigned int) * CHAR_BIT; break;
    case TARRAY_INT: nbits = sizeof(int) * CHAR_BIT; break;
    case TARRAY_WIDE: nbits = sizeof(Tcl_WideInt) * CHAR_BIT; break;
    case TARRAY_DOUBLE: nbits = sizeof(double) * CHAR_BIT; break;
    case TARRAY_OBJ: nbits = sizeof(Tcl_Obj *) * CHAR_BIT; break;
    case TARRAY_BYTE: nbits = sizeof(unsigned char) * CHAR_BIT; break;
    default:
        TArrayTypePanic(tatype);
    }
    thdrP->elem_bits = nbits;
    
    return thdrP;
}

TAHdr * TArrayAllocAndInit(Tcl_Interp *interp, unsigned char tatype,
                           int nelems, Tcl_Obj * const elems[],
                           int init_size)
{
    TAHdr *thdrP;

    if (elems) {
        /*
         * Initialization provided. If explicit size specified, fix
         * at that else leave some extra space.
         */
        if (init_size) {
            if (init_size < nelems)
                init_size = nelems;
        } else {
            init_size = nelems + TARRAY_EXTRA(nelems);
        }
    } else {
        nelems = 0;
    }

    thdrP = TArrayAlloc(tatype, init_size);

    if (elems != NULL && nelems != 0) {
        if (TAHdrSetFromObjs(interp, thdrP, 0, nelems, elems) != TCL_OK) {
            TARRAY_FREEMEM(thdrP);
            return NULL;
        }
    }

    return thdrP;

}

/* Deletes a range from a TAHdr. See asserts below for requirements */
void TAHdrDelete(TAHdr *thdrP, int first, int count)
{
    int nbytes;
    void *s, *d;

    TARRAY_ASSERT(! TAHDR_SHARED(thdrP));
    TARRAY_ASSERT(first >= 0);

    if (first >= thdrP->used)
        return;          /* Nothing to be deleted */
    
    if ((first + count) >= thdrP->used)
        count = thdrP->used - first;

    if (count <= 0)
        return;          /* Nothing to be deleted */

    /*
     * For all types other than BOOLEAN and OBJ, we can just memmove
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (thdrP->type) {
    case TARRAY_BOOLEAN:
        ba_copy(TAHDRELEMPTR(thdrP, ba_t, 0), first, 
                TAHDRELEMPTR(thdrP, ba_t, 0), first+count,
                thdrP->used-(first+count));
        thdrP->used -= count;
        return;

    case TARRAY_OBJ:
        /*
         * We have to deal with reference counts here. For the objects
         * we are deleting we need to decrement the reference counts.
         */

        TArrayDecrObjRefs(thdrP, first, count);
         
        /* Now we can just memcpy like the other types */
        nbytes = count * sizeof(Tcl_Obj *);
        s = TAHDRELEMPTR(thdrP, Tcl_Obj *, first+count);
        d = TAHDRELEMPTR(thdrP, Tcl_Obj *, first);
        break;

    case TARRAY_UINT:
    case TARRAY_INT:
        nbytes = count * sizeof(int);
        s = TAHDRELEMPTR(thdrP, int, first+count);
        d = TAHDRELEMPTR(thdrP, int, first);
        break;
    case TARRAY_WIDE:
        nbytes = count * sizeof(Tcl_WideInt);
        s = TAHDRELEMPTR(thdrP, Tcl_WideInt, first+count);
        d = TAHDRELEMPTR(thdrP, Tcl_WideInt, first);
        break;
    case TARRAY_DOUBLE:
        nbytes = count * sizeof(double);
        s = TAHDRELEMPTR(thdrP, double, first+count);
        d = TAHDRELEMPTR(thdrP, double, first);
        break;
    case TARRAY_BYTE:
        nbytes = count * sizeof(unsigned char);
        s = TAHDRELEMPTR(thdrP, unsigned char, first+count);
        d = TAHDRELEMPTR(thdrP, unsigned char, first);
        break;
    default:
        TArrayTypePanic(thdrP->type);
    }

    memmove(d, s, nbytes);      /* NOT memcpy since overlapping copy */

    thdrP->used -= count;
}


/* Copies partial content from one TAHdr to another. See asserts below
   for requirements */
void TAHdrCopy(TAHdr *dstP, int dst_first,
                   TAHdr *srcP, int src_first, int count)
{
    int nbytes;
    void *s, *d;

    TARRAY_ASSERT(dstP != srcP);
    TARRAY_ASSERT(dstP->type == srcP->type);
    TARRAY_ASSERT(! TAHDR_SHARED(dstP));
    TARRAY_ASSERT(src_first >= 0);
    if (src_first >= srcP->used)
        return;          /* Nothing to be copied */
    if ((src_first + count) > srcP->used)
        count = srcP->used - src_first;
    if (count <= 0)
        return;
    TARRAY_ASSERT((dst_first + count) <= dstP->allocated);

    if (dst_first < 0)
        dst_first = 0;
    else if (dst_first > dstP->used)
        dst_first = dstP->used;

    /*
     * For all types other than BOOLEAN and OBJ, we can just memcpy
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (srcP->type) {
    case TARRAY_BOOLEAN:
        ba_copy(TAHDRELEMPTR(dstP, ba_t, 0), dst_first,
                TAHDRELEMPTR(srcP, ba_t, 0), src_first, count);
        if ((dst_first + count) > dstP->used)
            dstP->used = dst_first + count;
        return;

    case TARRAY_OBJ:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment the reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        TArrayIncrObjRefs(srcP, src_first, count); /* Do this first */
        /* Note this call take care of the case where count exceeds
         * actual number in dstP
         */
        TArrayDecrObjRefs(dstP, dst_first, count);
         
        /* Now we can just memcpy like the other types */
        nbytes = count * sizeof(Tcl_Obj *);
        s = TAHDRELEMPTR(srcP, Tcl_Obj *, src_first);
        d = TAHDRELEMPTR(dstP, Tcl_Obj *, dst_first);
        break;

    case TARRAY_UINT:
    case TARRAY_INT:
        nbytes = count * sizeof(int);
        s = TAHDRELEMPTR(srcP, int, src_first);
        d = TAHDRELEMPTR(dstP, int, dst_first);
        break;
    case TARRAY_WIDE:
        nbytes = count * sizeof(Tcl_WideInt);
        s = TAHDRELEMPTR(srcP, Tcl_WideInt, src_first);
        d = TAHDRELEMPTR(dstP, Tcl_WideInt, dst_first);
        break;
    case TARRAY_DOUBLE:
        nbytes = count * sizeof(double);
        s = TAHDRELEMPTR(srcP, double, src_first);
        d = TAHDRELEMPTR(dstP, double, dst_first);
        break;
    case TARRAY_BYTE:
        nbytes = count * sizeof(unsigned char);
        s = TAHDRELEMPTR(srcP, unsigned char, src_first);
        d = TAHDRELEMPTR(dstP, unsigned char, dst_first);
        break;
    default:
        TArrayTypePanic(srcP->type);
    }

    memcpy(d, s, nbytes);

    if ((dst_first + count) > dstP->used)
        dstP->used = dst_first + count;

    return;
}

/* Note: nrefs of cloned array is 0 */
TAHdr *TAHdrClone(TAHdr *srcP, int minsize)
{
    TAHdr *thdrP;

    if (minsize == 0)
        minsize = srcP->allocated;
    else if (minsize < srcP->used)
        minsize = srcP->used;

    /* TBD - optimize these two calls */
    thdrP = TArrayAlloc(srcP->type, minsize);
    TAHdrCopy(thdrP, 0, srcP, 0, srcP->used);
    return thdrP;
}

Tcl_Obj *TArrayGridClone(Tcl_Interp *interp, Tcl_Obj *gridObj, int minsize)
{
    Tcl_Obj **taObjs;
    int i, ntaObjs;
    Tcl_Obj *cloneObj;

    if (Tcl_ListObjGetElements(interp, gridObj, &ntaObjs, &taObjs) != TCL_OK)
        return NULL;
    cloneObj = Tcl_NewListObj(ntaObjs, NULL);
    for (i = 0; i < ntaObjs; ++i) {
        TARRAY_ASSERT(taObjs[i]->typePtr == &gTArrayType);
        Tcl_ListObjAppendElement(interp, cloneObj,
                                 TArrayNewObj(TAHdrClone(TARRAYHDR(taObjs[i]), minsize)));
    }
    return cloneObj;
}

/*
 * Convert a Tcl_Obj to one that is suitable for modifying.
 * There are three cases to consider:
 * (1) If taObj is shared, then we cannot modify in place since
 *     the object is referenced elsewhere in the program. We have to
 *     clone the corresponding TAHdr and stick it in a new
 *     Tcl_Obj.
 * (2) If taObj is unshared, the corresponding TAHdr might
 *     still be shared (pointed to from elsewhere). In this case
 *     also, we clone the TAHdr but instead of allocating a new 
 *     Tcl_Obj, we store it as the internal rep of taObj.
 * (3) If taObj and its TAHdr are unshared, (a) we can modify in
 *     place unless (b) TAHdr is too small. In case (b) we have
 *     to follow the same path as (2).
 *
 * If flags contains TARRAY_MAKE_WRITABLE_REF, the returned object, 
 * whether what's passed in or newly allocated, has its ref count
 * incremented. Caller is responsible for decrementing as appropriate.
 * If flags does not contain TARRAY_MAKE_WRITABLE_REF, newly allocated
 * objects have ref count 0 and passed-in ones are returned with no change.
 *
 * Generally, if caller is immediately going to add the object to
 * (for example) a list or set it as the interp result, it should pass
 * flags as 0. If it adds to a list in some cases, and frees it in others
 * (such as error conditions), it should pass flags as TARRAY_MAKE_WRITABLE_REF
 * and then ALWAYS Tcl_DecrRefCount it.
 */
Tcl_Obj *TArrayMakeWritable(Tcl_Obj *taObj, int minsize, int prefsize, int flags)
{
    TAHdr *thdrP;

    TARRAY_ASSERT(taObj->typePtr == &gTArrayType);
    thdrP = TARRAYHDR(taObj);
    if (Tcl_IsShared(taObj)) {
        /* Case (1) */
        thdrP = TAHdrClone(thdrP, prefsize);
        TARRAY_ASSERT(thdrP);
        taObj = TArrayNewObj(thdrP);
    } else if (TAHDR_SHARED(thdrP) || thdrP->allocated < minsize) {
        /* Case (2) or (3b) */
        thdrP = TAHdrClone(thdrP, prefsize);
        TARRAY_ASSERT(thdrP);
        TAHDR_DECRREF(TARRAYHDR(taObj)); /* Release old */
        TARRAYHDR(taObj) = NULL;
        TARRAY_OBJ_SETREP(taObj, thdrP);
        Tcl_InvalidateStringRep(taObj);
    } else {
        /* Case (3) - can be reused, invalidate the string rep */
        Tcl_InvalidateStringRep(taObj);
    }

    if (flags & TARRAY_MAKE_WRITABLE_INCREF)
        Tcl_IncrRefCount(taObj);

    return taObj;
}

/* On error, returns NULL without gridObj being modified (though it may
   shimmer) */
Tcl_Obj *TArrayGridMakeWritable(Tcl_Interp *interp, Tcl_Obj *gridObj, int minsize, int prefsize, int flags)
{
    Tcl_Obj *writableObj;
    
    if (Tcl_IsShared(gridObj))
        writableObj = TArrayGridClone(interp, gridObj, prefsize);
    else {
        int n;
        int writable;
        int i;
        Tcl_Obj *taObj;
        TAHdr *thdrP;
        
        /* Even if the grid Tcl_Obj is not shared, the contained
         * TArray Tcl_Objs may be. Note we do not use Tcl_ListObjGetElements
         * and loop through the array because the underlying memory
         * storage *may* change when we replace the elements with
         * unshared ones.
         */
        if (Tcl_ListObjLength(interp, gridObj, &n) != TCL_OK)
            return NULL;
        /* First verify all elements are TArrays before doing anything */
        for (writable = 1, i = 0; i < n; ++i) {
            if (Tcl_ListObjIndex(interp, gridObj, i, &taObj)  != TCL_OK)
                return NULL;
            if (taObj->typePtr != &gTArrayType) {
                TArrayNotTArrayError(interp);
                return NULL;
            }
            thdrP = TARRAYHDR(taObj);
            if (Tcl_IsShared(taObj) ||
                TAHDR_SHARED(thdrP) || thdrP->allocated < minsize)
                writable = 0;
        }
        /* Now do any require changes */
        if (! writable) {
            /* Need to replace at least one TArray Tcl_Obj in the grid */
            for (i = 0; i < n; ++i) {
                Tcl_ListObjIndex(interp, gridObj, i, &taObj);
                thdrP = TARRAYHDR(taObj);
                if (Tcl_IsShared(taObj) ||
                    TAHDR_SHARED(thdrP) || thdrP->allocated < minsize) {
                    /* Note we INCREF and DecrRef because ListObjReplace
                       decrements deleted obj ref counts before incr refs
                       of added objects. Here it probably does not matter
                       since TArrayMakeWritable would have allocate new
                       object, but just to future-protect */
                    taObj = TArrayMakeWritable(taObj, minsize, prefsize,
                                               TARRAY_MAKE_WRITABLE_INCREF);
                    Tcl_ListObjReplace(interp, gridObj, i, 1, 1, &taObj);
                    Tcl_DecrRefCount(taObj); /* For INCREF above */
                }
            }
            Tcl_InvalidateStringRep(gridObj);
        }
        writableObj = gridObj;
    }
                    
    if (flags & TARRAY_MAKE_WRITABLE_INCREF)
        Tcl_IncrRefCount(writableObj);

    return writableObj;
}

TCL_RESULT TArrayGridFillFromObjs(
    Tcl_Interp *interp,
    Tcl_Obj *lowObj, Tcl_Obj *highObj,
    Tcl_Obj *const taObjs[], Tcl_Obj *const valueObjs[],
    int tuple_width, int flags)
{
    int i, low, count;
    TAHdr *thdr0P;
    TArrayValue values[32];
    TArrayValue *valuesP;
    Tcl_Obj *resultObjs[sizeof(values)/sizeof(values[0])];
    Tcl_Obj **resultObjsP;
    int status = TCL_ERROR;
    int new_size;

    /* Check for empty tuple so as to simplify loops below */
    if (tuple_width == 0)
        return TCL_OK;          /* Return empty result */

    /*
     * Extract low/high indices. Be careful not to shimmer because
     * in the unlikely but legal case where index is same object
     * as the passed tarray, we do not want to lose the tarray
     * representation. For example,
     *   set v [tarray create int {0}]
     *   tarray filltuples $v $v [list $v] 0
     * So if the index Tcl_Obj's are of tarrays, we dup them.
     */

    if (lowObj->typePtr == &gTArrayType)
        lowObj = Tcl_DuplicateObj(lowObj);
    else
        Tcl_IncrRefCount(lowObj); /* Since we will release at end */

    if (highObj->typePtr == &gTArrayType)
        highObj = Tcl_DuplicateObj(highObj);
    else
        Tcl_IncrRefCount(highObj); /* Since we will release at end */

    if (tuple_width > sizeof(values)/sizeof(values[0])) {
        valuesP = (TArrayValue *) ckalloc(tuple_width * sizeof(TArrayValue));
        resultObjsP = (Tcl_Obj **)ckalloc(tuple_width * sizeof(Tcl_Obj *));
    } else {
        valuesP = values;
        resultObjsP = resultObjs;
    }
        
    /*
     * Now verify tarrays and values. The latter should be of the
     * appropriate type. Also ensure all tarrays are the same size.
     */
    for (i = 0; i < tuple_width; ++i) {
        TAHdr *thdrP;
        if (TArrayVerifyType(interp, taObjs[i]) != TCL_OK)
            goto vamoose;
        thdrP = TARRAYHDR(taObjs[i]);
        if (i == 0)
            thdr0P = TARRAYHDR(taObjs[0]);
        else if (thdrP->used != thdr0P->used) {
            Tcl_SetResult(interp, "tarrays have differing number of elements", TCL_STATIC);
            goto vamoose;
        }
        if (TArrayValueFromObj(interp, valueObjs[i], thdrP->type, &valuesP[i])
            != TCL_OK)
            goto vamoose;
    }

    /* Get the limits of the range to set */
    if (RationalizeRangeIndices(interp, thdr0P, lowObj, highObj, &low, &count)
        != TCL_OK)
        return TCL_ERROR;

    /*
     * NOTE: NO ERRORS ARE EXPECTED BEYOND THIS POINT EXCEPT FATAL ONES
     * LIKE BUGCHECKS OR OUT OF MEMORY. Code below is written accordingly.
     */

    /*
     * Now that we have verified inputs are correct, get ready to
     * generate results. With respect to where to store the results,
     * there are three cases to consider for each tarray.
     * (1) If taObj[i] is shared, then we cannot modify in place since
     *     the object is referenced elsewhere in the program. We have to
     *     clone the corresponding thdrsP[i] and stick it in a new
     *     Tcl_Obj.
     * (2) If taObj[i] is unshared, the corresponding thdrP might
     *     still be shared (pointed to from elsewhere). In this case
     *     also, we clone the thdrP but instead of allocating a new 
     *     Tcl_Obj, we store it as the internal rep of taObj[i].
     * (3) If taObj[i] and its thdrP are unshared, (a) we can modify in
     *     place (b) unless thdrP is too small. In that case we have
     *     to follow the same path as (2).
     *
     * NOTE: taObjsP points into memory owned by objv[3] list. We cannot
     * write to it, hence we use a separate output area resultObjsP[].
     */

    /* If nothing to set, return existing tuple array as is */
    if (count == 0)
        Tcl_SetObjResult(interp, Tcl_NewListObj(tuple_width, taObjs));
    else {
        /* If we have to realloc anyway, we will leave a bit extra room */
        new_size = low + count + TARRAY_EXTRA(low+count);
        for (i = 0; i < tuple_width; ++i) {
            TARRAY_ASSERT(taObjs[i]->typePtr == &gTArrayType); // Verify no shimmering
            resultObjsP[i] = TArrayMakeWritable(taObjs[i], low+count, new_size, 0);
            TAHdrFill(interp, TARRAYHDR(resultObjs[i]),
                          &valuesP[i], low, count);
        }
        
        /* Caller should not set TARRAY_FILL_RETURN_ONE unless single tarray */
        TARRAY_ASSERT(tuple_width == 1 || (flags & TARRAY_FILL_SINGLE) == 0);
        if (flags & TARRAY_FILL_SINGLE)
            Tcl_SetObjResult(interp, resultObjsP[0]);
        else
            Tcl_SetObjResult(interp, Tcl_NewListObj(tuple_width, resultObjsP));
    }
    status = TCL_OK;
    
vamoose:                   /* interp must already hold error message */
    Tcl_DecrRefCount(lowObj);
    Tcl_DecrRefCount(highObj);

    if (resultObjsP != resultObjs)
        ckfree((char *) resultObjsP);
    if (valuesP != values)
        ckfree((char *) valuesP);

    return status;
}

/*
 * See asserts in code for entry conditions.
 * On success, gridObj (which must NOT be shared) is modified with the new
 * values. On error, gridObj is left unchanged.
 * interp is used only for errors,
 */
TCL_RESULT TArrayGridSetFromObjs(
    Tcl_Interp *interp,
    Tcl_Obj *lowObj,
    Tcl_Obj *gridObj,
    Tcl_Obj *valueObjs, /* List of lists (tuple values) */
    int flags)
{
    int i, low, count, grid_width;
    TAHdr *thdr0P;
    Tcl_Obj **taObjs;
    Tcl_Obj *resultObjs[32];
    Tcl_Obj **resultObjsP;
    TAHdr *thdrs[sizeof(resultObjs)/sizeof(resultObjs[0])];
    TAHdr **thdrsP;
    int status = TCL_ERROR;
    int new_size;

    TARRAY_ASSERT(! Tcl_IsShared(gridObj));

    if (Tcl_ListObjGetElements(interp, gridObj, &grid_width, &taObjs) != TCL_OK
        || Tcl_ListObjLength(interp, valueObjs, &count) != TCL_OK)
        return TCL_ERROR;

    /* Check for empty tuple or no new values so as to simplify loops below */
    if (grid_width == 0 || count == 0) {
        Tcl_SetObjResult(interp, gridObj);
        return TCL_OK;
    }

    /*
     * Extract low/high indices. Be careful not to shimmer because
     * in the unlikely but legal case where index is same object
     * as the passed tarray, we do not want to lose the tarray
     * representation. For example,
     *   set v [tarray create int {0}]
     *   tarray filltuples $v $v [list $v] 0
     * So if the index Tcl_Obj's are of tarrays, we dup them.
     */

    if (lowObj->typePtr == &gTArrayType)
        lowObj = Tcl_DuplicateObj(lowObj);
    else
        Tcl_IncrRefCount(lowObj); /* Since we will release at end */

    if (grid_width > sizeof(resultObjs)/sizeof(resultObjs[0])) {
        /* Allocate room for both resultObjs and thdrs in one shot */
        resultObjsP = (Tcl_Obj **)ckalloc(grid_width * sizeof(void *));
        thdrsP = (TAHdr **)&resultObjsP[grid_width];
    }
    else {
        resultObjsP = resultObjs;
        thdrsP = thdrs;
    }        

    /* Now verify tarrays are in fact tarrays and of the same size. */
    for (i = 0; i < grid_width; ++i) {
        TAHdr *thdrP;
        if (TArrayVerifyType(interp, taObjs[i]) != TCL_OK)
            goto vamoose;
        thdrP = TARRAYHDR(taObjs[i]);
        if (i == 0)
            thdr0P = TARRAYHDR(taObjs[0]);
        else if (thdrP->used != thdr0P->used) {
            Tcl_SetResult(interp, "tarrays have differing number of elements", TCL_STATIC);
            goto vamoose;
        }
    }

    /* Get the start of the range to set */
    if (IndexToInt(interp, lowObj, &low, thdr0P->used) != TCL_OK)
        goto vamoose;

    if (low < 0 || low > thdr0P->used) {
        TArrayIndexRangeError(interp, lowObj);
        goto vamoose;
    }

    count += low;               /* Needed size of array */

    /*
     * With respect to where to store the results,
     * there are three cases to consider for each tarray.
     * (1) If taObj[i] is shared, then we cannot modify in place since
     *     the object is referenced elsewhere in the program. We have to
     *     clone the corresponding thdrsP[i] and stick it in a new
     *     Tcl_Obj.
     * (2) If taObj[i] is unshared, the corresponding thdrP might
     *     still be shared (pointed to from elsewhere). In this case
     *     also, we clone the thdrP but instead of allocating a new 
     *     Tcl_Obj, we store it as the internal rep of taObj[i].
     * (3) If taObj[i] and its thdrP are unshared, (a) we can modify in
     *     place (b) unless thdrP is too small. In that case we have
     *     to follow the same path as (2).
     *
     * NOTE: taObjsP points into memory owned by objv[3] list. We cannot
     * write to it, hence we use a separate output area resultObjsP[].
     */

    TARRAY_ASSERT(count > 0);
    /* If we have to realloc anyway, we will leave a bit extra room */
    new_size = count + TARRAY_EXTRA(count);
    for (i = 0; i < grid_width; ++i) {
        TARRAY_ASSERT(taObjs[i]->typePtr == &gTArrayType); // Verify no shimmering
        resultObjsP[i] = TArrayMakeWritable(taObjs[i], count,
                               new_size, TARRAY_MAKE_WRITABLE_INCREF);
        thdrsP[i] = TARRAYHDR(resultObjsP[i]);
    }
        
    status = TAHdrGridSetFromObjs(interp, thdrsP, grid_width, valueObjs, low);

    if (status == TCL_OK) {
        /* Caller should not set TARRAY_FILL_RETURN_ONE unless single tarray */
        TARRAY_ASSERT(grid_width == 1 || (flags & TARRAY_FILL_SINGLE) == 0);
        if (flags & TARRAY_FILL_SINGLE)
            Tcl_SetObjResult(interp, resultObjsP[0]);
        else
            Tcl_SetObjResult(interp, Tcl_NewListObj(grid_width, resultObjsP));
    }

    for (i=0; i < grid_width; ++i)
        Tcl_DecrRefCount(resultObjsP[i]); /* Remove ref added by MakeWritable */

vamoose:                   /* interp must already hold error message */
    Tcl_DecrRefCount(lowObj);

    if (resultObjsP != resultObjs)
        ckfree((char *) resultObjsP);

    return status;
}

/* Returns a Tcl_Obj for a TArray slot. NOTE: WITHOUT its ref count incremented */
Tcl_Obj * TArrayIndex(Tcl_Interp *interp, TAHdr *thdrP, Tcl_Obj *indexObj)
{
    int index;

    if (IndexToInt(interp, indexObj, &index, thdrP->used-1) != TCL_OK)
        return NULL;
    if (index < 0 || index >= thdrP->used) {
        TArrayIndexRangeError(interp, indexObj);
        return NULL;
    }

    switch (thdrP->type) {
    case TARRAY_BOOLEAN:
        return Tcl_NewIntObj(ba_get(TAHDRELEMPTR(thdrP, ba_t, 0), index));
    case TARRAY_UINT:
        return Tcl_NewWideIntObj(*TAHDRELEMPTR(thdrP, unsigned int, index));
    case TARRAY_INT:
        return Tcl_NewIntObj(*TAHDRELEMPTR(thdrP, int, index));
    case TARRAY_WIDE:
        return Tcl_NewWideIntObj(*TAHDRELEMPTR(thdrP, Tcl_WideInt, index));
    case TARRAY_DOUBLE:
        return Tcl_NewDoubleObj(*TAHDRELEMPTR(thdrP, double, index));
    case TARRAY_BYTE:
        return Tcl_NewIntObj(*TAHDRELEMPTR(thdrP, unsigned char, index));
    case TARRAY_OBJ:
        return *TAHDRELEMPTR(thdrP, Tcl_Obj *, index);
    }
}

/* Returns a TAHdr of type int. The header's ref count is incremented
 * so caller should call TAHDR_DECRREF as appropriate
 */
TAHdr *TArrayConvertToIndices(Tcl_Interp *interp, Tcl_Obj *objP)
{
    TAHdr *thdrP;
    Tcl_Obj **elems;
    int       nelems;

    /* Indices should be a tarray of ints. If not, treat as a list
     * and convert it that way. Though that is slower, it should be rare
     * as all tarray indices are returned already in the proper format.
     */
    if (objP->typePtr == &gTArrayType && TARRAYTYPE(objP) == TARRAY_INT) {
        thdrP = TARRAYHDR(objP);
        thdrP->nrefs++;
        return thdrP;
    }

    if (Tcl_ListObjGetElements(interp, objP, &nelems, &elems) != TCL_OK)
        return NULL;

    thdrP = TArrayAllocAndInit(interp, TARRAY_INT, nelems, elems, 0);
    if (thdrP)
        thdrP->nrefs++;
    return thdrP;
}

/* Returns a newly allocated TAHdr (with ref count 0) containing the
   values from the specified indices */
TAHdr *TArrayGetValues(Tcl_Interp *interp, TAHdr *srcP, TAHdr *indicesP)
{
    TAHdr *thdrP;
    int i, count;
    int *indexP;

    if (indicesP->type != TARRAY_INT) {
        if (interp)
            Tcl_SetResult(interp, "Invalid type for tarray indices", TCL_STATIC);
        return NULL;
    }

    count = indicesP->used;
    thdrP = TArrayAlloc(srcP->type, count);
    if (thdrP == 0 || count == 0)
        return thdrP;

    indexP = TAHDRELEMPTR(indicesP, int, 0);

    switch (srcP->type) {
    case TARRAY_BOOLEAN:
        {
            ba_t *baP = TAHDRELEMPTR(thdrP, ba_t, 0);
            for (i = 0; i < count; ++i, ++indexP)
                ba_put(baP, i, *indexP);
        }
        break;
    case TARRAY_UINT:
    case TARRAY_INT:
        {
            unsigned int *uiP = TAHDRELEMPTR(thdrP, unsigned int, 0);
            for (i = 0; i < count; ++i, ++indexP, ++uiP)
                *uiP = *TAHDRELEMPTR(srcP, unsigned int, *indexP);
        }
        break;
    case TARRAY_WIDE:
        {
            Tcl_WideInt *wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, 0);
            for (i = 0; i < count; ++i, ++indexP, ++wideP)
                *wideP = *TAHDRELEMPTR(srcP, Tcl_WideInt, *indexP);
        }
        break;
    case TARRAY_DOUBLE:
        {
            double *dblP = TAHDRELEMPTR(thdrP, double, 0);
            for (i = 0; i < count; ++i, ++indexP, ++dblP)
                *dblP = *TAHDRELEMPTR(srcP, double, *indexP);
        }
        break;
    case TARRAY_BYTE:
        {
            unsigned char *ucP = TAHDRELEMPTR(thdrP, unsigned char, 0);
            for (i = 0; i < count; ++i, ++indexP, ++ucP)
                *ucP = *TAHDRELEMPTR(srcP, unsigned char, *indexP);
        }
        break;
    case TARRAY_OBJ:
        {
            Tcl_Obj **objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, 0);
            for (i = 0; i < count; ++i, ++indexP, ++objPP) {
                *objPP = *TAHDRELEMPTR(srcP, Tcl_Obj *, *indexP);
                Tcl_IncrRefCount(*objPP);
            }
        }
        break;
    default:
        TArrayTypePanic(srcP->type);
    }

    thdrP->used = count;
    return thdrP;
}


static TCL_RESULT TArraySearchBoolean(Tcl_Interp *interp, TAHdr * haystackP,
                                      Tcl_Obj *needleObj, int start,
                                      enum TArraySearchSwitches op, int flags)
{
    int bval;
    ba_t *baP;
    int pos;
    Tcl_Obj *resultObj;

    TARRAY_ASSERT(haystackP->type == TARRAY_BOOLEAN);

    if (op != TARRAY_SEARCH_OPT_EQ)
        return TArrayBadSearchOpError(interp, op);

    if (Tcl_GetBooleanFromObj(interp, needleObj, &bval) != TCL_OK)
        return TCL_ERROR;
    
    if (flags & TARRAY_SEARCH_INVERT)
        bval = !bval;

    /* First locate the starting point for the search */
    baP = TAHDRELEMPTR(haystackP, ba_t, 0);

    if (flags & TARRAY_SEARCH_ALL) {
        TAHdr *thdrP;
        thdrP = TArrayAlloc(
            flags & TARRAY_SEARCH_INLINE ? TARRAY_BOOLEAN : TARRAY_INT,
            10);                /* Assume 10 hits */
        pos = start;
        while ((pos = ba_find(baP, bval, pos, thdrP->used)) != -1) {
            /* Ensure enough space in target array */
            if (thdrP->used >= thdrP->allocated)
                thdrP = TArrayRealloc(thdrP, thdrP->used + TARRAY_EXTRA(thdrP->used));
            if (flags & TARRAY_SEARCH_INLINE)
                ba_put(TAHDRELEMPTR(thdrP, ba_t, 0), thdrP->used, bval);
            else
                *TAHDRELEMPTR(thdrP, int, thdrP->used) = pos;
            thdrP->used++;
            ++pos;
        }

        resultObj = TArrayNewObj(thdrP);
    } else {
        /* Return first found element */
        pos = ba_find(baP, bval, start, haystackP->used);
        resultObj = pos == -1 ?
            Tcl_NewObj() :
            Tcl_NewIntObj((flags & TARRAY_SEARCH_INLINE) ? bval : pos);
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}
                        
/* TBD - see how much performance is gained by separating this search function into
   type-specific functions */
static TCL_RESULT TArraySearchEntier(Tcl_Interp *interp, TAHdr * haystackP,
                                     Tcl_Obj *needleObj, int start, enum TArraySearchSwitches op, int flags)
{
    int offset;
    Tcl_Obj *resultObj;
    Tcl_WideInt needle, elem, min_val, max_val;
    int compare_result;
    int compare_wanted;
    int elem_size;
    char *p;

    switch (op) {
    case TARRAY_SEARCH_OPT_GT:
    case TARRAY_SEARCH_OPT_LT: 
    case TARRAY_SEARCH_OPT_EQ:
        break;
    default:
        return TArrayBadSearchOpError(interp, op);
    }

    if (Tcl_GetWideIntFromObj(interp, needleObj, &needle) != TCL_OK)
        return TCL_ERROR;

    p = TAHDRELEMPTR(haystackP, char, 0);
    switch (haystackP->type) {
    case TARRAY_INT:
        max_val = INT_MAX;
        min_val = INT_MIN;
        p += start * sizeof(int);
        elem_size = sizeof(int);
        break;
    case TARRAY_UINT:
        max_val = UINT_MAX;
        min_val = 0;
        p += start * sizeof(unsigned int);
        elem_size = sizeof(unsigned int);
        break;
    case TARRAY_WIDE:
        max_val = needle; /* No-op */
        min_val = needle;
        p += start * sizeof(Tcl_WideInt);
        elem_size = sizeof(Tcl_WideInt);
        break;
    case TARRAY_BYTE:
        max_val = UCHAR_MAX;
        min_val = 0;
        p += start * sizeof(unsigned char);
        elem_size = sizeof(unsigned char);
        break;
    default:
        TArrayTypePanic(haystackP->type);
    }

    if (needle > max_val || needle < min_val) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Integer \"%s\" type mismatch for typearray (type %d).", Tcl_GetString(needleObj), haystackP->type));
        return TCL_ERROR;
    }

    compare_wanted = flags & TARRAY_SEARCH_INVERT ? 0 : 1;

    if (flags & TARRAY_SEARCH_ALL) {
        TAHdr *thdrP;

        thdrP = TArrayAlloc(
            flags & TARRAY_SEARCH_INLINE ? haystackP->type : TARRAY_INT,
            10);                /* Assume 10 hits TBD */

        for (offset = start; offset < haystackP->used; ++offset, p += elem_size) {
            switch (haystackP->type) {
            case TARRAY_INT:  elem = *(int *)p; break;
            case TARRAY_UINT: elem = *(unsigned int *)p; break;
            case TARRAY_WIDE: elem = *(Tcl_WideInt *)p; break;
            case TARRAY_BYTE: elem = *(unsigned char *)p; break;
            }
            switch (op) {
            case TARRAY_SEARCH_OPT_GT: compare_result = (elem > needle); break;
            case TARRAY_SEARCH_OPT_LT: compare_result = (elem < needle); break;
            case TARRAY_SEARCH_OPT_EQ:
            default: compare_result = (elem == needle); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdrP->used >= thdrP->allocated)
                    thdrP = TArrayRealloc(thdrP, thdrP->used + TARRAY_EXTRA(thdrP->used));
                if (flags & TARRAY_SEARCH_INLINE) {
                    switch (thdrP->type) {
                    case TARRAY_INT:  *TAHDRELEMPTR(thdrP, int, thdrP->used) = (int) elem; break;
                    case TARRAY_UINT: *TAHDRELEMPTR(thdrP, unsigned int, thdrP->used) = (unsigned int) elem; break;
                    case TARRAY_WIDE: *TAHDRELEMPTR(thdrP, Tcl_WideInt, thdrP->used) = elem; break;
                    case TARRAY_BYTE:  *TAHDRELEMPTR(thdrP, unsigned char, thdrP->used) = (unsigned char) elem; break;
                    }
                } else {
                    *TAHDRELEMPTR(thdrP, int, thdrP->used) = offset;
                }
                thdrP->used++;
            }
        }

        resultObj = TArrayNewObj(thdrP);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, p += elem_size) {
            switch (haystackP->type) {
            case TARRAY_INT:  elem = *(int *)p; break;
            case TARRAY_UINT: elem = *(unsigned int *)p; break;
            case TARRAY_WIDE: elem = *(Tcl_WideInt *)p; break;
            case TARRAY_BYTE: elem = *(unsigned char *)p; break;
            }
            switch (op) {
            case TARRAY_SEARCH_OPT_GT: compare_result = (elem > needle); break;
            case TARRAY_SEARCH_OPT_LT: compare_result = (elem < needle); break;
            case TARRAY_SEARCH_OPT_EQ:
            default: compare_result = (elem == needle); break;
            }
            if (compare_result == compare_wanted)
                break;
        }
        if (offset >= haystackP->used) {
            /* No match */
            resultObj = Tcl_NewObj();
        } else {
            if (flags & TARRAY_SEARCH_INLINE)
                resultObj = Tcl_NewWideIntObj(elem);
            else
                resultObj = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}


static TCL_RESULT TArraySearchDouble(Tcl_Interp *interp, TAHdr * haystackP,
                                          Tcl_Obj *needleObj, int start, enum TArraySearchSwitches op, int flags)
{
    int offset;
    Tcl_Obj *resultObj;
    double dval, *dvalP;
    int compare_result;
    int compare_wanted;

    TARRAY_ASSERT(haystackP->type == TARRAY_DOUBLE);

    switch (op) {
    case TARRAY_SEARCH_OPT_GT:
    case TARRAY_SEARCH_OPT_LT: 
    case TARRAY_SEARCH_OPT_EQ:
        break;
    default:
        return TArrayBadSearchOpError(interp, op);
    }

    if (Tcl_GetDoubleFromObj(interp, needleObj, &dval) != TCL_OK)
        return TCL_ERROR;
    
    compare_wanted = flags & TARRAY_SEARCH_INVERT ? 0 : 1;

    /* First locate the starting point for the search */
    dvalP = TAHDRELEMPTR(haystackP, double, start);

    if (flags & TARRAY_SEARCH_ALL) {
        TAHdr *thdrP;

        thdrP = TArrayAlloc(
            flags & TARRAY_SEARCH_INLINE ? TARRAY_DOUBLE : TARRAY_INT,
            10);                /* Assume 10 hits */

        for (offset = start; offset < haystackP->used; ++offset, ++dvalP) {
            switch (op) {
            case TARRAY_SEARCH_OPT_GT: compare_result = (*dvalP > dval); break;
            case TARRAY_SEARCH_OPT_LT: compare_result = (*dvalP < dval); break;
            case TARRAY_SEARCH_OPT_EQ: compare_result = (*dvalP == dval); break;
            }

            if (compare_result == compare_wanted) {
                /* Have a match */
                /* Ensure enough space in target array */
                if (thdrP->used >= thdrP->allocated)
                    thdrP = TArrayRealloc(thdrP, thdrP->used + TARRAY_EXTRA(thdrP->used));
                if (flags & TARRAY_SEARCH_INLINE) {
                    *TAHDRELEMPTR(thdrP, double, thdrP->used) = *dvalP;
                } else {
                    *TAHDRELEMPTR(thdrP, int, thdrP->used) = offset;
                }
                thdrP->used++;
            }
        }

        resultObj = TArrayNewObj(thdrP);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++dvalP) {
            switch (op) {
            case TARRAY_SEARCH_OPT_GT: compare_result = (*dvalP > dval); break;
            case TARRAY_SEARCH_OPT_LT: compare_result = (*dvalP < dval); break;
            case TARRAY_SEARCH_OPT_EQ: compare_result = (*dvalP == dval); break;
            }
            if (compare_result == compare_wanted)
                break;
        }
        if (offset >= haystackP->used) {
            /* No match */
            resultObj = Tcl_NewObj();
        } else {
            if (flags & TARRAY_SEARCH_INLINE)
                resultObj = Tcl_NewDoubleObj(*dvalP);
            else
                resultObj = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}

static TCL_RESULT TArraySearchObj(Tcl_Interp *interp, TAHdr * haystackP,
                                  Tcl_Obj *needleObj, int start, enum TArraySearchSwitches op, int flags)
{
    int offset;
    Tcl_Obj **objPP;
    Tcl_Obj *resultObj;
    int compare_result;
    int compare_wanted;
    int nocase;
    Tcl_RegExp re;

    /* TBD - do we need to increment the haystacP ref to guard against shimmering */
    TARRAY_ASSERT(haystackP->type == TARRAY_OBJ);
    
    compare_wanted = flags & TARRAY_SEARCH_INVERT ? 0 : 1;
    nocase = flags & TARRAY_SEARCH_NOCASE;

    switch (op) {
    case TARRAY_SEARCH_OPT_GT:
    case TARRAY_SEARCH_OPT_LT: 
    case TARRAY_SEARCH_OPT_EQ:
    case TARRAY_SEARCH_OPT_PAT:
        break;
    case TARRAY_SEARCH_OPT_RE:
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
        return TArrayBadSearchOpError(interp, op);
    }

    /* First locate the starting point for the search */
    objPP = TAHDRELEMPTR(haystackP, Tcl_Obj *, start);


    if (flags & TARRAY_SEARCH_ALL) {
        TAHdr *thdrP;

        thdrP = TArrayAlloc(
            flags & TARRAY_SEARCH_INLINE ? TARRAY_OBJ : TARRAY_INT,
            10);                /* Assume 10 hits */

        for (offset = start; offset < haystackP->used; ++offset, ++objPP) {
            switch (op) {
            case TARRAY_SEARCH_OPT_GT:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) > 0; break;
            case TARRAY_SEARCH_OPT_LT: 
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) < 0; break;
            case TARRAY_SEARCH_OPT_EQ:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) == 0; break;
            case TARRAY_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(*objPP),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TARRAY_SEARCH_OPT_RE:
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
                    thdrP = TArrayRealloc(thdrP, thdrP->used + TARRAY_EXTRA(thdrP->used));
                if (flags & TARRAY_SEARCH_INLINE) {
                    Tcl_IncrRefCount(*objPP);
                    *TAHDRELEMPTR(thdrP, Tcl_Obj *, thdrP->used) = *objPP;
                } else {
                    *TAHDRELEMPTR(thdrP, int, thdrP->used) = offset;
                }
                thdrP->used++;
            }
        }

        resultObj = TArrayNewObj(thdrP);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++objPP) {
            switch (op) {
            case TARRAY_SEARCH_OPT_GT:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) > 0; break;
            case TARRAY_SEARCH_OPT_LT: 
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) < 0; break;
            case TARRAY_SEARCH_OPT_EQ:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) == 0; break;
            case TARRAY_SEARCH_OPT_PAT:
                compare_result = Tcl_StringCaseMatch(Tcl_GetString(*objPP),
                                                     Tcl_GetString(needleObj),
                                                     nocase ? TCL_MATCH_NOCASE : 0);
                break;
            case TARRAY_SEARCH_OPT_RE:
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
            if (flags & TARRAY_SEARCH_INLINE)
                resultObj = *objPP; /* No need to incr ref, the SetObjResult does it */
            else
                resultObj = Tcl_NewIntObj(offset);
        }
    }

    Tcl_SetObjResult(interp, resultObj);
    return TCL_OK;
}

TCL_RESULT TArray_SearchObjCmd(ClientData clientdata, Tcl_Interp *interp,
                              int objc, Tcl_Obj *const objv[])
{
    int flags;
    int start_index;
    int i, n, opt;
    TAHdr *haystackP;
    enum TArraySearchSwitches op;

    if (objc < 3) {
	Tcl_WrongNumArgs(interp, 1, objv, "?options? tarray pattern");
	return TCL_ERROR;
    }

    if (TArrayVerifyType(interp, objv[objc-2]) != TCL_OK)
        return TCL_ERROR;
    haystackP = TARRAYHDR(objv[objc-2]);
    flags = 0;
    start_index = 0;
    op = TARRAY_SEARCH_OPT_EQ;
    for (i = 1; i < objc-2; ++i) {
	if (Tcl_GetIndexFromObj(interp, objv[i], TArraySearchSwitches, "option", 0, &opt)
            != TCL_OK) {
            return TCL_ERROR;
	}
        switch ((enum TArraySearchSwitches) opt) {
        case TARRAY_SEARCH_OPT_ALL: flags |= TARRAY_SEARCH_ALL; break;
        case TARRAY_SEARCH_OPT_INLINE: flags |= TARRAY_SEARCH_INLINE; break;
        case TARRAY_SEARCH_OPT_INVERT: flags |= TARRAY_SEARCH_INVERT; break;
        case TARRAY_SEARCH_OPT_NOCASE: flags |= TARRAY_SEARCH_NOCASE; break;
        case TARRAY_SEARCH_OPT_START:
            if (i > objc-4)
                return TArrayBadArgError(interp, "-start");
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
        case TARRAY_SEARCH_OPT_EQ:
        case TARRAY_SEARCH_OPT_GT:
        case TARRAY_SEARCH_OPT_LT:
        case TARRAY_SEARCH_OPT_PAT:
        case TARRAY_SEARCH_OPT_RE:
            op = (enum TArraySearchSwitches) opt;
        }
    }

    switch (haystackP->type) {
    case TARRAY_BOOLEAN:
        return TArraySearchBoolean(interp, haystackP, objv[objc-1], start_index,op,flags);
    case TARRAY_INT:
    case TARRAY_UINT:
    case TARRAY_BYTE:
    case TARRAY_WIDE:
        return TArraySearchEntier(interp, haystackP, objv[objc-1], start_index, op, flags);
    case TARRAY_DOUBLE:
        return TArraySearchDouble(interp, haystackP, objv[objc-1], start_index, op, flags);
    case TARRAY_OBJ:
        return TArraySearchObj(interp, haystackP, objv[objc-1], start_index, op, flags);
    default:
        Tcl_SetResult(interp, "Not implemented", TCL_STATIC);
        return TCL_ERROR;
    }

}

int TArrayCompareObjs(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;
    int alen, blen, len;
    int comparison;

    a = Tcl_GetStringFromObj(oaP, &alen);
    alen = Tcl_NumUtfChars(a, alen); /* Num bytes -> num chars */
    b = Tcl_GetStringFromObj(obP, &blen);
    blen = Tcl_NumUtfChars(b, blen); /* Num bytes -> num chars */

    len = alen < blen ? alen : blen; /* len is the shorter length */
    
    comparison = (ignorecase ? Tcl_UtfNcasecmp : Tcl_UtfNcmp)(a, b, len);

    if (comparison == 0) {
        comparison = alen-blen;
    }
    return (comparison > 0) ? 1 : (comparison < 0) ? -1 : 0;
}
