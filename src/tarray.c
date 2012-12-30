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

#define TA_ENABLE_ASSERT 1
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
    "uint",
    "int",
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
    TA_SEARCH_OPT_ALL, TA_SEARCH_OPT_INLINE, TA_SEARCH_OPT_INVERT, TA_SEARCH_OPT_START, TA_SEARCH_OPT_EQ, TA_SEARCH_OPT_GT, TA_SEARCH_OPT_LT, TA_SEARCH_OPT_PAT, TA_SEARCH_OPT_RE, TA_SEARCH_OPT_NOCASE
};
/* Search flags */
#define TA_SEARCH_INLINE 1  /* Return values, not indices */
#define TA_SEARCH_INVERT 2  /* Invert matching expression */
#define TA_SEARCH_ALL    4  /* Return all matches */
#define TA_SEARCH_NOCASE 8  /* Ignore case */

const char *TArrayTypeString(int tatype)
{
    if (tatype < (sizeof(gTArrayTypeTokens)/sizeof(gTArrayTypeTokens[0]))) {
        return gTArrayTypeTokens[tatype];
    } else
        return "<invalid>";
}

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
        Tcl_SetErrorCode(interp, "TARRAY", "TCLOBJTYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayBadTypeError(Tcl_Interp *interp, TAHdr *thdrP)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("tarray is of the wrong type (%s)",
                                       TArrayTypeString(thdrP->type)));
        Tcl_SetErrorCode(interp, "TARRAY", "TYPE", NULL);
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

TCL_RESULT TArrayValueTypeError(Tcl_Interp *interp, Tcl_Obj *objP, int tatype)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Value %s not valid for typed array of type %s.",
                                       Tcl_GetString(objP),
                                       TArrayTypeString(tatype)));
        Tcl_SetErrorCode(interp, "TARRAY", "VALUE", "TYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayRowWidthError(Tcl_Interp *interp, int row_width, int grid_width)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Row width %d does not match grid width %d.", row_width, grid_width));
        Tcl_SetErrorCode(interp, "TARRAY", "ROW", "WIDTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayGridLengthError(Tcl_Interp *interp)
{
    if (interp) {
        Tcl_SetResult(interp,
                      "Columns in tarray grid have differing lengths.",
                      TCL_STATIC);
        Tcl_SetErrorCode(interp, "TARRAY", "GRID", "LENGTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT TArrayNoMemError(Tcl_Interp *interp, int req_size)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Memory allocation failed (%d bytes).",
                                       req_size));
        Tcl_SetErrorCode(interp, "TARRAY", "NOMEM", NULL);
    }
    return TCL_ERROR;
}

/*
 * Map numeric or string index to numeric integer index.
 */
TCL_RESULT IndexToInt(Tcl_Interp *interp, Tcl_Obj *objP, int *indexP, int end_value, int low, int high)
{
    char *s;
    int val;

    if (Tcl_GetIntFromObj(NULL, objP, &val) != TCL_OK) {
        s = Tcl_GetString(objP);
        if (strcmp(s, "end")) {
            if (interp != NULL) {
                Tcl_SetObjResult(interp, Tcl_ObjPrintf(
                                     "bad index '%s': must be 'end' or an integer value", s));
                Tcl_SetErrorCode(interp, "TARRAY", "VALUE", "INDEX", NULL);
            }
            return TCL_ERROR;
        }
        val = end_value;
    }
    
    if (val < low || val > high)
        return TArrayIndexRangeError(interp, objP);
    else
        return TCL_OK;
}

/* low has to be between 0 and one beyond last element.
   high is 0-INT_MAX
   if (high < low) count is returned as 0 (not an error)
*/
TCL_RESULT RationalizeRangeIndices(Tcl_Interp *interp, TAHdr *thdrP, Tcl_Obj *lowObj, Tcl_Obj *highObj, int *lowP, int *countP)
{
    int low, high;

    /* TBD - We allow low index to be 1 greater than last element. Caller should
     * check for this if appropriate. High index can be any greater value
     * than lower range.
     */
    if (IndexToInt(interp, lowObj, &low, thdrP->used-1, 0, thdrP->used) != TCL_OK)
        return TCL_ERROR;

    if (IndexToInt(interp, highObj, &high, thdrP->used-1, 0, INT_MAX) != TCL_OK)
        return TCL_ERROR;

    *lowP = low;
    if (high < low)
        *countP = 0;            /* This is how lrange behaves */
    else
        *countP = high - low + 1;

    return TCL_OK;
}

TCL_RESULT TArrayConvert(Tcl_Interp *interp, Tcl_Obj *objP)
{
    Tcl_Obj **elems;
    int nelems, tatype;
    
    if (objP->typePtr == &gTArrayType)
        return TCL_OK;

    /* See if we can convert it to one based on string representation */
    if (Tcl_ListObjGetElements(NULL, objP, &nelems, &elems) == TCL_OK
        && nelems == 3
        && !strcmp(Tcl_GetString(elems[0]), "tarray")
        && Tcl_GetIndexFromObj(interp, elems[1], gTArrayTypeTokens, "TArrayType",
                               TCL_EXACT, &tatype) == TCL_OK) {
        /* So far so good. Try and convert */
        TAHdr *thdrP;
        Tcl_Obj **valueObjs;
        int nvalues;
        
        if (Tcl_ListObjGetElements(interp, elems[2], &nvalues, &valueObjs)
            != TCL_OK)
            return TCL_ERROR;

        thdrP = TArrayAllocAndInit(interp, tatype, nvalues, valueObjs, 0);
        if (thdrP == NULL)
            return TCL_ERROR;

        /*
         * Get rid of old representation and stick in the new one. Note
         * string rep is NOT invalidated and must NOT be if it is shared.
         * In any case, no need to do so here.
         */
        if (objP->typePtr && objP->typePtr->freeIntRepProc) {
            objP->typePtr->freeIntRepProc(objP);
            objP->typePtr = NULL;
        }

        TA_OBJ_SETREP(objP, thdrP);
        return TCL_OK;
    }
                
    return TArrayNotTArrayError(interp);
}

TCL_RESULT TArrayValueFromObj(Tcl_Interp *interp, Tcl_Obj *objP,
                              unsigned char tatype, TArrayValue *tavP)
{
    int i;
    switch (tatype) {
    case TA_BOOLEAN:
        if (Tcl_GetBooleanFromObj(interp, objP, &i) != TCL_OK)
            return TCL_ERROR;
        tavP->bval = (i != 0);
        break;
    case TA_BYTE:
    case TA_INT:
        if (Tcl_GetIntFromObj(interp, objP, &tavP->ival) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TA_INT)
            break;
        if (tavP->ival > 255 || tavP->ival < 0)
            return TArrayValueTypeError(interp, objP, tatype);
        tavP->ucval = (unsigned char) tavP->ival;
        break;
    case TA_UINT:
    case TA_WIDE:
        if (Tcl_GetWideIntFromObj(interp, objP, &tavP->wval) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TA_WIDE)
            break;
        if (tavP->wval < 0 || tavP->wval > 0xFFFFFFFF)
            return TArrayValueTypeError(interp, objP, tatype);
        tavP->uival = (unsigned int) tavP->wval;
        break;
    case TA_DOUBLE:
        if (Tcl_GetDoubleFromObj(interp, objP, &tavP->dval) != TCL_OK)
            return TCL_ERROR;
        break;
    case TA_OBJ:
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

    TA_ASSERT(! TAHDR_SHARED(thdrP));
    TA_ASSERT((pos+count) <= thdrP->allocated);
    TA_ASSERT(pos <= thdrP->used);
    TA_ASSERT(thdrP->type == tavP->type);

    switch (thdrP->type) {
    case TA_BOOLEAN:
        ba_fill(TAHDRELEMPTR(thdrP, ba_t, 0), pos, count, tavP->bval);
        break;
    case TA_INT:
    case TA_UINT:
        if (tavP->ival == 0) {
            memset(TAHDRELEMPTR(thdrP, int, pos), 0, count*sizeof(int));
        } else {
            int *iP;
            iP = TAHDRELEMPTR(thdrP, int, pos);
            for (i = 0; i < count; ++i, ++iP)
                *iP = tavP->ival;
        }
        break;
        
    case TA_BYTE:
        memset(TAHDRELEMPTR(thdrP, unsigned char, pos), tavP->ucval, count);
        break;

    case TA_WIDE:
        if (tavP->wval == 0) {
            memset(TAHDRELEMPTR(thdrP, Tcl_WideInt, pos), 0, count*sizeof(Tcl_WideInt));
        } else {
            Tcl_WideInt *wideP;
            wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, pos);
            for (i = 0; i < count; ++i, ++wideP)
                *wideP = tavP->wval;
        }
        break;
    case TA_DOUBLE:
        {
            double *dvalP;
            dvalP = TAHDRELEMPTR(thdrP, double, pos);
            for (i = 0; i < count; ++i, ++dvalP)
                *dvalP = tavP->dval;
        }
        break;
    case TA_OBJ:
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

    if (thdrP->type == TA_OBJ) {
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

    if (thdrP->type == TA_OBJ) {
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
    if (thdrP->type == TA_OBJ) {
        TArrayDecrObjRefs(thdrP, 0, thdrP->used);
    }
    TA_FREEMEM(thdrP);
}


static void TArrayTypeFreeRep(Tcl_Obj *objP)
{
    TAHdr *thdrP;

    TA_ASSERT(objP->typePtr == &gTArrayType);

    thdrP = TARRAYHDR(objP); 
    TA_ASSERT(thdrP);

    TAHDR_DECRREF(thdrP);
    TARRAYHDR(objP) = NULL;
    objP->typePtr = NULL;
}

static void TArrayTypeDupObj(Tcl_Obj *srcObj, Tcl_Obj *dstObj)
{
    TA_ASSERT(srcObj->typePtr == &gTArrayType);
    TA_ASSERT(TARRAYHDR(srcObj) != NULL);
        
    TA_OBJ_SETREP(dstObj, TARRAYHDR(srcObj));
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
         * We know objc <= TA_MAX_OBJC, so this is safe.
         */

        flagPtr = (int *) TA_ALLOCMEM(objc * sizeof(int));
    }

    bytesNeeded +=
        sizeof("tarray ") - 1 /* -1 to exclude the null */
        + sizeof(" {") - 1 /* Start of list minus trailing null */
        + 1               /* Trailing "}" */
        + strlen(gTArrayTypeTokens[TA_OBJ]);
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

    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    objP->bytes = ckalloc(bytesNeeded);
    dst = objP->bytes;
    memcpy(dst, "tarray ", sizeof("tarray ")-1);
    dst += sizeof("tarray ") - 1;
    strcpy(dst, gTArrayTypeTokens[TA_OBJ]);
    dst += strlen(gTArrayTypeTokens[TA_OBJ]);
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
            TA_ASSERT((dst-objP->bytes) < bytesNeeded);
        }
        dst[-1] = '}';
    } else
        *dst++ = '}';
    *dst = '\0';
    TA_ASSERT((dst-objP->bytes) < bytesNeeded);
    objP->length = dst - objP->bytes;

    if (flagPtr != localFlags) {
        TA_FREEMEM((char *) flagPtr);
    }
}


static void TArrayTypeUpdateStringRep(Tcl_Obj *objP)
{
    unsigned int i, n, count;
    unsigned int allocated, unused, min_needed, prefix_len;
    char *cP;
    int max_elem_space;  /* Max space to print one element including
                            either terminating null or space */
    TAHdr *thdrP;
        
    TA_ASSERT(objP->typePtr == &gTArrayType);

    thdrP = TARRAYHDR(objP);
    TA_ASSERT(thdrP->type < sizeof(gTArrayTypeTokens)/sizeof(gTArrayTypeTokens[0]));

    objP->bytes = NULL;

    prefix_len = 
        sizeof("tarray ") - 1   /* -1 to exclude the null */
        + strlen(gTArrayTypeTokens[thdrP->type])
        + 2;                         /* Start of list " {" */
    min_needed = prefix_len + 1 + 1;            /* Trailing "}" and null */

    count = TARRAYELEMCOUNT(objP);
    if (count == 0) {
        /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
           defined as ckalloc */
        cP = ckalloc(min_needed);
        objP->bytes = cP;
        _snprintf(cP, min_needed, "tarray %s {}",
                  gTArrayTypeTokens[thdrP->type]);
        objP->length = min_needed - 1;
        return;
    }

    /* Code below based on count > 0 else terminating \0 will blow memory */

    /*
     * When output size cannot be calculated exactly, we allocate using
     * some estimate based on the type.
     */
        
    switch (TARRAYTYPE(objP)) {
    case TA_BOOLEAN:
        {
            /*
             * Special case Boolean since we know exactly how many chars will
             * be required 
             */
            ba_t *baP = TAHDRELEMPTR(thdrP, ba_t, 0);
            register ba_t ba = *baP;
            register ba_t ba_mask;

            /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
               defined as ckalloc */
            cP = ckalloc(min_needed + 2*count - 1);
            n = _snprintf(cP, min_needed, "tarray %s {",
                      gTArrayTypeTokens[TA_BOOLEAN]);
            TA_ASSERT(n > 0 && n < min_needed);
            objP->bytes = cP;
            cP += n;
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
            cP[-1] = '}';
            *cP = '\0';
            objP->length = cP - objP->bytes;
        }
        return;
                
    case TA_OBJ:
        UpdateStringForObjType(objP);
        return;
                
    case TA_UINT:
    case TA_INT:
        TA_ASSERT(sizeof(int) == 4); /* So max string space needed is 11 */
        max_elem_space = 11;
        break;
    case TA_WIDE:
        max_elem_space = TCL_INTEGER_SPACE;
        break;
    case TA_DOUBLE:
        max_elem_space = TCL_DOUBLE_SPACE;
        break;
    case TA_BYTE:
        max_elem_space = 3;
        break;
    default:
        TArrayTypePanic(thdrP->type);
    }
            
    /*
     * Assume an element averages half max space with room for at
     * least one max element. Note max_elem_space includes trailing ' '
     */
    allocated = min_needed + max_elem_space + ((max_elem_space + 1)/2)*count;
    unused = allocated - prefix_len;
    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    cP = ckalloc(allocated);
    objP->bytes = cP;
    _snprintf(cP, prefix_len+1, "tarray %s {", gTArrayTypeTokens[thdrP->type]);
    TA_ASSERT(strlen(cP) == prefix_len);
    cP += prefix_len;
    min_needed = max_elem_space + 2; /* space or terminating "}" and null */
    for (i = 0; i < count; ) {
        if (unused < min_needed) {
            n = allocated - unused; /* Used space */
            /* Increase assuming average space taken so far (roughly) */
            TA_ASSERT(i != 0);
            allocated += min_needed + (count - i) * (n/i);
            objP->bytes = ckrealloc(objP->bytes, allocated);
            cP = n + (char *) objP->bytes;
            unused = allocated - n;
        }
        /*
         * We nest loops for performance by minimizing switch jumps
         * At top of nested loops below, there is room for at least one elem
         */
        switch (thdrP->type) {
        case TA_UINT:
        case TA_INT:
            {
                int *intP = TAHDRELEMPTR(thdrP, int, i);
                char *fmt = thdrP->type == TA_UINT ? "%u" : "%d";
                while (i < count && unused >= min_needed) {
                    n = _snprintf(cP, unused, fmt, *intP++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_WIDE:
            {
                Tcl_WideInt *wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, i);
                while (i < count && unused >= min_needed) {
                    n = _snprintf(cP, unused, "%" TCL_LL_MODIFIER "d", *wideP++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_DOUBLE:
            /* Do not use _snprintf because of slight difference
               it does not include decimal point for whole ints. For
               consistency with Tcl, use Tcl_PrintDouble instead */
            {
                double *dblP = TAHDRELEMPTR(thdrP, double, i);
                while (i < count && unused >= min_needed) {
                    Tcl_PrintDouble(NULL, *dblP++, cP);
                    n = strlen(cP);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_BYTE:
            {
                unsigned char *ucP = TAHDRELEMPTR(thdrP, unsigned char, i);
                while (i < count && unused >= min_needed) {
                    n = _snprintf(cP, unused, "%u", *ucP++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        }
    }

    TA_ASSERT(unused >=1 );
    cP[-1] = '}';         /* Terminate list */
    *cP = '\0';
    objP->length = cP - objP->bytes; /* Terminating null not included in length */
            
    /* Only shrink array if unused space is comparatively too large */
    unused = allocated - (objP->length + 1);
    if (unused > (allocated / 8) && unused > 20)
        objP->bytes = ckrealloc(objP->bytes, objP->length + 1);
    return;
}

Tcl_Obj *TArrayNewObj(TAHdr *thdrP)
{
    Tcl_Obj *objP = Tcl_NewObj();
    Tcl_InvalidateStringRep(objP);
    TA_OBJ_SETREP(objP, thdrP);
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

    TA_ASSERT(thdrP->nrefs < 2);
    TA_ASSERT((first + nelems) <= thdrP->allocated);

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
     * Also for TA_OBJ there is no question of conversion and hence
     * no question of conversion errors.
     *
     */

    if (first < thdrP->used && thdrP->type != TA_OBJ) {
        /* Not appending, need to verify conversion */
        switch (thdrP->type) {
        case TA_BOOLEAN:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetBooleanFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TA_UINT:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
                if (wide < 0 || wide > 0xFFFFFFFF) {
                    TArrayValueTypeError(interp, elems[i], thdrP->type);
                    goto convert_error;
                }
            }
            break;

        case TA_INT:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TA_WIDE:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TA_DOUBLE:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetDoubleFromObj(interp, elems[i], &dval) != TCL_OK)
                    goto convert_error;
            }
            break;

        case TA_BYTE:
            for (i = 0; i < nelems; ++i) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival > 255 || ival < 0) {
                    TArrayValueTypeError(interp, elems[i], thdrP->type);
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
    case TA_BOOLEAN:
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

    case TA_UINT:
        {
            register unsigned int *uintP;
            uintP = TAHDRELEMPTR(thdrP, unsigned int, first);
            for (i = 0; i < nelems; ++i, ++uintP) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
                if (wide < 0 || wide > 0xFFFFFFFF) {
                    TArrayValueTypeError(interp, elems[i], thdrP->type);
                    goto convert_error;
                }
                *uintP = (unsigned int) wide;
            }
        }
        break;
    case TA_INT:
        {
            register int *intP;
            intP = TAHDRELEMPTR(thdrP, int, first);
            for (i = 0; i < nelems; ++i, ++intP) {
                if (Tcl_GetIntFromObj(interp, elems[i], intP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_WIDE:
        {
            register Tcl_WideInt *wideP;
            wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, first);
            for (i = 0; i < nelems; ++i, ++wideP) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], wideP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_DOUBLE:
        {
            register double *dblP;
            dblP = TAHDRELEMPTR(thdrP, double, first);
            for (i = 0; i < nelems; ++i, ++dblP) {
                if (Tcl_GetDoubleFromObj(interp, elems[i], dblP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_OBJ:
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

    case TA_BYTE:
        {
            register unsigned char *byteP;
            byteP = TAHDRELEMPTR(thdrP, unsigned char, first);
            for (i = 0; i < nelems; ++i, ++byteP) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival > 255 || ival < 0) {
                    TArrayValueTypeError(interp, elems[i], thdrP->type);
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
    TA_ASSERT(thdrP->type != TA_OBJ); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}

/* interp may be NULL (only used for errors) */
/* See asserts in code for prerequisite conditions */
TCL_RESULT TAHdrSetMultipleFromObjs(Tcl_Interp *interp,
                                TAHdr * const thdrs[], int nthdrs,
                                Tcl_Obj *tuples, int first)
{
    int t, r, ival;
    Tcl_WideInt wide;
    double dval;
    Tcl_Obj **rows;
    int nrows;
    int have_obj_cols;
    int have_other_cols;
    int need_data_validation;
    Tcl_Obj *valObj;
    TAHdr *thdrP;

    TA_ASSERT(nthdrs > 0);

    if (Tcl_ListObjGetElements(interp, tuples, &nrows, &rows) != TCL_OK)
        return TCL_ERROR;

    if (nrows == 0)
        return TCL_OK;          /* Nought to do */

    for (t = 0, have_obj_cols = 0, have_other_cols = 0; t < nthdrs; ++t) {
        TA_ASSERT(thdrs[t]->nrefs < 2); /* Unshared */
        TA_ASSERT(thdrs[t]->allocated >= (first + nrows)); /* 'Nuff space */
        TA_ASSERT(thdrs[t]->used == thdrs[0]->used); /* All same size */

        if (thdrs[t]->type == TA_OBJ)
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
     * TA_OBJ add a complication. They do not need a type check
     * but because their reference counts have to be managed, it is more
     * complicated to back track on errors when we skip the validation
     * checks in the pure append case. So we update these columns
     * only after everything else has been updated.
     */

    if (! have_other_cols) {
        /* Only TA_OBJ columns, data validation is a no-op */
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
     * any column is of type TA_OBJ, when an error occurs we have to
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
                thdrP = thdrs[t];
                switch (thdrP->type) {
                case TA_BOOLEAN:
                    if (Tcl_GetBooleanFromObj(interp, fields[t], &ival) != TCL_OK)
                        goto error_return;
                    break;
                case TA_UINT:
                    if (Tcl_GetWideIntFromObj(interp, fields[t], &wide) != TCL_OK)
                        goto error_return;
                    if (wide < 0 || wide > 0xFFFFFFFF) {
                        TArrayValueTypeError(interp, fields[t], thdrP->type);
                        goto error_return;
                    }
                    break;
                case TA_INT:
                    if (Tcl_GetIntFromObj(interp, fields[t], &ival) != TCL_OK)
                        goto error_return;
                    break;
                case TA_WIDE:
                    if (Tcl_GetWideIntFromObj(interp, fields[t], &wide) != TCL_OK)
                        goto error_return;
                    break;
                case TA_DOUBLE:
                    if (Tcl_GetDoubleFromObj(interp, fields[t], &dval) != TCL_OK)
                        goto error_return;
                    break;
                case TA_BYTE:
                    if (Tcl_GetIntFromObj(interp, fields[t], &ival) != TCL_OK)
                        goto error_return;
                    if (ival > 255 || ival < 0) {
                        TArrayValueTypeError(interp, fields[t], thdrP->type);
                        goto error_return;
                    }
                    break;
                case TA_OBJ:
                    break;      /* No validation */
                default:
                    TArrayTypePanic(thdrs[t]->type);
                }
            }
        }
    } else {
        /* We are not validating data but then validate row widths */
        /* We are doing this to simplify error rollback for TA_OBJ */
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
     * to the end, and we have to store TA_OBJ last to facilitate
     * rollback on errors as discussed earlier.
     */
    if (have_other_cols) {
        for (t=0; t < nthdrs; ++t) {
            /* Skip TA_OBJ on this round, until all other data is stored */
            thdrP = thdrs[t];
            if (thdrP->type == TA_OBJ)
                continue;
            switch (thdrs[t]->type) {
            case TA_BOOLEAN:
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
                        TA_ASSERT(valObj);
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

            case TA_UINT:
                {
                    register unsigned int *uintP;
                    uintP = TAHDRELEMPTR(thdrP, unsigned int, first);
                    for (r = 0; r < nrows; ++r, ++uintP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetWideIntFromObj(interp, valObj, &wide) != TCL_OK)
                            goto error_return;
                        if (wide < 0 || wide > 0xFFFFFFFF) {
                            TArrayValueTypeError(interp, valObj, thdrP->type);
                            goto error_return;
                        }
                        *uintP = (unsigned int) wide;
                    }
                }
                break;
            case TA_INT:
                {
                    register int *intP;
                    intP = TAHDRELEMPTR(thdrP, int, first);
                    for (r = 0; r < nrows; ++r, ++intP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetIntFromObj(interp, valObj, intP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TA_WIDE:
                {
                    register Tcl_WideInt *wideP;
                    wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, first);
                    for (r = 0; r < nrows; ++r, ++wideP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetWideIntFromObj(interp, valObj, wideP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TA_DOUBLE:
                {
                    register double *dblP;
                    dblP = TAHDRELEMPTR(thdrP, double, first);
                    for (r = 0; r < nrows; ++r, ++dblP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetDoubleFromObj(interp, valObj, dblP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TA_BYTE:
                {
                    register unsigned char *byteP;
                    byteP = TAHDRELEMPTR(thdrP, unsigned char, first);
                    for (r = 0; r < nrows; ++r, ++byteP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetIntFromObj(interp, valObj, &ival) != TCL_OK)
                            goto error_return;
                        if (ival > 255 || ival < 0) {
                            TArrayValueTypeError(interp, valObj, thdrP->type);
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

    /* Now that no errors are possible, update the TA_OBJ columns */
    for (t=0; t < nthdrs; ++t) {
        register Tcl_Obj **objPP;
        thdrP = thdrs[t];
        if (thdrP->type != TA_OBJ)
            continue;
        objPP = TAHDRELEMPTR(thdrP, Tcl_Obj *, first);
        for (r = 0; r < nrows ; ++r, ++objPP) {
            Tcl_ListObjIndex(interp, rows[r], t, &valObj);
            TA_ASSERT(valObj);
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
    case TA_BOOLEAN:
        space = BA_BYTES_NEEDED(0, count);
        break;
    case TA_UINT:
    case TA_INT:
        space = count * sizeof(int);
        break;
    case TA_WIDE:
        space = count * sizeof(Tcl_WideInt);
        break;
    case TA_DOUBLE:
        space = count * sizeof(double);
        break;
    case TA_OBJ:
        space = count * sizeof(Tcl_Obj *);
        break;
    case TA_BYTE:
        space = count * sizeof(unsigned char);
        break;
    default:
        TArrayTypePanic(tatype);
    }

    return sizeof(TAHdr) + space;
}

TAHdr *TArrayRealloc(Tcl_Interp *interp, TAHdr *oldP, int new_count)
{
    TAHdr *thdrP;

    TA_ASSERT(oldP->nrefs < 2);
    TA_ASSERT(oldP->used <= new_count);

    thdrP = (TAHdr *) TA_ATTEMPTREALLOCMEM((char *) oldP, TArrayCalcSize(oldP->type, new_count));
    if (thdrP)
        thdrP->allocated = new_count;
    else
        TArrayNoMemError(interp, new_count);
    return thdrP;
}

TAHdr * TArrayAlloc(Tcl_Interp *interp, unsigned char tatype, int count)
{
    unsigned char nbits;
    TAHdr *thdrP;

    if (count == 0)
            count = TA_DEFAULT_NSLOTS;
    thdrP = (TAHdr *) TA_ATTEMPTALLOCMEM(TArrayCalcSize(tatype, count));
    if (thdrP == NULL) {
        if (interp)
            TArrayNoMemError(interp, count);
        return NULL;
    }
    thdrP->nrefs = 0;
    thdrP->allocated = count;
    thdrP->used = 0;
    thdrP->type = tatype;
    switch (tatype) {
    case TA_BOOLEAN: nbits = 1; break;
    case TA_UINT: nbits = sizeof(unsigned int) * CHAR_BIT; break;
    case TA_INT: nbits = sizeof(int) * CHAR_BIT; break;
    case TA_WIDE: nbits = sizeof(Tcl_WideInt) * CHAR_BIT; break;
    case TA_DOUBLE: nbits = sizeof(double) * CHAR_BIT; break;
    case TA_OBJ: nbits = sizeof(Tcl_Obj *) * CHAR_BIT; break;
    case TA_BYTE: nbits = sizeof(unsigned char) * CHAR_BIT; break;
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
            init_size = nelems + TA_EXTRA(nelems);
        }
    } else {
        nelems = 0;
    }

    thdrP = TArrayAlloc(interp, tatype, init_size);
    if (thdrP) {
        if (elems != NULL && nelems != 0) {
            if (TAHdrSetFromObjs(interp, thdrP, 0, nelems, elems) != TCL_OK) {
                TAHDR_DECRREF(thdrP);
                thdrP = NULL;
            }
        }
    }

    return thdrP;               /* May be NULL on error */
}

/* Deletes a range from a TAHdr. See asserts below for requirements */
void TAHdrDelete(TAHdr *thdrP, int first, int count)
{
    int nbytes;
    void *s, *d;

    TA_ASSERT(! TAHDR_SHARED(thdrP));
    TA_ASSERT(first >= 0);

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
    case TA_BOOLEAN:
        ba_copy(TAHDRELEMPTR(thdrP, ba_t, 0), first, 
                TAHDRELEMPTR(thdrP, ba_t, 0), first+count,
                thdrP->used-(first+count));
        thdrP->used -= count;
        return;

    case TA_OBJ:
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

    case TA_UINT:
    case TA_INT:
        nbytes = count * sizeof(int);
        s = TAHDRELEMPTR(thdrP, int, first+count);
        d = TAHDRELEMPTR(thdrP, int, first);
        break;
    case TA_WIDE:
        nbytes = count * sizeof(Tcl_WideInt);
        s = TAHDRELEMPTR(thdrP, Tcl_WideInt, first+count);
        d = TAHDRELEMPTR(thdrP, Tcl_WideInt, first);
        break;
    case TA_DOUBLE:
        nbytes = count * sizeof(double);
        s = TAHDRELEMPTR(thdrP, double, first+count);
        d = TAHDRELEMPTR(thdrP, double, first);
        break;
    case TA_BYTE:
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

    TA_ASSERT(dstP != srcP);
    TA_ASSERT(dstP->type == srcP->type);
    TA_ASSERT(! TAHDR_SHARED(dstP));
    TA_ASSERT(src_first >= 0);
    if (src_first >= srcP->used)
        return;          /* Nothing to be copied */
    if ((src_first + count) > srcP->used)
        count = srcP->used - src_first;
    if (count <= 0)
        return;
    TA_ASSERT((dst_first + count) <= dstP->allocated);

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
    case TA_BOOLEAN:
        ba_copy(TAHDRELEMPTR(dstP, ba_t, 0), dst_first,
                TAHDRELEMPTR(srcP, ba_t, 0), src_first, count);
        if ((dst_first + count) > dstP->used)
            dstP->used = dst_first + count;
        return;

    case TA_OBJ:
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

    case TA_UINT:
    case TA_INT:
        nbytes = count * sizeof(int);
        s = TAHDRELEMPTR(srcP, int, src_first);
        d = TAHDRELEMPTR(dstP, int, dst_first);
        break;
    case TA_WIDE:
        nbytes = count * sizeof(Tcl_WideInt);
        s = TAHDRELEMPTR(srcP, Tcl_WideInt, src_first);
        d = TAHDRELEMPTR(dstP, Tcl_WideInt, dst_first);
        break;
    case TA_DOUBLE:
        nbytes = count * sizeof(double);
        s = TAHDRELEMPTR(srcP, double, src_first);
        d = TAHDRELEMPTR(dstP, double, dst_first);
        break;
    case TA_BYTE:
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
TAHdr *TAHdrClone(Tcl_Interp *interp, TAHdr *srcP, int minsize)
{
    TAHdr *thdrP;

    if (minsize == 0)
        minsize = srcP->allocated;
    else if (minsize < srcP->used)
        minsize = srcP->used;

    /* TBD - optimize these two calls */
    thdrP = TArrayAlloc(interp, srcP->type, minsize);
    if (thdrP)
        TAHdrCopy(thdrP, 0, srcP, 0, srcP->used);
    return thdrP;
}


/*
 * Convert a TArray Tcl_Obj to one that is suitable for modifying.
 * The Tcl_Obj must NOT be shared.
 * There are three cases to consider:
 * (1) Even though taObj is unshared, the corresponding TAHdr might
 *     still be shared (pointed to from elsewhere). In this case
 *     also, we clone the TAHdr and store it as the new internal rep.
 * (2) If its TAHdr is unshared, we can modify in
 *     place, unless 
 * (3) TAHdr is too small in which case we have to reallocate it.
 *
 * Invalidates the string rep in all cases.
 * Only fails on memory allocation failure.
 */
TCL_RESULT TArrayMakeModifiable(Tcl_Interp *interp,
                                Tcl_Obj *taObj, int minsize, int prefsize)
{
    TAHdr *thdrP;

    TA_ASSERT(taObj->typePtr == &gTArrayType);
    TA_ASSERT(! Tcl_IsShared(taObj));

    thdrP = TARRAYHDR(taObj);
    if (minsize < thdrP->used)
        minsize = thdrP->used;
    if (minsize > prefsize)
        prefsize = minsize;

    if (TAHDR_SHARED(thdrP)) {
        /* Case (1) */
        thdrP = TAHdrClone(interp, thdrP, prefsize);
        if (thdrP == NULL)
            return TCL_ERROR;   /* Note taObj is not changed */
        TAHDR_DECRREF(TARRAYHDR(taObj)); /* Release old */
        TARRAYHDR(taObj) = NULL;
        TA_OBJ_SETREP(taObj, thdrP);
        Tcl_InvalidateStringRep(taObj);
    } else if (thdrP->allocated < minsize) {
        /* Case (3). Note don't use TA_OBJ_SETREP as we are keeping all 
           fields and ref counts the same */
        Tcl_InvalidateStringRep(taObj);
        thdrP = TArrayRealloc(interp, thdrP, prefsize);
        if (thdrP)
            TARRAYHDR(taObj) = thdrP;
        else
            return TCL_ERROR;   /* Note taObj is not changed */
    } else {
        /* Case (2) - just reuse, invalidate the string rep */
        Tcl_InvalidateStringRep(taObj);
    }

    return TCL_OK;
}


/* Returns a Tcl_Obj for a TArray slot. NOTE: WITHOUT its ref count incremented */
Tcl_Obj * TArrayIndex(Tcl_Interp *interp, TAHdr *thdrP, Tcl_Obj *indexObj)
{
    int index;

    if (IndexToInt(interp, indexObj, &index, thdrP->used-1, 0, thdrP->used-1) != TCL_OK)
        return NULL;
    if (index < 0 || index >= thdrP->used) {
        TArrayIndexRangeError(interp, indexObj);
        return NULL;
    }

    switch (thdrP->type) {
    case TA_BOOLEAN:
        return Tcl_NewIntObj(ba_get(TAHDRELEMPTR(thdrP, ba_t, 0), index));
    case TA_UINT:
        return Tcl_NewWideIntObj(*TAHDRELEMPTR(thdrP, unsigned int, index));
    case TA_INT:
        return Tcl_NewIntObj(*TAHDRELEMPTR(thdrP, int, index));
    case TA_WIDE:
        return Tcl_NewWideIntObj(*TAHDRELEMPTR(thdrP, Tcl_WideInt, index));
    case TA_DOUBLE:
        return Tcl_NewDoubleObj(*TAHDRELEMPTR(thdrP, double, index));
    case TA_BYTE:
        return Tcl_NewIntObj(*TAHDRELEMPTR(thdrP, unsigned char, index));
    case TA_OBJ:
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
    if (objP->typePtr == &gTArrayType && TARRAYTYPE(objP) == TA_INT) {
        thdrP = TARRAYHDR(objP);
        thdrP->nrefs++;
        return thdrP;
    }

    if (Tcl_ListObjGetElements(interp, objP, &nelems, &elems) != TCL_OK)
        return NULL;

    thdrP = TArrayAllocAndInit(interp, TA_INT, nelems, elems, 0);
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

    if (indicesP->type != TA_INT) {
        if (interp)
            Tcl_SetResult(interp, "Invalid type for tarray indices", TCL_STATIC);
        return NULL;
    }

    count = indicesP->used;
    thdrP = TArrayAlloc(interp, srcP->type, count);
    if (thdrP == 0 || count == 0)
        return thdrP;

    indexP = TAHDRELEMPTR(indicesP, int, 0);

    switch (srcP->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = TAHDRELEMPTR(thdrP, ba_t, 0);
            for (i = 0; i < count; ++i, ++indexP)
                ba_put(baP, i, *indexP);
        }
        break;
    case TA_UINT:
    case TA_INT:
        {
            unsigned int *uiP = TAHDRELEMPTR(thdrP, unsigned int, 0);
            for (i = 0; i < count; ++i, ++indexP, ++uiP)
                *uiP = *TAHDRELEMPTR(srcP, unsigned int, *indexP);
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *wideP = TAHDRELEMPTR(thdrP, Tcl_WideInt, 0);
            for (i = 0; i < count; ++i, ++indexP, ++wideP)
                *wideP = *TAHDRELEMPTR(srcP, Tcl_WideInt, *indexP);
        }
        break;
    case TA_DOUBLE:
        {
            double *dblP = TAHDRELEMPTR(thdrP, double, 0);
            for (i = 0; i < count; ++i, ++indexP, ++dblP)
                *dblP = *TAHDRELEMPTR(srcP, double, *indexP);
        }
        break;
    case TA_BYTE:
        {
            unsigned char *ucP = TAHDRELEMPTR(thdrP, unsigned char, 0);
            for (i = 0; i < count; ++i, ++indexP, ++ucP)
                *ucP = *TAHDRELEMPTR(srcP, unsigned char, *indexP);
        }
        break;
    case TA_OBJ:
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

    TA_ASSERT(haystackP->type == TA_BOOLEAN);

    if (op != TA_SEARCH_OPT_EQ)
        return TArrayBadSearchOpError(interp, op);

    if (Tcl_GetBooleanFromObj(interp, needleObj, &bval) != TCL_OK)
        return TCL_ERROR;
    
    if (flags & TA_SEARCH_INVERT)
        bval = !bval;

    /* First locate the starting point for the search */
    baP = TAHDRELEMPTR(haystackP, ba_t, 0);

    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP;
        TAHdr *newP;
        thdrP = TArrayAlloc(interp, 
                            flags & TA_SEARCH_INLINE ? TA_BOOLEAN : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdrP == NULL)
            return TCL_ERROR;
        pos = start;
        while ((pos = ba_find(baP, bval, pos, thdrP->used)) != -1) {
            /* Ensure enough space in target array */
            if (thdrP->used >= thdrP->allocated)
                newP = TArrayRealloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
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

        resultObj = TArrayNewObj(thdrP);
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
    case TA_SEARCH_OPT_GT:
    case TA_SEARCH_OPT_LT: 
    case TA_SEARCH_OPT_EQ:
        break;
    default:
        return TArrayBadSearchOpError(interp, op);
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
        TArrayTypePanic(haystackP->type);
    }

    if (needle > max_val || needle < min_val) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Integer \"%s\" type mismatch for typearray (type %d).", Tcl_GetString(needleObj), haystackP->type));
        return TCL_ERROR;
    }

    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;

    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP, *newP;

        thdrP = TArrayAlloc(interp,
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
                    newP = TArrayRealloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
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

        resultObj = TArrayNewObj(thdrP);

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


static TCL_RESULT TArraySearchDouble(Tcl_Interp *interp, TAHdr * haystackP,
                                          Tcl_Obj *needleObj, int start, enum TArraySearchSwitches op, int flags)
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
        return TArrayBadSearchOpError(interp, op);
    }

    if (Tcl_GetDoubleFromObj(interp, needleObj, &dval) != TCL_OK)
        return TCL_ERROR;
    
    compare_wanted = flags & TA_SEARCH_INVERT ? 0 : 1;

    /* First locate the starting point for the search */
    dvalP = TAHDRELEMPTR(haystackP, double, start);

    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP, *newP;

        thdrP = TArrayAlloc(interp,
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
                    newP = TArrayRealloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
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

        resultObj = TArrayNewObj(thdrP);

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
        return TArrayBadSearchOpError(interp, op);
    }

    /* First locate the starting point for the search */
    objPP = TAHDRELEMPTR(haystackP, Tcl_Obj *, start);


    if (flags & TA_SEARCH_ALL) {
        TAHdr *thdrP, *newP;

        thdrP = TArrayAlloc(interp,
                            flags & TA_SEARCH_INLINE ? TA_OBJ : TA_INT,
                            10);                /* Assume 10 hits */
        if (thdrP == NULL)
            return TCL_ERROR;

        for (offset = start; offset < haystackP->used; ++offset, ++objPP) {
            switch (op) {
            case TA_SEARCH_OPT_GT:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) == 0; break;
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
                    newP = TArrayRealloc(interp, thdrP, thdrP->used + TA_EXTRA(thdrP->used));
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

        resultObj = TArrayNewObj(thdrP);

    } else {
        /* Return first found element */
        for (offset = start; offset < haystackP->used; ++offset, ++objPP) {
            switch (op) {
            case TA_SEARCH_OPT_GT:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) > 0; break;
            case TA_SEARCH_OPT_LT: 
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) < 0; break;
            case TA_SEARCH_OPT_EQ:
                compare_result = TArrayCompareObjs(*objPP, needleObj, nocase) == 0; break;
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

    if (TArrayConvert(interp, objv[objc-2]) != TCL_OK)
        return TCL_ERROR;
    haystackP = TARRAYHDR(objv[objc-2]);
    flags = 0;
    start_index = 0;
    op = TA_SEARCH_OPT_EQ;
    for (i = 1; i < objc-2; ++i) {
	if (Tcl_GetIndexFromObj(interp, objv[i], TArraySearchSwitches, "option", 0, &opt)
            != TCL_OK) {
            return TCL_ERROR;
	}
        switch ((enum TArraySearchSwitches) opt) {
        case TA_SEARCH_OPT_ALL: flags |= TA_SEARCH_ALL; break;
        case TA_SEARCH_OPT_INLINE: flags |= TA_SEARCH_INLINE; break;
        case TA_SEARCH_OPT_INVERT: flags |= TA_SEARCH_INVERT; break;
        case TA_SEARCH_OPT_NOCASE: flags |= TA_SEARCH_NOCASE; break;
        case TA_SEARCH_OPT_START:
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
        case TA_SEARCH_OPT_EQ:
        case TA_SEARCH_OPT_GT:
        case TA_SEARCH_OPT_LT:
        case TA_SEARCH_OPT_PAT:
        case TA_SEARCH_OPT_RE:
            op = (enum TArraySearchSwitches) opt;
        }
    }

    switch (haystackP->type) {
    case TA_BOOLEAN:
        return TArraySearchBoolean(interp, haystackP, objv[objc-1], start_index,op,flags);
    case TA_INT:
    case TA_UINT:
    case TA_BYTE:
    case TA_WIDE:
        return TArraySearchEntier(interp, haystackP, objv[objc-1], start_index, op, flags);
    case TA_DOUBLE:
        return TArraySearchDouble(interp, haystackP, objv[objc-1], start_index, op, flags);
    case TA_OBJ:
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



/* The tarray Tcl_Obj is modified */
TCL_RESULT TArrayFillFromObj(
    Tcl_Interp *interp,
    Tcl_Obj *lowObj, Tcl_Obj *highObj,
    Tcl_Obj *taObj,
    Tcl_Obj *valueObj)
{
    TAHdr *thdrP;
    int low, count;
    TArrayValue value;
    int status;

    TA_ASSERT(! Tcl_IsShared(taObj));

    if ((status = TArrayConvert(interp, taObj)) != TCL_OK)
        return status;
    thdrP = TARRAYHDR(taObj);
    status = TArrayValueFromObj(interp, valueObj, thdrP->type, &value);
    if (status != TCL_OK)
        return TCL_ERROR;

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

    /* Get the limits of the range to set */
    status = RationalizeRangeIndices(interp, thdrP, lowObj, highObj, &low, &count);
    if (status != TCL_OK)
        goto vamoose;

    /*
     * NOTE: NO ERRORS ARE EXPECTED BEYOND THIS POINT EXCEPT
     * LIKE BUGCHECKS OR OUT OF MEMORY. Code below is written accordingly.
     */

    /* If nothing to fill, return existing array as is */
    if (count) {
        /* Note this also invalidates the string rep as desired */
        if (TArrayMakeModifiable(interp, taObj,
                                 thdrP->used, thdrP->allocated) != TCL_OK) {
            status = TCL_ERROR;
        } else {
            thdrP = TARRAYHDR(taObj); /* Might have changed */
            TAHdrFill(interp, thdrP, &value, low, count);
        }
    }
    
vamoose:                   /* interp must already hold error message */
    Tcl_DecrRefCount(lowObj);
    Tcl_DecrRefCount(highObj);

    return status;
}

/* The tarray Tcl_Obj is modified */
TCL_RESULT TArraySetFromObjs(
    Tcl_Interp *interp,
    Tcl_Obj *firstObj, 
    Tcl_Obj *taObj,
    Tcl_Obj *valueListObj)
{
    TAHdr *thdrP;
    int i, first;
    TArrayValue value;
    int status;
    int new_size;
    Tcl_Obj **valueObjs;
    int nvalues;

    TA_ASSERT(! Tcl_IsShared(taObj));

    status = Tcl_ListObjGetElements(interp, valueListObj, &nvalues, &valueObjs);
    if (status != TCL_OK)
        return status;

    if ((status = TArrayConvert(interp, taObj)) != TCL_OK)
        return status;
    thdrP = TARRAYHDR(taObj);

    /*
     * Extract index. Be careful not to shimmer
     * in the unlikely error case where incorrect args passed,
     * we do not want to lose the tarray representation. For example,
     * So if the index Tcl_Obj's are of tarrays, we dup them.
     */

    if (firstObj->typePtr == &gTArrayType)
        firstObj = Tcl_DuplicateObj(firstObj);
    else
        Tcl_IncrRefCount(firstObj); /* Since we will release at end */

    /* Get the limits of the range to set */
    if ((status = IndexToInt(interp, firstObj, &first, thdrP->used, 0, thdrP->used)) != TCL_OK)
        goto vamoose;

    if (nvalues) {
        /* Note this also invalidates the string rep as desired */
        status = TArrayMakeModifiable(interp, taObj,
                                      thdrP->used, thdrP->allocated);
        if (status == TCL_OK) {
            /* Note even on error TAHdrSetFromObjs guarantees a consistent 
             * (though un-updated) taObj
             */
            thdrP = TARRAYHDR(taObj); /* Might have changed */
            status = TAHdrSetFromObjs(interp, thdrP, first, nvalues, valueObjs);
        }
    }
    
vamoose:                   /* interp must already hold error message */
    Tcl_DecrRefCount(firstObj);

    return status;
}


/* The grid Tcl_Obj gridObj is modified */
TCL_RESULT TGridFillFromObjs(
    Tcl_Interp *interp,
    Tcl_Obj *lowObj, Tcl_Obj *highObj,
    Tcl_Obj *gridObj,
    Tcl_Obj *rowObj)
{
    int i, low, count, row_width;
    TAHdr *gridHdrP;
    TArrayValue values[32];
    TArrayValue *valuesP;
    Tcl_Obj **taObjPP;
    int status;
    int new_size;
    int collength;

    TA_ASSERT(! Tcl_IsShared(gridObj));

    if ((status = TArrayConvert(interp, gridObj)) != TCL_OK)
        return status;

    gridHdrP = TARRAYHDR(gridObj);

    if ((status = Tcl_ListObjLength(interp, rowObj, &row_width)) != TCL_OK)
        return status;

    if (row_width != gridHdrP->used)
        return TArrayRowWidthError(interp, row_width, gridHdrP->used);

    /* Check for empty tuple so as to simplify loops below */
    if (row_width == 0)
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

    if (row_width > sizeof(values)/sizeof(values[0])) {
        valuesP = (TArrayValue *) TA_ALLOCMEM(row_width * sizeof(TArrayValue));
    } else {
        valuesP = values;
    }
        
    /* Make sure grid object is modifiable */
    if ((status = TArrayMakeModifiable(interp, gridObj,
                                       gridHdrP->used,
                                       gridHdrP->allocated)) != TCL_OK)
        goto vamoose;
    gridHdrP = TARRAYHDR(gridObj); /* Might have changed */

    /*
     * Now verify tarrays and values. The latter should be of the
     * appropriate type. Also ensure all tarrays are the same size
     * and can be modified.
     *
     * NOTE THAT AT ALL TIMES, gridObj has valid unmodified contents
     * (logically unmodified though allocated blocks might have changed)
     * so even on error we exit with a clean gridObj.
     */
    for (i = 0, taObjPP = TAHDRELEMPTR(gridHdrP, Tcl_Obj *, 0);
         i < row_width;
         ++i, ++taObjPP) {
        TAHdr *thdrP;
        Tcl_Obj *valueObj;
        Tcl_Obj *colObj = *taObjPP;

        if ((status = TArrayConvert(interp, *taObjPP)) != TCL_OK)
            goto vamoose;

        thdrP = TARRAYHDR(colObj);
        if (i == 0) {
            collength = thdrP->used;
            /* Get the limits of the range to set */
            status = RationalizeRangeIndices(interp, thdrP, lowObj, highObj, &low, &count);
            if (status != TCL_OK || count == 0)
                goto vamoose;   /* Error or nothing to do */
        }
        else if (thdrP->used != collength) {
            status = TArrayGridLengthError(interp);
            goto vamoose;
        }

        /* Preferred size in case we have to reallocate */
        new_size = low + count + TA_EXTRA(low+count); /* Disregarded if < allocated */

        /* We have already converted above */
        TA_ASSERT(colObj->typePtr == &gTArrayType);
        if (Tcl_IsShared(colObj)) {
            colObj = Tcl_DuplicateObj(colObj);
            Tcl_IncrRefCount(colObj);
            Tcl_DecrRefCount(*taObjPP);
            *taObjPP = colObj;
            thdrP = TARRAYHDR(colObj);
        }

        /* Note this also invalidates the string rep as desired */
        if ((status = TArrayMakeModifiable(interp, colObj, low+count, new_size)) != TCL_OK)
            goto vamoose;

        if ((status = Tcl_ListObjIndex(interp, rowObj, i, &valueObj)) != TCL_OK)
            goto vamoose;
        status = TArrayValueFromObj(interp, valueObj, thdrP->type, &valuesP[i]);
        if (status != TCL_OK)
            goto vamoose;
    }


    /*
     * We can now do the actual modifications. All validation and memory
     * allocations are done.
     * NOTE: NO ERRORS ARE EXPECTED BEYOND THIS POINT EXCEPT FATAL ONES
     */

    for (i = 0, taObjPP = TAHDRELEMPTR(gridHdrP, Tcl_Obj *, 0);
         i < row_width;
         ++i, ++taObjPP) {
        TAHdrFill(interp, TARRAYHDR(*taObjPP), &valuesP[i], low, count);
    }
    status = TCL_OK;
    
vamoose:                   /* interp must already hold error message */
    Tcl_DecrRefCount(lowObj);
    Tcl_DecrRefCount(highObj);
    if (valuesP != values)
        TA_FREEMEM((char *) valuesP);

    return status;
}
