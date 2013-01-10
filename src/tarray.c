/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include <string.h>
#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#include "tarray.h"

/*
 * TArray is a Tcl "type" used for densely storing arrays of elements
 * of a specific type.
 */
static void ta_type_dup(Tcl_Obj *srcP, Tcl_Obj *dstP);
static void ta_type_free_intrep(Tcl_Obj *objP);
static void ta_type_update_string(Tcl_Obj *objP);
struct Tcl_ObjType g_ta_type = {
    "tarray",
    ta_type_free_intrep,
    ta_type_dup,
    ta_type_update_string,
    NULL,     /* jenglish advises to keep this NULL */
};

/* Must match definitions in tarray.h ! */
const char *g_ta_type_tokens[] = {
    "boolean",
    "uint",
    "int",
    "wide",
    "double",
    "byte",
    "tclobj",
    NULL
};    

Tcl_ObjType *g_tcl_list_type_ptr;


/* TBD - in error and panic routines make sure strings are not too long */

const char *ta_type_string(int tatype)
{
    if (tatype < (sizeof(g_ta_type_tokens)/sizeof(g_ta_type_tokens[0]))) {
        return g_ta_type_tokens[tatype];
    } else
        return "<invalid>";
}

void ta_string_overflow_panic(const char *where)
{
    Tcl_Panic("Max size for a Tcl value (%d bytes) exceeded in %s", INT_MAX, where ? where : "unknown function");
}

void ta_type_panic(unsigned char tatype)
{
    Tcl_Panic("Unknown or unexpected tarray type %d", tatype);
}

void ta_shared_panic(const char *where)
{
    Tcl_Panic("Shared thdr_t passed for modification to %s.", where);
}

void ta_small_panic(thdr_t *thdrP, const char *where)
{
    Tcl_Panic("Insufficient space in thdr_t (allocated %d) in %s.", thdrP->allocated, where);
}

TCL_RESULT ta_missing_arg_error(Tcl_Interp *interp, char *optname)
{
    if (interp) {
        Tcl_SetObjResult(interp, Tcl_ObjPrintf("Missing argument to option '%s'", optname));
        Tcl_SetErrorCode(interp, "TARRAY", "ARGUMENT", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_not_tarray_error(Tcl_Interp *interp)
{
    if (interp) {
        Tcl_SetResult(interp, "Object is not a TArray", TCL_STATIC);
        Tcl_SetErrorCode(interp, "TARRAY", "TCLOBJTYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_bad_type_error(Tcl_Interp *interp, thdr_t *thdrP)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("tarray is of the wrong type (%s)",
                                       ta_type_string(thdrP->type)));
        Tcl_SetErrorCode(interp, "TARRAY", "TYPE", NULL);
    }
    return TCL_ERROR;
}


TCL_RESULT ta_index_range_error(Tcl_Interp *interp, int index)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("tarray index %d out of bounds", index));
        Tcl_SetErrorCode(interp, "TARRAY", "INDEX", "RANGE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_value_type_error(Tcl_Interp *interp, Tcl_Obj *objP, int tatype)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Value %s not valid for typed array of type %s.",
                                       Tcl_GetString(objP),
                                       ta_type_string(tatype)));
        Tcl_SetErrorCode(interp, "TARRAY", "VALUE", "TYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_row_width_error(Tcl_Interp *interp, int row_width, int grid_width)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Row width %d does not match grid width %d.", row_width, grid_width));
        Tcl_SetErrorCode(interp, "TARRAY", "ROW", "WIDTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_grid_length_error(Tcl_Interp *interp)
{
    if (interp) {
        Tcl_SetResult(interp,
                      "Columns in tarray grid have differing lengths.",
                      TCL_STATIC);
        Tcl_SetErrorCode(interp, "TARRAY", "GRID", "LENGTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_memory_error(Tcl_Interp *interp, int req_size)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Memory allocation failed (%d bytes).",
                                       req_size));
        Tcl_SetErrorCode(interp, "TARRAY", "NOMEM", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_indices_error(Tcl_Interp *interp, Tcl_Obj *objP)
{
    if (interp) {
        Tcl_SetObjResult(interp, Tcl_ObjPrintf("Invalid index list '%s'. Must be an integer, or a list or typed array of type int.", Tcl_GetString(objP)));
        Tcl_SetErrorCode(interp, "TARRAY", "VALUE", "INDEXLIST", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_index_error(Tcl_Interp *interp, Tcl_Obj *objP)
{
    if (interp) {
        Tcl_SetObjResult(interp, Tcl_ObjPrintf("Invalid index '%s'. Must be an integer or the keyword 'end'.", Tcl_GetString(objP)));
        Tcl_SetErrorCode(interp, "TARRAY", "VALUE", "INDEX", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_mismatched_types_error(Tcl_Interp *interp)
{
    if (interp) {
        Tcl_SetResult(interp, "tarray types are not compatible for attempted operation", TCL_STATIC);
        Tcl_SetErrorCode(interp, "TARRAY", "TYPE", "INCOMPATIBLE", NULL);
    }

    return TCL_ERROR;
}

TCL_RESULT ta_indices_count_error(Tcl_Interp *interp, int nindices, int nvalues)
{
    if (interp) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf("Number of indices (%d) not same as number of values (%d).", nindices, nvalues));
        Tcl_SetErrorCode(interp, "TARRAY", "INDICES", "COUNT", NULL);
    }

    return TCL_ERROR;
}

/*
 * Map numeric or string index to numeric integer index.
 */
TCL_RESULT ta_convert_index(Tcl_Interp *interp, Tcl_Obj *objP, int *indexP, int end_value, int low, int high)
{
    char *s;
    int val;

    /* Do type checks to avoid expensive shimmering in case of errors */
    if (objP->typePtr == &g_ta_type)
        return ta_index_error(interp, objP);

    if (objP->typePtr == g_tcl_list_type_ptr) {
        if (Tcl_ListObjLength(NULL, objP, &val) != TCL_OK || val != 1)
            return ta_index_error(interp, objP);
    }

    if (Tcl_GetIntFromObj(NULL, objP, &val) != TCL_OK) {
        s = Tcl_GetString(objP);
        if (strcmp(s, "end")) {
            return ta_index_error(interp, objP);
        }
        val = end_value;
    }
    
    if (val < low || val > high)
        return ta_index_range_error(interp, val);
    else {
        *indexP = val;
        return TCL_OK;
    }
}

/* low has to be between 0 and one beyond last element.
   high is 0-INT_MAX
   if (high < low) count is returned as 0 (not an error)
*/
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *interp, const thdr_t *thdrP, Tcl_Obj *lowObj, Tcl_Obj *highObj, int *lowP, int *countP)
{
    int low, high;

    /* TBD - We allow low index to be 1 greater than last element. Caller should
     * check for this if appropriate. High index can be any greater value
     * than lower range.
     */
    if (ta_convert_index(interp, lowObj, &low, thdrP->used-1, 0, thdrP->used) != TCL_OK)
        return TCL_ERROR;

    if (ta_convert_index(interp, highObj, &high, thdrP->used-1, 0, INT_MAX) != TCL_OK)
        return TCL_ERROR;

    *lowP = low;
    if (high < low)
        *countP = 0;            /* This is how lrange behaves */
    else
        *countP = high - low + 1;

    return TCL_OK;
}

TCL_RESULT tcol_convert(Tcl_Interp *interp, Tcl_Obj *objP)
{
    Tcl_Obj **elems;
    int nelems, tatype;
    
    if (objP->typePtr == &g_ta_type)
        return TCL_OK;

    /* See if we can convert it to one based on string representation */
    if (Tcl_ListObjGetElements(NULL, objP, &nelems, &elems) == TCL_OK
        && nelems == 3
        && !strcmp(Tcl_GetString(elems[0]), "tarray")
        && Tcl_GetIndexFromObj(interp, elems[1], g_ta_type_tokens, "TArrayType",
                               TCL_EXACT, &tatype) == TCL_OK) {
        /* So far so good. Try and convert */
        thdr_t *thdrP;
        Tcl_Obj **valueObjs;
        int nvalues;
        
        if (Tcl_ListObjGetElements(interp, elems[2], &nvalues, &valueObjs)
            != TCL_OK)
            return TCL_ERROR;

        thdrP = thdr_alloc_and_init(interp, tatype, nvalues, valueObjs, 0);
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
                
    return ta_not_tarray_error(interp);
}

TCL_RESULT ta_value_from_obj(Tcl_Interp *interp, Tcl_Obj *objP,
                              unsigned char tatype, ta_value_t *tavP)
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
            return ta_value_type_error(interp, objP, tatype);
        tavP->ucval = (unsigned char) tavP->ival;
        break;
    case TA_UINT:
    case TA_WIDE:
        if (Tcl_GetWideIntFromObj(interp, objP, &tavP->wval) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TA_WIDE)
            break;
        if (tavP->wval < 0 || tavP->wval > 0xFFFFFFFF)
            return ta_value_type_error(interp, objP, tatype);
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
        ta_type_panic(tatype);
    }

    tavP->type = tatype;
    return TCL_OK;
}

/*
 * Set the value of an element range at a position in a thdr_t.
 * See the asserts below for conditions under which this can be called
 */
void thdr_fill_range(Tcl_Interp *interp, thdr_t *thdrP,
                   const ta_value_t *tavP, int pos, int count)
{
    int i;

    TA_ASSERT(! thdr_sHARED(thdrP));
    TA_ASSERT((pos+count) <= thdrP->allocated);
    TA_ASSERT(pos <= thdrP->used);
    TA_ASSERT(thdrP->type == tavP->type);

    thdr_mark_unsorted(thdrP);
    switch (thdrP->type) {
    case TA_BOOLEAN:
        ba_fill(thdr_tELEMPTR(thdrP, ba_t, 0), pos, count, tavP->bval);
        break;
    case TA_INT:
    case TA_UINT:
        if (tavP->ival == 0) {
            memset(thdr_tELEMPTR(thdrP, int, pos), 0, count*sizeof(int));
        } else {
            int *iP;
            iP = thdr_tELEMPTR(thdrP, int, pos);
            for (i = 0; i < count; ++i, ++iP)
                *iP = tavP->ival;
        }
        break;
        
    case TA_BYTE:
        memset(thdr_tELEMPTR(thdrP, unsigned char, pos), tavP->ucval, count);
        break;

    case TA_WIDE:
        if (tavP->wval == 0) {
            memset(thdr_tELEMPTR(thdrP, Tcl_WideInt, pos), 0, count*sizeof(Tcl_WideInt));
        } else {
            Tcl_WideInt *wideP;
            wideP = thdr_tELEMPTR(thdrP, Tcl_WideInt, pos);
            for (i = 0; i < count; ++i, ++wideP)
                *wideP = tavP->wval;
        }
        break;
    case TA_DOUBLE:
        {
            double *dvalP;
            dvalP = thdr_tELEMPTR(thdrP, double, pos);
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
            objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, pos);
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
        ta_type_panic(thdrP->type);
    }

    if ((pos + count) > thdrP->used)
        thdrP->used = pos + count;
}

TCL_RESULT ta_verify_value_objs(Tcl_Interp *interp, int tatype,
                                int nelems, Tcl_Obj * const elems[])
{
    Tcl_Obj * const *objPP = elems;
    Tcl_Obj * const *end = elems + nelems;
    switch (tatype) {
    case TA_BOOLEAN:
        for ( ; objPP < end; ++objPP) {
            int ival;
            if (Tcl_GetBooleanFromObj(interp, *objPP, &ival) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_UINT:
        for ( ; objPP < end; ++objPP) {
            Tcl_WideInt wide;
            if (Tcl_GetWideIntFromObj(interp, *objPP, &wide) != TCL_OK)
                return TCL_ERROR;
            if (wide < 0 || wide > 0xFFFFFFFF) {
                return ta_value_type_error(interp, *objPP, tatype);
            }
        }
        break;

    case TA_INT:
        for ( ; objPP < end; ++objPP) {
            int ival;
            if (Tcl_GetIntFromObj(interp, *objPP, &ival) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_WIDE:
        for ( ; objPP < end; ++objPP) {
            Tcl_WideInt wide;
            if (Tcl_GetWideIntFromObj(interp, *objPP, &wide) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_DOUBLE:
        for ( ; objPP < end; ++objPP) {
            double dval;
            if (Tcl_GetDoubleFromObj(interp, *objPP, &dval) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_BYTE:
        for ( ; objPP < end; ++objPP) {
            int ival;
            if (Tcl_GetIntFromObj(interp, *objPP, &ival) != TCL_OK)
                return TCL_ERROR;
            if (ival > 255 || ival < 0) {
                ta_value_type_error(interp, *objPP, tatype);
                return TCL_ERROR;
            }
        }
        break;
    case TA_OBJ:
        break;                  /* Just pointers, nothing to verify */
    default:
        ta_type_panic(tatype);
    }
    return TCL_OK;
}


/* Verify that the specified index list is valid.
   We need to verify that the indices
   are not beyond the range. At the same time the indices may
   themselves extend the range. Sort indices to simplify this.
   Returns new size needed if array has to be grown or -1 on error.
*/
TCL_RESULT thdr_verify_indices(Tcl_Interp *interp, thdr_t *thdrP, thdr_t *indicesP, int *new_sizeP)
{
    int i, new_size;
    int *indexP, *endP;

    TA_ASSERT(indicesP->type == TA_INT);
    TA_ASSERT(thdr_sorted(indicesP));

    new_size = thdrP->used;
    indexP = thdr_tELEMPTR(indicesP, int, 0);
    endP = thdr_tELEMPTR(indicesP, int, indicesP->used);
    if (thdr_sort_order(indicesP) > 0) {
        /* Sort order is ascending. Make sure no gaps */
        while (indexP < endP) {
            i = *indexP++;
            if (i < new_size)
                continue;
            if (i > new_size)
                return ta_index_range_error(interp, i);
            new_size = i+1;       /* Appending without a gap */
        }
    } else {
        /* Sort order is descending. Go in reverse to make sure no gaps */
        while (indexP < endP) {
            i = *--endP;
            if (i < new_size)
                continue;
            if (i > new_size) {
                ta_index_range_error(interp, i);
                return -1;
            }
            new_size = i+1;       /* Appending without a gap */
        }
    }
    *new_sizeP = new_size;
    return TCL_OK;
}

/* thdrP must be large enough for largest index. And see asserts in code */
void thdr_fill_indices(Tcl_Interp *interp, thdr_t *thdrP, 
                      const ta_value_t *tavP, thdr_t *indicesP)
{
    int highest_index;
    int *indexP, *endP;

    TA_ASSERT(! thdr_sHARED(thdrP));
    TA_ASSERT(thdrP->type == tavP->type);
    TA_ASSERT(indicesP->type == TA_INT);
    TA_ASSERT(thdr_sorted(indicesP));

    if (indicesP->used == 0)
        return;          /* Nothing to do */

    /* Rest of code assumes > 0 indices */

    indexP = thdr_tELEMPTR(indicesP, int, 0);
    endP = thdr_tELEMPTR(indicesP, int, indicesP->used);

    /* Caller guarantees room for highest index value */
    highest_index = thdr_sort_order(indicesP) > 0 ? endP[-1] : indexP[0];
    TA_ASSERT(highest_index < thdrP->allocated);

    thdr_mark_unsorted(thdrP);
    switch (thdrP->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = thdr_tELEMPTR(thdrP, ba_t, 0);
            while (indexP < endP)
                ba_put(baP, *indexP++, tavP->bval);
        }
        break;
    case TA_INT:
    case TA_UINT:
        {
            int *iP = thdr_tELEMPTR(thdrP, int, 0);
            while (indexP < endP)
                iP[*indexP++] = tavP->ival;
        }
        break;
    case TA_BYTE:
        {
            unsigned char *ucP = thdr_tELEMPTR(thdrP, unsigned char, 0);
            while (indexP < endP)
                ucP[*indexP++] = tavP->ucval;
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *wideP;
            wideP = thdr_tELEMPTR(thdrP, Tcl_WideInt, 0);
            while (indexP < endP)
                wideP[*indexP] = tavP->wval;
        }
        break;
    case TA_DOUBLE:
        {
            double *dvalP;
            dvalP = thdr_tELEMPTR(thdrP, double, 0);
            while (indexP < endP)
                dvalP[*indexP] = tavP->dval;
        }
        break;
    case TA_OBJ:
        {
            Tcl_Obj **objPP;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts. Note that
             * indices may be sorted in either order.
             */
            objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, 0);
            while (indexP < endP) {
                Tcl_IncrRefCount(tavP->oval);
                if (*indexP < thdrP->used)
                    Tcl_DecrRefCount(objPP[*indexP]);
                objPP[*indexP] = tavP->oval;
            }
        }
        break;
    default:
        ta_type_panic(thdrP->type);
    }

    if (highest_index >= thdrP->used)
        thdrP->used = highest_index + 1;
}



/* Increments the ref counts of Tcl_Objs in a tarray making sure not
   to run past end of array */
void thdr_incr_obj_refs(thdr_t *thdrP, int first, int count)
{
    register int i;
    register Tcl_Obj **objPP;

    if (thdrP->type == TA_OBJ) {
        if ((first + count) > thdrP->used)
            count = thdrP->used - first;
        if (count <= 0)
            return;
        objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, first);
        for (i = 0; i < count; ++i, ++objPP) {
            Tcl_IncrRefCount(*objPP);
        }
    }
}

/* Decrements the ref counts of Tcl_Objs in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void thdr_decr_obj_refs(thdr_t *thdrP, int first, int count)
{
    register int i;
    register Tcl_Obj **objPP;

    if (thdrP->type == TA_OBJ) {
        if ((first + count) > thdrP->used)
            count = thdrP->used - first;
        if (count <= 0)
            return;
        objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, first);
        for (i = 0; i < count; ++i, ++objPP) {
            Tcl_DecrRefCount(*objPP);
        }
    }
}

void thdr_free(thdr_t *thdrP)
{
    if (thdrP->type == TA_OBJ) {
        thdr_decr_obj_refs(thdrP, 0, thdrP->used);
    }
    TA_FREEMEM(thdrP);
}


static void ta_type_free_intrep(Tcl_Obj *objP)
{
    thdr_t *thdrP;

    TA_ASSERT(objP->typePtr == &g_ta_type);

    thdrP = TARRAYHDR(objP); 
    TA_ASSERT(thdrP);

    thdr_t_DECRREF(thdrP);
    TARRAYHDR(objP) = NULL;
    objP->typePtr = NULL;
}

static void ta_type_dup(Tcl_Obj *srcObj, Tcl_Obj *dstObj)
{
    TA_ASSERT(srcObj->typePtr == &g_ta_type);
    TA_ASSERT(TARRAYHDR(srcObj) != NULL);
        
    TA_OBJ_SETREP(dstObj, TARRAYHDR(srcObj));
}


/* Called to generate a string implementation from an array of Tcl_Obj */
static void ta_type_update_string_for_objtype(Tcl_Obj *objP)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
    Tcl_Obj **objv;
    int objc;
    thdr_t *thdrP;
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    int i, length, bytesNeeded = 0;
    const char *elem;
    char *dst;

    thdrP = TARRAYHDR(objP);
    objv = thdr_tELEMPTR(thdrP, Tcl_Obj *, 0);
    objc = thdrP->used;

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
        + strlen(g_ta_type_tokens[TA_OBJ]);
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = TCL_DONT_QUOTE_HASH;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if (bytesNeeded < 0)
            ta_string_overflow_panic("ta_type_update_string_for_objtype");
    }
    if (bytesNeeded > INT_MAX - objc + 1)
        ta_string_overflow_panic("ta_type_update_string_for_objtype");

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
    strcpy(dst, g_ta_type_tokens[TA_OBJ]);
    dst += strlen(g_ta_type_tokens[TA_OBJ]);
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


static void ta_type_update_string(Tcl_Obj *objP)
{
    unsigned int i, n, count;
    unsigned int allocated, unused, min_needed, prefix_len;
    char *cP;
    int max_elem_space;  /* Max space to print one element including
                            either terminating null or space */
    thdr_t *thdrP;
        
    TA_ASSERT(objP->typePtr == &g_ta_type);

    thdrP = TARRAYHDR(objP);
    TA_ASSERT(thdrP->type < sizeof(g_ta_type_tokens)/sizeof(g_ta_type_tokens[0]));

    objP->bytes = NULL;

    prefix_len = 
        sizeof("tarray ") - 1   /* -1 to exclude the null */
        + strlen(g_ta_type_tokens[thdrP->type])
        + 2;                         /* Start of list " {" */
    min_needed = prefix_len + 1 + 1;            /* Trailing "}" and null */

    count = TARRAYELEMCOUNT(objP);
    if (count == 0) {
        /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
           defined as ckalloc */
        cP = ckalloc(min_needed);
        objP->bytes = cP;
        _snprintf(cP, min_needed, "tarray %s {}",
                  g_ta_type_tokens[thdrP->type]);
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
            ba_t *baP = thdr_tELEMPTR(thdrP, ba_t, 0);
            register ba_t ba = *baP;
            register ba_t ba_mask;

            /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
               defined as ckalloc */
            cP = ckalloc(min_needed + 2*count - 1);
            n = _snprintf(cP, min_needed, "tarray %s {",
                      g_ta_type_tokens[TA_BOOLEAN]);
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
        ta_type_update_string_for_objtype(objP);
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
        ta_type_panic(thdrP->type);
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
    _snprintf(cP, prefix_len+1, "tarray %s {", g_ta_type_tokens[thdrP->type]);
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
                int *intP = thdr_tELEMPTR(thdrP, int, i);
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
                Tcl_WideInt *wideP = thdr_tELEMPTR(thdrP, Tcl_WideInt, i);
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
                double *dblP = thdr_tELEMPTR(thdrP, double, i);
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
                unsigned char *ucP = thdr_tELEMPTR(thdrP, unsigned char, i);
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

Tcl_Obj *tcol_new(thdr_t *thdrP)
{
    Tcl_Obj *objP = Tcl_NewObj();
    Tcl_InvalidateStringRep(objP);
    TA_OBJ_SETREP(objP, thdrP);
    return objP;
}

/* thdrP must NOT be shared and must have enough slots */
/* interp may be NULL (only used for errors) */
TCL_RESULT thdr_put_objs(Tcl_Interp *interp, thdr_t *thdrP,
                                 int first, int nelems,
                                 Tcl_Obj * const elems[])
{
    int i, ival;
    Tcl_WideInt wide;
    int status;

    TA_ASSERT(thdrP->nrefs < 2);
    TA_ASSERT((first + nelems) <= thdrP->allocated);

    thdr_mark_unsorted(thdrP); /* TBD - optimize */

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
     */

    if (first < thdrP->used) {
        if ((status = ta_verify_value_objs(interp, thdrP->type, nelems, elems))
            != TCL_OK)
            return TCL_ERROR;
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
            baP = thdr_tELEMPTR(thdrP, ba_t, first / BA_UNIT_SIZE);
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
            uintP = thdr_tELEMPTR(thdrP, unsigned int, first);
            for (i = 0; i < nelems; ++i, ++uintP) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], &wide) != TCL_OK)
                    goto convert_error;
                if (wide < 0 || wide > 0xFFFFFFFF) {
                    ta_value_type_error(interp, elems[i], thdrP->type);
                    goto convert_error;
                }
                *uintP = (unsigned int) wide;
            }
        }
        break;
    case TA_INT:
        {
            register int *intP;
            intP = thdr_tELEMPTR(thdrP, int, first);
            for (i = 0; i < nelems; ++i, ++intP) {
                if (Tcl_GetIntFromObj(interp, elems[i], intP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_WIDE:
        {
            register Tcl_WideInt *wideP;
            wideP = thdr_tELEMPTR(thdrP, Tcl_WideInt, first);
            for (i = 0; i < nelems; ++i, ++wideP) {
                if (Tcl_GetWideIntFromObj(interp, elems[i], wideP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_DOUBLE:
        {
            register double *dblP;
            dblP = thdr_tELEMPTR(thdrP, double, first);
            for (i = 0; i < nelems; ++i, ++dblP) {
                if (Tcl_GetDoubleFromObj(interp, elems[i], dblP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_OBJ:
        {
            register Tcl_Obj **objPP;
            objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, first);
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
            byteP = thdr_tELEMPTR(thdrP, unsigned char, first);
            for (i = 0; i < nelems; ++i, ++byteP) {
                if (Tcl_GetIntFromObj(interp, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival > 255 || ival < 0) {
                    ta_value_type_error(interp, elems[i], thdrP->type);
                    goto convert_error;
                }
                *byteP = (unsigned char) ival;
            }
        }
        break;

    default:
        ta_type_panic(thdrP->type);
    }

    if ((first + nelems) > thdrP->used)
        thdrP->used = first + nelems;

    return TCL_OK;

convert_error:                  /* Interp should already contain errors */
    TA_ASSERT(thdrP->type != TA_OBJ); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}

TCL_RESULT thdr_place_objs(
    Tcl_Interp *interp,
    thdr_t *thdrP,               /* thdr_t to be modified - must NOT be shared */
    thdr_t *indicesP,            /* Contains indices. */
    int highest_in_indices,          /* Highest index in indicesP.
                                   If >= thdrP->used, all intermediate indices
                                   must also be present in indicesP. Caller
                                   must have checked
                                */
    int nvalues,                /* # values in valuesP */
    Tcl_Obj * const *valuesP)   /* Values to be stored */
{
    int *indexP, *endP;
    int status;
    ta_value_t v;

    TA_ASSERT(thdrP->nrefs < 2);
    TA_ASSERT(indicesP->type == TA_INT);
    TA_ASSERT(highest_in_indices < thdrP->allocated);

    if (indicesP->used > nvalues)
        return ta_indices_count_error(interp, indicesP->used, nvalues);

    if (nvalues == 0)
        return TCL_OK;          /* Nothing to change */

    thdr_mark_unsorted(thdrP); /* TBD - optimize */

    /*
     * In case of conversion errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors.
     */

    if ((status = ta_verify_value_objs(interp, thdrP->type, nvalues, valuesP))
        != TCL_OK)
        return status;

    /*
     * Now  store the values. Note we do not have to check
     * status on any conversion since we did so already.
     */

#define PLACEVALUES(type, fn, var) do {                 \
        type *p;                                        \
        p = thdr_tELEMPTR(thdrP, type, 0);             \
        while (indexP < endP) {                         \
            status = fn(interp, *valuesP++, &var);      \
            TA_ASSERT(status == TCL_OK);                \
            TA_ASSERT(*indexP < thdrP->allocated);      \
            TA_ASSERT(*indexP <= thdrP->used);          \
            p[*indexP++] = (type) var;                  \
        }                                               \
    } while (0)
    
    indexP = thdr_tELEMPTR(indicesP, int, 0);
    endP = indexP + nvalues;
    switch (thdrP->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = thdr_tELEMPTR(thdrP, ba_t, 0);
            while (indexP < endP) {
                status = Tcl_GetBooleanFromObj(interp, *valuesP++, &v.ival);
                TA_ASSERT(status == TCL_OK); /* Since values are verified */
                TA_ASSERT(*indexP < thdrP->allocated);
                TA_ASSERT(*indexP <= thdrP->used);
                ba_put(baP, *indexP++, v.ival);
            }
        }
        break;

    case TA_UINT:
        PLACEVALUES(unsigned int, Tcl_GetWideIntFromObj, v.wval);
        break;
    case TA_INT:
        PLACEVALUES(int, Tcl_GetIntFromObj, v.ival);
        break;
    case TA_WIDE:
        PLACEVALUES(Tcl_WideInt, Tcl_GetWideIntFromObj, v.wval);
        break;
    case TA_DOUBLE:
        PLACEVALUES(double, Tcl_GetDoubleFromObj, v.dval);
        break;
    case TA_OBJ:
        {
            int i;
            Tcl_Obj **objPP;
            objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, 0);
            /*
             * Reference counts makes this tricky. If replacing an existing
             * index we have to increment the new value's ref and decrement
             * the old value's. If the index points to a previously unused
             * slot, then the value there is garbage and Tcl_DecrRefCount
             * should not be called on it. The problem is we cannot distinguish
             * the cases up front using thdrP->used as a threshold because
             * indicesP is in arbitrary order AND indices may be repeated.
             * Hence what we do is to store NULL first in all unused slots
             * that will be written to mark what is unused.
             */
            for (i = thdrP->used; i <= highest_in_indices; ++i)
                objPP[i] = NULL;
            while (indexP < endP) {
                /* Careful about the order here! */
                Tcl_IncrRefCount(*valuesP);
                if (objPP[*indexP] != NULL)
                    Tcl_DecrRefCount(*objPP);/* Deref what was originally in that slot */
                objPP[*indexP] = *valuesP++;
            }
        }
        break;

    case TA_BYTE:
        PLACEVALUES(unsigned int, Tcl_GetIntFromObj, v.ival);
        break;
    default:
        ta_type_panic(thdrP->type);
    }

    if (highest_in_indices >= thdrP->used)
        thdrP->used = highest_in_indices + 1;

    return TCL_OK;
}

int thdr_required_size(unsigned char tatype, int count)
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
        ta_type_panic(tatype);
    }

    return sizeof(thdr_t) + space;
}

thdr_t *thdr_realloc(Tcl_Interp *interp, thdr_t *oldP, int new_count)
{
    thdr_t *thdrP;

    TA_ASSERT(oldP->nrefs < 2);
    TA_ASSERT(oldP->used <= new_count);

    thdrP = (thdr_t *) TA_ATTEMPTREALLOCMEM((char *) oldP, thdr_required_size(oldP->type, new_count));
    if (thdrP)
        thdrP->allocated = new_count;
    else
        ta_memory_error(interp, new_count);
    return thdrP;
}

thdr_t * thdr_alloc(Tcl_Interp *interp, unsigned char tatype, int count)
{
    unsigned char nbits;
    thdr_t *thdrP;

    if (count == 0)
            count = TA_DEFAULT_NSLOTS;
    thdrP = (thdr_t *) TA_ATTEMPTALLOCMEM(thdr_required_size(tatype, count));
    if (thdrP == NULL) {
        if (interp)
            ta_memory_error(interp, count);
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
        ta_type_panic(tatype);
    }
    thdrP->elem_bits = nbits;
    thdrP->flags = 0;

    return thdrP;
}

thdr_t * thdr_alloc_and_init(Tcl_Interp *interp, unsigned char tatype,
                           int nelems, Tcl_Obj * const elems[],
                           int init_size)
{
    thdr_t *thdrP;

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

    thdrP = thdr_alloc(interp, tatype, init_size);
    if (thdrP) {
        if (elems != NULL && nelems != 0) {
            if (thdr_put_objs(interp, thdrP, 0, nelems, elems) != TCL_OK) {
                thdr_t_DECRREF(thdrP);
                thdrP = NULL;
            }
        }
    }

    return thdrP;               /* May be NULL on error */
}

/* Deletes a range from a thdr_t. See asserts below for requirements */
void thdr_delete_range(thdr_t *thdrP, int first, int count)
{
    int n;
    void *s, *d;

    TA_ASSERT(! thdr_sHARED(thdrP));
    TA_ASSERT(first >= 0);

    if (first >= thdrP->used)
        return;          /* Nothing to be deleted */
    
    if ((first + count) >= thdrP->used)
        count = thdrP->used - first;

    if (count <= 0)
        return;          /* Nothing to be deleted */

#ifdef NOTNEEDED    /* Deletion does not change sort state! */
    thdr_mark_unsorted(thdrP);
#endif

    /*
     * For all types other than BOOLEAN and OBJ, we can just memmove
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (thdrP->type) {
    case TA_BOOLEAN:
        ba_copy(thdr_tELEMPTR(thdrP, ba_t, 0), first, 
                thdr_tELEMPTR(thdrP, ba_t, 0), first+count,
                thdrP->used-(first+count));
        thdrP->used -= count;
        return;

    case TA_OBJ:
        /*
         * We have to deal with reference counts here. For the objects
         * we are deleting we need to decrement the reference counts.
         */

        thdr_decr_obj_refs(thdrP, first, count);
         
        /* Now we can just memcpy like the other types */
        n = count + first;         /* Point beyond deleted elements */
        s = thdr_tELEMPTR(thdrP, Tcl_Obj *, n);
        d = thdr_tELEMPTR(thdrP, Tcl_Obj *, first);
        n = (thdrP->used - n) * sizeof(Tcl_Obj *); /* #bytes to move */
        break;

    case TA_UINT:
    case TA_INT:
        n = count + first;         /* Point beyond deleted elements */
        s = thdr_tELEMPTR(thdrP, int, n);
        d = thdr_tELEMPTR(thdrP, int, first);
        n = (thdrP->used - n) * sizeof(int); /* #bytes to move */
        break;
    case TA_WIDE:
        n = count + first;         /* Point beyond deleted elements */
        s = thdr_tELEMPTR(thdrP, Tcl_WideInt, n);
        d = thdr_tELEMPTR(thdrP, Tcl_WideInt, first);
        n = (thdrP->used - n) * sizeof(Tcl_WideInt); /* #bytes to move */
        break;
    case TA_DOUBLE:
        n = count + first;         /* Point beyond deleted elements */
        s = thdr_tELEMPTR(thdrP, double, n);
        d = thdr_tELEMPTR(thdrP, double, first);
        n = (thdrP->used - n) * sizeof(double); /* #bytes to move */
        break;
    case TA_BYTE:
        n = count + first;         /* Point beyond deleted elements */
        s = thdr_tELEMPTR(thdrP, unsigned char, n);
        d = thdr_tELEMPTR(thdrP, unsigned char, first);
        n = (thdrP->used - n) * sizeof(unsigned char); /* #bytes to move */
        break;
    default:
        ta_type_panic(thdrP->type);
    }

    memmove(d, s, n);      /* NOT memcpy since overlapping copy */

    thdrP->used -= count;
}

void thdr_delete_Indices(thdr_t *thdrP, thdr_t *indicesP)
{
    int i;
    int *indexP;

    TA_ASSERT(indicesP->type == TA_INT);

    /*
     * We have to be careful to delete from back to front so as to not
     * invalidate index positions when earlier ones are deleted
     */
    TA_ASSERT(thdr_sorted(indicesP));
    
    /*
     * TBD - this will be desperately slow. Fix
     */
    
    /* We always want to delete back to front. However the index array
     * may be presorted in any direction. So check and loop accordingly
     */
    i = indicesP->used;
    if (thdr_sort_order(indicesP) > 0) {
        /* Sort order is ascending so iterate index array back to front */
        indexP = thdr_tELEMPTR(indicesP, int, indicesP->used-1 );
        while (i--) {
            if (*indexP >= 0 && *indexP < thdrP->used)
                thdr_delete_range(thdrP, *indexP, 1);
            --indexP;
        }
    } else {
        /* Sort order is descending so iterate index array front to back */
        indexP = thdr_tELEMPTR(indicesP, int, 0);
        while (i--) {
            if (*indexP >= 0 && *indexP < thdrP->used)
                thdr_delete_range(thdrP, *indexP, 1);
            ++indexP;
        }
    }
}

void thdr_tReverse(thdr_t *thdrP)
{
    int existing_sort_order;
    if (thdrP->used == 0)
        return;

    existing_sort_order = thdr_sorted(thdrP);
    
#define SWAPALL(thdrP_, type_)                                          \
    do {                                                                \
        type_ *front;                                                   \
        type_ *back;                                                    \
        front = thdr_tELEMPTR((thdrP_), type_, 0);                       \
        back  = thdr_tELEMPTR((thdrP_), type_, (thdrP_)->used-1);        \
        while (front < back) {                                          \
            type_ temp;                                                 \
            temp = *front;                                              \
            *front++ = *back;                                           \
            *back++ = temp;                                             \
        }                                                               \
    } while (0)

    switch (thdrP->type) {
    case TA_BOOLEAN:
        ba_reverse(thdr_tELEMPTR(thdrP, ba_t, 0), 0, thdrP->used);
        break;
    case TA_OBJ:    SWAPALL(thdrP, Tcl_Obj*); break;
    case TA_UINT:   /* Fall thru */
    case TA_INT:    SWAPALL(thdrP, int); break;
    case TA_WIDE:   SWAPALL(thdrP, Tcl_WideInt); break;
    case TA_DOUBLE: SWAPALL(thdrP, double); break;
    case TA_BYTE:   SWAPALL(thdrP, unsigned char); break;
    default:
        ta_type_panic(thdrP->type);
    }

    if (existing_sort_order) {
        if (existing_sort_order < 0)
            thdr_mark_sorted_ascending(thdrP);
        else
            thdr_mark_sorted_descending(thdrP);
    }
}


/* Copies partial content from one thdr_t to another. See asserts below
   for requirements */
void thdr_copy(thdr_t *dstP, int dst_first,
                   thdr_t *srcP, int src_first, int count)
{
    int nbytes;
    void *s, *d;

    TA_ASSERT(dstP != srcP);
    TA_ASSERT(dstP->type == srcP->type);
    TA_ASSERT(! thdr_sHARED(dstP));
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

    thdr_mark_unsorted(dstP); /* TBD - optimize */

    /*
     * For all types other than BOOLEAN and OBJ, we can just memcpy
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (srcP->type) {
    case TA_BOOLEAN:
        ba_copy(thdr_tELEMPTR(dstP, ba_t, 0), dst_first,
                thdr_tELEMPTR(srcP, ba_t, 0), src_first, count);
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

        thdr_incr_obj_refs(srcP, src_first, count); /* Do this first */
        /* Note this call take care of the case where count exceeds
         * actual number in dstP
         */
        thdr_decr_obj_refs(dstP, dst_first, count);
         
        /* Now we can just memcpy like the other types */
        nbytes = count * sizeof(Tcl_Obj *);
        s = thdr_tELEMPTR(srcP, Tcl_Obj *, src_first);
        d = thdr_tELEMPTR(dstP, Tcl_Obj *, dst_first);
        break;

    case TA_UINT:
    case TA_INT:
        nbytes = count * sizeof(int);
        s = thdr_tELEMPTR(srcP, int, src_first);
        d = thdr_tELEMPTR(dstP, int, dst_first);
        break;
    case TA_WIDE:
        nbytes = count * sizeof(Tcl_WideInt);
        s = thdr_tELEMPTR(srcP, Tcl_WideInt, src_first);
        d = thdr_tELEMPTR(dstP, Tcl_WideInt, dst_first);
        break;
    case TA_DOUBLE:
        nbytes = count * sizeof(double);
        s = thdr_tELEMPTR(srcP, double, src_first);
        d = thdr_tELEMPTR(dstP, double, dst_first);
        break;
    case TA_BYTE:
        nbytes = count * sizeof(unsigned char);
        s = thdr_tELEMPTR(srcP, unsigned char, src_first);
        d = thdr_tELEMPTR(dstP, unsigned char, dst_first);
        break;
    default:
        ta_type_panic(srcP->type);
    }

    memcpy(d, s, nbytes);

    if ((dst_first + count) > dstP->used)
        dstP->used = dst_first + count;

    return;
}

/* Copies partial content from one thdr_t to another in reverse.
   See asserts below for requirements */
void thdr_copy_reversed(thdr_t *dstP, int dst_first,
                       thdr_t *srcP, int src_first, int count)
{
    TA_ASSERT(dstP != srcP);
    TA_ASSERT(dstP->type == srcP->type);
    TA_ASSERT(! thdr_sHARED(dstP));
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

    thdr_mark_unsorted(dstP); /* TBD - optimize for sorted arrays */

#define COPYREVERSE(type_, dstP_, doff_, srcP_, soff_, count_)          \
    do {                                                                \
        type_ *src;                                                     \
        type_ *dst;                                                     \
        int    i = (count_);                                            \
        src = thdr_tELEMPTR((srcP_), type_ , (soff_));                  \
        dst  = thdr_tELEMPTR((dstP_), type_ , 0);                       \
        dst += (doff_ ) + i - 1;                                        \
        while (i--) {                                                   \
            /* Remember caller ensured no overlap between src & dst */  \
            *dst-- = *src++;                                            \
        }                                                               \
    } while (0)

    switch (srcP->type) {
    case TA_BOOLEAN:
        ba_copy(thdr_tELEMPTR(dstP, ba_t, 0), dst_first,
                thdr_tELEMPTR(srcP, ba_t, 0), src_first, count);
        ba_reverse(thdr_tELEMPTR(dstP, ba_t, 0), dst_first, count);
        break;
    case TA_OBJ:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment the reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        thdr_incr_obj_refs(srcP, src_first, count); /* Do this first */
        /* Note this call take care of the case where count exceeds
         * actual number in dstP
         */
        thdr_decr_obj_refs(dstP, dst_first, count);
        COPYREVERSE(Tcl_Obj*, dstP, dst_first, srcP, src_first, count);
        break;

    case TA_UINT:
    case TA_INT:
        COPYREVERSE(int, dstP, dst_first, srcP, src_first, count);
        break;
    case TA_WIDE:
        COPYREVERSE(Tcl_WideInt, dstP, dst_first, srcP, src_first, count);
        break;
    case TA_DOUBLE:
        COPYREVERSE(double, dstP, dst_first, srcP, src_first, count);
        break;
    case TA_BYTE:
        COPYREVERSE(unsigned char, dstP, dst_first, srcP, src_first, count);
        break;
    default:
        ta_type_panic(srcP->type);
    }

    if ((dst_first + count) > dstP->used)
        dstP->used = dst_first + count;

    return;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone(Tcl_Interp *interp, thdr_t *srcP, int minsize)
{
    thdr_t *thdrP;

    if (minsize == 0)
        minsize = srcP->allocated;
    else if (minsize < srcP->used)
        minsize = srcP->used;

    /* TBD - optimize these two calls */
    thdrP = thdr_alloc(interp, srcP->type, minsize);
    if (thdrP) {
        thdr_copy(thdrP, 0, srcP, 0, srcP->used);
        thdr_copy_sort_status(thdrP, srcP);
    }
    return thdrP;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone_reversed(Tcl_Interp *interp, thdr_t *srcP, int minsize)
{
    thdr_t *thdrP;
    int existing_sort_order;

    existing_sort_order = thdr_sorted(srcP);

    if (minsize == 0)
        minsize = srcP->allocated;
    else if (minsize < srcP->used)
        minsize = srcP->used;

    /* TBD - optimize these two calls */
    thdrP = thdr_alloc(interp, srcP->type, minsize);
    if (thdrP) {
        thdr_copy_reversed(thdrP, 0, srcP, 0, srcP->used);
        if (existing_sort_order) {
            /* Sort order is not reversed */
            if (existing_sort_order < 0)
                thdr_mark_sorted_ascending(thdrP);
            else
                thdr_mark_sorted_descending(thdrP);
        }
    }
    return thdrP;
}

thdr_t *thdr_range(Tcl_Interp *interp, thdr_t *srcP, int low, int count)
{
    thdr_t *thdrP;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    thdrP = thdr_alloc(interp, srcP->type, count);
    if (thdrP) {
        thdr_copy(thdrP, 0, srcP, low, count);
        thdr_copy_sort_status(thdrP, srcP);
    }
    return thdrP;
}

Tcl_Obj *ta_range(Tcl_Interp *interp, Tcl_Obj *srcObj, int low, int count,
                     int fmt)
{
    int end;
    thdr_t *srcP;
    Tcl_Obj *objP;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    srcP = TARRAYHDR(srcObj);

    if (fmt == TA_FORMAT_TARRAY) {
        thdr_t *thdrP = thdr_range(interp, srcP, low, count);
        return thdrP == NULL ? NULL : tcol_new(thdrP);
    }

    end = low + count;
    if (end > srcP->used)
        end = srcP->used;

    /* Even dicts more efficiently built as lists and shimmered as necessary */
    objP = Tcl_NewListObj(end-low, NULL);

    switch (srcP->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = thdr_tELEMPTR(srcP, ba_t, 0);
            while (low < end) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low));
                Tcl_ListObjAppendElement(interp, objP, 
                                         Tcl_NewIntObj(ba_get(baP, low)));
                ++low;
            }
        }
        break;
    case TA_UINT:
        {
            unsigned int *uiP = thdr_tELEMPTR(srcP, unsigned int, low);
            unsigned int *uiendP = thdr_tELEMPTR(srcP, unsigned int, end);
            while (uiP < uiendP) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(interp, objP, Tcl_NewWideIntObj(*uiP++));
            }
        }
        break;
    case TA_INT:
        {
            int *iP = thdr_tELEMPTR(srcP, int, low);
            int *iendP = thdr_tELEMPTR(srcP, int, end);
            while (iP < iendP) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(*iP++));
            }
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *wideP = thdr_tELEMPTR(srcP, Tcl_WideInt, low);
            Tcl_WideInt *wideendP = thdr_tELEMPTR(srcP, Tcl_WideInt, end);
            while (wideP < wideendP) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(interp, objP, Tcl_NewWideIntObj(*wideP++));
            }
        }
        break;
    case TA_DOUBLE:
        {
            double *dblP = thdr_tELEMPTR(srcP, double, low);
            double *dblendP = thdr_tELEMPTR(srcP, double, end);
            while (dblP < dblendP) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(interp, objP, Tcl_NewDoubleObj(*dblP++));
            }
        }
        break;
    case TA_BYTE:
        {
            unsigned char *ucP = thdr_tELEMPTR(srcP, unsigned char, low);
            unsigned char *ucendP = thdr_tELEMPTR(srcP, unsigned char, end);
            while (ucP < ucendP) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(*ucP++));
            }
        }
        break;
    case TA_OBJ:
        {
            Tcl_Obj **oPP = thdr_tELEMPTR(srcP, Tcl_Obj *, low);
            Tcl_Obj **oendPP = thdr_tELEMPTR(srcP, Tcl_Obj *, end);
            while (oPP < oendPP) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(interp, objP, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(interp, objP, *oPP);
            }
        }
        break;
    default:
        ta_type_panic(srcP->type);
    }

    return objP;
}


/*
 * Convert a TArray Tcl_Obj to one that is suitable for modifying.
 * The Tcl_Obj must NOT be shared.
 * There are three cases to consider:
 * (1) Even though taObj is unshared, the corresponding thdr_t might
 *     still be shared (pointed to from elsewhere). In this case
 *     also, we clone the thdr_t and store it as the new internal rep.
 * (2) If its thdr_t is unshared, we can modify in
 *     place, unless 
 * (3) thdr_t is too small in which case we have to reallocate it.
 *
 * Invalidates the string rep in all cases.
 * Only fails on memory allocation failure.
 */
TCL_RESULT ta_make_modifiable(Tcl_Interp *interp,
                                Tcl_Obj *taObj, int minsize, int prefsize)
{
    thdr_t *thdrP;

    TA_ASSERT(taObj->typePtr == &g_ta_type);
    TA_ASSERT(! Tcl_IsShared(taObj));

    thdrP = TARRAYHDR(taObj);
    if (prefsize == 0)
        prefsize = thdrP->allocated;
    if (minsize < thdrP->used)
        minsize = thdrP->used;
    if (minsize > prefsize)
        prefsize = minsize;

    if (thdr_sHARED(thdrP)) {
        /* Case (1) */
        thdrP = thdr_clone(interp, thdrP, prefsize);
        if (thdrP == NULL)
            return TCL_ERROR;   /* Note taObj is not changed */
        thdr_t_DECRREF(TARRAYHDR(taObj)); /* Release old */
        TARRAYHDR(taObj) = NULL;
        TA_OBJ_SETREP(taObj, thdrP);
        Tcl_InvalidateStringRep(taObj);
    } else if (thdrP->allocated < minsize) {
        /* Case (3). Note don't use TA_OBJ_SETREP as we are keeping all 
           fields and ref counts the same */
        Tcl_InvalidateStringRep(taObj);
        thdrP = thdr_realloc(interp, thdrP, prefsize);
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
Tcl_Obj * thdr_index(thdr_t *thdrP, int index)
{
    TA_ASSERT(index >= 0 && index < thdrP->used);

    switch (thdrP->type) {
    case TA_BOOLEAN:
        return Tcl_NewIntObj(ba_get(thdr_tELEMPTR(thdrP, ba_t, 0), index));
    case TA_UINT:
        return Tcl_NewWideIntObj(*thdr_tELEMPTR(thdrP, unsigned int, index));
    case TA_INT:
        return Tcl_NewIntObj(*thdr_tELEMPTR(thdrP, int, index));
    case TA_WIDE:
        return Tcl_NewWideIntObj(*thdr_tELEMPTR(thdrP, Tcl_WideInt, index));
    case TA_DOUBLE:
        return Tcl_NewDoubleObj(*thdr_tELEMPTR(thdrP, double, index));
    case TA_BYTE:
        return Tcl_NewIntObj(*thdr_tELEMPTR(thdrP, unsigned char, index));
    case TA_OBJ:
        return *thdr_tELEMPTR(thdrP, Tcl_Obj *, index);
    default:
        ta_type_panic(thdrP->type);
    }
}

/*
 * Converts the passed Tcl_Obj objP to integer indexes. If a single index
 * stores it in *indexP and returns TA_INDEX_TYPE_INT. If multiple indices,
 * stores a thdr_t of type int containing the indices into *thdrPP and
 * returns TA_INDEX_TYPE_thdr_t. The thdr_t's ref count is incremented
 * so caller should call thdr_t_DECRREF as appropriate.
 *
 * If indexP is NULL, always returns as TA_INDEX_TYPE_thdr_t.
 *
 * This facility to return a single int or a index list should only
 * be used by commands where it does not matter whether {1} is treated
 * as a list or an int, for example the fill command, and the distinction
 * is just an efficiency issue. Commands should
 * not use this to pick whether a single index or a list was specified
 * if it impacts their semantics.
 */
int tcol_to_indices(Tcl_Interp *interp, Tcl_Obj *objP,
                           int want_sorted,
                           thdr_t **thdrPP, /* Cannot be NULL */
                           int *indexP)    /* Can be NULL */
{
    thdr_t *thdrP;
    Tcl_Obj **elems;
    int       n;
    TCL_RESULT status;

    /*
     * For efficiencies sake, we need to avoid shimmering. So we first
     * check for specific types and default to a list otherwise.
     */
    if (objP->typePtr == &g_ta_type) {
        if (TARRAYTYPE(objP) == TA_INT) {
            thdrP = TARRAYHDR(objP);
            if (want_sorted && ! thdr_sorted(thdrP)) {
                thdrP = thdr_clone(interp, thdrP, thdrP->used);
                if (thdrP == NULL)
                    return TA_INDEX_TYPE_ERROR;
                qsort(thdr_tELEMPTR(thdrP, int, 0), thdrP->used, sizeof(int), intcmp);
                thdr_mark_sorted_ascending(thdrP);
            }
            thdrP->nrefs++;
            *thdrPP = thdrP;
            return TA_INDEX_TYPE_thdr_t;
        } else {
            /* TBD - write conversion from other type tarrays */
            ta_indices_error(interp, objP);
            return TA_INDEX_TYPE_ERROR;
        }
    }

    /* To prevent shimmering, first check known to be a list */
    if (objP->typePtr != g_tcl_list_type_ptr && indexP != NULL) {
        status = Tcl_GetIntFromObj(NULL, objP, &n);
        if (status == TCL_OK) {
            *indexP = n;
            return TA_INDEX_TYPE_INT;
        }
        /* else fall through to try as list */
    }

    if (Tcl_ListObjGetElements(NULL, objP, &n, &elems) != TCL_OK) {
        ta_indices_error(interp, objP);
        return TA_INDEX_TYPE_ERROR;
    }

    thdrP = thdr_alloc_and_init(interp, TA_INT, n, elems, 0);
    if (thdrP) {
        if (want_sorted) {
            qsort(thdr_tELEMPTR(thdrP, int, 0), thdrP->used, sizeof(int), intcmp);
            thdr_mark_sorted_ascending(thdrP);
        }
        thdrP->nrefs++;
        *thdrPP = thdrP;
        return TA_INDEX_TYPE_thdr_t;
    } else
        return TA_INDEX_TYPE_ERROR;
}

/* Returns a newly allocated thdr_t (with ref count 0) containing the
   values from the specified indices */
Tcl_Obj *tcol_get(Tcl_Interp *interp, thdr_t *srcP, thdr_t *indicesP, int fmt)
{
    thdr_t *thdrP;
    int count, index, bound;
    int *indexP, *endP;
    Tcl_Obj *taObj;
    void *srcbaseP, *thdrbaseP;

    TA_ASSERT(indicesP->type == TA_INT);
    count = indicesP->used;

    if (fmt == TA_FORMAT_TARRAY) {
        thdrP = thdr_alloc(interp, srcP->type, count);
        if (thdrP == NULL)
            return NULL;
        thdrbaseP = thdr_tELEMPTR(thdrP, unsigned char, 0);
        taObj = tcol_new(thdrP);
    } else {
        thdrP = NULL;
        thdrbaseP = NULL;
        taObj = Tcl_NewListObj(fmt == TA_FORMAT_LIST ? count : 2*count, NULL);
    }
    if (count == 0)
        return taObj;           /* Empty index list so nothing to return */

#define tcol_get_COPY(type_, objfn_)                                   \
    do {                                                                \
        type_ *fromP = srcbaseP;                                        \
        type_ *toP = thdrbaseP;                                         \
        while (indexP < endP) {                                         \
            index = *indexP++;                                          \
            if (index < 0 || index >= bound)                            \
                goto index_error;                                       \
            if (fmt == TA_FORMAT_TARRAY)                                \
                *toP++ = fromP[index];                                  \
            else {                                                      \
                if (fmt == TA_FORMAT_DICT)                              \
                    Tcl_ListObjAppendElement(interp, taObj, Tcl_NewIntObj(index)); \
                Tcl_ListObjAppendElement(interp, taObj,                 \
                                         objfn_(fromP[index])); \
            }                                                           \
        }                                                               \
        if (fmt == TA_FORMAT_TARRAY)                                    \
            thdrP->used = count;                                        \
    } while (0)


    indexP = thdr_tELEMPTR(indicesP, int, 0);
    endP = indexP + count;
    srcbaseP = thdr_tELEMPTR(srcP, unsigned char, 0);
    bound = srcP->used;
    switch (srcP->type) {
    case TA_BOOLEAN:
        {
            ba_t *srcbaP = srcbaseP;
            ba_t *baP = thdrbaseP;
            int i;
            for (i = 0; indexP < endP; ++i, ++indexP) {
                index = *indexP; 
                if (index < 0 || index >= bound)
                    goto index_error;
                if (fmt == TA_FORMAT_TARRAY)
                    ba_put(baP, i, ba_get(srcbaP, index));
                else {
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(interp, taObj, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(interp, taObj,
                                             Tcl_NewIntObj(ba_get(srcbaP, index)));
                }
            }
        }
        break;
    case TA_UINT:
    case TA_INT:
        tcol_get_COPY(unsigned int, Tcl_NewIntObj);
        break;
    case TA_WIDE:
        tcol_get_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
        break;
    case TA_DOUBLE:
        tcol_get_COPY(double, Tcl_NewDoubleObj);
        break;
    case TA_BYTE:
        tcol_get_COPY(unsigned char, Tcl_NewIntObj);
        break;
    case TA_OBJ:
        /* Cannot use macro here because of ref counts etc. */
        {
            Tcl_Obj **objsrcPP = srcbaseP;
            Tcl_Obj **objPP = thdrbaseP;
            while (indexP < endP) {
                index = *indexP++; 
                if (index < 0 || index >= bound)
                    goto index_error;
                if (fmt == TA_FORMAT_TARRAY) {
                    *objPP = objsrcPP[index];
                    Tcl_IncrRefCount(*objPP);
                    ++objPP;
                    thdrP->used++; /* Bump as we go along in case taObj has
                                      to be released on error */
                } else {
                    /* No need to bump ref counts as lists take care of it */
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(interp, taObj, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(interp, taObj, objsrcPP[index]);
                }
            }
        }
        break;
    default:
        ta_type_panic(srcP->type);
    }

    return taObj;

index_error:   /* index should hold the current index in error */
    ta_index_range_error(interp, index);

    if (taObj)
        Tcl_DecrRefCount(taObj);
    return NULL;

}

/* See asserts for conditions */
TCL_RESULT tcol_delete(Tcl_Interp *interp, Tcl_Obj *taObj,
                        Tcl_Obj *indexA, Tcl_Obj *indexB)
{
    int low, count;
    int status;

    TA_ASSERT(! Tcl_IsShared(taObj));

    if ((status = tcol_convert(interp, taObj)) != TCL_OK)
        return status;

    status = ta_make_modifiable(interp, taObj, TARRAYELEMCOUNT(taObj),
                                  TARRAYELEMCOUNT(taObj));
    if (status == TCL_OK) {
        thdr_t *thdrP = TARRAYHDR(taObj);
        if (indexB) {
            status = ta_fix_range_bounds(interp, thdrP, indexA,
                                             indexB, &low, &count);
            if (status == TCL_OK)
                thdr_delete_range(thdrP, low, count);
        } else {
            /* Not a range, either a list or single index */
            thdr_t *indicesP;
            /* Note status is TCL_OK at this point */
            switch (tcol_to_indices(interp, indexA, 1, &indicesP, &low)) {
            case TA_INDEX_TYPE_ERROR:
                status = TCL_ERROR;
                break;
            case TA_INDEX_TYPE_INT:
                thdr_delete_range(thdrP, low, 1);
                break;
            case TA_INDEX_TYPE_thdr_t:
                thdr_delete_Indices(thdrP, indicesP);
                thdr_t_DECRREF(indicesP);
                break;
            }
        }
    }

    return status;
}

TCL_RESULT tcol_fill_obj(Tcl_Interp *interp, Tcl_Obj *taObj,
                             Tcl_Obj *valueObj,
                             Tcl_Obj *indexA, Tcl_Obj *indexB)
{
    int low, count;
    int status;
    ta_value_t value;

    TA_ASSERT(! Tcl_IsShared(taObj));

    if ((status = tcol_convert(interp, taObj)) != TCL_OK)
        return status;
    if ((status = ta_value_from_obj(interp, valueObj,
                                     TARRAYTYPE(taObj), &value)) != TCL_OK)
        return status;

    if (indexB) {
        status = ta_fix_range_bounds(interp, TARRAYHDR(taObj), indexA,
                                         indexB, &low, &count);
        if (status == TCL_OK && count != 0) {
            status = ta_make_modifiable(interp, taObj, low+count, TARRAYELEMSLOTS(taObj));
            if (status == TCL_OK)
                thdr_fill_range(interp, TARRAYHDR(taObj), &value, low, count);
        }
    } else {
        /* Not a range, either a list or single index */
        thdr_t *indicesP;
        /* Note status is TCL_OK at this point */
        switch (tcol_to_indices(interp, indexA, 1, &indicesP, &low)) {
        case TA_INDEX_TYPE_ERROR:
            status = TCL_ERROR;
            break;
        case TA_INDEX_TYPE_INT:
            if (low < 0 || low > TARRAYELEMCOUNT(taObj)) {
                ta_index_range_error(interp, low);
                status = TCL_ERROR;
            } else {
                status = ta_make_modifiable(interp, taObj, low+1, 0);
                if (status == TCL_OK)
                    thdr_fill_range(interp, TARRAYHDR(taObj), &value, low, 1);
            }
            break;
        case TA_INDEX_TYPE_thdr_t:
            status = thdr_verify_indices(interp, TARRAYHDR(taObj), indicesP, &count);
            if (status == TCL_OK) {
                status = ta_make_modifiable(interp, taObj, count, count); // TBD - count + extra?
                if (status == TCL_OK)
                    thdr_fill_indices(interp, TARRAYHDR(taObj), &value, indicesP);
            }
            thdr_t_DECRREF(indicesP);
            break;
        }
    }

    return status;
}

int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
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

TCL_RESULT tcol_copy_thdr(Tcl_Interp *interp, Tcl_Obj *taObj, thdr_t *srcP, Tcl_Obj *firstObj)
{
    int first, status;

    TA_ASSERT(! Tcl_IsShared(taObj));

    if ((status = tcol_convert(interp, taObj)) != TCL_OK)
        return status;
    if (TARRAYTYPE(taObj) != srcP->type)
        return ta_mismatched_types_error(interp);

    status = ta_convert_index(interp, firstObj, &first, TARRAYELEMCOUNT(taObj),
                        0, TARRAYELEMCOUNT(taObj));
    if (status == TCL_OK && srcP->used) {
        status = ta_make_modifiable(interp, taObj, first + srcP->used, 0);
        if (status == TCL_OK)
            thdr_copy(TARRAYHDR(taObj), first, srcP, 0, srcP->used); 
    }
    return status;
}

/* The tarray Tcl_Obj is modified */
TCL_RESULT tcol_put_objs(Tcl_Interp *interp, Tcl_Obj *taObj,
                             Tcl_Obj *valueListObj, Tcl_Obj *firstObj)
{
    int status;
    Tcl_Obj **valueObjs;
    int nvalues;
    int first;

    TA_ASSERT(! Tcl_IsShared(taObj));

    status = Tcl_ListObjGetElements(interp, valueListObj, &nvalues, &valueObjs);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(interp, taObj)) != TCL_OK)
        return status;

    /* Get the limits of the range to set */

    status = ta_convert_index(interp, firstObj, &first, TARRAYELEMCOUNT(taObj),
                        0, TARRAYELEMCOUNT(taObj));
    if (status == TCL_OK && nvalues) {
        /* Note this also invalidates the string rep as desired */
        status = ta_make_modifiable(interp, taObj, first + nvalues, 0);
        if (status == TCL_OK) {
            /* Note even on error thdr_put_objs guarantees a consistent 
             * and unchanged taObj
             */
            status = thdr_put_objs(interp, TARRAYHDR(taObj),
                                      first, nvalues, valueObjs);
        }
    }
    
    return status;
}

TCL_RESULT tcol_place_objs(Tcl_Interp *interp, Tcl_Obj *taObj,
                               Tcl_Obj *valueListObj,
                               Tcl_Obj *indicesObj)
{
    int new_size;
    int status;
    thdr_t *indicesP;
    Tcl_Obj **valueObjs;
    int nvalues;
    thdr_t *sortedP;

    TA_ASSERT(! Tcl_IsShared(taObj));

    status = Tcl_ListObjGetElements(interp, valueListObj, &nvalues, &valueObjs);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(interp, taObj)) != TCL_OK)
        return status;

    if (tcol_to_indices(interp, indicesObj, 0, &indicesP, NULL)
        != TA_INDEX_TYPE_thdr_t)
        return TCL_ERROR;

    if (indicesP->used == 0)
        return TCL_OK;

    /* For verification we will need to sort indices */
    sortedP = indicesP;
    if (! thdr_sorted(sortedP)) {
        sortedP = thdr_clone(interp, sortedP, 0);
        if (sortedP == NULL)
            return TCL_ERROR;
        qsort(thdr_tELEMPTR(sortedP, int, 0), sortedP->used, sizeof(int), intcmp);
        thdr_mark_sorted_ascending(sortedP);
    }

    status = thdr_verify_indices(interp, TARRAYHDR(taObj), sortedP, &new_size);
    if (sortedP != indicesP)
        thdr_t_DECRREF(sortedP);
    if (status == TCL_OK) {
        status = ta_make_modifiable(interp, taObj, new_size, new_size); // TBD - count + extra?
        if (status == TCL_OK) {
            status = thdr_place_objs(interp, TARRAYHDR(taObj), indicesP,
                                        new_size-1, /* Highest index in indicesP */
                                        nvalues, valueObjs);
        }
    }
    thdr_t_DECRREF(indicesP);

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
    thdr_t *gridHdrP;
    ta_value_t values[32];
    ta_value_t *valuesP;
    Tcl_Obj **taObjPP;
    int status;
    int new_size;
    int collength;

    TA_ASSERT(! Tcl_IsShared(gridObj));

    if ((status = tcol_convert(interp, gridObj)) != TCL_OK)
        return status;

    gridHdrP = TARRAYHDR(gridObj);

    if ((status = Tcl_ListObjLength(interp, rowObj, &row_width)) != TCL_OK)
        return status;

    if (row_width != gridHdrP->used)
        return ta_row_width_error(interp, row_width, gridHdrP->used);

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

    if (lowObj->typePtr == &g_ta_type)
        lowObj = Tcl_DuplicateObj(lowObj);
    else
        Tcl_IncrRefCount(lowObj); /* Since we will release at end */

    if (highObj->typePtr == &g_ta_type)
        highObj = Tcl_DuplicateObj(highObj);
    else
        Tcl_IncrRefCount(highObj); /* Since we will release at end */

    if (row_width > sizeof(values)/sizeof(values[0])) {
        valuesP = (ta_value_t *) TA_ALLOCMEM(row_width * sizeof(ta_value_t));
    } else {
        valuesP = values;
    }
        
    /* Make sure grid object is modifiable */
    if ((status = ta_make_modifiable(interp, gridObj,
                                       gridHdrP->used,
                                       gridHdrP->allocated)) != TCL_OK)
        goto vamoose;
    gridHdrP = TARRAYHDR(gridObj); /* Might have changed */

    /* Grids don't care about sort bits, but nevertheless.. */
    thdr_mark_unsorted(gridHdrP);

    /*
     * Now verify tarrays and values. The latter should be of the
     * appropriate type. Also ensure all tarrays are the same size
     * and can be modified.
     *
     * NOTE THAT AT ALL TIMES, gridObj has valid unmodified contents
     * (logically unmodified though allocated blocks might have changed)
     * so even on error we exit with a clean gridObj.
     */
    for (i = 0, taObjPP = thdr_tELEMPTR(gridHdrP, Tcl_Obj *, 0);
         i < row_width;
         ++i, ++taObjPP) {
        thdr_t *thdrP;
        Tcl_Obj *valueObj;
        Tcl_Obj *colObj = *taObjPP;

        if ((status = tcol_convert(interp, *taObjPP)) != TCL_OK)
            goto vamoose;

        thdrP = TARRAYHDR(colObj);
        if (i == 0) {
            collength = thdrP->used;
            /* Get the limits of the range to set */
            status = ta_fix_range_bounds(interp, thdrP, lowObj, highObj, &low, &count);
            if (status != TCL_OK || count == 0)
                goto vamoose;   /* Error or nothing to do */
        }
        else if (thdrP->used != collength) {
            status = ta_grid_length_error(interp);
            goto vamoose;
        }

        /* Preferred size in case we have to reallocate */
        new_size = low + count + TA_EXTRA(low+count); /* Disregarded if < allocated */

        /* We have already converted above */
        TA_ASSERT(colObj->typePtr == &g_ta_type);
        if (Tcl_IsShared(colObj)) {
            colObj = Tcl_DuplicateObj(colObj);
            Tcl_IncrRefCount(colObj);
            Tcl_DecrRefCount(*taObjPP);
            *taObjPP = colObj;
            thdrP = TARRAYHDR(colObj);
        }

        /* Note this also invalidates the string rep as desired */
        if ((status = ta_make_modifiable(interp, colObj, low+count, new_size)) != TCL_OK)
            goto vamoose;

        if ((status = Tcl_ListObjIndex(interp, rowObj, i, &valueObj)) != TCL_OK)
            goto vamoose;
        status = ta_value_from_obj(interp, valueObj, thdrP->type, &valuesP[i]);
        if (status != TCL_OK)
            goto vamoose;
    }


    /*
     * We can now do the actual modifications. All validation and memory
     * allocations are done.
     * NOTE: NO ERRORS ARE EXPECTED BEYOND THIS POINT EXCEPT FATAL ONES
     */

    for (i = 0, taObjPP = thdr_tELEMPTR(gridHdrP, Tcl_Obj *, 0);
         i < row_width;
         ++i, ++taObjPP) {
        thdr_fill_range(interp, TARRAYHDR(*taObjPP), &valuesP[i], low, count);
    }
    status = TCL_OK;
    
vamoose:                   /* interp must already hold error message */
    Tcl_DecrRefCount(lowObj);
    Tcl_DecrRefCount(highObj);
    if (valuesP != values)
        TA_FREEMEM((char *) valuesP);

    return status;
}



/* interp may be NULL (only used for errors) */
/* See asserts in code for prerequisite conditions */
TCL_RESULT thdr_tSetMultipleFromObjs(Tcl_Interp *interp,
                                thdr_t * const thdrs[], int nthdrs,
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
    thdr_t *thdrP;

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
                        ta_value_type_error(interp, fields[t], thdrP->type);
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
                        ta_value_type_error(interp, fields[t], thdrP->type);
                        goto error_return;
                    }
                    break;
                case TA_OBJ:
                    break;      /* No validation */
                default:
                    ta_type_panic(thdrs[t]->type);
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

            thdr_mark_unsorted(thdrP); /* TBD - optimize */
            switch (thdrs[t]->type) {
            case TA_BOOLEAN:
                {
                    register ba_t *baP;
                    ba_t ba, ba_mask;
                    int off;

                    /* Take care of the initial condition where the first bit
                       may not be aligned on a boundary */
                    baP = thdr_tELEMPTR(thdrP, ba_t, first / BA_UNIT_SIZE);
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
                    uintP = thdr_tELEMPTR(thdrP, unsigned int, first);
                    for (r = 0; r < nrows; ++r, ++uintP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetWideIntFromObj(interp, valObj, &wide) != TCL_OK)
                            goto error_return;
                        if (wide < 0 || wide > 0xFFFFFFFF) {
                            ta_value_type_error(interp, valObj, thdrP->type);
                            goto error_return;
                        }
                        *uintP = (unsigned int) wide;
                    }
                }
                break;
            case TA_INT:
                {
                    register int *intP;
                    intP = thdr_tELEMPTR(thdrP, int, first);
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
                    wideP = thdr_tELEMPTR(thdrP, Tcl_WideInt, first);
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
                    dblP = thdr_tELEMPTR(thdrP, double, first);
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
                    byteP = thdr_tELEMPTR(thdrP, unsigned char, first);
                    for (r = 0; r < nrows; ++r, ++byteP) {
                        Tcl_ListObjIndex(interp, rows[r], t, &valObj);
                        TA_ASSERT(valObj);
                        if (Tcl_GetIntFromObj(interp, valObj, &ival) != TCL_OK)
                            goto error_return;
                        if (ival > 255 || ival < 0) {
                            ta_value_type_error(interp, valObj, thdrP->type);
                            goto error_return;
                        }
                        *byteP = (unsigned char) ival;
                    }
                }
                break;
            default:
                ta_type_panic(thdrP->type);
            }
        }
    }

    /* Now that no errors are possible, update the TA_OBJ columns */
    for (t=0; t < nthdrs; ++t) {
        register Tcl_Obj **objPP;
        thdrP = thdrs[t];
        if (thdrP->type != TA_OBJ)
            continue;
        thdr_mark_unsorted(thdrP); /* TBD - optimize */
        objPP = thdr_tELEMPTR(thdrP, Tcl_Obj *, first);
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
