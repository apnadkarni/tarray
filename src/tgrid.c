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
 * TGrid is a Tcl "type" used for storing arrays of TArrays.
 * Internally it is stored as a thdr_t containing TArrays Tcl_Objs.
 */
static void TGridTypeDupObj(Tcl_Obj *srcP, Tcl_Obj *dstP);
static void TGridTypeFreeRep(Tcl_Obj *objP);
static void TGridTypeUpdateStringRep(Tcl_Obj *objP);
struct Tcl_ObjType gTGridType = {
    "TGrid",
    TGridTypeFreeRep,
    TGridTypeDupObj,
    TGridTypeUpdateStringRep,
    NULL,     /* jenglish advises to keep this NULL */
};

#define TGRID_LISTPTR(optr_) (*(Tcl_Obj **) (&((optr_)->internalRep.ptrAndLongRep.ptr)))

TA_INLINE thdr_t *TGridIntRep(Tcl_Obj *gridObj)
{
    TA_ASSERT(gridObj->typePtr == &gTGridType);
    return (thdr_t *) gridObj->internalRep.ptrAndLongRep.ptr;
}

TA_INLINE void TGridSetIntRep(Tcl_Obj *gridObj, thdr_t *thdrP)
{
    TA_ASSERT(gridObj->typePtr == &gTGridType);
    TA_ASSERT(thdrP->type == TA_OBJ);

    thdr_t_INCRREF(thdrP);
    gridObj->internalRep.ptrAndLongRep.ptr = thdrP;
    gridObj->typePtr = &gTGridType;
}

#if 0
TCL_RESULT TGridVerifyType(Tcl_Interp *interp, Tcl_Obj *gridObj)
{
    TBD;
    Tcl_Obj **taObjs;
    int ntaObjs;

    if (gridObj->typePtr != &gTGridType)
        return TCL_ERROR;

    TA_ASSERT(TGRID_LISTPTR(gridObj) != NULL);

    if (Tcl_ListObjGetElements(interp, TGRID_LISTPTR(gridObj),
                               &ntaObjs, &taObjs) != TCL_OK)
        return TCL_ERROR;

    while (ntaObjs--) {
        if (tcol_convert(interp, taObjs[ntaObjs]) != TCL_OK)
            return TCL_ERROR;
    }

    return TCL_OK;
}
#endif

static void TGridTypeFreeRep(Tcl_Obj *gridObj)
{
    thdr_t *thdrP;
    TA_ASSERT(gridObj->typePtr == &gTGridType);
    thdrP = TGridIntRep(gridObj);
    thdr_t_DECRREF(thdrP);
    gridObj->internalRep.ptrAndLongRep.ptr = NULL;
    gridObj->typePtr = NULL;
}

static void TGridTypeDupObj(Tcl_Obj *srcObj, Tcl_Obj *dstObj)
{
    thdr_t *thdrP;
    TA_ASSERT(srcObj->typePtr == &gTGridType);
    thdrP = TGridIntRep(gridObj);
    TGridSetIntRep(dstObj, thdrP);
}

static void TGridTypeUpdateStringRep(Tcl_Obj *gridObj)
{
    Tcl_Obj *listObj;
    thdr_t *thdrP;
    char *p;

    TA_ASSERT(gridObj->typePtr == &gTGridType);
    /*
     * Just construct a string from the internal list and copy it.
     * Not the most optimal way but grid string representation should
     * not normally be needed.
     */
    thdrP = TGridIntRep(gridObj);
    listObj = Tcl_NewListObj(thdrP->used, thdr_tELEMPTR(thdrP, Tcl_Obj *, 0));
    p = Tcl_GetStringFromObj(listObj, &gridObj->length);
    gridObj->bytes = ckalloc(gridObj->length+1);
    memcpy(gridObj->bytes, p, gridObj->length+1);
    Tcl_DecrRefCount(listObj);
    /* TBD - should we go delete the string reps of contained elements
       if they are not shared (to save memory(
    */
}

Tcl_Obj *TGridNewObj(Tcl_Interp *interp, int ntaObjs, Tcl_Obj * const taObjs[])
{
    int i;
    Tcl_Obj *gridObj;
    Tcl_Obj *listObj;

    for (i = 0; i < ntaObjs; ++i) {
        if (tcol_convert(interp, taObjs[i]) != TCL_OK)
            return NULL;
    }

    gridObj = Tcl_NewObj();
    Tcl_InvalidateStringRep(gridObj);
    listObj = Tcl_NewListObj(ntaObjs, taObjs);
    TGridSetIntRep(gridObj, listObj);
    return gridObj;
}

Tcl_Obj *TGridClone(Tcl_Interp *interp, Tcl_Obj *gridObj, int minsize)
{
    Tcl_Obj **taObjs;
    int i, ntaObjs;
    Tcl_Obj *listObj;
    Tcl_Obj *cloneObj;

    TA_ASSERT(gridObj->typePtr == &gTGridType);

    Tcl_ListObjGetElements(interp, TGRID_LISTPTR(listObj), &ntaObjs, &taObjs);
    listObj = Tcl_NewListObj(ntaObjs, NULL);
    for (i = 0; i < ntaObjs; ++i) {
        if (tcol_convert(interp, taObjs[i]) != TCL_OK) {
            Tcl_DecrRefCount(listObj);
            return TCL_ERROR;
        }
        TA_ASSERT(taObjs[i]->typePtr == &g_ta_type);
        Tcl_ListObjAppendElement(interp, listObj,
                                 tcol_new(thdr_clone(TARRAYHDR(taObjs[i]), minsize)));
    }

    cloneObj = Tcl_NewObj();
    Tcl_InvalidateStringRep(cloneObj);
    TGridSetIntRep(cloneObj, listObj);
    
    return cloneObj;
}

TCL_RESULT TGridConvert(Tcl_Interp *interp, Tcl_Obj *objP)
{
    Tcl_Obj **elems;
    int nelems;
    Tcl_Obj *listObj = NULL;

    if (objP->typePtr == &gTGridType)
        return TCL_OK;

    /* Be careful of order here. We are changing the type of objP
       (potentially) from list to tgrid. In the conversion, the list's
       internal rep may disappear so make a copy first as opposed to
       converting the list elements and then making a new list obj
    */
    listObj = Tcl_DuplicateObj(objP);
    if (Tcl_ListObjGetElements(interp, listObj, &nelems, &elems) == TCL_OK) {
        while (nelems--) {
            if (tcol_convert(interp, elems[nelems]) != TCL_OK)
                goto error_return;
        }
        if (objP->typePtr && objP->typePtr->freeIntRepProc) {
            objP->typePtr->freeIntRepProc(objP);
            objP->typePtr = NULL;
        }
        /*
         * NOTE for future: Tcl_InvalidateStringRep must NOT be called on
         * shared objects! If we want to save memory space by freeing
         * string remember to check Tcl_IsShared first.
         */
        TGridSetIntRep(objP, listObj);
        return TCL_OK;
    }

error_return:
    if (listObj)
        Tcl_DecrRefCount(listObj);
    return TCL_ERROR;
}

/*
 * Returns a grid object that is modifiable. Modifiable means that
 * the object itself is not shared, that its TArray object elements
 * are not shared, and the TArray->thdr_t are not shared (though objects
 * inside the thdr_t may be).
 * TBD - check if this is overkill. Do we have to unshare the TArrays
 * here ? What if caller only wants to modify a subset of the TArrays ?
 *
 * Returns a sharable TGrid Tcl_Obj.
 * On error, returns NULL without gridObj being modified (though it may
 *   have shimmered)
 */
Tcl_Obj *TGridMakeWritable(Tcl_Interp *interp, Tcl_Obj *gridObj, int minsize, int prefsize, int flags)
{
    Tcl_Obj *writableObj;

    /* TBD - is this routine even needed or useful */

    if (Tcl_IsShared(gridObj))
        writableObj = TGridClone(interp, gridObj, prefsize);
    else {
        int n;
        int writable;
        int i;
        Tcl_Obj *taObj;
        thdr_t *thdrP;
        
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
            if (taObj->typePtr != &g_ta_type) {
                ta_not_tarray_error(interp);
                return NULL;
            }
            thdrP = TARRAYHDR(taObj);
            if (Tcl_IsShared(taObj) ||
                thdr_sHARED(thdrP) || thdrP->allocated < minsize)
                writable = 0;
        }
        /* Now do any require changes */
        if (! writable) {
            /* Need to replace at least one TArray Tcl_Obj in the grid */
            for (i = 0; i < n; ++i) {
                Tcl_ListObjIndex(interp, gridObj, i, &taObj);
                thdrP = TARRAYHDR(taObj);
                if (Tcl_IsShared(taObj) ||
                    thdr_sHARED(thdrP) || thdrP->allocated < minsize) {
                    /* Note we INCREF and DecrRef because ListObjReplace
                       decrements deleted obj ref counts before incr refs
                       of added objects. Here it probably does not matter
                       since TArrayMakeWritable would have allocate new
                       object, but just to future-protect */
                    taObj = TArrayMakeWritable(taObj, minsize, prefsize,
                                               TA_MAKE_WRITABLE_INCREF);
                    Tcl_ListObjReplace(interp, gridObj, i, 1, 1, &taObj);
                    Tcl_DecrRefCount(taObj); /* For INCREF above */
                }
            }
            Tcl_InvalidateStringRep(gridObj);
        }
        writableObj = gridObj;
    }
                    
    if (flags & TA_MAKE_WRITABLE_INCREF)
        Tcl_IncrRefCount(writableObj);

    return writableObj;
}

TCL_RESULT TGridFillFromObjs(
    Tcl_Interp *interp,
    Tcl_Obj *lowObj, Tcl_Obj *highObj,
    Tcl_Obj *const taObjs[], Tcl_Obj *const valueObjs[],
    int tuple_width, int flags)
{
    int i, low, count;
    thdr_t *thdr0P;
    ta_value_t values[32];
    ta_value_t *valuesP;
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

    if (lowObj->typePtr == &g_ta_type)
        lowObj = Tcl_DuplicateObj(lowObj);
    else
        Tcl_IncrRefCount(lowObj); /* Since we will release at end */

    if (highObj->typePtr == &g_ta_type)
        highObj = Tcl_DuplicateObj(highObj);
    else
        Tcl_IncrRefCount(highObj); /* Since we will release at end */

    if (tuple_width > sizeof(values)/sizeof(values[0])) {
        valuesP = (ta_value_t *) ckalloc(tuple_width * sizeof(ta_value_t));
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
        thdr_t *thdrP;
        if (tcol_convert(interp, taObjs[i]) != TCL_OK)
            goto vamoose;
        thdrP = TARRAYHDR(taObjs[i]);
        if (i == 0)
            thdr0P = TARRAYHDR(taObjs[0]);
        else if (thdrP->used != thdr0P->used) {
            Tcl_SetResult(interp, "tarrays have differing number of elements", TCL_STATIC);
            goto vamoose;
        }
        if (ta_value_from_obj(interp, valueObjs[i], thdrP->type, &valuesP[i])
            != TCL_OK)
            goto vamoose;
    }

    /* Get the limits of the range to set */
    if (ta_fix_range_bounds(interp, thdr0P, lowObj, highObj, &low, &count)
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
        new_size = low + count + TA_EXTRA(low+count);
        for (i = 0; i < tuple_width; ++i) {
            TA_ASSERT(taObjs[i]->typePtr == &g_ta_type); // Verify no shimmering
            resultObjsP[i] = TArrayMakeWritable(taObjs[i], low+count, new_size, 0);
            thdr_tFill(interp, TARRAYHDR(resultObjs[i]),
                          &valuesP[i], low, count);
        }
        
        /* Caller should not set TA_FILL_RETURN_ONE unless single tarray */
        TA_ASSERT(tuple_width == 1 || (flags & TA_FILL_SINGLE) == 0);
        if (flags & TA_FILL_SINGLE)
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
TCL_RESULT TGridSetFromObjs(
    Tcl_Interp *interp,
    Tcl_Obj *lowObj,
    Tcl_Obj *gridObj,
    Tcl_Obj *valueObjs, /* List of lists (tuple values) */
    int flags)
{
    int i, low, count, grid_width;
    thdr_t *thdr0P;
    Tcl_Obj **taObjs;
    Tcl_Obj *resultObjs[32];
    Tcl_Obj **resultObjsP;
    thdr_t *thdrs[sizeof(resultObjs)/sizeof(resultObjs[0])];
    thdr_t **thdrsP;
    int status = TCL_ERROR;
    int new_size;

    TA_ASSERT(! Tcl_IsShared(gridObj));

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

    if (lowObj->typePtr == &g_ta_type)
        lowObj = Tcl_DuplicateObj(lowObj);
    else
        Tcl_IncrRefCount(lowObj); /* Since we will release at end */

    if (grid_width > sizeof(resultObjs)/sizeof(resultObjs[0])) {
        /* Allocate room for both resultObjs and thdrs in one shot */
        resultObjsP = (Tcl_Obj **)ckalloc(grid_width * sizeof(void *));
        thdrsP = (thdr_t **)&resultObjsP[grid_width];
    }
    else {
        resultObjsP = resultObjs;
        thdrsP = thdrs;
    }        

    /* Now verify tarrays are in fact tarrays and of the same size. */
    for (i = 0; i < grid_width; ++i) {
        thdr_t *thdrP;
        if (tcol_convert(interp, taObjs[i]) != TCL_OK)
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
    if (ta_convert_index(interp, lowObj, &low, thdr0P->used) != TCL_OK)
        goto vamoose;

    if (low < 0 || low > thdr0P->used) {
        ta_index_range_error(interp, lowObj);
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

    TA_ASSERT(count > 0);
    /* If we have to realloc anyway, we will leave a bit extra room */
    new_size = count + TA_EXTRA(count);
    for (i = 0; i < grid_width; ++i) {
        TA_ASSERT(taObjs[i]->typePtr == &g_ta_type); // Verify no shimmering
        resultObjsP[i] = TArrayMakeWritable(taObjs[i], count,
                               new_size, TA_MAKE_WRITABLE_INCREF);
        thdrsP[i] = TARRAYHDR(resultObjsP[i]);
    }
        
    status = thdr_tSetMultipleFromObjs(interp, thdrsP, grid_width, valueObjs, low);

    if (status == TCL_OK) {
        /* Caller should not set TA_FILL_RETURN_ONE unless single tarray */
        TA_ASSERT(grid_width == 1 || (flags & TA_FILL_SINGLE) == 0);
        if (flags & TA_FILL_SINGLE)
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
