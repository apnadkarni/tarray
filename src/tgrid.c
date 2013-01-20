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
static void TGridTypeDupObj(Tcl_Obj *psrc, Tcl_Obj *pdst);
static void TGridTypeFreeRep(Tcl_Obj *o);
static void TGridTypeUpdateStringRep(Tcl_Obj *o);
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

TA_INLINE void TGridSetIntRep(Tcl_Obj *gridObj, thdr_t *thdr)
{
    TA_ASSERT(gridObj->typePtr == &gTGridType);
    TA_ASSERT(thdr->type == TA_OBJ);

    thdr_incr_refs(thdr);
    gridObj->internalRep.ptrAndLongRep.ptr = thdr;
    gridObj->typePtr = &gTGridType;
}

#if 0
TCL_RESULT TGridVerifyType(Tcl_Interp *ip, Tcl_Obj *gridObj)
{
    TBD;
    Tcl_Obj **tcols;
    int ntcols;

    if (gridObj->typePtr != &gTGridType)
        return TCL_ERROR;

    TA_ASSERT(TGRID_LISTPTR(gridObj) != NULL);

    if (Tcl_ListObjGetElements(ip, TGRID_LISTPTR(gridObj),
                               &ntcols, &tcols) != TCL_OK)
        return TCL_ERROR;

    while (ntcols--) {
        if (tcol_convert(ip, tcols[ntcols]) != TCL_OK)
            return TCL_ERROR;
    }

    return TCL_OK;
}
#endif

static void TGridTypeFreeRep(Tcl_Obj *gridObj)
{
    thdr_t *thdr;
    TA_ASSERT(gridObj->typePtr == &gTGridType);
    thdr = TGridIntRep(gridObj);
    thdr_decr_refs(thdr);
    gridObj->internalRep.ptrAndLongRep.ptr = NULL;
    gridObj->typePtr = NULL;
}

static void TGridTypeDupObj(Tcl_Obj *srcObj, Tcl_Obj *dstObj)
{
    thdr_t *thdr;
    TA_ASSERT(srcObj->typePtr == &gTGridType);
    thdr = TGridIntRep(gridObj);
    TGridSetIntRep(dstObj, thdr);
}

static void TGridTypeUpdateStringRep(Tcl_Obj *gridObj)
{
    Tcl_Obj *listObj;
    thdr_t *thdr;
    char *p;

    TA_ASSERT(gridObj->typePtr == &gTGridType);
    /*
     * Just construct a string from the internal list and copy it.
     * Not the most optimal way but grid string representation should
     * not normally be needed.
     */
    thdr = TGridIntRep(gridObj);
    listObj = Tcl_NewListObj(thdr->used, THDRELEMPTR(thdr, Tcl_Obj *, 0));
    p = Tcl_GetStringFromObj(listObj, &gridObj->length);
    gridObj->bytes = ckalloc(gridObj->length+1);
    memcpy(gridObj->bytes, p, gridObj->length+1);
    Tcl_DecrRefCount(listObj);
    /* TBD - should we go delete the string reps of contained elements
       if they are not shared (to save memory(
    */
}

Tcl_Obj *TGridNewObj(Tcl_Interp *ip, int ntcols, Tcl_Obj * const tcols[])
{
    int i;
    Tcl_Obj *gridObj;
    Tcl_Obj *listObj;

    for (i = 0; i < ntcols; ++i) {
        if (tcol_convert(ip, tcols[i]) != TCL_OK)
            return NULL;
    }

    gridObj = Tcl_NewObj();
    Tcl_InvalidateStringRep(gridObj);
    listObj = Tcl_NewListObj(ntcols, tcols);
    TGridSetIntRep(gridObj, listObj);
    return gridObj;
}

Tcl_Obj *TGridClone(Tcl_Interp *ip, Tcl_Obj *gridObj, int minsize)
{
    Tcl_Obj **tcols;
    int i, ntcols;
    Tcl_Obj *listObj;
    Tcl_Obj *cloneObj;

    TA_ASSERT(gridObj->typePtr == &gTGridType);

    Tcl_ListObjGetElements(ip, TGRID_LISTPTR(listObj), &ntcols, &tcols);
    listObj = Tcl_NewListObj(ntcols, NULL);
    for (i = 0; i < ntcols; ++i) {
        if (tcol_convert(ip, tcols[i]) != TCL_OK) {
            Tcl_DecrRefCount(listObj);
            return TCL_ERROR;
        }
        TA_ASSERT(tcols[i]->typePtr == &g_tcol_type);
        Tcl_ListObjAppendElement(ip, listObj,
                                 tcol_new(thdr_clone(TARRAYHDR(tcols[i]), minsize)));
    }

    cloneObj = Tcl_NewObj();
    Tcl_InvalidateStringRep(cloneObj);
    TGridSetIntRep(cloneObj, listObj);
    
    return cloneObj;
}

TCL_RESULT TGridConvert(Tcl_Interp *ip, Tcl_Obj *o)
{
    Tcl_Obj **elems;
    int nelems;
    Tcl_Obj *listObj = NULL;

    if (o->typePtr == &gTGridType)
        return TCL_OK;

    /* Be careful of order here. We are changing the type of o
       (potentially) from list to tgrid. In the conversion, the list's
       internal rep may disappear so make a copy first as opposed to
       converting the list elements and then making a new list obj
    */
    listObj = Tcl_DuplicateObj(o);
    if (Tcl_ListObjGetElements(ip, listObj, &nelems, &elems) == TCL_OK) {
        while (nelems--) {
            if (tcol_convert(ip, elems[nelems]) != TCL_OK)
                goto error_return;
        }
        if (o->typePtr && o->typePtr->freeIntRepProc) {
            o->typePtr->freeIntRepProc(o);
            o->typePtr = NULL;
        }
        /*
         * NOTE for future: Tcl_InvalidateStringRep must NOT be called on
         * shared objects! If we want to save memory space by freeing
         * string remember to check Tcl_IsShared first.
         */
        TGridSetIntRep(o, listObj);
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
Tcl_Obj *TGridMakeWritable(Tcl_Interp *ip, Tcl_Obj *gridObj, int minsize, int prefsize, int flags)
{
    Tcl_Obj *writableObj;

    /* TBD - is this routine even needed or useful */

    if (Tcl_IsShared(gridObj))
        writableObj = TGridClone(ip, gridObj, prefsize);
    else {
        int n;
        int writable;
        int i;
        Tcl_Obj *tcol;
        thdr_t *thdr;
        
        /* Even if the grid Tcl_Obj is not shared, the contained
         * TArray Tcl_Objs may be. Note we do not use Tcl_ListObjGetElements
         * and loop through the array because the underlying memory
         * storage *may* change when we replace the elements with
         * unshared ones.
         */
        if (Tcl_ListObjLength(ip, gridObj, &n) != TCL_OK)
            return NULL;
        /* First verify all elements are TArrays before doing anything */
        for (writable = 1, i = 0; i < n; ++i) {
            if (Tcl_ListObjIndex(ip, gridObj, i, &tcol)  != TCL_OK)
                return NULL;
            if (tcol->typePtr != &g_tcol_type) {
                ta_not_tarray_error(ip);
                return NULL;
            }
            thdr = TARRAYHDR(tcol);
            if (Tcl_IsShared(tcol) ||
                thdr_shared(thdr) || thdr->usable < minsize)
                writable = 0;
        }
        /* Now do any require changes */
        if (! writable) {
            /* Need to replace at least one TArray Tcl_Obj in the grid */
            for (i = 0; i < n; ++i) {
                Tcl_ListObjIndex(ip, gridObj, i, &tcol);
                thdr = TARRAYHDR(tcol);
                if (Tcl_IsShared(tcol) ||
                    thdr_shared(thdr) || thdr->usable < minsize) {
                    /* Note we INCREF and DecrRef because ListObjReplace
                       decrements deleted obj ref counts before incr refs
                       of added objects. Here it probably does not matter
                       since TArrayMakeWritable would have allocate new
                       object, but just to future-protect */
                    tcol = TArrayMakeWritable(tcol, minsize, prefsize,
                                               TA_MAKE_WRITABLE_INCREF);
                    Tcl_ListObjReplace(ip, gridObj, i, 1, 1, &tcol);
                    Tcl_DecrRefCount(tcol); /* For INCREF above */
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
    Tcl_Interp *ip,
    Tcl_Obj *olow, Tcl_Obj *ohigh,
    Tcl_Obj *const tcols[], Tcl_Obj *const ovalues[],
    int tuple_width, int flags)
{
    int i, low, count;
    thdr_t *thdr0P;
    ta_value_t values[32];
    ta_value_t *pvalues;
    Tcl_Obj *oresults[sizeof(values)/sizeof(values[0])];
    Tcl_Obj **oresultsP;
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

    if (olow->typePtr == &g_tcol_type)
        olow = Tcl_DuplicateObj(olow);
    else
        Tcl_IncrRefCount(olow); /* Since we will release at end */

    if (ohigh->typePtr == &g_tcol_type)
        ohigh = Tcl_DuplicateObj(ohigh);
    else
        Tcl_IncrRefCount(ohigh); /* Since we will release at end */

    if (tuple_width > sizeof(values)/sizeof(values[0])) {
        pvalues = (ta_value_t *) ckalloc(tuple_width * sizeof(ta_value_t));
        oresultsP = (Tcl_Obj **)ckalloc(tuple_width * sizeof(Tcl_Obj *));
    } else {
        pvalues = values;
        oresultsP = oresults;
    }
        
    /*
     * Now verify tarrays and values. The latter should be of the
     * appropriate type. Also ensure all tarrays are the same size.
     */
    for (i = 0; i < tuple_width; ++i) {
        thdr_t *thdr;
        if (tcol_convert(ip, tcols[i]) != TCL_OK)
            goto vamoose;
        thdr = TARRAYHDR(tcols[i]);
        if (i == 0)
            thdr0P = TARRAYHDR(tcols[0]);
        else if (thdr->used != thdr0P->used) {
            Tcl_SetResult(ip, "tarrays have differing number of elements", TCL_STATIC);
            goto vamoose;
        }
        if (ta_value_from_obj(ip, ovalues[i], thdr->type, &pvalues[i])
            != TCL_OK)
            goto vamoose;
    }

    /* Get the limits of the range to set */
    if (ta_fix_range_bounds(ip, thdr0P, olow, ohigh, &low, &count)
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
     * (1) If tcol[i] is shared, then we cannot modify in place since
     *     the object is referenced elsewhere in the program. We have to
     *     clone the corresponding thdrsP[i] and stick it in a new
     *     Tcl_Obj.
     * (2) If tcol[i] is unshared, the corresponding thdr might
     *     still be shared (pointed to from elsewhere). In this case
     *     also, we clone the thdr but instead of allocating a new 
     *     Tcl_Obj, we store it as the internal rep of tcol[i].
     * (3) If tcol[i] and its thdr are unshared, (a) we can modify in
     *     place (b) unless thdr is too small. In that case we have
     *     to follow the same path as (2).
     *
     * NOTE: tcolsP points into memory owned by objv[3] list. We cannot
     * write to it, hence we use a separate output area oresultsP[].
     */

    /* If nothing to set, return existing tuple array as is */
    if (count == 0)
        Tcl_SetObjResult(ip, Tcl_NewListObj(tuple_width, tcols));
    else {
        /* If we have to realloc anyway, we will leave a bit extra room */
        new_size = low + count + TA_EXTRA(low+count);
        for (i = 0; i < tuple_width; ++i) {
            TA_ASSERT(tcols[i]->typePtr == &g_tcol_type); // Verify no shimmering
            oresultsP[i] = TArrayMakeWritable(tcols[i], low+count, new_size, 0);
            thdr_tFill(ip, TARRAYHDR(oresults[i]),
                          &pvalues[i], low, count);
        }
        
        /* Caller should not set TA_FILL_RETURN_ONE unless single tarray */
        TA_ASSERT(tuple_width == 1 || (flags & TA_FILL_SINGLE) == 0);
        if (flags & TA_FILL_SINGLE)
            Tcl_SetObjResult(ip, oresultsP[0]);
        else
            Tcl_SetObjResult(ip, Tcl_NewListObj(tuple_width, oresultsP));
    }
    status = TCL_OK;
    
vamoose:                   /* ip must already hold error message */
    Tcl_DecrRefCount(olow);
    Tcl_DecrRefCount(ohigh);

    if (oresultsP != oresults)
        ckfree((char *) oresultsP);
    if (pvalues != values)
        ckfree((char *) pvalues);

    return status;
}

/*
 * See asserts in code for entry conditions.
 * On success, gridObj (which must NOT be shared) is modified with the new
 * values. On error, gridObj is left unchanged.
 * ip is used only for errors,
 */
TCL_RESULT TGridSetFromObjs(
    Tcl_Interp *ip,
    Tcl_Obj *olow,
    Tcl_Obj *gridObj,
    Tcl_Obj *ovalues, /* List of lists (tuple values) */
    int flags)
{
    int i, low, count, grid_width;
    thdr_t *thdr0P;
    Tcl_Obj **tcols;
    Tcl_Obj *oresults[32];
    Tcl_Obj **oresultsP;
    thdr_t *thdrs[sizeof(oresults)/sizeof(oresults[0])];
    thdr_t **thdrsP;
    int status = TCL_ERROR;
    int new_size;

    TA_ASSERT(! Tcl_IsShared(gridObj));

    if (Tcl_ListObjGetElements(ip, gridObj, &grid_width, &tcols) != TCL_OK
        || Tcl_ListObjLength(ip, ovalues, &count) != TCL_OK)
        return TCL_ERROR;

    /* Check for empty tuple or no new values so as to simplify loops below */
    if (grid_width == 0 || count == 0) {
        Tcl_SetObjResult(ip, gridObj);
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

    if (olow->typePtr == &g_tcol_type)
        olow = Tcl_DuplicateObj(olow);
    else
        Tcl_IncrRefCount(olow); /* Since we will release at end */

    if (grid_width > sizeof(oresults)/sizeof(oresults[0])) {
        /* Allocate room for both oresults and thdrs in one shot */
        oresultsP = (Tcl_Obj **)ckalloc(grid_width * sizeof(void *));
        thdrsP = (thdr_t **)&oresultsP[grid_width];
    }
    else {
        oresultsP = oresults;
        thdrsP = thdrs;
    }        

    /* Now verify tarrays are in fact tarrays and of the same size. */
    for (i = 0; i < grid_width; ++i) {
        thdr_t *thdr;
        if (tcol_convert(ip, tcols[i]) != TCL_OK)
            goto vamoose;
        thdr = TARRAYHDR(tcols[i]);
        if (i == 0)
            thdr0P = TARRAYHDR(tcols[0]);
        else if (thdr->used != thdr0P->used) {
            Tcl_SetResult(ip, "tarrays have differing number of elements", TCL_STATIC);
            goto vamoose;
        }
    }

    /* Get the start of the range to set */
    if (ta_convert_index(ip, olow, &low, thdr0P->used) != TCL_OK)
        goto vamoose;

    if (low < 0 || low > thdr0P->used) {
        ta_index_range_error(ip, olow);
        goto vamoose;
    }

    count += low;               /* Needed size of array */

    /*
     * With respect to where to store the results,
     * there are three cases to consider for each tarray.
     * (1) If tcol[i] is shared, then we cannot modify in place since
     *     the object is referenced elsewhere in the program. We have to
     *     clone the corresponding thdrsP[i] and stick it in a new
     *     Tcl_Obj.
     * (2) If tcol[i] is unshared, the corresponding thdr might
     *     still be shared (pointed to from elsewhere). In this case
     *     also, we clone the thdr but instead of allocating a new 
     *     Tcl_Obj, we store it as the internal rep of tcol[i].
     * (3) If tcol[i] and its thdr are unshared, (a) we can modify in
     *     place (b) unless thdr is too small. In that case we have
     *     to follow the same path as (2).
     *
     * NOTE: tcolsP points into memory owned by objv[3] list. We cannot
     * write to it, hence we use a separate output area oresultsP[].
     */

    TA_ASSERT(count > 0);
    /* If we have to realloc anyway, we will leave a bit extra room */
    new_size = count + TA_EXTRA(count);
    for (i = 0; i < grid_width; ++i) {
        TA_ASSERT(tcols[i]->typePtr == &g_tcol_type); // Verify no shimmering
        oresultsP[i] = TArrayMakeWritable(tcols[i], count,
                               new_size, TA_MAKE_WRITABLE_INCREF);
        thdrsP[i] = TARRAYHDR(oresultsP[i]);
    }
        
    status = thdr_tSetMultipleFromObjs(ip, thdrsP, grid_width, ovalues, low);

    if (status == TCL_OK) {
        /* Caller should not set TA_FILL_RETURN_ONE unless single tarray */
        TA_ASSERT(grid_width == 1 || (flags & TA_FILL_SINGLE) == 0);
        if (flags & TA_FILL_SINGLE)
            Tcl_SetObjResult(ip, oresultsP[0]);
        else
            Tcl_SetObjResult(ip, Tcl_NewListObj(grid_width, oresultsP));
    }

    for (i=0; i < grid_width; ++i)
        Tcl_DecrRefCount(oresultsP[i]); /* Remove ref added by MakeWritable */

vamoose:                   /* ip must already hold error message */
    Tcl_DecrRefCount(olow);

    if (oresultsP != oresults)
        ckfree((char *) oresultsP);

    return status;
}
