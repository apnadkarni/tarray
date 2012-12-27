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
 * TGrid is a Tcl "type" used for storing arrays of TArrays.
 * It is essentially stored as a Tcl_Obj list internally. We do not
 * directly use a Tcl_Obj list as a grid of TArrays to avoid repeated
 * validation that a list of the proper format is being passed to us.
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
TA_INLINE TGridSetIntRep(Tcl_Obj *gridObj, Tcl_Obj *listObj)
{
    Tcl_IncrRefCount(listObj);
    TGRID_LISTPTR(gridObj) = listObj;
    gridObj->typePtr = &gTGridType;
}

TCL_RESULT TGridVerifyType(Tcl_Interp *interp, Tcl_Obj *gridObj)
{
    Tcl_Obj **taObjs;
    int ntaObjs;

    if (gridObj->typePtr != &gTGridType)
        return TCL_ERROR;

    TARRAY_ASSERT(TGRID_LISTPTR(gridObj) != NULL);

    if (Tcl_ListObjGetElements(interp, TGRID_LISTPTR(gridObj),
                               &ntaObjs, &taObjs) != TCL_OK)
        return TCL_ERROR;

    while (ntaObjs--) {
        if (TArrayVerifyType(interp, taObjs[ntaObjs]) != TCL_OK)
            return TCL_ERROR;
    }

    return TCL_OK;
}

static void TGridTypeFreeRep(Tcl_Obj *gridObj)
{
    Tcl_Obj *listObj;

    TARRAY_ASSERT(gridObj->typePtr == &gTGridType);

    listObj = TGRID_LISTPTR(gridObj);
    if (listObj) {
        Tcl_DecrRefCount(listObj);
        TGRID_LISTPTR(gridObj) = NULL;
    }
    gridObj->typePtr = NULL;
}

static void TGridTypeDupObj(Tcl_Obj *srcObj, Tcl_Obj *dstObj)
{
    Tcl_Obj *listObj;
    TARRAY_ASSERT(srcObj->typePtr == &gTGridType);
    TARRAY_ASSERT(TGridVerifyType(NULL, srcObj) == TCL_OK);

    listObj = TGRID_LISTPTR(srcObj);
    TGridSetIntRep(dstObj, listObj);
}

static void TGridTypeUpdateStringRep(Tcl_Obj *gridObj)
{
    /* Just construct a string from the internal list and copy it */
    Tcl_Obj *listObj;
    char *p;

    TARRAY_ASSERT(gridObj->typePtr == &gTGridType);
    TARRAY_ASSERT(TGridVerifyType(NULL, gridObj) == TCL_OK);

    listObj = TGRID_LISTPTR(gridObj);
    p = Tcl_GetStringFromObj(listObj, &gridObj->length);
    gridObj->bytes = ckalloc(gridObj->length+1);
    memcpy(gridObj->bytes, p, gridObj->length+1);
}

Tcl_Obj *TGridNewObj(Tcl_Interp *interp, int ntaObjs, Tcl_Obj * const taObjs[])
{
    int i;
    Tcl_Obj *gridObj;
    Tcl_Obj *listObj;

    for (i = 0; i < ntaObjs; ++i) {
        if (TArrayConvert(interp, taObjs[i]) != TCL_OK)
            return NULL;
    }

    gridObj = Tcl_NewObj();
    Tcl_InvalidateStringRep(gridObj);
    listObj = Tcl_NewListObj(ntaObjs, taObjs);
    TGridSetIntRep(gridObj, listObj);
    return gridObj;
}
