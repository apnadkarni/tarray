/*
 * Copyright (c) 2018, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file license.terms for license
 */

#include "tarray.h"
#define USE_RBC_STUBS 1
#include "rbcDecls.h"

static TCL_RESULT ta_rbc_vector_get_cmd(
    void *cdata,
    Tcl_Interp *ip,
    int objc,
    Tcl_Obj *const objv[]
) {
    Rbc_Vector *rbcV;
    double *toP, *fromP;
    int first, end, len;
    TCL_RESULT res;
    thdr_t *thdrP;

    if (objc < 2 || objc > 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "RBCVECTOR ?FIRST ?LAST??");
        return TCL_ERROR;
    }

    res = Rbc_GetVector(ip, Tcl_GetString(objv[1]), &rbcV);
    if (res != TCL_OK)
        return TCL_ERROR; /* Attempt to read a vector that does not exist */

    fromP = Rbc_VectorData(rbcV);
    len = Rbc_VectorLength(rbcV);

    first = 0;
    end = len;
    if (objc > 2) {
        res = Tcl_GetIntFromObj(ip, objv[2], &first);
        if (res != TCL_OK)
            return res;
        if (objc > 3) {
            res = Tcl_GetIntFromObj(ip, objv[3], &end);
            if (res != TCL_OK)
                return res;
        }
    }

    if (end < 0)
        end = 0;
    else if (end > len)
        end = len;
    if (first < 0)
        first = 0;
    else if (first > end)
        first = end;
        
    thdrP = thdr_alloc(ip, TA_DOUBLE, end-first);
    if (thdrP == NULL)
        return TCL_ERROR;

    toP = THDRELEMPTR(thdrP, double, first);
    memcpy(toP, fromP, (end-first)*sizeof(double));
    thdrP->used = end-first;
    TA_ASSERT(thdrP->used <= thdrP->usable);
    
    Tcl_SetObjResult(ip, tcol_new(thdrP));
    return TCL_OK;
}

static TCL_RESULT ta_rbc_vector_set_cmd(
    void *cdata,
    Tcl_Interp *ip,
    int objc,
    Tcl_Obj *const objv[]
) {
    Rbc_Vector *rbcV;
    double *fromP;
    int count;
    TCL_RESULT res;
    thdr_t *thdrP;
    span_t *spanP;
    char *vname;
    int free_on_error;

    if (objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "RBCVECTOR TARRAY");
        return TCL_ERROR;
    }

    /* Note: extract tcol last in case shared object with objv[1] etc.
       to avoid shimmering issues
    */
    res = tcol_convert(ip, objv[2]);
    if (res != TCL_OK)
        return res;

    thdrP = OBJTHDR(objv[2]);
    if (thdrP->type != TA_DOUBLE)
        return ta_bad_type_error(ip, thdrP);

    spanP = tcol_span(objv[2]);
    if (spanP) {
        fromP = THDRELEMPTR(thdrP, double, spanP->first);
        count = spanP->count;
    } else {
        fromP = THDRELEMPTR(thdrP, double, 0);
        count = thdrP->used;
    }

    vname = Tcl_GetString(objv[1]);
    if (Rbc_VectorExists2(ip, vname)) {
        res = Rbc_GetVector(ip, vname, &rbcV);
        free_on_error = 0;
    }
    else {
        res = Rbc_CreateVector(ip, vname, 0, &rbcV);
        free_on_error = 1;
    }

    if (res != TCL_OK)
        return TCL_ERROR;

    res = Rbc_ResetVector(rbcV, fromP, count, count, TCL_VOLATILE);
    if (res != TCL_OK) {
        if (free_on_error)
            Rbc_FreeVector(rbcV);
        return res;
    }

    Tcl_SetObjResult(ip, objv[1]);
    return TCL_OK;
}


TCL_RESULT ta_rbc_init_cmd(ClientData clientdata, Tcl_Interp *ip,
                           int objc, Tcl_Obj *const objv[])
{
    if (objc != 1) {
	Tcl_WrongNumArgs(ip, 1, objv, "");
	return TCL_ERROR;
    }

    if (!Rbc_InitStubs(ip, "0.1", 0)) {
        Tcl_SetResult(ip, "Failed to init rbc stubs.", TCL_STATIC);
        return TCL_ERROR;
    }

    Tcl_CreateObjCommand(ip, "tarray::rbc::vector_get", ta_rbc_vector_get_cmd, NULL, NULL);
    Tcl_CreateObjCommand(ip, "tarray::rbc::vector_set", ta_rbc_vector_set_cmd, NULL, NULL);

    return TCL_OK;
}
