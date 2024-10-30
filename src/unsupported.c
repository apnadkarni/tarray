/*
 * Copyright (c) 2012-2024, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file license.terms for license
 */

#include "tarray.h"

TCL_RESULT
ta_dump_cmd(ClientData clientdata, Tcl_Interp *ip,
                       int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *o;
    int i;
    char buf[2*TCL_INTEGER_SPACE+6];
    Tcl_Obj *oresult[30];

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "value");
        return TCL_ERROR;
    }

    o = objv[1];
    i = 0;
    oresult[i++] = Tcl_NewStringObj("Tcl_Obj*", -1);
    sprintf(buf, "%p", (void *) o);
    oresult[i++] = Tcl_NewStringObj(buf, -1);

    oresult[i++] = Tcl_NewStringObj("Tcl_Obj.refCount", -1);
    oresult[i++] = Tcl_NewWideIntObj(o->refCount);

    oresult[i++] = Tcl_NewStringObj("Tcl_Obj.type", -1);
    oresult[i++] = Tcl_NewStringObj(
        o->typePtr ? o->typePtr->name : "",
        -1);

    oresult[i++] = Tcl_NewStringObj("Tcl_Obj.bytes", -1);
    if (o->bytes) {
        oresult[i] = Tcl_NewStringObj("", -1);
	Tcl_AppendLimitedToObj(oresult[i], o->bytes, o->length, 80, "...");
        ++i;
    } else
        oresult[i++] = Tcl_NewObj();

    if (o->typePtr == &ta_column_type) {
        span_t *span = tcol_span(o);
        oresult[i++] = Tcl_NewStringObj("span*", -1);
        if (span == NULL) {
            /* Because gcc and vc++ differ in output for NULL %p */
            oresult[i++] = Tcl_NewIntObj(0);
        } else {
            sprintf(buf, "%p", (void *) span);
            oresult[i++] = Tcl_NewStringObj(buf, -1);
        }
        if (span) {
            oresult[i++] = Tcl_NewStringObj("span.nrefs", -1);
            oresult[i++] = Tcl_NewWideIntObj(span->nrefs);
            oresult[i++] = Tcl_NewStringObj("span.first",-1);
            oresult[i++] = Tcl_NewWideIntObj(span->first);
            oresult[i++] = Tcl_NewStringObj("span.count",-1);
            oresult[i++] = Tcl_NewWideIntObj(span->count);
        }
    } else if (o->typePtr == &ta_table_type) {
        oresult[i++] = Tcl_NewStringObj("columnnames", -1);
        oresult[i++] = table_column_names(o);
    }

    if (o->typePtr == &ta_column_type || o->typePtr == &ta_table_type) {
        thdr_t *thdr;
        char *s;
        oresult[i++] = Tcl_NewStringObj("thdr*", -1);
        thdr = o->typePtr == &ta_column_type ? tcol_thdr(o) : table_thdr(o);
        sprintf(buf, "%p", (void *) thdr);
        oresult[i++] = Tcl_NewStringObj(buf, -1);
        oresult[i++] = Tcl_NewStringObj("thdr.nrefs", -1);
        oresult[i++] = Tcl_NewWideIntObj(thdr->nrefs);
        oresult[i++] = Tcl_NewStringObj("thdr.usable",-1);
        oresult[i++] = Tcl_NewWideIntObj(thdr->usable);
        oresult[i++] = Tcl_NewStringObj("thdr.used",-1);
        oresult[i++] = Tcl_NewWideIntObj(thdr->used);
        oresult[i++] = Tcl_NewStringObj("thdr.type",-1);
        oresult[i++] = Tcl_NewStringObj(ta_type_string(thdr->type), -1);
        oresult[i++] = Tcl_NewStringObj("thdr.sort_order",-1);
        switch (thdr->sort_order) {
        case THDR_UNSORTED: s = "UNSORTED"; break;
        case THDR_SORTED_ASCENDING: s = "ASCENDING"; break;
        case THDR_SORTED_DESCENDING: s = "DESCENDING"; break;
        case THDR_SORTED_ASCENDING_NOCASE: s = "ASCENDING_NOCASE"; break;
        case THDR_SORTED_DESCENDING_NOCASE: s = "DESCENDING_NOCASE"; break;
        default:
            sprintf(buf, "%d", thdr->sort_order);
            s = buf;
            break;
        }
        oresult[i++] = Tcl_NewStringObj(s, -1);
    }

    TA_ASSERT(i <= (sizeof(oresult)/sizeof(oresult[0])));

    Tcl_SetObjResult(ip, Tcl_NewListObj(i, oresult));
    return TCL_OK;
}

TCL_RESULT
ta_compiler_info_cmd(ClientData clientdata, Tcl_Interp *ip,
                     int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *objs[6];

    if (objc != 1) {
        Tcl_WrongNumArgs(ip, 1, objv, NULL);
	return TCL_ERROR;
    }

    objs[0] = Tcl_NewStringObj("compiler", -1);
#if defined(_MSC_VER)
    objs[1] = Tcl_ObjPrintf("vc++ %u", _MSC_VER);
#elif defined(__GNUC__)
    objs[1] = Tcl_NewStringObj("gcc" __VERSION__, -1);
#else
    objs[1] = Tcl_NewStringObj("unknown", -1);
#endif

    objs[2] = Tcl_NewStringObj("opts", -1);

    objs[3] = Tcl_NewListObj(0, NULL);
#ifdef TA_MT_ENABLE
    Tcl_ListObjAppendElement(NULL, objs[3], Tcl_NewStringObj("mt_enabled", -1));
#endif
#if TA_ENABLE_ASSERT
    Tcl_ListObjAppendElement(NULL, objs[3], Tcl_NewStringObj("asserts_enabled", -1));
#endif

    /* Which Tcl did we build against ? (As opposed to run time) */
    objs[4] = Tcl_NewStringObj("tcl_headers", -1);
    objs[5] = Tcl_NewStringObj(TCL_PATCH_LEVEL, -1);

    Tcl_SetObjResult(ip, Tcl_NewListObj(6, objs));
    return TCL_OK;
}

TCL_RESULT
ta_mt_split_cmd(ClientData clientdata, Tcl_Interp *ip,
                     int objc, Tcl_Obj *const objv[])
{
    if (objc != 6) {
        Tcl_WrongNumArgs(ip, 1, objv, "TATYPE FIRST COUNT MIN_HINT NSIZES");
        return TCL_ERROR;
    }

    Tcl_Size first, count, min_hint, nsizes;
    int tatype;
    if (Tcl_GetIntFromObj(ip, objv[1], &tatype) != TCL_OK ||
        Tcl_GetSizeIntFromObj(ip, objv[2], &first) != TCL_OK ||
        Tcl_GetSizeIntFromObj(ip, objv[3], &count) != TCL_OK ||
        Tcl_GetSizeIntFromObj(ip, objv[4], &min_hint) != TCL_OK ||
        Tcl_GetSizeIntFromObj(ip, objv[5], &nsizes) != TCL_OK) {
        return TCL_ERROR;
    }

    switch (tatype) {
    case TA_STRING:
    case TA_ANY:
    case TA_INT:
    case TA_UINT:
    case TA_DOUBLE:
    case TA_WIDE:
    case TA_BYTE:
        break;
    default:
        Tcl_SetResult(ip, "Invalid type for multithreading", TCL_STATIC);
        return TCL_ERROR;
    }

#ifdef TA_MT_ENABLE
    Tcl_Size sizes[16];
    Tcl_Obj *ores;
    Tcl_Size i, split_count;
    if (nsizes > ARRAYSIZE(sizes)) {
        Tcl_SetResult(ip, "Invalid array size", TCL_STATIC);
        return TCL_ERROR;
    }
    split_count = thdr_calc_mt_split_ex(tatype, first, count, min_hint, nsizes, sizes);
    ores = Tcl_NewListObj(nsizes, NULL);
    for (i = 0; i < split_count; ++i)
        Tcl_ListObjAppendElement(ip, ores, Tcl_NewWideIntObj(sizes[i]));
    Tcl_SetObjResult(ip, ores);
    return TCL_OK;
#else
    Tcl_SetResult(ip, "Multithreading not enabled", TCL_STATIC);
    return TCL_ERROR;
#endif
}

TCL_RESULT
ta_config_cmd(ClientData clientdata, Tcl_Interp *ip,
                       int objc, Tcl_Obj *const objv[])
{
    const char *vname;
    Tcl_WideInt wide;
    int *pint = 0;
    Tcl_Size *psize = 0;
    if (objc < 2 || objc > 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "CONFIGVAR ?VALUE?");
        return TCL_ERROR;
    }
    if (objc == 3) {
        if (Tcl_GetWideIntFromObj(ip, objv[2], &wide) != TCL_OK)
        return TCL_ERROR;
    }

    vname = Tcl_GetString(objv[1]);
    if (ta_strequal(vname, "experiment"))
        pint = &ta_experiment;
    else if (ta_strequal(vname, "full_validation"))
        pint = &ta_full_validation;
#ifdef TA_MT_ENABLE
    else if (ta_strequal(vname, "sort_mt_threshold"))
        psize = &ta_sort_mt_threshold;
    else if (ta_strequal(vname, "sort_mt_enable_any"))
        pint = &ta_sort_mt_enable_any;
    else if (ta_strequal(vname, "search_mt_threshold"))
        psize = &ta_search_mt_threshold;
    else if (ta_strequal(vname, "fill_mt_threshold"))
        psize = &ta_fill_mt_threshold;
    else if (ta_strequal(vname, "minmax_mt_threshold"))
        psize = &ta_minmax_mt_threshold;
    else if (ta_strequal(vname, "fold_mt_threshold"))
        psize = &ta_fold_mt_threshold;
    else if (ta_strequal(vname, "math_mt_threshold"))
        psize = &ta_math_mt_threshold;
#endif
    else {
        Tcl_SetResult(ip, "Invalid config setting name.", TCL_STATIC);
        return TCL_ERROR;
    }

    TA_ASSERT(pint || psize);
    if (pint) {
        if (objc==3)
            *pint = (int)wide;
        wide = *pint;
    }
    else {
        if (objc==3)
            *psize = (Tcl_Size)wide;
        wide = *psize;

    }

    Tcl_SetObjResult(ip, Tcl_NewWideIntObj(wide));
    return TCL_OK;

}
