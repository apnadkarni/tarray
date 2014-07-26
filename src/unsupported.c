/*
 * Copyright (c) 2012-2013, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file license.terms for license
 */

#include "tarray.h"

TCL_RESULT ta_dump_cmd(ClientData clientdata, Tcl_Interp *ip,
                       int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *o;
    int i;
    char buf[2*TCL_INTEGER_SPACE+6];
    Tcl_Obj *oresult[22];

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
    oresult[i++] = Tcl_NewIntObj(o->refCount);

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

    if (o->typePtr == &ta_table_type) {
        oresult[i++] = Tcl_NewStringObj("columnames", -1);
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
        oresult[i++] = Tcl_NewIntObj(thdr->nrefs);
        oresult[i++] = Tcl_NewStringObj("thdr.usable",-1);
        oresult[i++] = Tcl_NewIntObj(thdr->usable);
        oresult[i++] = Tcl_NewStringObj("thdr.used",-1);
        oresult[i++] = Tcl_NewIntObj(thdr->used);
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
