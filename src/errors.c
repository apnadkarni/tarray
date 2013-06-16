/*
 * Copyright (c) 2012, 2013 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include "tarray.h"



/* TBD - in error and panic routines make sure strings are not too long */

void ta_string_overflow_panic(const char *where)
{
    Tcl_Panic("Max size for a Tcl value (%d bytes) exceeded in %s", INT_MAX, where ? where : "unknown function");
}

void ta_type_panic(int tatype)
{
    Tcl_Panic("Unknown or unexpected tarray type %d", tatype);
}

void ta_shared_panic(const char *where)
{
    Tcl_Panic("Shared thdr_t passed for modification to %s.", where);
}

void ta_small_panic(thdr_t *thdr, const char *where)
{
    Tcl_Panic("Insufficient space in thdr_t (allocated %d) in %s.", thdr->usable, where);
}

TCL_RESULT ta_missing_arg_error(Tcl_Interp *ip, char *optname)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Missing argument to option '%s'", optname));
        Tcl_SetErrorCode(ip, "TARRAY", "ARGUMENT", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_invalid_opt_error(Tcl_Interp *ip, char *optname)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid option '%.32s'", optname));
        Tcl_SetErrorCode(ip, "TARRAY", "OPTION", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_not_column_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip, "Object is not a TArray column", TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "TCLOBJTYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_not_table_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip, "Object is not a TArray table", TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "TCLOBJTYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_bad_type_error(Tcl_Interp *ip, thdr_t *thdr)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("tarray is of the wrong type (%s)",
                                       ta_type_string(thdr->type)));
        Tcl_SetErrorCode(ip, "TARRAY", "TYPE", NULL);
    }
    return TCL_ERROR;
}


TCL_RESULT ta_index_range_error(Tcl_Interp *ip, int index)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("tarray index %d out of bounds", index));
        Tcl_SetErrorCode(ip, "TARRAY", "INDEX", "RANGE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_bad_count_error(Tcl_Interp *ip, int count)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Invalid count %d", count));
        Tcl_SetErrorCode(ip, "TARRAY", "COUNT", "RANGE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_value_type_error(Tcl_Interp *ip, Tcl_Obj *o, int tatype)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Value '%.40s' not valid for type %s.",
                                       Tcl_GetString(o),
                                       ta_type_string(tatype)));
        Tcl_SetErrorCode(ip, "TARRAY", "VALUE", "TYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_row_width_error(Tcl_Interp *ip, int row_width, int min_width)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Row or table width %d less than destination width %d.", row_width, min_width));
        Tcl_SetErrorCode(ip, "TARRAY", "ROW", "WIDTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_table_length_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip,
                      "Columns in tarray table have differing lengths.",
                      TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "GRID", "LENGTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_memory_error(Tcl_Interp *ip, int req_size)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Memory allocation failed (%d bytes).",
                                       req_size));
        Tcl_SetErrorCode(ip, "TARRAY", "NOMEM", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_indices_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid index list '%s'. Must be an integer, or a list or typed array of type int.", Tcl_GetString(o)));
        Tcl_SetErrorCode(ip, "TARRAY", "VALUE", "INDEXLIST", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_index_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid index '%s'. Must be an integer or the keyword 'end'.", Tcl_GetString(o)));
        Tcl_SetErrorCode(ip, "TARRAY", "VALUE", "INDEX", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_mismatched_types_error(Tcl_Interp *ip, int typea, int typeb)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("tarray types %s and %s are not compatible for attempted operation", ta_type_string(typea), ta_type_string(typeb)));
        Tcl_SetErrorCode(ip, "TARRAY", "TYPE", "INCOMPATIBLE", NULL);
    }

    return TCL_ERROR;
}

TCL_RESULT ta_indices_count_error(Tcl_Interp *ip, int nindices, int nvalues)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Number of indices (%d) not same as number of values (%d).", nindices, nvalues));
        Tcl_SetErrorCode(ip, "TARRAY", "INDICES", "COUNT", NULL);
    }

    return TCL_ERROR;
}

TCL_RESULT ta_invalid_range_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Invalid index range limit '%.80s'.",
                                       Tcl_GetString(o)));
        Tcl_SetErrorCode(ip, "TARRAY", "RANGE", "VALUE", NULL);
    }
    return TCL_ERROR;
}


TCL_RESULT ta_column_name_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("No column with name '%.80s'.",
                                       Tcl_GetString(o)));
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "COLUMN", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_duplicate_columns_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Duplicate columns specified in column list '%.80s'.", Tcl_GetString(o)));
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "COLUMN", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_column_index_error(Tcl_Interp *ip, int colindex)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Column index '%d' out of bounds.", colindex));
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "COLUMN", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_multiple_columns_error(Tcl_Interp *ip, int colindex)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Column index '%d' specified multiple times in column list.", colindex));
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "COLUMN", NULL);
    }
    return TCL_ERROR;
}
