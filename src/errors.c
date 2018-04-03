/*
 * Copyright (c) 2012-2015 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include "tarray.h"



/* TBD - in error and panic routines make sure strings are not too long */

static TCL_RESULT error_from_obj(Tcl_Interp *ip, char *s, Tcl_Obj *o)
{
    if (ip) {
        Tcl_Obj *eobj;
        /* Take care not to generate a string if one does not exist */
        /* TBD - maybe only do this based on the object type ?*/
        if (o && o->bytes)
            eobj = Tcl_ObjPrintf("%s '%.40s'.", s, o->bytes);
        else
            eobj = Tcl_ObjPrintf("%s.", s);
        Tcl_SetObjResult(ip, eobj);
    }
    return TCL_ERROR;
}

void ta_string_overflow_panic(const char *where)
{
    Tcl_Panic("Max size for a Tcl value (%d bytes) exceeded in %s", INT_MAX, where ? where : "unknown function");
}

void ta_type_panic(int tatype)
{
    Tcl_Panic("Unknown or unexpected tarray type %d", tatype);
}

void ta_operator_panic(int op)
{
    Tcl_Panic("Unknown or unexpected operator %d", op);
}

void ta_shared_panic(const char *where)
{
    Tcl_Panic("Shared thdr_t passed for modification to %s.", where);
}

void ta_small_panic(thdr_t *thdr, const char *where)
{
    Tcl_Panic("Insufficient space in thdr_t (allocated %d) in %s.", thdr->usable, where);
}

void ta_memory_panic(int req_size)
{
    Tcl_Panic("Memory allocation failed (%d bytes).", req_size);
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
        if (optname)
            Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid option '%.32s'.", optname));
        else
            Tcl_SetResult(ip, "Invalid option.", TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "OPTION", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_not_column_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip, "Object is not a column.", TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "TCLOBJTYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_not_table_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip, "Object is not a table.", TCL_STATIC);
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
                         Tcl_ObjPrintf("tarray index %d out of bounds.", index));
        Tcl_SetErrorCode(ip, "TARRAY", "INDEX", "RANGE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_bad_count_error(Tcl_Interp *ip, int count)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Invalid count %d specified.", count));
        Tcl_SetErrorCode(ip, "TARRAY", "COUNT", "RANGE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_value_type_error(Tcl_Interp *ip, Tcl_Obj *o, int tatype)
{
    if (ip) {
        const char *typestr = ta_type_string(tatype);
        if (o && o->bytes)
            Tcl_SetObjResult(ip,
                             Tcl_ObjPrintf("Value '%.40s' not valid for type %s.",
                                           o->bytes, typestr));
        else
            Tcl_SetObjResult(ip,
                             Tcl_ObjPrintf("Value not valid for type %s.",
                                           typestr));
        Tcl_SetErrorCode(ip, "TARRAY", "VALUE", "TYPE", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_row_width_error(Tcl_Interp *ip, int row_width, int min_width)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Row or table width %d does not match destination width %d.", row_width, min_width));
        Tcl_SetErrorCode(ip, "TARRAY", "ROW", "WIDTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_table_length_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip,
                      "Columns in table have differing lengths.",
                      TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "LENGTH", NULL);
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

TCL_RESULT ta_limit_error(Tcl_Interp *ip, Tcl_WideInt req_count)
{
    if (ip) {
        Tcl_Obj *ores;
        char *fmt = "Requested array size (%ld) greater than limit.";
        if (req_count > INT_MAX || req_count < INT_MIN) {
            /* Because Tcl_ObjPrintf as of 8.6.4 cannot print wides with %ld */
            Tcl_Obj *o = Tcl_NewWideIntObj(req_count);
            Tcl_IncrRefCount(o);
            ores = Tcl_Format(ip, fmt, 1, &o);
            Tcl_DecrRefCount(o);
        } else
            ores = Tcl_ObjPrintf(fmt, req_count);
        if (ores)
            Tcl_SetObjResult(ip, ores);
        else
            Tcl_SetResult(ip, "Array size limit exceeded.", TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "SIZELIMIT", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_indices_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip)
        Tcl_SetErrorCode(ip, "TARRAY", "VALUE", "INDEXLIST", NULL);
    return error_from_obj(ip, "Invalid index list", o);
}

TCL_RESULT ta_index_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip)
        Tcl_SetErrorCode(ip, "TARRAY", "VALUE", "INDEX", NULL);
    return error_from_obj(ip, "Invalid index", o);
}

TCL_RESULT ta_mismatched_types_error(Tcl_Interp *ip, int typea, int typeb)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("tarray types %s and %s are not compatible for attempted operation.", ta_type_string(typea), ta_type_string(typeb)));
        Tcl_SetErrorCode(ip, "TARRAY", "TYPE", "INCOMPATIBLE", NULL);
    }

    return TCL_ERROR;
}

TCL_RESULT ta_invalid_op_for_type(Tcl_Interp *ip, int typea)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Operation is invalid for type %s.", ta_type_string(typea)));
        Tcl_SetErrorCode(ip, "TARRAY", "TYPE", "OPERATION", NULL);
    }

    return TCL_ERROR;
}

TCL_RESULT ta_invalid_op_for_table(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip, "Operation is invalid for tables.", TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "OPERATION", NULL);
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
    if (ip)
        Tcl_SetErrorCode(ip, "TARRAY", "RANGE", "VALUE", NULL);
    return error_from_obj(ip, "Invalid index range limit", o);
}


TCL_RESULT ta_column_name_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip)
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "COLUMN", NULL);
    return error_from_obj(ip, "No column with specified name", o);
}

TCL_RESULT ta_duplicate_columns_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip)
        Tcl_SetErrorCode(ip, "TARRAY", "TABLE", "COLUMN", NULL);
    return error_from_obj(ip, "Duplicate columns specified in column list", o);
}

TCL_RESULT ta_conflicting_options_error(Tcl_Interp *ip, const char *optA, const char *optB)
{
    if (ip) {
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Options %s and %s cannot be used together.", optA, optB));
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

TCL_RESULT ta_column_lengths_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip,
                      "Columns have differing lengths.",
                      TCL_STATIC);
        Tcl_SetErrorCode(ip, "TARRAY", "COLUMN", "LENGTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_check_column_type(Tcl_Interp *ip, thdr_t *thdr, int wanted_type)
{
    if (thdr->type != wanted_type)
        return ta_bad_type_error(ip, thdr);
    return TCL_OK;
}

TCL_RESULT ta_invalid_operand_error(Tcl_Interp *ip, Tcl_Obj *o)
{
    if (ip)
        Tcl_SetErrorCode(ip, "TARRAY", "OPERAND", NULL);
    return error_from_obj(ip, "Invalid operand(s)", o);
}

TCL_RESULT ta_invalid_argcount(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetErrorCode(ip, "TARRAY", "ARGCOUNT", NULL);
        Tcl_SetResult(ip, "Invalid number of arguments.", TCL_STATIC);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_integer_overflow_error(Tcl_Interp *ip, char *precision, Tcl_WideInt val)
{
    if (ip) {
        Tcl_SetErrorCode(ip, "TARRAY", "OPERAND", "OVERFLOW", NULL);
        if (val) {
            /* ObjPrintf does not handle wide ints so we have to use buffer */
            char buf[200];
            snprintf(buf, sizeof(buf), "Value %" TCL_LL_MODIFIER "d does not fit in a %s.", val, precision);
            Tcl_SetResult(ip, buf, TCL_VOLATILE);
        } else {
            Tcl_SetObjResult(ip, Tcl_ObjPrintf("%s overflow.", precision));
        }
    }
    return TCL_ERROR;
}

TCL_RESULT ta_integer_overflow_obj_error(Tcl_Interp *ip, char *precision, Tcl_Obj *o)
{
    Tcl_Obj *err;
    if (ip) {
        Tcl_SetErrorCode(ip, "TARRAY", "OPERAND", "OVERFLOW", NULL);
        if (o) {
            err = Tcl_ObjPrintf("Value %s does not fit in a %s.", 
                                Tcl_GetString(o), precision);
        } else {
            err = Tcl_ObjPrintf("%s overflow.", precision);
        }
        Tcl_SetObjResult(ip, err);
    }
    return TCL_ERROR;
}


TCL_RESULT ta_negative_count_error(Tcl_Interp *ip, int count)
{
    if (ip) {
        Tcl_SetErrorCode(ip, "TARRAY", "COUNT", NULL);
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Negative value %d specified as count.", count));
    }
    return TCL_ERROR;
}

TCL_RESULT ta_invalid_rng_bounds(Tcl_Interp *ip, ta_value_t *plow, ta_value_t *phigh)
{
    Tcl_Obj *olow, *ohigh;
    if (ip) {
        olow = ta_value_to_obj(plow);
        ohigh = ta_value_to_obj(phigh);
    
        Tcl_SetErrorCode(ip, "TARRAY", "RANDOM", "BOUNDS", NULL);
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Invalid random number range bounds %s-%s.",
                                       Tcl_GetString(olow), Tcl_GetString(ohigh)));
        Tcl_DecrRefCount(olow);
        Tcl_DecrRefCount(ohigh);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_invalid_source_column_value(Tcl_Interp *ip, int row, int col, int tatype, Tcl_Obj *val)
{
    if (ip != NULL) {
        Tcl_Obj *e;

        /* Take care not to generate string rep if one does not exist */
        if (val && val->bytes)
            e = Tcl_ObjPrintf("Invalid value '%.40s' for type %s in row %d column %d of source data.", val->bytes, ta_type_string(tatype), row, col);
        else
            e = Tcl_ObjPrintf("Invalid value for type %s in row %d column %d of source data.", row, col, ta_type_string(tatype));
        
        Tcl_SetObjResult(ip, e);
    }

    return TCL_ERROR;
}

TCL_RESULT ta_invalid_source_row_width(Tcl_Interp *ip, int row, int nfields, int ncols)
{
    if (ip)
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Width %d of source row %d does not match expected destination width %d.",
                                       nfields, row, ncols));
    return TCL_ERROR;
}
