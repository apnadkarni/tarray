/*
 * Copyright (c) 2024 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include "tarray.h"

int ta_experiment;
int ta_full_validation;         /* Can really slow down! */

#ifdef TA_MT_ENABLE
/*
 * Thresholds for multithreading.
 * TBD - need to benchmark and set. Likely to depend on compiler.
 */
int ta_fill_mt_threshold = TA_MT_THRESHOLD_DEFAULT;
int ta_minmax_mt_threshold = TA_MT_THRESHOLD_DEFAULT;
#endif


const Tcl_ObjType *g_tcl_int_type_ptr;
const Tcl_ObjType *g_tcl_double_type_ptr;
const Tcl_ObjType *g_tcl_wide_type_ptr;
const Tcl_ObjType *g_tcl_dict_type_ptr;
const Tcl_ObjType *g_tcl_list_type_ptr;
const Tcl_ObjType *g_tcl_string_type_ptr;

#define CHECK2ARGS(ip_, objc_, objv_)                   \
    do {                                                \
        if (objc_ != 3) {                               \
            Tcl_WrongNumArgs(ip_, 1, objv_, "arg arg"); \
            return TCL_ERROR;                           \
        }                                               \
    } while (0)

#define FN2ARG(name_, type_, typestr_, getfn_, opfn_, newobjfn_)        \
    static TCL_RESULT                                                   \
    name_(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[]) \
    {                                                                   \
        type_ a, b, r;                                                  \
                                                                        \
        CHECK2ARGS(ip, objc, objv);                                     \
        if (getfn_(ip, objv[1], &a) != TCL_OK ||                        \
            getfn_(ip, objv[2], &b) != TCL_OK) {                        \
            return TCL_ERROR;                                           \
        }                                                               \
                                                                        \
        if (opfn_(a, b, &r))                                            \
            return ta_integer_overflow_error(ip, typestr_, 0);          \
        else {                                                          \
            Tcl_SetObjResult(ip, newobjfn_(r));                         \
            return TCL_OK;                                              \
        }                                                               \
    }

FN2ARG(ta_addu8_cmd, uint8_t, "unsigned 8-bit integer", ta_get_uint8_from_obj, ovf_add_uint8, Tcl_NewIntObj)
FN2ARG(ta_subu8_cmd, uint8_t, "unsigned 8-bit integer", ta_get_uint8_from_obj, ovf_sub_uint8, Tcl_NewIntObj)
FN2ARG(ta_mulu8_cmd, uint8_t, "unsigned 8-bit integer", ta_get_uint8_from_obj, ovf_mul_uint8, Tcl_NewIntObj)

FN2ARG(ta_add32_cmd, int32_t, "32-bit integer", ta_get_int_from_obj, ovf_add_int32, Tcl_NewIntObj)
FN2ARG(ta_sub32_cmd, int32_t, "32-bit integer", ta_get_int_from_obj, ovf_sub_int32, Tcl_NewIntObj)
FN2ARG(ta_mul32_cmd, int32_t, "32-bit integer", ta_get_int_from_obj, ovf_mul_int32, Tcl_NewIntObj)

FN2ARG(ta_addu32_cmd, uint32_t, "unsigned 32-bit integer", ta_get_uint_from_obj, ovf_add_uint32, Tcl_NewWideIntObj)
FN2ARG(ta_subu32_cmd, uint32_t, "unsigned 32-bit integer", ta_get_uint_from_obj, ovf_sub_uint32, Tcl_NewWideIntObj)
FN2ARG(ta_mulu32_cmd, uint32_t, "unsigned 32-bit integer", ta_get_uint_from_obj, ovf_mul_uint32, Tcl_NewWideIntObj)

FN2ARG(ta_add64_cmd, uint32_t, "64-bit integer", ta_get_int64_from_obj, ovf_add_int64, Tcl_NewWideIntObj)
FN2ARG(ta_sub64_cmd, uint32_t, "64-bit integer", ta_get_int64_from_obj, ovf_sub_int64, Tcl_NewWideIntObj)
FN2ARG(ta_mul64_cmd, uint32_t, "64-bit integer", ta_get_int64_from_obj, ovf_mul_int64, Tcl_NewWideIntObj)

static TCL_RESULT
ta_same_tclobj_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    CHECK2ARGS(ip, objc, objv);
    Tcl_SetObjResult(ip, Tcl_NewBooleanObj(objv[1] == objv[2]));
    return TCL_OK;
}

/*
 * If status is success, sets ip result to ores. Always returns status.
 * ores might be deallocated unless caller makes sure it is holding a
 * ref count. Primary purpose of this trivial routine is to deal
 * with returning an object that might have been either allocated
 * or be an existing object passed in the objv[] array to a command.
 */
TCL_RESULT ta_return_result(Tcl_Interp *ip, TCL_RESULT status, Tcl_Obj *ores)
{
    Tcl_IncrRefCount(ores);
    if (status == TCL_OK)
        Tcl_SetObjResult(ip, ores);
    Tcl_DecrRefCount(ores);
    return status;
}

/*
 * Sets the value of the variable given by ovarname to ovalue and sets the
 * the interp result to the resulting value of the variable (which may be
 * different from ovalue because of traces).
 * 
 * IMPORTANT: Caller should NOT access ovalue again unless it has protected
 * it by bumping its ref count.
 */
TCL_RESULT ta_set_var_result(Tcl_Interp *ip, TCL_RESULT status, Tcl_Obj *ovarname, Tcl_Obj *ovalue)
{
    Tcl_Obj *oresult;
    Tcl_IncrRefCount(ovalue);
    if (status == TCL_OK) {
        oresult = Tcl_ObjSetVar2(ip, ovarname, NULL, ovalue, TCL_LEAVE_ERR_MSG);
        if (oresult)
            Tcl_SetObjResult(ip, oresult);
        else
            status = TCL_ERROR;
    }
    Tcl_DecrRefCount(ovalue);

    return status;
}

/*  TBD - tests and docs for the types command */
static TCL_RESULT
ta_types_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    int i;
    Tcl_Obj *o;
    Tcl_Obj *res;

    res = Tcl_NewListObj(objc-1, NULL);
    for (i = 1; i < objc; ++i) {
        o = objv[i];

        /* Try to convert to a tarray type but only if the type pointer is
         * a list, string or NULL. We don't want to disturb other types
         */
        if (o->typePtr == NULL ||
            o->typePtr == g_tcl_string_type_ptr ||
            o->typePtr == g_tcl_list_type_ptr) {
            if (table_convert(NULL, o) != TCL_OK)
                tcol_convert(NULL, o);
        }

        if (table_affirm(o)) {
            Tcl_ListObjAppendElement(ip, res, Tcl_NewStringObj("table", sizeof("table")-1));
        } else if (tcol_affirm(o)) {
            Tcl_ListObjAppendElement(ip, res,
                                     Tcl_NewStringObj(ta_type_string(tcol_type(o)), -1));
        } else {
            Tcl_ListObjAppendElement(ip, res, Tcl_NewObj());
        }
    }
    Tcl_SetObjResult(ip, res);
    return TCL_OK;
    
}

static TCL_RESULT
ta_define_commands(Tcl_Interp *ip)
{
    struct cmdDefs {
        const char *name;
        Tcl_ObjCmdProc *fn;
        void *cdata;
        Tcl_CmdDeleteProc *delFn;
    } cmds[] = {
        {"::tarray::_same_tclobj", ta_same_tclobj_cmd, NULL, NULL},
        {"::tarray::types", ta_types_cmd, NULL, NULL},
        {"::tarray::column::bitsset", tcol_bitsset_cmd, NULL, NULL},
        {"::tarray::column::create", tcol_create_cmd, NULL, NULL},
        {"::tarray::column::delete", tcol_delete_cmd, NULL, NULL},
        {"::tarray::column::fill", tcol_fill_cmd, NULL, NULL},
        {"::tarray::column::vfill", tcol_vfill_cmd, NULL, NULL},
        {"::tarray::column::vdelete", tcol_vdelete_cmd, NULL, NULL},
        {"::tarray::column::cast", tcol_cast_cmd, NULL, NULL},
        {"::tarray::column::size", tcol_size_cmd, NULL, NULL},
        {"::tarray::column::type", tcol_type_cmd, NULL, NULL},
        {"::tarray::column::get", tcol_get_cmd, (void*)TA_RETRIEVE_GET, NULL},
        {"::tarray::column::index", tcol_index_cmd, NULL, NULL},
        {"::tarray::column::insert", tcol_insert_cmd, NULL, NULL},
        {"::tarray::column::inject", tcol_inject_cmd, NULL, NULL},
        {"::tarray::column::intersect3", tcol_intersect3_cmd, NULL, NULL},
        {"::tarray::column::place", tcol_place_cmd, NULL, NULL},
        {"::tarray::column::range", tcol_get_cmd, (void*)TA_RETRIEVE_RANGE, NULL},
        {"::tarray::column::equal", tcol_equal_cmd, (void *)0, NULL},
        {"::tarray::column::identical", tcol_equal_cmd, (void *)1, NULL},
        {"::tarray::column::put", tcol_put_cmd, NULL, NULL},
        {"::tarray::column::series", tcol_series_cmd, NULL, NULL},
        {"::tarray::column::search", tcol_search_cmd, NULL, NULL},
        {"::tarray::column::Sort", tcol_sort_cmd, NULL, NULL}, /* TBD - why is this Sort, not sort */
        {"::tarray::column::lookup", tcol_lookup_cmd, NULL, NULL},
        {"::tarray::column::minmax", tcol_minmax_cmd, NULL, NULL},
        {"::tarray::column::math", tcol_math_cmd, NULL, NULL},
        {"::tarray::column::fold", tcol_fold_cmd, NULL, NULL},
        {"::tarray::column::reverse", tcol_reverse_cmd, NULL, NULL},
        {"::tarray::column::vinject", tcol_vinject_cmd, NULL, NULL},
        {"::tarray::column::vinsert", tcol_vinsert_cmd, NULL, NULL},
        {"::tarray::column::vplace", tcol_vplace_cmd, NULL, NULL},
        {"::tarray::column::vput", tcol_vput_cmd, NULL, NULL},
        {"::tarray::column::vreverse", tcol_vreverse_cmd, NULL, NULL},
        {"::tarray::column::vsort", tcol_vsort_cmd, NULL, NULL},
        {"::tarray::column::_equalintervals", tcol_equalintervals_cmd, NULL, NULL},
        {"::tarray::column::_sortmerge_helper", tcol_sortmerge_helper_cmd, NULL, NULL},

        {"::tarray::table::put", table_put_cmd, NULL, NULL},
        {"::tarray::table::vput", table_vput_cmd, NULL, NULL},
        {"::tarray::table::fill", table_fill_cmd, NULL, NULL},
        {"::tarray::table::vfill", table_vfill_cmd, NULL, NULL},
        {"::tarray::table::delete", table_delete_cmd, NULL, NULL},
        {"::tarray::table::vdelete", table_vdelete_cmd, NULL, NULL},
        {"::tarray::table::get", table_get_cmd, (void *)TA_RETRIEVE_GET, NULL},
        {"::tarray::table::range", table_get_cmd, (void *)TA_RETRIEVE_RANGE, NULL},
        {"::tarray::table::index", table_index_cmd, NULL, NULL},
        {"::tarray::table::insert", table_insert_cmd, NULL, NULL},
        {"::tarray::table::vinsert", table_vinsert_cmd, NULL, NULL},
        {"::tarray::table::inject", table_inject_cmd, NULL, NULL},
        {"::tarray::table::vinject", table_vinject_cmd, NULL, NULL},
        {"::tarray::table::place", table_place_cmd, NULL, NULL},
        {"::tarray::table::vplace", table_vplace_cmd, NULL, NULL},
        {"::tarray::table::reverse", table_reverse_cmd, NULL, NULL},
        {"::tarray::table::vreverse", table_vreverse_cmd, NULL, NULL},
        {"::tarray::table::size", table_size_cmd, (void *) 0, NULL},
        {"::tarray::table::width", table_size_cmd, (void *) 1, NULL},
        {"::tarray::table::column", table_column_cmd, NULL, NULL},
        {"::tarray::table::vcolumn", table_vcolumn_cmd, NULL, NULL},
        {"::tarray::table::_columns", table__columns_cmd, NULL, NULL},
        {"::tarray::table::cnames", table_cnames_cmd, NULL, NULL},
        {"::tarray::table::slice", table_slice_cmd, NULL, NULL},

        {"::tarray::addu8", ta_addu8_cmd, NULL, NULL}, /* TBD - doc and test these */
        {"::tarray::subu8", ta_subu8_cmd, NULL, NULL},
        {"::tarray::mulu8", ta_mulu8_cmd, NULL, NULL},
        {"::tarray::add32", ta_add32_cmd, NULL, NULL},
        {"::tarray::sub32", ta_sub32_cmd, NULL, NULL},
        {"::tarray::mul32", ta_mul32_cmd, NULL, NULL},
        {"::tarray::addu32", ta_addu32_cmd, NULL, NULL},
        {"::tarray::subu32", ta_subu32_cmd, NULL, NULL},
        {"::tarray::mulu32", ta_mulu32_cmd, NULL, NULL},
        {"::tarray::add64", ta_add64_cmd, NULL, NULL},
        {"::tarray::sub64", ta_sub64_cmd, NULL, NULL},
        {"::tarray::mul64", ta_mul64_cmd, NULL, NULL},
    };
    int i;

    TA_ASSERT(ba_sanity_check() == 0);
    g_tcl_int_type_ptr = Tcl_GetObjType("int");
    g_tcl_double_type_ptr = Tcl_GetObjType("double");
    g_tcl_wide_type_ptr = Tcl_GetObjType("wide");
    g_tcl_dict_type_ptr = Tcl_GetObjType("dict");
    g_tcl_list_type_ptr = Tcl_GetObjType("list");
    g_tcl_string_type_ptr = Tcl_GetObjType("string");

    for (i = 0; i < sizeof(cmds)/sizeof(cmds[0]); ++i) {
        Tcl_CreateObjCommand(ip, cmds[i].name, cmds[i].fn,
                             cmds[i].cdata, cmds[i].delFn);
    }
    Tcl_NRCreateCommand(ip, "::tarray::loop",
                        ta_loop_cmd, ta_loop_nr_cmd, NULL, NULL);
    {
        /* RNG object */
	Tcl_CreateObjCommand(ip, "::tarray::rng",
                             ta_rng_cmd, Tcl_Alloc(sizeof(ta_cmd_counter)), ta_rng_destructor);

        /* Commands related to random number generation */
	ta_rng_t *prng;
        prng = ckalloc(sizeof(ta_rng_t));
        tcol_random_init(prng);
        prng->nrefs = 4;        /* For each command reference below */
	/*
	 * Note all commands have the same ta_random_rng_delete
	 * as last parameter. That's not a typo
	 */
	Tcl_CreateObjCommand(ip, "::tarray::column::random",
                             tcol_random_cmd, prng, ta_random_rng_delete);
        Tcl_CreateObjCommand(ip, "::tarray::randseed",
                             ta_randseed_cmd, prng, ta_random_rng_delete);
        Tcl_CreateObjCommand(ip, "::tarray::column::shuffle",
                             tcol_shuffle_cmd, prng, ta_random_rng_delete);
        Tcl_CreateObjCommand(ip, "::tarray::column::vshuffle",
                             tcol_vshuffle_cmd, prng, ta_random_rng_delete);
    }
    return TCL_OK;
}

TCL_RESULT
ta_real_init (Tcl_Interp *ip)
{
    return ta_define_commands(ip);
}
