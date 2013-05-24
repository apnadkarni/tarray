/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#include "tarray.h"

#ifdef TA_MT_ENABLE
/*
 * Thresholds for multithreading.
 * TBD - need to benchmark and set. Likely to depend on compiler.
 */
int ta_fill_mt_threshold = 100000;
#endif

/*
 * TArray is a Tcl "type" used for densely storing arrays of elements
 * of a specific type. For reasons of efficiency in type checking,
 * we define two types - tcolumn that holds an array of elements,
 * and table, which is just a tcolumn (and hence uses the same member
 * functions) each element of which is a Tcl_Obj
 * that is itself a tcolumn.
 */
static void ta_type_dup(Tcl_Obj *psrc, Tcl_Obj *pdst);
static void ta_type_free_intrep(Tcl_Obj *o);
static void ta_type_update_string(Tcl_Obj *o);
struct Tcl_ObjType g_tcol_type = {
    "tcolumn",
    ta_type_free_intrep,
    ta_type_dup,
    ta_type_update_string,
    NULL,     /* jenglish advises to keep this NULL */
};
struct Tcl_ObjType g_table_type = {
    "table",
    ta_type_free_intrep,
    ta_type_dup,
    ta_type_update_string,
    NULL,     /* jenglish advises to keep this NULL */
};

/* Must match definitions in tarray.h ! */
const char *g_type_tokens[] = {
    "boolean",
    "uint",
    "int",
    "wide",
    "double",
    "byte",
    "any",
    NULL
};    

Tcl_ObjType *g_tcl_list_type_ptr;


/* TBD - in error and panic routines make sure strings are not too long */

const char *ta_type_string(int tatype)
{
    if (tatype < (sizeof(g_type_tokens)/sizeof(g_type_tokens[0]))) {
        return g_type_tokens[tatype];
    } else
        return "<invalid>";
}

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
        Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid option '%s'", optname));
        Tcl_SetErrorCode(ip, "TARRAY", "OPTION", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_not_tarray_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip, "Object is not a TArray", TCL_STATIC);
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
                         Tcl_ObjPrintf("Value %s not valid for type %s.",
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
                         Tcl_ObjPrintf("Invalid index range specification '%s'.",
                                       Tcl_GetString(o)));
        Tcl_SetErrorCode(ip, "TARRAY", "RANGE", "VALUE", NULL);
    }
    return TCL_ERROR;
}


TCL_RESULT ta_get_uint_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned int *pui)
{
    Tcl_WideInt wide;
    if (Tcl_GetWideIntFromObj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < 0 || wide > 0xFFFFFFFF)
        return ta_value_type_error(ip, o, TA_UINT);
    *pui = (unsigned int) wide;
    return TCL_OK;
}

/* We cannot rely on Tcl_GetIntFromObj because that does not signal an
   error on signed overflow (i.e. 0x80000000 is treated as a valid integer)
*/
TCL_RESULT ta_get_int_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int *pi)
{
    Tcl_WideInt wide;
    if (Tcl_GetWideIntFromObj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < INT_MIN || wide > INT_MAX)
        return ta_value_type_error(ip, o, TA_INT);
    *pi = (int) wide;
    return TCL_OK;
}

TCL_RESULT ta_get_byte_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned char *pb)
{
    int i;
    if (Tcl_GetIntFromObj(ip, o, &i) != TCL_OK)
        return TCL_ERROR;
    if (i < 0 || i > 255)
        return ta_value_type_error(ip, o, TA_BYTE);
    *pb = (unsigned char) i;
    return TCL_OK;
}


/* Increments the ref counts of Tcl_Objs in a tarray making sure not
   to run past end of array */
void thdr_incr_obj_refs(thdr_t *thdr, int first, int count)
{
    if (thdr->type == TA_ANY) {
        if ((first + count) > thdr->used)
            count = thdr->used - first;
        /* Note count might even be negative, check */
        if (count > 0) {
            Tcl_Obj **pobjs, **end;
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
            end = pobjs + count;
            while (pobjs < end) {
                Tcl_IncrRefCount(*pobjs);
                ++pobjs;
            }
        }
    }
}

/* Decrements the ref counts of Tcl_Objs in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void thdr_decr_obj_refs(thdr_t *thdr, int first, int count)
{
    if (thdr->type == TA_ANY) {
        if ((first + count) > thdr->used)
            count = thdr->used - first;
        /* Note count might even be negative, check */
        if (count > 0) {
            Tcl_Obj **pobjs, **end;
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
            end = pobjs + count;
            while (pobjs < end) {
                Tcl_DecrRefCount(*pobjs);
                ++pobjs;
            }
        }
    }
}

void thdr_ensure_obj_strings(thdr_t *thdr)
{
    Tcl_Obj **pobjs, **end;

    TA_ASSERT(thdr->type == TA_ANY);

    pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    end = pobjs + thdr->used;
    while (pobjs < end) {
        if ((*pobjs)->bytes == NULL)
            Tcl_GetString(*pobjs);
        TA_ASSERT((*pobjs)->bytes != NULL);
        ++pobjs;
    }
}


#ifdef TA_MT_ENABLE
/*
 * For multithreaded operations, the array needs to be split up such that
 * the boundary is aligned at a point where the threads will not interfere
 * with one another. This is an issue only for the sizes smaller
 * than an atomic memory unit (assumed to be int).
 * Returns number of elements in first partition. This will never be 0
 * unless count is passed in as 0.
 * *psecond_block_size will hold number in second. This may be 0.
 *
 * Does not check if range is too small to be worthwhile multithreading.
 * That depends on operation and is up to the caller.
 *
 * Note tatype must not be TA_BOOLEAN.
 */
int thdr_calc_mt_split(int tatype, int first, int count, int *psecond_block_size)
{
    int second_block_size;
    int memunits;
    
    /* Assumes the thdr array is aligned properly and that processors
     * already access ints, wides and doubles atomically.
     */
    switch (tatype) {
    case TA_ANY:
    case TA_INT:
    case TA_UINT:
    case TA_DOUBLE:
    case TA_WIDE:
        second_block_size = count/2;
        break;
        
    case TA_BYTE:
        /* Assumes int is appropriate for memory atomicity */
        if (count < (2*sizeof(int)))
            second_block_size = 0;
        else {
            /* Remember first may not be aligned apprpriately */
            int aligned_first;
            int remaining_count;
            aligned_first = (first + sizeof(int) - 1) & (- (int) sizeof(int));
            remaining_count = count - (aligned_first - first);
            TA_ASSERT(remaining_count >= 0);
            memunits = remaining_count / sizeof(int);
            second_block_size = remaining_count - ((memunits / 2) * sizeof(int));
        }
        break;

    /* BOOLEAN and ANY cannot be multithreaded */
    case TA_BOOLEAN:
    default:
        ta_type_panic(tatype);
    }

    *psecond_block_size = second_block_size;
    return count - second_block_size;
}
#endif

/*
 * Updates TA_ANY elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_place_ta_objs(thdr_t *thdr,
                        thdr_t *pindices,
                        Tcl_Obj * const *ovalues,
                        int new_size
    )
{
    int i;
    int *pindex, *end;
    Tcl_Obj **pobjs;

    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + pindices->used;
    pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);

    /*
     * Reference counts makes this tricky. If replacing an existing
     * index we have to increment the new value's ref and decrement
     * the old value's. If the index points to a previously unused
     * slot, then the value there is garbage and Tcl_DecrRefCount
     * should not be called on it. The problem is we cannot distinguish
     * the cases up front using thdr->used as a threshold because
     * pindices is in arbitrary order AND indices may be repeated.
     * Hence what we do is to store NULL first in all unused slots
     * that will be written to mark what is unused.
     */
    for (i = thdr->used; i < new_size; ++i)
        pobjs[i] = NULL;        /* TBD - optimization - memset ? */
    while (pindex < end) {
        /* Careful about the order here! */
        Tcl_IncrRefCount(*ovalues);
        if (pobjs[*pindex] != NULL)
            Tcl_DecrRefCount(pobjs[*pindex]);/* Deref what was originally in that slot */
        pobjs[*pindex++] = *ovalues++;
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

/*
 * Fills TA_ANY elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_fill_ta_objs(thdr_t *thdr,
                       thdr_t *pindices,
                       Tcl_Obj *oval,
                       int new_size
    )
{
    int i;
    int *pindex, *end;
    Tcl_Obj **pobjs;

    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + pindices->used;
    pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);

    /*
     * Reference counts makes this tricky. If replacing an existing
     * index we have to increment the new value's ref and decrement
     * the old value's. If the index points to a previously unused
     * slot, then the value there is garbage and Tcl_DecrRefCount
     * should not be called on it. The problem is we cannot distinguish
     * the cases up front using thdr->used as a threshold because
     * pindices is in arbitrary order AND indices may be repeated.
     * Hence what we do is to store NULL first in all unused slots
     * that will be written to mark what is unused.
     */
    for (i = thdr->used; i < new_size; ++i)
        pobjs[i] = NULL;        /* TBD - optimization - memset ? */
    while (pindex < end) {
        TA_ASSERT(*pindex >= 0);
        /* Careful about the order here! */
        Tcl_IncrRefCount(oval);
        if (pobjs[*pindex] != NULL)
            Tcl_DecrRefCount(pobjs[*pindex]);/* Deref what was originally in that slot */
        pobjs[*pindex++] = oval;
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

/*
 * Map numeric or string index to numeric integer index.
 */
TCL_RESULT ta_convert_index(Tcl_Interp *ip, Tcl_Obj *o, int *pindex, int end_value, int low, int high)
{
    int val;

    /* Do type checks to avoid expensive shimmering in case of errors */
    if (tcol_affirm(o))
        return ta_index_error(ip, o);

    if (o->typePtr == g_tcl_list_type_ptr) {
        if (Tcl_ListObjLength(NULL, o, &val) != TCL_OK || val != 1)
            return ta_index_error(ip, o);
    }

    if (Tcl_GetIntFromObj(NULL, o, &val) != TCL_OK) {
        if (! ta_strequal(Tcl_GetString(o), "end"))
            return ta_index_error(ip, o);
        val = end_value;
    }
    
    if (val < low || val > high)
        return ta_index_range_error(ip, val);
    else {
        *pindex = val;
        return TCL_OK;
    }
}

/* 
 * Parse range bounds. low has to be between 0 and nelems.
 * "end" is treated nelems-1 (ie. last index)
 * high has to be 0-INT_MAX
 * if (high < low) count is returned as 0 (not an error)
 * Negative indices treated as 0
 */
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *ip, int nelems, Tcl_Obj *olow, Tcl_Obj *ohigh, int *plow, int *pcount)
{
    int low, high;

    if (ta_convert_index(ip, olow, &low, nelems-1, INT_MIN, nelems) != TCL_OK)
        return TCL_ERROR;

    /* We allow -1 in case nelems == 0, since 'end' will then return -1 */
    if (ta_convert_index(ip, ohigh, &high, nelems-1, -1, INT_MAX) != TCL_OK)
        return TCL_ERROR;

    if (low < 0)
        low = 0;
    *plow = low;
    if (high < low)
        *pcount = 0;            /* This is how lrange behaves */
    else
        *pcount = high - low + 1;

    return TCL_OK;
}

TCL_RESULT tcol_convert_from_other(Tcl_Interp *ip, Tcl_Obj *o)
{
    Tcl_Obj **elems;
    int nelems, tatype;
    
    /* See if we can convert it to one based on string representation */
    if (Tcl_ListObjGetElements(NULL, o, &nelems, &elems) == TCL_OK
        && nelems == 3
        && !strcmp(Tcl_GetString(elems[0]), "tarray")
        && Tcl_GetIndexFromObj(ip, elems[1], g_type_tokens, "TArrayType",
                               TCL_EXACT, &tatype) == TCL_OK) {
        /* So far so good. Try and convert */
        thdr_t *thdr;
        Tcl_Obj **ovalues;
        int nvalues;
        
        if (Tcl_ListObjGetElements(ip, elems[2], &nvalues, &ovalues)
            != TCL_OK)
            return TCL_ERROR;

        thdr = thdr_alloc_and_init(ip, tatype, nvalues, ovalues, 0);
        if (thdr == NULL)
            return TCL_ERROR;

        /*
         * Get rid of old representation and stick in the new one. Note
         * string rep is NOT invalidated and must NOT be if it is shared.
         * In any case, no need to do so here.
         */
        if (o->typePtr && o->typePtr->freeIntRepProc) {
            o->typePtr->freeIntRepProc(o);
            o->typePtr = NULL;
        }

        ta_set_intrep(o, thdr);
        return TCL_OK;
    }
                
    return ta_not_tarray_error(ip);
}



TCL_RESULT ta_value_from_obj(Tcl_Interp *ip, Tcl_Obj *o,
                              unsigned char tatype, ta_value_t *ptav)
{
    int i, status;

    switch (tatype) {
    case TA_BOOLEAN:
        if ((status = Tcl_GetBooleanFromObj(ip, o, &i)) == TCL_OK)
            ptav->bval = (i != 0);
        break;
    case TA_BYTE: status = ta_get_byte_from_obj(ip, o, &ptav->ucval); break;
    case TA_INT: status = ta_get_int_from_obj(ip, o, &ptav->ival); break;
    case TA_UINT: status = ta_get_uint_from_obj(ip, o, &ptav->uival); break;
    case TA_WIDE: status = Tcl_GetWideIntFromObj(ip, o, &ptav->wval); break;
    case TA_DOUBLE: status = Tcl_GetDoubleFromObj(ip, o, &ptav->dval); break;
    case TA_ANY: ptav->oval = o; status = TCL_OK; break;
    default:
        ta_type_panic(tatype);
    }
    if (status == TCL_OK)
        ptav->type = tatype;
    return status;
}

struct thdr_fill_mt_context {
    ta_value_t tav;
    void *base;
    int   nelems;
};

static void thdr_fill_int_mt_worker(struct thdr_fill_mt_context *pctx)
{
    int val = pctx->tav.ival;
    int *pint, *end;

    /*
      TBD - newer MS compiler generates as good code for any fill,
      no need to special case to memset for 0. However, VC++ 6 does not.
      Check if gcc same and ifdef memset code for specific compilers
    */
    if (val == 0) {
        memset(pctx->base, 0, pctx->nelems*sizeof(int));
        return;
    }

    pint = pctx->base;
    end = pint + pctx->nelems;
    while (pint < end)
        *pint++ = val;
}

static void thdr_fill_double_mt_worker(struct thdr_fill_mt_context *pctx)
{
    double val = pctx->tav.dval;
    double *pdbl, *end;
    
    pdbl = pctx->base;
    end = pdbl + pctx->nelems;
    while (pdbl < end)
        *pdbl++ = val;
}

static void thdr_fill_wide_mt_worker(struct thdr_fill_mt_context *pctx)
{
    Tcl_WideInt val = pctx->tav.wval;
    Tcl_WideInt *pwide, *end;
    
    /*
      TBD - newer MS compiler generates as good code for any fill,
      no need to special case to memset for 0. However, VC++ 6 does not.
      Check if gcc same and ifdef memset code for specific compilers
    */
    if (val == 0) {
        memset(pctx->base, 0, pctx->nelems*sizeof(Tcl_WideInt));
        return;
    }

    pwide = pctx->base;
    end = pwide + pctx->nelems;
    while (pwide < end)
        *pwide++ = val;
}

static void thdr_fill_byte_mt_worker(struct thdr_fill_mt_context *pctx)
{
    memset(pctx->base, pctx->tav.ucval, pctx->nelems);
}

/*
 * Use multiple threads to fill scalar values. 
 * WARNING: Does NOT update thdr header (like thdr->used etc.)
 * Caller must do that.
 */
void thdr_fill_scalars(Tcl_Interp *ip, thdr_t *thdr,
                       const ta_value_t *ptav, int pos, int count)
{
    void (*workerfn)(struct thdr_fill_mt_context *);
    int elem_size;
    struct thdr_fill_mt_context fill_context[2];

    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(thdr->nrefs <= 1);
    TA_ASSERT((pos+count) <= thdr->usable);

    /*
      TBD - depending on compiler generated code, multithreading
      fills serves no purpose. VS 2012 translates to reps instruction
      and does not benefit from multithreading. VC++ 6 does. Benchmark
      and set default multithreading threshold accordingly.
    */

    switch (thdr->type) {
    case TA_BYTE:
        workerfn = thdr_fill_byte_mt_worker;
        break;
    case TA_WIDE:
        workerfn = thdr_fill_wide_mt_worker;
        break;
    case TA_DOUBLE:
        workerfn = thdr_fill_double_mt_worker;
        break;

    case TA_INT:
    case TA_UINT:
        workerfn = thdr_fill_int_mt_worker;
        break;
    default:
        ta_type_panic(thdr->type);
    }

    fill_context[0].nelems = thdr_calc_mt_split(thdr->type, pos, count, &fill_context[1].nelems);
    TA_ASSERT((fill_context[0].nelems + fill_context[1].nelems) == count);

    elem_size = thdr->elem_bits / CHAR_BIT;
    fill_context[0].tav = *ptav;
    fill_context[0].base = (pos*elem_size) + THDRELEMPTR(thdr, unsigned char, 0);
    if (count < ta_fill_mt_threshold || fill_context[1].nelems == 0) {
        fill_context[0].nelems = count;
        workerfn(&fill_context[0]);
    } else {
        /* Multiple threads */
        ta_mt_group_t grp;
        fill_context[1].tav = *ptav;
        fill_context[1].base = (elem_size*fill_context[0].nelems) + (char*)fill_context[0].base;
        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* TBD - check return code */ ta_mt_group_async_f(grp, &fill_context[1], workerfn);
        workerfn(&fill_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);
    }
}

/*
 * Set the value of an element range at a position in a thdr_t.
 * See the asserts below for conditions under which this can be called
 */
void thdr_fill_range(Tcl_Interp *ip, thdr_t *thdr,
                     const ta_value_t *ptav, int pos, int count, int insert)
{
    int i;
    int new_used;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pos <= thdr->used);
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(count >= 0);

    if (count == 0)
        return;

    new_used = thdr_recompute_occupancy(thdr, &pos, count, insert);
    TA_ASSERT(new_used <= thdr->usable);

    if (insert)
        thdr_make_room(thdr, pos, count);

    /* NOTE in insert mode, thdr is in inconsistent state as intermediate
       slots are uninitialized. This is specially relevant for TA_ANY */

    thdr->sort_order = THDR_UNSORTED;
    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_fill(THDRELEMPTR(thdr, ba_t, 0), pos, count, ptav->bval);
        break;
    case TA_BYTE:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_INT:
    case TA_UINT:
        thdr_fill_scalars(ip, thdr, ptav, pos, count);
        break;
        
    case TA_ANY:
        {
            Tcl_Obj **pobjs;
            int n;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts. Note when inserting
             * no decrements are to be done.
             */
            i = pos;    /* Starting position to write */
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, pos);
            if (! insert) {
                /* First loop overwriting existing elements */
                n = pos + count;
                if (n > thdr->used)
                    n = thdr->used;
                for (; i < n; ++i) {
                    /* Be careful of the order */
                    Tcl_IncrRefCount(ptav->oval);
                    Tcl_DecrRefCount(*pobjs);
                    *pobjs++ = ptav->oval;
                }
            }

            /* Now loop over new elements being appended */
            for (; i < pos+count; ++i) {
                Tcl_IncrRefCount(ptav->oval);
                *pobjs++ = ptav->oval;
            }
        }
        break;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_used <= thdr->usable);
    thdr->used = new_used;
}

TCL_RESULT ta_verify_value_objs(Tcl_Interp *ip, int tatype,
                                int nelems, Tcl_Obj * const elems[])
{
    Tcl_Obj * const *pobjs = elems;
    Tcl_Obj * const *end = elems + nelems;

#define ta_verify_value_LOOP(type, fn)          \
    do {                                        \
        for ( ; pobjs < end; ++pobjs) {         \
            type val;                           \
            if (fn(ip, *pobjs, &val) != TCL_OK) \
                return TCL_ERROR;               \
        }                                       \
    } while (0)
        
    switch (tatype) {
    case TA_BOOLEAN:
        ta_verify_value_LOOP(int, Tcl_GetBooleanFromObj);
        break;
    case TA_UINT:
        ta_verify_value_LOOP(unsigned int, ta_get_uint_from_obj);
        break;
    case TA_INT:
        ta_verify_value_LOOP(int, ta_get_int_from_obj);
        break;
    case TA_WIDE:
        ta_verify_value_LOOP(Tcl_WideInt, Tcl_GetWideIntFromObj);
        break;
    case TA_DOUBLE:
        ta_verify_value_LOOP(double, Tcl_GetDoubleFromObj);
        break;
    case TA_BYTE:
        ta_verify_value_LOOP(unsigned char, ta_get_byte_from_obj);
        break;
    case TA_ANY:
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
   Returns new size needed in *new_sizeP
*/
TCL_RESULT thdr_verify_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *pindices, int *new_sizeP)
{
    int cur, used, highest, status;
    int *pindex, *end;
    thdr_t *psorted = NULL;

    TA_ASSERT(pindices->type == TA_INT);

    used = thdr->used;
    pindex = THDRELEMPTR(pindices, int, 0);
    end = THDRELEMPTR(pindices, int, pindices->used);

    /* Special cases so we don't go through the long path */
    if (pindices->used < 2) {
        /* 0/1 element list is always sorted so mark it as such anyways ! */
        thdr->sort_order = THDR_SORTED_ASCENDING;
        if (pindices->used == 0)
            *new_sizeP = 0;
        else {
            /* One index specified */
            if (*pindex < 0 || *pindex > used)
                return ta_index_range_error(ip, *pindex);
            *new_sizeP = used;
            if (*pindex == used)
                *new_sizeP = used+1;
        }
        return TCL_OK;
    }    

    /*
     * If indices are not sorted, we need to sort them to ensure no gaps.
     * Potentially we could use a bit array to do it differently but...TBD
     */
    if (pindices->sort_order == THDR_UNSORTED) {
        psorted = thdr_clone(ip, pindices, 0);
        if (psorted == NULL)
            return TCL_ERROR;
        qsort(THDRELEMPTR(psorted, int, 0), psorted->used, sizeof(int), intcmp);
        psorted->sort_order = THDR_SORTED_ASCENDING;
        pindices = psorted;
        pindex = THDRELEMPTR(pindices, int, 0);
        end = THDRELEMPTR(pindices, int, pindices->used);
    }

    /* Make sure no negative indices and no gaps in indices */
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        /* Indices are sorted ascending. So to check for negative indices,
         * only need to check first entry.
         */
        if (*pindex < 0) {
            status = ta_index_range_error(ip, *pindex);
            goto vamoose;
        }
        
        /*
         * We will start going backward until we hit a gap in the sequence
         * and error out at that point. If we reach the current size,
         * we can stop since any further indices will be within the current
         * limits.
         */
        TA_ASSERT(pindex < end); /* Since we already special cased 0/1 above */
        cur = *--end;
        highest = cur;
        while (pindex < end && cur > used) {
            --end;
            /* Sorted, so can only be cur or cur-1 */
            if (*end != cur && *end != (cur-1))
                break;          /* Gap! */
            cur = *end;
        }
    } else {
        TA_ASSERT(pindex < end); /* Since we already special cased 0/1 above */

        /* Indices are sorted descending. So to check for negative indices,
         * only need to check last entry.
         */
        if (end[-1] < 0) {
            status = ta_index_range_error(ip, *pindex);
            goto vamoose;
        }

        /* Same as above loop but in reverse since sorted in reverse order */
        cur = *pindex++;
        highest = cur;
        while (pindex < end && cur > used) {
            if (*pindex != cur && *pindex != (cur-1))
                break;
            cur = *pindex++;
        }
    }

    if (cur <= used) {
        *new_sizeP = highest >= used ? highest + 1 : used;
        status = TCL_OK;
    } else
        status = ta_index_range_error(ip, cur);
vamoose:
    if (psorted)
        thdr_decr_refs(psorted);

    return status;

}

/* thdr must be large enough for largest index. And see asserts in code */
void thdr_fill_indices(Tcl_Interp *ip, thdr_t *thdr, 
                       const ta_value_t *ptav, thdr_t *pindices,
                       int new_size
    )
{
    int *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(pindices->type == TA_INT);

    /* Caller guarantees room for highest index value */
    TA_ASSERT(new_size <= thdr->usable);

    if (pindices->used == 0)
        return;          /* Nothing to do */

    /* Rest of code assumes > 0 indices */

    pindex = THDRELEMPTR(pindices, int, 0);
    end = THDRELEMPTR(pindices, int, pindices->used);

    thdr->sort_order = THDR_UNSORTED;
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                ba_put(baP, *pindex++, ptav->bval);
            }
        }
        break;
    case TA_INT:
    case TA_UINT:
        {
            int *pint = THDRELEMPTR(thdr, int, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                pint[*pindex++] = ptav->ival;
            }
        }
        break;
    case TA_BYTE:
        {
            unsigned char *ucP = THDRELEMPTR(thdr, unsigned char, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                ucP[*pindex++] = ptav->ucval;
            }
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *pwide;
            pwide = THDRELEMPTR(thdr, Tcl_WideInt, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                pwide[*pindex++] = ptav->wval;
            }
        }
        break;
    case TA_DOUBLE:
        {
            double *pdbl;
            pdbl = THDRELEMPTR(thdr, double, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                pdbl[*pindex++] = ptav->dval;
            }
        }
        break;
    case TA_ANY:
        thdr_fill_ta_objs(thdr, pindices, ptav->oval, new_size);
        return;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

void thdr_free(thdr_t *thdr)
{
    if (thdr->type == TA_ANY) {
        thdr_decr_obj_refs(thdr, 0, thdr->used);
    }
    TA_FREEMEM(thdr);
}


static void ta_type_free_intrep(Tcl_Obj *o)
{
    thdr_t *thdr;

    TA_ASSERT(tcol_affirm(o));

    thdr = TARRAYHDR(o); 
    TA_ASSERT(thdr);

    thdr_decr_refs(thdr);
    TARRAYHDR(o) = NULL;
    o->typePtr = NULL;
}

static void ta_type_dup(Tcl_Obj *osrc, Tcl_Obj *odst)
{
    TA_ASSERT(tcol_affirm(osrc));
    TA_ASSERT(TARRAYHDR(osrc) != NULL);
        
    ta_set_intrep(odst, TARRAYHDR(osrc));
}


/* Called to generate a string implementation from an array of Tcl_Obj */
static void ta_type_update_string_for_objtype(Tcl_Obj *o)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
    Tcl_Obj **objv;
    int objc;
    thdr_t *thdr;
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    int i, length;
    size_t bytesNeeded;
    const char *elem;
    char *dst;

    thdr = TARRAYHDR(o);
    objv = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    objc = thdr->used;

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

    bytesNeeded =
        sizeof("tarray ") - 1 /* -1 to exclude the null */
        + sizeof(" {") - 1 /* Start of list minus trailing null */
        + 1               /* Trailing "}" */
        + strlen(g_type_tokens[TA_ANY]);
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = TCL_DONT_QUOTE_HASH;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if ((((size_t)1) << (sizeof(bytesNeeded)*CHAR_BIT - 1)) & bytesNeeded)
            ta_string_overflow_panic("ta_type_update_string_for_objtype");
    }
    if ((bytesNeeded + objc + 1) > INT_MAX)
        ta_string_overflow_panic("ta_type_update_string_for_objtype");

    bytesNeeded += objc;        /* For separators and terminating null */

    /*
     * Pass 2: copy into string rep buffer.
     */

    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    o->bytes = ckalloc(bytesNeeded);
    dst = o->bytes;
    memcpy(dst, "tarray ", sizeof("tarray ")-1);
    dst += sizeof("tarray ") - 1;
    strcpy(dst, g_type_tokens[TA_ANY]);
    dst += strlen(g_type_tokens[TA_ANY]);
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
            TA_ASSERT(dst < (o->bytes + bytesNeeded));
        }
        dst[-1] = '}';
    } else
        *dst++ = '}';
    *dst = '\0';
    TA_ASSERT(dst < (o->bytes + bytesNeeded));
    o->length = dst - o->bytes;

    if (flagPtr != localFlags) {
        TA_FREEMEM((char *) flagPtr);
    }
}


static void ta_type_update_string(Tcl_Obj *o)
{
    unsigned int i, n, count;
    unsigned int allocated, unused, min_needed, prefix_len;
    char *cP;
    int max_elem_space;  /* Max space to print one element including
                            either terminating null or space */
    thdr_t *thdr;
        
    TA_ASSERT(tcol_affirm(o));

    thdr = TARRAYHDR(o);
    TA_ASSERT(thdr->type < sizeof(g_type_tokens)/sizeof(g_type_tokens[0]));

    o->bytes = NULL;

    prefix_len = 
        sizeof("tarray ") - 1   /* -1 to exclude the null */
        + strlen(g_type_tokens[thdr->type])
        + 2;                         /* Start of list " {" */
    min_needed = prefix_len + 1 + 1;            /* Trailing "}" and null */

    count = tcol_occupancy(o);
    if (count == 0) {
        /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
           defined as ckalloc */
        cP = ckalloc(min_needed);
        o->bytes = cP;
        snprintf(cP, min_needed, "tarray %s {}",
                  g_type_tokens[thdr->type]);
        o->length = min_needed - 1;
        return;
    }

    /* Code below based on count > 0 else terminating \0 will blow memory */

    /*
     * When output size cannot be calculated exactly, we allocate using
     * some estimate based on the type.
     */
        
    switch (tcol_type(o)) {
    case TA_BOOLEAN:
        {
            /*
             * Special case Boolean since we know exactly how many chars will
             * be required 
             */
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            register ba_t ba;
            register ba_t ba_mask;

            /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
               defined as ckalloc */
            cP = ckalloc(min_needed + 2*count - 1);
            n = snprintf(cP, min_needed, "tarray %s {",
                      g_type_tokens[TA_BOOLEAN]);
            TA_ASSERT(n > 0 && n < min_needed);
            o->bytes = cP;
            cP += n;
            n = count / BA_UNIT_SIZE;
            for (i = 0; i < n; ++i, ++baP) {
                ba = *baP;
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
            o->length = cP - o->bytes;
        }
        return;
                
    case TA_ANY:
        ta_type_update_string_for_objtype(o);
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
        ta_type_panic(thdr->type);
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
    o->bytes = cP;
    snprintf(cP, prefix_len+1, "tarray %s {", g_type_tokens[thdr->type]);
    TA_ASSERT(strlen(cP) == prefix_len);
    cP += prefix_len;
    min_needed = max_elem_space + 2; /* space or terminating "}" and null */
    for (i = 0; i < count; ) {
        if (unused < min_needed) {
            n = allocated - unused; /* Used space */
            /* Increase assuming average space taken so far (roughly) */
            TA_ASSERT(i != 0);
            allocated += min_needed + (count - i) * (n/i);
            o->bytes = ckrealloc(o->bytes, allocated);
            cP = n + (char *) o->bytes;
            unused = allocated - n;
        }
        /*
         * We nest loops for performance by minimizing switch jumps
         * At top of nested loops below, there is room for at least one elem
         */
        switch (thdr->type) {
        case TA_UINT:
        case TA_INT:
            {
                int *intP = THDRELEMPTR(thdr, int, i);
                char *fmt = thdr->type == TA_UINT ? "%u" : "%d";
                while (i < count && unused >= min_needed) {
                    n = snprintf(cP, unused, fmt, *intP++);
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
                Tcl_WideInt *pwide = THDRELEMPTR(thdr, Tcl_WideInt, i);
                while (i < count && unused >= min_needed) {
                    n = snprintf(cP, unused, "%" TCL_LL_MODIFIER "d", *pwide++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_DOUBLE:
            /* Do not use snprintf because of slight difference
               it does not include decimal point for whole ints. For
               consistency with Tcl, use Tcl_PrintDouble instead */
            {
                double *pdbl = THDRELEMPTR(thdr, double, i);
                while (i < count && unused >= min_needed) {
                    Tcl_PrintDouble(NULL, *pdbl++, cP);
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
                unsigned char *ucP = THDRELEMPTR(thdr, unsigned char, i);
                while (i < count && unused >= min_needed) {
                    n = snprintf(cP, unused, "%u", *ucP++);
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
    o->length = cP - o->bytes; /* Terminating null not included in length */
            
    /* Only shrink array if unused space is comparatively too large */
    unused = allocated - (o->length + 1);
    if (unused > (allocated / 8) && unused > 20)
        o->bytes = ckrealloc(o->bytes, o->length + 1);
    return;
}

Tcl_Obj *tcol_new(thdr_t *thdr)
{
    Tcl_Obj *o;

    if (thdr == NULL)
        return NULL;
    o = Tcl_NewObj();
    Tcl_InvalidateStringRep(o);
    ta_set_intrep(o, thdr);
    return o;
}

/* thdr must NOT be shared and must have enough slots */
/* ip may be NULL (only used for errors) */
TCL_RESULT thdr_put_objs(Tcl_Interp *ip, thdr_t *thdr, int first,
                         int nelems, Tcl_Obj * const elems[], int insert)
{
    int i, ival;
    int status;
    int new_used;

    TA_ASSERT(thdr->nrefs < 2);

    new_used = thdr_recompute_occupancy(thdr, &first, nelems, insert);
    TA_ASSERT(new_used <= thdr->usable); /* Caller should have ensured */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /*
     * In case of conversion errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors.
     *
     * As a special optimization, when appending to the end, we do
     * not need to first check. We directly store the values and in case
     * of errors, simply do not update size. Note this works even for
     * the insertion case.
     */

    if (first < thdr->used) {
        if ((status = ta_verify_value_objs(ip, thdr->type, nelems, elems))
            != TCL_OK)
            return TCL_ERROR;
    }

    /* Make room if necessary */
    if (insert)
        thdr_make_room(thdr, first, nelems);

    /*
     * Now actually store the values. Note we still have to check
     * status on conversion since we did not do checks when we are appending
     * to the end.
     */
#define thdr_put_OBJCOPY(type, fn)              \
    do {                                        \
        type *p;                                \
        p = THDRELEMPTR(thdr, type, first);     \
        for (i = 0; i < nelems; ++i, ++p) {     \
            if (fn(ip, elems[i], p) != TCL_OK)  \
                goto convert_error;             \
        }                                       \
    } while (0)

    switch (thdr->type) {
    case TA_BOOLEAN:
       {
           register ba_t *baP;
           ba_t ba, ba_mask;
           int off;

           /* Take care of the initial condition where the first bit
              may not be aligned on a boundary */
           baP = THDRELEMPTR(thdr, ba_t, first / BA_UNIT_SIZE);
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
               if (Tcl_GetBooleanFromObj(ip, elems[i], &ival) != TCL_OK)
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
    case TA_UINT: thdr_put_OBJCOPY(unsigned int, ta_get_uint_from_obj); break;
    case TA_INT: thdr_put_OBJCOPY(int, ta_get_int_from_obj); break;
    case TA_WIDE: thdr_put_OBJCOPY(Tcl_WideInt, Tcl_GetWideIntFromObj); break;
    case TA_DOUBLE:thdr_put_OBJCOPY(double, Tcl_GetDoubleFromObj); break;
    case TA_BYTE: thdr_put_OBJCOPY(unsigned char, ta_get_byte_from_obj); break;
    case TA_ANY:
        {
            register Tcl_Obj **pobjs;
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
            for (i = 0; i < nelems; ++i, ++pobjs) {
                /* Careful about the order here! */
                Tcl_IncrRefCount(elems[i]);
                /* Only release existing elements if we were not inserting */
                if (!insert) {
                    if ((first + i) < thdr->used) {
                        /* Deref what was originally in that slot */
                        Tcl_DecrRefCount(*pobjs);
                    }
                }
                *pobjs = elems[i];
            }
        }
        break;

    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_used <= thdr->usable);
    thdr->used = new_used;

    return TCL_OK;

convert_error:                  /* Interp should already contain errors */
    TA_ASSERT(thdr->type != TA_ANY); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}


/* Caller must have done all the checks in the TA_ASSERTS below ! */
void thdr_place_objs(
    Tcl_Interp *ip,
    thdr_t *thdr,               /* thdr_t to be modified - must NOT be shared */
    thdr_t *pindices,            /* Contains indices. If any >= thdr->used,
                                    all intermediate indices must also be
                                    present in pindices. Caller
                                    must have checked */
    int new_size,
    int nvalues,                /* # values in ovalues */
    Tcl_Obj * const *ovalues)   /* Values to be stored, must be type verified */
{
    int *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pindices->type == TA_INT);
    TA_ASSERT(new_size <= thdr->usable);
    TA_ASSERT(pindices->used <= nvalues);
    TA_ASSERT(ta_verify_value_objs(ip, thdr->type, nvalues, ovalues) == TCL_OK);

    if (nvalues == 0)
        return;          /* Nothing to change */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /* Note we do not check conversion status since caller must check */

#define PLACEVALUES(type, fn) do {                                      \
        type *p;                                                        \
        p = THDRELEMPTR(thdr, type, 0);                                 \
        while (pindex < end) {                                          \
            TA_ASSERT(*pindex < thdr->usable);                          \
            TA_NOFAIL(fn(ip, *ovalues++, &p[*pindex++]), TCL_OK);       \
        }                                                               \
    } while (0)
    
    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + pindices->used;
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            int bval;
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            while (pindex < end) {
                TA_NOFAIL(Tcl_GetBooleanFromObj(ip, *ovalues++, &bval), TCL_OK);
                TA_ASSERT(*pindex < thdr->usable);
                ba_put(baP, *pindex++, bval);
            }
        }
        break;

    case TA_UINT:
        PLACEVALUES(unsigned int, ta_get_uint_from_obj);
        break;
    case TA_INT:
        PLACEVALUES(int, ta_get_int_from_obj);
        break;
    case TA_WIDE:
        PLACEVALUES(Tcl_WideInt, Tcl_GetWideIntFromObj);
        break;
    case TA_DOUBLE:
        PLACEVALUES(double, Tcl_GetDoubleFromObj);
        break;
    case TA_ANY:
        thdr_place_ta_objs(thdr, pindices, ovalues, new_size);
        return;
    case TA_BYTE:
        PLACEVALUES(unsigned char, ta_get_byte_from_obj);
        break;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

void thdr_place_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *psrc, thdr_t *pindices, int new_size)
{
    int *pindex, *end;
    int i;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pindices->type == TA_INT);
    TA_ASSERT(new_size <= thdr->usable);
    TA_ASSERT(pindices->used <= psrc->used);
    TA_ASSERT(thdr->type == psrc->type);

    if (pindices->used == 0)
        return;          /* Nothing to change */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
    
#define thdr_place_COPYINDICES(type) do {       \
        type *dst, *src;                        \
        dst = THDRELEMPTR(thdr, type, 0);       \
        src = THDRELEMPTR(psrc, type, 0);       \
        while (pindex < end) {                  \
            TA_ASSERT(*pindex < thdr->usable);  \
            dst[*pindex++] = *src++;            \
        }                                       \
    } while (0)
    
    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + pindices->used;
    
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *dst = THDRELEMPTR(thdr, ba_t, 0);
            ba_t *src = THDRELEMPTR(psrc, ba_t, 0);
            for (i = 0; pindex < end; ++pindex, ++i) {
                TA_ASSERT(*pindex < thdr->usable);
                ba_put(dst, *pindex, ba_get(src, i));
            }
        }
        break;

    case TA_UINT:
        thdr_place_COPYINDICES(unsigned int);
        break;
    case TA_INT:
        thdr_place_COPYINDICES(int);
        break;
    case TA_WIDE:
        thdr_place_COPYINDICES(Tcl_WideInt);
        break;
    case TA_DOUBLE:
        thdr_place_COPYINDICES(double);
        break;
    case TA_ANY:
        /* Tricky 'cause of ref counts. See comments in thdr_place_ta_objs */
        {
            Tcl_Obj **dst = THDRELEMPTR(thdr, Tcl_Obj *, 0);
            Tcl_Obj **src = THDRELEMPTR(psrc, Tcl_Obj *, 0);
            for (i = thdr->used; i < new_size; ++i)
                dst[i] = NULL;        /* TBD - optimization - memset ? */
            while (pindex < end) {
                /* Careful about the order here! */
                TA_ASSERT(*pindex < thdr->usable);
                Tcl_IncrRefCount(*src);
                if (dst[*pindex] != NULL)
                    Tcl_DecrRefCount(dst[*pindex]);/* Deref what was originally in that slot */
                dst[*pindex++] = *src++;
            }

        }
        break;
    case TA_BYTE:
        thdr_place_COPYINDICES(unsigned char);
        break;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

int thdr_required_size(int tatype, int count)
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
    case TA_ANY:
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

thdr_t *thdr_realloc(Tcl_Interp *ip, thdr_t *oldP, int new_count)
{
    thdr_t *thdr;
    int sz;

    TA_ASSERT(oldP->nrefs < 2);
    TA_ASSERT(oldP->used <= new_count);

    /* We allocate one more for the sentinel */
    sz = thdr_required_size(oldP->type, new_count + 1);
    thdr = (thdr_t *) TA_ATTEMPTREALLOCMEM((char *) oldP, sz);
    if (thdr)
        thdr->usable = new_count;
    else
        ta_memory_error(ip, sz);
    return thdr;
}

thdr_t * thdr_alloc(Tcl_Interp *ip, int tatype, int count)
{
    unsigned char nbits;
    int sz;
    thdr_t *thdr;

    if (count < 0)
            count = TA_DEFAULT_NSLOTS;
    /* We allocate one extra slot for the sentinel */
    sz = thdr_required_size(tatype, count + 1);
    thdr = (thdr_t *) TA_ATTEMPTALLOCMEM(sz);
    if (thdr == NULL) {
        if (ip)
            ta_memory_error(ip, sz);
        return NULL;
    }
    thdr->nrefs = 0;
    thdr->usable = count;
    thdr->used = 0;
    thdr->type = tatype;
    switch (tatype) {
    case TA_BOOLEAN: nbits = 1; break;
    case TA_UINT: nbits = sizeof(unsigned int) * CHAR_BIT; break;
    case TA_INT: nbits = sizeof(int) * CHAR_BIT; break;
    case TA_WIDE: nbits = sizeof(Tcl_WideInt) * CHAR_BIT; break;
    case TA_DOUBLE: nbits = sizeof(double) * CHAR_BIT; break;
    case TA_ANY: nbits = sizeof(Tcl_Obj *) * CHAR_BIT; break;
    case TA_BYTE: nbits = sizeof(unsigned char) * CHAR_BIT; break;
    default:
        ta_type_panic(tatype);
    }
    thdr->elem_bits = nbits;
    thdr->sort_order = THDR_UNSORTED;

    return thdr;
}

thdr_t * thdr_alloc_and_init(Tcl_Interp *ip, int tatype,
                           int nelems, Tcl_Obj * const elems[],
                           int init_size)
{
    thdr_t *thdr;

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

    thdr = thdr_alloc(ip, tatype, init_size);
    if (thdr) {
        if (elems != NULL && nelems != 0) {
            if (thdr_put_objs(ip, thdr, 0, nelems, elems, 0) != TCL_OK) {
                thdr_decr_refs(thdr);
                thdr = NULL;
            }
        }
    }

    return thdr;               /* May be NULL on error */
}

/* Deletes a range from a thdr_t. See asserts below for requirements */
void thdr_delete_range(thdr_t *thdr, int first, int count)
{
    int n;
    void *s, *d;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(first >= 0);

    if (first >= thdr->used)
        return;          /* Nothing to be deleted */
    
    if ((first + count) >= thdr->used)
        count = thdr->used - first;

    if (count <= 0)
        return;          /* Nothing to be deleted */

#ifdef NOTNEEDED    /* Deletion does not change sort state! */
    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
#endif

    /*
     * For all types other than BOOLEAN and OBJ, we can just memmove
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_copy(THDRELEMPTR(thdr, ba_t, 0), first, 
                THDRELEMPTR(thdr, ba_t, 0), first+count,
                thdr->used-(first+count));
        break;

    case TA_ANY:
        /*
         * We have to deal with reference counts here. For the objects
         * we are deleting we need to decrement the reference counts.
         */
        thdr_decr_obj_refs(thdr, first, count);
         
        /* FALLTHRU - now we can just memmove like the other types */
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_BYTE:
        n = count + first;         /* Point beyond deleted elements */
        n = thdr_compute_move(thdr,
                              first, /* Offset to destination */
                              n,     /* Offset to where to copy from (src) */
                              thdr->used - n, /* # elements - src to end */
                              &d,             /* Receive ptr to dest */
                              &s);            /* Receive ptr to src */
        memmove(d, s, n);      /* NOT memcpy since overlapping copy */
        break;
    default:
        ta_type_panic(thdr->type);
    }

    thdr->used -= count;
}

void thdr_delete_indices(thdr_t *thdr, thdr_t *pindices)
{
    int prev;
    int *pindex, *plimit;

    TA_ASSERT(pindices->type == TA_INT);

    /*
     * We have to be careful to delete from back to front so as to not
     * invalidate index positions when earlier ones are deleted
     */
    TA_ASSERT(pindices->sort_order == THDR_SORTED_ASCENDING || pindices->sort_order == THDR_SORTED_DESCENDING);
    
    /* TBD - this will be desperately slow. Fix */
    
    /* We always want to delete back to front. However the index array
     * may be presorted in any direction. So check and loop accordingly
     * Because duplicates might be present, we keep track of last deleted
     * index in variable prev.
     */
    if (pindices->used == 0)
        return;
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        /* Sort order is ascending so iterate index array back to front */
        plimit = THDRELEMPTR(pindices, int, 0);
        pindex = plimit + pindices->used - 1;
        prev = *pindex + 1;     /* Dummy value for first iteration */
        for ( ; pindex >= plimit; --pindex) {
            if (*pindex != prev && *pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            prev = *pindex;
        }
    } else {
        /* Sort order is descending so iterate index array front to back */
        pindex = THDRELEMPTR(pindices, int, 0);
        prev = *pindex + 1;     /* Dummy value for first iteration */
        for (plimit = pindex + pindices->used - 1; pindex <= plimit; ++pindex) {
            if (*pindex != prev && *pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            prev = *pindex;
        }
    }
}

void thdr_reverse(thdr_t *thdr)
{
    int orig_order;

    if (thdr->used == 0)
        return;

    orig_order = thdr->sort_order;
    
#define SWAPALL(thdr_, type_)                                          \
    do {                                                                \
        type_ *front;                                                   \
        type_ *back;                                                    \
        front = THDRELEMPTR((thdr_), type_, 0);                       \
        back  = THDRELEMPTR((thdr_), type_, (thdr_)->used-1);        \
        while (front < back) {                                          \
            type_ temp;                                                 \
            temp = *front;                                              \
            *front++ = *back;                                           \
            *back-- = temp;                                             \
        }                                                               \
    } while (0)

    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_reverse(THDRELEMPTR(thdr, ba_t, 0), 0, thdr->used);
        break;
    case TA_ANY:    SWAPALL(thdr, Tcl_Obj*); break;
    case TA_UINT:   /* Fall thru */
    case TA_INT:    SWAPALL(thdr, int); break;
    case TA_WIDE:   SWAPALL(thdr, Tcl_WideInt); break;
    case TA_DOUBLE: SWAPALL(thdr, double); break;
    case TA_BYTE:   SWAPALL(thdr, unsigned char); break;
    default:
        ta_type_panic(thdr->type);
    }

    switch (orig_order) {
    case THDR_SORTED_ASCENDING: thdr->sort_order = THDR_SORTED_DESCENDING; break;
    case THDR_SORTED_DESCENDING: thdr->sort_order = THDR_SORTED_ASCENDING; break;
    case THDR_SORTED_ASCENDING_NOCASE: thdr->sort_order = THDR_SORTED_DESCENDING_NOCASE; break;
    case THDR_SORTED_DESCENDING_NOCASE: thdr->sort_order = THDR_SORTED_ASCENDING_NOCASE; break;
    }
}


/* Copies partial content from one thdr_t to another. See asserts below
   for requirements */
void thdr_copy(thdr_t *pdst, int dst_first,
               thdr_t *psrc, int src_first, int count, int insert)
{
    int nbytes;
    void *s, *d;
    int new_used;
    int elem_size;

    TA_ASSERT(pdst != psrc);
    TA_ASSERT(pdst->type == psrc->type);
    TA_ASSERT(! thdr_shared(pdst));
    TA_ASSERT(src_first >= 0);

    if (src_first >= psrc->used)
        return;          /* Nothing to be copied */
    if ((src_first + count) > psrc->used)
        count = psrc->used - src_first;
    if (count <= 0)
        return;

    new_used = thdr_recompute_occupancy(pdst, &dst_first, count, insert);
    TA_ASSERT(new_used <= pdst->usable); /* Caller should have ensured */

    pdst->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /*
     * For all types other than BOOLEAN and OBJ, we can just memcpy
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (psrc->type) {
    case TA_BOOLEAN:
        d = THDRELEMPTR(pdst, ba_t, 0);
        if (insert) {
            /* First make room by copying bits up */
            ba_copy(d, dst_first+count, d, dst_first, pdst->used-dst_first);
        }
        /* Now insert or overwrite in place */
        ba_copy(d, dst_first, THDRELEMPTR(psrc, ba_t, 0), src_first, count);
        break;

    case TA_ANY:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment the reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        thdr_incr_obj_refs(psrc, src_first, count); /* Do this first */
        if (! insert) {
            /*
             * Overwriting so decr refs of existing elements.
             * Note this call take care of the case where count exceeds
             * actual number in pdst
             */
            thdr_decr_obj_refs(pdst, dst_first, count);
        }

        /* FALLTHRU - Now we can just move memory like the other types */
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_BYTE:
        if (insert)
            thdr_make_room(pdst, dst_first, count);

        elem_size = pdst->elem_bits / CHAR_BIT;
        nbytes = count * elem_size;
        d = (dst_first * elem_size) + THDRELEMPTR(pdst, char, 0);
        s = (src_first * elem_size) + THDRELEMPTR(psrc, char, 0);
        memcpy(d, s, nbytes);
        break;

    default:
        ta_type_panic(psrc->type);
    }

    TA_ASSERT(new_used <= pdst->usable);
    pdst->used = new_used;
}

/* Copies partial content from one thdr_t to another in reverse.
   See asserts below for requirements */
void thdr_copy_reversed(thdr_t *pdst, int dst_first,
                       thdr_t *psrc, int src_first, int count)
{
    TA_ASSERT(pdst != psrc);
    TA_ASSERT(pdst->type == psrc->type);
    TA_ASSERT(! thdr_shared(pdst));
    TA_ASSERT(src_first >= 0);

    if (src_first >= psrc->used)
        return;          /* Nothing to be copied */
    if ((src_first + count) > psrc->used)
        count = psrc->used - src_first;
    if (count <= 0)
        return;
    TA_ASSERT((dst_first + count) <= pdst->usable);

    if (dst_first < 0)
        dst_first = 0;
    else if (dst_first > pdst->used)
        dst_first = pdst->used;

    pdst->sort_order = THDR_UNSORTED; /* TBD - optimize */

#define COPYREVERSE(type_, pdst_, doff_, psrc_, soff_, count_)          \
    do {                                                                \
        type_ *src;                                                     \
        type_ *dst;                                                     \
        int    i = (count_);                                            \
        src = THDRELEMPTR((psrc_), type_ , (soff_));                  \
        dst  = THDRELEMPTR((pdst_), type_ , 0);                       \
        dst += (doff_ ) + i - 1;                                        \
        while (i--) {                                                   \
            /* Remember caller ensured no overlap between src & dst */  \
            *dst-- = *src++;                                            \
        }                                                               \
    } while (0)

    switch (psrc->type) {
    case TA_BOOLEAN:
        ba_copy(THDRELEMPTR(pdst, ba_t, 0), dst_first,
                THDRELEMPTR(psrc, ba_t, 0), src_first, count);
        ba_reverse(THDRELEMPTR(pdst, ba_t, 0), dst_first, count);
        break;
    case TA_ANY:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment the reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        thdr_incr_obj_refs(psrc, src_first, count); /* Do this first */
        /* Note this call take care of the case where count exceeds
         * actual number in pdst
         */
        thdr_decr_obj_refs(pdst, dst_first, count);
        COPYREVERSE(Tcl_Obj*, pdst, dst_first, psrc, src_first, count);
        break;

    case TA_UINT:
    case TA_INT:
        COPYREVERSE(int, pdst, dst_first, psrc, src_first, count);
        break;
    case TA_WIDE:
        COPYREVERSE(Tcl_WideInt, pdst, dst_first, psrc, src_first, count);
        break;
    case TA_DOUBLE:
        COPYREVERSE(double, pdst, dst_first, psrc, src_first, count);
        break;
    case TA_BYTE:
        COPYREVERSE(unsigned char, pdst, dst_first, psrc, src_first, count);
        break;
    default:
        ta_type_panic(psrc->type);
    }

    if ((dst_first + count) > pdst->used)
        pdst->used = dst_first + count;

    TA_ASSERT(pdst->used <= pdst->usable);

    return;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone(Tcl_Interp *ip, thdr_t *psrc, int minsize)
{
    thdr_t *thdr;

    if (minsize == 0)
        minsize = psrc->usable;
    else if (minsize < psrc->used)
        minsize = psrc->used;

    /* TBD - optimize these two calls */
    thdr = thdr_alloc(ip, psrc->type, minsize);
    if (thdr) {
        thdr_copy(thdr, 0, psrc, 0, psrc->used, 0);
        thdr->sort_order = psrc->sort_order;
    }
    return thdr;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone_reversed(Tcl_Interp *ip, thdr_t *psrc, int minsize)
{
    thdr_t *thdr;
    int orig_order;

    orig_order = psrc->sort_order;

    if (minsize == 0)
        minsize = psrc->usable;
    else if (minsize < psrc->used)
        minsize = psrc->used;

    /* TBD - optimize these two calls */
    thdr = thdr_alloc(ip, psrc->type, minsize);
    if (thdr) {
        thdr_copy_reversed(thdr, 0, psrc, 0, psrc->used);
        switch (orig_order) {
        case THDR_SORTED_ASCENDING: thdr->sort_order = THDR_SORTED_DESCENDING; break;
        case THDR_SORTED_DESCENDING: thdr->sort_order = THDR_SORTED_ASCENDING; break;
        case THDR_SORTED_ASCENDING_NOCASE: thdr->sort_order = THDR_SORTED_DESCENDING_NOCASE; break;
        case THDR_SORTED_DESCENDING_NOCASE: thdr->sort_order = THDR_SORTED_ASCENDING_NOCASE; break;
        }
    }
    return thdr;
}

thdr_t *thdr_range(Tcl_Interp *ip, thdr_t *psrc, int low, int count)
{
    thdr_t *thdr;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    thdr = thdr_alloc(ip, psrc->type, count);
    if (thdr) {
        thdr_copy(thdr, 0, psrc, low, count, 0);
        thdr->sort_order = psrc->sort_order;
    }
    return thdr;
}

Tcl_Obj *tcol_index(Tcl_Interp *ip, Tcl_Obj *tcol, int index)
{
    thdr_t *thdr;

    if (tcol_convert(ip, tcol) != TCL_OK)
        return NULL;
    thdr = TARRAYHDR(tcol);
    if (index < 0 || index >= thdr->used) {
        ta_index_range_error(ip, index);
        return NULL;
    }
    return thdr_index(thdr, index);
}

Tcl_Obj *tcol_range(Tcl_Interp *ip, Tcl_Obj *osrc, int low, int count,
                     int fmt)
{
    int end;
    thdr_t *psrc;
    Tcl_Obj *o;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    psrc = TARRAYHDR(osrc);

    if (fmt == TA_FORMAT_TARRAY) {
        thdr_t *thdr = thdr_range(ip, psrc, low, count);
        return thdr == NULL ? NULL : tcol_new(thdr);
    }

    end = low + count;
    if (end > psrc->used)
        end = psrc->used;

#define tcol_range_COPY(type_, objfn_)                                  \
    do {                                                                \
        type_ *p = THDRELEMPTR(psrc, type_, low);                \
        type_ *pend = THDRELEMPTR(psrc, type_, end);             \
        while (p < pend) {                                              \
            if (fmt == TA_FORMAT_DICT)                                  \
                Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));  \
            Tcl_ListObjAppendElement(ip, o, objfn_(*p++));              \
        }                                                               \
    } while (0)

    /* Even dicts more efficiently built as lists and shimmered as necessary */
    o = Tcl_NewListObj(fmt == TA_FORMAT_DICT ? (2*(end-low)) : (end-low), NULL);
    switch (psrc->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = THDRELEMPTR(psrc, ba_t, 0);
            while (low < end) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low));
                Tcl_ListObjAppendElement(ip, o, 
                                         Tcl_NewIntObj(ba_get(baP, low)));
                ++low;
            }
        }
        break;
    case TA_UINT:
        tcol_range_COPY(unsigned int, Tcl_NewWideIntObj);
        break;
    case TA_INT:
        tcol_range_COPY(int, Tcl_NewIntObj);
        break;
    case TA_WIDE:
        tcol_range_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
        break;
    case TA_DOUBLE:
        tcol_range_COPY(double, Tcl_NewDoubleObj);
        break;
    case TA_BYTE:
        tcol_range_COPY(unsigned char, Tcl_NewIntObj);
        break;
    case TA_ANY:
        tcol_range_COPY(Tcl_Obj*, (Tcl_Obj*)); /* Cast acts as a null function */
        break;
    default:
        ta_type_panic(psrc->type);
    }

    return o;
}


/*
 * Convert a TArray Tcl_Obj to one that is suitable for modifying.
 * The Tcl_Obj must NOT be shared.
 * There are three cases to consider:
 * (1) Even though tcol is unshared, the corresponding thdr_t might
 *     still be shared (pointed to from elsewhere). In this case
 *     also, we clone the thdr_t and store it as the new internal rep.
 * (2) If its thdr_t is unshared, we can modify in
 *     place, unless 
 * (3) thdr_t is too small in which case we have to reallocate it.
 *
 * Invalidates the string rep in all cases.
 * Only fails on memory allocation failure.
 */
TCL_RESULT tcol_make_modifiable(Tcl_Interp *ip,
                                Tcl_Obj *tcol, int minsize, int prefsize)
{
    thdr_t *thdr;

    TA_ASSERT(tcol_affirm(tcol));
    TA_ASSERT(! Tcl_IsShared(tcol));

    thdr = TARRAYHDR(tcol);
    if (prefsize == 0)
        prefsize = thdr->usable;
    if (minsize < thdr->used)
        minsize = thdr->used;
    if (minsize > prefsize)
        prefsize = minsize;

    if (thdr_shared(thdr)) {
        /* Case (1) */
        thdr = thdr_clone(ip, thdr, prefsize);
        if (thdr == NULL)
            return TCL_ERROR;   /* Note tcol is not changed */
        ta_replace_intrep(tcol, thdr);
    } else if (thdr->usable < minsize) {
        /* Case (3). Note don't use ta_set_intrep as we are keeping all 
           fields and ref counts the same */
        Tcl_InvalidateStringRep(tcol);
        thdr = thdr_realloc(ip, thdr, prefsize);
        if (thdr)
            TARRAYHDR(tcol) = thdr;
        else
            return TCL_ERROR;   /* Note tcol is not changed */
    } else {
        /* Case (2) - just reuse, invalidate the string rep */
        Tcl_InvalidateStringRep(tcol);
    }

    return TCL_OK;
}


/* Returns a Tcl_Obj for a TArray slot. NOTE: WITHOUT its ref count incremented */
Tcl_Obj * thdr_index(thdr_t *thdr, int index)
{
    TA_ASSERT(index >= 0 && index < thdr->used);

    switch (thdr->type) {
    case TA_BOOLEAN:
        return Tcl_NewIntObj(ba_get(THDRELEMPTR(thdr, ba_t, 0), index));
    case TA_UINT:
        return Tcl_NewWideIntObj(*THDRELEMPTR(thdr, unsigned int, index));
    case TA_INT:
        return Tcl_NewIntObj(*THDRELEMPTR(thdr, int, index));
    case TA_WIDE:
        return Tcl_NewWideIntObj(*THDRELEMPTR(thdr, Tcl_WideInt, index));
    case TA_DOUBLE:
        return Tcl_NewDoubleObj(*THDRELEMPTR(thdr, double, index));
    case TA_BYTE:
        return Tcl_NewIntObj(*THDRELEMPTR(thdr, unsigned char, index));
    case TA_ANY:
        return *THDRELEMPTR(thdr, Tcl_Obj *, index);
    default:
        ta_type_panic(thdr->type);
        return NULL;
    }
}

/*
 * Converts the passed Tcl_Obj o to integer indexes. If a single index
 * stores it in *pindex and returns TA_INDEX_TYPE_INT. If multiple indices,
 * stores a thdr_t of type int containing the indices into *thdrP and
 * returns TA_INDEX_TYPE_THDR. The thdr_t's ref count is incremented
 * so caller should call thdr_decr_refs as appropriate.
 *
 * If pindex is NULL, always returns as TA_INDEX_TYPE_THDR.
 *
 * This facility to return a single int or a index list should only
 * be used by commands where it does not matter whether {1} is treated
 * as a list or an int, for example the fill command, and the distinction
 * is just an efficiency issue. Commands should
 * not use this to pick whether a single index or a list was specified
 * if it impacts their semantics.
 */
int ta_obj_to_indices(Tcl_Interp *ip, Tcl_Obj *o,
                      int want_sorted,
                      int end,        /* Value to use for "end",
                                         ignored if pindex == NULL */
                      thdr_t **thdrP, /* Cannot be NULL */
                      int *pindex)    /* Can be NULL */
{
    thdr_t *thdr;
    Tcl_Obj **elems;
    int       n;
    TCL_RESULT status;

    /*
     * For efficiencies sake, we need to avoid shimmering. So we first
     * check for specific types and default to a list otherwise.
     */
    if (tcol_affirm(o)) {
        if (tcol_type(o) == TA_INT) {
            thdr = TARRAYHDR(o);
            if (want_sorted && thdr->sort_order == THDR_UNSORTED) {
                thdr = thdr_clone(ip, thdr, thdr->used);
                if (thdr == NULL)
                    return TA_INDEX_TYPE_ERROR;
                qsort(THDRELEMPTR(thdr, int, 0), thdr->used, sizeof(int), intcmp);
                thdr->sort_order = THDR_SORTED_ASCENDING;
            }
            thdr->nrefs++;
            *thdrP = thdr;
            return TA_INDEX_TYPE_THDR;
        } else {
            /* TBD - write conversion from other type tarrays */
            ta_indices_error(ip, o);
            return TA_INDEX_TYPE_ERROR;
        }
    }

    /* If pindex is NULL, caller never wants to be treated as a single index */
    if (pindex != NULL) {
        /* To prevent shimmering, first check known to be a list */
        if (o->typePtr != g_tcl_list_type_ptr && pindex != NULL) {
            status = ta_get_int_from_obj(NULL, o, &n);
            if (status == TCL_OK) {
                *pindex = n;
                return TA_INDEX_TYPE_INT;
            }
            if (ta_strequal(Tcl_GetString(o), "end")) {
                *pindex = end;
                return TA_INDEX_TYPE_INT;
            }
        }
        /* else fall through to try as list */
    }

    if (Tcl_ListObjGetElements(NULL, o, &n, &elems) != TCL_OK) {
        ta_indices_error(ip, o);
        return TA_INDEX_TYPE_ERROR;
    }

    /* Even as a list, it may be the single element "end". Check for that */
    if (n == 1) {
        if (ta_strequal(Tcl_GetString(elems[0]), "end")) {
            if (pindex != NULL) {
                *pindex = end;
                return TA_INDEX_TYPE_INT;
            }
        }            
    }

    thdr = thdr_alloc_and_init(ip, TA_INT, n, elems, 0);
    if (thdr) {
        if (want_sorted) {
            qsort(THDRELEMPTR(thdr, int, 0), thdr->used, sizeof(int), intcmp);
            thdr->sort_order = THDR_SORTED_ASCENDING;
        }
        thdr->nrefs++;
        *thdrP = thdr;
        return TA_INDEX_TYPE_THDR;
    } else
        return TA_INDEX_TYPE_ERROR;
}

/* Returns a newly allocated thdr_t (with ref count 0) containing the
   values from the specified indices */
Tcl_Obj *tcol_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, int fmt)
{
    thdr_t *psrc;
    thdr_t *thdr;
    int count, index, bound;
    int *pindex, *end;
    Tcl_Obj *tcol;
    void *srcbase, *thdrbase;

    if (tcol_convert(ip, osrc) != TCL_OK)
        return NULL;

    TA_ASSERT(pindices->type == TA_INT);
    count = pindices->used;

    psrc = TARRAYHDR(osrc);
    if (fmt == TA_FORMAT_TARRAY) {
        thdr = thdr_alloc(ip, psrc->type, count);
        if (thdr == NULL)
            return NULL;
        thdrbase = THDRELEMPTR(thdr, unsigned char, 0);
        tcol = tcol_new(thdr);
    } else {
        thdr = NULL;
        thdrbase = NULL;
        tcol = Tcl_NewListObj(fmt == TA_FORMAT_LIST ? count : 2*count, NULL);
    }
    if (count == 0)
        return tcol;           /* Empty index list so nothing to return */

#define tcol_get_COPY(type_, objfn_)                                    \
    do {                                                                \
        type_ *fromP = srcbase;                                         \
        type_ *tpobjs = thdrbase;                                       \
        switch (fmt) {                                                  \
        case TA_FORMAT_TARRAY:                                          \
            while (pindex < end) {                                      \
                index = *pindex++;                                      \
                if (index < 0 || index >= bound)                        \
                    goto index_error;                                   \
                *tpobjs++ = fromP[index];                               \
            }                                                           \
            thdr->used = count;                                         \
            break;                                                      \
        case TA_FORMAT_LIST:                                            \
            while (pindex < end) {                                      \
                index = *pindex++;                                      \
                if (index < 0 || index >= bound)                        \
                    goto index_error;                                   \
                Tcl_ListObjAppendElement(ip, tcol,                      \
                                         objfn_(fromP[index]));         \
            }                                                           \
            break;                                                      \
        case TA_FORMAT_DICT:                                            \
            while (pindex < end) {                                      \
                index = *pindex++;                                      \
                if (index < 0 || index >= bound)                        \
                    goto index_error;                                   \
                Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index)); \
                Tcl_ListObjAppendElement(ip, tcol,                      \
                                         objfn_(fromP[index]));         \
            }                                                           \
            break;                                                      \
        }                                                               \
    } while (0)

    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + count;
    srcbase = THDRELEMPTR(psrc, unsigned char, 0);
    bound = psrc->used;
    switch (psrc->type) {
    case TA_BOOLEAN:
        {
            ba_t *srcbaP = srcbase;
            ba_t *baP = thdrbase;
            int i;
            if (fmt == TA_FORMAT_TARRAY) {
                for (i = 0; pindex < end; ++i, ++pindex) {
                    index = *pindex; 
                    if (index < 0 || index >= bound)
                        goto index_error;
                    ba_put(baP, i, ba_get(srcbaP, index));
                }
                thdr->used = count;             \
            } else {
                for (i = 0; pindex < end; ++i, ++pindex) {
                    index = *pindex; 
                    if (index < 0 || index >= bound)
                        goto index_error;
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol,
                                             Tcl_NewIntObj(ba_get(srcbaP, index)));
                }
            }
        }
        break;
    case TA_UINT:
        tcol_get_COPY(unsigned int, Tcl_NewWideIntObj);
        break;
    case TA_INT:
        tcol_get_COPY(int, Tcl_NewIntObj);
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
    case TA_ANY:
        /* Cannot use macro here because of ref counts etc. */
        {
            Tcl_Obj **srcobjs = srcbase;
            Tcl_Obj **pobjs = thdrbase;
            while (pindex < end) {
                index = *pindex++; 
                if (index < 0 || index >= bound)
                    goto index_error;
                if (fmt == TA_FORMAT_TARRAY) {
                    *pobjs = srcobjs[index];
                    Tcl_IncrRefCount(*pobjs);
                    ++pobjs;
                    thdr->used++; /* Bump as we go along in case tcol has
                                      to be released on error */
                } else {
                    /* No need to bump ref counts as lists take care of it */
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol, srcobjs[index]);
                }
            }
        }
        break;
    default:
        ta_type_panic(psrc->type);
    }

    return tcol;

index_error:   /* index should hold the current index in error */
    ta_index_range_error(ip, index);

    if (tcol)
        Tcl_DecrRefCount(tcol);
    return NULL;

}

/* See asserts for conditions */
TCL_RESULT tcol_delete(Tcl_Interp *ip, Tcl_Obj *tcol,
                        Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int low, count;
    int status;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    status = tcol_make_modifiable(ip, tcol, tcol_occupancy(tcol), 0);
    if (status == TCL_OK) {
        thdr_t *thdr = TARRAYHDR(tcol);
        if (indexb) {
            status = ta_fix_range_bounds(ip, thdr->used, indexa,
                                             indexb, &low, &count);
            if (status == TCL_OK)
                thdr_delete_range(thdr, low, count);
        } else {
            /* Not a range, either a list or single index */
            thdr_t *pindices;
            /* Note status is TCL_OK at this point */
            switch (ta_obj_to_indices(ip, indexa, 1, thdr->used-1,
                                      &pindices, &low)) {
            case TA_INDEX_TYPE_ERROR:
                status = TCL_ERROR;
                break;
            case TA_INDEX_TYPE_INT:
                if (low >= 0) 
                    thdr_delete_range(thdr, low, 1);
                break;
            case TA_INDEX_TYPE_THDR:
                thdr_delete_indices(thdr, pindices);
                thdr_decr_refs(pindices);
                break;
            }
        }
    }

    return status;
}

TCL_RESULT tcol_insert_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                           Tcl_Obj *opos, Tcl_Obj *ocount)
{
    int status;
    TA_ASSERT(! Tcl_IsShared(tcol));

    if (ocount == NULL) {
        /* Values may be given as a column or a list */
        if ((status = tcol_convert(NULL, ovalue)) == TCL_OK)
            status = tcol_copy_thdr(ip, tcol, TARRAYHDR(ovalue), opos, 1);
        else
            status = tcol_put_objs(ip, tcol, ovalue, opos, 1);
    } else {
        int pos, count, used;
        if ((status = ta_get_int_from_obj(ip, ocount, &count)) == TCL_OK &&
            (status = tcol_convert(ip, tcol)) == TCL_OK) {
            used = tcol_occupancy(tcol);
            if (count > 0) {
                if ((status = tcol_make_modifiable(ip, tcol, count+used, 0)) == TCL_OK &&
                    (status = ta_convert_index(ip, opos, &pos, used,
                                           0, used)) == TCL_OK) {
                    ta_value_t tav;
                    if ((status = ta_value_from_obj(ip, ovalue,
                                                    tcol_type(tcol), &tav)) == TCL_OK)
                        thdr_fill_range(ip, TARRAYHDR(tcol), &tav, pos, count, 1);
                }
            } else if (count < 0) {
                status = ta_bad_count_error(ip, count);
            } else {
                status = TCL_OK; /* count == 0, nothing to do */
            }
        }
    }
    return status;
}


TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                         Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int low, count, nelems;
    int status;
    ta_value_t value;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if ((status = ta_value_from_obj(ip, ovalue,
                                     tcol_type(tcol), &value)) != TCL_OK)
        return status;

    nelems = tcol_occupancy(tcol);
    if (indexb) {
        status = ta_fix_range_bounds(ip, nelems, indexa, indexb, &low, &count);
        if (status == TCL_OK && count != 0) {
            status = tcol_make_modifiable(ip, tcol, low+count, 0);
            if (status == TCL_OK)
                thdr_fill_range(ip, TARRAYHDR(tcol), &value, low, count, 0);
        }
    } else {
        /* Not a range, either a list or single index */
        thdr_t *pindices;
        /* Note status is TCL_OK at this point */
        switch (ta_obj_to_indices(ip, indexa, 1, nelems-1, &pindices, &low)) {
        case TA_INDEX_TYPE_ERROR:
            status = TCL_ERROR;
            break;
        case TA_INDEX_TYPE_INT:
            if (low < 0 || low > nelems) {
                ta_index_range_error(ip, low);
                status = TCL_ERROR;
            } else {
                status = tcol_make_modifiable(ip, tcol, low+1, 0);
                if (status == TCL_OK)
                    thdr_fill_range(ip, TARRAYHDR(tcol), &value, low, 1, 0);
            }
            break;
        case TA_INDEX_TYPE_THDR:
            status = thdr_verify_indices(ip, TARRAYHDR(tcol), pindices, &count);
            if (status == TCL_OK && count > 0) {
                status = tcol_make_modifiable(ip, tcol, count, count); // TBD - count + extra?
                if (status == TCL_OK)
                    thdr_fill_indices(ip, TARRAYHDR(tcol), &value, pindices, count);
            }
            thdr_decr_refs(pindices);
            break;
        }
    }

    return status;
}

TCL_RESULT tcol_reverse(Tcl_Interp *ip, Tcl_Obj *tcol)
{
    thdr_t *thdr;
    int status;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (thdr_shared(TARRAYHDR(tcol))) {
        thdr = thdr_clone_reversed(ip, TARRAYHDR(tcol), 0);
        if (thdr == NULL)
            return TCL_ERROR;
        ta_replace_intrep(tcol, thdr);
    } else {
        thdr_reverse(TARRAYHDR(tcol));
        Tcl_InvalidateStringRep(tcol);
    }
    return TCL_OK;
}

int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;
    int comparison;

    /* TBD - maybe check first letter before calling ? But be careful of case-insensitivity*/
#if 0
    int alen, blen, len;

    a = Tcl_GetStringFromObj(oaP, &alen);
    b = Tcl_GetStringFromObj(obP, &blen);
    
    // This is much slower than the strcmp method below but might be more
    // "correct". On the other hand the strcmp method below is both faster
    // and Tcl compatible with lsort and lsearch
    alen = Tcl_NumUtfChars(a, alen); /* Num bytes -> num chars */
    blen = Tcl_NumUtfChars(b, blen); /* Num bytes -> num chars */

    len = alen < blen ? alen : blen; /* len is the shorter length */
    comparison = (ignorecase ? Tcl_UtfNcasecmp : Tcl_UtfNcmp)(a, b, len);
    if (comparison == 0) {
        comparison = alen-blen;
    }

#else
    a = oaP->bytes;
    if (a == NULL)
        a = Tcl_GetString(oaP);
    b = obP->bytes;
    if (b == NULL)
        b = Tcl_GetString(obP);

    /* Following Tcl's lsort we just use the Unicode code point collation
       order which means a simple strcmp suffices */
    /* TBD - Tcl has fixed this post-8.6. Check and change this accordingly */
    if (ignorecase) {
        comparison = _stricmp(a, b);
    } else {
        if (*a == *b)
            comparison = strcmp(a, b);
        else
            comparison = *(unsigned char *)a - *(unsigned char *)b;
    }
#endif

    return (comparison > 0) ? 1 : ((comparison < 0) ? -1 : 0);
}

TCL_RESULT tcol_copy_thdr(Tcl_Interp *ip, Tcl_Obj *tcol, thdr_t *psrc,
                          Tcl_Obj *ofirst, /* NULL -> end */
                          int insert)
{
    int first, status, cur_used;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if (tcol_type(tcol) != psrc->type)
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    cur_used = tcol_occupancy(tcol);
    first = cur_used; /* By default, append */
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &first, first, 0, first);
    if (status == TCL_OK && psrc->used) {
        status = tcol_make_modifiable(ip, tcol,
                                      psrc->used + (insert ? cur_used : first),
                                      0);
        if (status == TCL_OK)
            thdr_copy(TARRAYHDR(tcol), first, psrc, 0, psrc->used, insert); 
    }
    return status;
}

TCL_RESULT tcol_put_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                         Tcl_Obj *ovalues,
                         Tcl_Obj *ofirst, /* NULL -> end */
                         int insert)
{
    int status;
    Tcl_Obj **values;
    int nvalues;
    int n, cur_used;

    TA_ASSERT(! Tcl_IsShared(tcol));

    status = Tcl_ListObjGetElements(ip, ovalues, &nvalues, &values);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    /* Get the limits of the range to set */

    cur_used = tcol_occupancy(tcol);
    n = cur_used;
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &n, n, 0, n);
    /* n contains starting offset */
    if (status == TCL_OK && nvalues) {
        /* Note this also invalidates the string rep as desired */
        status = tcol_make_modifiable(ip, tcol,
                                      nvalues + (insert ? cur_used : n),
                                      0);
        if (status == TCL_OK) {
            /* Note even on error thdr_put_objs guarantees a consistent 
             * and unchanged tcol
             */
            status = thdr_put_objs(ip, TARRAYHDR(tcol),
                                   n, nvalues, values, insert);
        }
    }
    
    return status;
}

TCL_RESULT tcol_place_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                           Tcl_Obj *ovaluelist,
                           Tcl_Obj *oindices)
{
    int new_size;
    int status;
    thdr_t *pindices;
    Tcl_Obj **ovalues;
    int nvalues;

    TA_ASSERT(! Tcl_IsShared(tcol));

    status = Tcl_ListObjGetElements(ip, ovaluelist, &nvalues, &ovalues);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL)
        != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        if (pindices->used > nvalues)
            status = ta_indices_count_error(ip, pindices->used, nvalues);
        else {
            status = ta_verify_value_objs(ip, tcol_type(tcol), nvalues, ovalues);
            if (status == TCL_OK) {
                status = thdr_verify_indices(ip, TARRAYHDR(tcol), pindices, &new_size);
                if (status == TCL_OK) {
                    status = tcol_make_modifiable(ip, tcol, new_size, new_size); // TBD - count + extra?
                    thdr_place_objs(ip, TARRAYHDR(tcol), pindices,
                                    new_size,
                                    nvalues, ovalues);
                }
            }
        }
    }

    thdr_decr_refs(pindices);
    return status;
}

TCL_RESULT tcol_place_indices(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *osrc,
                              Tcl_Obj *oindices)
{
    int new_size;
    int status;
    thdr_t *pindices, *psrc;

    TA_ASSERT(! Tcl_IsShared(tcol));
    TA_ASSERT(osrc->typePtr == &g_tcol_type);

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    psrc = TARRAYHDR(osrc);
    if (psrc->type != tcol_type(tcol))
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        if (pindices->used > psrc->used)
            status = ta_indices_count_error(ip, pindices->used, psrc->used);
        else {
            status = thdr_verify_indices(ip, TARRAYHDR(tcol), pindices, &new_size);
            if (status == TCL_OK) {
                status = tcol_make_modifiable(ip, tcol, new_size, new_size); // TBD - count + extra?
                thdr_place_indices(ip, TARRAYHDR(tcol), psrc, pindices, new_size);
            }
        }
    }
    thdr_decr_refs(pindices);

    return status;
}

