/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include <string.h>
#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#include "tarray.h"

/*
 * TArray is a Tcl "type" used for densely storing arrays of elements
 * of a specific type. For reasons of efficiency in type checking,
 * we define two types - tcolumn that holds an array of elements,
 * and tgrid, which is just a tcolumn (and hence uses the same member
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
struct Tcl_ObjType g_tgrid_type = {
    "tgrid",
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
    "tclobj",
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

void ta_type_panic(unsigned char tatype)
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

TCL_RESULT ta_value_type_error(Tcl_Interp *ip, Tcl_Obj *o, int tatype)
{
    if (ip) {
        Tcl_SetObjResult(ip,
                         Tcl_ObjPrintf("Value %s not valid for typed array of type %s.",
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
                         Tcl_ObjPrintf("Row or grid width %d less than destination width %d.", row_width, min_width));
        Tcl_SetErrorCode(ip, "TARRAY", "ROW", "WIDTH", NULL);
    }
    return TCL_ERROR;
}

TCL_RESULT ta_grid_length_error(Tcl_Interp *ip)
{
    if (ip) {
        Tcl_SetResult(ip,
                      "Columns in tarray grid have differing lengths.",
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

/*
 * Map numeric or string index to numeric integer index.
 */
TCL_RESULT ta_convert_index(Tcl_Interp *ip, Tcl_Obj *o, int *pindex, int end_value, int low, int high)
{
    char *s;
    int val;

    /* Do type checks to avoid expensive shimmering in case of errors */
    if (tcol_affirm(o))
        return ta_index_error(ip, o);

    if (o->typePtr == g_tcl_list_type_ptr) {
        if (Tcl_ListObjLength(NULL, o, &val) != TCL_OK || val != 1)
            return ta_index_error(ip, o);
    }

    if (Tcl_GetIntFromObj(NULL, o, &val) != TCL_OK) {
        s = Tcl_GetString(o);
        if (strcmp(s, "end")) {
            return ta_index_error(ip, o);
        }
        val = end_value;
    }
    
    if (val < low || val > high)
        return ta_index_range_error(ip, val);
    else {
        *pindex = val;
        return TCL_OK;
    }
}

/* low has to be between 0 and one beyond last element.
   high is 0-INT_MAX
   if (high < low) count is returned as 0 (not an error)
*/
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *ip, const thdr_t *thdr, Tcl_Obj *olow, Tcl_Obj *ohigh, int *plow, int *pcount)
{
    int low, high;

    /* TBD - We allow low index to be 1 greater than last element. Caller should
     * check for this if appropriate. High index can be any greater value
     * than lower range.
     */
    if (ta_convert_index(ip, olow, &low, thdr->used-1, 0, thdr->used) != TCL_OK)
        return TCL_ERROR;

    if (ta_convert_index(ip, ohigh, &high, thdr->used-1, 0, INT_MAX) != TCL_OK)
        return TCL_ERROR;

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


TCL_RESULT tgrid_convert_from_other(Tcl_Interp *ip, Tcl_Obj *o)
{
    int status;
    thdr_t *thdr;
    Tcl_Obj **ptcol;
    Tcl_Obj **end;
    int nelems;

    if ((status = tcol_convert(ip, o)) != TCL_OK)
        return status;
    thdr = TARRAYHDR(o);
    if (thdr->type != TA_OBJ)
        return ta_bad_type_error(ip, thdr);
    if (tcol_occupancy(o) == 0)
        return TCL_OK;

    ptcol = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    nelems = tcol_occupancy(*ptcol);
    end = thdr->used + ptcol;
    while (ptcol < end) {
        if ((status = tcol_convert(ip, *ptcol)) != TCL_OK)
            return status;
        /* All must have same number of elements */
        if (tcol_occupancy(*ptcol) != nelems)
            return ta_grid_length_error(ip);
        ++ptcol;
    }
    
    o->typePtr = &g_tgrid_type;
    return TCL_OK;
}


TCL_RESULT ta_value_from_obj(Tcl_Interp *ip, Tcl_Obj *o,
                              unsigned char tatype, ta_value_t *ptav)
{
    int i;
    switch (tatype) {
    case TA_BOOLEAN:
        if (Tcl_GetBooleanFromObj(ip, o, &i) != TCL_OK)
            return TCL_ERROR;
        ptav->bval = (i != 0);
        break;
    case TA_BYTE:
    case TA_INT:
        if (Tcl_GetIntFromObj(ip, o, &ptav->ival) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TA_INT)
            break;
        if (ptav->ival > 255 || ptav->ival < 0)
            return ta_value_type_error(ip, o, tatype);
        ptav->ucval = (unsigned char) ptav->ival;
        break;
    case TA_UINT:
    case TA_WIDE:
        if (Tcl_GetWideIntFromObj(ip, o, &ptav->wval) != TCL_OK)
            return TCL_ERROR;
        if (tatype == TA_WIDE)
            break;
        if (ptav->wval < 0 || ptav->wval > 0xFFFFFFFF)
            return ta_value_type_error(ip, o, tatype);
        ptav->uival = (unsigned int) ptav->wval;
        break;
    case TA_DOUBLE:
        if (Tcl_GetDoubleFromObj(ip, o, &ptav->dval) != TCL_OK)
            return TCL_ERROR;
        break;
    case TA_OBJ:
        ptav->oval = o;
        break;
    default:
        ta_type_panic(tatype);
    }

    ptav->type = tatype;
    return TCL_OK;
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

    thdr->sort_order = THDR_UNSORTED;
    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_fill(THDRELEMPTR(thdr, ba_t, 0), pos, count, ptav->bval);
        break;
    case TA_INT:
    case TA_UINT:
        if (ptav->ival == 0) {
            memset(THDRELEMPTR(thdr, int, pos), 0, count*sizeof(int));
        } else {
            int *pint;
            pint = THDRELEMPTR(thdr, int, pos);
            for (i = 0; i < count; ++i, ++pint)
                *pint = ptav->ival;
        }
        break;
        
    case TA_BYTE:
        memset(THDRELEMPTR(thdr, unsigned char, pos), ptav->ucval, count);
        break;

    case TA_WIDE:
        if (ptav->wval == 0) {
            memset(THDRELEMPTR(thdr, Tcl_WideInt, pos), 0, count*sizeof(Tcl_WideInt));
        } else {
            Tcl_WideInt *pwide;
            pwide = THDRELEMPTR(thdr, Tcl_WideInt, pos);
            for (i = 0; i < count; ++i, ++pwide)
                *pwide = ptav->wval;
        }
        break;
    case TA_DOUBLE:
        {
            double *pdbl;
            pdbl = THDRELEMPTR(thdr, double, pos);
            for (i = 0; i < count; ++i, ++pdbl)
                *pdbl = ptav->dval;
        }
        break;
    case TA_OBJ:
        {
            Tcl_Obj **pobjs;
            int n;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts.
             */
            /* First loop overwriting existing elements */
            n = pos + count;
            if (n > thdr->used)
                n = thdr->used;
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, pos);
            for (i = pos; i < n; ++i) {
                /* Be careful of the order */
                Tcl_IncrRefCount(ptav->oval);
                Tcl_DecrRefCount(*pobjs);
                *pobjs = ptav->oval;
            }

            /* Now loop over new elements being appended */
            for (; i < pos+count; ++i) {
                Tcl_IncrRefCount(ptav->oval);
                *pobjs = ptav->oval;
            }
        }
        break;
    default:
        ta_type_panic(thdr->type);
    }

    thdr->used = new_used;
}

TCL_RESULT ta_verify_value_objs(Tcl_Interp *ip, int tatype,
                                int nelems, Tcl_Obj * const elems[])
{
    Tcl_Obj * const *pobjs = elems;
    Tcl_Obj * const *end = elems + nelems;
    switch (tatype) {
    case TA_BOOLEAN:
        for ( ; pobjs < end; ++pobjs) {
            int ival;
            if (Tcl_GetBooleanFromObj(ip, *pobjs, &ival) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_UINT:
        for ( ; pobjs < end; ++pobjs) {
            Tcl_WideInt wide;
            if (Tcl_GetWideIntFromObj(ip, *pobjs, &wide) != TCL_OK)
                return TCL_ERROR;
            if (wide < 0 || wide > 0xFFFFFFFF) {
                return ta_value_type_error(ip, *pobjs, tatype);
            }
        }
        break;

    case TA_INT:
        for ( ; pobjs < end; ++pobjs) {
            int ival;
            if (Tcl_GetIntFromObj(ip, *pobjs, &ival) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_WIDE:
        for ( ; pobjs < end; ++pobjs) {
            Tcl_WideInt wide;
            if (Tcl_GetWideIntFromObj(ip, *pobjs, &wide) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_DOUBLE:
        for ( ; pobjs < end; ++pobjs) {
            double dval;
            if (Tcl_GetDoubleFromObj(ip, *pobjs, &dval) != TCL_OK)
                return TCL_ERROR;
        }
        break;

    case TA_BYTE:
        for ( ; pobjs < end; ++pobjs) {
            int ival;
            if (Tcl_GetIntFromObj(ip, *pobjs, &ival) != TCL_OK)
                return TCL_ERROR;
            if (ival > 255 || ival < 0) {
                ta_value_type_error(ip, *pobjs, tatype);
                return TCL_ERROR;
            }
        }
        break;
    case TA_OBJ:
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
   Returns new size needed if array has to be grown or -1 on error.
*/
TCL_RESULT thdr_verify_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *pindices, int *new_sizeP)
{
    int i, new_size;
    int *pindex, *end;

    TA_ASSERT(pindices->type == TA_INT);
    TA_ASSERT(thdr->sort_order == THDR_SORTED_ASCENDING || thdr->sort_order == THDR_SORTED_DESCENDING);

    new_size = thdr->used;
    pindex = THDRELEMPTR(pindices, int, 0);
    end = THDRELEMPTR(pindices, int, pindices->used);
    /* Make sure no gaps in indices */
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        while (pindex < end) {
            i = *pindex++;
            if (i < new_size)
                continue;
            if (i > new_size)
                return ta_index_range_error(ip, i);
            new_size = i+1;       /* Appending without a gap */
        }
    } else {
        while (pindex < end) {
            i = *--end;
            if (i < new_size)
                continue;
            if (i > new_size) {
                ta_index_range_error(ip, i);
                return -1;
            }
            new_size = i+1;       /* Appending without a gap */
        }
    }
    *new_sizeP = new_size;
    return TCL_OK;
}

/* thdr must be large enough for largest index. And see asserts in code */
void thdr_fill_indices(Tcl_Interp *ip, thdr_t *thdr, 
                      const ta_value_t *ptav, thdr_t *pindices)
{
    int highest_index;
    int *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(pindices->type == TA_INT);
    TA_ASSERT(pindices->sort_order == THDR_SORTED_ASCENDING || pindices->sort_order == THDR_SORTED_DESCENDING);

    if (pindices->used == 0)
        return;          /* Nothing to do */

    /* Rest of code assumes > 0 indices */

    pindex = THDRELEMPTR(pindices, int, 0);
    end = THDRELEMPTR(pindices, int, pindices->used);

    /* Caller guarantees room for highest index value */
    highest_index = pindices->sort_order > 0 ? end[-1] : pindex[0];
    TA_ASSERT(highest_index < thdr->usable);

    thdr->sort_order = THDR_UNSORTED;
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            while (pindex < end)
                ba_put(baP, *pindex++, ptav->bval);
        }
        break;
    case TA_INT:
    case TA_UINT:
        {
            int *pint = THDRELEMPTR(thdr, int, 0);
            while (pindex < end)
                pint[*pindex++] = ptav->ival;
        }
        break;
    case TA_BYTE:
        {
            unsigned char *ucP = THDRELEMPTR(thdr, unsigned char, 0);
            while (pindex < end)
                ucP[*pindex++] = ptav->ucval;
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *pwide;
            pwide = THDRELEMPTR(thdr, Tcl_WideInt, 0);
            while (pindex < end)
                pwide[*pindex] = ptav->wval;
        }
        break;
    case TA_DOUBLE:
        {
            double *pdbl;
            pdbl = THDRELEMPTR(thdr, double, 0);
            while (pindex < end)
                pdbl[*pindex] = ptav->dval;
        }
        break;
    case TA_OBJ:
        {
            Tcl_Obj **pobjs;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts. Note that
             * indices may be sorted in either order.
             */
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);
            while (pindex < end) {
                Tcl_IncrRefCount(ptav->oval);
                if (*pindex < thdr->used)
                    Tcl_DecrRefCount(pobjs[*pindex]);
                pobjs[*pindex] = ptav->oval;
            }
        }
        break;
    default:
        ta_type_panic(thdr->type);
    }

    if (highest_index >= thdr->used)
        thdr->used = highest_index + 1;
}



/* Increments the ref counts of Tcl_Objs in a tarray making sure not
   to run past end of array */
void thdr_incr_obj_refs(thdr_t *thdr, int first, int count)
{
    register int i;
    register Tcl_Obj **pobjs;

    if (thdr->type == TA_OBJ) {
        if ((first + count) > thdr->used)
            count = thdr->used - first;
        if (count <= 0)
            return;
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
        for (i = 0; i < count; ++i, ++pobjs) {
            Tcl_IncrRefCount(*pobjs);
        }
    }
}

/* Decrements the ref counts of Tcl_Objs in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void thdr_decr_obj_refs(thdr_t *thdr, int first, int count)
{
    register int i;
    register Tcl_Obj **pobjs;

    if (thdr->type == TA_OBJ) {
        if ((first + count) > thdr->used)
            count = thdr->used - first;
        if (count <= 0)
            return;
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
        for (i = 0; i < count; ++i, ++pobjs) {
            Tcl_DecrRefCount(*pobjs);
        }
    }
}

void thdr_free(thdr_t *thdr)
{
    if (thdr->type == TA_OBJ) {
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
    int i, length, bytesNeeded = 0;
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

    bytesNeeded +=
        sizeof("tarray ") - 1 /* -1 to exclude the null */
        + sizeof(" {") - 1 /* Start of list minus trailing null */
        + 1               /* Trailing "}" */
        + strlen(g_type_tokens[TA_OBJ]);
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = TCL_DONT_QUOTE_HASH;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if (bytesNeeded < 0)
            ta_string_overflow_panic("ta_type_update_string_for_objtype");
    }
    if (bytesNeeded > INT_MAX - objc + 1)
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
    strcpy(dst, g_type_tokens[TA_OBJ]);
    dst += strlen(g_type_tokens[TA_OBJ]);
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
            TA_ASSERT((dst-o->bytes) < bytesNeeded);
        }
        dst[-1] = '}';
    } else
        *dst++ = '}';
    *dst = '\0';
    TA_ASSERT((dst-o->bytes) < bytesNeeded);
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
        _snprintf(cP, min_needed, "tarray %s {}",
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
            register ba_t ba = *baP;
            register ba_t ba_mask;

            /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
               defined as ckalloc */
            cP = ckalloc(min_needed + 2*count - 1);
            n = _snprintf(cP, min_needed, "tarray %s {",
                      g_type_tokens[TA_BOOLEAN]);
            TA_ASSERT(n > 0 && n < min_needed);
            o->bytes = cP;
            cP += n;
            n = count / BA_UNIT_SIZE;
            for (i = 0; i < n; ++i, ++baP) {
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
                
    case TA_OBJ:
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
    _snprintf(cP, prefix_len+1, "tarray %s {", g_type_tokens[thdr->type]);
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
                    n = _snprintf(cP, unused, fmt, *intP++);
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
                    n = _snprintf(cP, unused, "%" TCL_LL_MODIFIER "d", *pwide++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_DOUBLE:
            /* Do not use _snprintf because of slight difference
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
                    n = _snprintf(cP, unused, "%u", *ucP++);
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
    Tcl_WideInt wide;
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
    case TA_UINT:
        {
            register unsigned int *uintP;
            uintP = THDRELEMPTR(thdr, unsigned int, first);
            for (i = 0; i < nelems; ++i, ++uintP) {
                if (Tcl_GetWideIntFromObj(ip, elems[i], &wide) != TCL_OK)
                    goto convert_error;
                if (wide < 0 || wide > 0xFFFFFFFF) {
                    ta_value_type_error(ip, elems[i], thdr->type);
                    goto convert_error;
                }
                *uintP = (unsigned int) wide;
            }
        }
        break;
    case TA_INT:
        {
            register int *intP;
            intP = THDRELEMPTR(thdr, int, first);
            for (i = 0; i < nelems; ++i, ++intP) {
                if (Tcl_GetIntFromObj(ip, elems[i], intP) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_WIDE:
        {
            register Tcl_WideInt *pwide;
            pwide = THDRELEMPTR(thdr, Tcl_WideInt, first);
            for (i = 0; i < nelems; ++i, ++pwide) {
                if (Tcl_GetWideIntFromObj(ip, elems[i], pwide) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_DOUBLE:
        {
            register double *pdbl;
            pdbl = THDRELEMPTR(thdr, double, first);
            for (i = 0; i < nelems; ++i, ++pdbl) {
                if (Tcl_GetDoubleFromObj(ip, elems[i], pdbl) != TCL_OK)
                    goto convert_error;
            }
        }
        break;

    case TA_OBJ:
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

    case TA_BYTE:
        {
            register unsigned char *byteP;
            byteP = THDRELEMPTR(thdr, unsigned char, first);
            for (i = 0; i < nelems; ++i, ++byteP) {
                if (Tcl_GetIntFromObj(ip, elems[i], &ival) != TCL_OK)
                    goto convert_error;
                if (ival > 255 || ival < 0) {
                    ta_value_type_error(ip, elems[i], thdr->type);
                    goto convert_error;
                }
                *byteP = (unsigned char) ival;
            }
        }
        break;

    default:
        ta_type_panic(thdr->type);
    }

    thdr->used = new_used;

    return TCL_OK;

convert_error:                  /* Interp should already contain errors */
    TA_ASSERT(thdr->type != TA_OBJ); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}

TCL_RESULT thdr_place_objs(
    Tcl_Interp *ip,
    thdr_t *thdr,               /* thdr_t to be modified - must NOT be shared */
    thdr_t *pindices,            /* Contains indices. */
    int highest_in_indices,          /* Highest index in pindices.
                                   If >= thdr->used, all intermediate indices
                                   must also be present in pindices. Caller
                                   must have checked
                                */
    int nvalues,                /* # values in pvalues */
    Tcl_Obj * const *pvalues)   /* Values to be stored */
{
    int *pindex, *end;
    int status;
    ta_value_t v;

    TA_ASSERT(thdr->nrefs < 2);
    TA_ASSERT(pindices->type == TA_INT);
    TA_ASSERT(highest_in_indices < thdr->usable);

    if (pindices->used > nvalues)
        return ta_indices_count_error(ip, pindices->used, nvalues);

    if (nvalues == 0)
        return TCL_OK;          /* Nothing to change */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /*
     * In case of conversion errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors.
     */

    if ((status = ta_verify_value_objs(ip, thdr->type, nvalues, pvalues))
        != TCL_OK)
        return status;

    /*
     * Now  store the values. Note we do not have to check
     * status on any conversion since we did so already.
     */

#define PLACEVALUES(type, fn, var) do {                 \
        type *p;                                        \
        p = THDRELEMPTR(thdr, type, 0);             \
        while (pindex < end) {                         \
            status = fn(ip, *pvalues++, &var);      \
            TA_ASSERT(status == TCL_OK);                \
            TA_ASSERT(*pindex < thdr->usable);      \
            TA_ASSERT(*pindex <= thdr->used);          \
            p[*pindex++] = (type) var;                  \
        }                                               \
    } while (0)
    
    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + nvalues;
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            while (pindex < end) {
                status = Tcl_GetBooleanFromObj(ip, *pvalues++, &v.ival);
                TA_ASSERT(status == TCL_OK); /* Since values are verified */
                TA_ASSERT(*pindex < thdr->usable);
                TA_ASSERT(*pindex <= thdr->used);
                ba_put(baP, *pindex++, v.ival);
            }
        }
        break;

    case TA_UINT:
        PLACEVALUES(unsigned int, Tcl_GetWideIntFromObj, v.wval);
        break;
    case TA_INT:
        PLACEVALUES(int, Tcl_GetIntFromObj, v.ival);
        break;
    case TA_WIDE:
        PLACEVALUES(Tcl_WideInt, Tcl_GetWideIntFromObj, v.wval);
        break;
    case TA_DOUBLE:
        PLACEVALUES(double, Tcl_GetDoubleFromObj, v.dval);
        break;
    case TA_OBJ:
        {
            int i;
            Tcl_Obj **pobjs;
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
            for (i = thdr->used; i <= highest_in_indices; ++i)
                pobjs[i] = NULL;
            while (pindex < end) {
                /* Careful about the order here! */
                Tcl_IncrRefCount(*pvalues);
                if (pobjs[*pindex] != NULL)
                    Tcl_DecrRefCount(*pobjs);/* Deref what was originally in that slot */
                pobjs[*pindex] = *pvalues++;
            }
        }
        break;

    case TA_BYTE:
        PLACEVALUES(unsigned int, Tcl_GetIntFromObj, v.ival);
        break;
    default:
        ta_type_panic(thdr->type);
    }

    if (highest_in_indices >= thdr->used)
        thdr->used = highest_in_indices + 1;

    return TCL_OK;
}

int thdr_required_size(unsigned char tatype, int count)
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
    case TA_OBJ:
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

thdr_t * thdr_alloc(Tcl_Interp *ip, unsigned char tatype, int count)
{
    unsigned char nbits;
    int sz;
    thdr_t *thdr;

    if (count == 0)
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
    case TA_OBJ: nbits = sizeof(Tcl_Obj *) * CHAR_BIT; break;
    case TA_BYTE: nbits = sizeof(unsigned char) * CHAR_BIT; break;
    default:
        ta_type_panic(tatype);
    }
    thdr->elem_bits = nbits;
    thdr->sort_order = THDR_UNSORTED;

    return thdr;
}

thdr_t * thdr_alloc_and_init(Tcl_Interp *ip, unsigned char tatype,
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

    case TA_OBJ:
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
    int i;
    int *pindex;

    TA_ASSERT(pindices->type == TA_INT);

    /*
     * We have to be careful to delete from back to front so as to not
     * invalidate index positions when earlier ones are deleted
     */
    TA_ASSERT(pindices->sort_order == THDR_SORTED_ASCENDING || pindices->sort_order == THDR_SORTED_DESCENDING);
    
    /*
     * TBD - this will be desperately slow. Fix
     */
    
    /* We always want to delete back to front. However the index array
     * may be presorted in any direction. So check and loop accordingly
     */
    i = pindices->used;
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        /* Sort order is ascending so iterate index array back to front */
        pindex = THDRELEMPTR(pindices, int, pindices->used-1 );
        while (i--) {
            if (*pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            --pindex;
        }
    } else {
        /* Sort order is descending so iterate index array front to back */
        pindex = THDRELEMPTR(pindices, int, 0);
        while (i--) {
            if (*pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            ++pindex;
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
    case TA_OBJ:    SWAPALL(thdr, Tcl_Obj*); break;
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
        if (insert) {
            /* First make room by copying bits up */
            d = THDRELEMPTR(pdst, ba_t, 0);
            ba_copy(d, dst_first+count, d, dst_first, pdst->used-dst_first);
        }
        /* Now insert or overwrite in place */
        ba_copy(d, dst_first, THDRELEMPTR(psrc, ba_t, 0), src_first, count);
        break;

    case TA_OBJ:
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
    case TA_OBJ:
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

    orig_order = thdr->sort_order;

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

Tcl_Obj *ta_range(Tcl_Interp *ip, Tcl_Obj *srcObj, int low, int count,
                     int fmt)
{
    int end;
    thdr_t *psrc;
    Tcl_Obj *o;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    psrc = TARRAYHDR(srcObj);

    if (fmt == TA_FORMAT_TARRAY) {
        thdr_t *thdr = thdr_range(ip, psrc, low, count);
        return thdr == NULL ? NULL : tcol_new(thdr);
    }

    end = low + count;
    if (end > psrc->used)
        end = psrc->used;

    /* Even dicts more efficiently built as lists and shimmered as necessary */
    o = Tcl_NewListObj(end-low, NULL);

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
        {
            unsigned int *pui = THDRELEMPTR(psrc, unsigned int, low);
            unsigned int *pend = THDRELEMPTR(psrc, unsigned int, end);
            while (pui < pend) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(ip, o, Tcl_NewWideIntObj(*pui++));
            }
        }
        break;
    case TA_INT:
        {
            int *pint = THDRELEMPTR(psrc, int, low);
            int *pend = THDRELEMPTR(psrc, int, end);
            while (pint < pend) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(*pint++));
            }
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *pwide = THDRELEMPTR(psrc, Tcl_WideInt, low);
            Tcl_WideInt *pend = THDRELEMPTR(psrc, Tcl_WideInt, end);
            while (pwide < pend) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(ip, o, Tcl_NewWideIntObj(*pwide++));
            }
        }
        break;
    case TA_DOUBLE:
        {
            double *pdbl = THDRELEMPTR(psrc, double, low);
            double *pend = THDRELEMPTR(psrc, double, end);
            while (pdbl < pend) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(ip, o, Tcl_NewDoubleObj(*pdbl++));
            }
        }
        break;
    case TA_BYTE:
        {
            unsigned char *puch = THDRELEMPTR(psrc, unsigned char, low);
            unsigned char *pend = THDRELEMPTR(psrc, unsigned char, end);
            while (puch < pend) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(*puch++));
            }
        }
        break;
    case TA_OBJ:
        {
            Tcl_Obj **pobjsP = THDRELEMPTR(psrc, Tcl_Obj *, low);
            Tcl_Obj **pend = THDRELEMPTR(psrc, Tcl_Obj *, end);
            while (pobjsP < pend) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));
                Tcl_ListObjAppendElement(ip, o, *pobjsP);
            }
        }
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

TCL_RESULT tgrid_make_modifiable(Tcl_Interp *ip,
                                Tcl_Obj *tgrid,
                                 int minsize, /* Min *contained* cols */
                                 int prefsize /* Pref size  *contained* cols */
    )
{
    int i, status;
    Tcl_Obj **tcols;

    TA_ASSERT(! Tcl_IsShared(tgrid));

    if ((status = tgrid_convert(ip, tgrid)) != TCL_OK)
        return status;
    
    /*
     * First make the tgrid object itself modifiable in case its thdr
     * is shared
     */
    if (thdr_shared(TARRAYHDR(tgrid)) &&
        (status = tcol_make_modifiable(ip, tgrid, 0, 0)) != TCL_OK)
        return status;

    /* Now make its contained columns modifiable */
    tcols = THDRELEMPTR(TARRAYHDR(tgrid), Tcl_Obj *, 0);
    i = tcol_occupancy(tgrid);
    while (i--) {
        Tcl_Obj *tcol;
        tcol = tcols[i];
        if (Tcl_IsShared(tcol)) {
            tcol = Tcl_DuplicateObj(tcol);
            Tcl_IncrRefCount(tcol);
            Tcl_DecrRefCount(tcols[i]);
            tcols[i] = tcol;
        }
        if ((status = tcol_make_modifiable(ip, tcol, minsize, prefsize)) != TCL_OK)
            return status;
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
    case TA_OBJ:
        return *THDRELEMPTR(thdr, Tcl_Obj *, index);
    default:
        ta_type_panic(thdr->type);
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
int tcol_to_indices(Tcl_Interp *ip, Tcl_Obj *o,
                           int want_sorted,
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

    /* To prevent shimmering, first check known to be a list */
    if (o->typePtr != g_tcl_list_type_ptr && pindex != NULL) {
        status = Tcl_GetIntFromObj(NULL, o, &n);
        if (status == TCL_OK) {
            *pindex = n;
            return TA_INDEX_TYPE_INT;
        }
        /* else fall through to try as list */
    }

    if (Tcl_ListObjGetElements(NULL, o, &n, &elems) != TCL_OK) {
        ta_indices_error(ip, o);
        return TA_INDEX_TYPE_ERROR;
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
Tcl_Obj *tcol_get(Tcl_Interp *ip, thdr_t *psrc, thdr_t *pindices, int fmt)
{
    thdr_t *thdr;
    int count, index, bound;
    int *pindex, *end;
    Tcl_Obj *tcol;
    void *srcbase, *thdrbase;

    TA_ASSERT(pindices->type == TA_INT);
    count = pindices->used;

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

#define tcol_get_COPY(type_, objfn_)                                   \
    do {                                                                \
        type_ *fromP = srcbase;                                        \
        type_ *tpobjs = thdrbase;                                         \
        while (pindex < end) {                                         \
            index = *pindex++;                                          \
            if (index < 0 || index >= bound)                            \
                goto index_error;                                       \
            if (fmt == TA_FORMAT_TARRAY)                                \
                *tpobjs++ = fromP[index];                                  \
            else {                                                      \
                if (fmt == TA_FORMAT_DICT)                              \
                    Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index)); \
                Tcl_ListObjAppendElement(ip, tcol,                 \
                                         objfn_(fromP[index])); \
            }                                                           \
        }                                                               \
        if (fmt == TA_FORMAT_TARRAY)                                    \
            thdr->used = count;                                        \
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
            for (i = 0; pindex < end; ++i, ++pindex) {
                index = *pindex; 
                if (index < 0 || index >= bound)
                    goto index_error;
                if (fmt == TA_FORMAT_TARRAY)
                    ba_put(baP, i, ba_get(srcbaP, index));
                else {
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol,
                                             Tcl_NewIntObj(ba_get(srcbaP, index)));
                }
            }
        }
        break;
    case TA_UINT:
    case TA_INT:
        tcol_get_COPY(unsigned int, Tcl_NewIntObj);
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
    case TA_OBJ:
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
            status = ta_fix_range_bounds(ip, thdr, indexa,
                                             indexb, &low, &count);
            if (status == TCL_OK)
                thdr_delete_range(thdr, low, count);
        } else {
            /* Not a range, either a list or single index */
            thdr_t *pindices;
            /* Note status is TCL_OK at this point */
            switch (tcol_to_indices(ip, indexa, 1, &pindices, &low)) {
            case TA_INDEX_TYPE_ERROR:
                status = TCL_ERROR;
                break;
            case TA_INDEX_TYPE_INT:
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
        if ((status = Tcl_GetIntFromObj(ip, ocount, &count)) == TCL_OK &&
            (status = tcol_convert(ip, tcol)) == TCL_OK) {
            used = tcol_occupancy(tcol);
            if (count < 0)
                count = 0;      /* Should we error instead? */
            if ((status = tcol_make_modifiable(ip, tcol, count+used, 0)) == TCL_OK &&
                (status = ta_convert_index(ip, opos, &pos, used,
                                           0, used)) == TCL_OK) {
                ta_value_t tav;
                if ((status = ta_value_from_obj(ip, ovalue,
                                                tcol_type(tcol), &tav)) == TCL_OK)
                    thdr_fill_range(ip, TARRAYHDR(tcol), &tav, pos, count, 1);
            }
        }
    }
    return status;
}


TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                         Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int low, count;
    int status;
    ta_value_t value;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if ((status = ta_value_from_obj(ip, ovalue,
                                     tcol_type(tcol), &value)) != TCL_OK)
        return status;

    if (indexb) {
        status = ta_fix_range_bounds(ip, TARRAYHDR(tcol), indexa,
                                         indexb, &low, &count);
        if (status == TCL_OK && count != 0) {
            status = tcol_make_modifiable(ip, tcol, low+count, 0);
            if (status == TCL_OK)
                thdr_fill_range(ip, TARRAYHDR(tcol), &value, low, count, 0);
        }
    } else {
        /* Not a range, either a list or single index */
        thdr_t *pindices;
        /* Note status is TCL_OK at this point */
        switch (tcol_to_indices(ip, indexa, 1, &pindices, &low)) {
        case TA_INDEX_TYPE_ERROR:
            status = TCL_ERROR;
            break;
        case TA_INDEX_TYPE_INT:
            if (low < 0 || low > tcol_occupancy(tcol)) {
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
            if (status == TCL_OK) {
                status = tcol_make_modifiable(ip, tcol, count, count); // TBD - count + extra?
                if (status == TCL_OK)
                    thdr_fill_indices(ip, TARRAYHDR(tcol), &value, pindices);
            }
            thdr_decr_refs(pindices);
            break;
        }
    }

    return status;
}

Tcl_Obj *tcol_reverse(Tcl_Interp *ip, Tcl_Obj *tcol)
{
    thdr_t *thdr;
    TCL_RESULT status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (Tcl_IsShared(tcol) || thdr_shared(TARRAYHDR(tcol))) {
        thdr = thdr_clone_reversed(ip, TARRAYHDR(tcol), 0);
        if (thdr == NULL)
            return NULL;
        if (Tcl_IsShared(tcol))
            return tcol_new(thdr);
        else
            ta_replace_intrep(tcol, thdr);
    } else {
        thdr_reverse(thdr);
        Tcl_InvalidateStringRep(tcol);
    }
    return tcol;
}

int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;
    int alen, blen, len;
    int comparison;

    a = Tcl_GetStringFromObj(oaP, &alen);
    alen = Tcl_NumUtfChars(a, alen); /* Num bytes -> num chars */
    b = Tcl_GetStringFromObj(obP, &blen);
    blen = Tcl_NumUtfChars(b, blen); /* Num bytes -> num chars */

    len = alen < blen ? alen : blen; /* len is the shorter length */
    
    comparison = (ignorecase ? Tcl_UtfNcasecmp : Tcl_UtfNcmp)(a, b, len);

    if (comparison == 0) {
        comparison = alen-blen;
    }
    return (comparison > 0) ? 1 : (comparison < 0) ? -1 : 0;
}

TCL_RESULT tcol_copy_thdr(Tcl_Interp *ip, Tcl_Obj *tcol, thdr_t *psrc, Tcl_Obj *ofirst, int insert)
{
    int first, status;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if (tcol_type(tcol) != psrc->type)
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    status = ta_convert_index(ip, ofirst, &first, tcol_occupancy(tcol),
                        0, tcol_occupancy(tcol));
    if (status == TCL_OK && psrc->used) {
        status = tcol_make_modifiable(ip, tcol, first + psrc->used, 0);
        if (status == TCL_OK)
            thdr_copy(TARRAYHDR(tcol), first, psrc, 0, psrc->used, insert); 
    }
    return status;
}

TCL_RESULT tcol_put_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                         Tcl_Obj *ovalues, Tcl_Obj *ofirst, int insert)
{
    int status;
    Tcl_Obj **values;
    int nvalues;
    int n;

    TA_ASSERT(! Tcl_IsShared(tcol));

    status = Tcl_ListObjGetElements(ip, ovalues, &nvalues, &values);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    /* Get the limits of the range to set */

    n = tcol_occupancy(tcol);
    status = ta_convert_index(ip, ofirst, &n, n, 0, n);
    /* n contains starting offset */
    if (status == TCL_OK && nvalues) {
        /* Note this also invalidates the string rep as desired */
        status = tcol_make_modifiable(ip, tcol, n + nvalues, 0);
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
    thdr_t *psorted;

    TA_ASSERT(! Tcl_IsShared(tcol));

    status = Tcl_ListObjGetElements(ip, ovaluelist, &nvalues, &ovalues);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (tcol_to_indices(ip, oindices, 0, &pindices, NULL)
        != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    if (pindices->used == 0)
        return TCL_OK;

    /* For verification we will need to sort indices */
    psorted = pindices;
    if (psorted->sort_order == THDR_UNSORTED) {
        psorted = thdr_clone(ip, psorted, 0);
        if (psorted == NULL)
            return TCL_ERROR;
        qsort(THDRELEMPTR(psorted, int, 0), psorted->used, sizeof(int), intcmp);
        psorted->sort_order = THDR_SORTED_ASCENDING;
    }

    status = thdr_verify_indices(ip, TARRAYHDR(tcol), psorted, &new_size);
    if (psorted != pindices)
        thdr_decr_refs(psorted);
    if (status == TCL_OK) {
        status = tcol_make_modifiable(ip, tcol, new_size, new_size); // TBD - count + extra?
        if (status == TCL_OK) {
            status = thdr_place_objs(ip, TARRAYHDR(tcol), pindices,
                                        new_size-1, /* Highest index in pindices */
                                        nvalues, ovalues);
        }
    }
    thdr_decr_refs(pindices);

    return status;
}

/* The grid Tcl_Obj gridObj is modified */
TCL_RESULT tgrid_fill_obj(
    Tcl_Interp *ip,
    Tcl_Obj *tgrid,
    Tcl_Obj *orow,
    Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int i, low, count, row_width;
    ta_value_t values[32];
    ta_value_t *pvalues;
    Tcl_Obj **tcolPP;
    int status;
    int col_len;
    Tcl_Obj **ovalues;

    TA_ASSERT(! Tcl_IsShared(tgrid));

    if ((status = Tcl_ListObjGetElements(ip, orow, &row_width, &ovalues)) != TCL_OK ||
        (status = tgrid_convert(ip, tgrid)) != TCL_OK)
        return status;

    count = tgrid_width(tgrid);
    if (row_width < count)
        return ta_row_width_error(ip, row_width, count);

    /* Check for empty tuple so as to simplify loops below */
    if (row_width == 0)
        return TCL_OK;          /* Return empty result */

    if (row_width > sizeof(values)/sizeof(values[0])) {
        pvalues = (ta_value_t *) TA_ALLOCMEM(row_width * sizeof(ta_value_t));
    } else {
        pvalues = values;
    }

    tcolPP = THDRELEMPTR(TARRAYHDR(tgrid), Tcl_Obj *, 0);
    col_len = tcol_occupancy(*tcolPP); /* #items in first column */

    /* Validate column lengths and value types */
    for (i = 0; i < row_width; ++i) {
        if (tcol_occupancy(tcolPP[i]) != col_len) {
            status = ta_grid_length_error(ip);
            goto vamoose;
        }
        status = ta_value_from_obj(ip, ovalues[i], tcol_type(tcolPP[i]), &pvalues[i]);
        if (status != TCL_OK)
            goto vamoose;
    }

    /*
     * Ok, now we have validated the columns are the right length and
     * values are the right type.
     */

    /* Figure out if we are given a range or a sequence of indices */
    if (indexb) {
        /* Given a range */
        status = ta_fix_range_bounds(ip, TARRAYHDR(tcolPP[0]), indexa,
                                     indexb, &low, &count);
        if (status != TCL_OK || count == 0)
            goto vamoose;      /* Either error or empty range */
        if ((status = tgrid_make_modifiable(ip, tgrid, low+count, 0)) != TCL_OK)
            goto vamoose;
        tcolPP = THDRELEMPTR(TARRAYHDR(tgrid), Tcl_Obj *, 0); /* Might have changed! */
        for (i = 0; i < row_width; ++i)
            thdr_fill_range(ip, TARRAYHDR(tcolPP[i]),
                            &values[i], low, count, 0);
    } else {
        /* Not a range, either a list or single index */
        thdr_t *pindices;
        /* Note status is TCL_OK at this point */
        switch (tcol_to_indices(ip, indexa, 1, &pindices, &low)) {
        case TA_INDEX_TYPE_ERROR:
            status = TCL_ERROR;
            break;
        case TA_INDEX_TYPE_INT:
            if (low < 0 || low > col_len) {
                ta_index_range_error(ip, low);
                status = TCL_ERROR;
            } else {
                status = tgrid_make_modifiable(ip, tgrid, low+1, 0);
                tcolPP = THDRELEMPTR(TARRAYHDR(tgrid), Tcl_Obj *, 0); /* Might have changed! */
                if (status == TCL_OK) {
                    for (i = 0; i < row_width; ++i)
                        thdr_fill_range(ip, TARRAYHDR(tcolPP[i]),
                                        &values[i], low, 1, 0);
                }
            }
            break;
        case TA_INDEX_TYPE_THDR:
            status = thdr_verify_indices(ip, TARRAYHDR(tcolPP[0]), pindices, &count);
            if (status == TCL_OK) {
                status = tgrid_make_modifiable(ip, tgrid, count, count); // TBD - count + extra?
                tcolPP = THDRELEMPTR(TARRAYHDR(tgrid), Tcl_Obj *, 0); /* Might have changed! */
                if (status == TCL_OK) {
                    for (i = 0; i < row_width; ++i)
                        thdr_fill_indices(ip, TARRAYHDR(tcolPP[i]),
                                          &values[i], pindices);
                }
            }
            thdr_decr_refs(pindices);
            break;
        }
    }

vamoose:
    /* status contains TCL_OK or other code */
    /* ip must already hold error message in case of error */
    if (pvalues != values)
        TA_FREEMEM((char *) pvalues);

    return status;

}

/* The grid Tcl_Obj gridObj is modified */
TCL_RESULT OBSOLETETGridFillFromObjs(
    Tcl_Interp *ip,
    Tcl_Obj *olow, Tcl_Obj *ohigh,
    Tcl_Obj *gridObj,
    Tcl_Obj *rowObj)
{
    int i, low, count, row_width;
    thdr_t *gridHdrP;
    ta_value_t values[32];
    ta_value_t *pvalues;
    Tcl_Obj **tcolPP;
    int status;
    int new_size;
    int collength;

    TA_ASSERT(! Tcl_IsShared(gridObj));

    if ((status = tcol_convert(ip, gridObj)) != TCL_OK)
        return status;

    gridHdrP = TARRAYHDR(gridObj);

    if ((status = Tcl_ListObjLength(ip, rowObj, &row_width)) != TCL_OK)
        return status;

    if (row_width != gridHdrP->used)
        return ta_row_width_error(ip, row_width, gridHdrP->used);

    /* Check for empty tuple so as to simplify loops below */
    if (row_width == 0)
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

    if (tcol_affirm(olow))
        olow = Tcl_DuplicateObj(olow);
    else
        Tcl_IncrRefCount(olow); /* Since we will release at end */

    if (tcol_affirm(ohigh))
        ohigh = Tcl_DuplicateObj(ohigh);
    else
        Tcl_IncrRefCount(ohigh); /* Since we will release at end */

    if (row_width > sizeof(values)/sizeof(values[0])) {
        pvalues = (ta_value_t *) TA_ALLOCMEM(row_width * sizeof(ta_value_t));
    } else {
        pvalues = values;
    }
        
    /* Make sure grid object is modifiable */
    if ((status = tcol_make_modifiable(ip, gridObj,
                                       gridHdrP->used, 0)) != TCL_OK)
        goto vamoose;
    gridHdrP = TARRAYHDR(gridObj); /* Might have changed */

    /* Grids don't care about sort bits, but nevertheless.. */
    gridHdrP->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /*
     * Now verify tarrays and values. The latter should be of the
     * appropriate type. Also ensure all tarrays are the same size
     * and can be modified.
     *
     * NOTE THAT AT ALL TIMES, gridObj has valid unmodified contents
     * (logically unmodified though allocated blocks might have changed)
     * so even on error we exit with a clean gridObj.
     */
    for (i = 0, tcolPP = THDRELEMPTR(gridHdrP, Tcl_Obj *, 0);
         i < row_width;
         ++i, ++tcolPP) {
        thdr_t *thdr;
        Tcl_Obj *ovalue;
        Tcl_Obj *colObj = *tcolPP;

        if ((status = tcol_convert(ip, *tcolPP)) != TCL_OK)
            goto vamoose;

        thdr = TARRAYHDR(colObj);
        if (i == 0) {
            collength = thdr->used;
            /* Get the limits of the range to set */
            status = ta_fix_range_bounds(ip, thdr, olow, ohigh, &low, &count);
            if (status != TCL_OK || count == 0)
                goto vamoose;   /* Error or nothing to do */
        }
        else if (thdr->used != collength) {
            status = ta_grid_length_error(ip);
            goto vamoose;
        }

        /* Preferred size in case we have to reallocate */
        new_size = low + count + TA_EXTRA(low+count); /* Disregarded if < usable */

        /* We have already converted above */
        TA_ASSERT(tcol_affirm(colObj));
        if (Tcl_IsShared(colObj)) {
            colObj = Tcl_DuplicateObj(colObj);
            Tcl_IncrRefCount(colObj);
            Tcl_DecrRefCount(*tcolPP);
            *tcolPP = colObj;
            thdr = TARRAYHDR(colObj);
        }

        /* Note this also invalidates the string rep as desired */
        if ((status = tcol_make_modifiable(ip, colObj, low+count, new_size)) != TCL_OK)
            goto vamoose;

        if ((status = Tcl_ListObjIndex(ip, rowObj, i, &ovalue)) != TCL_OK)
            goto vamoose;
        status = ta_value_from_obj(ip, ovalue, thdr->type, &pvalues[i]);
        if (status != TCL_OK)
            goto vamoose;
    }


    /*
     * We can now do the actual modifications. All validation and memory
     * allocations are done.
     * NOTE: NO ERRORS ARE EXPECTED BEYOND THIS POINT EXCEPT FATAL ONES
     */
    for (i = 0, tcolPP = THDRELEMPTR(gridHdrP, Tcl_Obj *, 0);
         i < row_width;
         ++i, ++tcolPP) {
        thdr_fill_range(ip, TARRAYHDR(*tcolPP), &pvalues[i], low, count, 0);
    }
    status = TCL_OK;
    
vamoose:                   /* ip must already hold error message */
    Tcl_DecrRefCount(olow);
    Tcl_DecrRefCount(ohigh);
    if (pvalues != values)
        TA_FREEMEM((char *) pvalues);

    return status;
}

TCL_RESULT tcols_validate_obj_row_widths(Tcl_Interp *ip, int width,
                                         int nrows, Tcl_Obj * const rows[])
{
    int r, i;
    for (r = 0; r < nrows; ++r) {
        if (Tcl_ListObjLength(ip, rows[r], &i) == TCL_ERROR)
            return TCL_ERROR;
        /* Width of row must not be too short, longer is ok */
        if (i < width)
            return ta_row_width_error(ip, i, width);
    }
    return TCL_OK;
}

TCL_RESULT tcols_validate_obj_rows(Tcl_Interp *ip, int ntcols,
                                   Tcl_Obj * const *tcols,
                                   int nrows, Tcl_Obj * const rows[])
{
    int r, t;
    ta_value_t v;
    
    /*
     * We could either iterate vertically or horizontally
     *   for (per thdr)
     *     switch (thdr->type)
     *       for (per row)
     *         field <- Tcl_ListObjIndex
     *         validate field
     * or
     *   for (per row)
     *     fields <- Tcl_ListObjGetElements
     *     per field
     *       switch thdr->type
     *         validate field
     *
     * Not clear which will perform better - first case inner loop has
     * a call thru a pointer (Tcl_ListObjIndex). Second case inner loop has a
     * switch, probably faster than an indirect call, so we go with that for
     * now.
     * TBD - measure and decide if it even matters.
     */
    for (r = 0; r < nrows; ++r) {
        Tcl_Obj **fields;
        int nfields;
        
        if (Tcl_ListObjGetElements(ip, rows[r], &nfields, &fields)
            != TCL_OK)
            return TCL_ERROR;

        /* Must have sufficient fields, more is ok */
        if (nfields < ntcols)
            return ta_row_width_error(ip, nfields, ntcols);

        for (t = 0; t < ntcols; ++t) {
            int tatype = TARRAYHDR(tcols[t])->type;
            switch (tatype) {
            case TA_BOOLEAN:
                if (Tcl_GetBooleanFromObj(ip, fields[t], &v.ival) != TCL_OK)
                    return TCL_ERROR;
                break;
            case TA_UINT:
                if (Tcl_GetWideIntFromObj(ip, fields[t], &v.wval) != TCL_OK)
                    return TCL_ERROR;
                if (v.wval < 0 || v.wval > 0xFFFFFFFF) {
                    ta_value_type_error(ip, fields[t], tatype);
                    return TCL_ERROR;
                }
                break;
            case TA_INT:
                if (Tcl_GetIntFromObj(ip, fields[t], &v.ival) != TCL_OK)
                    return TCL_ERROR;
                break;
            case TA_WIDE:
                if (Tcl_GetWideIntFromObj(ip, fields[t], &v.wval) != TCL_OK)
                    return TCL_ERROR;
                break;
            case TA_DOUBLE:
                if (Tcl_GetDoubleFromObj(ip, fields[t], &v.dval) != TCL_OK)
                    return TCL_ERROR;
                break;
            case TA_BYTE:
                if (Tcl_GetIntFromObj(ip, fields[t], &v.ival) != TCL_OK)
                    return TCL_ERROR;
                if (v.ival > 255 || v.ival < 0) {
                    ta_value_type_error(ip, fields[t], tatype);
                    return TCL_ERROR;
                }
                break;
            case TA_OBJ:
                break;      /* No validation */
            default:
                ta_type_panic(tatype);
            }
        }
    }

    return TCL_OK;
}


/* ip may be NULL (only used for errors) */
/* See asserts in code for prerequisite conditions */
TCL_RESULT tcols_put_objs(Tcl_Interp *ip, int ntcols, Tcl_Obj * const *tcols,
                          int nrows, Tcl_Obj * const *rows,
                          int first, int insert)
{
    int t, r, ival;
    Tcl_WideInt wide;
    int have_obj_cols;
    int have_other_cols;
    int need_data_validation;
    Tcl_Obj *oval;
    thdr_t *thdr, *thdr0;


    if (ntcols == 0 || nrows == 0)
        return TCL_OK;          /* Nought to do */

    thdr0 = TARRAYHDR(tcols[0]);
    for (t = 0, have_obj_cols = 0, have_other_cols = 0; t < ntcols; ++t) {
        thdr = TARRAYHDR(tcols[t]);
        TA_ASSERT(! Tcl_IsShared(tcols[t]));
        TA_ASSERT(! thdr_shared(thdr));
        TA_ASSERT(thdr->usable >= (first + nrows)); /* 'Nuff space */
        TA_ASSERT(thdr->used == thdr0->used); /* All same size */

        if (thdr->type == TA_OBJ)
            have_obj_cols = 1;
        else
            have_other_cols = 1;
    }

    /*
     * In case of errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors
     * or to hold new Tcl_Values so conversion does not need to be repeated.
     *
     * There are two kinds of errors - data type errors (e.g. attempt
     * to store a non-integer into an integer field) and structural
     * errors (e.g. a row not having enough elements).
     *
     * As a special optimization, when appending to the end, we do
     * not need to first check. We directly store the values and in case
     * of errors, simply not update the old size.
     *
     * TA_OBJ add a complication. They do not need a type check
     * but because their reference counts have to be managed, it is more
     * complicated to back track on errors when we skip the validation
     * checks in the pure append case. So we update these columns
     * only after everything else has been updated.
     */

    if (! have_other_cols) {
        /* Only TA_OBJ columns, data validation is a no-op */
        need_data_validation = 0;
    } else if (first >= thdr0->used) {
        /*
         * Pure append, not overwriting so rollback becomes easy and
         * no need for prevalidation step.
         */
        need_data_validation = 0;
    } else
        need_data_validation = 1;
       

    /*
     * TBD - optmization. Check which of these alternativs is better
     *  - current implementation
     *  - Do not call tcols_validate_obj_row_widths but check
     *    return of Tcl_ListObjIndex in storage loop
     *  - always call thdrs_validate_obj_rows (even if appending)
     *    and dispense with error checking in storage loop.
     */
    if (need_data_validation) {
        if (tcols_validate_obj_rows(ip, ntcols, tcols, nrows, rows) != TCL_OK)
            return TCL_ERROR;
    } else {
        /*
         * We are not validating data but then validate row widths 
         * We are doing this to simplify error rollback for TA_OBJ
         */
        if (tcols_validate_obj_row_widths(ip, ntcols, nrows, rows) != TCL_OK)
            return TCL_ERROR;
    }

    /* We could either iterate vertically or horizontally
     *   for (per thdr)
     *     switch (thdr->type)
     *       for (per row)
     *         field <- Tcl_ListObjIndex
     *         validate field
     * or
     *   for (per row)
     *     fields <- Tcl_ListObjGetElements
     *     per field
     *       switch thdr->type
     *         validate field
     *
     * Not clear which will perform better - first case inner loop has
     * a indirect call (Tcl_ListObjIndex). Second case inner loop has a switch
     * (probably faster than a indirect call). On the other hand, when actually
     * writing out to the array, cache effects might make the former
     * faster (writing consecutive locations).
     *
     * As it turns out, we use the first method for a different reason -
     * when we are strictly appending without overwriting, we do not
     * validate since rollback is easy. The complication is that if
     * any column is of type TA_OBJ, when an error occurs we have to
     * rollback that column's Tcl_Obj reference counts. Keeping track
     * of this is more involved using the second scheme and much simpler
     * with the first scheme. Hence we go with that.
     */

    /*
     * Now actually store the values. Note we still have to check
     * status on conversion in case we did not do checks when we are appending
     * to the end, and we have to store TA_OBJ last to facilitate
     * rollback on errors as discussed earlier.
     */
    if (have_other_cols) {
        for (t=0; t < ntcols; ++t) {
            /* Skip TA_OBJ on this round, until all other data is stored */
            thdr = TARRAYHDR(tcols[t]);
            if (thdr->type == TA_OBJ)
                continue;

            if (insert)
                thdr_make_room(thdr, first, nrows);

            thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
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
                    } else
                        ba = 0;
                    for (r = 0; r < nrows; ++r) {
                        Tcl_ListObjIndex(ip, rows[r], t, &oval);
                        TA_ASSERT(oval);
                        if (Tcl_GetBooleanFromObj(ip, oval, &ival) != TCL_OK)
                            goto error_return;
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
                        /* We have some leftover bits in ba that need
                        to be stored.  * We need to *merge* these into
                        the corresponding word * keeping the existing
                        high index bits.  * Note the bit indicated by
                        ba_mask also has to be preserved, * not
                        overwritten.  */
                        *baP = ba | (*baP & BITMASKGE(ba_mask));
                    }
                }
                break;

            case TA_UINT:
                {
                    register unsigned int *uintP;
                    uintP = THDRELEMPTR(thdr, unsigned int, first);
                    for (r = 0; r < nrows; ++r, ++uintP) {
                        Tcl_ListObjIndex(ip, rows[r], t, &oval);
                        TA_ASSERT(oval);
                        if (Tcl_GetWideIntFromObj(ip, oval, &wide) != TCL_OK)
                            goto error_return;
                        if (wide < 0 || wide > 0xFFFFFFFF) {
                            ta_value_type_error(ip, oval, thdr->type);
                            goto error_return;
                        }
                        *uintP = (unsigned int) wide;
                    }
                }
                break;
            case TA_INT:
                {
                    register int *intP;
                    intP = THDRELEMPTR(thdr, int, first);
                    for (r = 0; r < nrows; ++r, ++intP) {
                        Tcl_ListObjIndex(ip, rows[r], t, &oval);
                        TA_ASSERT(oval);
                        if (Tcl_GetIntFromObj(ip, oval, intP) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TA_WIDE:
                {
                    register Tcl_WideInt *pwide;
                    pwide = THDRELEMPTR(thdr, Tcl_WideInt, first);
                    for (r = 0; r < nrows; ++r, ++pwide) {
                        Tcl_ListObjIndex(ip, rows[r], t, &oval);
                        TA_ASSERT(oval);
                        if (Tcl_GetWideIntFromObj(ip, oval, pwide) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TA_DOUBLE:
                {
                    register double *pdbl;
                    pdbl = THDRELEMPTR(thdr, double, first);
                    for (r = 0; r < nrows; ++r, ++pdbl) {
                        Tcl_ListObjIndex(ip, rows[r], t, &oval);
                        TA_ASSERT(oval);
                        if (Tcl_GetDoubleFromObj(ip, oval, pdbl) != TCL_OK)
                            goto error_return;
                    }
                }
                break;
            case TA_BYTE:
                {
                    register unsigned char *byteP;
                    byteP = THDRELEMPTR(thdr, unsigned char, first);
                    for (r = 0; r < nrows; ++r, ++byteP) {
                        Tcl_ListObjIndex(ip, rows[r], t, &oval);
                        TA_ASSERT(oval);
                        if (Tcl_GetIntFromObj(ip, oval, &ival) != TCL_OK)
                            goto error_return;
                        if (ival > 255 || ival < 0) {
                            ta_value_type_error(ip, oval, thdr->type);
                            goto error_return;
                        }
                        *byteP = (unsigned char) ival;
                    }
                }
                break;
            default:
                ta_type_panic(thdr->type);
            }
        }
    }

    /* Now that no errors are possible, update the TA_OBJ columns */
    for (t=0; t < ntcols; ++t) {
        register Tcl_Obj **pobjs;
        thdr = TARRAYHDR(tcols[t]);
        if (thdr->type != TA_OBJ)
            continue;
        if (insert)
            thdr_make_room(thdr, first, nrows);
        thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
        for (r = 0; r < nrows ; ++r, ++pobjs) {
            Tcl_ListObjIndex(ip, rows[r], t, &oval);
            TA_ASSERT(oval);
            /* Careful about the order here! */
            Tcl_IncrRefCount(oval);
            /* Release old elems only if we were not inserting */
            if (!insert) {
                if ((first + r) < thdr->used) {
                    /* Deref what was originally in that slot */
                    Tcl_DecrRefCount(*pobjs);
                }
            }
            *pobjs = oval;
        }
    }

    /* Now finally, update all the counts */
    for (t=0; t < ntcols; ++t) {
        if ((first + nrows) > TARRAYHDR(tcols[t])->used)
            TARRAYHDR(tcols[t])->used = first + nrows;
    }

    return TCL_OK;

error_return:                  /* Interp should already contain errors */
    return TCL_ERROR;
}

TCL_RESULT tgrid_put_objs(Tcl_Interp *ip, Tcl_Obj *tgrid,
                          Tcl_Obj *orows, Tcl_Obj *ofirst, int insert)
{
    int status;
    Tcl_Obj **rows;
    int nrows;
    int n;

    TA_ASSERT(! Tcl_IsShared(tgrid));

    status = Tcl_ListObjGetElements(ip, orows, &nrows, &rows);
    if (status != TCL_OK ||     /* Not a list */
        nrows == 0 ||           /* Nothing to modify */
        (status = tgrid_convert(ip, tgrid)) != TCL_OK || /* Not a grid */
        tgrid_width(tgrid) == 0) /* No columns to update */ {
        return status;           /* Maybe OK or ERROR */
    }

    /* Get the limits of the range to set */
    n = tgrid_length(tgrid);
    status = ta_convert_index(ip, ofirst, &n, n, 0, n);
    /* n contains starting offset */
    if (status == TCL_OK) {
        /* Note this also invalidates the string rep as desired */
        status = tgrid_make_modifiable(ip, tgrid, n + nrows, 0);
        if (status == TCL_OK) {
            /* Note even on error tcols_put_objs guarantees a consistent 
             * and unchanged tcols
             */
            status = tcols_put_objs(ip, tgrid_width(tgrid),
                                    tgrid_columns(tgrid),
                                    nrows, rows, n, insert);
        }
    }
    
    return status;
}

TCL_RESULT tcols_copy(Tcl_Interp *ip,
                      int ntcols,
                      Tcl_Obj * const *dstcols, int dst_elem_first,
                      Tcl_Obj * const *srccols, int src_elem_first,
                      int count,
                      int insert)
{
    int i;
    thdr_t *psrc, *pdst;

    /* First do checks, then the copy so as to not error out half way */
    for (i = 0; i < ntcols; ++i) {
        TA_ASSERT(! Tcl_IsShared(dstcols[i]));
        TA_ASSERT(tcol_affirm(dstcols[i]));
        TA_ASSERT(tcol_affirm(srccols[i]));
        TA_ASSERT(tcol_occupancy(dstcols[i]) == tcol_occupancy(dstcols[0]));

        pdst = TARRAYHDR(dstcols[i]);
        psrc = TARRAYHDR(srccols[i]);

        TA_ASSERT(! thdr_shared(pdst));
        TA_ASSERT(pdst->usable >= (insert ? pdst->used + count : dst_elem_first + count));

        if (pdst->type != psrc->type)
            return ta_mismatched_types_error(ip, pdst->type, psrc->type);
    }
    
    /* Now that *all* columns have been checked, do the actual copy */
    for (i = 0; i < ntcols; ++i) {
        thdr_copy(TARRAYHDR(dstcols[i]), dst_elem_first,
                  TARRAYHDR(srccols[i]), src_elem_first, count, insert);
    }    

    return TCL_OK;
}

TCL_RESULT tgrid_copy(Tcl_Interp *ip, Tcl_Obj *dstgrid, Tcl_Obj *srcgrid, Tcl_Obj *ofirst, int insert)
{
    int first, status;
    Tcl_Obj **dstcols;
    Tcl_Obj **srccols;
    int count, new_min_size;

    TA_ASSERT(! Tcl_IsShared(dstgrid));

    if ((status = tgrid_convert(ip, dstgrid)) != TCL_OK ||
        (status = tgrid_convert(ip, srcgrid)) != TCL_OK)
        return status;

    if (tgrid_width(dstgrid) > tgrid_width(srcgrid))
        return ta_row_width_error(ip, tgrid_width(srcgrid), tgrid_width(srcgrid));

    srccols = tgrid_columns(srcgrid);
    count = tcol_occupancy(srccols[0]);
    dstcols = tgrid_columns(dstgrid);
    if (insert)
        new_min_size = tcol_occupancy(dstcols[0]) + count;
    else
        new_min_size = first + count;
    status = tgrid_make_modifiable(ip, dstgrid, new_min_size, 0);
    if (status != TCL_OK)
        return status;
    dstcols = tgrid_columns(dstgrid); /* Re-init - might have changed */

    status = ta_convert_index(ip, ofirst, &first,
                              tcol_occupancy(dstcols[0]),
                              0, tcol_occupancy(dstcols[0]));
    if (status != TCL_OK)
        return status;

    return tcols_copy(ip, tgrid_width(dstgrid), dstcols, first,
                      srccols, 0, count, insert);
}

TCL_RESULT tgrid_delete(Tcl_Interp *ip, Tcl_Obj *tgrid,
                        Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int status;
    int i;

    TA_ASSERT(! Tcl_IsShared(tgrid));

    if ((status = tgrid_convert(ip, tgrid)) != TCL_OK ||
        (status = tgrid_make_modifiable(ip, tgrid, tgrid_length(tgrid), 0)) != TCL_OK)
        return status;

    i = tgrid_width(tgrid);
    while (i--) {
        if ((status = tcol_delete(ip, tgrid_column(tgrid, i),
                                  indexa, indexb)) != TCL_OK)
            break;
    }
    return status;
}
