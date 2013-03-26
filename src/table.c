/*
 * Copyright (c) 2013, Ashok P. Nadkarni
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


TCL_RESULT tcols_fill_range(
    Tcl_Interp *ip,
    int ntcols,
    Tcl_Obj **tcols,            /* Must be unshared and large enough */
    Tcl_Obj *orow,              /* Value to fill */
    int pos,
    int count,
    int insert)
{
    int i, row_width, col_len, status;
    ta_value_t values[32];
    ta_value_t *pvalues = values;
    Tcl_Obj **ovalues;
    
    if (ntcols == 0 || count == 0)
        return TCL_OK;

    if ((status = Tcl_ListObjGetElements(ip, orow, &row_width, &ovalues)) != TCL_OK)
        return status;

    if (row_width < ntcols)
        return ta_row_width_error(ip, row_width, ntcols);

    if (row_width > sizeof(values)/sizeof(values[0]))
        pvalues = (ta_value_t *) TA_ALLOCMEM(row_width * sizeof(ta_value_t));

    col_len = tcol_occupancy(tcols[0]); /* #items in first column */
    /* Validate column lengths and value types */
    for (i = 0; i < ntcols; ++i) {
        if (tcol_occupancy(tcols[i]) != col_len) {
            status = ta_table_length_error(ip);
            goto vamoose;
        }
        status = ta_value_from_obj(ip, ovalues[i], tcol_type(tcols[i]), &pvalues[i]);
        if (status != TCL_OK)
            goto vamoose;
    }

    for (i = 0; i < ntcols; ++i)
        thdr_fill_range(ip, TARRAYHDR(tcols[i]),
                        &values[i], pos, count, insert);

    /* status will already contain TCL_OK */

vamoose:
    /* status contains TCL_OK or other code */
    /* ip must already hold error message in case of error */
    if (pvalues != values)
        TA_FREEMEM((char *) pvalues);

    return status;

}

TCL_RESULT tcols_fill_indices(
    Tcl_Interp *ip,
    int ntcols,
    Tcl_Obj **tcols,            /* Must be unshared and large enough */
    Tcl_Obj *orow,              /* Value to fill */
    thdr_t *pindices,
    int new_size
    )
{
    int i, row_width, status, col_len;
    ta_value_t values[32];
    ta_value_t *pvalues = values;
    Tcl_Obj **ovalues;

    TA_ASSERT(pindices->type == TA_INT);

    if (ntcols == 0 || pindices->used == 0)
        return TCL_OK;          /* Nothing to do */

    if ((status = Tcl_ListObjGetElements(ip, orow, &row_width, &ovalues)) != TCL_OK)
        return status;

    if (row_width < ntcols)
        return ta_row_width_error(ip, row_width, ntcols);

    if (row_width > sizeof(values)/sizeof(values[0]))
        pvalues = (ta_value_t *) TA_ALLOCMEM(row_width * sizeof(ta_value_t));

    /* Validate column lengths and value types */
    col_len = tcol_occupancy(tcols[0]); /* #items in first column */
    for (i = 0; i < ntcols; ++i) {
        if (tcol_occupancy(tcols[i]) != col_len) {
            status = ta_table_length_error(ip);
            goto vamoose;
        }
        status = ta_value_from_obj(ip, ovalues[i], tcol_type(tcols[i]), &pvalues[i]);
        if (status != TCL_OK)
            goto vamoose;
    }

    /* Now that verification is complete, go do the actual changes */
    for (i = 0; i < ntcols; ++i)
        thdr_fill_indices(ip, TARRAYHDR(tcols[i]), &values[i], pindices, new_size);
    
    /* status will already be TCL_OK */

vamoose:
    /* status contains TCL_OK or other code */
    /* ip must already hold error message in case of error */
    if (pvalues != values)
        TA_FREEMEM((char *) pvalues);

    return status;

}

TCL_RESULT table_convert_from_other(Tcl_Interp *ip, Tcl_Obj *o)
{
    int status;
    thdr_t *thdr;
    Tcl_Obj **ptcol;
    Tcl_Obj **end;
    int nelems;

    if ((status = tcol_convert(ip, o)) != TCL_OK)
        return status;
    thdr = TARRAYHDR(o);
    if (thdr->type != TA_ANY)
        return ta_bad_type_error(ip, thdr);
    if (tcol_occupancy(o) != 0) {
        ptcol = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        nelems = tcol_occupancy(*ptcol);
        end = thdr->used + ptcol;
        while (ptcol < end) {
            if ((status = tcol_convert(ip, *ptcol)) != TCL_OK)
                return status;
            /* All must have same number of elements */
            if (tcol_occupancy(*ptcol) != nelems)
                return ta_table_length_error(ip);
            ++ptcol;
        }
    }

    o->typePtr = &g_table_type;
    return TCL_OK;
}

TCL_RESULT table_make_modifiable(Tcl_Interp *ip,
                                Tcl_Obj *table,
                                 int minsize, /* Min *contained* cols */
                                 int prefsize /* Pref size  *contained* cols */
    )
{
    int i, status;
    Tcl_Obj **tcols;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;
    
    /*
     * First make the table object itself modifiable in case its thdr
     * is shared. Note this also invalidates its string representation.
     */
    if (thdr_shared(TARRAYHDR(table)) &&
        (status = tcol_make_modifiable(ip, table, 0, 0)) != TCL_OK)
        return status;

    /* Now make its contained columns modifiable */
    tcols = THDRELEMPTR(TARRAYHDR(table), Tcl_Obj *, 0);
    i = tcol_occupancy(table);
    while (i--) {
        Tcl_Obj *tcol;
        tcol = tcols[i];
        if (Tcl_IsShared(tcol)) {
            tcol = Tcl_DuplicateObj(tcol);
            Tcl_IncrRefCount(tcol);
            Tcl_DecrRefCount(tcols[i]);
            tcols[i] = tcol;
        }
        if ((status = tcol_make_modifiable(ip, tcol, minsize, prefsize))
            != TCL_OK) {
            /* Note tcol is still valid and consistent though unmodifiable */
            return status;
        }
    }

    return TCL_OK;
}

TCL_RESULT table_fill_obj(
    Tcl_Interp *ip,
    Tcl_Obj *table,
    Tcl_Obj *orow,
    Tcl_Obj *indexa,
    Tcl_Obj *indexb,             /* Can be NULL */
    int insert)
{
    int low, count;
    int status;
    int col_len, ncols;
    thdr_t *pindices;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;
    ncols = table_width(table);
    col_len = table_length(table);
    if (indexb) {
        /* Given a range */
        status = ta_fix_range_bounds(ip, col_len, indexa, indexb, &low, &count);
        if (status != TCL_OK || count == 0)
            return status;
        if ((status = table_make_modifiable(ip, table,
                                            (insert ? col_len : low) + count,
                                            0)) != TCL_OK)
            return status;
        return tcols_fill_range(ip, ncols, table_columns(table), orow,
                                low, count, insert);
    }

    /* A single index arg, so must be an index or an index column or list */

    /* Note status is TCL_OK at this point */
    switch (ta_obj_to_indices(ip, indexa, 1, col_len - 1, &pindices, &low)) {
    case TA_INDEX_TYPE_ERROR:
        status = TCL_ERROR;
        break;
    case TA_INDEX_TYPE_INT:
        if (low < 0 || low > col_len) {
            ta_index_range_error(ip, low);
            status = TCL_ERROR;
        } else {
            status = table_make_modifiable(ip, table,
                                           (insert ? col_len : low) + 1,
                                           0);
            if (status == TCL_OK)
                status = tcols_fill_range(ip, ncols, table_columns(table),
                                          orow, low, 1, insert);
        }
        break;
    case TA_INDEX_TYPE_THDR:
        if (insert) {
            Tcl_SetResult(ip, "Internal error: attempt to use insert mode with index list", TCL_STATIC);
            return TCL_ERROR;

        }
        status = thdr_verify_indices(ip, TARRAYHDR(table_column(table, 0)), pindices, &count);
        if (status == TCL_OK && count > 0) {
            status = table_make_modifiable(ip, table, count, count); // TBD - count + extra?
            status = tcols_fill_indices(ip, ncols, table_columns(table),
                                        orow, pindices, count);
        }
        thdr_decr_refs(pindices);
        break;
    }

    /* status contains TCL_OK or other code */
    /* ip must already hold error message in case of error */
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
                if (ta_get_uint_from_obj(ip, fields[t], &v.uival) != TCL_OK)
                    return TCL_ERROR;
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
                if (ta_get_byte_from_obj(ip, fields[t], &v.ucval) != TCL_OK)
                    return TCL_ERROR;
                break;
            case TA_ANY:
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
         if (insert)
             TA_ASSERT(thdr->usable >= (thdr->used + nrows)); /* 'Nuff space */
         else
             TA_ASSERT(thdr->usable >= (first + nrows)); /* 'Nuff space */
         TA_ASSERT(thdr->used == thdr0->used); /* All same size */

         if (thdr->type == TA_ANY)
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
      * TA_ANY add a complication. They do not need a type check
      * but because their reference counts have to be managed, it is more
      * complicated to back track on errors when we skip the validation
      * checks in the pure append case. So we update these columns
      * only after everything else has been updated.
      */

     if (! have_other_cols) {
         /* Only TA_ANY columns, data validation is a no-op */
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
          * We are doing this to simplify error rollback for TA_ANY
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
      * any column is of type TA_ANY, when an error occurs we have to
      * rollback that column's Tcl_Obj reference counts. Keeping track
      * of this is more involved using the second scheme and much simpler
      * with the first scheme. Hence we go with that.
      */

     /*
      * Now actually store the values. Note we still have to check
      * status on conversion in case we did not do checks when we are appending
      * to the end, and we have to store TA_ANY last to facilitate
      * rollback on errors as discussed earlier.
      */
 #define tcols_put_COPY(type, pos, fn)                           \
     do {                                                        \
         type *p;                                                \
         Tcl_Obj *o; \
         p = THDRELEMPTR(thdr, type, first);                     \
         for (r = 0; r < nrows; ++r, ++p) {                      \
             Tcl_ListObjIndex(ip, rows[r], pos, &o);             \
             TA_ASSERT(o);                                    \
             if (fn(ip, o, p) != TCL_OK)       \
                 goto error_return;                              \
         }                                                       \
     } while (0)

     if (have_other_cols) {
         for (t=0; t < ntcols; ++t) {
             /* Skip TA_ANY on this round, until all other data is stored */
             thdr = TARRAYHDR(tcols[t]);
             if (thdr->type == TA_ANY)
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
                 tcols_put_COPY(unsigned int, t, ta_get_uint_from_obj);
                 break;
             case TA_INT:
                 tcols_put_COPY(unsigned int, t, Tcl_GetIntFromObj);
                 break;
             case TA_WIDE:
                 tcols_put_COPY(Tcl_WideInt, t, Tcl_GetWideIntFromObj);
                 break;
             case TA_DOUBLE:
                 tcols_put_COPY(double, t, Tcl_GetDoubleFromObj);
                 break;
             case TA_BYTE:
                 tcols_put_COPY(unsigned char, t, ta_get_byte_from_obj);
                 break;
             default:
                 ta_type_panic(thdr->type);
             }
         }
     }

     /* Now that no errors are possible, update the TA_ANY columns */
     for (t=0; t < ntcols; ++t) {
         register Tcl_Obj **pobjs;
         thdr = TARRAYHDR(tcols[t]);
         if (thdr->type != TA_ANY)
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
         if (insert) {
             TARRAYHDR(tcols[t])->used += nrows;
         } else {
             if ((first + nrows) > TARRAYHDR(tcols[t])->used)
                 TARRAYHDR(tcols[t])->used = first + nrows;
         }
     }
     
     return TCL_OK;

 error_return:                  /* Interp should already contain errors */
     return TCL_ERROR;
 }

 TCL_RESULT tcols_place_objs(Tcl_Interp *ip, int ntcols, Tcl_Obj * const *tcols,
                             thdr_t *pindices, Tcl_Obj *orows,
                             int new_size)
 {
     Tcl_Obj **prow;
     int i, nrows, status;
     Tcl_Obj **rows;
     Tcl_Obj *o;

     TA_ASSERT(pindices->type == TA_INT);

     for (i = 0; i < ntcols; ++i) {
         TA_ASSERT(! Tcl_IsShared(tcols[i]));
         TA_ASSERT(tcol_affirm(tcols[i]));
         TA_ASSERT(! thdr_shared(TARRAYHDR(tcols[i])));
         TA_ASSERT(TARRAYHDR(tcols[i])->usable >= new_size);
    }

    if (ntcols == 0 || pindices->used == 0)
        return TCL_OK;          /* Nothing to do */
    
    status = Tcl_ListObjGetElements(ip, orows, &nrows, &rows);
    if (status != TCL_OK || nrows == 0)
        return status;          /* Maybe OK or ERROR */

    if (pindices->used > nrows)
        return ta_indices_count_error(ip, pindices->used, nrows);

    if ((status = tcols_validate_obj_rows(ip, ntcols, tcols, nrows, rows)) != TCL_OK)
        return status;
    
    /* Note we will panic on failures because all validity checks are done */
#define tcols_place_COPY(type, fn)                                      \
    do {                                                                \
        type *p;                                                        \
        p = THDRELEMPTR(TARRAYHDR(tcols[i]), type, 0);                  \
        while (pindex < end) {                                          \
            TA_ASSERT(*pindex < TARRAYHDR(tcols[i])->usable);           \
            TA_NOFAIL(Tcl_ListObjIndex(ip, *prow++, i, &o), TCL_OK);    \
            TA_ASSERT(o != NULL);                                       \
            TA_NOFAIL(fn(ip, o, &p[*pindex++]), TCL_OK);                \
        }                                                               \
    } while (0)

    for (i = 0; i < ntcols; ++i) {
        int *pindex, *end;

        TARRAYHDR(tcols[i])->sort_order = THDR_UNSORTED;
        pindex = THDRELEMPTR(pindices, int, 0);
        end = pindex + pindices->used;
        prow = rows;
        switch (tcol_type(tcols[i])) {
        case TA_UINT:
            tcols_place_COPY(unsigned int, ta_get_uint_from_obj);
            break;
        case TA_INT:
            tcols_place_COPY(int, Tcl_GetIntFromObj);
            break;
        case TA_WIDE:
            tcols_place_COPY(Tcl_WideInt, Tcl_GetWideIntFromObj);
            break;
        case TA_DOUBLE:
            tcols_place_COPY(double, Tcl_GetDoubleFromObj);
            break;
        case TA_BYTE:
            tcols_place_COPY(unsigned char, ta_get_byte_from_obj);
            break;
        case TA_BOOLEAN:
            {
                ba_t *baP = THDRELEMPTR(TARRAYHDR(tcols[i]), ba_t, 0);
                while (pindex < end) {
                    int bval;
                    TA_ASSERT(*pindex < TARRAYHDR(tcols[i])->usable);
                    TA_NOFAIL(Tcl_ListObjIndex(ip, *prow++, i, &o), TCL_OK);
                    TA_ASSERT(o != NULL);
                    TA_NOFAIL(Tcl_GetBooleanFromObj(ip, o, &bval), TCL_OK);
                    ba_put(baP, *pindex++, bval);
                }
            }
            break;
        case TA_ANY:
            {
                Tcl_Obj **pobjs;
                int j;

                pobjs = THDRELEMPTR(TARRAYHDR(tcols[i]), Tcl_Obj *, 0);

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
                for (j = tcol_occupancy(tcols[i]); j < new_size; ++j)
                    pobjs[j] = NULL;        /* TBD - optimization - memset ? */
                while (pindex < end) {
                    /* Careful about the order here! */
                    TA_ASSERT(*pindex < TARRAYHDR(tcols[i])->usable);
                    TA_NOFAIL(Tcl_ListObjIndex(ip, *prow++, i, &o), TCL_OK);
                    TA_ASSERT(o != NULL);
                    Tcl_IncrRefCount(o);
                    if (pobjs[*pindex] != NULL)
                        Tcl_DecrRefCount(pobjs[*pindex]);/* Deref what was originally in that slot */
                    pobjs[*pindex++] = o;
                }               
            }
            break;
        default:
            ta_type_panic(tcol_type(tcols[i]));
        }

        TARRAYHDR(tcols[i])->used = new_size;
    }

    return TCL_OK;
}

TCL_RESULT tcols_place_indices(Tcl_Interp *ip, int ntcols, Tcl_Obj * const *tcols, Tcl_Obj * const *srccols, thdr_t *pindices, int new_size)
{
    int i;

    TA_ASSERT(pindices->type == TA_INT);
    
    if (ntcols == 0 || pindices->used == 0)
        return TCL_OK;          /* Nothing to do */

    for (i = 0; i < ntcols; ++i) {
        TA_ASSERT(! Tcl_IsShared(tcols[i]));
        TA_ASSERT(tcol_affirm(tcols[i]));
        TA_ASSERT(tcol_affirm(srccols[i]));
        TA_ASSERT(! thdr_shared(TARRAYHDR(tcols[i])));
        TA_ASSERT(TARRAYHDR(tcols[i])->usable >= new_size);

        if (tcol_type(tcols[i]) != tcol_type(srccols[i]))
            return ta_mismatched_types_error(ip, tcol_type(tcols[i]), tcol_type(srccols[i]));
        if (pindices->used > tcol_occupancy(srccols[i]))
            return ta_indices_count_error(ip, pindices->used, tcol_occupancy(srccols[i]));
    }

    /* Now all validation done, do the actual copy */
    for (i = 0; i < ntcols; ++i) {
        thdr_place_indices(ip, TARRAYHDR(tcols[i]), TARRAYHDR(srccols[i]),
                           pindices, new_size);
    }
    return TCL_OK;
}


TCL_RESULT table_put_objs(Tcl_Interp *ip, Tcl_Obj *table,
                          Tcl_Obj *orows,
                          Tcl_Obj *ofirst, /* NULL -> end of table */
                          int insert)
{
    int off, nrows, old_size, status;
    Tcl_Obj **rows;

    TA_ASSERT(! Tcl_IsShared(table));

    status = Tcl_ListObjGetElements(ip, orows, &nrows, &rows);
    if (status != TCL_OK ||     /* Not a list */
        nrows == 0 ||           /* Nothing to modify */
        (status = table_convert(ip, table)) != TCL_OK || /* Not a table */
        table_width(table) == 0) /* No columns to update */ {
        return status;           /* Maybe OK or ERROR */
    }

    /* Get the limits of the range to set */
    old_size = table_length(table);
    off = old_size;
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &off, old_size, 0, old_size);

    /* n contains starting offset (end if not specified) */
    if (status == TCL_OK) {

        /* Note this also invalidates the string rep as desired */
        status = table_make_modifiable(ip, table,
                                       (insert ? old_size : off) + nrows,
                                       0);
        if (status == TCL_OK) {
            /* Note even on error tcols_put_objs guarantees a consistent 
             * and unchanged tcols
             */
            status = tcols_put_objs(ip, table_width(table),
                                    table_columns(table),
                                    nrows, rows, off, insert);
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

TCL_RESULT table_copy(Tcl_Interp *ip, Tcl_Obj *dstable, Tcl_Obj *srctable,
                      Tcl_Obj *ofirst, /* NULL -> end of table */
                      int insert)
{
    int first, status;
    Tcl_Obj **dstcols;
    Tcl_Obj **srccols;
    int count, new_min_size;

    TA_ASSERT(! Tcl_IsShared(dstable));

    if ((status = table_convert(ip, dstable)) != TCL_OK ||
        (status = table_convert(ip, srctable)) != TCL_OK)
        return status;

    if (table_width(dstable) > table_width(srctable))
        return ta_row_width_error(ip, table_width(srctable), table_width(srctable));

    srccols = table_columns(srctable);
    count = tcol_occupancy(srccols[0]);
    dstcols = table_columns(dstable);
    first = tcol_occupancy(dstcols[0]); /* Default is end */
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &first, first, 0, first);
    if (status != TCL_OK)
        return status;

    if (insert)
        new_min_size = tcol_occupancy(dstcols[0]) + count;
    else
        new_min_size = first + count;
    status = table_make_modifiable(ip, dstable, new_min_size, 0);
    if (status != TCL_OK)
        return status;

    dstcols = table_columns(dstable); /* Re-init - might have changed */

    return tcols_copy(ip, table_width(dstable), dstcols, first,
                      srccols, 0, count, insert);
}

TCL_RESULT table_delete(Tcl_Interp *ip, Tcl_Obj *table,
                        Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int status;
    int i;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK ||
        (status = table_make_modifiable(ip, table, table_length(table), 0)) != TCL_OK)
        return status;

    i = table_width(table);
    while (i--) {
        if ((status = tcol_delete(ip, table_column(table, i),
                                  indexa, indexb)) != TCL_OK)
            break;
    }
    return status;
}

Tcl_Obj *table_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, int fmt)
{
    int i, width, *pindex, *end, index;
    Tcl_Obj **srccols;
    Tcl_Obj *olist = NULL;
    Tcl_Obj **olistelems;

    if (table_convert(ip, osrc) != TCL_OK)
        return NULL;

    /* TBD - TA_ASSERT validate table consistency */

    width = table_width(osrc);
    srccols = table_columns(osrc);

    TA_ASSERT(pindices->type == TA_INT);

    /* Special case when no columns are defined for the table but indices are specified */
    if (width == 0 && pindices->used) {
        index = *THDRELEMPTR(pindices, int, 0);
        goto index_error;
    }

    if (fmt == TA_FORMAT_TARRAY) {
        Tcl_Obj **tcols;
        thdr_t *thdr;

        if ((thdr = thdr_alloc(ip, TA_ANY, width)) == NULL)
            return NULL;
        tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        for (i = 0; i < width; ++i) {
            tcols[i] = tcol_get(ip, srccols[i], pindices, TA_FORMAT_TARRAY);
            if (tcols[i])
                thdr->used++; /* Update as we go so freeing on error simpler */
            else {
                thdr_decr_refs(thdr);
                return NULL;
            }
        }
        return table_new(thdr);
    }

    /*
     * We have to return as either a list or a dict. To create a list, we
     * we will preallocate a list of the same size as pindices. We will
     * the loop through each column appending the value to the corresponding
     * slot in that list. Dicts are treated the same way except that the
     * values are alternated with the indices. Note creating a dict as a
     * list and letting it shimmer when necessary is more efficient than
     * creating it as a dict.
     */
    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + pindices->used;
    if (fmt == TA_FORMAT_DICT) {
        olist = Tcl_NewListObj(2*pindices->used, NULL);
        while (pindex < end) {
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewIntObj(*pindex++));
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(width, NULL));
        }        
    } else {
        olist = Tcl_NewListObj(pindices->used, NULL);
        i = pindices->used;
        while (i--)
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(width, NULL));
    }

    Tcl_ListObjGetElements(ip, olist, &i, &olistelems); /* i just dummy temp */

#define table_get_COPY(type_, objfn_)                                   \
    do {                                                                \
        type_ *from = srcbase;                                          \
        for (; pindex < end; j += incr, ++pindex) {                     \
            index = *pindex;    /* Should hold index in error */ \
            if (index < 0 || index >= bound)                            \
                goto index_error;                                       \
            Tcl_ListObjAppendElement(ip, olistelems[j], objfn_(from[index])); \
        } \
    } while (0)

    for (i = 0; i < width; ++i) {
        void *srcbase;
        int j, incr, bound;

        pindex = THDRELEMPTR(pindices, int, 0);
        end = pindex + pindices->used;
        srcbase = THDRELEMPTR(TARRAYHDR(srccols[i]), unsigned char, 0);
        bound = tcol_occupancy(srccols[i]);
        if (fmt == TA_FORMAT_DICT) {
            /* Values are in alternate slots since mixed with indices */
            j = 1;
            incr = 2;
        } else {
            j = 0;
            incr = 1;
        }
            
        switch (tcol_type(srccols[i])) {
        case TA_BOOLEAN:
            {
                ba_t *srcbaP = srcbase;
                for (; pindex < end; j += incr, ++pindex) {
                    index = *pindex;
                    if (index < 0 || index >= bound)
                        goto index_error;
                    Tcl_ListObjAppendElement(ip, olistelems[j],
                                             Tcl_NewIntObj(ba_get(srcbaP, index)));
                }
            }
        case TA_UINT:
            table_get_COPY(unsigned int, Tcl_NewWideIntObj);
            break;
        case TA_INT:
            table_get_COPY(int, Tcl_NewIntObj);
            break;
        case TA_WIDE:
            table_get_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
            break;
        case TA_DOUBLE:
            table_get_COPY(double, Tcl_NewDoubleObj);
            break;
        case TA_BYTE:
            table_get_COPY(unsigned char, Tcl_NewIntObj);
            break;
        case TA_ANY:
            /* We can use macro here as well because of ref counts will be
               taken care of by the lists themselves. The (Tcl_Obj *) is
               passed as essentially a no-op conversion function
            */
            table_get_COPY(Tcl_Obj *, (Tcl_Obj *));
            break;
        default:
            ta_type_panic(tcol_type(srccols[i]));
        }
    }

    return olist;

index_error:   /* index should hold the current index in error */
    ta_index_range_error(ip, index);

    if (olist)
        Tcl_DecrRefCount(olist);
    return NULL;
}

Tcl_Obj *table_range(Tcl_Interp *ip, Tcl_Obj *osrc, int low, int count, int fmt)
{
    int i, width, end;
    Tcl_Obj **srccols;
    Tcl_Obj *olist = NULL;
    Tcl_Obj **olistelems;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    /* TBD - TA_ASSERT validate table consistency */

    if (table_convert(ip, osrc) != TCL_OK)
        return NULL;
    width = table_width(osrc);
    srccols = table_columns(osrc); /* Note srccols[0] invalid if width == 0 ! */

    if (fmt == TA_FORMAT_TARRAY) {
        Tcl_Obj **tcols;
        thdr_t *thdr;

        if ((thdr = thdr_alloc(ip, TA_ANY, width)) == NULL)
            return NULL;
        tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        for (i = 0; i < width; ++i) {
            tcols[i] = tcol_range(ip, srccols[i], low, count, TA_FORMAT_TARRAY);
            if (tcols[i])
                thdr->used++; /* Update as we go so freeing on error simpler */
            else {
                thdr_decr_refs(thdr);
                return NULL;
            }
        }
        return table_new(thdr);
    }

    /*
     * We have to return as either a list or a dict. To create a list, we
     * we will preallocate a list of the same size as pindices. We will
     * the loop through each column appending the value to the corresponding
     * slot in that list. Dicts are treated the same way except that the
     * values are alternated with the indices. Note creating a dict as a
     * list and letting it shimmer when necessary is more efficient than
     * creating it as a dict.
     */
    if (width == 0)
        return Tcl_NewListObj(0, NULL); /* Table has no columns */
    end = low + count;
    if (end > tcol_occupancy(srccols[0]))
        end = tcol_occupancy(srccols[0]);
    count = end-low;
        
    if (fmt == TA_FORMAT_DICT) {
        olist = Tcl_NewListObj(2*count, NULL);
        for (i = low; i < end; ++i) {
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewIntObj(i));
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(width, NULL));
        }        
    } else {
        olist = Tcl_NewListObj(count, NULL);
        i = count;
        while (i--)
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(width, NULL));
    }

    Tcl_ListObjGetElements(ip, olist, &i, &olistelems); /* i just dummy temp */
                
#define table_range_COPY(type_, objfn_)                                 \
    do {                                                                \
        type_ *p = THDRELEMPTR(TARRAYHDR(srccols[i]), type_, low);      \
        type_ *pend = p + count;                                    \
        while (p < pend) {                                              \
            Tcl_ListObjAppendElement(ip, olistelems[j], objfn_(*p++));  \
            j += incr;                                                  \
        }                                                               \
    } while (0)                                                         \

    for (i = 0; i < width; ++i) {
        int j, incr;

        if (fmt == TA_FORMAT_DICT) {
            /* Values are in alternate slots since mixed with indices */
            j = 1;
            incr = 2;
        } else {
            j = 0;
            incr = 1;
        }
            
        switch (tcol_type(srccols[i])) {
        case TA_BOOLEAN:
            {
                ba_t *srcbaP = THDRELEMPTR(TARRAYHDR(srccols[i]), ba_t, 0);
                int k;
                for (k = low; k < end; j += incr, ++k) {
                    Tcl_ListObjAppendElement(ip, olistelems[j],
                                             Tcl_NewIntObj(ba_get(srcbaP, k)));
                }
            }
            break;
        case TA_UINT:
            table_range_COPY(unsigned int, Tcl_NewWideIntObj);
            break;
        case TA_INT:
            table_range_COPY(int, Tcl_NewIntObj);
            break;
        case TA_WIDE:
            table_range_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
            break;
        case TA_DOUBLE:
            table_range_COPY(double, Tcl_NewDoubleObj);
            break;
        case TA_BYTE:
            table_range_COPY(unsigned char, Tcl_NewIntObj);
            break;
        case TA_ANY:
            /* We can use macro here as well because of ref counts will be
               taken care of by the lists themselves. The (Tcl_Obj *) is
               passed as essentially a no-op conversion function
            */
            table_range_COPY(Tcl_Obj *, (Tcl_Obj *));
            break;
        default:
            ta_type_panic(tcol_type(srccols[i]));
        }
    }

    return olist;
}

Tcl_Obj *table_index(Tcl_Interp *ip, Tcl_Obj *table, int index)
{
    int i, width;
    Tcl_Obj **srccols;
    Tcl_Obj *olist = NULL;
    Tcl_Obj *o;

    /* TBD - TA_ASSERT validate table consistency */
    if (table_convert(ip, table) != TCL_OK)
        return NULL;
    width = table_width(table);
    if (width == 0) {
        ta_index_range_error(ip, index);
        return NULL;
    }

    srccols = table_columns(table);
    olist = Tcl_NewListObj(width, NULL);
    for (i = 0; i < width; ++i) {
        o = tcol_index(ip, srccols[i], index);
        if (o)
            Tcl_ListObjAppendElement(ip, olist, o);
        else {
            Tcl_DecrRefCount(olist);
            return NULL;
        }
    }
    return olist;
}

TCL_RESULT table_insert_obj(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Obj *ocount)
{
    int status;

    TA_ASSERT(! Tcl_IsShared(table));
    
    if (ocount == NULL) {
        /* Values may be given as a column or a list */
        if ((status = table_convert(NULL, ovalue)) == TCL_OK)
            status =  table_copy(ip, table, ovalue, opos, 1);
        else
            status =  table_put_objs(ip, table, ovalue, opos, 1);
    } else {
        int pos, count, col_len;
        if ((status = Tcl_GetIntFromObj(ip, ocount, &count)) == TCL_OK &&
            (status = table_convert(ip, table)) == TCL_OK) {

            if (count == 0)
                return TCL_OK;  /* Nothing to do */
            if (count > 0) {
                col_len = table_length(table);
                if ((status = table_make_modifiable(ip, table, count+col_len, 0)) == TCL_OK &&
                    (status = ta_convert_index(ip, opos, &pos, col_len,
                                               0, col_len)) == TCL_OK) {
                    status = tcols_fill_range(ip, table_width(table),
                                              table_columns(table), ovalue,
                                              pos, count, 1);
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

TCL_RESULT table_place_objs(Tcl_Interp *ip, Tcl_Obj *table,
                           Tcl_Obj *orows,
                           Tcl_Obj *oindices)
{
    thdr_t *pindices;
    Tcl_Obj **tcols;
    int ntcols;
    int new_size;
    int status;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK || 
        (ntcols = table_width(table)) == 0) 
        return status;           /* Maybe OK or ERROR */

    tcols = table_columns(table);

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        status = thdr_verify_indices(ip, TARRAYHDR(tcols[0]), pindices, &new_size);
        if (status == TCL_OK) {
            status = table_make_modifiable(ip, table, new_size, new_size);
            if (status == TCL_OK) {
                tcols = table_columns(table); /* (table_make_modifiable) */
                status =  tcols_place_objs(ip, ntcols, tcols, pindices, orows, new_size);
            }
        }
    }
    
    thdr_decr_refs(pindices);
    return status;
}


TCL_RESULT table_place_indices(Tcl_Interp *ip, Tcl_Obj *table,
                           Tcl_Obj *psrc, Tcl_Obj *oindices)
{
    thdr_t *pindices;
    Tcl_Obj **tcols;
    int ntcols;
    int new_size;
    int status;

    TA_ASSERT(! Tcl_IsShared(table));
    TA_ASSERT(table_affirm(psrc));

    if ((status = table_convert(ip, table)) != TCL_OK || 
        (ntcols = table_width(table)) == 0) 
        return status;           /* Maybe OK or ERROR */

    if (table_width(psrc) < ntcols)
        return ta_row_width_error(ip, table_width(psrc), ntcols);

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        tcols = table_columns(table);
        status = thdr_verify_indices(ip, TARRAYHDR(tcols[0]), pindices, &new_size);
        if (status == TCL_OK) {
            status = table_make_modifiable(ip, table, new_size, new_size);
            if (status == TCL_OK) {
                tcols = table_columns(table); /* (table_make_modifiable) */
                status =  tcols_place_indices(ip, ntcols, tcols, table_columns(psrc), pindices, new_size);
            }
        }
    }
    
    thdr_decr_refs(pindices);
    return status;
}

TCL_RESULT table_reverse(Tcl_Interp *ip, Tcl_Obj *table)
{
    int i, status;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK ||
        (status = table_make_modifiable(ip, table, 0, 0)) != TCL_OK)
        return status;

    i = table_width(table);
    while (i--) {
        TA_NOFAIL(tcol_reverse(ip, table_column(table, i)), TCL_OK);
    }

    return TCL_OK;
}
