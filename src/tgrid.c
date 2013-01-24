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
        status = ta_fix_range_bounds(ip, tcol_occupancy(tcolPP[0]), indexa,
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
                          Tcl_Obj *orows,
                          Tcl_Obj *ofirst, /* NULL -> end of grid */
                          int insert)
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
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &n, n, 0, n);
    /* n contains starting offset (end if not specified) */
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

TCL_RESULT tgrid_copy(Tcl_Interp *ip, Tcl_Obj *dstgrid, Tcl_Obj *srcgrid,
                      Tcl_Obj *ofirst, /* NULL -> end of grid */
                      int insert)
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

    first = tcol_occupancy(dstcols[0]); /* Default is end */
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &first, first, 0, first);

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

Tcl_Obj *tgrid_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, int fmt)
{
    int i, width, *pindex, *end, index;
    Tcl_Obj **srccols;
    Tcl_Obj *olist = NULL;
    Tcl_Obj **olistelems;

    if (tgrid_convert(ip, osrc) != TCL_OK)
        return NULL;
    width = tgrid_width(osrc);
    srccols = tgrid_columns(osrc);

    TA_ASSERT(pindices->type == TA_INT);

    if (fmt == TA_FORMAT_TARRAY) {
        Tcl_Obj **tcols;
        thdr_t *thdr;

        if ((thdr = thdr_alloc(ip, TA_OBJ, tgrid_width(osrc))) == NULL)
            return NULL;
        tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        for (i = 0; i < width; ++i) {
            tcols[i] = tcol_get(ip, srccols[i], pindices, TA_FORMAT_TARRAY);
            if (tcols[i] == NULL) {
                thdr_decr_refs(thdr);
                return NULL;
            }
            thdr->used++; /* Update as we go so freeing on error is simpler */
        }
        return tgrid_new(thdr);
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

#define tgrid_get_COPY(type_, objfn_)                                   \
    do {                                                                \
        type_ *from = srcbase;                                          \
        for (; pindex < end; j += incr, ++pindex) {                     \
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
            tgrid_get_COPY(unsigned int, Tcl_NewWideIntObj);
            break;
        case TA_INT:
            tgrid_get_COPY(int, Tcl_NewIntObj);
            break;
        case TA_WIDE:
            tgrid_get_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
            break;
        case TA_DOUBLE:
            tgrid_get_COPY(double, Tcl_NewDoubleObj);
            break;
        case TA_BYTE:
            tgrid_get_COPY(unsigned char, Tcl_NewIntObj);
            break;
        case TA_OBJ:
            /* We can use macro here as well because of ref counts will be
               taken care of by the lists themselves. The (Tcl_Obj *) is
               passed as essentially a no-op conversion function
            */
            tgrid_get_COPY(Tcl_Obj *, (Tcl_Obj *));
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
