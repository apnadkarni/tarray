/*
 * Copyright (c) 2013-2018, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include <string.h>
#include "tcl.h"

#include "tarray.h"


/*
 * Struct to hold mapping of passed in data to columns in a table
 */
typedef struct column_map_s {
    int            *pmapped_indices;   /* Array mapping to column index. May
                                          point to either mapped_indices[] or
                                          allocated memory. */
    int             mapped_indices_count; /* Number of elements of above */

    Tcl_Obj       **pmapped_columns;  /* Array of mapped columns. Maybe 
                                         NULL indicating not initialized, or
                                         point to mapped_columns[] or
                                         allocated memory */
    int             mapped_column_count; /* Number of elements of above */

    /* TBD - actually this may not be needed here. If columns cannot be repeated
       in the map, then to check if all columns are specified checking 
       mapped_indices_count against table width is sufficient. The usage array
       can then be internal to the function building the map */
    unsigned char  *pcolumn_usage; /* Array of flags, one per table column.
                                      If non-0, corresponding column
                                      is included in the map. May point
                                      to column_usage[] or allocated memory.
                                      Number of entries is equal to number
                                      of columns in table.
                                   */
    /* Following are static areas to avoid memory allocation */
    Tcl_Obj       *mapped_columns[10];
    int            mapped_indices[10];
    unsigned char  column_usage[40];
} column_map_t;


/*
 * A TArray table is a Tcl_Obj type used for densely storing an array
 * of TArray columns. The Tcl_Obj internal rep contains a pointer to
 * to a thdr_t internal representation and mapping from column names
 * to indices.
 */
static void table_type_dup(Tcl_Obj *psrc, Tcl_Obj *pdst);
static void table_type_free_intrep(Tcl_Obj *o);
struct Tcl_ObjType ta_table_type = {
    "tarray_table",
    table_type_free_intrep,
    table_type_dup,
    ta_update_string_for_variable_element_size,
    NULL,     /* jenglish advises to keep this NULL */
};

/******************************************************************/

/* Panics on consistency check failure. int return value so it can
 be called from TA_ASSERT */
int table_check(Tcl_Interp *ip, Tcl_Obj *otab)
{
    thdr_t *thdr;
    Tcl_Obj **tcols;
    int ncols;
    int i;

    if (table_convert(ip, otab) != TCL_OK || ! table_affirm(otab))
        Tcl_Panic("Tcl_Obj is not a table");

    thdr = table_thdr(otab);
    if (thdr == NULL)
        Tcl_Panic("NULL thdr in Tcl_Obj");
    if (thdr->type != TA_ANY)
        Tcl_Panic("Table thdr->type not TA_ANY");
    if (thdr->nrefs < 1)
        Tcl_Panic("Table thdr->nrefs (%d) < 1", thdr->nrefs);
    
    ncols = thdr->used;
    tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    for (i = 0; i < ncols; ++i) {
        if (tcols[i]->refCount < 1)
            Tcl_Panic("Table column ref count (%d) < 1", tcols[i]->refCount);
        tcol_check(ip, tcols[i]);
    }

    return 1;
}

TCL_RESULT column_map_missing_columns_error(Tcl_Interp *ip)
{
    if (ip)
        Tcl_SetResult(ip, "All columns in a table must be specified in a column map when extending the table.", TCL_STATIC);
    return TCL_ERROR;
}

static void column_map_reset(column_map_t *pmap)
{
    if (pmap->pmapped_indices != pmap->mapped_indices) {
        TA_FREEMEM(pmap->pmapped_indices);
        pmap->pmapped_indices = pmap->mapped_indices;
    }
    if (pmap->pmapped_columns != pmap->mapped_columns) {
        TA_FREEMEM(pmap->pmapped_columns);
        pmap->pmapped_columns = pmap->mapped_columns;
    }
    if (pmap->pcolumn_usage != pmap->column_usage) {
        TA_FREEMEM(pmap->pcolumn_usage);
        pmap->pcolumn_usage = pmap->column_usage;
    }
}

static TCL_RESULT column_map_init(Tcl_Interp *ip, Tcl_Obj *omap, Tcl_Obj *table,
                            column_map_t *pmap)
{
    int n, width;
    Tcl_Obj **objs;

    pmap->pmapped_indices = pmap->mapped_indices;
    pmap->pmapped_columns = pmap->mapped_columns;
    pmap->pcolumn_usage = pmap->column_usage;
    pmap->mapped_column_count = 0;

    if (omap == NULL) {
        pmap->mapped_indices_count = 0;
        return TCL_OK;
    }

    if (Tcl_ListObjGetElements(ip, omap, &n, &objs) != TCL_OK)
        return TCL_ERROR;

    if (n == 0) {
        Tcl_SetResult(ip, "A column map must have at least one column specified.", TCL_STATIC);
        return TCL_ERROR;
    }

    if (n > ARRAYSIZE(pmap->mapped_indices))
        pmap->pmapped_indices = (int *) TA_ALLOCMEM(n * sizeof(*pmap->pmapped_indices));
    pmap->mapped_indices_count = n;

    TA_ASSERT(table_affirm(table));
    width = table_width(table);

    if (width > ARRAYSIZE(pmap->column_usage))
        pmap->pcolumn_usage = (unsigned char *) TA_ALLOCMEM(width * sizeof(*pmap->pcolumn_usage));
    memset(pmap->pcolumn_usage, 0, width*sizeof(*pmap->pcolumn_usage));

    while (n--) {
        int colnum;
        if (Tcl_GetIntFromObj(NULL, objs[n], &colnum) != TCL_OK &&
            table_parse_column_index(ip, table, objs[n], &colnum) != TCL_OK)
            goto error_handler;

        if (colnum < 0 || colnum >= width) {
            ta_column_index_error(ip, colnum);
            goto error_handler;
        }
        if (pmap->pcolumn_usage[colnum]) {
            ta_multiple_columns_error(ip, colnum);
            goto error_handler;
        }
        pmap->pcolumn_usage[colnum] = 1;
        pmap->pmapped_indices[n] = colnum;
    }
    
    return TCL_OK;
    
error_handler:
    column_map_reset(pmap);
    return TCL_ERROR;
}


TA_INLINE TCL_RESULT column_map_verify(
    Tcl_Interp *ip, column_map_t *pmap, int width, int cur_size, int new_size)
{

    /* If we are not growing the table, the column map can be a subset,
       else it must include all the columns */

    if (new_size <= cur_size)
        return TCL_OK;

    /* New size is larger so map must include all columns */

    /* Special case - identity mapping */
    if (pmap->mapped_indices_count == 0)
        return TCL_OK;

    /*
     * When building the map we do not allow duplicates. Therefore if
     * number of mapped indices is equal to number of table columns,
     * all columns are included in the map
     */
    if (pmap->mapped_indices_count == width)
        return TCL_OK;

    return column_map_missing_columns_error(ip);
}
                                                         
static TCL_RESULT column_map_get_columns(
    Tcl_Interp *ip, column_map_t *pmap, Tcl_Obj *table,
    Tcl_Obj ***pcolumns, int *pncolumns)
{
    TA_ASSERT(table_affirm(table));
    if (pmap->mapped_indices_count == 0) {
        /* Identity mapping */
        *pcolumns = table_columns(table);
        *pncolumns = table_width(table);
        return TCL_OK;
    }

    if (pmap->mapped_column_count == 0) {
        /* We have not yet mapped the columns */
        int n;
        Tcl_Obj **tcols = THDRELEMPTR(table_thdr(table), Tcl_Obj *, 0);
        n = pmap->mapped_indices_count;
        pmap->mapped_column_count = n;
        TA_ASSERT(pmap->pmapped_columns == pmap->mapped_columns);
        if (n > ARRAYSIZE(pmap->mapped_columns))
            pmap->pmapped_columns = TA_ALLOCMEM(n * sizeof(*pmap->pmapped_columns));
        while (n--)
            pmap->pmapped_columns[n] = tcols[pmap->pmapped_indices[n]];
    }

    TA_ASSERT(pmap->mapped_column_count == pmap->mapped_indices_count);

    *pcolumns = pmap->pmapped_columns;
    *pncolumns = pmap->mapped_column_count;
    return TCL_OK;
}

/* Never fails, Tcl_Obj returned without ref count incremented */
static Tcl_Obj *column_map_get_column_names(column_map_t *pmap, Tcl_Obj *table)
{
    Tcl_Obj *ocolnames, *ocolname;
    int i;

    TA_ASSERT(table_affirm(table));
    if (pmap->mapped_indices_count == 0) {
        /* Identity mapping */
        /* To avoid misunderstandings about ref counts, always return
           a duplicated column definition object */
        return Tcl_DuplicateObj(OBJCOLNAMES(table));
    }

    ocolnames = Tcl_NewListObj(0, NULL);
    for (i = 0; i < pmap->mapped_indices_count; ++i) {
        TA_NOFAIL(table_column_index_to_name(NULL, table, pmap->pmapped_indices[i], &ocolname), TCL_OK);
        Tcl_ListObjAppendElement(NULL, ocolnames, ocolname);
        Tcl_ListObjAppendElement(NULL, ocolnames, Tcl_NewIntObj(i));
    }
    return ocolnames;
}


TCL_RESULT tcols_fill_range(
    Tcl_Interp *ip,
    int ntcols,
    Tcl_Obj **tcols,            /* Must be unshared, no span and large enough */
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

    if (row_width != ntcols)
        return ta_row_width_error(ip, row_width, ntcols);

    if (row_width > sizeof(values)/sizeof(values[0]))
        pvalues = (ta_value_t *) TA_ALLOCMEM(row_width * sizeof(ta_value_t));

    col_len = tcol_occupancy(tcols[0]); /* #items in first column */
    /* Validate column lengths and value types. Start with i=0 since
       we need to validate corresponding value as well */
    for (i = 0; i < ntcols; ++i) {
        TA_ASSERT(! Tcl_IsShared(tcols[i]));
        if (tcol_occupancy(tcols[i]) != col_len) {
            status = ta_table_length_error(ip);
            goto vamoose;
        }
        status = ta_value_from_obj(ip, ovalues[i], tcol_type(tcols[i]), &pvalues[i]);
        if (status != TCL_OK)
            goto vamoose;
    }

    for (i = 0; i < ntcols; ++i) {
        thdr_t *thdr = tcol_thdr(tcols[i]);
        TA_ASSERT(! thdr_shared(thdr));
        TA_ASSERT(tcol_span(tcols[i]) == NULL);
        thdr_fill_range(ip, thdr, &pvalues[i], pos, count, insert);
        ta_value_clear(&pvalues[i]);
    }

    /* status will already contain TCL_OK */

vamoose:
    /* status contains TCL_OK or other code */
    /* ip must already hold error message in case of error */
    if (pvalues != values)
        TA_FREEMEM((char *) pvalues);

    return status;

}

/*
 * table type routines
 */
static void table_type_free_intrep(Tcl_Obj *o)
{ 
   thdr_t *thdr;
    Tcl_Obj *ocolnames;

    TA_ASSERT(table_affirm(o));

    thdr = OBJTHDR(o); 
    TA_ASSERT(thdr);
    thdr_decr_refs(thdr);
    OBJTHDR(o) = NULL;

    ocolnames = OBJCOLNAMES(o);
    TA_ASSERT(ocolnames);
    Tcl_DecrRefCount(ocolnames);
    OBJCOLNAMES(o) = NULL;
    
    o->typePtr = NULL;
}

static void table_type_dup(Tcl_Obj *osrc, Tcl_Obj *odst)
{
    TA_ASSERT(table_affirm(osrc));
    TA_ASSERT(OBJTHDR(osrc));
    TA_ASSERT(OBJCOLNAMES(osrc));
    
    table_set_intrep(odst, OBJTHDR(osrc), OBJCOLNAMES(osrc));
}

/* Never returns NULL */
Tcl_Obj *table_column_names (Tcl_Obj *otab)
{
    Tcl_Obj  *onames;
    Tcl_Obj **elems;
    int       i, nelems;

    TA_ASSERT(table_affirm(otab));
    TA_ASSERT(OBJCOLNAMES(otab));

    /* Note Dicts are guaranteed to return keys in same order as creation
       when iterated over so just by looping we get correct column order. */

    TA_NOFAIL(Tcl_ListObjGetElements(NULL, OBJCOLNAMES(otab), &nelems, &elems), TCL_OK);

    TA_ASSERT((nelems & 1) == 0);

    onames = Tcl_NewListObj(0, NULL);
    for (i = 0; i < nelems; i += 2) {
        Tcl_ListObjAppendElement(NULL, onames, elems[i]);
    }
    
    return onames;
}

/* Returns the name for a column. The returned Tcl_Obj does NOT have its
   ref count incremented! 
*/
TCL_RESULT table_column_index_to_name(
    Tcl_Interp *ip,             /* May be NULL */
    Tcl_Obj *otab,
    int colindex,
    Tcl_Obj **pname)
{
    Tcl_Obj *onames, *oname;
    int      i, status;

    TA_ASSERT(table_affirm(otab));

    onames = OBJCOLNAMES(otab);
    TA_ASSERT(onames);

    /* Note Dicts are guaranteed to return keys in same order as creation
       when iterated over so we assume names are in column index order */

    i = 2*colindex;
    status = Tcl_ListObjIndex(ip, onames, i, &oname);
    if (status == TCL_OK) {
        if (oname) {
            *pname = oname;
            /* Cross check */
            TA_ASSERT(Tcl_ListObjIndex(NULL, onames, i+1, &oname) == TCL_OK);
            TA_ASSERT(oname);
            TA_ASSERT(Tcl_GetIntFromObj(NULL, oname, &i) == TCL_OK);
            TA_ASSERT(i == colindex);
            return TCL_OK;
        }
        /* Not in range */
        status = ta_column_index_error(ip, colindex);
    }
    return status;
}

TCL_RESULT table_parse_column_index(Tcl_Interp *ip,
                                      Tcl_Obj *table, Tcl_Obj *oindex,
                                      int *pindex)
{
    int colindex;
    Tcl_Obj *onames, *o;

    TA_ASSERT(table_affirm(table));

    if (Tcl_GetIntFromObj(NULL, oindex, &colindex) == TCL_OK) {
        if (colindex < 0 || colindex >= table_width(table))
            return ta_column_index_error(ip, colindex);
        *pindex = colindex;
        return TCL_OK;
    }

    /* Not an integer, see if name */
    onames = OBJCOLNAMES(table);
    TA_ASSERT(onames);
              
    TA_NOFAIL(Tcl_DictObjGet(ip, onames, oindex, &o), TCL_OK);
    if (o == NULL)
        return ta_column_name_error(ip, oindex);

    TA_NOFAIL(Tcl_GetIntFromObj(NULL, o, &colindex), TCL_OK);
    TA_ASSERT(colindex < table_width(table));
    *pindex = colindex;
    return TCL_OK;
}


/*
 * Table manipulation functions
 */

TCL_RESULT tcols_fill_indices(
    Tcl_Interp *ip,
    int ntcols,
    Tcl_Obj **tcols,            /* Must be unshared, no span and large enough */
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

    if (row_width != ntcols)
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
    for (i = 0; i < ntcols; ++i) {
        TA_ASSERT(tcol_span(tcols[i]) == NULL);
        thdr_fill_indices(ip, tcol_thdr(tcols[i]), &pvalues[i], pindices, new_size);
        ta_value_clear(&pvalues[i]);
    }

    /* status will already be TCL_OK */

vamoose:
    /* status contains TCL_OK or other code */
    /* ip must already hold error message in case of error */
    if (pvalues != values)
        TA_FREEMEM((char *) pvalues);

    return status;

}

/* SHould only be called if o is not a table already */
TCL_RESULT table_convert_from_other(Tcl_Interp *ip, Tcl_Obj *o)
{
    int       i, ntcols, nelems, ncolnames, status;
    thdr_t   *thdr;
    Tcl_Obj **elems, **tcols, **colnames;
    Tcl_Obj  *colnames_map, *re;

    TA_ASSERT(! table_affirm(o));
    
    if (tcol_affirm(o) ||
        Tcl_ListObjGetElements(NULL, o, &nelems, &elems) != TCL_OK ||
        nelems != 3 ||
        strcmp(Tcl_GetString(elems[0]), ta_table_type.name)) {
        return ta_not_table_error(ip);
    }

    /* Do the columns */
    if ((status = Tcl_ListObjGetElements(ip, elems[2], &ntcols, &tcols))
        != TCL_OK)
        return status;
    for (i = 0; i < ntcols; ++i) {
        if ((status = tcol_convert(ip, tcols[i])) != TCL_OK)
            return status;
        /* All must have same length */
        if (tcol_occupancy(tcols[i]) != tcol_occupancy(tcols[0]))
            return ta_table_length_error(ip);
    }
    thdr = thdr_alloc_and_init(ip, TA_ANY, ntcols, tcols, ntcols);
    if (thdr == NULL)
        return TCL_ERROR;

    /* Now store the column names as name -> index pairs. */
    if ((status = Tcl_ListObjGetElements(ip, elems[1], &ncolnames, &colnames))
        != TCL_OK) {
        thdr_decr_refs(thdr);
        return status;
    }
    colnames_map = Tcl_NewListObj(0, NULL);
    re = Tcl_NewStringObj("^[_[:alpha:]][-_[:alnum:]]*$", -1);
    for (i = 0; i < ncolnames; ++i) {
        int match = Tcl_RegExpMatchObj(ip, colnames[i], re);
        if (match <= 0) {
            if (match == 0 && ip)
                Tcl_AppendResult(ip, "Invalid column name syntax '",
                                 Tcl_GetString(colnames[i]), "'.", NULL);
            Tcl_DecrRefCount(re);
            Tcl_DecrRefCount(colnames_map);
            thdr_decr_refs(thdr);
            return TCL_ERROR;
        }
        Tcl_ListObjAppendElement(ip, colnames_map, colnames[i]);
        Tcl_ListObjAppendElement(ip, colnames_map, Tcl_NewIntObj(i));
    }
    Tcl_DecrRefCount(re);

    /*
     * Get rid of old representation and stick in the new one. Note
     * string rep is NOT invalidated and must NOT be if it is shared.
     * In any case, no need to do so here.
     */
    if (o->typePtr && o->typePtr->freeIntRepProc) {
        o->typePtr->freeIntRepProc(o);
        o->typePtr = NULL;
    }

    table_set_intrep(o, thdr, colnames_map);
    return TCL_OK;
}

/* Makes the table data store modifiable. Does NOT make the column names
   modifiable */
TCL_RESULT table_make_modifiable(Tcl_Interp *ip,
                                Tcl_Obj *table,
                                 int minsize, /* Minsize of *contained* cols */
                                 int prefsize /* Pref size of contained cols */
    )
{
    int i, status;
    thdr_t *thdr;
    Tcl_Obj **tcols;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;
    
    /* Make the table object itself modifiable in case its thdr is shared */
    thdr = table_thdr(table);
    if (thdr_shared(thdr)) {
        thdr = thdr_clone(ip, thdr, 0, NULL);
        /* Note this also invalidates its string representation. */
        table_replace_intrep(table, thdr, NULL);
    } else {
        /* Not shared. Just invalidate the string rep. Was bug #18 */
        Tcl_InvalidateStringRep(table);
    }

    /* Now make its contained columns modifiable */
    tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    i = thdr->used;
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
        TA_ASSERT(! thdr_shared(tcol_thdr(tcol)));
        TA_ASSERT(tcol_span(tcol) == NULL);
    }

    return TCL_OK;
}

TCL_RESULT table_fill_obj(
    Tcl_Interp *ip,
    Tcl_Obj *table,
    Tcl_Obj *orow,
    Tcl_Obj *indexa,
    Tcl_Obj *indexb,             /* Can be NULL */
    Tcl_Obj *omap,
    int insert)
{
    int low, count;
    int status;
    int ncols;
    thdr_t *pindices = NULL;
    column_map_t colmap;
    int cur_size, new_size;
    Tcl_Obj **tcols;
    int is_range = 0;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;

    ncols = table_width(table);
    cur_size = table_length(table);

    if (indexb) {
        /* Given a range */
        if ((status = ta_fix_range_bounds(ip, cur_size, indexa, indexb, &low, &count)) != TCL_OK)
            return status;
        is_range = 1;
    } else {
        /* A single index arg, must be an index or an index column or list */
        switch (ta_obj_to_indices(ip, indexa, 1, cur_size - 1, &pindices, &low)) {
        case TA_INDEX_TYPE_ERROR:
            return TCL_ERROR;
        case TA_INDEX_TYPE_INT:
            if (low < 0 || low > cur_size)
                return ta_index_range_error(ip, low);
            count = 1;
            is_range = 1;       /* Treat as a range of size 1 */
            break;
        case TA_INDEX_TYPE_THDR:
            if (insert) {
                Tcl_SetResult(ip, "Internal error: attempt to use insert mode with index list", TCL_STATIC);
                return TCL_ERROR;
            }
            if ((status = thdr_verify_indices_in_range(ip, cur_size, pindices, &new_size)) != TCL_OK)
                return status;
            count = pindices->used;
            is_range = 0;
            break;
        }
    }

    /* For ranges, find the new size. We already know it if index list */
    if (is_range) {
        if (insert)
            new_size = cur_size + count;
        else
            new_size = (low + count) > cur_size ? (low + count) : cur_size ;
    }

    /*
     * Verify new size is compatible with column mapping, then make
     * the table modifiable, and only then retrieve the columns to
     * be changed (since making the table modifiable can change
     * allocation.
     */
    status = TCL_ERROR; /* Assume the worst */
    if (column_map_init(ip, omap, table, &colmap) == TCL_OK &&
        column_map_verify(ip, &colmap, ncols, cur_size, new_size) == TCL_OK &&
        table_make_modifiable(ip, table, new_size, 0) == TCL_OK &&
        column_map_get_columns(ip, &colmap, table, &tcols, &ncols) == TCL_OK) {
        if (count == 0)
            status = TCL_OK; /* No-op */
        else {
            if (is_range)
                status = tcols_fill_range(ip, ncols, tcols, orow,
                                          low, count, insert);
            else
                status = tcols_fill_indices(ip, ncols, tcols,
                                            orow, pindices, new_size);
        }
    }

    /*
     * status contains TCL_OK or other code
     * ip must already hold error message in case of error
     * colmap must have been initialized
     */
    column_map_reset(&colmap);
    if (pindices)
        thdr_decr_refs(pindices);
    return status;
}

TCL_RESULT tcols_validate_obj_row_widths(Tcl_Interp *ip, int width,
                                         int nrows, Tcl_Obj * const rows[])
{
    int r, i;
    for (r = 0; r < nrows; ++r) {
        if (Tcl_ListObjLength(ip, rows[r], &i) == TCL_ERROR)
            return TCL_ERROR;
        if (i != width)
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
        
        if (Tcl_ListObjGetElements(NULL, rows[r], &nfields, &fields) != TCL_OK) {
            Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid list syntax in row %d of source data.", r));
            return TCL_ERROR;
        }

        if (nfields != ntcols)
            return ta_invalid_source_row_width(ip, r, nfields, ntcols);

        for (t = 0; t < ntcols; ++t) {
            int res;
            int tatype = tcol_thdr(tcols[t])->type;
            switch (tatype) {
            case TA_BOOLEAN: res = Tcl_GetBooleanFromObj(NULL, fields[t], &v.ival); break;
            case TA_UINT:    res = ta_get_uint_from_obj(NULL, fields[t], &v.uival); break;
            case TA_INT:     res = Tcl_GetIntFromObj(NULL, fields[t], &v.ival); break;
            case TA_WIDE:    res = Tcl_GetWideIntFromObj(NULL, fields[t], &v.wval); break;
            case TA_DOUBLE:  res = Tcl_GetDoubleFromObj(NULL, fields[t], &v.dval); break;
            case TA_BYTE:    res = ta_get_byte_from_obj(NULL, fields[t], &v.ucval); break;
            case TA_ANY:     res = TCL_OK; break;      /* No validation */
            case TA_STRING:  res = TCL_OK; break;      /* No validation */
            default:         ta_type_panic(tatype);
            }
            if (res != TCL_OK)
                return ta_invalid_source_column_value(ip, r, t, tatype, fields[t]);
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
    int have_cols_that_need_validation;
    int need_data_validation;
    Tcl_Obj *oval;
    thdr_t *thdr, *thdr0;

    if (ntcols == 0 || nrows == 0)
        return TCL_OK;          /* Nought to do */

    thdr0 = tcol_thdr(tcols[0]);
    for (t = 0, have_cols_that_need_validation = 0; t < ntcols; ++t) {
        thdr = tcol_thdr(tcols[t]);
        TA_ASSERT(! Tcl_IsShared(tcols[t]));
        TA_ASSERT(! thdr_shared(thdr));
        TA_ASSERT(tcol_span(tcols[t]) == NULL);
        if (insert)
            TA_ASSERT(thdr->usable >= (thdr->used + nrows)); /* 'Nuff space */
        else
            TA_ASSERT(thdr->usable >= (first + nrows)); /* 'Nuff space */
        TA_ASSERT(thdr->used == thdr0->used); /* All same size */
        
        if (thdr->type != TA_ANY && thdr->type != TA_STRING)
            have_cols_that_need_validation = 1;
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
     * TA_ANY/TA_STRING add a complication. They do not need a type check
     * but because their reference counts have to be managed, it is more
     * complicated to back track on errors when we skip the validation
     * checks in the pure append case. So we update these columns
     * only after everything else has been updated.
     */

    if (! have_cols_that_need_validation) {
        /* Only TA_ANY/TA_STRING columns, data validation is a no-op */
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
         * We are doing this to simplify error rollback for TA_ANY/TA_STRING
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
     * any column is of type TA_ANY/TA_STRING, when an error occurs we have to
     * rollback that column's Tcl_Obj reference counts. Keeping track
     * of this is more involved using the second scheme and much simpler
     * with the first scheme. Hence we go with that.
     */

    /*
     * Now actually store the values. Note we still have to check
     * status on conversion in case we did not do checks when we are appending
     * to the end, and we have to store TA_ANY/TA_STRING last to facilitate
     * rollback on errors as discussed earlier.
     */
#define tcols_put_COPY(type, pos, fn)                            \
    do {                                                         \
        type *p;                                                 \
        Tcl_Obj *o;                                              \
        p = THDRELEMPTR(thdr, type, first);                      \
        for (r = 0; r < nrows; ++r, ++p) {                       \
            Tcl_ListObjIndex(ip, rows[r], pos, &o);              \
            TA_ASSERT(o);                                        \
            if (fn(ip, o, p) != TCL_OK)                          \
                goto error_return;                               \
        }                                                        \
    } while (0)

    if (have_cols_that_need_validation) {
        for (t=0; t < ntcols; ++t) {
            /* Skip TA_ANY on this round, until all other data is stored */
            thdr = tcol_thdr(tcols[t]);
            if (thdr->type == TA_ANY || thdr->type == TA_STRING)
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
                         * Offset is off within a ba_t. Get the ba_t at that 
                         * location preserving the preceding bits within
                         * the char.
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
                tcols_put_COPY(int, t, Tcl_GetIntFromObj);
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

    /* Now that no errors can occur, update the TA_ANY and TA_STRING columns */
    for (t=0; t < ntcols; ++t) {
        thdr = tcol_thdr(tcols[t]);
        if (thdr->type != TA_ANY && thdr->type != TA_STRING)
            continue;
        if (insert)
            thdr_make_room(thdr, first, nrows);
        thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
        if (thdr->type == TA_ANY) {
            register Tcl_Obj **pobjs;
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
        } else {
            /* TA_STRING */
            register tas_t **pptas;
            pptas = THDRELEMPTR(thdr, tas_t *, first);
            for (r = 0; r < nrows ; ++r, ++pptas) {
                Tcl_ListObjIndex(ip, rows[r], t, &oval);
                TA_ASSERT(oval);
                /* Release old elems only if we were not inserting */
                if (!insert) {
                    if ((first + r) < thdr->used) {
                        /* Deref what was originally in that slot */
                        tas_unref(*pptas);
                    }
                }
                *pptas = tas_from_obj(oval);
                thdr_lookup_add(thdr, first + r);
            }
        }
    }

    /* Now finally, update all the counts */
    for (t=0; t < ntcols; ++t) {
        if (insert) {
            tcol_thdr(tcols[t])->used += nrows;
        } else {
            if ((first + nrows) > tcol_thdr(tcols[t])->used)
                tcol_thdr(tcols[t])->used = first + nrows;
        }
    }
     
    return TCL_OK;

 error_return:                  /* Interp should already contain errors */
    return TCL_ERROR;
 }

static  TCL_RESULT tcols_place_objs(Tcl_Interp *ip, int ntcols,
                                    Tcl_Obj * const *tcols, Tcl_Obj *orows,
                                    thdr_t *pindices, int new_size)
 {
     Tcl_Obj **prow;
     int i, nrows, status;
     Tcl_Obj **rows;
     Tcl_Obj *o;

     TA_ASSERT(pindices->type == TA_INT);

     for (i = 0; i < ntcols; ++i) {
         TA_ASSERT(! Tcl_IsShared(tcols[i]));
         TA_ASSERT(tcol_affirm(tcols[i]));
         TA_ASSERT(! thdr_shared(tcol_thdr(tcols[i])));
         TA_ASSERT(tcol_span(tcols[i]) == NULL);
         TA_ASSERT(tcol_thdr(tcols[i])->usable >= new_size);
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
        p = THDRELEMPTR(tcol_thdr(tcols[i]), type, 0);                  \
        while (pindex < end) {                                          \
            TA_ASSERT(*pindex < tcol_thdr(tcols[i])->usable);           \
            TA_NOFAIL(Tcl_ListObjIndex(ip, *prow++, i, &o), TCL_OK);    \
            TA_ASSERT(o != NULL);                                       \
            TA_NOFAIL(fn(ip, o, &p[*pindex++]), TCL_OK);                \
        }                                                               \
    } while (0)

    for (i = 0; i < ntcols; ++i) {
        int *pindex, *end;

        tcol_thdr(tcols[i])->sort_order = THDR_UNSORTED;
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
                ba_t *baP = THDRELEMPTR(tcol_thdr(tcols[i]), ba_t, 0);
                while (pindex < end) {
                    int bval;
                    TA_ASSERT(*pindex < tcol_thdr(tcols[i])->usable);
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

                pobjs = THDRELEMPTR(tcol_thdr(tcols[i]), Tcl_Obj *, 0);

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
                    TA_ASSERT(*pindex < tcol_thdr(tcols[i])->usable);
                    TA_NOFAIL(Tcl_ListObjIndex(ip, *prow++, i, &o), TCL_OK);
                    TA_ASSERT(o != NULL);
                    Tcl_IncrRefCount(o);
                    if (pobjs[*pindex] != NULL)
                        Tcl_DecrRefCount(pobjs[*pindex]);/* Deref what was originally in that slot */
                    pobjs[*pindex++] = o;
                }               
            }
            break;

        case TA_STRING:
            {
                tas_t **pptas;
                thdr_t *cur_thdr;
                int j;

                cur_thdr = tcol_thdr(tcols[i]);
                pptas = THDRELEMPTR(cur_thdr, tas_t *, 0);

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
                for (j = cur_thdr->used; j < new_size; ++j)
                    pptas[j] = NULL;        /* TBD - optimization - memset ? */
                while (pindex < end) {
                    /* Careful about the order here! */
                    TA_ASSERT(*pindex < cur_thdr->usable);
                    TA_NOFAIL(Tcl_ListObjIndex(ip, *prow++, i, &o), TCL_OK);
                    TA_ASSERT(o != NULL);
                    if (pptas[*pindex] != NULL)
                        tas_unref(pptas[*pindex]);/* Deref original slot */
                    pptas[*pindex] = tas_from_obj(o);
                    thdr_lookup_add(cur_thdr, *pindex);
                    ++pindex;
                }               
            }
            break;

        default:
            ta_type_panic(tcol_type(tcols[i]));
        }

        tcol_thdr(tcols[i])->used = new_size;
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
        TA_ASSERT(! thdr_shared(tcol_thdr(tcols[i])));
        TA_ASSERT(tcol_span(tcols[i]) == NULL);
        TA_ASSERT(tcol_thdr(tcols[i])->usable >= new_size);

        if (tcol_type(tcols[i]) != tcol_type(srccols[i]))
            return ta_mismatched_types_error(ip, tcol_type(tcols[i]), tcol_type(srccols[i]));
        if (pindices->used > tcol_occupancy(srccols[i]))
            return ta_indices_count_error(ip, pindices->used, tcol_occupancy(srccols[i]));
    }

    /* Now all validation done, do the actual copy */
    for (i = 0; i < ntcols; ++i) {
        thdr_place_indices(ip, tcol_thdr(tcols[i]), OBJTHDR(srccols[i]),
                           OBJTHDRSPAN(srccols[i]), pindices, new_size);
    }
    return TCL_OK;
}


TCL_RESULT table_put_objs(Tcl_Interp *ip, Tcl_Obj *table,
                          Tcl_Obj *orows,
                          Tcl_Obj *ofirst, /* NULL -> end of table */
                          Tcl_Obj *omap,
                          int insert)
{
    int off, nrows, old_size, new_size, status, ntcols;
    Tcl_Obj **rows, **tcols;
    column_map_t colmap;

    TA_ASSERT(! Tcl_IsShared(table));

    /* 
     * Note on reference counting / shimmering (related to Bug 20)
     *  - table is expected to be unshared so no need to worry about that.
     *  - orows and ofirst might point to the same object. Thus
     *    we have to be careful to extract the integer value first
     *    and then shimmer orows to a list. Otherwise, the list intrep
     *    for ovalues might be shimmered away when ofirst is shimmered
     *    to int.
     */

    status = table_convert(ip, table);
    if (status != TCL_OK)
        return status;
    
    status = Tcl_ListObjLength(ip, orows, &nrows);
    
    if (status != TCL_OK ||     /* Not a list */
        nrows == 0 ||           /* Nothing to modify */
        table_width(table) == 0) /* No columns to update */ {
        return status;           /* Maybe OK or ERROR */
    }

    /* Get the limits of the range to set */
    old_size = table_length(table);
    /* off contains starting offset (end if not specified) */
    off = old_size;
    if (ofirst) {
        if ((status = ta_convert_index(ip, ofirst, &off, old_size, 0, old_size)) != TCL_OK)
            return status;
    }

    if (insert)
        new_size = old_size + nrows;
    else
        new_size = (off + nrows) > old_size ? (off + nrows) : old_size;

    if ((status = column_map_init(ip, omap, table, &colmap)) != TCL_OK)
        return status;

    /* Verify new size is compatible with column mapping */
    status = Tcl_ListObjGetElements(ip, orows, &nrows, &rows);
    if (status == TCL_OK) {
        status = TCL_ERROR;
        if (column_map_verify(ip, &colmap, table_width(table), old_size, new_size) == TCL_OK) {
            /* Actually only *subset* of columns need to be modifiable - TBD */
            if (table_make_modifiable(ip, table, new_size, new_size) == TCL_OK) {
                /* Note this must be AFTER table_make_modifiable as columns might change */
                if (column_map_get_columns(ip, &colmap, table, &tcols, &ntcols) == TCL_OK) {
                    /* Note even on error tcols_put_objs guarantees a consistent 
                     * and unchanged tcols
                     */
                    status = tcols_put_objs(ip, ntcols, tcols,
                                            nrows, rows, off, insert);
                }
            }
        }
    }

    column_map_reset(&colmap);
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
        TA_ASSERT(tcol_span(dstcols[i]) == NULL);
        TA_ASSERT(tcol_affirm(srccols[i]));
        TA_ASSERT(tcol_occupancy(dstcols[i]) == tcol_occupancy(dstcols[0]));

        pdst = tcol_thdr(dstcols[i]);
        psrc = tcol_thdr(srccols[i]);

        TA_ASSERT(! thdr_shared(pdst));
        TA_ASSERT(pdst->usable >= (insert ? pdst->used + count : dst_elem_first + count));

        if (pdst->type != psrc->type)
            return ta_mismatched_types_error(ip, pdst->type, psrc->type);
    }
    
    /* Now that *all* columns have been checked, do the actual copy */
    for (i = 0; i < ntcols; ++i) {
        thdr_t *src_thdr;
        span_t *src_span;
        int src_first, src_size;
        src_thdr = tcol_thdr(srccols[i]);
        src_span = tcol_span(srccols[i]);
        if (src_span) {
            src_first = src_span->first;
            src_size = src_span->count;
        } else {
            src_first = 0;
            src_size = src_thdr->used;
        }
        if ((src_elem_first + count) > src_size)
            count = src_size - src_elem_first; 
        thdr_copy(tcol_thdr(dstcols[i]), dst_elem_first,
                  tcol_thdr(srccols[i]), src_first + src_elem_first, count, insert);
    }    

    return TCL_OK;
}

/* Assumes properly structured thdr */
Tcl_Obj *table_new(thdr_t *thdr, Tcl_Obj *ocolumns)
{
    Tcl_Obj *o;

    TA_ASSERT(thdr->type == TA_ANY);
    TA_ASSERT(ocolumns);

    if (thdr == NULL)
        return NULL;
    o = Tcl_NewObj();
    Tcl_InvalidateStringRep(o);
    table_set_intrep(o, thdr, ocolumns);
    return o;
}


TCL_RESULT table_copy(Tcl_Interp *ip, Tcl_Obj *dstable, Tcl_Obj *srctable,
                      Tcl_Obj *ofirst, /* NULL -> end of table */
                      Tcl_Obj *omap,
                      int insert)
{
    int first, ntcols, status;
    Tcl_Obj **dstcols;
    Tcl_Obj **srccols;
    int count, cur_size, new_size;
    column_map_t colmap;

    TA_ASSERT(! Tcl_IsShared(dstable));

    if ((status = table_convert(ip, dstable)) != TCL_OK ||
        (status = table_convert(ip, srctable)) != TCL_OK)
        return status;

    srccols = table_columns(srctable);
    count = tcol_occupancy(srccols[0]);
    dstcols = table_columns(dstable);
    cur_size = tcol_occupancy(dstcols[0]);
    first = cur_size; /* Default is end */
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &first, first, 0, first);
    if (status != TCL_OK)
        return status;

    if (insert)
        new_size = cur_size + count;
    else
        new_size = (first + count) > cur_size ? (first + count) : cur_size;

    if ((status = column_map_init(ip, omap, dstable, &colmap)) != TCL_OK)
        return status;

    /* Verify new size is compatible with column mapping */
    if (column_map_verify(ip, &colmap, table_width(dstable), cur_size, new_size) == TCL_OK) {
        /* Actually only *subset* of columns need to be modifiable - TBD */
        if (table_make_modifiable(ip, dstable, new_size, new_size) == TCL_OK) {
            /* Note this must be AFTER table_make_modifiable as columns might change */
            if (column_map_get_columns(ip, &colmap, dstable, &dstcols, &ntcols) == TCL_OK) {
                if (ntcols != table_width(srctable))
                    status = ta_row_width_error(ip, table_width(srctable), ntcols);
                else
                    status = tcols_copy(ip, ntcols, dstcols, first,
                                        srccols, 0, count, insert);
            }
        }
    }

    column_map_reset(&colmap);
    return status;
}

TCL_RESULT table_delete(Tcl_Interp *ip, Tcl_Obj *table,
                        Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int status;
    int i;

    TA_ASSERT(! Tcl_IsShared(table));

    /* TBD - as for columns, make more efficient for the case where
       we are deleting from the front or the back. This means
       not calling table_make_modifiable since that will dup the columns
       as well.
    */
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

static Tcl_Obj *table_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, Tcl_Obj *omap, int fmt)
{
    int            i, nsrccols, index;
    int            *pindex, *end;
    Tcl_Obj      **srccols;
    Tcl_Obj       *olist = NULL;
    Tcl_Obj      **olistelems;
    column_map_t   colmap;

    if (table_convert(ip, osrc) != TCL_OK)
        return NULL;

    /* TBD - TA_ASSERT validate table consistency */

    TA_ASSERT(pindices->type == TA_INT);

    /* Do first since expect to do column_map_reset on returns */
    if (column_map_init(ip, omap, osrc, &colmap) != TCL_OK)
        return NULL;

    if (column_map_get_columns(ip, &colmap, osrc,
                               &srccols, &nsrccols) != TCL_OK) {
        column_map_reset(&colmap);
        return NULL;
    }

    /* Special case when no columns are defined for the table but indices are specified */
    if (nsrccols == 0 && pindices->used) {
        index = *THDRELEMPTR(pindices, int, 0);
        goto index_error;
    }
    
    if (fmt == TA_FORMAT_TARRAY) {
        Tcl_Obj **tcols;
        thdr_t *thdr;
        Tcl_Obj *otab;

        if ((thdr = thdr_alloc(ip, TA_ANY, nsrccols)) == NULL)
            goto error_handler;

        tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        for (i = 0; i < nsrccols; ++i) {
            tcols[i] = tcol_get(ip, srccols[i], pindices, TA_FORMAT_TARRAY);
            if (tcols[i]) {
                Tcl_IncrRefCount(tcols[i]);
                thdr->used++; /* Update as we go so freeing on error simpler */
            } else {
                thdr_decr_refs(thdr);
                goto error_handler;
            }
        }
        otab = table_new(thdr, column_map_get_column_names(&colmap, osrc));
        column_map_reset(&colmap);
        return otab;
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
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(nsrccols, NULL));
        }        
    } else {
        olist = Tcl_NewListObj(pindices->used, NULL);
        i = pindices->used;
        while (i--)
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(nsrccols, NULL));
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

    for (i = 0; i < nsrccols; ++i) {
        void *srcbase;
        int j, incr, bound, span_start;
        thdr_t *src_thdr;
        span_t *span;

        TA_ASSERT(tcol_affirm(srccols[i]));
        
        pindex = THDRELEMPTR(pindices, int, 0);
        end = pindex + pindices->used;
        src_thdr = OBJTHDR(srccols[i]);
        span = OBJTHDRSPAN(srccols[i]);
        if (span) {
            span_start = span->first;
            bound = span->count;
        } else {
            span_start = 0;
            bound = src_thdr->used;
        }
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
                ba_t *srcbaP = THDRELEMPTR(src_thdr, ba_t, 0);
                for (; pindex < end; j += incr, ++pindex) {
                    index = *pindex;
                    if (index < 0 || index >= bound)
                        goto index_error;
                    Tcl_ListObjAppendElement(ip, olistelems[j],
                                             Tcl_NewIntObj(ba_get(srcbaP, span_start+index)));
                }
            }
        case TA_UINT:
            srcbase = THDRELEMPTR(src_thdr, unsigned int, span_start);
            table_get_COPY(unsigned int, Tcl_NewWideIntObj);
            break;
        case TA_INT:
            srcbase = THDRELEMPTR(src_thdr, int, span_start);
            table_get_COPY(int, Tcl_NewIntObj);
            break;
        case TA_WIDE:
            srcbase = THDRELEMPTR(src_thdr, Tcl_WideInt, span_start);
            table_get_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
            break;
        case TA_DOUBLE:
            srcbase = THDRELEMPTR(src_thdr, double, span_start);
            table_get_COPY(double, Tcl_NewDoubleObj);
            break;
        case TA_BYTE:
            srcbase = THDRELEMPTR(src_thdr, unsigned char, span_start);
            table_get_COPY(unsigned char, Tcl_NewIntObj);
            break;
        case TA_ANY:
            /* We can use macro here as well because of ref counts will be
               taken care of by the lists themselves. The (Tcl_Obj *) is
               passed as essentially a no-op conversion function
            */
            srcbase = THDRELEMPTR(src_thdr, Tcl_Obj *, span_start);
            table_get_COPY(Tcl_Obj *, (Tcl_Obj *));
            break;
        case TA_STRING:
            srcbase = THDRELEMPTR(src_thdr, tas_t *, span_start);
            table_get_COPY(tas_t *, tas_to_obj);
            break;
        default:
            ta_type_panic(tcol_type(srccols[i]));
        }
    }

    column_map_reset(&colmap);
    return olist;

index_error:   /* index should hold the current index in error, colmap
                  must have been initialized */
    ta_index_range_error(ip, index);

error_handler:    /* colmap must have been initialized */
    column_map_reset(&colmap);
    if (olist)
        Tcl_DecrRefCount(olist);
    return NULL;
}

static Tcl_Obj *table_range(Tcl_Interp *ip, Tcl_Obj *osrc, int low, int count, Tcl_Obj *omap, int fmt)
{
    int i, nsrccols, end;
    Tcl_Obj **srccols;
    Tcl_Obj *olist = NULL;
    Tcl_Obj **olistelems;
    column_map_t   colmap;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    /* TBD - TA_ASSERT validate table consistency */

    if (table_convert(ip, osrc) != TCL_OK)
        return NULL;

    /* Do first since expect to do column_map_reset on returns */
    if (column_map_init(ip, omap, osrc, &colmap) != TCL_OK)
        return NULL;

    if (column_map_get_columns(ip, &colmap, osrc,
                               &srccols, &nsrccols) != TCL_OK) {
        column_map_reset(&colmap);
        return NULL;
    }

    if (fmt == TA_FORMAT_TARRAY) {
        Tcl_Obj **tcols;
        thdr_t *thdr;
        Tcl_Obj *otab;

        if ((thdr = thdr_alloc(ip, TA_ANY, nsrccols)) == NULL)
            return NULL;
        tcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        for (i = 0; i < nsrccols; ++i) {
            tcols[i] = tcol_range(ip, srccols[i], low, count, TA_FORMAT_TARRAY);
            if (tcols[i]) {
                Tcl_IncrRefCount(tcols[i]);
                thdr->used++; /* Update as we go so freeing on error simpler */
            } else {
                thdr_decr_refs(thdr);
                return NULL;
            }
        }

        otab = table_new(thdr, column_map_get_column_names(&colmap, osrc));
        column_map_reset(&colmap);
        return otab;
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
    if (nsrccols == 0)
        return Tcl_NewListObj(0, NULL); /* Table has no columns */
    end = low + count;
    if (end > tcol_occupancy(srccols[0]))
        end = tcol_occupancy(srccols[0]);
    count = end-low;

    if (fmt == TA_FORMAT_DICT) {
        olist = Tcl_NewListObj(2*count, NULL);
        for (i = low; i < end; ++i) {
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewIntObj(i));
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(nsrccols, NULL));
        }
    } else {
        olist = Tcl_NewListObj(count, NULL);
        i = count;
        while (i--)
            Tcl_ListObjAppendElement(ip, olist, Tcl_NewListObj(nsrccols, NULL));
    }

    Tcl_ListObjGetElements(ip, olist, &i, &olistelems); /* i just dummy temp */

#define table_range_COPY(type_, objfn_)                                 \
    do {                                                                \
        type_ *p = THDRELEMPTR(src_thdr, type_, src_first); \
        type_ *pend = p + count;                                    \
        while (p < pend) {                                              \
            Tcl_ListObjAppendElement(ip, olistelems[j], objfn_(*p++));  \
            j += incr;                                                  \
        }                                                               \
    } while (0)                                                         \

    for (i = 0; i < nsrccols; ++i) {
        int j, incr, src_first;
        thdr_t *src_thdr;
        span_t *src_span;

        if (fmt == TA_FORMAT_DICT) {
            /* Values are in alternate slots since mixed with indices */
            j = 1;
            incr = 2;
        } else {
            j = 0;
            incr = 1;
        }
        src_thdr = tcol_thdr(srccols[i]);
        src_span = tcol_span(srccols[i]);
        if (src_span) {
            TA_ASSERT((low+count) <= src_span->count);
            src_first = low + src_span->first;
        } else {
            TA_ASSERT((low+count) <= src_thdr->used);
            src_first = low;
        }

        switch (src_thdr->type) {
        case TA_BOOLEAN:
            {
                ba_t *srcbaP = THDRELEMPTR(src_thdr, ba_t, 0);
                int k;
                int src_end = src_first + count; /* Not same as "end" var which is logical end */
                for (k = src_first; k < src_end; j += incr, ++k) {
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
        case TA_STRING:
            /* We can use macro here as well for the same reason. */
            table_range_COPY(tas_t *, tas_to_obj);
            break;
        default:
            ta_type_panic(src_thdr->type);
        }
    }

    column_map_reset(&colmap);
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

TCL_RESULT table_insert_row(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, int count, Tcl_Obj *omap)
{
    int status;
    int pos, col_len, ncols;
    Tcl_Obj **tcols;
    column_map_t colmap;

    TA_ASSERT(! Tcl_IsShared(table));
    
    if ((status = table_convert(ip, table)) == TCL_OK) {

        col_len = table_length(table);

        if (column_map_init(ip, omap, table, &colmap) != TCL_OK ||
            column_map_verify(ip, &colmap, table_width(table), col_len, count+col_len) != TCL_OK)
            return TCL_ERROR;

        if (count > 0) {
            if ((status = table_make_modifiable(ip, table, count+col_len, 0)) == TCL_OK &&
                (status = ta_convert_index(ip, opos, &pos, col_len,
                                           0, col_len)) == TCL_OK &&
                (status = column_map_get_columns(ip, &colmap, table, &tcols, &ncols)) == TCL_OK) {
                status = tcols_fill_range(ip, ncols, tcols,
                                          ovalue, pos, count, 1);
            }
        } else if (count < 0) {
            status = ta_bad_count_error(ip, count);
        } else {
            status = TCL_OK; /* count == 0, nothing to do */
        }
    }
    return status;
}

TCL_RESULT table_inject_rows(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Obj *omap)
{
    TA_ASSERT(! Tcl_IsShared(table));
    
    /* Values may be given as a column or a list */
    if (table_convert(NULL, ovalue) == TCL_OK)
        return table_copy(ip, table, ovalue, opos, omap, 1);
    else
        return table_put_objs(ip, table, ovalue, opos, omap, 1);
}


TCL_RESULT table_place(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalues,
                       Tcl_Obj *oindices, Tcl_Obj *omap)
{
    thdr_t        *pindices;
    Tcl_Obj      **tcols;
    int            ntcols, cur_size, new_size, status;
    column_map_t   colmap;

    TA_ASSERT(! Tcl_IsShared(table));

    if ((status = table_convert(ip, table)) != TCL_OK || 
        (ntcols = table_width(table)) == 0) 
        return status;           /* Maybe OK or ERROR */

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    if (pindices->used == 0) {
        thdr_decr_refs(pindices);
        return TCL_OK;           /* Nothing to be done */
    }

    if ((status = column_map_init(ip, omap, table, &colmap)) != TCL_OK)
        return status;

    cur_size = table_length(table);
    status = TCL_ERROR;
    if (thdr_verify_indices_in_range(ip, cur_size, pindices, &new_size) != TCL_OK)
        goto vamoose;

    /* Verify new size is compatible with column mapping */
    if (column_map_verify(ip, &colmap, ntcols, cur_size, new_size) != TCL_OK)
        goto vamoose;

    /* Actually only *subset* of columns need to be modifiable - TBD */
    if (table_make_modifiable(ip, table, new_size, new_size) != TCL_OK)
        goto vamoose;

    /* Note this must be done AFTER table_make_modifiable as columns might change */
    if (column_map_get_columns(ip, &colmap, table, &tcols, &ntcols) != TCL_OK)
        goto vamoose;

    if (table_convert(NULL, ovalues) == TCL_OK) {
        /* Source values specified as a table */
        if (table_width(ovalues) != ntcols) {
            ta_row_width_error(ip, table_width(ovalues), ntcols);
            goto vamoose;
        }
        status =  tcols_place_indices(ip, ntcols, tcols, table_columns(ovalues), pindices, new_size);
    } else {
        /* Source values specified as a list of rows */
        status =  tcols_place_objs(ip, ntcols, tcols, ovalues, pindices, new_size);
    }
    
vamoose:
    /* Before jumping here, colmap must have been initialized, 
       pindices allocated and status must hold return value */
    column_map_reset(&colmap);
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

TCL_RESULT table_retrieve(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                          int command)
{
    Tcl_Obj *table, *omap;
    int      i, status, opt, minargs;
    int      fmt = TA_FORMAT_TARRAY;
    /* Note order of options matches switch below */
    static const char *table_retrieve_options[] = {
        "-table",
        "-list",
        "-dict",
        "-columns",
        NULL
    };

    minargs = command == TA_RETRIEVE_GET ? 2 : 3;

    if (objc < 1+minargs) {
        Tcl_WrongNumArgs(ip, 1, objv, command == TA_RETRIEVE_GET ? "?OPTIONS? TABLE INDEXLIST" : "?OPTIONS? TABLE LOW HIGH");
        return TCL_ERROR;
    }

    table = objv[objc-minargs];
    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;

    omap = NULL;
    for (i = 1; i < objc-minargs; ++i) {
        if ((status = ta_opt_from_obj(ip, objv[i], table_retrieve_options,
                                          "option", TCL_EXACT, &opt)) != TCL_OK)
            return TCL_ERROR;
        switch (opt) {
        case 0: fmt = TA_FORMAT_TARRAY; break;
        case 1: fmt = TA_FORMAT_LIST; break;
        case 2: fmt = TA_FORMAT_DICT; break;
        case 3:
            ++i;
            if (i >= objc-minargs) {
                ta_missing_arg_error(ip, "-columns");
                return TCL_ERROR;
            }
            omap = objv[i];
            break;
        }
    }
       
    if (command == TA_RETRIEVE_GET) {
        thdr_t  *pindices;

        if (ta_obj_to_indices(ip, objv[objc-1], 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
            return TCL_ERROR;

        table = table_get(ip, table, pindices, omap, fmt);
        thdr_decr_refs(pindices);
    } else {
        /* Range LOW HIGH */
        int low, count;
        status = ta_fix_range_bounds(ip, table_length(table),
                                     objv[objc-2], objv[objc-1],
                                     &low, &count);
        if (status != TCL_OK)
            return status;
        table = table_range(ip, table, low, count, omap, fmt);
    }

    if (table) {
        TA_ASSERT(fmt != TA_FORMAT_TARRAY || table_check(ip, table));
        Tcl_SetObjResult(ip, table);
        return TCL_OK;
    } else
        return TCL_ERROR;
}

TCL_RESULT table_get_column(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *colspec)
{
    thdr_t *thdr;
    Tcl_Obj *tcol;
    int status, pos;

    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;

    if ((status = table_parse_column_index(ip, table, colspec, &pos))
        != TCL_OK)
        return status;

    thdr = table_thdr(table);
    tcol = *THDRELEMPTR(thdr, Tcl_Obj*, pos);
    TA_ASSERT(tcol_check(ip, tcol));
    Tcl_SetObjResult(ip, tcol);
    return TCL_OK;
}

TCL_RESULT table_set_column(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *colspec, Tcl_Obj *newcol)
{
    thdr_t *thdr;
    Tcl_Obj *tcol;
    int tab_len, status, pos;

    TA_ASSERT(! Tcl_IsShared(table));
    if ((status = table_convert(ip, table)) != TCL_OK)
        return status;

    if ((status = table_parse_column_index(ip, table, colspec, &pos))
        != TCL_OK)
        return status;

    thdr = table_thdr(table);
    tcol = *THDRELEMPTR(thdr, Tcl_Obj*, pos);
    tab_len = tcol_occupancy(tcol);
    TA_ASSERT(tab_len == table_length(table));

    if (newcol == tcol)
        return TCL_OK;

    /*
     * Need to replace the column. Verify compatibility:
     *  - the argument must be a column
     *  - the type of the column must be the same
     *  - the length of the column must be the same
     */
    if ((status = tcol_convert(ip, newcol)) != TCL_OK)
        return status;

    if (tcol_type(newcol) != tcol_type(tcol))
        return ta_mismatched_types_error(ip, tcol_type(newcol),
                                         tcol_type(tcol));
    if (tcol_occupancy(newcol) != tab_len)
        return ta_column_lengths_error(ip);

    /* TBD - actually the contained columns need not be modifiable.
       only the table itself needs to be */
    if ((status = table_make_modifiable(ip, table, tab_len, tab_len)) != TCL_OK)
        return status;

    thdr = table_thdr(table);   /* Re-init since make_modifiable might change it */
    TA_ASSERT(thdr->nrefs == 1);
    /* Have to re-init tcol since that might be changed in above calls */
    tcol = *THDRELEMPTR(thdr, Tcl_Obj*, pos);
    Tcl_IncrRefCount(newcol);
    *THDRELEMPTR(thdr, Tcl_Obj*, pos) = newcol;
    Tcl_DecrRefCount(tcol);

    return TCL_OK;
}

static TCL_RESULT
table_put_parseargs(Tcl_Interp *ip, int objc,
                    Tcl_Obj * const *objv,
                    Tcl_Obj **potab, Tcl_Obj **povalues,
                    Tcl_Obj **pomap, Tcl_Obj **poff)
{
    if (objc < 3 || objc > 6) {
        if (ip)
            Tcl_WrongNumArgs(ip, 1, objv, "?-columns COLUMNMAP? TABLE VALUES ?POSITION?");
        return TCL_ERROR;
    }

    switch (objc) {
    case 3:
    case 4:
        /* Cannot contain an valid option. Parse as though no options */
        *pomap = NULL;
        *potab = objv[1];
        *povalues = objv[2];
        *poff = objc == 4 ? objv[3] : NULL;
        break;
    case 5:
    case 6:
        /* If valid, options must be present */
        /* To prevent shimmering, don't check for options if a tarray */
        if (strcmp(Tcl_GetString(objv[1]), "-columns"))
            return ta_invalid_opt_error(ip, Tcl_GetString(objv[1]));
        *pomap = objv[2];
        *potab = objv[3];
        *povalues = objv[4];
        *poff = objc == 6 ? objv[5] : NULL;
        break;
    }
    return TCL_OK;
}

TCL_RESULT table_put_cmd(ClientData clientdata, Tcl_Interp *ip,
                         int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table, *ovalues, *omap, *ofirst;
    int status;

    if ((status = table_put_parseargs(ip, objc, objv, &table, &ovalues, &omap, &ofirst)) != TCL_OK)
        return status;

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    /* Values may be given as a table or a Tcl list */
    if (table_convert(NULL, ovalues) == TCL_OK)
        status = table_copy(ip, table, ovalues, ofirst, omap, 0);
    else
        status = table_put_objs(ip, table, ovalues, ofirst, omap, 0);

    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT table_vput_cmd(ClientData clientdata, Tcl_Interp *ip,
                          int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *ovar, *table, *ovalues, *omap, *ofirst;
    int status;

    if ((status = table_put_parseargs(ip, objc, objv, &ovar, &ovalues, &omap, &ofirst)) != TCL_OK)
        return status;

    table = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    /* Values may be given as a table or a Tcl list */
    if (table_convert(NULL, ovalues) == TCL_OK)
        status = table_copy(ip, table, ovalues, ofirst, omap, 0);
    else
        status = table_put_objs(ip, table, ovalues, ofirst, omap, 0);

    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, ovar, table);
}

static TCL_RESULT
table_fill_parseargs(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                     Tcl_Obj **potab, Tcl_Obj **porow, Tcl_Obj **aindex,
                     Tcl_Obj **bindex, Tcl_Obj **pomap)
{
    int argoff;

    if (objc < 4 || objc > 7) {
        if (ip)
            Tcl_WrongNumArgs(ip, 1, objv, "?-columns COLUMNMAP? TABLE ROW (INDEXLIST | LOW ?HIGH?)");
        return TCL_ERROR;
    }

    argoff = 0;
    *pomap = NULL;
    /* If 4 or 5 args, no options are present */
    if (objc > 5) {
        /* To prevent shimmering, don't check for options if a tarray */
        if (strcmp(Tcl_GetString(objv[1]), "-columns"))
            return ta_invalid_opt_error(ip, Tcl_GetString(objv[1]));
        argoff = 2;
        *pomap = objv[2];
    }

    *potab = objv[++argoff];
    *porow = objv[++argoff];
    *aindex = objv[++argoff];
    *bindex = ++argoff < objc ? objv[argoff] : NULL;

    return TCL_OK;
}

TCL_RESULT
table_fill_cmd(ClientData clientdata, Tcl_Interp *ip,
                          int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table, *orow, *aindex, *bindex, *omap;
    int status;

    if (table_fill_parseargs(ip, objc, objv, &table, &orow, &aindex, &bindex, &omap) != TCL_OK)
        return TCL_ERROR;

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    status = table_fill_obj(ip, table, orow, aindex, bindex, omap, 0);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT
table_vfill_cmd(ClientData clientdata, Tcl_Interp *ip,
                           int objc, Tcl_Obj *const objv[])
{
    int status;
    Tcl_Obj *ovar, *table, *orow, *aindex, *bindex, *omap;

    if (table_fill_parseargs(ip, objc, objv, &ovar, &orow, &aindex, &bindex, &omap) != TCL_OK)
        return TCL_ERROR;

    table = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    status = table_fill_obj(ip, table, orow, aindex, bindex, omap, 0);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, ovar, table);
}

TCL_RESULT
table_delete_cmd(ClientData clientdata, Tcl_Interp *ip,
                 int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    int status;

    if (objc != 3 && objc != 4) {
	Tcl_WrongNumArgs(ip, 1, objv, "TABLE (INDEXLIST | LOW ?HIGH?)");
	return TCL_ERROR;
    }

    table = objv[1];
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    status = table_delete(ip, table, objv[2], objc == 4 ? objv[3] : NULL);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT table_vdelete_cmd(ClientData clientdata, Tcl_Interp *ip,
                             int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    int status;

    if (objc != 3 && objc != 4) {
	Tcl_WrongNumArgs(ip, 1, objv, "TABLEVAR (INDEXLIST | LOW ?HIGH?)");
	return TCL_ERROR;
    }

    table = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    status = table_delete(ip, table, objv[2], objc == 4 ? objv[3] : NULL);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, objv[1], table);
}

TCL_RESULT
table_get_cmd(ClientData cdata, Tcl_Interp *ip,
              int objc, Tcl_Obj *const objv[])
{
    return table_retrieve(ip, objc, objv, (int)cdata);
}

TCL_RESULT
table_index_cmd(ClientData cdata, Tcl_Interp *ip,
                int objc, Tcl_Obj *const objv[])
{
    int ix;
    Tcl_Obj *o;
    Tcl_Obj *grid;

    if (objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLE INDEX");
        return TCL_ERROR;
    }
    grid = objv[1];
    if (table_convert(ip, grid) == TCL_OK) {
        int end = table_length(grid) - 1;
	if (ta_convert_index(ip, objv[2], &ix, end, 0, end) == TCL_OK) {
            o = table_index(ip, grid, ix);
            if (o) {
                Tcl_SetObjResult(ip, o);
                return TCL_OK;
            }
        }
    }
    return TCL_ERROR;
}

static TCL_RESULT
table_insert_parseargs(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                       Tcl_Obj **potab, Tcl_Obj **povalues, Tcl_Obj **pofirst,
                       int *pcount, Tcl_Obj **pomap)
{
    int argoff;

    if (objc < 4 || objc > 7) {
        if (ip)
            Tcl_WrongNumArgs(ip, 1, objv, "?-columns COLUMNMAP? TABLE ROW LOW ?COUNT?");
        return TCL_ERROR;
    }

    argoff = 0;
    *pomap = NULL;
    /* If 4 or 5 args, no options are present */
    if (objc > 5) {
        if (strcmp(Tcl_GetString(objv[1]), "-columns"))
            return ta_invalid_opt_error(ip, Tcl_GetString(objv[1]));
        argoff = 2;
        *pomap = objv[2];
    }

    *potab = objv[++argoff];
    *povalues = objv[++argoff];
    *pofirst = objv[++argoff];
    if (++argoff < objc) {
        if (ta_get_int_from_obj(ip, objv[argoff], pcount) != TCL_OK)
            return TCL_ERROR;
    } else
        *pcount = 1;

    return TCL_OK;
}

TCL_RESULT
table_insert_cmd(ClientData cdata, Tcl_Interp *ip,
    int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table, *ovalues, *ofirst, *omap;
    int status, count;

    if (table_insert_parseargs(ip, objc, objv, &table, &ovalues,
                               &ofirst, &count, &omap) != TCL_OK)
        return TCL_ERROR;

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    status = table_insert_row(ip, table, ovalues, ofirst, count, omap);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT
table_vinsert_cmd(ClientData cdata, Tcl_Interp *ip,
                  int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *ovar, *table, *ovalues, *ofirst, *omap;
    int status, count;

    if (table_insert_parseargs(ip, objc, objv, &ovar, &ovalues,
                               &ofirst, &count, &omap) != TCL_OK)
        return TCL_ERROR;

    table = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    status = table_insert_row(ip, table, ovalues, ofirst, count, omap);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, ovar, table);
}

static TCL_RESULT
table_inject_parseargs(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                       Tcl_Obj **potab, Tcl_Obj **povalues,
                       Tcl_Obj **pofirst, Tcl_Obj **pomap)
{
    int argoff;

    if (objc != 4 && objc != 6) {
        if (ip)
            Tcl_WrongNumArgs(ip, 1, objv, "?-columns COLUMNMAP? TABLE ROWS POS");
        return TCL_ERROR;
    }

    argoff = 0;
    *pomap = NULL;
    if (objc == 6) {
        if (strcmp(Tcl_GetString(objv[1]), "-columns"))
            return ta_invalid_opt_error(ip, Tcl_GetString(objv[1]));
        argoff = 2;
        *pomap = objv[2];
    }

    *potab = objv[++argoff];
    *povalues = objv[++argoff];
    *pofirst = objv[++argoff];
    return TCL_OK;
}

TCL_RESULT
table_inject_cmd(ClientData cdata, Tcl_Interp *ip,
                 int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table, *ovalues, *ofirst, *omap;
    int status;

    if (table_inject_parseargs(ip, objc, objv, &table, &ovalues,
                               &ofirst, &omap) != TCL_OK)
        return TCL_ERROR;

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    status = table_inject_rows(ip, table, ovalues, ofirst, omap);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT
table_vinject_cmd (ClientData cdata, Tcl_Interp *ip,
                   int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *ovar, *table, *ovalues, *ofirst, *omap;
    int status;

    if (table_inject_parseargs(ip, objc, objv, &ovar, &ovalues,
                               &ofirst, &omap) != TCL_OK)
        return TCL_ERROR;

    table = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);
    status = table_inject_rows(ip, table, ovalues, ofirst, omap);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, ovar, table);
}

static TCL_RESULT
table_place_parseargs(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                      Tcl_Obj **potab, Tcl_Obj **povalues,
                      Tcl_Obj **poindices, Tcl_Obj **pomap)
{
    int argpos;
    char *s;

    if (objc == 4) {
        argpos = 0;
        *pomap = NULL;
    } else {
        if (objc != 6) {
            if (ip)
                Tcl_WrongNumArgs(ip, 1, objv, "?-columns COLUMNMAP? TABLE VALUES INDICES");
            return TCL_ERROR;
        }
        s = Tcl_GetString(objv[1]);
        if (strcmp(s, "-columns"))
            return ta_invalid_opt_error(ip, s);
        argpos = 2;
        *pomap = objv[2];
    }

    *potab = objv[1+argpos];
    *povalues = objv[2+argpos];
    *poindices = objv[3+argpos];

    return TCL_OK;
}

TCL_RESULT
table_place_cmd(ClientData cdata, Tcl_Interp *ip,
                int objc, Tcl_Obj *const objv[])
{
    int status;
    Tcl_Obj *otab, *ovalues, *oindices, *omap;

    if (table_place_parseargs(ip, objc, objv, &otab, &ovalues,
                              &oindices, &omap) != TCL_OK)
        return TCL_ERROR;

    if (Tcl_IsShared(otab))
        otab = Tcl_DuplicateObj(otab);

    status = table_place(ip, otab, ovalues, oindices, omap);
    TA_ASSERT(status != TCL_OK || table_check(ip, otab));
    return ta_return_result(ip, status, otab);
}

TCL_RESULT
table_vplace_cmd(ClientData cdata, Tcl_Interp *ip,
                int objc, Tcl_Obj *const objv[])
{
    int status;
    Tcl_Obj *ovar, *ovalues, *oindices, *omap, *otab;

    if (table_place_parseargs(ip, objc, objv, &ovar, &ovalues,
                              &oindices, &omap) != TCL_OK)
        return TCL_ERROR;

    otab = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (otab == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(otab))
        otab = Tcl_DuplicateObj(otab);

    status = table_place(ip, otab, ovalues, oindices, omap);
    TA_ASSERT(status != TCL_OK || table_check(ip, otab));
    return ta_set_var_result(ip, status, ovar, otab);
}

TCL_RESULT
table_reverse_cmd(ClientData clientdata, Tcl_Interp *ip,
                  int objc, Tcl_Obj *const objv[])
{
    TCL_RESULT status;
    Tcl_Obj *table;
    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLE");
        return TCL_ERROR;
    }
    table = objv[1];
    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    status = table_reverse(ip, table);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT
table_vreverse_cmd(ClientData clientdata, Tcl_Interp *ip,
                   int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    Tcl_Obj *ovar;
    TCL_RESULT status;

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLEVAR");
        return TCL_ERROR;
    }

    ovar = objv[1];
    table = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    status = table_reverse(ip, table);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, ovar, table);
}

TCL_RESULT
table_size_cmd(ClientData cdata, Tcl_Interp *ip,
               int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    Tcl_WideInt size;

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLE");
        return TCL_ERROR;
    }
    table = objv[1];
    if (table_convert(ip, table) != TCL_OK)
        return TCL_ERROR;
    size = (int)cdata ? table_width(table) : table_length(table);
    Tcl_SetObjResult(ip, Tcl_NewWideIntObj(size));
    return TCL_OK;
}

TCL_RESULT
table_column_cmd(ClientData cdata, Tcl_Interp *ip,
                 int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    TCL_RESULT status;

    if (objc != 3 && objc != 4) {
	Tcl_WrongNumArgs(ip, 1, objv, "TABLEVAR COLSPEC ?NEWCOLUMN?");
	return TCL_ERROR;
    }

    table = objv[1];
    if (objc == 3)
        return table_get_column(ip, table, objv[2]);

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    status = table_set_column(ip, table, objv[2], objv[3]);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_return_result(ip, status, table);
}

TCL_RESULT
table_vcolumn_cmd(ClientData cdata, Tcl_Interp *ip,
                  int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    TCL_RESULT status;

    if (objc != 3 && objc != 4) {
	Tcl_WrongNumArgs(ip, 1, objv, "TABLEVAR COLSPEC ?NEWCOLUMN?");
	return TCL_ERROR;
    }

    table = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
    if (table == NULL)
        return TCL_ERROR;

    if (objc == 3)
        return table_get_column(ip, table, objv[2]);

    if (Tcl_IsShared(table))
        table = Tcl_DuplicateObj(table);

    status = table_set_column(ip, table, objv[2], objv[3]);
    TA_ASSERT(status != TCL_OK || table_check(ip, table));
    return ta_set_var_result(ip, status, objv[1], table);
}

TCL_RESULT
table__columns_cmd(ClientData cdata, Tcl_Interp *ip,
                   int objc, Tcl_Obj *const objv[])
{
    thdr_t *thdr;
    Tcl_Obj **tcols;
    Tcl_Obj *table;

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLE");
        return TCL_ERROR;
    }
    table = objv[1];
    if (table_convert(ip, table) != TCL_OK)
        return TCL_ERROR;

    thdr = OBJTHDR(table);
    tcols = THDRELEMPTR(thdr, Tcl_Obj*, 0);
    Tcl_SetObjResult(ip, Tcl_NewListObj(thdr->used, tcols));
    return TCL_OK;
}

TCL_RESULT
table_cnames_cmd(ClientData cdata, Tcl_Interp *ip,
    int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLE");
        return TCL_ERROR;
    }
    table = objv[1];
    if (table_convert(ip, table) != TCL_OK)
        return TCL_ERROR;
    Tcl_SetObjResult(ip, table_column_names(table));
    return TCL_OK;
}

TCL_RESULT
table_slice_cmd(ClientData cdata, Tcl_Interp *ip,
                int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *table;
    Tcl_Obj *collist;
    Tcl_Obj *ocolnames, *ocolname, **pdstcols;
    int      count, status, srcindex;
    thdr_t  *thdr;
    int i;
    Tcl_Obj *oindex;

    if (objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "TABLE COLUMNLIST");
        return TCL_ERROR;
    }
    table = objv[1];
    collist = objv[2];

    /* Have to protect against shimmering between table and the collist
       (this would be a caller bug but can cause a crash). In any case,
       since there is no reasonable call in practice where the table and 
       column list are the same, we simply don't allow the two to be
       the same rather than play games with duping objects */
    if (table == collist) {
        Tcl_SetResult(ip, "Invalid column name list", TCL_STATIC);
        return TCL_ERROR;
    }

    if (table_convert(ip, table) != TCL_OK)
        return TCL_ERROR;

    if ((status = Tcl_ListObjLength(ip, collist, &count)) != TCL_OK)
        return status;
    thdr = thdr_alloc(ip, TA_ANY, count);
    if (thdr == NULL)
        return TCL_ERROR;
    /* TBD - maybe we can use column_map_get_columns here ? */
    pdstcols = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    ocolnames = Tcl_NewListObj(0, NULL);
    for (i = 0; i < count; ++i) {
        TA_NOFAIL(Tcl_ListObjIndex(ip, collist, i, &oindex), TCL_OK);
        TA_ASSERT(oindex);
        if (table_parse_column_index(ip, table, oindex, &srcindex) != TCL_OK)
            break;
        if (table_column_index_to_name(ip, table, srcindex, &ocolname) != TCL_OK)
            break;
        /* Everything seems in order. Store name->index mapping */
        Tcl_ListObjAppendElement(NULL, ocolnames, ocolname);
        Tcl_ListObjAppendElement(NULL, ocolnames, Tcl_NewIntObj(i));
        /* Store the column in the output */
        pdstcols[i] = table_column(table, srcindex);
        Tcl_IncrRefCount(pdstcols[i]);
        thdr->used += 1;
    }
    if (i < count)
            status = TCL_ERROR; /* Early termination => error */
    else {
        /*
         * So far so good but tThere is one last thing to be checked - no
         * duplicate names. We do this by checking size of the column
         * names dictionary.
         */
        TA_NOFAIL(Tcl_DictObjSize(ip, ocolnames, &i), TCL_OK);
        status = i == count ? TCL_OK : ta_duplicate_columns_error(ip, collist);
    }

    if (status == TCL_ERROR) {
        thdr_decr_refs(thdr);
        Tcl_DecrRefCount(ocolnames);
    } else {
        Tcl_Obj *otab = table_new(thdr, ocolnames);
        TA_ASSERT(table_check(ip, otab));
        Tcl_SetObjResult(ip, otab);
    }
    return status;
}
