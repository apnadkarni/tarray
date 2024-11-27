#ifndef TA_H
#define TA_H

#ifdef _WIN32
#include <windows.h>            /* TBD - define LEAN_AND_MEAN ? */
#else
#define _stricmp strcasecmp
#endif

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <time.h>
#include <ctype.h>

#include <stdint.h>

#include "tcl.h"

#if TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION < 7
# undef Tcl_Size
# ifndef Tcl_Size
  typedef int Tcl_Size;
# endif
# define Tcl_GetSizeIntFromObj Tcl_GetIntFromObj
# define TCL_SIZE_MAX      INT_MAX
# define TCL_SIZE_MODIFIER ""
#endif
#define TCL_SIZE_MIN (-TCL_SIZE_MAX - 1)

#ifdef HAVE_LIBDISPATCH
# include <dispatch/dispatch.h>
#elif !defined(_WIN32)
# ifdef TA_MT_ENABLE
#  error Threadpool enabled but libdispatch not available.
# endif
#endif

#if defined(_WIN32)
/* Use compiler version since it protects against A being a pointer */
# ifndef ARRAYSIZE
#  define ARRAYSIZE(A) RTL_NUMBER_OF_V2(A)
# endif
#else
# define ARRAYSIZE(A) (sizeof(A)/sizeof(A[0]))
#endif

#ifndef TA_INLINE
# ifdef _MSC_VER
#  define TA_INLINE __inline  /* Because VC++ 6 only accepts "inline" in C++  */
# else
#  define TA_INLINE static inline
# endif
#endif

#if TA_ENABLE_ASSERT
#  if TA_ENABLE_ASSERT == 1
#    define TA_ASSERT(bool_) (void)( (bool_) || (Tcl_Panic("Assertion (%s) failed at line %d in file %s.", #bool_, __LINE__, __FILE__), 0) )
#  elif TA_ENABLE_ASSERT == 2
#    define TA_ASSERT(bool_) (void)( (bool_) || (DebugOutput("Assertion (" #bool_ ") failed at line " MAKESTRINGLITERAL2(__LINE__) " in file " __FILE__ "\n"), 0) )
#  elif TA_ENABLE_ASSERT == 3
#    define TA_ASSERT(bool_) do { if (! (bool_)) { __asm int 3 } } while (0)
#  else
#    error Invalid value for TA_ENABLE_ASSERT
#  endif
#else
#define TA_ASSERT(bool_) ((void) 0)
#endif

#if TA_ENABLE_ASSERT
# define TA_NOFAIL(expr, val) TA_ASSERT((expr) == (val))
#else
# define TA_NOFAIL(expr, val) do { (void) (expr) ; } while (0)
#endif


#define BA_ASSERT TA_ASSERT
#if TA_ENABLE_ASSERT
#define BA_ENABLE_ASSERT 1
#endif
#include "bitarray.h"           /* Include AFTER tcl.h and assert definitions */
#include "timsort.h"
#include "tamath.h"

#include "pcg_basic.h"

typedef int TCL_RESULT;
#define CHECK_OK(x) do { TCL_RESULT res = x; if (res != TCL_OK) return res; } while (0)

/* Various portability defines */
#ifdef _MSC_VER
# define snprintf _snprintf
#endif

/* TA_ALLOCMEM and TA_REALLOCMEM call should panic on failure to allocate */
/* TBD - change to use inline functions */
#define TA_ALLOCMEM(n_) (void*)ckalloc(n_)
#define TA_FREEMEM(p_) if (p_) ckfree((char *)p_)
#define TA_REALLOCMEM ckrealloc
#define TA_ATTEMPTALLOCMEM(n_) (void*)attemptckalloc(n_)
#define TA_ATTEMPTREALLOCMEM attemptckrealloc

/* Space required for a numeric string */
#define TA_NUMERIC_SPACE 40

/* Must match g_type_tokens definition in tarray.c ! */
#define TA_NONE 0
#define TA_UINT 1
#define TA_INT 2
#define TA_WIDE 3
#define TA_DOUBLE 4
#define TA_BYTE 5
#define TA_ANY 6
#define TA_STRING 7
#define TA_BOOLEAN 8
#define TA_INDEX (sizeof(Tcl_Size) == sizeof(int) ? TA_INT : TA_WIDE)

/* Type of tarray */
enum ta_collection_type_e {
    TA_COLL_NONE,               /* Not a collection */
    TA_COLL_LIST,               /* Tcl list */
    TA_COLL_COLUMN,             /* tarray column */
    TA_COLL_TABLE,              /* tarray table */
};

typedef struct tas_t {
    unsigned char nrefs;
#define TAS_MAX_NREFS 255
    char          s[1];         /* Actually variable size, null terminated */
} tas_t;

typedef struct ta_value_s {
    unsigned char type;
    union {
        int           bval : 1;
        unsigned int  uival;
        int           ival;
        Tcl_WideInt   wval;
        double        dval;
        unsigned char ucval;
        Tcl_Obj *     oval;
        tas_t *       ptas;

        /*
         * 64-bit unsinged is not a valid tarray type.
         * It is however used in some intermediate calcuations.
         * The type field above will be invalid in such cases.
         */
        uint64_t      uwval;
    };
} ta_value_t;

/* Lookup table for string thdrs */
typedef Tcl_HashTable *tas_lookup_t;
#define TAS_LOOKUP_INVALID_HANDLE NULL

extern const char *g_type_tokens[];

#define TA_FORMAT_TARRAY 0
#define TA_FORMAT_LIST 1
#define TA_FORMAT_DICT 2

/* MUST match order of switches[] in tcol_parse_sort_options */
#define TA_SORT_DECREASING 1
#define TA_SORT_INDICES    2
#define TA_SORT_NOCASE     4
#define TA_SORT_INDIRECT   8

/* Pointers to Tcl's built-in type descriptors */
extern const Tcl_ObjType *g_tcl_int_type_ptr;
extern const Tcl_ObjType *g_tcl_double_type_ptr;
extern const Tcl_ObjType *g_tcl_wide_type_ptr;
extern const Tcl_ObjType *g_tcl_dict_type_ptr;
extern const Tcl_ObjType *g_tcl_list_type_ptr;
extern const Tcl_ObjType *g_tcl_string_type_ptr;

/* How many slots to allocate by default */
#define TA_DEFAULT_NSLOTS 1000

#define TA_MAX_ELEM_SIZE (sizeof(double) > sizeof(int) ? (sizeof(double) > sizeof(void*) ? sizeof(double) : sizeof(void*)) : sizeof(int))
/* Max usable count - not including space for sentinel */
#define TA_MAX_COUNT (int)(((size_t)UINT_MAX - sizeof(thdr_t))/TA_MAX_ELEM_SIZE)

/*
 * A span_t is used to store a span or subrange limits 
 */
typedef struct span_s {
    Tcl_Size nrefs;
    Tcl_Size first;
    Tcl_Size count;
} span_t;

/*
 * A thdr_t stores an array of elements of a specific type. It is used
 * as the internal representation of TArray column as well as a TArray
 * table.
 */
typedef union thdr_s {
    void *pointer_aligner;
    double double_aligner;
    struct {
        tas_lookup_t lookup;  /* Index. Currently only used with type string */
        Tcl_Size nrefs;              /* Ref count when shared between Tcl_Objs */
        Tcl_Size usable;             /* Number of slots that can be used. This
                                   is one less than number allocated as
                                   we keep one as a sentinel.
                                */
        Tcl_Size used;
        unsigned char type;
        unsigned char elem_bits; /* Size of element in bits */
        unsigned char sort_order;
#define THDR_UNSORTED                 0
#define THDR_SORTED_ASCENDING         1
#define THDR_SORTED_DESCENDING        2
#define THDR_SORTED_ASCENDING_NOCASE  3
#define THDR_SORTED_DESCENDING_NOCASE 4
#define SORT_ORDER_IS_NOCASE(order_) ((order_) == THDR_SORTED_ASCENDING_NOCASE || (order_) == THDR_SORTED_DESCENDING_NOCASE)
#define SORT_ORDER_IS_CASE(order_) ((order_) == THDR_SORTED_ASCENDING || (order_) == THDR_SORTED_DESCENDING)
#define SORT_ORDER_IS_ASCENDING(order_) ((order_) == THDR_SORTED_ASCENDING || (order_) == THDR_SORTED_ASCENDING_NOCASE)
#define SORT_ORDER_IS_DESCENDING(order_) ((order_) == THDR_SORTED_DESCENDING || (order_) == THDR_SORTED_DESCENDING_NOCASE)
    };
} thdr_t;

/* How much extra slots to allocate when allocating memory. n_ should
 * be number of elements currently. TBD - guard against overflow
 */
#define TA_EXTRA(n_)  \
    ((n_) < 10 ? 10 : ((n_) < 100 ? (n_) : ((n_) < 800 ? 100 : ((n_)/8))))

void thdr_free(thdr_t *thdr);
thdr_t *thdr_realloc(Tcl_Interp *, thdr_t *oldP, Tcl_Size new_count);
thdr_t *thdr_alloc(Tcl_Interp *, int tatype, Tcl_Size count);
thdr_t *thdr_alloc_and_init(Tcl_Interp *, int tatype, Tcl_Size nelems,
                            struct Tcl_Obj *const *elems , Tcl_Size init_size);
thdr_t *thdr_alloc_bitmap(Tcl_Interp *ip, Tcl_Size count);
thdr_t *thdr_indices_to_bitmap(Tcl_Interp *,Tcl_Size size,thdr_t *src,span_t *span);
TCL_RESULT tcol_grow_intrep(Tcl_Interp *ip, Tcl_Obj *o, Tcl_Size new_size);

#define THDRELEMPTR(thdr_, type_, index_) ((index_) + (type_ *)(sizeof(thdr_t) + (char *) (thdr_)))
/* thdr_t holding indices are Tcl_Size */
#define THDRINDEXELEMPTR(thdr_, index_) THDRELEMPTR(thdr_, Tcl_Size, index_)

/*
 * Random number generation using PCG
 */
typedef struct ta_rng_s {
    pcg32_random_t rng[2];
    Tcl_Size nrefs;
} ta_rng_t;

typedef struct ta_rng_instance_s {
    Tcl_Command cmd_token;
    ta_rng_t rng;
    ta_value_t lbound;
    ta_value_t ubound;
    unsigned char bounded;
    unsigned char rtype;
} ta_rng_instance_t;

typedef Tcl_WideInt ta_cmd_counter;

/*
 * Inline functions to manipulate internal representation of Tarray columns
 */

/* 
 * Retrieve a lvalue reference to the field used to point to the thdr_t 
 * This applies to columns and tables.
 */
#define OBJTHDR(optr_) (*(thdr_t **) (&((optr_)->internalRep.twoPtrValue.ptr1)))

/* 
 * Retrieve a lvalue reference to the field used to point to the column 
 * span (range). Only applies to columns. If this field is NULL, the entire
 * thdr_t is the span.
 */
#define OBJTHDRSPAN(optr_) (*(span_t **) (&((optr_)->internalRep.twoPtrValue.ptr2)))

/*
 * Retrieve the indx of the first element of a column.
 */
#define OBJTHDRFIRST(optr_) (OBJTHDRSPAN(optr_) ? (OBJTHDRSPAN(optr_))->first : 0)

/*
 * Retrieve a lvalue reference to the field used to point to the column 
 * names for a table. Only applies to tables.
 */
#define OBJCOLNAMES(optr_) (*(Tcl_Obj **) (&((optr_)->internalRep.twoPtrValue.ptr2)))

extern struct Tcl_ObjType ta_column_type;
extern struct Tcl_ObjType ta_table_type;
extern struct Tcl_ObjType ta_index_type;


TCL_RESULT ta_return_result(Tcl_Interp *ip, TCL_RESULT status, Tcl_Obj *ores);
TCL_RESULT ta_set_var_result(Tcl_Interp *ip, TCL_RESULT status, Tcl_Obj *ovarname, Tcl_Obj *ovalue);

const char *ta_type_string(int tatype);
void ta_update_string_for_variable_element_size(Tcl_Obj *o);
enum ta_collection_type_e ta_detect_collection_type(Tcl_Obj *o);

int ta_utf8_compare(char *, char *, int ignorecase);
int ta_utf8_equal(char *, char *, int ignorecase);
int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase);
int ta_obj_equal(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase);
int ta_opt_from_obj(Tcl_Interp *ip, Tcl_Obj *o, const char *const *ptable,
                    const char *msg, int flags, int *popt);

/*
 * Error and panic routines
 */
void ta_string_overflow_panic(const char *where);
void ta_type_panic(int tatype);
void ta_operator_panic(int oper);
void ta_shared_panic(const char *where);
void ta_small_panic(thdr_t *thdr, const char *where);
void ta_memory_panic(Tcl_Size);
TCL_RESULT ta_conflicting_options_error(Tcl_Interp *ip, const char *optA, const char *optB);
TCL_RESULT ta_not_column_error(Tcl_Interp *);
TCL_RESULT ta_not_table_error(Tcl_Interp *);
TCL_RESULT ta_search_op_error(Tcl_Interp *, int op);
TCL_RESULT ta_value_type_error(Tcl_Interp *, Tcl_Obj *o, int tatype);
TCL_RESULT ta_table_length_error(Tcl_Interp *);
TCL_RESULT ta_row_width_error(Tcl_Interp *, Tcl_Size rowwidth, Tcl_Size tablewidth);
TCL_RESULT ta_bad_type_error(Tcl_Interp *ip, thdr_t *thdr);
TCL_RESULT ta_memory_error(Tcl_Interp *, Tcl_Size size);
TCL_RESULT ta_limit_error(Tcl_Interp *, Tcl_WideInt);
TCL_RESULT ta_indices_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_index_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_index_range_error(Tcl_Interp *ip, Tcl_Size index);
TCL_RESULT ta_invalid_range_error(Tcl_Interp *ip, Tcl_Obj *);
TCL_RESULT ta_bad_count_error(Tcl_Interp *ip, Tcl_Size count);
TCL_RESULT ta_mismatched_types_error(Tcl_Interp *ip, int typea, int typeb);
TCL_RESULT ta_invalid_op_for_type(Tcl_Interp *ip, int typea);
TCL_RESULT ta_invalid_op_for_table(Tcl_Interp *ip);
TCL_RESULT ta_indices_count_error(Tcl_Interp *ip, Tcl_Size nindices, Tcl_Size nvalues);
TCL_RESULT ta_missing_arg_error(Tcl_Interp *ip, char *optname);
TCL_RESULT ta_invalid_opt_error(Tcl_Interp *ip, char *optname);
TCL_RESULT ta_column_name_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_column_index_error(Tcl_Interp *ip, Tcl_Size index);
TCL_RESULT ta_duplicate_columns_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_multiple_columns_error(Tcl_Interp *ip, Tcl_Size colindex);
TCL_RESULT ta_column_lengths_error(Tcl_Interp *ip);
TCL_RESULT ta_invalid_operand_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_invalid_argcount(Tcl_Interp *ip);
TCL_RESULT ta_integer_overflow_error(Tcl_Interp *ip, const char *precision, Tcl_WideInt val);
TCL_RESULT ta_integer_overflow_from_double_error(Tcl_Interp *ip, const char *precision, double val);
TCL_RESULT ta_integer_overflow_obj_error(Tcl_Interp *ip, const char *precision, Tcl_Obj *o);
TCL_RESULT ta_negative_count_error(Tcl_Interp *ip, Tcl_Size);
TCL_RESULT ta_invalid_rng_bounds(Tcl_Interp *ip, ta_value_t *, ta_value_t *);
TCL_RESULT ta_invalid_source_column_value(Tcl_Interp *ip, Tcl_Size row, Tcl_Size col, int tatype, Tcl_Obj *val);
TCL_RESULT ta_invalid_source_row_width(Tcl_Interp *ip, Tcl_Size row, Tcl_Size nfields, Tcl_Size ncols);

TCL_RESULT ta_check_column_type(Tcl_Interp *ip, thdr_t *thdr, int wanted_type);

TA_INLINE Tcl_Size ta_strlen(const char *s) {
    return (Tcl_Size)strlen(s);
}

/* tas_t interface */
#define TAS_ALLOCMEM TA_ALLOCMEM
#define TAS_FREEMEM  TA_FREEMEM
tas_t *tas_alloc(Tcl_Size len);
tas_t *tas_alloc_nbytes(char *s, Tcl_Size len);
TA_INLINE tas_t *tas_alloc_string(char *s) {
    return tas_alloc_nbytes(s, (Tcl_Size) strlen(s));
}
TA_INLINE tas_t * tas_from_obj(Tcl_Obj *o) {
    Tcl_Size len;
    char *s = Tcl_GetStringFromObj(o, &len);
    return tas_alloc_nbytes(s, len);
}
TA_INLINE Tcl_Obj *tas_to_obj(tas_t *ptas) {
    return Tcl_NewStringObj(ptas->s, -1);
}

TA_INLINE tas_t *tas_from_double(double val)
{
    tas_t *ptas = tas_alloc(TCL_DOUBLE_SPACE);
    Tcl_PrintDouble(NULL, val, &ptas->s[0]);
    return ptas;
}

TA_INLINE tas_t *tas_from_int(int val)
{
    tas_t *ptas = tas_alloc(12);
    sprintf(&ptas->s[0], "%d", val);
    return ptas;
}

TA_INLINE tas_t *tas_from_uint(unsigned int val)
{
    tas_t *ptas = tas_alloc(12);
    sprintf(&ptas->s[0], "%u", val);
    return ptas;
}

TA_INLINE tas_t *tas_from_wide(Tcl_WideInt val)
{
    tas_t *ptas = tas_alloc(TCL_INTEGER_SPACE);
    sprintf(&ptas->s[0], "%" TCL_LL_MODIFIER "d", val);
    return ptas;
}

TA_INLINE tas_t *tas_dup(tas_t *src) {
    TA_ASSERT(src->nrefs > 0);
    return tas_alloc_string(&src->s[0]);
}
/* Returned tas may not be same as src ! */
TA_INLINE tas_t *tas_ref(tas_t *src) {
    TA_ASSERT(src->nrefs > 0);
    if (src->nrefs < TAS_MAX_NREFS) {
        src->nrefs += 1;
        return src;
    } else
        return tas_dup(src);
}
TA_INLINE void tas_unref(tas_t *ptas)
{
    TA_ASSERT(ptas->nrefs > 0);
    ptas->nrefs -= 1;
    if (ptas->nrefs == 0)
        TAS_FREEMEM(ptas);
}
TA_INLINE int tas_equal(tas_t *a, tas_t *b, int nocase)
{
    TA_ASSERT(a->nrefs > 0 && b->nrefs > 0);
#if 0
    if (a == b)
        return 1;
    if (a->s[0] != b->s[0] && !nocase)
        return 0;
#endif
    /* TBD - should we call ta_utf8_equal instead. Currently that
       just calls str*cmp but that might change in future */
    return nocase ? ! _stricmp(a->s, b->s) : ! strcmp(a->s, b->s);
}
TA_INLINE int tas_compare(tas_t *a, tas_t *b, int nocase)
{
    TA_ASSERT(a->nrefs > 0 && b->nrefs > 0);
    if (a == b)
        return 0;
    return ta_utf8_compare(a->s, b->s, nocase);
}
tas_lookup_t tas_lookup_new(void);
void tas_lookup_free(tas_lookup_t lookup);
int tas_lookup_entry(tas_lookup_t lookup, tas_t *ptas, Tcl_Size *pval);
void tas_lookup_add(tas_lookup_t lookup, tas_t *ptas, Tcl_Size val);
int tas_lookup_delete(tas_lookup_t lookup, tas_t *ptas);

void thdr_lookup_build(thdr_t *thdr, span_t *span);

TCL_RESULT ta_get_int8_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int8_t *pb);
TCL_RESULT ta_get_uint8_from_obj(Tcl_Interp *ip, Tcl_Obj *o, uint8_t *pb);
#define ta_get_byte_from_obj ta_get_uint8_from_obj
TCL_RESULT ta_get_uint_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned int *pui);
TCL_RESULT ta_get_int_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int *pi);
TCL_RESULT ta_get_wide_from_obj(Tcl_Interp *ip, Tcl_Obj *o, Tcl_WideInt *pi);
#ifdef _MSC_VER
#define ta_get_int64_from_obj ta_get_wide_from_obj
#else
/* Gcc complains about pointer mismatch between int64_t (long int)
   and Tcl_WideInt (long long int) so define a wrapper
*/
TA_INLINE TCL_RESULT ta_get_int64_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int64_t *pi64)
{
    TCL_RESULT res;
    Tcl_WideInt wide;
    res = ta_get_wide_from_obj(ip, o, &wide);
    if (res == TCL_OK) {
        *pi64 = (int64_t) wide;
    }
    return res;
}
#endif
TCL_RESULT ta_get_count_from_obj(Tcl_Interp *ip, Tcl_Obj *o,
               int zero_allowed, Tcl_Size *pi);

TCL_RESULT ta_get_double_from_string(Tcl_Interp *ip, const char *s, double *pi);
TCL_RESULT ta_get_boolean_from_string(Tcl_Interp *ip, const char *s, int *pi);
TCL_RESULT ta_get_wide_from_string(Tcl_Interp *ip, const char *s, Tcl_WideInt *pi);
TCL_RESULT ta_get_uint8_from_string(Tcl_Interp *ip, const char *s, unsigned char *pi);
TCL_RESULT ta_get_int_from_string(Tcl_Interp *ip, const char *s, int *pi);
TCL_RESULT ta_get_uint_from_string(Tcl_Interp *ip, const char *s, unsigned int *pi);

void thdr_incr_obj_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count);
void thdr_decr_obj_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count);
void thdr_incr_tas_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count);
void thdr_decr_tas_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count);

TCL_RESULT tcol_convert_from_other(Tcl_Interp *, Tcl_Obj *o);
TCL_RESULT table_convert_from_other(Tcl_Interp *, Tcl_Obj *o);
void thdr_ensure_obj_strings(thdr_t *thdr, span_t *);

TCL_RESULT ta_value_from_obj(Tcl_Interp *, Tcl_Obj *o,
                              unsigned char tatype, ta_value_t *ptav);
Tcl_Obj *ta_value_to_obj(ta_value_t *ptav);
int ta_value_compare(ta_value_t *pa, ta_value_t *pb, int ignore_case);
void ta_value_clear(ta_value_t *);
TA_INLINE void ta_value_init(ta_value_t *ptav) {
    ptav->type = TA_NONE;
}
void ta_value_init_max(unsigned char tatype, ta_value_t *ptav);

void thdr_fill_range(Tcl_Interp *, thdr_t *thdr,
                     const ta_value_t *ptav, Tcl_Size pos, Tcl_Size count, int insert);
void thdr_fill_ta_objs(thdr_t *thdr, thdr_t *pindices, Tcl_Obj *oval, Tcl_Size highest_in_indices);
TCL_RESULT thdr_put_objs(Tcl_Interp *ip, thdr_t *thdr, Tcl_Size first,
                         Tcl_Size nelems, Tcl_Obj * const elems[], int insert);
void thdr_place_ta_objs(thdr_t *thdr, thdr_t *pindices, Tcl_Obj *const *ovalues,
                        Tcl_Size highest_in_indices);
void thdr_fill_indices(Tcl_Interp *, thdr_t *thdr, const ta_value_t *ptav,
                       thdr_t *pindices, Tcl_Size highest_index);
Tcl_Obj *thdr_index(thdr_t *thdr, Tcl_Size index);
void thdr_index_ta_value(thdr_t *thdr, Tcl_Size index, ta_value_t *tavP);
char *thdr_index_string(thdr_t *thdr, Tcl_Size thdr_index, char buf[TA_NUMERIC_SPACE]);
double thdr_index_double(thdr_t *thdr, Tcl_Size thdr_index);
Tcl_WideInt thdr_index_wide(thdr_t *thdr, Tcl_Size thdr_index);

Tcl_Obj * tcol_new(thdr_t *thdr);
TCL_RESULT tcol_make_modifiable(Tcl_Interp *ip, Tcl_Obj *tcol,
                                Tcl_Size minsize, Tcl_Size prefsize);
TCL_RESULT table_make_modifiable(Tcl_Interp *ip, Tcl_Obj *table,
                                 Tcl_Size minsize, Tcl_Size prefsize);

TCL_RESULT tcols_put_objs(Tcl_Interp *ip, Tcl_Size ntcols,
                          Tcl_Obj *const *tcols, Tcl_Size nrows,
                          Tcl_Obj *const *rows, Tcl_Size first, int insert);

void thdr_place_objs(Tcl_Interp *, thdr_t *thdr, thdr_t *pindices,
                     Tcl_Size new_size,
                     Tcl_Size nvalues, Tcl_Obj * const *pvalues);
void thdr_place_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *psrc,
                        span_t *src_span, thdr_t *pindices, Tcl_Size new_size);

Tcl_Size thdr_required_size(int tatype, Tcl_Size count);
void thdr_reverse(thdr_t *tdrhP);
void thdr_copy_reversed(thdr_t *pdst, Tcl_Size dst_first, thdr_t *psrc,
                        Tcl_Size src_first, Tcl_Size count);
void thdr_copy(thdr_t *pdst, Tcl_Size dst_first, thdr_t *psrc,
               Tcl_Size src_first, Tcl_Size count, int insert);
TCL_RESULT thdr_copy_cast(Tcl_Interp *ip, thdr_t *pdst, Tcl_Size dst_first,
                          thdr_t *psrc, Tcl_Size src_first, Tcl_Size count,
                          int insert, int strict);
thdr_t *thdr_to_indexcolumn(Tcl_Interp *ip, thdr_t *thdr, span_t *span);
void thdr_delete_range(thdr_t *thdr, Tcl_Size first, Tcl_Size count);
void thdr_delete_indices(thdr_t *thdr, thdr_t *pindices);
thdr_t *thdr_clone(Tcl_Interp *, thdr_t *psrc, Tcl_Size init_size, span_t *);
thdr_t *thdr_clone_reversed(Tcl_Interp *, thdr_t *psrc, Tcl_Size init_size, span_t *);
TCL_RESULT ta_verify_value_objs(Tcl_Interp *intepr, int tatype,
                             Tcl_Size nelems, Tcl_Obj * const elems[]);
TCL_RESULT thdr_verify_indices_in_range(Tcl_Interp *ip, Tcl_Size current_size,
                                        thdr_t *pindices, Tcl_Size *new_sizeP);
int ta_obj_to_indices(struct Tcl_Interp *, struct Tcl_Obj *o, int want_sorted,
                      Tcl_Size end, thdr_t **thdrP, Tcl_Size *pindex);
#define TA_INDEX_TYPE_ERROR 0
#define TA_INDEX_TYPE_INT   1
#define TA_INDEX_TYPE_THDR 2

int thdr_check(Tcl_Interp *, thdr_t *);

int tcol_check(Tcl_Interp *, Tcl_Obj *);
TCL_RESULT tcol_retrieve(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                         int command);
Tcl_Obj *tcol_range(Tcl_Interp *ip, Tcl_Obj *srcObj, Tcl_Size low, Tcl_Size count,
                     int fmt);
TCL_RESULT tcol_trim_end(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Size low, Tcl_Size count);
TCL_RESULT tcol_delete(Tcl_Interp *ip, Tcl_Obj *tcol,
                        Tcl_Obj *indexA, Tcl_Obj *indexB);
TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                         Tcl_Obj *indexA, Tcl_Obj *indexB);
TCL_RESULT tcol_insert_elem(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Size count);
TCL_RESULT tcol_inject_elems(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                             Tcl_Obj *opos);
TCL_RESULT tcol_reverse(Tcl_Interp *ip, Tcl_Obj *tcol);

TCL_RESULT tcols_fill_range(Tcl_Interp *ip, Tcl_Size ntcols, Tcl_Obj **tcols,
                            Tcl_Obj *orow, Tcl_Size pos, Tcl_Size count, int insert);
TCL_RESULT tcols_fill_indices(Tcl_Interp *ip, Tcl_Size ntcols,
                              Tcl_Obj **tcols, Tcl_Obj *orow, thdr_t *pindices,
                              Tcl_Size highest_index);
int tcol_equality_test(Tcl_Interp *, Tcl_Obj *cola, Tcl_Obj *colb, int strict);
    
void tcol_random_init(ta_rng_t *prng);
TCL_RESULT tcol_random_cmd(ClientData,Tcl_Interp *,int,Tcl_Obj *const objv[]);
TCL_RESULT ta_randseed_cmd(ClientData,Tcl_Interp *,int,Tcl_Obj *const objv[]);
TCL_RESULT tcol_shuffle_cmd(ClientData,Tcl_Interp *,int,Tcl_Obj *const objv[]);
TCL_RESULT tcol_vshuffle_cmd(ClientData,Tcl_Interp *,int,Tcl_Obj *const objv[]);
void ta_random_rng_delete(ClientData cdata);
TCL_RESULT ta_rng_fixup_bounds(Tcl_Interp *, ta_value_t *, ta_value_t *, int);

int table_check(Tcl_Interp *, Tcl_Obj *);
Tcl_Obj *table_new(thdr_t *thdr, Tcl_Obj *ocolumns);
Tcl_Obj *table_column_names (Tcl_Obj *otab);
TCL_RESULT table_parse_column_index(Tcl_Interp *ip,
                                    Tcl_Obj *table, Tcl_Obj *oindex,
                                    Tcl_Size *pindex);
TCL_RESULT table_column_index_to_name(Tcl_Interp *ip, Tcl_Obj *otab,
                                      Tcl_Size colindex, Tcl_Obj **pname);

TCL_RESULT table_fill_obj(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *orow, Tcl_Obj *indexa, Tcl_Obj *indexb, Tcl_Obj *omap, int insert);
TCL_RESULT table_put_objs(Tcl_Interp *ip, Tcl_Obj *table,
                          Tcl_Obj *orows, Tcl_Obj *ofirst,
                          Tcl_Obj *omap, int insert);
TCL_RESULT table_copy(Tcl_Interp *ip, Tcl_Obj *dstable, Tcl_Obj *srctable, Tcl_Obj *ofirst, Tcl_Obj *omap, int insert);
TCL_RESULT table_delete(Tcl_Interp *ip, Tcl_Obj *table,
                        Tcl_Obj *indexa, Tcl_Obj *indexb);
TCL_RESULT table_retrieve(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                          int command);
#define TA_RETRIEVE_GET 0
#define TA_RETRIEVE_RANGE 1
TCL_RESULT table_reverse(Tcl_Interp *interp, Tcl_Obj *table);
Tcl_Obj *table_index(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Size index);
TCL_RESULT table_place_objs(Tcl_Interp *ip, Tcl_Obj *table,
                            Tcl_Obj *orows, Tcl_Obj *oindices);
TCL_RESULT table_place_indices(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *psrc,
                               Tcl_Obj *oindices, Tcl_Obj *omap);
TCL_RESULT table_place(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *psrc,
                               Tcl_Obj *oindices, Tcl_Obj *omap);
TCL_RESULT table_insert_row(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Size count, Tcl_Obj *omap);
TCL_RESULT table_inject_rows(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Obj *omap);
TCL_RESULT table_get_column(Tcl_Interp *, Tcl_Obj *table, Tcl_Obj *);
TCL_RESULT table_set_column(Tcl_Interp *, Tcl_Obj *table, Tcl_Obj *, Tcl_Obj *);

Tcl_Obj *tcol_index(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Size index);
Tcl_Obj *tcol_get(struct Tcl_Interp *, Tcl_Obj *osrc, thdr_t *pindices, int fmt);
Tcl_Size TArrayNumSetBits(thdr_t *thdr);
TCL_RESULT tcol_copy_thdr(Tcl_Interp *, Tcl_Obj *tcol, thdr_t *psrc, span_t *, Tcl_Obj *firstObj, int insert);
TCL_RESULT tcol_put_objs(Tcl_Interp *, Tcl_Obj *tcol,
                         Tcl_Obj *valueListObj, Tcl_Obj *firstObj, int insert);
TCL_RESULT tcol_place_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                               Tcl_Obj *valueListObj, Tcl_Obj *indicesObj);
TCL_RESULT tcol_place_indices(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *osrc,
                              Tcl_Obj *oindices);
TCL_RESULT ta_convert_index(Tcl_Interp *, Tcl_Obj *o, Tcl_Size *pindex,
                      Tcl_Size end_value, Tcl_Size low, Tcl_Size high);
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *, Tcl_Size nelems, Tcl_Obj *olow, Tcl_Obj *ohigh, Tcl_Size *plow, Tcl_Size *pcount);
TCL_RESULT ta_parse_range_option_value(Tcl_Interp *ip, Tcl_Size nelems, Tcl_Obj *rangeObj, Tcl_Size *plow, Tcl_Size *pcount);

TCL_RESULT tcols_validate_obj_row_widths(Tcl_Interp *ip, Tcl_Size width,
                                         Tcl_Size nrows, Tcl_Obj * const rows[]);
TCL_RESULT tcols_validate_obj_rows(Tcl_Interp *ip, Tcl_Size ntcols,
                                   Tcl_Obj * const *tcols,
                                   Tcl_Size nrows, Tcl_Obj * const rows[]);

TCL_RESULT tcols_copy(Tcl_Interp *ip, Tcl_Size ntcols,
                      Tcl_Obj * const *dstcols, Tcl_Size dst_elem_first,
                      Tcl_Obj * const *srccols, Tcl_Size src_elem_first,
                      Tcl_Size count, int insert);

void tarray_qsort_r(void *a, size_t n, size_t es, void *thunk, int (*cmp)(void *, const void *, const void *));
int intcmp(const void *a, const void *b);
int intcmprev(const void *a, const void *b);
int uintcmp(const void *a, const void *b);
int uintcmprev(const void *a, const void *b);
int widecmp(const void *a, const void *b);
int widecmprev(const void *a, const void *b);
int tclsizecmp(const void *a, const void *b);
int tclsizecmprev(const void *a, const void *b);
int doublecmp(const void *a, const void *b);
int doublecmprev(const void *a, const void *b);
int bytecmp(const void *a, const void *b);
int bytecmprev(const void *a, const void *b);
int tclobjcmp(const void *a, const void *b);
int tclobjcmprev(const void *a, const void *b);
int tclobjcmpnocase(const void *a, const void *b);
int tclobjcmprevnocase(const void *a, const void *b);

int intcmpindexed(const void *a, const void *b, void *);
int intcmpindexedrev(const void *a, const void *b, void *);
int uintcmpindexed(const void *a, const void *b, void *);
int uintcmpindexedrev(const void *a, const void *b, void *);
int widecmpindexed(const void *a, const void *b, void *);
int widecmpindexedrev(const void *a, const void *b, void *);
int tclsizecmpindexed(const void *a, const void *b, void *);
int tclsizecmpindexedrev(const void *a, const void *b, void *);
int doublecmpindexed(const void *a, const void *b, void *);
int doublecmpindexedrev(const void *a, const void *b, void *);
int bytecmpindexed(const void *a, const void *b, void *);
int bytecmpindexedrev(const void *a, const void *b, void *);
int tclobjcmpindexed(const void *a, const void *b, void *);
int tclobjcmpindexedrev(const void *a, const void *b, void *);
int tclobjcmpnocaseindexed(const void *a, const void *b, void *);
int tclobjcmpnocaseindexedrev(const void *a, const void *b, void *);

TCL_RESULT tcol_parse_sort_options(Tcl_Interp *ip,
                                   int objc, Tcl_Obj *const objv[],
                                   int *pflags, Tcl_Obj **ptarget);
TCL_RESULT tcol_sort(Tcl_Interp *ip, Tcl_Obj *tcol, int flags);


TCL_RESULT tcol_sort_indirect(Tcl_Interp *ip, Tcl_Obj *oindices, Tcl_Obj *otarget, int flags);

Tcl_ObjCmdProc parseargs_cmd;
Tcl_ObjCmdProc ta_loop_cmd;
Tcl_ObjCmdProc ta_loop_nr_cmd;
Tcl_ObjCmdProc ta_rbc_init_stubs_cmd;
Tcl_ObjCmdProc tcol_equalintervals_cmd;
Tcl_ObjCmdProc tcol_create_cmd;
Tcl_ObjCmdProc tcol_delete_cmd;
Tcl_ObjCmdProc tcol_vdelete_cmd;
Tcl_ObjCmdProc tcol_fold_cmd;
Tcl_ObjCmdProc tcol_lookup_cmd;
Tcl_ObjCmdProc tcol_math_cmd;
Tcl_ObjCmdProc tcol_minmax_cmd;
Tcl_ObjCmdProc tcol_search_cmd;
Tcl_ObjCmdProc tcol_series_cmd;
Tcl_ObjCmdProc tcol_sortmerge_helper_cmd;
Tcl_ObjCmdProc tcol_size_cmd;
Tcl_ObjCmdProc tcol_type_cmd;
Tcl_ObjCmdProc tcol_equal_cmd;
Tcl_ObjCmdProc tcol_identical_cmd;
Tcl_ObjCmdProc tcol_cast_cmd;
Tcl_ObjCmdProc tcol_index_cmd;
Tcl_ObjCmdProc tcol_get_cmd;
Tcl_ObjCmdProc tcol_sort_cmd;
Tcl_ObjCmdProc tcol_vsort_cmd;
Tcl_ObjCmdProc tcol_bitsset_cmd;
Tcl_ObjCmdProc tcol_insert_cmd;
Tcl_ObjCmdProc tcol_vinsert_cmd;
Tcl_ObjCmdProc tcol_inject_cmd;
Tcl_ObjCmdProc tcol_vinject_cmd;
Tcl_ObjCmdProc tcol_put_cmd;
Tcl_ObjCmdProc tcol_vplace_cmd;
Tcl_ObjCmdProc tcol_place_cmd;
Tcl_ObjCmdProc tcol_vput_cmd;
Tcl_ObjCmdProc tcol_fill_cmd;
Tcl_ObjCmdProc tcol_vfill_cmd;
Tcl_ObjCmdProc tcol_reverse_cmd;
Tcl_ObjCmdProc tcol_vreverse_cmd;
Tcl_ObjCmdProc tcol_intersect3_cmd;

Tcl_ObjCmdProc table_put_cmd;
Tcl_ObjCmdProc table_vput_cmd;
Tcl_ObjCmdProc table_fill_cmd;
Tcl_ObjCmdProc table_vfill_cmd;
Tcl_ObjCmdProc table_delete_cmd;
Tcl_ObjCmdProc table_vdelete_cmd;
Tcl_ObjCmdProc table_get_cmd;
Tcl_ObjCmdProc table_index_cmd;
Tcl_ObjCmdProc table_insert_cmd;
Tcl_ObjCmdProc table_vinsert_cmd;
Tcl_ObjCmdProc table_inject_cmd;
Tcl_ObjCmdProc table_vinject_cmd;
Tcl_ObjCmdProc table_place_cmd;
Tcl_ObjCmdProc table_vplace_cmd;
Tcl_ObjCmdProc table_reverse_cmd;
Tcl_ObjCmdProc table_vreverse_cmd;
Tcl_ObjCmdProc table_size_cmd;
Tcl_ObjCmdProc table_width_cmd;
Tcl_ObjCmdProc table_column_cmd;
Tcl_ObjCmdProc table_vcolumn_cmd;
Tcl_ObjCmdProc table__columns_cmd;
Tcl_ObjCmdProc table_cnames_cmd;
Tcl_ObjCmdProc table_slice_cmd;

Tcl_CmdDeleteProc ta_rng_destructor;
Tcl_ObjCmdProc ta_rng_cmd;

Tcl_ObjCmdProc ta_rbc_init_stubs_cmd;

Tcl_ObjCmdProc ta_compiler_info_cmd;
Tcl_ObjCmdProc ta_config_cmd;
Tcl_ObjCmdProc ta_dump_cmd;
Tcl_ObjCmdProc ta_mt_split_cmd;

extern int ta_experiment;
extern int ta_full_validation;

#ifdef TA_MT_ENABLE
/* Threshold for when sorts are multithreaded */
#define TA_MT_THRESHOLD_DEFAULT 10000
extern Tcl_Size ta_search_mt_threshold;
extern Tcl_Size ta_sort_mt_threshold;
extern int ta_sort_mt_enable_any;
extern Tcl_Size ta_fill_mt_threshold;
extern Tcl_Size ta_minmax_mt_threshold;
extern Tcl_Size ta_fold_mt_threshold;
extern Tcl_Size ta_math_mt_threshold;
/* Multithreading support */
Tcl_Size thdr_calc_mt_split(int tatype, Tcl_Size first,
                       Tcl_Size count, Tcl_Size *psecond_block_size);
Tcl_Size thdr_calc_mt_split_ex(int tatype, Tcl_Size first, Tcl_Size count, Tcl_Size min_hint,
                          Tcl_Size nsizes, Tcl_Size sizes[]);

# ifdef HAVE_LIBDISPATCH

typedef dispatch_group_t    ta_mt_group_t;
typedef dispatch_function_t ta_mt_function_t;
typedef dispatch_time_t     ta_mt_time_t;
#define TA_MT_TIME_NOW DISPATCH_TIME_NOW
#define TA_MT_TIME_FOREVER DISPATCH_TIME_FOREVER
TA_INLINE ta_mt_group_t ta_mt_group_create(void) {
    return dispatch_group_create();
}
TA_INLINE int ta_mt_group_async_f(ta_mt_group_t grp, void *ctx, ta_mt_function_t fn) {
    dispatch_queue_t q = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    if (q == NULL)
        return -1;
    dispatch_group_async_f(grp, q, ctx, fn);
    return 0;
}
TA_INLINE long ta_mt_group_wait(ta_mt_group_t grp, ta_mt_time_t timeout) {
    return dispatch_group_wait(grp, timeout);
}
TA_INLINE void ta_mt_group_retain(ta_mt_group_t grp) {
    dispatch_retain(grp);
}
TA_INLINE void ta_mt_group_release(ta_mt_group_t grp) {
    dispatch_release(grp);
}

# elif defined(_WIN32)

typedef struct ta_mt_group_s *ta_mt_group_t;
typedef void(*ta_mt_function_t )(void *);
typedef DWORD ta_mt_time_t;
#define TA_MT_TIME_NOW 0
#define TA_MT_TIME_FOREVER INFINITE

ta_mt_group_t ta_mt_group_create(void);
int ta_mt_group_async_f(ta_mt_group_t grp, void *ctx, ta_mt_function_t fn);
long ta_mt_group_wait(ta_mt_group_t grp, ta_mt_time_t timeout);
void ta_mt_group_retain(ta_mt_group_t grp);
void ta_mt_group_release(ta_mt_group_t grp);

# endif

#else /* ! TA_MT_ENABLE */

typedef void(*ta_mt_function_t )(void *);

#endif

/*
 *  Inlined functions
 */
TA_INLINE TCL_RESULT ta_parse_type(Tcl_Interp *ip, Tcl_Obj *o, unsigned char *ptype) {
    /* The first elem of type array corresponds to TA_NONE so left out */
    int tatype;
    TCL_RESULT status;
    status = ta_opt_from_obj(ip, o, &g_type_tokens[1], "column", TCL_EXACT, &tatype);
    if (status != TCL_OK)
        return status;
    *ptype = (unsigned char) (tatype+1); /* Compensate for off-by-1 offset into array */
    return TCL_OK;
}
TA_INLINE void thdr_incr_refs(thdr_t *thdr)  { thdr->nrefs++; }
TA_INLINE void thdr_decr_refs(thdr_t *thdr) {
    if (thdr->nrefs-- <= 1) thdr_free(thdr);
}
TA_INLINE int thdr_shared(thdr_t *thdr) { return thdr->nrefs > 1; }
TA_INLINE span_t *span_alloc(Tcl_Size first, Tcl_Size count) {
    span_t *span;
    span = TA_ALLOCMEM(sizeof(*span));
    span->nrefs = 0;
    span->first = first;
    span->count = count;
    return span;
}

TA_INLINE void thdr_clear(thdr_t *thdr) {
    void *p = THDRELEMPTR(thdr, unsigned char, 0);
    if (thdr->type == TA_BOOLEAN)
        ba_fill(p, 0, thdr->used, 0);
    else
        memset(p, 0, (thdr->elem_bits/CHAR_BIT) * thdr->used);
}



TA_INLINE void thdr_complement(thdr_t *thdr) {
    TA_ASSERT(thdr->type == TA_BOOLEAN);
    TA_ASSERT(! thdr_shared(thdr));
    if (thdr->used) {
        ba_complement(THDRELEMPTR(thdr, ba_t, 0), 0, thdr->used);
    }
}

TA_INLINE void span_free(span_t *span) { TA_FREEMEM(span); }
TA_INLINE void span_incr_refs(span_t *span) { span->nrefs++; }
TA_INLINE void span_decr_refs(span_t *span) {
    if (span->nrefs-- <= 1) span_free(span);
}
TA_INLINE int span_shared(span_t *span) { return span->nrefs > 1; }
/* This checks that o is currently a tcol, not that it can be converted to one */
TA_INLINE int tcol_affirm(Tcl_Obj *o) {
    return (o->typePtr == &ta_column_type);
}

TA_INLINE int table_affirm(Tcl_Obj *o) {
    return (o->typePtr == &ta_table_type); 
}

TA_INLINE TCL_RESULT tcol_convert(Tcl_Interp *ip, Tcl_Obj *o) {
    return tcol_affirm(o) ? TCL_OK : tcol_convert_from_other(ip, o);
}

/* Return the thdr a column */
TA_INLINE thdr_t *tcol_thdr(Tcl_Obj *o) {
    TA_NOFAIL(tcol_convert(NULL, o), TCL_OK);
    return OBJTHDR(o);
}

/* Return the span of a column */
TA_INLINE span_t *tcol_span(Tcl_Obj *o) {
    TA_NOFAIL(tcol_convert(NULL, o), TCL_OK);
    return OBJTHDRSPAN(o);
}

/* Sets a Tcl_Obj's internal rep pointer assuming it is uninitialized */
TA_INLINE void tcol_set_intrep(Tcl_Obj *o, thdr_t *thdr, span_t *span) {
    thdr_incr_refs(thdr);
    OBJTHDR(o) = thdr;
    if (span)
        span_incr_refs(span);
    OBJTHDRSPAN(o) = span;
    o->typePtr = &ta_column_type;
}

/*
 * Set an initialized internal rep for a Tcl_Obj, invalidating 
 * the string rep 
 */
TA_INLINE void tcol_replace_intrep(Tcl_Obj *o, thdr_t *thdr, span_t *span) {
    TA_ASSERT(tcol_affirm(o));
    TA_ASSERT(! Tcl_IsShared(o));
    TA_ASSERT(tcol_thdr(o) != NULL);
    thdr_incr_refs(thdr);       /* BEFORE thdr_decr_ref in case same */
    thdr_decr_refs(OBJTHDR(o));
    OBJTHDR(o) = thdr;
    if (span)
        span_incr_refs(span);
    if (OBJTHDRSPAN(o))
        span_decr_refs(OBJTHDRSPAN(o));
    OBJTHDRSPAN(o) = span;
    Tcl_InvalidateStringRep(o);
}


/*
 * Inline functions to manipulate internal representation of Tarray tables
 */


/* Return the internal rep of a table */
TA_INLINE thdr_t *table_thdr(Tcl_Obj *o) {
    TA_ASSERT(table_affirm(o));
    return OBJTHDR(o);
}

/* Sets a Tcl_Obj's internal rep pointer assuming it is uninitialized */
TA_INLINE void table_set_intrep(Tcl_Obj *o, thdr_t *thdr, Tcl_Obj *ocolnames)
{
    thdr_incr_refs(thdr);
    OBJTHDR(o) = thdr;
    Tcl_IncrRefCount(ocolnames);
    OBJCOLNAMES(o) = ocolnames;
    o->typePtr = &ta_table_type;
}

/*
 * Set an initialized internal rep for a Tcl_Obj, invalidating 
 * the string rep.
 * ocolnames is ignored if it is NULL.
 */
TA_INLINE void table_replace_intrep(Tcl_Obj *o, thdr_t *thdr,
                                    Tcl_Obj *ocolnames) {
    TA_ASSERT(table_affirm(o));
    TA_ASSERT(! Tcl_IsShared(o));
    TA_ASSERT(table_thdr(o) != NULL);

    thdr_incr_refs(thdr);       /* BEFORE thdr_decr_ref in case same */
    thdr_decr_refs(OBJTHDR(o));
    OBJTHDR(o) = thdr;

    if (ocolnames) {
        Tcl_IncrRefCount(ocolnames);
        if (OBJCOLNAMES(o))
            Tcl_DecrRefCount(OBJCOLNAMES(o));
        OBJCOLNAMES(o) = ocolnames;
    }

    Tcl_InvalidateStringRep(o);
}
TA_INLINE tas_lookup_t thdr_lookup_init(thdr_t *thdr) {
    thdr->lookup = tas_lookup_new();
    return thdr->lookup;
}
TA_INLINE void thdr_lookup_add(thdr_t *thdr, Tcl_Size pos) {
    TA_ASSERT(thdr->type == TA_STRING);
    /* Note we do not check against thdr->used because some callers do
       not update that until later */
    TA_ASSERT(thdr->usable > pos);
    if (thdr->lookup != TAS_LOOKUP_INVALID_HANDLE)
        tas_lookup_add(thdr->lookup, *THDRELEMPTR(thdr, tas_t *, pos), pos);
}
TA_INLINE void thdr_lookup_addn(thdr_t *thdr, Tcl_Size start, Tcl_Size count) {
    tas_t **pptas;
    Tcl_Size i;
    /* Note we do not check against thdr->used because some callers do
       not update that until later */
    TA_ASSERT((start+count) <= thdr->usable);
    if (thdr->lookup == TAS_LOOKUP_INVALID_HANDLE)
        return;
    pptas = THDRELEMPTR(thdr, tas_t *, start);
    for (i = 0; i < count; ++i, ++pptas)
        tas_lookup_add(thdr->lookup, *pptas, (start+i));
}
TA_INLINE void thdr_lookup_delete(thdr_t *thdr, Tcl_Size pos) {
    TA_ASSERT(thdr->type == TA_STRING);
    /* Note we do not check against thdr->used because some callers do
       not update that until later */
    TA_ASSERT(thdr->usable > pos);
    if (thdr->lookup != TAS_LOOKUP_INVALID_HANDLE)
        tas_lookup_delete(thdr->lookup, *THDRELEMPTR(thdr, tas_t *, pos));
}
TA_INLINE void thdr_lookup_free(thdr_t *thdr) {
    TA_ASSERT(thdr->type == TA_STRING);
    if (thdr->lookup != TAS_LOOKUP_INVALID_HANDLE) {
        tas_lookup_free(thdr->lookup);
        thdr->lookup = TAS_LOOKUP_INVALID_HANDLE;
    }
}

TA_INLINE int ta_strequal(const char *a, const char *b)
{
    /* Note a[0] may be == b[0] == \0 so do not pass a+1, b+1 to strcmp */
    return a[0] == b[0] && strcmp(a, b) == 0;
}

TA_INLINE unsigned char tcol_type(Tcl_Obj *o) { return tcol_thdr(o)->type; }
TA_INLINE Tcl_Size tcol_occupancy(Tcl_Obj *o) {
    thdr_t *thdr = tcol_thdr(o);
    span_t *span = OBJTHDRSPAN(o);
    if (span) {
        TA_ASSERT((span->first + span->count) <= thdr->used);
        return span->count;
    }
    else
        return thdr->used;
}

/*
 * Return the starting index and store count of elements in *countP
 */
TA_INLINE Tcl_Size thdr_start_and_count(thdr_t *thdr, span_t *span, Tcl_Size *countP)
{
    if (span) {
        *countP = span->count;
        return span->first;
    } else {
        *countP = thdr->used;
        return 0;
    }
}

/*
 * For a given thdr, computes the pointers to a source and destination offset
 * as well as converts count number of elements to equivalent bytes.
 * Must not be called for TA_BOOLEAN as that combines bits into ba_t's.
 */
TA_INLINE Tcl_Size thdr_compute_move(thdr_t *thdr, Tcl_Size dst_off, Tcl_Size src_off, Tcl_Size count, void **ppdst, void **ppsrc)
{
    char *p = THDRELEMPTR(thdr, char, 0);
    Tcl_Size elem_size = thdr->elem_bits / CHAR_BIT; /* sizeof of one element in bytes */
    TA_ASSERT(thdr->type != TA_BOOLEAN);
    *ppsrc = (src_off * elem_size) + p;
    *ppdst = (dst_off * elem_size) + p;
    return count * elem_size;
}

/* Sanitize destination offset for writing and also
 *   Recompute the new value of used slots when writing or inserting
 */
TA_INLINE Tcl_Size thdr_recompute_occupancy(thdr_t *thdr, Tcl_Size *poff, Tcl_Size count, int insert)
{
    Tcl_Size off = *poff;
    Tcl_Size occupancy;

    TA_ASSERT(count >= 0);

    if (off < 0)
        off = 0;
    else if (off > thdr->used)
        off = thdr->used;

    if (insert)
        occupancy = thdr->used + count;
    else {
        /* New utilization depends whether we are extending the array or not */
        if ((off + count) > thdr->used)
            occupancy = off + count;
        else
            occupancy = thdr->used;
    }

    *poff = off;
    return occupancy;
}

/* Make room for count elements at offset off. Caller must have ensured allocation */
TA_INLINE void thdr_make_room(thdr_t *thdr, Tcl_Size off, Tcl_Size count)
{
    void *d, *s;
    Tcl_Size nbytes;

    if (thdr->type == TA_BOOLEAN) {
        d = THDRELEMPTR(thdr, char, 0);
        ba_copy(d, off + count, d, off, thdr->used - off);
    } else {
        nbytes = thdr_compute_move(thdr, off + count, off,
                                   thdr->used - off, &d, &s);
        memmove(d, s, nbytes); /* Not memcpy, overlapping! */
    }
}


TA_INLINE TCL_RESULT table_convert(Tcl_Interp *ip, Tcl_Obj *table) 
{
    return table->typePtr == &ta_table_type ? TCL_OK : table_convert_from_other(ip, table);
}

TA_INLINE Tcl_Size table_width(Tcl_Obj *table)
{
    TA_NOFAIL(table_convert(NULL, table), TCL_OK);
    return table_thdr(table)->used;
}

TA_INLINE Tcl_Obj **table_columns(Tcl_Obj *table)
{
    TA_NOFAIL(table_convert(NULL, table), TCL_OK);
    return THDRELEMPTR(table_thdr(table), Tcl_Obj *, 0);
}

TA_INLINE Tcl_Obj *table_column(Tcl_Obj *table, Tcl_Size i)
{
    TA_NOFAIL(table_convert(NULL, table), TCL_OK);
    return *THDRELEMPTR(table_thdr(table), Tcl_Obj *, i);
}

TA_INLINE Tcl_Size table_length(Tcl_Obj *table)
{
    TA_NOFAIL(table_convert(NULL, table), TCL_OK);
    return table_width(table) == 0 ? 0 : tcol_occupancy(table_column(table, 0));
}

/* Generic tarray helpers */
TA_INLINE enum ta_collection_type_e ta_collection_type(Tcl_Obj *o)
{
    if (tcol_affirm(o))
        return TA_COLL_COLUMN;
    else if (table_affirm(o))
        return TA_COLL_TABLE;
    else
        return ta_detect_collection_type(o);
}

/*
 * Math - checking for valid doubles
 */
TA_INLINE int ta_isNaN(double dbl)
{
    /* If dbl is NaN, dbl != dbl */
    return ! (dbl == dbl);
}

TA_INLINE int ta_finite_double(double dbl)
{
    /* If dbl is Nan, both comparisons fail.
       If it is -Inf or Inf, one or the other fails
    */
    return (dbl <= DBL_MAX && dbl >= -DBL_MAX);
}

/* Random number generation inlines */
/* Based on PCG basic c distribution pcg32x2-demo.c */
TA_INLINE uint64_t pcg32x2_random_r(pcg32_random_t rng[2])
{
    return ((uint64_t)(pcg32_random_r(&rng[0])) << 32)
            | pcg32_random_r(&rng[1]);
}

/* Returns floating point in range 0 to 1 */
TA_INLINE double pcgdouble_random_r(pcg32_random_t rng[2])
{
#if defined(_MSC_VER) && _MSC_VER <= 1200
    return ldexp((double)(int64_t)pcg32x2_random_r(rng), -64);
#else
    return ldexp((double)pcg32x2_random_r(rng), -64);
#endif
}

/* bound must be positive! */
TA_INLINE double pcgdouble_boundedrand_r(pcg32_random_t rng[2], double bound)
{
    return bound * pcgdouble_random_r(rng);
}

TA_INLINE int pcgbool_random_r(pcg32_random_t *prng)
{
    return pcg32_random_r(prng) & 1;
}

TA_INLINE uint64_t pcg32x2_boundedrand_r(pcg32_random_t rng[2], uint64_t bound)
{
    uint64_t threshold = -bound % bound;
    for (;;) {
        uint64_t r = pcg32x2_random_r(rng);
        if (r >= threshold)
            return r % bound;
    }
}

TCL_RESULT ta_real_init(Tcl_Interp *);

/*
  Local Variables:
  compile-command: "envset x64 && tclsh build.tcl extension -config ../src/tarray.cfg -keep -target win32-dev64"
  End:
*/

#endif
