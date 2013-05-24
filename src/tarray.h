#ifndef TA_H
#define TA_H

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS /* Disable Visual C++ snprintf security warnings */
#include <windows.h>            /* TBD - define LEAN_AND_MEAN ? */
#endif

#include <limits.h>
#include <string.h>
#include <stdlib.h>

#include "tcl.h"

#ifdef TA_USE_LIBDISPATCH
# include "dispatch.h"
#endif

#ifndef ARRAYSIZE
/* Older SDK's do not define this */
#define ARRAYSIZE(A) RTL_NUMBER_OF(A)
#endif

#ifndef TA_INLINE
# ifdef _MSC_VER
#  define TA_INLINE __inline  /* Because VC++ 6 only accepts "inline" in C++  */
# elif __GNUC__ && !__GNUC_STDC_INLINE__
#  define TA_INLINE extern inline
# else
#  define TA_INLINE inline
# endif
#endif

/* If building out of twapi pool, use its settings */
#if defined(TWAPI_ENABLE_ASSERT) && !defined(TA_ENABLE_ASSERT)
#define TA_ENABLE_ASSERT TWAPI_ENABLE_ASSERT
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
#include "bitarray.h"           /* Include AFTER assert definitions */
#include "timsort.h"

typedef int TCL_RESULT;

/* Various portability defines */
#ifdef _MSC_VER
# define snprintf _snprintf
#endif

/* TA_ALLOCMEM and TA_REALLOCMEM call should panic on failure to allocate */
#define TA_ALLOCMEM ckalloc
#define TA_FREEMEM(p_) if (p_) ckfree((char *)p_)
#define TA_REALLOCMEM ckrealloc
#define TA_ATTEMPTALLOCMEM attemptckalloc
#define TA_ATTEMPTREALLOCMEM attemptckrealloc


/* Must match g_type_tokens definition in tarray.c ! */
#define TA_BOOLEAN 0
#define TA_UINT 1
#define TA_INT 2
#define TA_WIDE 3
#define TA_DOUBLE 4
#define TA_BYTE 5
#define TA_ANY 6

typedef struct ta_value_s {
    unsigned char type;
    union {
        int          bval : 1;
        unsigned int uival;
        int          ival;
        Tcl_WideInt  wval;
        double       dval;
        unsigned char ucval;
        Tcl_Obj *     oval;
    };
} ta_value_t;

extern const char *g_type_tokens[];

/* Must match order of gFormatOptions in tarray.critcl ! */
#define TA_FORMAT_TARRAY 0
#define TA_FORMAT_LIST 1
#define TA_FORMAT_DICT 2

/* MUST match order of switches[] in tcol_parse_sort_options */
#define TA_SORT_DECREASING 1
#define TA_SORT_INDICES    2
#define TA_SORT_NOCASE     4
#define TA_SORT_INDIRECT   8

/* Pointers to Tcl's built-in type descriptors */
extern Tcl_ObjType *g_tcl_list_type_ptr;

/* How many slots to allocate by default */
#define TA_DEFAULT_NSLOTS 1000

#define TA_MAX_ELEM_SIZE (sizeof(double) > sizeof(int) ? (sizeof(double) > sizeof(void*) ? sizeof(double) : sizeof(void*)) : sizeof(int))
#define TA_MAX_COUNT (1 + (int)(((size_t)UINT_MAX - sizeof(thdr_t))/TA_MAX_ELEM_SIZE))

typedef union thdr_s {
    void *pointer_aligner;
    double double_aligner;
    struct {
        int nrefs;              /* Ref count when shared between Tcl_Objs */
        int usable;             /* Number of slots that can be used. This
                                   is one less than number allocated as
                                   we keep one as a sentinel.
                                */
        int used;
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
#define THDRELEMPTR(thdr_, type_, index_) ((index_) + (type_ *)(sizeof(thdr_t) + (char *) (thdr_)))
#define TARRAYHDR(optr_) (*(thdr_t **) (&((optr_)->internalRep.ptrAndLongRep.ptr)))

/* How much extra slots to allocate when allocating memory. n_ should
 * be number of elements currently. TBD - guard against overflow
 */
#define TA_EXTRA(n_)  \
    ((n_) < 10 ? 10 : ((n_) < 100 ? (n_) : ((n_) < 800 ? 100 : ((n_)/8))))

extern struct Tcl_ObjType g_tcol_type;
extern struct Tcl_ObjType g_table_type;

/*
 * Error and panic routines
 */
const char *ta_type_string(int tatype);
void ta_type_panic(int tatype);
void ta_shared_panic(const char *where);
void ta_small_panic(thdr_t *thdr, const char *where);
TCL_RESULT ta_not_tarray_error(Tcl_Interp *);
TCL_RESULT ta_search_op_error(Tcl_Interp *, int op);
TCL_RESULT ta_value_type_error(Tcl_Interp *, Tcl_Obj *o, int tatype);
TCL_RESULT ta_table_length_error(Tcl_Interp *);
TCL_RESULT ta_row_width_error(Tcl_Interp *, int rowwidth, int tablewidth);
TCL_RESULT ta_bad_type_error(Tcl_Interp *ip, thdr_t *thdr);
TCL_RESULT ta_memory_error(Tcl_Interp *, int size);
TCL_RESULT ta_indices_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_index_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_index_range_error(Tcl_Interp *ip, int index);
TCL_RESULT ta_invalid_range_error(Tcl_Interp *ip, Tcl_Obj *);
TCL_RESULT ta_bad_count_error(Tcl_Interp *ip, int count);
TCL_RESULT ta_mismatched_types_error(Tcl_Interp *ip, int typea, int typeb);
TCL_RESULT ta_indices_count_error(Tcl_Interp *ip, int nindices, int nvalues);
TCL_RESULT ta_missing_arg_error(Tcl_Interp *ip, char *optname);
TCL_RESULT ta_invalid_opt_error(Tcl_Interp *ip, char *optname);

TCL_RESULT ta_get_byte_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned char *pb);
TCL_RESULT ta_get_uint_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned int *pui);
TCL_RESULT ta_get_int_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int *pi);

void thdr_incr_obj_refs(thdr_t *thdr,int first,int count);
void thdr_decr_obj_refs(thdr_t *thdr,int first,int count);
void thdr_free(thdr_t *thdr);
TCL_RESULT tcol_convert_from_other(Tcl_Interp *, Tcl_Obj *o);
TCL_RESULT table_convert_from_other(Tcl_Interp *, Tcl_Obj *o);
void thdr_ensure_obj_strings(thdr_t *thdr);

TCL_RESULT TGridVerifyType(Tcl_Interp *, Tcl_Obj *tableObj);
Tcl_Obj *TGridNewObj(Tcl_Interp *, int nobjs, Tcl_Obj *const tcols[]);
Tcl_Obj *TGridClone(Tcl_Interp *, Tcl_Obj *tableObj, int minsize);
TCL_RESULT TGridConvert(Tcl_Interp *, Tcl_Obj *o);
TCL_RESULT ta_value_from_obj(Tcl_Interp *, Tcl_Obj *o,
                              unsigned char tatype, ta_value_t *ptav);
void thdr_fill_range(Tcl_Interp *, thdr_t *thdr,
                     const ta_value_t *ptav, int pos, int count, int insert);
void thdr_fill_ta_objs(thdr_t *thdr, thdr_t *pindices, Tcl_Obj *oval, int highest_in_indices);
TCL_RESULT thdr_put_objs(Tcl_Interp *ip, thdr_t *thdr, int first,
                         int nelems, Tcl_Obj * const elems[], int insert);
void thdr_place_ta_objs(thdr_t *thdr, thdr_t *pindices, Tcl_Obj * const *ovalues, int highest_in_indices);
void thdr_fill_indices(Tcl_Interp *, thdr_t *thdr,
                       const ta_value_t *ptav, thdr_t *pindices, int highest_index);
Tcl_Obj *thdr_index(thdr_t *thdr, int index);

TCL_RESULT TGridFillFromObjs(Tcl_Interp *, Tcl_Obj *olow, Tcl_Obj *ohigh,
                             Tcl_Obj *tableObj, Tcl_Obj *rowObj);

Tcl_Obj * tcol_new(thdr_t *thdr);
TCL_RESULT tcol_make_modifiable(Tcl_Interp *ip, Tcl_Obj *tcol, int minsize, int prefsize);
TCL_RESULT table_make_modifiable(Tcl_Interp *ip,
                                 Tcl_Obj *table, int minsize, int prefsize);

TCL_RESULT tcols_put_objs(Tcl_Interp *ip, int ncols, Tcl_Obj * const *tcols,
                          int nrows, Tcl_Obj * const *rows,
                          int first, int insert);

void thdr_place_objs(Tcl_Interp *, thdr_t *thdr, thdr_t *pindices,
                     int new_size,
                     int nvalues, Tcl_Obj * const *pvalues);
void thdr_place_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *psrc, thdr_t *pindices, int new_size);

int thdr_required_size(int tatype, int count);
thdr_t *thdr_realloc(Tcl_Interp *, thdr_t *oldP,int new_count);
thdr_t *thdr_alloc(Tcl_Interp *, int tatype, int count);
thdr_t *thdr_alloc_and_init(Tcl_Interp *, int tatype,int nelems,struct Tcl_Obj *const *elems ,int init_size);
void thdr_reverse(thdr_t *tdrhP);
void thdr_copy_reversed(thdr_t *pdst,int dst_first,thdr_t *psrc,int src_first,int count);
void thdr_copy(thdr_t *pdst,int dst_first,thdr_t *psrc,int src_first,int count, int insert);
void thdr_delete_range(thdr_t *thdr, int first, int count);
void thdr_delete_indices(thdr_t *thdr, thdr_t *pindices);
thdr_t *thdr_clone(Tcl_Interp *, thdr_t *psrc, int init_size);
thdr_t *thdr_clone_reversed(Tcl_Interp *, thdr_t *psrc, int init_size);
TCL_RESULT ta_verify_value_objs(Tcl_Interp *intepr, int tatype,
                             int nelems, Tcl_Obj * const elems[]);
TCL_RESULT thdr_verify_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *pindices, int *new_sizeP);
int ta_obj_to_indices(struct Tcl_Interp *, struct Tcl_Obj *o,
                      int want_sorted, int end, thdr_t **thdrP, int *pindex);
#define TA_INDEX_TYPE_ERROR 0
#define TA_INDEX_TYPE_INT   1
#define TA_INDEX_TYPE_THDR 2

thdr_t *thdr_range(Tcl_Interp *ip, thdr_t *psrc, int low, int count);
Tcl_Obj *tcol_range(Tcl_Interp *ip, Tcl_Obj *srcObj, int low, int count,
                     int fmt);
TCL_RESULT tcol_delete(Tcl_Interp *ip, Tcl_Obj *tcol,
                        Tcl_Obj *indexA, Tcl_Obj *indexB);
TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                         Tcl_Obj *indexA, Tcl_Obj *indexB);
TCL_RESULT tcol_insert_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                           Tcl_Obj *opos, Tcl_Obj *ocount);
TCL_RESULT tcol_reverse(Tcl_Interp *ip, Tcl_Obj *tcol);

TCL_RESULT tcols_fill_range(Tcl_Interp *ip, int ntcols, Tcl_Obj **tcols,
                            Tcl_Obj *orow, int pos, int count, int insert);
TCL_RESULT tcols_fill_indices(Tcl_Interp *ip, int ntcols,
                              Tcl_Obj **tcols, Tcl_Obj *orow, thdr_t *pindices,
                              int highest_index);
 TCL_RESULT tcols_place_objs(Tcl_Interp *ip, int ntcols, Tcl_Obj * const *tcols,
                             thdr_t *pindices, Tcl_Obj *orows,
                             int new_size);
TCL_RESULT tcols_place_indices(Tcl_Interp *ip, int ntcols, Tcl_Obj * const *tcols, Tcl_Obj * const *srccols, thdr_t *pindices, int new_size);

TCL_RESULT table_fill_obj(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *orow, Tcl_Obj *indexa, Tcl_Obj *indexb, int insert);
TCL_RESULT table_put_objs(Tcl_Interp *ip, Tcl_Obj *table,
                          Tcl_Obj *orows, Tcl_Obj *ofirst, int insert);
TCL_RESULT table_copy(Tcl_Interp *ip, Tcl_Obj *dstable, Tcl_Obj *srctable, Tcl_Obj *ofirst, int insert);
TCL_RESULT table_delete(Tcl_Interp *ip, Tcl_Obj *table,
                        Tcl_Obj *indexa, Tcl_Obj *indexb);
Tcl_Obj *table_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, int fmt);
Tcl_Obj *table_range(Tcl_Interp *ip, Tcl_Obj *osrc, int low, int count, int fmt);
TCL_RESULT table_reverse(Tcl_Interp *interp, Tcl_Obj *table);
Tcl_Obj *table_index(Tcl_Interp *ip, Tcl_Obj *table, int index);
TCL_RESULT table_place_objs(Tcl_Interp *ip, Tcl_Obj *table,
                            Tcl_Obj *orows, Tcl_Obj *oindices);
TCL_RESULT table_place_indices(Tcl_Interp *ip, Tcl_Obj *table,
                               Tcl_Obj *psrc, Tcl_Obj *oindices);
TCL_RESULT table_insert_obj(Tcl_Interp *ip, Tcl_Obj *table, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Obj *ocount);

Tcl_Obj *tcol_index(Tcl_Interp *ip, Tcl_Obj *tcol, int index);
Tcl_Obj *tcol_get(struct Tcl_Interp *, Tcl_Obj *osrc, thdr_t *pindices, int fmt);
int TArrayNumSetBits(thdr_t *thdr);
TCL_RESULT tcol_copy_thdr(Tcl_Interp *, Tcl_Obj *tcol, thdr_t *psrc, Tcl_Obj *firstObj, int insert);
TCL_RESULT tcol_put_objs(Tcl_Interp *, Tcl_Obj *tcol,
                         Tcl_Obj *valueListObj, Tcl_Obj *firstObj, int insert);
TCL_RESULT tcol_place_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                               Tcl_Obj *valueListObj, Tcl_Obj *indicesObj);
TCL_RESULT tcol_place_indices(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *osrc,
                              Tcl_Obj *oindices);
TCL_RESULT ta_convert_index(Tcl_Interp *, Tcl_Obj *o, int *pindex,
                      int end_value, int low, int high);
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *, int nelems, Tcl_Obj *olow, Tcl_Obj *ohigh, int *plow, int *pcount);

TCL_RESULT tcols_validate_obj_row_widths(Tcl_Interp *ip, int width,
                                         int nrows, Tcl_Obj * const rows[]);
TCL_RESULT tcols_validate_obj_rows(Tcl_Interp *ip, int ntcols,
                                   Tcl_Obj * const *tcols,
                                   int nrows, Tcl_Obj * const rows[]);

TCL_RESULT tcols_copy(Tcl_Interp *ip, int ntcols,
                      Tcl_Obj * const *dstcols, int dst_elem_first,
                      Tcl_Obj * const *srccols, int src_elem_first,
                      int count, int insert);

/*
 * Search and sort routines
 */
int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase);

void tarray_qsort_r(void *a, size_t n, size_t es, void *thunk, int (*cmp)(void *, const void *, const void *));
int intcmp(const void *a, const void *b);
int intcmprev(const void *a, const void *b);
int uintcmp(const void *a, const void *b);
int uintcmprev(const void *a, const void *b);
int widecmp(const void *a, const void *b);
int widecmprev(const void *a, const void *b);
int doublecmp(const void *a, const void *b);
int doublecmprev(const void *a, const void *b);
int bytecmp(const void *a, const void *b);
int bytecmprev(const void *a, const void *b);
int tclobjcmp(const void *a, const void *b);
int tclobjcmprev(const void *a, const void *b);
int tclobjcmpnocase(const void *a, const void *b);
int tclobjcmprevnocase(const void *a, const void *b);

int intcmpindexed(void *, const void *a, const void *b);
int intcmpindexedrev(void *, const void *a, const void *b);
int uintcmpindexed(void *, const void *a, const void *b);
int uintcmpindexedrev(void *, const void *a, const void *b);
int widecmpindexed(void *, const void *a, const void *b);
int widecmpindexedrev(void *, const void *a, const void *b);
int doublecmpindexed(void *, const void *a, const void *b);
int doublecmpindexedrev(void *, const void *a, const void *b);
int bytecmpindexed(void *, const void *a, const void *b);
int bytecmpindexedrev(void *, const void *a, const void *b);
int tclobjcmpindexed(void *, const void *a, const void *b);
int tclobjcmpindexedrev(void *, const void *a, const void *b);
int tclobjcmpnocaseindexed(void *, const void *a, const void *b);
int tclobjcmpnocaseindexedrev(void *, const void *a, const void *b);

TCL_RESULT tcol_parse_sort_options(Tcl_Interp *ip,
                                   int objc, Tcl_Obj *const objv[],
                                   int *pflags, Tcl_Obj **ptarget);
TCL_RESULT tcol_sort(Tcl_Interp *ip, Tcl_Obj *tcol, int flags);

TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *ip,
                                      int objc, Tcl_Obj *const objv[]);
TCL_RESULT tcol_sort_indirect(Tcl_Interp *ip, Tcl_Obj *oindices, Tcl_Obj *otarget, int flags);

TCL_RESULT ta_dump_cmd(ClientData clientdata, Tcl_Interp *ip,
                       int objc, Tcl_Obj *const objv[]);

#ifdef TA_MT_ENABLE
/* Threshold for when sorts are multithreaded */
extern int ta_sort_mt_threshold;
extern int ta_sort_mt_enable_any;
extern int ta_fill_mt_threshold;
/* Multithreading support */
int thdr_calc_mt_split(int tatype, int first, int count, int *psecond_block_size);

# ifdef TA_USE_LIBDISPATCH

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

#endif

/*
 *  Inlined functions
 */
TA_INLINE int ta_strequal(const char *a, const char *b)
{
    /* Note a[0] may be == b[0] == \0 so do not pass a+1, b+1 to strcmp */
    return a[0] == b[0] && strcmp(a, b) == 0;
}

TA_INLINE void thdr_incr_refs(thdr_t *thdr)  { thdr->nrefs++; }
TA_INLINE void thdr_decr_refs(thdr_t *thdr) {
    if (thdr->nrefs-- <= 1) thdr_free(thdr);
}
TA_INLINE int thdr_shared(thdr_t *thdr) { return thdr->nrefs > 1; }

TA_INLINE int tcol_affirm(Tcl_Obj *o)
{
    return (o->typePtr == &g_tcol_type || o->typePtr == &g_table_type);
}

TA_INLINE TCL_RESULT tcol_convert(Tcl_Interp *ip, Tcl_Obj *o) 
{
    return tcol_affirm(o) ? TCL_OK : tcol_convert_from_other(ip, o);
}

/* Sets a Tcl_Obj's internal rep pointer. Assumes the Tcl_Obj int rep is
   invalid / uninitialized */
TA_INLINE void ta_set_intrep(Tcl_Obj *o, thdr_t *thdr) {
    thdr_incr_refs(thdr);
    TARRAYHDR(o) = thdr;
    o->typePtr = &g_tcol_type;                 \
}

TA_INLINE void ta_replace_intrep(Tcl_Obj *o, thdr_t *thdr) {
    TA_ASSERT(tcol_affirm(o));
    TA_ASSERT(! Tcl_IsShared(o));
    TA_ASSERT(TARRAYHDR(o) != NULL);
    thdr_incr_refs(thdr);       /* BEFORE thdr_decr_ref in case same */
    thdr_decr_refs(TARRAYHDR(o));
    TARRAYHDR(o) = thdr;
    Tcl_InvalidateStringRep(o);
}


TA_INLINE unsigned char tcol_type(Tcl_Obj *o) { return TARRAYHDR(o)->type; }
TA_INLINE int tcol_occupancy(Tcl_Obj *o) { return TARRAYHDR(o)->used; }

#ifdef OBSOLETE
TA_INLINE int thdr_sorted(thdr_t *thdr) {
    return thdr->flags & THDR_F_SORTED;
}
TA_INLINE int thdr_sort_order(thdr_t *thdr) {
    return thdr->flags & THDR_F_SORTED 
        ? (thdr->flags & THDR_F_ASCENDING ? 1 : -1)
        : 0;
}
TA_INLINE void thdr_mark_unsorted(thdr_t *thdr) {
    thdr->flags &= ~THDR_F_SORTED;
}
TA_INLINE void thdr_mark_sorted_ascending(thdr_t *thdr) {
    thdr->flags |= THDR_F_SORTED | THDR_F_ASCENDING;
}
TA_INLINE void thdr_mark_sorted_descending(thdr_t *thdr) {
    thdr->flags &= ~THDR_F_ASCENDING;
    thdr->flags |= THDR_F_SORTED;
}
TA_INLINE void thdr_copy_sort_status(thdr_t *thdr, thdr_t *fromP) {
    thdr->flags &= ~(THDR_F_SORTED | THDR_F_ASCENDING);
    thdr->flags |= fromP->flags & (THDR_F_SORTED | THDR_F_ASCENDING);
}
#endif

/*
 * For a given thdr, computes the pointers to a source and destination offset
 * as well as converts count number of elements to equivalent bytes.
 * Must not be called for TA_BOOLEAN as that combines bits into ba_t's.
 */
TA_INLINE int thdr_compute_move(thdr_t *thdr, int dst_off, int src_off, int count, void **ppdst, void **ppsrc)
{
    char *p = THDRELEMPTR(thdr, char, 0);
    int elem_size = thdr->elem_bits / CHAR_BIT; /* sizeof of one element in bytes */
    TA_ASSERT(thdr->type != TA_BOOLEAN);
    *ppsrc = (src_off * elem_size) + p;
    *ppdst = (dst_off * elem_size) + p;
    return count * elem_size;
}

/* Sanitize destination offset for writing and also
 *   Recompute the new value of used slots when writing or inserting
 */
TA_INLINE int thdr_recompute_occupancy(thdr_t *thdr, int *poff, int count, int insert)
{
    int off = *poff;
    int occupancy;

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
TA_INLINE void thdr_make_room(thdr_t *thdr, int off, int count)
{
    void *d, *s;
    int nbytes;

    if (thdr->type == TA_BOOLEAN) {
        d = THDRELEMPTR(thdr, char, 0);
        ba_copy(d, off + count, d, off, thdr->used - off);
    } else {
        nbytes = thdr_compute_move(thdr, off + count, off,
                                   thdr->used - off, &d, &s);
        memmove(d, s, nbytes); /* Not memcpy, overlapping! */
    }
}

TA_INLINE int table_affirm(Tcl_Obj *o)
{
    return (o->typePtr == &g_table_type); 
}

TA_INLINE TCL_RESULT table_convert(Tcl_Interp *ip, Tcl_Obj *table) 
{
    return table->typePtr == &g_table_type ? TCL_OK : table_convert_from_other(ip, table);
}

TA_INLINE int table_width(Tcl_Obj *table)
{
    TA_ASSERT(table_affirm(table));
    return tcol_occupancy(table);
}

TA_INLINE Tcl_Obj **table_columns(Tcl_Obj *table)
{
    TA_ASSERT(table_affirm(table));
    return THDRELEMPTR(TARRAYHDR(table), Tcl_Obj *, 0);
}

TA_INLINE Tcl_Obj *table_column(Tcl_Obj *table, int i)
{
    TA_ASSERT(table_affirm(table));
    return *THDRELEMPTR(TARRAYHDR(table), Tcl_Obj *, i);
}

TA_INLINE int table_length(Tcl_Obj *table)
{
    TA_ASSERT(table_affirm(table));
    return table_width(table) == 0 ? 0 : tcol_occupancy(table_column(table, 0));
}

/* Assumes properly structured thdr */
TA_INLINE Tcl_Obj *table_new(thdr_t *thdr)
{
    Tcl_Obj *table;
    TA_ASSERT(thdr->type == TA_ANY);
    table = tcol_new(thdr);
    if (table)
        table->typePtr = &g_table_type;
    return table;
}

#endif
