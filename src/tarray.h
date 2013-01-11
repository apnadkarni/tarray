#ifndef TA_H
#define TA_H

#include <limits.h>
#include <stdlib.h>

#include "tcl.h"
#include "bitarray.h"

#ifndef TA_INLINE
# ifdef _MSC_VER
#  define TA_INLINE __inline  /* Because VC++ 6 only accepts "inline" in C++  */
# elif __GNUC__ && !__GNUC_STDC_INLINE__
#  define TA_INLINE extern inline
# else
#  define TA_INLINE inline
# endif
#endif

#define TA_ENABLE_ASSERT 1      /* TBD - remove after development */

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

typedef int TCL_RESULT;

/* TA_ALLOCMEM and TA_REALLOCMEM call should panic on failure to allocate */
#define TA_ALLOCMEM ckalloc
#define TA_FREEMEM(p_) if (p_) ckfree((char *)p_)
#define TA_REALLOCMEM ckrealloc
#define TA_ATTEMPTALLOCMEM attemptckalloc
#define TA_ATTEMPTREALLOCMEM attemptckrealloc


/* Must match g_ta_type_tokens definition in tarray.c ! */
#define TA_BOOLEAN 0
#define TA_UINT 1
#define TA_INT 2
#define TA_WIDE 3
#define TA_DOUBLE 4
#define TA_BYTE 5
#define TA_OBJ 6

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

extern const char *g_ta_type_tokens[];

/* Must match order of gFormatOptions in tarray.critcl ! */
#define TA_FORMAT_TARRAY 0
#define TA_FORMAT_LIST 1
#define TA_FORMAT_DICT 2

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
        int allocated;
        int used;
        unsigned char type;
        unsigned char elem_bits; /* Size of element in bits */
        unsigned char flags;
#define THDR_F_SORTED    0x01
#define THDR_F_ASCENDING 0x02
    };
} thdr_t;
#define THDRELEMPTR(thdr_, type_, index_) ((index_) + (type_ *)(sizeof(thdr_t) + (char *) (thdr_)))
#define TARRAYHDR(optr_) (*(thdr_t **) (&((optr_)->internalRep.ptrAndLongRep.ptr)))

/* How much extra slots to allocate when allocating memory. n_ should
 * be number of elements currently. TBD - guard against overflow
 */
#define TA_EXTRA(n_)  \
    ((n_) < 10 ? 10 : ((n_) < 100 ? (n_) : ((n_) < 800 ? 100 : ((n_)/8))))

extern struct Tcl_ObjType g_ta_type;
extern struct Tcl_ObjType gTGridType;


/*
 * Error and panic routines
 */
const char *ta_type_string(int tatype);
void ta_type_panic(unsigned char tatype);
void ta_shared_panic(const char *where);
void ta_small_panic(thdr_t *thdr, const char *where);
TCL_RESULT ta_not_tarray_error(Tcl_Interp *);
TCL_RESULT ta_search_op_error(Tcl_Interp *, int op);
TCL_RESULT ta_value_type_error(Tcl_Interp *, Tcl_Obj *o, int tatype);
TCL_RESULT ta_grid_length_error(Tcl_Interp *);
TCL_RESULT ta_row_width_error(Tcl_Interp *, int rowwidth, int gridwidth);
TCL_RESULT ta_bad_type_error(Tcl_Interp *ip, thdr_t *thdr);
TCL_RESULT ta_memory_error(Tcl_Interp *, int size);
TCL_RESULT ta_indices_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_index_error(Tcl_Interp *ip, Tcl_Obj *o);
TCL_RESULT ta_index_range_error(Tcl_Interp *ip, int index);
TCL_RESULT ta_mismatched_types_error(Tcl_Interp *ip);
TCL_RESULT ta_indices_count_error(Tcl_Interp *ip, int nindices, int nvalues);
void thdr_incr_obj_refs(thdr_t *thdr,int first,int count);
void thdr_decr_obj_refs(thdr_t *thdr,int first,int count);
void thdr_free(thdr_t *thdr);
TCL_RESULT tcol_convert(Tcl_Interp *, Tcl_Obj *o);

TCL_RESULT TGridVerifyType(Tcl_Interp *, Tcl_Obj *gridObj);
Tcl_Obj *TGridNewObj(Tcl_Interp *, int nobjs, Tcl_Obj *const tcols[]);
Tcl_Obj *TGridClone(Tcl_Interp *, Tcl_Obj *gridObj, int minsize);
TCL_RESULT TGridConvert(Tcl_Interp *, Tcl_Obj *o);
TCL_RESULT ta_value_from_obj(Tcl_Interp *, Tcl_Obj *o,
                              unsigned char tatype, ta_value_t *ptav);
void thdr_fill_range(Tcl_Interp *, thdr_t *thdr,
                    const ta_value_t *ptav, int pos, int count);
void thdr_fill_indices(Tcl_Interp *, thdr_t *thdr,
                            const ta_value_t *ptav, thdr_t *indicesP);
Tcl_Obj *thdr_index(thdr_t *thdr, int index);

TCL_RESULT thdr_tSetMultipleFromObjs(Tcl_Interp *,
                                    thdr_t * const thdrs[], int nthdrs,
                                    Tcl_Obj *tuples, int first);
TCL_RESULT TGridFillFromObjs(Tcl_Interp *, Tcl_Obj *lowObj, Tcl_Obj *ohigh,
                             Tcl_Obj *gridObj, Tcl_Obj *rowObj);

Tcl_Obj * tcol_new(thdr_t *thdr);
TCL_RESULT tcol_make_modifiable(Tcl_Interp *ip, Tcl_Obj *tcol, int minsize, int prefsize);

TCL_RESULT thdr_put_objs(struct Tcl_Interp *,thdr_t *thdr,int first,int nelems,struct Tcl_Obj *const *elems );
TCL_RESULT thdr_place_objs(Tcl_Interp *, thdr_t *thdr, thdr_t *indicesP,
                              int highest_in_indices,
                              int nvalues, Tcl_Obj * const *valuesP);
int thdr_required_size(unsigned char tatype,int count);
thdr_t *thdr_realloc(Tcl_Interp *, thdr_t *oldP,int new_count);
thdr_t *thdr_alloc(Tcl_Interp *, unsigned char tatype, int count);
thdr_t *thdr_alloc_and_init(Tcl_Interp *,unsigned char tatype,int nelems,struct Tcl_Obj *const *elems ,int init_size);
void thdr_reverse(thdr_t *tdrhP);
void thdr_copy_reversed(thdr_t *dstP,int dst_first,thdr_t *srcP,int src_first,int count);
void thdr_copy(thdr_t *dstP,int dst_first,thdr_t *srcP,int src_first,int count);
void thdr_delete_range(thdr_t *thdr, int first, int count);
void thdr_delete_indices(thdr_t *thdr, thdr_t *indicesP);
thdr_t *thdr_clone(Tcl_Interp *, thdr_t *srcP, int init_size);
thdr_t *thdr_clone_reversed(Tcl_Interp *, thdr_t *srcP, int init_size);
TCL_RESULT ta_verify_value_objs(Tcl_Interp *intepr, int tatype,
                             int nelems, Tcl_Obj * const elems[]);
TCL_RESULT thdr_verify_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *indicesP, int *new_sizeP);
int tcol_to_indices(struct Tcl_Interp *, struct Tcl_Obj *o,
                           int want_sorted, thdr_t **thdrP, int *pindex);
#define TA_INDEX_TYPE_ERROR 0
#define TA_INDEX_TYPE_INT   1
#define TA_INDEX_TYPE_thdr_t 2

thdr_t *thdr_range(Tcl_Interp *ip, thdr_t *srcP, int low, int count);
Tcl_Obj *ta_range(Tcl_Interp *ip, Tcl_Obj *srcObj, int low, int count,
                     int fmt);
TCL_RESULT tcol_delete(Tcl_Interp *ip, Tcl_Obj *tcol,
                        Tcl_Obj *indexA, Tcl_Obj *indexB);
TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *valueObj,
                      Tcl_Obj *indexA, Tcl_Obj *indexB);
Tcl_Obj *tcol_get(struct Tcl_Interp *, thdr_t *srcP, thdr_t *indicesP, int fmt);
int TArrayNumSetBits(thdr_t *thdr);
TCL_RESULT tcol_copy_thdr(Tcl_Interp *, Tcl_Obj *tcol, thdr_t *srcP, Tcl_Obj *firstObj);
TCL_RESULT tcol_put_objs(Tcl_Interp *, Tcl_Obj *tcol,
                             Tcl_Obj *valueListObj, Tcl_Obj *firstObj);
TCL_RESULT tcol_place_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                               Tcl_Obj *valueListObj, Tcl_Obj *indicesObj);
TCL_RESULT ta_convert_index(Tcl_Interp *, Tcl_Obj *o, int *pindex,
                      int end_value, int low, int high);
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *, const thdr_t *thdr, Tcl_Obj *lowObj, Tcl_Obj *ohigh, int *plow, int *pcount);
TCL_RESULT TGridSetFromObjs(Tcl_Interp *, Tcl_Obj *lowObj, Tcl_Obj *gridObj,
    Tcl_Obj *valueObjs, /* Each element is a list (tuple value) */
    int flags);

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


/* Tcl script level commands */
TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *ip,
                                      int objc, Tcl_Obj *const objv[]);

/* Inlined functions */
TA_INLINE void thdr_incr_refs(thdr_t *thdr)  { thdr->nrefs++; }
TA_INLINE void thdr_decr_refs(thdr_t *thdr) {
    if (thdr->nrefs-- <= 1) thdr_free(thdr);
}
TA_INLINE int thdr_shared(thdr_t *thdr) { return thdr->nrefs > 1; }

/* Sets a Tcl_Obj's internal rep pointer. Assumes the Tcl_Obj int rep is
   invalid / uninitialized */
TA_INLINE ta_set_intrep(Tcl_Obj *obj, thdr_t *thdr) {
    thdr_incr_refs(thdr);
    TARRAYHDR(obj) = thdr;
    obj->typePtr = &g_ta_type;                 \
}

TA_INLINE unsigned char tcol_type(Tcl_Obj *obj) { return TARRAYHDR(obj)->type; }
TA_INLINE int tcol_occupancy(Tcl_Obj *obj) { return TARRAYHDR(obj)->used; }

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
