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

/* Must match g_ta_type_tokens definition in tarray.c ! */
#define TA_BOOLEAN 0
#define TA_UINT 1
#define TA_INT 2
#define TA_WIDE 3
#define TA_DOUBLE 4
#define TA_BYTE 5
#define TA_OBJ 6

typedef struct TArrayValue_s {
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
} TArrayValue;

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
#define TA_MAX_COUNT (1 + (int)(((size_t)UINT_MAX - sizeof(TAHdr))/TA_MAX_ELEM_SIZE))

typedef union TAHdr_s {
    void *pointer_aligner;
    double double_aligner;
    struct {
        int nrefs;              /* Ref count when shared between Tcl_Objs */
        int allocated;
        int used;
        unsigned char type;
        unsigned char elem_bits; /* Size of element in bits */
        unsigned char flags;
#define TAHDR_F_SORTED    0x01
#define TAHDR_F_ASCENDING 0x02
    };
} TAHdr;
#define TAHDRELEMPTR(thdr_, type_, index_) ((index_) + (type_ *)(sizeof(TAHdr) + (char *) (thdr_)))
#define TAHDRELEMUSEDBYTES(thdr_) ((((thdr_)->used * (thdr_)->elem_bits) + CHAR_BIT-1) / CHAR_BIT)

#define TARRAYHDR(optr_) (*(TAHdr **) (&((optr_)->internalRep.ptrAndLongRep.ptr)))
#define TARRAYTYPE(optr_) (TARRAYHDR(optr_)->type)
#define TARRAYELEMSLOTS(optr_) ((TARRAYHDR(optr_))->allocated)
#define TARRAYELEMCOUNT(optr_) ((TARRAYHDR(optr_))->used)
#define TARRAYELEMPTR(optr_, type_, index_) TAHDRELEMPTR(TARRAYHDR(optr_), type_, index_)

/* How much extra slots to allocate when allocating memory. n_ should
 * be number of elements currently. TBD - guard against overflow
 */
#define TA_EXTRA(n_)  \
    ((n_) < 10 ? 10 : ((n_) < 100 ? (n_) : ((n_) < 800 ? 100 : ((n_)/8))))

/* Can a TAHdr block be modified ? Must be unshared and large enough */
#define TAHDR_SHARED(th_) ((th_)->nrefs > 1)
#define TAHDR_WRITABLE(th_, size_) (TARRAYHDR_SHARED(th_) && (th_)->allocated >= (size_))

extern struct Tcl_ObjType g_ta_type;
extern struct Tcl_ObjType gTGridType;

#define TAHDR_INCRREF(thdr_) do { (thdr_)->nrefs++; } while (0)
#define TAHDR_DECRREF(thdr_)                                            \
    do {                                                                \
        TAHdr *h_ = (thdr_);  /* Temp in case parameter has sideeffects*/ \
        if (--(h_)->nrefs <= 0)                                         \
            thdr_free(h_);                                          \
    } while (0)

/* Sets a Tcl_Obj's internal rep pointer. Assumes the Tcl_Obj int rep is
   invalid / uninitialized */
#define TA_OBJ_SETREP(obj_, thdr_)                                     \
    do {                                                                \
        TAHdr *h_ = (thdr_);  /* Temp in case parameter has sideeffects*/ \
        Tcl_Obj *o_ = (obj_);  /* Temp in case parameter has sideeffects*/ \
        TAHDR_INCRREF(h_); \
        TARRAYHDR(o_) = h_; \
        (o_)->typePtr = &g_ta_type; \
    } while (0)

/* ALLOCATE_ARRAY call should panic on failure to allocate */
#define TA_ALLOCMEM ckalloc
#define TA_FREEMEM(p_) if (p_) ckfree((char *)p_)
#define TA_REALLOCMEM ckrealloc
#define TA_ATTEMPTALLOCMEM attemptckalloc
#define TA_ATTEMPTREALLOCMEM attemptckrealloc

TA_INLINE int thdr_sorted(TAHdr *thdrP) {
    return thdrP->flags & TAHDR_F_SORTED;
}
TA_INLINE int thdr_sort_order(TAHdr *thdrP) {
    return thdrP->flags & TAHDR_F_SORTED 
        ? (thdrP->flags & TAHDR_F_ASCENDING ? 1 : -1)
        : 0;
}
TA_INLINE void thdr_mark_unsorted(TAHdr *thdrP) {
    thdrP->flags &= ~TAHDR_F_SORTED;
}
TA_INLINE void thdr_mark_sorted_ascending(TAHdr *thdrP) {
    thdrP->flags |= TAHDR_F_SORTED | TAHDR_F_ASCENDING;
}
TA_INLINE void thdr_mark_sorted_descending(TAHdr *thdrP) {
    thdrP->flags &= ~TAHDR_F_ASCENDING;
    thdrP->flags |= TAHDR_F_SORTED;
}
TA_INLINE void thdr_copy_sort_status(TAHdr *thdrP, TAHdr *fromP) {
    thdrP->flags &= ~(TAHDR_F_SORTED | TAHDR_F_ASCENDING);
    thdrP->flags |= fromP->flags & (TAHDR_F_SORTED | TAHDR_F_ASCENDING);
}

/*
 * Error and panic routines
 */
const char *ta_type_string(int tatype);
void ta_type_panic(unsigned char tatype);
void ta_shared_panic(const char *where);
void ta_small_panic(TAHdr *thdrP, const char *where);
TCL_RESULT ta_not_tarray_error(Tcl_Interp *);
TCL_RESULT ta_search_op_error(Tcl_Interp *, int op);
TCL_RESULT ta_value_type_error(Tcl_Interp *, Tcl_Obj *objP, int tatype);
TCL_RESULT ta_grid_length_error(Tcl_Interp *);
TCL_RESULT ta_row_width_error(Tcl_Interp *, int rowwidth, int gridwidth);
TCL_RESULT ta_bad_type_error(Tcl_Interp *interp, TAHdr *thdrP);
TCL_RESULT ta_memory_error(Tcl_Interp *, int size);
TCL_RESULT ta_indices_error(Tcl_Interp *interp, Tcl_Obj *objP);
TCL_RESULT ta_index_error(Tcl_Interp *interp, Tcl_Obj *objP);
TCL_RESULT ta_index_range_error(Tcl_Interp *interp, int index);
TCL_RESULT ta_mismatched_types_error(Tcl_Interp *interp);
TCL_RESULT ta_indices_count_error(Tcl_Interp *interp, int nindices, int nvalues);
void thdr_incr_obj_refs(TAHdr *thdrP,int first,int count);
void thdr_decr_obj_refs(TAHdr *thdrP,int first,int count);
void thdr_free(TAHdr *thdrP);
TCL_RESULT tcol_convert(Tcl_Interp *, Tcl_Obj *objP);

TCL_RESULT TGridVerifyType(Tcl_Interp *, Tcl_Obj *gridObj);
Tcl_Obj *TGridNewObj(Tcl_Interp *, int nobjs, Tcl_Obj *const taObjs[]);
Tcl_Obj *TGridClone(Tcl_Interp *, Tcl_Obj *gridObj, int minsize);
TCL_RESULT TGridConvert(Tcl_Interp *, Tcl_Obj *objP);
TCL_RESULT ta_value_from_obj(Tcl_Interp *, Tcl_Obj *objP,
                              unsigned char tatype, TArrayValue *tavP);
void thdr_fill_range(Tcl_Interp *, TAHdr *thdrP,
                    const TArrayValue *tavP, int pos, int count);
void thdr_fill_indices(Tcl_Interp *, TAHdr *thdrP,
                            const TArrayValue *tavP, TAHdr *indicesP);
Tcl_Obj *thdr_index(TAHdr *thdrP, int index);

TCL_RESULT TAHdrSetMultipleFromObjs(Tcl_Interp *,
                                    TAHdr * const thdrs[], int nthdrs,
                                    Tcl_Obj *tuples, int first);
TCL_RESULT TGridFillFromObjs(Tcl_Interp *, Tcl_Obj *lowObj, Tcl_Obj *highObj,
                             Tcl_Obj *gridObj, Tcl_Obj *rowObj);

Tcl_Obj * tcol_new(TAHdr *thdrP);
TCL_RESULT ta_make_modifiable(Tcl_Interp *interp, Tcl_Obj *taObj, int minsize, int prefsize);

TCL_RESULT thdr_put_objs(struct Tcl_Interp *,TAHdr *thdrP,int first,int nelems,struct Tcl_Obj *const *elems );
TCL_RESULT thdr_place_objs(Tcl_Interp *, TAHdr *thdrP, TAHdr *indicesP,
                              int highest_in_indices,
                              int nvalues, Tcl_Obj * const *valuesP);
int thdr_required_size(unsigned char tatype,int count);
TAHdr *thdr_realloc(Tcl_Interp *, TAHdr *oldP,int new_count);
TAHdr *thdr_alloc(Tcl_Interp *, unsigned char tatype, int count);
TAHdr *thdr_alloc_and_init(Tcl_Interp *,unsigned char tatype,int nelems,struct Tcl_Obj *const *elems ,int init_size);
void TAHdrReverse(TAHdr *tdrhP);
void thdr_copy_reversed(TAHdr *dstP,int dst_first,TAHdr *srcP,int src_first,int count);
void thdr_copy(TAHdr *dstP,int dst_first,TAHdr *srcP,int src_first,int count);
void thdr_delete_range(TAHdr *thdrP, int first, int count);
void thdr_delete_Indices(TAHdr *thdrP, TAHdr *indicesP);
TAHdr *thdr_clone(Tcl_Interp *, TAHdr *srcP, int init_size);
TAHdr *thdr_clone_reversed(Tcl_Interp *, TAHdr *srcP, int init_size);
TCL_RESULT ta_verify_value_objs(Tcl_Interp *intepr, int tatype,
                             int nelems, Tcl_Obj * const elems[]);
TCL_RESULT thdr_verify_indices(Tcl_Interp *interp, TAHdr *thdrP, TAHdr *indicesP, int *new_sizeP);
int tcol_to_indices(struct Tcl_Interp *, struct Tcl_Obj *objP,
                           int want_sorted, TAHdr **thdrPP, int *indexP);
#define TA_INDEX_TYPE_ERROR 0
#define TA_INDEX_TYPE_INT   1
#define TA_INDEX_TYPE_TAHDR 2

TAHdr *thdr_range(Tcl_Interp *interp, TAHdr *srcP, int low, int count);
Tcl_Obj *ta_range(Tcl_Interp *interp, Tcl_Obj *srcObj, int low, int count,
                     int fmt);
TCL_RESULT tcol_delete(Tcl_Interp *interp, Tcl_Obj *taObj,
                        Tcl_Obj *indexA, Tcl_Obj *indexB);
TCL_RESULT tcol_fill_obj(Tcl_Interp *interp, Tcl_Obj *taObj, Tcl_Obj *valueObj,
                      Tcl_Obj *indexA, Tcl_Obj *indexB);
Tcl_Obj *tcol_get(struct Tcl_Interp *, TAHdr *srcP, TAHdr *indicesP, int fmt);
int TArrayNumSetBits(TAHdr *thdrP);
TCL_RESULT tcol_copy_thdr(Tcl_Interp *, Tcl_Obj *taObj, TAHdr *srcP, Tcl_Obj *firstObj);
TCL_RESULT tcol_put_objs(Tcl_Interp *, Tcl_Obj *taObj,
                             Tcl_Obj *valueListObj, Tcl_Obj *firstObj);
TCL_RESULT tcol_place_objs(Tcl_Interp *interp, Tcl_Obj *taObj,
                               Tcl_Obj *valueListObj, Tcl_Obj *indicesObj);
TCL_RESULT ta_convert_index(Tcl_Interp *, Tcl_Obj *objP, int *indexP,
                      int end_value, int low, int high);
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *, const TAHdr *thdrP, Tcl_Obj *lowObj, Tcl_Obj *highObj, int *lowP, int *countP);
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
TCL_RESULT tcol_search_cmd(ClientData clientdata, Tcl_Interp *interp,
                                      int objc, Tcl_Obj *const objv[]);

#endif
