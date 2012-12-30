#ifndef TA_H
#define TA_H

#include <limits.h>
#include <stdlib.h>

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

/* Must match gTArrayTypeTokens definition in tarray.c ! */
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

typedef Tcl_Obj *TArrayObjPtr;

extern const char *gTArrayTypeTokens[];

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

extern struct Tcl_ObjType gTArrayType;
extern struct Tcl_ObjType gTGridType;

#define TAHDR_INCRREF(thdr_) do { (thdr_)->nrefs++; } while (0)
#define TAHDR_DECRREF(thdr_)                                            \
    do {                                                                \
        TAHdr *h_ = (thdr_);  /* Temp in case parameter has sideeffects*/ \
        if (--(h_)->nrefs <= 0)                                         \
            TAHdrFree(h_);                                          \
    } while (0)

/* Sets a Tcl_Obj's internal rep pointer. Assumes the Tcl_Obj int rep is
   invalid / uninitialized */
#define TA_OBJ_SETREP(obj_, thdr_)                                     \
    do {                                                                \
        TAHdr *h_ = (thdr_);  /* Temp in case parameter has sideeffects*/ \
        Tcl_Obj *o_ = (obj_);  /* Temp in case parameter has sideeffects*/ \
        TAHDR_INCRREF(h_); \
        TARRAYHDR(o_) = h_; \
        (o_)->typePtr = &gTArrayType; \
    } while (0)

/* ALLOCATE_ARRAY call should panic on failure to allocate */
#define TA_ALLOCMEM ckalloc
#define TA_FREEMEM(p_) if (p_) ckfree((char *)p_)
#define TA_REALLOCMEM ckrealloc
#define TA_ATTEMPTALLOCMEM attemptckalloc
#define TA_ATTEMPTREALLOCMEM attemptckrealloc

/*
 * Error and panic routines
 */
const char *TArrayTypeString(int tatype);
void TArrayTypePanic(unsigned char tatype);
void TArraySharedPanic(const char *where);
void TArrayTooSmallPanic(TAHdr *thdrP, const char *where);
TCL_RESULT TArrayBadArgError(Tcl_Interp *, const char *optname);
TCL_RESULT TArrayNotTArrayError(Tcl_Interp *);
TCL_RESULT TArrayBadSearchOpError(Tcl_Interp *, int op);
TCL_RESULT TArrayValueTypeError(Tcl_Interp *, Tcl_Obj *objP, int tatype);
TCL_RESULT TArrayGridLengthError(Tcl_Interp *);
TCL_RESULT TArrayRowWidthError(Tcl_Interp *, int rowwidth, int gridwidth);
TCL_RESULT TArrayBadTypeError(Tcl_Interp *interp, TAHdr *thdrP);
TCL_RESULT TArrayNoMemError(Tcl_Interp *, int size);

void TArrayIncrObjRefs(TAHdr *thdrP,int first,int count);
void TArrayDecrObjRefs(TAHdr *thdrP,int first,int count);
void TAHdrFree(TAHdr *thdrP);
TCL_RESULT TArrayConvert(Tcl_Interp *, Tcl_Obj *objP);

TCL_RESULT TGridVerifyType(Tcl_Interp *, Tcl_Obj *gridObj);
Tcl_Obj *TGridNewObj(Tcl_Interp *, int nobjs, Tcl_Obj *const taObjs[]);
Tcl_Obj *TGridClone(Tcl_Interp *, Tcl_Obj *gridObj, int minsize);
TCL_RESULT TGridConvert(Tcl_Interp *, Tcl_Obj *objP);
TCL_RESULT TArrayValueFromObj(Tcl_Interp *, Tcl_Obj *objP,
                              unsigned char tatype, TArrayValue *tavP);
void TAHdrFill(Tcl_Interp *, TAHdr *thdrP,
                                   const TArrayValue *tavP, int pos, int count);
TCL_RESULT TAHdrSetMultipleFromObjs(Tcl_Interp *,
                                    TAHdr * const thdrs[], int nthdrs,
                                    Tcl_Obj *tuples, int first);
TCL_RESULT TArrayFillFromObj(Tcl_Interp *, Tcl_Obj *lowObj, Tcl_Obj *highObj,
                             Tcl_Obj *taObj, Tcl_Obj *valueObj);
TCL_RESULT TGridFillFromObjs(Tcl_Interp *, Tcl_Obj *lowObj, Tcl_Obj *highObj,
                             Tcl_Obj *gridObj, Tcl_Obj *rowObj);

Tcl_Obj * TArrayNewObj(TAHdr *thdrP);
TCL_RESULT TArrayMakeModifiable(Tcl_Interp *interp, Tcl_Obj *taObj, int minsize, int prefsize);

TCL_RESULT TAHdrSetFromObjs(struct Tcl_Interp *,TAHdr *thdrP,int first,int nelems,struct Tcl_Obj *const *elems );
int TArrayCalcSize(unsigned char tatype,int count);
TAHdr *TArrayRealloc(Tcl_Interp *, TAHdr *oldP,int new_count);
TAHdr *TArrayAlloc(Tcl_Interp *, unsigned char tatype, int count);
TAHdr *TArrayAllocAndInit(Tcl_Interp *,unsigned char tatype,int nelems,struct Tcl_Obj *const *elems ,int init_size);
void TAHdrCopy(TAHdr *dstP,int dst_first,TAHdr *srcP,int src_first,int count);
void TAHdrDelete(TAHdr *thdrP, int first, int count);
TAHdr *TAHdrClone(Tcl_Interp *, TAHdr *srcP, int init_size);
struct Tcl_Obj *TArrayIndex(struct Tcl_Interp *,TAHdr *thdrP, Tcl_Obj *index);
TAHdr *TArrayConvertToIndices(struct Tcl_Interp *, struct Tcl_Obj *objP);
TAHdr *TArrayGetValues(struct Tcl_Interp *, TAHdr *srcP, TAHdr *indicesP);
int TArrayNumSetBits(TAHdr *thdrP);
TCL_RESULT TArraySetRange(Tcl_Interp *, TAHdr *dstP, int dst_first, int count, Tcl_Obj *objP);
TCL_RESULT IndexToInt(Tcl_Interp *, Tcl_Obj *objP, int *indexP,
                      int end_value, int low, int high);
TCL_RESULT RationalizeRangeIndices(Tcl_Interp *, TAHdr *thdrP, Tcl_Obj *lowObj, Tcl_Obj *highObj, int *lowP, int *countP);
TCL_RESULT TGridSetFromObjs(Tcl_Interp *, Tcl_Obj *lowObj, Tcl_Obj *gridObj,
    Tcl_Obj *valueObjs, /* Each element is a list (tuple value) */
    int flags);

/*
 * Search and sort routines
 */
int TArrayCompareObjs(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase);

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
TCL_RESULT TArray_SearchObjCmd(ClientData clientdata, Tcl_Interp *interp,
                                      int objc, Tcl_Obj *const objv[]);

#endif
