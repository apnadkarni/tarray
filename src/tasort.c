#include "tcl.h"
#include "tarray.h"
#ifdef TA_SORT_MT
# include "dispatch.h"
/*
 * Threshold for when sorts are multithreaded. Based on some preliminary
 * tests. Can be changed at runtime (tarray::unsupported::set_sort_mt_threshold)
 */
int ta_sort_mt_threshold = 5000;
#endif


/* Comparison functions for sorting */
#define RETCMP(a_, b_, t_)                      \
    return ((*(t_ *)(a_)) > (*(t_ *)(b_)) ? 1 : ( (*(t_ *)(a_)) == (*(t_ *)(b_)) ? 0 : -1))
int intcmp(const void *a, const void *b) { return *(int*)a-*(int*)b; }
int intcmprev(const void *a, const void *b) { return *(int*)b-*(int*)a; }
/* Note for doubles, wides, unsigned int etc, (*a-*b) won't do */
int bytecmp(const void *a, const void *b) { RETCMP(a, b, unsigned char); }
int bytecmprev(const void *a, const void *b) { RETCMP(b, a, unsigned char); }
int uintcmp(const void *a, const void *b) { RETCMP(a, b, unsigned int); }
int uintcmprev(const void *a, const void *b) { RETCMP(b, a, unsigned int); }
int widecmp(const void *a, const void *b) { RETCMP(a, b, Tcl_WideInt); }
int widecmprev(const void *a, const void *b) { RETCMP(b, a, Tcl_WideInt); }
int doublecmp(const void *a, const void *b) { RETCMP(a, b, double); }
int doublecmprev(const void *a, const void *b) { RETCMP(b, a, double); }
int tclobjcmp(const void *a, const void *b) {
    return ta_obj_compare(*(Tcl_Obj **)a, *(Tcl_Obj **)b, 0);
}
int tclobjcmprev(const void *a, const void *b) {
    return ta_obj_compare(*(Tcl_Obj **)b, *(Tcl_Obj **)a, 0);
}
int tclobjcmpnocase(const void *a, const void *b) {
    return ta_obj_compare(*(Tcl_Obj **)a, *(Tcl_Obj **)b, 1);
}
int tclobjcmpnocaserev(const void *a, const void *b) {
    return  ta_obj_compare(*(Tcl_Obj **)b, *(Tcl_Obj **)a, 1);
}

/*
 * Comparison functions for sorting when the passed values are integer
 * indexes into a TArrayHdr.
 * ai_ and bi_ are pointers to int indexes into the TArrayHdr
 * pointed to by v_ which is of type t_
 */
#define RETCMPINDEXED(ai_, bi_, t_, v_)                 \
    do {                                                \
        t_ a_ = *((*(int*)(ai_)) + (t_ *)v_);           \
        t_ b_ = *((*(int*)(bi_)) + (t_ *)v_);           \
        if (a_ < b_)                                    \
            return -1;                                  \
        else if (a_ > b_)                               \
            return 1;                                   \
        else                                            \
            return 0; \
    } while (0)

#define RETCMPINDEXEDREV(ai_, bi_, t_, v_)                 \
    do {                                                \
        t_ a_ = *((*(int*)(ai_)) + (t_ *)v_);           \
        t_ b_ = *((*(int*)(bi_)) + (t_ *)v_);           \
        if (a_ < b_)                                    \
            return 1;                                  \
        else if (a_ > b_)                               \
            return -1;                                   \
        else                                            \
            return 0; \
    } while (0)


int intcmpindexed(void *ctx, const void *a, const void *b) { RETCMPINDEXED(a,b,int, ctx); }
int intcmpindexedrev(void *ctx, const void *a, const void *b) { RETCMPINDEXEDREV(a,b,int, ctx); }
int uintcmpindexed(void *ctx, const void *a, const void *b) { RETCMPINDEXED(a,b,unsigned int, ctx); }
int uintcmpindexedrev(void *ctx, const void *a, const void *b) { RETCMPINDEXEDREV(a,b,unsigned int, ctx); }
int widecmpindexed(void *ctx, const void *a, const void *b) { RETCMPINDEXED(a,b,Tcl_WideInt, ctx); }
int widecmpindexedrev(void *ctx, const void *a, const void *b) { RETCMPINDEXEDREV(a,b,Tcl_WideInt, ctx); }
int doublecmpindexed(void *ctx, const void *a, const void *b) { RETCMPINDEXED(a,b,double, ctx); }
int doublecmpindexedrev(void *ctx, const void *a, const void *b) { RETCMPINDEXEDREV(a,b,double, ctx); }
int bytecmpindexed(void *ctx, const void *a, const void *b) { RETCMPINDEXED(a,b,unsigned char, ctx); }
int bytecmpindexedrev(void *ctx, const void *a, const void *b) { RETCMPINDEXEDREV(a,b,unsigned char, ctx); }
int tclobjcmpindexed(void *ctx, const void *ai, const void *bi) {
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    return ta_obj_compare(a, b, 0);
}
int tclobjcmpindexedrev(void *ctx, const void *ai, const void *bi) {
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    return ta_obj_compare(b, a, 0);
}
int tclobjcmpnocaseindexed(void *ctx, const void *ai, const void *bi) {
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    return ta_obj_compare(a, b, 1);
}
int tclobjcmpnocaseindexedrev(void *ctx, const void *ai, const void *bi) {
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    return ta_obj_compare(b, a, 1);
}
int booleancmpindexed(void *ctx, const void *ai, const void *bi) {
    unsigned char uca, ucb;
    uca = ba_get((ba_t *)ctx, *(int *)ai);
    ucb = ba_get((ba_t *)ctx, *(int *)bi);
    return uca == ucb ? 0 : (uca ? 1 : -1);
}
int booleancmpindexedrev(void *ctx, const void *ai, const void *bi) {
    unsigned char uca, ucb;
    uca = ba_get((ba_t *)ctx, *(int *)ai);
    ucb = ba_get((ba_t *)ctx, *(int *)bi);
    return uca == ucb ? 0 : (uca ? -1 : 1);
}

#ifdef USE_QSORT

/*
 * qsort code - from BSD via speedtables
 */

/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)qsort.c	8.1 (Berkeley) 6/4/93";
#endif /* LIBC_SCCS and not lint */

#if !defined(_MSC_VER)
// #include <sys/cdefs.h>
#endif

#include <stdlib.h>

#define I_AM_QSORT_R

#ifdef _MSC_VER
// MS VC 6 does not like "inline" in C files
#define inline __inline
#endif

#ifdef I_AM_QSORT_R
typedef int		 cmp_t(void *, const void *, const void *);
#else
typedef int		 cmp_t(const void *, const void *);
#endif
static inline char	*med3(char *, char *, char *, cmp_t *, void *);
static inline void	 swapfunc(char *, char *, int, int);

#ifndef _MSC_VER
#define min(a, b)	(((a) < (b)) ? (a) : (b))
#endif

/*
 * Qsort routine from Bentley & McIlroy's "Engineering a Sort Function".
 */
#define swapcode(TYPE, parmi, parmj, n) { 		\
	long i = (n) / sizeof (TYPE); 			\
	TYPE *pi = (TYPE *) (parmi); 		\
	TYPE *pj = (TYPE *) (parmj); 		\
	do { 						\
		TYPE	t = *pi;		\
		*pi++ = *pj;				\
		*pj++ = t;				\
        } while (--i > 0);				\
}

#define SWAPINIT(a, es) swaptype = ((char *)a - (char *)0) % sizeof(long) || \
	es % sizeof(long) ? 2 : es == sizeof(long)? 0 : 1;

static inline void
swapfunc(a, b, n, swaptype)
	char *a, *b;
	int n, swaptype;
{
	if(swaptype <= 1)
		swapcode(long, a, b, n)
	else
		swapcode(char, a, b, n)
}

#define swap(a, b)					\
	if (swaptype == 0) {				\
		long t = *(long *)(a);			\
		*(long *)(a) = *(long *)(b);		\
		*(long *)(b) = t;			\
	} else						\
		swapfunc(a, b, es, swaptype)

#define vecswap(a, b, n) 	if ((n) > 0) swapfunc(a, b, n, swaptype)

#ifdef I_AM_QSORT_R
#define	CMP(t, x, y) (cmp((t), (x), (y)))
#else
#define	CMP(t, x, y) (cmp((x), (y)))
#endif

static inline char *
med3(char *a, char *b, char *c, cmp_t *cmp, void *thunk
#ifndef I_AM_QSORT_R
__unused
#endif
)
{
	return CMP(thunk, a, b) < 0 ?
	       (CMP(thunk, b, c) < 0 ? b : (CMP(thunk, a, c) < 0 ? c : a ))
              :(CMP(thunk, b, c) > 0 ? b : (CMP(thunk, a, c) < 0 ? a : c ));
}

#ifdef I_AM_QSORT_R
void
tarray_qsort_r(void *a, size_t n, size_t es, void *thunk, cmp_t *cmp)
#else
#define thunk NULL
void
qsort(void *a, size_t n, size_t es, cmp_t *cmp)
#endif
{
	char *pa, *pb, *pc, *pd, *pl, *pm, *pn;
	size_t d, r;
	int cmp_result;
	int swaptype, swap_cnt;

loop:	SWAPINIT(a, es);
	swap_cnt = 0;
	if (n < 7) {
		for (pm = (char *)a + es; pm < (char *)a + n * es; pm += es)
			for (pl = pm; 
			     pl > (char *)a && CMP(thunk, pl - es, pl) > 0;
			     pl -= es)
				swap(pl, pl - es);
		return;
	}
	pm = (char *)a + (n / 2) * es;
	if (n > 7) {
		pl = a;
		pn = (char *)a + (n - 1) * es;
		if (n > 40) {
			d = (n / 8) * es;
			pl = med3(pl, pl + d, pl + 2 * d, cmp, thunk);
			pm = med3(pm - d, pm, pm + d, cmp, thunk);
			pn = med3(pn - 2 * d, pn - d, pn, cmp, thunk);
		}
		pm = med3(pl, pm, pn, cmp, thunk);
	}
	swap(a, pm);
	pa = pb = (char *)a + es;

	pc = pd = (char *)a + (n - 1) * es;
	for (;;) {
		while (pb <= pc && (cmp_result = CMP(thunk, pb, a)) <= 0) {
			if (cmp_result == 0) {
				swap_cnt = 1;
				swap(pa, pb);
				pa += es;
			}
			pb += es;
		}
		while (pb <= pc && (cmp_result = CMP(thunk, pc, a)) >= 0) {
			if (cmp_result == 0) {
				swap_cnt = 1;
				swap(pc, pd);
				pd -= es;
			}
			pc -= es;
		}
		if (pb > pc)
			break;
		swap(pb, pc);
		swap_cnt = 1;
		pb += es;
		pc -= es;
	}
	if (swap_cnt == 0) {  /* Switch to insertion sort */
		for (pm = (char *)a + es; pm < (char *)a + n * es; pm += es)
			for (pl = pm; 
			     pl > (char *)a && CMP(thunk, pl - es, pl) > 0;
			     pl -= es)
				swap(pl, pl - es);
		return;
	}

	pn = (char *)a + n * es;
	r = min(pa - (char *)a, pb - pa);
	vecswap(a, pb - r, r);
	r = min(pd - pc, pn - pd - es);
	vecswap(pb, pn - r, r);
	if ((r = pb - pa) > es)
#ifdef I_AM_QSORT_R
                tarray_qsort_r(a, r / es, es, thunk, cmp);
#else
		qsort(a, r / es, es, cmp);
#endif
	if ((r = pd - pc) > es) {
		/* Iterate rather than recurse to save stack space */
		a = pn - r;
		n = r / es;
		goto loop;
	}
/*		qsort(pn - r, r / es, es, cmp);*/
}

#endif // USE_QSORT

TCL_RESULT tcol_parse_sort_options(Tcl_Interp *ip,
                                   int objc, Tcl_Obj *const objv[],
                                   int *pflags, Tcl_Obj **ptarget)
{
    static const char *switches[] = {
        "-decreasing", "-increasing", "-indices", "-nocase", "-indirect", NULL
    };
    int i, opt;
    int flags = 0;

    *ptarget = NULL;

    /* Note objv[] is entire command line so last objv[] element is array */
    for (i = 1; i < objc-1; ++i) {
	if (Tcl_GetIndexFromObj(ip, objv[i], switches, "option", 0, &opt)
            != TCL_OK) {
            return TCL_ERROR;
	}
        switch (opt) {
        case 0: flags |= TA_SORT_DECREASING; break;
        case 1: flags &= ~TA_SORT_DECREASING; break;
        case 2: flags |= TA_SORT_INDICES; break;
        case 3: flags |= TA_SORT_NOCASE; break;
        case 4:
            if (++i >= objc-1) {
                Tcl_SetResult(ip, "Missing argument to -indirect option.", TCL_STATIC);
                return TCL_ERROR;
            }
            flags |= TA_SORT_INDIRECT;
            *ptarget = objv[i];
            break;
        }
    }
    *pflags = flags;
    return TCL_OK;
}

#ifdef TA_SORT_MT

/* Structure to hold the context for each sorting thread */
struct ta_sort_mt_context {
    void *base;                 /* Base of the sort array */
    int   nelems;               /* Number of elements to sort */
    int   elem_size;            /* Size of each element */
    int (*cmpfn)(const void*, const void*); /* Comparison function */
    int (*cmpindexedfn)(void*, const void*, const void*); /* for indexed compares */
    void *arg;                  /* Passed through for indexed compares */
};

static void ta_sort_mt_worker(struct ta_sort_mt_context *pctx)
{
    timsort(pctx->base, pctx->nelems, pctx->elem_size, pctx->cmpfn);
}

static void ta_sort_mt_worker_r(struct ta_sort_mt_context *pctx)
{
    timsort_r(pctx->base, pctx->nelems, pctx->elem_size, pctx->arg, pctx->cmpindexedfn);
}

#endif

/* Note this routine does not handle booleans (because they are sorted
   differently using just counts) and TA_ANY (because Tcl_Obj comparisons
   are not thread safe
*/
static void thdr_sort_scalars(thdr_t *thdr, int decr, thdr_t *psrc)
{
    int (*cmp)(const void*, const void*);
    int (*cmpind)(void *, const void*, const void*);

    TA_ASSERT(thdr->nrefs < 2);

    if (psrc) {
        /* Sort indices */
        TA_ASSERT(thdr->type == TA_INT);
        switch (psrc->type) {
        case TA_BYTE: cmpind = decr ? bytecmpindexedrev : bytecmpindexed; break;
        case TA_UINT: cmpind = decr ? uintcmpindexedrev : uintcmpindexed; break;
        case TA_INT: cmpind = decr ? intcmpindexedrev : intcmpindexed; break;
        case TA_WIDE: cmpind = decr ? widecmpindexedrev : widecmpindexed; break;
        case TA_DOUBLE: cmpind = decr ? doublecmpindexedrev : doublecmpindexed; break;
        case TA_ANY: /* FALLTHRU */
        case TA_BOOLEAN: /* FALLTHRU */
        default:
            ta_type_panic(thdr->type);
        }
    } else {
        switch (thdr->type) {
        case TA_BYTE: cmp = decr ? bytecmprev : bytecmp; break;
        case TA_UINT: cmp = decr ? uintcmprev : uintcmp; break;
        case TA_INT: cmp = decr ? intcmprev : intcmp; break;
        case TA_WIDE: cmp = decr ? widecmprev : widecmp; break;
        case TA_DOUBLE: cmp = decr ? doublecmprev : doublecmp; break;
        case TA_ANY: /* FALLTHRU */
        case TA_BOOLEAN: /* FALLTHRU */
        default:
            ta_type_panic(thdr->type);
        }
    }


#ifdef TA_SORT_MT
    if (thdr->used > ta_sort_mt_threshold) {
        dispatch_group_t grp;
        dispatch_queue_t q;
        int elem_size;
        int align_size;
        struct ta_sort_mt_context sort_context[2];

        /* We have to make sure that when we are sorting "small elements"
           like bytes, threads do not interfere with each other at the
           boundaries of the array partitions on processors where
           memory access is not atomic. So we divide the array such 
           that the partition falls on a boundary that is a multiple
           of atomic type. We assume (TBD) that void* satisfies this
           We calculate a boundary that is a multiple of both the
           data type and void*.
           We assume base of array is aligned appropriately and divide
           elements in (roughly) half.
           X = elem_size*(psorted->used/2) gives #bytes in first partition
           X/align_size gives number of alignment units in first partition
           (discarding extra into second partition)
           (X/align_size)*(align_size/elem_size) gives number of data
           elements in first partition
           But (align_size/elem_size) is exactly sizeof(void*). So number
           of elements in first partition is (X/align_size)*sizeof(void*)
        */
        elem_size = thdr->elem_bits / CHAR_BIT;
        align_size = elem_size * sizeof(void*); /* Want LCM but this will do */
        sort_context[0].nelems = ((elem_size * thdr->used/2) / align_size)*sizeof(void *);

        sort_context[0].base = THDRELEMPTR(thdr, unsigned char, 0);
        sort_context[0].elem_size = elem_size;
        if (psrc) {
            sort_context[0].cmpfn = NULL;
            sort_context[0].cmpindexedfn = cmpind;
            sort_context[0].arg = THDRELEMPTR(psrc, unsigned char, 0);
        } else {
            sort_context[0].cmpfn = cmp;
            sort_context[0].cmpindexedfn = NULL;
            sort_context[0].arg = NULL;
        }
        sort_context[1].base = elem_size*sort_context[0].nelems + (char *)sort_context[0].base;
        sort_context[1].nelems = thdr->used - sort_context[0].nelems;
        sort_context[1].elem_size = elem_size;
        sort_context[1].cmpfn = sort_context[0].cmpfn;
        sort_context[1].cmpindexedfn = sort_context[0].cmpindexedfn;
        sort_context[1].arg = sort_context[0].arg;
        
        if (sort_context[0].nelems && sort_context[1].nelems) {
            grp = dispatch_group_create();
            TA_ASSERT(grp != NULL);
            q = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
            TA_ASSERT(q != NULL);
            if (psrc) {
                dispatch_group_async_f(grp, q, &sort_context[0],
                                       ta_sort_mt_worker_r);
                timsort_r(sort_context[1].base, sort_context[1].nelems, sort_context[1].elem_size, sort_context[1].arg, cmpind);
            } else {
                dispatch_group_async_f(grp, q, &sort_context[0],
                                       ta_sort_mt_worker);
                timsort(sort_context[1].base, sort_context[1].nelems, elem_size, cmp);
            }
            dispatch_group_wait(grp, DISPATCH_TIME_FOREVER);
            dispatch_release(grp);
        }
    }

#endif /* TA_SORT_MT */
    
    /* Now sort the entire array. TBD - would an in-place merge be faster when
       partially sorted above ? */
    if (psrc) {
        timsort_r(THDRELEMPTR(thdr, unsigned char, 0), thdr->used,
                  thdr->elem_bits / CHAR_BIT,
                  THDRELEMPTR(psrc, unsigned char, 0), cmpind);
    } else {
        timsort(THDRELEMPTR(thdr, unsigned char, 0), thdr->used,
                thdr->elem_bits / CHAR_BIT, cmp);
    }

    /* Note when indirect/indexed sorting, returned indices are NOT sorted! */
    if (psrc == NULL) {
        thdr->sort_order =
            decr ? THDR_SORTED_DESCENDING : THDR_SORTED_ASCENDING;
    }
}



TCL_RESULT tcol_sort(Tcl_Interp *ip, Tcl_Obj *tcol, int flags)
{
    int i, n;
    thdr_t *psrc;
    thdr_t *psorted;
    int (*cmpfn)(const void*, const void*);
    int (*cmpindexedfn)(void *, const void*, const void*);
    int status;
    int decreasing = flags & TA_SORT_DECREASING;
    int return_indices = flags & TA_SORT_INDICES;
    int nocase = flags & TA_SORT_NOCASE;
    int orig_sort_state;

    /* Even if returning indices, they are returned in tcol
     * so we must be able to modify tcol */
    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    psrc = TARRAYHDR(tcol);

    /*
     * If values are already sorted in suitable order, we can make use
     * of it. For TA_ANY, we need to check whether sort order was
     * case sensitive or not
     */
    if (psrc->type != TA_ANY) {
        nocase = 0;             /* Ignored for other types */
        if (psrc->sort_order == THDR_UNSORTED)
            orig_sort_state = 0; /* Can't use sort state */
        else if ((decreasing && psrc->sort_order == THDR_SORTED_DESCENDING) ||
                 ((!decreasing) && psrc->sort_order == THDR_SORTED_ASCENDING))
            orig_sort_state = 1; /* Sort state matches what's specified now */
        else {
            if (return_indices || psrc->type != TA_BOOLEAN)
                orig_sort_state = -1; /* Sorted but in reverse order */
            else
                orig_sort_state = 0; /* For BOOLEAN, faster to explictly sort */
        }
    }
    else {
        switch (psrc->sort_order) {
        case THDR_UNSORTED:
            orig_sort_state = 0;
            break;
        case THDR_SORTED_ASCENDING:
        case THDR_SORTED_DESCENDING:
            if (nocase)
                orig_sort_state = 0; /* Can't use because different case matching */
            else if (psrc->sort_order == THDR_SORTED_DESCENDING && decreasing ||
                     psrc->sort_order == THDR_SORTED_ASCENDING && !decreasing)
                orig_sort_state = 1; /* Same sort order */
            else
                orig_sort_state = -1; /* Reverse order */
            break;
        case THDR_SORTED_ASCENDING_NOCASE:
        case THDR_SORTED_DESCENDING_NOCASE:
            if (!nocase)
                orig_sort_state = 0; /* Can't use because different case matching */
            else if (psrc->sort_order == THDR_SORTED_DESCENDING_NOCASE && decreasing ||
                     psrc->sort_order == THDR_SORTED_ASCENDING_NOCASE && !decreasing)
                orig_sort_state = 1; /* Same sort order */
            else
                orig_sort_state = -1; /* Reverse order */
            break;
        }
    }

    if (return_indices) {
        /* Caller wants indices to be returned */
        int *indexP;
        psorted = thdr_alloc(ip, TA_INT, psrc->used);
        if (psorted == NULL)
            return TCL_ERROR;
        /* Initialize the indexes */
        psorted->used = psrc->used;
        indexP = THDRELEMPTR(psorted, int, 0);

        /* TBD - just reversing the order of a presorted array is no good
           since the sort will not be a stable sort. So for now dump that
           idea. */
        if (orig_sort_state < 0)
            orig_sort_state = 0;

        if (orig_sort_state > 0) {
            /* Input already sorted in right order */
            for (i = 0; i < psorted->used; ++i, ++indexP)
                *indexP = i;
            psorted->sort_order = THDR_SORTED_ASCENDING; /* indices order */
        } else if (orig_sort_state < 0) {
            /* Sorted but in reverse order. */
            for (i = psorted->used; i > 0; ++indexP)
                *indexP = --i;
            psorted->sort_order = THDR_SORTED_DESCENDING; /* indices order */
        } else {
            /* Input is not already sorted */

            /* Init the indices array for sorting */
            for (i = 0; i < psorted->used; ++i, ++indexP)
                *indexP = i;

            if (psrc->type == TA_ANY) {
                if (nocase) 
                    cmpindexedfn = decreasing ? tclobjcmpnocaseindexedrev : tclobjcmpnocaseindexed;
                else
                    cmpindexedfn = decreasing ? tclobjcmpindexedrev : tclobjcmpindexed;

                timsort_r(THDRELEMPTR(psorted, int, 0),
                          psorted->used, sizeof(int),
                          THDRELEMPTR(psrc, unsigned char, 0),
                          cmpindexedfn);

            } else if (psrc->type == TA_BOOLEAN) {
                timsort_r(THDRELEMPTR(psorted, int, 0),
                          psorted->used, sizeof(int),
                          THDRELEMPTR(psrc, unsigned char, 0),
                          decreasing ? booleancmpindexedrev : booleancmpindexed
                    );
            } else {
                thdr_sort_scalars(psorted, decreasing, psrc);
            }

            /* Note list of indices is NOT sorted, do not mark it as such ! */
        }

        // Need to create a new Tcl_Obj
        ta_replace_intrep(tcol, psorted);
        return TCL_OK;
    } 
        
    /* We want actual sorted data, not indices */
    if (orig_sort_state > 0)
        return TCL_OK;        /* Already sorted in desired order */
    
    if (orig_sort_state < 0) {
        /*
         * Need to reverse the order. If thdr is also unshared, we can do this
         * in place else need to allocate a new object and array
         */
        if (thdr_shared(psrc)) {
            /* Cannot modify in place. Need to dup it */
            psrc = thdr_clone_reversed(ip, psrc, 0);
            if (psrc == NULL)
                return TCL_ERROR;
            ta_replace_intrep(tcol, psrc);
        } else {
            /* Reverse in place  */
            thdr_reverse(psrc);
            Tcl_InvalidateStringRep(tcol);
        }
        return TCL_OK;
    }

    /* Data not sorted so we have to do it */

    /*
     * We want sorted contents, not indices. If object is not shared,
     * we can sort in place, else need to create a new object.
     */
    /* TBD - Why not using ta_make_modifiable here ? */
    if (thdr_shared(psrc)) {
        /* Cannot modify in place. Need to dup it */
        psorted = thdr_clone(ip, psrc, 0);
        if (psorted == NULL)
            return TCL_ERROR;
        ta_replace_intrep(tcol, psorted);
    } else {
        psorted = psrc;
        Tcl_InvalidateStringRep(tcol);
    }

    /*
     * Return sorted contents. Boolean type we treat separately
     * since we just need to count how many 1's and 0's.
     */
    if (psorted->type == TA_ANY) {
        if (nocase)
            cmpfn = decreasing ? tclobjcmpnocaserev : tclobjcmpnocase;
        else
            cmpfn = decreasing ? tclobjcmprev : tclobjcmp;

        timsort(THDRELEMPTR(psorted, unsigned char, 0), psorted->used,
                psorted->elem_bits / CHAR_BIT, cmpfn);

        if (decreasing)
            psorted->sort_order =
                nocase ? THDR_SORTED_DESCENDING_NOCASE : THDR_SORTED_DESCENDING;
        else
            psorted->sort_order =
                nocase ? THDR_SORTED_ASCENDING_NOCASE : THDR_SORTED_ASCENDING;

    } else if (psorted->type == TA_BOOLEAN) {
        ba_t *baP = THDRELEMPTR(psorted, ba_t, 0);
        n = ba_count_ones(baP, 0, psorted->used); /* Number of 1's set */
        if (decreasing) {
            ba_fill(baP, 0, n, 1);
            ba_fill(baP, n, psorted->used - n, 0);
        } else {
            ba_fill(baP, 0, psorted->used - n, 0);
            ba_fill(baP, psorted->used - n, n, 1);
        }

        psorted->sort_order =
            decreasing ? THDR_SORTED_DESCENDING : THDR_SORTED_ASCENDING;
    } else {
        /* Something other than TA_ANY and TA_BOOLEAN */
        thdr_sort_scalars(psorted, decreasing, NULL);
    }

    return TCL_OK;
}

TCL_RESULT tcol_sort_indirect(Tcl_Interp *ip, Tcl_Obj *oindices, Tcl_Obj *otarget, int flags)
{
    int *pindex, *pend, n, status;
    thdr_t *pindices, *ptarget;
    int (*cmpindexedfn)(void *, const void*, const void*);
    int decreasing = flags & TA_SORT_DECREASING;
    int nocase = flags & TA_SORT_NOCASE;

    TA_ASSERT(! Tcl_IsShared(oindices));

    if ((status = tcol_convert(ip, oindices)) != TCL_OK ||
        (status = tcol_convert(ip, otarget)) != TCL_OK)
        return status;

    pindices = TARRAYHDR(oindices);
    if (pindices->type != TA_INT)
        return ta_bad_type_error(ip, pindices);

    /* Validate indices for bounds */
    n = tcol_occupancy(otarget);
    pindex = THDRELEMPTR(pindices, int, 0);
    pend = pindex + pindices->used;
    while (pindex < pend) {
        if (*pindex >= n || *pindex < 0)
            return ta_index_range_error(ip, *pindex);
        ++pindex;
    }
    
    ptarget = TARRAYHDR(otarget);

    switch (ptarget->type) {
    case TA_BYTE:
        cmpindexedfn = decreasing ? bytecmpindexedrev : bytecmpindexed;
        break;
    case TA_BOOLEAN:
        cmpindexedfn = decreasing ? booleancmpindexedrev : booleancmpindexed;
        break;
    case TA_UINT:
        cmpindexedfn = decreasing ? uintcmpindexedrev : uintcmpindexed;
        break;
    case TA_INT:
        cmpindexedfn = decreasing ? intcmpindexedrev : intcmpindexed;
        break;
    case TA_WIDE:
        cmpindexedfn = decreasing ? widecmpindexedrev : widecmpindexed;
        break;
    case TA_DOUBLE:
        cmpindexedfn = decreasing ? doublecmpindexedrev : doublecmpindexed;
        break;
    case TA_ANY:
        if (nocase) 
            cmpindexedfn = decreasing ? tclobjcmpnocaseindexedrev : tclobjcmpnocaseindexed;
        else
            cmpindexedfn = decreasing ? tclobjcmpindexedrev : tclobjcmpindexed;
                break;
    default:
        ta_type_panic(ptarget->type);
    }

    /* Note list of indices is NOT sorted, do not mark it as such ! */
#ifdef USE_QSORT
    tarray_qsort_r(THDRELEMPTR(pindices, int, 0), pindices->used,
                   sizeof(int), THDRELEMPTR(ptarget, unsigned char, 0),
                   cmpindexedfn);
#else
    timsort_r(THDRELEMPTR(pindices, int, 0),
              pindices->used, sizeof(int),
              THDRELEMPTR(ptarget, unsigned char, 0),
              cmpindexedfn);
#endif
    return TCL_OK;
}
