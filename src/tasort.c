#include "tcl.h"
#include "tarray.h"

/*
 * Comparison functions for qsort. For a stable sort, if two items are
 * equal, we compare the addresses. Note expression is a bit more complicated
 * because we cannot just return result of a subtraction when the type
 * is wider than an int.
 */
#define RETCMP(a_, b_, t_)                              \
    do {                                                \
        if (*(t_*)(a_) > *(t_*)(b_))                    \
            return 1;                                   \
        else if (*(t_*)(a_) < *(t_*)(b_))               \
            return -1;                                  \
        else {                                          \
            if ((char *)(a_) == (char *)(b_))           \
                return 0;                               \
            else if ((char *)(a_) > (char *)(b_))       \
                return 1;                               \
            else                                        \
                return -1;                              \
        }                                               \
    } while (0)

/* Compare in reverse (for decreasing order). Note we cannot just
   reverse arguments to RETCMP since we are also comparing actual
   addresses and the sense for those should be fixed.
*/
#define RETCMPREV(a_, b_, t_)                           \
    do {                                                \
        if (*(t_*)(a_) > *(t_*)(b_))                    \
            return -1;                                  \
        else if (*(t_*)(a_) < *(t_*)(b_))               \
            return 1;                                   \
        else {                                          \
            if ((char *)(a_) == (char *)(b_))           \
                return 0;                               \
            else if ((char *)(a_) > (char *)(b_))       \
                return 1;                               \
            else                                        \
                return -1;                              \
        }                                               \
    } while (0)

int intcmp(const void *a, const void *b) { RETCMP(a,b,int); }
int intcmprev(const void *a, const void *b) { RETCMPREV(a,b,int); }
int uintcmp(const void *a, const void *b) { RETCMP(a,b,unsigned int); }
int uintcmprev(const void *a, const void *b) { RETCMPREV(a,b,unsigned int); }
int widecmp(const void *a, const void *b) { RETCMP(a,b,Tcl_WideInt); }
int widecmprev(const void *a, const void *b) { RETCMPREV(a,b,Tcl_WideInt); }
int doublecmp(const void *a, const void *b) { RETCMP(a,b,double); }
int doublecmprev(const void *a, const void *b) { RETCMPREV(a,b,double); }
int bytecmp(const void *a, const void *b) { RETCMP(a,b,unsigned char); }
int bytecmprev(const void *a, const void *b) { RETCMPREV(a,b,unsigned char); }
int tclobjcmp(const void *a, const void *b)
{
    int n;
    n = ta_obj_compare(*(Tcl_Obj **)a, *(Tcl_Obj **)b, 0);
    if (n)
        return n;
    else
        return a > b ? 1 : (a < b ? -1 : 0);
}
int tclobjcmprev(const void *a, const void *b)
{
    int n;
    n = ta_obj_compare(*(Tcl_Obj **)b, *(Tcl_Obj **)a, 0);
    if (n)
        return n;
    else
        return a > b ? 1 : (a < b ? -1 : 0);
}
int tclobjcmpnocase(const void *a, const void *b)
{
    int n;
    n = ta_obj_compare(*(Tcl_Obj **)a, *(Tcl_Obj **)b, 1);
    if (n)
        return n;
    else
        return a > b ? 1 : (a < b ? -1 : 0);
}
int tclobjcmpnocaserev(const void *a, const void *b)
{
    int n;
    n = ta_obj_compare(*(Tcl_Obj **)b, *(Tcl_Obj **)a, 1);
    if (n)
        return n;
    else
        return a > b ? 1 : (a < b ? -1 : 0);
}

/*
 * Comparison functions for tarray_qsort_r. 
 * The passed values are integer indexes into a TArrayHdr
 * For a stable sort, if two items are
 * equal, we compare the indexes.
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
            return *(int *)(ai_) - *(int *)(bi_);       \
    } while (0)


/* Compare in reverse (for decreasing order). Note we cannot just
   reverse arguments to RETCMPINDEXED since we are also comparing actual
   addresses and the sense for those should be fixed.
*/
#define RETCMPINDEXEDREV(ai_, bi_, t_, v_)              \
    do {                                                \
        t_ a_ = *(*(int*)(ai_) + (t_ *)v_); \
        t_ b_ = *(*(int*)(bi_) + (t_ *)v_);             \
        if (a_ < b_)                                    \
            return 1;                                   \
        else if (a_ > b_)                               \
            return -1;                                  \
        else                                            \
            return *(int *)(ai_) - *(int *)(bi_);       \
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
int tclobjcmpindexed(void *ctx, const void *ai, const void *bi)
{
    int n;
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    n = ta_obj_compare(a, b, 0);
    if (n)
        return n;
    else
        return *(int *)ai - *(int *)bi;
}
int tclobjcmpindexedrev(void *ctx, const void *ai, const void *bi)
{
    int n;
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    n = ta_obj_compare(b, a, 0);
    if (n)
        return n;
    else
        return *(int *)ai - *(int *)bi;
}
int tclobjcmpnocaseindexed(void *ctx, const void *ai, const void *bi)
{
    int n;
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    n = ta_obj_compare(a, b, 1);
    if (n)
        return n;
    else
        return *(int *)ai - *(int *)bi;
}
int tclobjcmpnocaseindexedrev(void *ctx, const void *ai, const void *bi)
{
    int n;
    Tcl_Obj *a = *(*(int *)ai + (Tcl_Obj **)ctx);
    Tcl_Obj *b = *(*(int *)bi + (Tcl_Obj **)ctx);
    n = ta_obj_compare(b, a, 1);
    if (n)
        return n;
    else
        return *(int *)ai - *(int *)bi;
}

int booleancmpindexed(void *ctx, const void *ai, const void *bi)
{
    unsigned char uca, ucb;

    uca = ba_get((ba_t *)ctx, *(int *)ai);
    ucb = ba_get((ba_t *)ctx, *(int *)bi);
    if (uca == ucb) {
        /* Both bits set or unset, use index position to differentiate */
        return *(int *)ai - *(int *)bi;
    } else if (uca)
        return 1;
    else
        return -1;
}

int booleancmpindexedrev(void *ctx, const void *ai, const void *bi)
{
    unsigned char uca, ucb;

    uca = ba_get((ba_t *)ctx, *(int *)ai);
    ucb = ba_get((ba_t *)ctx, *(int *)bi);

    if (uca == ucb) {
        /* Both bits set or unset, use index position to differentiate */
        return *(int *)ai - *(int *)bi;
    } else if (uca)
        return -1;
    else
        return 1;
}



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

TCL_RESULT tcol_parse_sort_options(Tcl_Interp *ip,
                                   int objc, Tcl_Obj *const objv[],
                                   int *pflags)
{
    static const char *switches[] = {
        "-decreasing", "-increasing", "-indices", "-nocase", NULL
    };
    int i, opt;
    int flags = 0;

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
        case 3: flags |= TA_SORT_NOCASE;
        }
    }
    *pflags = flags;
    return TCL_OK;
}

TCL_RESULT tcol_sort(Tcl_Interp *ip, Tcl_Obj *tcol, int flags)
{
    int i, n;
    thdr_t *psrc;
    thdr_t *psorted;
    int (__cdecl *cmpfn)(const void*, const void*);
    int (__cdecl *cmpindexedfn)(void *, const void*, const void*);
    int status;
    int decreasing = flags & TA_SORT_DECREASING;
    int return_indices = flags & TA_SORT_INDICES;
    int nocase = flags & TA_SORT_NOCASE;
    int orig_sort_state;

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

            switch (psrc->type) {
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
                ta_type_panic(psorted->type);
            }
            /* Note list of indices is NOT sorted, do not mark it as such ! */
            tarray_qsort_r(THDRELEMPTR(psorted, int, 0), psorted->used,
                           sizeof(int), THDRELEMPTR(psrc, unsigned char, 0),
                           cmpindexedfn);
        }
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
    if (psorted->type == TA_BOOLEAN) {
        ba_t *baP = THDRELEMPTR(psorted, ba_t, 0);
        n = ba_count_ones(baP, 0, psorted->used); /* Number of 1's set */
        if (decreasing) {
            ba_fill(baP, 0, n, 1);
            ba_fill(baP, n, psorted->used - n, 0);
        } else {
            ba_fill(baP, 0, psorted->used - n, 0);
            ba_fill(baP, psorted->used - n, n, 1);
        }
    } else {
        switch (psorted->type) {
        case TA_BYTE:
            cmpfn = decreasing ? bytecmprev : bytecmp;
            break;
        case TA_UINT:
            cmpfn = decreasing ? uintcmprev : uintcmp;
            break;
        case TA_INT:
            cmpfn = decreasing ? intcmprev : intcmp;
            break;
        case TA_WIDE:
            cmpfn = decreasing ? widecmprev : widecmp;
            break;
        case TA_DOUBLE:
            cmpfn = decreasing ? doublecmprev : doublecmp;
            break;
        case TA_ANY:
            if (nocase)
                cmpfn = decreasing ? tclobjcmpnocaserev : tclobjcmpnocase;
            else
                cmpfn = decreasing ? tclobjcmprev : tclobjcmp;
            break;
        default:
            ta_type_panic(psorted->type);
        }
        qsort(THDRELEMPTR(psorted, unsigned char, 0), psorted->used,
              psorted->elem_bits / CHAR_BIT, cmpfn);
    }

    if (decreasing) {
        if (nocase)
            psorted->sort_order = THDR_SORTED_DESCENDING_NOCASE;
        else
            psorted->sort_order = THDR_SORTED_DESCENDING;
    } else {
        if (nocase)
            psorted->sort_order = THDR_SORTED_ASCENDING_NOCASE;
        else
            psorted->sort_order = THDR_SORTED_ASCENDING;
    }

    return TCL_OK;
}

