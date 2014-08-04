/*
 * Multi threading support for tarray
 *
 * NOTES:
 *  - not intended to be a general purpose library, geared towards tarray needs
 *  - On Windows we use native calls. Elsewhere, the libdispatch library is
 *    required.
 */

#include "tarray.h"

#ifdef _WIN32

struct ta_mt_workitem_s {
    struct ta_mt_group_s *owner; /* Containing work item group */
    ta_mt_function_t workfn;    /* Function to dispatch */
    void   *workarg;            /* Argument to pass to workfn */
    HANDLE sig;               /* Used to signal when work item is done */
};
    
#define TA_MT_MAX_GROUP_SIZE 16
struct ta_mt_group_s {
    LONG nrefs;
    unsigned int nworkitems;
    struct ta_mt_workitem_s workitems[TA_MT_MAX_GROUP_SIZE];
};

ta_mt_group_t ta_mt_group_create() 
{
    ta_mt_group_t grp;

    grp = (ta_mt_group_t) TA_ATTEMPTALLOCMEM(sizeof(*grp));
    if (grp) {
        grp->nrefs = 1;
        grp->nworkitems = 0;
    }
    return grp;
}

static DWORD WINAPI ta_mt_worker(struct ta_mt_workitem_s *pitem)
{
    pitem->workfn(pitem->workarg);
    SetEvent(pitem->sig);
    ta_mt_group_release(pitem->owner); /* retain() when item was queued */
    return 0;
}

int ta_mt_group_async_f(ta_mt_group_t grp, void *ctx, ta_mt_function_t fn) {
    struct ta_mt_workitem_s *pitem;

    if (grp->nworkitems >= ARRAYSIZE(grp->workitems))
        return -1;

    pitem = &grp->workitems[grp->nworkitems];
    pitem->owner = grp;
    pitem->workfn  = fn;
    pitem->workarg = ctx;
    pitem->sig = CreateEvent(NULL, TRUE, FALSE, NULL); /* Manual reset event */
    if (pitem->sig == NULL)
        return -2;
    grp->nworkitems += 1;
    
    ta_mt_group_retain(grp); /* Since work item has a reference to it */
    if (!QueueUserWorkItem(ta_mt_worker, pitem, WT_EXECUTEDEFAULT)) {
        CloseHandle(pitem->sig);
        grp->nworkitems -= 1;
        ta_mt_group_release(grp);/* Matches retain above since worker will not do it */
        return -3;
    }

    return 0;
}

long ta_mt_group_wait(ta_mt_group_t grp, ta_mt_time_t timeout) 
{
    DWORD i;
    HANDLE sigs[TA_MT_MAX_GROUP_SIZE];

    if (grp->nworkitems == 0)
        return 0;

    for (i = 0; i < grp->nworkitems; ++i)
        sigs[i] = grp->workitems[i].sig;

    i = WaitForMultipleObjects(grp->nworkitems, sigs, TRUE, timeout);
    if (i < WAIT_OBJECT_0 || i >= (WAIT_OBJECT_0+grp->nworkitems))
        return i;               /* Error  / timeout etc. */
    else
        return 0;
}

void ta_mt_group_retain(ta_mt_group_t grp)
{
    InterlockedIncrement(&grp->nrefs);
}

void ta_mt_group_release(ta_mt_group_t grp) 
{
    if (InterlockedDecrement(&grp->nrefs) <= 0) {
        unsigned int i;
        for (i = 0; i < grp->nworkitems; ++i) {
            TA_ASSERT(grp->workitems[i].owner == grp);
            if (grp->workitems[i].sig != NULL)
                CloseHandle(grp->workitems[i].sig);
        }
        TA_FREEMEM(grp);
    }
}

#endif
