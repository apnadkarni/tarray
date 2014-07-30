/*
 * Copyright (c) 2012-2014 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#include "tarray.h"

#define TAS_ALLOC malloc
#define TAS_FREE  free

/* Note len does not include trailing null and unlike Tcl_Obj's
   returned ref count is 1 on allocation */
tas_t *tas_alloc(char *s, int len)
{
    tas_t *ptas;
    int sz;

    TA_ASSERT(len >= 0);

    sz = sizeof(tas_t) + len; /* tas_t already accounts for trailing null */
    ptas = TAS_ALLOC(sz);
    if (ptas == NULL)
        ta_memory_panic(sz);
    memcpy(&ptas->s[0], s, len+1);
    ptas->nrefs = 1;
    return ptas;
}

tas_t * tas_from_obj(Tcl_Obj *o)
{
    int len;
    char *s;
    s = Tcl_GetStringFromObj(o, &len);
    return tas_alloc(s, len);
}

Tcl_Obj *tas_to_obj(tas_t *ptas)
{
    return Tcl_NewStringObj(ptas->s, -1);
}

tas_t *tas_dup(tas_t *src)
{
    TA_ASSERT(src->nrefs > 0);
    return tas_alloc(&src->s[0], strlen(src->s));
}

/* Returned tas may not be same as src ! */
tas_t *tas_ref(tas_t *src)
{
    TA_ASSERT(src->nrefs > 0);
    if (src->nrefs < TAS_MAX_NREFS) {
        src->nrefs += 1;
        return src;
    } else
        return tas_dup(src);
}

void tas_unref(tas_t *ptas)
{
    TA_ASSERT(ptas->nrefs > 0);
    ptas->nrefs -= 1;
    if (ptas->nrefs == 0)
        TAS_FREE(ptas);
}

int tas_equal(tas_t *a, tas_t *b, int nocase)
{
    TA_ASSERT(a->nrefs > 0 && b->nrefs > 0);
    return ta_utf8_equal(a->s, b->s, nocase);
}

int tas_compare(tas_t *a, tas_t *b, int nocase)
{
    TA_ASSERT(a->nrefs > 0 && b->nrefs > 0);
    return ta_utf8_compare(a->s, b->s, nocase);
}
