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


/* The descriptor for our custom hash */
static void tas_hash_free(Tcl_HashEntry *he);
static unsigned int tas_hash_compute(Tcl_HashTable *phashtab, void *pkey);
static int tas_hash_compare(void *pkey, Tcl_HashEntry *he);
static Tcl_HashEntry *tas_hash_alloc(Tcl_HashTable *phashtab, void *pkey);

const Tcl_HashKeyType tas_hash_type = {
    TCL_HASH_KEY_TYPE_VERSION,	/* version */
    0,				/* flags */
    tas_hash_compute,		/* hashKeyProc */
    tas_hash_compare,		/* compareKeysProc */
    tas_hash_alloc,		/* allocEntryProc */
    tas_hash_free		/* freeEntryProc */
};

void tas_hash_table_init(Tcl_HashTable *phashtab)
{
    Tcl_InitCustomHashTable(phashtab, TCL_CUSTOM_PTR_KEYS, &tas_hash_type);
}

static Tcl_HashEntry *tas_hash_alloc(Tcl_HashTable *phashtab, void *pkey)
{
    Tcl_HashEntry *he = ckalloc(sizeof(Tcl_HashEntry));
    he->key.oneWordValue = tas_ref(pkey);
    he->clientData = NULL;
    return he;
}

static int tas_hash_compare(void *pkey, Tcl_HashEntry *he)
{
    tas_t *ptas1 = pkey;
    tas_t *ptas2 = (tas_t *) he->key.oneWordValue;

    register const char *p1, *p2;
    register int l1, l2;

    if (ptas1 == ptas2)
        return 1;

    return tas_equal(ptas1, ptas2, 0);
}

static void tas_hash_free(Tcl_HashEntry *he)
{
    tas_unref((tas_t *) he->key.oneWordValue);
    ckfree(he);
}

static unsigned int tas_hash_compute(Tcl_HashTable *phashtab, void *pkey)
{
    const char *s = ((tas_t *)pkey)->s;
    const unsigned char *string = pkey;
    unsigned int result;
    unsigned char c;

    /* Exact copy of Tcl's string hash */
    if ((result = *string) != 0) {
	while ((c = *++string) != 0) {
	    result += (result << 3) + c;
	}
    }

    return result;
}
