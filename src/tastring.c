/*
 * Copyright (c) 2012-2014 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include "tarray.h"

tas_t *tas_alloc(int len)
{
    tas_t *ptas;
    int sz;

    TA_ASSERT(len >= 0);

    sz = sizeof(tas_t) + len; /* tas_t already accounts for trailing null 
                               so actual size len+1 */
    ptas = TAS_ALLOCMEM(sz);
    ptas->s[0] = 0;
    ptas->nrefs = 1;
    return ptas;
}


/* Note len does not include trailing null and unlike Tcl_Obj's
   returned ref count is 1 on allocation */
tas_t *tas_alloc_nbytes(char *s, int len)
{
    tas_t *ptas;

    TA_ASSERT(len >= 0);
    ptas = tas_alloc(len);  /* Available space will be len+1 */
    memcpy(&ptas->s[0], s, len);
    ptas->s[len] = 0;           /* Always terminate */
    return ptas;
}



/* The descriptor for our custom hash */
static void tas_hash_free(Tcl_HashEntry *he);
static unsigned int tas_hash_compute(Tcl_HashTable *phashtab, void *pkey);
static int tas_hash_compare(void *pkey, Tcl_HashEntry *he);
static Tcl_HashEntry *tas_hash_alloc(Tcl_HashTable *phashtab, void *pkey);

Tcl_HashKeyType tas_hash_type = {
    TCL_HASH_KEY_TYPE_VERSION,	/* version */
    0,				/* flags */
    tas_hash_compute,		/* hashKeyProc */
    tas_hash_compare,		/* compareKeysProc */
    tas_hash_alloc,		/* allocEntryProc */
    tas_hash_free		/* freeEntryProc */
};


static void tas_hash_table_init(Tcl_HashTable *phashtab)
{
    Tcl_InitCustomHashTable(phashtab, TCL_CUSTOM_PTR_KEYS, &tas_hash_type);
}

static Tcl_HashEntry *tas_hash_alloc(Tcl_HashTable *phashtab, void *pkey)
{
    Tcl_HashEntry *he = (Tcl_HashEntry *)ckalloc(sizeof(Tcl_HashEntry));
    he->key.oneWordValue = (char *) tas_ref(pkey);
    he->clientData = NULL;
    return he;
}

static int tas_hash_compare(void *pkey, Tcl_HashEntry *he)
{
    tas_t *ptas1 = pkey;
    tas_t *ptas2 = (tas_t *) he->key.oneWordValue;

    if (ptas1 == ptas2)
        return 1;
    return tas_equal(ptas1, ptas2, 0);
}

static void tas_hash_free(Tcl_HashEntry *he)
{
    tas_unref((tas_t *) he->key.oneWordValue);
    ckfree((char *)he);
}

static unsigned int tas_hash_compute(Tcl_HashTable *phashtab, void *pkey)
{
    const char *s = ((tas_t *)pkey)->s;
    unsigned int result;
    unsigned char c;

    /* Exact copy of Tcl's string hash */
    if ((result = *s) != 0) {
	while ((c = *++s) != 0) {
	    result += (result << 3) + c;
	}
    }

    return result;
}

tas_lookup_t tas_lookup_new()
{
    tas_lookup_t lookup = (Tcl_HashTable *) TAS_ALLOCMEM(sizeof(Tcl_HashTable));
    tas_hash_table_init(lookup);
    return lookup;
}

void tas_lookup_free(tas_lookup_t lookup)
{
    Tcl_DeleteHashTable(lookup);
    TAS_FREEMEM(lookup);
}

int tas_lookup_entry(tas_lookup_t lookup, tas_t *ptas, int *pval)
{
    Tcl_HashEntry *he;
    he = Tcl_FindHashEntry(lookup, (char *)ptas);
    if (he) {
        if (pval)
            *pval = (int) Tcl_GetHashValue(he);
        return 1;
    } else
        return 0;
}

void tas_lookup_add(tas_lookup_t lookup, tas_t *ptas, int val)
{
    int newly_created;
    Tcl_HashEntry *he;

    he = Tcl_CreateHashEntry(lookup, (char *)ptas, &newly_created);
    /* NOTE WE OVERWRITE EXISTING VALUE IF ANY */
    Tcl_SetHashValue(he, (ClientData) val);
}

int tas_lookup_delete(tas_lookup_t lookup, tas_t *ptas)
{
    Tcl_HashEntry *he;
    he = Tcl_FindHashEntry(lookup, (char *)ptas);
    if (he == NULL)
        return 0;
    else {
        Tcl_DeleteHashEntry(he);
        return 1;
    }
}


/* TBD - delete from lookup when element removed from thdr? Or keep lazy
   in which case do we need to provide a "garbage collection" function?
   Note that even if you delete, lookup and thdr may not be matched since
   the item may occur elsewhere in the thdr and a linear search still
   has to be done next time a lookup if made for that stirng */
   
