#include "tcl.h"
#include "tarray.h"

#define Tcl_Panic panic

#ifdef _MSC_VER
#define kh_inline __inline
#else
#define kh_inline inline
#endif

static kh_inline void *safe_malloc(size_t sz)
{
    void *p = malloc(sz);
    if (p == NULL)
        Tcl_Panic("Memory allocation failure");
    return p;
}

static kh_inline void safe_free(void *p)
{
    free(p);
}

static kh_inline void *safe_realloc(void *p, size_t sz)
{
    p = realloc(p, sz);
    if (p == NULL)
        Tcl_Panic("Memory allocation failure");
    return p;
}

static kh_inline void *safe_calloc(size_t n, size_t sz)
{
    void *p = calloc(n, sz);
    if (p == NULL)
        Tcl_Panic("Memory allocation failure");
    return p;
}

#define kmalloc safe_malloc
#define kfree safe_free
#define kcalloc safe_calloc
#define krealloc safe_realloc

#include "khash.h"

/*
 * For Tcl_Obj * and tas_t (string) types, we need to define the hash
 * and comparison functions.
 */
typedef Tcl_Obj *kh_tclobj_t;
static kh_inline khint_t kh_tclobj_hash_func(Tcl_Obj *o)
{
    /* Just use the same one that khash uses for char * type */
    __ac_X31_hash_string(Tcl_GetString(o));
}
static kh_inline khint_t kh_tclobj_hash_equal(Tcl_Obj *a, Tcl_Obj *b)
{
    return strcmp(Tcl_GetString(a), Tcl_GetString(b)) == 0;
}

typedef const tas_t *kh_tas_t;
static kh_inline khint_t kh_tas_hash_func(const tas_t *tas)
{
    /* Just use the same one that khash uses for char * type */
    __ac_X31_hash_string(tas->s);
}
static kh_inline khint_t kh_tas_hash_equal(const tas_t *a, const tas_t *b)
{
    return strcmp(a->s, b->s) == 0;
}


/*
 * Define the various combinations of maps and sets. The keys may
 * be 32- and 64-bit integers, tas_t and Tcl_Obj *. The values may
 * be those types plus doubles and bytes. Unsigned ints are emulated with
 * ints for both keys or values. Boolean values will be emulated with bytes.
 * Currently though we only instantiate mappings to ints until we have a use for
 * the others.
 */
KHASH_INIT(int2int, khint32_t, int32_t, 1, kh_int_hash_func, kh_int_hash_equal)
KHASH_INIT(wide2int, khint64_t, int32_t, 1, kh_int64_hash_func, kh_int64_hash_equal)
KHASH_INIT(tas2int, kh_tas_t, int32_t, 1, kh_tas_hash_func, kh_tas_hash_equal)
KHASH_INIT(tclobj2int, kh_tclobj_t, int32_t, 1, kh_tclobj_hash_func, kh_as_hash_equal)
              
#if 0
void panic(s) {}
int int2int()
{
    int ret, is_missing;
    khiter_t k;
    khash_t(int2int) *h = kh_init(int2int);
	k = kh_put(int2int, h, 5, &ret);
	kh_value(h, k) = 10;
	k = kh_get(int2int, h, 10);
	is_missing = (k == kh_end(h));
	k = kh_get(int2int, h, 5);
	kh_del(int2int, h, k);
	for (k = kh_begin(h); k != kh_end(h); ++k)
		if (kh_exist(h, k)) kh_value(h, k) = 1;
	kh_destroy(int2int, h);
	return 0;
}
int wide2int()
{
    int ret, is_missing;
    khiter_t k;
    khash_t(wide2int) *h = kh_init(wide2int);
	k = kh_put(wide2int, h, 5, &ret);
	kh_value(h, k) = 10;
	k = kh_get(wide2int, h, 10);
	is_missing = (k == kh_end(h));
	k = kh_get(wide2int, h, 5);
	kh_del(wide2int, h, k);
	for (k = kh_begin(h); k != kh_end(h); ++k)
		if (kh_exist(h, k)) kh_value(h, k) = 1;
	kh_destroy(wide2int, h);
	return 0;
}
#endif
