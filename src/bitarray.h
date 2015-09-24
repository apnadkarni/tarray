#ifndef BITARRAY_H
#define BITARRAY_H

#include <limits.h>             /* CHAR_BIT etc. */

#if !defined(BA_ASSERT)
# ifdef BA_ENABLE_ASSERT
#  include <assert.h>
#  define BA_ASSERT assert
# else
#  define BA_ASSERT(bool_) ((void) 0)
# endif
#endif

#ifdef _MSC_VER
# define LIT64(x) (x)
#else
# define LIT64X(x, suff) (x ## suff)
# define LIT64(x) LIT64X(x, LLU)
#endif

/*
 * # bits in a unit - must be 8, 32 or 64. Pointers passed to all routines
 * MUST be aligned appropriately.
 */
#ifndef BA_UNIT_SIZE
# define BA_UNIT_SIZE 32
#endif

/* NOTE: ba_t MUST BE unsigned ELSE SHIFTS AND MASKS WILL BREAK */

#if BA_UNIT_SIZE == 8 
  typedef unsigned char ba_t;
  typedef signed char sba_t;
#elif BA_UNIT_SIZE == 32 
  typedef unsigned int ba_t;
  typedef int sba_t;
#elif BA_UNIT_SIZE == 64
# if defined(_MSC_VER)
    typedef unsigned __int64 ba_t;
    typedef __int64 sba_t;
# elif defined(__GNUC__)
    typedef unsigned long long ba_t;
    typedef long long sba_t;
# else
#   error Please define 64-bit ba_t appropriately for your compiler.
# endif
#else
# error Invalid BA_UNIT_SIZE
#endif

/* Calculate *total* ba_t units required for storing nbits_ at offset off_ */
#define BA_UNITS_NEEDED(off_, nbits_) (((off_)+(nbits_)+BA_UNIT_SIZE-1) / BA_UNIT_SIZE)
/* Ditto but in bytes */
#define BA_BYTES_NEEDED(off_, nbits_) (BA_UNITS_NEEDED(off_, nbits_) * (BA_UNIT_SIZE/CHAR_BIT))

/* Bits are numbered from LSB (0) to MSB */

#define BITPOSMASK(pos_) ba_position_mask(pos_)
/*
 * Return a mask where all bit positions up to, but not including pos
 * are 1, remaining are 0. For example, BITMASKLT(2) -> 00000011
 */
#define BITPOSMASKLT(pos_) ((ba_t)(BITPOSMASK(pos_)-1))
#define BITPOSMASKGE(pos_) ((ba_t) (- (sba_t)BITPOSMASK(pos_)))
#define BITPOSMASKGT(pos_) ((ba_t)(BITPOSMASKGE(pos_) - BITPOSMASK(pos_)))


/* Mask for next bit */
#define BITMASKNEXT(mask_) ((ba_t)((mask_) << 1))
#define BITMASKGE(mask_) ((ba_t)(-(sba_t)(mask_)))

#ifndef BA_INLINE
# ifdef _MSC_VER
#  define BA_INLINE __inline  /* Because VC++ 6 only accepts "inline" in C++  */
# elif __GNUC__ && !__GNUC_STDC_INLINE__
#  define BA_INLINE extern inline
# else
#  define BA_INLINE inline
# endif
#endif

/* Some constants needed for bit operations */
#if BA_UNIT_SIZE == 64
# define BA_MASK_01 LIT64(0x0101010101010101)
# define BA_MASK_33 LIT64(0x3333333333333333)
# define BA_MASK_55 LIT64(0x5555555555555555)
# define BA_MASK_0F LIT64(0x0F0F0F0F0F0F0F0F)
# define BA_MASK_FF LIT64(0x00FF00FF00FF00FF)
# define BA_MASK_FFFF LIT64(0x0000FFFF0000FFFF)
#else
# define BA_MASK_01 0x01010101
# define BA_MASK_33 0x33333333
# define BA_MASK_55 0x55555555
# define BA_MASK_0F 0x0F0F0F0F
# define BA_MASK_FF 0x00FF00FF
# define BA_MASK_FFFF 0x0000FFFF
#endif

/* Return a mask containing a 1 at a bit position,
   e.g. BITPOSMASK(2) -> 00100000 */
BA_INLINE ba_t ba_position_mask(int pos)
{
    BA_ASSERT(pos < BA_UNIT_SIZE);
    return ((ba_t) 1) << pos;
}

/* Count bits in unit */
BA_INLINE int ba_count_unit_ones(ba_t ba)
{
#if BA_UNIT_SIZE == 32
    // See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetNaive
    ba = ba - ((ba >> 1) & BA_MASK_55); // reuse input as temporary
    ba = (ba & BA_MASK_33) + ((ba >> 2) & BA_MASK_33);     // temp
    return ((ba + (ba >> 4) & BA_MASK_0F) * BA_MASK_01) >> 24; // count
#else
    int nbits;
    /* TBD - Need to optimize these cases too */
    for (nbits = 0; ba; ++nbits)
        ba &= ba - 1; // clear the least significant bit set
    return nbits;
#endif
    
}


/*
 * Merges the bits from b specified by mask with the non-masked bits of a
 * A faster version of (a & ~mask) | (b & mask)
 */
BA_INLINE ba_t ba_merge_unit(ba_t a, ba_t b, ba_t mask)
{
    return a ^ ((a ^ b) & mask);
}

/* Caller should take care of case where there are not n bits
   valid at offset off */
BA_INLINE ba_t ba_getn(const ba_t *baP, int off, int n)
{
    BA_ASSERT(n > 0 && n <= BA_UNIT_SIZE);
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    if (off == 0)
        return (n == BA_UNIT_SIZE ? *baP : (*baP & BITPOSMASKLT(n)));
    else {
        /* We have to be careful here that though off + n may be valid,
           off+BA_UNIT_SIZE may not be so do not try to get more bits than
           asked for.
        */
        if ((off+n) > BA_UNIT_SIZE) {
            /*  n bits are spread between baP and baP+1 */
            return ((baP[1] & BITPOSMASKLT(off+n-BA_UNIT_SIZE)) << (BA_UNIT_SIZE - off)) | (baP[0] >> off);
        } else {
            /* Entire range is within one ba_t */
            BA_ASSERT(n < BA_UNIT_SIZE); /* If == would have hit one of the cases above */
            n += off;
            BA_ASSERT(n <= BA_UNIT_SIZE);
            if (n == BA_UNIT_SIZE)
                return *baP >> off;
            else 
                return (BITPOSMASKLT(n) & *baP) >> off;
        }
    }
}

/* Caller should take care of case where there are not n bits of memory
   valid at offset off */
BA_INLINE void ba_putn(ba_t *baP, int off, ba_t ba, int n)
{
    BA_ASSERT(n > 0 && n <= BA_UNIT_SIZE);

    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    if (off == 0)
        *baP = (n == BA_UNIT_SIZE ? ba : ba_merge_unit(ba, *baP, BITPOSMASKGE(n)));
    else {
        if ((n+off) > BA_UNIT_SIZE) {
            /* bits are spread across two ba_t units */
            baP[0] = ba_merge_unit(baP[0], (ba_t)(ba << off), BITPOSMASKGE(off));
            n = off + n - BA_UNIT_SIZE;  /* # bits to store in top word */
            baP[1] = ba_merge_unit(baP[1], (ba_t)(ba >> (BA_UNIT_SIZE - off)),
                                   BITPOSMASKLT(n));
        } else {
            /* Bits fit in one word */
            BA_ASSERT(n < BA_UNIT_SIZE); /* If == would have hit one of the cases above */
            ba <<= off;
            n += off;
            BA_ASSERT(n <= BA_UNIT_SIZE);
            if (n == BA_UNIT_SIZE) {
                *baP = ba_merge_unit(*baP, ba, BITPOSMASKGE(off));
            } else
                *baP = ba_merge_unit(ba,
                                     *baP,
                                     (ba_t) (BITPOSMASKLT(off) | BITPOSMASKGE(n)));
        }
    }
}


/* Caller should take care of case where there are not BA_UNIT_SIZE bits
   valid at offset off */
BA_INLINE ba_t ba_get_unit(ba_t *baP, int off)
{
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    if (off == 0)
        return *baP;            /* Faster path */
    else 
        return ba_getn(baP, off, BA_UNIT_SIZE);
}

/* Caller should take care of case where there are not BA_UNIT_SIZE bits
   valid at offset off */
BA_INLINE void ba_put_unit(ba_t *baP, int off, ba_t ba)
{
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    if (off == 0)
        *baP = ba;              /* Fast path */
    else
        ba_putn(baP, off, ba, BA_UNIT_SIZE);
}

BA_INLINE ba_t ba_reverse_unit(ba_t ba)
{
    /* See http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel*/
    /* TBD - time and pick the other alternatives there */

#if BA_UNIT_SIZE == 8

    return (ba_t) (((ba * 0x0802LU & 0x22110LU) | (ba * 0x8020LU & 0x88440LU)) * 0x10101LU >> 16);

#elif BA_UNIT_SIZE == 32

    ba = ((ba >> 1) & BA_MASK_55) | ((ba & BA_MASK_55) << 1); /* odd<->even */
    ba = ((ba >> 2) & BA_MASK_33) | ((ba & BA_MASK_33) << 2); /* swap pairs */
    ba = ((ba >> 4) & BA_MASK_0F) | ((ba & BA_MASK_0F) << 4); /* swap nibbles */
    ba = ((ba >> 8) & BA_MASK_FF) | ((ba & BA_MASK_FF) << 8); /* swap bytes */
    /* swap 2-byte long pairs */
    return ( ba >> 16 ) | ( ba << 16);

#else

    ba = ((ba >> 1) & BA_MASK_55) | ((ba & BA_MASK_55) << 1); /* odd<->even */
    ba = ((ba >> 2) & BA_MASK_33) | ((ba & BA_MASK_33) << 2); /* swap pairs */
    ba = ((ba >> 4) & BA_MASK_0F) | ((ba & BA_MASK_0F) << 4); /* swap nibbles */
    ba = ((ba >> 8) & BA_MASK_FF) | ((ba & BA_MASK_FF) << 8); /* swap bytes */
    ba = ((ba >> 16) & BA_MASK_FFFF) | ((ba & BA_MASK_FFFF) << 16); /* swap words */
    return ( ba >> 32) | ( ba << 32); /* swap 32bits*/

#endif
}


BA_INLINE int ba_get(ba_t *baP, int off)
{
    return (baP[off / BA_UNIT_SIZE] & BITPOSMASK(off % BA_UNIT_SIZE)) != 0;
}

BA_INLINE void ba_put(ba_t *baP, int off, int val)
{
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    if (val)
        *baP |= BITPOSMASK(off);
    else
        *baP &= ~ BITPOSMASK(off);
}

BA_INLINE void ba_set(ba_t *baP, int off)
{
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    *baP |= BITPOSMASK(off);
}

BA_INLINE void ba_reset(ba_t *baP, int off)
{
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    *baP &= ~ BITPOSMASK(off);
}

void ba_copy(ba_t *dst, int dst_off, const ba_t *src, int src_off, int len);
void ba_fill(ba_t *baP, int off, int count, int ival);
int ba_find(ba_t *baP, int bval, int offset, int count);
int ba_count_ones(ba_t *baP, int off, int count);
int ba_count_zeroes(ba_t *baP, int off, int count);
void ba_reverse(ba_t *baP, int off, int len);
void ba_conjunct (ba_t *srca, int offa, ba_t *srcb, int offb, int count, ba_t *dst, int dstoff);
void ba_disjunct (ba_t *srca, int offa, ba_t *srcb, int offb, int count, ba_t *dst, int dstoff);
int ba_sanity_check(void);

#endif
