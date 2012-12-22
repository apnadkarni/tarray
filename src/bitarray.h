#ifndef BITARRAY_H
#define BITARRAY_H

#include <limits.h>             /* CHAR_BIT etc. */

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
#elif BA_UNIT_SIZE == 32 
  typedef unsigned int ba_t;
#elif BA_UNIT_SIZE == 64
# if defined(_MSC_VER)
    typedef unsigned __int64 ba_t;
# elif defined(__GNUC__)
    typedef unsigned long long ba_t;
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

/* Return a mask containing a 1 at a bit position (MSB being bit 0) 
   BITPOSMASK(2) -> 00100000 */
#define BITPOSMASK(pos_) ((ba_t)(((ba_t) 1) << (pos_)))
#define BITPOSMASKLT(pos_) (BITPOSMASK(pos_)-1)
#define BITPOSMASKGE(pos_) (- BITPOSMASK(pos_))
#define BITPOSMASKGT(pos_) (BITPOSMASKGE(pos_) - BITPOSMASK(pos_))

/*
 * Return a mask where all bit positions up to, but not including pos
 * are 1, remaining are 0. For example, BITMASKLT(2) -> 00000011
 */

/* Mask for next bit */
#define BITMASKNEXT(mask_) ((ba_t)((mask_) << 1))
#define BITMASKGE(mask_) ((ba_t)(-(mask_)))

#ifdef _MSC_VER
# define BA_INLINE __inline  /* Because VC++ 6 only accepts "inline" in C++  */
#else
# define BA_LINLINE inline
#endif
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
int ba_count_set(ba_t *baP, int off, int count);
int ba_count_reset(ba_t *baP, int off, int count);

#endif
