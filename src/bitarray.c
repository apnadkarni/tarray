/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include <string.h>
#include "bitarray.h"

/* Caller must have ensured enough space in destination (see BA_BYTES_NEEDED) */
void ba_copy(ba_t *dst, int dst_off, const ba_t *src, int src_off, int len)
{
    const ba_t *from;
    ba_t *to;
    ba_t mask;
    int from_internal_off, to_internal_off, ba_len;

    if (len == 0 || (dst == src && dst_off == src_off))
        return;
        
    /* Point to where memory containing source / destination bits */
    from = src + (src_off / BA_UNIT_SIZE);
    to = dst + (dst_off / BA_UNIT_SIZE);

    /* Offsets into the ba unit at that location */
    from_internal_off = src_off % BA_UNIT_SIZE;
    to_internal_off = dst_off % BA_UNIT_SIZE;

    /*
     * If source and destination offsets have same alignment, we can
     * copy initial bits, then move chunks of memory and then copy
     * left over bits. If alignments are different, we are forced to
     * read a unit from the source, merge it with the destination
     * and then write it back.
     */
    if (from_internal_off == to_internal_off) {
        /*
         * Copy the first few bits to get to the ba_unit boundary
         * Note if ba_t is not unsigned char, we cannot copy just to
         * the byte boundary and then use memmove.
        */
        if (to_internal_off) {
            /* We do have some initial bits to copy. Use an intermediary ba
             * in case to and from are same. We have to consider two cases:
             * when the # bits to be copied extends to the end of the
             * ba_t unit, and when # bits is so small that only middle bits
             * have to be modified.
             */
            mask = BITPOSMASKLT(to_internal_off); /* Dest bits to be preserved */
            if ((len + to_internal_off) >= BA_UNIT_SIZE) {
                len -= BA_UNIT_SIZE - to_internal_off;
            } else {
                /* Need to preserve bits at other end of unit as well */
                to_internal_off += len;
                mask |= BITPOSMASKGE(to_internal_off); /* upper bits to preserve */
                len = 0;
            }
            *to = (*to & mask) | (*from & ~mask);
            ++to;
            ++from;
        }

        if (len == 0)
            return;

        ba_len = len / BA_UNIT_SIZE; /* # ba_t units to copy */
        len = len % BA_UNIT_SIZE;    /* # left over bits */

        /* Copy middle chunk  Note we need to copy to the ba_t boundary,
         * not byte boundary, else will not work correctly depending
         * on platform byte order and our bit order settings.
         */
        if (ba_len) {
            /* Not memcpy since may overlap! */
            memmove(dst, src, ba_len * sizeof(ba_t));
            from += ba_len;
            to += ba_len;
        }

        if (len == 0)
            return;             /* No leftover bits */

        /* Finally, do left-over bits. */
        mask = BITPOSMASKGT(len);  /* Destination bits to preserve */
        *to = (*to & mask) | (*from & ~mask);

    } else {
        /*
         * Sadly, to and from have different alignments. Nought to do
         * but pluck out fragments and merge. We want writes to memory
         * to be aligned so we will collect destination units and write
         * them out
         */
        ba_t ba;
        int nbits;

        /* Start out by aligning the destination */
        if (to_internal_off) {
            mask = BITPOSMASKLT(to_internal_off); /* Dest bits to preserve */
            if ((len + to_internal_off) >= BA_UNIT_SIZE) {
                nbits = BA_UNIT_SIZE - to_internal_off; /* #bits need of src */
                len -= nbits;
            } else {
                /* Need to preserve bits at other end of unit as well */
                nbits = len + to_internal_off; /* Temp to pass to macro in
                                                  case macro double evals */
                mask |= BITPOSMASKGE(nbits); /* upper bits to preserve */
                nbits = len;              /* # bits need of source */
                len = 0;
            }
            /* Collect nbits bits from the source into low bits of ba */
            ba = (*from & BITPOSMASKGE(from_internal_off)) >> from_internal_off;
            if ((nbits + from_internal_off) <= BA_UNIT_SIZE) {
                /* One source unit was enough to supply nbits. */
                from_internal_off += nbits;
                from_internal_off %= BA_UNIT_SIZE; /* Probably not needed? */
            } else {
                /* Need to collect from two source units */
                int nbits2;     /* # bits needed from second unit */
                ++from;         /* Point to next second unit */
                nbits2 = (nbits + from_internal_off) - BA_UNIT_SIZE;
                ba |= (*from & BITPOSMASKLT(nbits2)) << (BA_UNIT_SIZE - from_internal_off);
                from_internal_off = nbits - (BA_UNIT_SIZE - from_internal_off);
            }
            /* ba contains required bits in low order. Store them in dest */
            ba <<= to_internal_off;
            *to = (*to & mask) | (ba & ~mask);
            ++to;
        }

        if (len == 0)
            return;

        /*
         * Sigh...all we have done so far is update the first destination unit!
         * At this point
         *  - to points to the aligned destination (effectively, 
         *    to_internal_off is immaterial)
         *  - from points to the unit containing bits to be copied
         *  - len has been updated to reflect # bits copied so far
         *  - from_internal_off has been updated to reflect some
         *    bits have been copied from the source
         */

        ba_len = len / BA_UNIT_SIZE; /* # ba_t units to copy */
        len = len % BA_UNIT_SIZE;    /* # left over bits */
        while (ba_len--) {
            /* Again note use of temporary ba in case to and from overlap */
            ba = *from++ >> from_internal_off;
            ba |= *from << (BA_UNIT_SIZE - from_internal_off);
            *to++ = ba;
        }

        /* Now we have the left over len bits */
        if (len == 0)
            return;

        ba = *from >> from_internal_off;
        if ((from_internal_off+len) > BA_UNIT_SIZE) {
            /* Still more bits needed */
            ++from;
            ba |= *from << (BA_UNIT_SIZE - from_internal_off);
        }
        mask = BITPOSMASKGE(len);  /* Dest bits to preserve */
        *to = (*to & mask) | (ba & ~mask);
    }

    /* Phew! */
}

void ba_fill(ba_t *baP, int off, int count, int ival)
{
    int bitpos;

    if (count == 0)
        return;

    /* First set the bits to get to a char boundary */
    baP += off / BA_UNIT_SIZE;
    bitpos = off % BA_UNIT_SIZE; /* Offset of bit within a char */
    if (bitpos != 0) {
        if (ival)
            *baP++ |= BITPOSMASKGE(bitpos);
        else
            *baP++ &= ~ BITPOSMASKGE(bitpos);
        count -= (BA_UNIT_SIZE - bitpos);
    }
    /* Now copy full bytes with memset */
    memset(baP, ival ? 0xff : 0, sizeof(ba_t) * (count/BA_UNIT_SIZE));
    baP += count / BA_UNIT_SIZE;
    count = count % BA_UNIT_SIZE;                      /* # remaining bits */
    if (count) {
        if (ival)
            *baP |= BITPOSMASKLT(count);
        else
            *baP &= ~ BITPOSMASKLT(count);
    }
}

/* count is total # bits in baP, not beyond offset */
int ba_find(ba_t *baP, int bval, int off, int count)
{
    ba_t ba, skip, ba_mask;
    int pos = -1;

    if (count <= off)
        return -1;

    skip = BITPOSMASKGE(0);
    if (bval)
        skip = ~skip;
    

    /* TBD - this code to be optimized */

    /* First locate the starting point for the search */
    baP += off/BA_UNIT_SIZE;
    ba_mask = BITPOSMASK(off % BA_UNIT_SIZE);
    for (; off < count; ba_mask = BITPOSMASK(0)) {
        /*
         * At top of loop, *baP potentially has a matching
         * bit, ba_mask contains position at which to begin match
         */
        ba = *baP++;
        if (ba == skip) {
            /* Looking for 1's and uc is all 0's or vice versa */
            off += BA_UNIT_SIZE;
            continue;
        }
        while (ba_mask) {
            /* Compare bit against 1 or 0 as appropriate */
                if ((bval && (ba_mask & ba)) ||
                    !(bval || (ba_mask & ba))) {
                    /* Match but note this may be beyond count */
                    return pos >= count ? -1 : pos;
                }
                ba_mask >>= 1;
                ++off;
            }
    }

    return -1;
}

/* Find number bits set in a bit array */
int ba_count_reset(ba_t *baP,  int off, int count)
{
    if (count <= off)
        return 0;
    else
        return (count-off) - ba_count_set(baP, off, count);
}


int ba_count_set(ba_t *baP, int off, int count)
{
    ba_t ba;
    int i, n, nbits;
    int *iP;

    if (count <= off)
        return 0;

    nbits = 0;
    baP += off / BA_UNIT_SIZE;
    ba = *baP & BITPOSMASKGE(off % BA_UNIT_SIZE);
    n = ((count + off) / BA_UNIT_SIZE) * BA_UNIT_SIZE;
    /* At this point,
     * baP points to ba_t containing first bit of interest
     * ba contains *baP with bits not of interest masked off
     * n is count minus the partial bits in the last ba_t
     */
    while (off < n) {
        /* At top of loop ba contains the bits to count */
#if BA_UNIT_SIZE == 32
        // See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetNaive
        ba = ba - ((ba >> 1) & 0x55555555); // reuse input as temporary
        ba = (ba & 0x33333333) + ((ba >> 2) & 0x33333333);     // temp
        nbits += ((ba + (ba >> 4) & 0xF0F0F0F) * 0x1010101) >> 24; // count
#else
        /* TBD - Need to optimize these cases too */
        for (; ba; ++nbits)
        {
            ba &= ba - 1; // clear the least significant bit set
        }
#endif
        ba = *baP++;
        off += BA_UNIT_SIZE;
    }

    /* When above loop terminates, ba contains any partial bits (if any)
     * in the last ba_t of the specified range
     */
    if (off < count) {
        ba = ba & BITPOSMASKLT(count-off);
#if BA_UNIT_SIZE == 32
        // See http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetNaive
        ba = ba - ((ba >> 1) & 0x55555555); // reuse input as temporary
        ba = (ba & 0x33333333) + ((ba >> 2) & 0x33333333);     // temp
        nbits += ((ba + (ba >> 4) & 0xF0F0F0F) * 0x1010101) >> 24; // count
#else
        /* TBD - Need to optimize these cases too */
        for (; ba; ++nbits)
        {
            ba &= ba - 1; // clear the least significant bit set
        }
#endif
    }

    return nbits;
}
