/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include "bitarray.h"



/* Caller must have ensured enough space in destination (see BA_BYTES_NEEDED) */
void ba_copy(const ba_t *dst, int dst_off, const ba_t *src, int src_off,
             int len)
{
    ba_t *from, *to;
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
            ba = (*from & BITPOSMASKGE(from_internal_offset)) >> from_internal_offset;
            if ((nbits + from_internal_off) <= BA_UNIT_SIZE) {
                /* One source unit was enough to supply nbits. */
                from_internal_offset += nbits;
                from_internal_offset %= BA_UNIT_SIZE; /* Probably not needed? */
            } else {
                /* Need to collect from two source units */
                int nbits2;     /* # bits needed from second unit */
                ++from;         /* Point to next second unit */
                nbits2 = (nbits + from_internal_offset) - BA_UNIT_SIZE;
                ba |= (*from & BITPOSMASKLT(nbits2)) << (BA_UNIT_SIZE - from_internal_offset);
                from_internal_offset = nbits - (BA_UNIT_SIZE - from_internal_offset);
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
         *  - from_internal_offset has been updated to reflect some
         *    bits have been copied from the source
         */

        ba_len = len / BA_UNIT_SIZE; /* # ba_t units to copy */
        len = len % BA_UNIT_SIZE;    /* # left over bits */
        nbits = 
        while (ba_len--) {
            /* Again note use of temporary ba in case to and from overlap */
            ba = *from++ >> from_internal_offset;
            ba |= *from << (BA_UNIT_SIZE - from_internal_offset);
            *to++ = ba;
        }

        /* Now we have the left over len bits */
        if (len == 0)
            return;

        ba = *from >> from_internal_offset;
        if ((from_internal_offset+len) > BA_UNIT_SIZE) {
            /* Still more bits needed */
            ++from;
            ba |= *from << (BA_UNIT_SIZE - from_internal_offset);
        }
        mask = BITPOSMASKGE(len);  /* Dest bits to preserve */
        *to = (*to & mask) | (ba & ~mask);
    }

    /* Phew! */
}

int bitarray_bit(ba_t *baP, int off)
{
    return (baP[off / BA_UNIT_SIZE] & BITPOSMASK(off % BA_UNIT_SIZE)) != 0;
}

void bitarray_set(ba_t *baP, int off, int val)
{
    baP += off / BA_UNIT_SIZE;
    off = off % BA_UNIT_SIZE;
    if (val)
        *baP |= BITPOSMASK(off);
    else
        *baP &= ~BITPOSMASK(off);
}

void bitarray_fill(ba_t *baP, int off, int count, int ival)
{
    ba_t ba;
    int bitpos;

    if (count == 0)
        return;

    /* First set the bits to get to a char boundary */
    baP += off / BA_UNIT_SIZE;
    bitpos = off % CHAR_BIT; /* Offset of bit within a char */
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

