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
         * the byte boundary and then use memmove. We need to copy
         * to the ba_t boundary else will not work correctly depending
         * on platform byte order and our bit order settings.
        */
        if (to_internal_off) {
            /* We do have some initial bits to copy. Use an intermediary ba
             * in case to and from are same. We have to consider two cases:
             * when the # bits to be copied extends to the end of the
             * ba_t unit, and when # bits is so small that only middle bits
             * have to be modified.
             */
            mask = BITMASKLT(to_internal_off); /* Dest bits to be preserved */
            if ((len + to_internal_off) >= BA_UNIT_SIZE) {
                len -= BA_UNIT_SIZE - to_internal_off;
            } else {
                /* Need to preserve bits at other end of unit as well */
                to_internal_off += len;
                mask |= BITMASKGE(to_internal_off); /* upper bits to preserve */
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

        /* Copy middle chunk */
        if (ba_len) {
            memmove(dst, src, ba_len); /* Not memcpy since may overlap! */
            from += ba_len;
            to += ba_len;
        }

        if (len == 0)
            return;             /* No leftover bits */

        /* Finally, do left-over bits. */
        mask = BITMASKGT(len);  /* Destination bits to preserve */
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
            mask = BITMASKLT(to_internal_off); /* Dest bits to preserve */
            if ((len + to_internal_off) >= BA_UNIT_SIZE) {
                nbits = BA_UNIT_SIZE - to_internal_off;
                len -= nbits;
            } else {
                /* Need to preserve bits at other end of unit as well */
                nbits = len;
                to_internal_off += bits;
                mask |= BITMASKGE(to_internal_off); /* upper bits to preserve */
                len = 0;
            }
            /* Collect nbits bits from the source */
            if (nbits > (BA_UNIT_SIZE - from_internal_offset)) {
                /* Need to collect from two source units */
                ba = *from++ & BITMASKGE(from_internal_offset);

                from_internal_offset -= nbits;
            } else {
            }

        }

        if (len == 0)
            return;

        TBD;


    }

    

}

