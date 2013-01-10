/*
 * Copyright (c) 2012, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#include <string.h>
#if __GNUC__ && !__GNUC_STDC_INLINE__
#define BA_INLINE
#endif
#include "bitarray.h"

static void ba_copy_unaligned_upward(ba_t *to, int to_internal_off, const ba_t *from, int from_internal_off, int len)
{
    ba_t ba, mask;
    int nbits, ba_len;

    BA_ASSERT(to_internal_off < BA_UNIT_MASK);
    BA_ASSERT(from_internal_off < BA_UNIT_MASK);
    BA_ASSERT(to_internal_off != from_internal_off); /* Because then should be calling a faster routine */
    BA_ASSERT(from > to || (from == to && from_internal_off > to_internal_off));      /* else should be calling ba_copy_unaligned_downward */

    if (len == 0)
        return;                 /* Also simplifies logic below */

    /* Start out by aligning the destination */
    if (to_internal_off) {
        /* Align destination by writing out the fractional bits */
        nbits = BA_UNIT_SIZE - to_internal_off;
        if (nbits > len) {
            nbits = len;
            len = 0;
        } else {
            len -= nbits;
        }
        ba = ba_getn(from, from_internal_off, nbits);
        ba_putn(to, to_internal_off, ba, nbits);
        ++to;
        from_internal_off += nbits;
        if (from_internal_off >= BA_UNIT_SIZE) {
            from_internal_off -= BA_UNIT_SIZE;
            ++from;
        }
    }

    if (len == 0)
        return;

    /*
     * At this point
     *  - to points to the aligned destination (to_internal_off is irrelevant)
     *  - from points to the unit containing bits to be copied
     *  - len has been updated to reflect # bits copied so far
     *  - from_internal_off has been updated to reflect some
     *    bits have been copied from the source
     */

    ba_len = len / BA_UNIT_SIZE; /* # whole ba_t units to copy */
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

    ba = ba_getn(from, from_internal_off, len);
    ba_putn(to, 0, ba, len);
}

static void ba_copy_unaligned_downward(ba_t *to, int to_internal_off, const ba_t *from, int from_internal_off, int len)
{
    ba_t ba, mask;
    int nbits, ba_len;
    int off;

    BA_ASSERT(to_internal_off < BA_UNIT_MASK);
    BA_ASSERT(from_internal_off < BA_UNIT_MASK);
    BA_ASSERT(to_internal_off != from_internal_off); /* Because then should be calling a faster routine */
    BA_ASSERT(from < to || (from == to && from_internal_off < to_internal_off));      /* else should be calling ba_copy_unaligned_upward */

    if (len == 0)
        return;                 /* Also simplifies logic below */

    /* We  will be copying backwards so find end of the destination */
    off = to_internal_off + len - 1; /* Offset of last bit to write to */
    to += off / BA_UNIT_SIZE;        /* to -> last ba_t to be written to */
    nbits = (off % BA_UNIT_SIZE) + 1; /* Number of bits to write in last ba_t */
    if (nbits > len)
        nbits = len;

    off = from_internal_off + len - nbits;
    from += off / BA_UNIT_SIZE;
    from_internal_off = off % BA_UNIT_SIZE;
    BA_ASSERT(from_internal_off != 0);         /* Since to & from unaligned */

    /* Start out by writing the partial "left over" bits at end */
    if (nbits < BA_UNIT_SIZE) {
        ba = ba_getn(from, from_internal_off, nbits);
        ba_putn(to, 0, ba, nbits);
        len -= nbits;
        --to;
        --from; /* Essentially move the source BA_UNIT_SIZE bits for next copy */
    }

    /* Now we move whole ba_t units into the destination */
    ba_len = len / BA_UNIT_SIZE; /* # whole ba_t units to copy */
    len = len % BA_UNIT_SIZE;    /* # left over bits */
    while (ba_len--) {
        /* Note use of temporary ba in case to and from overlap */
        ba = *from >> from_internal_off;
        ba |= from[1] << (BA_UNIT_SIZE - from_internal_off);
        *to-- = ba;
        --from;
    }
            
    /* Now we have the left over len bits */
    if (len == 0)
        return;

    ba = ba_getn(from, from_internal_off, len);
    ba_putn(to, BA_UNIT_SIZE - len, ba, len);
}


/* Caller must have ensured enough space in destination (see BA_BYTES_NEEDED) */
/* TBD - test with overlapping moves, both where dst > src and src > dst */
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
     * If source and destination offsets have same alignment,
     * we can move chunks of memory and then copy
     * left over bits. If alignments are different, we are forced to
     * read a unit from the source, merge it with the destination
     * and then write it back.
     */
    if (from_internal_off == to_internal_off) {
        ba_t *leadingP;
        ba_t saved_leading_partial;
        int  leading_partial_len;
        ba_t saved_trailing_partial;

        /*
         * Same alignment on source and destination. We can copy
         * entire ba_t units, taking care of extra bits separately.
         * We have to be careful in case source and destination overlap
         * and source addr < dest address that overlapping bytes of
         * source are not written into as destination *before* they
         * are copied out. memmove() guarantees this for the source
         * and destination buffers it's passed, but we also
         * have to ensure this for the extra bits outside the ba_t alignment.
         * We do this by saving off the source extra bits at the beginning
         * and end and writing them to the destination after the memmove
         * is done.
         */
        if (to_internal_off) {
            /* We will save the partial bits and write them
               to destination later in case of overlap. */
            saved_trailing_partial = from[(from_internal_off + len - 1) / BA_UNIT_SIZE];
            leadingP = to;
            if ((len + to_internal_off) >= BA_UNIT_SIZE) {
                leading_partial_len = BA_UNIT_SIZE - to_internal_off;
                len -= leading_partial_len;
            } else {
                /* Entire length fits in first ba_t */
                leading_partial_len = len;
                len = 0;
            }
            saved_leading_partial = ba_getn(from, to_internal_off, leading_partial_len);
            ++to;               /* Point to aligned ba_t's */
            ++from;
        }

        if (len) {
            /* Leading partial bits were not all that are to be copied. */
            ba_len = len / BA_UNIT_SIZE; /* # ba_t units to copy */
            len = len % BA_UNIT_SIZE;    /* # left over bits */

            /* Copy middle chunk  Note we need to copy to the ba_t boundary,
             * not byte boundary, else will not work correctly depending
             * on platform byte order and our bit order settings.
             */
            if (ba_len) {
                /* Not memcpy since may overlap! */
                memmove(to, from, ba_len * sizeof(ba_t));
                from += ba_len;
                to += ba_len;
            }
            
            if (len) {
                /* There are left over trailing bits */
                ba_putn(to, 0, saved_trailing_partial, len);
            }
        }
        
        /* Now finally write the first partial, if there was one */
        if (to_internal_off) {
            ba_putn(leadingP, to_internal_off, saved_leading_partial,
                    leading_partial_len);
        }
    } else {
        /* to_internal_off != from_internal_off so we have to merge/copy fragments */
        if ((from > to) || (from == to && from_internal_off > to_internal_off))
            ba_copy_unaligned_upward(to, to_internal_off, from, from_internal_off, len);
        else
            ba_copy_unaligned_downward(to, to_internal_off, from, from_internal_off, len);
    }
    /* Phew! */
}

void ba_copy_reverse(ba_t *dst, int dst_off, const ba_t *src, int src_off, int len)
{
    /* TBD - make more efficient but tricky when overlapping */
    ba_copy(dst, dst_off, src, src_off, len);
    ba_reverse(dst, dst_off, len);
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
    

    /* TBD - this code to be optimized
       see http://bits.stephan-brumme.com/lowestBitSet.html or
       http://www.steike.com/code/bits/debruijn/ or
       ffs and equivalent
     */

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
int ba_count_zeroes(ba_t *baP,  int off, int count)
{
    if (count <= off)
        return 0;
    else
        return (count-off) - ba_count_ones(baP, off, count);
}


int ba_count_ones(ba_t *baP, int off, int count)
{
    ba_t ba;
    int n, nbits;

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
        nbits += ba_count_unit_ones(ba);
        ba = *baP++;
        off += BA_UNIT_SIZE;
    }

    /* When above loop terminates, ba contains any partial bits (if any)
     * in the last ba_t of the specified range
     */
    if (off < count) {
        ba = ba & BITPOSMASKLT(count-off);
        nbits += ba_count_unit_ones(ba);
    }

    return nbits;
}

void ba_reverse(ba_t *baP, int off, int len)
{
    ba_t front, end;
    int end_off;

    /* TBD - optimize by aligning front or back */
    end_off = off + len - BA_UNIT_SIZE;
    /* We will loop until there is overlap */
    while (end_off >= (off + BA_UNIT_SIZE)) {
        front = ba_reverse_unit(ba_get_unit(baP, off));
        end = ba_reverse_unit(ba_get_unit(baP, end_off));
        ba_put_unit(baP, off, end);
        ba_put_unit(baP, end_off, front);
        off += BA_UNIT_SIZE;
        end_off -= BA_UNIT_SIZE;
    }
    /* Now we have to do the overlapping bits */
    len = end_off + BA_UNIT_SIZE - off;
    BA_ASSERT(len >= 0 && len < BA_UNIT_SIZE);
    if (len) {
        front = ba_getn(baP, off, len); /* Get the overlapping bits */
        /* Reverse the bits and shift into position */
        end = ba_reverse_unit(front) >> (BA_UNIT_SIZE-len);
        ba_putn(baP, off, front, len);
    }
}

#ifdef BA_TEST
int main()
{
    /* Temp means of trying specific tests. Actual test suite is in
       the tarray code */
    unsigned char a[] = {0xaa, 0x00, 0xff, 0x55, 0x33};
    ba_copy(a, 12, a, 4, 13);
    printf("%x %x %x %x %x", a[0], a[1], a[2], a[3], a[4]);
}

#endif
