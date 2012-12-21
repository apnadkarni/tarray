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
#  error Please define 64-bit ba_t appropriately for your compiler.
# endif
#else
# error Invalid BA_UNIT_SIZE
#endif

/* Calculate *total* ba_t units required for storing nbits_ at offset off_ */
#define BA_UNITS_NEEDED(off_, nbits_) (((off_)+(nbits_)+BA_UNIT_SIZE-1) / BA_UNIT_SIZE)
/* Ditto but in bytes */
#define BA_BYTES_NEEDED(off_, nbits_) (BA_UNITS_NEEDED(off_, nbits_) * (BA_UNIT_SIZE/CHAR_BIT))

#endif
