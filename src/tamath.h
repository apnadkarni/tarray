#ifndef TAMATH_H
#define TAMATH_H

/* Following clones tclInt.h portability definitions of nan and inf */
#ifdef _MSC_VER
#    define TA_ISINFINITE(d)	(!(_finite((d))))
#    define TA_ISNAN(d)		(_isnan((d)))
#else
#    define TA_ISINFINITE(d)	((d) > DBL_MAX || (d) < -DBL_MAX)
#    ifdef NO_ISNAN
#	 define TA_ISNAN(d)	((d) != (d))
#    else
#	 define TA_ISNAN(d)	(isnan(d))
#    endif
#endif

/* 
 * Functions for overflow detection in integer operations.
 * Only operations used in tarray are defined.
 * References:
 *   https://wiki.sei.cmu.edu/confluence/display/c/INT32-C.+Ensure+that+operations+on+signed+integers+do+not+result+in+overflow
 *   https://wiki.sei.cmu.edu/confluence/display/c/INT30-C.+Ensure+that+unsigned+integer+operations+do+not+wrap
 *   https://gcc.gnu.org/onlinedocs/gcc/Integer-Overflow-Builtins.html
 */

#ifdef __FAST_MATH__
# error -ffast-math option of GCC will cause incorrect calculations.
#endif

/* Visual C++ prior to Visual Studio 2010 do not have stdint */
#if defined(_MSC_VER) && _MSC_VER < 1700
#include "ms_stdint.h"
#else
#include <stdint.h>
#endif

#ifndef TA_INLINE
# ifdef _MSC_VER
#  define TA_INLINE __inline  /* Because VC++ 6 only accepts "inline" in C++  */
# elif __GNUC__ && !__GNUC_STDC_INLINE__
#  define TA_INLINE extern inline
# else
#  define TA_INLINE inline
# endif
#endif


#if __GNUC__ >= 5

/****************************************************************
 * gcc has direct built-ins for integer overflow detection 
 ****************************************************************/

/* Generic gcc built-in */
#define OVF_ADD_FN(type_) \
    TA_INLINE int ovf_add_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) { \
    return __builtin_add_overflow(a, b, presult);                       \
}
#define OVF_ADDU_FN OVF_ADD_FN

OVF_ADD_FN(int8)
OVF_ADDU_FN(uint8)

/* For 32-bit use exact built-ins */
#define ovf_add_int32  __builtin_sadd_overflow
#define ovf_add_uint32 __builtin_uadd_overflow
/* For 64-bit assumes l is 64 bits - TBD */
#define ovf_add_int64  __builtin_saddl_overflow
#define ovf_add_uint64  __builtin_uaddl_overflow

/* Generic gcc built-in */
#define OVF_SUB_FN(type_)                                               \
    TA_INLINE int ovf_sub_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) { \
    return __builtin_sub_overflow(a, b, presult);                       \
}
#define OVF_SUBU_FN OVF_SUB_FN

OVF_SUB_FN(int8)
OVF_SUBU_FN(uint8)

/* For 32-bit use exact built-ins */
#define ovf_sub_int32  __builtin_ssub_overflow
#define ovf_sub_uint32 __builtin_usub_overflow
/* For 64-bit assumes l is 64 bits */
#define ovf_sub_int64  __builtin_ssubl_overflow

/* Generic gcc built-in */
#define OVF_MUL_FN(type_)                                               \
    TA_INLINE int ovf_mul_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) { \
    return __builtin_mul_overflow(a, b, presult);                       \
}
#define OVF_MULU_FN OVF_MUL_FN

OVF_MUL_FN(int8)
OVF_MULU_FN(uint8)

/* For 32-bit use exact built-ins */
#define ovf_mul_int32  __builtin_smul_overflow
#define ovf_mul_uint32 __builtin_umul_overflow
/* For 64-bit assumes l is 64 bits */
#define ovf_mul_int64  __builtin_smull_overflow
#define ovf_mul_uint64  __builtin_umull_overflow

#else

/****************************************************************
 * Not gcc - define portable versions
 ****************************************************************/

#define OVF_ADD_FN(type_, min_, max_) \
    TA_INLINE int ovf_add_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) \
    {                                                                   \
        int64_t result = (int64_t) a + (int64_t) b;                     \
        *presult = (type_ ## _t) result;                                \
        return (result > max_ || result < min_);                        \
    }

#define OVF_ADDU_FN(type_) \
    TA_INLINE int ovf_add_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) \
    {                                                                   \
        *presult = a + b;                                     \
        return *presult < a;                                              \
    }

OVF_ADDU_FN(uint8)
OVF_ADD_FN(int32, INT32_MIN, INT32_MAX)
OVF_ADDU_FN(uint32)
OVF_ADDU_FN(uint64)

TA_INLINE int ovf_add_int64(int64_t a, int64_t b, int64_t *presult) {
    *presult = a + b;
    return ((b > 0 && a > (INT64_MAX - b)) ||
            (b < 0) && a < (INT64_MIN - b));
}

#define OVF_SUB_FN(type_, min_, max_) \
    TA_INLINE int ovf_sub_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) \
    {                                                                   \
        int64_t result = (int64_t) a - (int64_t) b;                     \
        *presult = (type_ ## _t) result;                                \
        return (result > max_ || result < min_);                        \
    }

#define OVF_SUBU_FN(type_) \
    TA_INLINE int ovf_sub_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) \
    {                                                                   \
        *presult = a - b;                                              \
        return (a < b) ;                                                \
    }

OVF_SUB_FN(int8, INT8_MIN, INT8_MAX)
OVF_SUBU_FN(uint8)
OVF_SUB_FN(int32, INT32_MIN, INT32_MAX)
OVF_SUBU_FN(uint32)

TA_INLINE int ovf_sub_int64(int64_t a, int64_t b, int64_t *presult) {
    *presult = a - b;
    return ((b > 0 && a < (INT64_MIN + b)) ||
            (b < 0) && a > (INT64_MAX + b));
}

#define OVF_MUL_FN(type_, min_, max_) \
    TA_INLINE int ovf_mul_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) \
    {                                                                   \
        int64_t result = (int64_t) a * (int64_t) b;                     \
        *presult = (type_ ## _t) result;                                \
        return (result > max_ || result < min_);                        \
    }

#define OVF_MULU_FN(type_, max_) \
    TA_INLINE int ovf_mul_ ## type_ (type_ ## _t a, type_ ## _t b, type_ ## _t *presult) \
    {                                                                   \
        int64_t result = (int64_t) a * (int64_t) b;                     \
        *presult = (type_ ## _t) result;                                \
        return (result > max_);                                         \
    }

OVF_MUL_FN(int8, INT8_MIN, INT8_MAX)
OVF_MULU_FN(uint8, UINT8_MAX)
OVF_MUL_FN(int32, INT32_MIN, INT32_MAX)
OVF_MULU_FN(uint32, UINT32_MAX)

int ovf_mul_int64_impl(int64_t a, int64_t b, int64_t *presult);
int ovf_mul_uint64_impl(uint64_t a, uint64_t b, uint64_t *presult);


#if 0 && defined(_MSC_VER) && defined(_M_AMD64)
/* DISABLED because no way to tell if _mul128 overflowed */
#include <intrin.h>
#pragma intrinsic(_mul128)

TA_INLINE int ovf_mul_int64(int64_t a, int64_t b, int64_t *presult) {
    __int64 high;
    *presult = _mul128(a, b, &high);
    return (high != 0);
}

#else

/* Too long to be inline - defined in tamath.c */
#define ovf_mul_int64 ovf_mul_int64_impl
#define ovf_mul_uint64 ovf_mul_uint64_impl

#endif


#endif /* __GNUC__ >= 5 */


#endif
