/*
 * Copyright (c) 2016 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#include "tarray.h"
#include "pcg_basic.h"

static TCL_RESULT ta_invalid_bounds(Tcl_Interp *ip) {
    Tcl_SetErrorCode(ip, "TARRAY", "RANDOM", "BOUNDS", NULL);
    Tcl_SetResult(ip, "Invalid random number range bounds.", TCL_STATIC);
    return TCL_ERROR;
}

/* Based on PCG basic c distribution pcg32x2-demo.c */
TA_INLINE uint64_t pcg32x2_random_r(pcg32_random_t rng[2])
{
    return ((uint64_t)(pcg32_random_r(&rng[0])) << 32)
            | pcg32_random_r(&rng[1]);
}

/* Returns floating point in range 0 to 1 */
TA_INLINE double pcgdouble_random_r(pcg32_random_t rng[2])
{
    return ldexp((double)pcg32x2_random_r(rng), -64);
}

TA_INLINE int pcgbool_random_r(pcg32_random_t *prng)
{
    return pcg32_random_r(prng) & 1;
}

/* From PCG basic c distribution pcg32x2-demo.c */
static uint64_t pcg32x2_boundedrand_r(pcg32_random_t rng[2], uint64_t bound)
{
    uint64_t threshold = -bound % bound;
    for (;;) {
        uint64_t r = pcg32_random_r(rng);
        if (r >= threshold)
            return r % bound;
    }
}

/* bound must be positive! */
TA_INLINE double pcgdouble_boundedrand_r(pcg32_random_t rng[2], double bound)
{
    return bound * pcgdouble_random_r(rng);
}

void tcol_random_cmd_delete(ClientData cdata)
{
    ckfree(cdata);
}

TCL_RESULT tcol_random_cmd(ClientData cdata, Tcl_Interp *ip,
                         int objc, Tcl_Obj *const objv[])
{
    unsigned int count;
    Tcl_Obj *tcol, *olbound, *oubound;
    ta_value_t lbound, ubound;
    TCL_RESULT res;
    int tatype;
    ta_rng_t *prng = (ta_rng_t *)cdata;
    thdr_t *thdr = NULL;

    olbound = oubound = NULL;
    switch (objc) {
    case 3: break;
    case 4: olbound = objv[3]; break;
    case 5: olbound = objv[3]; oubound = objv[4]; break;
    default:
	Tcl_WrongNumArgs(ip, 1, objv, "TYPE SIZE ?LOWBOUND ?HIGHBOUND??");
	return TCL_ERROR;
    }

    if (ta_parse_type(ip, objv[1], &tatype) != TCL_OK)
	return TCL_ERROR;

    res = ta_get_uint_from_obj(ip, objv[2], &count);
    if (res != TCL_OK)
        return res;

    switch (tatype) {
    case TA_BOOLEAN:
        /* For booleans, bounds are ignored */
        break;
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_BYTE:
        if (olbound) {
            res = ta_value_from_obj(ip, olbound, tatype, &lbound);
            if (res != TCL_OK)
                return res;
            if (oubound) {
                res = ta_value_from_obj(ip, oubound, tatype, &ubound);
                if (res != TCL_OK)
                    return res;
            }
        }
        break;
    default:
        return ta_invalid_op_for_type(ip, tatype);
    }

    thdr = thdr_alloc(ip, tatype, count);
    if (thdr == NULL)
        return TCL_ERROR;

    res = TCL_OK;
    if (count == 0)
        goto vamoose;

    if (olbound == NULL) {
        /*
         * No bounds specified at all.
         * Note there is a significant performance difference between
         * pcg32_random_r and pcg32_boundedrand_r so we special case
         * these.
         */
        if (tatype == TA_INT || tatype == TA_UINT) {
            unsigned int *p = THDRELEMPTR(thdr, unsigned int, 0);
            unsigned int *end = p + count;
            while (p < end)
                *p++ = pcg32_random_r(&prng->rng[0]);
            goto vamoose;
        } else if (tatype == TA_WIDE) {
            Tcl_WideInt *p = THDRELEMPTR(thdr, Tcl_WideInt, 0);
            Tcl_WideInt *end = p + count;
            while (p < end)
                *p++ = pcg32x2_random_r(prng->rng);
            goto vamoose;
        } else if (tatype == TA_DOUBLE) {
            double *p = THDRELEMPTR(thdr, double, 0);
            double *end = p + count;
            while (p < end)
                *p++ = pcgdouble_random_r(prng->rng);
            goto vamoose;
        }
    }

    /* We have to deal with bounds or type is not 32-bit integer */

#define RFILL_(type_, fld_, basetype_, fn_, prng_)                            \
    do {                                                                \
        type_ *p = THDRELEMPTR(thdr, type_, 0);                         \
        type_ *end = p + count;                                         \
        basetype_ bound;                                                \
        /* Note pcg crash if lbound==ubound since ubound is open interval */ \
        if (lbound. fld_ == ubound. fld_) {                             \
            res = ta_invalid_bounds(ip);                                \
            break;                                                      \
        }                                                               \
        if (lbound. fld_ > ubound. fld_) {                              \
            type_ temp = lbound. fld_;                                  \
            lbound. fld_ = ubound. fld_;                                \
            ubound. fld_ = temp;                                        \
        }                                                               \
        bound = ubound. fld_ - lbound. fld_;                            \
        while (p < end) {                                               \
            basetype_ temp = fn_(prng_, bound);                    \
            *p++ = (type_) (temp + lbound. fld_);                       \
        }                                                               \
    } while (0)

    switch (tatype) {
    case TA_UINT:
        TA_ASSERT(olbound);
        if (oubound == NULL)
            ubound.uival = UINT32_MAX;
        RFILL_(unsigned int, uival, uint32_t, pcg32_boundedrand_r, &prng->rng[0]);
        break;
    case TA_INT:
        TA_ASSERT(olbound);
        if (oubound == NULL)
            ubound.ival = INT32_MAX;
        RFILL_(int, ival, uint32_t, pcg32_boundedrand_r, &prng->rng[0]);
        break;
    case TA_WIDE:
        TA_ASSERT(olbound);
        if (oubound == NULL)
            ubound.wval = INT64_MAX;
        /* Note for wides we pass rng, not rng[0]. C will actually
           treat both the same but conceptually different as the 32x2 expects
           an array of two rngs */
        RFILL_(Tcl_WideInt, wval, uint64_t, pcg32x2_boundedrand_r, prng->rng);
        break;        
            
    case TA_DOUBLE:
        TA_ASSERT(olbound);
        /* TBD - how about over/underflows if bounds specified? */
        if (oubound == NULL)
            ubound.dval = DBL_MAX; 
        RFILL_(double, dval, double, pcgdouble_boundedrand_r, prng->rng);
        break;

    case TA_BYTE:
        if (oubound == NULL) {
            ubound.ucval = UINT8_MAX;
            if (olbound == NULL)
                lbound.ucval = 0;
        }
        RFILL_(unsigned char, ucval, uint32_t, pcg32_boundedrand_r, &prng->rng[0]);
        break;

    case TA_BOOLEAN:
        /* Note: Bounds are ignored for booleans */
       {
           ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
           int i;
           for (i = 0; i < count; ++i)
               ba_put(baP, i, pcgbool_random_r(&prng->rng[0]));
       }
       break;

    default:
        ta_type_panic(tatype);
        break;
    }
    
vamoose: /* If res != TCL_OK, ip must already hold error */
    
    if (res == TCL_OK) {    
        thdr->used = count;
        Tcl_SetObjResult(ip, tcol_new(thdr));
        return TCL_OK;
    }
    else {
        if (thdr)
            thdr_decr_refs(thdr);
        return TCL_ERROR;
    }
}

