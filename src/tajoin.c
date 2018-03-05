/*
 * Copyright (c) 2015, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file license.terms for license
 */

#include "tarray.h"

TCL_RESULT tcol_sortmerge_helper_cmd(ClientData clientdata, Tcl_Interp *ip,
                                     int objc, Tcl_Obj *const objv[])
{ 
    Tcl_Obj *aindex, *bindex, *acol, *bcol;
    thdr_t *aithdr, *bithdr, *athdr, *bthdr, *amatch, *bmatch;
    int a, b, alen, blen, aifirst, bifirst, afirst, bfirst;
    int *aiptr, *biptr;
    Tcl_Obj *objs[2];
    int atype, status;
    int nocase;
    
    amatch = NULL;
    bmatch = NULL;
    
    if (objc < 5 || objc > 6) {
	Tcl_WrongNumArgs(ip, 1, objv, "INDEXA COLA INDEXB COLB ?NOCASE?");
	return TCL_ERROR;
    }

    nocase = 0;
    if (objc == 6 &&
        ((status = Tcl_GetIntFromObj(ip, objv[5], &nocase)) != TCL_OK))
            return status;
    
    aindex = objv[1];
    acol = objv[2];
    bindex = objv[3];
    bcol = objv[4];

    if ((status = tcol_convert(ip, aindex)) != TCL_OK ||
        (status = ta_check_column_type(ip, OBJTHDR(aindex), TA_INT)) != TCL_OK ||
        (status = tcol_convert(ip, acol)) != TCL_OK ||
        (status = tcol_convert(ip, bindex)) != TCL_OK ||
        (status = ta_check_column_type(ip, OBJTHDR(bindex), TA_INT)) != TCL_OK ||
        (status = tcol_convert(ip, bcol)) != TCL_OK)
        return status;

    athdr = OBJTHDR(acol);
    atype = athdr->type; 
    bthdr = OBJTHDR(bcol);
    if (atype != bthdr->type)
        return ta_mismatched_types_error(ip, atype, bthdr->type);
    if (atype == TA_BOOLEAN)
        return ta_bad_type_error(ip, athdr);
    
    alen = tcol_occupancy(aindex);
    blen = tcol_occupancy(bindex);
    if (alen != tcol_occupancy(acol) ||
        blen != tcol_occupancy(bcol)) {
        return ta_column_lengths_error(ip);
    }

    aifirst = OBJTHDRFIRST(aindex);
    bifirst = OBJTHDRFIRST(bindex);
    afirst = OBJTHDRFIRST(acol);
    bfirst = OBJTHDRFIRST(bcol);
    
    aithdr = OBJTHDR(aindex);
    bithdr = OBJTHDR(bindex);
    aiptr = THDRELEMPTR(aithdr, int, aifirst);
    biptr = THDRELEMPTR(bithdr, int, bifirst);

    /* Note the output vectors have to be the same size.*/
    amatch = thdr_alloc(ip, TA_INT, alen); /* TBD what should this size be ? */
    bmatch = thdr_alloc(ip, TA_INT, alen);

#define OUTPUT_MATCH_(apos_,bpos_)                                            \
    do {                                                                \
        TA_ASSERT(amatch->usable == bmatch->usable);                    \
        TA_ASSERT(amatch->used == bmatch->used);                        \
        if (amatch->used >= amatch->usable) {                           \
            thdr_t *pnew;                                               \
            pnew = thdr_realloc(ip, amatch, amatch->used + TA_EXTRA(amatch->used)); \
            if (pnew == NULL)                                           \
                goto error_handler;                                     \
            amatch = pnew;                                              \
            pnew = thdr_realloc(ip, bmatch, amatch->used + TA_EXTRA(bmatch->used)); \
            if (pnew == NULL)                                           \
                goto error_handler;                                     \
            bmatch = pnew;                                              \
            TA_ASSERT(amatch->usable == bmatch->usable);                \
            TA_ASSERT(amatch->used == bmatch->used);                    \
        }                                                               \
        *THDRELEMPTR(amatch, int, amatch->used) = apos_;                \
        amatch->used += 1;                                              \
        *THDRELEMPTR(bmatch, int, bmatch->used) = bpos_;                \
        bmatch->used += 1;                                              \
    } while (0)

#define MERGE_(TYPE_, CMPTYPE_)                                         \
    do {                                                                \
        TYPE_ *aptr;                                                    \
        TYPE_ *bptr;                                                    \
        aptr = THDRELEMPTR(athdr, TYPE_, afirst);                       \
        bptr = THDRELEMPTR(bthdr, TYPE_, bfirst);                       \
        a = b = 0;                                                      \
        while (a < alen && b < blen) {                                  \
            TYPE_ aval;                                                 \
            TYPE_ bval;                                                 \
            CMPTYPE_   cmp;                                             \
            aval = aptr[aiptr[a]];                                      \
            bval = bptr[biptr[b]];                                      \
            cmp = CMP_(aval, bval);                                     \
            if (cmp > 0)                                                \
                ++b; /* aval > bval */                                  \
            else if (cmp < 0)                                           \
                ++a; /* aval < bval */                                  \
            else {                                                      \
                /* Values aval and bval (from A[a] and B[b]) match.     \
                 * Outer loop below iterates through elements which have the  \
                 * same value. For each such element, the inner loop    \
                 * iterates through the elements of B starting at position \
                 * b that match aval.                                   \
                 */                                                     \
                do {                                                    \
                    int b2 = b;                                         \
                    do {                                                \
                        OUTPUT_MATCH_(aiptr[a], biptr[b2]);             \
                        ++b2;                                           \
                    } while (b2 < blen && (CMP_(aval,bptr[biptr[b2]]) == 0)); \
                    ++a;                                                \
                } while (a < alen && (CMP_(aptr[aiptr[a]], bval) == 0)); \
            }                                                           \
        }                                                               \
    } while (0)

    
    switch (atype) {
#define CMP_(a_, b_) ((a_) - (b_))
    case TA_INT:
        MERGE_(int, int);
        break;
    case TA_WIDE:
        MERGE_(Tcl_WideInt, Tcl_WideInt);
        break;
    case TA_DOUBLE:
        MERGE_(double, double);
        break;
        /* For unsigned, need slightly different compare since not sure how compiler will treat unsigned subtraction */
#undef CMP_
#define CMP_(a_, b_) ((a_) >= (b_) ? ((a_) - (b_)) : - 1)
    case TA_BYTE:
        MERGE_(unsigned char, int);
        break;
    case TA_UINT:
        MERGE_(unsigned int, int);
        break;
    case TA_ANY:
#undef CMP_
#define CMP_(a_, b_) ta_obj_compare((a_), (b_), nocase)        
        MERGE_(Tcl_Obj*, int);
        break;
    case TA_STRING:
#undef CMP_
#define CMP_(a_, b_) tas_compare((a_), (b_), nocase)        
        MERGE_(tas_t*, int);
        break;
    default:
        ta_bad_type_error(ip, athdr);
        goto error_handler;
    }
    
    objs[0] = tcol_new(amatch);
    objs[1] = tcol_new(bmatch);
    Tcl_SetObjResult(ip, Tcl_NewListObj(2, objs));
    return TCL_OK;
    
error_handler:
    /* interp must already contain the error */
    if (amatch)
        thdr_decr_refs(amatch);
    if (bmatch)
        thdr_decr_refs(bmatch);
    return TCL_ERROR;
}
