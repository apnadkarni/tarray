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

TCL_RESULT ta_rng_fixup_bounds(Tcl_Interp *ip, ta_value_t *low, ta_value_t *high, int init_high)
{
    TA_ASSERT(low && high);
    TA_ASSERT(init_high || low->type == high->type);

    if (init_high)
        ta_value_init_max(low->type, high);

    /*
     * Swap values if low > high.
     * If values are equal, error since PCG will infinite loop. Upper
     * bound is exclusive.
     */
#define FIXUP(fld_)                                     \
    do {                                                \
        ta_value_t temp;                                \
        if (low->fld_ < high->fld_) return TCL_OK;      \
        if (low->fld_ > high->fld_) {                   \
            temp = *low;                                \
            low->fld_ = high->fld_;                     \
            high->fld_ = temp.fld_;                     \
            return TCL_OK;                              \
        }                                               \
    } while (0)

    switch (low->type) {
    case TA_BOOLEAN: FIXUP(bval); break;
    case TA_BYTE: FIXUP(ucval); break;
    case TA_INT: FIXUP(ival); break;
    case TA_UINT: FIXUP(uival); break;
    case TA_WIDE: FIXUP(wval); break;
    case TA_DOUBLE: FIXUP(dval); break;
    }
    return ta_invalid_rng_bounds(ip, low, high);
}

void tcol_random_init(ta_rng_t *prng)
{
    uint64_t seed, seq;
#ifdef _WIN32
    LARGE_INTEGER pfc;
    QueryPerformanceCounter(&pfc); /* Never fails on XP and later */
    seed = pfc.QuadPart;
#else
    time_t t;
    seed = time(&t);
#endif
    seq = (uint64_t)(intptr_t) &prng; /* Any value will do */
    /* 
     * The two 32-bit random generators are used to generate 64-bit
     * random values. Therefore the
     * sequence parameter that determines the random stream
     * must not be the same even if the seed (second param) is different.
     * Just use complement of first as the second.
     * (See pcg32x3-demo.c in PCG distribution)
     */
    pcg32_srandom_r(&prng->rng[0], seed ^ (uint64_t)&prng->rng[0], seq);
    pcg32_srandom_r(&prng->rng[1], seed ^ (uint64_t)&prng->rng[1], ~seq);
}

/* Command deletion callback for tcol_random_cmd and tcol_randseed_cmd */
void ta_random_rng_delete(ClientData cdata)
{
    ta_rng_t *prng = (ta_rng_t *)cdata;
    if (prng->nrefs == 1)
        ckfree(cdata);
    else
        prng->nrefs--;
}

TCL_RESULT tcol_random_cmd(ClientData cdata, Tcl_Interp *ip,
                         int objc, Tcl_Obj *const objv[])
{
    Tcl_Size count;
    Tcl_Obj *olbound, *oubound;
    ta_value_t lbound, ubound;
    TCL_RESULT res;
    unsigned char tatype;
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

    res = ta_get_count_from_obj(ip, objv[2], 1, &count);
    if (res != TCL_OK)
        return res;

    switch (tatype) {
    case TA_BOOLEAN:
        /* For booleans, bounds are ignored */
        break;
    case TA_BYTE:
        if (olbound == NULL) {
            lbound.type = ubound.type = TA_BYTE;
            lbound.ucval = 0;
            ubound.ucval = 255;
            break;
        }
        /* FALLTHRU */
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
        if (olbound) {
            res = ta_value_from_obj(ip, olbound, tatype, &lbound);
            if (res != TCL_OK)
                return res;
            if (oubound) {
                res = ta_value_from_obj(ip, oubound, tatype, &ubound);
                if (res == TCL_OK)
                    res = ta_rng_fixup_bounds(ip, &lbound, &ubound, 0);
            } else {
                res = ta_rng_fixup_bounds(ip, &lbound, &ubound, 1);
            }
            if (res != TCL_OK)
                return res;
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
         * Note TA_BYTE has implicit bounds.
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

    /* We have to deal with bounds */

#define RFILL_(type_, fld_, basetype_, fn_, prng_)                           \
    do {                                                                     \
        type_ *p   = THDRELEMPTR(thdr, type_, 0);                            \
        type_ *end = p + count;                                              \
        basetype_ bound;                                                     \
        /* Note pcg crash if lbound==ubound since ubound is open interval */ \
        TA_ASSERT(lbound.fld_ < ubound.fld_);                                \
        bound = ubound.fld_ - lbound.fld_;                                   \
        while (p < end) {                                                    \
            basetype_ temp = fn_(prng_, bound);                              \
            *p++           = (type_)(temp + lbound.fld_);                    \
        }                                                                    \
    } while (0)

    switch (tatype) {
    case TA_UINT:
        TA_ASSERT(olbound);
        RFILL_(unsigned int, uival, uint32_t, pcg32_boundedrand_r, &prng->rng[0]);
        break;
    case TA_INT:
        TA_ASSERT(olbound);
        RFILL_(int, ival, uint32_t, pcg32_boundedrand_r, &prng->rng[0]);
        break;
    case TA_WIDE:
        TA_ASSERT(olbound);
        /* Note for wides we pass rng, not rng[0]. C will actually
           treat both the same but conceptually different as the 32x2 expects
           an array of two rngs */
        RFILL_(Tcl_WideInt, wval, uint64_t, pcg32x2_boundedrand_r, prng->rng);
        break;

    case TA_DOUBLE:
        TA_ASSERT(olbound);
        /* TBD - how about over/underflows if bounds specified? */
        RFILL_(double, dval, double, pcgdouble_boundedrand_r, prng->rng);
        break;

    case TA_BYTE:
        RFILL_(unsigned char, ucval, uint32_t, pcg32_boundedrand_r, &prng->rng[0]);
        break;

    case TA_BOOLEAN:
        /* Note: Bounds are ignored for booleans */
       {
           ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
           Tcl_Size i;
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

TCL_RESULT ta_randseed_cmd(ClientData cdata, Tcl_Interp *ip,
                         int objc, Tcl_Obj *const objv[])
{
    TCL_RESULT res;
    ta_rng_t *prng = (ta_rng_t *)cdata;
    Tcl_WideInt seed1, seed2;

    if (objc != 1 && objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "?SEED1 SEED2?");
        return TCL_ERROR;
    }

    if (objc == 1) {
        /* Reinit to random value */
        tcol_random_init(prng);
    } else {
        /* This is a *deterministic* rng (oxymoron). Useful for
         * reproducible tests. We allow caller to specify seeds
         * and use constant (arbitrary) sequence values
         */
        if ((res = Tcl_GetWideIntFromObj(ip, objv[1], &seed1)) != TCL_OK ||
            (res = Tcl_GetWideIntFromObj(ip, objv[2], &seed2)) != TCL_OK)
            return res;
        pcg32_srandom_r(&prng->rng[0], seed1, 0xf0f0f0f0);
        pcg32_srandom_r(&prng->rng[1], seed2, 0x0f0f0f0f);
    }

    return TCL_OK;
}

static ta_value_t
get_bounded_random(ta_rng_t *rng, ta_value_t *lbound, ta_value_t *ubound)
{
    ta_value_t tav;

    TA_ASSERT(lbound->type == ubound->type);
    TA_ASSERT(lbound->type != TA_BOOLEAN); /* Does not support bounds */
    switch (lbound->type) {
    case TA_BYTE:
        tav.ucval = pcg32_boundedrand_r(&rng->rng[0],
                                        ubound->ucval - lbound->ucval);
        tav.ucval += lbound->ucval;
        break;
    case TA_INT:
        tav.ival = pcg32_boundedrand_r(&rng->rng[0],
                                       ubound->ival - lbound->ival);
        tav.ival += lbound->ival;
        break;
    case TA_UINT:
        tav.uival = pcg32_boundedrand_r(&rng->rng[0],
                                        ubound->uival - lbound->uival);
        tav.uival += lbound->uival;
        break;
    case TA_WIDE:
        tav.wval = pcg32x2_boundedrand_r(rng->rng,
                                         ubound->wval - lbound->wval);
        tav.wval += lbound->wval;
        break;
    case TA_DOUBLE:
        tav.dval = pcgdouble_boundedrand_r(rng->rng,
                                           ubound->dval - lbound->dval);
        tav.dval += lbound->dval;
        break;
    default:
        ta_type_panic(lbound->type);
        break;
    }
    tav.type = lbound->type;
    return tav;
}

static TCL_RESULT ta_rng_get_method(ta_rng_instance_t *instance, Tcl_Interp *ip,
                                    int objc, Tcl_Obj *const objv[])
{
    Tcl_Size i, count;
    int bounded;
    Tcl_Obj *ores = NULL;   /* Just to silence bogus gcc uninitialized use warnings */
    ta_value_t lbound, ubound;

    count = 1;
    bounded = instance->bounded;
    if (bounded) {
        lbound = instance->lbound;
        ubound = instance->ubound;
    }
    if (objc) {
        if (Tcl_GetIntFromObj(ip, objv[0], &count) != TCL_OK)
            return TCL_ERROR;
        if (count < 0)
            return ta_negative_count_error(ip, count);
        if (objc > 1) {
            if (objc != 3)
                return ta_invalid_argcount(ip);
            if (instance->rtype == TA_BOOLEAN) {
                /* Booleans do not support bounds */
                return ta_invalid_op_for_type(ip, instance->rtype);
            }
            if (ta_value_from_obj(ip, objv[1],
                                  instance->rtype, &lbound) != TCL_OK ||
                ta_value_from_obj(ip, objv[2],
                                  instance->rtype, &ubound) != TCL_OK ||
                ta_rng_fixup_bounds(ip, &lbound, &ubound, 0) != TCL_OK)
                return TCL_ERROR;
            bounded = 1;
        }
    }

    if (count == 0)
        return TCL_OK;

    if (count > 1)
        ores = Tcl_NewListObj(count, NULL);
    if (bounded) {
        for (i = 0; i < count; ++i) {
            ta_value_t tav;
            tav = get_bounded_random(&instance->rng, &lbound, &ubound);
            if (count == 1) {
                ores = ta_value_to_obj(&tav);
                break;
            }
            Tcl_ListObjAppendElement(ip, ores, ta_value_to_obj(&tav));
        }
    } else {
        /* Unbounded */
        TA_ASSERT(instance->rtype != TA_BYTE);
        for (i = 0; i < count; ++i) {
            Tcl_Obj *o = NULL; /* Init just to silence gcc's bogus warnings */
            switch (instance->rtype) {
            case TA_BOOLEAN:
                o = Tcl_NewIntObj(pcgbool_random_r(&instance->rng.rng[0]));
                break;
            case TA_INT:
                o = Tcl_NewIntObj(pcg32_random_r(&instance->rng.rng[0]));
                break;
            case TA_UINT:
                o = Tcl_NewWideIntObj(pcg32_random_r(&instance->rng.rng[0]));
                break;
            case TA_WIDE:
                o = Tcl_NewWideIntObj(pcg32x2_random_r(instance->rng.rng));
                break;
            case TA_DOUBLE:
                o = Tcl_NewDoubleObj(pcgdouble_random_r(instance->rng.rng));
                break;
            default:
                ta_type_panic(instance->rtype);
                break;
            }
            if (count == 1) {
                ores = o;
                break;
            }
            Tcl_ListObjAppendElement(ip, ores, o);
        }
    }
    Tcl_SetObjResult(ip, ores);
    return TCL_OK;

}

static TCL_RESULT
ta_rng_instance_cmd(ClientData cdata, Tcl_Interp *ip,
                    int objc, Tcl_Obj *const objv[])
{
    ta_rng_instance_t *instance = (ta_rng_instance_t *)cdata;
    static const char* cmds[] = {
        "destroy", /* Syntax: <instance> destroy */
        "get",     /* Syntax: <instance> get cmd<<?COUNT? ?LOWER UPPER?>> */
        "seed",    /* Syntax: <instance> seed SEED1 SEED2 */
        NULL
    };
    enum cmd {DESTROY, GET, SEED};
    int cmd_index;

    if (objc < 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "option ?arg arg ...?");
        return TCL_ERROR;
    }
    if (Tcl_GetIndexFromObj(ip, objv[1],
                            cmds, "option", 0, &cmd_index) != TCL_OK) {
	return TCL_ERROR;
    }

    switch (cmd_index) {
    case DESTROY:
        if (objc != 2) {
            Tcl_WrongNumArgs(ip, 2, objv, NULL);
            return TCL_ERROR;
        }
        Tcl_DeleteCommandFromToken(ip, instance->cmd_token);
        /* Note instance will be freed by the command destructor */
        break;

    case GET:
        return ta_rng_get_method(instance, ip, objc-2, objv+2);

    case SEED:
        if (objc != 4) {
            Tcl_WrongNumArgs(ip, 2, objv, "SEED1 SEED2");
            return TCL_ERROR;
        } else {
            Tcl_WideInt seed1;
            Tcl_WideInt seed2;
            if (Tcl_GetWideIntFromObj(ip, objv[2], &seed1) != TCL_OK ||
                Tcl_GetWideIntFromObj(ip, objv[3], &seed2) != TCL_OK)
                return TCL_ERROR;
            pcg32_srandom_r(&instance->rng.rng[0], seed1, 0xf0f0f0f0);
            pcg32_srandom_r(&instance->rng.rng[1], seed2, 0x0f0f0f0f);
        }
        break;
    }

    return TCL_OK;
}

static void ta_rng_instance_destructor(ClientData cdata)
{
    Tcl_Free(cdata);
}

void ta_rng_destructor(ClientData cdata)
{
    Tcl_Free(cdata);
}

TCL_RESULT ta_rng_cmd(ClientData cdata, Tcl_Interp *ip,
                       int objc, Tcl_Obj *const objv[])
{
    ta_cmd_counter *counterP = (ta_cmd_counter *) cdata;
    ta_rng_instance_t *instance = NULL;
    Tcl_Obj *resultObj = NULL;
    static const char *cmds[] = {"new", "create", NULL};
    enum cmd { NEW, CREATE };
    int cmd_index;
    int cmd_is_create;

    if (objc < 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "new|create args...");
        return TCL_ERROR;
    }
    if (Tcl_GetIndexFromObj(ip, objv[1], &cmds, "subcommand", 0, &cmd_index) != TCL_OK)
        return TCL_ERROR;

    if (cmd_index == NEW) {
        if (objc < 3 || objc > 5) {
            Tcl_WrongNumArgs(ip, 1, objv, "new TYPE ?LOWBOUND ?HIGHBOUND??");
            return TCL_ERROR;
        }
        cmd_is_create = 0;
        Tcl_Namespace *ns = Tcl_GetCurrentNamespace(ip);
        *counterP += 1;
        /* ns->name is "" for global namespace */
        resultObj = Tcl_ObjPrintf("%s%s%lld", ns->fullName, ns->name[0] ? "::rng" : "rng", *counterP);
    } else {
        if (objc < 4 || objc > 6) {
            Tcl_WrongNumArgs(ip, 1, objv, "create OBJNAME TYPE ?LOWBOUND ?HIGHBOUND??");
            return TCL_ERROR;
        }
        cmd_is_create = 1;
        const char *name = Tcl_GetString(objv[2]);
        if (name[0] == ':' && name[1] == ':') {
            /* Fully qualified name */
            resultObj = objv[2];
        } else {
            /* Name is not fully qualified */
            Tcl_Namespace *ns = Tcl_GetCurrentNamespace(ip);
            /* ns->name is "" for global namespace */
            resultObj = Tcl_ObjPrintf("%s%s%s", ns->fullName, ns->name[0] ? "::" : "", name);
        }
    }

    instance = (ta_rng_instance_t *) Tcl_Alloc(sizeof(*instance));
    instance->bounded = 0;

    /* NOTE: resultObj has not been IncrRefCount'ed. */
    if (ta_parse_type(ip, objv[2+cmd_is_create], &instance->rtype) != TCL_OK)
        goto error;
    switch (instance->rtype) {
    case TA_BOOLEAN: case TA_BYTE: case TA_INT:
    case TA_UINT: case TA_WIDE: case TA_DOUBLE:
        break;
    default:
        ta_invalid_op_for_type(ip, instance->rtype);
        goto error;
    }

    tcol_random_init(&instance->rng);
    /* Note TA_BOOLEAN do not obey bounds */
    if (objc > (3+cmd_is_create) && instance->rtype != TA_BOOLEAN) {
        instance->bounded = 1;
        if (ta_value_from_obj(ip, objv[3+cmd_is_create], instance->rtype, &instance->lbound) != TCL_OK)
            goto error;
        if (objc > (4+cmd_is_create)) {
            if (ta_value_from_obj(ip, objv[4+cmd_is_create], instance->rtype, &instance->ubound) != TCL_OK)
                goto error;
        }
        if (ta_rng_fixup_bounds(ip,
                                &instance->lbound, &instance->ubound,
                                objc <= (4+cmd_is_create)) != TCL_OK)
            goto error;
    } else if (instance->rtype == TA_BYTE) {
        instance->bounded = 1;
        instance->lbound.type = instance->ubound.type = TA_BYTE;
        instance->lbound.ucval = 0;
        instance->ubound.ucval = 255;
    }


    instance->cmd_token = Tcl_CreateObjCommand(ip,
                                               Tcl_GetString(resultObj),
                                               ta_rng_instance_cmd,
                                               (ClientData)instance,
                                               ta_rng_instance_destructor);

    Tcl_SetObjResult(ip, resultObj);
    return TCL_OK;

error:
    if (resultObj)
        Tcl_DecrRefCount(resultObj);
    if (instance)
        Tcl_Free((char *)instance);
    return TCL_ERROR;
}

TCL_RESULT tcol_shuffle(Tcl_Interp *ip, ta_rng_t *prng, Tcl_Obj *tcol)
{
    thdr_t *thdr;
    span_t *span;
    Tcl_Size n;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if (tcol_convert(ip, tcol) != TCL_OK)
        return TCL_ERROR;;

    thdr = OBJTHDR(tcol);
    if (thdr->type == TA_BOOLEAN)
        return ta_invalid_op_for_type(ip, TA_BOOLEAN);
    span = OBJTHDRSPAN(tcol);

    if (thdr_shared(thdr) || span) {
        /* Construct shuffle into a new column.
         * "Inside-out" version of Fisher-Yates shuffle
         */
#define SHUFFLECOPY(type_)                                                    \
    do {                                                                      \
        Tcl_Size i, j;                                                        \
        type_ *from = THDRELEMPTR(thdr, type_, start);                        \
        type_ *to   = THDRELEMPTR(thdr2, type_, 0);                           \
        for (i = 0; i < n; ++i) {                                             \
            j     = pcg32_boundedrand_r(&prng->rng[0], i + 1); /* j in 0:i */ \
            to[i] = to[j];                                                    \
            to[j] = from[i];                                                  \
        }                                                                     \
    } while (0)

        thdr_t *thdr2;
        Tcl_Size start;
        if (span) {
            start = span->first;
            n = span->count;
        } else {
            start = 0;
            n = thdr->used;
        }
        thdr2 = thdr_alloc(ip, thdr->type, n);
        thdr2->used = n; /* Init BEFORE thdr_incr_*_refs called below */
        switch (thdr->type) {
        case TA_BYTE: SHUFFLECOPY(unsigned char); break;
        case TA_INT: /* FALLTHRU */
        case TA_UINT: SHUFFLECOPY(int); break;
        case TA_WIDE: SHUFFLECOPY(Tcl_WideInt); break;
        case TA_DOUBLE: SHUFFLECOPY(double); break;
        case TA_ANY: 
            SHUFFLECOPY(Tcl_Obj *);
            thdr_incr_obj_refs(thdr2, 0, n);
            break;
        case TA_STRING:
            SHUFFLECOPY(Tcl_Obj *);
            thdr_incr_tas_refs(thdr2, 0, n);
            break;
        default:
            ta_type_panic(thdr->type);
            break;
        }
        tcol_replace_intrep(tcol, thdr2, NULL);
    } else {
        /* Shuffle in place - Fisher-Yates shuffle */
#define SHUFFLE(type_)                                                  \
        do {                                                            \
            Tcl_Size i, j;                                                   \
            type_ temp;                                                 \
            type_ *p = THDRELEMPTR(thdr, type_, 0);                     \
            for (i = n; i > 1; --i) {                                   \
                j = pcg32_boundedrand_r(&prng->rng[0], i); /* j in 0:(i-1) */ \
                temp = p[j];                                            \
                p[j] = p[i-1];                                          \
                p[i-1] = temp;                                          \
            }                                                           \
        } while (0)
        n = thdr->used;
        switch (thdr->type) {
        case TA_BYTE: SHUFFLE(unsigned char); break;
        case TA_INT: /* FALLTHRU */
        case TA_UINT: SHUFFLE(int); break;
        case TA_WIDE: SHUFFLE(Tcl_WideInt); break;
        case TA_DOUBLE: SHUFFLE(double); break;
        case TA_ANY: /* FALLTHRU */
        case TA_STRING: SHUFFLE(Tcl_Obj *); break;
        default:
            ta_type_panic(thdr->type);
            break;
        }
        Tcl_InvalidateStringRep(tcol);
    }

    return TCL_OK;
}

TCL_RESULT tcol_shuffle_cmd(ClientData cdata, Tcl_Interp *ip,
                            int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    TCL_RESULT res;

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }

    tcol = objv[1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    res = tcol_shuffle(ip, (ta_rng_t *)cdata, tcol);
    return ta_return_result(ip, res, tcol);
}

TCL_RESULT tcol_vshuffle_cmd(ClientData cdata, Tcl_Interp *ip,
                            int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol, *ovar;
    TCL_RESULT res;

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }

    ovar = objv[1];
    tcol = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    res = tcol_shuffle(ip, (ta_rng_t *)cdata, tcol);
    TA_ASSERT(res != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, res, ovar, tcol);
}
