/*
 * Copyright (c) 2012-2024 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#if __GNUC__
#  if __GNUC_STDC_INLINE__
#    define TA_INLINE extern inline
#  else
#    define TA_INLINE
#  endif
#endif

#include "tarray.h"

/* Must match definitions in tarray.h ! */
const char *g_type_tokens[] = {
    "<typeerror!>", /* 0 is not a valid tarray type. */
    "uint",
    "int",
    "wide",
    "double",
    "byte",
    "any",
    "string",
    "boolean",
    NULL
};

/*
 * A TArray column is a Tcl_Obj type used for densely storing arrays
 * of a specific type. The Tcl_Obj internal representation points
 * to a thdr_t internal representation of the column. 
 */
static void tcol_type_dup(Tcl_Obj *psrc, Tcl_Obj *pdst);
static void tcol_type_free_intrep(Tcl_Obj *o);
static void tcol_type_update_string(Tcl_Obj *o);
struct Tcl_ObjType ta_column_type = {
    "tarray_column",
    tcol_type_free_intrep,
    tcol_type_dup,
    tcol_type_update_string,
    NULL,     /* jenglish advises to keep this NULL */
};

/*
 * A Tcl object to parse tarray index tokens and cache the result so
 * we do not parse "end" or "last" repeatedly. We could actually use the
 * more sophisticated Tcl equivalent but unfortunately that is not exported
 * and moreover in Tcl 9 is quirky in end conditions (encoding of end+2,
 * end+3 etc. will all map to the same value)
 */
static int ta_indexobj_from_any(Tcl_Interp *interp, Tcl_Obj *o);
static void ta_indexobj_update_string(Tcl_Obj *);
struct Tcl_ObjType ta_index_type = {
    "tarray_index",
    NULL, /* No need for a free proc */
    NULL,
    ta_indexobj_update_string,
    NULL, /* convert from any - jenglish advises to keep this NULL */
};
TA_INLINE Tcl_Size ta_indexobj_get_offset(Tcl_Obj *o) {
    TA_ASSERT(o->typePtr == &ta_index_type);
    return (Tcl_Size) o->internalRep.wideValue;
}
TA_INLINE void ta_indexobj_shimmer(Tcl_Obj *o, Tcl_Size off) {
    if (o->typePtr && o->typePtr->freeIntRepProc)
        o->typePtr->freeIntRepProc(o);
    o->typePtr = &ta_index_type;
    o->internalRep.wideValue = off;
}

/*
 * indexobj routines
 */
static void ta_indexobj_update_string(Tcl_Obj *o)
{
    char buffer[TCL_INTEGER_SPACE + 5];
    Tcl_Size len;
    len = ta_indexobj_get_offset(o);
    if (len == 0)
        strcpy(buffer, "end");
    else if (len > 0)
        snprintf(buffer, sizeof(buffer), "end+%" TCL_SIZE_MODIFIER "d", len);
    else
        snprintf(buffer, sizeof(buffer), "end%" TCL_SIZE_MODIFIER "d", len);

    len = (Tcl_Size) ta_strlen(buffer);
    o->bytes = ckalloc(len+1);
    memcpy(o->bytes, buffer, len+1);
    o->length = len;
}

/*
 * Look for a string of the form "end[+-]offset" and convert it to an
 * internal representation holding the offset.
 * Returns TCL_OK if ok, TCL_ERROR if the string was badly formed.
 * If interp is not NULL, stores an error message in the interpreter
 * result.
 * NOTE: Naming is a little unfortunate - the "from_any" does not refer
 * to our TA_ANY
 */
int ta_indexobj_from_any(
    Tcl_Interp *interp,		/* Tcl interpreter or NULL */
    Tcl_Obj *o)		/* Pointer to the object to parse */
{
    /* Copied from Tcl's SetEndOffsetFromAny which is not public */
    Tcl_Size offset;	/* Offset in the "end-offset" expression */
    const char *bytes;	/* String rep of the object */
    Tcl_Size length;	/* Length of the object's string rep */

    if (o->typePtr == &ta_index_type) {
	return TCL_OK;
    }

    /* Check for a string rep of the right form. */
    bytes = Tcl_GetStringFromObj(o, &length);
    if ((*bytes != 'e') ||
        (strncmp(bytes, "end", (size_t)((length > 3) ? 3 : length)) != 0)) {
        goto error_handler;
    }

    /* Convert the string rep. */

    if (length <= 3) {
        offset = 0;
    } else if ((length > 4) && ((bytes[3] == '-') || (bytes[3] == '+'))) {
        /*
         * This is our limited string expression evaluator. Pass everything
         * after "end-" to Tcl_GetInt, then reverse for offset. Tcl_GetInt
         * accepts whitespace so check for next char to avoid it
         */
        if (bytes[4] < '0' || bytes[4] > '9')
            goto error_handler;
#if TCL_MAJOR_VERSION < 9
        if (Tcl_GetInt(interp, bytes + 4, &offset) != TCL_OK) {
            return TCL_ERROR;
        }
#else
        void *valueP;
        int number_type;
        Tcl_WideInt wide;
        if (Tcl_GetNumber(interp, bytes + 4, -1, &valueP, &number_type)
                != TCL_OK
            || number_type != TCL_NUMBER_INT)
            goto error_handler;
        wide = *(Tcl_WideInt *)valueP;
        if (wide > TCL_SIZE_MAX)
            goto error_handler;
        offset = (Tcl_Size)wide;
#endif
        if (bytes[3] == '-') {
            offset = -offset;
        }
    } else {
        /* Conversion failed. Report the error. */
        goto error_handler;
    }

    /* The conversion succeeded. Free the old internal rep and set the new */
    ta_indexobj_shimmer(o, offset);

    return TCL_OK;

error_handler:
    if (interp != NULL) {
        Tcl_SetObjResult(interp,
                         Tcl_ObjPrintf(
                             "bad index \"%s\": must be end?[+-]integer?",
                             bytes));
        Tcl_SetErrorCode(interp, "TCL", "VALUE", "INDEX", NULL);
    }
    return TCL_ERROR;
}

/* Panics on consistency check failure. int return value so it can
 be called from TA_ASSERT */
int thdr_check(Tcl_Interp *ip, thdr_t *thdr)
{
    Tcl_Obj **objPP;
    tas_t **pptas;
    Tcl_Size i;

    if (!ta_full_validation)
        return 1;

    if (thdr->used > thdr->usable) {
        Tcl_Panic("thdr->used (%" TCL_SIZE_MODIFIER "d) > thdr->usable (%" TCL_SIZE_MODIFIER "d)", thdr->used, thdr->usable);
    }

    switch (thdr->type) {
    case TA_BOOLEAN:
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_BYTE:
        break;
    case TA_ANY:
        objPP = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        for (i = 0; i < thdr->used; ++i) {
            if (objPP[i]->refCount < 1)
                Tcl_Panic("thdr TA_ANY element ref count (%" TCL_SIZE_MODIFIER "d) < 1", objPP[i]->refCount);
        }
        break;
    case TA_STRING:
        pptas = THDRELEMPTR(thdr, tas_t *, 0);
        for (i = 0; i < thdr->used; ++i) {
            if (pptas[i]->nrefs == 0)
                Tcl_Panic("thdr TA_STRING: element ref count == 0");
        }
        if (thdr->lookup != TAS_LOOKUP_INVALID_HANDLE) {
            /* TBD - verify hash table */
        }
        break;
    default:
        ta_type_panic(thdr->type);
    }

    return 1;
}


int tcol_check(Tcl_Interp *ip, Tcl_Obj *tcol)
{
    thdr_t *thdr;

    if (!ta_full_validation)
        return 1;

    if (tcol_convert(ip, tcol) != TCL_OK || ! tcol_affirm(tcol))
        Tcl_Panic("Tcl_Obj is not a column");

    thdr = tcol_thdr(tcol);
    if (thdr == NULL)
        Tcl_Panic("NULL thdr in Tcl_Obj");
    if (thdr->nrefs < 1)
        Tcl_Panic("Column thdr->nrefs (%" TCL_SIZE_MODIFIER "d) < 1", thdr->nrefs);
    
    /* Note even if the tcol's OBJTHDRSPAN() field is non-null,
       the WHOLE thdr must still be valid */
    thdr_check(ip, thdr);

    return 1;
}

/* Wrapper around Tcl_GetIndexFromObj to prevent unnecessary shimmering */
int ta_opt_from_obj(Tcl_Interp *ip, Tcl_Obj *o, const char *const *ptable,
                    const char *msg, int flags, int *popt)
{
    /* If already known to be a column or table, don't shimmer it */
    if (tcol_affirm(o) || table_affirm(o)) {
        /* Whole point was to not generate a string rep so error
           message has to be generic */
        return ta_invalid_opt_error(ip, NULL);
    }
    return Tcl_GetIndexFromObj(ip, o, ptable, msg, flags, popt);
}

const char *ta_type_string(int tatype)
{
    if (tatype < (sizeof(g_type_tokens)/sizeof(g_type_tokens[0]) - 1))
        return g_type_tokens[tatype];
    else
        return "<invalid>";
}

/* Generally call the inline ta_collection_type wrapper instead of this */
enum ta_collection_type_e ta_detect_collection_type(Tcl_Obj *o)
{
    Tcl_Size len;
    if (tcol_convert(NULL, o) == TCL_OK)
        return TA_COLL_COLUMN;
    if (table_convert(NULL, o) == TCL_OK)
        return TA_COLL_TABLE;
    if (Tcl_ListObjLength(NULL, o, &len) == TCL_OK)
        return TA_COLL_LIST;
    return TA_COLL_NONE;
}

static void tcol_type_free_intrep(Tcl_Obj *o)
{
    thdr_t *thdr;
    span_t *span;

    TA_ASSERT(tcol_affirm(o));

    thdr = OBJTHDR(o); 
    span = OBJTHDRSPAN(o); 
    TA_ASSERT(thdr);

    if (span)
        span_decr_refs(span);
    thdr_decr_refs(thdr);
    OBJTHDR(o) = NULL;
    OBJTHDRSPAN(o) = NULL;
    o->typePtr = NULL;
}

static void tcol_type_dup(Tcl_Obj *osrc, Tcl_Obj *odst)
{
    TA_ASSERT(tcol_affirm(osrc));
    TA_ASSERT(OBJTHDR(osrc) != NULL);
        
    tcol_set_intrep(odst, OBJTHDR(osrc), OBJTHDRSPAN(osrc));
}

/* Called to generate a string implementation for tarray columns of
   type TA_ANY/TA_STRING or and tables whose elements are do not
   have a max size
*/
void ta_update_string_for_variable_element_size(Tcl_Obj *o)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
    union {
        Tcl_Obj **objv;      /* Used for TA_ANY */
        tas_t **pptas;              /* Used for TA_STRING */
    } u;
    Tcl_Size objc, span_start;
    thdr_t *thdr;
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    Tcl_Size i;
    Tcl_Size length, lengths[LOCAL_SIZE], *lengthPtr = NULL;
    size_t bytesNeeded;
    const char *elem;
    char *dst;
    int quote_hash;
    Tcl_Obj *onames = NULL;
    const char *names = NULL;
    Tcl_Size names_len;
    unsigned char tatype;
    span_t *span;

    TA_ASSERT(o->typePtr == &ta_column_type || o->typePtr == &ta_table_type);

    /* TBD - warn in log if converting a large tarray to string */
    
    thdr = OBJTHDR(o);
    objc = thdr->used;
    span_start = 0;
    if (o->typePtr == &ta_column_type && (span = OBJTHDRSPAN(o)) != NULL) {
        objc = span->count;
        span_start = span->first;
        TA_ASSERT(span_start+objc <= thdr->used);
    }

    tatype = thdr->type;
    TA_ASSERT(tatype == TA_ANY || tatype == TA_STRING);
    if (tatype == TA_ANY)
        u.objv = THDRELEMPTR(thdr, Tcl_Obj *, span_start);
    else
        u.pptas = THDRELEMPTR(thdr, tas_t *, span_start);

    /*
     * Pass 1: estimate space, gather flags.
     */

    if (objc <= LOCAL_SIZE) {
        flagPtr = localFlags;
        lengthPtr = lengths;
    } else {
        /*
         * We know objc <= TA_MAX_OBJC, so this is safe.
         */
        flagPtr = (int *) TA_ALLOCMEM(objc * sizeof(int));
        /* For TA_ANY or table, we can get length through 
         * Tcl_GetStringFromObj as and when required so do
         * not need to allocate lengthPtr.
         */
        if (tatype == TA_STRING)
            lengthPtr = (Tcl_Size *) TA_ALLOCMEM(objc * sizeof(Tcl_Size));
    }

    bytesNeeded = ta_strlen(o->typePtr->name);
    if (o->typePtr == &ta_table_type) {
        /* If a table, need to add on column names */
        TA_ASSERT(OBJCOLNAMES(o));
        onames = table_column_names(o);
        names = Tcl_GetStringFromObj(onames, &names_len);
        bytesNeeded += 1 + 1 + names_len + 1; /* With lead space, enclosing {} */
    } else {
        /* If a column, need to add the column type */
        names_len = ta_strlen(g_type_tokens[tatype]);
        bytesNeeded += 1 + names_len;/* With lead space */
    }

    bytesNeeded += 1 + 1 + 1; /* Lead space and enclosing {} for objv */

    quote_hash = TCL_DONT_QUOTE_HASH; /* TBD - is this right? */
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = quote_hash;
        quote_hash = 0;
        if (tatype == TA_ANY)
            elem = Tcl_GetStringFromObj(u.objv[i], &length);
        else {
            elem = u.pptas[i]->s;
            lengthPtr[i] = ta_strlen(u.pptas[i]->s); /* Will need later */
            length = lengthPtr[i];
        }
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if ((((size_t)1) << (sizeof(bytesNeeded)*CHAR_BIT - 1)) & bytesNeeded)
            ta_string_overflow_panic("ta_update_string_for_variable_element_size");
    }
    if ((bytesNeeded + objc + 1) > INT_MAX)
        ta_string_overflow_panic("ta_update_string_for_variable_element_size");

    /* For separators and terminating null. Note this may result in one extra
       byte being allocated since the separator is not needed for last element.
       However, the +1 is needed to take care of the case where objc==0
    */
    bytesNeeded += objc + 1;

    /*
     * Pass 2: copy into string rep buffer.
     */

    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    o->bytes = ckalloc(bytesNeeded);
    dst = o->bytes;
    strcpy(dst, o->typePtr->name);
    dst += strlen(dst);
    if (o->typePtr == &ta_table_type) {
        *dst++ = ' ';
        *dst++ = '{';
        memcpy(dst, names, names_len);
        dst += names_len;
        *dst++ = '}';
    } else {
        *dst++ = ' ';
        memcpy(dst, g_type_tokens[tatype], names_len);
        dst += names_len;
    }

    *dst++ = ' ';
    *dst++ = '{';
    /* TBD - handle objc==0 case. Huh? Isn't that handled in the else part ? */
    if (objc) {
        quote_hash = TCL_DONT_QUOTE_HASH;
        for (i = 0; i < objc; i++) {
            flagPtr[i] |= quote_hash;
            quote_hash = 0;
            if (tatype == TA_ANY)
                elem = Tcl_GetStringFromObj(u.objv[i], &length);
            else {
                elem = u.pptas[i]->s;
                length = lengthPtr[i];
            }
            dst += Tcl_ConvertCountedElement(elem, length, dst, flagPtr[i]);
            *dst++ = ' ';
            /* Assert <, not <= because need to add terminating "}" */
            TA_ASSERT(dst < (o->bytes + bytesNeeded));
        }
        dst[-1] = '}';
    } else
        *dst++ = '}';
    *dst = '\0';
    TA_ASSERT(dst < (o->bytes + bytesNeeded));
    o->length = (Tcl_Size) (dst - o->bytes);

    if (flagPtr != localFlags) {
        TA_FREEMEM((char *) flagPtr);
    }
    if (lengthPtr && lengthPtr != lengths) {
        TA_FREEMEM((char *) lengthPtr);
    }
    if (onames)
        Tcl_DecrRefCount(onames);
}

static void tcol_type_update_string(Tcl_Obj *o)
{
    size_t i, n, count, span_start;
    size_t allocated, unused, min_needed, prefix_len;
    char *cP;
    size_t max_elem_space;  /* Max space to print one element including
                                 either terminating null or space */
    thdr_t *thdr;
    span_t *span;

    max_elem_space = 100; /* Unnecessary, keep gcc happy for all code paths */

    TA_ASSERT(tcol_affirm(o));

    thdr = OBJTHDR(o);
    TA_ASSERT(thdr->type < (sizeof(g_type_tokens)/sizeof(g_type_tokens[0]) - 1));
    count = thdr->used;
    span_start = 0;
    if ((span = OBJTHDRSPAN(o)) != NULL) {
        count = span->count;
        span_start = span->first;
        TA_ASSERT(span_start+count <= (size_t) thdr->used);
    }

    o->bytes = NULL;

    prefix_len = ta_strlen(o->typePtr->name)
        + 1            /* Space */
        + ta_strlen(g_type_tokens[thdr->type])
        + 2;                         /* Start of list " {" */
    min_needed = prefix_len + 1 + 1;            /* Trailing "}" and null */

    if (count == 0) {
        /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
           defined as ckalloc */
        cP = ckalloc(min_needed);
        o->bytes = cP;
        snprintf(cP, min_needed, "%s %s {}", o->typePtr->name,
                 g_type_tokens[thdr->type]);
        o->length = (Tcl_Size) min_needed - 1;
        return;
    }

    /* Code below based on count > 0 else terminating \0 will blow memory */

    /*
     * When output size cannot be calculated exactly, we allocate using
     * some estimate based on the type.
     */

    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            /*
             * Special case Boolean since we know exactly how many chars will
             * be required
             */
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            size_t span_end;

            /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
               defined as ckalloc */
            cP = ckalloc(min_needed + 2*count - 1);
            n = snprintf(cP, min_needed, "%s %s {", o->typePtr->name,
                         g_type_tokens[TA_BOOLEAN]);
            TA_ASSERT(n > 0 && n < min_needed);
            o->bytes = cP;
            cP += n;
            span_end = span_start + count;
            for (i = span_start; i < span_end; ++i) {
                *cP++ = ba_get(baP, (ba_off_t) i) ? '1' : '0';
                *cP++ = ' ';
            }
            cP[-1] = '}';
            *cP = '\0';
            o->length = (Tcl_Size) (cP - o->bytes);
            TA_ASSERT((size_t) o->length < (min_needed + 2*count - 1));
        }
        return;

    case TA_ANY:
    case TA_STRING:
        /* TBD - can't we check for this much earlier in this function? */
        ta_update_string_for_variable_element_size(o);
        return;

    case TA_UINT:
    case TA_INT:
        TA_ASSERT(sizeof(int) == 4); /* So max string space needed is 11 */
        max_elem_space = 11;
        break;
    case TA_WIDE:
        max_elem_space = TCL_INTEGER_SPACE;
        break;
    case TA_DOUBLE:
        max_elem_space = TCL_DOUBLE_SPACE;
        break;
    case TA_BYTE:
        max_elem_space = 3;
        break;
    default:
        ta_type_panic(thdr->type);
    }

    /*
     * Assume an element averages half max space with room for at
     * least one max element. Note max_elem_space includes trailing ' '
     */
    allocated = min_needed + max_elem_space + ((max_elem_space + 1)/2)*count;
    unused = allocated - prefix_len;
    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    cP = ckalloc(allocated);
    o->bytes = cP;
    snprintf(cP, prefix_len+1, "%s %s {",
             o->typePtr->name, g_type_tokens[thdr->type]);
    TA_ASSERT(strlen(cP) == prefix_len);
    cP += prefix_len;
    min_needed = max_elem_space + 2; /* space or terminating "}" and null */
    for (i = 0; i < count; ) {
        if (unused < min_needed) {
            n = allocated - unused; /* Used space */
            /* Increase assuming average space taken so far (roughly) */
            TA_ASSERT(i != 0);
            allocated += min_needed + (count - i) * (n/i);
            o->bytes = ckrealloc(o->bytes, allocated);
            cP = n + (char *) o->bytes;
            unused = allocated - n;
        }
        /*
         * We nest loops for performance by minimizing switch jumps
         * At top of nested loops below, there is room for at least one elem
         */
        switch (thdr->type) {
        case TA_UINT:
        case TA_INT:
            {
                int *intP = THDRELEMPTR(thdr, int, (span_start+i));
                char *fmt = thdr->type == TA_UINT ? "%u" : "%d";
                while (i < count && unused >= min_needed) {
                    n = snprintf(cP, unused, fmt, *intP++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_WIDE:
            {
                Tcl_WideInt *pwide = THDRELEMPTR(thdr, Tcl_WideInt, (span_start+i));
                while (i < count && unused >= min_needed) {
                    n = snprintf(cP, unused, "%" TCL_LL_MODIFIER "d", *pwide++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_DOUBLE:
            /* Do not use snprintf because of slight difference
               it does not include decimal point for whole ints. For
               consistency with Tcl, use Tcl_PrintDouble instead */
            {
                double *pdbl = THDRELEMPTR(thdr, double, (span_start+i));
                while (i < count && unused >= min_needed) {
                    Tcl_PrintDouble(NULL, *pdbl++, cP);
                    n = strlen(cP);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        case TA_BYTE:
            {
                unsigned char *ucP = THDRELEMPTR(thdr, unsigned char, (span_start+i));
                while (i < count && unused >= min_needed) {
                    n = snprintf(cP, unused, "%u", *ucP++);
                    TA_ASSERT(n > 0 && n < unused);
                    ++i;
                    cP += n;
                    *cP++ = ' ';
                    unused -= n+1;
                }
            }
            break;
        }
    }

    TA_ASSERT(unused >=1 );
    cP[-1] = '}';         /* Terminate list */
    *cP = '\0';
    o->length = (Tcl_Size) (cP - o->bytes); /* Terminating null not included in length */

    /*  TBD - Only shrink array if unused space is comparatively too large */
    unused = allocated - (o->length + 1);
    if (unused > (allocated / 8) && unused > 20)
        o->bytes = ckrealloc(o->bytes, o->length + 1);
    return;
}

/* We cannot rely on Tcl_GetWideIntFromObj because that does not signal an
   error on signed overflow
*/
TCL_RESULT ta_get_wide_from_obj(Tcl_Interp *ip, Tcl_Obj *o, Tcl_WideInt *pwide)
{
    Tcl_WideInt wide;
    Tcl_Size n;
    TCL_RESULT status;
    /*
     * Avoid shimmering in case of wrong arguments as shimmering from
     * tables/columns can be VERY expensive 
     * TBD - for doubles, should we do a conversion to wide and
     * see if pure integer ?
     */
    if (o->typePtr == &ta_column_type ||
        o->typePtr == &ta_table_type ||
        o->typePtr == g_tcl_dict_type_ptr ||
        o->typePtr == g_tcl_double_type_ptr ||
        (o->typePtr == g_tcl_list_type_ptr &&
         (Tcl_ListObjLength(NULL, o, &n) != TCL_OK || n != 1))) {
        return ta_value_type_error(ip, o, TA_WIDE);
    }

    status = Tcl_GetWideIntFromObj(ip, o, &wide);
    if (status != TCL_OK)
        return status;
#if TCL_MAJOR_VERSION > 8
    /* TBD - check that Tcl 9 does in fact check overflows */
    if (pwide)
        *pwide = wide;
    return TCL_OK;
#else

    char c, *p;

    /* To deal with Tcl8's lack of overflow checking, see if the
     * sign matches the string representation if there is one.
     */
    if (o->bytes == NULL) {
        /* No string rep so can't check for integer overflow */
        if (pwide)
            *pwide = wide;
        return TCL_OK;
    }

    /* 
     * Check for overflow to prevent assignment of unsigned 64-bit 
     * values that do not fit in 64 bit signed integers. However, we
     * want hexadecimal, octal and binary values that do not have
     * an explicit sign to be treated as a pure bit pattern that can
     * be directly assigned without checking for overflow. Thus
     * 0x8000000000000000 is allowed but 9223372036854775808 is treated
     * as an overflow.
     */
    p = o->bytes;
    while (((c = *p) != '\0') && isascii(c) && isspace(c))
        ++p;
    if (*p == '0') {
        /* Octal, hex, binary all begin with 0. Always permit them */
    } else {
        if ((wide > 0 && *p == '-') || (wide < 0 && *p != '-')) {
            return ta_integer_overflow_obj_error(ip, "64-bit integer", o);
        }
    }
    if (pwide)
        *pwide = wide;
    return TCL_OK;
#endif
}

TCL_RESULT ta_get_uint_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned int *pui)
{
    Tcl_WideInt wide;
    if (ta_get_wide_from_obj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < 0 || wide > 0xFFFFFFFF)
        return ta_integer_overflow_error(ip, "unsigned 32-bit integer", wide);
    *pui = (unsigned int) wide;
    return TCL_OK;
}

/* We cannot rely on Tcl_GetIntFromObj because that does not signal an
   error on signed overflow (i.e. 0x80000000 is treated as a valid integer)
   TBD - is this still true with Tcl 9?
*/
TCL_RESULT ta_get_int_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int *pi)
{
    Tcl_WideInt wide;
    if (ta_get_wide_from_obj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < INT_MIN || wide > INT_MAX)
        return ta_integer_overflow_error(ip, "32-bit integer", wide);
    *pi = (int) wide;
    return TCL_OK;
}

/* Get a 31-bit positive integer value - for arguments that represent counts */
TCL_RESULT ta_get_count_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int zero_allowed, Tcl_Size *pi)
{
    Tcl_Size val;
    if (Tcl_GetSizeIntFromObj(ip, o, &val) != TCL_OK)
        return TCL_ERROR;
    if (val < 0 || (val == 0 && ! zero_allowed))
        return ta_bad_count_error(ip, val);
    *pi = val;
    return TCL_OK;
}

TCL_RESULT ta_get_uint8_from_obj(Tcl_Interp *ip, Tcl_Obj *o, uint8_t *pb)
{
    Tcl_WideInt wide;
    if (ta_get_wide_from_obj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < 0 || wide > UINT8_MAX)
        return ta_integer_overflow_error(ip, "unsigned 8-bit integer", wide);
    *pb = (unsigned char) wide;
    return TCL_OK;
}

TCL_RESULT ta_get_int8_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int8_t *pb)
{
    Tcl_WideInt wide;
    if (ta_get_wide_from_obj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < INT8_MIN || wide > INT8_MAX)
        return ta_integer_overflow_error(ip, "8-bit integer", wide);
    *pb = (int8_t) wide;
    return TCL_OK;
}

TCL_RESULT ta_get_wide_from_string(Tcl_Interp *ip, const char *s, Tcl_WideInt *pi)
{
    TCL_RESULT status;
    /* TBD - optimize using _strtoi64 or Tcl_GetNumber */
    Tcl_Obj *o = Tcl_NewStringObj(s, -1);
    status = ta_get_wide_from_obj(ip, o, pi);
    Tcl_DecrRefCount(o);
    return status;
}

TCL_RESULT ta_get_int_from_string(Tcl_Interp *ip, const char *s, int *pi)
{
    Tcl_WideInt wide;
    /* TBD - optimize using _strtoi64 or Tcl_GetInt */
    if (ta_get_wide_from_string(ip, s, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < INT_MIN || wide > INT_MAX)
        return ta_integer_overflow_error(ip, "32-bit integer", wide);
    *pi = (int) wide;
    return TCL_OK;
}

TCL_RESULT ta_get_uint_from_string(Tcl_Interp *ip, const char *s, unsigned int *pi)
{
    Tcl_WideInt wide;
    if (ta_get_wide_from_string(ip, s, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < 0 || wide > UINT_MAX)
        return ta_integer_overflow_error(ip, "32-bit unsigned integer", wide);
    *pi = (unsigned int) wide;
    return TCL_OK;
}

TCL_RESULT ta_get_uint8_from_string(Tcl_Interp *ip, const char *s, unsigned char *pi)
{
    Tcl_WideInt wide;
    if (ta_get_wide_from_string(ip, s, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < 0 || wide > UCHAR_MAX)
        return ta_integer_overflow_error(ip, "8-bit unsigned integer", wide);
    *pi = (unsigned char) wide;
    return TCL_OK;
}


TCL_RESULT ta_get_boolean_from_string(Tcl_Interp *ip, const char *s, int *pi)
{
    TCL_RESULT status;
    /* TBD - optimize using Tcl_GetBoolean */
    Tcl_Obj *o = Tcl_NewStringObj(s, -1);
    status = Tcl_GetBooleanFromObj(ip, o, pi);
    Tcl_DecrRefCount(o);
    return status;
}

TCL_RESULT ta_get_double_from_string(Tcl_Interp *ip, const char *s, double *pi)
{
    TCL_RESULT status;
    /* TBD - optimize using Tcl_GetDouble */
    Tcl_Obj *o = Tcl_NewStringObj(s, -1);
    status = Tcl_GetDoubleFromObj(ip, o, pi);
    Tcl_DecrRefCount(o);
    return status;
}

void thdr_lookup_build(thdr_t *thdr, span_t *span)
{
    Tcl_Size i, count;
    tas_t **pptas;
    tas_lookup_t lookup;

    TA_ASSERT(thdr->type == TA_STRING);
    thdr_lookup_free(thdr);
    lookup = thdr_lookup_init(thdr);
    i = span ? span->first : 0;
    count = span ? span->count : thdr->used;
    pptas = THDRELEMPTR(thdr, tas_t *, i);
    while (i < count) {
        tas_lookup_add(lookup, *pptas, i);
        ++i;
        ++pptas;
    }
}


/* Increments the ref counts of tas_t elements in a tarray making sure not
   to run past end of array. Note the elements may be replaced if
   ref counts overflow.
*/
void thdr_incr_tas_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count)
{
    TA_ASSERT(thdr->type == TA_STRING);
    if ((first + count) > thdr->used)
        count = thdr->used - first;
    /* Note count might even be negative, check */
    if (count > 0) {
        tas_t **pptas, **end;
        pptas = THDRELEMPTR(thdr, tas_t *, first);
        end = pptas + count;
        while (pptas < end) {
            *pptas = tas_ref(*pptas);
            ++pptas;
        }
    }
}

/* Decrements the ref counts of tas_t elements in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void thdr_decr_tas_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count)
{
    TA_ASSERT(thdr->type == TA_STRING);
    if ((first + count) > thdr->used)
        count = thdr->used - first;
    /* Note count might even be negative, check */
    if (count > 0) {
        tas_t **pptas, **end;
        pptas = THDRELEMPTR(thdr, tas_t *, first);
        end = pptas + count;
        while (pptas < end) {
            tas_unref(*pptas);
            /* TBD - should we also NULL the element ? */
            ++pptas;
        }
    }
}

/* Increments the ref counts of Tcl_Objs in a tarray making sure not
   to run past end of array */
void thdr_incr_obj_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count)
{
    TA_ASSERT(thdr->type == TA_ANY);
    if ((first + count) > thdr->used)
        count = thdr->used - first;
    /* Note count might even be negative, check */
    if (count > 0) {
        Tcl_Obj **pobjs, **end;
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
        end = pobjs + count;
        while (pobjs < end) {
            Tcl_IncrRefCount(*pobjs);
            ++pobjs;
        }
    }
}

/* Decrements the ref counts of Tcl_Objs in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void thdr_decr_obj_refs(thdr_t *thdr, Tcl_Size first, Tcl_Size count)
{
    TA_ASSERT(thdr->type == TA_ANY);
    if ((first + count) > thdr->used)
        count = thdr->used - first;
    /* Note count might even be negative, check */
    if (count > 0) {
        Tcl_Obj **pobjs, **end;
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
        end = pobjs + count;
        while (pobjs < end) {
            Tcl_DecrRefCount(*pobjs);
            /* TBD - should we NULL the element ? */
            ++pobjs;
        }
    }
}

/* Make sure objects in the tarray span have string representations */
void thdr_ensure_obj_strings(thdr_t *thdr, span_t *span)
{
    Tcl_Obj **pobjs, **end;

    TA_ASSERT(thdr->type == TA_ANY);
    if (span) {
        TA_ASSERT(span->first >= 0 && (span->first+span->count) <= thdr->used);
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, span->first);
        end = pobjs + span->count;
    } else {
        pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);
        end = pobjs + thdr->used;
    }
    while (pobjs < end) {
        if ((*pobjs)->bytes == NULL)
            Tcl_GetString(*pobjs);
        TA_ASSERT((*pobjs)->bytes != NULL);
        ++pobjs;
    }
}


#ifdef TA_MT_ENABLE
/*
 * For multithreaded operations, the array needs to be split up such that
 * the boundary is aligned at a point where the threads will not interfere
 * with one another. This is an issue only for the sizes smaller
 * than an atomic memory unit (assumed to be int).
 * Returns number of elements in first partition. This will never be 0
 * unless count is passed in as 0.
 * *psecond_block_size will hold number in second. This may be 0.
 *
 * Does not check if range is too small to be worthwhile multithreading.
 * That depends on operation and is up to the caller.
 *
 * Note tatype must not be TA_BOOLEAN.
 */
Tcl_Size thdr_calc_mt_split(int tatype, Tcl_Size first, Tcl_Size count, Tcl_Size *psecond_block_size)
{
    Tcl_Size second_block_size;
    Tcl_Size memunits;
    
    /* Assumes the thdr array is aligned properly and that processors
     * already access ints, wides and doubles atomically.
     */
    switch (tatype) {
    case TA_STRING:
    case TA_ANY:
    case TA_INT:
    case TA_UINT:
    case TA_DOUBLE:
    case TA_WIDE:
        second_block_size = count/2;
        break;
        
    case TA_BYTE:
        /* TBD - Assumes int is appropriate for memory atomicity */
        if (count < (2*sizeof(int)))
            second_block_size = 0;
        else {
            /* Remember first may not be aligned apprpriately */
            Tcl_Size aligned_first;
            Tcl_Size remaining_count;
            aligned_first = (first + sizeof(int) - 1) & (- (int) sizeof(int));
            remaining_count = count - (aligned_first - first);
            TA_ASSERT(remaining_count >= 0);
            memunits = remaining_count / sizeof(int);
            second_block_size = remaining_count - ((memunits / 2) * sizeof(int));
        }
        break;

    /* BOOLEAN and ANY cannot be multithreaded */
    case TA_BOOLEAN:
    default:
        second_block_size = 0; /* To keep gcc happy */
        ta_type_panic(tatype);
    }

    *psecond_block_size = second_block_size;
    return count - second_block_size;
}

/*
 * For multithreaded operations, the array needs to be split up such that
 * the boundary is aligned at a point where the threads will not interfere
 * with one another. This is an issue only for the sizes smaller
 * than an atomic memory unit (assumed to be int).
 *
 * Caller should pass in nsizes the size of the sizes[] array which
 * should also correspond to *max* number threads to split into.
 * On return, sizes[] will contain the number of elements to assign
 * to each thread, some of which may be set to 0.
 * 
 * Caller should pass min_hint as minimum number of elements a thread
 * should process. This is only treated as a hint.
 *
 * first is the offset of the first element. Caller should set this
 * appropriately to ensure split is done taking alignment into account.
 *
 * Returns number of threads to use or equivalently how many slots
 * of sizes[] are filled. Remaining slots in sizes[] are NOT INITIALIZED.
 */
Tcl_Size
thdr_calc_mt_split_ex(int tatype,
                      Tcl_Size first,
                      Tcl_Size count,
                      Tcl_Size min_hint,
                      Tcl_Size nsizes,
                      Tcl_Size sizes[])
{
    Tcl_Size nthreads;
    Tcl_Size split_size;
    Tcl_Size i;
    Tcl_Size aligned_first;

    if (min_hint < 1000)
        min_hint = 1000;

    /* Assumes the thdr array is aligned properly (into which first is
     * an offset) and that processors access ints, wides and doubles atomically.
     */

    /*
     * Figure out how many threads we should use. We would like every thread
     * to have at least min_hint elements to work with under the assumption
     * that otherwise it is not worth assigning a separate thread.
     */
    if (count <= (2*min_hint) || nsizes == 1) {
        sizes[0] = count;
        return 1;
    }

    nthreads = count / min_hint;
    if (nthreads > nsizes)
        nthreads = nsizes;

    TA_ASSERT(nthreads > 1);    /* Loops below assume this */

    switch (tatype) {
    case TA_STRING:
    case TA_ANY:
    case TA_INT:
    case TA_UINT:
    case TA_DOUBLE:
    case TA_WIDE:
        split_size = count / nthreads;
        for (i = 1; i < nthreads; ++i) {
            sizes[i] = split_size;
            count -= split_size;
        }
        TA_ASSERT(count > 0);
        sizes[0] = count;
        break;

    case TA_BYTE:
        /* Assumes count > 2*sizeof(int) because of checks at top */

        /*
         * TBD - Assume int is appropriate for memory atomicity. So the
         * boundaries for splitting have to be aligned.
         * Remember 'first' may not be aligned appropriately so do that first.
         */
        aligned_first = (first + sizeof(int) - 1) & (- (int) sizeof(int));
        sizes[0] = aligned_first - first;
        count -= sizes[0];

        split_size = count / nthreads;
        /* We have to make sure split_size is also aligned */
        split_size = (split_size + sizeof(int) - 1) & (- (int) sizeof(int));
        sizes[0] += split_size;
        TA_ASSERT(count >= split_size);
        count -= split_size;
        for (i = 1; i < nthreads; ++i) {
            if (count > split_size) {
                sizes[i] = split_size;
                count -= split_size;
            } else {
                sizes[i] = count;
                count = 0;
                break;
            }
        }
        if (i == nthreads) {
            if (count > 0) {
                /* Can this actually happen ? */
                sizes[nthreads-1] += count;
            }
        } else {
            TA_ASSERT(count == 0);
        }
        break;

    /* BOOLEAN and ANY cannot be multithreaded */
    case TA_BOOLEAN:
    default:
        ta_type_panic(tatype);
    }

    return nthreads;
}
#endif

/*
 * Updates TA_STRING elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_place_ta_strings(thdr_t *thdr,
                           thdr_t *pindices,
                           Tcl_Obj * const *ovalues,
                           Tcl_Size new_size
    )
{
    Tcl_Size i;
    Tcl_Size *pindex, *end;
    tas_t **pptas;

    TA_ASSERT(thdr->nrefs <= 1);
    
    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + pindices->used;
    pptas = THDRELEMPTR(thdr, tas_t *, 0);

    /*
     * Reference counts makes this tricky. If replacing an existing
     * index we have to increment the new value's ref and decrement
     * the old value's. If the index points to a previously unused
     * slot, then the value there is garbage and Tcl_DecrRefCount
     * should not be called on it. The problem is we cannot distinguish
     * the cases up front using thdr->used as a threshold because
     * pindices is in arbitrary order AND indices may be repeated.
     * Hence what we do is to store NULL first in all unused slots
     * that will be written to mark what is unused.
     */

    /* TBD - we do not bother to erase thdr_lookup entries. Should we ? */
    for (i = thdr->used; i < new_size; ++i)
        pptas[i] = NULL;        /* TBD - optimization - memset ? */
    while (pindex < end) {
        if (pptas[*pindex] != NULL)
            tas_unref(pptas[*pindex]); /* Deref original slot content */
        pptas[*pindex] = tas_from_obj(*ovalues);
        thdr_lookup_add(thdr, *pindex);
        pindex++;
        ovalues++;
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

/*
 * Updates TA_ANY elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_place_ta_objs(thdr_t *thdr,
                        thdr_t *pindices,
                        Tcl_Obj * const *ovalues,
                        Tcl_Size new_size
    )
{
    Tcl_Size i;
    Tcl_Size *pindex, *end;
    Tcl_Obj **pobjs;

    TA_ASSERT(thdr->nrefs <= 1);

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + pindices->used;
    pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);

    /*
     * Reference counts makes this tricky. If replacing an existing
     * index we have to increment the new value's ref and decrement
     * the old value's. If the index points to a previously unused
     * slot, then the value there is garbage and Tcl_DecrRefCount
     * should not be called on it. The problem is we cannot distinguish
     * the cases up front using thdr->used as a threshold because
     * pindices is in arbitrary order AND indices may be repeated.
     * Hence what we do is to store NULL first in all unused slots
     * that will be written to mark what is unused.
     */
    for (i = thdr->used; i < new_size; ++i)
        pobjs[i] = NULL;        /* TBD - optimization - memset ? */
    while (pindex < end) {
        /* Careful about the order here! */
        Tcl_IncrRefCount(*ovalues);
        if (pobjs[*pindex] != NULL)
            Tcl_DecrRefCount(pobjs[*pindex]);/* Deref original slot content */
        pobjs[*pindex++] = *ovalues++;
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

/*
 * Fills TA_STRING elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_fill_ta_strings(thdr_t *thdr,
                          thdr_t *pindices,
                          tas_t *ptas,
                          Tcl_Size new_size
    )
{
    Tcl_Size i;
    Tcl_Size *pindex, *end;
    tas_t **pptas;

    if (pindices->used == 0)
        return;                 /* Code below assumes at least one */

    TA_ASSERT(thdr->nrefs <= 1);

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + pindices->used;
    pptas = THDRELEMPTR(thdr, tas_t *, 0);

    /*
     * Reference counts makes this tricky. If replacing an existing
     * index we have to increment the new value's ref and decrement
     * the old value's. If the index points to a previously unused
     * slot, then the value there is garbage and Tcl_DecrRefCount
     * should not be called on it. The problem is we cannot distinguish
     * the cases up front using thdr->used as a threshold because
     * pindices is in arbitrary order AND indices may be repeated.
     * Hence what we do is to store NULL first in all unused slots
     * that will be written to mark what is unused.
     */

    /* TBD - we do not bother to erase thdr_lookup entries. Should we ? */

    for (i = thdr->used; i < new_size; ++i)
        pptas[i] = NULL;        /* TBD - optimization - memset ? */
    while (pindex < end) {
        TA_ASSERT(*pindex >= 0);
        if (pptas[*pindex] != NULL)
            tas_unref(pptas[*pindex]);/* Deref original slot content */
        pptas[*pindex] = tas_ref(ptas);
        ++pindex;
    }
    /* Safe since we already checked indices has at least one element */
    --pindex;
    thdr_lookup_add(thdr, *pindex);

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

/*
 * Fills TA_ANY elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_fill_ta_objs(thdr_t *thdr,
                       thdr_t *pindices,
                       Tcl_Obj *oval,
                       Tcl_Size new_size
    )
{
    Tcl_Size i;
    Tcl_Size *pindex, *end;
    Tcl_Obj **pobjs;

    TA_ASSERT(thdr->nrefs <= 1);

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + pindices->used;
    pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);

    /*
     * Reference counts makes this tricky. If replacing an existing
     * index we have to increment the new value's ref and decrement
     * the old value's. If the index points to a previously unused
     * slot, then the value there is garbage and Tcl_DecrRefCount
     * should not be called on it. The problem is we cannot distinguish
     * the cases up front using thdr->used as a threshold because
     * pindices is in arbitrary order AND indices may be repeated.
     * Hence what we do is to store NULL first in all unused slots
     * that will be written to mark what is unused.
     */
    for (i = thdr->used; i < new_size; ++i)
        pobjs[i] = NULL;        /* TBD - optimization - memset ? */
    while (pindex < end) {
        TA_ASSERT(*pindex >= 0);
        /* Careful about the order here! */
        Tcl_IncrRefCount(oval);
        if (pobjs[*pindex] != NULL)
            Tcl_DecrRefCount(pobjs[*pindex]);/* Deref what was originally in that slot */
        pobjs[*pindex++] = oval;
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

/*
 * Map numeric or string index to numeric integer index.
 */
TCL_RESULT
ta_convert_index(Tcl_Interp *ip,
                 Tcl_Obj *o,
                 Tcl_Size *pindex,
                 Tcl_Size end_value,
                 Tcl_Size low,
                 Tcl_Size high)
{
    Tcl_Size val;

    /* Do type checks to avoid expensive shimmering in case of errors */
    if (tcol_affirm(o) || table_affirm(o))
        return ta_index_error(ip, o);

    if (o->typePtr == g_tcl_list_type_ptr) {
        if (Tcl_ListObjLength(NULL, o, &val) != TCL_OK || val != 1)
            return ta_index_error(ip, o);
    }

    if (Tcl_GetSizeIntFromObj(NULL, o, &val) != TCL_OK) {
        if (ta_indexobj_from_any(NULL, o) != TCL_OK)
            return ta_index_error(ip, o);
        val = end_value + ta_indexobj_get_offset(o);
    }

    /* Note it is ok for val to be greater than end_value as it is used
       in calls where (for example) the column is extended */

    if (val < low || val > high)
        return ta_index_range_error(ip, val);
    else {
        *pindex = val;
        return TCL_OK;
    }
}

/* 
 * Parse range bounds. low has to be between 0 and nelems.
 * high has to be 0-INT_MAX
 * olow cannot be NULL.
 * If ohigh is NULL, it is treated as nelems-1
 * if (high < low) count is returned as 0 (not an error)
 * Negative indices treated as 0
 */
TCL_RESULT
ta_fix_range_bounds(Tcl_Interp *ip,
                    Tcl_Size nelems,
                    Tcl_Obj *olow,
                    Tcl_Obj *ohigh,
                    Tcl_Size *plow,
                    Tcl_Size *pcount)
{
    Tcl_Size low, high;

    /* TBD - need we restrict low to < nelems? Some routines which allow that
     cannot call this */
    if (ta_convert_index(ip, olow, &low, nelems-1, TCL_SIZE_MIN, nelems) != TCL_OK)
        return TCL_ERROR;

    if (ohigh) {
        if (ta_convert_index(ip, ohigh, &high, nelems-1, TCL_SIZE_MIN, TCL_SIZE_MAX) != TCL_OK)
            return TCL_ERROR;
    } else {
        high = nelems-1;        /* If nelems==0, will be dealt with below */
    }

    if (low < 0)
        low = 0;
    *plow = low;
    if (high < low)
        *pcount = 0;            /* This is how lrange behaves */
    else
        *pcount = high - low + 1;

    return TCL_OK;
}

TCL_RESULT
ta_parse_range_option_value(Tcl_Interp *ip,
                            Tcl_Size nelems,
                            Tcl_Obj *orange,
                            Tcl_Size *plow,
                            Tcl_Size *pcount)
{
    Tcl_Obj **objs;
    Tcl_Size nobjs;

    /*
     * Do type checks to avoid expensive shimmering in case of caller error
     * and also verify format
     */
    if (tcol_affirm(orange) ||
        table_affirm(orange) ||
        Tcl_ListObjGetElements(NULL, orange, &nobjs, &objs) != TCL_OK ||
        (nobjs != 1 && nobjs != 2)) {
        return ta_invalid_range_error(ip, orange);
    }

    return ta_fix_range_bounds(ip, nelems,
                               objs[0],
                               nobjs == 1 ? NULL : objs[1],
                               plow, pcount);
}

/* SHould only be called if o is not a column already */
TCL_RESULT tcol_convert_from_other(Tcl_Interp *ip, Tcl_Obj *o)
{
    Tcl_Obj **elems;
    Tcl_Size nelems;
    unsigned char tatype;

    TA_ASSERT(! tcol_affirm(o));

    /* Avoid shimmering tables if passed a table by mistake */
    if (table_affirm(o))
        return ta_not_column_error(ip);

    /* See if we can convert it to one based on string representation */
    if (Tcl_ListObjGetElements(NULL, o, &nelems, &elems) == TCL_OK
        && nelems == 3
        && !strcmp(Tcl_GetString(elems[0]), ta_column_type.name)
        && ta_parse_type(ip, elems[1], &tatype) == TCL_OK) {
        /* So far so good. Try and convert */
        thdr_t *thdr;
        Tcl_Obj **ovalues;
        Tcl_Size nvalues;

        if (Tcl_ListObjGetElements(ip, elems[2], &nvalues, &ovalues)
            != TCL_OK)
            return TCL_ERROR;

        thdr = thdr_alloc_and_init(ip, tatype, nvalues, ovalues, 0);
        if (thdr == NULL)
            return TCL_ERROR;

        /*
         * Get rid of old representation and stick in the new one. Note
         * string rep is NOT invalidated and must NOT be if it is shared.
         * In any case, no need to do so here.
         */
        if (o->typePtr && o->typePtr->freeIntRepProc) {
            o->typePtr->freeIntRepProc(o);
            o->typePtr = NULL;
        }

        tcol_set_intrep(o, thdr, NULL);
        return TCL_OK;
    }

    return ta_not_column_error(ip);
}


/* For TA_STRING, the return value has a ref count of 1. It is released
   when ta_value_clear is called. Caller may not call that if it wants
   to hold on or passes that elsewhere.
   Note TA_ANY is treated differently as it does not alloc a new object
   but just passes back what was passed in. Thus ta_value_clear will
   not release it.
*/
TCL_RESULT ta_value_from_obj(Tcl_Interp *ip, Tcl_Obj *o,
                              unsigned char tatype, ta_value_t *ptav)
{
    int i, status;

    switch (tatype) {
    case TA_BOOLEAN:
        if ((status = Tcl_GetBooleanFromObj(ip, o, &i)) == TCL_OK)
            ptav->bval = (i != 0);
        break;
    case TA_BYTE: status = ta_get_byte_from_obj(ip, o, &ptav->ucval); break;
    case TA_INT: status = ta_get_int_from_obj(ip, o, &ptav->ival); break;
    case TA_UINT: status = ta_get_uint_from_obj(ip, o, &ptav->uival); break;
    case TA_WIDE: status = ta_get_wide_from_obj(ip, o, &ptav->wval); break;
    case TA_DOUBLE: status = Tcl_GetDoubleFromObj(ip, o, &ptav->dval); break;
    case TA_ANY: ptav->oval = o; status = TCL_OK; break;
    case TA_STRING: ptav->ptas = tas_from_obj(o); status = TCL_OK; break;
    default:
        status = TCL_ERROR; /* Keep gcc happy */
        ta_type_panic(tatype);
    }
    if (status == TCL_OK)
        ptav->type = tatype;
    return status;
}

Tcl_Obj *ta_value_to_obj(ta_value_t *ptav)
{
    switch (ptav->type) {
    case TA_BOOLEAN: return Tcl_NewBooleanObj(ptav->bval);
    case TA_BYTE: return Tcl_NewIntObj(ptav->ucval);
    case TA_INT: return Tcl_NewIntObj(ptav->ival);
    case TA_UINT: return Tcl_NewWideIntObj(ptav->uival);
    case TA_WIDE: return Tcl_NewWideIntObj(ptav->wval);
    case TA_DOUBLE: return Tcl_NewDoubleObj(ptav->dval);
    case TA_ANY: return ptav->oval;
    case TA_STRING: return Tcl_NewStringObj(ptav->ptas->s, -1);
    default:
        ta_type_panic(ptav->type);
    }
    return NULL;
}

void ta_value_init_max(unsigned char tatype, ta_value_t *ptav)
{
    TA_ASSERT(tatype != TA_ANY && tatype != TA_STRING);

    switch (tatype) {
    case TA_BOOLEAN: ptav->bval = 1; break;
    case TA_BYTE: ptav->ucval = UINT8_MAX; break;
    case TA_INT: ptav->ival = INT32_MAX; break;
    case TA_UINT: ptav->uival = UINT32_MAX; break;
    case TA_WIDE: ptav->wval = INT64_MAX; break;
    case TA_DOUBLE: ptav->dval = DBL_MAX; break;
    }
    ptav->type = tatype;
}

/* TBD - make inline */
void ta_value_clear(ta_value_t *ptav)
{
    /* NOTE: For TA_ANY, Tcl_DecrRefCount is NOT called. See comments
       in ta_value_from_obj
    */
    if (ptav->type == TA_STRING)
        tas_unref(ptav->ptas);
}

/* Values MUST be same type, not even just both compatible */
int ta_value_compare(ta_value_t *pa, ta_value_t *pb, int ignore_case)
{
    TA_ASSERT(pa->type == pb->type);

    /* Just subtraction can lead to overflows! */
#define VALUE_CMP_(a, b, fld) \
    (a->fld > b->fld ? 1 : (a->fld == b->fld ? 0 : -1))

    switch (pa->type) {
    case TA_BOOLEAN: return VALUE_CMP_(pa, pb, bval);
    case TA_UINT:    return VALUE_CMP_(pa, pb, uival);
    case TA_INT:     return VALUE_CMP_(pa, pb, ival);
    case TA_WIDE:    return VALUE_CMP_(pa, pb, wval);
    case TA_DOUBLE:  return VALUE_CMP_(pa, pb, dval);
    case TA_BYTE:    return VALUE_CMP_(pa, pb, ucval);
    case TA_ANY:     return ta_obj_compare(pa->oval, pb->oval, ignore_case);
    case TA_STRING:  return tas_compare(pa->ptas, pb->ptas, ignore_case);
    default:
        ta_type_panic(pa->type);
        return 0;                   /* To keep compiler happy */
    }
#undef VALUE_CMP_
}

struct thdr_fill_mt_context {
    ta_value_t tav;
    void *base;
    Tcl_Size nelems;
};

static void thdr_fill_int_mt_worker(void *pv)
{
    struct thdr_fill_mt_context *pctx = pv;
    int val = pctx->tav.ival;
    int *pint, *end;

    /*
      TBD - newer MS compiler generates as good code for any fill,
      no need to special case to memset for 0. However, VC++ 6 does not.
      Check if gcc same and ifdef memset code for specific compilers
    */
    if (val == 0) {
        memset(pctx->base, 0, pctx->nelems*sizeof(int));
        return;
    }

    pint = pctx->base;
    end = pint + pctx->nelems;
    while (pint < end)
        *pint++ = val;
}

static void thdr_fill_double_mt_worker(void *pv)
{
    struct thdr_fill_mt_context *pctx = pv;
    double val = pctx->tav.dval;
    double *pdbl, *end;

    pdbl = pctx->base;
    end = pdbl + pctx->nelems;
    while (pdbl < end)
        *pdbl++ = val;
}

static void thdr_fill_wide_mt_worker(void *pv)
{
    struct thdr_fill_mt_context *pctx = pv;
    Tcl_WideInt val = pctx->tav.wval;
    Tcl_WideInt *pwide, *end;

    /*
      TBD - newer MS compiler generates as good code for any fill,
      no need to special case to memset for 0. However, VC++ 6 does not.
      Check if gcc same and ifdef memset code for specific compilers
    */
    if (val == 0) {
        memset(pctx->base, 0, pctx->nelems*sizeof(Tcl_WideInt));
        return;
    }

    pwide = pctx->base;
    end = pwide + pctx->nelems;
    while (pwide < end)
        *pwide++ = val;
}

static void thdr_fill_byte_mt_worker(void *pv)
{
    struct thdr_fill_mt_context *pctx = pv;
    memset(pctx->base, pctx->tav.ucval, pctx->nelems);
}

/*
 * Use multiple threads to fill scalar values. 
 * WARNING: Does NOT update thdr header (like thdr->used etc.)
 * Caller must do that.
 */
void thdr_fill_scalars(Tcl_Interp *ip, thdr_t *thdr,
                       const ta_value_t *ptav, Tcl_Size pos, Tcl_Size count)
{
    ta_mt_function_t workerfn = NULL;
    int elem_size;
    struct thdr_fill_mt_context fill_context[2];

    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(thdr->nrefs <= 1);
    TA_ASSERT((pos+count) <= thdr->usable);

    /*
      TBD - depending on compiler generated code, multithreading
      fills serves no purpose. VS 2012 translates to reps instruction
      and does not benefit from multithreading. VC++ 6 does. Benchmark
      and set default multithreading threshold accordingly.
    */

    switch (thdr->type) {
    case TA_BYTE:
        workerfn = thdr_fill_byte_mt_worker;
        break;
    case TA_WIDE:
        workerfn = thdr_fill_wide_mt_worker;
        break;
    case TA_DOUBLE:
        workerfn = thdr_fill_double_mt_worker;
        break;

    case TA_INT:
    case TA_UINT:
        workerfn = thdr_fill_int_mt_worker;
        break;
    default:
        ta_type_panic(thdr->type);
    }


    elem_size = thdr->elem_bits / CHAR_BIT;
    fill_context[0].tav = *ptav;
    fill_context[0].base = (pos*elem_size) + THDRELEMPTR(thdr, unsigned char, 0);
#ifdef TA_MT_ENABLE
    fill_context[0].nelems = thdr_calc_mt_split(thdr->type, pos, count, &fill_context[1].nelems);
    TA_ASSERT((fill_context[0].nelems + fill_context[1].nelems) == count);

    if (count < ta_fill_mt_threshold || fill_context[1].nelems == 0) {
        fill_context[0].nelems = count;
        workerfn(&fill_context[0]);
    } else {
        /* Multiple threads */
        ta_mt_group_t grp;
        fill_context[1].tav = *ptav;
        fill_context[1].base = (elem_size*fill_context[0].nelems) + (char*)fill_context[0].base;
        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* TBD - check return code */ ta_mt_group_async_f(grp, &fill_context[1], workerfn);
        workerfn(&fill_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);
    }
#else
    fill_context[0].nelems = count;
    workerfn(&fill_context[0]);
#endif
}

/*
 * Set the value of an element range at a position in a thdr_t.
 * See the asserts below for conditions under which this can be called
 */
void
thdr_fill_range(Tcl_Interp *ip,
                thdr_t *thdr,
                const ta_value_t *ptav,
                Tcl_Size pos,
                Tcl_Size count,
                int insert)
{
    Tcl_Size i;
    Tcl_Size new_used;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pos <= thdr->used);
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(count >= 0);

    /* Code below depends on this check ! */
    if (count == 0)
        return;

    new_used = thdr_recompute_occupancy(thdr, &pos, count, insert);
    TA_ASSERT(new_used <= thdr->usable);

    if (insert)
        thdr_make_room(thdr, pos, count);

    /* NOTE in insert mode, thdr is in inconsistent state as intermediate
       slots are uninitialized. This is specially relevant for TA_ANY
       and TA_STRING
    */

    thdr->sort_order = THDR_UNSORTED;
    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_fill(THDRELEMPTR(thdr, ba_t, 0), pos, count, ptav->bval);
        break;
    case TA_BYTE:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_INT:
    case TA_UINT:
        thdr_fill_scalars(ip, thdr, ptav, pos, count);
        break;
        
    case TA_ANY:
        {
            Tcl_Obj **pobjs;
            Tcl_Size n;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts. Note when inserting
             * no decrements are to be done.
             */
            i = pos;    /* Starting position to write */
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, pos);
            if (! insert) {
                /* First loop overwriting existing elements */
                n = pos + count;
                if (n > thdr->used)
                    n = thdr->used;
                for (; i < n; ++i) {
                    /* Be careful of the order */
                    Tcl_IncrRefCount(ptav->oval);
                    Tcl_DecrRefCount(*pobjs);
                    *pobjs++ = ptav->oval;
                }
            }

            /* Now loop over new elements being appended */
            for (; i < pos+count; ++i) {
                Tcl_IncrRefCount(ptav->oval);
                *pobjs++ = ptav->oval;
            }
        }
        break;

    case TA_STRING:
        {
            tas_t **pptas;
            Tcl_Size n;

            /*
             * We have to deal with reference counts here. For the object
             * we are copying we need to increment the reference counts
             * that many times. For objects being overwritten,
             * we need to decrement reference counts. Note when inserting
             * no decrements are to be done.
             */
            i = pos;    /* Starting position to write */
            pptas = THDRELEMPTR(thdr, tas_t *, pos);
            if (! insert) {
                /* First loop overwriting existing elements */
                n = pos + count;
                if (n > thdr->used)
                    n = thdr->used;
                for (; i < n; ++i) {
                    /* Be careful of the order */
                    tas_t *ptas = tas_ref(ptav->ptas);
                    tas_unref(*pptas);
                    *pptas++ = ptas;
                }
            }

            /* Now loop over new elements being appended */
            for (; i < pos+count; ++i)
                *pptas++ = tas_ref(ptav->ptas);

            /* Update the lookup if it exists. */
            TA_ASSERT(count > 0);
            thdr_lookup_add(thdr, pos+count-1);
        }
        break;

    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_used <= thdr->usable);
    thdr->used = new_used;
}



TCL_RESULT ta_verify_value_objs(Tcl_Interp *ip, int tatype,
                                Tcl_Size nelems, Tcl_Obj * const elems[])
{
    Tcl_Obj * const *pobjs = elems;
    Tcl_Obj * const *end = elems + nelems;

#define ta_verify_value_LOOP(type, fn)          \
    do {                                        \
        for ( ; pobjs < end; ++pobjs) {         \
            type val;                           \
            if (fn(ip, *pobjs, &val) != TCL_OK) \
                return TCL_ERROR;               \
        }                                       \
    } while (0)

    switch (tatype) {
    case TA_BOOLEAN:
        ta_verify_value_LOOP(int, Tcl_GetBooleanFromObj);
        break;
    case TA_UINT:
        ta_verify_value_LOOP(unsigned int, ta_get_uint_from_obj);
        break;
    case TA_INT:
        ta_verify_value_LOOP(int, ta_get_int_from_obj);
        break;
    case TA_WIDE:
        ta_verify_value_LOOP(Tcl_WideInt, ta_get_wide_from_obj);
        break;
    case TA_DOUBLE:
        ta_verify_value_LOOP(double, Tcl_GetDoubleFromObj);
        break;
    case TA_BYTE:
        ta_verify_value_LOOP(unsigned char, ta_get_byte_from_obj);
        break;
    case TA_ANY:
    case TA_STRING:
        break; /* Nothing to verify */
    default:
        ta_type_panic(tatype);
    }
    return TCL_OK;
}


/* Verify that the specified index list is valid.
   We need to verify that the indices
   are not beyond the range. At the same time the indices may
   themselves extend the range. Sort indices to simplify this.
   Returns new size needed in *new_sizeP
*/
TCL_RESULT thdr_verify_indices_in_range(Tcl_Interp *ip, Tcl_Size current_size, thdr_t *pindices, Tcl_Size *new_sizeP)
{
    Tcl_Size cur, highest;
    int status;
    Tcl_Size *pindex, *end;
    thdr_t *psorted = NULL;

    TA_ASSERT(pindices->type == TA_INDEX);

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = THDRINDEXELEMPTR(pindices, pindices->used);

    /* Special cases so we don't go through the long path */
    if (pindices->used < 2) {
        if (pindices->used == 0)
            *new_sizeP = 0;
        else {
            /* One index specified */
            if (*pindex < 0 || *pindex > current_size)
                return ta_index_range_error(ip, *pindex);
            *new_sizeP = current_size;
            if (*pindex == current_size)
                *new_sizeP = current_size + 1;
        }
        return TCL_OK;
    }

    /*
     * If indices are not sorted, we need to sort them to ensure no gaps.
     * Potentially we could use a bit array to do it differently but...TBD
     */
    if (pindices->sort_order == THDR_UNSORTED) {
        psorted = thdr_clone(ip, pindices, 0, NULL);
        if (psorted == NULL)
            return TCL_ERROR;
        qsort(THDRINDEXELEMPTR(psorted, 0), psorted->used, sizeof(Tcl_Size), tclsizecmp);
        psorted->sort_order = THDR_SORTED_ASCENDING;
        pindices = psorted;
        pindex = THDRINDEXELEMPTR(pindices, 0);
        end = THDRINDEXELEMPTR(pindices, pindices->used);
    }

    /* Make sure no negative indices and no gaps in indices */
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        /* Indices are sorted ascending. So to check for negative indices,
         * only need to check first entry.
         */
        if (*pindex < 0) {
            status = ta_index_range_error(ip, *pindex);
            goto vamoose;
        }
        
        /*
         * We will start going backward until we hit a gap in the sequence
         * and error out at that point. If we reach the current size,
         * we can stop since any further indices will be within the current
         * limits.
         */
        TA_ASSERT(pindex < end); /* Since we already special cased 0/1 above */
        cur = *--end;
        highest = cur;
        while (pindex < end && cur > current_size) {
            --end;
            /* Sorted, so can only be cur or cur-1 */
            if (*end != cur && *end != (cur-1))
                break;          /* Gap! */
            cur = *end;
        }
    } else {
        TA_ASSERT(pindex < end); /* Since we already special cased 0/1 above */

        /* Indices are sorted descending. So to check for negative indices,
         * only need to check last entry.
         */
        if (end[-1] < 0) {
            status = ta_index_range_error(ip, *pindex);
            goto vamoose;
        }

        /* Same as above loop but in reverse since sorted in reverse order */
        cur = *pindex++;
        highest = cur;
        while (pindex < end && cur > current_size) {
            if (*pindex != cur && *pindex != (cur-1))
                break;
            cur = *pindex++;
        }
    }

    if (cur <= current_size) {
        *new_sizeP = highest >= current_size ? highest + 1 : current_size;
        status = TCL_OK;
    } else
        status = ta_index_range_error(ip, cur);
vamoose:
    if (psorted)
        thdr_decr_refs(psorted);

    return status;

}

/* thdr must be large enough for largest index. And see asserts in code */
void thdr_fill_indices(Tcl_Interp *ip, thdr_t *thdr, 
                       const ta_value_t *ptav, thdr_t *pindices,
                       Tcl_Size new_size
    )
{
    Tcl_Size *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(pindices->type == TA_INDEX);

    /* Caller guarantees room for highest index value */
    TA_ASSERT(new_size <= thdr->usable);

    if (pindices->used == 0)
        return;          /* Nothing to do */

    /* Rest of code assumes > 0 indices */

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = THDRINDEXELEMPTR(pindices, pindices->used);

    thdr->sort_order = THDR_UNSORTED;
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                ba_put(baP, *pindex++, ptav->bval);
            }
        }
        break;
    case TA_INT:
    case TA_UINT:
        {
            int *pint = THDRELEMPTR(thdr, int, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                pint[*pindex++] = ptav->ival;
            }
        }
        break;
    case TA_BYTE:
        {
            unsigned char *ucP = THDRELEMPTR(thdr, unsigned char, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                ucP[*pindex++] = ptav->ucval;
            }
        }
        break;
    case TA_WIDE:
        {
            Tcl_WideInt *pwide;
            pwide = THDRELEMPTR(thdr, Tcl_WideInt, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                pwide[*pindex++] = ptav->wval;
            }
        }
        break;
    case TA_DOUBLE:
        {
            double *pdbl;
            pdbl = THDRELEMPTR(thdr, double, 0);
            while (pindex < end) {
                TA_ASSERT(*pindex >= 0);
                pdbl[*pindex++] = ptav->dval;
            }
        }
        break;
    case TA_ANY:
        thdr_fill_ta_objs(thdr, pindices, ptav->oval, new_size);
        return;
    case TA_STRING:
        thdr_fill_ta_strings(thdr, pindices, ptav->ptas, new_size);
        return;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

void thdr_free(thdr_t *thdr)
{
    if (thdr == NULL)
        return;
    switch (thdr->type) {
    case TA_ANY:
        thdr_decr_obj_refs(thdr, 0, thdr->used);
        break;
    case TA_STRING:
        thdr_lookup_free(thdr);
        thdr_decr_tas_refs(thdr, 0, thdr->used);
        break;
    }
    TA_FREEMEM(thdr);
}


Tcl_Obj *tcol_new(thdr_t *thdr)
{
    Tcl_Obj *o;

    if (thdr == NULL)
        return NULL;
    o = Tcl_NewObj();
    Tcl_InvalidateStringRep(o);
    tcol_set_intrep(o, thdr, NULL);
    return o;
}

Tcl_Obj *tcol_new_span(thdr_t *thdr, Tcl_Size first, Tcl_Size count)
{
    Tcl_Obj *o;
    span_t *span;

    TA_ASSERT(thdr);
    TA_ASSERT(first >= 0);
    TA_ASSERT(count >= 0);
    TA_ASSERT(first+count <= thdr->used);

    o = Tcl_NewObj();
    Tcl_InvalidateStringRep(o);
    if (first == 0 && count == thdr->used)
        span = NULL;
    else
        span = span_alloc(first, count);

    tcol_set_intrep(o, thdr, span);
    return o;
}

/* thdr must NOT be shared and must have enough slots */
/* ip may be NULL (only used for errors) */
TCL_RESULT thdr_put_objs(Tcl_Interp *ip, thdr_t *thdr, Tcl_Size first,
                         Tcl_Size nelems, Tcl_Obj * const elems[], int insert)
{
    int ival, status;
    Tcl_Size i, new_used;

    TA_ASSERT(thdr->nrefs < 2);

    new_used = thdr_recompute_occupancy(thdr, &first, nelems, insert);
    TA_ASSERT(new_used <= thdr->usable); /* Caller should have ensured */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /*
     * In case of conversion errors, we have to keep the old values
     * so we loop through first to verify there are no errors and then
     * a second time to actually store the values. The arrays can be
     * very large so we do not want to allocate a temporary
     * holding area for saving old values to be restored in case of errors.
     *
     * As a special optimization, when appending to the end, we do
     * not need to first check. We directly store the values and in case
     * of errors, simply do not update size. Note this works even for
     * the insertion case.
     */

    if (first < thdr->used) {
        if ((status = ta_verify_value_objs(ip, thdr->type, nelems, elems))
            != TCL_OK)
            return TCL_ERROR;
    }

    /* Make room if necessary */
    if (insert)
        thdr_make_room(thdr, first, nelems);

    /*
     * Now actually store the values. Note we still have to check
     * status on conversion since we did not do checks when we are appending
     * to the end.
     */
#define thdr_put_OBJCOPY(type, fn)              \
    do {                                        \
        type *p;                                \
        p = THDRELEMPTR(thdr, type, first);     \
        for (i = 0; i < nelems; ++i, ++p) {     \
            if (fn(ip, elems[i], p) != TCL_OK)  \
                goto convert_error;             \
        }                                       \
    } while (0)

    switch (thdr->type) {
    case TA_BOOLEAN:
       {
           register ba_t *baP;
           ba_t ba, ba_mask, off;

           /* NOTE: same code as in thdr_numerics_from_tas_strings except there
              we are dealing with tas_t and thus cannot be shared with this.
              If you fix a bug here, fix it there as well*/
           
           /* Take care of the initial condition where the first bit
              may not be aligned on a boundary */
           baP = THDRELEMPTR(thdr, ba_t, first / BA_UNIT_SIZE);
           off = first % BA_UNIT_SIZE; /* Offset of bit within a char */
           ba_mask = BITPOSMASK(off); /* The bit pos corresponding to 'first' */
           if (off != 0) {
               /*
                * Offset is off within a ba_t. Get the ba_t at that location
                * preserving the preceding bits within the char.
                */
               ba = *baP & BITPOSMASKLT(off);
           } else {
               ba = 0;
           }
           for (i = 0; i < nelems; ++i) {
               if (Tcl_GetBooleanFromObj(ip, elems[i], &ival) != TCL_OK)
                   goto convert_error;
               if (ival)
                   ba |= ba_mask;
               ba_mask = BITMASKNEXT(ba_mask);
               if (ba_mask == 0) {
                   *baP++ = ba;
                   ba = 0;
                   ba_mask = BITPOSMASK(0);
               }
           }
           if (ba_mask != BITPOSMASK(0)) {
               /* We have some leftover bits in ba that need to be stored.
                * We need to *merge* these into the corresponding word
                * keeping the existing high index bits.
                * Note the bit indicated by ba_mask also has to be preserved,
                * not overwritten.
                */
               *baP = ba | (*baP & BITMASKGE(ba_mask));
           }
       }
       break;
    case TA_UINT: thdr_put_OBJCOPY(unsigned int, ta_get_uint_from_obj); break;
    case TA_INT: thdr_put_OBJCOPY(int, ta_get_int_from_obj); break;
    case TA_WIDE: thdr_put_OBJCOPY(Tcl_WideInt, ta_get_wide_from_obj); break;
    case TA_DOUBLE:thdr_put_OBJCOPY(double, Tcl_GetDoubleFromObj); break;
    case TA_BYTE: thdr_put_OBJCOPY(unsigned char, ta_get_byte_from_obj); break;
    case TA_ANY:
        {
            register Tcl_Obj **pobjs;
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
            for (i = 0; i < nelems; ++i, ++pobjs) {
                /* Careful about the order here! */
                Tcl_IncrRefCount(elems[i]);
                /* Only release existing elements if we were not inserting */
                if (!insert) {
                    if ((first + i) < thdr->used) {
                        /* Deref what was originally in that slot */
                        Tcl_DecrRefCount(*pobjs);
                    }
                }
                *pobjs = elems[i];
            }
        }
        break;
    case TA_STRING:
        {
            register tas_t **pptas;
            pptas = THDRELEMPTR(thdr, tas_t *, first);
            for (i = 0; i < nelems; ++i, ++pptas) {
                /* Only release existing elements if we were not inserting */
                if (!insert) {
                    if ((first + i) < thdr->used) {
                        /* Deref what was originally in that slot */
                        tas_unref(*pptas);
                    }
                }
                *pptas = tas_from_obj(elems[i]);
                thdr_lookup_add(thdr, first + i);
            }
        }
        break;

    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_used <= thdr->usable);
    thdr->used = new_used;

    return TCL_OK;

convert_error:                  /* Interp should already contain errors */
    TA_ASSERT(thdr->type != TA_ANY && thdr->type != TA_STRING); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}


/* Caller must have done all the checks in the TA_ASSERTS below ! */
void thdr_place_objs(
    Tcl_Interp *ip,
    thdr_t *thdr,               /* thdr_t to be modified - must NOT be shared */
    thdr_t *pindices,           /* Contains indices. If any >= thdr->used,
                                    all intermediate indices must also be
                                    present in pindices. Caller
                                    must have checked */
    Tcl_Size new_size,
    Tcl_Size nvalues,           /* # values in ovalues */
    Tcl_Obj * const *ovalues)   /* Values to be stored, must be type verified */
{
    Tcl_Size *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pindices->type == TA_INDEX);
    TA_ASSERT(new_size <= thdr->usable);
    TA_ASSERT(pindices->used <= nvalues);
    TA_ASSERT(ta_verify_value_objs(ip, thdr->type, nvalues, ovalues) == TCL_OK);

    if (nvalues == 0)
        return;          /* Nothing to change */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /* Note we do not check conversion status since caller must check */

#define PLACEVALUES(type, fn) do {                                      \
        type *p;                                                        \
        p = THDRELEMPTR(thdr, type, 0);                                 \
        while (pindex < end) {                                          \
            TA_ASSERT(*pindex < thdr->usable);                          \
            TA_NOFAIL(fn(ip, *ovalues++, &p[*pindex++]), TCL_OK);       \
        }                                                               \
    } while (0)

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + pindices->used;
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            int bval;
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            while (pindex < end) {
                TA_NOFAIL(Tcl_GetBooleanFromObj(ip, *ovalues++, &bval), TCL_OK);
                TA_ASSERT(*pindex < thdr->usable);
                ba_put(baP, *pindex++, bval);
            }
        }
        break;

    case TA_UINT:
        PLACEVALUES(unsigned int, ta_get_uint_from_obj);
        break;
    case TA_INT:
        PLACEVALUES(int, ta_get_int_from_obj);
        break;
    case TA_WIDE:
        PLACEVALUES(Tcl_WideInt, ta_get_wide_from_obj);
        break;
    case TA_DOUBLE:
        PLACEVALUES(double, Tcl_GetDoubleFromObj);
        break;
    case TA_ANY:
        thdr_place_ta_objs(thdr, pindices, ovalues, new_size);
        return;
    case TA_STRING:
        thdr_place_ta_strings(thdr, pindices, ovalues, new_size);
        return;
    case TA_BYTE:
        PLACEVALUES(unsigned char, ta_get_byte_from_obj);
        break;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

void
thdr_place_indices(Tcl_Interp *ip,
                   thdr_t *thdr,
                   thdr_t *psrc,
                   span_t *src_span,
                   thdr_t *pindices,
                   Tcl_Size new_size)
{
    Tcl_Size *pindex, *end;
    Tcl_Size i, src_first;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pindices->type == TA_INDEX);
    TA_ASSERT(new_size <= thdr->usable);
    TA_ASSERT(thdr->type == psrc->type);

    if (src_span) {
        src_first = src_span->first;
        TA_ASSERT(pindices->used <= src_span->count);
    } else {
        src_first = 0;
        TA_ASSERT(pindices->used <= psrc->used);
    }

    if (pindices->used == 0)
        return;          /* Nothing to change */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */

#define thdr_place_COPYINDICES(type) do {       \
        type *dst, *src;                        \
        dst = THDRELEMPTR(thdr, type, 0);       \
        src = THDRELEMPTR(psrc, type, src_first);  \
        while (pindex < end) {                  \
            TA_ASSERT(*pindex < thdr->usable);  \
            dst[*pindex++] = *src++;            \
        }                                       \
    } while (0)

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + pindices->used;

    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *dst = THDRELEMPTR(thdr, ba_t, 0);
            ba_t *src = THDRELEMPTR(psrc, ba_t, 0);
            for (i = 0; pindex < end; ++pindex, ++i) {
                TA_ASSERT(*pindex < thdr->usable);
                ba_put(dst, *pindex, ba_get(src, src_first+i));
            }
        }
        break;

    case TA_UINT:
        thdr_place_COPYINDICES(unsigned int);
        break;
    case TA_INT:
        thdr_place_COPYINDICES(int);
        break;
    case TA_WIDE:
        thdr_place_COPYINDICES(Tcl_WideInt);
        break;
    case TA_DOUBLE:
        thdr_place_COPYINDICES(double);
        break;
    case TA_ANY:
        /* Tricky 'cause of ref counts. See comments in thdr_place_ta_objs */
        {
            Tcl_Obj **dst = THDRELEMPTR(thdr, Tcl_Obj *, 0);
            Tcl_Obj **src = THDRELEMPTR(psrc, Tcl_Obj *, src_first);
            for (i = thdr->used; i < new_size; ++i)
                dst[i] = NULL;        /* TBD - optimization - memset ? */
            while (pindex < end) {
                /* Careful about the order here! */
                TA_ASSERT(*pindex < thdr->usable);
                Tcl_IncrRefCount(*src);
                if (dst[*pindex] != NULL)
                    Tcl_DecrRefCount(dst[*pindex]);/* Deref what was originally in that slot */
                dst[*pindex++] = *src++;
            }

        }
        break;
    case TA_STRING:
        /* Tricky 'cause of ref counts. See comments in thdr_place_ta_strings */
        {
            tas_t **dst = THDRELEMPTR(thdr, tas_t *, 0);
            tas_t **src = THDRELEMPTR(psrc, tas_t *, src_first);
            for (i = thdr->used; i < new_size; ++i)
                dst[i] = NULL;        /* TBD - optimization - memset ? */
            while (pindex < end) {
                /* Careful about the order here! */
                tas_t *ptas = tas_ref(*src);
                TA_ASSERT(*pindex < thdr->usable);
                if (dst[*pindex] != NULL)
                    tas_unref(dst[*pindex]);/* Deref original slot */
                dst[*pindex] = ptas;
                thdr_lookup_add(thdr, *pindex);
                ++pindex;
                ++src;
            }
        }
        break;

    case TA_BYTE:
        thdr_place_COPYINDICES(unsigned char);
        break;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

Tcl_Size thdr_required_size(int tatype, Tcl_Size count)
{
    Tcl_Size space;

    switch (tatype) {
    case TA_BOOLEAN:
        space = BA_BYTES_NEEDED(0, count);
        break;
    case TA_UINT:
    case TA_INT:
        space = count * sizeof(int);
        break;
    case TA_WIDE:
        space = count * sizeof(Tcl_WideInt);
        break;
    case TA_DOUBLE:
        space = count * sizeof(double);
        break;
    case TA_ANY:
        space = count * sizeof(Tcl_Obj *);
        break;
    case TA_STRING:
        space = count * sizeof(tas_t *);
        break;
    case TA_BYTE:
        space = count * sizeof(unsigned char);
        break;
    default:
        space = 0; /* Keep gcc happy */
        ta_type_panic(tatype);
    }

    return sizeof(thdr_t) + space;
}

static TCL_RESULT thdr_roundup_alloc_size(Tcl_Size current_count, Tcl_Size requested_count, Tcl_Size *pcount)
{
    Tcl_Size extra, rounded;
    TA_ASSERT(requested_count > current_count);

    extra = TA_EXTRA(current_count);
    if ((TA_MAX_COUNT - current_count) < extra)
        extra = TA_MAX_COUNT - current_count;
    if ((requested_count - current_count) > extra)
        extra = requested_count - current_count;
    rounded = current_count + extra;
    if (rounded > TA_MAX_COUNT)
        return TCL_ERROR;

    *pcount = rounded;
    TA_ASSERT(*pcount >= requested_count);
    return TCL_OK;
}


/* ip may be NULL. oldP must not be shared (ref count must be 0 or 1)
   Ref count of new returned thdr same as that of oldP
 */
thdr_t *thdr_realloc(Tcl_Interp *ip, thdr_t *oldP, Tcl_Size new_count)
{
    thdr_t *thdr;
    Tcl_Size sz;

    /* TBD - check new_count against TA_MAX_COUNT */

    TA_ASSERT(oldP->nrefs < 2);
    /* The following assert is needed because we must not inadvertently
       reduce the size to less than what is already in use since that
       will result in Tcl_Obj* leaks in case the thdr is of type
       TA_STRING or TA_ANY. */
    TA_ASSERT(oldP->used <= new_count);

    /* We allocate one more for the sentinel */
    sz = thdr_required_size(oldP->type, new_count + 1);
    thdr = (thdr_t *) TA_ATTEMPTREALLOCMEM((char *) oldP, sz);
    if (thdr)
        thdr->usable = new_count;
    else
        ta_memory_error(ip, sz);
    return thdr;
}

/* ip may be NULL */
thdr_t * thdr_alloc(Tcl_Interp *ip, int tatype, Tcl_Size count)
{
    unsigned char nbits;
    Tcl_Size sz;
    thdr_t *thdr;

    /* TBD - check count against TA_MAX_COUNT */

    if (count < 0)
            count = TA_DEFAULT_NSLOTS;
    /* We allocate one extra slot for the sentinel */
    sz = thdr_required_size(tatype, count + 1);
    thdr = (thdr_t *) TA_ATTEMPTALLOCMEM(sz);
    if (thdr == NULL) {
        if (ip)
            ta_memory_error(ip, sz);
        return NULL;
    }
    thdr->lookup = TAS_LOOKUP_INVALID_HANDLE;
    thdr->nrefs = 0;
    thdr->usable = count;
    thdr->used = 0;
    thdr->type = tatype;
    switch (tatype) {
    case TA_BOOLEAN: nbits = 1; break;
    case TA_UINT: nbits = sizeof(unsigned int) * CHAR_BIT; break;
    case TA_INT: nbits = sizeof(int) * CHAR_BIT; break;
    case TA_WIDE: nbits = sizeof(Tcl_WideInt) * CHAR_BIT; break;
    case TA_DOUBLE: nbits = sizeof(double) * CHAR_BIT; break;
    case TA_ANY: nbits = sizeof(Tcl_Obj *) * CHAR_BIT; break;
    case TA_STRING: nbits = sizeof(tas_t *) * CHAR_BIT; break;
    case TA_BYTE: nbits = sizeof(unsigned char) * CHAR_BIT; break;
    default:
        nbits = 0; /* Keep gcc happy */
        ta_type_panic(tatype);
    }
    thdr->elem_bits = nbits;
    thdr->sort_order = THDR_UNSORTED;

    return thdr;
}

thdr_t * thdr_alloc_and_init(Tcl_Interp *ip, int tatype,
                           Tcl_Size nelems, Tcl_Obj * const elems[],
                           Tcl_Size init_size)
{
    thdr_t *thdr;

    if (elems) {
        /*
         * Initialization provided. If explicit size specified, fix
         * at that else leave some extra space.
         */
        if (init_size) {
            if (init_size < nelems)
                init_size = nelems;
        } else {
            init_size = nelems + TA_EXTRA(nelems);
        }
    } else {
        nelems = 0;
    }

    thdr = thdr_alloc(ip, tatype, init_size);
    if (thdr) {
        if (elems != NULL && nelems != 0) {
            if (thdr_put_objs(ip, thdr, 0, nelems, elems, 0) != TCL_OK) {
                thdr_decr_refs(thdr);
                thdr = NULL;
            }
        }
    }

    return thdr;               /* May be NULL on error */
}

/* Returns 0-initialized boolean column */
thdr_t *thdr_alloc_bitmap(Tcl_Interp *ip, Tcl_Size count)
{
    thdr_t *thdr;
    
    thdr = thdr_alloc(ip, TA_BOOLEAN, count);
    if (thdr == NULL)
        return NULL;
    ba_fill(THDRELEMPTR(thdr, ba_t, 0), 0, count, 0);
    thdr->used = count;
    return thdr;
}

/* Convert a integer index column to a bitmap index. */
thdr_t *thdr_indices_to_bitmap(
    Tcl_Interp *ip,
    Tcl_Size size, /* Size of bitmap. Errors if any index is higher */
    thdr_t *src, span_t *span)
{
    thdr_t *thdr;
    Tcl_Size *pindex;
    Tcl_Size count, pos;
    ba_t *baP;

    TA_ASSERT(src->type == TA_INDEX);
    pindex = THDRINDEXELEMPTR(src, 0);
    count = src->used;
    if (span) {
        count = span->count;
        pindex += span->first;
    }
    thdr = thdr_alloc_bitmap(ip, size);
    if (thdr == NULL)
        return NULL;
    baP = THDRELEMPTR(thdr, ba_t, 0);
    for (pos = 0; pos < count; ++pos, ++pindex) {
        if (*pindex >= size) {
            thdr_decr_refs(thdr);
            ta_index_range_error(ip, *pindex);
            return NULL;
        }
        ba_put(baP, *pindex, 1);
    }
    return thdr;
}

/* Grow the internal rep to a minimum size */
TCL_RESULT tcol_grow_intrep(Tcl_Interp *ip, Tcl_Obj *o, Tcl_Size new_size)
{
    thdr_t *thdr;

    TA_ASSERT(tcol_affirm(o));
    TA_ASSERT(! Tcl_IsShared(o));
    
    thdr = OBJTHDR(o);
    TA_ASSERT(thdr);
    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(OBJTHDRSPAN(o) == NULL);
    TA_ASSERT(new_size > thdr->usable);

    Tcl_InvalidateStringRep(o);
    thdr = thdr_realloc(ip, thdr, new_size);
    if (thdr == NULL)
        return TCL_ERROR;   /* Note tcol is not changed */

    /* Note don't use tcol_replace_intrep as we are keeping all 
       fields and ref counts the same */
    OBJTHDR(o) = thdr;
    return TCL_OK;
}


/* Deletes a range from a thdr_t. See asserts below for requirements */
void thdr_delete_range(thdr_t *thdr, Tcl_Size first, Tcl_Size count)
{
    Tcl_Size n;
    void *s, *d;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(first >= 0);

    if (first >= thdr->used)
        return;          /* Nothing to be deleted */
    
    if ((first + count) >= thdr->used)
        count = thdr->used - first;

    if (count <= 0)
        return;          /* Nothing to be deleted */

#ifdef NOTNEEDED    /* Deletion does not change sort state! */
    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
#endif

    /*
     * For all types other than BOOLEAN and OBJ, we can just memmove
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_copy(THDRELEMPTR(thdr, ba_t, 0), first, 
                THDRELEMPTR(thdr, ba_t, 0), first+count,
                thdr->used-(first+count));
        break;

    case TA_ANY:
    case TA_STRING:
        /*
         * We have to deal with reference counts here. For the objects
         * we are deleting we need to decrement the reference counts.
         */
        if (thdr->type == TA_ANY)
            thdr_decr_obj_refs(thdr, first, count);
        else
            thdr_decr_tas_refs(thdr, first, count);
        /* FALLTHRU - now we can just memmove like the other types */
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_BYTE:
        n = count + first;         /* Point beyond deleted elements */
        n = thdr_compute_move(thdr,
                              first, /* Offset to destination */
                              n,     /* Offset to where to copy from (src) */
                              thdr->used - n, /* # elements - src to end */
                              &d,             /* Receive ptr to dest */
                              &s);            /* Receive ptr to src */
        memmove(d, s, n);      /* NOT memcpy since overlapping copy */
        break;
    default:
        ta_type_panic(thdr->type);
    }

    thdr->used -= count;
}

void thdr_delete_indices(thdr_t *thdr, thdr_t *pindices)
{
    Tcl_Size prev;
    Tcl_Size *pindex, *plimit;

    TA_ASSERT(pindices->type == TA_INDEX);

    /* TBD - does this handle the case where thdr and pindices are the same ?*/

    /*
     * We have to be careful to delete from back to front so as to not
     * invalidate index positions when earlier ones are deleted
     */
    TA_ASSERT(pindices->sort_order == THDR_SORTED_ASCENDING || pindices->sort_order == THDR_SORTED_DESCENDING);

    /* TBD - this will be desperately slow. Fix - see remove_helper in cram */

    /* We always want to delete back to front. However the index array
     * may be presorted in any direction. So check and loop accordingly
     * Because duplicates might be present, we keep track of last deleted
     * index in variable prev.
     */
    if (pindices->used == 0)
        return;
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        /* Sort order is ascending so iterate index array back to front */
        plimit = THDRINDEXELEMPTR(pindices, 0);
        pindex = plimit + pindices->used - 1;
        prev = *pindex + 1;     /* Dummy value for first iteration */
        for ( ; pindex >= plimit; --pindex) {
            if (*pindex != prev && *pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            prev = *pindex;
        }
    } else {
        /* Sort order is descending so iterate index array front to back */
        pindex = THDRINDEXELEMPTR(pindices, 0);
        prev = *pindex + 1;     /* Dummy value for first iteration */
        for (plimit = pindex + pindices->used - 1; pindex <= plimit; ++pindex) {
            if (*pindex != prev && *pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            prev = *pindex;
        }
    }
}

void thdr_reverse(thdr_t *thdr)
{
    int orig_order;

    TA_ASSERT(! thdr_shared(thdr));

    if (thdr->used == 0)
        return;

    orig_order = thdr->sort_order;
    
#define SWAPALL(thdr_, type_)                                          \
    do {                                                                \
        type_ *front;                                                   \
        type_ *back;                                                    \
        front = THDRELEMPTR((thdr_), type_, 0);                       \
        back  = THDRELEMPTR((thdr_), type_, (thdr_)->used-1);        \
        while (front < back) {                                          \
            type_ temp;                                                 \
            temp = *front;                                              \
            *front++ = *back;                                           \
            *back-- = temp;                                             \
        }                                                               \
    } while (0)

    switch (thdr->type) {
    case TA_BOOLEAN:
        ba_reverse(THDRELEMPTR(thdr, ba_t, 0), 0, thdr->used);
        break;
    case TA_ANY:    SWAPALL(thdr, Tcl_Obj*); break;
    case TA_STRING:
        SWAPALL(thdr, tas_t*);
        thdr_lookup_free(thdr); /* Lookup table no longer valid */
        break;
    case TA_UINT:   /* Fall thru */
    case TA_INT:    SWAPALL(thdr, int); break;
    case TA_WIDE:   SWAPALL(thdr, Tcl_WideInt); break;
    case TA_DOUBLE: SWAPALL(thdr, double); break;
    case TA_BYTE:   SWAPALL(thdr, unsigned char); break;
    default:
        ta_type_panic(thdr->type);
    }

    switch (orig_order) {
    case THDR_SORTED_ASCENDING: thdr->sort_order = THDR_SORTED_DESCENDING; break;
    case THDR_SORTED_DESCENDING: thdr->sort_order = THDR_SORTED_ASCENDING; break;
    case THDR_SORTED_ASCENDING_NOCASE: thdr->sort_order = THDR_SORTED_DESCENDING_NOCASE; break;
    case THDR_SORTED_DESCENDING_NOCASE: thdr->sort_order = THDR_SORTED_ASCENDING_NOCASE; break;
    }
}

void thdr_copy(thdr_t *pdst, Tcl_Size dst_first,
               thdr_t *psrc, Tcl_Size src_first, Tcl_Size count, int insert)
{
    Tcl_Size nbytes;
    void *s, *d;
    Tcl_Size new_used;
    int elem_size;

    TA_ASSERT(pdst != psrc);
    TA_ASSERT(pdst->type == psrc->type);
    TA_ASSERT(! thdr_shared(pdst));
    TA_ASSERT(src_first >= 0);

    if (src_first >= psrc->used)
        return;          /* Nothing to be copied */
    if ((src_first + count) > psrc->used)
        count = psrc->used - src_first;
    if (count <= 0)
        return;                 /* Code below DEPENDS on this */

    new_used = thdr_recompute_occupancy(pdst, &dst_first, count, insert);
    TA_ASSERT(new_used <= pdst->usable); /* Caller should have ensured */

    pdst->sort_order = THDR_UNSORTED; /* TBD - optimize */

    /*
     * For all types other than BOOLEAN and OBJ, we can just memcpy
     * Those two types have complication in that BOOLEANs are compacted
     * into bytes and the copy may not be aligned on a byte boundary.
     * For OBJ types, we have to deal with reference counts.
     */
    switch (psrc->type) {
    case TA_BOOLEAN:
        d = THDRELEMPTR(pdst, ba_t, 0);
        if (insert) {
            /* First make room by copying bits up */
            ba_copy(d, dst_first+count, d, dst_first, pdst->used-dst_first);
        }
        /* Now insert or overwrite in place */
        ba_copy(d, dst_first, THDRELEMPTR(psrc, ba_t, 0), src_first, count);
        break;

    case TA_STRING:
        /*
         * TA_STRING pointers don't have (effectively) unlimited
         * reference counts and cannot follow the same pattern as TA_ANY
         * below. When duplicating, we may get a different pointer
         * so cannot just memcpy as in the TA_ANY case.
         * We need to do an explicit pointer-by-pointer copy.
         */
        if (insert)
            thdr_make_room(pdst, dst_first, count);
        else
            thdr_decr_tas_refs(pdst, dst_first, count);
        {
            tas_t **srctas, **dsttas, **srcend;
            srctas = THDRELEMPTR(psrc, tas_t *, src_first);
            srcend = srctas + count;
            dsttas = THDRELEMPTR(pdst, tas_t *, dst_first);
            while (srctas < srcend) {
                *dsttas++ = tas_ref(*srctas++);
            }
            thdr_lookup_addn(pdst, dst_first, count);
        }
        break;

    case TA_ANY:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        thdr_incr_obj_refs(psrc, src_first, count); /* Do this first */
        if (! insert) {
            /*
             * Overwriting so decr refs of existing elements.
             * Note this call takes care of the case where count exceeds
             * actual number in pdst
             */
            thdr_decr_obj_refs(pdst, dst_first, count);
        }
        /* FALLTHRU - Now we can just move memory like the other types */
    case TA_UINT:
    case TA_INT:
    case TA_WIDE:
    case TA_DOUBLE:
    case TA_BYTE:
        if (insert)
            thdr_make_room(pdst, dst_first, count);

        elem_size = pdst->elem_bits / CHAR_BIT;
        nbytes = count * elem_size;
        d = (dst_first * elem_size) + THDRELEMPTR(pdst, char, 0);
        s = (src_first * elem_size) + THDRELEMPTR(psrc, char, 0);
        memcpy(d, s, nbytes);
        break;

    default:
        ta_type_panic(psrc->type);
    }

    TA_ASSERT(new_used <= pdst->usable);
    pdst->used = new_used;
}

/* TBD - adding support for TA_ANY and TA_STRING here would make it complete
   and callable for setting any array type from a TA_STRING_SOURCE like
   thdr_puts_objs */
static TCL_RESULT
thdr_numerics_from_tas_strings(Tcl_Interp *ip,
                               thdr_t *pdst,
                               Tcl_Size dst_first,
                               tas_t *const pptas_src[],
                               Tcl_Size count,
                               int insert)
{
    tas_t * const *pptas;
    tas_t * const *end;

    /* Need to validate all values first to ensure they can
     * be translated to numerics since on error dstp must be
     * unmodified.
     */

    /* IMPORTANT NOTE ****
       This does NOT update pdst->used
    ******/

    /* TBD - may be faster to just create a temp array and then
       call thdr_clone rather than looping twice to validate and
       then store */

#define TASVALIDATE(type_, fn_)                         \
    do {                                                \
        type_ val;                                      \
        TCL_RESULT status;                              \
        pptas = pptas_src;                              \
        end = pptas + count;                            \
        while (pptas < end) {                           \
            status = fn_(ip, (*pptas)->s, &val);        \
            if (status != TCL_OK)                       \
                return status;                          \
            ++pptas;                                    \
        }                                               \
    } while (0)

#define TAS2NUM(type_, fn_)                             \
    do {                                                \
        type_ *p;                                       \
        TASVALIDATE(type_, fn_); \
        /* No errors now do the actual conversion. */ \
        pptas = pptas_src;                              \
        end = pptas + count;                            \
        if (insert) \
            thdr_make_room(pdst, dst_first, count); \
        p = THDRELEMPTR(pdst, type_, dst_first);        \
        while (pptas < end) {                           \
            TA_NOFAIL(fn_(ip, (*pptas)->s, p), TCL_OK); \
            ++p;                                        \
            ++pptas;                                    \
        }                                               \
    } while (0)

    if (pdst->type == TA_DOUBLE) {
        double *pdbl;
        TASVALIDATE(double, ta_get_double_from_string);
        /* No errors, now do the actual conversion */
        pdbl = THDRELEMPTR(pdst, double, dst_first);
        if (insert)
            thdr_make_room(pdst, dst_first, count);
        pptas = pptas_src;
        end = pptas + count;
        while (pptas < end) {
            TA_NOFAIL(ta_get_double_from_string(ip, (*pptas)->s, pdbl), TCL_OK);
            ++pptas;
            ++pdbl;
        }
    } else if (pdst->type == TA_BOOLEAN) {
        ba_t *baP;
        ba_t ba, ba_mask, off;
        Tcl_Size i;

        TASVALIDATE(int, ta_get_boolean_from_string);

        /* NOTE: same code as in thdr_put_objs except there we 
           are dealing with Tcl_Obj's and thus cannot be shared.
           If you fix a bug here, fix it there as well*/
        if (insert)
            thdr_make_room(pdst, dst_first, count);

        baP = THDRELEMPTR(pdst, ba_t, dst_first / BA_UNIT_SIZE);
        off = dst_first % BA_UNIT_SIZE; /* Offset of bit within a char */
        ba_mask = BITPOSMASK(off); /* The bit pos corresponding to 'first' */
        if (off != 0) {
            /*
             * Offset is off within a ba_t. Get the ba_t at that location
             * preserving the preceding bits within the char.
             */
            ba = *baP & BITPOSMASKLT(off);
        } else {
            ba = 0;
        }
        for (i = 0; i < count; ++i) {
            int ival;
            TA_NOFAIL(ta_get_boolean_from_string(ip, pptas_src[i]->s, &ival), TCL_OK);
            if (ival)
                ba |= ba_mask;
            ba_mask = BITMASKNEXT(ba_mask);
            if (ba_mask == 0) {
                *baP++ = ba;
                ba = 0;
                ba_mask = BITPOSMASK(0);
            }
        }
        if (ba_mask != BITPOSMASK(0)) {
            /* We have some leftover bits in ba that need to be stored.
             * We need to *merge* these into the corresponding word
             * keeping the existing high index bits.
             * Note the bit indicated by ba_mask also has to be preserved,
             * not overwritten.
             */
            *baP = ba | (*baP & BITMASKGE(ba_mask));
        }
    } else {
        switch (pdst->type) {
        case TA_BYTE: TAS2NUM(unsigned char, ta_get_uint8_from_string); break;
        case TA_INT:  TAS2NUM(int, ta_get_int_from_string); break;
        case TA_UINT: TAS2NUM(unsigned int, ta_get_uint_from_string); break;
        case TA_WIDE: TAS2NUM(Tcl_WideInt, ta_get_wide_from_string); break;
        }
    }
#undef TAS2NUM
#undef TASVALIDATE
    return TCL_OK;
}


/* Copies partial content from one thdr_t to another of a different type. 
   See asserts below for requirements */
TCL_RESULT
thdr_copy_cast(Tcl_Interp *ip,
               thdr_t *pdst,
               Tcl_Size dst_first,
               thdr_t *psrc,
               Tcl_Size src_first,
               Tcl_Size count,
               int insert,
               int strict)
{
    Tcl_Size new_used;
    TCL_RESULT status;
    ta_value_t error_value;

    TA_ASSERT(pdst != psrc);
    TA_ASSERT(! thdr_shared(pdst));
    TA_ASSERT(src_first >= 0);

    error_value.type = TA_NONE;

    if (pdst->type == psrc->type) {
        thdr_copy(pdst, dst_first, psrc, src_first, count, insert);
        return TCL_OK;
    }

    if (src_first >= psrc->used)
        return TCL_OK;          /* Nothing to be copied */
    if ((src_first + count) > psrc->used)
        count = psrc->used - src_first;
    if (count <= 0)
        return TCL_OK;                 /* Code below DEPENDS on this */

    /* Special case src type TA_ANY since we already have a routine that does
       all the hard work to convert Tcl_Obj * arrays to any type */
    if (psrc->type == TA_ANY) {
        return thdr_put_objs(ip, pdst, dst_first, count, 
                             THDRELEMPTR(psrc, Tcl_Obj*, src_first),
                             insert);
    }
    
    new_used = thdr_recompute_occupancy(pdst, &dst_first, count, insert);
    TA_ASSERT(new_used <= pdst->usable); /* Caller should have ensured */

    pdst->sort_order = THDR_UNSORTED; /* TBD - optimize */

#define NUM2NUM_UNCHECKED(dsttype_, srctype_)           \
    do {                                                \
        dsttype_ *to;                                   \
        srctype_ *from;                                 \
        to = THDRELEMPTR(pdst, dsttype_, dst_first);    \
        from = THDRELEMPTR(psrc, srctype_, src_first);  \
        while (count--) { *to++ = (dsttype_) *from++; } \
    } while (0)

#define NUM2NUM_CHECKED(dsttype_, srctype_, low_, high_)        \
    do {                                                        \
        dsttype_ *to;                                           \
        srctype_ *from;                                         \
        to = THDRELEMPTR(pdst, dsttype_, dst_first);            \
        from = THDRELEMPTR(psrc, srctype_, src_first);          \
        while (count--) {                                       \
            if (*from < (low_) || *from > (high_)) {            \
                error_value.type = TA_WIDE;                     \
                error_value.wval = *from;                       \
                status = TCL_ERROR;                             \
                break;                                          \
            }                                                   \
            *to++ = (dsttype_) *from++;                         \
        }                                                       \
    } while (0)

#define NUM2NUM(dsttype_, srctype_, low_, high_)        \
    do { \
        if (strict)                                             \
            NUM2NUM_CHECKED(dsttype_, srctype_, low_, high_); \
        else \
            NUM2NUM_UNCHECKED(dsttype_, srctype_); \
    } while (0)
    
#define DOUBLE2NUM_CHECKED(dsttype_, low_, high_)       \
    do {                                                \
        dsttype_ *to;                                   \
        double *from;                                   \
        to = THDRELEMPTR(pdst, dsttype_, dst_first);    \
        from = THDRELEMPTR(psrc, double, src_first);    \
        while (count--) {                               \
            double integer_part;                        \
            if (modf(*from, &integer_part) != 0 ||      \
                integer_part < (low_) ||                \
                integer_part > (high_)) {               \
                error_value.type = TA_DOUBLE;           \
                error_value.dval = *from;               \
                status = TCL_ERROR;                     \
                break;                                  \
            }                                           \
            *to++ = (dsttype_) integer_part;            \
            ++from;                                     \
        }                                               \
    } while (0)

#define DOUBLE2NUM(dsttype_, low_, high_)        \
    do { \
        if (strict)                                             \
            DOUBLE2NUM_CHECKED(dsttype_, low_, high_); \
        else \
            NUM2NUM_UNCHECKED(dsttype_, double); \
    } while (0)

    /* TBD - optimize */
#define BOOL2NUM(dsttype_)                                              \
    do {                                                                \
        ba_t *baP = THDRELEMPTR(psrc, ba_t, 0);                         \
        dsttype_ *to;                                                   \
        Tcl_Size i, end;                                                     \
        to = THDRELEMPTR(pdst, dsttype_, dst_first);                    \
        for (i = src_first, end = src_first + count ; i < end; ++i)     \
            *to++ = ba_get(baP, i);                                     \
    } while (0)

#define NUM2ANY(srctype_, cvt_fn_)                     \
    do {                                                \
        Tcl_Obj * *to;                                  \
        srctype_ *from;                                 \
        to = THDRELEMPTR(pdst, Tcl_Obj*, dst_first);    \
        from = THDRELEMPTR(psrc, srctype_, src_first);  \
        while (count--) {                               \
            *to = cvt_fn_(*from++);                    \
            Tcl_IncrRefCount(*to);                      \
            ++to;                                       \
        }                                               \
    } while (0)
    
#define NUM2STRING(srctype_, cvt_fn_)                   \
    do {                                                \
        tas_t **to;                                     \
        srctype_ *from;                                 \
        to = THDRELEMPTR(pdst, tas_t*, dst_first);      \
        from = THDRELEMPTR(psrc, srctype_, src_first);  \
        while (count--) {                               \
            *to = cvt_fn_(*from++);                     \
            ++to;                                       \
        }                                               \
    } while (0)
    
/* Note NUM2BOOL, unlike others only copies the middle bits (whole ba_t's) */
/* Variable ba must have been initialized before this macro is invoked */
#define NUM2BOOL(type_)                                                 \
    do {                                                                \
        type_ *src = THDRELEMPTR(psrc, type_, src_first);               \
        type_ *end = src + count;                                       \
        while (src < end) {                                             \
            if (*src != 0)                                              \
                ba |= ba_mask;                                          \
            ba_mask = BITMASKNEXT(ba_mask);                             \
            if (ba_mask == 0) {                                         \
                *baP++ = ba;                                            \
                ba = 0;                                                 \
                ba_mask = BITPOSMASK(0);                                \
            }                                                           \
            ++src;                                                      \
        }                                                               \
    } while (0)

    /* Remember we handled psrc->type == TA_ANY above for *all* dst types */
    
    status = TCL_OK;
    switch (pdst->type) {
    case TA_BOOLEAN:
        if (psrc->type == TA_STRING) {
            status = thdr_numerics_from_tas_strings
                (ip, pdst, dst_first,
                 THDRELEMPTR(psrc, tas_t *, src_first), count, insert);
            break;
        } else {
            register ba_t *baP;
            ba_t ba, ba_mask, off;

            if (insert)
                thdr_make_room(pdst, dst_first, count);
            
            baP = THDRELEMPTR(pdst, ba_t, dst_first / BA_UNIT_SIZE);
            off = dst_first % BA_UNIT_SIZE; /* Offset of bit within a char */
            ba_mask = BITPOSMASK(off); /* The bit pos corresponding to 'first' */
            if (off != 0) {
                /*
                 * Offset is off within a ba_t. Get the ba_t at that location
                 * preserving the preceding bits within the char.
                 */
                ba = *baP & BITPOSMASKLT(off);
            } else
                ba = 0;

            switch (psrc->type) {
            case TA_BYTE: NUM2BOOL(unsigned char); break;
            case TA_INT:  NUM2BOOL(int); break;
            case TA_UINT: NUM2BOOL(unsigned int); break;
            case TA_WIDE: NUM2BOOL(Tcl_WideInt); break;
            case TA_DOUBLE: NUM2BOOL(double); break;
            }

            if (ba_mask != BITPOSMASK(0)) {
                /* We have some leftover bits in ba that need to be stored.
                 * We need to *merge* these into the corresponding word
                 * keeping the existing high index bits.
                 * Note the bit indicated by ba_mask also has to be preserved,
                 * not overwritten.
                 */
                *baP = ba | (*baP & BITMASKGE(ba_mask));
            }
        }
        break;

    case TA_STRING:
        if (insert)
            thdr_make_room(pdst, dst_first, count);
        else
            thdr_decr_tas_refs(pdst, dst_first, count);
        switch (psrc->type) {
        case TA_BOOLEAN:
            {
                ba_t *baP = THDRELEMPTR(psrc, ba_t, 0);
                Tcl_Size low, end;
                tas_t **podst;
                tas_t *one, *zero;
                one = tas_alloc_nbytes("1", 1);
                zero = tas_alloc_nbytes("0", 1);
                podst = THDRELEMPTR(pdst, tas_t *, dst_first);
                for (low = src_first, end = src_first+count; low < end; ++podst, ++low) {
                    /* 
                     * We reuse strings 0 and 1 for memory efficiency.
                     * Note order in which we assign and ref. This is because
                     * tas_t refcounts are single byte and once they are at max
                     * a new tas_t is returned. Doing it as shown ensures we
                     * don't keep allocating a new tas_t once the ref count
                     * in one/zero was at max as would happen if we did
                     *    *podst = tas_ref(one)
                     */
                    if (ba_get(baP, low)) {
                        *podst = one;
                        one = tas_ref(*podst);
                    } else {
                        *podst = zero;
                        zero = tas_ref(*podst);
                    }
                }
                tas_unref(one);
                tas_unref(zero);
                break; /* src == TA_BOOLEAN */
            }
            break;
        case TA_BYTE: NUM2STRING(unsigned char, tas_from_int); break;
        case TA_INT: NUM2STRING(int, tas_from_int); break;
        case TA_UINT: NUM2STRING(unsigned int, tas_from_uint); break;
        case TA_WIDE: NUM2STRING(Tcl_WideInt, tas_from_wide); break;
        case TA_DOUBLE: NUM2STRING(double, tas_from_double); break;
        }
        thdr_lookup_addn(pdst, dst_first, count);
        break; /* dst == TA_STRING */

    case TA_ANY:
        if (insert)
            thdr_make_room(pdst, dst_first, count);
        else {
            /*
             * Overwriting so decr refs of existing elements.
             * Note this call handles the case where count exceeds
             * actual number in pdst
             */
            thdr_decr_obj_refs(pdst, dst_first, count);
        }
        switch (psrc->type) {
        case TA_BOOLEAN:
            {
                ba_t *baP = THDRELEMPTR(psrc, ba_t, 0);
                Tcl_Size low, end;
                Tcl_Obj **podst;
                podst = THDRELEMPTR(pdst, Tcl_Obj *, dst_first);
                for (low = src_first, end = src_first+count; low < end; ++podst, ++low) {
                    *podst = Tcl_NewIntObj(ba_get(baP, low));
                    Tcl_IncrRefCount(*podst);
                }
            }
            break; /* src == TA_BOOLEAN */

        case TA_BYTE: NUM2ANY(unsigned char, Tcl_NewIntObj); break;
        case TA_INT:NUM2ANY(int, Tcl_NewIntObj); break;
        case TA_UINT:NUM2ANY(unsigned int, Tcl_NewWideIntObj); break;
        case TA_WIDE:NUM2ANY(Tcl_WideInt, Tcl_NewWideIntObj); break;
        case TA_DOUBLE: NUM2ANY(double, Tcl_NewDoubleObj); break;
        case TA_STRING: 
            {
                tas_t **srctas, **srcend;
                Tcl_Obj **podst;
                podst = THDRELEMPTR(pdst, Tcl_Obj *, dst_first);
                srctas = THDRELEMPTR(psrc, tas_t *, src_first);
                srcend = srctas + count;
                while (srctas < srcend) {
                    *podst = Tcl_NewStringObj((*srctas)->s, -1);
                    Tcl_IncrRefCount(*podst);
                    ++podst;
                    ++srctas;
                }
                break; /* src == TA_STRING */
            }
        }
        break; /* dst == TA_ANY */

    default:
        /* Numeric destination */
        if (psrc->type == TA_STRING) {
            status = thdr_numerics_from_tas_strings
                (ip, pdst, dst_first,
                 THDRELEMPTR(psrc, tas_t *, src_first), count, insert);
            break;
        }
        if (insert)
            thdr_make_room(pdst, dst_first, count);
        switch (pdst->type) {
        case TA_BYTE:
            switch (psrc->type) {
            case TA_BOOLEAN: BOOL2NUM(unsigned char); break;
            case TA_INT: NUM2NUM(unsigned char, int, 0, UCHAR_MAX); break;
            case TA_UINT: NUM2NUM(unsigned char, unsigned int, 0, UCHAR_MAX); break;
            case TA_WIDE: NUM2NUM(unsigned char, Tcl_WideInt, 0, UCHAR_MAX); break;
            case TA_DOUBLE: DOUBLE2NUM(unsigned char, 0, UCHAR_MAX); break;
            }
            break;
        case TA_INT:
            switch (psrc->type) {
            case TA_BOOLEAN: BOOL2NUM(int); break;
            case TA_BYTE: NUM2NUM_UNCHECKED(int, unsigned char); break;
            case TA_UINT: NUM2NUM(int, unsigned int, 0, INT_MAX); break;
            case TA_WIDE: NUM2NUM(int, Tcl_WideInt, INT_MIN, INT_MAX); break;
            case TA_DOUBLE: DOUBLE2NUM(int, INT_MIN, INT_MAX); break;
            }
            break;
        case TA_UINT:
            switch (psrc->type) {
            case TA_BOOLEAN: BOOL2NUM(unsigned int); break;
            case TA_BYTE: NUM2NUM_UNCHECKED(unsigned int, unsigned char); break;
            case TA_INT: NUM2NUM(unsigned int, int, 0, UINT_MAX); break;
            case TA_WIDE: NUM2NUM(unsigned int, Tcl_WideInt, 0, UINT_MAX); break;
            case TA_DOUBLE: DOUBLE2NUM(unsigned int, 0, UINT_MAX); break;
            }
            break;
        case TA_WIDE:
            switch (psrc->type) {
            case TA_BOOLEAN: BOOL2NUM(Tcl_WideInt); break;
            case TA_BYTE: NUM2NUM_UNCHECKED(Tcl_WideInt, unsigned char); break;
            case TA_INT: NUM2NUM_UNCHECKED(Tcl_WideInt, int); break;
            case TA_UINT: NUM2NUM_UNCHECKED(Tcl_WideInt, unsigned int); break;
            case TA_DOUBLE: DOUBLE2NUM(Tcl_WideInt, INT64_MIN, INT64_MAX); break;
            }
            break;
        case TA_DOUBLE:
            switch (psrc->type) {
            case TA_BOOLEAN: BOOL2NUM(double); break;
            case TA_BYTE: NUM2NUM_UNCHECKED(double, unsigned char); break;
            case TA_INT: NUM2NUM_UNCHECKED(double, int); break;
            case TA_UINT: NUM2NUM_UNCHECKED(double, unsigned int); break;
            case TA_WIDE: NUM2NUM_UNCHECKED(double, Tcl_WideInt); break;
            }
            break;
        default: ta_type_panic(psrc->type);
        }
    }

    if (status == TCL_OK) {
        TA_ASSERT(new_used <= pdst->usable);
        pdst->used = new_used;
    } else if (error_value.type == TA_WIDE) {
        /* If non-0, the contained value did not fit into destination width */
        status = ta_integer_overflow_error(ip, ta_type_string(pdst->type), error_value.wval);
    } else if (error_value.type == TA_DOUBLE) {
        status = ta_integer_overflow_from_double_error(ip, ta_type_string(pdst->type), error_value.dval);
    }

    return status;

#undef NUM2NUM_UNCHECKED
#undef NUM2NUM_CHECKED
#undef BOOL2NUM
#undef NUM2ANY
#undef NUM2STRING
}

/* Copies partial content from one thdr_t to another in reverse.
   See asserts below for requirements */
void thdr_copy_reversed(thdr_t *pdst, Tcl_Size dst_first,
                       thdr_t *psrc, Tcl_Size src_first, Tcl_Size count)
{
    TA_ASSERT(pdst != psrc);
    TA_ASSERT(pdst->type == psrc->type);
    TA_ASSERT(! thdr_shared(pdst));
    TA_ASSERT(src_first >= 0);

    if (src_first >= psrc->used)
        return;          /* Nothing to be copied */
    if ((src_first + count) > psrc->used)
        count = psrc->used - src_first;
    if (count <= 0)
        return;                 /* Code below DEPENDS on this */
    TA_ASSERT((dst_first + count) <= pdst->usable);

    if (dst_first < 0)
        dst_first = 0;
    else if (dst_first > pdst->used)
        dst_first = pdst->used;

    pdst->sort_order = THDR_UNSORTED; /* TBD - optimize */

#define COPYREVERSE(type_, pdst_, doff_, psrc_, soff_, count_)         \
    do {                                                               \
        type_ *src;                                                    \
        type_ *dst;                                                    \
        Tcl_Size i = (count_);                                         \
        src        = THDRELEMPTR((psrc_), type_, (soff_));             \
        dst        = THDRELEMPTR((pdst_), type_, 0);                   \
        dst += (doff_) + i - 1;                                        \
        while (i--) {                                                  \
            /* Remember caller ensured no overlap between src & dst */ \
            *dst-- = *src++;                                           \
        }                                                              \
    } while (0)

    switch (psrc->type) {
    case TA_BOOLEAN:
        ba_copy(THDRELEMPTR(pdst, ba_t, 0), dst_first,
                THDRELEMPTR(psrc, ba_t, 0), src_first, count);
        ba_reverse(THDRELEMPTR(pdst, ba_t, 0), dst_first, count);
        break;
    case TA_ANY:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment the reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        thdr_incr_obj_refs(psrc, src_first, count); /* Do this first */
        /* Note this call take care of the case where count exceeds
         * actual number in pdst
         */
        thdr_decr_obj_refs(pdst, dst_first, count);
        COPYREVERSE(Tcl_Obj*, pdst, dst_first, psrc, src_first, count);
        break;

    case TA_STRING:
        /*
         * TA_STRING pointers don't have (effectively) unlimited
         * reference counts and cannot follow the same pattern as TA_ANY
         * below. When duplicating, we may get a different pointer
         * so cannot just memcpy as in the TA_ANY case.
         * We need to do an explicit pointer-by-pointer copy.
         */
        thdr_decr_tas_refs(pdst, dst_first, count);
        {
            tas_t **srctas, **dsttas;
            Tcl_Size i  = count;
            srctas = THDRELEMPTR(psrc, tas_t *, src_first);
            dsttas = THDRELEMPTR(pdst, tas_t *, 0);
            dsttas += dst_first + i - 1;
            while (i--)
                *dsttas-- = tas_ref(*srctas++);
            thdr_lookup_addn(pdst, dst_first, count);
        }
        break;

    case TA_UINT:
    case TA_INT:
        COPYREVERSE(int, pdst, dst_first, psrc, src_first, count);
        break;
    case TA_WIDE:
        COPYREVERSE(Tcl_WideInt, pdst, dst_first, psrc, src_first, count);
        break;
    case TA_DOUBLE:
        COPYREVERSE(double, pdst, dst_first, psrc, src_first, count);
        break;
    case TA_BYTE:
        COPYREVERSE(unsigned char, pdst, dst_first, psrc, src_first, count);
        break;
    default:
        ta_type_panic(psrc->type);
    }

    if ((dst_first + count) > pdst->used)
        pdst->used = dst_first + count;

    TA_ASSERT(pdst->used <= pdst->usable);

    return;
}

/*
 * Converts a thdr to an integer type thdr used for indexing. 
 * The main difference with a normal cast to an integer column
 * is that bit arrays are converted to a sequence of integers corresponding
 * to the bits that are set, not as 0/1 of the same length as the
 * source column.
 * Returned column is ref incremented already since for integer
 * columns, the same one is returned if there is no span.
 */
thdr_t *thdr_to_indexcolumn(Tcl_Interp *ip, thdr_t *thdr, span_t *span)
{
    thdr_t *to;
    Tcl_Size count, start;

    if (span) {
        start = span->first;
        count = span->count;
    } else {
        /*
         * If column is already index type, and no span specified, we can just
         * return the same column
         */
        if (thdr->type == TA_INDEX) {
            thdr_incr_refs(thdr);
            return thdr;
        }
        start = 0;
        count = thdr->used;
    }

    if (thdr->type != TA_BOOLEAN) {
        to = thdr_alloc(ip, TA_INDEX, count);
        if (to == NULL)
            return to;
        if (thdr_copy_cast(ip, to, 0, thdr, start, count, 0, 1) != TCL_OK) {
            thdr_decr_refs(to);
            return NULL;
        }
    } else {
        ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
        Tcl_Size *p;
        Tcl_Size i, ones;

        /* TBD - optimize at least for case where start == 0 */
        ones = ba_count_ones(baP, start, start+count);
        to = thdr_alloc(ip, TA_INDEX, ones);
        p = THDRINDEXELEMPTR(to, 0);
        for (i = 0; i < count; ++i) {
            if (ba_get(baP, start+i))
                *p++ = i;
        }
        TA_ASSERT((p-THDRINDEXELEMPTR(to, 0)) == ones);
        to->used = ones;
        to->sort_order = THDR_SORTED_ASCENDING;
    }
    thdr_incr_refs(to);
    return to;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone(Tcl_Interp *ip, thdr_t *psrc, Tcl_Size minsize, span_t *span)
{
    thdr_t *thdr;
    Tcl_Size count, start;

    if (span) {
        start = span->first;
        count = span->count;
    } else {
        start = 0;
        count = psrc->used;
    }

    if (minsize == 0)
        minsize = psrc->usable;
    else if (minsize < count)
        minsize = count;

    /* TBD - optimize these two calls */
    thdr = thdr_alloc(ip, psrc->type, minsize);
    if (thdr) {
        thdr_copy(thdr, 0, psrc, start, count, 0);
        thdr->sort_order = psrc->sort_order;
    }
    return thdr;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone_reversed(Tcl_Interp *ip, thdr_t *psrc, Tcl_Size minsize, span_t *span)
{
    thdr_t *thdr;
    int orig_order;
    Tcl_Size count, start;

    if (span) {
        start = span->first;
        count = span->count;
    } else {
        start = 0;
        count = psrc->used;
    }

    orig_order = psrc->sort_order;

    if (minsize == 0)
        minsize = psrc->usable;
    else if (minsize < count)
        minsize = count;

    /* TBD - optimize these two calls */
    thdr = thdr_alloc(ip, psrc->type, minsize);
    if (thdr) {
        thdr_copy_reversed(thdr, 0, psrc, start, count);
        switch (orig_order) {
        case THDR_SORTED_ASCENDING: thdr->sort_order = THDR_SORTED_DESCENDING; break;
        case THDR_SORTED_DESCENDING: thdr->sort_order = THDR_SORTED_ASCENDING; break;
        case THDR_SORTED_ASCENDING_NOCASE: thdr->sort_order = THDR_SORTED_DESCENDING_NOCASE; break;
        case THDR_SORTED_DESCENDING_NOCASE: thdr->sort_order = THDR_SORTED_ASCENDING_NOCASE; break;
        }
    }
    return thdr;
}

Tcl_Obj *tcol_index(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Size index)
{
    thdr_t *thdr;
    span_t *span;
    Tcl_Size first, count;

    if (tcol_convert(ip, tcol) != TCL_OK)
        return NULL;
    thdr = OBJTHDR(tcol);
    span = OBJTHDRSPAN(tcol);
    if (span) {
        first = span->first;
        count = span->count;
    } else {
        first = 0;
        count = thdr->used;
    }
    if (index < 0 || index >= count) {
        ta_index_range_error(ip, index);
        return NULL;
    }
    return thdr_index(thdr, first + index);
}

Tcl_Obj *tcol_range(Tcl_Interp *ip, Tcl_Obj *osrc, Tcl_Size low, Tcl_Size count,
                     int fmt)
{
    Tcl_Size end;
    thdr_t *psrc;
    Tcl_Obj *o;
    span_t *span;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);
    TA_ASSERT(tcol_affirm(osrc));

    psrc = OBJTHDR(osrc);
    span = OBJTHDRSPAN(osrc);
    if (span) {
        TA_ASSERT(span->first >= 0);
        TA_ASSERT(span->count >= 0);
        TA_ASSERT((span->first + span->count) <= psrc->used);
        if (low > span->count) {
            /* Starting index is beyond column size. Create empty span */
            low = span->first;
            end = low;
        } else {
            low += span->first;
            end = low + count;
            if (end > (span->first + span->count))
                end = span->first + span->count;
        }
    } else {
        end = low + count;
        if (end > psrc->used)
            end = psrc->used;
    }

    if (fmt == TA_FORMAT_TARRAY)
        return tcol_new_span(psrc, low, end-low);

    /* TBD - optimize by allocating array and calling Tcl_NewListObj */
#define tcol_range_COPY(type_, objfn_)                                  \
    do {                                                                \
        type_ *p = THDRELEMPTR(psrc, type_, low);                \
        type_ *pend = THDRELEMPTR(psrc, type_, end);             \
        while (p < pend) {                                              \
            if (fmt == TA_FORMAT_DICT)                                  \
                Tcl_ListObjAppendElement(ip, o, Tcl_NewWideIntObj(low++));  \
            Tcl_ListObjAppendElement(ip, o, objfn_(*p++));              \
        }                                                               \
    } while (0)

    /* Even dicts more efficiently built as lists and shimmered as necessary */
    o = Tcl_NewListObj(fmt == TA_FORMAT_DICT ? (2*(end-low)) : (end-low), NULL);
    switch (psrc->type) {
    case TA_BOOLEAN:
        {
            ba_t *baP = THDRELEMPTR(psrc, ba_t, 0);
            while (low < end) {
                if (fmt == TA_FORMAT_DICT)
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewWideIntObj(low));
                Tcl_ListObjAppendElement(ip, o,
                                         Tcl_NewIntObj(ba_get(baP, low)));
                ++low;
            }
        }
        break;
    case TA_UINT:
        tcol_range_COPY(unsigned int, Tcl_NewWideIntObj);
        break;
    case TA_INT:
        tcol_range_COPY(int, Tcl_NewIntObj);
        break;
    case TA_WIDE:
        tcol_range_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
        break;
    case TA_DOUBLE:
        tcol_range_COPY(double, Tcl_NewDoubleObj);
        break;
    case TA_BYTE:
        tcol_range_COPY(unsigned char, Tcl_NewIntObj);
        break;
    case TA_ANY:
        tcol_range_COPY(Tcl_Obj*, (Tcl_Obj*)); /* Cast acts as a null function */
        break;
    case TA_STRING:
        tcol_range_COPY(tas_t*, tas_to_obj);
        break;
    default:
        ta_type_panic(psrc->type);
    }

    return o;
}


/*
 * Convert a TArray Tcl_Obj to one that is suitable for modifying.
 * The Tcl_Obj must NOT be shared.
 * There are the following cases to consider:
 * (1) Even though tcol is unshared, the corresponding thdr_t might
 *     still be shared (pointed to from elsewhere). In this case
 *     also, we clone the thdr_t and store it as the new internal rep.
 * (2) If its thdr_t is unshared, we can modify in
 *     place, unless 
 * (3) thdr_t is too small in which case we have to reallocate it.
 * (4) The tcol is a span of the thdr in which case we need to 
 *     allocate a non-span version.
 *
 * Invalidates the string rep in all cases.
 * Only fails on memory allocation failure.
 *
 * TBD - check all calls to tcol_make_modifiable to see if passed prefsize
 * is sensible else array will grow one element at a time with bad 
 * performance. Maybe even always allocate extra independent of prefsize.
 *
 * TBD - make a inline fastpath version of this
 */
TCL_RESULT tcol_make_modifiable(Tcl_Interp *ip,
                                Tcl_Obj *tcol, Tcl_Size minsize, Tcl_Size prefsize)
{
    thdr_t *thdr;
    span_t *span;

    TA_ASSERT(! Tcl_IsShared(tcol));
    if (tcol_convert(ip, tcol) != TCL_OK)
        return TCL_ERROR;

    thdr = OBJTHDR(tcol);
    span = OBJTHDRSPAN(tcol);
    if (span) {
        /* Case (4) */
        thdr = thdr_clone(ip, thdr, minsize > prefsize ? minsize : prefsize, span);
        if (thdr == NULL)
            return TCL_ERROR;   /* Note tcol is not changed */
        tcol_replace_intrep(tcol, thdr, NULL);
        TA_ASSERT(tcol_thdr(tcol)->usable >= minsize);
        return TCL_OK;
    }
 
    if (thdr->usable >= minsize && !thdr_shared(thdr)) {
        /* Case (2) - just reuse, invalidate the string rep */
        Tcl_InvalidateStringRep(tcol);
        return TCL_OK;
    }

    /* We are going to have to reallocate anyway so make a lame attempt
       to be half-clever about it */

    if (minsize < thdr->used)
        minsize = thdr->used;   /* We will NOT shrink below this */
    if (prefsize == 0)
        prefsize = thdr->usable;
    if (minsize > prefsize)
        prefsize = minsize;

    /* Ensure a minimum free space remaining after reallocation if we are growing the array */
    if (prefsize > thdr->used) {
        if (thdr_roundup_alloc_size(thdr->used, prefsize, &prefsize) != TCL_OK)
            return ta_limit_error(ip, prefsize);
    }

    if (thdr_shared(thdr)) {
        /* Case (1) */
        thdr = thdr_clone(ip, thdr, prefsize, NULL);
        if (thdr == NULL)
            return TCL_ERROR;   /* Note tcol is not changed */
        tcol_replace_intrep(tcol, thdr, NULL);
    } else {
        /* Case (3). */
        TA_ASSERT(thdr->usable < minsize);
        if (tcol_grow_intrep(ip, tcol, prefsize) != TCL_OK)
            return TCL_ERROR;
    }

    TA_ASSERT(tcol_thdr(tcol)->usable >= minsize);
    return TCL_OK;
}


/* Returns a Tcl_Obj for a TArray slot. NOTE: WITHOUT its ref count incremented */
Tcl_Obj * thdr_index(thdr_t *thdr, Tcl_Size index)
{
    TA_ASSERT(index >= 0 && index < thdr->used);

    switch (thdr->type) {
    case TA_BOOLEAN:
        return Tcl_NewIntObj(ba_get(THDRELEMPTR(thdr, ba_t, 0), index));
    case TA_UINT:
        return Tcl_NewWideIntObj(*THDRELEMPTR(thdr, unsigned int, index));
    case TA_INT:
        return Tcl_NewIntObj(*THDRELEMPTR(thdr, int, index));
    case TA_WIDE:
        return Tcl_NewWideIntObj(*THDRELEMPTR(thdr, Tcl_WideInt, index));
    case TA_DOUBLE:
        return Tcl_NewDoubleObj(*THDRELEMPTR(thdr, double, index));
    case TA_BYTE:
        return Tcl_NewIntObj(*THDRELEMPTR(thdr, unsigned char, index));
    case TA_ANY:
        return *THDRELEMPTR(thdr, Tcl_Obj *, index);
    case TA_STRING:
        return tas_to_obj(*THDRELEMPTR(thdr, tas_t*, index));
    default:
        ta_type_panic(thdr->type);
        return NULL;
    }
}

/* NOTE: Caller must call ta_value_clear as needed. See comment for
   ta_value_from_obj */
void thdr_index_ta_value(thdr_t *thdr, Tcl_Size index, ta_value_t *tavP)
{
    TA_ASSERT(index >= 0 && index < thdr->used);

    tavP->type = thdr->type;
    switch (tavP->type) {
    case TA_BOOLEAN:
        tavP->bval = ba_get(THDRELEMPTR(thdr, ba_t, 0), index); break;
    case TA_UINT:
        tavP->uival = *THDRELEMPTR(thdr, unsigned int, index); break;
    case TA_INT:
        tavP->ival = *THDRELEMPTR(thdr, int, index); break;
    case TA_WIDE:
        tavP->wval = *THDRELEMPTR(thdr, Tcl_WideInt, index); break;
    case TA_DOUBLE:
        tavP->dval = *THDRELEMPTR(thdr, double, index); break;
    case TA_BYTE:
        tavP->ucval = *THDRELEMPTR(thdr, unsigned char, index); break;
    case TA_ANY:
        /* Note failure to increment ref count is intentional as 
           ta_value_clear does not release it. See comments to
           ta_value_from_obj */
        tavP->oval = *THDRELEMPTR(thdr, Tcl_Obj *, index); break;
    case TA_STRING:
        /* NOTE CALLER has to unref! */
        tavP->ptas = tas_ref(*THDRELEMPTR(thdr, tas_t *, index)); break;
    default:
        ta_type_panic(thdr->type);
    }
}

/*
 * Returns a string representation of the element at the specified
 * thdr index. In the case of TA_ANY or TA_STRING this points to the
 * element's stored string. For numeric types, the string is stored
 * in the buffer pointed to by BufP which must be at least 
 * TA_NUMERIC_SPACE bytes
 */
char *thdr_index_string(thdr_t *thdr, Tcl_Size thdr_index, char buf[TA_NUMERIC_SPACE])
{
    TA_ASSERT(thdr->used > thdr_index);
    switch (thdr->type) {
    case TA_BOOLEAN:
        buf[0] = ba_get(THDRELEMPTR(thdr, ba_t, 0), thdr_index) ? '1' : '0';
        buf[1] = 0;
        break;
    case TA_BYTE:
        snprintf(buf, TA_NUMERIC_SPACE, "%d",
                 *THDRELEMPTR(thdr, unsigned char, thdr_index));
        break;
    case TA_INT:
        snprintf(buf, TA_NUMERIC_SPACE, "%d",
                 *THDRELEMPTR(thdr, int, thdr_index));
        break;
    case TA_UINT:
        snprintf(buf, TA_NUMERIC_SPACE, "%u",
                 *THDRELEMPTR(thdr, unsigned int, thdr_index));
        break;
    case TA_WIDE:
        snprintf(buf, TA_NUMERIC_SPACE, "%" TCL_LL_MODIFIER "d",
                 *THDRELEMPTR(thdr, Tcl_WideInt, thdr_index));
        break;
    case TA_DOUBLE:
        Tcl_PrintDouble(NULL, *THDRELEMPTR(thdr, double, thdr_index), buf);
        break;
    case TA_ANY:
        return Tcl_GetString(*THDRELEMPTR(thdr, Tcl_Obj *, thdr_index));
    case TA_STRING: 
        return (*THDRELEMPTR(thdr, tas_t *, thdr_index))->s;
    default:
        ta_type_panic(thdr->type);
        return 0;           /* To keep compiler happy */
    }
    return buf;
}

/*
 * Returns a double corresponding to a numeric thdr element
 * Will panic on non-numeric
 */
double thdr_index_double(thdr_t *thdr, Tcl_Size thdr_index)
{
    TA_ASSERT(thdr->used > thdr_index);
    switch (thdr->type) {
    case TA_BOOLEAN:
        return ba_get(THDRELEMPTR(thdr, ba_t, 0), thdr_index);
    case TA_BYTE:
        return *THDRELEMPTR(thdr, unsigned char, thdr_index);
    case TA_INT:
        return *THDRELEMPTR(thdr, int, thdr_index);
    case TA_UINT:
        return *THDRELEMPTR(thdr, unsigned int, thdr_index);
    case TA_WIDE:
        return (double) *THDRELEMPTR(thdr, Tcl_WideInt, thdr_index);
    case TA_DOUBLE:
        return *THDRELEMPTR(thdr, double, thdr_index);
    default:
        ta_type_panic(thdr->type);
        return 0;           /* To keep compiler happy */
    }
}

Tcl_WideInt thdr_index_wide(thdr_t *thdr, Tcl_Size thdr_index)
{
    TA_ASSERT(thdr->used > thdr_index);
    switch (thdr->type) {
    case TA_BOOLEAN:
        return ba_get(THDRELEMPTR(thdr, ba_t, 0), thdr_index);
    case TA_BYTE:
        return *THDRELEMPTR(thdr, unsigned char, thdr_index);
    case TA_INT:
        return *THDRELEMPTR(thdr, int, thdr_index);
    case TA_UINT:
        return *THDRELEMPTR(thdr, unsigned int, thdr_index);
    case TA_WIDE:
        return *THDRELEMPTR(thdr, Tcl_WideInt, thdr_index);
    case TA_DOUBLE:
        return (Tcl_WideInt) *THDRELEMPTR(thdr, double, thdr_index);
    default:
        ta_type_panic(thdr->type);
        return 0;           /* To keep compiler happy */
    }
}

/*
 * Returns a double corresponding to a numeric thdr element
 * Will panic on non-numeric
 */

struct thdr_minmax_mt_context {
    /* min_value, max_value -
     * Contains the actual min value. Note that in the case of TA_STRING
     * this should NOT be cleared on exit as we do not increment the
     * ptas' ref count due to multithreading considerations. It
     * is safe to do this since the value cannot be changed or otherwise
     * accessed in the meanwhile
     */
    ta_value_t min_value;
    ta_value_t max_value;

    /* Index of the min value and max value */
    Tcl_Size min_index;
    Tcl_Size max_index;

    /* Where the thread should begin its checks and count of elements to check*/
    void *base;
    Tcl_Size   nelems;

    unsigned char type;
    char ignore_case;
};


static void thdr_minmax_mt_worker (void *pv)
{
    struct thdr_minmax_mt_context *pctx = pv;

    TA_ASSERT(pctx->nelems > 0);

#define MINMAXLOOP(pctx_, type_, minptr_, maxptr_)                      \
    do {                                                                \
        type_ *p, *pend;                                                \
        type_ minval, maxval;                                           \
        type_ *pmin, *pmax;                                             \
        p      = (type_ *)(pctx_)->base;                                \
        pend   = p + (pctx_)->nelems;                                   \
        minval = maxval = *p;                                           \
        pmin = pmax = p;                                                \
        while (p < pend) {                                              \
            TA_ASSERT(maxval >= minval);                                \
            if (*p < minval) {                                          \
                minval = *p;                                            \
                pmin   = p;                                             \
            }                                                           \
            else if (*p > maxval) {                                     \
                maxval = *p;                                            \
                pmax   = p;                                             \
            }                                                           \
            ++p;                                                        \
        }                                                               \
        (pctx_)->min_index = (Tcl_Size)(pmin - (type_ *)(pctx_)->base); \
        *(minptr_)         = minval;                                    \
        (pctx_)->max_index = (Tcl_Size)(pmax - (type_ *)(pctx_)->base); \
        *(maxptr_)         = maxval;                                    \
    } while (0)

    switch (pctx->type) {
    case TA_UINT:
        MINMAXLOOP(pctx, unsigned int, &pctx->min_value.uival, &pctx->max_value.uival);
        break;
    case TA_INT:
        MINMAXLOOP(pctx, int, &pctx->min_value.ival, &pctx->max_value.ival);
        break;
    case TA_WIDE:
        MINMAXLOOP(pctx, Tcl_WideInt, &pctx->min_value.wval, &pctx->max_value.wval);
        break;
    case TA_DOUBLE:
        MINMAXLOOP(pctx, double, &pctx->min_value.dval, &pctx->max_value.dval);
        break;
    case TA_BYTE:
        MINMAXLOOP(pctx, unsigned char, &pctx->min_value.ucval, &pctx->max_value.ucval);
        break;
    case TA_ANY:
        {
            Tcl_Obj **p, **pend;
            Tcl_Obj *minval, *maxval;
            Tcl_Obj **pmin, **pmax;
            int ignore_case = pctx->ignore_case;
            TA_ASSERT(pctx->nelems > 0);
            p = (Tcl_Obj **) pctx->base;
            pend = p + pctx->nelems;
            minval = maxval = *p;
            pmin = pmax = p;
            ++p;
            while (p < pend) {
                if (ta_obj_compare(*p, minval, ignore_case) < 0) {
                    minval = *p;
                    pmin = p;
                } else if (ta_obj_compare(*p, maxval, ignore_case) > 0) {
                    maxval = *p;
                    pmax = p;
                }
                ++p;
            }
            pctx->min_index      = (Tcl_Size)(pmin - (Tcl_Obj **)pctx->base);
            pctx->min_value.oval = minval;
            pctx->max_index      = (Tcl_Size)(pmax - (Tcl_Obj **)pctx->base);
            pctx->max_value.oval = maxval;
        }
        break;
    case TA_STRING:
        {
            tas_t **p, **pend;
            tas_t *minval, *maxval;
            tas_t **pmin, **pmax;
            int ignore_case = pctx->ignore_case;
            TA_ASSERT(pctx->nelems > 0);
            p = (tas_t **) pctx->base;
            pend = p + pctx->nelems;
            minval = maxval = *p;
            pmin = pmax = p;
            ++p;
            while (p < pend) {
                if (tas_compare(*p, minval, ignore_case) < 0) {
                    minval = *p;
                    pmin = p;
                } else if (tas_compare(*p, maxval, ignore_case) > 0) {
                    maxval = *p;
                    pmax = p;
                }
                ++p;
            }
            pctx->min_index = (Tcl_Size)(pmin - (tas_t **)pctx->base);
            pctx->max_index = (Tcl_Size)(pmax - (tas_t **)pctx->base);
            /* NOTE: multithreaded so do NOT update ref counts
               (through tas_ref). Instead, copy pointers as is */
            pctx->min_value.ptas = minval;
            pctx->max_value.ptas = maxval;
        }
        break;
    default:
        /* Not handled here */
        ta_type_panic(pctx->type);
        break;
    }
    pctx->min_value.type = pctx->type;
    pctx->max_value.type = pctx->type;
}


/* Returns the minimum and maximum */
static void
thdr_minmax(thdr_t *thdr,
            Tcl_Size start,
            Tcl_Size count,
            int ignore_case,
            Tcl_Size *min_indexP,
            Tcl_Size *max_indexP,
            Tcl_Obj **ppomin,
            Tcl_Obj **ppomax)
{
    struct thdr_minmax_mt_context mt_context[2];
    int elem_size;
    int min_grp, max_grp;

    TA_ASSERT(start >= 0 && start < thdr->used);
    TA_ASSERT(count > 0);
    TA_ASSERT((start + count) <= thdr->used);

    if (thdr->type != TA_ANY && thdr->type != TA_STRING)
        ignore_case = 0;        /* Allows us to combine some sorted checks */

    if ((SORT_ORDER_IS_NOCASE(thdr->sort_order) && ignore_case) ||
        (SORT_ORDER_IS_CASE(thdr->sort_order) && !ignore_case)) {
        ta_value_t mintav, maxtav;
        if (SORT_ORDER_IS_ASCENDING(thdr->sort_order)) {
            *min_indexP = start;
            thdr_index_ta_value(thdr, start, &mintav);
            *max_indexP = start + count - 1;
            thdr_index_ta_value(thdr, start+count-1, &maxtav);
        } else {
            TA_ASSERT(SORT_ORDER_IS_DESCENDING(thdr->sort_order));
            *max_indexP = start;
            thdr_index_ta_value(thdr, start, &maxtav);
            *min_indexP = start + count - 1;
            thdr_index_ta_value(thdr, start+count-1, &mintav);
        }
        if (ppomin)
            *ppomin = ta_value_to_obj(&mintav);
        if (ppomax)
            *ppomax = ta_value_to_obj(&maxtav);
        ta_value_clear(&mintav);
        ta_value_clear(&maxtav);
        return;
    }

    if (thdr->type == TA_BOOLEAN) {
        Tcl_Size min_index, max_index;
        ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
        min_index = ba_find(baP, 0, start, start+count);
        max_index = ba_find(baP, 1, start, start+count);
        if (min_index < 0)
            min_index = start;   /* No 0's so first element (1) is the min */
        if (max_index < 0)
            max_index = start;   /* No 1's so first element (0) is the min */
        *min_indexP = min_index;
        if (ppomin)
            *ppomin = Tcl_NewBooleanObj(ba_get(baP, min_index));
        *max_indexP = max_index;
        if (ppomax)
            *ppomax = Tcl_NewBooleanObj(ba_get(baP, max_index));
        return;
    }

    elem_size = thdr->elem_bits / CHAR_BIT;
    mt_context[0].type = thdr->type;
    mt_context[0].ignore_case = ignore_case;
    mt_context[0].base = (start*elem_size) + THDRELEMPTR(thdr, unsigned char, 0);

#ifdef TA_MT_ENABLE
    mt_context[0].nelems = thdr_calc_mt_split(thdr->type, start, count, &mt_context[1].nelems);
    TA_ASSERT((mt_context[0].nelems + mt_context[1].nelems) == count);

    if (count < ta_minmax_mt_threshold || mt_context[1].nelems == 0) {
        mt_context[0].nelems = count;
        thdr_minmax_mt_worker(&mt_context[0]);
        min_grp = 0;
        max_grp = 0;
    } else {
        /* Multiple threads */
        ta_mt_group_t grp;

        /* TBD - actually only need to ensure strings over range of interest,
           not entire thdr */
        if (thdr->type == TA_ANY)
            thdr_ensure_obj_strings(thdr, NULL); /* Prevent races if string rep does not exist */

        mt_context[1].type = thdr->type;
        mt_context[1].base = (elem_size*mt_context[0].nelems) + (char*)mt_context[0].base;
        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* TBD - check return code */ ta_mt_group_async_f(grp, &mt_context[1], thdr_minmax_mt_worker);
        thdr_minmax_mt_worker(&mt_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);
        /* We will need to see if which thread had smaller/greater value */
        min_grp = (ta_value_compare(&mt_context[0].min_value, &mt_context[1].min_value, ignore_case) > 0);
        max_grp = (ta_value_compare(&mt_context[0].max_value, &mt_context[1].max_value, ignore_case) < 0);
    }

#else /* not TA_MT_ENABLE */

    mt_context[0].nelems = count;
    thdr_minmax_mt_worker(&mt_context[0]);
    min_grp = 0;
    max_grp = 0;

#endif

    if (ppomin)
        *ppomin = ta_value_to_obj(&mt_context[min_grp].min_value);
    if (min_grp == 0)
        *min_indexP = start + mt_context[0].min_index;
    else
        *min_indexP = start + mt_context[0].nelems + mt_context[1].min_index;

    if (ppomax)
        *ppomax = ta_value_to_obj(&mt_context[max_grp].max_value);
    if (max_grp == 0)
        *max_indexP = start + mt_context[0].max_index;
    else
        *max_indexP = start + mt_context[0].nelems + mt_context[1].max_index;

    /* Do NOT call ta_value_clear on mt_context[].min_value/max_value as 
       ref counts on ptas fields (if applicable) would not have been
       updated in the worker thread (to avoid multithreading issues)
    */

    return;
}


/*
 * Converts the passed Tcl_Obj o to integer indexes. If a single index
 * stores it in *pindex and returns TA_INDEX_TYPE_INT. If multiple indices,
 * stores a thdr_t of type int containing the indices into *thdrP and
 * returns TA_INDEX_TYPE_THDR. The thdr_t's ref count is incremented
 * so caller should call thdr_decr_refs as appropriate.
 *
 * If pindex is NULL, always returns as TA_INDEX_TYPE_THDR.
 *
 * IMPORTANT:
 * This facility to return a single int or a index list should only
 * be used by commands where it does not matter whether {1} is treated
 * as a list or an int, for example the fill command, and the distinction
 * is just an efficiency issue. Commands should
 * not use this to pick whether a single index or a list was specified
 * if it impacts their semantics.
 */
int
ta_obj_to_indices(Tcl_Interp *ip,
                  Tcl_Obj *o,
                  int want_sorted,
                  Tcl_Size end,     /* Value to use for "end",
                                       ignored if pindex == NULL */
                  thdr_t **thdrP,   /* Cannot be NULL */
                  Tcl_Size *pindex) /* Can be NULL */
{
    thdr_t *thdr;
    Tcl_Obj **elems;
    Tcl_Size n;
    TCL_RESULT status;

    /*
     * For efficiencies sake, we need to avoid shimmering. So we first
     * check for specific types and default to a list otherwise.
     */
    if (table_affirm(o)) {
        return ta_indices_error(ip, NULL); // NULL because we don't want to generate a string rep for a table
    }

    if (! tcol_affirm(o)) {
        /* If pindex is NULL, caller never wants to be treated as a single index */
        if (pindex != NULL) {
            /* To prevent shimmering, first check known to be a list */
            if (o->typePtr != g_tcl_list_type_ptr) {
                status = ta_convert_index(
                    NULL, o, &n, end, TCL_SIZE_MIN, TCL_SIZE_MAX);
                if (status == TCL_OK) {
                    *pindex = n;
                    return TA_INDEX_TYPE_INT;
                }
            }
            /* else fall through to try as list */
        }

        if (Tcl_ListObjGetElements(NULL, o, &n, &elems) != TCL_OK) {
            ta_indices_error(ip, o);
            return TA_INDEX_TYPE_ERROR;
        }

        /* Even as a list, it may be the single element "end". Check for that */
        if (n == 1) {
            if (pindex != NULL) {
                if (ta_convert_index(
                        NULL, elems[0], &n, end, TCL_SIZE_MIN, TCL_SIZE_MAX)
                    == TCL_OK) {
                    *pindex = n;
                    return TA_INDEX_TYPE_INT;
                }
            }
        }

        /*
         * Now that we know it is a list, see if it is actually a tarray
         * in list disguise
         */
        tcol_convert(NULL, o);  /* Ignore error - handled below */
    }

    if (tcol_affirm(o)) {
        thdr = thdr_to_indexcolumn(ip, OBJTHDR(o), OBJTHDRSPAN(o));
        /* NOTE thdr is ref incremented and needs a decrref at some point! */

        if (thdr == NULL) {
            ta_indices_error(ip, o);
            return TA_INDEX_TYPE_ERROR;
        }
        if (want_sorted && thdr->sort_order == THDR_UNSORTED) {
            thdr_t *thdr2 = thdr_clone(ip, thdr, thdr->used, NULL);
            thdr_decr_refs(thdr);
            thdr = thdr2;
            if (thdr == NULL)
                return TA_INDEX_TYPE_ERROR;
            qsort(THDRINDEXELEMPTR(thdr, 0), thdr->used, sizeof(Tcl_Size), tclsizecmp);
            thdr->sort_order = THDR_SORTED_ASCENDING;
            thdr_incr_refs(thdr);
        }
        *thdrP = thdr;
        return TA_INDEX_TYPE_THDR;
    }

    /* Not a column */
    thdr = thdr_alloc_and_init(ip, TA_INDEX, n, elems, 0);
    if (thdr) {
        if (want_sorted) {
            qsort(THDRINDEXELEMPTR(thdr, 0), thdr->used, sizeof(Tcl_Size), tclsizecmp);
            thdr->sort_order = THDR_SORTED_ASCENDING;
        }
        thdr->nrefs++;
        *thdrP = thdr;
        return TA_INDEX_TYPE_THDR;
    } else
        return TA_INDEX_TYPE_ERROR;
}

/* 
 * Returns a newly allocated thdr_t (with ref count 0) containing the
 * values from the specified indices
 */
Tcl_Obj *tcol_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, int fmt)
{
    thdr_t *psrc;
    thdr_t *thdr;
    Tcl_Size count, index, bound, span_first;
    Tcl_Size *pindex, *end;
    Tcl_Obj *tcol;
    void *srcbase, *thdrbase;
    span_t *span;

    TA_ASSERT(pindices->type == TA_INDEX);
    count = pindices->used;

    if (tcol_convert(ip, osrc) != TCL_OK)
        return NULL;

    psrc = OBJTHDR(osrc);
    if (fmt == TA_FORMAT_TARRAY) {
        thdr = thdr_alloc(ip, psrc->type, count);
        if (thdr == NULL)
            return NULL;
        thdrbase = THDRELEMPTR(thdr, unsigned char, 0);
        tcol = tcol_new(thdr);
    } else {
        thdr = NULL;
        thdrbase = NULL;
        tcol = Tcl_NewListObj(fmt == TA_FORMAT_LIST ? count : 2*count, NULL);
    }
    if (count == 0)
        return tcol;           /* Empty index list so nothing to return */


#define tcol_get_COPY(type_, objfn_)                                    \
    do {                                                                \
        type_ *fromP = srcbase;                                         \
        type_ *tpobjs = thdrbase;                                       \
        switch (fmt) {                                                  \
        case TA_FORMAT_TARRAY:                                          \
            while (pindex < end) {                                      \
                index = *pindex++;                                      \
                if (index < 0 || index >= bound)                        \
                    goto index_error;                                   \
                *tpobjs++ = fromP[index];                  \
            }                                                           \
            thdr->used = count;                                         \
            break;                                                      \
        case TA_FORMAT_LIST:                                            \
            while (pindex < end) {                                      \
                index = *pindex++;                                      \
                if (index < 0 || index >= bound)                        \
                    goto index_error;                                   \
                Tcl_ListObjAppendElement(ip, tcol,                      \
                                         objfn_(fromP[index]));         \
            }                                                           \
            break;                                                      \
        case TA_FORMAT_DICT:                                            \
            while (pindex < end) {                                      \
                index = *pindex++;                                      \
                if (index < 0 || index >= bound)                        \
                    goto index_error;                                   \
                Tcl_ListObjAppendElement(ip, tcol, Tcl_NewWideIntObj(index)); \
                Tcl_ListObjAppendElement(ip, tcol,                      \
                                         objfn_(fromP[index]));         \
            }                                                           \
            break;                                                      \
        }                                                               \
    } while (0)

    pindex = THDRINDEXELEMPTR(pindices, 0);
    end = pindex + count;
    span = OBJTHDRSPAN(osrc);
    if (span) {
        span_first = span->first;
        bound = span->count;
    } else {
        span_first = 0;
        bound = psrc->used;
    }
    switch (psrc->type) {
    case TA_BOOLEAN:
        {
            ba_t *srcbaP = THDRELEMPTR(psrc, ba_t, 0);
            ba_t *baP = thdrbase;
            Tcl_Size i;
            if (fmt == TA_FORMAT_TARRAY) {
                for (i = 0; pindex < end; ++i, ++pindex) {
                    index = *pindex;
                    if (index < 0 || index >= bound)
                        goto index_error;
                    ba_put(baP, i, ba_get(srcbaP, span_first+index));
                }
                thdr->used = count;             \
            } else {
                for (i = 0; pindex < end; ++i, ++pindex) {
                    index = *pindex;
                    if (index < 0 || index >= bound)
                        goto index_error;
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewWideIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol,
                                             Tcl_NewIntObj(ba_get(srcbaP, span_first + index)));
                }
            }
        }
        break;
    case TA_UINT:
        srcbase = THDRELEMPTR(psrc, unsigned int, span_first);
        tcol_get_COPY(unsigned int, Tcl_NewWideIntObj);
        break;
    case TA_INT:
        srcbase = THDRELEMPTR(psrc, int, span_first);
        tcol_get_COPY(int, Tcl_NewIntObj);
        break;
    case TA_WIDE:
        srcbase = THDRELEMPTR(psrc, Tcl_WideInt, span_first);
        tcol_get_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
        break;
    case TA_DOUBLE:
        srcbase = THDRELEMPTR(psrc, double, span_first);
        tcol_get_COPY(double, Tcl_NewDoubleObj);
        break;
    case TA_BYTE:
        srcbase = THDRELEMPTR(psrc, unsigned char, span_first);
        tcol_get_COPY(unsigned char, Tcl_NewIntObj);
        break;
    case TA_ANY:
        /* Cannot use macro here because of ref counts etc. */
        {
            Tcl_Obj **srcobjs = THDRELEMPTR(psrc, Tcl_Obj *, span_first);
            Tcl_Obj **pobjs = thdrbase;
            while (pindex < end) {
                index = *pindex++; 
                if (index < 0 || index >= bound)
                    goto index_error;
                if (fmt == TA_FORMAT_TARRAY) {
                    *pobjs = srcobjs[index];
                    Tcl_IncrRefCount(*pobjs);
                    ++pobjs;
                    thdr->used++; /* Bump as we go along in case tcol has
                                      to be released on error */
                } else {
                    /* No need to bump ref counts as lists take care of it */
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewWideIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol, srcobjs[index]);
                }
            }
        }
        break;

    case TA_STRING:
        /* Cannot use macro here because of ref counts etc. */
        {
            tas_t **srctas = THDRELEMPTR(psrc, tas_t *, span_first);
            tas_t **ptas = thdrbase;
            while (pindex < end) {
                index = *pindex++; 
                if (index < 0 || index >= bound)
                    goto index_error;
                if (fmt == TA_FORMAT_TARRAY) {
                    *ptas++ = tas_ref(srctas[index]);
                    thdr->used++; /* Bump as we go along in case tcol has
                                      to be released on error */
                } else {
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewWideIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol, tas_to_obj(srctas[index]));
                }
            }
        }
        break;

    default:
        ta_type_panic(psrc->type);
    }

    return tcol;

index_error:   /* index should hold the current index in error */
    ta_index_range_error(ip, index);

    if (tcol)
        Tcl_DecrRefCount(tcol);
    return NULL;

}

/* Trims the leading or end elements of a column. See asserts for conditions */
TCL_RESULT
tcol_trim_end(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Size low, Tcl_Size count)
{
    thdr_t *thdr;
    Tcl_Size current_count, offset;

    TA_ASSERT(! Tcl_IsShared(tcol));
    TA_ASSERT(low >= 0);
    TA_ASSERT(tcol_affirm(tcol));
    current_count = tcol_occupancy(tcol);
    TA_ASSERT(low == 0 || (low+count) >= current_count);

    if (count == 0)
        return TCL_OK; /* Nothing to be done */

    if (low == 0 && (low+count) >= current_count) {
        /* Deleting all elements in the column. Return an empty column */
        thdr = thdr_alloc(ip, tcol_type(tcol), 0);
        if (thdr == NULL)
            return TCL_ERROR;
        tcol_replace_intrep(tcol, thdr, NULL);
        return TCL_OK;
    }

    if ((low+count) > current_count)
        count = current_count - low;

    thdr = OBJTHDR(tcol);

    if (low == 0) {
        /* Trimming from front */
        offset = count;
    } else {
        /* Trimming from back */
        offset = 0;
    }

    tcol_replace_intrep(tcol, thdr, span_alloc(OBJTHDRFIRST(tcol)+offset, current_count - count));
    TA_ASSERT(tcol_check);
    return TCL_OK;
}

/* See asserts for conditions */
TCL_RESULT tcol_delete(Tcl_Interp *ip, Tcl_Obj *tcol,
                        Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    Tcl_Size low, count, current_count;
    int status;
    thdr_t *thdr;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    current_count = tcol_occupancy(tcol);

    if (indexb) {
        /* Delete a range. */
        status = ta_fix_range_bounds(ip, current_count, indexa,
                                     indexb, &low, &count);
        if (status != TCL_OK)
            return status;
        TA_ASSERT(low >= 0);

        /* If the column is already empty or range to be deleted is empty
           return column as is */
        if (current_count == 0 || count == 0)
            return TCL_OK;

        /* If we are deleting from the front or the back we can just
           adjust the span */
        if (low == 0 || (low+count) >= current_count) {
            return tcol_trim_end(ip, tcol, low, count);
        }
        status = tcol_make_modifiable(ip, tcol, current_count, 0);
        if (status != TCL_OK)
            return status;
        thdr = tcol_thdr(tcol);
        thdr_delete_range(thdr, low, count);
        return TCL_OK;
    } else {
        /* Not a range, either a list or single index */
        thdr_t *pindices;
        int index_type;

        index_type = ta_obj_to_indices(ip, indexa, 1, current_count-1, &pindices, &low);
        if (index_type == TA_INDEX_TYPE_ERROR)
            return TCL_ERROR;
        if (current_count == 0)
            return TCL_OK;
        if (index_type == TA_INDEX_TYPE_INT &&
            (low == 0 || low == (current_count-1))) {
            return tcol_trim_end(ip, tcol, low, 1);
        }
        
        status = tcol_make_modifiable(ip, tcol, current_count, 0);
        if (status != TCL_OK)
            return status;
        thdr = tcol_thdr(tcol);
        if (index_type == TA_INDEX_TYPE_INT) {
            if (low >= 0) 
                thdr_delete_range(thdr, low, 1);
        } else {
            /* TA_INDEX_TYPE_THDR */
            thdr_delete_indices(thdr, pindices);
            thdr_decr_refs(pindices);
        }
        return TCL_OK;
    }
}

TCL_RESULT tcol_insert_elem(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                            Tcl_Obj *opos, Tcl_Size count) 
{
    int status;
    Tcl_Size pos, used;
    TA_ASSERT(! Tcl_IsShared(tcol));
    if ((status = tcol_convert(ip, tcol)) == TCL_OK) {
        used = tcol_occupancy(tcol);
        if (count > 0) {
            if ((status = tcol_make_modifiable(ip, tcol, count+used, 0)) == TCL_OK &&
                (status = ta_convert_index(ip, opos, &pos, used,
                                           0, used)) == TCL_OK) {
                ta_value_t tav;
                if ((status = ta_value_from_obj(ip, ovalue,
                                                tcol_type(tcol), &tav)) == TCL_OK) {
                    thdr_fill_range(ip, tcol_thdr(tcol), &tav, pos, count, 1);
                    ta_value_clear(&tav);
                }
            }
        } else if (count < 0) {
            status = ta_bad_count_error(ip, count);
        } else {
            status = TCL_OK; /* count == 0, nothing to do */
        }
    }
    return status;
}

TCL_RESULT tcol_inject_elems(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                             Tcl_Obj *opos)
{
    TA_ASSERT(! Tcl_IsShared(tcol));

    /* Values may be given as a column or a list */
    if (tcol_convert(NULL, ovalue) == TCL_OK)
        return tcol_copy_thdr(ip, tcol, OBJTHDR(ovalue), OBJTHDRSPAN(ovalue), opos, 1);
    else
        return tcol_put_objs(ip, tcol, ovalue, opos, 1);
}

TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                         Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    Tcl_Size low, count, nelems;
    int status;
    ta_value_t tav;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if ((status = ta_value_from_obj(ip, ovalue,
                                    tcol_type(tcol), &tav)) != TCL_OK)
        return status;

    nelems = tcol_occupancy(tcol);
    if (indexb) {
        status = ta_fix_range_bounds(ip, nelems, indexa, indexb, &low, &count);
        if (status == TCL_OK && count != 0) {
            status = tcol_make_modifiable(ip, tcol, low+count, 0);
            if (status == TCL_OK)
                thdr_fill_range(ip, tcol_thdr(tcol), &tav, low, count, 0);
        }
    } else {
        /* Not a range, either a list or single index */
        thdr_t *pindices;
        /* Note status is TCL_OK at this point */
        switch (ta_obj_to_indices(ip, indexa, 1, nelems-1, &pindices, &low)) {
        case TA_INDEX_TYPE_ERROR:
            status = TCL_ERROR;
            break;
        case TA_INDEX_TYPE_INT:
            if (low < 0 || low > nelems) {
                ta_index_range_error(ip, low);
                status = TCL_ERROR;
            } else {
                status = tcol_make_modifiable(ip, tcol, low+1, 0);
                if (status == TCL_OK)
                    thdr_fill_range(ip, tcol_thdr(tcol), &tav, low, 1, 0);
            }
            break;
        case TA_INDEX_TYPE_THDR:
            status = thdr_verify_indices_in_range(ip, tcol_occupancy(tcol), pindices, &count);
            if (status == TCL_OK && count > 0) {
                status = tcol_make_modifiable(ip, tcol, count, count); // TBD - count + extra?
                if (status == TCL_OK)
                    thdr_fill_indices(ip, tcol_thdr(tcol), &tav, pindices, count);
            }
            thdr_decr_refs(pindices);
            break;
        }
    }
    ta_value_clear(&tav);
    return status;
}

TCL_RESULT tcol_reverse(Tcl_Interp *ip, Tcl_Obj *tcol)
{
    thdr_t *thdr;
    span_t *span;
    int status;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    thdr = OBJTHDR(tcol);
    span = OBJTHDRSPAN(tcol);

    if (span || thdr_shared(thdr)) {
        thdr = thdr_clone_reversed(ip, thdr, 0, span);
        if (thdr == NULL)
            return TCL_ERROR;
        tcol_replace_intrep(tcol, thdr, NULL);
    } else {
        thdr_reverse(thdr);
        Tcl_InvalidateStringRep(tcol);
    }
    return TCL_OK;
}

int ta_utf8_compare(char *a, char *b, int ignorecase)
{
    int comparison;

    /* TBD - check first letter ? But be careful of case-insensitivity*/
#if 0
    // This is much slower than the strcmp method below but might be more
    // "correct". On the other hand the strcmp method below is both faster
    // and Tcl compatible with lsort and lsearch
    int alen, blen, len;

    alen = strlen(a);
    blen = strlen(b);

    alen = Tcl_NumUtfChars(a, alen); /* Num bytes -> num chars */
    blen = Tcl_NumUtfChars(b, blen); /* Num bytes -> num chars */

    len = alen < blen ? alen : blen; /* len is the shorter length */
    comparison = (ignorecase ? Tcl_UtfNcasecmp : Tcl_UtfNcmp)(a, b, len);
    if (comparison == 0) {
        comparison = alen-blen;
    }

#else

    /* Following Tcl's lsort we just use the Unicode code point collation
       order which means a simple strcmp but this is incorrect for -nocase */
    /* TBD - Tcl has fixed this post-8.6. Check and change this accordingly */
    if (ignorecase) {
        comparison = _stricmp(a, b);
    } else {
        if (*a == *b)
            comparison = strcmp(a, b);
        else
            comparison = *(unsigned char *)a - *(unsigned char *)b;
    }
#endif

    return (comparison > 0) ? 1 : ((comparison < 0) ? -1 : 0);
}

int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;

    a = oaP->bytes;
    if (a == NULL)
        a = Tcl_GetString(oaP);
    b = obP->bytes;
    if (b == NULL)
        b = Tcl_GetString(obP);

    return ta_utf8_compare(a, b, ignorecase);
}


int ta_utf8_equal(char *a, char *b, int ignorecase)
{
    /* TBD - check first letter ? But be careful of case-insensitivity*/
#if 0
    // This is much slower than the strcmp method below but might be more
    // "correct". On the other hand the strcmp method below is both faster
    // and Tcl compatible with lsort and lsearch
    int alen, blen, len;

    alen = strlen(a);
    blen = strlen(b);

    if (alen != blen)
        return 0;

    // This is much slower than the strcmp method below but might be more
    // "correct". On the other hand the strcmp method below is both faster
    // and Tcl compatible with lsort and lsearch
    alen = Tcl_NumUtfChars(a, alen); /* Num bytes -> num chars */
    blen = Tcl_NumUtfChars(b, blen); /* Num bytes -> num chars */

    if (alen != blen)
        return 0; /* Note this is different from above check, alen/blen have changed*/

    return ! (ignorecase ? Tcl_UtfNcasecmp : Tcl_UtfNcmp)(a, b, alen);

#else

    /* Following Tcl's lsort we just use the Unicode code point collation
       order which means a simple strcmp but this is incorrect for -nocase */
    /* TBD - Tcl has fixed this post-8.6. Check and change this accordingly */

    return ignorecase ? ! _stricmp(a, b) : ! strcmp(a, b);
#endif
}

int ta_obj_equal(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;
    Tcl_Size alen, blen;

    /* TBD - optimize for when both are table or columns */
    a = Tcl_GetStringFromObj(oaP, &alen);
    b = Tcl_GetStringFromObj(obP, &blen);

    if (alen != blen)
        return 0;

    return ta_utf8_equal(a, b, ignorecase);
}

TCL_RESULT tcol_copy_thdr(Tcl_Interp *ip, Tcl_Obj *tcol,
                          thdr_t *psrc, span_t *src_span,
                          Tcl_Obj *ofirst, /* NULL -> end */
                          int insert)
{
    Tcl_Size first, cur_used, src_first, count;
    int status;
    
    TA_ASSERT(! Tcl_IsShared(tcol));
    
    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if (tcol_type(tcol) != psrc->type)
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    if (src_span) {
        src_first = src_span->first;
        count = src_span->count;
    } else {
        src_first = 0;
        count = psrc->used;
    }
    cur_used = tcol_occupancy(tcol);
    first = cur_used; /* By default, append */
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &first, first, 0, first);
    if (status == TCL_OK && count) {
        status = tcol_make_modifiable(ip, tcol,
                                      count + (insert ? cur_used : first),
                                      0);
        if (status == TCL_OK)
            thdr_copy(tcol_thdr(tcol), first, psrc, src_first, count, insert); 
    }
    return status;
}

TCL_RESULT tcol_put_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                         Tcl_Obj *ovalues,
                         Tcl_Obj *ofirst, /* NULL -> end */
                         int insert)
{
    int status;
    Tcl_Obj **values;
    Tcl_Size nvalues, n, cur_used;

    TA_ASSERT(! Tcl_IsShared(tcol));

    /*
     * Note on reference counting / shimmering (related to Bug 20)
     *  - tcol is expected to be unshared so no need to worry about that.
     *  - ovalues and ofirst might point to the same object. Thus
     *    we have to be careful to extract the integer ovalue first
     *    and then shimmer ovalues to a list. Otherwise, the list intrep
     *    for ovalues might be shimmered away when ofirst is shimmered
     *    to int.
     */
    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    cur_used = tcol_occupancy(tcol);
    n = cur_used;
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &n, n, 0, n);
    if (status != TCL_OK)
        return status;

    status = Tcl_ListObjGetElements(ip, ovalues, &nvalues, &values);
    if (status != TCL_OK)
        return status;

    /* n contains starting offset */
    if (status == TCL_OK && nvalues) {
        /* Note this also invalidates the string rep as desired */
        status = tcol_make_modifiable(ip, tcol,
                                      nvalues + (insert ? cur_used : n),
                                      0);
        if (status == TCL_OK) {
            /* Note even on error thdr_put_objs guarantees a consistent 
             * and unchanged tcol
             */
            status = thdr_put_objs(ip, tcol_thdr(tcol),
                                   n, nvalues, values, insert);
        }
    }

    return status;
}

TCL_RESULT tcol_place_objs(Tcl_Interp *ip, Tcl_Obj *tcol,
                           Tcl_Obj *ovaluelist,
                           Tcl_Obj *oindices)
{
    Tcl_Size new_size;
    Tcl_Size nvalues;
    int status;
    thdr_t *pindices;
    Tcl_Obj **ovalues;

    TA_ASSERT(! Tcl_IsShared(tcol));

    status = Tcl_ListObjGetElements(ip, ovaluelist, &nvalues, &ovalues);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL)
        != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        if (pindices->used > nvalues)
            status = ta_indices_count_error(ip, pindices->used, nvalues);
        else {
            status = ta_verify_value_objs(ip, tcol_type(tcol), nvalues, ovalues);
            if (status == TCL_OK) {
                status = thdr_verify_indices_in_range(ip, tcol_occupancy(tcol), pindices, &new_size);
                if (status == TCL_OK) {
                    status = tcol_make_modifiable(ip, tcol, new_size, new_size); // TBD - count + extra?
                    thdr_place_objs(ip, tcol_thdr(tcol), pindices,
                                    new_size,
                                    nvalues, ovalues);
                }
            }
        }
    }

    thdr_decr_refs(pindices);
    return status;
}

TCL_RESULT tcol_place_indices(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *osrc,
                              Tcl_Obj *oindices)
{
    Tcl_Size new_size;
    int status;
    thdr_t *pindices, *psrc;
    span_t *src_span;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    
    TA_ASSERT(tcol_affirm(osrc));
    psrc = OBJTHDR(osrc);
    src_span = OBJTHDRSPAN(osrc);
    if (psrc->type != tcol_type(tcol))
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        if (pindices->used > tcol_occupancy(osrc))
            status = ta_indices_count_error(ip, pindices->used, psrc->used);
        else {
            status = thdr_verify_indices_in_range(ip, tcol_occupancy(tcol), pindices, &new_size);
            if (status == TCL_OK) {
                status = tcol_make_modifiable(ip, tcol, new_size, new_size); // TBD - count + extra?
                thdr_place_indices(ip, tcol_thdr(tcol), psrc, src_span, pindices, new_size);
            }
        }
    }
    thdr_decr_refs(pindices);

    return status;
}

TCL_RESULT tcol_retrieve(Tcl_Interp *ip, int objc, Tcl_Obj * const *objv,
                         int command)
{
    Tcl_Obj *tcol;
    int      status, opt, minargs;
    int      fmt = TA_FORMAT_TARRAY;
    /* Note order of options matches switch below */
    static const char *tcol_get_options[] = {
        "-column", "-list", "-dict", NULL
    };

    minargs = command == TA_RETRIEVE_GET ? 2 : 3;

    if (objc < 1+minargs || objc > 2+minargs) {
        Tcl_WrongNumArgs(ip, 1, objv, command == TA_RETRIEVE_GET ? "?OPTIONS? COLUMN INDEXLIST" : "?OPTIONS? COLUMN LOW HIGH");
        return TCL_ERROR;
    }

    tcol = objv[objc-minargs];
    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (objc == 2+minargs) {
        /* An option is specified */
        if ((status = ta_opt_from_obj(ip, objv[1], tcol_get_options,
                                          "option", TCL_EXACT, &opt)) != TCL_OK)
            return TCL_ERROR;
        switch (opt) {
        case 0: fmt = TA_FORMAT_TARRAY; break;
        case 1: fmt = TA_FORMAT_LIST; break;
        case 2: fmt = TA_FORMAT_DICT; break;
        }
    }
       
    if (command == TA_RETRIEVE_GET) {
        thdr_t  *pindices;

        if (ta_obj_to_indices(ip, objv[objc-1], 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
            return TCL_ERROR;

        tcol = tcol_get(ip, tcol, pindices, fmt);
        thdr_decr_refs(pindices);
    } else {
        /* Range LOW HIGH */
        Tcl_Size low, count;
        status = ta_fix_range_bounds(ip, tcol_occupancy(tcol),
                                     objv[objc-2], objv[objc-1],
                                     &low, &count);
        if (status != TCL_OK)
            return status;
        tcol = tcol_range(ip, tcol, low, count, fmt);
    }

    if (tcol) {
        TA_ASSERT(fmt != TA_FORMAT_TARRAY || tcol_check(ip, tcol));
        Tcl_SetObjResult(ip, tcol);
        return TCL_OK;
    } else
        return TCL_ERROR;
}

TCL_RESULT tcol_minmax_cmd(ClientData clientdata, Tcl_Interp *ip,
                           int objc, Tcl_Obj *const objv[])
{
    int opt;
    Tcl_Size range_start, range_count;
    int ignore_case = 0, want_indices = 0;
    TCL_RESULT status;
    static const char *tcol_minmax_switches[] = {
        "-range", "-indices", "-nocase", NULL
    };
    enum tcol_minmax_switches_e {
        TA_MINMAX_OPT_RANGE, TA_MINMAX_OPT_INDICES, TA_MINMAX_OPT_NOCASE
    };
    Tcl_Size i, min_index, max_index, tcol_count;
    thdr_t *thdr;
    Tcl_Obj *objs[2];
    Tcl_Obj *tcol;
    span_t *span;

    if (objc < 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "?options? column");
        return TCL_ERROR;
    }

    tcol = objv[objc-1];
    if (tcol_convert(ip, tcol) != TCL_OK)
        return TCL_ERROR;
    thdr = OBJTHDR(tcol);
    span = OBJTHDRSPAN(tcol);
    range_start = 0;
    tcol_count = tcol_occupancy(tcol);
    if (tcol_count == 0) {
        Tcl_SetResult(ip, "Cannot determine min-max values for an empty column.", TCL_STATIC);
        return TCL_ERROR;
    }
    range_count = tcol_count;
    for (i = 1; i < objc-1; ++i) {
	status = ta_opt_from_obj(ip, objv[i], tcol_minmax_switches, "option", 0, &opt);
        if (status != TCL_OK)
            return status;

        switch ((enum tcol_minmax_switches_e) opt) {
        case TA_MINMAX_OPT_RANGE:
            if (i > objc-3)
                return ta_missing_arg_error(ip, "-range");
            ++i;
            status = ta_parse_range_option_value(ip, range_count, objv[i], &range_start, &range_count);
            if (status != TCL_OK)
                return status;
            break;
        case TA_MINMAX_OPT_INDICES:
            want_indices = 1;
            break;
        case TA_MINMAX_OPT_NOCASE:
            ignore_case = 1;
            break;
        }
    }

    if (range_count <= 0 ||
        range_start >= tcol_count ||
        (range_start+range_count) > tcol_count) {
        return ta_invalid_range_error(ip, NULL);
    }

    if (span)
        range_start += span->first;
    
    if (want_indices) {
        thdr_minmax(thdr, range_start, range_count, ignore_case,
                    &min_index, &max_index, NULL, NULL);
        if (span) {
            min_index -= span->first;
            max_index -= span->first;
        }
        TA_ASSERT(min_index >= 0 && min_index < tcol_count);
        TA_ASSERT(max_index >= 0 && max_index < tcol_count);
        objs[0] = Tcl_NewWideIntObj(min_index);
        objs[1] = Tcl_NewWideIntObj(max_index);
    } else {
        thdr_minmax(thdr, range_start, range_count, ignore_case,
                    &min_index, &max_index, &objs[0], &objs[1]);
    }

    Tcl_SetObjResult(ip, Tcl_NewListObj(2, objs));
    return TCL_OK;
}

TCL_RESULT tcol_equalintervals_cmd(ClientData clientdata, Tcl_Interp *ip,
                                int objc, Tcl_Obj *const objv[])
{
    thdr_t *thdr, *buckets, *lows_thdr;
    int opt;
    Tcl_Size nbuckets, first, count;
    ta_value_t minval, maxval, step, last;
    span_t *span;
    static const char *cmds[] = {
        "count", "sum", "values", "indices", NULL
    };
    enum flags_e {
        TA_COUNT_CMD, TA_SUM_CMD, TA_VALUES_CMD, TA_INDICES_CMD
    };
    Tcl_Obj *tcol;
    Tcl_Obj *objs[2];
    ta_uwide_t u64;

    if (objc != 7) {
	Tcl_WrongNumArgs(ip, 1, objv, "COL count|sum|values|indices NBUCKETS MIN MAX STEP");
	return TCL_ERROR;
    }

    buckets   = NULL;
    lows_thdr = NULL;

    /* How to bucketize */
    CHECK_OK( ta_opt_from_obj(ip, objv[2], cmds, "command", 0, &opt) );

    /* Number of buckets - this ensures nbuckets > 0 */
    CHECK_OK( ta_get_count_from_obj(ip, objv[3], 0, &nbuckets) );

    tcol = objv[1];
    CHECK_OK( tcol_convert(ip, tcol) );
    span = OBJTHDRSPAN(tcol);
    thdr = tcol_thdr(tcol);

    /*
     * Get the step and minval values and ensure the covered
     * range does not overflow the type range.
     * The lower limit of the highest bucket is
     * last = minval + (nbuckets-1)*step. We have to ensure that
     * this does not overflow. Note it is ok for 
     * the last *bucket* to overflow (in which case it will be
     * less than "step" units wide).
     */

    switch (thdr->type) {
    case TA_DOUBLE:
        CHECK_OK( ta_value_from_obj(ip, objv[4], TA_DOUBLE, &minval) );
        CHECK_OK( ta_value_from_obj(ip, objv[5], TA_DOUBLE, &maxval) );
        if (maxval.dval < minval.dval)
            goto invalid_limits;
        CHECK_OK( ta_value_from_obj(ip, objv[6], TA_DOUBLE, &step) );
        if (step.dval <= 0)
            goto nonpositive_step;
        TA_ASSERT(nbuckets > 0);
        last.dval = minval.dval + (nbuckets-1)*step.dval;
        if (TA_ISINFINITE(last.dval))
            goto invalid_bucket_interval;
        break;
    case TA_BYTE:
    case TA_INT:
    case TA_UINT:
    case TA_WIDE:
        CHECK_OK( ta_value_from_obj(ip, objv[4], TA_WIDE, &minval) );
        CHECK_OK( ta_value_from_obj(ip, objv[5], TA_WIDE, &maxval) );
        if (maxval.wval < minval.wval)
            goto invalid_limits;
#if TCL_MAJOR_VERSION < 9
        CHECK_OK( ta_value_from_obj(ip, objv[6], TA_WIDE, &step) );
        step.uwval = step.wval;
#else
        Tcl_WideUInt uw;
        Tcl_GetWideUIntFromObj(ip, objv[6], &uw);
        step.uwval = uw;
#endif
        if (step.uwval == 0 || step.uwval > ((uint64_t)INT64_MAX+1))
            goto nonpositive_step;

        /* All step calculations now unsigned. Note that the step
           can be greated than the signed max value */

        /* Calculate lower bound of last bucket */
        if (ovf_mul_uint64(step.uwval, (nbuckets-1), &u64))
            goto invalid_bucket_interval;
        /* u64 is diff between start of first bucket and last bucket */
        if (minval.wval >= 0) {
            if (ovf_add_uint64((uint64_t)minval.wval, u64, &u64))
                goto invalid_bucket_interval;
            /* Above was unsigned overflow check, also check signed */
            if (u64 > INT64_MAX)
                goto invalid_bucket_interval;
            last.wval = (int64_t) u64;
        } else {
            /* We don't have a signed/unsigned combination of ovf_ routines */
            uint64_t positive_min = (uint64_t) -minval.wval;
            if (positive_min <= u64) {
                u64 -= positive_min;
                if (u64 > INT64_MAX)
                    goto invalid_bucket_interval;
                last.wval = (int64_t) u64;
            } else  {
                /* Overflow not possible since INT64_MIN < minval < 0
                   and its magnitude is greater than u64 */
                last.wval = minval.wval + u64;
            }
        }

        switch (thdr->type) {
        case TA_BYTE:
            /* Check the first and last lower bounds */
            if (minval.wval < 0 || last.wval > UINT8_MAX)
                goto invalid_bucket_interval;
            minval.ucval = (unsigned char) minval.wval;
            last.ucval  = (unsigned char) last.wval;
            if (step.wval > UINT8_MAX) {
                if ((step.wval-1) == UINT8_MAX && nbuckets == 1) {
                    /* Deals with edge cast min=0, step=256 */
                    TA_ASSERT(last.wval == minval.wval);
                } else
                    goto invalid_bucket_interval;
            }
            step.ucval  = (unsigned char) step.wval;
            /* Already know maxval > 0 'cause maxval > minval */
            if (maxval.wval > UINT8_MAX)
                maxval.wval = UINT8_MAX;
            else
                maxval.ucval = (unsigned char) maxval.wval;
            break;
        case TA_INT:
            /* Check the first and last lower bounds */
            if (minval.wval < INT32_MIN ||
                (last.wval > INT32_MAX && nbuckets != 1))
                goto invalid_bucket_interval;
            minval.ival = (int) minval.wval;
            last.ival  = (int) last.wval;
            /* NOTE: check against UINT32_MAX, not INT32_MAX as
               step needs to be unsigned to cover full signed range */
            if (step.wval > UINT32_MAX) {
                if ((step.wval-1) == UINT32_MAX && nbuckets == 1) {
                    /* Deals with edge cast min=0, step=0xffffffff */
                    TA_ASSERT(last.wval == minval.wval);
                } else 
                    goto invalid_bucket_interval;
            }
            step.uival  = (unsigned int) step.wval; /* NOTE - use uival, not ival */
            /* Already know maxval > INT32_MIN 'cause maxval > minval */
            if (maxval.wval > INT32_MAX)
                maxval.ival = INT32_MAX;
            else
                maxval.ival = (int) maxval.wval;
            break;
        case TA_UINT:
            if (minval.wval < 0 || last.wval > UINT32_MAX)
                goto invalid_bucket_interval;
            minval.uival = (unsigned int) minval.wval;
            last.uival  = (unsigned int) last.wval;
            if (step.wval > UINT32_MAX) {
                if ((step.wval-1) == UINT32_MAX && nbuckets == 1) {
                    /* Deals with edge cast min=0, step=0xffffffff */
                    TA_ASSERT(last.wval == minval.wval);
                } else 
                    goto invalid_bucket_interval;
            }
            step.uival  = (unsigned int) step.wval;
            if (maxval.wval > UINT32_MAX) /* Already know maxval > 0 'cause maxval > minval */
                maxval.uival = UINT32_MAX;
            else
                maxval.uival = (unsigned int) maxval.wval;
            break;
        case TA_WIDE:
            /* No further range checks for wides. */
            break;
        }
        break;
    case TA_BOOLEAN:
    case TA_ANY:
    case TA_STRING:
    default:
        return ta_invalid_op_for_type(ip, thdr->type);

    }

    first = thdr_start_and_count(thdr, span, &count);
    
    /* lows_thdr holds lower limits of intervals */
    lows_thdr = thdr_alloc(ip, thdr->type, nbuckets);
    if (lows_thdr == NULL)
        goto error_return;

#define FILL_LOWS(type_, field_)                        \
    do {                                                \
        type_ *plows;                                   \
        Tcl_Size i;                                     \
        plows = THDRELEMPTR(lows_thdr, type_, 0);       \
        for (i = 0; i < nbuckets; ++i)                  \
            plows[i] = (type_) (minval.field_ + i * step.field_); \
        lows_thdr->used       = nbuckets;               \
        lows_thdr->sort_order = THDR_SORTED_ASCENDING;  \
    } while (0)

/*
 * NOTE ABOUT bucket_index ASSERTIONS
 * Computation of bucket width is necessarily inexact for doubles.
 * and thus it is possible that values may lie beyond the
 * last bucket. We deal with this by clamping down these
 * values to the last bucket.
 */
#define OUTSIDE_LIMITS(i_, field_) (pdata[i_] < minval.field_ || pdata[i_] > maxval.field_)

#define BUCKET_INDEX(val_, field_, unsigned_type_, step_field_)      \
    ((Tcl_Size)(((val_) > 0 && minval.field_ < 0)                    \
                    ? ((((unsigned_type_)(val_)) + (-minval.field_)) \
                       / step.step_field_)                           \
                    : (((val_) - minval.field_) / step.step_field_)))

#define FILL_COUNTS(type_, field_, unsigned_type_, step_field_)         \
    do {                                                                \
        Tcl_Size i, bucket_index;                                       \
        type_ *pdata;                                                   \
        Tcl_Size *pbucket;                                              \
        buckets = thdr_alloc(ip, TA_INDEX, nbuckets);                   \
        if (buckets == NULL)                                            \
            goto error_return;                                          \
        buckets->used = nbuckets;                                       \
        thdr_clear(buckets);                                            \
        pbucket = THDRINDEXELEMPTR(buckets, 0);                         \
        pdata   = THDRELEMPTR(thdr, type_, first);                      \
        if (nbuckets == 1) {                                            \
            /* Special case. Note step here NOT accurate so loop */     \
            /* below will not work */                                   \
            for (i = 0; i < count; ++i) {                               \
                if (OUTSIDE_LIMITS(i, field_))                          \
                    continue;                                           \
                pbucket[0] += 1;                                        \
            }                                                           \
        }                                                               \
        else {                                                          \
            for (i = 0; i < count; ++i) {                               \
                if (OUTSIDE_LIMITS(i, field_))                          \
                    continue;                                           \
                if (pdata[i] > last.field_)                             \
                    bucket_index = nbuckets - 1;                        \
                else                                                    \
                    bucket_index = BUCKET_INDEX(                        \
                        pdata[i], field_, unsigned_type_, step_field_); \
                if (bucket_index < 0 && bucket_index >= nbuckets)       \
                    goto bucket_error;                                  \
                pbucket[bucket_index] += 1;                             \
            }                                                           \
        }                                                               \
    } while (0)

#define FILL_SUMS(                                                        \
    type_, field_, sum_type_, unsigned_type_, step_field_, addfn_)        \
    do {                                                                  \
        Tcl_Size i, bucket_index;                                         \
        type_ *pdata;                                                     \
        sum_type_ *pbucket;                                               \
        buckets = thdr_alloc(                                             \
            ip, thdr->type == TA_DOUBLE ? TA_DOUBLE : TA_WIDE, nbuckets); \
        if (buckets == NULL)                                              \
            goto error_return;                                            \
        buckets->used = nbuckets;                                         \
        thdr_clear(buckets);                                              \
        pbucket = THDRELEMPTR(buckets, sum_type_, 0);                     \
        pdata   = THDRELEMPTR(thdr, type_, first);                        \
        if (nbuckets == 1) {                                              \
            /* Special case. Note step here NOT accurate so loop */       \
            /* below will not work */                                     \
            for (i = 0; i < count; ++i) {                                 \
                if (OUTSIDE_LIMITS(i, field_))                            \
                    continue;                                             \
                if (addfn_(pbucket[0], pdata[i], &pbucket[0]))            \
                    goto computation_overflow;                            \
            }                                                             \
        }                                                                 \
        else {                                                            \
            for (i = 0; i < count; ++i) {                                 \
                if (OUTSIDE_LIMITS(i, field_))                            \
                    continue;                                             \
                if (pdata[i] > last.field_)                               \
                    bucket_index = nbuckets - 1;                          \
                else                                                      \
                    bucket_index = BUCKET_INDEX(                          \
                        pdata[i], field_, unsigned_type_, step_field_);   \
                if (bucket_index < 0 && bucket_index >= nbuckets)         \
                    goto bucket_error;                                    \
                /* TBD - overflow checks? */                              \
                if (addfn_(pbucket[bucket_index], pdata[i], &pbucket[bucket_index]))            \
                    goto computation_overflow;                            \
            }                                                             \
        }                                                                 \
    } while (0)

#define FILL_INDICES(type_, field_, unsigned_type_, step_field_)           \
    do {                                                                   \
        Tcl_Size i, bucket_index, initial_size;                            \
        type_ *pdata;                                                      \
        Tcl_Obj **pbucket;                                                 \
        buckets = thdr_alloc(ip, TA_ANY, nbuckets);                        \
        if (buckets == NULL)                                               \
            goto error_return;                                             \
        pbucket = THDRELEMPTR(buckets, Tcl_Obj *, 0);                      \
        for (i = 0, initial_size = count / nbuckets; i < nbuckets; ++i) {  \
            pbucket[i] = tcol_new(thdr_alloc(ip, TA_INDEX, initial_size)); \
            if (pbucket[i] == NULL)                                        \
                goto error_return;                                         \
            Tcl_IncrRefCount(pbucket[i]);                                  \
            buckets->used += 1;                                            \
        }                                                                  \
        pdata = THDRELEMPTR(thdr, type_, first);                           \
        for (i = 0; i < count; ++i) {                                      \
            thdr_t *inner_thdr;                                            \
            if (OUTSIDE_LIMITS(i, field_))                                 \
                continue;                                                  \
            if (nbuckets == 1)                                             \
                bucket_index = 0; /* step possible invalid */              \
            else if (pdata[i] > last.field_)                               \
                bucket_index = nbuckets - 1;                               \
            else                                                           \
                bucket_index = BUCKET_INDEX(                               \
                    pdata[i], field_, unsigned_type_, step_field_);        \
            if (bucket_index < 0 && bucket_index >= nbuckets)              \
                goto bucket_error;                                         \
            inner_thdr = OBJTHDR(pbucket[bucket_index]);                   \
            if (tcol_make_modifiable(                                      \
                    ip, pbucket[bucket_index], 1 + inner_thdr->used, 0)    \
                != TCL_OK)                                                 \
                goto error_return;                                         \
            inner_thdr = OBJTHDR(                                          \
                pbucket[bucket_index]); /* Reload, might have changed */   \
            *THDRINDEXELEMPTR(inner_thdr, inner_thdr->used) = i;           \
            inner_thdr->used += 1;                                         \
        }                                                                  \
    } while (0)

#define FILL_VALUES(type_, field_, unsigned_type_, step_field_)              \
    do {                                                                     \
        Tcl_Size i, bucket_index, initial_size;                              \
        type_ *pdata;                                                        \
        Tcl_Obj **pbucket;                                                   \
        buckets = thdr_alloc(ip, TA_ANY, nbuckets);                          \
        if (buckets == NULL)                                                 \
            goto error_return;                                               \
        pbucket = THDRELEMPTR(buckets, Tcl_Obj *, 0);                        \
        for (i = 0, initial_size = count / nbuckets; i < nbuckets; ++i) {    \
            pbucket[i] = tcol_new(thdr_alloc(ip, thdr->type, initial_size)); \
            if (pbucket[i] == NULL)                                          \
                goto error_return;                                           \
            Tcl_IncrRefCount(pbucket[i]);                                    \
            buckets->used += 1;                                              \
        }                                                                    \
        pdata = THDRELEMPTR(thdr, type_, first);                             \
        for (i = 0; i < count; ++i) {                                        \
            thdr_t *inner_thdr;                                              \
            if (OUTSIDE_LIMITS(i, field_))                                   \
                continue;                                                    \
            if (nbuckets == 1)                                               \
                bucket_index = 0; /* step possible invalid */                \
            else if (pdata[i] > last.field_)                                 \
                bucket_index = nbuckets - 1;                                 \
            else                                                             \
                bucket_index = BUCKET_INDEX(                                 \
                    pdata[i], field_, unsigned_type_, step_field_);          \
            if (bucket_index < 0 && bucket_index >= nbuckets)                \
                goto bucket_error;                                           \
            inner_thdr = OBJTHDR(pbucket[bucket_index]);                     \
            if (tcol_make_modifiable(                                        \
                    ip, pbucket[bucket_index], 1 + inner_thdr->used, 0)      \
                != TCL_OK)                                                   \
                goto error_return;                                           \
            inner_thdr = OBJTHDR(                                            \
                pbucket[bucket_index]); /* Reload, might have changed */     \
            *THDRELEMPTR(inner_thdr, type_, inner_thdr->used) = pdata[i];    \
            inner_thdr->used += 1;                                           \
        }                                                                    \
    } while (0)

    switch (thdr->type) {
    case TA_BYTE:
        FILL_LOWS(unsigned char, ucval);
        switch ((enum flags_e)opt) {
        case TA_COUNT_CMD: FILL_COUNTS(unsigned char, ucval, unsigned char, ucval ); break;
        case TA_SUM_CMD: FILL_SUMS(unsigned char, ucval, Tcl_WideInt, unsigned char, ucval, ovf_add_int64); break;
        case TA_VALUES_CMD: FILL_VALUES(unsigned char, ucval, unsigned char, ucval); break;
        case TA_INDICES_CMD: FILL_INDICES(unsigned char, ucval, unsigned char, ucval); break;
        }
        break;
    case TA_INT:
        FILL_LOWS(int, ival);
        switch ((enum flags_e)opt) {
            /* NOTE: last two macro params are *unsigned* types */
        case TA_COUNT_CMD: FILL_COUNTS(int, ival, unsigned int, uival); break;
        case TA_SUM_CMD: FILL_SUMS(int, ival, Tcl_WideInt, unsigned int, uival, ovf_add_int64); break;
        case TA_VALUES_CMD: FILL_VALUES(int, ival, unsigned int, uival); break;
        case TA_INDICES_CMD: FILL_INDICES(int, ival, unsigned int, uival); break;
        }
        break;
    case TA_UINT:
        FILL_LOWS(unsigned int, uival);
        switch ((enum flags_e)opt) {
        case TA_COUNT_CMD: FILL_COUNTS(unsigned int, uival, unsigned int, uival); break;
        case TA_SUM_CMD: FILL_SUMS(unsigned int, uival, Tcl_WideInt, unsigned int, uival, ovf_add_int64); break;
        case TA_VALUES_CMD: FILL_VALUES(unsigned int, uival, unsigned int, uival); break;
        case TA_INDICES_CMD: FILL_INDICES(unsigned int, uival, unsigned int, uival); break;
        }
        break;
    case TA_WIDE:
        FILL_LOWS(Tcl_WideInt, wval);
        switch ((enum flags_e)opt) {
        case TA_COUNT_CMD: FILL_COUNTS(Tcl_WideInt, wval, uint64_t, uwval); break;
        case TA_SUM_CMD: FILL_SUMS(Tcl_WideInt, wval, Tcl_WideInt, uint64_t, uwval, ovf_add_int64); break;
        case TA_VALUES_CMD: FILL_VALUES(Tcl_WideInt, wval, uint64_t, uwval); break;
        case TA_INDICES_CMD: FILL_INDICES(Tcl_WideInt, wval, uint64_t, uwval); break;
        }
        break;
    case TA_DOUBLE:
        FILL_LOWS(double, dval);
        switch ((enum flags_e)opt) {
        case TA_COUNT_CMD: FILL_COUNTS(double, dval, double, dval); break;
        case TA_SUM_CMD: FILL_SUMS(double, dval, double, double, dval, ovf_add_double); break;
        case TA_VALUES_CMD: FILL_VALUES(double, dval, double, dval); break;
        case TA_INDICES_CMD: FILL_INDICES(double, dval, double, dval); break;
        }
        break;
    }

    objs[0] = tcol_new(lows_thdr);
    objs[1] = tcol_new(buckets);

    Tcl_SetObjResult(ip, Tcl_NewListObj(2, objs));
    return TCL_OK;

computation_overflow:
    Tcl_SetResult(ip, "Computational overflow occurred.", TCL_STATIC);
    goto error_return;

invalid_limits:
    Tcl_SetResult(ip, "The specified maximum must not be less than the specified minimum.", TCL_STATIC);
    goto error_return;

bucket_error:
    Tcl_SetResult(ip, "Internal error: bucket index is not within limits.", TCL_STATIC);
    goto error_return;

nonpositive_step: /* objv[6] must be step value */
    Tcl_SetObjResult(ip, Tcl_ObjPrintf("Step value %s does not fit in target type width.", Tcl_GetString(objv[6])));
    goto error_return;

invalid_bucket_interval:
    Tcl_SetResult(ip, "The bucket parameters are invalid or out of range for the column type.", TCL_STATIC);
    
error_return:
    thdr_free(buckets);
    thdr_free(lows_thdr);
    return TCL_ERROR;
}

TCL_RESULT
tcol_create_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Size nelems, init_size;
    unsigned char tatype;
    Tcl_Obj **elems;
    thdr_t *thdrP;
    Tcl_Obj *o;

    if (objc < 2 || objc > 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "TYPE ?INITVAL? ?INITSIZE?");
        return TCL_ERROR;
    }

    if (ta_parse_type(ip, objv[1], &tatype) != TCL_OK)
	return TCL_ERROR;

    init_size = 0;
    nelems = 0;
    elems = NULL;
    if (objc > 2) {
        if (objc > 3) {
            if (Tcl_GetSizeIntFromObj(ip, objv[3], &init_size) != TCL_OK)
                return TCL_ERROR;
        }
        if (tcol_convert(NULL, objv[2]) == TCL_OK) {
            span_t *span;
            Tcl_Size first, count;
            thdr_t *thdr2;
            o = objv[2];
            if (tcol_type(o) == tatype) {
                Tcl_SetObjResult(ip, o); /* Already the right type */
                return TCL_OK;
            }
            thdrP = tcol_thdr(o);

            /* Special case - if signed/unsigned issue AND unshared
               AND no string rep just flip the thdr header */
            if (! Tcl_IsShared(o) && o->bytes == NULL) {
                if (thdrP->nrefs == 1) {
                    if ((thdrP->type == TA_INT && tatype == TA_UINT)
                        || (thdrP->type == TA_UINT && tatype == TA_INT)) {
                        thdrP->type = tatype;
                        thdrP->sort_order = THDR_UNSORTED;
                        Tcl_SetObjResult(ip, o);
                        return TCL_OK;
                    }
                }
            }

            span = tcol_span(o);
            if (span) {
                first = span->first;
                count = span->count;
            } else {
                first = 0;
                count = thdrP->used;
            }

            thdr2 = thdr_alloc(ip, tatype, count > init_size ? count : init_size);
            if (thdr2 == NULL)
                return TCL_ERROR;
            if (thdr_copy_cast(ip, thdr2, 0, thdrP, first, count, 0, 1) != TCL_OK) {
                thdr_decr_refs(thdr2);
                return TCL_ERROR;
            }
            o = tcol_new(thdr2);
            TA_ASSERT(tcol_check(ip, o));
            Tcl_SetObjResult(ip, o);
            return TCL_OK;
        }

        /* Initializer is not a column. Treat as a list */
        if (Tcl_ListObjGetElements(ip, objv[2], &nelems, &elems) != TCL_OK)
            return TCL_ERROR;
    }

    thdrP = thdr_alloc_and_init(ip, tatype, nelems, elems, init_size);
    if (thdrP == NULL)
        return TCL_ERROR;

    o = tcol_new(thdrP);

    TA_ASSERT(tcol_check(ip, o));
    Tcl_SetObjResult(ip, o);
    return TCL_OK;

}

TCL_RESULT
tcol_delete_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;

    if (objc != 3 && objc != 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "TARRAY (INDEXLIST | LOW ?HIGH?)");
        return TCL_ERROR;
    }

    tcol = objv[1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(objv[1]);

    status = tcol_delete(ip, tcol, objv[2], objc == 4 ? objv[3] : NULL);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);

}

TCL_RESULT
tcol_vdelete_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;

    if (objc != 3 && objc != 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "TARRAYVAR (INDEXLIST | LOW ?HIGH?)");
        return TCL_ERROR;
    }

    tcol = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    status = tcol_delete(ip, tcol, objv[2], objc == 4 ? objv[3] : NULL);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, objv[1], tcol);
}

TCL_RESULT
tcol_size_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }
    if (tcol_convert(ip, objv[1]) != TCL_OK)
        return TCL_ERROR;
    Tcl_SetObjResult(ip, Tcl_NewWideIntObj(tcol_occupancy(objv[1])));
    return TCL_OK;
}

TCL_RESULT
tcol_type_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }
    if (tcol_convert(ip, objv[1]) != TCL_OK)
        return TCL_ERROR;
    Tcl_SetObjResult(ip,
                     Tcl_NewStringObj(ta_type_string(tcol_type(objv[1])), -1));
    return TCL_OK;
}

TCL_RESULT
tcol_equal_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    int equal;
    if (objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }
    if (tcol_convert(ip, objv[1]) != TCL_OK || tcol_convert(ip, objv[2]) != TCL_OK)
        return TCL_ERROR;
    equal = tcol_equality_test(ip, objv[1], objv[2], cdata != NULL);
    Tcl_SetObjResult(ip, Tcl_NewBooleanObj(equal));
    return TCL_OK;
}

TCL_RESULT
tcol_cast_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Size count, first;
    unsigned char tatype;
    thdr_t *thdr, *thdr2;
    Tcl_Obj *o;
    span_t *span;

    if (objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "TYPE COLUMN");
        return TCL_ERROR;
    }

    if (ta_parse_type(ip, objv[1], &tatype) != TCL_OK)
	return TCL_ERROR;

    /* Convert the column AFTER COLTYPE conversion above to protect against
       shimmering */
    if (tcol_convert(ip, objv[2]) != TCL_OK)
        return TCL_ERROR;

    if (tcol_type(objv[2]) == tatype) {
        Tcl_SetObjResult(ip, objv[2]);
        return TCL_OK;
    }

    /* Special case - if signed/unsigned issue AND unshared just flip the
     * thdr header. Note that even if the thdr->nrefs is 1, the thdr
     * is shared if the Tcl_Obj is itself shared.
     */
    thdr = tcol_thdr(objv[2]);
    if (((thdr->type == TA_INT && tatype == TA_UINT)
         ||
         (thdr->type == TA_UINT && tatype == TA_INT))
        && thdr->nrefs == 1
        && ! Tcl_IsShared(objv[2])) {
        /* Need to invalid existing string rep since a int like -1
         * would now have a string rep of 4294967295 and vice versa */
        Tcl_InvalidateStringRep(objv[2]);
        thdr->type = tatype;
        thdr->sort_order = THDR_UNSORTED;
        Tcl_SetObjResult(ip, objv[2]);
        return TCL_OK;
    }

    span = tcol_span(objv[2]);
    if (span) {
       first = span->first;
       count = span->count;
    } else {
        first = 0;
        count = thdr->used;
    }

    thdr2 = thdr_alloc(ip, tatype, count);
    if (thdr2 == NULL)
        return TCL_ERROR;
    if (thdr_copy_cast(ip, thdr2, 0, thdr, first, count, 0, 0) != TCL_OK) {
        thdr_decr_refs(thdr2);
        return TCL_ERROR;
    }
    o = tcol_new(thdr2);
    TA_ASSERT(tcol_check(ip, o));
    Tcl_SetObjResult(ip, o);
    return TCL_OK;
}

TCL_RESULT
tcol_index_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Size ix;
    Tcl_Obj *o;
    if (objc != 3) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN INDEX");
        return TCL_ERROR;
    }
    if (ta_convert_index(ip, objv[2], &ix, tcol_occupancy(objv[1])-1, 0, tcol_occupancy(objv[1])-1) != TCL_OK)
        return TCL_ERROR;

    /* Convert the column AFTER INDEX conversion above to protect against
       shimmering in case both refer to same object */
    if (tcol_convert(ip, objv[1]) != TCL_OK)
        return TCL_ERROR;

    o = tcol_index(ip, objv[1], ix);
    if (o) {
        Tcl_SetObjResult(ip, o);
        return TCL_OK;
    } else
        return TCL_ERROR;
}

TCL_RESULT
tcol_get_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    return tcol_retrieve(ip, objc, objv, (int) (intptr_t) cdata);
}

TCL_RESULT
tcol_sort_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    int flags, status;
    Tcl_Obj *tcol;
    Tcl_Obj *otarget = NULL;

    if (objc < 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "?options? tarray");
        return TCL_ERROR;
    }

    if ((status = tcol_parse_sort_options(ip, objc, objv, &flags, &otarget)) != TCL_OK)
        return status;

    tcol = objv[objc-1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    if (flags & TA_SORT_INDIRECT) {
        TA_ASSERT(otarget != NULL);
        status = tcol_sort_indirect(ip, tcol, otarget, flags);
    } else
        status = tcol_sort(ip, tcol, flags);

    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vsort_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;
    int flags;
    Tcl_Obj *otarget = NULL;

    if (objc < 2) {
    	Tcl_WrongNumArgs(ip, 1, objv, "?options? COLUMNVAR");
        return TCL_ERROR;
    }

    if ((status = tcol_parse_sort_options(ip, objc, objv, &flags, &otarget)) != TCL_OK)
        return status;

    if (flags & TA_SORT_INDICES) {
        Tcl_SetResult(ip, "Invalid option -indices", TCL_STATIC);
        return TCL_ERROR;
    }

    tcol = Tcl_ObjGetVar2(ip, objv[objc-1], NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    if (flags & TA_SORT_INDIRECT) {
        TA_ASSERT(otarget != NULL);
        status = tcol_sort_indirect(ip, tcol, otarget, flags);
    } else {
        status = tcol_sort(ip, tcol, flags);
    }

    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, objv[objc-1], tcol);
}

/* TBD - is this documented? Also test cases */
TCL_RESULT
tcol_bitsset_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol = objv[1];

    thdr_t *thdrP;
    span_t *span;
    Tcl_Size count, first;

    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }
    if (tcol_convert(ip, tcol) != TCL_OK)
        return TCL_ERROR;

    thdrP = tcol_thdr(tcol);

    if (thdrP->type != TA_BOOLEAN) {
        Tcl_SetResult(ip, "tarray type is not boolean", TCL_STATIC);
        return TCL_ERROR;
    }

    span = tcol_span(tcol);
    if (span) {
        first = span->first;
        count = span->count;
    } else {
        first = 0;
        count = thdrP->used;
    }
    Tcl_SetObjResult(
        ip,
        Tcl_NewWideIntObj(ba_count_ones(THDRELEMPTR(thdrP, ba_t, 0), first, first+count)));
    return TCL_OK;

}

TCL_RESULT
tcol_insert_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;
    Tcl_Size count;

    if (objc != 4 && objc != 5) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN VALUE POSITION ?COUNT?");
        return TCL_ERROR;
    }

    if (objc == 5) {
        status = ta_get_count_from_obj(ip, objv[4], 1, &count);
        if (status != TCL_OK)
            return status;
    } else
        count = 1;

    tcol = objv[1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    status = tcol_insert_elem(ip, tcol, objv[2], objv[3], count);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vinsert_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;
    Tcl_Size count;

    if (objc != 4 && objc != 5) {
    	Tcl_WrongNumArgs(ip, 1, objv, "COLUMNVAR VALUE POSITION ?COUNT?");
	return TCL_ERROR;
    }

    if (objc == 5) {
        status = ta_get_count_from_obj(ip, objv[4], 1, &count);
        if (status != TCL_OK)
            return status;
    } else
        count = 1;

    tcol = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    status = tcol_insert_elem(ip, tcol, objv[2], objv[3], count);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, objv[1], tcol);
}

TCL_RESULT
tcol_inject_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;

    if (objc != 4) {
    	Tcl_WrongNumArgs(ip, 1, objv, "COLUMN VALUES POSITION");
	return TCL_ERROR;
    }

    tcol = objv[1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    status = tcol_inject_elems(ip, tcol, objv[2], objv[3]);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vinject_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;

    if (objc != 4) {
    	Tcl_WrongNumArgs(ip, 1, objv, "COLUMNVAR VALUES POSITION");
	return TCL_ERROR;
    }

    tcol = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    status = tcol_inject_elems(ip, tcol, objv[2], objv[3]);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, objv[1], tcol);
}

TCL_RESULT
tcol_put_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    Tcl_Obj *valuesObj;
    int status;

    if (objc < 3 || objc > 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN VALUES ?FIRST?");
        return TCL_ERROR;
    }

    tcol = objv[1];
    valuesObj = objv[2];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    /* Values may be given as a list or a tarray */
    if (tcol_convert(NULL, objv[2]) == TCL_OK) {
        /* It is a tarray. Verify the right type */
        status = tcol_copy_thdr(ip, tcol, tcol_thdr(valuesObj), tcol_span(valuesObj), objc == 4 ? objv[3] : NULL, 0);
    } else {
        status = tcol_put_objs(ip, tcol, valuesObj, objc == 4 ? objv[3] : NULL, 0);
    }

    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vput_cmd (void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *varObj;
    Tcl_Obj *valuesObj;
    Tcl_Obj *tcol;
    int status;

    if (objc != 3 && objc != 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLVAR VALUES ?FIRST?");
        return TCL_ERROR;
    }
    varObj = objv[1];
    valuesObj = objv[2];

    tcol = Tcl_ObjGetVar2(ip, varObj, NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    /* Values may be given as a list or a tarray */
    if (tcol_convert(NULL, objv[2]) == TCL_OK) {
        /* It is a tarray */
        status = tcol_copy_thdr(ip, tcol, tcol_thdr(valuesObj),
                                tcol_span(valuesObj),
                                objc == 4 ? objv[3] : NULL, 0);
    } else {
        status = tcol_put_objs(ip, tcol, valuesObj, objc == 4 ? objv[3] : NULL, 0);
    }
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, varObj, tcol);
}

TCL_RESULT
tcol_place_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    Tcl_Obj *valuesObj;
    int status;

    if (objc != 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN VALUES INDICES");
        return TCL_ERROR;
    }

    tcol = objv[1];
    valuesObj = objv[2];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    if (tcol_convert(NULL, valuesObj) == TCL_OK)
        status = tcol_place_indices(ip, tcol, valuesObj, objv[3]);
    else
        status = tcol_place_objs(ip, tcol, valuesObj, objv[3]);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vplace_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *varObj;
    Tcl_Obj *valuesObj;
    Tcl_Obj *tcol;
    int status;

    if (objc != 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLVAR VALUES INDICES");
        return TCL_ERROR;
    }

    varObj = objv[1];
    valuesObj = objv[2];
    tcol = Tcl_ObjGetVar2(ip, varObj, NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    if (tcol_convert(NULL, valuesObj) == TCL_OK)
        status = tcol_place_indices(ip, tcol, valuesObj, objv[3]);
    else
        status = tcol_place_objs(ip, tcol, valuesObj, objv[3]);

    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, varObj, tcol);
}

TCL_RESULT
tcol_fill_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;

    if (objc != 4 && objc != 5) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN VALUE (INDEXLIST | LOW ?HIGH?)");
        return TCL_ERROR;
    }

    tcol = objv[1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    status = tcol_fill_obj(ip, tcol, objv[2], objv[3],
                           objc == 5 ? objv[4] : NULL);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vfill_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *tcol;
    int status;

    if (objc != 4 && objc != 5) {
        Tcl_WrongNumArgs(
            ip, 1, objv, "TCOLUMNVAR VALUE (INDEXLIST | LOW ?HIGH?)");
        return TCL_ERROR;
    }

    tcol = Tcl_ObjGetVar2(ip, objv[1], NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    status = tcol_fill_obj(ip, tcol, objv[2], objv[3], objc == 5 ? objv[4] : NULL);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, objv[1], tcol);
}

TCL_RESULT
tcol_reverse_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    TCL_RESULT status;
    Tcl_Obj *tcol;
    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLUMN");
        return TCL_ERROR;
    }
    tcol = objv[1];
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);
    status = tcol_reverse(ip, tcol);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_return_result(ip, status, tcol);
}

TCL_RESULT
tcol_vreverse_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    Tcl_Obj *ovar;
    Tcl_Obj *tcol;
    TCL_RESULT status;
    if (objc != 2) {
        Tcl_WrongNumArgs(ip, 1, objv, "COLVAR");
        return TCL_ERROR;
    }
    ovar = objv[1];
    tcol = Tcl_ObjGetVar2(ip, ovar, NULL, TCL_LEAVE_ERR_MSG);
    if (tcol == NULL)
        return TCL_ERROR;
    if (Tcl_IsShared(tcol))
        tcol = Tcl_DuplicateObj(tcol);

    status = tcol_reverse(ip, tcol);
    TA_ASSERT(status != TCL_OK || tcol_check(ip, tcol));
    return ta_set_var_result(ip, status, ovar, tcol);
}

TCL_RESULT
tcol_intersect3_cmd(void *cdata, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{

    Tcl_Obj *acol, *bcol;
    thdr_t *athdr, *bthdr;
    span_t *aspan, *bspan;
    Tcl_Size afirst, acount, bfirst, bcount, nmin;
    thdr_t *aintersectb=NULL, *aminusb=NULL, *bminusa=NULL;
    int atype;
    unsigned char order, a_current_order, b_current_order;
    int free_acol = 0, free_bcol = 0;
    TCL_RESULT status = TCL_ERROR;
    int sort_flags, nocase = 0;
    Tcl_Obj *results[3];

    if (objc != 3 && objc != 4) {
        Tcl_WrongNumArgs(ip, 1, objv, "?-nocase? COLUMNA COLUMNB");
        return TCL_ERROR;
    }

    if (objc == 4) {
        if (! ta_strequal(Tcl_GetString(objv[1]), "-nocase"))
            return ta_invalid_opt_error(ip, "-nocase");
        nocase = 1;
    }

    acol = objv[objc-2];
    bcol = objv[objc-1];

    if ((status = tcol_convert(ip, acol)) != TCL_OK ||
        (status = tcol_convert(ip, bcol)) != TCL_OK)
        return status;

    athdr = OBJTHDR(acol);
    aspan = OBJTHDRSPAN(acol);
    atype = athdr->type;
    acount = aspan? aspan->count : athdr->used;
    bthdr = OBJTHDR(bcol);
    bspan = OBJTHDRSPAN(bcol);
    bcount = bspan? bspan->count : bthdr->used;

    if (athdr == bthdr && aspan == bspan) {
        /* TBD - also special case when either column is zero size */
        /* Optimize same object. Dunno how common this might be but ... */
        results[0] = acol;
        aminusb = thdr_alloc(ip, atype, 0);
        if (aminusb == NULL)
            goto vamoose;
        results[1] = tcol_new(aminusb);
        results[2] = results[1];
        Tcl_SetObjResult(ip, Tcl_NewListObj(3, results));
        return TCL_OK;
    }

    if (atype != bthdr->type)
        return ta_mismatched_types_error(ip, atype, bthdr->type);

    a_current_order = athdr->sort_order;
    b_current_order = bthdr->sort_order;

    /* Ensure both columns are in same sort order */
    order = THDR_UNSORTED;
    if (atype == TA_ANY || atype == TA_STRING) {
        /* TA_ANY/TA_STRING separated because we have to deal with nocase */
        if (a_current_order == b_current_order) {
            /* If unsorted or sorted with different case-sensitivity, resort */
            if ((nocase && SORT_ORDER_IS_NOCASE(a_current_order)) ||
                (!nocase && SORT_ORDER_IS_CASE(a_current_order))) {
                /* Both sorted appropriately. Lucky us, nought to do */
                order = a_current_order;
            } else {
                /* Either unsorted or wrong case sensitivity. We have to
                   sort/resort but try to make it as close to current
                   order for efficiency
                */
                if (SORT_ORDER_IS_DESCENDING(a_current_order))
                    order = nocase ? THDR_SORTED_DESCENDING_NOCASE : THDR_SORTED_DESCENDING;
                else
                    order = nocase ? THDR_SORTED_ASCENDING_NOCASE : THDR_SORTED_ASCENDING;
            }
        } else {
            /* Sort state is different for a and b */
            if ((nocase && SORT_ORDER_IS_NOCASE(a_current_order)) ||
                (!nocase && SORT_ORDER_IS_CASE(a_current_order))) {
                /* a is sorted appropriately. */
                if ((nocase && SORT_ORDER_IS_NOCASE(b_current_order)) ||
                    (!nocase && SORT_ORDER_IS_CASE(b_current_order))) {
                    /* b is also sorted appropriately but different from a, choose to sort smaller one.*/
                    order = acount < bcount ? b_current_order : a_current_order;
                } else
                    order = a_current_order; /* Only b needs sorting */
            } else {
                /* a is not sorted appropriately. */
                if ((nocase && SORT_ORDER_IS_NOCASE(b_current_order)) ||
                    (!nocase && SORT_ORDER_IS_CASE(b_current_order))) {
                    /* But b is so use its order */
                    order = b_current_order; /* sort a as b */
                } else {
                    /* Neither is sorted appropriately. Both will have to
                       be sorted. Still try to optimize by picking
                       something closer to the bigger column.
                    */
                    if (acount >= bcount) {
                        if (SORT_ORDER_IS_DESCENDING(a_current_order))
                            order = nocase ? THDR_SORTED_DESCENDING_NOCASE : THDR_SORTED_DESCENDING;
                        else if (SORT_ORDER_IS_ASCENDING(a_current_order))
                            order = nocase ? THDR_SORTED_ASCENDING_NOCASE : THDR_SORTED_ASCENDING;
                    }
                    if (order == 0) {
                        if (SORT_ORDER_IS_DESCENDING(b_current_order))
                            order = nocase ? THDR_SORTED_DESCENDING_NOCASE : THDR_SORTED_DESCENDING;
                        else
                            order = nocase ? THDR_SORTED_ASCENDING_NOCASE : THDR_SORTED_ASCENDING;
                    }
                }
            }
        }
    } else if (atype != TA_BOOLEAN) {
        if (a_current_order == b_current_order) {
            if (a_current_order == THDR_UNSORTED)
                order = THDR_SORTED_ASCENDING;/* Both need sorting */
            else
                order = a_current_order;
        }
        else {
            /* a and b not sorted the same */
            if (a_current_order == THDR_UNSORTED)
                order = b_current_order; /* Sort a same as b */
            else if (bthdr->sort_order == THDR_UNSORTED)
                order = a_current_order; /* Sort b same as a */
            else {
                /* Both are sorted but not in same order. Resort the smaller */
                if (athdr->used < bthdr->used)
                    order = b_current_order;
                else
                    order = a_current_order;
            }
        }
    }

    if (atype != TA_BOOLEAN) {
        switch (order) {
        case THDR_SORTED_ASCENDING:         sort_flags = 0; break;
        case THDR_SORTED_DESCENDING:        sort_flags = TA_SORT_DECREASING; break;
        case THDR_SORTED_ASCENDING_NOCASE:  sort_flags = TA_SORT_NOCASE; break;
        case THDR_SORTED_DESCENDING_NOCASE: sort_flags = TA_SORT_NOCASE | TA_SORT_DECREASING; break;
        default:
            sort_flags = 0; /* Keep gcc happy */
            Tcl_Panic("Unexpected sort order %d in function intersect", order);
        }

        if (a_current_order != order) {
            /* a needs to be sorted. Needs to be unshared to do so */
            if (Tcl_IsShared(acol)) {
                acol = Tcl_DuplicateObj(acol);
                free_acol = 1;
            }
            if (tcol_sort(ip, acol, sort_flags) != TCL_OK)
                goto vamoose;
            athdr = tcol_thdr(acol);
            aspan = tcol_span(acol);
            TA_ASSERT(aspan == NULL);
            TA_ASSERT(acount == tcol_occupancy(acol));
        }

        if (b_current_order != order) {
            /* b needs to be sorted. Needs to be unshared to do so */
            if (Tcl_IsShared(bcol)) {
                bcol = Tcl_DuplicateObj(bcol);
                free_bcol = 1;
            }
            if (tcol_sort(ip, bcol, sort_flags) != TCL_OK)
                goto vamoose;
            bthdr = tcol_thdr(bcol);
            bspan = tcol_span(bcol);
            TA_ASSERT(bspan == NULL);
            TA_ASSERT(bcount == tcol_occupancy(bcol));
        }
    }

    /*
     * Create 3 arrays to hold the a intersect b, a-b, b-a.
     * Max num elems in intersect is smaller of two arrays.
     * Max num in a-b and b-a is num in a and b respectively.
     */
    nmin = acount;
    if (nmin > bcount)
        nmin = bcount;

    aintersectb = thdr_alloc(ip, atype, nmin);
    aminusb = thdr_alloc(ip, atype, acount);
    bminusa = thdr_alloc(ip, atype, bcount);
    if (aintersectb == NULL || aminusb == NULL || bminusa == NULL)
        goto vamoose;

    afirst = aspan ? aspan->first : 0;
    bfirst = bspan ? bspan->first : 0;

    /* IMPORTANT - INTERSECT must NOT error out else ref counts for
       type TA_ANY will be broken as they are updated later */
#define EQ(x_, y_) ((x_) == (y_))
#define LT(x_, y_) ((x_) < (y_))
#define INTERSECT(t_)                                                       \
    do {                                                                    \
        t_ *pintersect = THDRELEMPTR(aintersectb, t_, 0);                   \
        t_ *paminusb   = THDRELEMPTR(aminusb, t_, 0);                       \
        t_ *pbminusa   = THDRELEMPTR(bminusa, t_, 0);                       \
        t_ *a          = THDRELEMPTR(athdr, t_, afirst);                    \
        t_ *b          = THDRELEMPTR(bthdr, t_, bfirst);                    \
        t_ *aend       = a + acount;                                        \
        t_ *bend       = b + bcount;                                        \
                                                                            \
        if (SORT_ORDER_IS_ASCENDING(order)) {                               \
            while (a < aend && b < bend) {                                  \
                if (EQ(*a, *b)) {                                           \
                    *pintersect++ = *a;                                     \
                    ++a;                                                    \
                    ++b;                                                    \
                }                                                           \
                else if (LT(*a, *b)) {                                      \
                    *paminusb++ = *a++;                                     \
                }                                                           \
                else {                                                      \
                    *pbminusa++ = *b++;                                     \
                }                                                           \
            }                                                               \
        }                                                                   \
        else {                                                              \
            /* Sorted in descending order */                                \
            while (a < aend && b < bend) {                                  \
                if (EQ(*a, *b)) {                                           \
                    *pintersect++ = *a;                                     \
                    ++a;                                                    \
                    ++b;                                                    \
                }                                                           \
                else if (LT(*a, *b)) {                                      \
                    *pbminusa++ = *b++;                                     \
                }                                                           \
                else {                                                      \
                    *paminusb++ = *a++;                                     \
                }                                                           \
            }                                                               \
        }                                                                   \
                                                                            \
        /* Now copy left over elements */                                   \
        while (a < aend) {                                                  \
            *paminusb++ = *a++;                                             \
        }                                                                   \
        while (b < bend) {                                                  \
            *pbminusa++ = *b++;                                             \
        }                                                                   \
                                                                            \
        aintersectb->used =                                                 \
            (Tcl_Size)(pintersect - THDRELEMPTR(aintersectb, t_, 0));       \
        aminusb->used = (Tcl_Size)(paminusb - THDRELEMPTR(aminusb, t_, 0)); \
        bminusa->used = (Tcl_Size)(pbminusa - THDRELEMPTR(bminusa, t_, 0)); \
    } while (0)

    switch (atype) {
    case TA_BOOLEAN:
       {
           Tcl_Size ones_in_a, ones_in_b, zeroes_in_a, zeroes_in_b;
           Tcl_Size ones, zeroes;
           ba_t *pbits;
           ones_in_a = ba_count_ones(THDRELEMPTR(athdr, ba_t, 0), afirst, afirst+acount);
           ones_in_b = ba_count_ones(THDRELEMPTR(bthdr, ba_t, 0), bfirst, bfirst+bcount);
           zeroes_in_a = acount - ones_in_a;
           zeroes_in_b = bcount - ones_in_b;
           ones = ones_in_a < ones_in_b ? ones_in_a : ones_in_b;
           zeroes = zeroes_in_a < zeroes_in_b ? zeroes_in_a : zeroes_in_b;
           pbits = THDRELEMPTR(aintersectb, ba_t, 0);
           ba_fill(pbits, 0, zeroes, 0);
           ba_fill(pbits, zeroes, ones, 1);
           aintersectb->used = ones + zeroes;
           pbits = THDRELEMPTR(aminusb, ba_t, 0);
           if (zeroes_in_a > zeroes) {
               aminusb->used = zeroes_in_a - zeroes;
               ba_fill(pbits, 0, aminusb->used, 0);
           }
           if (ones_in_a > ones) {
               ba_fill(pbits, aminusb->used, ones_in_a - ones, 1);
               aminusb->used += ones_in_a - ones;
           }
           pbits = THDRELEMPTR(bminusa, ba_t, 0);
           if (zeroes_in_b > zeroes) {
               bminusa->used = zeroes_in_b - zeroes;
               ba_fill(pbits, 0, bminusa->used, 0);
           }
           if (ones_in_b > ones) {
               ba_fill(pbits, bminusa->used, ones_in_b - ones, 1);
               bminusa->used += ones_in_b - ones;
           }
       }
       break;

    case TA_UINT:
        INTERSECT(unsigned int);
        break;
    case TA_INT:
        INTERSECT(int);
        break;
    case TA_WIDE:
        INTERSECT(Tcl_WideInt);
        break;
    case TA_DOUBLE:
        INTERSECT(double);
        break;
    case TA_BYTE:
        INTERSECT(unsigned char);
        break;
    case TA_ANY:
#undef EQ
#undef LT
        /* TBD - why not EQ -> ta_obj_equal ? */
#define EQ(x_, y_) (ta_obj_compare((x_), (y_), nocase) == 0)
#define LT(x_, y_) (ta_obj_compare((x_), (y_), nocase) < 0)
        INTERSECT(Tcl_Obj *);
        /* Need to update ref counts */
        thdr_incr_obj_refs(aintersectb, 0, aintersectb->used);
        thdr_incr_obj_refs(aminusb, 0, aminusb->used);
        thdr_incr_obj_refs(bminusa, 0, bminusa->used);
        break;
    case TA_STRING:
#undef EQ
#undef LT
#define EQ(x_, y_) (tas_equal((x_), (y_), nocase))
#define LT(x_, y_) (tas_compare((x_), (y_), nocase) < 0)
        INTERSECT(tas_t *);
        /* Need to update ref counts */
        thdr_incr_tas_refs(aintersectb, 0, aintersectb->used);
        thdr_incr_tas_refs(aminusb, 0, aminusb->used);
        thdr_incr_tas_refs(bminusa, 0, bminusa->used);
        break;
    }

#undef EQ
#undef LT
    TA_ASSERT(aintersectb->used <= aintersectb->usable);
    TA_ASSERT(aminusb->used <= aminusb->usable);
    TA_ASSERT(bminusa->used <= bminusa->usable);
           
    /* TBD - shrink arrays if underutilized */

    results[0] = tcol_new(aintersectb);
    results[1] = tcol_new(aminusb);
    results[2] = tcol_new(bminusa);
    TA_ASSERT(tcol_check(ip, results[0]));
    TA_ASSERT(tcol_check(ip, results[1]));
    TA_ASSERT(tcol_check(ip, results[2]));
    Tcl_SetObjResult(ip, Tcl_NewListObj(3, results));

    status = TCL_OK;

vamoose:
    if (free_acol)
        Tcl_DecrRefCount(acol);
    if (free_bcol)
        Tcl_DecrRefCount(bcol);
    if (status != TCL_OK) {
        if (aintersectb)
            thdr_decr_refs(aintersectb);
        if (aminusb)
            thdr_decr_refs(aminusb);
        if (bminusa)
            thdr_decr_refs(bminusa);
    }

    return status;
}

/*
  Local Variables:
  compile-command: "envset x64 && tclsh build.tcl extension -config ../src/tarray.cfg -keep -target win32-dev64"
  End:
*/

