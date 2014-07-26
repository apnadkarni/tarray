/*
 * Copyright (c) 2012, 2013 Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file LICENSE for license
 */

#if __GNUC__ && !__GNUC_STDC_INLINE__
/* Force generation of code for inline - older gnu compilers */
#define TA_INLINE
#endif

#include "tarray.h"

int ta_experiment;

#ifdef TA_MT_ENABLE
/*
 * Thresholds for multithreading.
 * TBD - need to benchmark and set. Likely to depend on compiler.
 */
int ta_fill_mt_threshold = 100000;
int ta_minmax_mt_threshold = 100000;
#endif

/* Must match definitions in tarray.h ! */
const char *g_type_tokens[] = {
    "boolean",
    "uint",
    "int",
    "wide",
    "double",
    "byte",
    "any",
    NULL
};    

Tcl_ObjType *g_tcl_list_type_ptr;

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


/* Panics on consistency check failure. int return value so it can
 be called from TA_ASSERT */
int thdr_check(Tcl_Interp *ip, thdr_t *thdr)
{
    Tcl_Obj **objPP;
    int i;

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
                Tcl_Panic("thdr TA_ANY element ref count (%d) < 1", objPP[i]->refCount);
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

    if (tcol_convert(ip, tcol) != TCL_OK || ! tcol_affirm(tcol))
        Tcl_Panic("Tcl_Obj is not a column");

    thdr = tcol_thdr(tcol);
    if (thdr == NULL)
        Tcl_Panic("NULL thdr in Tcl_Obj");
    if (thdr->nrefs < 1)
        Tcl_Panic("Column thdr->nrefs (%d) < 1", thdr->nrefs);
    
    thdr_check(ip, thdr);

    return 1;
}

/*
 * A Tcl object to parse tarray index tokens and cache the result so
 * we do not parse "end" or "last" repeatedly. We could actually use the
 * more sophisticated Tcl equivalent but unfortunately that is not exported.
 */
static void ta_indexobj_update_string(Tcl_Obj *);
struct Tcl_ObjType ta_index_type = {
    "tarray_index",
    NULL, /* No need for a free proc */
    NULL, 
    ta_indexobj_update_string,
    NULL, /* convert from any - jenglish advises to keep this NULL */
};

const char *ta_type_string(int tatype)
{
    if (tatype < (sizeof(g_type_tokens)/sizeof(g_type_tokens[0]))) {
        return g_type_tokens[tatype];
    } else
        return "<invalid>";
}

static void tcol_type_free_intrep(Tcl_Obj *o)
{
    thdr_t *thdr;

    TA_ASSERT(tcol_affirm(o));

    thdr = OBJTHDR(o); 
    TA_ASSERT(thdr);

    thdr_decr_refs(thdr);
    OBJTHDR(o) = NULL;
    o->typePtr = NULL;
}

static void tcol_type_dup(Tcl_Obj *osrc, Tcl_Obj *odst)
{
    TA_ASSERT(tcol_affirm(osrc));
    TA_ASSERT(OBJTHDR(osrc) != NULL);
        
    tcol_set_intrep(odst, OBJTHDR(osrc));
}

#ifdef NOTUSED
/* Returns a ckalloc'ed string rep of the Tcl_Obj array */
static char *ta_generate_string_from_objv(int objc, Tcl_Obj *const *objv, int *plen)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    int i, length;
    size_t bytesNeeded;
    const char *elem;
    char *dst, *str;
    int quote_hash;

    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc. Tcl_Obj.bytes (where caller might need to
       store the result) requires ckalloc'ed memory
    */

    if (objc == 0) {
        str = ckalloc(sizeof(*str));
        *str = '\0';
        *plen = 0;
        return str;
    }

    /*
     * Pass 1: estimate space, gather flags.
     */

    if (objc <= LOCAL_SIZE) {
        flagPtr = localFlags;
    } else {
        /*
         * We know objc <= TA_MAX_OBJC, so this is safe.
         */

        flagPtr = (int *) ckalloc(objc * sizeof(int));
    }

    bytesNeeded = 0;
    quote_hash = 0;
    for (i = 0; i < objc; i++) {
        flagPtr[i] = quote_hash;
        quote_hash = TCL_DONT_QUOTE_HASH;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if ((((size_t)1) << (sizeof(bytesNeeded)*CHAR_BIT - 1)) & bytesNeeded)
            ta_string_overflow_panic("tcol_type_update_string_for_objtype");
    }
    if ((bytesNeeded + objc + 1) > INT_MAX)
        ta_string_overflow_panic("tcol_type_update_string_for_objtype");

    bytesNeeded += objc;        /* For separators and terminating null */

    /*
     * Pass 2: copy into string rep buffer.
     */

    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    str = ckalloc(bytesNeeded);
    dst = str;
    quote_hash = 0;
    for (i = 0; i < objc; i++) {
        flagPtr[i] |= quote_hash;
        quote_hash = TCL_DONT_QUOTE_HASH;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        dst += Tcl_ConvertCountedElement(elem, length, dst, flagPtr[i]);
        *dst++ = ' ';
    }
    *--dst = '\0';
    TA_ASSERT(dst < (str + bytesNeeded));
    *plen = dst - str;

    if (flagPtr != localFlags) {
        ckfree((char *) flagPtr);
    }

    return str;
}
#endif


/* Called to generate a string implementation for tarray columns of type TA_ANY
   and tables */
void ta_update_string_for_table_or_type_any(Tcl_Obj *o)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
    Tcl_Obj **objv;
    int objc;
    thdr_t *thdr;
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    int i, length;
    size_t bytesNeeded;
    const char *elem;
    char *dst;
    int quote_hash;
    Tcl_Obj *onames = NULL;
    const char *names;
    int names_len;

    TA_ASSERT(o->typePtr == &ta_column_type || o->typePtr == &ta_table_type);

    thdr = OBJTHDR(o);
    objv = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    objc = thdr->used;

    /*
     * Pass 1: estimate space, gather flags.
     */

    if (objc <= LOCAL_SIZE) {
        flagPtr = localFlags;
    } else {
        /*
         * We know objc <= TA_MAX_OBJC, so this is safe.
         */

        flagPtr = (int *) TA_ALLOCMEM(objc * sizeof(int));
    }

    bytesNeeded = strlen(o->typePtr->name);
    if (o->typePtr == &ta_table_type) {
        /* If a table, need to add on column names */
        TA_ASSERT(OBJCOLNAMES(o));
        onames = table_column_names(o);
        names = Tcl_GetStringFromObj(onames, &names_len);
        bytesNeeded += 1 + 1 + names_len + 1; /* With lead space, enclosing {} */
    } else {
        /* If a column, need to add the column type (always TA_ANY) in
           this routine as other types are handled elsewhere */
        TA_ASSERT(thdr->type == TA_ANY);
        names_len = strlen(g_type_tokens[TA_ANY]);
        bytesNeeded += 1 + names_len;/* With lead space */
    }

    bytesNeeded += 1 + 1 + 1; /* Lead space and enclosing {} for objv */

    quote_hash = TCL_DONT_QUOTE_HASH;
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = quote_hash;
        quote_hash = 0;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if ((((size_t)1) << (sizeof(bytesNeeded)*CHAR_BIT - 1)) & bytesNeeded)
            ta_string_overflow_panic("tcol_type_update_string_for_objtype");
    }
    if ((bytesNeeded + objc + 1) > INT_MAX)
        ta_string_overflow_panic("tcol_type_update_string_for_objtype");

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
        memcpy(dst, g_type_tokens[TA_ANY], names_len);
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
            elem = Tcl_GetStringFromObj(objv[i], &length);
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
    o->length = dst - o->bytes;

    if (flagPtr != localFlags) {
        TA_FREEMEM((char *) flagPtr);
    }
    if (onames)
        Tcl_DecrRefCount(onames);
}


/* Called to generate a string implementation from an array of Tcl_Obj */
static void OBSOLETEtcol_type_update_string_for_objtype(Tcl_Obj *o)
{
    /* Copied almost verbatim from the Tcl's UpdateStringOfList */
    Tcl_Obj **objv;
    int objc;
    thdr_t *thdr;
#   define LOCAL_SIZE 20
    int localFlags[LOCAL_SIZE], *flagPtr = NULL;
    int i, length;
    size_t bytesNeeded;
    const char *elem;
    char *dst;
    int quote_hash;

    thdr = OBJTHDR(o);
    objv = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    objc = thdr->used;

    /*
     * Pass 1: estimate space, gather flags.
     */

    if (objc <= LOCAL_SIZE) {
        flagPtr = localFlags;
    } else {
        /*
         * We know objc <= TA_MAX_OBJC, so this is safe.
         */

        flagPtr = (int *) TA_ALLOCMEM(objc * sizeof(int));
    }

    bytesNeeded =
        sizeof("tarray ") - 1 /* -1 to exclude the null */
        + sizeof(" {") - 1 /* Start of list minus trailing null */
        + 1               /* Trailing "}" */
        + strlen(g_type_tokens[TA_ANY]);
    quote_hash = TCL_DONT_QUOTE_HASH;
    for (i = 0; i < objc; i++) {
        /* TCL_DONT_QUOTE_HASH since we are not at beginning of string */
        flagPtr[i] = quote_hash;
        quote_hash = 0;
        elem = Tcl_GetStringFromObj(objv[i], &length);
        bytesNeeded += Tcl_ScanCountedElement(elem, length, &flagPtr[i]);
        if ((((size_t)1) << (sizeof(bytesNeeded)*CHAR_BIT - 1)) & bytesNeeded)
            ta_string_overflow_panic("tcol_type_update_string_for_objtype");
    }
    if ((bytesNeeded + objc + 1) > INT_MAX)
        ta_string_overflow_panic("tcol_type_update_string_for_objtype");

    bytesNeeded += objc+1;        /* For separators and terminating null */

    /*
     * Pass 2: copy into string rep buffer.
     */

    /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
       defined as ckalloc */
    o->bytes = ckalloc(bytesNeeded);
    dst = o->bytes;
    memcpy(dst, "tarray ", sizeof("tarray ")-1);
    dst += sizeof("tarray ") - 1;
    strcpy(dst, g_type_tokens[TA_ANY]);
    dst += strlen(g_type_tokens[TA_ANY]);
    *dst++ = ' ';
    *dst++ = '{';
    /* TBD - handle objc==0 case */
    if (objc) {
        quote_hash = TCL_DONT_QUOTE_HASH;
        for (i = 0; i < objc; i++) {
            flagPtr[i] |= quote_hash;
            quote_hash = 0;
            elem = Tcl_GetStringFromObj(objv[i], &length);
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
    o->length = dst - o->bytes;

    if (flagPtr != localFlags) {
        TA_FREEMEM((char *) flagPtr);
    }
}

static void tcol_type_update_string(Tcl_Obj *o)
{
    unsigned int i, n, count;
    unsigned int allocated, unused, min_needed, prefix_len;
    char *cP;
    int max_elem_space;  /* Max space to print one element including
                            either terminating null or space */
    thdr_t *thdr;
        
    TA_ASSERT(tcol_affirm(o));

    thdr = OBJTHDR(o);
    TA_ASSERT(thdr->type < sizeof(g_type_tokens)/sizeof(g_type_tokens[0]));

    o->bytes = NULL;

    prefix_len = strlen(o->typePtr->name)
        + 1            /* Space */
        + strlen(g_type_tokens[thdr->type])
        + 2;                         /* Start of list " {" */
    min_needed = prefix_len + 1 + 1;            /* Trailing "}" and null */

    count = tcol_occupancy(o);
    if (count == 0) {
        /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
           defined as ckalloc */
        cP = ckalloc(min_needed);
        o->bytes = cP;
        snprintf(cP, min_needed, "%s %s {}", o->typePtr->name,
                 g_type_tokens[thdr->type]);
        o->length = min_needed - 1;
        return;
    }

    /* Code below based on count > 0 else terminating \0 will blow memory */

    /*
     * When output size cannot be calculated exactly, we allocate using
     * some estimate based on the type.
     */
        
    switch (tcol_type(o)) {
    case TA_BOOLEAN:
        {
            /*
             * Special case Boolean since we know exactly how many chars will
             * be required 
             */
            ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
            register ba_t ba;
            register ba_t ba_mask;

            /* Note this MUST be ckalloc, not TA_ALLOCMEM which might not be
               defined as ckalloc */
            cP = ckalloc(min_needed + 2*count - 1);
            n = snprintf(cP, min_needed, "%s %s {", o->typePtr->name,
                         g_type_tokens[TA_BOOLEAN]);
            TA_ASSERT(n > 0 && n < min_needed);
            o->bytes = cP;
            cP += n;
            n = count / BA_UNIT_SIZE;
            for (i = 0; i < n; ++i, ++baP) {
                ba = *baP;
                for (ba_mask = BITPOSMASK(0); ba_mask ; ba_mask = BITMASKNEXT(ba_mask)) {
                    *cP++ = (ba & ba_mask) ? '1' : '0';
                    *cP++ = ' ';
                }
            }
            n = count - n*BA_UNIT_SIZE;    /* Left over bits in last byte */
            if (n) {
                ba = *baP;
                for (i = 0, ba_mask = BITPOSMASK(0); i < n; ++i, ba_mask = BITMASKNEXT(ba_mask)) {
                    *cP++ = (ba & ba_mask) ? '1' : '0';
                    *cP++ = ' ';
                }
            }
            cP[-1] = '}';
            *cP = '\0';
            o->length = cP - o->bytes;
        }
        return;
                
    case TA_ANY:
        ta_update_string_for_table_or_type_any(o);
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
                int *intP = THDRELEMPTR(thdr, int, i);
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
                Tcl_WideInt *pwide = THDRELEMPTR(thdr, Tcl_WideInt, i);
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
                double *pdbl = THDRELEMPTR(thdr, double, i);
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
                unsigned char *ucP = THDRELEMPTR(thdr, unsigned char, i);
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
    o->length = cP - o->bytes; /* Terminating null not included in length */
            
    /* Only shrink array if unused space is comparatively too large */
    unused = allocated - (o->length + 1);
    if (unused > (allocated / 8) && unused > 20)
        o->bytes = ckrealloc(o->bytes, o->length + 1);
    return;
}

/*
 * indexobj routines
 */
static void ta_indexobj_update_string(Tcl_Obj *o)
{
    char buffer[TCL_INTEGER_SPACE + 5];
    int len;

    if (o->internalRep.longValue == 0)
        strcpy(buffer, "end");
    else if (o->internalRep.longValue > 0)
        _snprintf(buffer, sizeof(buffer), "end+%d", o->internalRep.longValue);
    else
        _snprintf(buffer, sizeof(buffer), "end%d", o->internalRep.longValue);

    len = strlen(buffer);
    o->bytes = ckalloc(len+1);
    memcpy(o->bytes, buffer, len+1);
    o->length = len;
}

/*
 * Copied from Tcl's SetEndOffsetFromAny which is unfortunately not public
 * Look for a string of the form "end[+-]offset" and convert it to an
 * internal representation holding the offset.
 * Returns TCL_OK if ok, TCL_ERROR if the string was badly formed.
 * If interp is not NULL, stores an error message in the interpreter
 * result.
 */
int ta_indexobj_from_any(
    Tcl_Interp *interp,		/* Tcl interpreter or NULL */
    Tcl_Obj *o)		/* Pointer to the object to parse */
{
    int offset;			/* Offset in the "end-offset" expression */
    const char *bytes;	/* String rep of the object */
    int length;			/* Length of the object's string rep */

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
	if (Tcl_GetInt(interp, bytes+4, &offset) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (bytes[3] == '-') {
	    offset = -offset;
	}
    } else {
	/* Conversion failed. Report the error. */
        goto error_handler;
    }

    /* The conversion succeeded. Free the old internal rep and set the new */
    if (o->typePtr && o->typePtr->freeIntRepProc)
        o->typePtr->freeIntRepProc(o);
    o->internalRep.longValue = offset;
    o->typePtr = &ta_index_type;

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

TCL_RESULT ta_get_uint_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned int *pui)
{
    Tcl_WideInt wide;
    if (Tcl_GetWideIntFromObj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < 0 || wide > 0xFFFFFFFF)
        return ta_value_type_error(ip, o, TA_UINT);
    *pui = (unsigned int) wide;
    return TCL_OK;
}

/* We cannot rely on Tcl_GetIntFromObj because that does not signal an
   error on signed overflow (i.e. 0x80000000 is treated as a valid integer)
*/
TCL_RESULT ta_get_int_from_obj(Tcl_Interp *ip, Tcl_Obj *o, int *pi)
{
    Tcl_WideInt wide;
    if (Tcl_GetWideIntFromObj(ip, o, &wide) != TCL_OK)
        return TCL_ERROR;
    if (wide < INT_MIN || wide > INT_MAX)
        return ta_value_type_error(ip, o, TA_INT);
    *pi = (int) wide;
    return TCL_OK;
}

TCL_RESULT ta_get_byte_from_obj(Tcl_Interp *ip, Tcl_Obj *o, unsigned char *pb)
{
    int i;
    if (Tcl_GetIntFromObj(ip, o, &i) != TCL_OK)
        return TCL_ERROR;
    if (i < 0 || i > 255)
        return ta_value_type_error(ip, o, TA_BYTE);
    *pb = (unsigned char) i;
    return TCL_OK;
}


/* Increments the ref counts of Tcl_Objs in a tarray making sure not
   to run past end of array */
void thdr_incr_obj_refs(thdr_t *thdr, int first, int count)
{
    if (thdr->type == TA_ANY) {
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
}

/* Decrements the ref counts of Tcl_Objs in a tarray.
   Does NOT CLEAR ANY OTHER HEADER FIELDS. CALLER MUST DO THAT 
*/
void thdr_decr_obj_refs(thdr_t *thdr, int first, int count)
{
    if (thdr->type == TA_ANY) {
        if ((first + count) > thdr->used)
            count = thdr->used - first;
        /* Note count might even be negative, check */
        if (count > 0) {
            Tcl_Obj **pobjs, **end;
            pobjs = THDRELEMPTR(thdr, Tcl_Obj *, first);
            end = pobjs + count;
            while (pobjs < end) {
                Tcl_DecrRefCount(*pobjs);
                ++pobjs;
            }
        }
    }
}


/* Make sure all objects in the tarray have string representations */
void thdr_ensure_obj_strings(thdr_t *thdr)
{
    Tcl_Obj **pobjs, **end;

    TA_ASSERT(thdr->type == TA_ANY);

    pobjs = THDRELEMPTR(thdr, Tcl_Obj *, 0);
    end = pobjs + thdr->used;
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
int thdr_calc_mt_split(int tatype, int first, int count, int *psecond_block_size)
{
    int second_block_size;
    int memunits;
    
    /* Assumes the thdr array is aligned properly and that processors
     * already access ints, wides and doubles atomically.
     */
    switch (tatype) {
    case TA_ANY:
    case TA_INT:
    case TA_UINT:
    case TA_DOUBLE:
    case TA_WIDE:
        second_block_size = count/2;
        break;
        
    case TA_BYTE:
        /* Assumes int is appropriate for memory atomicity */
        if (count < (2*sizeof(int)))
            second_block_size = 0;
        else {
            /* Remember first may not be aligned apprpriately */
            int aligned_first;
            int remaining_count;
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
        ta_type_panic(tatype);
    }

    *psecond_block_size = second_block_size;
    return count - second_block_size;
}
#endif

/*
 * Updates TA_ANY elements at the specific indices pindices[].
 * Also updates thdr->used.
 */
void thdr_place_ta_objs(thdr_t *thdr,
                        thdr_t *pindices,
                        Tcl_Obj * const *ovalues,
                        int new_size
    )
{
    int i;
    int *pindex, *end;
    Tcl_Obj **pobjs;

    pindex = THDRELEMPTR(pindices, int, 0);
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
            Tcl_DecrRefCount(pobjs[*pindex]);/* Deref what was originally in that slot */
        pobjs[*pindex++] = *ovalues++;
    }

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
                       int new_size
    )
{
    int i;
    int *pindex, *end;
    Tcl_Obj **pobjs;

    pindex = THDRELEMPTR(pindices, int, 0);
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
TCL_RESULT ta_convert_index(Tcl_Interp *ip, Tcl_Obj *o, int *pindex, int end_value, int low, int high)
{
    int val;

    /* Do type checks to avoid expensive shimmering in case of errors */
    if (tcol_affirm(o) || table_affirm(o))
        return ta_index_error(ip, o);

    if (o->typePtr == g_tcl_list_type_ptr) {
        if (Tcl_ListObjLength(NULL, o, &val) != TCL_OK || val != 1)
            return ta_index_error(ip, o);
    }

    if (Tcl_GetIntFromObj(NULL, o, &val) != TCL_OK) {
        if (ta_indexobj_from_any(NULL, o) != TCL_OK)
            return ta_index_error(ip, o);
        val = end_value + o->internalRep.longValue;
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
TCL_RESULT ta_fix_range_bounds(Tcl_Interp *ip, int nelems, Tcl_Obj *olow, Tcl_Obj *ohigh, int *plow, int *pcount)
{
    int low, high;

    /* TBD - need we restrict low to < nelems? Some routines which allow that
     cannot call this */
    if (ta_convert_index(ip, olow, &low, nelems-1, INT_MIN, nelems) != TCL_OK)
        return TCL_ERROR;

    if (ohigh) {
        if (ta_convert_index(ip, ohigh, &high, nelems-1, INT_MIN, INT_MAX) != TCL_OK)
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

TCL_RESULT ta_parse_range_option_value(Tcl_Interp *ip, int nelems, Tcl_Obj *orange, int *plow, int *pcount)
{
    Tcl_Obj **objs;
    int       nobjs;

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
    int nelems, tatype;
    
    /* See if we can convert it to one based on string representation */
    if (Tcl_ListObjGetElements(NULL, o, &nelems, &elems) == TCL_OK
        && nelems == 3
        && !strcmp(Tcl_GetString(elems[0]), ta_column_type.name)
        && Tcl_GetIndexFromObj(ip, elems[1], g_type_tokens, "TArrayType",
                               TCL_EXACT, &tatype) == TCL_OK) {
        /* So far so good. Try and convert */
        thdr_t *thdr;
        Tcl_Obj **ovalues;
        int nvalues;
        
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

        tcol_set_intrep(o, thdr);
        return TCL_OK;
    }
                
    return ta_not_column_error(ip);
}



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
    case TA_WIDE: status = Tcl_GetWideIntFromObj(ip, o, &ptav->wval); break;
    case TA_DOUBLE: status = Tcl_GetDoubleFromObj(ip, o, &ptav->dval); break;
    case TA_ANY: ptav->oval = o; status = TCL_OK; break;
    default:
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
    default:
        ta_type_panic(ptav->type);
    }
    return NULL;
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
    default:
        ta_type_panic(pa->type);
        return 0;                   /* To keep compiler happy */
    }
#undef VALUE_CMP_
}

struct thdr_fill_mt_context {
    ta_value_t tav;
    void *base;
    int   nelems;
};

static void thdr_fill_int_mt_worker(struct thdr_fill_mt_context *pctx)
{
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

static void thdr_fill_double_mt_worker(struct thdr_fill_mt_context *pctx)
{
    double val = pctx->tav.dval;
    double *pdbl, *end;
    
    pdbl = pctx->base;
    end = pdbl + pctx->nelems;
    while (pdbl < end)
        *pdbl++ = val;
}

static void thdr_fill_wide_mt_worker(struct thdr_fill_mt_context *pctx)
{
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

static void thdr_fill_byte_mt_worker(struct thdr_fill_mt_context *pctx)
{
    memset(pctx->base, pctx->tav.ucval, pctx->nelems);
}

/*
 * Use multiple threads to fill scalar values. 
 * WARNING: Does NOT update thdr header (like thdr->used etc.)
 * Caller must do that.
 */
void thdr_fill_scalars(Tcl_Interp *ip, thdr_t *thdr,
                       const ta_value_t *ptav, int pos, int count)
{
    void (*workerfn)(struct thdr_fill_mt_context *);
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
void thdr_fill_range(Tcl_Interp *ip, thdr_t *thdr,
                     const ta_value_t *ptav, int pos, int count, int insert)
{
    int i;
    int new_used;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pos <= thdr->used);
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(count >= 0);

    if (count == 0)
        return;

    new_used = thdr_recompute_occupancy(thdr, &pos, count, insert);
    TA_ASSERT(new_used <= thdr->usable);

    if (insert)
        thdr_make_room(thdr, pos, count);

    /* NOTE in insert mode, thdr is in inconsistent state as intermediate
       slots are uninitialized. This is specially relevant for TA_ANY */

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
            int n;

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
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_used <= thdr->usable);
    thdr->used = new_used;
}

TCL_RESULT ta_verify_value_objs(Tcl_Interp *ip, int tatype,
                                int nelems, Tcl_Obj * const elems[])
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
        ta_verify_value_LOOP(Tcl_WideInt, Tcl_GetWideIntFromObj);
        break;
    case TA_DOUBLE:
        ta_verify_value_LOOP(double, Tcl_GetDoubleFromObj);
        break;
    case TA_BYTE:
        ta_verify_value_LOOP(unsigned char, ta_get_byte_from_obj);
        break;
    case TA_ANY:
        break;                  /* Just pointers, nothing to verify */
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
TCL_RESULT thdr_verify_indices_in_range(Tcl_Interp *ip, int current_size, thdr_t *pindices, int *new_sizeP)
{
    int cur, highest, status;
    int *pindex, *end;
    thdr_t *psorted = NULL;

    TA_ASSERT(pindices->type == TA_INT);

    pindex = THDRELEMPTR(pindices, int, 0);
    end = THDRELEMPTR(pindices, int, pindices->used);

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
        psorted = thdr_clone(ip, pindices, 0);
        if (psorted == NULL)
            return TCL_ERROR;
        qsort(THDRELEMPTR(psorted, int, 0), psorted->used, sizeof(int), intcmp);
        psorted->sort_order = THDR_SORTED_ASCENDING;
        pindices = psorted;
        pindex = THDRELEMPTR(pindices, int, 0);
        end = THDRELEMPTR(pindices, int, pindices->used);
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
                       int new_size
    )
{
    int *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(thdr->type == ptav->type);
    TA_ASSERT(pindices->type == TA_INT);

    /* Caller guarantees room for highest index value */
    TA_ASSERT(new_size <= thdr->usable);

    if (pindices->used == 0)
        return;          /* Nothing to do */

    /* Rest of code assumes > 0 indices */

    pindex = THDRELEMPTR(pindices, int, 0);
    end = THDRELEMPTR(pindices, int, pindices->used);

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
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

void thdr_free(thdr_t *thdr)
{
    if (thdr->type == TA_ANY) {
        thdr_decr_obj_refs(thdr, 0, thdr->used);
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
    tcol_set_intrep(o, thdr);
    return o;
}

/* thdr must NOT be shared and must have enough slots */
/* ip may be NULL (only used for errors) */
TCL_RESULT thdr_put_objs(Tcl_Interp *ip, thdr_t *thdr, int first,
                         int nelems, Tcl_Obj * const elems[], int insert)
{
    int i, ival;
    int status;
    int new_used;

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
           ba_t ba, ba_mask;
           int off;

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
    case TA_WIDE: thdr_put_OBJCOPY(Tcl_WideInt, Tcl_GetWideIntFromObj); break;
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

    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_used <= thdr->usable);
    thdr->used = new_used;

    return TCL_OK;

convert_error:                  /* Interp should already contain errors */
    TA_ASSERT(thdr->type != TA_ANY); /* Else we may need to deal with ref counts */

    return TCL_ERROR;

}


/* Caller must have done all the checks in the TA_ASSERTS below ! */
void thdr_place_objs(
    Tcl_Interp *ip,
    thdr_t *thdr,               /* thdr_t to be modified - must NOT be shared */
    thdr_t *pindices,            /* Contains indices. If any >= thdr->used,
                                    all intermediate indices must also be
                                    present in pindices. Caller
                                    must have checked */
    int new_size,
    int nvalues,                /* # values in ovalues */
    Tcl_Obj * const *ovalues)   /* Values to be stored, must be type verified */
{
    int *pindex, *end;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pindices->type == TA_INT);
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
    
    pindex = THDRELEMPTR(pindices, int, 0);
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
        PLACEVALUES(Tcl_WideInt, Tcl_GetWideIntFromObj);
        break;
    case TA_DOUBLE:
        PLACEVALUES(double, Tcl_GetDoubleFromObj);
        break;
    case TA_ANY:
        thdr_place_ta_objs(thdr, pindices, ovalues, new_size);
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

void thdr_place_indices(Tcl_Interp *ip, thdr_t *thdr, thdr_t *psrc, thdr_t *pindices, int new_size)
{
    int *pindex, *end;
    int i;

    TA_ASSERT(! thdr_shared(thdr));
    TA_ASSERT(pindices->type == TA_INT);
    TA_ASSERT(new_size <= thdr->usable);
    TA_ASSERT(pindices->used <= psrc->used);
    TA_ASSERT(thdr->type == psrc->type);

    if (pindices->used == 0)
        return;          /* Nothing to change */

    thdr->sort_order = THDR_UNSORTED; /* TBD - optimize */
    
#define thdr_place_COPYINDICES(type) do {       \
        type *dst, *src;                        \
        dst = THDRELEMPTR(thdr, type, 0);       \
        src = THDRELEMPTR(psrc, type, 0);       \
        while (pindex < end) {                  \
            TA_ASSERT(*pindex < thdr->usable);  \
            dst[*pindex++] = *src++;            \
        }                                       \
    } while (0)
    
    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + pindices->used;
    
    switch (thdr->type) {
    case TA_BOOLEAN:
        {
            ba_t *dst = THDRELEMPTR(thdr, ba_t, 0);
            ba_t *src = THDRELEMPTR(psrc, ba_t, 0);
            for (i = 0; pindex < end; ++pindex, ++i) {
                TA_ASSERT(*pindex < thdr->usable);
                ba_put(dst, *pindex, ba_get(src, i));
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
            Tcl_Obj **src = THDRELEMPTR(psrc, Tcl_Obj *, 0);
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
    case TA_BYTE:
        thdr_place_COPYINDICES(unsigned char);
        break;
    default:
        ta_type_panic(thdr->type);
    }

    TA_ASSERT(new_size <= thdr->usable);
    thdr->used = new_size;
}

int thdr_required_size(int tatype, int count)
{
    int space;

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
    case TA_BYTE:
        space = count * sizeof(unsigned char);
        break;
    default:
        ta_type_panic(tatype);
    }

    return sizeof(thdr_t) + space;
}

thdr_t *thdr_realloc(Tcl_Interp *ip, thdr_t *oldP, int new_count)
{
    thdr_t *thdr;
    int sz;

    TA_ASSERT(oldP->nrefs < 2);
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

thdr_t * thdr_alloc(Tcl_Interp *ip, int tatype, int count)
{
    unsigned char nbits;
    int sz;
    thdr_t *thdr;

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
    case TA_BYTE: nbits = sizeof(unsigned char) * CHAR_BIT; break;
    default:
        ta_type_panic(tatype);
    }
    thdr->elem_bits = nbits;
    thdr->sort_order = THDR_UNSORTED;

    return thdr;
}

thdr_t * thdr_alloc_and_init(Tcl_Interp *ip, int tatype,
                           int nelems, Tcl_Obj * const elems[],
                           int init_size)
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

/* Deletes a range from a thdr_t. See asserts below for requirements */
void thdr_delete_range(thdr_t *thdr, int first, int count)
{
    int n;
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
        /*
         * We have to deal with reference counts here. For the objects
         * we are deleting we need to decrement the reference counts.
         */
        thdr_decr_obj_refs(thdr, first, count);
         
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
    int prev;
    int *pindex, *plimit;

    TA_ASSERT(pindices->type == TA_INT);

    /*
     * We have to be careful to delete from back to front so as to not
     * invalidate index positions when earlier ones are deleted
     */
    TA_ASSERT(pindices->sort_order == THDR_SORTED_ASCENDING || pindices->sort_order == THDR_SORTED_DESCENDING);
    
    /* TBD - this will be desperately slow. Fix */
    
    /* We always want to delete back to front. However the index array
     * may be presorted in any direction. So check and loop accordingly
     * Because duplicates might be present, we keep track of last deleted
     * index in variable prev.
     */
    if (pindices->used == 0)
        return;
    if (pindices->sort_order == THDR_SORTED_ASCENDING) {
        /* Sort order is ascending so iterate index array back to front */
        plimit = THDRELEMPTR(pindices, int, 0);
        pindex = plimit + pindices->used - 1;
        prev = *pindex + 1;     /* Dummy value for first iteration */
        for ( ; pindex >= plimit; --pindex) {
            if (*pindex != prev && *pindex >= 0 && *pindex < thdr->used)
                thdr_delete_range(thdr, *pindex, 1);
            prev = *pindex;
        }
    } else {
        /* Sort order is descending so iterate index array front to back */
        pindex = THDRELEMPTR(pindices, int, 0);
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


/* Copies partial content from one thdr_t to another. See asserts below
   for requirements */
void thdr_copy(thdr_t *pdst, int dst_first,
               thdr_t *psrc, int src_first, int count, int insert)
{
    int nbytes;
    void *s, *d;
    int new_used;
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
        return;

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

    case TA_ANY:
        /*
         * We have to deal with reference counts here. For the objects
         * we are copying (source) we need to increment the reference counts.
         * For objects in destination that we are overwriting, we need
         * to decrement reference counts.
         */

        thdr_incr_obj_refs(psrc, src_first, count); /* Do this first */
        if (! insert) {
            /*
             * Overwriting so decr refs of existing elements.
             * Note this call take care of the case where count exceeds
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

/* Copies partial content from one thdr_t to another in reverse.
   See asserts below for requirements */
void thdr_copy_reversed(thdr_t *pdst, int dst_first,
                       thdr_t *psrc, int src_first, int count)
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
        return;
    TA_ASSERT((dst_first + count) <= pdst->usable);

    if (dst_first < 0)
        dst_first = 0;
    else if (dst_first > pdst->used)
        dst_first = pdst->used;

    pdst->sort_order = THDR_UNSORTED; /* TBD - optimize */

#define COPYREVERSE(type_, pdst_, doff_, psrc_, soff_, count_)          \
    do {                                                                \
        type_ *src;                                                     \
        type_ *dst;                                                     \
        int    i = (count_);                                            \
        src = THDRELEMPTR((psrc_), type_ , (soff_));                  \
        dst  = THDRELEMPTR((pdst_), type_ , 0);                       \
        dst += (doff_ ) + i - 1;                                        \
        while (i--) {                                                   \
            /* Remember caller ensured no overlap between src & dst */  \
            *dst-- = *src++;                                            \
        }                                                               \
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

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone(Tcl_Interp *ip, thdr_t *psrc, int minsize)
{
    thdr_t *thdr;

    if (minsize == 0)
        minsize = psrc->usable;
    else if (minsize < psrc->used)
        minsize = psrc->used;

    /* TBD - optimize these two calls */
    thdr = thdr_alloc(ip, psrc->type, minsize);
    if (thdr) {
        thdr_copy(thdr, 0, psrc, 0, psrc->used, 0);
        thdr->sort_order = psrc->sort_order;
    }
    return thdr;
}

/* Note: nrefs of cloned array is 0 */
thdr_t *thdr_clone_reversed(Tcl_Interp *ip, thdr_t *psrc, int minsize)
{
    thdr_t *thdr;
    int orig_order;

    orig_order = psrc->sort_order;

    if (minsize == 0)
        minsize = psrc->usable;
    else if (minsize < psrc->used)
        minsize = psrc->used;

    /* TBD - optimize these two calls */
    thdr = thdr_alloc(ip, psrc->type, minsize);
    if (thdr) {
        thdr_copy_reversed(thdr, 0, psrc, 0, psrc->used);
        switch (orig_order) {
        case THDR_SORTED_ASCENDING: thdr->sort_order = THDR_SORTED_DESCENDING; break;
        case THDR_SORTED_DESCENDING: thdr->sort_order = THDR_SORTED_ASCENDING; break;
        case THDR_SORTED_ASCENDING_NOCASE: thdr->sort_order = THDR_SORTED_DESCENDING_NOCASE; break;
        case THDR_SORTED_DESCENDING_NOCASE: thdr->sort_order = THDR_SORTED_ASCENDING_NOCASE; break;
        }
    }
    return thdr;
}

thdr_t *thdr_range(Tcl_Interp *ip, thdr_t *psrc, int low, int count)
{
    thdr_t *thdr;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    thdr = thdr_alloc(ip, psrc->type, count);
    if (thdr) {
        thdr_copy(thdr, 0, psrc, low, count, 0);
        thdr->sort_order = psrc->sort_order;
    }
    return thdr;
}

Tcl_Obj *tcol_index(Tcl_Interp *ip, Tcl_Obj *tcol, int index)
{
    thdr_t *thdr;

    if (tcol_convert(ip, tcol) != TCL_OK)
        return NULL;
    thdr = tcol_thdr(tcol);
    if (index < 0 || index >= thdr->used) {
        ta_index_range_error(ip, index);
        return NULL;
    }
    return thdr_index(thdr, index);
}

Tcl_Obj *tcol_range(Tcl_Interp *ip, Tcl_Obj *osrc, int low, int count,
                     int fmt)
{
    int end;
    thdr_t *psrc;
    Tcl_Obj *o;

    TA_ASSERT(low >= 0);
    TA_ASSERT(count >= 0);

    psrc = tcol_thdr(osrc);

    if (fmt == TA_FORMAT_TARRAY) {
        thdr_t *thdr = thdr_range(ip, psrc, low, count);
        return thdr == NULL ? NULL : tcol_new(thdr);
    }

    end = low + count;
    if (end > psrc->used)
        end = psrc->used;

#define tcol_range_COPY(type_, objfn_)                                  \
    do {                                                                \
        type_ *p = THDRELEMPTR(psrc, type_, low);                \
        type_ *pend = THDRELEMPTR(psrc, type_, end);             \
        while (p < pend) {                                              \
            if (fmt == TA_FORMAT_DICT)                                  \
                Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low++));  \
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
                    Tcl_ListObjAppendElement(ip, o, Tcl_NewIntObj(low));
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
    default:
        ta_type_panic(psrc->type);
    }

    return o;
}


/*
 * Convert a TArray Tcl_Obj to one that is suitable for modifying.
 * The Tcl_Obj must NOT be shared.
 * There are three cases to consider:
 * (1) Even though tcol is unshared, the corresponding thdr_t might
 *     still be shared (pointed to from elsewhere). In this case
 *     also, we clone the thdr_t and store it as the new internal rep.
 * (2) If its thdr_t is unshared, we can modify in
 *     place, unless 
 * (3) thdr_t is too small in which case we have to reallocate it.
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
                                Tcl_Obj *tcol, int minsize, int prefsize)
{
    thdr_t *thdr;

    TA_ASSERT(tcol_affirm(tcol));
    TA_ASSERT(! Tcl_IsShared(tcol));

    thdr = tcol_thdr(tcol);
    if (prefsize == 0)
        prefsize = thdr->usable;
    if (minsize < thdr->used)
        minsize = thdr->used;
    if (minsize > prefsize)
        prefsize = minsize;

    if (thdr_shared(thdr)) {
        /* Case (1) */
        thdr = thdr_clone(ip, thdr, prefsize);
        if (thdr == NULL)
            return TCL_ERROR;   /* Note tcol is not changed */
        tcol_replace_intrep(tcol, thdr);
    } else if (thdr->usable < minsize) {
        /* Case (3). */
        if (tcol_grow_intrep(ip, tcol, prefsize) != TCL_OK)
            return TCL_ERROR;
    } else {
        /* Case (2) - just reuse, invalidate the string rep */
        Tcl_InvalidateStringRep(tcol);
    }

    return TCL_OK;
}


/* Returns a Tcl_Obj for a TArray slot. NOTE: WITHOUT its ref count incremented */
Tcl_Obj * thdr_index(thdr_t *thdr, int index)
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
    default:
        ta_type_panic(thdr->type);
        return NULL;
    }
}

struct thdr_minmax_mt_context {
    ta_value_t min_value;
    ta_value_t max_value;
    int min_index;
    int max_index;
    void *base;
    int   nelems;
    unsigned char type;
    char ignore_case;
};

#define MINMAXLOOP(pctx_, type_, minptr_, maxptr_)      \
    do {                                                \
        type_ *p, *pend;                            \
        type_ minval, maxval;                           \
        type_ *pmin, *pmax;                             \
        p = (type_ *) (pctx_)->base;                \
        pend = p + (pctx_)->nelems;                 \
        minval = maxval = *p;                       \
        pmin = pmax = p;                            \
        while (p < pend) {                          \
            TA_ASSERT(maxval >= minval);                \
            if (*p < minval) {                          \
                minval = *p; \
                pmin = p;                           \
            } else if (*p > maxval) {               \
                maxval = *p; \
                pmax = p;                           \
            } \
            ++p;                                    \
        }                                               \
        (pctx_)->min_index = pmin - (type_ *) (pctx_)->base;     \
        *(minptr_) = minval;                             \
        (pctx_)->max_index = pmax - (type_ *) (pctx_)->base;     \
        *(maxptr_) = maxval;                             \
    } while (0)

static void thdr_minmax_mt_worker (struct thdr_minmax_mt_context *pctx)
{
    TA_ASSERT(pctx->nelems > 0);
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
            pctx->min_index = pmin - (Tcl_Obj **)pctx->base;
            pctx->min_value.oval = minval;
            pctx->max_index = pmax - (Tcl_Obj **)pctx->base;
            pctx->max_value.oval = maxval;
        }
        break;
    case TA_BOOLEAN:
        /* Not handled here */
        ta_type_panic(pctx->type);
        break;
    }
    pctx->min_value.type = pctx->type;
    pctx->max_value.type = pctx->type;
}


/* Returns the minimum and maximum */
static void thdr_minmax(thdr_t *thdr, int start, int count, int ignore_case, int *min_indexP, int *max_indexP, ta_value_t *minP, ta_value_t *maxP)
{
    struct thdr_minmax_mt_context mt_context[2];
    int elem_size;
    int min_grp, max_grp;

    TA_ASSERT(start >= 0 && start < thdr->used);
    TA_ASSERT(count > 0);
    TA_ASSERT((start + count) <= thdr->used);

    if (thdr->type == TA_BOOLEAN) {
        int min_index, max_index;
        ba_t *baP = THDRELEMPTR(thdr, ba_t, 0);
        min_index = ba_find(baP, 0, start, thdr->used);
        max_index = ba_find(baP, 1, start, thdr->used);
        if (min_index < 0)
            min_index = 0;      /* No 0's so first element (1) is the min */
        if (max_index < 0)
            max_index = 0;      /* No 0's so first element (1) is the min */
        *min_indexP = min_index;
        *max_indexP = max_index;
        minP->bval = ba_get(baP, min_index);
        maxP->bval = ba_get(baP, max_index);
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

        if (thdr->type == TA_ANY)
            thdr_ensure_obj_strings(thdr); /* Prevent races if string rep does not exist */

        mt_context[1].type = thdr->type;
        mt_context[1].base = (elem_size*mt_context[0].nelems) + (char*)mt_context[0].base;
        grp = ta_mt_group_create();
        TA_ASSERT(grp != NULL); /* TBD */
        /* TBD - check return code */ ta_mt_group_async_f(grp, &mt_context[1], thdr_minmax_mt_worker);
        thdr_minmax_mt_worker(&mt_context[0]);
        ta_mt_group_wait(grp, TA_MT_TIME_FOREVER);
        ta_mt_group_release(grp);
        /* We will need to see if which thread had smaller/greater value */
        min_grp = (ta_value_compare(&mt_context[0].min_value, &mt_context[1].min_value, ignore_case) <= 0);
        max_grp = (ta_value_compare(&mt_context[0].max_value, &mt_context[1].max_value, ignore_case) >= 0);
    }

#else /* not TA_MT_ENABLE */

    mt_context[0].nelems = count;
    thdr_minmax_mt_worker(&mt_context[0]);
    min_grp = 0;
    max_grp = 0;

#endif

    *minP = mt_context[min_grp].min_value;
    if (min_grp == 0)
        *min_indexP = start + mt_context[0].min_index;
    else
        *min_indexP = start + mt_context[0].nelems + mt_context[1].min_index;

    *maxP = mt_context[max_grp].max_value;
    if (max_grp == 0)
        *max_indexP = start + mt_context[0].max_index;
    else
        *max_indexP = start + mt_context[0].nelems + mt_context[1].max_index;

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
 * This facility to return a single int or a index list should only
 * be used by commands where it does not matter whether {1} is treated
 * as a list or an int, for example the fill command, and the distinction
 * is just an efficiency issue. Commands should
 * not use this to pick whether a single index or a list was specified
 * if it impacts their semantics.
 */
int ta_obj_to_indices(Tcl_Interp *ip, Tcl_Obj *o,
                      int want_sorted,
                      int end,        /* Value to use for "end",
                                         ignored if pindex == NULL */
                      thdr_t **thdrP, /* Cannot be NULL */
                      int *pindex)    /* Can be NULL */
{
    thdr_t *thdr;
    Tcl_Obj **elems;
    int       n;
    TCL_RESULT status;

    /*
     * For efficiencies sake, we need to avoid shimmering. So we first
     * check for specific types and default to a list otherwise.
     */
    if (tcol_affirm(o)) {
        if (tcol_type(o) == TA_INT) {
            thdr = tcol_thdr(o);
            if (want_sorted && thdr->sort_order == THDR_UNSORTED) {
                thdr = thdr_clone(ip, thdr, thdr->used);
                if (thdr == NULL)
                    return TA_INDEX_TYPE_ERROR;
                qsort(THDRELEMPTR(thdr, int, 0), thdr->used, sizeof(int), intcmp);
                thdr->sort_order = THDR_SORTED_ASCENDING;
            }
            thdr->nrefs++;
            *thdrP = thdr;
            return TA_INDEX_TYPE_THDR;
        } else {
            /* TBD - write conversion from other type tarrays */
            ta_indices_error(ip, o);
            return TA_INDEX_TYPE_ERROR;
        }
    }

    /* TBD - should we not try to convert to a tcol ? Above only checks if already a tcol */

    /* If pindex is NULL, caller never wants to be treated as a single index */
    if (pindex != NULL) {
        /* To prevent shimmering, first check known to be a list */
        if (o->typePtr != g_tcl_list_type_ptr && pindex != NULL) {
            status = ta_convert_index(NULL, o, &n, end, INT_MIN, INT_MAX);
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
            if (ta_convert_index(NULL, elems[0], &n, end, INT_MIN, INT_MAX) == TCL_OK) {
                *pindex = n;
                return TA_INDEX_TYPE_INT;
            }
        }
    }

    thdr = thdr_alloc_and_init(ip, TA_INT, n, elems, 0);
    if (thdr) {
        if (want_sorted) {
            qsort(THDRELEMPTR(thdr, int, 0), thdr->used, sizeof(int), intcmp);
            thdr->sort_order = THDR_SORTED_ASCENDING;
        }
        thdr->nrefs++;
        *thdrP = thdr;
        return TA_INDEX_TYPE_THDR;
    } else
        return TA_INDEX_TYPE_ERROR;
}

/* Returns a newly allocated thdr_t (with ref count 0) containing the
   values from the specified indices */
Tcl_Obj *tcol_get(Tcl_Interp *ip, Tcl_Obj *osrc, thdr_t *pindices, int fmt)
{
    thdr_t *psrc;
    thdr_t *thdr;
    int count, index, bound;
    int *pindex, *end;
    Tcl_Obj *tcol;
    void *srcbase, *thdrbase;

    if (tcol_convert(ip, osrc) != TCL_OK)
        return NULL;

    TA_ASSERT(pindices->type == TA_INT);
    count = pindices->used;

    psrc = tcol_thdr(osrc);
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
                *tpobjs++ = fromP[index];                               \
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
                Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index)); \
                Tcl_ListObjAppendElement(ip, tcol,                      \
                                         objfn_(fromP[index]));         \
            }                                                           \
            break;                                                      \
        }                                                               \
    } while (0)

    pindex = THDRELEMPTR(pindices, int, 0);
    end = pindex + count;
    srcbase = THDRELEMPTR(psrc, unsigned char, 0);
    bound = psrc->used;
    switch (psrc->type) {
    case TA_BOOLEAN:
        {
            ba_t *srcbaP = srcbase;
            ba_t *baP = thdrbase;
            int i;
            if (fmt == TA_FORMAT_TARRAY) {
                for (i = 0; pindex < end; ++i, ++pindex) {
                    index = *pindex; 
                    if (index < 0 || index >= bound)
                        goto index_error;
                    ba_put(baP, i, ba_get(srcbaP, index));
                }
                thdr->used = count;             \
            } else {
                for (i = 0; pindex < end; ++i, ++pindex) {
                    index = *pindex; 
                    if (index < 0 || index >= bound)
                        goto index_error;
                    if (fmt == TA_FORMAT_DICT)
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol,
                                             Tcl_NewIntObj(ba_get(srcbaP, index)));
                }
            }
        }
        break;
    case TA_UINT:
        tcol_get_COPY(unsigned int, Tcl_NewWideIntObj);
        break;
    case TA_INT:
        tcol_get_COPY(int, Tcl_NewIntObj);
        break;
    case TA_WIDE:
        tcol_get_COPY(Tcl_WideInt, Tcl_NewWideIntObj);
        break;
    case TA_DOUBLE:
        tcol_get_COPY(double, Tcl_NewDoubleObj);
        break;
    case TA_BYTE:
        tcol_get_COPY(unsigned char, Tcl_NewIntObj);
        break;
    case TA_ANY:
        /* Cannot use macro here because of ref counts etc. */
        {
            Tcl_Obj **srcobjs = srcbase;
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
                        Tcl_ListObjAppendElement(ip, tcol, Tcl_NewIntObj(index));
                    Tcl_ListObjAppendElement(ip, tcol, srcobjs[index]);
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

/* See asserts for conditions */
TCL_RESULT tcol_delete(Tcl_Interp *ip, Tcl_Obj *tcol,
                        Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int low, count;
    int status;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    status = tcol_make_modifiable(ip, tcol, tcol_occupancy(tcol), 0);
    if (status == TCL_OK) {
        thdr_t *thdr = tcol_thdr(tcol);
        if (indexb) {
            status = ta_fix_range_bounds(ip, thdr->used, indexa,
                                             indexb, &low, &count);
            if (status == TCL_OK)
                thdr_delete_range(thdr, low, count);
        } else {
            /* Not a range, either a list or single index */
            thdr_t *pindices;
            /* Note status is TCL_OK at this point */
            switch (ta_obj_to_indices(ip, indexa, 1, thdr->used-1,
                                      &pindices, &low)) {
            case TA_INDEX_TYPE_ERROR:
                status = TCL_ERROR;
                break;
            case TA_INDEX_TYPE_INT:
                if (low >= 0) 
                    thdr_delete_range(thdr, low, 1);
                break;
            case TA_INDEX_TYPE_THDR:
                thdr_delete_indices(thdr, pindices);
                thdr_decr_refs(pindices);
                break;
            }
        }
    }

    return status;
}

TCL_RESULT tcol_insert_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                           Tcl_Obj *opos, Tcl_Obj *ocount)
{
    int status;
    TA_ASSERT(! Tcl_IsShared(tcol));

    if (ocount == NULL) {
        /* Values may be given as a column or a list */
        if ((status = tcol_convert(NULL, ovalue)) == TCL_OK)
            status = tcol_copy_thdr(ip, tcol, tcol_thdr(ovalue), opos, 1);
        else
            status = tcol_put_objs(ip, tcol, ovalue, opos, 1);
    } else {
        int pos, count, used;
        if ((status = ta_get_int_from_obj(ip, ocount, &count)) == TCL_OK &&
            (status = tcol_convert(ip, tcol)) == TCL_OK) {
            used = tcol_occupancy(tcol);
            if (count > 0) {
                if ((status = tcol_make_modifiable(ip, tcol, count+used, 0)) == TCL_OK &&
                    (status = ta_convert_index(ip, opos, &pos, used,
                                           0, used)) == TCL_OK) {
                    ta_value_t tav;
                    if ((status = ta_value_from_obj(ip, ovalue,
                                                    tcol_type(tcol), &tav)) == TCL_OK)
                        thdr_fill_range(ip, tcol_thdr(tcol), &tav, pos, count, 1);
                }
            } else if (count < 0) {
                status = ta_bad_count_error(ip, count);
            } else {
                status = TCL_OK; /* count == 0, nothing to do */
            }
        }
    }
    return status;
}


TCL_RESULT tcol_fill_obj(Tcl_Interp *ip, Tcl_Obj *tcol, Tcl_Obj *ovalue,
                         Tcl_Obj *indexa, Tcl_Obj *indexb)
{
    int low, count, nelems;
    int status;
    ta_value_t value;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if ((status = ta_value_from_obj(ip, ovalue,
                                     tcol_type(tcol), &value)) != TCL_OK)
        return status;

    nelems = tcol_occupancy(tcol);
    if (indexb) {
        status = ta_fix_range_bounds(ip, nelems, indexa, indexb, &low, &count);
        if (status == TCL_OK && count != 0) {
            status = tcol_make_modifiable(ip, tcol, low+count, 0);
            if (status == TCL_OK)
                thdr_fill_range(ip, tcol_thdr(tcol), &value, low, count, 0);
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
                    thdr_fill_range(ip, tcol_thdr(tcol), &value, low, 1, 0);
            }
            break;
        case TA_INDEX_TYPE_THDR:
            status = thdr_verify_indices_in_range(ip, tcol_occupancy(tcol), pindices, &count);
            if (status == TCL_OK && count > 0) {
                status = tcol_make_modifiable(ip, tcol, count, count); // TBD - count + extra?
                if (status == TCL_OK)
                    thdr_fill_indices(ip, tcol_thdr(tcol), &value, pindices, count);
            }
            thdr_decr_refs(pindices);
            break;
        }
    }

    return status;
}

TCL_RESULT tcol_reverse(Tcl_Interp *ip, Tcl_Obj *tcol)
{
    thdr_t *thdr;
    int status;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    if (thdr_shared(tcol_thdr(tcol))) {
        thdr = thdr_clone_reversed(ip, tcol_thdr(tcol), 0);
        if (thdr == NULL)
            return TCL_ERROR;
        tcol_replace_intrep(tcol, thdr);
    } else {
        thdr_reverse(tcol_thdr(tcol));
        Tcl_InvalidateStringRep(tcol);
    }
    return TCL_OK;
}

int ta_obj_compare(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;
    int comparison;

    /* TBD - maybe check first letter before calling ? But be careful of case-insensitivity*/
#if 0
    int alen, blen, len;

    a = Tcl_GetStringFromObj(oaP, &alen);
    b = Tcl_GetStringFromObj(obP, &blen);
    
    // This is much slower than the strcmp method below but might be more
    // "correct". On the other hand the strcmp method below is both faster
    // and Tcl compatible with lsort and lsearch
    alen = Tcl_NumUtfChars(a, alen); /* Num bytes -> num chars */
    blen = Tcl_NumUtfChars(b, blen); /* Num bytes -> num chars */

    len = alen < blen ? alen : blen; /* len is the shorter length */
    comparison = (ignorecase ? Tcl_UtfNcasecmp : Tcl_UtfNcmp)(a, b, len);
    if (comparison == 0) {
        comparison = alen-blen;
    }

#else
    a = oaP->bytes;
    if (a == NULL)
        a = Tcl_GetString(oaP);
    b = obP->bytes;
    if (b == NULL)
        b = Tcl_GetString(obP);

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


int ta_obj_equal(Tcl_Obj *oaP, Tcl_Obj *obP, int ignorecase)
{
    char *a, *b;

    /* TBD - maybe check first letter before calling ? But be careful of case-insensitivity*/
#if 0
    int alen, blen, comparison;

    a = Tcl_GetStringFromObj(oaP, &alen);
    b = Tcl_GetStringFromObj(obP, &blen);

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
    a = oaP->bytes;
    if (a == NULL)
        a = Tcl_GetString(oaP);
    b = obP->bytes;
    if (b == NULL)
        b = Tcl_GetString(obP);

    TA_ASSERT(a && b);

    if (ignorecase) {
        return ! _stricmp(a, b);
    } else {
        if (oaP->length != obP->length)
            return 0;
        if (*a != *b)
            return 0;
        else
            return ! memcmp(a, b, oaP->length);
    }
#endif
}

TCL_RESULT tcol_copy_thdr(Tcl_Interp *ip, Tcl_Obj *tcol, thdr_t *psrc,
                          Tcl_Obj *ofirst, /* NULL -> end */
                          int insert)
{
    int first, status, cur_used;

    TA_ASSERT(! Tcl_IsShared(tcol));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;
    if (tcol_type(tcol) != psrc->type)
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    cur_used = tcol_occupancy(tcol);
    first = cur_used; /* By default, append */
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &first, first, 0, first);
    if (status == TCL_OK && psrc->used) {
        status = tcol_make_modifiable(ip, tcol,
                                      psrc->used + (insert ? cur_used : first),
                                      0);
        if (status == TCL_OK)
            thdr_copy(tcol_thdr(tcol), first, psrc, 0, psrc->used, insert); 
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
    int nvalues;
    int n, cur_used;

    TA_ASSERT(! Tcl_IsShared(tcol));

    status = Tcl_ListObjGetElements(ip, ovalues, &nvalues, &values);
    if (status != TCL_OK)
        return status;

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    /* Get the limits of the range to set */

    cur_used = tcol_occupancy(tcol);
    n = cur_used;
    if (ofirst)
        status = ta_convert_index(ip, ofirst, &n, n, 0, n);
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
    int new_size;
    int status;
    thdr_t *pindices;
    Tcl_Obj **ovalues;
    int nvalues;

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
    int new_size;
    int status;
    thdr_t *pindices, *psrc;

    TA_ASSERT(! Tcl_IsShared(tcol));
    TA_ASSERT(tcol_affirm(osrc));

    if ((status = tcol_convert(ip, tcol)) != TCL_OK)
        return status;

    psrc = tcol_thdr(osrc);
    if (psrc->type != tcol_type(tcol))
        return ta_mismatched_types_error(ip, tcol_type(tcol), psrc->type);

    if (ta_obj_to_indices(ip, oindices, 0, 0, &pindices, NULL) != TA_INDEX_TYPE_THDR)
        return TCL_ERROR;

    status = TCL_OK;
    if (pindices->used > 0) {
        if (pindices->used > psrc->used)
            status = ta_indices_count_error(ip, pindices->used, psrc->used);
        else {
            status = thdr_verify_indices_in_range(ip, tcol_occupancy(tcol), pindices, &new_size);
            if (status == TCL_OK) {
                status = tcol_make_modifiable(ip, tcol, new_size, new_size); // TBD - count + extra?
                thdr_place_indices(ip, tcol_thdr(tcol), psrc, pindices, new_size);
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
        if ((status = Tcl_GetIndexFromObj(ip, objv[1], tcol_get_options,
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
        int low, count;
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
    int i, opt, start, count;
    int ignore_case = 0, want_indices = 0;
    TCL_RESULT status;
    static const char *tcol_minmax_switches[] = {
        "-range", "-indices", "-nocase", NULL
    };
    enum tcol_minmax_switches_e {
        TA_MINMAX_OPT_RANGE, TA_MINMAX_OPT_INDICES, TA_MINMAX_OPT_NOCASE
    };
    int min_index, max_index;
    ta_value_t minval, maxval;
    thdr_t *thdr;
    Tcl_Obj *objs[2];

    if (objc < 2) {
	Tcl_WrongNumArgs(ip, 1, objv, "?options? column");
	return TCL_ERROR;
    }
    
    if (tcol_convert(ip, objv[objc-1]) != TCL_OK)
        return TCL_ERROR;
    thdr = tcol_thdr(objv[objc-1]);
    start = 0;
    count = thdr->used;
    for (i = 1; i < objc-1; ++i) {
	status = Tcl_GetIndexFromObj(ip, objv[i], tcol_minmax_switches, "option", 0, &opt);
        if (status != TCL_OK)
            return status;

        switch ((enum tcol_minmax_switches_e) opt) {
        case TA_MINMAX_OPT_RANGE:
            if (i > objc-3)
                return ta_missing_arg_error(ip, "-range");
            ++i;
            status = ta_parse_range_option_value(ip, count, objv[i], &start, &count);
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

    if (count <= 0 ||
        start > thdr->used ||
        (start+count) > thdr->used) {
        return ta_invalid_range_error(ip, NULL);
    }

    thdr_minmax(thdr, start, count, ignore_case,
                &min_index, &max_index, &minval, &maxval);

    if (want_indices) {
        objs[0] = Tcl_NewIntObj(min_index);
        objs[1] = Tcl_NewIntObj(max_index);
    } else {
        objs[0] = ta_value_to_obj(&minval);
        objs[1] = ta_value_to_obj(&maxval);
    }
    Tcl_SetObjResult(ip, Tcl_NewListObj(2, objs));
    return TCL_OK;
}
