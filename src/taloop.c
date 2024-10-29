/*
 * Copyright (c) 2024, Ashok P. Nadkarni
 * All rights reserved.
 *
 * See the file license.terms for license
 */

#include "tarray.h"

/* 
 * Various commands that make use of the looping infrastructure. Not all
 * are implemented yet.
 */
enum ta_loop_op_e {
    TA_LOOP_LOOP,
    TA_LOOP_MAP,
};

struct ta_loop_state {
    /*
     * If looping over a column, holds its thdr.
     * NULL if looping over a table or list
     */
    thdr_t *thdr;           /* The thdr we are looping over */
    /*
     * If looping over a column, holds its span (which may be NULL)
     * NULL if looping over a table or list
     */
    span_t *span;           /* The span within the thdr. NULL if entire
                                   thdr is to be iterated */ 
    /*
     * Holds the table or list Tcl_Obj if looping over a list.
     * NULL if looping over column
     */
    Tcl_Obj *ocoll;
    Tcl_Obj *ovar;          /* Name of iteration variable */
    Tcl_Obj *oindexvar;     /* Name of iteration index variable, maybe NULL */
    Tcl_Obj *oscript;       /* Script body */
    Tcl_Obj *ocmdname;      /* Needed for error reporting */
    Tcl_Size count;         /* Count of items   */
    Tcl_Size next;          /* Index of next item to iterate */
    enum ta_loop_op_e cmd;     /* TA_LOOP_COLITER, TA_LOOP_TABMAP etc. */
    enum ta_collection_type_e colltype; /* Table, column or list operand */
};

/* Not all these are implemented yet */
#define TA_LOOP_COLITER 1
#define TA_MAP_CMD  1

static TCL_RESULT ta_loop_assign_vars(Tcl_Interp *ip, 
                                      struct ta_loop_state *pstate)
{
    Tcl_Size tindex;
    Tcl_Obj *val, *varval;
    
    tindex = pstate->next;
    if (tindex == pstate->count)
        return TCL_BREAK;

    pstate->next += 1;
    
    switch (pstate->colltype) {
    case TA_COLL_COLUMN:
        if (pstate->span) {
            TA_ASSERT((tindex+pstate->span->first) < pstate->thdr->used);
            val = thdr_index(pstate->thdr, tindex + pstate->span->first);
        } else {
            TA_ASSERT(tindex < pstate->thdr->used);
            val = thdr_index(pstate->thdr, tindex);
        }
        break;
    case TA_COLL_TABLE:
        /* TBD - could be made more efficient */
        val = table_index(ip, pstate->ocoll, tindex);
        break;
    case TA_COLL_LIST:
        TA_NOFAIL(Tcl_ListObjIndex(ip, pstate->ocoll, tindex, &val), TCL_OK);
        break;
    default:
        Tcl_Panic("Unknown collection type");
    }

    if (val == NULL)
        return TCL_BREAK; /* Over? Should not really happen */

    /* val may or may not have references from elsewhere. On non-error
       cases, it gets assigned to the variable and we're done with it.
       On errors on the other hand, we need to free it if not referenced
       from elsewhere. Easiest is to just incr and decr its ref count.
       This will free it if unreferenced. Note if the ObjSetVar was
       successful, it would be holding an additional reference and
       val would not be actually freed.
    */
    Tcl_IncrRefCount(val);
    varval = Tcl_ObjSetVar2(ip, pstate->ovar, NULL, val, TCL_LEAVE_ERR_MSG);
    Tcl_DecrRefCount(val);

    if (varval == NULL) {
        Tcl_AppendObjToErrorInfo(ip, Tcl_ObjPrintf(
			"\n    (setting %s loop variable \"%s\")",
                        Tcl_GetString(pstate->ocmdname),
			Tcl_GetString(pstate->ovar)));
        return TCL_ERROR;
    }

    if (pstate->oindexvar) {
        Tcl_Obj *oindex = Tcl_NewWideIntObj(tindex);
        varval = Tcl_ObjSetVar2(ip, pstate->oindexvar, NULL, oindex, TCL_LEAVE_ERR_MSG);
        if (varval == NULL) {
            Tcl_DecrRefCount(oindex);
            Tcl_AppendObjToErrorInfo(ip, Tcl_ObjPrintf(
                                         "\n    (setting %s loop variable \"%s\")",
                                         Tcl_GetString(pstate->ocmdname),
                                         Tcl_GetString(pstate->oindexvar)));
            return TCL_ERROR;
        }
    }
    
    return TCL_OK;
}

static void ta_loop_cleanup (Tcl_Interp *ip, struct ta_loop_state *pstate)
{
    TA_ASSERT(pstate);
    if (pstate->thdr)
        thdr_decr_refs(pstate->thdr);
    if (pstate->span)
        span_decr_refs(pstate->span);
    if (pstate->ocoll)
        Tcl_DecrRefCount(pstate->ocoll);
    if (pstate->ovar)
        Tcl_DecrRefCount(pstate->ovar);
    if (pstate->oindexvar)
        Tcl_DecrRefCount(pstate->oindexvar);
    if (pstate->oscript)
        Tcl_DecrRefCount(pstate->oscript);
    if (pstate->ocmdname)
        Tcl_DecrRefCount(pstate->ocmdname);
    TA_FREEMEM(pstate);
}

static TCL_RESULT ta_loop_step(ClientData data[], Tcl_Interp *ip, int status)
{
    struct ta_loop_state *pstate = data[0];

    /*
     * status is passed in as the Tcl result code from execution of the
     * prior iteration
     */
    switch (status) {
    case TCL_CONTINUE:
	status = TCL_OK;
	break;
    case TCL_OK:
#if 0
	if (statePtr->resultList != NULL) {
	    Tcl_ListObjAppendElement(interp, statePtr->resultList,
		    Tcl_GetObjResult(interp));
	}
#endif
	break;
    case TCL_BREAK:
	status = TCL_OK;
	goto finish;
    case TCL_ERROR:
	Tcl_AppendObjToErrorInfo(ip, Tcl_ObjPrintf(
		"\n    (\"%s\" body line %d)",
                Tcl_GetString(pstate->ocmdname),
		Tcl_GetErrorLine(ip)));
    default:
	goto done;
    }

    /* On to the next iteration */
    
    if (pstate->next < pstate->count) {
        status = ta_loop_assign_vars(ip, pstate);
        if (status != TCL_OK)
            goto done;

        Tcl_NRAddCallback(ip, ta_loop_step, pstate, NULL, NULL, NULL);
        return Tcl_NREvalObj(ip, pstate->oscript, 0);
    }

    /*
     * We're done. Tidy up our work space and finish off.
     */

  finish:
    if (pstate->cmd == TA_LOOP_COLITER)
        Tcl_ResetResult(ip);
    else {
#if 0
	Tcl_SetObjResult(interp, statePtr->resultList);
	statePtr->resultList = NULL;	/* Don't clean it up */
#endif
    }

  done:
    ta_loop_cleanup(ip, pstate);
    return status;
}


/* Following the Tcl implementation of foreach/map, we have a common
 * routine implementing column/table loop and map (eventually)
 */
static TCL_RESULT
ta_looper(Tcl_Interp *ip,
          enum ta_loop_op_e cmd,
          int objc,
          Tcl_Obj *const objv[])
{
    struct ta_loop_state *pstate;
    thdr_t *thdr;
    span_t *span;
    Tcl_Size count;
    Tcl_Obj *collObj;
    TCL_RESULT status;
    enum ta_collection_type_e colltype;
    
    TA_ASSERT(cmd == TA_LOOP_LOOP); /* Only handle this currently */

    if (objc != 4 && objc != 5) {
        Tcl_WrongNumArgs(ip, 1, objv, "?indexvar? var tarray script");
        return TCL_ERROR;
    }

    collObj = objc == 4 ? objv[2] : objv[3];
    colltype = ta_collection_type(collObj);
    switch (colltype) {
    case TA_COLL_COLUMN:
        TA_ASSERT(tcol_affirm(collObj)); /* Because of above call */
        thdr = OBJTHDR(collObj);
        span = OBJTHDRSPAN(collObj);
        count = span ? span->count : thdr->used;
        collObj = NULL;
        break;
    case TA_COLL_TABLE:
        thdr = NULL;
        span = NULL;
        count = table_length(collObj);
        break;
    case TA_COLL_LIST:
        thdr = NULL;
        span = NULL;
        TA_NOFAIL(Tcl_ListObjLength(NULL, collObj, &count), TCL_OK);
        break;
    case TA_COLL_NONE: /* FALLTHRU */
    default:
        Tcl_SetResult(ip, "Operand is not a column, table or list.", TCL_STATIC);
        return TCL_ERROR;
    }

    if (count == 0) {
        /* Empty. Nothing to do */
        return TCL_OK;
    }
    
    pstate = TA_ALLOCMEM(sizeof(*pstate));

    /* 
     * Now fill in the pstate. We increase ref counts to protect
     * against shimmering or deallocation of intreps during execution
     * of scripts. This may not be necessary for all fields but
     * easier to be safe.
     */
    if (thdr)
        thdr_incr_refs(thdr);
    pstate->thdr = thdr;
    if (span)
        span_incr_refs(span);
    pstate->span = span;
    if (collObj) {
        /* Instead of just incr'ing the ref for the collection. Dup it
           which is not too expensive for lists and tables. That way
           the intrep will never shimmer
        */
        collObj = Tcl_DuplicateObj(collObj);
        Tcl_IncrRefCount(collObj);
    }
    pstate->ocoll = collObj;
    pstate->ocmdname = objv[0];
    Tcl_IncrRefCount(pstate->ocmdname);
    if (objc == 4) {
        pstate->oindexvar = NULL;
        pstate->ovar = objv[1];
    } else {
        pstate->oindexvar = objv[1];
        Tcl_IncrRefCount(pstate->oindexvar);
        pstate->ovar = objv[2];
    }
    Tcl_IncrRefCount(pstate->ovar);
    pstate->oscript = objv[objc-1];
    Tcl_IncrRefCount(pstate->oscript);
    pstate->count = count;
    pstate->next = 0;
    pstate->cmd = cmd;
    pstate->colltype = colltype;

    /* All ready to begin loop. Assign the variable and begin loop. */
    status = ta_loop_assign_vars(ip, pstate);
    if (status == TCL_OK) {
        Tcl_NRAddCallback(ip, ta_loop_step, pstate, NULL, NULL, NULL);
        status = Tcl_NREvalObj(ip, pstate->oscript, 0);
    } else {
        ta_loop_cleanup(ip, pstate);
    }

    return status;
}

TCL_RESULT ta_loop_cmd(ClientData dummy, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    return Tcl_NRCallObjProc(ip, ta_loop_nr_cmd, dummy, objc, objv);
}

TCL_RESULT ta_loop_nr_cmd(ClientData dummy, Tcl_Interp *ip, int objc, Tcl_Obj *const objv[])
{
    return ta_looper(ip, TA_LOOP_LOOP, objc, objv);
}
