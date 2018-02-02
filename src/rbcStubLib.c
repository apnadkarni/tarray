/*
 * rbcStubLib.c --
 *
 *  Stub object that will be statically linked into extensions that wish
 *  to access the RBC API.
 *
 * Copyright (c) 2018 Ashok P. Nadkarni
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id$
 */

#ifndef USE_TCL_STUBS
#   define USE_TCL_STUBS
#endif

#include "tcl.h"
#include "rbcDecls.h"

const RbcStubs *rbcStubsPtr;

/*
 *----------------------------------------------------------------------
 *
 * Rbc_InitStubs --
 *
 *  Checks that the correct version of RBC is loaded and that it
 *  supports stubs. It then initialises the stub table pointers.
 *
 * Results:
 *  The actual version of RBC that satisfies the request, or
 *  NULL to indicate that an error occurred.
 *
 * Side effects:
 *  Sets the stub table pointers.
 *
 *----------------------------------------------------------------------
 */

const char *
Rbc_InitStubs(
	Tcl_Interp *interp,
	const char *version,
	int exact
) {
    const char *result;
    void *data;

    result = Tcl_PkgRequireEx(interp, "rbc", (CONST84 char *) version, exact, &data);
    if (!result || !data) {
        return NULL;
    }

    rbcStubsPtr = (const RbcStubs *) data;
    return result;
}
