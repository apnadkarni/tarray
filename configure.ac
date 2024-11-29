#!/bin/bash -norc
dnl	This file is an input file used by the GNU "autoconf" program to
dnl	generate the file "configure", which is run during installation
dnl	to configure the system for the local environment.

#-----------------------------------------------------------------------
# Based on sample configure.ac for Tcl Extensions.  The only places you
# should need to modify this file are marked by the string __CHANGE__
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# __CHANGE__
# Set your package name and version numbers here.
#
# This initializes the environment with PACKAGE_NAME and PACKAGE_VERSION
# set as provided.  These will also be added as -D defs in your Makefile
# so you can encode the package version directly into the source files.
# This will also define a special symbol for Windows (BUILD_<PACKAGE_NAME>
# so that we create the export library with the dll.
#-----------------------------------------------------------------------

# Leave a space after the comma for release.cmd to parse
AC_INIT([tarray], [2.0a0])

#--------------------------------------------------------------------
# Call TEA_INIT as the first TEA_ macro to set up initial vars.
# This will define a ${TEA_PLATFORM} variable == "unix" or "windows"
# as well as PKG_LIB_FILE and PKG_STUB_LIB_FILE.
#--------------------------------------------------------------------

TEA_INIT(3.13)

AC_CONFIG_AUX_DIR(tclconfig)

#--------------------------------------------------------------------
# Load the tclConfig.sh file
#--------------------------------------------------------------------

TEA_PATH_TCLCONFIG
TEA_LOAD_TCLCONFIG

#--------------------------------------------------------------------
# Load the tkConfig.sh file if necessary (Tk extension)
#--------------------------------------------------------------------

#TEA_PATH_TKCONFIG
#TEA_LOAD_TKCONFIG

#-----------------------------------------------------------------------
# Handle the --prefix=... option by defaulting to what Tcl gave.
# Must be called after TEA_LOAD_TCLCONFIG and before TEA_SETUP_COMPILER.
#-----------------------------------------------------------------------

TEA_PREFIX

#-----------------------------------------------------------------------
# Standard compiler checks.
# This sets up CC by using the CC env var, or looks for gcc otherwise.
# This also calls AC_PROG_CC and a few others to create the basic setup
# necessary to compile executables.
#-----------------------------------------------------------------------

TEA_SETUP_COMPILER

#-----------------------------------------------------------------------
# __CHANGE__
# Specify the C source files to compile in TEA_ADD_SOURCES,
# public headers that need to be installed in TEA_ADD_HEADERS,
# stub library C source files to compile in TEA_ADD_STUB_SOURCES,
# and runtime Tcl library files in TEA_ADD_TCL_SOURCES.
# This defines PKG(_STUB)_SOURCES, PKG(_STUB)_OBJECTS, PKG_HEADERS
# and PKG_TCL_SOURCES.
#-----------------------------------------------------------------------

TEA_ADD_SOURCES([src/bitarray.c \
                     src/column.c \
                     src/errors.c \
                     src/mt.c \
                     src/parseargs.c \
                     src/pcg_basic.c \
                     src/rbcStubLib.c \
                     src/table.c \
                     src/tafold.c \
                     src/tajoin.c \
                     src/taloop.c \
                     src/tamath.c \
                     src/tarandom.c \
                     src/tarbc.c \
                     src/tarray.c \
                     src/tasearch.c \
                     src/tasort.c \
                     src/tastring.c \
                     src/timsort.c \
                     src/timsort_r.c\
                     src/unsupported.c])
TEA_ADD_HEADERS([])
TEA_ADD_INCLUDES([-I${srcdir}/src])
TEA_ADD_STUB_SOURCES([])
TEA_ADD_TCL_SOURCES([
                       src/tarray.tcl
                       src/taprint.tcl
                       src/tabulate.tcl
                       src/tarbc.tcl
                       src/parser.tcl
                       src/dbimport.tcl
                   ])


#--------------------------------------------------------------------
# __CHANGE__
#
# You can add more files to clean if your extension creates any extra
# files by extending CLEANFILES.
# Add pkgIndex.tcl if it is generated in the Makefile instead of ./configure
# and change Makefile.in to move it from CONFIG_CLEAN_FILES to BINARIES var.
#
# A few miscellaneous platform-specific items:
# TEA_ADD_* any platform specific compiler/build info here.
#--------------------------------------------------------------------

if test "${TEA_PLATFORM}" = "windows" ; then
    # Ensure no empty if clauses
    CLEANFILES="$CLEANFILES pkgIndex.tcl *.lib *.dll *.exp *.ilk *.pdb vc*.pch"
    #TEA_ADD_SOURCES([win/winFile.c])
    #TEA_ADD_INCLUDES([-I\"$(${CYGPATH} ${srcdir}/win)\"])
    TEA_ADD_LIBS([rpcrt4.lib])
else
    # Ensure no empty else clauses
    CLEANFILES="$CLEANFILES pkgIndex.tcl"
    if test "`uname -s`" = "Darwin" ; then
        TEA_ADD_LIBS([-framework CoreFoundation])
    fi
fi

#--------------------------------------------------------------------
# __CHANGE__
# Choose which headers you need.  Extension authors should try very
# hard to only rely on the Tcl public header files.  Internal headers
# contain private data structures and are subject to change without
# notice.
# This MUST be called after TEA_LOAD_TCLCONFIG / TEA_LOAD_TKCONFIG
#--------------------------------------------------------------------

TEA_PUBLIC_TCL_HEADERS
#TEA_PRIVATE_TCL_HEADERS

#TEA_PUBLIC_TK_HEADERS
#TEA_PRIVATE_TK_HEADERS
#TEA_PATH_X

#--------------------------------------------------------------------
# tarray specific configuration
#--------------------------------------------------------------------

AC_ARG_ENABLE([threadpool],
              AS_HELP_STRING([--enable-threadpool],
                             [disable thread pool [default: on]]),
	      [], [enable_threadpool=yes])

AS_VAR_IF([enable_threadpool], [yes],
          [AC_DEFINE([TA_MT_ENABLE], 1, [enable thread pool])])

if test "${TEA_PLATFORM}" = "windows" ; then
    # Ensure no empty if clauses
    :
else
    if test "`uname -s`" = "Darwin" ; then
        TEA_ADD_LIBS([-ldispatch])
    else
        AC_CHECK_LIB(dispatch, dispatch_group_create)
    fi
fi


#--------------------------------------------------------------------
# Check whether --enable-threads or --disable-threads was given.
# This auto-enables if Tcl was compiled threaded.
#--------------------------------------------------------------------

TEA_ENABLE_THREADS

#--------------------------------------------------------------------
# The statement below defines a collection of symbols related to
# building as a shared library instead of a static library.
#--------------------------------------------------------------------

TEA_ENABLE_SHARED

#--------------------------------------------------------------------
# This macro figures out what flags to use with the compiler/linker
# when building shared/static debug/optimized objects.  This information
# can be taken from the tclConfig.sh file, but this figures it all out.
#--------------------------------------------------------------------

TEA_CONFIG_CFLAGS

TEA_ADD_CFLAGS([-DTCL_NO_DEPRECATED])

#--------------------------------------------------------------------
# Set the default compiler switches based on the --enable-symbols option.
#--------------------------------------------------------------------

TEA_ENABLE_SYMBOLS

#--------------------------------------------------------------------
# This macro generates a line to use when building a library.  It
# depends on values set by the TEA_ENABLE_SHARED, TEA_ENABLE_SYMBOLS,
# and TEA_LOAD_TCLCONFIG macros above.
#--------------------------------------------------------------------

TEA_MAKE_LIB

#--------------------------------------------------------------------
# Determine the name of the tclsh and/or wish executables in the
# Tcl and Tk build directories or the location they were installed
# into. These paths are used to support running test cases only,
# the Makefile should not be making use of these paths to generate
# a pkgIndex.tcl file or anything else at extension build time.
#--------------------------------------------------------------------

TEA_PROG_TCLSH
#TEA_PROG_WISH

#--------------------------------------------------------------------
# Setup a *Config.sh.in configuration file.
#--------------------------------------------------------------------

#TEA_EXPORT_CONFIG([sample])
#AC_SUBST(SAMPLE_VAR)

#--------------------------------------------------------------------
# Needed for test DLL suffix depending on platform
#--------------------------------------------------------------------
AC_SUBST(SHLIB_SUFFIX)

#--------------------------------------------------------------------
# Needed for linking against tommath on Windows. TCL_TOMMATH_LIB_NAME
# will be set in tclConfig.sh when required (Windows, Tcl 9)
#--------------------------------------------------------------------
# No longer needed
#if test "X${TCL_TOMMATH_LIB_NAME}" != "X" ; then
    #TCL_TOMMATH_LIB=\${libdir}/${TCL_TOMMATH_LIB_NAME}
#fi
AC_SUBST(TCL_TOMMATH_LIB)

#--------------------------------------------------------------------
# Specify files to substitute AC variables in. You may alternatively
# have a special pkgIndex.tcl.in or other files which require
# substituting the AC variables in. Include these here.
#--------------------------------------------------------------------

AC_CONFIG_FILES([Makefile pkgIndex.tcl])

#--------------------------------------------------------------------
# Finally, substitute all of the various values into the files
# specified with AC_CONFIG_FILES.
#--------------------------------------------------------------------

AC_OUTPUT
