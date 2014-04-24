# Simple Tcl script to build tarray
# Set up the appropriate environment and specify the corresponding -target
# Should be one of the built-in Critcl targets or if -config tarray.cfg
# is specified, one of the targets in that file.
# No checks are made that the build env and target match

# NOTE: if you need to use a debugger, use -keep option so that source
# files are preserved


package require critcl 3.1
package require critcl::app
package require platform

set buildarea [file normalize [file join [pwd] .. build]]

# Note argv will override -target, -pkg and -libdir options if specified
critcl::app::main [list -pkg -libdir [file join $buildarea lib] -includedir [file join $buildarea include] -cache [file join $buildarea cache] {*}$argv tarray tarray.critcl]

