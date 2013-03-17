# Simple Tcl script to build tarray
# Set up the appropriate environment and specify the corresponding -target
# (should be one of dev32, dev64, release32 or release64)
# No checks are made that the build env and target match


package require critcl 3.1
package require critcl::app
package require platform

set buildarea [file normalize [file join [pwd] .. build]]

# Note argv will override -target, -pkg and -libdir options if specified
critcl::app::main [list -pkg -libdir [file join $buildarea lib] -includedir [file join $buildarea include] -cache [file join $buildarea cache] -config tarray.cfg -keep {*}$argv tarray tarray.critcl]

