# Simple Tcl script to build tarray
# Set up the appropriate environment and specify the corresponding -target
# Should be one of the built-in Critcl targets or if -config tarray.cfg
# is specified, one of the targets in that file.
# No checks are made that the build env and target match
#
# Example: tclsh build.tcl ext -config tarray.cfg -keep -target win32-dev64
# NOTE: if you need to use a debugger, use -keep option so that source
# files are preserved


package require critcl 3.1
package require critcl::app
package require platform

proc usage {} {
    set script [info script]
    puts "Usage:\n  $script extension\n  $script tea\n  $script test"
    exit 1
}
set buildarea [file normalize [file join [pwd] .. build]]

# Note argv will override -target, -pkg and -libdir options if specified

switch -exact -- [lindex $argv 0] {
    ext -
    extension {
        critcl::app::main [list -pkg -libdir [file join $buildarea lib] -includedir [file join $buildarea include] -cache [file join $buildarea cache] -clean {*}[lrange $argv 1 end] tarray tarray.critcl]
    }
    tea {
        critcl::app::main [list -tea -libdir [file join $buildarea tea] {*}[lrange $argv 1 end] tarray tarray.critcl]
    }
    test {
        cd ../tests
        set env(TCLLIBPATH) [linsert $env(TCLLIBPATH) 0 [file join $buildarea lib]]
        set fd [open |[list [info nameofexecutable] all.tcl -notfile xtal*.test {*}[lrange $argv 1 end]]]
        while {[gets $fd line] >= 0} {
            puts $line
        }
        close $fd
    }
    default {
        usage
    }
}
