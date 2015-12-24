# Simple Tcl script to build tarray_ui

proc usage {} {
    puts "Usage:\n  [info script] package"
    exit 1
}
set buildarea [file normalize [file join [pwd] .. build]]

# Note argv will override -target, -pkg and -libdir options if specified

switch -exact -- [lindex $argv 0] {
    "" - package {
        set dir [file join $buildarea lib tarray_ui]
        file delete -- $dir
        file mkdir $dir
        file copy widgets.tcl color.tcl pkgIndex.tcl uiversion.tcl $dir
    }
    default {
        usage
    }
}
