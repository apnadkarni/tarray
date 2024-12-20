namespace eval tarray {
    proc version {} {return 2.0a0}
    # Print version if this file is the main script. Used during builds.
    # Also check if safe interp in which case argv0 will not be defined
    if {[info exists ::argv0] && [file tail [info script]] eq [file tail [lindex $::argv0 0]]} {
        puts [version]
        return
    }
}
return [tarray::version]
