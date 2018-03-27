namespace eval tarray::ui {
    proc version {} {return 0.9}
    # Print version if this file is the main script. Used during builds.
    # Also check if safe interp in which case argv0 will not be defined
    if {[info exists ::argv0] && [file tail [info script]] eq [file tail [lindex $::argv0 0]]} {
        puts [version]
        return
    }
}
return [tarray::ui::version]
