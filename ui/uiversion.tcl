namespace eval tarray::ui {
    proc version {} {return 0.8}
    # Print version if this file is the main script. Used during builds.
    if {[file tail [info script]] eq [file tail [lindex $argv0 0]]} {
        puts [version]
    }
}
return [tarray::ui::version]
