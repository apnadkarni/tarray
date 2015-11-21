#
# Copyright (c) 2012-2015, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#
namespace eval tarray {
    namespace eval column {}
    namespace eval table {}

    # The default characters used by the tabulate package are not ASCII
    # so define an ASCII set based style
    variable tabulate_style_ascii {
        top {
            left +
            padding -
            separator +
            right +
        }
        separator {
            left |
            padding -
            separator |
            right |
        }
        row {
            left |
            padding { }
            separator |
            right |
        }
        bottom {
            left +
            padding -
            separator +
            right +
        }
    }
}

proc tarray::_parse_print_opts {nelems optargs} {
    set opts [dict merge {
        -full 0
        -channel stdout
    } $optargs]

    if {[dict get $opts -full]} {
        set nhead $nelems
        set ntail 0
    } else {
        if {[dict exists $opts -head]} {
            set nhead [dict get $opts -head]
        } else {
            if {[dict exists $opts -tail]} {
                set nhead 0
            } else {
                set nhead 5
            }
        }
        if {[dict exists $opts -tail]} {
            set ntail [dict get $opts -tail]
        } else {
            if {[dict exists $opts -head]} {
                set ntail 0
            } else {
                set ntail 5
            }
        }
    }
    
    if {($nhead + $ntail) >= $nelems} {
        set nhead $nelems
        set ntail 0
    }

    return [list $nhead $ntail [dict get $opts -channel]]
}


proc tarray::column::prettify {c args} {
    # _parse_print_opts accepts -channel but this command does not
    if {[dict exists $args -channel]} {
        error "Invalid option '\"-channel\""
    }
    lassign [tarray::_parse_print_opts [size $c] $args] nhead ntail chan
    if {[type $c] in {string any}} {
        set sep "\n"
        set sep2 "\n...\n"
    } else {
        set sep ", "
        set sep2 "..."
    }
    set l {};                   # In case nhead and ntail are both 0
    if {$nhead} {
        set l [range -list $c 0 [expr {$nhead-1}]]
    }
    if {$ntail} {
        set l2 [range -list $c end-[expr {$ntail-1}] end]
        if {$nhead == 0} {
            set l $l2
        } else {
            lappend l $sep2
            set l [concat $l[set l {}] $l2]
        }
    }
    return [join $l $sep]
}

proc tarray::column::print {c args} {
    lassign [tarray::_parse_print_opts [size $c] $args] nhead ntail chan
    puts $chan [tarray::column::prettify $c -head $nhead -tail $ntail]
    return
}

proc tarray::table::prettify {t args} {
    # _parse_print_opts accepts -channel but this command does not
    if {[dict exists $args -channel]} {
        error "Invalid option '\"-channel\""
    }
    
    set ncols [width $t]
    set nrows [size $t]
    lassign [tarray::_parse_print_opts $nrows $args] nhead ntail
    
    set rows [list [cnames $t]]
    if {$nhead} {
        set rows [concat $rows [range -list $t 0 [expr {$nhead-1}]]]
        if {$ntail} {
            # Separator to indicate hidden rows
            lappend rows [lrepeat $ncols .]
        }
    }
    if {$ntail} {
        if {$nhead == 0} {
            # Separator to indicate hidden leading rows
            lappend rows [lrepeat $ncols .]
        }
        set rows [concat $rows [range -list $t end-[expr {$ntail-1}] end]]
    } 

    if {[dict exists $args -style] && [dict get $args -style] eq "graphics"} {
        # Use the default UTF-8 graphics characters for table skeleton
        return [tabulate::tabulate -align auto -data $rows]
    } else {
        return [tabulate::tabulate -align auto -data $rows -style $::tarray::tabulate_style_ascii]
    }
}

proc tarray::table::print {t args} {
    lassign [tarray::_parse_print_opts [size $t] $args] nhead ntail chan
    puts $chan [tarray::table::prettify $t -head $nhead -tail $ntail]
    return
}

proc tarray::prettify {val args} {
    lassign [types $val] type
    return [switch -exact -- [lindex [types $val] 0] {
        ""      { return -level 0 $val }
        table   {table prettify $val {*}$args}
        default {column prettify $val {*}$args}
    }]
}
 
proc tarray::print {val args} {
    lassign [types $val] type
    return [switch -exact -- [lindex [types $val] 0] {
        ""      {
            set opts [dict merge {-channel stdout} $args]
            puts [dict get $opts -channel] $val
        }
        table   {table print $val {*}$args}
        default {column print $val {*}$args}
    }]
}

#
# The "report" commands make use of the report package
# TBD - delete these unless they offer some advantage over use of
# tabulate
proc tarray::table::report {t args} {
    variable _report_ctr

    set ncols [width $t]
    set nrows [size $t]
    lassign [tarray::_parse_print_opts $nrows $args] nhead ntail chan
    
    set m [namespace current]::matrix[incr _report_ctr]
    struct::matrix $m
    set r [namespace current]::report$_report_ctr
    report::report $r $ncols style ta_captionedtable 1
    try {
        $m add columns $ncols
        $m add rows 1
        $m set row 0 [cnames $t]
        set rownum 0
        if {$nhead} {
            $m add rows $nhead
            for {set i 0} {$i < $nhead} {incr i} {
                $m set row [incr rownum] [index $t $i]
            }
            if {$ntail} {
                $m add rows 1
                $m set row [incr rownum] [lrepeat $ncols .]
            }
        }
        if {$ntail} {
            $m add rows $ntail
            if {$nhead == 0} {
                $m add rows 1
                $m set row [incr rownum] [lrepeat $ncols .]
            }
            for {set i [expr {$ntail-1}]} {$i >= 0} {incr i -1} { 
                $m set row [incr rownum] [index $t end-$i]
            }
        } 
        $r printmatrix2channel $m $chan
        return
    } finally {
        $r destroy
        $m destroy
    }
}

proc tarray::report {ta args} {
    uplevel #0 {
        package require report
        package require struct::matrix
    }
    # Following check it to allow re-sourcing of file during development
    if {"ta_simpletable" in [::report::styles]} {
        ::report::rmstyle ta_simpletable
        ::report::rmstyle ta_captionedtable
    }
    ::report::defstyle ta_simpletable {} {
        data    set [split "[string repeat "| "   [columns]]|"]
        top     set [split "[string repeat "+ - " [columns]]+"]
        bottom  set [top get]
        top     enable
        bottom  enable
    }

    ::report::defstyle ta_captionedtable {{n 1}} {
        ta_simpletable
        topdata   set [data get]
        topcapsep set [top get]
        topcapsep enable
        tcaption $n
    }

    proc [namespace current]::report {ta args} {
        lassign [types $ta] type
        return [switch -exact -- [lindex [types $ta] 0] {
            ""      {
                set opts [dict merge {-channel stdout} $args]
                puts [dict get $opts -channel] $ta
            }
            table   {table report $ta {*}$args}
            default {column print $ta {*}$args}
        }]
    }
    tailcall report $ta {*}$args
}
