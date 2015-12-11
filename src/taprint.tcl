#
# Copyright (c) 2012-2015, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#
namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
}

proc tarray::_parse_print_opts {nelems optargs} {
    set opts [dict merge {
        -full 0
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

    return [list $nhead $ntail]
}


proc tarray::column::prettify {c args} {
    lassign [tarray::_parse_print_opts [size $c] $args] nhead ntail
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
    if {[llength $args] & 1} {
        set args [lassign $args chan]
    } else {
        set chan stdout
    }
    lassign [tarray::_parse_print_opts [size $c] $args] nhead ntail
    puts $chan [tarray::column::prettify $c -head $nhead -tail $ntail]
    return
}

proc tarray::table::prettify {t args} {
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

    set alignments [lmap type [tarray::types {*}[tarray::table::columns $t]] {
        switch -exact -- $type {
            boolean - byte - int - uint - wide - double { lindex right }
            default { lindex left }
        }
    }]
    
    if {[dict exists $args -style] && [dict get $args -style] eq "graphics"} {
        # Use the default UTF-8 graphics characters for table skeleton
        return [tabulate::tabulate -alignments $alignments -data $rows -style $::tabulate::style::default]
    } else {
        return [tabulate::tabulate -alignments $alignments -data $rows -style $::tabulate::style::loFi]
    }
}

proc tarray::table::print {t args} {
    if {[llength $args] & 1} {
        set args [lassign $args chan]
    } else {
        set chan stdout
    }
    lassign [tarray::_parse_print_opts [size $t] $args] nhead ntail
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
            if {[llength $args] & 1} {
                set args [lassign $args chan]
            } else {
                set chan stdout
            }
            puts $chan $val
        }
        table   {table print $val {*}$args}
        default {column print $val {*}$args}
    }]
}

