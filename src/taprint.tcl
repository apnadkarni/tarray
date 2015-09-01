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

proc tarray::table::print {t args} {
    variable _report_ctr

    set ncols [width $t]
    set nrows [size $t]
    lassign [tarray::_parse_print_opts $nrows $args] nhead ntail
    
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
        return [$r printmatrix $m]
    } finally {
        $r destroy
        $m destroy
    }
}

proc tarray::column::print {c args} {
    lassign [tarray::_parse_print_opts [size $c] $args] nhead ntail
    if {[type $c] in {string any}} {
        set sep "\n"
        set sep2 "\n...\n"
    } else {
        set sep ", "
        set sep2 "..."
    }
    if {$nhead} {
        set l [range -list $c 0 [expr {$nhead-1}]]
        if {$ntail == 0} {
            return [join $l $sep]
        }
    }
    if {$ntail} {
        set l2 [range -list $c end-[expr {$ntail-1}] end]
        if {$nhead == 0} {
            return [join $l2 $sep]
        } else { 
            return "[join $l $sep]$sep2[join $l2 $sep]"
        }
    }
    # Both head and tail 0
    return ""
}

proc tarray::print {ta args} {
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


    proc [namespace current]::print {ta args} {
        lassign [types $ta] type
        return [switch -exact -- [lindex [types $ta] 0] {
            ""      {return $ta}
            table   {table print $ta {*}$args}
            default {column print $ta {*}$args}
        }]
    }
    tailcall print $ta {*}$args
}
