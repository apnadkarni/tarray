namespace eval tarray {}

proc tarray::print_table {t} {
    variable _report_ctr
    set m [namespace current]::matrix[incr _report_ctr]
    struct::matrix $m
    try {
        set nrows [table size $t]
        set ncols [table width $t]
        $m add rows [expr {1+$nrows}]
        $m add columns $ncols
        $m set row 0 [table cnames $t]
        foreach row [table range -list $t 0 end] {
            $m set row [incr rownum] $row
        }
        report::report r $ncols style ta_captionedtable 1
        return [r printmatrix $m]
    } finally {
        r destroy
        $m destroy
    }
}

proc tarray::print_column {c} {
    return [join [column range -list $c 0 end] {, }]
}

proc tarray::print {ta} {
    uplevel #0 {
        package require report
        package require struct::matrix
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


    proc [namespace current]::print {ta} {
        lassign [types $ta] type
        return [switch -exact -- [lindex [types $ta] 0] {
            "" {return $ta}
            table {print_table $ta}
            default {print_column $ta}
        }]
    }
    tailcall print $ta
}
