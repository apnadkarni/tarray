source [file join [file dirname [info script]] testutil.tcl]

proc make_list_of_doubles {count} {
    time {lappend l [expr {rand()}]} $count
    return $l
}

proc make_list_of_strings {count} {
    # Multiply by 10 just so every string does not start with 0
    # and append "x" so it gets stored as a string
    time {lappend l "[lrepeat 4 [expr {10*rand()}]]x"} $count
    return $l
}

proc benchmark {n} {
    set results {}

    # Difference between lany and lstr is that the latter is already
    # string representation where the former will be converted during
    # the sort
    set lstr [make_list_of_strings $n]
    set ldbl [make_list_of_doubles $n]

    lsort $lstr;            # Prime to ensure string rep generated
        
    dict set results $n strings list [time {lsort $lstr}]
    dict set results $n doubles list [time {lsort -real $ldbl}]

    # First do non-threaded by setting thresholds very high
    tarray::unsupported::config sort_mt_threshold 100000000
    tarray::unsupported::config sort_mt_enable_any 0
    set tstr [tarray::column create any $lstr]
    set tdbl [tarray::column create double $ldbl]

    dict set results $n strings unthreaded [time {tarray::column sort $tstr}]
    dict set results $n doubles unthreaded [time {tarray::column sort $tdbl}]

    # Now do threaded sorts
    tarray::unsupported::config sort_mt_threshold 10
    tarray::unsupported::config sort_mt_enable_any 1

    dict set results $n strings threaded [time {tarray::column sort $tstr}]
    dict set results $n doubles threaded [time {tarray::column sort $tdbl}]

    # Now list -> tarray -> list
    unset tstr tdbl
    dict set results $n strings mixed [time {tarray::column sort [tarray::column create any $lstr]}]
    dict set results $n doubles mixed [time {tarray::column sort [tarray::column create double $ldbl]}]

    return $results
}

proc pdict {results {out stdout} {indent ""}} {
    dict for {k v} $results {
        if {[string match -nocase "value is a dict*" [tcl::unsupported::representation $v]]} {
            puts $out "${indent}$k:"
            pdict $v $out "${indent}  "
        } else {
            puts $out "${indent}$k: $v"
        }
    }
}

proc scale {base timing} {
    set timing [lindex $timing 0]
    set ratio [expr {double($base)/$timing}]
    return [format "%10d (%2.2f)" $timing $ratio]
}

proc printbenchmark {results {out stdout}} {
    dict for {size sizedata} $results {
        dict for {type typedata} $sizedata {
            dict with typedata {
                set base [lindex $list 0]
                puts $out [format "%9d %10s %10d %s %s %s" $size $type $base [scale $base $unthreaded] [scale $base $threaded] [scale $base $mixed]]
            }
        }
    }
}

if {[file normalize $::argv0] eq [file normalize [info script]]} {
    puts "Version:  [package require tarray]"
    puts "Run date: [clock format [clock seconds]]"
    puts [format "%9s %10s %10s %16s %16s %16s" Size Type lsort singlethread multithread lsort+mt]
    if {[llength $::argv] == 0} {
        set argv [list 1000 10000 100000 1000000]
    }
    foreach arg $argv {
        printbenchmark  [benchmark $arg]
    }
}
