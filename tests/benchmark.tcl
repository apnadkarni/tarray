source [file join [file dirname [info script]] testutil.tcl]

proc make_list_of_doubles {count} {
    time {lappend l [expr {rand()}]} $count
    return $l
}

proc make_list_of_unique_doubles {count} {
    set l {}
    while {[llength $l] < $count} {
        time {lappend l [expr {rand()}]} [expr {$count - [llength $l]}]
        set l [lsort -unique -real $l]
    }
    return $l
}

proc make_list_of_ints {count} {
    time {lappend l [expr {int(10000*rand())}]} $count
    return $l
}

proc make_list_of_unique_ints {count} {
    time {lappend l [incr i]} $count
    return $l
}

proc make_list_of_strings {count} {
    # Multiply by 10 just so every string does not start with 0
    # and append "x" so it gets stored as a string
    time {lappend l "[lrepeat 4 [expr {10*rand()}]]x"} $count
    return $l
}

proc make_list_of_unique_strings {count} {
    set l {}
    while {[llength $l] < $count} {
        time {lappend l "[expr {10*rand()}]x"} [expr {$count - [llength $l]}]
        set l [lsort -unique $l]
    }
    return $l
}

proc mt_disable {} {
    tarray::unsupported::config sort_mt_threshold 100000000
    tarray::unsupported::config sort_mt_enable_any 0
}
proc mt_enable {} {
    tarray::unsupported::config sort_mt_threshold 10
    tarray::unsupported::config sort_mt_enable_any 1
}

proc searchbench {n} {
    set results {}

    # TA_ANY
    set lstr [make_list_of_unique_strings $n]
    set needle [lindex $lstr end]
    dict set results $n strings list [time {lsearch -exact $lstr $needle}]
    set tstr [tarray::column create any $lstr]
    mt_disable
    dict set results $n strings unthreaded [time {tarray::column search $tstr $needle}]
    mt_enable
    dict set results $n strings threaded [time {tarray::column search $tstr $needle}]
     # Now list -> tarray
    dict set results $n strings mixed [time {tarray::column search [tarray::column create any $lstr] $needle}]

    unset lstr tstr;                 # Recover memory

    set ldbl [make_list_of_unique_doubles $n]
    set needle [lindex $ldbl end]
    dict set results $n doubles list [time {lsearch -real $ldbl $needle}]
    set tdbl [tarray::column create double $ldbl]
    mt_disable
    dict set results $n doubles unthreaded [time {tarray::column search $tdbl $needle}]
    mt_enable
    dict set results $n doubles threaded [time {tarray::column search $tdbl $needle}]
    dict set results $n doubles mixed [time {tarray::column search [tarray::column create double $ldbl] $needle}]
    unset ldbl tdbl

    set lint [make_list_of_unique_ints $n]
    set needle [lindex $lint end]
    dict set results $n ints list [time {lsearch -integer $lint $needle}]
    set tint [tarray::column create int $lint]
    mt_disable
    dict set results $n ints unthreaded [time {tarray::column search $tint $needle}]
    mt_enable
    dict set results $n ints threaded [time {tarray::column search $tint $needle}]
    dict set results $n ints mixed [time {tarray::column search [tarray::column create int $lint] $needle}]
    unset lint tint

    return $results
}

proc sortbench {n} {
    set results {}

    # TA_ANY
    set lstr [make_list_of_strings $n]
    dict set results $n strings list [time {lsort $lstr}]
    set tstr [tarray::column create any $lstr]
    mt_disable
    dict set results $n strings unthreaded [time {tarray::column sort $tstr}]
    mt_enable
    dict set results $n strings threaded [time {tarray::column sort $tstr}]
     # Now list -> tarray -> list
    dict set results $n strings mixed [time {tarray::column get -list [tarray::column sort [tarray::column create any $lstr]] 0 end}]

    unset lstr tstr;                 # Recover memory

    set ldbl [make_list_of_doubles $n]
    dict set results $n doubles list [time {lsort -real $ldbl}]
    set tdbl [tarray::column create double $ldbl]
    mt_disable
    dict set results $n doubles unthreaded [time {tarray::column sort $tdbl}]
    mt_enable
    dict set results $n doubles threaded [time {tarray::column sort $tdbl}]
    dict set results $n doubles mixed [time {tarray::column get -list [tarray::column sort [tarray::column create double $ldbl]] 0 end}]
    unset ldbl tdbl

    set lint [make_list_of_ints $n]
    dict set results $n ints list [time {lsort -integer $lint}]
    set tint [tarray::column create int $lint]
    mt_disable
    dict set results $n ints unthreaded [time {tarray::column sort $tint}]
    mt_enable
    dict set results $n ints threaded [time {tarray::column sort $tint}]
    dict set results $n ints mixed [time {tarray::column get -list [tarray::column sort [tarray::column create int $lint]] 0 end}]
    unset lint tint

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
        puts ""
    }
}

if {[file normalize $::argv0] eq [file normalize [info script]]} {
    array set opts {
        -sizes {1000 10000 100000 1000000}
        -benchmarks {sort search}
    }
    if {[catch {array set opts $::argv}]} {
        puts stderr "Usage: [file tail [info script]] ?-sizes SIZES? ?-benchmarks TYPES?"
        exit 1
    }

    puts "Version:  [package require tarray]"
    puts "Run date: [clock format [clock seconds]]"
    puts [format "%9s %10s %10s %16s %16s %16s" Size Type lsort singlethread multithread lsort+mt]
    foreach bench $opts(-benchmarks) {
        foreach size $opts(-sizes) {
            puts "$bench\n====\n"
            printbenchmark  [switch -exact -- $bench {
                sort { sortbench $size }
                search { searchbench $size }
                default {
                    puts stderr "Unknown benchmark '$bench'"
                    exit 1
                }
            }]
        }
    }
}
