namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
    namespace eval unsupported {}
    namespace eval test {}
}

interp alias {} tarray::column::count {} tarray::column::search -count
proc tarray::table::create {def {init {}} {size 0}} {
    set colnames {}
    set cols {}
    array set seen {}
    foreach {colname coltype} $def {
        if {[info exists seen($colname)]} {
            error "Duplicate column name '$colname'."
        }
        set seen($colname) 1
        lappend colnames $colname
        lappend cols [tarray::column::create $coltype {} $size]
    }

    return [inject [list tarray_table $colnames $cols] $init end]
}

# TBD - document and test
proc tarray::table::create2 {colnames columns} {
    if {[llength $colnames] != [llength $columns]} {
        error "Column names differ in number from specified columns"
    }
    if {[llength $columns] != 0} {
        foreach colname $colnames {
            if {[info exists seen($colname)]} {
                error "Duplicate column name '$colname'."
            }
            set seen($colname) 1
        }
        # Make sure all columns are the same length
        set len [tarray::column::size [lindex $columns 0]]
        foreach col [lrange $columns 1 end] {
            if {[tarray::column::size $col] != $len} {
                error "Columns differ in length"
            }
        }
    }
    # TBD - does this result in columns shimmering ?
    return [list tarray_table $colnames $columns]
}

proc tarray::table::columns {tab args} {
    if {[llength $args] == 0} {
        return [_columns $tab]
    }
    if {[llength $args] > 1} {
        error "wrong # args: should be \"table columns TABLE ?COLNAMES?\""
    }
    set columns {}
    foreach colname [lindex $args 0] {
        lappend columns [column $tab $colname]
    }
    return $columns
}

# TBD - document and test
proc tarray::table::definition {tab {cnames {}}} {
    if {[llength $cnames] == 0} {
        set cnames [cnames $tab]
    }
    set def {}
    foreach cname $cnames {
        lappend def $cname [tarray::column type [column $tab $cname]]
    }
    return $def
}

# TBD - document and test
proc tarray::table::sort {args} {
    set sort_opts {}
    set format_opts {}
    set want_indices 0
    foreach arg [lrange $args 0 end-2] {
        switch -exact -- $arg {
            -indices {set want_indices 1}
            -increasing -
            -decreasing -
            -nocase { lappend sort_opts $arg}
            -dict -
            -list -
            -table { lappend format_opts $arg }
            default {
                error "Invalid option '$arg'"
            }
        }
    }

    set tab [lindex $args end-1]
    set indices [tarray::column::sort -indices {*}$sort_opts [column $tab [lindex $args end]]]
    if {$want_indices} {
        return $indices
    } else {
        return [get {*}$format_opts $tab $indices]
    }
}

# TBD - doc and test
proc tarray::table::join {atab acolname btab bcolname args} {
    set nocase 0
    set acolumns [cnames $atab]
    set bcolumns [cnames $btab]
    set nargs [llength $args]
    for {set i 0} {$i < $nargs} {incr i} {
        switch -exact -- [lindex $args $i] {
            -nocase   { set nocase 1 }
            -acolumns {
                incr i
                if {$i == $nargs} {
                    error "Missing argument for option -acolumns"
                }
                set acolumns [lindex $args $i]
            }
            -bcolumns {
                incr i
                if {$i == $nargs} {
                    error "Missing argument for option -bcolumns"
                }
                set bcolumns [lindex $args $i]
            }
            default {
                error "Invalid option '$arg'"
            }
        }
    }
    set acol [column $atab $acolname]
    set asorted [tarray::column sort -indices $acol]
    set bcol [column $btab $bcolname]
    set bsorted [tarray::column sort -indices $bcol]
    lassign [tarray::column::_sortmerge_helper $asorted $acol $bsorted $bcol $nocase] aindices bindices

    set acolnames {}
    set anewcolnames {}
    foreach pair $acolumns {
        lassign $pair colname newcolname
        if {$newcolname eq ""} {
            set newcolname $colname
        }
        lappend acolnames $colname
        lappend anewcolnames $newcolname
    }
    set bcolnames {}
    set bnewcolnames {}
    foreach pair $bcolumns {
        lassign $pair colname newcolname
        if {$newcolname eq ""} {
            set newcolname $colname
        }
        lappend bcolnames $colname
        lappend bnewcolnames $newcolname
    }

    if {[llength $acolnames]} {
        set aslice [columns [get -columns $acolnames $atab $aindices]]
    } else {
        set aslice {}
    }
    if {[llength $bcolnames]} {
        set bslice [columns [get -columns $bcolnames $btab $bindices]]
    } else {
        set bslice {}
    }
    
    return [create2 [concat $anewcolnames $bnewcolnames] [concat $aslice $bslice]]
}
    

proc tarray::tsource {arg1 args} {
    if {[llength $args] == 0} {
        set path $arg1
    } else {
        if {[llength $args] != 2 || $arg1 ne "-encoding"} {
            error "invalid syntax: should be \"tsource ?-encoding ENCODING? PATH\""
        }
        lassign $args encoding path
    }
    set fd [open $path r]
    try {
        if {[info exists encoding]} {
            fconfigure $fd -encoding $encoding -translation auto
        } else {
            fconfigure $fd -translation auto
        }
        set script [read $fd]
        return [uplevel 1 [list [namespace current]::tscript $script]]
    } finally {
        close $fd
    }
}

proc tarray::unsupported::build_info {} {
    set result ""
    catch {append result [encoding convertfrom utf-8 [critcl_info]]}
    catch {
        foreach {k val} [compiler_info] {
            append result "\n    [format %-15s $k] $val"
        }
    }
    append result "\n    [format %-15s source_revision] [hg_id]"
    return $result
}

proc tarray::db::create {def {init {}} {size 0}} {
    variable _tables
    variable _table_ctr

    set ncols [expr {[llength $def] / 2}]
    set table [list ]
    set table [dict create]
    set col -1
    foreach {name type} $def {
        lappend table [tarray::column create $type {} $size]
        dict set table fields $name [list type $type column [incr $col]]
    }
    dict set table data $table
    set tok "table#[incr _table_ctr]"
    set _tables($tok) $table
    return $tok
}

proc tarray::unsupported::crandom {varname type count} {
    # Do not use lrandom because that will affect memory usage in benchmarks
    upvar 1 $varname col
    set col [tarray::column create $type {} $count]
    # TBD - return entire range of floats
    # TBD - larger numbers are more likely. Change to return equal
    # number from each range 0-9, 10-99, 100-999 etc.
    switch $type {
        boolean { time {tarray::column vput col [expr {rand() > 0.5}]} $count }
        uint { time {tarray::column vput col [expr {wide(0xffffffff*rand())}]} $count }
        int { time {tarray::column vput col [expr {wide(0x7fffffff*(rand()-0.5))}]} $count }
        wide { time {tarray::column vput col [expr {wide(0x7fffffffffffffff*rand())}]} $count }
        byte { time {tarray::column vput col [expr {round(255*rand())}]} $count }
        double {time {tarray::column vput col [tcl::mathfunc::rand]} $count}
        string -
        any {
            time {
                set n [expr {round(100*rand())}]
                tarray::column vput col [string repeat $n $n]$type
            } $count
        }
        default {error "Unknown type $type"}
    }
    return
}

proc tarray::unsupported::lrandom {varname type count} {
    upvar 1 $varname l
    set l {}
    # TBD - return entire range of floats
    # TBD - larger numbers are more likely. Change to return equal
    # number from each range 0-9, 10-99, 100-999 etc.
    switch $type {
        boolean { time {lappend l [expr {rand() > 0.5}]} $count }
        uint { time {lappend l [expr {wide(0xffffffff*rand())}]} $count }
        int { time {lappend l [expr {wide(0x7fffffff*(rand()-0.5))}]} $count }
        wide { time {lappend l [expr {wide(0x7fffffffffffffff*rand())}]} $count }
        byte { time {lappend l [expr {round(255*rand())}]} $count }
        double {time {lappend l [tcl::mathfunc::rand]} $count}
        string -
        any {
            time {
                set n [expr {round(100*rand())}]
                lappend l [string repeat $n $n]$type
            } $count
        }
        default {error "Unknown type $type"}
    }
    return
}

interp alias {} tarray::column::+ {} tarray::column::math +
interp alias {} tarray::column::- {} tarray::column::math -
interp alias {} tarray::column::* {} tarray::column::math *
interp alias {} tarray::column::/ {} tarray::column::math /
interp alias {} tarray::column::& {} tarray::column::math &
interp alias {} tarray::column::| {} tarray::column::math |

interp alias {} tarray::column::sum {} tarray::column::fold +

namespace eval tarray {
    
    namespace eval column {
        namespace ensemble create -map {
            cast cast
            count count
            create create
            delete delete
            dump dump
            fill fill
            fold fold
            get get
            index index
            inject inject
            insert insert
            intersect3 intersect3
            lookup lookup
            math math
            minmax minmax
            place place
            put put
            range range
            reverse reverse
            size size
            search search
            sort sort
            sum sum
            type type
            vdelete vdelete
            vfill vfill
            vinject vinject
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            vsort vsort
            + +
            - -
            * *
            / /
            & &
            | |
        }
    }

    namespace eval table {
        namespace ensemble create -map {
            column column
            columns columns
            cnames cnames
            create create
            definition definition
            delete delete
            fill fill
            get get
            index index
            inject inject
            insert insert
            join join
            place place
            put put
            range range
            reverse reverse
            size size
            slice slice
            vcolumn vcolumn
            vdelete vdelete
            vfill vfill
            vinject vinject
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            width width
        }
    }

    namespace export column table tscript tsource print
}

