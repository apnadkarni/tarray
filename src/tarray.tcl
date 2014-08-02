namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
    namespace eval unsupported {}
    namespace eval test {}
}

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

    return [insert [list tarray_table $colnames $cols] $init end]
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

proc tarray::unsupported::lrandom {type count} {
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
                lappend l $type[string repeat $n $n]
            } $count
        }
        default {error "Unknown type $type"}
    }
    return $l
}

namespace eval tarray {

    namespace eval column {
        namespace ensemble create -map {
            create create
            delete delete
            dump dump
            fill fill
            get get
            index index
            insert insert
            intersect3 intersect3
            minmax minmax
            place place
            put put
            range range
            reverse reverse
            size size
            search search
            sort sort
            type type
            vdelete vdelete
            vfill vfill
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            vsort vsort
        }
    }

    namespace eval table {
        namespace ensemble create -map {
            column column
            cnames cnames
            create create
            delete delete
            fill fill
            get get
            index index
            insert insert
            place place
            put put
            range range
            reverse reverse
            size size
            slice slice
            vdelete vdelete
            vfill vfill
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            width width
        }
    }

    namespace export column table
}

