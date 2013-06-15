namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
}

proc tarray::table::create {def {init {}}} {
    set colnames {}
    set cols {}
    array set seen {}
    foreach {colname coltype} $def {
        if {[info exists seen($colname)]} {
            error "Duplicate column name '$colname'"
        }
        set seen($colname) 1
        lappend colnames $colname
        lappend cols [tarray::column::create $coltype {}]
    }

    return [insert [list tarray_table $colnames $cols] $init end]
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

