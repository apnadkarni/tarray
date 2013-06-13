namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
}

proc tarray::table::create {def} {
    set colnames {}
    set cols {}
    foreach {colname coltype} $def {
        lappend colnames $colname
        lappend cols [tarray::column::create $coltype {}]
    }

    return [list tarray_table $colnames $cols]
}

interp alias {} tarray::table::width {} tarray::column::XXXsize

proc tarray::table::column {tab colnum} {
    # A table itself is a column (containing columns)
XXX    return [tarray::column::index $tab $colnum]
}

interp alias {} XXXtarray::table::slice {} tarray::column::get

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
            create create
            delete delete
            fill fill
            get get
            index index
            insert insert
            place place
            put put
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

