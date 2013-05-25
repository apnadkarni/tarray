namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
}

proc tarray::table::create {args} {
    set cols {}
    switch -exact -- [llength $args] {
        1 {
            # Verify each element is a column and of the correct size
            foreach col [lindex $args 0] {
                if {[tarray::column::size $col] != [tarray::column::size [lindex $args 0 0]]} {
                    error "Columns in table initializer are not the same size."
                }
                lappend cols $col
            }
            return [tarray::column::create any $cols]
        }
        2 -
        3 {
            set initsize 0
            if {[llength $args] == 3} {
                set initsize [lindex $args 2]
            }
            foreach type [lindex $args 0] {
                lappend cols [tarray::column::create $type {} $initsize]
            }
            set tab [tarray::column::create any $cols]
            tarray::table::vput tab [lindex $args 1]
            return $tab
        }
        0 {
            return [tarray::column::create any {}]
        }
        default {
            return -level 1 -code error "wrong # args:"
        }
    }

}

proc tarray::table::_create_from_list {types initvals {initsize 0}} {
    set cols {}
    foreach type $types init $initvals {
        lappend cols [tarray::column::create $type $init $initsize]
    }
    return [tarray::column::create any $cols]
}

interp alias {} tarray::table::width {} tarray::column::size

proc tarray::table::column {tab colnum} {
    # A table itself is a column (containing columns)
    return [tarray::column::index $tab $colnum]
}

interp alias {} tarray::table::slice {} tarray::column::get

proc tarray::unsupported::build_info {} {
    set result ""
    catch {append result [encoding convertfrom utf-8 [critcl_info]]}
    catch {
        foreach {k val} [compiler_info] {
            append result "\n    [format %-15s $k] $val"
        }
    }
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

