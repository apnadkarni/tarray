namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
}

proc tarray::table::create {args} {
    set cols {}
    switch -exact -- [llength $args] {
        1 {
            # Verify each element is a column
            foreach col [lindex $args 0] {
                tarray::column::size $col
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

proc tarray::table::column {table colnum} {
    # A table itself is a column (containing columns)
    return [tarray::column::index $table $colnum]
}

proc tarray::table::size {table} {
    return [tarray::column::size [tarray::table::column $table 0]]
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
            fill fill
            get get
            index index
            insert insert
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
            place place
            put put
            reverse reverse
            size size
            vdelete vdelete
            vfill vfill
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
        }
    }

    namespace export column ttable
}

