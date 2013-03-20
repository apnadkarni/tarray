namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval table {}
}

proc tarray::table::create {types {initvals {}} {initsize 0}} {
    set cols {}
    foreach type $types {
        lappend cols [tarray::column::create $type {} $initsize]
    }

    set table [tarray::column::create any $cols]
    
}

proc tarray::table::column {table colnum} {
    # A table itself is a column (containing columns)
    return [tarray::column::index $table $colnum]
}

proc tarray::table::size {table} {
    return [tarray::column::size [tarray::table::column $table 0]]
}

proc tarray::table::create {def {init {}} {size 0}} {
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
            create create
            delete delete
            fill fill
            put put
        }
    }

    namespace export column ttable
}

