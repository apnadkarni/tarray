namespace eval tarray {
    namespace eval column {}
    namespace eval grid {}
    namespace eval table {}
}

proc tarray::grid::create {types {initvals {}} {initsize 0}} {
    set cols {}
    foreach type $types {
        lappend cols [tarray::column::create $type {} $initsize]
    }

    set grid [tarray::column::create tclobj $cols]
    
}

proc tarray::grid::column {grid colnum} {
    # A grid itself is a column (containing columns)
    return [tarray::column::index $grid $colnum]
}

proc tarray::grid::size {grid} {
    return [tarray::column::size [tarray::grid::column $grid 0]]
}

proc tarray::table::create {def {init {}} {size 0}} {
    variable _tables
    variable _table_ctr

    set ncols [expr {[llength $def] / 2}]
    set grid [list ]
    set table [dict create]
    set col -1
    foreach {name type} $def {
        lappend grid [tarray::column create $type {} $size]
        dict set table fields $name [list type $type column [incr $col]]
    }
    dict set table data $grid
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
            put put
            reverse reverse
            size size
            search search
            sort sort
            type type
            vdelete vdelete
            vfill vfill
            vinsert vinsert
            vput vput
            vreverse vreverse
            vsort vsort
        }
    }

    namespace eval grid {
        namespace ensemble create -map {
            create create
            delete delete
            fill fill
            put put
        }
    }

    namespace export column ttable
}

