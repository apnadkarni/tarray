namespace eval tarray {
    namespace eval column {}
    namespace eval grid {}
    namespace eval table {}
}

proc tarray::grid::delete {tab low {high ""}} {
    if {$high eq ""} {
        set high $low
    }

    foreach ta $tab[set tab ""] {
        lappend tab [delete $ta $low $high]
    }

    return $tab
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
            put put
            size size
            search search
            sort sort
            type type
            vdelete vdelete
            vfill vfill
            vput vput
        }
    }

    namespace eval grid {
        namespace ensemble create -map {
            delete delete
            fill fill
        }
    }

    namespace export column ttable
}

