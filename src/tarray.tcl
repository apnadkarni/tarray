proc tarray::tabledelete {tab low {high ""}} {
    if {$high eq ""} {
        set high $low
    }

    foreach ta $tab[set tab ""] {
        lappend tab [delete $ta $low $high]
    }

    return $tab
}

namespace eval tarray {

    namespace ensemble create -command tarray -map {
        create create
        delete delete
        fill fill
        get get
        index index
        list list
        range range
        size size
        search search
        sort sort
        type type
    }

    namespace ensemble create -command ttable -map {
        delete tabledelete
        fill tablefill
    }

    namespace export tarray ttable
}

