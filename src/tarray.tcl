proc tarray::tupledelete {tab low {high ""}} {
    if {$high eq ""} {
        set high $low
    }

    foreach ta $tab[set tab ""] {
        lappend tab [delete $ta $low $high]
    }

    return $tab
}
