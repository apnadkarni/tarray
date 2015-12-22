
package require tarray
namespace path tarray

# Snippets:
# load c:/tcl/860/x64/lib/sqlite3/sqlite3384t.dll Sqlite3
# set recs [db eval {select name from geo where population > 1000000}]


# Note the cities db is from geonames.org

proc extract_rec {rec} {
    set row [list [lindex $rec 0] [lindex $rec 1] [lindex $rec 8]\
                 [lindex $rec 4] [lindex $rec 5] [lindex $rec 14]]
    set elevation [lindex $rec 15]
    if {$elevation eq ""} {set elevation 0}
    lappend row $elevation
    return $row
}


proc tarray_cities {dbvar fn} {
    upvar 1 $dbvar cities
    set cities [tarray::table create {geonameid int name string country string latitude double longitude double population wide elevation int} {} 25000]
    set fd [open $fn]
    fconfigure $fd -encoding utf-8
    try {
        while {[gets $fd line] >= 0} {
            set rec [split $line \t]
            if {[llength $rec] != 19} {
                continue;           # Bad or empty line ?
            }
            tarray::table vfill cities [extract_rec $rec] end+1
        }
    } finally {
        close $fd
    }
    return
}

proc tarray_cities2 {dbvar fn} {
    upvar 1 $dbvar cities
    set cities [tarray::table create {geonameid int name string country string latitude double longitude double population uint elevation int} {} 25000]
    set fd [open $fn]
    fconfigure $fd -encoding utf-8 -buffersize 1000000 -buffering full
    try {
        set l {}
        set i 0
        while {[gets $fd line] >= 0} {
            set rec [split $line \t]
            if {[llength $rec] != 19} {
                continue;           # Bad or empty line ?
            }
            lappend l [extract_rec $rec]
            if {[incr i] > 1000} {
                tarray::table vput cities $l
                set i 0
                set l {}
            }
        }
        if {[llength $l]} {
            tarray::table vput cities $l
        }
    } finally {
        close $fd
    }
    return
}

proc list_cities {dbvar fn} {
    upvar 1 $dbvar cities
    set cities {}
    set fd [open $fn]
    try {
        while {[gets $fd line] >= 0} {
            set rec [split $line \t]
            if {[llength $rec] != 19} {
                continue;           # Bad or empty line ?
            }
            lappend cities [extract_rec $rec]
        }
    } finally {
        close $fd
    }
    return
}

proc sqlite_cities {dbcmd fn} {
    package require sqlite3
    sqlite3 $dbcmd :memory:
    $dbcmd eval {
        CREATE table geo(geonameid INTEGER, name TEXT, country TEXT, latitude FLOAT, longitude FLOAT, population INTEGER, elevation INTEGER);
    }

    # We do not just call list_cities to load because we do not 
    # want to pollute memory usage measurement
    set cities {}
    set fd [open $fn]
    try {
        while {[gets $fd line] >= 0} {
            set rec [split $line \t]
            if {[llength $rec] != 19} {
                continue;           # Bad or empty line ?
            }
            lassign [extract_rec $rec] geonameid name country latitude longitude population elevation
            db eval {insert into geo values($geonameid,$name,$country,$latitude,$longitude,$population,$elevation)}
        }
    } finally {
        close $fd
    }
    return
}

proc toMB {nbytes} {
    return "[expr {($nbytes+524288)/1048576}] MB"
}

proc benchmark {implementation {fn cities15000.txt} {iterations 10}} {
    package require twapi
    array set membase [twapi::get_process_info [pid] -virtualbytespeak -workingsetpeak -pagefilebytespeak]

    switch $implementation {
        sqlite -
        sqlite3 {
            sqlite_cities db $fn
            # Prime the sql prepared statement cache
            db eval {select name from geo where population > 1000000}
            set time [time {db eval {select name from geo where population > 1000000}} $iterations]
        }
        tarray {
            tarray_cities tab $fn
            set time [time {table get -columns name $tab [column search -all -gt [table column $tab population] 1000000]} $iterations]
        }
        list {
            list_cities l $fn
            set time 0
        }
    }
    array set mem [twapi::get_process_info [pid] -virtualbytespeak -workingsetpeak -pagefilebytespeak]

    puts "$implementation results:"
    foreach {opt text} {
        -virtualbytespeak "Virtual Bytes"
        -workingsetpeak "Working Set"
        -pagefilebytespeak "Page file"
    } {
        puts "$text: [toMB [expr {$mem($opt) - $membase($opt)}]]"
    }
    puts $time
}


if {0} {
    tarray_cities citydb

    # Return list of city names
    table column $citydb name

    # Return list of city names sorted by population
    column sort -indices [table column $citydb population]
}
