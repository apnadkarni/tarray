#
# Copyright (c) 2015-2016, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

namespace eval tarray {
    namespace eval column {}
    namespace eval table {}
    namespace eval db {}
    namespace eval unsupported {}
    namespace eval test {}
}

interp alias {} tarray::column::count {} tarray::column::search -count

proc tarray::column::bitmap0 {{count 0} {init {}}} {
    return [fill [fill [create boolean {} $count] 0 0 [incr count -1]] 1 $init]
}
proc tarray::column::bitmap1 {{count 0} {init {}}} {
    return [fill [fill [create boolean {} $count] 1 0 [incr count -1]] 0 $init]
}

proc tarray::column::_group_by_equal_intervals {col compute nintervals args} {
    dict size $args;            # Verify dictionary format
    if {[dict exists $args -min]} {
        set min [dict get $args -min]
    }
    if {[dict exists $args -max]} {
        set max [dict get $args -max]
    }
    if {![info exists min] || ![info exists max]} {
        lassign [minmax $col] smallest largest
        if {![info exists min]} {
            set min $smallest
        }
        if {![info exists max]} {
            set max $largest
        }
    }

    if {$min > $max} {
        error "Invalid bucket range $min-$max."
    }
    if {$nintervals <= 0} {
        error "Number of buckets must be greater than zero."
    }

    if {[type $col] eq "double"} {
        # Take care to compute as doubles in case values passed in
        # as integers. Note that thanks to FP inexact representations
        # this is not entirely accurate. The C code will take care
        # of clamping values exceeding the highest bucket to that
        # bucket.
        set step [expr {(double($max) - $min) / $nintervals}]
    } else {
        set step [expr {(($max - $min) / $nintervals) + 1}]
    }
    
    return [_equalintervals $col $compute $nintervals $min $step]
}

proc tarray::column::groupby {method compute col args} {

    if {[tcl::prefix match {equalintervals command} $method] eq "equalintervals"} {
        return [tarray::table::create2 \
                    [list Bucket [string totitle $compute]] \
                    [_group_by_equal_intervals $col $compute {*}$args]]
    }

    if {[llength $args] != 1} {
        error "Wrong #args: should be \"column groupby command COMPUTE COLUMN CMDPREFIX.\""
    }

    set buckets {}
    tarray::loop i e $col {
        switch -exact -- [catch { {*}[lindex $args 0] $i $e } bucket ropts] {
            0 {}
            3 { break }
            4 { continue }
            default {
                dict incr ropts -level
                return -options $ropts $bucket
            }
        }
        switch -exact -- $compute {
            count   { dict incr buckets $bucket }
            indices { dict lappend buckets $bucket $i }
            values  { dict lappend buckets $bucket $e }
            sum     { 
                if {![dict exists $buckets $bucket]} {
                    dict set buckets $bucket 0
                }
                dict set buckets $bucket [expr {[dict get $buckets $bucket] + $e}]
            }
        }
    }
    
    switch -exact -- $compute {
        count {
            # Note [dict values] returns elements in same order as [dict keys]
            set groups [create int [dict values $buckets]]
        }
        sum {
            if {[type $col] in {double string any}} {
                set groups [create double [dict values $buckets]]
            } else {
                set groups [create wide [dict values $buckets]]
            }
        }
        indices {
            set indices {}
            foreach bucket [dict keys $buckets] {
                lappend indices [create int [dict get $buckets $bucket]]
            }
            set groups [create any $indices]
        }
        values {
            set values {}
            foreach bucket [dict keys $buckets] {
                lappend values [create [type $col] [dict get $buckets $bucket]]
            }
            set groups [create any $values]
        }
    }

    return [tarray::table::create2 \
                [list Bucket [string totitle $compute]] \
                [list [create any [dict keys $buckets]] $groups]]
}



proc tarray::table::create {def {init {}} {size 0}} {
    set colnames {}
    set cols {}
    array set seen {}
    foreach {colname coltype} $def {
        if {[info exists seen($colname)]} {
            error "Duplicate column name '$colname'."
        }
        set seen($colname) 1
        lappend colnames $colname
        lappend cols [tarray::column::create $coltype {} $size]
    }

    return [inject [list tarray_table $colnames $cols] $init end]
}

proc tarray::table::create2 {colnames columns} {
    if {[llength $colnames] != [llength $columns]} {
        error "Column names differ in number from specified columns."
    }
    if {[llength $columns] != 0} {
        foreach colname $colnames {
            if {![regexp {^[_[:alpha:]][-_[:alnum:]]*$} $colname]} {
                error "Invalid column name syntax '$colname'."
            }
            if {[info exists seen($colname)]} {
                error "Duplicate column name '$colname'."
            }
            set seen($colname) 1
        }
        # Make sure all columns are the same length
        set len [tarray::column::size [lindex $columns 0]]
        foreach col [lrange $columns 1 end] {
            if {[tarray::column::size $col] != $len} {
                throw [list TARRAY TABLE LENGTH] "Columns in table have differing lengths."
            }
        }
    }
    # TBD - does this result in columns shimmering ?
    return [list tarray_table $colnames $columns]
}

proc tarray::table::columns {tab args} {
    if {[llength $args] == 0} {
        return [_columns $tab]
    }
    if {[llength $args] > 1} {
        error "wrong # args: should be \"table columns TABLE ?COLNAMES?\""
    }
    set columns {}
    foreach colname [lindex $args 0] {
        lappend columns [column $tab $colname]
    }
    return $columns
}

proc tarray::table::ctype {tab cname} {
    return [tarray::column type [tarray::table::column $tab $cname]]
}

proc tarray::table::definition {tab {cnames {}}} {
    if {[llength $cnames] == 0} {
        set cnames [cnames $tab]
    }
    set def {}
    foreach cname $cnames {
        lappend def $cname [tarray::column type [column $tab $cname]]
    }
    return $def
}

proc tarray::table::sort {args} {
    if {[llength $args] < 2} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? table column"
    }
    set sort_opts {}
    set format_opts {}
    set want_indices 0
    set opts [lrange $args 0 end-2]
    set n [llength $opts]
    for {set i 0} {$i < $n} {incr i} {
        set arg [lindex $opts $i]
        switch -exact -- $arg {
            -indices {set want_indices 1}
            -increasing -
            -decreasing -
            -nocase { lappend sort_opts $arg}
            -columns {
                if {[incr i] == $n} {
                    error "No value supplied for option -columns"
                }
                lappend format_opts $arg [lindex $opts $i]
            }
            -dict -
            -list -
            -table { lappend format_opts $arg }
            default {
                error "Invalid option '$arg'."
            }
        }
    }

    set tab [lindex $args end-1]
    set indices [tarray::column::sort -indices {*}$sort_opts [column $tab [lindex $args end]]]
    if {$want_indices} {
        return $indices
    } else {
        return [get {*}$format_opts $tab $indices]
    }
}

proc tarray::table::join {args} {
    set nargs [llength $args]
    if {[llength $args] < 2} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? TABLEA TABLEB"
    }

    # Variable index:
    # tab0, tab1 - input data tables
    # cnames0, cnames1 - column names of above
    # tab0col, tab1col - names of columns to be compared
    # tab0inc, tab1inc - output columns

    set tab0 [lindex $args end-1]
    set tab1 [lindex $args end]
    incr nargs -2

    set nocase 0
    set cnames0 [cnames $tab0]
    set tab0inc $cnames0;       # By default include all columns
    set cnames1 [cnames $tab1]
    set tab1inc $cnames1
    set t1suffix "_t1"
    for {set i 0} {$i < $nargs} {incr i} {
        set opt [tcl::prefix match {
            -nocase -on -t0cols -t1cols -t1suffix
        } [lindex $args $i]]
        switch -exact -- $opt {
            -on {
                if {[incr i] == $nargs} {
                    error "Missing value for option -on."
                }
                set on_cols [lindex $args $i]
                switch -exact -- [llength $on_cols] {
                    0 {}
                    1 {
                        set tab0col [lindex $on_cols 0]
                        set tab1col $tab0col
                    }
                    2 { lassign $on_cols tab0col tab1col }
                    default {
                        error "At most two column names may be specified for the -on option."
                    }
                }
            }
            -nocase   { set nocase 1 }
            -t0cols {
                if {[incr i] == $nargs} {
                    error "Missing argument for option -t0cols."
                }
                set tab0inc [lindex $args $i]
            }
            -t1cols {
                if {[incr i] == $nargs} {
                    error "Missing argument for option -t1cols."
                }
                set tab1inc [lindex $args $i]
            }
            -t1suffix { 
                if {[incr i] == $nargs} {
                    error "Missing argument for option -t1suffix."
                }
                set t1suffix [lindex $args $i]
            }
            default {
                error "Invalid option '$arg'."
            }
        }
    }

    # If the comparison columns have not been specified, find
    # a column name common to both columns.
    if {![info exists tab0col]} {
        # Loop to find the first common name.
        foreach c0 $cnames0 {
            foreach c1 $cnames1 {
                if {$c0 eq $c1} {
                    set tab0col $c0
                    break
                }
            }
        }
        if {![info exists tab0col]} {
            error "Unable to find matching column names for join."
        }
        set tab1col $tab0col
    }
    if {$tab0col ni $cnames0} {
        error "Column $tab0col not in table."
    }
    if {$tab1col ni $cnames1} {
        error "Column $tab1col not in table."
    }

    set col0 [column $tab0 $tab0col]
    set col0indices [tarray::column sort -indices $col0]
    set col1 [column $tab1 $tab1col]
    set col1indices [tarray::column sort -indices $col1]
    lassign [tarray::column::_sortmerge_helper \
                 $col0indices $col0 \
                 $col1indices $col1 \
                 $nocase] tab0indices tab1indices

    # Move on to the output side. Collect the names of the columns to
    # be included in the output. Moreover, rename columns in case of
    # clashes or if caller requested it.
    # cnames{0,1} contain column names of input tables
    # tab{0,1}inc are names of input columns to be included in result
    # tab1out are names of output columns for tab1 (potentially renamed)
    # (Note currently there is no tab0out as tab0 columns are not renamed.)

    if {[llength $tab0inc] == 0} {
        # No columns from tab0 to be included in output so no need
        # to rename tab1 columns
        set tab1out $tab1inc
    } else {
        # Rename every tab1 column that is clashing with tab0
        set tab1out [lmap c1 $tab1inc {
            if {$c1 in $tab0inc} {
                append c1 $t1suffix
            }
            set c1
        }]
    }

    # Now retrieve the actual data
    if {[llength $tab0inc]} {
        set out0 [columns [get -columns $tab0inc $tab0 $tab0indices]]
    } else {
        set out0 {}
    }
    if {[llength $tab1inc]} {
        set out1 [columns [get -columns $tab1inc $tab1 $tab1indices]]
    } else {
        set out1 {}
    }

    return [create2 [concat $tab0inc $tab1out] [concat $out0 $out1]]
}
    
proc tarray::table::csvimport {args} {
    variable tclcsv_loaded
    if {![info exists tclcsv_loaded]} {
        uplevel #0 package require tclcsv
        set tclcsv_loaded 1
    }

    if {[llength $args] == 0} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? PATH"
    }
    set path [lindex $args end]
    set args [lrange $args 0 end-1]
    dict size $args;            # Verify dictionary format
    
    set fd [open $path r]

    try {
        foreach opt {-encoding -translation} {
            if {[dict exists $args $opt]} {
                chan configure $fd -encoding [dict get $args $opt]
                dict unset args $opt
            }
        }
        if {[dict exists $args -sniff]} {
            set sniff [dict get $args -sniff]
            dict unset args -sniff
            if {$sniff} {
                set opts [dict merge [tclcsv::sniff $fd] $args]
            }
        }
        if {![info exists opts]} {
            set opts $args
        }
        # Get header if present. Otherwise we will just do it later based
        # on data content.
        if {! [catch {
            lassign [tclcsv::sniff_header {*}$opts $fd] types header
            set def {}
            foreach type $types title $header {
                if {$title eq ""} {
                    set title "Col_[incr colnum]"
                } else {
                    regsub -all {[^[:alnum:]_]} $title _ title
                }
                lappend def $title [dict get {integer wide real double string string} $type]
            }
        }]} {
            set tab [create $def]
            if {[llength $header]} {
                lappend opts -startline 1
            }
        }
        set reader [tclcsv::reader new {*}$opts $fd]
        while {1} {
            set recs [$reader next 1000]
            if {![info exists tab]} {
                # We were not able to tell table format above. Do it here
                # based on content.
                set colnum -1
                set def {}
                foreach field [lindex $recs 0] {
                    lappend def ColX_[incr colnum] any
                }
                set tab [create $def]
            }
            vput tab $recs
            if {[llength $recs] < 1000} {
                if {[$reader eof]} {
                    break
                }
            }
        }
    } finally {
        if {[info exists reader]} {
            $reader destroy
        }
        close $fd
    }
    return $tab
}

proc tarray::table::csvexport {args} {
    variable tclcsv_loaded
    if {![info exists tclcsv_loaded]} {
        uplevel #0 package require tclcsv
        set tclcsv_loaded 1
    }
    
    if {[llength $args] < 2} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? PATH TABLE"
    }
    set tab  [lindex $args end]
    set path [lindex $args end-1]
    set args [lrange $args 0 end-2]

    set append 0
    if {[dict exists $args -append]} {
        set append [dict get $args -append]
        dict unset args -append
    }
    if {[file exists $path] && ! $append} {
        if {![dict exists $args -force] ||
            [dict get $args -force] != 1} {
            error "File $path exists. Use -force 1 to overwrite."
        }
    }
    dict unset args -force
    if {$append} {
        set fd [open $path a]
    } else {
        set fd [open $path w]
    }
    try {
        foreach opt {-encoding -translation} {
            if {[dict exists $args $opt]} {
                chan configure $fd $opt [dict get $args $opt]
                dict unset args $opt
            }
        }
        if {[dict exists $args -header]} {
            set header [dict get $args -header]
            dict unset args -header
            tclcsv::csv_write {*}$args $fd [list $header]
        }
        # To reduce memory usage, write out a 1000 rows at a time
        set nrows [size $tab]
        set n 0
        while {$n < $nrows} {
            # Note: it's ok if we pass index beyond size to table::range
            ::tclcsv::csv_write {*}$args $fd [range -list $tab $n [incr n 1000]]
        }
    } finally {
        close $fd
    }
}

proc tarray::table::identical {ta tb} {
    if {[cnames $ta] ne [cnames $tb]} {
        return 0;
    }
    foreach ca [columns $ta] cb [columns $tb] {
        if {![tarray::column identical $ca $cb]} {
            return 0
        }
    }
    return 1;
}

proc tarray::table::equal {ta tb} {
    if {[width $ta] != [width $tb]} {
        return 0;
    }
    foreach ca [columns $ta] cb [columns $tb] {
        if {![tarray::column equal $ca $cb]} {
            return 0
        }
    }
    return 1;
}

proc tarray::column::width {col {format %s}} {
    if {[size $col] == 0} {
        return 0
    }
    switch -exact -- [type $col] {
        boolean { set len [string length [format $format 0]] }
        byte -
        int -
        uint -
        wide -
        double {
            # Note length of min can be greater (consider negative numbers)
            lassign [minmax $col] min max
            set minlen [string length [format $format $min]]
            set maxlen [string length [format $format $max]]
            if {$minlen > $maxlen} {
                set len $minlen
            } else {
                set len $maxlen
            }
        }
        string -
        any {
            set len 0
            tarray::loop val $col {
                set n [string length [format $format $val]]
                if {$n > $len} {
                    set len $n
                }
            }                
        }
    }
    return $len
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


proc tarray::unsupported::crandom {varname type count} {
    # Do not use lrandom because that will affect memory usage in benchmarks
    upvar 1 $varname col
    # TBD - return entire range of floats
    # TBD - larger numbers are more likely. Change to return equal
    # number from each range 0-9, 10-99, 100-999 etc.
    switch $type {
        boolean -
        byte -
        int -
        uint -
        wide -
        double {
            set col [tarray::column random $type $count]
        }
        string -
        any {
            set col [tarray::column create $type {} $count]
            time {
                set n [expr {round(100*rand())}]
                tarray::column vput col [string repeat $n $n]$type
            } $count
        }
        default {error "Unknown type $type"}
    }
    return
}

# Replace with C
proc tarray::unsupported::lrandom {varname type count} {
    upvar 1 $varname l
    set l {}
    # TBD - return entire range of floats
    # TBD - larger numbers are more likely. Change to return equal
    # number from each range 0-9, 10-99, 100-999 etc.
    switch $type {
        boolean -
        byte -
        int -
        uint -
        wide -
        double {
            set r [tarray::rng new $type]
            set l [$r get $count]
            $r destroy
        }
        string -
        any {
            time {
                set n [expr {round(100*rand())}]
                lappend l [string repeat $n $n]$type
            } $count
        }
        default {error "Unknown type $type"}
    }
    return
}

namespace eval tarray::samples {}
proc tarray::samples::init {} {
    variable rainfall

    set rainfall [tarray::table::create {
        Month string Rainfall double Temperature double
    } {
        {Jan 48.7     22.12}
        {Feb 78.7     24.03}
        {Mar 100.3    26.38}
        {Apr 439.1    27.84}
        {May 1118.9 27.11}
        {Jun 797.4  24.92}
        {Jul 1096.8   23.99}
        {Aug 1388.1 24.13}
        {Sep 1858.9 24.35}
        {Oct 1616.6   24.1}
        {Nov 592.4  22.88}
        {Dec 172  21.76}
    }]
    
    variable freelancers
    set freelancers [tarray::table create {
        Id int Name string Rate int Experience int City string
    } {
        {1 Peter   100 15 Boston}
        {2 John    85  10 {New York}}
        {3 Joan    90  10 {New York}}
        {4 Marcos  110 20 Chicago}
        {5 Kim     95  8  {San Francisco}}
        {6 Mani    105 12 Boston}
        {7 Idaman  70  5  Miami}
    }]

    variable freelancer_skills
    set freelancer_skills [tarray::table create {
        Id int Language string
    } {
        {1 C}
        {1 C++}
        {1 Java}
        {1 Tcl}
        {2 Java}
        {2 Javascript}
        {3 Objective-C}
        {3 Swift}
        {4 Assembler}
        {4 C}
        {4 C++}
        {4 Tcl}
        {4 {Visual Basic}}
        {4 SQL}
        {5 Javascript}
        {5 PHP}
        {5 Ruby}
        {6 Fortran}
        {6 R}
        {6 C++}
        {7 Python}
    }]

    proc init {args} {}
}

proc tarray::samples::get {tabname} {
    variable $tabname
    init
    return [set $tabname]
}

interp alias {} tarray::column::+ {} tarray::column::math +
interp alias {} tarray::column::- {} tarray::column::math -
interp alias {} tarray::column::* {} tarray::column::math *
interp alias {} tarray::column::/ {} tarray::column::math /
interp alias {} tarray::column::&& {} tarray::column::math &&
interp alias {} tarray::column::|| {} tarray::column::math ||
interp alias {} tarray::column::^^ {} tarray::column::math ^^
interp alias {} tarray::column::& {} tarray::column::math &
interp alias {} tarray::column::| {} tarray::column::math |
interp alias {} tarray::column::^ {} tarray::column::math ^
interp alias {} tarray::column::== {} tarray::column::math ==
interp alias {} tarray::column::!= {} tarray::column::math !=
interp alias {} tarray::column::< {} tarray::column::math <
interp alias {} tarray::column::<= {} tarray::column::math <=
interp alias {} tarray::column::> {} tarray::column::math >
interp alias {} tarray::column::>= {} tarray::column::math >=

interp alias {} tarray::column::sum {} tarray::column::fold +

namespace eval tarray {
    
    namespace eval column {
        namespace ensemble create -map {
            bitmap0 bitmap0
            bitmap1 bitmap1
            bucketize bucketize
            cast cast
            count count
            create create
            delete delete
            dump dump
            equal equal
            fill fill
            fold fold
            get get
            groupby groupby
            identical identical
            index index
            inject inject
            insert insert
            intersect3 intersect3
            lookup lookup
            loop ::tarray::loop
            math math
            width width
            minmax minmax
            place place
            prettify prettify
            print print
            put put
            random random
            range range
            reverse reverse
            size size
            search search
            series series
            shuffle shuffle
            sort sort
            sum sum
            type type
            vdelete vdelete
            vfill vfill
            vinject vinject
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            vshuffle vshuffle
            vsort vsort
            + +
            - -
            * *
            / /
            && &&
            || ||
            ^^ ^^
            & &
            | |
            ^ ^
            == ==
            != !=
            <  <
            <= <=
            >  >
            >= >=
        }
    }

    namespace eval table {
        namespace ensemble create -map {
            column column
            columns columns
            cnames cnames
            create create
            create2 create2
            csvexport csvexport
            csvimport csvimport
            ctype ctype
            definition definition
            delete delete
            equal equal
            fill fill
            get get
            identical identical
            index index
            inject inject
            insert insert
            join join
            loop ::tarray::loop
            place place
            prettify prettify
            print print
            put put
            range range
            reverse reverse
            size size
            slice slice
            sort sort
            vcolumn vcolumn
            vdelete vdelete
            vfill vfill
            vinject vinject
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            width width
        }
    }

    namespace export bitmap0 bitmap1 column loop prettify print randseed rng table
}

