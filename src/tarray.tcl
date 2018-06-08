#
# Copyright (c) 2015-2016, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

namespace eval tarray {
    namespace eval column { namespace path [namespace parent] }
    namespace eval table { namespace path [namespace parent] }
    namespace eval unsupported { namespace path [namespace parent] }
    namespace eval test { namespace path [namespace parent] }

    proc lambda {arglist body {ns {}}} {
        return [list ::apply [list $arglist $body $ns]]
    }

    # Fully qualify command prefix
    proc _fqcp {cmdprefix {level 2}} {
        set cmd [lindex $cmdprefix 0]
        if {[string range $cmd 0 1] eq "::"} {
            return $cmdprefix
        }
        set qual_cmd [uplevel $level [list namespace which -command $cmd]]
        if {$qual_cmd eq ""} {
            error "Command $cmd not found in caller namespace."
        }
        return [lreplace $cmdprefix 0 0 $qual_cmd]
    }
}

interp alias {} tarray::column::count {} tarray::column::search -count

proc tarray::column::bitmap0 {{count 0} {init {}}} {
    return [fill [fill [create boolean {} $count] 0 0 [incr count -1]] 1 $init]
}
proc tarray::column::bitmap1 {{count 0} {init {}}} {
    return [fill [fill [create boolean {} $count] 1 0 [incr count -1]] 0 $init]
}

proc tarray::column::values {col} {
    return [range -list $col 0 end]
}

proc tarray::table::rows {tab} {
    return [range -list $tab 0 end]
}

# TBD - document and test
# TBD - type overflows need to be checked
proc tarray::column::linspace {start stop count args} {
    dict size $args;            # Verify dictionary format

    parseargs args {
        {type.arg double {byte int uint wide double}}
        {open.bool 0}
    } -maxleftover 0 -setvars

    if {$start > $stop} {
        error "Specified of range $start is greater than the end $stop."
    }

    if {![string is integer -strict $count] || $count <= 0} {
        error "Count must be a positive integer."
    }
    if {$count == 1} {
        if {! $open} {
            if {$start != $stop} {
                error "Must not specify -open option as false if start and stop values are different and count is 1."
            }
        }
        return [create $type [list $start]]
    }

    # NOTE: count > 1 beyond this point
    
    set div $count
    if {!$open} {
        incr div -1
    }

    if {$type ne "double"} {
        if {!([string is integer -strict $start] &&
              [string is integer -strict $stop])} {
            error "Start and stop arguments must be integers if return type is $type."
        }
        set step [expr {($stop-$start)/$div}]
        if {($start + $step*$div) != $stop} {
            if {$open} {
                set range "\[$start, $stop\)"
            } else {
                set range "\[$start, $stop\]"
            }
            error "Cannot have $count integer values with integral spacing in the range $range."
        }
        if {!$open} {
            incr stop $step
        }
        return [create $type [series $start $stop $step]]
    }

    # Ensure operands are treated as doubles
    set start [tcl::mathfunc::double $start]
    set stop  [tcl::mathfunc::double $stop]

    # Credits: numpy
    set result [series 0.0 $count 1]

    set delta [expr {$stop - $start}]
    set step [expr {$delta / $div}]

    # TBD - a column vmath command would perform better here

    if {$step == 0} {
        set result [math / $result $div]
        set result [math * $result $delta]
    } else {
        set result [math * $result $step]
    }

    set result [math + $result $start]
    if {!$open} {
        # Overwrite last element which might have exceeded bound
        vfill result $stop end
    }

    return $result
}

proc tarray::column::histogram {args} {
    parseargs args {
        {compute.radio count {count sum values indices}}
        min.arg
        max.arg
        {cnames.arg {LowerBound Data}}
    } -setvars
        
    if {[llength $args] != 2} {
        error "wrong #args: should be \"column histogram ?options? COL ARG\"."
    }
    lassign $args col nintervals
    
    if {$nintervals <= 0} {
        error "Number of buckets must be greater than zero."
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

    if {[type $col] eq "double"} {
        # Take care to compute as doubles in case values passed in
        # as integers. Note that thanks to FP inexact representations
        # this is not entirely accurate. The C code will take care
        # of clamping values exceeding the highest bucket to that
        # bucket.
        set max [tcl::mathfunc::double $max]
        set min [tcl::mathfunc::double $min]
        set step [expr {($max - $min) / $nintervals}]
        set upper [expr {$min + $nintervals * $step}]
        if {$upper < $max} {
            set step [expr {$step + (($max - $upper)/$nintervals)}]
        }
    } else {
        set step [expr {(($max - $min) / $nintervals) + 1}]
    }
    
    return [tarray::table::create2 $cnames [_equalintervals $col $compute $nintervals $min $max $step]]
}

proc tarray::column::categorize {args} {

    parseargs args {
        {collect.radio indices {values indices}}
        categorizer.arg
        {cnames.arg {Category Data}}
        categorytype.arg
    } -setvars

    if {[llength $args] != 1} {
        error "Wrong #args: should be \"column categorize ?options? COLUMN\"."
    }
    set col [lindex $args 0]

    set buckets {}

    # Breaking out loops in various cases is verbose but significantly faster
    if {[info exists categorizer]} {
        set categorizer [tarray::_fqcp $categorizer]
        if {$collect eq "indices"} {
            tarray::loop i e $col {
                switch -exact -- [catch { {*}$categorizer $i $e } bucket ropts] {
                    0 {}
                    3 { break }
                    4 { continue }
                    default {
                        dict incr ropts -level
                        return -options $ropts $bucket
                    }
                }
                dict lappend buckets $bucket $i
            }
        } else {
            tarray::loop i e $col {
                switch -exact -- [catch { {*}$categorizer $i $e } bucket ropts] {
                    0 {}
                    3 { break }
                    4 { continue }
                    default {
                        dict incr ropts -level
                        return -options $ropts $bucket
                    }
                }
                dict lappend buckets $bucket $e
            }
        }
        if {![info exists categorytype]} {
            set categorytype any
        }
    } else {
        # Categorize based on value itself
        if {$collect eq "indices"} {
            tarray::loop i e $col {
                dict lappend buckets $e $i
            }
        } else {
            tarray::loop i e $col {
                dict lappend buckets $e $e
            }
        }
        if {![info exists categorytype]} {
            set categorytype [type $col]
        }
    }
    
    if {$collect eq "indices"} {
        set indices {}
        foreach bucket [dict keys $buckets] {
            lappend indices [create int [dict get $buckets $bucket]]
        }
        set groups [create any $indices]
    } else {
        set values {}
        foreach bucket [dict keys $buckets] {
            lappend values [create [type $col] [dict get $buckets $bucket]]
        }
        set groups [create any $values]
    }

    return [tarray::table::create2 $cnames [list [create $categorytype [dict keys $buckets]] $groups]]
}

proc tarray::column::summarize {args} {
    array set opts [parseargs args {
        count
        summarizer.arg
        {summarytype.arg any {boolean byte int uint wide double string any}}
        sum
    }]

    if {[llength $args] != 1} {
        error "wrong # args: should be \"column summarize ?options? DATACOL\"."
    }
    set data_col [lindex $args 0]
    set nbuckets [size $data_col]
    set opttotal [expr {$opts(sum) + $opts(count) + [info exists opts(summarizer)]}]
    if {$opttotal > 1} {
        error "Only one among -count, -sum and -summarizer may be specified."
    }
    if {$opttotal == 0} {
        set opts(count) 1
    }

    if {$opts(count)} {
        set col [create int {} $nbuckets]
        loop i e $data_col {
            vfill col [size $e] $i
        }
    } elseif {$opts(sum)} {
        if {$nbuckets == 0} {
            set col [create wide]
        } else {
            if {[type [index $data_col 0]] eq "double"} {
                set sum_type double
            } else {
                set sum_type wide
            }
            set col [create $sum_type {} $nbuckets]
            loop i e $data_col {
                vfill col [sum $e] $i
            }
        }
    } else {
        set fqcn [tarray::_fqcp $opts(summarizer)]
        set col [create $opts(summarytype) {} $nbuckets]
        loop i e $data_col {
            vfill col [{*}$fqcn $i $e] $i
        }
    }
        
    return $col
}

proc tarray::table::summarize {args} {
    # Retrieve our options
    parseargs args {
        {cname.arg Summary}
        summarizer.arg
    } -setvars -ignoreunknown

    # Save remaining options to be passed on
    set saved_args $args


    # Strip off options to get at table argument.
    # Don't really care about the values
    parseargs args {
        count
        {summarytype.arg any {boolean byte int uint wide double string any}}
        sum
    }

    set nargs [llength $args]
    if {$nargs < 1 || $nargs > 3} {
        error "wrong # args: should be \"table summarize ?options? TABLE ??LABELCOL? DATACOL?\"."
    }
    set tab [lindex $args 0]
    if {$nargs == 1} {
        set label_col_name [lindex [table cnames $tab] 0]
        set label_col [table column $tab 0]
        set data_col  [table column $tab 1]
    } elseif {$nargs == 2} {
        set label_col_name [lindex [table cnames $tab] 0]
        set label_col [table column $tab 0]
        set data_col  [table column $tab [lindex $args 1]]
    } else {
        set label_col_name [lindex $args 1]
        set label_col [table column $tab $label_col_name]
        set data_col  [table column $tab [lindex $args 2]]
    }

    # Remove our arguments from options to be passed on
    set saved_args [lrange $saved_args 0 end-$nargs]

    # We had namespace qualified the summarizer call back
    # with respect to our caller. Add back that option if it
    # was specified
    if {[info exists summarizer]} {
        # Fully qualify callback before passing it on
        set fqcn [tarray::_fqcp $summarizer]
        lappend saved_args -summarizer $fqcn
    }

    return [create2 \
                [list $label_col_name $cname] \
                [list $label_col \
                     [tarray::column::summarize {*}$saved_args $data_col]]]
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

proc tarray::table::Sort {args} {
    array set opts [parseargs args {
        {order.radio increasing {increasing decreasing}}
        nocase
        indices
        columns.arg
        {format.radio table {table list dict}}
    } -hyphenated]; # -hyphenated to get back "-increasing", not "increasing" etc.
    if {[llength $args] != 2} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? table column"
    }
    lassign $args tab colname

    set sort_opts [list $opts(-order)]
    if {$opts(-nocase)} {
        lappend sort_opts -nocase
    }

    set format_opts [list $opts(-format)]
    if {[info exists opts(-columns)]} {
        lappend format_opts -columns $opts(-columns)
    }

    set indices [tarray::column::Sort -indices {*}$sort_opts [column $tab $colname]]
    if {$opts(-indices)} {
        return $indices
    } else {
        return [get {*}$format_opts $tab $indices]
    }
}

proc tarray::table::Join {args} {

    parseargs args {
        on.arg
        nocase
        t0cols.arg
        t1cols.arg
        {t1suffix.arg _t1}
    } -setvars

    if {[llength $args] != 2} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? TABLEA TABLEB"
    }

    # Variable index:

    # tab0, tab1 - input data tables
    lassign $args tab0 tab1
    # cnames0, cnames1 - column names of above
    set cnames0 [cnames $tab0]
    set cnames1 [cnames $tab1]

    # tab0col, tab1col - names of columns to be compared
    if {![info exists on] || [llength $on] == 0} {
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
    } elseif {[llength $on] == 1} {
        set tab0col [lindex $on 0]
        set tab1col $tab0col
    } elseif {[llength $on] == 2} {
        lassign $on tab0col tab1col
    } else {
        error "At most two column names may be specified for the -on option."
    }
    if {$tab0col ni $cnames0} {
        error "Column $tab0col not in table."
    }
    if {$tab1col ni $cnames1} {
        error "Column $tab1col not in table."
    }

    if {![info exists t0cols]} {
        set t0cols $cnames0;       # By default include all columns
    }

    if {![info exists t1cols]} {
        set t1cols $cnames1
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
    # t{0,1}cols are names of input columns to be included in result
    # tab1out are names of output columns for tab1 (potentially renamed)
    # (Note currently there is no tab0out as tab0 columns are not renamed.)

    if {[llength $t0cols] == 0} {
        # No columns from tab0 to be included in output so no need
        # to rename tab1 columns
        set tab1out $t1cols
    } else {
        # Rename every tab1 column that is clashing with tab0
        set tab1out [lmap c1 $t1cols {
            if {$c1 in $t0cols} {
                append c1 $t1suffix
            }
            set c1
        }]
    }

    # Now retrieve the actual data
    if {[llength $t0cols]} {
        set out0 [columns [get -columns $t0cols $tab0 $tab0indices]]
    } else {
        set out0 {}
    }
    if {[llength $t1cols]} {
        set out1 [columns [get -columns $t1cols $tab1 $tab1indices]]
    } else {
        set out1 {}
    }

    return [create2 [concat $t0cols $tab1out] [concat $out0 $out1]]
}
    
proc tarray::table::csvimport {args} {
    variable tclcsv_loaded
    if {![info exists tclcsv_loaded]} {
        uplevel #0 package require tclcsv
        set tclcsv_loaded 1
    }

    parseargs args {
        encoding.arg
        translation.arg
        sniff
    } -setvars -ignoreunknown

    if {[llength $args] == 0} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? PATH|CHANNEL"
    }

    set source [lindex $args end]
    set args [lrange $args 0 end-1]
    if {$source in [chan names]} {
        set fd $source
        set close_fd 0
    } else {
        set fd [open $source r]
        set close_fd 1
    }

    try {
        foreach opt {encoding translation} {
            if {[info exists $opt]} {
                chan configure $fd -$opt [set $opt]
            }
        }
        if {$sniff} {
            set args [dict merge [tclcsv::sniff $fd] $args]
        }
        # Get header if present. Otherwise we will just do it later based
        # on data content.
        if {! [catch {
            lassign [tclcsv::sniff_header {*}$args $fd] types header
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
                lappend args -startline 1
            }
        }
        set reader [tclcsv::reader new {*}$args $fd]
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
        if {$close_fd} {
            close $fd
        }
    }
    return $tab
}

proc tarray::table::csvexport {args} {
    variable tclcsv_loaded
    if {![info exists tclcsv_loaded]} {
        uplevel #0 package require tclcsv
        set tclcsv_loaded 1
    }
    
    parseargs args {
        append
        force
        encoding.arg
        translation.arg
        header.arg
    } -setvars -ignoreunknown
    
    if {[llength $args] < 2} {
        error "wrong # args: should be \"[lindex [info level 0] 0] ?options? PATH TABLE"
    }
    set tab  [lindex $args end]
    set source [lindex $args end-1]
    set args [lrange $args 0 end-2]

    if {$source in [chan names]} {
        set fd $source
        set close_fd 0
    } else {
        if {[file exists $source] && ! $append && ! $force} {
            error "File $source exists. Use -force to overwrite."
        }
        if {$append} {
            set fd [open $source a]
        } else {
            set fd [open $source w]
        }
        set close_fd 1
    }

    try {
        foreach opt {encoding translation} {
            if {[info exists $opt]} {
                chan configure $fd -$opt [set $opt]
            }
        }

        if {[info exists header]} {
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
        if {$close_fd} {
            close $fd
        }
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

# TBD - document fold
interp alias {} tarray::column::sum {} tarray::column::fold +

namespace eval tarray {
    
    namespace eval column {
        namespace ensemble create -map {
            bitmap0 bitmap0
            bitmap1 bitmap1
            bucketize bucketize
            cast cast
            categorize categorize
            count count
            create create
            delete delete
            dump dump
            equal equal
            fill fill
            fold fold
            get get
            groupby groupby
            histogram histogram
            identical identical
            index index
            inject inject
            insert insert
            intersect3 intersect3
            linspace linspace
            lookup lookup
            loop ::tarray::loop
            math math
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
            sort Sort
            sum sum
            summarize summarize
            type type
            values values
            vdelete vdelete
            vfill vfill
            vinject vinject
            vinsert vinsert
            vplace vplace
            vput vput
            vreverse vreverse
            vshuffle vshuffle
            vsort vsort
            width width
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
            join Join
            loop ::tarray::loop
            place place
            prettify prettify
            print print
            put put
            range range
            reverse reverse
            rows rows
            size size
            slice slice
            sort Sort
            summarize summarize
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

    namespace export bitmap0 bitmap1 column loop parseargs oneopt prettify print randseed rng table

}

