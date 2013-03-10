package require Tcl 8.5
package require platform
package require tcltest

# TBD - special tests to stress boolean types
# TBD - add tests for all commands to insert / delete large amount of data
# (to make sure thdr_t.used/usable are correctly calculated 
# TBD - add tests for all commands where source operand is same as dest operand
# TBD - add tests for all commands where operand is wrong type of column

if {![info exists tarray::test::known]} {
    namespace eval tarray::test {
        namespace import ::tcltest::test

        set topdir [file dirname [file dirname [file normalize [info script]]]]
        set pkgdir [file join $topdir build lib tarray]
        if {[file exists [file join $pkgdir pkgIndex.tcl]]} {
            set auto_path [linsert $auto_path 0 $pkgdir]
        }

        ################################################################
        # Define standard data used in tests

        # TBD - really need to give more thought to test data sets. It is just
        # scattershot right now

        #
        # Define the valid values
        variable counts {0 1 2 7 8 15 16 17 31 32 33 63 64 65 256 1000 32567 32568 100000}
        variable good;   # List of lists of valid values
        variable sample; # Array indexed by type used in general tests
        array set sample {}
        foreach count $counts {
            # Booleans. Note the unaligned patterns are important
            foreach pat {0 1 01 10 011 010 1001 1100 11000011 101010101} {
                lappend good(boolean) [lrepeat $count {*}[split $pat ""]]
            }

            set anyl {}

            # [time] is nothing but a repeat mechanism

            # doubles
            set i 0.5
            set l [list 0.0]
            time {lappend l $i; lappend anyl $i; set i [expr {$i * -1.0001}]} $count
            lappend good(double) $l
            
            # Note integers are started from arbitrary offsets so that they
            # do not match up with their index position (important when
            # testing search -inline etc.)
            # ints
            set i 100
            set l {}
            time {set si [expr {$i & 1 ? -$i : $i}]; lappend l $si ; lappend anyl $si; incr i} $count
            lappend good(int) $l

            # uints
            set i 1001
            set l {}
            time {lappend l [incr i]} $count
            lappend good(uint) $l

            # Byte
            set i 128
            set l {}
            time {lappend l [expr {[incr i] & 0xff}]} $count
            lappend good(byte) $l
            
            # wides
            set i 100000
            set l {}
            time {set w [expr {$i & 1 ? -($i*$i) : ($i*$i)}] ; lappend l $w ; lappend anyl $w; incr i} $count
            lappend good(wide) $l

            lappend good(any) $anyl

            # Use 1000 value version as the sample values (arbitrary)
            # Note for booleans this uses the 101010101 unaligned pattern which
            # is what we want
            if {$count == 2} {
                foreach type {any boolean byte double int uint wide} {
                    set sample($type) [lindex $good($type) end]
                }
            }
        }

        # Differently formatted valid values
        lappend good(boolean) {0 1 5 -5 -1 1.0 1e0 -1e2 true false TRUE FALSE t f T F y n Y N yes no YES NO on off ON OFF}
        lappend good(int) {0 1 -1 2147483647 -2147483648 0xdef -0xabc  012 -077}
        lappend good(uint) {0 1 0xdef  0xffffffff 0x80000000 2147483648 4294967295 012}
        lappend good(byte) {0 1 0xff 0x80 012 255}
        lappend good(wide) {0 1 -1 0xdef -0xabc  0x1ffffffff -0x1ffffffff 012 -077 987654321098 -987654321098 9223372036854775807 -9223372036854775808}
        lappend good(double) {0 1 0.0 1.1 0x2 -0x3 011 -022 -1.1 1e10 1e-10 2.2e20 -3.3e-30 Inf -Inf}

        #
        # Invalid values. Note Unlike the good array, the bad array holds
        # a single list of bad values as they have to be checked one at a time
        # Note booleans are anything accepted by Tcl_GetBooleanFromObj which
        # is different from [string is boolean] which does not accept arbitrary
        # numerics.
        variable bad
        set bad(boolean) {abc}
        set bad(byte) {abc -1 256 1.0 1e0 true}
        set bad(int) {abc 1e1 0x1ffffffff 4294967295 false}
        set bad(uint) {abc -1 0x1ffffffff 987654321098 1.0 1e0 true}
        set bad(wide) {abc 1e1 1.0 9223372036854775808 -9223372036854775809 false}
        set bad(double) {abc true}

        ################################################################
        # Utility functions

        # Manufacture the column-equivalent rep of a list
        proc crep {type values} {
            return [list tarray $type $values]
        }

        proc validate {ta} {
            if {[llength $ta] != 3 ||
                [lindex $ta 0] ne "tarray" ||
                [lindex $ta 1] ni {any byte boolean double int uint wide}} {
                error "Value [string range $ta 0 40]... is not a tarray"
            }
        }

        # Test two lists for equality based on type
        proc lequal {type avals bvals} {
            if {[llength $avals] != [llength $bvals]} {
                return 0
            }

            switch -exact -- $type {
                boolean {
                    set i 0
                    foreach aval $avals bval $bvals {
                        if {!$aval != !$bval} {puts "$avals != $bvals at position $i" ;  return 0 }
                        incr i
                    }
                }
                any {
                    foreach aval $avals bval $bvals {
                        if {$aval ne $bval} { return 0 }
                    }
                }
                default {
                    foreach aval $avals bval $bvals {
                        if {$aval != $bval} { return 0 }
                    }
                }
            }

            return 1
        }

        # Test two columns for equality
        tcltest::customMatch column tarray::test::cequal
        proc cequal {a b} {
            validate $a
            validate $b

            lassign $a _ atype avals
            lassign $b _ btype bvals
            if {$atype ne $btype} {
                error "Tarray types $atype and $btype do not match"
            }

            return [lequal $atype $avals $bvals]
        }

        # Compare a column and a list for equality
        proc clequal {col type l} {
            return [cequal $col [crep $type $l]]
        }

        # Convenience proc to verify that result is as expected and original
        # is not changed
        proc compare_tcols_lists {type args} {
            foreach {tcol l} $args {
                if {! [clequal $tcol $type $l]} { return [incr ret] }
            }
            return 0
        }

        proc col_change_and_verify {type init expected op args} {
            set tcol [tarray::column create $type $init]
            # Note we have to do the operation and *then* check that
            # tcol is unchanged.
            return [compare_tcols_lists $type [tarray::column $op $tcol {*}$args] $expected $tcol $init]
        }

        # Deprecated - use col_change_and_verify instead, better syntax
        proc change_and_verify_col {type init op operands expected} {
            set tcol [tarray::column create $type $init]
            # Note we have to do the operation and *then* check that
            # tcol is unchanged.
            return [compare_tcols_lists $type [tarray::column $op $tcol {*}$operands] $expected $tcol $init]
        }

        proc vchange_and_verify_col {type init vop operands expected} {
            set tcol [tarray::column create $type $init]
            # Note we have to do the operation and then check that
            # tcol also has the new value
            return [compare_tcols_lists $type [tarray::column $vop $tcol {*}$operands] $expected $tcol $expected]
        }

        proc newcolumn {type {init {}}} {
            return [tarray::column create $type $init]
        }

        proc indices {low high} {
            set l {}
            while {$low <= $high} {
                lappend l $low
                incr low
            }
        }

        proc indexcolumn {args} {
            return [tarray::column create int [concat {*}$args]]
        }

        proc samplerange {type {low 0} {high end}} {
            variable sample
            return [lrange $sample($type) $low $high]
        }

        proc samplecolumn {type {low 0} {high end}} {
            variable sample
            # NOTE: do NOT replace this with a pre-created sample column as
            # tests might depend on a unshared column object
            return [tarray::column create $type [samplerange $type $low $high]]
        }

        proc samplevalue {type {pos 0}} {
            variable sample
            return [lindex $sample($type) $pos]
        }

        proc samplevalues {type indexlist} {
            variable sample
            set l {}
            foreach i $indexlist {
                lappend l [samplevalue $type $i]
            }
            return $l
        }

        proc lmax {l} {
            set max [lindex $l 0]
            foreach val [lrange $l 1 end] {
                if {$val > $max} {set max $val}
            }
            return $max
        }

        proc lmin {l} {
            set min [lindex $l 0]
            foreach val [lrange $l 1 end] {
                if {$val < $min} {set min $val}
            }
            return $min
        }

        proc samplemax {type} {
            variable sample
            if {$type eq "boolean"} { return 1 }
            return [lmax $sample($type)]
        }

        proc samplemin {type} {
            variable sample
            if {$type eq "boolean"} { return 1 }
            return [lmin $sample($type)]
        }

        proc samplemin {type} {
            variable sample
            if {$type eq "boolean"} { return 0 }
            return [lmin $sample($type)]
        }

        proc samplesize {type} {
            variable sample
            return [llength $sample($type)]
        }

        proc badvalues {type} {
            variable bad
            return $bad($type); # Note will fail for type 'any'
        }

        proc samplefilter {type args} {
            variable sample
            set l $sample($type)
            foreach arg $args {
                set l [lsearch -inline -exact -not -all $l[set l {}] $arg]
            }
            return $l
        }

        proc lambda {arglist body {ns {}}} {
            list ::apply [list $arglist $body $ns]
        }

        proc lverify {l lambda} {
            foreach val $l {
                if {![{*}$lambda $val]} {
                    return 0
                }
            }
            return 1
        }

    }

    package require tarray
}
