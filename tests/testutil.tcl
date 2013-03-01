package require Tcl 8.5
package require platform
package require tcltest

if {![info exists tarray::test::known]} {
    namespace eval tarray::test {
        set topdir [file dirname [file dirname [file normalize [info script]]]]
        set pkgdir [file join $topdir build lib tarray]
        if {[file exists [file join $pkgdir pkgIndex.tcl]]} {
            set auto_path [linsert $auto_path 0 $pkgdir]
        }

        ################################################################
        # Define standard data used in tests

        #
        # Common list with known values that can be used for multiple types
        # used to initialize various tarrays in tests
        if {0} {
            variable known
            set known [list ]
            set i -1
            time {lappend known [incr i]} 256; # 0-255 -> So suitable for byte type
        }

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
            
            # ints
            set i 0
            set l {}
            time {set si [expr {$i & 1 ? -$i : $i}]; lappend l $si ; lappend anyl $si; incr i} $count
            lappend good(int) $l

            # uints
            set i -1
            set l {}
            time {lappend l [incr i]} $count
            lappend good(uint) $l

            # Byte
            set i -1
            set l {}
            time {lappend l [expr {[incr i] & 0xff}]} $count
            lappend good(byte) $l
            
            # wides
            set i 0
            set l {}
            time {set w [expr {$i & 1 ? -($i*$i) : ($i*$i)}] ; lappend l $w ; lappend anyl $w; incr i} $count
            lappend good(wide) $l

            lappend good(any) $anyl

            # Use 1000 value version as the sample values (arbitrary)
            # Note for booleans this uses the 101010101 unaligned pattern which
            # is what we want
            if {$count == 1000} {
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

            if {[llength $avals] != [llength $bvals]} {
                return 0
            }

            switch -exact -- $atype {
                boolean {
                    foreach aval $avals bval $bvals {
                        if {!$aval != !$bval} { return 0 }
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
    }

    package require tarray
}
