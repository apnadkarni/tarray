package require Tcl 8.5
package require tcltest 2.2
source [file join [file dirname [info script]] testutil.tcl]

# Test configuration options that may be set are:
# (currently none)

tcltest::configure -testdir [file dirname [file normalize [info script]]]
tcltest::configure -tmpdir $::env(TEMP)/tarray-test/[clock seconds]

eval tcltest::configure $argv
tcltest::runAllTests
puts "All done."
