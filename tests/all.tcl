package require Tcl 8.6-
package require tcltest 2.2

# Test configuration options that may be set are:
# (currently none)

tcltest::configure -testdir [file dirname [file normalize [info script]]]
if {[info exists env(TEMP)]} {
    tcltest::configure -tmpdir $::env(TEMP)/tarray-test/[clock seconds]
} else {
    if {[file exists /tmp] && [file isdirectory /tmp]} {
	tcltest::configure -tmpdir /tmp/tarray-test/[clock seconds]
    } else {
	error "Unable to figure out TEMP directory. Please set the TEMP env var"
    }
}

# ERROR_ON_FAILURES for github actions
set ErrorOnFailures [info exists env(ERROR_ON_FAILURES)]
# NOTE: Do NOT unset ERROR_ON_FAILURES if recursing to subdirectories
unset -nocomplain env(ERROR_ON_FAILURES)
eval tcltest::configure $argv
if {[tcltest::runAllTests] && $ErrorOnFailures} {exit 1}

puts "All done."
