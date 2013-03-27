# Simple script to build docs

set dtplite [file join [file dirname [info nameofexecutable]] dtplite.tcl]
if {! [file exists $dtplite]} {
    set dtplite [auto_execok dtplite.tcl]
    if {! [file exists $dtplite]} {
        set dtplite c:/tcl/apps/dtplite
    }
    if {! [file exists $dtplite]} {
        set dtplite c:/tcl/apps/dtplite.tcl
    }
    if {! [file exists $dtplite]} {
        error "dtplite not found."
    }
}

# As an aside, note that dtplite does not like output dir to be called
# just html. You will get strange permission denied errors.
lassign $argv output_dir
if {$output_dir eq ""} {
    set output_dir [file join [file dirname [info script]] output]
}
exec [info nameofexecutable] $dtplite -postnav Introduction files/tarray.html -postnav "Column Reference" files/column.html -postnav "Table Reference" files/table.html -toc tarray.tocdoc -o $output_dir html [file dirname [ info script]]
