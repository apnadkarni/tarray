# This is adapted from dbohdan's tabulate package
# Currently minor modification to align numbers to the right via -align auto
# Tabulate -- turn standard input into a table.
# Copyright (C) 2015 Danyil Bohdan
# License: MIT
# Slightly modified to replace utf-8 chars with equivalent escape sequences. 
namespace eval ::tabulate {
    variable version 0.4.0
    variable defaultStyle {
        top {
            left \U250C
            padding \U2500
            separator \U252C
            right \U2510
        }
        separator {
            left \U251C
            padding \U2500
            separator \U253C
            right \U2524
        }
        row {
            left \U2502
            padding { }
            separator \U2502
            right \U2502
        }
        bottom {
            left \U2514
            padding \U2500
            separator \U2534
            right \U2518
        }
    }
}

# Return a value from dictionary like [dict get] would if it is there.
# Otherwise return the default value.
proc ::tabulate::dict-get-default {dictionary default args} {
    if {[dict exists $dictionary {*}$args]} {
        dict get $dictionary {*}$args
    } else {
        return $default
    }
}

# Format a list as a table row. Does *not* append a newline after the row.
proc ::tabulate::formatRow {row columnWidths substyle alignment} {
    set result {}
    append result [dict get $substyle left]
    set fieldCount [expr { [llength $columnWidths] / 2 }]
    for {set i 0} {$i < $fieldCount} {incr i} {
        set field [lindex $row $i]
        set padding [expr {
            [dict get $columnWidths $i] - [string length $field]
        }]
        set column_alignment [lindex $alignment $i]
        if {$column_alignment eq ""} {
            set column_alignment [lindex $alignment end]
        }
        switch -exact -- $column_alignment {
            center {
                set rightPadding [expr { $padding / 2 }]
                set leftPadding [expr { $padding - $rightPadding }]
            }
            left {
                set rightPadding $padding
                set leftPadding 0
            }
            right {
                set rightPadding 0
                set leftPadding $padding
            }
            default {
                error "unknown row alignment: \"$alignment\""
            }
        }
        append result [string repeat [dict get $substyle padding] $leftPadding]
        append result $field
        append result [string repeat [dict get $substyle padding] $rightPadding]
        if {$i < $fieldCount - 1} {
            append result [dict get $substyle separator]
        }
    }
    append result [dict get $substyle right]
    return $result
}

# Convert a list of lists into a string representing a table in pseudographics.
proc ::tabulate::tabulate args {
    set data [dict get $args -data]
    set style [::tabulate::dict-get-default $args \
            $::tabulate::defaultStyle -style]
    set align [::tabulate::dict-get-default $args center -align]

    # Find out the maximum width of each column.
    set columnWidths {} ;# Dictionary.
    foreach row $data {
        for {set i 0} {$i < [llength $row]} {incr i} {
            set field [lindex $row $i]
            set currentLength [string length $field]
            set width [::tabulate::dict-get-default $columnWidths 0 $i]
            if {($currentLength > $width) || ($width == 0)} {
                dict set columnWidths $i $currentLength
            }
        }
    }

    # If alignment is auto, examine a few rows for numeric types
    # Don't want to include that in the above loop for performance reasons.
    if {$align eq "auto"} {
        # Figure out alignment for all columns
        # Assume right-aligned. If any not a number, we'll change to left
        set align [lrepeat [dict size $columnWidths] right]
        set num_left_aligned 0
        if {[llength $data] == 1} {
            set start_row 0
        } else {
            set start_row 1;    # In case a header is present
        }
        foreach row [lrange $data $start_row 10] {
            for {set i 0} {$i < [llength $row]} {incr i} {
                if {[lindex $align $i] eq "left"} {
                    # Already left aligned, no need to check field content
                }
                set field [lindex $row $i]
                if {[string is entier -strict $field]
                    || [string is double -strict $field]} {
                    continue
                }
                lset align $i left
                incr num_left_aligned
            }
            if {$num_left_aligned == [dict size $columnWidths]} {
                # All left aligned already. No point checking further rows
                break
            }
        }
    }
            
    # A dummy row for formatting the table's decorative elements with
    # [formatRow].
    set emptyRow {}
    for {set i 0} {$i < ([llength $columnWidths] / 2)} {incr i} {
        lappend emptyRow {}
    }

    set result {}
    set rowCount [llength $data]
    # Top of the table.
    lappend result [::tabulate::formatRow $emptyRow \
            $columnWidths [dict get $style top] $align]
    # For each row...
    for {set i 0} {$i < $rowCount} {incr i} {
        set row [lindex $data $i]
        # Row.
        lappend result [::tabulate::formatRow $row \
                $columnWidths [dict get $style row] $align]
        # Separator.
        if {$i < $rowCount - 1} {
            lappend result [::tabulate::formatRow $emptyRow \
                    $columnWidths [dict get $style separator] $align]
        }
    }
    # Bottom of the table.
    lappend result [::tabulate::formatRow $emptyRow \
            $columnWidths [dict get $style bottom] $align]

    return [join $result \n]
}

# Read the input, process the command line options and output the result.
proc ::tabulate::main {argv0 argv} {
    set data [lrange [split [read stdin] \n] 0 end-1]

    # Input field separator. If none is given treat each line of input as a Tcl
    # list.
    set FS [::tabulate::dict-get-default $argv {} -FS]
    if {$FS ne {}} {
        set updateData {}
        foreach line $data {
            lappend updateData [split $line $FS]
        }
        set data $updateData
        dict unset argv FS
    }

    puts [tabulate -data $data {*}$argv]
}

#ifndef SQAWK
# If this is the main script...
if {[info exists argv0] && ([file tail [info script]] eq [file tail $argv0])} {
    ::tabulate::main $argv0 $argv
}
#endif
