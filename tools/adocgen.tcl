package require textutil
package require cmdline
package require sha256
package require tcl::chan::fifo

#
# Note. There are 3 interpreters active at a time.
# - The master (that is sourcing this file) and is driving the whole process
# - "slave" which is interpreting the scripts that are being documented
#   This may be a safe interpreter depending on whether the -unsafe option
#   is specified or not.
# - "expander" which is a safe interpreter for expanding macros embedded
#   in text fragments. Used for cross references etc.

namespace eval docgen {
    # Keep track of table of contents
    variable toc {}
    
    # default_option_values contains the options to be used if
    # an option is not specified for a command.
    # options contains the values to use for the next execution of a command.
    # option_stack is used for saving and restoring current option values
    # wrapindicator "  \u221f "
    variable default_option_values
    variable options
    array set default_option_values {
        singleline 0
        outputlimit 5
        outputwrap false
        outputindicator "\u2192 "
        wrapindicator "  \u21b3 "
        errorindicator "\u00d8 "
        norun false
        errorok false
    }
    array set options [array get default_option_values]
    variable option_stack
    array set option_stack {}
    
    # slave_stdout_chan holds a virtual channel into which output
    # from the slave interpreter is directed
    variable slave_stdout_chan

    # Utility procs loaded into every slave interpreter
    variable init_script {
        namespace import tcl::mathop::*

        # We would like to just set stdout to point to the virtual channel
        # but can't figure out how to override stdout in slave without
        # also overriding in the master. So override puts instead
        rename ::puts ::_puts
        proc puts args {
            switch [llength $args] {
                1 { set args [list $::stdout_chan [lindex $args 0]] }
                2 {
                    switch -exact -- [lindex $args 0] {
                        -nonewline { set args [linsert $args 1 $::stdout_chan] }
                        stdout -
                        stderr {set args [lreplace $args 0 0 $::stdout_chan]}
                    }
                }
                3 {
                    if {[lindex $args 0] eq "-nonewline" &&
                        [lindex $args 1] in {stdout stderr}} {
                        set args [lreplace $args 1 1 $::stdout_chan]
                    }
                }
            }
            uplevel 1 ::_puts $args
        }
    }
}

# Gets current value of option and resets it to default
proc docgen::get_option_and_reset opt {
    variable default_option_values
    variable options
    set val $options($opt)
    set options($opt) $default_option_values($opt)
    return $val
}

proc docgen::new_slave {} {
    variable init_script
    variable options
    variable slave_stdout_chan

    if {$options(unsafe)} {
        interp create -- slave
    } else {
        interp create -safe -- slave
    }

    set slave_stdout_chan [tcl::chan::fifo]
    chan configure $slave_stdout_chan -buffering none -blocking 0 -translation lf
    interp share {} $slave_stdout_chan slave
    slave eval "set ::stdout_chan $slave_stdout_chan"
    slave eval $init_script
    interp alias slave debug {} ::puts stderr
    if {[file exists [file join $options(scriptdir) book.tcl]]} {
        slave eval [read_file [file join $options(scriptdir) book.tcl]]
    }
}

# In pass 1, executes the expander commands replacing the original content
# with the expanded content. Returns a list.
# In pass 2, executes the docgen commands and returns a string containing
# their output.
proc docgen::docgen {docdef source_changed pass} {
    variable default_option_values
    variable options
    variable slave_stdout_chan
    variable option_stack

    # First pass should not be using a slave interp
    if {$pass == 2} {
        new_slave
    }
    set output {} 
    while {[llength $docdef]} {
        set docdef [lassign $docdef command]

        if {$pass == 2} {
            # Reset any left over from the slave output channel
            read $slave_stdout_chan
            # If source has not changed, do not run anything in pass 2 as output
            # is discarded. The exception is text as we need to parse that
            # to collect table of contents
            if {$command ne "text" && ! $source_changed} {
                continue
            }
        }
        
        switch -exact -- $command {
            include -
            syntax -
            listing -
            shell -
            shell1 -
            fauxshell -
            script -
            xtal -
            xtal_shell -
            uiscript -
            program -
            summary -
            text {
                set arg [getarg $command docdef]
                if {$pass == 1} {
                    lappend output $command [expander eval [list phase1 expand [textutil::undent $arg]]]
                } else {
                    if {[catch {
                        append output [$command $arg]
                    } errormsg]} {
                        puts ERROR:$errormsg$::errorInfo
                        exit1 $command
                    }
                }
            }
            references {
                if {$pass == 1} {
                    # A little awkard because historically this was done in
                    # the expander and just being lazy and reusing that code
                    set expander_text "(([list references [getarg $command docdef]]))"
                    lappend output text [expander eval [list phase1 expand $expander_text]]
                }
            }
            eval {
                set arg [getarg $command docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    slave_eval $arg
                }
            }
            tbd {
                set arg [getarg TBD docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    puts stderr "TBD: [string trim [lindex [split [string trim $arg] \n] 0]]"
                }
            }
            tbw {
                set arg [getarg TBD docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    puts stderr "TBW: [string trim [lindex [split [string trim $arg] \n] 0]]"
                    if {$options(draft)} {
                        append output "\n\n[text $arg]\n\n"
                        append output "\n*This section to be written.*\n"
                    }
                }
            }
            comment {
                # Just a comment
                set arg [getarg $command docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    # Ignore
                }
            }
            singleline -
            prompt -
            norun -
            errorok -
            outputwrap -
            outputlimit {
                set arg [getarg $command docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    set options($command) $arg
                }
            }
            singleline-default -
            outputwrap-default -
            outputlimit-default {
                set arg [getarg $command docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    set command [string range $command 0 end-8]
                    set options($command) $arg
                    set default_option_values($command) $options($command)
                }
            }
            push {
                set opt [getarg push docdef]
                set val [getarg push docdef]
                if {$pass == 1} {
                    lappend output $command $opt $val
                } else {
                    lappend option_stack($opt) $default_option_values($opt)
                    set options($opt) $val
                    set default_option_values($opt) $val
                }
            }
            pop {
                set opt [getarg pop docdef]
                if {$pass == 1} {
                    lappend output $command $opt
                } else {
                    if {[info exists option_stack($opt)] &&
                        [llength $option_stack($opt)]} {
                        set option_stack($opt) [lassign $option_stack($opt) default_option_values($opt)]
                        set options($opt) $default_option_values($opt)
                    }
                }
            }
            softpage {
                set arg [getarg softpage docdef]
                if {$pass == 1} {
                    lappend output $command $arg
                } else {
                    if {[backend] in {docbook docbook5}} {
                        error "Apache FOP does not support dbfo-need PI"
                        append output [passthru "<?dbfo-need height=\"$arg\" ?>"]
                    }
                }
            }
            hardpage {
                if {$pass == 1} {
                    lappend output $command
                } else {
                    append output "\n<<<\n"
                }
            }
            reset {
                if {$pass == 1} {
                    lappend output $command
                } else {
                    interp delete slave
                    new_slave
                }
            }
            default {
                exit1 "Unknown command '[string range $command 0 40]'"
            }
        }
    }

    if {$pass == 2} {
        interp delete slave
    }
    return $output
}

proc docgen::slave_eval {cmd} {
    # Some tutorials deal with object reference counts and how
    # they change. Running $cmd directly affects this to we do a
    # string range to in essence create a new object
    slave eval [string range $cmd 0 end]
}

# Parses callouts from a shell. Returns list of 3 elems -
# script text to be displayed (will not be valid Tcl),
# list of callouts and the last callout index used
proc docgen::extract_shell_callouts {script {callout_index 1}} {
    set display {}
    set callouts {}
    foreach line [split $script \n] {
        if {[regexp {^(.*);\s*#\s*(.*)$} $line -> code comment]} {
            lappend display "[format_code_line $code] <$callout_index>"
            lappend callouts "<$callout_index> $comment"
            incr callout_index
        } else {
            lappend display [format_code_line $line]
        }
    }
    return [list [join $display \n] $callouts $callout_index]
}


# Formats a script for display. Returns list of 3 elems -
# script text to be displayed (will not be valid Tcl),
# list of callouts and the last callout index used
proc docgen::format_script {script lang {formatted_output {}}} {
    variable options

    # Superfluous \'s are for emacs indentation workarounds
    set display_text ""
    set comment_text ""
    set display {}
    set callouts {}
    set callout_index 1
    set collect 1;              # Collecting lines for display
    foreach line [split $script \n] {
        if {[regexp {^\s*#\s*adocgen\s+(\S+)} $line -> collect]} {
            # This source line sets the value of collect
            continue
        }
        if {!$collect} {
            continue
        }
        if {[regexp {^\s*#\s*(.*)$} $line -> comment]} {
            # Pure comment. Treat as documentation text
            if {[llength $display]} {
                append display_text "\n[source_highlight $lang]----\n[join $display \n]\n----\n[join $callouts \n]\n"
            }
            append comment_text "\n$comment"
            set display {}
            set callouts {}
            set callout_index 1
        } else {
            if {[string length $comment_text]} {
                append display_text "\n$comment_text\n"
            }
            set comment_text ""
            if {[regexp {^(.*);\s*#\s*(.*)$} $line -> code comment]} {
                lappend display "[format_code_line $code] <$callout_index>"
                lappend callouts "<$callout_index> $comment"
                incr callout_index
            } else {
                # Only append the code line if it is not just blanks
                # or if it is blanks between non-blank lines
                if {[string length [string trim $line]] ||
                    [llength $display]} {
                    lappend display [format_code_line $line]
                }
            }
        }
    }

    if {[string length $comment_text]} {
        append display_text "\n$comment_text\n"
    }
    if {[llength $display]} {
        append display_text "\n[source_highlight $lang]----\n[join $display \n]\n$formatted_output----\n[join $callouts \n]\n"
    }

    return $display_text
}

# Returns text verbatim except left aligned to shortest common blank prefix
proc docgen::text {text} {
    # TBD - probably do not need undent here since already done in pass 1
    return [textutil::undent $text]
}

proc docgen::summary {text} {
    return "\n\n== Chapter summary\n\n[textutil::undent $text]"
}

proc docgen::format_code_line {line} {
    variable options

    # Split line to a max length. Stick \ at end of all lines except last
    # Leading space has to be preserved so indentation will display properly

    # Find the leading space. Must be a simpler way to do this ?
    if {![regexp -indices {\S} $line first_nonspace_indices]} {
        return "";           # Pure spaces
    }
    set leading_count [lindex $first_nonspace_indices 0]
    set line [string range $line $leading_count end]
    if {$leading_count > 40} {
        set leading_count 40
    }

    # Note is just a rough estimate. Real max is a bit more or less
    set adjust_len [expr {$options(sourcelinelen)-$leading_count}]
    set leading_space [string repeat { } $leading_count]
    set prefix $leading_space

    # We don't use textutil::adjust here because that compresses
    # multiple space into one
    set output_line ""
    while {[string length $line] > $adjust_len} {
        set break [string last { } $line $adjust_len]
        if {$break <= 20} {
            # No space char found or too short. Try looking forward
            set break [string first { } $line $adjust_len]
        }
        if {$break <= 20} {
            # Output all remaining. No reasonable breaking point
            # (do not want very short lines < 20 chars)
            append output_line "${prefix}$line"
            set line ""
        } else {
            append output_line "${prefix}[string range $line 0 $break]"
            set line [string range $line $break+1 end]
        }
        set prefix "\\\n$leading_space    "
    }

    return "${output_line}${prefix}$line"
}

# Returns script marked for output, verifying it runs without errors
proc docgen::script {script {lang Tcl}} {
    variable options
    variable slave_stdout_chan
    
    set line_limit [get_option_and_reset outputlimit]
    set wrap [get_option_and_reset outputwrap]
    set errorok [get_option_and_reset errorok]

    if {[file exists $script]} {
        set fname $script
        set script [read_file $fname]
    }
    set script [textutil::undent [string trim $script \n]]
    set is_error 0
    if {![get_option_and_reset norun]} {
        if {[catch {
            switch -exact -- $lang {
                Xtal {slave_eval "xtal::xtal { $script }"}
                Tcl  {slave_eval $script}
                default {
                    set errorok 0
                    error "Unsupported script language $lang"
                }
            }
        } result]} {
            if {!$errorok} {
                if {[info exists fname]} {
                    error "Error in example script file \"$fname\""
                } else {
                    error "Error in example script \"[string range $script 0 99]...\" ($result)"
                }
            }
            set is_error 1
        } else {
            set is_error 0
        }
    } else {
        set result ""
    }
    
    if {$result ne ""} {
        append result \n
    }
    return "[format_script $script $lang [format_command_output $result[read $slave_stdout_chan] $wrap $is_error $line_limit]]"
}

proc docgen::xtal {script} {
    return [script $script Xtal]
}

# Like script but waits for user to tell when to continue.
proc docgen::uiscript {script {lang Tcl}} {
    variable options

    if {[file exists $script]} {
        set fname $script
        set script [read_file $fname]
    }

    set script [textutil::undent [string trim $script \n]]
    if {![get_option_and_reset norun]} {
        if {[catch {
            if {! $options(noui)} {
                package require Tk
                wm withdraw .
                slave_eval $script
                set ok [tk_messageBox -type yesno -title "adocgen uiscript test" -message "The following test script is running:\n[string range $script 0 400]\n...\n\nYou should have seen a corresponding window. Click Yes if the window was displayed correctly, else No."]
                if {! $ok} {
                    error "User indicated script failed to run successfully."
                }
            }
        } msg]} {
            if {[info exists fname]} {
                error "Error in example script file \"$fname\" ($msg)"
            } else {
                error "Error in example script \"[string range $script 0 99]...\" ($msg)"
            }
        }
    }
    return [format_script $script $lang]

    puts "\n=====\n$script===\n"
    set script [format_script $lang $script]
    puts "\n=====\n$script===\n"
    return $script
}

# Removes all comments and returns formatted as code. Unlike
# script and uiscript, does not run the program
proc docgen::listing {script {lang Tcl}} {
    variable options

    if {[file exists $script]} {
        switch -nocase -- [file extension $script] {
            .tcl {set file_heading "# "; set lang Tcl }
            .xtal {set file_heading "# "; set lang Xtal }
            .vbs {set file_heading "' "; set lang VBScript}
            .bat -
            .cmd {set file_heading "rem "; set lang BAT}
            default {set file_heading "# "}
        }
        append file_heading "[file tail $script]\n"
        set script [read_file $script]
    } else {
        set file_heading ""
    }

    set script [textutil::undent [string trim $script \n]]

    if {$lang eq "Tcl"} {
        regsub -all -line {^\s*#.*$} $script {} script
        regsub -all -line {;\s*#.*$} $script {} script

        set lines {}
        foreach line [split $script \n] {
            lappend lines [format_code_line $line]
        }
        set script [join $lines \n]
    }

    regsub -all -line "\n{2,}" [string trim $script] \n\n script

    return "\n[source_highlight $lang]----\n${file_heading}$script\n----\n"
}

# Displays a syntax block
proc docgen::syntax {script {lang Tcl}} {
    variable options

    if {[file exists $script]} {
        switch -nocase -- [file extension $script] {
            .tcl {set file_heading "# "; set lang Tcl }
            .xtal {set file_heading "# "; set lang Xtal }
            .vbs {set file_heading "' "; set lang VBScript}
            .bat -
            .cmd {set file_heading "rem "; set lang BAT}
            default {set file_heading "# "}
        }
        append file_heading "[file tail $script]\n"
        set script [read_file $script]
    } else {
        set file_heading ""
    }

    set script [textutil::undent [string trim $script \n]]

    return "\n\[subs=\"normal\"\]\n[source_highlight $lang]----\n$script\n----\n"
}

proc docgen::include {path} {
    return [read_file $path]
}

proc docgen::format_command_output {lines wrap is_error args} {
    variable options
    if {$is_error} {
        set sep $options(errorindicator)
    } else {
        set sep $options(outputindicator)
    }
    set wrapper " $options(wrapindicator)"
    set output ""

    set omit_lines 0
    if {[llength $args]} {
        lassign $args line_limit
        set lines [split [textutil::untabify2 $lines] \n]
        if {[lindex $lines end] eq ""} {
            set lines [lrange $lines 0 end-1]
        }
        if {$line_limit} {
            if {[llength $lines] > $line_limit} {
                set lines [lrange $lines 0 $line_limit-1]
                set omit_lines 1
            }
        }
        
    }
    foreach line $lines {
        # Convert ^Z else programs will treat it as end of file.
        set line [string map [list \x1a "^Z"] $line]
        if {[string length $line] > $options(sourcelinelen)} {
            if {$wrap} {
                foreach line [split [textutil::adjust $line -length $options(sourcelinelen)] \n] {
                    append output "$sep$line\n"
                    set sep $wrapper
                }
                set sep "  "
            } else {
                set line [string range $line 0 [expr {$options(sourcelinelen)-4}]]...
                append output "$sep$line\n"
                set sep "  "
            }
        } else {
            append output "$sep$line\n"
            set sep "  "
        }
    }

    # Note that if line_limit was negative we don't even output
    # the warning because output was not intended to be shown
    if {$omit_lines && $line_limit >= 0} {
        append output "...Additional lines omitted...\n"
    }

    return $output
}

# Returns script as though run interactively
proc docgen::shell {script {lang Tcl}} {
    variable options
    variable slave_stdout_chan

    if {[file exists $script]} {
        set script [read_file $script]
    }

    set cmd ""
    set output ""
    set callouts {}
    set callout_index 1
    set singleline [get_option_and_reset singleline]
    set line_limit [get_option_and_reset outputlimit]
    set wrap [get_option_and_reset outputwrap]
    set prompt [get_option_and_reset prompt]
    set errorok [get_option_and_reset errorok]
    foreach line [split [textutil::undent [string trim $script \n]] \n] {
        if {$cmd ne ""} {
            append cmd \n;      # For formatting to come out right
        }
        append cmd $line
        if {[info complete $cmd]} {
            if {$singleline && [string trim $cmd] eq ""} {
                # If single line mode, ignore blank commands. Note
                # need to trim because of shell1 does padding with
                # spaces to align output
                continue
            }
            lassign [extract_shell_callouts $cmd $callout_index] display callouts2 callout_index
            lappend callouts {*}$callouts2
            if {[catch {
                switch -exact -- $lang {
                    Tcl {interp eval slave $cmd}
                    Xtal {interp eval slave [list xtal::xtal $cmd]}
                    default {
                        set errorok 0
                        error "$lang execution not supported"
                    }
                }
            } result]} {
                if {! $errorok} {
                    fail "Error in example script \"[string range $cmd 0 99]...\" ($result)"
                }
                set is_error 1
            } else {
                set is_error 0
            }
            set stdout_content [read $slave_stdout_chan]
            if {$singleline && $stdout_content eq "" && $result eq ""} {
                set lines "(empty)"
            } else {
                set lines "$stdout_content$result"
            }
            if {$singleline} {
                # If there is a callout, we have to move it to the last
                # part of the line after the output display
                if {[regexp {^(.*)(\s<\d+>\s*)$} $display -> prefix suffix]} {
                    append output "$prompt$prefix "
                    set callout_suffix $suffix
                } else {
                    append output "$prompt$display"
                    # If there was no output, we need to tack on a newline
                    # ourselves else just a space to separate the output
                    # indicator
                    if {[string length $lines]} {
                        append output " "
                    } else {
                        append output \n
                    }
                    set callout_suffix ""
                }
            } else {
                append output "$prompt$display\n"
                set callout_suffix ""
            }
            if {[string length $callout_suffix]} {
                # Callout suffix has to come BEFORE the ending newlines
                append output [string trimright [format_command_output $lines $wrap $is_error $line_limit]] $callout_suffix \n
            } else {
                append output [format_command_output $lines $wrap $is_error $line_limit] $callout_suffix
            }
            set cmd ""
        }
    }
    if {[string length [string trim $cmd]]} {
        error "Incomplete line \"$cmd\""
    }
    # TBD - Is there a "console" block type instead of [source] ?
    return "\n[source_highlight $lang]----\n[string trim $output]\n----\n[join $callouts \n]\n"
}

proc docgen::shell1 {script} {
    variable options
    set options(singleline) 1
    set options(prompt) ""
    # Make all lines same length
    set len 0
    set lines [split $script \n]
    foreach line $lines {
        # If there is a following comment, exclude it
        if {[regexp {^(.*);\s*#\s*(.*)$} $line -> code comment]} {
            set line $code
        }
        if {[string length $line] > $len} {
            set len [string length $line]
        }
    }
    set pad [string repeat { } $len]
    set script [join [lmap line $lines {
        if {[regexp {^(.*);\s*#\s*(.*)$} $line -> code comment]} {
            append code $pad
            return -level 0 "[string range $code 0 $len-1]; # $comment"
        } else {
            string range "$line$pad" 0 $len-1
        }
    }] \n]
    return [shell $script]
}

proc docgen::xtal_shell {script} {
    return [shell $script Xtal]
}

# Returns shell script as though run interactively without actually running it
# Any line with a % is a assumed to be command. Lines without % are output
# of previous command
proc docgen::fauxshell {script {lang Tcl}} {
    variable options

    if {[file exists $script]} {
        set script [read_file $script]
    }

    set cmd ""
    set output ""
    set callouts {}
    set callout_index 1
    set wrap [get_option_and_reset outputwrap]
    set prompt [get_option_and_reset prompt]
    set line_limit [get_option_and_reset outputlimit]
    set command_output {}
    foreach line [split [textutil::undent [string trim $script \n]] \n] {
        if {$cmd ne ""} {
            # We are parsing a command
            append cmd \n;      # For formatting to come out right
            append cmd $line
        } else {
            # We are not parsing a command. May be either output of
            # previous command or new command.
            if {[string index $line 0] eq "%"} {
                # Print out output of previous command if any
                append output [format_command_output $command_output $wrap 0]
                set nlines 0
                set command_output {}
                set cmd [string trim [string range $line 1 end]]
            } else {
                incr nlines
                if {$line_limit == 0 || $nlines <= $line_limit} {
                    lappend command_output $line
                } elseif {$nlines == ($line_limit+1)} {
                    lappend command_output "...Additional lines omitted...\n"
                } else {
                    # Ignore output line
                }
                continue
            }
        }

        if {[info complete $cmd]} {
            lassign [extract_shell_callouts $cmd $callout_index] display callouts2 callout_index
            lappend callouts {*}$callouts2
            append output "$prompt$display\n"
            set cmd ""
        }
    }
    if {[string length $cmd]} {
        error "Incomplete line \"$cmd\""
    }
    append output [format_command_output $command_output $wrap 0]

    # TBD - Is there a "console" block type instead of [source] ?
    return "\n[source_highlight $lang]----\n[string trim $output]\n----\n[join $callouts \n]\n"
}


# Runs an external program and prints result
proc docgen::program {script} {
    variable options

    set errorok [get_option_and_reset errorok]
    
    if {[file exists $script]} {
        set fname $script
        set script [read_file $fname]
    }

    set script [textutil::undent [string trim $script \n]]
    set program [auto_execok [lindex $script 0]]
    # Try for some minimal protection, only against inadvertent errors.
    # From a safe interp would not be allowed to exec anyways
    # TBD - Eewww. Fix these checks to be at least a bit palatable.
    if {![regexp -nocase {(^|/)(hello|cmd.exe /c (type|dir|copy|rename)|move|tclkit-cli|cscript|wish.*|tclsh.*|logman|wevtutil|xperf|ls)(\.exe)?$} $program]} {
        error "[lindex $script 0] ($program) not in allowed list of external programs"
    }

    if {$::tcl_platform(platform) eq "windows"} {
        # For Windows \\->\ for display purposes
        set script_display [string map {\\\\ \\} $script]
    } else {
        set script_display $script
    }
    set output "[string tolower [file nativename [pwd]]]> $script_display\n"

    if {![get_option_and_reset norun]} {
        if {[catch {
            exec {*}$program {*}[lrange $script 1 end]
        } program_output]} {
            if {! $errorok} {
                if {[info exists fname]} {
                    error "Error in example script file \"$fname\""
                } else {
                    error "Error in example script \"[string range $script 0 99]...\" ($program_output)"
                }
            }
            set program_output "$options(errorindicator)$program_output"
        }
        set line_limit [get_option_and_reset outputlimit]
        set wrap [get_option_and_reset outputwrap]
        set lines [split $program_output \n]
        set omit_lines 0
        if {$line_limit} {
            if {[llength $lines] > $line_limit} {
                set lines [lrange $lines 0 $line_limit-1]
                set omit_lines 1
            }
        }
    
        append output [format_command_output $lines $wrap 0]
        if {$omit_lines} {
            append output "...Additional lines omitted...\n"
        }
    }

    # TBD - Is there a "console" block type instead of [source] ?
    return "\n[source_highlight {}]----\n[string trim $output]\n----\n"
}

proc docgen::getarg {command argsvar} {
    upvar 1 $argsvar var

    if {[llength $var] == 0} {
        error "No value specified for command $command"
    }

    set var [lassign $var arg]
    return $arg
}


proc usage {} {
    return "Usage: [file tail [info nameofexecutable]] $::argv0 ?options? INPUTFILE1 ?INPUTFILE2...?"
}

proc exit1 {msg} {
    if {$msg ne ""} {
        puts stderr $msg
    }
    exit 1
}

proc docgen::source_highlight {{lang {}}} {
    variable options
    if {$lang eq ""} {
        if {$options(highlight) eq ""} {
            return ""
        } else {
            return "\[source,$options(highlight)\]\n"
        }
    } else {
        if {$options(highlight) eq ""} {
            return "\[source,$lang\]\n"
        } else {
            return "\[source,$lang,$options(highlight)\]\n"
        }
    }
}

proc docgen::process_files files {
    variable options
    variable current_file
    variable toc

    if {[llength $files] == 0} {
        # Treat as error since empty list may be because no files
        # match due to mistyping
        error "No matching files"
    }

    array set content {}
    array set sha {}
    # Do pass 1 processing to collect links. We have collect links
    # for all files BEFORE processing them to handle cross-file links
    foreach fn $files {
        progress "Processing $fn Pass 1"
        flush stdout
        set current_file $fn
        if {[catch {
            set raw_content [read_file $fn]
            set sha($fn) [sha2::sha256 -hex -- $raw_content]
            # Run Pass 1 to replace (()) commands
            set content($fn) [docgen::docgen $raw_content 1 1]
        } msg]} {
            error "Error pass1 $fn: $msg" $::errorInfo
        }
    }                      
    
    foreach fn $files {
        progress "Processing $fn Pass 2"
        set current_file $fn
        if {$options(outdir) eq ""} {
            set outfn [file rootname $fn].ad
        } else {
            file mkdir $options(outdir); # Make sure it exists
            set outfn [file join $options(outdir) [file rootname [file tail $fn]].ad]
        }
        # If file already exists, see if it was created from this exact source
        # in which case we do not need to re-run the code
        set source_changed 1
        if {$options(cache) && [file exists $outfn]} {
            set fd [open $outfn r]
            set sig_line [gets $fd]
            close $fd
            if {[regexp {^// adocgen sig ([[:xdigit:]]{64})$} $sig_line -> sig]} {
                if {[string equal -nocase $sig $sha($fn)]} {
                    # Source unchanged, no need to regenerate output
                    set source_changed 0
                }
            }
        }

        if {[catch {
            expander eval [list phase2 expand [docgen::docgen $content($fn) $source_changed 2]]
        } output]} {
            error "Error pass2 $fn: $output" $::errorInfo
        }
        if {! $options(dryrun) && $source_changed } {
            if {! $options(overwrite) && [file exists $outfn]} {
                error "File $outfn already exists"
            }
            set fd [open $outfn w]
            chan configure $fd -encoding utf-8
            puts $fd "// adocgen sig $sha($fn)"
            puts $fd $output
            close $fd
        }
    }

    if {$options(maketoc) ne ""} {
        if {$options(outdir) eq ""} {
            set tocfn $options(maketoc)
        } else {
            set tocfn [file join $options(outdir) $options(maketoc)]
        }
        set fd [open $tocfn w]
        chan configure $fd -encoding utf-8
        dict for {target tocentry} $toc {
            set target [file rootname [file tail $target]].html
            set text ". link:${target}\[[dict get $tocentry text]\]"
            if {$options(byline) && [dict exists $tocentry author]} {
                append text " \[small\]#_[dict get $tocentry author]_#"
            }
            puts $fd $text
        }
        close $fd
    }

    if {$options(makemaster) ne ""} {
        if {$options(outdir) eq ""} {
            set incfn $options(makemaster)
        } else {
            set incfn [file join $options(outdir) $options(makemaster)]
        }
        set fd [open $incfn w]
        chan configure $fd -encoding utf-8
        dict for {target tocentry} $toc {
            set target [file rootname [file tail $target]].ad
            puts $fd "\ninclude::${target}\[\]"
        }
        close $fd
    }
}

proc docgen::read_file {path {eofmarker "\n<eof>"}} {
    set fd [open $path]
    set content [read $fd]
    close $fd
    if {$eofmarker ne ""} {
        set eof [string first $eofmarker $content]
        if {$eof >= 0} {
            return [string range $content 0 $eof-1]
        }
    }
    return $content
}

proc docgen::progress {msg} {
    puts stdout $msg
    flush stdout
}

proc docgen::warn {msg} {
    puts stderr $msg
}

proc docgen::fail {msg} {
    puts stderr $msg
    puts stderr $::errorInfo
    error $msg
}

proc docgen::getoption {optname} {
    variable options
    return $options($optname)
}

proc docgen::get_current_file {} {
    variable current_file
    return $current_file
}

proc docgen::add_to_toc {file key text} {
    variable toc

    dict set toc $file $key $text
}

proc docgen::backend_docbook {} {
    if {[backend] in {docbook docbook5}} {
        return 1
    } else {
        return 0
    }
}

proc docgen::backend {} {
    return [getoption backend]
}

# Generate pass: macro for ascii doc. We have to protect
# if text contains special characters
proc docgen::passthru {text} {
    # Do NOT escape [ else the \ will show up in output
    # | is mapped otherwise asciidoc table markup gets confused
    set text [string map [list \] \\\] | "&#124;"] $text]
    return "pass:\[$text\]"
}

proc docgen::expander_init {} {
    # Use safe::interpCreate instead of interp create -safe because
    # we want to be able to load packages
    safe::interpCreate expander
    interp alias expander fail {} docgen::fail
    interp alias expander warn {} docgen::warn
    interp alias expander getoption {} docgen::getoption
    interp alias expander get_current_file {} docgen::get_current_file
    interp alias expander add_to_toc {} docgen::add_to_toc
    interp alias expander passthru {} docgen::passthru
    interp alias expander backend {} docgen::backend
    interp alias expander backend_docbook {} docgen::backend_docbook
    expander eval {
        package require textutil::expander
        global links
        array set links {}
        global delimiters
        array set delimiters {start1 (( end1 )) start2 ((2 end2 ))}
        global cmdrefs
        set cmdrefs [list ]

        # sections is used to keep track of sections if they
        # are created via the ((==)) commands as opposed to literal
        # == in the text. This currently used only for index generation.
        # Each element is a pair consisting of the section
        # depth and a dictionary whose elements are:
        #   Id - the section id
        #   IndexIds - id's of index entries for this section
        global sections
        set sections [list ]

        textutil::expander phase1
        phase1 setbrackets $delimiters(start1) $delimiters(end1)
        textutil::expander phase2
        phase2 setbrackets $delimiters(start2) $delimiters(end2)

        # If args supplied, they are added as *separate* index terms,
        # Each is of the form "primary,secondary,tertiary" ie.
        # using , to separate and with only primary being mandatory
        proc header {level id {text ""} args} {
            global sections

            if {$id eq "*"} {
                set id id_[info cmdcount]
            }
                
            set indexterms {}
            set endterms {}
            
            # Pop off all section entries at this level or lower
            # if this is a section header. Otherwise, for . entries
            # we do not pop off anything
            if {$level in {.Table .}} {
                set depth 100000; # Some random high number
            } else {
                set depth [string length $level]
            }
            
            # Pop off sections at the same or nested levels
            while {[llength $sections]} {
                set index_ids {}
                if {[lindex $sections end 0] < $depth} {
                    break
                }
                # Pop the section
                set section [lindex $sections end]
                set sections [lrange $sections[set sections ""] 0 end-1]
                # If the popped section had any index entries, close them.
                if {[backend_docbook]} {
                    if {[dict exists [lindex $section 1] IndexIds]} {
                        foreach index_id [dict get [lindex $section 1] IndexIds] {
                            append endterms [passthru "<indexterm class='endofrange' startref='$index_id'></indexterm>"]
                        }
                    }
                }
            }
            
            # Collect the index entries for this section if any
            set index_ids {}
            if {[indices_supported]} {
                if {[backend_docbook]} {
                    append indexterms [join [lmap term $args {
                        set index_id id_index_[info cmdcount]
                        lappend index_ids $index_id
                        indexterm [split $term ,] $index_id
                    }] " "]
                } else {
                    if {[llength $args]} {
                        warn "Backend does not support index terms in section titles (section $id, $text)"
                    }
                }
            }
            # Push this section on to the top
            lappend sections [list $depth [list Id $id IndexIds $index_ids]]
            
            if {[info exists ::links($id)]} {
                warn "Duplicate link anchor $id"
            }

            if {[backend_docbook]} {
                # For Docbook, we want to pick up text from docbook stylesheet
                # configuration. Set link text to empty else it will
                # asciidoctor will output a xreflabel that overrides
                # the stylesheet.
                set ::links($id) [list [get_current_file] ""]
            } else {
                # We do not want the link text to include command names etc.
                # For example "File statistics: `file stat`, `file lstat`"
                # should generate link text of "File statistics" 
                set ::links($id) [list [get_current_file] [regsub {^(.*[^:]):\s.*$} $text \\1]]
            }
            if {0} {
                Not really. It was the non-transparent background in the
                stylesheet for code that was causing this
                # Also get rid of `` as font changes mess up text width calculations
                # at least for the docbook backend.
                set ::links($id) [string map {` {}} $::links($id)]
            }

            set adoc "$endterms\n"
            if {$level eq "."} {
                # Header that does not go into ToC and can follow any level
                if {[backend_docbook]} {
                    append adoc "\[\[$id\]\]\n\[discrete\]\n===== $text $indexterms";
                } else {
                    append adoc "\[discrete\]\n===== $text \[\[$id\]\]$indexterms"; # No space after.
                }
            } elseif {$level eq ".Table"} {
                if {[backend_docbook]} {
                    append adoc "\[\[$id\]\]\n.$text $indexterms";
                } else {
                    append adoc ".$text \[\[$id\]\]$indexterms"; # No space after.
                }
                
            } else {
                if {$level eq "="} {
                    if {[llength $args]} {
                        error "Top level heading cannot have index terms as the document header has to come right after and inserting an index term may have unknown consequences"
                    }
                    add_to_toc [get_current_file] text $text
                    if {[getoption draft]} {
                        append text " \[small\]#\[DRAFT\]#"
                    }
                }
                if {[backend_docbook]} {
                    append adoc "\[\[$id\]\]\n$level $text$indexterms"
                } else {
                    append adoc "\[\[$id, $text\]\]\n$level $text$indexterms"
                }
            }

            if {[regexp {={2,}} $level] &&
                [string match -nocase *html* [backend]]} {
                switch -exact -- $level {
                    == {set css_class ta_toplink_h2}
                    === {set css_class ta_toplink_h3}
                    default { set css_class ta_toplink }
                }
                return "pass:\[<a class='$css_class' href='#top'>Top&nbsp;&uarr;</a>\]\n$adoc"
            } else {
                return $adoc
            }
        }

        interp alias {} . {} header .
        interp alias {} .Table {} header .Table
        interp alias {} = {} header =
        interp alias {} == {} header ==
        interp alias {} === {} header ===
        interp alias {} ==== {} header ====
        interp alias {} ===== {} header =====

        # Pass1 - Add a link
        proc "^" {id {text ""}} {
            if {$text eq ""} {
                return "$::delimiters(start2)[list link2 $id]$::delimiters(end2)"
            } else {
                return "$::delimiters(start2)[list link2 $id $text]$::delimiters(end2)"
            }
        }

        proc see {id} {
            return "(see [^ $id])"
        }
        
        # Pass1 = Add a link to text formatted as code
        proc "^`" {id text} {
            # Surround text with backquotes to mark as code. We do it this
            # way so output is <code><a>....</a></code> rather than
            # the other way around which makes fopub formatting broken
            return "`[^ $id $text]`"
        }

        # replace link in pass 2
        proc link2 {id {text ""}} {
            if {![info exists ::links($id)]} {
                warn "Warning: Link $id not defined."
                if {$text ne ""} {
                    return "<<$id, $text>>"
                } else {
                    # For docbook, we set id text to empty so docbook
                    # config for links is obeyed
                    if {![backend_docbook]} {
                        warn "Warning:  Link $id text is an empty string"
                    }
                    return "<<$id>>"
                }
            }
            lassign $::links($id) target_file target_text
            if {$text ne ""} {
                set target_text $text
            } elseif {$target_text eq ""} {
                # For docbook, we set id text to empty so docbook
                # config for links is obeyed
                if {![backend_docbook]} {
                    warn "Warning:  Link $id text is an empty string"
                }
            }
            if {$target_text eq ""} {
                return "<<$id>>"
            }
            # If link target is the same file (only use file name as comparison)
            # a simple ref is enough.
            set target_file [file tail [file rootname $target_file]]
            set current_file [file tail [file rootname [get_current_file]]]
            if {[string equal -nocase $target_file $current_file]} {
                # Link within the same file
                return "<<$id, $target_text>>"
            } else {
                # Link to a different file. When creating a book, the 
                # different files may actually land up in the same
                # output file. asciidoctor handles this correctly.
                # TBD - check what asciidoc does
                # Note the file extension need not be specified
                return "<<${target_file}#$id, $target_text>>"
            }
        }

        # anchor
        proc a {id {text ""}} {
            if {[info exists ::links($id)]} {
                warn "Duplicate link anchor $id"
            }
            set ::links($id) [list [get_current_file] $text]
            return "\[\[$id\]\]$text"
        }

        proc ref {id {text ""}} {
            if {[info exists ::links($id)]} {
                warn "Duplicate link anchor $id"
            }

            set text [string trim $text]
            if {$text eq ""} {
                set text $id
            }
            set ::links($id) [list [get_current_file] $text]
            return "\[\[$id\]\]$id"
        }

        # Escape XML char entities - from wiki
        proc xmlesc {text {attribute 0} } {
            if {$attribute} {
                return [string map {& &amp; < &lt; > &gt; \' &apos; \" &quot;} $text]
            } else {
                return [string map {& &amp; < &lt; > &gt;} $text]
            }
        }

        # Index entries
        # $terms is of a list of primary term, secondary term, tertiary term
        # where the secondary and tertiary terms may be missing.
        # `` are replaced by <literal></literal>
        proc indexterm {terms {id {}}} {
            if {![indices_supported]} {
                # Other back ends do not support indices
                return ""
            }
            set nterms [llength $terms]
            if {$nterms == 0 || $nterms > 3} {
                error "Number of index terms must be between 1 and 3. Terms: [join $terms {, }]"
            }

            if {[backend] eq "pdf"} {
                # PDF backend gets confused by * in indexterm
                set first_term [lindex $terms 0]
                regsub -all {\*} $first_term {{asterisk}} first_term
                set first_term [string map {< "{lt}"} $first_term]
                regsub -all {[\]#~]} $first_term {\\\0} first_term
                lset terms 0 $first_term
                return "indexterm:\[[join $terms ,]\]"
            } 

            # Backend is docbook.
            # Replace `` with <literal> since this is not seen by asciidoctor
            # and we have to display the literal font ourselves
            set terms [lmap term $terms {
                string trim [regsub -all {`([^`]*)`} [xmlesc $term] {<literal>\1</literal>}]
            }]

            # Asciidoc(tor) automatically duplicate secondary terms to
            # primary levels which we do not want so use a passthrough
            if {$id eq ""} {
                append xml "<indexterm>"
            } else {
                # The corresponding class='endofrange' is generated
                # by the header commands.
                # We do not use zone attribute because not supported
                # by Apache FOP
                append xml "<indexterm class='startofrange' id='$id'>"
            }
            append xml "<primary>[lindex $terms 0]</primary>"
            if {$nterms > 1} {
                append xml "<secondary>[lindex $terms 1]</secondary>"
            }
            if {$nterms > 2} {
                append xml "<tertiary>[lindex $terms 2]</tertiary>"
            }
            append xml "</indexterm>"
            return [passthru $xml]
        }

        # Single index entry and also display it in text. If
        # $secondary not empty, the index is two-level but
        # secondary is not included in displayed text
        proc + {entry {secondary {}}} {
            # Displays inline text
            if {![indices_supported]} {
                return $entry
            }
            if {$secondary eq ""} {
                return "indexterm2:\[$entry\]"
            } else {
                return "[++ $entry $secondary]$entry"
            }
        }
        
        # Index entries. args is list of secondary terms.
        proc ++ {primary args} {
            if {[llength $args] == 0} {
                return [indexterm [list $primary]]
            }
            set xml {}
            foreach secondary $args {
                append xml [indexterm [list $primary $secondary]]
            }
            return $xml
        }

        # Tertiary index entries
        proc +++ {primary secondary args} {
            if {[llength $args] == 0} {
                return [indexterm [list $primary $secondary]]
            }
            set xml {}
            foreach tertiary $args {
                append xml [indexterm [list $primary $secondary $tertiary]]
            }
            return $xml
        }

        # Index entry for a class
        proc +class {cls} {
            return [string cat [++ "`$cls` class"] [++ classes `$cls`] `$cls`]
        }

        # Index entry for a method
        proc +method {meth {cls {}}} {
            if {$cls eq ""} {
                return [string cat [++ "`$meth` method"] `$meth`]
            } else {
                return [string cat [++ "`$meth` method" "`$cls` class"] `$meth`]
            }
        }

        # Index entry for a package
        proc +pkg {pkg} {
            return [string cat [++ "`$pkg` package"] [++ packages `$pkg`] `$pkg`]
        }

        # Environment variable
        proc +env {var} {
            return [string cat \
                        [++ "`$var` environment variable"] \
                        [++ "environment variables" `$var`] \
                        `$var`]
        }

        # Index entry for a global variable
        proc +global {var} {
            return [string cat \
                        [++ "`$var` global variable"] \
                        [++ "global variables" `$var`] \
                        `$var`]
        }

        # Index entry for a command
        proc +cmd {cmd args} {
            switch -exact -- [llength $args] {
                0 {
                    # For namespace commands generate index for both forms -
                    # with and without namespace. We do not do this for
                    # ensemble commands as index nesting gets deep
                    set ns [namespace qualifiers $cmd]
                    if {$ns eq ""} {
                        return [string cat [++ "`$cmd` command"] `$cmd`]
                    } else {
                        set ns [string trimleft $ns ::]
                        set tail [namespace tail $cmd]
                        return [string cat [++ "`$cmd` command"] [++ "`$tail` command" "in namespace $ns"] `$cmd`]
                    }
                }
                1 {
                    set subcmd [lindex $args 0]
                    return [string cat [++ "`$cmd` command" "`$subcmd` subcommand"] "`$cmd $subcmd`"]
                }
                2 {
                    lassign $args subcmd subsub
                    return [string cat [+++ "`$cmd` command" "`$subcmd` subcommand" "`$subsub` subcommand"] "`$cmd $subcmd $subsub`"]
                }
                3 {
                    error "Too many arguments"
                }
            }
        }

        # Index entry AND a hyperlink for a command
        proc +^cmd {link cmd} {
            lassign $cmd maincmd subcmd
            set link [^` $link $cmd]
            if {$subcmd eq ""} {
                set index [++ "`$maincmd` command"]
            } else {
                set index [++ "`$maincmd` command" "`$subcmd` subcommand"]
            }
            return "$index$link"
        }

        # Index entry for a command option
        proc +opt {cmd opt} {
            if {[llength $cmd] == 1} {
                return [string cat [++ "`$cmd` command" "`$opt` option"] "`$opt`"]
            } else {
                set subcmd [lassign $cmd cmd]
                return [string cat [+++ "`$cmd` command" "`$subcmd` subcommand" "`$opt` option"] "`$opt`"]
            }
        }
        
        # Introspection
        proc +introspect name {
            return [string cat [++ introspection $name] [++ $name introspection]]
        }
        
        # See another index entry
        proc +see {term other {secondary {}}} {
            if {! [indices_supported]} {
                return ""
            }

            if {[backend_docbook]} {
                # Asciidoctor does not process markthrough so convert
                # `` to <literal></literal> ourselves
                set term [string trim [regsub -all {`([^`]*)`} [xmlesc $term] {<literal>\1</literal>}]]
                set secondary [string trim [regsub -all {`([^`]*)`} [xmlesc $secondary] {<literal>\1</literal>}]]
                set other [string trim [regsub -all {`([^`]*)`} [xmlesc $other] {<literal>\1</literal>}]]
                append xml "<indexterm><primary>$term</primary>"
                if {$secondary ne ""} {
                    append xml "<secondary>$secondary</secondary>"
                }
                append xml "<see>$other</see></indexterm>"
                return [passthru $xml]
            } else {
                # PDF backend has no "see OTHER" index type.
                warn "PDF output does not support SEE index terms ($term)"
                return ""
            }
        }
        
        # See also index entry
        proc +also {term also_term {secondary {}}} {
            if {![indices_supported]} {
                return ""
            }
            
            if {[backend_docbook]} {
                # Asciidoctor does not process markthrough so convert
                # `` to <literal></literal> ourselves
                set term [string trim [regsub -all {`([^`]*)`} [xmlesc $term] {<literal>\1</literal>}]]
                set secondary [string trim [regsub -all {`([^`]*)`} [xmlesc $secondary] {<literal>\1</literal>}]]
                set also_term [string trim [regsub -all {`([^`]*)`} [xmlesc $also_term] {<literal>\1</literal>}]]
                set xml "<indexterm><primary>$term</primary>"
                if {$secondary ne ""} {
                    append xml "<secondary>$secondary</secondary>"
                }
                append xml "<seealso>$also_term</seealso></indexterm>"
                return [passthru $xml]
            } else {
                warn "PDF output does not support SEEALSO index terms ($term)"
                return "";      # PDF backend does not support Also index terms - TBD
            }

        }
        
        # Helper for outputting special characters
        proc chars {s} {
            return [subst -novariables -nocommands $s]
        }
        
        proc cmddef {id text {level 0}} {
            lappend ::cmdrefs $id
            return [header [string repeat = [incr level 4]] $id $text]
        }

        proc cmdrefs2 {} {
            # Should be called in second pass to generate links to commands
            if {[llength $::cmdrefs]} {
                if {0} {
                    set table "|===\n\n"
                    foreach id $::cmdrefs {
                        append table | [link2 $id] \n\n
                    }
                    append table "|===\n\n"
                    return $table
                } else {
                    set ul "\[no-bullet\]\n"
                    foreach id $::cmdrefs {
                        append ul "* [link2 $id]\n"
                    }
                    return "$ul\n"
                }
            } else {
                return ""
            }
        }

        proc cmdrefs {} {
            return "$::delimiters(start2)[list cmdrefs2]$::delimiters(end2)"
        }

        # Defines author
        proc author {author_name} {
            add_to_toc [get_current_file] author $author_name
            if {[getoption doctype] ne "book"} {
                return "\[small\]#_Copyright (C) [clock format [clock seconds] -format %Y] $author_name. All rights reserved._#"
            } else {
                return ""
            }
        }
        
        proc image {id img {text ""}} {
            # NOTE: image is split into a pass1 and a pass2. The pass1
            # is needed to collect links. Actual text subst is in pass2
            # because the substituted text will contain newlines which
            # cause problem with indent calculations if done in pass1.

            if {[info exists ::links($id)]} {
                warn "Duplicate link anchor $id"
            }
            if {[backend_docbook]} {
                # For Docbook, we want to pick up text from docbook stylesheet
                # configuration. Set link text to empty else it will
                # asciidoctor will output a xreflabel that overrides
                # the stylesheet.
                set ::links($id) [list [get_current_file] ""]
            } else {
                set ::links($id) [list [get_current_file] $text]
            }
            # Note we set up image2 to be called in pass2
            return "$::delimiters(start2)[list image2 $id $img $text]$::delimiters(end2)"
        }

        proc image2 {id img text} {
            if {$text ne ""} {
                set title ".$text\n"
            } else {
                set title ""
            }
            return "\[\[$id\]\]\n${title}image::$img\[align=\"center\"\,alt=\"$text\"\]"
        }

        proc ui {text} {
            return "\[ui\]#$text#"
        }
        
        proc indices_supported {} {
            if {[backend] in {pdf docbook docbook5}} {
                return 1
            } else {
                return 0
            }
        }
        
        # Generates a references section
        proc references {text} {
            if {[llength $text] == 0} {
                return ""
            }
            append display_text "\n\n== References\n"
            # TBD - docbook pdf output overflows left column append display_text "\[horizontal\]\n"
            foreach {id title description} $text {
                append display_text "[ref $id $title]::\n"
                append display_text "_[string trim $title]_"
                if {[string length $description]} {
                    # Get rid of newline and leading spaces as it will
                    # make asciidoc it is list or literal text
                    append display_text ", [regsub -all {\s+} $description { }]"
                }
                append display_text "\n"
            }
            return $display_text
        }
    }
}

if {[catch {
    array set docgen::default_option_values [cmdline::getoptions argv {
        {byline          "Show author in Table of Contents"}
        {cache           "Use cache for unchanged documents (WARNING: does NOT account for changes in includes"}
        {doctype.arg article "Document type - article or book"}
        {draft           "Mark as a draft"}
        {dryrun          "Do not actually write output files"}
        {backend.arg html5 "Output backend - as passed to asciidoctor -b option"}
        {highlight.arg "" "Source highlighter to use"}
        {maketoc.arg ""  "Name of file to store table of contents entries"}
        {makemaster.arg "" "Name of file to store master document"}
        {noui            "Do not run scripts that require a UI"}
        {outdir.arg  ""  "Directory for output files"}
        {overwrite       "Force - overwrite any existing files"}
        {prompt.arg "% " "Prompt to show in shell commands"}
        {sourcelinelen.arg 0 "Max length of a source line"}
        {unsafe          "Unsafe mode - do not run in a safe interpreter"}
        {scriptdir.arg scripts "Default directory for scripts"}
        {author.arg "" "Name of author"}
    } [usage]]

    if {$docgen::default_option_values(sourcelinelen) == 0} {
        if {$docgen::default_option_values(backend) in {pdf docbook}} {
            set docgen::default_option_values(sourcelinelen) 90
        } else {
            set docgen::default_option_values(sourcelinelen) 80
        }
    }
    array set docgen::options [array get docgen::default_option_values]

    docgen::expander_init
    docgen::process_files [cmdline::getfiles $argv 1]

} msg]} {
    exit1 $::errorInfo
}

exit 0
