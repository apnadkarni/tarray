namespace eval xtal::shell {
    variable wish_monkeypatch {
        # ::tk::ConsoleInvoke --
        # Processes the command line input.  If the command is complete it
        # is evaled in the main interpreter.  Otherwise, the continuation
        # prompt is added and more input may be added.
        #
        # Arguments:
        # None.
        proc ::tk::ConsoleInvoke {args} {
            set ranges [.console tag ranges input]
            set cmd ""
            if {[llength $ranges]} {
                set pos 0
                while {[lindex $ranges $pos] ne ""} {
                    set start [lindex $ranges $pos]
                    set end [lindex $ranges [incr pos]]
                    append cmd [.console get $start $end]
                    incr pos
                }
            }
            if {$cmd eq ""} {
                ConsolePrompt
            } elseif {[info complete $cmd]} {
                if {[catch {
                    set is_xtal [consoleinterp eval [list ::xtal::shell::XtalCmd? $cmd]]
                }] == 0 && $is_xtal} {
                    set cmd "::xtal::xtal {[string trimright $cmd \n]}\n"
                }
                .console mark set output end
                .console tag delete input
                set result [consoleinterp record $cmd]
                if {$result ne ""} {
                    puts $result
                }
                ConsoleHistory reset
                ConsolePrompt
            } else {
                ConsolePrompt partial
            }
            .console yview -pickplace insert
        }
    }
}

proc xtal::shell::XtalCmd? {cmd} {
    # cmd is expected to be a properly formed list
    # where [info complete $cmd] will return true

    # Commands beginning with xtal are assumed to be invocations of
    # Xtal from Tcl (so in effect not an xtal script)
    if {[regexp {^\s*(::)?(xtal::)?xtal\s} $cmd]} {
        return 0
    }

    # Try translating it. On failure, assume not xtal
    if {[catch {::xtal::translate $cmd}]} {
        return 0
    }

    # Syntactically xtal. Could be single word Tcl commands like
    # "exit" which would be treated as xtal variables. Guard against
    # that by checking that the variable exists, otherwise a Tcl command.
    # We also have to ensure we don't confuse x+1 and f() as Tcl
    # commands.
    if {[llength $cmd] == 1 && 
        ![regexp {[^[:alnum:]:_]+} [lindex $cmd 0]]} {
        return [uplevel #0 [list info exists [lindex $cmd 0]]]
    }

    return 1
}
    
proc xtal::shell {} {
    console eval $::xtal::shell::wish_monkeypatch
}
