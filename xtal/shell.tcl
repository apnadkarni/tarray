
#
# Copyright (c) 2015, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

# This file implements support for Xtal syntax in the Wish console and tkcon.
# Currently this is implemented by monkey patching the command dispatch
# code for those packages at runtime. Since the Wish console and tkcon
# dispatch code has been stable for many many years, assumption is it is
# unlikely to change significantly in the future.
#
# Down the road, alternative implementations, for example using
# tkcon getcommand and friends, may be explored.

namespace eval xtal::shell {
    variable options
    array set options {prettify 1}

    # The following is an adaptation of the command dispatch code
    # from the wish console. We will use this to replace the wish
    # console dispatch at runtime.
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
                set result [consoleinterp eval [list ::xtal::shell::Prettify_graphic 0 [consoleinterp record $cmd]]]
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

    # The following is an adaptation of the command dispatch code from
    # tkcon. We will use this to replace the tkcon dispatch at runtime
    variable tkcon_monkeypatch {
        proc ::tkcon::EvalCmd {w cmd} {
            variable OPT
            variable PRIV

            $w mark set output end
            if {$cmd ne ""} {
                set code 0
                if {$OPT(subhistory)} {
                    set ev [EvalSlave history nextid]
                    incr ev -1
                    ## FIX: calcmode doesn't work with requesting history events
                    if {$cmd eq "!!"} {
                        set code [catch {EvalSlave history event $ev} cmd]
                        if {!$code} {$w insert output $cmd\n stdin}
                    } elseif {[regexp {^!(.+)$} $cmd dummy event]} {
                        ## Check last event because history event is broken
                        set code [catch {EvalSlave history event $ev} cmd]
                        if {!$code && ![string match ${event}* $cmd]} {
                            set code [catch {EvalSlave history event $event} cmd]
                        }
                        if {!$code} {$w insert output $cmd\n stdin}
                    } elseif {[regexp {^\^([^^]*)\^([^^]*)\^?$} $cmd dummy old new]} {
                        set code [catch {EvalSlave history event $ev} cmd]
                        if {!$code} {
                            regsub -all -- $old $cmd $new cmd
                            $w insert output $cmd\n stdin
                        }
                    } elseif {$OPT(calcmode) && ![catch {expr $cmd} err]} {
                        AddSlaveHistory $cmd
                        set cmd $err
                        set code -1
                    }
                }
                if {$code} {
                    $w insert output $cmd\n stderr
                } else {
                    # Check if an XTAL command
                    if {[catch {
                        set is_xtal [EvalAttached [list ::xtal::shell::XtalCmd? $cmd]]
                    }] == 0 && $is_xtal} {
                        set cmd "::xtal::xtal {[string trimright $cmd \n]}\n"
                    }

                    ## We are about to evaluate the command, so move the limit
                    ## mark to ensure that further <Return>s don't cause double
                    ## evaluation of this command - for cases like the command
                    ## has a vwait or something in it
                    $w mark set limit end
                    if {$OPT(nontcl) && ($PRIV(apptype) eq "interp")} {
                        set code [catch {EvalSend $cmd} res]
                        if {$code == 1} {
                            set PRIV(errorInfo) "Non-Tcl errorInfo not available"
                        }
                    } elseif {$PRIV(apptype) eq "socket"} {
                        set code [catch {EvalSocket $cmd} res]
                        if {$code == 1} {
                            set PRIV(errorInfo) "Socket-based errorInfo not available"
                        }
                    } else {
                        set code [catch {EvalAttached $cmd} res]
                        if {$code == 1} {
                            if {[catch {EvalAttached [list set errorInfo]} err]} {
                                set PRIV(errorInfo) "Error getting errorInfo:\n$err"
                            } else {
                                set PRIV(errorInfo) $err
                            }
                        }
                    }
                    if {![winfo exists $w]} {
                        # early abort - must be a deleted tab
                        return
                    }
                    AddSlaveHistory $cmd
                    # Run any user defined result filter command.  The command is
                    # passed result code and data.
                    if {[llength $OPT(resultfilter)]} {
                        set cmd [linsert $OPT(resultfilter) end $code $res]
                        if {[catch {EvalAttached $cmd} res2]} {
                            $w insert output "Filter failed: $res2" stderr \n stdout
                        } else {
                            set res $res2
                        }
                    }
                    catch {EvalAttached [list set _ $res]}
                    set maxlen $OPT(maxlinelen)
                    set trailer ""
                    if {($maxlen > 0) && ([string length $res] > $maxlen)} {
                        # If we exceed maximum desired output line length, truncate
                        # the result and add "...+${num}b" in error coloring
                        set trailer ...+[expr {[string length $res]-$maxlen}]b
                        set res [string range $res 0 $maxlen]
                    }
                    if {$code} {
                        if {$OPT(hoterrors)} {
                            set tag [UniqueTag $w]
                            $w insert output $res [list stderr $tag] \n$trailer stderr
                            $w tag bind $tag <Enter> \
                                [list $w tag configure $tag -under 1]
                            $w tag bind $tag <Leave> \
                                [list $w tag configure $tag -under 0]
                            $w tag bind $tag <ButtonRelease-1> \
                                "if {!\[info exists tk::Priv(mouseMoved)\] || !\$tk::Priv(mouseMoved)} \
			    {[list $OPT(edit) -attach [Attach] -type error -- $PRIV(errorInfo)]}"
                        } else {
                            $w insert output $res\n$trailer stderr
                        }
                    } elseif {$res ne ""} {
                        $w insert output $res stdout $trailer stderr \n stdout
                    }
                }
            }
            Prompt
            set PRIV(event) [EvalSlave history nextid]
        }
    }
}

# Interactive command loop in tclsh - adapted from http://wiki.tcl.tk/1968 
namespace eval xtal::shell::tclsh {
    proc banghist {val} {
        variable verbose_history
        if {![string compare $val "!"]} {set val ""}
        if {$verbose_history} {puts "[::history event $val]"}
        ::history redo $val
    }
    
    proc read_stdin {} {
        global tcl_prompt1
        variable eventLoop
        variable long_command
        set l [gets stdin]
        if {[eof stdin]} {
            set eventLoop "done"     ;# terminate the vwait eventloop
        } else {
            if {[string compare $l {}]} {
                append long_command "\n$l"
                set l $long_command
                if {[info complete $l]} {
                    if {[catch {
                        set is_xtal [::xtal::shell::XtalCmd? $l]
                    }] == 0 && $is_xtal} {
                        set l "::xtal::xtal {[string trimright $l \n]}\n"
                    }
                    if {[catch {uplevel \#0 history add [list $l] exec} err]} {
                        puts stderr $err
                    } elseif {[string compare $err {}]} {
                        puts [xtal::shell::Prettify_ascii 0 $err]
                    }
                    set long_command ""
                    catch $tcl_prompt1
                } else {
                    puts -nonewline "> "
                }
            } elseif {![string compare $long_command {}]} {
                catch $tcl_prompt1
            } else {
                puts -nonewline "> "
            }
            flush stdout
        }
    }

    proc repl {} {
        variable long_command ""
        variable verbose_history 0
        if {![catch {rename ::unknown ::_xtal_tcl_unknown}]} {
            proc ::unknown {cmdname args} {
                if {[regexp "^!..*" $cmdname]} {
                    ::xtal::shell::tclsh::banghist [string range $cmdname 1 end]
                } else {
                    ::_xtal_tcl_unknown $cmdname $args
                }
            }
        }
    
        if {![info exists ::tcl_prompt1]} {
            set ::tcl_prompt1 {puts -nonewline "xtal ([history nextid]) % "}
            set prompt_replaced 1
        } else {
            set prompt_replaced 0
        }

        # set up our keyboard read event handler
        # Vector stdin data to the socket
        fileevent stdin readable [namespace current]::read_stdin
        
        catch $::tcl_prompt1
        flush stdout
        # wait for and handle or stdin events...
        vwait [namespace current]::eventLoop
        if {[info procs ::_xtal_tcl_unknown] ne ""} {
            rename ::unknown ""
            rename ::_xtal_tcl_unknown ::unknown
        }
        if {$prompt_replaced} {
            unset -nocomplain ::tcl_prompt1
        }
    }
}

# Prettify output. The interface params correspond to those for the
# tkcon resultfilter command
proc xtal::shell::Prettify_graphic {errorcode result} {
    variable options
    if {$options(prettify)} {
        return [tarray::prettify $result -style graphics]
    } else {
        return $result
    }
}
proc xtal::shell::Prettify_ascii {errorcode result} {
    variable options
    if {$options(prettify)} {
        return [tarray::prettify $result]
    } else {
        return $result
    }
}
            
# Heuristic to check if passed command string might be Xtal
proc xtal::shell::XtalCmd? {cmd} {
    # cmd is expected to be a properly formed list
    # where [info complete $cmd] will return true. There are special
    # cases though where info complete returns 1 but the string
    # is not a well formed list (unmatched braces)

    # Commands beginning with xtal are assumed to be invocations of
    # Xtal from Tcl (so in effect not an xtal script)
    if {[regexp {^\s*(::)?(xtal::)?xtal\s} $cmd]} {
        return 0
    }

    # Commands wrapped in <> are Tcl scripts wrapped in Xtal <>
    if {[regexp {^\s*<.*>\s*$} $cmd]} {
        return 1
    }
    
    # Try translating it.
    if {[catch {::xtal::translate $cmd}]} {
        # Translation failed. Could be because it is actually Tcl
        # or could be Xtal but containing syntax errors.
        if {[lsearch -exact $cmd "="] >= 1 } {
            # Assume Xtal assignment
            return 1
        }
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
    if {[info commands ::tkcon] eq "::tkcon"} {
        ::tkcon eval eval $::xtal::shell::tkcon_monkeypatch
        ::tkcon resultfilter ::xtal::shell::Prettify_graphic
    } elseif {[info commands ::console] eq "::console"} {
        ::console eval $::xtal::shell::wish_monkeypatch
    } elseif {$::tcl_interactive} {
        xtal::shell::tclsh::repl
    } else {
        error "Unsupported console environment."
    }
    return
}

