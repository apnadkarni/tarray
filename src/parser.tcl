package require pt::pgen
package require pt::ast
package require pt::util
package require fileutil
# Next line because the generated code has a return which
# causes script to exit if not caught
switch [catch {
    eval [pt::pgen peg [fileutil::cat selector.peg] oo -class ::tarray::SelectorBase -package ::tarray::selector -version 0.1]
} msg opts] {
    0 - 2 {}
    default {
        return -options $opts $msg
    }
}

proc prast {s} {
    set ast [parser parset $s]
    tarray::selector::print $s $ast
}

oo::class create tarray::Selector {
    superclass tarray::SelectorBase
    method parset text {
        my variable Asts
        if {! [info exists Asts($text)]} {
            set Asts($text) [next $text]
        }
        return $Asts($text)
    }
    method print text {
        tarray::selector::print $text [my parset $text]
    }
}
tarray::Selector create tarray::selector::parser

namespace eval tarray::selector {
    namespace export print
    namespace ensemble create
}

proc tarray::selector::Print {s ast} {
    set children [lassign $ast type start end]
    set result   [list [list <$type> :: $start $end [string range $s $start $end]]]

    # The arguments are already processed for printing
    foreach c $children {
	foreach line $c {
	    lappend result "    $line"
	}
    }
    return $result
}


proc tarray::selector::print {s ast} {
    return [join [pt::ast::bottomup [list [namespace current]::Print $s] $ast] \n]    
}

namespace eval tarray::selector::interpreter {}

proc tarray::selector::interpret {expr} {
    set calc [Interpreter new]
    try {
        uplevel 1 [list $calc compute $expr]
    } finally {
        $calc destroy
    }
}

oo::class create tarray::selector::Interpreter {
    variable Parser Expr FrameLevel
    constructor {} {
        set Parser tarray::selector::parser
    }

    method compute {expr} {
        set Expr $expr
        set ast [$Parser parset $expr]
        set FrameLevel "#[expr {[info level]-1}]"
        return [my {*}$ast]
    }

    method Selector {from to firstarg args} {
        # firstarg broken out as separate argument because it is always required

        set result [my {*}$firstarg]
        foreach {orop andterm} $args {
            if {$result} break; # Shortcut evaluation
            set result [my {*}$andterm]
        }
        return [expr {!!$result}]
    }

    method AndTerm {from to firstarg args} {
        # firstarg broken out as separate argument because it is always required

        set result [my {*}$firstarg]
        foreach {andop boolterm} $args {
            if {! $result} break; # Shortcut evaluation
            set result [my {*}$boolterm]
        }
        return [expr {!!$result}]
    }

    method BoolTerm {from to firstarg args} {
        set result [my {*}$firstarg]
        if {[llength $args]} {
            lassign $args relop baseterm
            set baseterm [my {*}$baseterm]
            # Instead of defining a method for each operator, just
            # pick it out from the child node
            set op [string range $Expr {*}[lrange $relop 1 2]]
            set result [switch -exact -- $op {
                == - != - <= - >= - < - > {
                    tcl::mathop::$op $result $baseterm
                }            

                =^ { string equal -nocase $baseterm $result }
                !^ { string compare -nocase $baseterm $result }

                =~ { regexp -- $baseterm $result }
                !~ { expr {![regexp -- $baseterm $result]} }
                =^~ { regexp -nocase -- $baseterm $result }
                !^~ { expr {![regexp -nocase -- $baseterm $result]} }

                =* { string match $baseterm $result }
                !* { expr {![string match $baseterm $result]} }
                =^* { string match -nocase $baseterm $result }
                !^* { expr {![string match -nocase $baseterm $result]} }

                default { error "Invalid operator $op" }
            }]
        }
        return [expr {!!$result}]
    }

    method BaseTerm {from to args} {
        set val [my {*}[lindex $args end]]
        if {[llength $args] == 1} {
            return $val
        }
        if {[lindex [lindex $args 0] 0] ne "NotOp"} {
            error "Internal error in compiler: Expected NotOp, got [lindex [lindex $args 0] 0]"
        }
        return [expr {! $val}]
    }

    method RealNumber {from to} {
        return [string range $Expr $from $to]
    }

    method StringLiteral {from to} {
        # Need to adjust by one char to remove enclosing quotes
        return [string range $Expr [incr from] [incr to -1]]
    }

    method Var {from to} {
        return [uplevel $FrameLevel [list set [string range $Expr $from $to]]]
    }
}

