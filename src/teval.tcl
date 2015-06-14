package require pt::pgen
package require pt::ast
package require pt::util
package require fileutil
# Next line because the generated code has a return which
# causes script to exit if not caught
switch [catch {
    eval [pt::pgen peg [fileutil::cat teval.peg] oo -class tarray::TEvalParserBase -package tarray::teval -version 0.1]
} msg opts] {
    0 - 2 {}
    default {
        return -options $opts $msg
    }
}

oo::class create tarray::TEvalParser {
    superclass tarray::TEvalParserBase
    method parset text {
        my variable Asts
        if {! [info exists Asts($text)]} {
            set Asts($text) [next $text]
        }
        return $Asts($text)
    }
    method print text {
        tarray::ast::print $text [my parset $text]
    }
}

proc tarray::teval {script} {
    set ip [TEvalInterpreter new]
    try {
        uplevel 1 [list $ip interpret $expr]
    } finally {
        $ip destroy
    }
}

oo::class create tarray::TEvalCompiler {
    variable Script FrameLevel Result

    constructor {} {
        tarray::TEvalParser create parser
    }

    forward print parser print

    method compile {script} {
        set Script $script
        set ast [parser parset $script]
        set FrameLevel "#[expr {[info level]-1}]"
        return [my {*}$ast]
    }

    method _child {from to child} {
        return [my {*}$child]
    }
    method _extract {from to args} {
        return [string range $Script $from $to]
    }

    method Program {from to args} {
        set result ""
        foreach statement $args {
            append result [my {*}$statement]\n
        }
        return $result
    }

    forward Statement my _child

    method Assignment {from to lvalue op expr} {
        return "set [my {*}$lvalue] [my {*}$expr]"
    }

    forward LValue my _child
    forward Identifier my _extract
    forward Expression my _child
    forward LogicalOrExpr my _child
    forward LogicalAndExpr my _child
    forward BitOrExpr my _child
    forward BitXorExpr my _child
    forward BitAndExpr my _child
    forward EqExpr my _child
    forward AddExpr my _child
    forward MulExpr my _child
    forward UnaryExpr my _child
    forward PostfixExpr my _child
    method PrimaryExpr {from to child} {
        if {[lindex $child 0] eq "Identifier"} {
            # Because of our rules for identifiers, we do not have to
            # worry about escaping funky identifier names.
            return "\[set [string range $Script $from $to]\]"
        } else {
            return [my {*}$child]
        }
    }
}

proc prast {parser s} {
    if {[catch {
        set ast [$parser parset $s]
    } msg]} {
        # Note we use string match and not lindex because other messages
        # may not be well formed lists
        if {[string match pt::rde* $msg]} {
            puts "ERROR: [pt::util::error2readable $msg $s]"
        } else {
            puts "ERROR: $msg"
        }
    } else {
        tarray::ast::print $s $ast
    }
}
namespace eval tarray::ast {}
proc tarray::ast::Print {s ast} {
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


proc tarray::ast::print {s ast} {
    puts [join [pt::ast::bottomup [list [namespace current]::Print $s] $ast] \n]    
}

tarray::TEvalCompiler create tc
