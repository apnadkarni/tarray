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

oo::class create tarray::TEvalInterpreter {
    variable Script FrameLevel Result

    constructor {} {
        tarray::TEvalParser create parser
    }

    forward print parser print

    method interpret {script} {
        set Script $script
        set ast [parser parset $script]
        set FrameLevel "#[expr {[info level]-1}]"
        return [my {*}$ast]
    }

    method Program {from to args} {
        set Result ""
        foreach statement $args {
            my {*}$statement
        }
        return $Result
    }

    method Statements {from to args} {
        foreach statement $args {
            puts $args
            #my {*}$statement
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
