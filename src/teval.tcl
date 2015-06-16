package require pt::pgen
package require pt::ast
package require pt::util
package require fileutil
# Next line because the generated code has a return which
# causes script to exit if not caught
switch [catch {
    eval [pt::pgen peg [fileutil::cat teval.peg] oo -class tarray::teval::ParserBase -package tarray::teval -version 0.1]
} msg opts] {
    0 - 2 {}
    default {
        return -options $opts $msg
    }
}

namespace eval tarray::teval {
    proc tempvar {} {
        variable _tempname_ctr
        return _teval[incr _tempname_ctr]
    }
}

oo::class create tarray::teval::Parser {
    superclass tarray::teval::ParserBase
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

proc tarray::teval::eval {script} {
    set tc [Compiler new]
    try {
        uplevel 1 [list $tc compile $script]
    } finally {
        $tc destroy
    }
}

oo::class create tarray::teval::Compiler {
    variable Script FrameLevel Result Compilations

    constructor {} {
        namespace path ::tarray::teval
        tarray::teval::Parser create parser
    }

    forward print parser print

    method compile {script} {
        if {[info exists Compilations($script)]} {
            return $Compilations($script)
        }

        set Script $script
        set ast [parser parset $script]
        set FrameLevel "#[expr {[info level]-1}]"
        return [set Compilations($script) [my {*}$ast]]
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
        lassign [my {*}$lvalue] name index_or_range
        set rvalue [my {*}$expr]
        if {[llength $index_or_range] == 0} {
            return "set [list $name] $rvalue"
        } else {
            return "tarray::column::vfill [list $name] $rvalue $index_or_range"
        }
    }

    method LValue {from to ident {index_or_range {}}} {
        set name [my {*}$ident]
        if {[llength $index_or_range] == 0} {
            return [list $name]
        }
        return [list $name {*}[my {*}$index_or_range]]
    }

    forward Identifier my _extract
    method Expression {from to child} {
        return "\[expr {[my {*}$child]}\]"
    }
    
    method _join_specific_operator {op from to args} {
        join [lmap child $args {
            my {*}$child
        }] $op
    }

    method _join_operator {from to first_child args} {
        if {[llength $args] == 0} {
            return [my {*}$first_child]
        } else {
            return [my _join_specific_operator [my {*}[lindex $args 0]] $from $to $first_child {*}[lrange $args 1 end]]
        }
    }

    forward LogicalOrExpr my _join_specific_operator  ||
    forward LogicalAndExpr my _join_specific_operator &&
    forward BitOrExpr my _join_specific_operator      |
    forward BitXorExpr my _join_specific_operator     ^
    forward BitAndExpr my _join_specific_operator     &

    forward RelExpr my _join_operator
    forward AddExpr my _join_operator
    forward MulExpr my _join_operator

    method UnaryExpr {from to args} {
        if {[llength $args] == 1} {
            return [my {*}[lindex $args 0]]
        } else {
            return "[my {*}[lindex $args 0]][my {*}[lindex $args 1]]"
        }
    }

    method PostfixExpr {from to first_child args} {
        set expr [my {*}$first_child]
        if {[llength $args] == 0} {
            return $expr
        }
        foreach child $args {
            set expr [my {*}$child $expr]
        }
        return $expr
    }

    method PostfixOp {from to child expr} {
        switch -exact -- [lindex $child 0] {
            Index {
                return "\[tarray::teval::runtime::Index $expr [my {*}$child]\]"
            }
            Selector {
            }
            FunctionOp {
            }
            SliceOp {
            }
        }
    }

    method PrimaryExpr {from to child} {
        if {[lindex $child 0] eq "Identifier"} {
            # Because of our rules for identifiers, we do not have to
            # worry about escaping funky identifier names.
            return "\[set [string range $Script $from $to]\]"
        } else {
            return [my {*}$child]
        }
    }
    method Index {from to {expr {}}} {
        if {[llength $expr] == 0} {
            return [string range $Script $from $to]
        }
        return [my {*}$expr]
    }

    forward Range my _child
    method RangeLow {from to expr} {
        return [list [{*}$expr] "end"]
    }
    method RangeHigh {from to expr} {
        return [list 0 [{*}$expr]]
    }
    method RangeLowHigh {from to lowexpr highexpr} {
        return [list [{*}$lowexpr] [{*}$highexpr]]
    }
    method RangeFull {from to} {
        return [list 0 "end"]
    }

    forward Number my  _extract
    forward RelOp my   _extract
    forward UnaryOp my _extract
    forward AddOp my   _extract
    forward MulOp my   _extract

    method Sequence {from to args} {
        return "\[list [join [lmap child $args {my {*}$child}] { }]\]"
    }

    forward String my _extract
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

namespace eval tarray::teval::runtime {
    proc Index {val index} {
        return [switch -exact -- [tarray::type $val] {
            table {
                tarray::table::index $val $index
            }
            "" {
                lindex $val $index
            }
            default {
                tarray::column::index $val $index
            }
        }]
    }
}


tarray::teval::Compiler create tc
