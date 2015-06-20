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

namespace eval tarray::ast {
    proc Print {s ast} {
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

    proc print {s ast} {
        puts [join [pt::ast::bottomup [list [namespace current]::Print $s] $ast] \n]    
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
    variable Script

    constructor {args} {
        next {*}$args
    }

    method print {text} {
        tarray::ast::print $text [my parset $text]
    }

    method compile {text} {
        set Script $text
        if {[catch {my parset $text} ast eropts]} {
            if {[string match {pt::rde *} $ast]} {
                error [pt::util::error2readable $text $ast]
            } else {
                return -options $eropts $ast
            }
        }
        return [pt::ast bottomup [list [namespace which my] node] $ast]
    }

    method node {ast} {
        return [my {*}$ast]
    }

    method _child {from to ast} {
        return $ast
    }

    method _extract {name from to} {
        return [list $name [string range $Script $from $to]]
    }

    method Program {from to args} {
        return $args
    }

    method Statement {from to args} {
        if {[llength $args]} {
            return [lindex $args 0]
        } else {
            return {}
        }
    }

    method Assignment {from to lvalue assignop expr} {
        return [list [lindex $assignop 1] $lvalue $expr]
    }

    method MultiAssignment {from to args} {
        error "Multi assignments not implemented"
    }
    forward Expression my _child

    method Identifier {from to} {
        return [list Identifier [string range $Script $from $to]]
    }

    forward PrimaryExpr my _child
    forward PostfixOp my _child

    method PostfixExpr {from to primary_expr args} {
        if {[llength $args] == 0} {
            return $primary_expr
        }
        return [list PostfixExpr $primary_expr {*}$args]
    }

    method UnaryExpr {from to postfix_expr args} {
        if {[llength $args] == 0} {
            return $postfix_expr
        } else {
            # postfix_expr is actually UnaryOp.
            # $args should be a single argument again of type UnaryExpr
            # or a descendant
            switch -exact -- [lindex $args 0 0] {
                Number {
                    #NOTE: cannot combine this with the UnaryExpr case below!
                    return [list Number [expr "[lindex $postfix_expr 1][lindex $args 0 1]"]]
                }
                UnaryExpr {
                    if {[lindex $postfix_expr 1] eq "+"} {
                        # {UnaryExpr + {UnaryExpr ...}}
                        #   -> {UnaryExpr ...} (the + is a no-op)
                        return [lindex $args 0]
                    } else {
                        # {UnaryExpr - {UnaryExpr - X}}
                        #   -> X
                        # (because of above we know the sign in
                        # a UnaryExpr is always "-" hence the two "-" cancel)
                        return [lindex $args 0 2]
                    }
                } 
                default {
                    return [list UnaryExpr [lindex $postfix_expr 1] {*}$args]
                }
            }
        }
    }

    method _binop {nodename from to first_child args} {
        # $args contains remaining children (currently at most 1)
        if {[llength $args] == 0} {
            # Node has only one child, just promote it.
            return $first_child
        } else {
            # Fold constants if first child and second are both numbers.
            if {[lindex $first_child 0] eq "Number" &&
                [lindex $args 1 0] eq "Number"} {
                return [list Number [expr "[lindex $first_child 1][lindex $args 0 1][lindex $args 1 1]"]]
            } else {
                return [list [lindex $args 0 1] $first_child [lindex $args 1]]
            }
        }
    }

    forward MulExpr my _binop MulExpr
    forward AddExpr my _binop AddExpr
    forward RelExpr my _binop RelExpr
    forward BitAndExpr my _binop BitAndExpr
    forward BitOrExpr my _binop BitOrExpr
    forward BitXorExpr my _binop BitXorExpr
    forward LogicalAndExpr my _binop LogicalAndExpr
    forward LogicalOrExpr my _binop LogicalOrExpr
    
    forward UnaryOp my _extract UnaryOp
    forward Number my _extract Number
    forward String my _extract String
    forward MulOp my _extract MulOp
    forward AddOp my _extract AddOp
    forward RelOp my _extract RelOp
    forward BitAndOp my _extract BitAndOp
    forward BitOrOp my _extract BitOrOp
    forward BitXorOp my _extract BitXorOp
    forward LogicalAndOp my _extract LogicalAndOp
    forward LogicalOrOp my _extract LogicalOrOp
    forward AssignOp my _extract AssignOp

    method LValue {from to first_child args} {
        if {[llength $args] == 0} {
            return $first_child;
        } else {
            return [list Tarray [lindex $first_child 1] {*}$args]
        }
    }

    method Selector {from to child} {
        return [list Selector $child]
    }
    method RangeExpr {from to first_child args} {
        if {[llength $args] == 0} {
            return $first_child
        } else {
            return [list Range $first_child [lindex $args 0]]
        }
    }

    method FunctionCall {from to {child {}}} {
        return [linsert $child 0 FunctionCall]
    }

    method ArgumentExprList {from to args} {
        return $args
    }

    method ColumnSlice {from to child} {
        # assert - child is Identifier or IdentifierList
        if {[lindex $child 0] eq "Identifier"} {
            return [list Column [lindex $child 1]]
        } else {
            return [list Table [lindex $child 1]]
        }
    }

    method IdentifierList {from to args} {
        return [list IdentifierList [lmap arg $args {
            lindex $arg 1
        }]]
    }

    method BuiltinIdentifier {from to} {
        return [list BuiltinIdentifier [string range $Script $from $to]]
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
    variable Script Compilations IndexNestingLevel 
    variable Code NConstants Constants NVariables Variables

    constructor {{optimize 1}} {
        namespace path ::tarray::teval
        tarray::teval::Parser create parser $optimize
    }

    forward print parser print

    method compile {script} {
        if {[info exists Compilations($script)]} {
            return $Compilations($script)
        }

        set ir [parser compile $script]

        # Initialize the per-compile variables
        set Script $script
        set Code [list ]
        set NConstants 0
        set Constants [list ]
        set NVariables 0
        set Variables [list ]
        set IndexNestingLevel 0

        return [set Compilations($script) [list $Variables [my {*}$ast]]]
    }

    method _constslot {const type} {
        if {[dict exists $Constants $const $type]} {
            return [dict get $Constants $const $type]
        }
        dict set Constants $const $type [set slot __k$NConstants]
        incr NConstants
        return $slot
    }

    method _varslot {varname} {
        if {[dict exists $Variables $varname]} {
            return [dict get $Variables $varname Slot]
        }
        dict set Variables $varname Slot [set slot __v$NVariables]
        incr NVariables
        return $slot
    }

    method _child {from to child} {
        return [my {*}$child]
    }
    method _extract {from to args} {
        return [string range $Script $from $to]
    }

    method _literal {type from to} {
        return [list $type [string range $Script $from $to]]
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
            return "set [list $name] \[$rvalue\]"
        } else {
            return "tarray::column::vfill [list $name] \[$rvalue\] $index_or_range"
        }
    }

    method LValue {from to ident {index_or_range {}}} {
        set name [my {*}$ident]
        if {[llength $index_or_range] == 0} {
            return [list $name]
        }
        return [list $name {*}[my {*}$index_or_range]]
    }

    method Expression {from to child} {
        return "expr {[my {*}$child]}"
    }
    
    method _join_specific_operator {op from to args} {
        if {[llength $args] == 1} {
            return [lindex $args 0]
        } else {
            return "\[tarray::teval::runtime::$op [join [lmap child $args {my {*}$child}] { }]\]"
        }
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

    forward RelOp my   _extract
    forward UnaryOp my _extract
    forward AddOp my   _extract
    forward MulOp my   _extract

    method Sequence {from to args} {
        return "\[list [join [lmap child $args {my {*}$child}] { }]\]"
    }

    forward String my _literal String
    forward Number my  _literal Number

    method Identifier {from to} {
        return [list Identifier [my _varslot [string range $from $to]]]
    }

    method BuiltinIdentifier {from to args} {
        puts "NEST: $IndexNestingLevel"
        if {$IndexNestingLevel == 0} {
            error "Built-in identifier [string range $Script $from $to] can only be used within a column or table indexing scope."
        }
        return "load [string range $Script [incr from] $to]"
    }

    method ColumnIdentifier {from to args} {
        if {$IndexNestingLevel == 0} {
            error "Built-in identifier [string range $Script $from $to] can only be used within a column or table indexing scope."
        }
        return "load [string range $Script [incr from] $to]"
    }

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




tarray::teval::Parser create tp
#tarray::teval::Compiler create tc
#tarray::teval::Compiler create td 0

