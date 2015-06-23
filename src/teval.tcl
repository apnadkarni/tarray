if {1} {
    lappend auto_path ../build/lib
    package require tarray
}
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

    
    method _print {node {indent {}}} {
        set children [lassign $node name]
        puts "${indent}$name"
        foreach child $children {
            my _print $child "  ${indent}"
        }
    }

    method print {text} {
        foreach stmt [my compile $text] {
            my _print $stmt
        }
    }

    method compile {text} {
        set Script $text
        if {[catch {my parset $text} ast eropts]} {
            if {[string match {pt::rde *} $ast]} {
                error [pt::util::error2readable $ast $text]
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

    # Common method used by most binary operator expressions
    method _binop {nodename from to first_child {op {}} {second_child {}}} {
        # $args contains remaining children (currently at most 1)
        if {[llength $op] == 0} {
            # Node has only one child, just promote it.
            return $first_child
        } else {
            # Fold constants if first child and second are both numbers.
            if {[lindex $first_child 0] eq "Number" &&
                [lindex $second_child 0] eq "Number"} {
                return [list Number [expr "[lindex $first_child 1][lindex $op 1][lindex $second_child 1]"]]
            } else {
                return [list [lindex $op 1] $first_child $second_child]
            }
        }
    }

    method Program {from to args} {
        return $args
    }

    method Statement {from to {child {}}} {
        if {[llength $child]} {
            return [list Statement $child]
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

    method LValue {from to first_child args} {
        if {[llength $args] == 0} {
            return $first_child;
        } else {
            return [list Tarray [lindex $first_child 1] {*}$args]
        }
    }

    forward Expression my _child
    forward LogicalOrExpr my _binop LogicalOrExpr
    forward LogicalAndExpr my _binop LogicalAndExpr
    method RangeExpr {from to first_child args} {
        if {[llength $args] == 0} {
            return $first_child
        } else {
            return [list Range $first_child [lindex $args 0]]
        }
    }
    forward BitOrExpr my _binop BitOrExpr
    forward BitXorExpr my _binop BitXorExpr
    forward BitAndExpr my _binop BitAndExpr
    forward RelExpr my _binop RelExpr
    forward AddExpr my _binop AddExpr
    forward MulExpr my _binop MulExpr

    method UnaryExpr {from to postfix_expr args} {
        if {[llength $args] == 0} {
            return $postfix_expr
        } else {
            # postfix_expr is actually UnaryOp.
            # $args should be a single argument again of type UnaryExpr
            # or a descendant
            set op [lindex $postfix_expr 1]
            set postfix_expr [lindex $args 0]
            # Note: A unary + is a no-op so we could ignore it. However
            # we don't discard it because it is an error if the operand
            # is not numeric (which we cannot know if it is not a literal).
            # Consecutive ++ can be shrunk to +. Consecutive
            # "--" or "~~" can be completely discarded. "!" changes
            # the value of numerics (!! is not a no-op) so we leave it
            # alone.
            if {[lindex $postfix_expr 0] eq "UnaryExpr" &&
                $op eq [lindex $postfix_expr 1]} {
                if {$op in {- ~}} {
                    return [lindex $postfix_expr 2]
                }
                if {$op eq "+"} {
                    return $postfix_expr
                }
            }

            # If the child is also a Unary expression
            return [list UnaryExpr $op {*}$args]
        }
    }
    
    method PostfixExpr {from to primary_expr args} {
        if {[llength $args] == 0} {
            return $primary_expr
        }
        return [list PostfixExpr $primary_expr {*}$args]
    }

    forward PrimaryExpr my _child

    forward PostfixOp my _child
    method Selector {from to child} {
        return [list Selector $child]
    }

    method FunctionCall {from to {child {}}} {
        return [linsert $child 0 FunctionCall]
    }

    method ArgumentExprList {from to args} {
        return $args
    }

    method Column {from to child} {
        return [list Column $child]
    }

    method Columns {from to {child {}}} {
        return [linsert $child 0 Columns]
    }

    method ColumnList {from to args} {
        return $args
    }

    method ColumnIdentifier {from to args} {
        return $args
    }

    forward UnaryOp my _extract UnaryOp
    forward MulOp my _extract MulOp
    forward AddOp my _extract AddOp
    forward RelOp my _extract RelOp
    forward BitAndOp my _extract BitAndOp
    forward BitOrOp my _extract BitOrOp
    forward BitXorOp my _extract BitXorOp
    forward LogicalAndOp my _extract LogicalAndOp
    forward LogicalOrOp my _extract LogicalOrOp
    forward AssignOp my _extract AssignOp

    method Identifier {from to} {
        return [list Identifier [string range $Script $from $to]]
    }

    method BuiltinIdentifier {from to} {
        return [list BuiltinIdentifier [string range $Script $from $to]]
    }

    forward Number my _extract Number
    method String {from to} {
        return [list String [subst -novariables -nocommands [string range $Script $from+1 $to-1]]]
    }
    method Sequence {from to args} {
        return [list Sequence $args]
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
    variable NConstants Constants NVariables Variables
    variable NRegisters

    constructor {} {
        namespace path ::tarray::teval
        tarray::teval::Parser create parser
    }

    forward print parser print

    method compile {script} {
        if {[info exists Compilations($script)]} {
            return $Compilations($script)
        }

        set ir [parser compile $script]

        # Initialize the per-compile variables
        set Script $script
        set NConstants 0
        set Constants [list ]
        set NVariables 0
        set Variables [list ]
        set IndexNestingLevel 0
        set NRegisters 0

        set code {}
        foreach stmt $ir {
            append code [my {*}$stmt]\n
        }
        
        return [set Compilations($script) $code]
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

    method Statement {child} {
        # If not an assignment operator, for example just a function call
        # or variable name, need explicit return else we land up with
        # something like {[set x]} as the compiled code
        if {[lindex $child 0] in {= += -= *= /=}} {
            return [my {*}$child]
        } else {
            return "return -level 0 [my {*}$child]"
        }
    }

    method = {lvalue rvalue} {
        lassign $lvalue type ident indexexpr
        switch -exact -- $type {
            Identifier {
                return "set $ident [my {*}$rvalue]"
            }
            Tarray {
                # We are assigning to elements in a tarray. The elements
                # to be assigned may be specified through a range or
                # a general expression that results in an index or index list.
                switch -exact -- [lindex $indexexpr 0] {
                    Range {
                        return "tarray::teval::rt::tfill $ident [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        return "tarray::teval::rt::tfill $ident [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        return "tarray::teval::rt::tassign $ident [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                }
            }
            default {
                error "Internal error: Unexpected node type [lindex $lvalue 0]"
            }
        }
    }

    method _mathop {op first second} {
        return "\[tarray::teval::rt::mathop $op [my {*}$first] [my {*}$second]\]"
    }

    forward LogicalOrExpr my _join_specific_operator  ||
    forward LogicalAndExpr my _join_specific_operator &&

    forward + my _mathop +
    forward - my _mathop -
    forward * my _mathop *
    forward / my _mathop /
    forward | my _mathop |
    forward ^ my _mathop ^
    forward & my _mathop &
    forward == my _mathop ==
    forward < my _mathop <
    forward <= my _mathop <=
    forward > my _mathop >
    forward >= my _mathop >=

    method UnaryExpr {op child} {
        return "\[tarray::teval::rt::unary $op [my {*}$child]\]"
    }

    method PostfixExpr {primary_expr args} {
        set primary [my {*}$primary_expr]

        if {[llength $args] == 0} {
            return $primary
        }

        foreach postexpr $args {
            switch -exact -- [lindex $postexpr 0] {
                Selector {
                    set frag {
                        tarray::teval::rt::push_selector_context %VALUE%
                        try {
                            return -level 0 [tarray::teval::rt::selector [tarray::teval::rt::selector_context] %SELECTEXPR%]
                        } finally {
                            tarray::teval::rt::pop_selector_context
                        }
                    }
                    set primary "\[[string map [list %VALUE% $primary %SELECTEXPR%  [my {*}$postexpr]] $frag]\]"
                }
                FunctionCall {                
                    TBD
                }
                Column {
                    TBD
                }
                Columns {
                    TBD
                }
            }
        }
        return "$primary"
    }

    method Selector {child} {
        return [my {*}$child]
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

    forward RelOp my   _extract
    forward UnaryOp my _extract
    forward AddOp my   _extract
    forward MulOp my   _extract

    method Sequence {from to args} {
        return "\[list [join [lmap child $args {my {*}$child}] { }]\]"
    }

    method String s {return "{$s}"}
    method Number {n} {return $n}
    method Range {low high} {
        return "\[list [my {*}$low] [my {*}$high]\]"
    }

    method Identifier {ident} {
        return "\[[list set $ident]\]"
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

namespace eval tarray::teval::rt {
    variable _selector_contexts {}

    proc selector_context {} {
        variable _selector_contexts
        return [lindex $_selector_contexts 0]
    }

    proc push_selector_context {val} {
        variable _selector_contexts
        lappend _selector_contexts $val
    }

    proc pop_selector_context {} {
        variable _selector_contexts
        # Fastest Pop list from http://wiki.tcl.tk/22619
        set r [lindex $_selector_contexts end]
        set _selector_contexts [lreplace $_selector_contexts [set _selector_contexts end] end] ; # Make sure [lreplace] operates on unshared object
        return $r
    }

    proc tfill {varname value args} {
        upvar 1 $varname var
        # args is either a single numeric literal or a range low high pair
        return [switch -exact -- [tarray::types $var] {
            table { tarray::table::vfill var $value {*}$args }
            "" { error "$varname is not a column or table." }
            default { tarray::column::vfill var $value {*}$args }
        }]
    }

    proc tassign {varname value index} {
        upvar 1 $varname var

        # varname is the name of a column or table variable (must exist)
        # value is the value to be assigned
        # index is a general expression
        #
        # If value is a tarray of the same type as target variable,
        # we use place to update the target array. In this case
        # indexlist must be a int tarray or an int list else
        # vplace will throw an error.
        #
        # If the above is not true, value is filled in all locations
        # specified by the index. If its type is not compatible with
        # the target array, an error is raised by vfill.
        #
        # index might be a single integer value, a list of integers or
        # something else. For the first two, vplace/vfill do the right
        # thing. For others, they will raise an error.

        lassign [tarray::types $var $value] vartype valuetype
        if {$vartype eq ""} {
            error "$varname is not a column or table."
        }

        if {$valuetype eq $vartype} {
            if {$vartype eq "table"} {
                return [tarray::table::vplace var $value $index]
            } else {
                return [tarray::column::vplace var $value $index]
            }
        }            

        # Either value is not a tarray or is a tarray of the wrong type.
        # In the latter case, we simply treat it as a single value to
        # fill into the target (possibly raising an error in case incompatible).
        # In the former case, there is actually ambiguity since value may
        # be a list compatible with the target array. For now we 
        # always treat it as a single value to be filled into target.
        if {$vartype eq "table"} {
            return [tarray::table::vfill var $value $index]
        } else {
            return [tarray::column::vfill var $value $index]
        }
    }

    proc col== {col val} {
        return [tarray::column::search -all -eq $col $val]
    }

    proc col!= {col val} {
        return [tarray::column::search -all -not -eq $col $val]
    }

    proc col< {col val} {
        return [tarray::column::search -all -lt $col $val]
    }

    proc col<= {col val} {
        return [tarray::column::search -all -not -gt $col $val]
    }

    proc col> {col val} {
        return [tarray::column::search -all -gt $col $val]
    }

    proc col>= {col val} {
        return [tarray::column::search -all -not -lt $col $val]
    }

    proc col=^ {col val} {
        return [tarray::column::search -all -nocase -eq $col $val]
    }

    proc col!^ {col val} {
        return [tarray::column::search -all -nocase -not -eq $col $val]
    }

    proc col=~ {col val} {
        return [tarray::column::search -all -re $col $val]
    }

    proc col!~ {col val} {
        return [tarray::column::search -all -not -re $col $val]
    }

    proc col=^~ {col val} {
        return [tarray::column::search -all -nocase -re $col $val]
    }

    proc col!^~ {col val} {
        return [tarray::column::search -all -nocase -not -re $col $val]
    }

    proc col=* {col val} {
        return [tarray::column::search -all -pat $col $val]
    }

    proc col!* {col val} {
        return [tarray::column::search -all -not -pat $col $val]
    }

    proc col=^* {col val} {
        return [tarray::column::search -all -nocase -pat $col $val]
    }

    proc col!^* {col val} {
        return [tarray::column::search -all -nocase -not -pat $col $val]
    }

    proc mathop {op a b} {
        lassign [tarray::types $a $b] atype btype
        if {$atype ne ""} {
            return [col$op $a $b]
        } elseif {$btype ne ""} {
            return [col$op $b $a]
        } else {
            # Neither is a tarray
            return [tcl::mathop::$op $a $b]
        }
    }

    proc unary {op a} {
        if {[tarray::types $a] eq ""} {
            return [expr "$op\$a"]
        } else {
            return [tarray::column::unary $op $a]
        }
    }

    proc and {a b} {
        lassign [tarray::types $a $b] atype btype

        if {$atype eq "" && $btype eq ""} {
            # Neither is a tarray
            return [expr {$a && $b}]
        }
        
        if {$atype eq ""} {
            # Only b is a tarray. Return as is if a is true
            if {$a} {
                return $b
            } else {
                return [tarray::column int {}]
            }
        }

        if {$btype eq ""} {
            # Only a is a tarray. Return as is if b is true
            if {$b} {
                return $a
            } else {
                return [tarray::column int {}]
            }
        }

        # Both are tarrays. Return the intersection
        # TBD - optimize if a or b are empty or does intersect3 already do that
        # TBD - are the elements in increasing order after intersect?
        return [lindex [tarray::column::intersect3 $a $b] 0]
    }

    proc or {a b} {
        lassign [tarray::types $a $b] atype btype

        if {$atype eq "" && $btype eq ""} {
            # Neither is a tarray
            return [expr {$a || $b}]
        }
        
        if {$atype eq ""} {
            # Only b is a tarray. Return as is if a is true
            if {$a} {
                # TBD - return ENTIRE array indices since a is true
                return $b
            } else {
                return $b
            }
        }

        if {$btype eq ""} {
            # Only a is a tarray. Return as is if b is true
            if {$b} {
                # TBD - should return ENTIRE array indices since b is true,
                # not just $a 
                return $a
            } else {
                return $a
            }
        }

        # Both are tarrays. Return the intersection
        # TBD - are the elements in increasing order after union?
        # TBD - implement union
        return [tarray::column::union $a $b]
    }

    proc selector {a selexpr} {
        lassign [tarray::types $a] atype
        if {[tarray::types $selexpr] eq ""} {
            # Not a column, treat as an index
            if {$atype eq "table"} {
                return [tarray::table::index $a $selexpr]
            } else {
                return [tarray::column::index $a $selexpr]
            }
        } else {
            # Treat $selexpr as a index column
            if {$atype eq "table"} {
                return [tarray::table::get $a $selexpr]
            } else {
                return [tarray::column::get $a $selexpr]
            }
        }
    }

    proc Index {val index} {
        return [switch -exact -- [tarray::types $val] {
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
tarray::teval::Compiler create tc


