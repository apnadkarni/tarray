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

namespace eval tarray::teval {}

proc tarray::tscript {script} {
    teval::Compiler create tcompiler
    proc [namespace current]::tscript {script} {
        uplevel 1 [tcompiler compile $script]
    }
    tailcall tscript $script
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
            return [switch -exact -- [lindex $args 0 0] {
                Column {
                    list LValueTableColumn [lindex $first_child 1] [lindex $args 0 1] {*}[lrange $args 1 end]
                }
                Columns {
                    list LValueTableColumns [lindex $first_child 1] [lrange [lindex $args 0] 1 end] {*}[lrange $args 1 end]
                }
                default {
                    list LValueTarray [lindex $first_child 1] {*}$args
                }
            }]
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

    method IndexSelector {from to child} {
        return [list IndexSelector $child]
    }

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

    method ColumnIdentifier {from to child} {
        return $child
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

    method Sequence {from to {child {}}} {
        return [linsert $child 0 Sequence]
    }
    method SequenceElements {from to args} {
        return $args
    }

}

oo::class create tarray::teval::Compiler {
    variable Script Compilations SelectorNestingLevel

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
        set SelectorNestingLevel 0

        set code {}
        foreach stmt $ir {
            append code [my {*}$stmt]\n
        }
        
        return [set Compilations($script) $code]
    }

    method eval {script} {
        uplevel 1 [my compile $script]
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
                lassign $lvalue type ident indexexpr
                return "set $ident [my {*}$rvalue]"
            }
            LValueTarray {
                # We are assigning to elements in a tarray. The elements
                # to be assigned may be specified through a range or
                # a general expression that results in an index or index list.
                lassign $lvalue type ident indexexpr
                switch -exact -- [lindex $indexexpr 0] {
                    Range {
                        return "tarray::teval::rt::fill $ident [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        return "tarray::teval::rt::fill $ident [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        return "tarray::teval::rt::assign $ident [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                }
            }
            LValueTableColumn {
                # Assigning to a single column.
                lassign $lvalue type table column indexexpr
                if {[lindex $column 0] eq "Identifier"} {
                    set column [lindex $column 1]
                } else {
                    set column [my {*}$column]
                }
                switch -exact -- [lindex $indexexpr 0] {
                    "" {
                        # No index -> entire column to be operated
                        return "tarray::teval::rt::table_column_fill $ident \[list $column\] [my {*}$rvalue]"
                    }
                    Range {
                        return "tarray::teval::rt::table_column_fill $ident \[list $column\] [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        return "tarray::teval::rt::table_column_fill $ident \[list $column\] [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        return "tarray::teval::rt::tab_col_assign $ident \[list $column\] [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    
                }
            }

            LValueTableColumns {
                lassign $lvalue type ident columns indexexpr
                set collist {}
                foreach column $columns {
                    if {[lindex $column 0] eq "Identifier"} {
                        lappend collist [lindex $column 1]
                    } else {
                        lappend collist [my {*}$column]
                    }
                }
                set collist [join $collist { }]
                switch -exact -- [lindex $indexexpr 0] {
                    "" {
                        # No index -> whole columns to be operated
                        return "tarray::teval::rt::table_column_fill $ident \[list $collist\] [my {*}$rvalue]"
                    }
                    Range {
                        return "tarray::teval::rt::table_column_fill $ident \[list $collist\] [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        return "tarray::teval::rt::table_column_fill $ident \[list $collist\] [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        return "tarray::teval::rt::tab_col_assign $ident \[list $collist\] [my {*}$rvalue] [my {*}$indexexpr]"
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

    method _boolop {op first second} {
        # TBD - short circuit ?
        return "\[tarray::teval::rt::$op [my {*}$first] [my {*}$second]\]"
    }
    forward && my _boolop and
    forward || my _boolop or

    method _strop {op first second} {
        return "\[tarray::teval::rt::strop $op [my {*}$first] [my {*}$second]\]"
    }
    forward =^ my _strop =^
    forward !^ my _strop !^
    forward =~ my _strop =~
    forward !~ my _strop !~
    forward =~^ my _strop =~^
    forward !~^ my _strop !~^
    forward =* my _strop =*
    forward !* my _strop !*
    forward =*^ my _strop =*^
    forward !*^ my _strop !*^

    method UnaryExpr {op child} {
        return "\[tarray::teval::rt::unary $op [my {*}$child]\]"
    }

    method PostfixExpr {primary_expr args} {
        if {[llength $args] == 0} {
            return [my {*}$primary_expr]
        }

        # For functions, we take identifier as the name of the function,
        # not as a variable containing the name of a function.
        if {[lindex $args 0 0] eq "FunctionCall" &&
            [lindex $primary_expr 0] eq "Identifier" } {
            set primary [lindex $primary_expr 1]
        } else {
            set primary [my {*}$primary_expr]
        }

        foreach postexpr $args {
            switch -exact -- [lindex $postexpr 0] {
                IndexSelector {
                    set frag {
                        tarray::teval::rt::push_selector_context %VALUE%
                        try {
                            return -level 0 %SELECTEXPR%
                        } finally {
                            tarray::teval::rt::pop_selector_context
                        }
                    }
                    incr SelectorNestingLevel
                    set primary "\[[string map [list %VALUE% $primary %SELECTEXPR%  [my {*}$postexpr]] $frag]\]"
                    incr SelectorNestingLevel -1
                }
                Selector {
                    set frag {
                        tarray::teval::rt::push_selector_context %VALUE%
                        try {
                            return -level 0 [tarray::teval::rt::selector [tarray::teval::rt::selector_context] %SELECTEXPR%]
                        } finally {
                            tarray::teval::rt::pop_selector_context
                        }
                    }
                    incr SelectorNestingLevel
                    set primary "\[[string map [list %VALUE% $primary %SELECTEXPR%  [my {*}$postexpr]] $frag]\]"
                    incr SelectorNestingLevel -1
                }
                FunctionCall {                
                    set fnargs {}
                    foreach fnarg [lrange $postexpr 1 end] {
                        lappend fnargs [my {*}$fnarg]
                    }
                    set primary "\[$primary [join $fnargs { }]\]"
                }
                Column {
                    set primary "\[tarray::table::column $primary [my {*}$postexpr]\]"
                }
                Columns {
                    set primary "\[tarray::table::slice $primary [my {*}$postexpr]\]"
                }
            }
        }
        return "$primary"
    }

    method IndexSelector {child} {
        return [my {*}$child]
    }

    method Selector {child} {
        return [my {*}$child]
    }

    method Column {child} {
        if {[lindex $child 0] eq "Identifier"} {
            return [lindex $child 1]
        } else {
            return [my {*}$child]
        }
    }

    method Columns {args} {
        set cols [lmap colarg $args {
            if {[lindex $colarg 0] eq "Identifier"} {
                lindex $colarg 1
            } else {
                my {*}$colarg
            }
        }]
        return "\[list [join $cols]\]"
    }

    method Sequence {args} {
        return "\[list [join [lmap arg $args {
            my {*}$arg
        }] { }]\]"
    }
    method String s {return "{$s}"}
    method Number {n} {return $n}
    method Range {low high} {
        if {$SelectorNestingLevel} {
            return "\[tarray::teval::rt::selector_range \[list [my {*}$low] [my {*}$high]\]\]"
        } else {
            return "\[list [my {*}$low] [my {*}$high]\]"
        }
    }

    method Identifier {ident} {
        return "\[[list set $ident]\]"
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

    proc fill {varname value args} {
        upvar 1 $varname var
        # args is either a single numeric literal or a range low high pair
        return [switch -exact -- [tarray::types $var] {
            table { tarray::table::vfill var $value {*}$args }
            "" { error "$varname is not a column or table." }
            default { tarray::column::vfill var $value {*}$args }
        }]
    }

    proc assign {varname value index} {
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

    proc col=~^ {col val} {
        return [tarray::column::search -all -nocase -re $col $val]
    }

    proc col!~^ {col val} {
        return [tarray::column::search -all -nocase -not -re $col $val]
    }

    proc col=* {col val} {
        return [tarray::column::search -all -pat $col $val]
    }

    proc col!* {col val} {
        return [tarray::column::search -all -not -pat $col $val]
    }

    proc col=*^ {col val} {
        return [tarray::column::search -all -nocase -pat $col $val]
    }

    proc col!*^ {col val} {
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

    proc strop {op a b} {
        lassign [tarray::types $a $b] atype btype
        if {$atype ne ""} {
            return [col$op $a $b]
        } elseif {$btype ne ""} {
            return [col$op $b $a]
        } else {
            # Neither is a tarray
            return [switch -exact -- $op {
                =^ {string equal -nocase $a $b}
                !^ {expr {![string equal -nocase $a $b]}}
                =~ {regexp -- $b $a}
                !~ {expr {![regexp -- $b $a]}}
                =~^ {regexp -nocase -- $b $a}
                !~^ {expr {![regexp -nocase -- $b $a]}}
                =* {string match $b $a}
                !* {expr {![string match $b $a]}}
                =*^ {string match -nocase $b $a}
                !*^ {expr {![string match -nocase $b $a]}}
            }]
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
        # Can't use tarray::intersect3+tarray::sort because of 
        # uniqueness requirement
        # TBD - are the elements in increasing order after union?
        # TBD - implement union in C
        return [tarray::column::create int \
                    [lsort -integer -unique \
                         [concat [tarray::column::range -list $a 0 end] \
                              [tarray::column::range -list $b 0 end]]]]
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

    proc selector_range {range} {
        # TBD - replace with C version
        lassign $range low high
        if {$low < 0} {
            set low 0
        }
        if {$high eq "end"} {
            set high [expr {[size [selector_context]] - 1}]
        }
        set l {}
        for {set i $low} {$i <= $high} {incr i} {
            lappend l $i
        }
        return [tarray::column::create int $l]
    }

    proc size {tab_or_col} {
        if {[tarray::types $tab_or_col] eq "table"} {
            return [tarray::table::size [selector_context]]
        } else {
            return [tarray::column::size [selector_context]]
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


namespace path tarray
set I [column create int {10 20 30 40 50}]
set T [table create {i int s string} {{10 ten} {20 twenty} {30 thirty}}]
