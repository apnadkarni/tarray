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
                        return "tarray::teval::rt::assign_range $ident [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        return "tarray::teval::rt::assign_element $ident [my {*}$rvalue] [my {*}$indexexpr]"
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
                # Assigning to a single column within a table
                lassign $lvalue type table column indexexpr
                if {[lindex $column 0] eq "Identifier"} {
                    set column [lindex $column 1]
                } else {
                    set column [my {*}$column]
                }
                switch -exact -- [lindex $indexexpr 0] {
                    "" {
                        # T.c = ....
                        # No index -> entire column to be operated
                        return "tarray::table::vcolumn $table $column [my {*}$rvalue]"
                    }
                    Range {
                        # T.c[4:j] = ...
                        return "tarray::teval::rt::table_column_assign_range $table $column [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric literal index
                        # T.c[0] = ...
                        return "tarray::table::vfill -columns \[list $column\] $table [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        return "tarray::teval::rt::table_column_assign $table $column [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                }
            }

            LValueTableColumns {
                lassign $lvalue type table columns indexexpr
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
                        # T.(a,b) = ...
                        # No index -> whole columns to be operated
                        return "tarray::teval::rt::table_columns_replace $table \[list $collist\] [my {*}$rvalue]"
                    }
                    Range {
                        return "tarray::teval::rt::table_columns_assign_range $table \[list $collist\] [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        # T.(a,b)[0] = ...
                        return "tarray::table::vfill -columns \[list $collist\] $table [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        return "tarray::teval::rt::table_columns_assign $table \[list $collist\] [my {*}$rvalue] [my {*}$indexexpr]"
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

    proc assign_element {varname value index} {
        # Assign a value to a single column or table element
        upvar 1 $varname var
        return [switch -exact -- [lindex [tarray::types $var] 0] {
            table { tarray::table::vfill var $value $index }
            "" { error "$varname is not a column or table." }
            default { tarray::column::vfill var $value $index }
        }]
    }

    proc assign_range {varname value low high} {
        # varname is the name of a column or table variable (must exist)
        # value is the value to be assigned
        # [low high] is the range to assign to
        
        upvar 1 $varname var

        lassign [tarray::types $var $value] vartype valuetype
        
        if {$vartype eq ""} {
            error "$varname is not a column or table."
        }

        # TBD - need to handle "end" in range specification
        
        if {$low > $high} {
            error "Range lower limit $low is greater than upper limit $high."
        }
        set target_size [expr {$high - $low + 1}]
        if {$vartype eq "table"} {

            # If the value is also a table, we assume each row in the value
            # is to be assigned successively to the target range. Otherwise
            # it is a value to be filled in the target range.
            if {$valuetype ne "table"} {
                if {$valuetype ne ""} {
                    error "Cannot assign a column to a table"
                }

                # Try converting to a table first.
                set table_def [tarray::table::definition $var]
                if {[catch {
                    set value [tarray::table::create $table_def $value]
                }]} {
                    # Try treating as a single row of the table
                    return [tarray::table::vfill var $value $low $high]
                }
            }

            # We have to use a put. Make sure the source range
            # and target range match
            set source_size [tarray::table::size $value]
            if {$target_size != $source_size} {
                error "Source size $source_size differs from target table range $low:$high."
            }
            return [tarray::table::vput var $value $low]
        }

        # vartype is a column type
        if {$valuetype eq "table"} {
            # No possibility of conversion. But target might be
            # of type any in which case we have to fill the range
            return [tarray::column::vfill var $value $low $high]
        } else {
            if {$valuetype eq ""} {
                # Try and convert to column of appropriate type
                if {[catch {
                    set value [tarray::column::create [tarray::column::type $var] $value]}]} {
                    # Try treating as a single element of column
                    return [tarray::column::vfill var $value $low $high]
                }
            }

            # col->col We have to use a put. Make sure the source range
            # and target range match
            set source_size [tarray::column::size $value]
            if {$target_size != $source_size} {
                if {$source_size == 1} {
                    return [tarray::column::vfill var [tarray::column::index $value 0] $low $high]
                } else {
                    error "Source size $source_size differs from target column range $low:$high."
                }
            } else {
                return [tarray::column::vput var $value $low]
            }
        }
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
        } else {

            # Either value is not a tarray or is a tarray of the wrong
            # type.  In the latter case, we simply treat it as a
            # single value to fill into the target (possibly raising
            # an error in case incompatible).  In the former case,
            # there is actually ambiguity since value may be a list
            # compatible with the target array. For now we always
            # treat it as a single value to be filled into target.

            if {$vartype eq "table"} {
                return [tarray::table::vfill var $value $index]
            } else {
                return [tarray::column::vfill var $value $index]
            }
        }
    }

    proc table_column_assign_range {varname colname value low high} {
        # varname is the name of a table variable (must exist)
        # colname is the name of the column to assign to
        # value is the value to be assigned
        # [low high] is the range to assign to
        
        upvar 1 $varname var

        lassign [tarray::types $var $value] vartype valuetype
        
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        # TBD - need to handle "end" in range specification
        
        if {$low > $high} {
            error "Range lower limit $low is greater than upper limit $high."
        }
        set target_size [expr {$high - $low + 1}]

        # If the value is table, we assume each row in the value
        # is to be assigned successively to the target range. Otherwise
        # it is a value to be filled in the target range.
        if {$valuetype ne "table"} {
            if {$valuetype ne ""} {
                # If the specified value is a column, convert it to
                # a table
                set value [tarray::table::create2 [list $colname] [list $value]]
            } else {
                # Plain Tcl value, Try converting to a table first.
                set table_def [tarray::table::definition $var [list $colname]]
                if {[catch {
                    set value [tarray::table::create $table_def $value]
                }]} {
                    # value cannot be treated as a column or rowvalues
                    # Try treating as a single cell of the table
                    return [tarray::table::vfill -columns [list $colname] var $value $low $high]
                }
            }
        }

        # We were passed a value that was a table or could be converted to one
        # We have to use a put. Make sure the source range
        # and target range match
        set source_size [tarray::table::size $value]
        if {$target_size != $source_size} {
            error "Source size $source_size differs from target table range $low:$high."
        }
        return [tarray::table::vput -columns [list $colname] var $value $low]
    }

    proc table_column_assign {varname colname value index} {
        # varname is the name of a table variable (must exist)
        # colname is the column name
        # value is the value to be assigned
        # index is a general expression, might be a single integer value,
        # an integer column, a list of integers or
        # something else.
        
        upvar 1 $varname var
        lassign [tarray::types $var $value $index] vartype valuetype indextype
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        if {$indextype ne "" && $indextype ne "int"} {
            error "Invalid type for index. Must be an integer, integer list or column."
        }

        if {$valuetype ne "table"} {
            if {$indextype eq ""} {
                set index [tarray::column create int $index]
                set indextype int
            }
            if {$valuetype ne ""} {
                # If the specified value is a column, convert it to a table
                set value [tarray::table::create2 [list $colname] [list $value]]
            } else {
                # Plain Tcl value. 
                # Try converting to a table first.
                set table_def [tarray::table::definition $var [list $colname]]
                if {[catch {
                    set value2 [tarray::table::create $table_def $value]
                    # Successful conversion. Now check if the size
                    # of the value table is equal to the size of the index
                    # list. If not, then we cannot use vplace below.
                    # Only possibility is that it is a single element to
                    # be filled.
                    set size [tarray::table::size $value2]
                    if {$size != [tarray::column::size $index] && $size == 1} {
                        return [tarray::table::vfill -columns [list $colname] var $value $index]
                    }
                    # All is hunky dory or there is an type error. Either way
                    # let the vplace deal with it. We've done our best
                    set value $value2                    
                }]} {
                    # value cannot be treated as a column or rowvalues
                    # Try treating as a single cell of the table
                    return [tarray::table::vfill -columns [list $colname] var $value $index]
                }
            }
        }


        # If value is a table we use vplace to update the target array.
        # In this case, the table dimensions and type must match and
        # indexlist must be a int tarray or an int list else
        # vplace will throw an error.
        return [tarray::table::vplace -columns [list $colname] var $value $index]
    }

    proc table_columns_replace {varname colnames value} {
        # Replaces the specified columns. value must also be a table
        upvar 1 $varname var
        lassign [tarray::types $var $value] vartype valuetype
        if {$vartype ne "table" && $valuetype ne "table"} {
            error "Operand is not a tarray table"
        }

        if {[tarray::table::width $value] != [llength $colnames]} {
            error "Number of source and target columns differ."
        }

        # TBD - check efficiency
        set var2 $var;          # Don't want to modify var in case or errors
        
        foreach colname $colnames newcol [tarray::table::columns $value] {
            tarray::table::vcolumn var2 $colname $newcol
        }
        return [set var $var2]
    }

    proc table_columns_assign_range {varname colnames value low high} {
        # varname is the name of a table variable (must exist)
        # colnames is the list of column names to assign to
        # value is the value to be assigned
        # [low high] is the range to assign to
        
        upvar 1 $varname var

        lassign [tarray::types $var $value] vartype valuetype
        
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        # TBD - need to handle "end" in range specification
        
        if {$low > $high} {
            error "Range lower limit $low is greater than upper limit $high."
        }
        set target_size [expr {$high - $low + 1}]

        # If the value is table, we assume each row in the value
        # is to be assigned successively to the target range. Otherwise
        # it is a value to be filled in the target range.
        if {$valuetype ne "table"} {
            if {$valuetype ne ""} {
                error "Cannot assign a column to a table"
            }

            # Plain Tcl value, Try converting to a table first.
            set table_def [tarray::table::definition $var $colnames]
            if {[catch {
                set value [tarray::table::create $table_def $value]
            }]} {
                # value cannot be treated as a table or rowvalues
                # Try treating as a single row of the table
                return [tarray::table::vfill -columns $colnames var $value $low $high]
            }
        }

        # We were passed a value that was a table or could be converted to one
        # We have to use a put. Make sure the source range
        # and target range match
        set source_size [tarray::table::size $value]
        if {$target_size != $source_size} {
            error "Source size $source_size differs from target table range $low:$high."
        }
        return [tarray::table::vput -columns $colnames var $value $low]
    }

    proc table_columns_assign {varname colnames value index} {
        # varname is the name of a table variable (must exist)
        # colnames is a list of column names
        # value is the value to be assigned
        # index is a general expression - might be a single integer value,
        # a list of integers or
        # something else. For the first two, vplace/vfill do the right
        # thing. For others, they will raise an error.
        
        upvar 1 $varname var
        lassign [tarray::types $var $value] vartype valuetype
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        if {$valuetype ne "table"} {
            if {$valuetype ne ""} {
                error "Cannot assign a column to a table"
            }

            # Plain Tcl value, Try converting to a table first.
            set table_def [tarray::table::definition $var $colnames]
            if {[catch {
                set value [tarray::table::create $table_def $value]
            }]} {
                # value cannot be treated as list of rowvalues
                # Try treating as a single rowvalue of the table
                return [tarray::table::vfill -columns $colnames var $value $index]
            }
        }

        # In this case, the table dimensions and type must match and
        # indexlist must be a int tarray or an int list else
        # vplace will throw an error.
        return [tarray::table::vplace -columns $colnames var $value $index]
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
        if {[lindex [tarray::types $a] 0] eq ""} {
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
        if {[lindex [tarray::types $selexpr] 0] eq ""} {
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
        if {[lindex [tarray::types $tab_or_col] 0] eq "table"} {
            return [tarray::table::size [selector_context]]
        } else {
            return [tarray::column::size [selector_context]]
        }
    }

    proc Index {val index} {
        return [switch -exact -- [lindex [tarray::types $val] 0] {
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
