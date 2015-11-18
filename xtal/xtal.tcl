namespace eval xtal {
    # In production code we use the critcl C-based parser. For development
    # build the parser on the fly using the oo based parser. To do this,
    # set the _use_oo_parser to 1 in the shell, package require tarray
    # and then source this file.
    variable _use_oo_parser
    if {![info exists _use_oo_parser]} {
        set _use_oo_parser 0
    }

    namespace export xtal
}

package require tarray
if {! $xtal::_use_oo_parser} {
    # Production - we are already embedded into the critcl based parser
    # so nothing to do
} else {
    # Development / grammar debug
    catch {xtal::ParserBase destroy}
    catch {xtal::Parser destroy}
    catch {xtal::Compiler destroy}
    lappend auto_path [file join [pwd] ../build/lib]
    package require pt::pgen
    package require fileutil
    # Next line because the generated code has a return which
    # causes script to exit if not caught
    switch [catch {
        eval [pt::pgen peg [fileutil::cat ../xtal/xtal.peg] oo -class xtal::ParserBase -package xtal -version 0.1]
    } msg opts] {
        0 - 2 {}
        default {
            return -options $opts $msg
        }
    }
}

package require pt::ast
package require pt::util

# tcllib-1.17 (or the corresponding pt package) has a bug in Readables
# with the use of the undefined variable details. Replace that routine
# in case present. TBD - once fixed in pt releases, revisit this
catch {
    # Make sure pt::util::Readables is loaded (in case of lazy loading)
    catch {pt::util::Readables}
    if {[regexp {details} [info body pt::util::Readables]]} {
        proc ::pt::util::Readables {msgs} {
            set cl {}
            set r {}
            foreach pe $msgs {
                switch -exact -- [lindex $pe 0] {
                    t {
                        # Fuse to multiple 't'-tags into a single 'cl'-tag.
                        lappend cl [lindex $pe 1]
                    }
                    cl {
                        # Fuse multiple 'cl'-tags into one.
                        foreach c [split [lindex $pe 1]] { lappend cl $c }
                    }
                    default {
                        lappend r [Readable $pe]
                    }
                }
            }
            if {[set n [llength $cl]]} {
                if {$n > 1} {
                    lappend r [Readable [list cl [join [lsort -dict $cl] {}]]]
                } else {
                    lappend r [Readable [list t [lindex $cl 0]]]
                }
            }
            return [lsort -dict $r]
        }
    }
}

namespace eval xtal::ast {
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

proc xtal::_map_search_op {op} {
    # Maps relational operators to column search switches
    # Note the order of options in each entry is important as they 
    # are checked for in other places.
    set map {
        ==  {-eq}
        !=  {-not -eq}
        <   {-lt}
        <=  {-not -gt}
        >   {-gt}
        >=  {-not -lt}
        =^  {-nocase -eq}
        !^  {-nocase -not -eq}
        ~  {-re}
        !~  {-not -re}
        ~^ {-nocase -re}
        !~^ {-nocase -not -re}
    }
    
    if {[dict exists $map $op]} {
        return [dict get $map $op]
    } else {
        return ""
    }
}
    
proc xtal::_map_search_op_reverse {op} {
    # Like _map_search_op except that instead of COLUMN OP OPERAND
    # this maps OPERAND OP COLUMN
    # Note regexp and glob operators are missing here since
    # for consistency, the pattern must only appear on the RHS side
    # of the operator.
    # Note the order of options in each entry is important as they 
    # are checked for in other places.
    set map {
        ==  {-eq}
        !=  {-not -eq}
        <   {-gt}
        <=  {-not -lt}
        >   {-lt}
        >=  {-not -gt}
        =^  {-nocase -eq}
        !^  {-nocase -not -eq}
    }
    if {[dict exists $map $op]} {
        return [dict get $map $op]
    } else {
        return ""
    }
} 

proc xtal::_init_compiler {} {
    xtal::Compiler create compiler $::xtal::_use_oo_parser
    ::proc _init_compiler {args} {}
    return
}

proc xtal::translate {script} {
    _init_compiler
    ::proc translate {script} {
        return [compiler translate $script]
    }
    tailcall translate $script
}

proc xtal::xtal {script} {
    uplevel 1 [translate $script]
}

proc xtal::function {name arguments body} {
   _init_compiler
    ::proc [namespace current]::proc {name arguments body} {
        uplevel 1 [list ::proc $name $arguments [compiler translate $body]]
    }
    tailcall [namespace current]::proc $name $arguments $body
}

proc xtal::method {class name arguments body} {
   _init_compiler
    ::proc [namespace current]::method {class name arguments body} {
        uplevel 1 [list ::oo::define $class method $name $arguments [compiler translate $body]]
    }
    tailcall [namespace current]::method $class $name $arguments $body
}

proc xtal::objmethod {obj name arguments body} {
   _init_compiler
    ::proc [namespace current]::objmethod {obj name arguments body} {
        uplevel 1 [list ::oo::objdefine $obj method $name $arguments [compiler translate $body]]
    }
    tailcall [namespace current]::objmethod $obj $name $arguments $body
}

proc xtal::translate_file {args} {
    set nargs [llength $args]
    if {$nargs != 1 && $nargs != 3} {
        error "invalid syntax: should be \"tsource ?-encoding ENCODING? PATH\""
    }
    set path [lindex $arg end]
    if {$nargs == 3} {
        if {[lindex $args 0] ne "-encoding"} {
            error "invalid syntax: should be \"tsource ?-encoding ENCODING? PATH\""
        }
        set encoding [lindex $args 1]
    }
    set fd [open $path r]
    try {
        if {[info exists encoding]} {
            fconfigure $fd -encoding $encoding -translation auto
        } else {
            fconfigure $fd -translation auto
        }
        set script [read $fd]
        return [translate $script]
    } finally {
        close $fd
    }
}

proc xtal::source {args} {
    set script [translate_file {*}$args]
    return [uplevel 1 [list [namespace current]::xtal $script]]
}

proc xtal::compile {from to args} {
    set nargs [llength $args]
    if {$nargs != 2 && $nargs != 4} {
        error "invalid syntax: should be \"tcompile ?-encoding ENCODING? XTALFILE TCLFILE\""
    }
    set from [lindex $args end-1]
    set to [lindex $args end]
    set args [lrange $args 0 end-2]
    set script [translate_file $from {*}$args]
    set fd [open $to w]
    try {
        if {[dict exists $args -encoding]} {
            fconfigure $fd -translation auto -encoding [dict get $args -encoding]
        } else {
            fconfigure $fd -translation auto
        }
        puts $fd $script
    } finally {
        close $fd
    }
}


oo::class create xtal::Parser {
    variable Script

    constructor {use_oo_parser} {
        if {$use_oo_parser} {
            xtal::ParserBase create baseparser
        } else {
            xtal::ParserBase_critcl baseparser
        }
    }

    forward parse baseparser parse
    forward parset baseparser parset
    
    method ast {text} {
        xtal::ast::print $text [my parset $text]
    }
    
    method _print {node {indent {}}} {
        set children [lassign $node name]
        puts "${indent}$name -> $children"
        foreach child $children {
            my _print $child "    ${indent}"
        }
    }

    method print {text} {
        foreach stmt [my translate $text] {
            my _print $stmt
        }
    }

    method translate {text} {
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

    method Program {from to child} {
        return $child
    }

    method Block {from to args} {
        return $args
    }

    method Statement {from to {child {}}} {
        if {[llength $child]} {
            return [list Statement $child]
        } else {
            return {}
        }
    }

    method IfStatement {from to args} {
        return [list IfStatement {*}$args]
    }

    method ElseClause {from to clause} {
        return [list ElseClause $clause]
    }

    method ElseifClause {from to expr clause} {
        return [list ElseifClause $expr $clause]
    }
                
    method WhileStatement {from to args} {
        return [list WhileStatement {*}$args]
    }

    method ReturnStatement {from to {expr {}}} {
        return [list ReturnStatement $expr]
    }
    
    method BreakStatement {from to} {
        return [list BreakStatement]
    }

    method ContinueStatement {from to} {
        return [list ContinueStatement]
    }
    
    method ForRangeStatement {from to loopvar low args} { 
        set nargs [llength $args]
        set loopvar [lindex $loopvar 1]
        if {$nargs == 1} {
            # No upper limit, increment defaults to 1
            return [list ForRangeStatement $loopvar $low {} {Number 1} [lindex $args 0]]
        }

        if {$nargs == 2} {
            if {[lindex $args 0 0] eq "ForRangeIncrement"} {
                # No upper limit but increment specified
                return [list ForRangeStatement $loopvar $low {} [lindex $args 0 1] [lindex $args 1]]
            } else {
                # Upper limit, default increment
                return [list ForRangeStatement $loopvar $low [lindex $args 0] {Number 1} [lindex $args 1]]
            }
        }

        if {$nargs == 3 &&
            [lindex $args 0 0] ne "ForRangeIncrement" &&
            [lindex $args 1 0] eq "ForRangeIncrement"} {
            # Both upper limit and increment specified
            return [list ForRangeStatement $loopvar $low [lindex $args 0] [lindex $args 1 1] [lindex $args 2]]
        }

        error "Internal error parsing for statement. Unexpected argument count or types"
    }

    method ForRangeIncrement {from to args} {
        return [list ForRangeIncrement {*}$args]
    }
    
    method ForEachStatement {from to var args} { 
        if {[llength $args] == 2} {
            # foreach loopvar collection body
            return [list ForEachStatement [lindex $var 1] {*}$args]
        } else {
            # foreach keyvar loopvar collection body
            return [list ForEachKeyedStatement [lindex $var 1] [lindex $args 0 1] [lindex $args 1] [lindex $args 2]]
        }
    }

    method TryStatement {from to block args} {
        return [list TryStatement $block {*}$args]
    }
    
    method OnHandler {from to returncode args} {
        # on errorcode (, var)* body
        set vars [lmap arg [lrange $args 0 end-1] {
            lindex $arg 1
        }]
        return [list OnHandler [lindex $returncode 1] $vars [lindex $args end]]
    }
    
    method TrapHandler {from to trappattern args} {
        # trap trappattern (, var)* body
        set vars [lmap arg [lrange $args 0 end-1] {
            lindex $arg 1
        }]
        return [list TrapHandler $trappattern $vars [lindex $args end]]
    }
    
    method FinallyClause {from to block} {
        return [list FinallyClause $block]
    }

    forward ReturnCode my _extract ReturnCode
    
    method ThrowStatement {from to args} {
        return [list ThrowStatement {*}$args]
    }
                           
    method Assignment {from to lvalue assignop expr} {
       if {[lindex $lvalue 0] eq "Identifier"} {
            set ident [lindex $lvalue 1]
            # Try for some optimization that can make use of the v* form of
            # when target of assignment is a simple identifier
            switch -exact -- [lindex $expr 0] {
                SortCommand {
                    # We can optimize sorts of the form
                    # var = @sort var options
                    # provided options does not contain the -indices option
                    if {[lindex $expr 1 0] eq "Identifier" &&
                        [lindex $expr 1 1] eq $ident &&
                        ([llength $expr] == 2 ||
                         ([lindex $expr 2 0] eq "SortOptions" &&
                          "-indices" ni [lindex $expr 2 1]))} {
                        return [list VSort $ident [lindex $expr 2 1]]
                    }
                }
                BuiltInCall {
                    if {[lindex $expr 2 1 0 0] eq "Identifier" &&
                        [lindex $expr 2 1 0 1] eq $ident} {
                        switch -exact -- [lindex $expr 1] {
                            delete -
                            inject -
                            insert -
                            reverse {
                                return [list VBuiltInCall v[lindex $expr 1] [lindex $expr 2]]
                            }
                        }
                    }
                }
            }
        }

        # No optimizations detected
        return [list [lindex $assignop 1] $lvalue $expr]
    }

    method LValue {from to first_child args} {
        if {[llength $args] == 0} {
            return $first_child;
        } else {
            return [switch -exact -- [lindex $args 0 0] {
                Element {
                    # args is element node followed by remaining args
                    #
                    list LValueElement [lindex $first_child 1] {*}[lrange $args 0 end]
                }
                TableColumns {
                    # args is column node, remaining args
                    list LValueTableColumns [lindex $first_child 1] {*}$args
                }
                default {
                    list LValueTarray [lindex $first_child 1] {*}$args
                }
            }]
        }
    }

    forward Expression my _child

    method RangeExpr {from to first_child args} {
        if {[llength $args] == 0} {
            # RangeExpr EXPR
            return $first_child
        } elseif {[llength $args] == 1} {
            # RangeExpr EXPR
            return [list Range $first_child]
        } else {
            # RangeExpr EXPR : EXPR 
            return [list Range $first_child [lindex $args 1]]
        }
    }

    method RangeSeparator {from to} {
        return [list RangeSeparator [string range $Script $from $to]]
    }
            
    method AddExpr {from to first_child args} {
        # args will be a list of alternating AddOp and operand nodes
        if {[llength $args] == 0} {
            # Simply promote and return the child
            return $first_child
        }

        # TBD - AddExpr reordering for optimization might lead
        # to different overflow behaviour
        # Maybe it should not do that? Probably ok for integers
        # because even with overflow, result will be the same

        # We want to do constant folding and also optimize number of
        # calls the runtime will make to tarray::column::math.
        # const will hold the result of constant folding
        # positives will hold the non-const operands whose operator is +
        # negatives will hold the non-const operands whose operator is -
        # Note the operand itself may be positive or negative as well!
        set negatives {}
        if {[lindex $first_child 0] eq "Number"} {
            set const [lindex $first_child 1]
            set positives {}
        } else {
            set const 0
            set positives [list $first_child]
        }
        foreach {op operand} $args {
            if {[lindex $operand 0] eq "Number"} {
                # Note: don't use incr because it cannot handle op=-, operand=-4
                append const [lindex $op 1] [lindex $operand 1]
            } else {
                if {[lindex $op 1] eq "+"} {
                    lappend positives $operand
                } else {
                    lappend negatives $operand
                }
            }
        }
        # Result is const + positive - negatives. Optimize for when
        # various components are missing.
        set const [expr $const]; # Fold constants
        if {$const == 0} {
            if {[llength $positives] == 0} {
                # No positives, const 0
                if {[llength $negatives] == 0} {
                    # No positives, no negatives const 0
                    return [list Number 0]
                } else {
                    # No positives, at least one negative, const 0
                    return [list - [list Number 0] {*}$negatives]
                }
            } else {
                # At least one positive, const 0
                if {[llength $negatives] == 0} {
                    # Only positives, no negatives, const = 0
                    if {[llength $positives] == 1} {
                        # A single positive, no negatives, const 0
                        return [lindex $positives 0]
                    } else {
                        # Multiple positives, no negatives, const 0
                        return [list + {*}$positives]
                    }
                } else {
                    # Both positives and negatives, const 0
                    if {[llength $positives] == 1} {
                        # Single positive, at least one negative, const 0
                        if {[llength $negatives] == 1} {
                            # Single positive, single negative, const 0
                            return [list - [lindex $positives 0] [lindex $negatives 0]]
                        } else {
                            # Single positive, multiple negative, const 0
                            return [list - [lindex $positives 0] [list + {*}$negatives]]
                        }
                    } else {
                        # Multiple positive, at least one negative, const 0
                        if {[llength $negatives] == 1} {
                            # Multiple positive, single negative, const 0
                            return [list - [list + {*}$positives] [lindex $negatives 0]]
                        } else {
                            # Multiple positive, multiple negatives, const 0
                            return [list - [list + {*}$positives] [list + {*}$negatives]]
                        }
                    }
                }                
            }
        } else {
            # const non-0
            set const [list Number $const]
            if {[llength $positives] == 0} {
                # No positives, const non-0
                if {[llength $negatives] == 0} {
                    # No positives, no negatives const non-0
                    return $const
                } else {
                    # No positives, at least one negative, const non-0
                    return [list - $const {*}$negatives]
                }
            } else {
                # At least one positive, const non-0
                if {[llength $negatives] == 0} {
                    # Only positives, no negatives, const non-0
                    return [list + $const {*}$positives]
                } else {
                    # Both positives and negatives, const non-0
                    return [list - [list + $const {*}$positives] {*}$negatives]
                }
            }
        }
        error "Internal error: missed a case in AddExpr!"
    }

    method _leftassoc_fold {from to first_child args} {
        if {[llength $args] == 0} {
            return $first_child
        }
        set command {}
        set prev_operand $first_child
        set fold 1
        foreach {op operand} $args {
            set op [lindex $op 1]
            if {$fold} {
                if {[lindex $prev_operand 0] eq "Number" &&
                    [lindex $operand 0] eq "Number"} {
                    # Fold leading constants
                    set prev_operand [list Number [expr [list [lindex $prev_operand 1] $op [lindex $operand 1]]]]
                } else {
                    # Can't fold any more
                    set prev_operand [list $op $prev_operand $operand]
                    set fold 0
                }
            } else {
                # No folding going on now. If this operation same as last
                # operation, add the new operand
                if {$prev_op eq $op} {
                    lappend prev_operand $operand
                } else {
                    set prev_operand [list $op $prev_operand $operand]
                }
            }
            set prev_op $op
        }
        return $prev_operand
    }

    forward LogicalOrExpr my _leftassoc_fold
    forward LogicalAndExpr my _leftassoc_fold
    forward MulExpr my _leftassoc_fold
    forward BitOrExpr my _leftassoc_fold
    forward BitXorExpr my _leftassoc_fold
    forward BitAndExpr my _leftassoc_fold
    forward RelExpr my _leftassoc_fold

    method UnaryExpr {from to postfix_expr args} {
        if {[llength $args] == 0} {
            return $postfix_expr
        } else {
            # postfix_expr is actually UnaryOp.
            # $args should be a single argument again of type UnaryExpr
            # or a descendant
            set op [lindex $postfix_expr 1]
            set postfix_expr [lindex $args 0]

            if {[lindex $postfix_expr 0] eq "Number"} {
                return [list Number [::tcl::mathop::$op [lindex $postfix_expr 1]]]
            } elseif {[lindex $postfix_expr 0] eq "UnaryExpr" &&
                      $op eq [lindex $postfix_expr 1]} {
                # Note: A unary + is a no-op so we could ignore it. However
                # we don't discard it because it is an error if the operand
                # is not numeric (which we cannot know if it is not a literal).
                # Consecutive ++ can be shrunk to +. Consecutive
                # "--" or "~~" can be completely discarded. "!" changes
                # the value of numerics (!! is not a no-op) so we leave it
                # alone.

                if {$op in {- ~}} {
                    return [lindex $postfix_expr 2]
                }
                if {$op eq "+"} {
                    return $postfix_expr
                }
            }

            # Special cases above did not apply. Need to do operation at runtime
            return [list UnaryExpr $op {*}$args]
        }
    }
    
    method PostfixExpr {from to primary_expr args} {
        if {[llength $args] == 0} {
            return $primary_expr
        }
        # Optimize I[I RelOp Operand] to I[@@ RelOp Operand]
        # in preparation for further optimization in second pass.
        if {[lindex $primary_expr 0] eq "Identifier" &&
            [lindex $args 0 0] eq "Selector"} {
            set selector [lindex $args 0 1]
            if {[llength $selector] == 3} {
                # TBD - do we need to check [lindex $selector 0] is a RelOp?
                if {[lindex $selector 1 0] eq "Identifier" &&
                    [lindex $selector 1 1] eq [lindex $primary_expr 1]} {
                    lset selector 1 "SelectorContext"
                }
                if {[lindex $selector 2 0] eq "Identifier" &&
                    [lindex $selector 2 1] eq [lindex $primary_expr 1]} {
                    lset selector 2 "SelectorContext"
                }
                return [list PostfixExpr $primary_expr [list Selector $selector] {*}[lrange $args 1 end]]
            } 
        }
        return [list PostfixExpr $primary_expr {*}$args]
    }

    forward PrimaryExpr my _child

    forward BuiltIn my _child
    
    forward PostfixOp my _child

    method ColumnType {from to} {
        return [string range $Script $from $to]
    }

    method ColumnConstructor {from to coltype args} {
        return [list ColumnConstructor $coltype {*}$args]
    }

    method TableConstructor {from to args} {
        if {[llength $args] == 1} {
            # A single arg may be the column definition list or the initializer
            if {[lindex $args 0 0] eq "TableColumnDefs"} {
                return [list TableConstructor [lindex $args 0 1] [list Sequence]]
            } else {
                return [list TableConstructor [list ] [lindex $args 0]]
            }
        } else {
            # Both column definition list and initializer are present
            return [list TableConstructor [lindex $args 0 1] [lindex $args 1]]
        }
    }

    method TableColumnDefs {from to args} {
        return [list TableColumnDefs [concat {*}$args] ]
    }

    method TableColumnDef {from to colname coltype} {
        return [list [lindex $colname 1] $coltype]
    }
        
    method Selector {from to child} {
        return [list Selector $child]
    }

    method FunctionCall {from to args} {
        return [linsert $args 0 FunctionCall]
    }

    method ArgumentList {from to args} {
        return [list ArgumentList {*}$args]
    }

    method Argument {from to args} {
        return $args
    }

    method Element {from to op child} {
        return [list Element $child]
    }

    method TableColumns {from to op {child {}}} {
        return [linsert $child 0 TableColumns]
    }

    method TableColumnList {from to args} {
        return $args
    }

    method ColumnIdentifier {from to child} {
        return $child
    }
    
    method ElementIdentifier {from to child} {
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
    forward ColumnOp my _extract ColumnOp
    forward ElementOp my _extract ElementOp

    method SelectorContext {from to} {
        return SelectorContext
    }

    method ParameterIdentifier {from to} {
        # TBD - why not forward this
        return [string range $Script $from $to]
    }
    
    method Identifier {from to} {
        # TBD - why not forward this to _extract?
        return [list Identifier [string range $Script $from $to]]
    }

    method IndirectIdentifier {from to} {
        return [list IndirectIdentifier [string range $Script [expr {1+$from}] $to]]
    }

    method IndirectLiteral {from to child} {
        return [list IndirectLiteral [lindex $child 1]]
    }

    method OptionString {from to} {
        # TBD - why not forward this to _extract?
        return [list OptionString [string range $Script $from $to]]
    }

    forward Number my _extract Number

    method String {from to child} {
        return $child
    }
    
    method PlainString {from to} {
        return [list PlainString [subst -novariables -nocommands [string range $Script $from+1 $to-1]]]
    }

    # We have to deal with TclStrings at run time because substitutions
    # need to happen then
    method TclString {from to} {
        # TBD - Optimize - if no special chars then return as PlainString
        # so as to not do unnecessary runtime processing
        return [list TclString [string range $Script $from+1 $to-1]]
    }
    
    method Sequence {from to {child {}}} {
        return [linsert $child 0 Sequence]
    }

    method SequenceContent {from to args} {
        return $args
    }

    method TclScriptBlock {from to child} {
        return $child
    }
    
    method TclScript {from to} {
        return [list TclScript [string trim [string range $Script $from $to]]]
    }

    method ListCast {from to child} {
        return [list ListCast $child]
    }
    
    method DictCast {from to child} {
        return [list DictCast $child]
    }

    method SortCommand {from to expr args} {
        return [list SortCommand $expr {*}$args]
    }
    
    method SortOptions {from to args} {
        return [list SortOptions $args]
    }
    
    method SortOption {from to} {
        return "-[string range $Script $from $to]"
    }

    method SearchCommand {from to expr args} {
        return [list SearchCommand $expr {*}$args]
    }
    
    method SearchTarget {from to expr} {
        return [list SearchTarget $expr]
    }
    
    method SearchOption {from to} {
        return "-[string range $Script $from $to]"
    }

    method BuiltInCall {from to args} {
        return [list BuiltInCall {*}$args]
    }

    method BuiltInFunction {from to} {
        return [string range $Script $from $to]
    }

    method FunctionDefinition {from to name params body} {
        return [list FunctionDefinition [lindex $name 1] $params $body]
    }

    method ParameterDefinitions {from to args} {
        return $args
    }
    
    method Parameter {from to args} {
        return $args
    }
}

oo::class create xtal::Compiler {
    variable Script Compilations SelectorNestingLevel IndentLevel

    constructor {use_oo_parser} {
        namespace path ::xtal
        xtal::Parser create parser $use_oo_parser
    }

    forward print parser print

    method translate {script} {
        if {[info exists Compilations($script)]} {
            return $Compilations($script)
        }

        set ir [parser translate $script]

        # Initialize the per-compile variables
        set Script $script
        set SelectorNestingLevel 0
        set IndentLevel 0
        set code {}
        foreach stmt $ir {
            # Disregard empty nodes (blank lines)
            if {[llength $stmt] != 0} {
                append code [my {*}$stmt]\n
            }
        }
        
        return [set Compilations($script) $code]
    }

    method eval {script} { uplevel 1 [my translate $script] }

    method Indent {} {return [string repeat "  " $IndentLevel]}
    
    method Statement {child} {
        # If not an assignment operator, for example just a function call
        # or variable name, need explicit return else we land up with
        # something like {[set x]} as the compiled code
        if {[lindex $child 0] in {= IfStatement WhileStatement ForRangeStatement ForEachStatement ForEachKeyedStatement ReturnStatement BreakStatement ContinueStatement FunctionDefinition TryStatement ThrowStatement}} {
            return "[my Indent][my {*}$child]"
        } else {
            return "[my Indent]return -level 0 [my {*}$child]"
        }
    }

    method _clause_to_code {clause} {
        incr IndentLevel
        set code ""
        foreach stmt $clause {
            if {[llength $stmt]} {
                append code "[my {*}$stmt]\n"
            }
        }
        incr IndentLevel -1
        return $code
    }
    
    method IfStatement {cond clause args} {
        set code "if {\[xtal::rt::condition [my {*}$cond]\]} {\n[my _clause_to_code $clause][my Indent]}"

        foreach arg $args {
            append code " [my {*}$arg]"
        }

        append code \n
        return $code

        set code "if {\[xtal::rt::condition [my {*}$cond]\]} {\n$then_code"
        if {[llength $args] == 0} {
            return "${code}[my Indent]\}"
        }

        set else_code [my _clause_to_code [lindex $args end]]
        return "${code}} {\n[my Indent]$else_code[my Indent]}"
        #return "if {\[xtal::rt::condition [my {*}$cond]\]} {\n$then_code} {\n[my Indent]$else_code[my Indent]}"
    }

    method ElseClause {clause} {
        return "else {\n[my Indent][my _clause_to_code $clause][my Indent]}"
    }
    
    method ElseifClause {cond clause} {
        return "elseif {\[xtal::rt::condition [my {*}$cond]\]} {\n[my _clause_to_code $clause][my Indent]}"
    }

    method ReturnStatement {expr} {
        if {[llength $expr]} {
            return "return [my {*}$expr]"
        } else {
            return "return"
        }
    }
    
    method BreakStatement {} {
        return "break"
    }

    method ContinueStatement {} {
        return "continue"
    }
    
    method WhileStatement {cond clause} {
        return "while {\[xtal::rt::condition [my {*}$cond]\]} {\n[my _clause_to_code $clause][my Indent]}"
    }

    method TryStatement {body args} {
        set code "try {\n[my _clause_to_code $body]\n[my Indent]}"
        foreach arg $args {
            switch -exact -- [lindex $arg 0] {
                OnHandler {
                    append code " on [lindex $arg 1] {[lindex $arg 2]} {\n[my _clause_to_code [lindex $arg 3]][my Indent]}"
                }
                TrapHandler {
                    append code " trap [my {*}[lindex $arg 1]] {[lindex $arg 2]} {\n[my _clause_to_code [lindex $arg 3]][my Indent]}"
                }
                FinallyClause {
                    append code " finally {\n[my _clause_to_code [lindex $arg 1]][my Indent]}"
                }
            }
        }
        return $code
    }
    
    method ThrowStatement {args} {
        if {[llength $args] > 1} {
            return "throw [my Sequence {*}$args] [my {*}[lindex $args end]]"
        } else {
            return "error [my {*}[lindex $args 0]]"
        }
    }
    
    method ForRangeStatement {loopvar low high incr clause} {
        if {[llength $high]} {
            return "for {set $loopvar [my {*}$low]} {\[set $loopvar\] <= [my {*}$high]} {incr $loopvar [my {*}$incr]} {\n[my _clause_to_code $clause][my Indent]}"
        } else {
            # Make the conditional 1 since Tcl for does not allow empty cond
            return "for {set $loopvar [my {*}$low]} {1} {incr $loopvar [my {*}$incr]} {\n[my _clause_to_code $clause][my Indent]}"
        }
    }

    method ForEachKeyedStatement {keyvar loopvar looptarget clause} {
        return "xtal::rt::keyedforloop $keyvar $loopvar [my {*}$looptarget] {\n[my _clause_to_code $clause][my Indent]}"
    }
    
    method ForEachStatement {loopvar looptarget clause} {
        return "xtal::rt::forloop $loopvar [my {*}$looptarget] {\n[my _clause_to_code $clause][my Indent]}"
    }

    method = {lvalue rvalue} {
        lassign $lvalue type ident indexexpr
        switch -exact -- $type {
            Identifier {
                return "set $ident [my {*}$rvalue]"
            }
            LValueTarray {
                # We are assigning to elements in a tarray. The elements
                # to be assigned may be specified through a range or
                # a general expression that results in an index or index list.
                lassign $lvalue type ident indexexpr
                switch -exact -- [lindex $indexexpr 0] {
                    Range {
                        return "xtal::rt::tarray_assign_range $ident [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric index
                        return "xtal::rt::tarray_assign_element $ident [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        set frag "xtal::rt::tarray_assign $ident [my {*}$rvalue] [my {*}$indexexpr]"
                        # TBD - optimize by only pushing context if selector has @@
                        return [string map [list %IDENT% "\[set $ident\]" %FRAG% $frag] {
                            xtal::rt::push_selector_context %IDENT%
                            try {
                                return -level 0 [%FRAG%]
                            } finally {
                                xtal::rt::pop_selector_context
                            }
                        }]
                    }
                }
            }
            LValueElement {
                # Assigning to a single element within a dict/column/table
                lassign $lvalue elemtype operand element indexexpr
                set element [my Element $element]

                # Note - only tables and columns can have indices
                # TBD - if $indexexpr is not "", it has to be a table?
                # Because C.elem[index] does not make sense if C is a column
                switch -exact -- [lindex $indexexpr 0] {
                    "" {
                        # T.c = ....
                        # No index 
                        return "xtal::rt::assign_element $operand $element [my {*}$rvalue]"
                    }
                    Range {
                        # T.c[4:j] = ...
                        return "xtal::rt::table_column_assign_range $operand $element [my {*}$rvalue] {*}[my {*}$indexexpr]"
                    }
                    Number {
                        # Single numeric literal index
                        # T.c[0] = ...
                        return "xtal::rt::table_column_assign $operand $element [my {*}$rvalue] [my {*}$indexexpr]"
                        #return "tarray::table::vfill -columns \[list $element\] $operand [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                    default {
                        # Index is general expression (including single vars)
                        # The actual operation depends on both the
                        # lvalue and the rvalue
                        set stmt "xtal::rt::table_column_assign $operand $element [my {*}$rvalue] [my {*}$indexexpr]"
                        return "xtal::rt::push_selector_context \[xtal::rt::element \[set $operand\] $element\]\ntry {   $stmt\n} finally {\nxtal::rt::pop_selector_context\n}\n"
                    }
                }
            }

            LValueTableColumns {
                lassign $lvalue type table columns indexexpr
                # columns is {TableColumns colnode1....}
                set collist [lmap column [lrange $columns 1 end] {
                    my Element $column
                }]
                
                set collist [join $collist { }]
                switch -exact -- [lindex $indexexpr 0] {
                    "" {
                        # T.(a,b) = ...
                        # No index -> whole columns to be operated
                        return "xtal::rt::table_columns_replace $table \[list $collist\] [my {*}$rvalue]"
                    }
                    Range {
                        return "xtal::rt::table_columns_assign_range $table \[list $collist\] [my {*}$rvalue] {*}[my {*}$indexexpr]"
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
                        return "xtal::rt::table_columns_assign $table \[list $collist\] [my {*}$rvalue] [my {*}$indexexpr]"
                    }
                }
            }

            default {
                error "Internal error: Unexpected node type [lindex $lvalue 0]"
            }
        }
    }

    # TBD - optimizations
    # _mathop, _relop, _strop result in switching on $op at runtime. Change to
    # switch on $op during compile time (may be use _map_search_op etc.)

    method _mathop {op args} {
        return "\[tarray::column::math $op [join [lmap arg $args { my {*}$arg }] { }]\]"
    }

    forward + my _mathop +
    forward - my _mathop -
    forward * my _mathop *
    forward / my _mathop /
    forward | my _mathop |
    forward & my _mathop &
    forward ^ my _mathop ^

    method _relop {op first second} {
        return "\[xtal::rt::relop $op [my {*}$first] [my {*}$second]\]"
    }

    forward == my _relop ==
    forward != my _relop !=
    forward < my _relop <
    forward <= my _relop <=
    forward > my _relop >
    forward >= my _relop >=

    method _boolop {op args} {
        # TBD - short circuit ?
        set operands [lmap operand $args {
            my {*}$operand
        }]
        return "\[xtal::rt::$op [join $operands { }]\]"
    }
    forward && my _boolop and
    forward || my _boolop or

    method _strop {op first second} {
        return "\[xtal::rt::strop $op [my {*}$first] [my {*}$second]\]"
    }
    forward =^ my _strop =^
    forward !^ my _strop !^
    forward ~ my _strop ~
    forward !~ my _strop !~
    forward ~^ my _strop ~^
    forward !~^ my _strop !~^
    # Next 4 are currently removed from the PEG grammar
    # forward =* my _strop =*
    # forward !* my _strop !*
    # forward =*^ my _strop =*^
    # forward !*^ my _strop !*^

    method UnaryExpr {op child} {
        return "\[xtal::rt::unary $op [my {*}$child]\]"
    }

    method SelectorGenerate {primary_expr selector_expr} {
        set frag {
            xtal::rt::push_selector_context %VALUE%
            try {
                return -level 0 [%COMMAND%]
            } finally {
                xtal::rt::pop_selector_context
            }
        }
        set first_op [lindex $selector_expr 1 0]
        incr SelectorNestingLevel
        try {
            if {$first_op eq "Range"} {
                # Optimize C[range]
                set range_high [lindex $selector_expr 1 2]
                if {[llength $range_high]} {
                    set range_high [my {*}$range_high]
                } else {
                    set range_high end
                }
                set command "xtal::rt::range \[xtal::rt::selector_context\] [my {*}[lindex $selector_expr 1 1]] $range_high"
            } elseif {[lindex $selector_expr 1 1] eq "SelectorContext" &&
                      [set searchop [xtal::_map_search_op $first_op]] ne ""} {
                # Optimize C[@@ > 10]
                set command "xtal::rt::search_@@ {$searchop} \[xtal::rt::selector_context\] [my {*}[lindex $selector_expr 1 2]]"
            } elseif {[lindex $selector_expr 1 2] eq "SelectorContext" &&
                      [set searchop [xtal::_map_search_op_reverse $first_op]] ne ""} {
                # Optimize C[10 > @@] etc.
                set command "xtal::rt::search_@@ {$searchop} \[xtal::rt::selector_context\] [my {*}[lindex $selector_expr 1 1]]"
            }

            if {![info exists command]} {
                # No optimization matched. Use generic selector method
                set command "xtal::rt::selector \[xtal::rt::selector_context\] [my {*}$selector_expr]"
            }
            set primary "\[[string map [list %VALUE% $primary_expr %COMMAND% $command] $frag]\]"
        } finally {
            incr SelectorNestingLevel -1
        }
        return $primary
    }
    
    method PostfixExpr {primary_expr args} {
        if {[llength $args] == 0} {
            return [my {*}$primary_expr]
        }

        # For functions, we take identifier as the name of the function,
        # not as a variable containing the name of a function.
        if {[lindex $args 0 0] eq "FunctionCall"} {
            switch -exact -- [lindex $primary_expr 0] {
                IndirectLiteral -
                Identifier { set primary [lindex $primary_expr 1] }
                IndirectIdentifier {
                    set primary "\[xtal::rt::dereference [lindex $primary_expr 1]\]"
                }
                default { set primary [my {*}$primary_expr] }
            }
        } else {
            set primary [my {*}$primary_expr]
        }


        foreach postexpr $args {
            switch -exact -- [lindex $postexpr 0] {
                Selector {
                    set primary [my SelectorGenerate $primary $postexpr]
                }
                FunctionCall {                
                    set fnargs {}
                    set methods {}
                    # Each fnarg might be a method name, a plain argument
                    # or an option with value argument
                    foreach elem [lrange $postexpr 1 end] {
                        if {[lindex $elem 0] eq "Element"} {
                            lappend methods [my {*}$elem]
                        } else {
                            # ArgumentList
                            foreach argelem [lrange $elem 1 end] {
                                foreach fnarg $argelem {
                                    lappend fnargs [my {*}$fnarg]
                                }
                            }
                        }
                    }
                    set primary "\[$primary [join $methods { }] [join $fnargs { }]\]"
                }
                Element {
                    set primary "\[xtal::rt::element $primary [my {*}$postexpr]\]"
                }
                TableColumns {
                    set primary "\[tarray::table::slice $primary [my {*}$postexpr]\]"
                }
            }
        }
        return "$primary"
    }

    method Selector {child} {
        return [my {*}$child]
    }

    method Element {child} {
        switch -exact -- [lindex $child 0] {
            IndirectLiteral -
            Identifier { return [lindex $child 1] }
            IndirectIdentifier {
                return "\[xtal::rt::dereference [lindex $child 1]\]"
            }
            default { return [my {*}$child] }
        }
    }

    method TableColumns {args} {
        set cols [lmap colarg $args {
            my Element $colarg
        }]
        return "\[list [join $cols]\]"
    }

    method SelectorContext {} {
        return "\[xtal::rt::selector_context\]"
    }

    method ColumnConstructor {coltype {inivalue {}}} {
        if {[llength $inivalue]} {
            return "\[xtal::rt::column_create $coltype [my {*}$inivalue]\]"
        } else {
            return "\[tarray::column create $coltype {}\]"
        }
    }

    method TableConstructor {coldefs inivalue} {
        if {[llength $inivalue]} {
            return "\[tarray::table::create {$coldefs} [my {*}$inivalue]\]"
        } else {
            return "\[tarray::table::create {$coldefs}\]"
        }
    }
    
    method Sequence {args} {
        return "\[list [join [lmap arg $args {
            my {*}$arg
        }] { }]\]"
    }
    
    method PlainString s {return "{$s}"}
    method TclString s {return "\[xtal::rt::tclstring {$s}\]"}
    method OptionString s {return "{$s}"}
    method Number {n} {return $n}
    method RangeEnd {} {return "end"}
    method Range {low args} {
        if {[llength $args]} {
            set high [my {*}[lindex $args 0]]
        } else {
            set high "end"
        }
        if {$SelectorNestingLevel} {
            return "\[xtal::rt::selector_range \[list [my {*}$low] $high\]\]"
        } else {
            return "\[list [my {*}$low] $high\]"
        }
    }

    method IndirectIdentifier {ident} {
        return "\[xtal::rt::dereference2 $ident\]"
    }
    
    method IndirectLiteral {lit} {
        return "\[xtal::rt::dereference {$lit}\]"
    }

    method Identifier {ident} {
        return "\[set $ident\]"
    }

    method TclScript s {return "\[try {\n$s\n}\]"}

    method ListCast {expr} {
        return "\[xtal::rt::listcast [my {*}$expr]\]"
    }

    method DictCast {expr} {
        return "\[xtal::rt::dictcast [my {*}$expr]\]"
    }

    method VSort {ident options} {
        return "\[tarray::column::vsort $options {$ident}\]"
    }
    
    method SortCommand {operand args} {

        set options {}
        foreach arg $args {
            if {[lindex $arg 0] eq "SortOptions"} {
                set options [lindex $arg 1]
            } else {
                set target $arg
            }
        }
        if {[info exists target]} {
            return "\[xtal::rt::sort_indirect [my {*}$operand] [my {*}$target] {$options}\]"
        } else {
            return "\[xtal::rt::sort [my {*}$operand] {$options}\]"
        }
    }

    method SearchCommand {operand args} {
        if {[lindex $args 0 0] eq "SearchTarget"} {
            set search_col [my {*}[lindex $args 0]]
            set op [xtal::_map_search_op [lindex $args 1 1]]
            if {$op eq ""} {
                error "Unknown relational operator [lindex $args 1 1]"
            }
            set search_val [my {*}[lindex $args 2]]
            set opts "-among [my {*}$operand] [lrange $args 3 end]"
            # return "\[xtal::rt::search [my {*}[lindex $args 0]] [xtal::_map_search_op [lindex $args 1 1]] [my {*}[lindex $args 2]] -among [my {*}$operand] [lrange $args 3 end]\]"
        } else {
            set search_col [my {*}$operand]
            set op [xtal::_map_search_op [lindex $args 0 1]]
            if {$op eq ""} {
                error "Unknown relational operator [lindex $args 0 1]"
            }
            set search_val [my {*}[lindex $args 1]]
            set opts "[lrange $args 2 end]"
            # return "\[xtal::rt::search [my {*}$operand] [xtal::_map_search_op [lindex $args 0 1]] [my {*}[lindex $args 1]] [lrange $args 2 end]\]" 
        }
        return "\[tarray::column::search $opts $op $search_col $search_val\]"
    }
    
    method SearchTarget {postexpr} {
        return [my {*}$postexpr]
    }

    method BuiltInCall {fn arglist} {
        set fn [dict get {
            delete xtal::rt::delete
            inject xtal::rt::inject
            insert xtal::rt::insert
            lookup tarray::column::lookup
            reverse xtal::rt::reverse
            sum tarray::column::sum
        } $fn]
        set fnargs {}
        foreach argelem [lrange $arglist 1 end] {
            foreach fnarg $argelem {
                lappend fnargs [my {*}$fnarg]
            }
        }
        return "\[$fn [join $fnargs { }]\]"
    }
    
    method VBuiltInCall {fn arglist} {
        set fnargs {}
        foreach argelem [lrange $arglist 2 end] {
            foreach fnarg $argelem {
                lappend fnargs [my {*}$fnarg]
            }
        }
        return "\[xtal::rt::$fn {[lindex $arglist 1 0 1]} [join $fnargs { }]\]"
    }

    method FunctionDefinition {name params body} {
        set code [my _clause_to_code $body]
        set arguments {}
        foreach param $params {
            if {[llength $param] == 2} {
                lappend arguments "\[list [lindex $param 0] [my {*}[lindex $param 1]]\]"
            } else {
                lappend arguments [lindex $param 0]
            }
        }
        return "proc {$name} \[list [join $arguments { }]\] {\n$code[my Indent]}"
    }
}

namespace eval xtal::rt {
    variable _selector_contexts {}

    proc tclstring s {
        return [uplevel 1 [list subst $s]]
    }
    
    proc selector_context {} {
        variable _selector_contexts
        if {[llength $_selector_contexts]} {
            return [lindex $_selector_contexts end]
        }
        error "Not in a selector context"
    }

    proc is_selector_context {val} {
        variable _selector_contexts
        if {[llength $_selector_contexts]} {
            return [::tarray::_same_tclobj $val [lindex $_selector_contexts end]]
        } else {
            return 0
        }
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

    proc column_create {type inival} {
        return [switch -exact -- [lindex [tarray::types $inival] 0] {
            table   { error "Cannot convert a table to a column" }
            ""      { tarray::column::create $type $inival }
            default { tarray::column::cast $inival $type }
        }]
    }
    
    proc tarray_assign_element {varname value index} {
        # Assign a value to a single column or table element
        upvar 1 $varname var
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        return [switch -exact -- [lindex [tarray::types $var] 0] {
            table { tarray::table::vfill var $value $index }
            "" { lset var $index $value }
            default { tarray::column::vfill var $value $index }
        }]
    }

    proc tarray_assign_range {varname value low high} {
        # varname is the name of a column or table variable (must exist)
        # value is the value to be assigned
        # [low high] is the range to assign to
        
        upvar 1 $varname var

        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        
        lassign [tarray::types $var $value] vartype valuetype
        
        if {$high eq "end"} {
            set high [size $var]
            incr high -1
        }

        set target_size [expr {$high - $low + 1}]
        if {$target_size < 1} {
            error "Range lower limit $low is greater than upper limit $high."
        }
        

        if {$vartype eq ""} {
            # variable to be treated as a list
            switch -exact -- $valuetype {
                table {
                    set value_size [tarray::table::size $value]
                    set value [tarray::table::range $value 0 end]
                }
                "" { set value_size [llength $value] }
                default {
                    set value_size [tarray::column::size $value]
                    set value [tarray::column::range -list $value 0 end]
                }
            }
            if {$value_size != $target_size} {
                error "Source size $value_size differs from target range $low:$high."
            }
            set lsize [llength $var]
            if {$low > $lsize || $low < 0} {
                # Note $low == $lsize is ok. Will extend the vector
                error "Range lower limit $low is out of bounds."
            }
            # Need to workaround a Tcl lreplace bug/inconsistency
            # see http://core.tcl.tk/tcl/tktview?name=47ac84309b or
            # http://core.tcl.tk/tcl/tktview?name=578c2fd960
            if {$low == $lsize} {
                lappend var {*}$value
            } else {
                set var [lreplace $var[set var ""] $low $high {*}$value]
            }
            return $var
        }
        
        if {$vartype eq "table"} {

            # If the value is also a table, we assume each row in the value
            # is to be assigned successively to the target range. Otherwise
            # it is a value to be filled in the target range.
            if {$valuetype eq "table"} {
                set source_size [tarray::table::size $value]
            } else {
                if {$valuetype ne ""} {
                    error "Cannot assign a column to a table range."
                }
                # List of rows
                set source_size [llength $value]
            }

            # We have to use a put. Make sure the source range
            # and target range match
            if {$target_size != $source_size} {
                error "Source size $source_size differs from target table range $low:$high."
            }
            return [tarray::table::vput var $value $low]
        }

        # vartype is a column type
        if {$valuetype eq "table"} {
            # No possibility of conversion. But target might be
            # of type any in which case we have to fill the range
            error "Cannot assign a table to a column range."
        } 

        if {$valuetype eq ""} {
            set source_size [llength $value]
        } else {
            set source_size [tarray::column::size $value]
            if {$vartype ne $valuetype} {
                set value [tarray::column::cast $value $vartype]
            }
        }

        if {$target_size != $source_size} {
            error "Source size $source_size differs from target column range $low:$high."
        }
        return [tarray::column::vput var $value $low]
    }

    proc tarray_assign {varname value index} {
        upvar 1 $varname var
        # varname is the name of a column or table variable (must exist)
        # value is the value to be assigned
        # index is a general expression
        #

        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        
        lassign [tarray::types $var $value $index] vartype valuetype indextype

        if {$indextype eq ""} {
            set index_len [llength $index]
            # Special case - if a single index, treat as assignment of
            # a single value
            if {$index_len == 1} {
                set value [list $value]
            }
        } elseif {$indextype eq "int"} {
            set index_len [tarray::column::size $index]
        } else {
            error "Index must be a integer, an integer list, or an integer column."
        }
        if {$valuetype eq ""} {
            set value_len [llength $value]
        } elseif {$valuetype eq "table"} {
            set value_len [tarray::table::size $value]
        } else {
            set value_len [tarray::column::size $value]
        }

        # The vplace commands do not mind if source len is greater than
        # target length but current xtal semantics do not allow this so
        # explicitly check.
        if {$index_len != $value_len} {
            error "Number of indices ($index_len) not same as number of values ($value_len)."
        }
        if {$index_len == 0} {
            return $var
        }

        if {$vartype eq ""} {
            # Assume var is a list
            if {$indextype eq "int"} {
                set index [tarray::column::range -list $index 0 end]
            } else {
                if {$index_len == 1} {
                    lset var $index [lindex $value 0]
                    return $var
                }
            }

            if {$valuetype eq "table"} {
                set value [tarray::table::range -list $value 0 end]
            } elseif {$valuetype ne ""} {
                set value [tarray::column::range -list $value 0 end]
            }

            # Indices need to be in order else extending the list
            # length will not work right.
            set indirect_indices [lsort -indices -integer -increasing $index]

            # Also need to validate indices BEFORE making any modifications
            if {[lindex $index [lindex $indirect_indices 0]] < 0} {
                error "Invalid index ([lindex $index [lindex $indirect_indices 0]])."
            }
            set index_limit [llength $var]
            foreach indirect_index $indirect_indices {
                # Test in order of most likely
                set i [lindex $index $indirect_index]
                if {$i < $index_limit} continue
                if {$i == $index_limit} {
                    incr index_limit
                    continue
                }
                # There is a gap in the indices while extending the list
                error "Invalid index ($i)."
            }
            
            # Now we are ready to update the var
            foreach indirect_index $indirect_indices {
                lset var [lindex $index $indirect_index] [lindex $value $indirect_index]
            }
            return $var
        }

        # Variable is a table or column

        if {$indextype eq "" && $index_len == 1} {
            # Treat as a single value
            if {$vartype eq "table"} {
                return [tarray::table::vfill var [lindex $value 0] $index]
            } else {
                return [tarray::column::vfill var [lindex $value 0] $index]
            }
        }
        
        if {$valuetype eq $vartype} {
            # Both source and destination are same type.
            if {$vartype eq "table"} {
                return [tarray::table::vplace var $value $index]
            } else {
                return [tarray::column::vplace var $value $index]
            }
        }

        # Either value is not a tarray or is a tarray of a different type.
        if {$vartype eq "table"} {
            # In case of tables, there is no cast/conversion possible
            # so just do the assignment. value must be a table of list
            # of rows. No need to check because vplace will error out itself.
            return [tarray::table::vplace var $value $index]
        }

        # Variable is a column. Value is a column of a different type or
        # a list of values. In the former case we need to cast. In the
        # latter case vplace will convert the list itself.
        if {$valuetype ne ""} {
            set value [tarray::column::cast $value $vartype]
        }
        return [tarray::column::vplace var $value $index]
    }

    proc table_column_assign_range {varname colname value low high} {
        # varname is the name of a table variable (must exist)
        # colname is the name of the column to assign to
        # value is the value to be assigned
        # [low high] is the range to assign to
        
        upvar 1 $varname var

        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        lassign [tarray::types $var $value] vartype valuetype
        
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        # TBD - does xtal permit the keyword end ?
        if {$high eq "end"} {
            set high [tarray::table size $var]
            incr high -1
        }
        
        set target_size [expr {$high - $low + 1}]
        if {$target_size < 1} {
            error "Range lower limit $low is greater than upper limit $high."
        }

        if {$valuetype eq "table"} {
            set source_size [tarray::table::size $value]
        } elseif {$valuetype eq ""} {
            # Plain Tcl value
            set source_size [llength $value]
            # Convert the list of values to a list of rows
            set value [lmap val $value { list $val }]
        } else {
            # If the specified value is a column, convert it to
            # a table
            set value [tarray::table::create2 [list $colname] [list $value]]
            set source_size [tarray::table::size $value]
        }

        # We were passed a value that was a table or could be converted to one
        # We have to use a put. Make sure the source range
        # and target range match
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
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        lassign [tarray::types $var $value $index] vartype valuetype indextype
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        if {$indextype eq ""} {
            set index_len [llength $index]
            # Special case - if a single index, treat as assignment of
            # a single value
            if {$index_len == 1} {
                set value [list $value]
            }
        } elseif {$indextype eq "int"} {
            set index_len [tarray::column::size $index]
        } else {
            error "Index must be a integer, an integer list, or an integer column."
        }
        if {$valuetype eq ""} {
            set value_len [llength $value]
        } elseif {$valuetype eq "table"} {
            error "Cannot assign table to a column."
        } else {
            set value_len [tarray::column::size $value]
        }

        # The vplace commands do not mind if source len is greater than
        # target length but current xtal semantics do not allow this so
        # explicitly check.
        if {$index_len != $value_len} {
            error "Number of indices ($index_len) not same as number of values ($value_len)."
        }
        if {$index_len == 0} {
            return $var
        }

        # If the specified value is a column, convert it to a table
        # else it will be a list of rows
        if {$valuetype eq ""} {
            # Need to make each elem in list a row
            # TBD - likely to be slow. Need optimization
            set value [lmap val $value {
                list $val
            }]
        } elseif {$valuetype ne "table"} {
            # It's a column
            set value [tarray::table::create2 [list $colname] [list $value]]
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
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        lassign [tarray::types $var $value] vartype valuetype
        if {$vartype ne "table" || $valuetype ne "table"} {
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
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }

        lassign [tarray::types $var $value] vartype valuetype
        
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        if {$high eq "end"} {
            set high [tarray::table size $var]
            incr high -1
        }
        
        # TBD - does xtal permit the keyword end ?
        set target_size [expr {$high - $low + 1}]
        if {$target_size < 1} {
            error "Range lower limit $low is greater than upper limit $high."
        }

        if {$valuetype eq ""} {
            set value_len [llength $value]
        } elseif {$valuetype eq "table"} {
            set value_len [tarray::table::size $value]
        } else {
            error "Cannot assign a column to a table."
        }

        # We were passed a value that was a table or could be converted to one
        # We have to use a put. Make sure the source range
        # and target range match
        if {$target_size != $value_len} {
            error "Source size $value_len differs from target table range $low:$high."
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
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        lassign [tarray::types $var $value $index] vartype valuetype indextype
        if {$vartype ne "table"} {
            error "$varname is not a table."
        }

        if {$indextype eq ""} {
            set index_len [llength $index]
            # Special case - if a single index, treat as assignment of
            # a single value
            if {$index_len == 1} {
                set value [list $value]
            }
        } elseif {$indextype eq "int"} {
            set index_len [tarray::column::size $index]
        } else {
            error "Index must be a integer, an integer list, or an integer column."
        }
        if {$valuetype eq ""} {
            set value_len [llength $value]
        } elseif {$valuetype eq "table"} {
            set value_len [tarray::table::size $value]
        } else {
            error "Cannot assign a column to a table"
        }

        # The vplace commands do not mind if source len is greater than
        # target length but current xtal semantics do not allow this so
        # explicitly check.
        if {$index_len != $value_len} {
            error "Number of indices ($index_len) not same as number of values ($value_len)."
        }
        if {$index_len == 0} {
            return $var
        }


        # In this case, the table dimensions and type must match and
        # indexlist must be a int tarray or an int list else
        # vplace will throw an error.
        return [tarray::table::vplace -columns $colnames var $value $index]
    }

    proc assign_element {varname elem value} {
        # For a table sets entire column $elem to $value
        # For a column assigns $value to index $elem
        # Else treats as dictionary and assigns $value to key
        upvar 1 $varname var
        if {[info exists var]} {
            switch -exact -- [lindex [tarray::types $var] 0] {
                ""      { return [dict set var $elem $value] }
                "table" { return [tarray::table::vcolumn var $elem $value] }
                default {
                    return [tarray::column::vfill var $value [tarray::column::search $var $elem]]
                }
            }
        } else {
            return [dict set var $elem $value]
        }
    }

    proc element {operand element} {
        switch -exact -- [lindex [tarray::types $operand] 0] {
            ""      { return [dict get $operand $element] }
            "table" { return [tarray::table::column $operand $element] }
            default {
                set index [tarray::column::search $operand $element]
                if {$index < 0} {
                    error "Value \"$element\" not found in column"
                }
                return $index
            }
        }
    }
    
    proc range {operand low high} {
        switch -exact -- [lindex [tarray::types $operand] 0] {
            ""      { return [lrange $operand $low $high] }
            "table" { return [tarray::table::range $operand $low $high] }
            default { return [tarray::column::range $operand $low $high] }
        }
    }

    proc _relop_check {a b} {
        lassign [tarray::types $a $b] atype btype
        if {$atype eq "table" || $btype eq "table"} {
            error "Tables cannot be operands for a relational operator."
        }
        return [list $atype $btype]
    }

    proc relop {op a b} {
        lassign [_relop_check $a $b] atype btype
        if {$atype eq ""} {
            if {[is_selector_context $a]} {
                # $a is the selector context and so treat as a list
                if {$btype ne "" || [is_selector_context $b]} {
                    error "Operator $op not supported between columns or columns and lists."
                }
                # Return an int column containing matching indices
                return [matching_list_indices $op $a $b]
            }
            # $a is a scalar
            if {$btype eq ""} {
                if {[is_selector_context $b]} {
                    # a is scalar, b is not
                    return [matching_list_indices $op $b $a]
                } 
                # Both scalars
                return [tcl::mathop::$op $a $b]
            } else {
                return [tarray::column::search -all {*}[xtal::_map_search_op_reverse $op] $b $a]
            }
        } else {
            # $a is a column
            if {$btype ne "" || [is_selector_context $b]} {
                error "Operator $op not supported between columns or columns and lists"
            }
            # $b is a scalar
            return [tarray::column::search -all {*}[xtal::_map_search_op $op] $a $b]
        }
    }

    # TBD - is matching_column_indices actually used anywhere?
    proc matching_column_indices {op haystack needle} {
        return [switch -exact -- $op {
            ==  { tarray::column::search -all -eq $haystack $needle }
            !=  { tarray::column::search -all -not -eq $haystack $needle }
            <   { tarray::column::search -all -lt $haystack $needle }
            <=  { tarray::column::search -all -not -gt $haystack $needle }
            >   { tarray::column::search -all -gt $haystack $needle }
            >=  { tarray::column::search -all -not -lt $haystack $needle }
            =^  { tarray::column::search -all -nocase -eq $haystack $needle }
            !^  { tarray::column::search -all -nocase -not -eq $haystack $needle }
            ~  { tarray::column::search -all -re $haystack $needle }
            !~  { tarray::column::search -all -not -re $haystack $needle }
            ~^ { tarray::column::search -all -nocase -re $haystack $needle }
            !~^ { tarray::column::search -all -nocase -not -re $haystack $needle }
        }]
    }

    proc matching_list_indices {op haystack needle} {
        # haystack must be a list, not column
        set indices [switch -exact -- $op {
            ==  {lsearch -all -exact $haystack $needle}
            !=  {lsearch -all -not -exact $haystack $needle}
            =^  {lsearch -all -nocase -exact $haystack $needle}
            !^  {lsearch -all -nocase -not -exact $haystack $needle}
            ~   {lsearch -all -regexp $haystack $needle}
            !~  {lsearch -all -not -regexp $haystack $needle}
            ~^  {lsearch -all -nocase -regexp $haystack $needle}
            !~^ {lsearch -all -nocase -not -regexp $haystack $needle}
            default {
                error "Operator $op not supported on lists."
            }
        }]
        return [::tarray::column create int $indices]
    }

    proc strop {op a b} {
        lassign [_relop_check $a $b] atype btype
        if {$atype eq ""} {
            if {[is_selector_context $a]} {
                # $a is the selector context and so treat as a list
                if {$btype ne "" || [is_selector_context $b]} {
                    error "Operator $op not supported between columns or columns and lists"
                }
                # Return an int column containing matching indices
                return [matching_list_indices $op $a $b]
            }
            # $a is not a column and not a selector context (i.e. scalar)
            if {$btype eq ""} {
                if {[is_selector_context $b]} {
                    # a is scalar, b is not. Only permit equality/inequality
                    if {$op in {=^ !^}} {
                        return [matching_list_indices $op $b $a]
                    }
                    error "The right hand operand of operator $op cannot be a vector."
                } 
                # a and b are both scalars
                return [switch -exact -- $op {
                    =^ {string equal -nocase $a $b}
                    !^ {expr {![string equal -nocase $a $b]}}
                    ~ {regexp -- $b $a}
                    !~ {expr {![regexp -- $b $a]}}
                    ~^ {regexp -nocase -- $b $a}
                    !~^ {expr {![regexp -nocase -- $b $a]}}
                }]
            } else {
                # a is scalar, b is column. Only permit equality/inequality
                return [switch -exact -- $op {
                    =^ { tarray::column::search -all -nocase -eq $b $a }
                    !^ { tarray::column::search -all -nocase -not -eq $b $a }
                    default {error "The right hand operand of operator $op cannot be a vector."}
                }]
            }
        } else {
            # a is a column
            if {$btype ne "" || [is_selector_context $b]} {
                error "Operator $op not supported between columns or columns and lists"
            }
            # a column, b scalar
            return [switch -exact -- $op {
                =^  { tarray::column::search -all -nocase -eq $b $a }
                !^  { tarray::column::search -all -nocase -not -eq $b $a }
                ~  { tarray::column::search -all -re $a $b }
                !~  { tarray::column::search -all -not -re $a $b }
                ~^ { tarray::column::search -all -nocase -re $a $b }
                !~^ { tarray::column::search -all -nocase -not -re $a $b }
            }]
        }
    }

    proc unary {op a} {
        set type [lindex [tarray::types $a] 0]
        switch -exact -- $type {
            "" {
                if {$op eq "%"} {
                    return [llength $a]
                } else {
                    return [expr "$op\$a"]
                }
            }
            "table" {
                if {$op eq "%"} {
                    return [tarray::table::size $a]
                }
                error "Unary op $op not implemented for tables"
            }
            default {
                # TBD - replace these with column::unary calls when implemented
                switch -exact -- $op {
                    "%" { return [tarray::column::size $a] }
                    "-" { return [tarray::column::math - 0 $a] }
                    "+" {
                        if {$type in {byte int uint wide double}} {
                            return $a
                        }
                    }
                    "~" {
                        if {$type in {boolean byte int uint wide}} {
                            return [tarray::column::math ^ $a [dict get {boolean 1 byte 0xff int -1 uint 0xffffffff wide 0xffffffffffffffff} $type]]
                        }
                    } 
                }
                error "Unary op $op not implemented for columns of type $type"
            }
        }

        
        if {$type eq ""} {
            return [expr "$op\$a"]
        } else {
            # TBD
            error "Unary op $op not implemented for columns and tables"
            return [tarray::column::unary $op $a]
        }
    }

    proc and {args} {
        set result [_and2 [lindex $args 0] [lindex $args 1]]
        foreach arg [lrange $args 2 end] {
            set result [_and2 $result $arg]
        }
        return $result
    }

    proc _and2 {a b} {
        lassign [tarray::types $a $b] atype btype

        if {$atype eq "" && $btype eq ""} {
            # Neither is a tarray
            return [expr {$a && $b}]
        }
        
        if {$atype eq "table" || $btype eq "table"} {
            error "Tables cannot be used as operands of a logical operator."
        }

        if {$atype ne "" && $btype ne ""} {
            # Both are columns. Treat as intersection operator.
            # TBD - optimize if a or b are empty or does intersect3 already do that
            # TBD - are the elements in increasing order after intersect?
            return [lindex [tarray::column::intersect3 $a $b] 0]
        }

        # TBD - for consistency with the || case (see the _or2 routine)
        # we disallow this until we figure out what to do there.
        error "Invalid logical and operation on a column and a scalar."
        
        Rest of this code to be enabled in the future.
        # At most one of a and b are scalars
        if {$atype eq ""} {
            # b is a column. Only permit if a is a boolean
            if {[string is boolean $b]} {
                if {$b} {
                    return $a
                } else {
                    return [::tarray::column create $atype]
                }
            }
        } else {
            if {[string is boolean $a]} {
                if {$a} {
                    return $b
                } else {
                    return [::tarray::column create $btype]
                }
            }
        }

        error "Columns can only be logically and'ed with boolean scalars"
    }

    proc or {args} {
        set result [_or2 [lindex $args 0] [lindex $args 1]]
        foreach arg [lrange $args 2 end] {
            set result [_or2 $result $arg]
        }
        return $result
    }

    proc _or2 {a b} {
        lassign [tarray::types $a $b] atype btype

        if {$atype eq "" && $btype eq ""} {
            # Neither is a tarray
            return [expr {$a || $b}]
        }
        
        if {$atype eq "table" || $btype eq "table"} {
            error "Tables cannot be used as operands of a logical operator."
        }

        if {$atype ne "" && $btype ne ""} {
            # Both are tarrays. Treat as intersection operator
            # Can't use tarray::intersect3+tarray::sort because of 
            # uniqueness requirement
            # TBD - are the elements in increasing order after union?
            # TBD - implement union in C
            return [tarray::column::create int \
                        [lsort -integer -unique \
                             [concat [tarray::column::range -list $a 0 end] \
                                  [tarray::column::range -list $b 0 end]]]]
        }
        
        # TBD  - we would like C[I || 1] to mean entire column C
        # At this point we only have a = I and b = 1 and don't know
        # how to convey "all" elements of C. So for now disallow.
        error "Invalid logical or operation on a column and a scalar."
    }

    proc selector {a selexpr} {
        lassign [tarray::types $a $selexpr] atype seltype

        if {$seltype eq "" && [string is integer -strict $selexpr]} {
            # Not a column, treat as an index
            return [switch -exact -- $atype {
                ""      {
                    set n [llength $a]
                    if {$selexpr < 0 || $selexpr >= $n} {
                            error "list index out of range"
                        }
                    lindex $a $selexpr
                }
                table   { tarray::table::index $a $selexpr }
                default { tarray::column::index $a $selexpr }
            }]
        }

        # Either operand is not a list or index is not a simple integer
        return [switch -exact -- $atype {
            "" {
                # Operand is a list
                set n [llength $a]
                if {$seltype ne ""} {
                    # If indices are not a list, convert them to one
                    # TBD - would it be faster to iterate retrieving one
                    # element from the index column at a time? Probably
                    # replace with tarray::iterate once implemented
                    set selexpr [tarray::column::range -list $selexpr 0 end]
                }
                lmap pos $selexpr {
                    if {$pos < 0 || $pos >= $n} {
                        error "list index out of range"
                    }
                    lindex $a $pos
                }
            }
            table   { tarray::table::get $a $selexpr }
            default { tarray::column::get $a $selexpr }
        }]
    }

    proc selector_range {range} {
        # TBD - replace with C version
        lassign $range low high
        if {$low < 0} {
            set low 0
        }
        set max [expr {[size [selector_context]] - 1}]
        if {$high eq "end"} {
            set high $max
        } else {
            if {$high > $max} {
                set high $max
            }
        }
        set l {}
        for {set i $low} {$i <= $high} {incr i} {
            lappend l $i
        }
        return [tarray::column::create int $l]
    }

    proc size {val} {
        return [switch -exact -- [lindex [tarray::types $val] 0] {
            "" { llength $val }
            table { tarray::table::size $val }
            default { tarray::column::size $val }
        }]
    }

    proc dereference {varname} {
        upvar 1 $varname var
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        # Avoid shimmering of columns and tables
        if {[lindex [tarray::types $var] 0] ne ""} {
            error "Dereferencing of columns and tables not permitted."
        }
        return $var
    }

    proc dereference2 {varname} {
        upvar 1 $varname var
        if {![info exists var]} {
            error "can't read \"$varname\": no such variable"
        }
        # Avoid shimmering of columns and tables
        if {[lindex [tarray::types $var] 0] ne ""} {
            error "Dereferencing of columns and tables not permitted."
        }
        upvar 1 $var var2
        return $var2
    }

    proc condition {expr} {
        switch -exact -- [lindex [tarray::types $expr] 0] {
            ""    { return $expr }
            table { return [expr {[tarray::table::size $expr] > 0}] }
            default { return [expr {[tarray::column::size $expr] > 0}] }
        }
    }

    proc forloop {loopvar expr body} {
        # TBD - for columns and tables perhaps looping using
        # an index would be faster and/or cheaper in memory?
        switch -exact -- [lindex [tarray::types $expr] 0] {
            ""    {set l $expr}
            table {set l [tarray::table::range -list $expr 0 end]}
            default {set l [tarray::column::range -list $expr 0 end]}
        }
        uplevel 1 [list foreach $loopvar $l $body]
    }
    
    proc keyedforloop {keyvar loopvar expr body} {
        # TBD - for columns and tables perhaps looping using
        # an index would be faster and/or cheaper in memory?
        switch -exact -- [lindex [tarray::types $expr] 0] {
            ""      {set l $expr}
            table   {set l [tarray::table::range -dict $expr 0 end]}
            default {set l [tarray::column::range -dict $expr 0 end]}
        }
        uplevel 1 [list dict for [list $keyvar $loopvar] $l $body]
    }

    proc listcast {val} {
        return [switch -exact -- [lindex [tarray::types $val] 0] {
            table { tarray::table::range -list $val 0 end }
            ""    { error "Operand of @list must be a column or table" } 
            default { tarray::column::range -list $val 0 end }
        }]
    }

    proc dictcast {val} {
        return [switch -exact -- [lindex [tarray::types $val] 0] {
            table { tarray::table::range -dict $val 0 end }
            ""    { error "Operand of @dict must be a column or table" } 
            default { tarray::column::range -dict $val 0 end }
        }]
    }

    proc throwerror {errorcode} {
        # Assumes last element of errorcode is the message
        return -code error -level 2 -errorcode $errorcode [lindex $errorcode end]
    }
    
    proc sort {operand options} {
        return [switch -exact -- [lindex [tarray::types $operand] 0] {
            table {
                # TBD - check in C source that $target is not a column or
                # table to prevent shimmering
                tarray::table::sort {*}$options $operand 0
            }
            ""      { error "Operand of @sort must be a column or table" } 
            default { tarray::column::sort {*}$options $operand}
        }]
    }

    proc sort_indirect {operand target options} {
        return [switch -exact -- [lindex [tarray::types $operand] 0] {
            table {
                # TBD - check in C source that $target is not a column or
                # table to prevent shimmering
                tarray::table::sort {*}$options $operand $target
            }
            ""      { error "Operand of @sort must be a column or table" } 
            default { tarray::column::sort -indirect $target {*}$options $operand}
        }]
    }

    proc delete {operand args} {
        return [switch -exact -- [lindex [tarray::types $operand] 0] {
            table { tarray::table::delete $operand {*}$args }
            "" { error "Operand is not a column or able" }
            default { tarray::column::delete $operand {*}$args }
        }]
    }

    proc vdelete {ident args} {
        upvar 1 $ident var
        if {![info exists var]} {
            error "can't read \"$ident\": no such variable"
        }
        return [switch -exact -- [lindex [tarray::types $var] 0] {
            table { tarray::table::vdelete var {*}$args }
            "" { error "Operand is not a column or able" }
            default { tarray::column::vdelete var {*}$args }
        }]
    }

    proc inject {operand args} {
        return [switch -exact -- [lindex [tarray::types $operand] 0] {
            table { tarray::table::inject $operand {*}$args }
            "" { error "Operand is not a column or able" }
            default { tarray::column::inject $operand {*}$args }
        }]
    }

    proc vinject {ident args} {
        upvar 1 $ident var
        if {![info exists var]} {
            error "can't read \"$ident\": no such variable"
        }
        return [switch -exact -- [lindex [tarray::types $var] 0] {
            table { tarray::table::vinject var {*}$args }
            "" { error "Operand is not a column or able" }
            default { tarray::column::vinject var {*}$args }
        }]
    }
    
    proc insert {operand args} {
        return [switch -exact -- [lindex [tarray::types $operand] 0] {
            table { tarray::table::insert $operand {*}$args }
            "" { error "Operand is not a column or able" }
            default { tarray::column::insert $operand {*}$args }
        }]
    }

    proc vinsert {ident args} {
        upvar 1 $ident var
        if {![info exists var]} {
            error "can't read \"$ident\": no such variable"
        }
        return [switch -exact -- [lindex [tarray::types $var] 0] {
            table { tarray::table::vinsert var {*}$args }
            "" { error "Operand is not a column or able" }
            default { tarray::column::vinsert var {*}$args }
        }]
    }
    
    proc reverse {operand} {
        return [switch -exact -- [lindex [tarray::types $operand] 0] {
            table { tarray::table::reverse $operand }
            ""    { lreverse $operand }
            default { tarray::column::reverse $operand }
        }]
    }

    proc vreverse {ident} {
        upvar 1 $ident var
        if {![info exists var]} {
            error "can't read \"$ident\": no such variable"
        }
        return [switch -exact -- [lindex [tarray::types $var] 0] {
            table { tarray::table::vreverse var }
            "" { error "Operand is not a column or able" }
            default { tarray::column::vreverse var }
        }]
    }

    proc search_@@ {search_op haystack needle} {
        return [switch -exact -- [lindex [tarray::types $haystack] 0] {
            table { error "Tables cannot be used as search operands" }
            ""    {
                # Assume lists
                switch -exact $search_op {
                    -eq { lsearch -exact -all -inline $haystack $needle }
                    "-not -eq" { lsearch -not -exact -all -inline $haystack $needle }
                    "-nocase -eq" { lsearch -nocase -exact -all -inline $haystack $needle }
                    "-nocase -not -eq" { lsearch -nocase -not -exact -all -inline $haystack $needle }
                    
                    -re { lsearch -regexp -all -inline $haystack $needle }
                    "-not -re" { lsearch -not -regexp -all -inline $haystack $needle }
                    "-nocase -re" { lsearch -nocase -regexp -all -inline $haystack $needle }
                    "-nocase -not -re" { lsearch -nocase -not -regexp -all -inline $haystack $needle }

                    -pat { lsearch -glob -all -inline $haystack $needle }
                    "-not -pat" { lsearch -not -glob -all -inline $haystack $needle }
                    "-nocase -pat" { lsearch -nocase -glob -all -inline $haystack $needle }
                    "-nocase -not -pat" { lsearch -nocase -not -glob -all -inline $haystack $needle }
                    default {
                        error "The specified selector operator is invalid for lists."
                    }
                }
            }
            default { tarray::column::search -inline -all {*}$search_op $haystack $needle }
        }]
    }
}

