namespace path [list tarray xtal]
proc xtal::testconstexpr {expr desc} {
    set t [xtal $expr]
    set e [expr $expr]
    if {$t != $e} {
        puts stderr "$desc failed for <$expr>. $t != $e"
    }
}
catch {tp destroy}
catch {tc destroy}
if {1} {
    xtal::Parser create tp $::xtal::_use_oo_parser
    xtal::Compiler create tc $::xtal::_use_oo_parser
    set I [column create int {10 20 30 40 50}]
    set J [column create int {100 200 300 400 500}]
    set T [table create {i int s string} {{10 ten} {200 twenty} {3000 thirty}}]
}
if {1} {
    catch {table slice $T $T};  # Caused crash due to shimmering, now should return error
    proc getI {} {return $::I}
    xtal::xtal {I[@@ < 30]}
    xtal::xtal {I.20}
    xtal::xtal {I. "10"}
    xtal::xtal {I[I < 30]}
    xtal::xtal {getI()[@@ > 30]}
    set x i
    xtal::xtal {T[T.i < 35]}
    xtal::xtal {T[T.$x < 45]}
    xtal::xtal {T.(i,s)[@@.i > 40]}
    xtal::xtal {K = J}
    xtal::xtal {K[0:] = I[0:]}
    xtal::xtal {K[0:1] = J[0:1]}
    xtal::xtal {K[2:4] = {99,100,101}}
    xtal::xtal {K[1:] = lrepeat(%K-1, 999)}
    xtal::xtal {K[{3,4}] = I[{4,3}]}
    xtal::xtal {T.i[0:1] = I[3:4]}
    xtal::xtal {T.(s,i)}
    xtal::xtal {T.s. thirty}
    xtal::xtal {T.s. "thirty"}
    xtal::xtal {T.i[@@ < 400] = 10*T.i[@@ < 400]}
    set col s
    xtal::xtal {T.$col[0:1] = {'abc', "cba"} }
    xtal::xtal {T.$col[1:] = lrepeat(%T-1, 'def')}
    xtal::xtal {% I}
    xtal::xtal {% {1,2,3}}

    namespace eval xtal {
        testconstexpr {4-2+2} "+- Left associativity"
        testconstexpr {4-2-2} "- Left associativity"
        testconstexpr {1+2*3} "+* Operator precedence"
        testconstexpr {1||0&&0} "Logical operator precedence"
    }
    catch {C destroy}
    oo::class create C { method m {args} {puts [join $args ,]} }
    set o [C new]
    xtal::xtal {a = 'b' ; b = 99; $a}
    xtal::xtal {$o.m('abc', 10)}
    xtal::xtal {$o.m(
                  'abci'
                  ,
                  10
                  )}
    set d {a 1 b 2 c 3}
    xtal::xtal { d.b }
    set x c
    xtal::xtal {d.$x}
    set a 0 ; set b 1
    xtal::xtal { < expr {$a > $b} > }
    xtal::xtal {<expr {$a > $b}>}
    xtal::xtal { <
        expr {$a > $b}
        > }
    xtal::xtal {
        a = 1 ; b = 2
        <
        puts [expr {$a > $b}]
        >
    }
    xtal::xtal {
        <lappend l 99>
        a = b
    }
    
    xtal::xtal { a = <clock seconds> }
    xtal::xtal { a = <clock seconds>; }
    xtal::xtal { a = <clock seconds> ;}
    xtal::xtal { a = <clock seconds> ; c = a}
    xtal::xtal { a = <
        clock seconds> ; b = a
    }

    xtal::xtal { @table () }
    xtal::xtal { @table () {} }
    xtal::xtal { @table (i int) }
    xtal::xtal { @table (i int) {{2}}}
    xtal::xtal { @table (i int, s string) }
    xtal::xtal { @table (i int, s string ) {{2, 'two'}, {3, 'three'}} }
    xtal::xtal { @table (i
                      int,
                      s string
                      ) {
                          {2, 'two'},
                          {3, 'three'}
                      } }

    set "variable with spaces" "value of variable with spaces"
    xtal::xtal {
        puts($"variable with spaces")
        var = "variable with spaces"
        puts($var)
    }

    xtal::xtal {
        function fn () {puts( "Function fn")}
        function fn2 (a, b=5+6) {return a+b}
        fn()
        fn2( 1, 2)
        fn2 (10 )
    }
    fn
    fn2 1 2
    fn2 10
    xtal::xtal {
        try {
            a = 1
        } on error {puts('error')} finally {puts('finally')}
    }
    
    xtal::xtal {
        try {
            throw 'TSCRIPT', 'TEST', "Just a test"
        } on error res opts {
            puts(res)
            puts (opts)
        } finally {
            puts('finally')
        }
    }

    xtal::xtal {
        try {
            nosuchvar
        } trap {'TCL', 'XXX'} {
            puts("Should not trigger")
        } trap {'TCL', 'LOOKUP'} res opts {
            <puts "Result: $res">
            <puts "Opts: $opts">
        } finally {
            puts('finally')
        }
    }

    xtal::xtal {
        Rainfall = @double {
            11.0, 23.3, 18.4, 14.7, 70.3, 180.5, 210.2, 205.8, 126.4, 64.9, 33.1, 19.2
        }
        Emps = @table (
                       Name string,   Salary uint,   Age uint,   Location string
                       ) {
                           {'Sally',      70000,      32,       'Boston'},
                           {'Tom',        65000,      36,       'Boston'},
                           {'Dick',       80000,      40,       "New York"},
                           {'Harry',      45000,      37,       "New York"},
                           {'Amanda',     48000,      35,       'Seattle'}
                       }
    }
}
