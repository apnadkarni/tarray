# (c) 2018 Ashok P. Nadkarni
# All rights reserved.
# See the file license.terms for license

lappend auto_path d:/tcl/lib [pwd]
package require snit
::snit::widgetadaptor tarray::ui::graph {
    # Specifies the 
    option -legend -configuremethod _set_legend_option

    # Specifies the name of the table column to use as the X-axis
    option -xcolumn -configuremethod _set_xcolumn_option

    delegate option * to hull

    # Holds the data table being displayed
    variable _table
    
    # Index column holding order of elements to be plotted.
    # The graph data is expected to be in ascending x-value order.
    # This column holds that order.
    variable _indices

    # Array mapping columns to vectors names. Indexed by column name
    variable _vectors

    # Indicates when the widget has been constructed
    variable _constructed 0

    constructor {tab args} {
        set _table $tab
        array set _vectors {}

        installhull using rbc::graph
        $self configurelist $args

        # If the X-axis column is not specified, default it
        if {$options(-xcolumn) eq ""} {
            $self configure -xcolumn [lindex [tarray::table cnames $_table] 0]
        }
        
        # Widget is constructed
        set _constructed 1
    }

    destructor {
        foreach {- v} [array get _vectors] {
            rbc::vector destroy $v
        }
    }

    method element {oper cname args} {
        # If it is creation of one of our columns, handle it specially.
        if {$cname in [tarray::table cnames $_table]} {
            # Must not specify -xdata or -ydata as those should be based
            # on table columns
            foreach {opt -} $args {
                if {$opt in {-xdata -ydata}} {
                    error "Options -xdata and -ydata must not be specified for plotting table columns."
                }
            }
            if {$oper eq "create"} {
                $self _vectorify $cname
                lappend args -xdata $_vectors($options(-xcolumn)) -ydata $_vectors($cname) 
            }
        }
        $hull element $oper $cname {*}$args
    }

    method _set_xcolumn_option {optname cname} {
        if {$cname ni [tarray::table cnames $_table]} {
            error "Table does not contain column named $cname"
        }
        set tcol [tarray::table column $_table $cname]
        set ctype [tarray::column type $tcol]
        if {$ctype in {any string}} {
            # Create a index vector that maps to the non-numeric values
            set _vectors($cname) [rbc::vector create #auto]
            set n [tarray::column size $tcol]
            set _indices [tarray::column series $n]
            $_vectors($cname) seq 0 [expr {$n-1}]
            # Add the callback to map index to value
            $win axis configure x -subdivisions 1 -command [list $self _map_xvalue]
        } else {
            set _indices [tarray::column sort -indices $tcol]
            $self _vectorify $cname
            # Remove any callback that mapped indexes to string values
            $win axis configure x -subdivisions 2 -command {}
        }
        set options(-xcolumn) $cname
        return
    }

    method _map_xvalue {w val} {
        set ival [tcl::mathfunc::int $val]
        if {$ival != $val} {
            # Not an major tick, should not be labeled
            return ""
        }
        return [tarray::column index [tarray::table::column $_table $options(-xcolumn)] $ival]
    }

    method _vectorify {cname} {
        if {[info exists _vectors($cname)]} return
        set _vectors($cname) [::rbc::vector create #auto]
        if {[catch {
            tarray::rbc::tovector $_vectors($cname) [tarray::table::column $_table $cname] $_indices
        } result ropts]} {
            rbc::vector destroy $_vectors($cname)
            unset _vectors($cname)
            return -options $ropts $result
        }
    }

    delegate method * to hull
}

package require rbc
package require tarray
set tab [tarray::table create {
    name string code double population double
} {
    {China 100 1350000000}
    {Vatican 200 850}
    {India 300 1200000000}
}]
destroy .win
