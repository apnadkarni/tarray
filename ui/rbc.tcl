# (c) 2018 Ashok P. Nadkarni
# All rights reserved.
# See the file license.terms for license

lappend auto_path d:/tcl/lib [pwd]
package require snit
::snit::widgetadaptor tarray::ui::chart {

    ################################################################
    # Variables
    
    # Keeps track of the various plots in the chart. Dictionary indexed by
    # the RBC plot name and each element being a nested inner dictionary
    # with the following keys
    #  Type - either "bar" or "line"
    #  Column - name of the corresponding column. Only exists if the
    #      the plot data came from a table column.
    variable _plots
    
    # When plotting graphs, we sort the data values based on the
    # X value. The _sorted_indices array, indexed by column name,
    # keeps track of these sorted tarray index columns.
    variable _sorted_indices

    # _vectors is an nested dictionary indexed by table column names.  Each
    # element is a dictionary with zero or more keys each of which is
    # either the empty string or the name of another table column.
    # In the case of the empty string, the corresponding value is the
    # name of the RBC vector corresponding to the array index column
    # with values in the same order. If a non-empty string, say X,
    # X must be the name of a table column. The RBC vector elements
    # are then ordered in the same order as the *ascending sorted*
    # value of X. For example, if C is the array column index,
    # [dict get $_vectors(C) ""] is the RBC vector with elements in
    # same order as C. [dict get $_vectors(C) X] is the RBC vector
    # with C's elements but sorted corresponding to X's ascending order.
    variable _vectors
    
    # _table holds the data table being displayed
    variable _table
    
    # Indicates when the widget has been constructed
    variable _constructed 0

    ################################################################
    # Options
    
    # The column that provides the X-axis values can be specified
    # as an option to the plot method. If unspecifed there, the
    # default is the column specified by the -xcolumn option.
    option -xcolumn

    delegate option * to hull

    ################################################################
    # Methods
    constructor {tab args} {
        set _table $tab
        set _vectors {}

        installhull using rbc::graph
        $self configurelist $args

        # Widget is constructed
        set _constructed 1
    }

    destructor {
        foreach v [array names _vectors] {
            rbc::vector destroy $v
        }
    }

    method _create {plot_type name args} {
        if {$plot_type ni {bar line element}} {
            throw {TARRAY RBC INVARGS} "Unknown plot type \"$plot_type\"."
        }

        if {$name in [$win element names]} {
            throw {TARRAY RBC EXISTS} "Plot \"$name\" already exists in chart \"$win\."
        }
        if {[dict exists $args -ycolumn]} {
            set cname [dict get $args -ycolumn]
            if {$cname ni [tarray::table cnames $_table]} {
                throw {TARRAY RBC INVCOLUMN} "Unknown column '$cname'"
            }
            dict unset args -ycolumn
        } elseif {$name in [tarray::table cnames $_table]} {
            set cname $name
        }

        dict set _plots $name Type $plot_type

        # cname set -> plotting a table column of that name
        # else -> raw data passed directly to RBC widget.
        if {[info exists cname]} {
            # We are plotting table column data
            dict set _plots $name Column $cname
            if {[dict exists $args -xdata]} {
                if {[dict exists $args -xcolumn]} {
                    throw {TARRAY RBC INVARGS} "Options -xdata and -xcolumn must not be specified together."
                }
                # If caller has specified -xdata, then the table column
                # is assumed to already be in the desired order corresponding
                # to the -xdata values.
                lappend args -ydata [$self _vector $cname]
            } else {
                if {[dict exists $args -xcolumn]} {
                    set xcolname [dict get $args -xcolumn]
                } else {
                    # If neither -xdata or -xcolumn specified use default
                    set xcolname ""
                }
                # Create an sorted x-vector and y vector in the order
                # of the x-vector values
                lassign [$self _xyvector $xcolname $cname] xvec yvec
                dict unset args -xdata
                dict unset args -ydata
                dict unset args -xcolumn
                lappend args -xdata $xvec -ydata $yvec
            }
        }

        $hull $plot_type create $name {*}$args
    }
    method {element create} {args} {
        $self _create element {*}$args
    }
    method {line create} {args} {
        $self _create line {*}$args
    }
    method {bar create} {args} {
        $self _create bar {*}$args
    }
    delegate method {element *} to hull using "%c element %m"
    delegate method {line *} to hull using "%c line %m"
    delegate method {bar *} to hull using "%c bar %m"

    method _set_xcolumn_option {optname cname} {
        if {$cname ni [tarray::table cnames $_table]} {
            error "Table does not contain column named $cname"
        }
        set options(-xcolumn) $cname
    }

    method _setup_sorted_column {cname} {
        set tcol [tarray::table column $_table $cname]
        set ctype [tarray::column type $tcol]
        if {$ctype in {any string}} {
            set n [tarray::column size $tcol]
            set _sorted_indices($cname) [tarray::column series $n]
        } else {
            set _sorted_indices($cname) [tarray::column sort -indices $tcol]
        }
        return
    }

    method TBD-xaxis {} {
        if {$ctype in {any string}} {
            # Create a index vector that maps to the non-numeric values
            set _vectors($cname) [rbc::vector create #auto]
            set n [tarray::column size $tcol]
            set _indices [tarray::column series $n]
            $_vectors($cname) seq 0 [expr {$n-1}]
            # Add the callback to map index to value
            $win axis configure x -subdivisions 1 -command [mymethod _map_xvalue]
        } else {
            # Remove any callback that mapped indexes to string values
            $win axis configure x -subdivisions 2 -command {}
        }
    }

    # Return an RBC vector whose elements are in the same order as
    # the specfied table column.
    method _vector {cname} {
        # If we have already created this vector, return it.
        if {[dict exists $_vectors $cname ""]} {
            return [dict get $_vectors $cname ""]
        }
        # Create a new RBC vector
        set vec [::rbc::vector create #auto]
        # Copy the column elements to it
        if {[catch {
            tarray::rbc::tovector $vec [tarray::table::column $_table $cname]
        } result ropts]} {
            rbc::vector destroy $vec
            return -options $ropts $result
        }
        # Remember it for further use
        dict set _vectors $cname "" $vec
        return $vec
    }

    # Returns a (X RBC vector, Y RBC vector) pair for the yname table
    # column whose elements are ordered based on the ascending sort
    # order of the xname table column.
    method _xyvector {xname yname} {
        if {$xname eq ""} {
            set xname $options(-xcolumn)
        }
        if {$xname eq ""} {
            set xname [lindex [tarray::table cnames $_table] 0]
        }

        set ycol [tarray::table column $_table $yname]
        if {[tarray::column type $ycol] in {string any}} {
            error "Column $yname is not numeric and cannot be used for the Y-component of a graph."
        }

        # The vectors generated for the graph have to be in the ascending
        # sort order for the X column. So generate the indices of the
        # X column in order of sorted values.
        if {![info exists _sorted_indices($xname)]} {
            $self _setup_sorted_column $xname
        }

        # Now generate the X vector in sorted order if it does not exist
        if {[dict exists $_vectors $xname $xname]} {
            set xvec [dict get $_vectors $xname $xname]
        } else {
            set xvec [rbc::vector create #auto]
            set xcol [tarray::table column $_table $xname]
            if {[tarray::column type $xcol] in {any string}} {
                # For non-numeric columns, the index in the table will
                # act as the numeric value. That is what will be displayed
                # on the axis. If a label is to be shown instead, the
                # application can register an appropriate callback
                # using the RBC axis configure command.
                $xvec seq 0 [expr {[tarray::column size $xcol]-1}]
            } else {
                # Create a RBC vector for the X column in sorted order
                tarray::rbc::tovector $xvec $xcol $_sorted_indices($xname)
            }
            dict set _vectors $xname $xname $xvec
        }

        # Now generate the Y vector in sort order of X
        if {[dict exists $_vectors $yname $xname]} {
            set yvec [dict get $_vectors $yname $xname]
        } else {
            set yvec [rbc::vector create #auto]
            tarray::rbc::tovector $yvec $ycol $_sorted_indices($xname)
            dict set _vectors $yname $xname $yvec
        }        

        # Return the sorted RBC vector pair
        return [list $xvec $yvec]
    }

    delegate method * to hull
}

package require rbc
package require tarray
set tab [tarray::table create {
    code double name string population double
} {
    {100 China 1350000000}
    {300 India 1200000000}
    {200 Vatican 850}
}]
destroy .win
