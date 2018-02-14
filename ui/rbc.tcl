# (c) 2018 Ashok P. Nadkarni
# All rights reserved.
# See the file license.terms for license

lappend auto_path d:/tcl/lib [pwd]
package require snit

::snit::widgetadaptor tarray::ui::rbc::chart {
    ################################################################
    # Type variables

    # Rbc element option names. Array indexed by element type.
    typevariable _rbc_plot_option_names

    ################################################################
    # Variables
    
    # Keeps track of the various plots in the chart. Dictionary indexed by
    # the RBC plot name and each element being a nested inner dictionary
    # with the following keys
    #  RbcType - "element", "bar" or "line"
    #  XColumn - name of the corresponding X column. Only exists if the
    #      the plot data came from a table column.
    #  YColumn - name of the corresponding Y column. Only exists if the
    #      the plot data came from a table column.
    variable _plots {}
    
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
    variable _vectors {}
    
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

        installhull using rbc::graph
        $self configurelist $args

        # Widget is constructed
        set _constructed 1
    }

    destructor {
        dict for {ycol xcols} $_vectors {
            dict for {xcol v} $xcols {
                rbc::vector destroy $v
            }
        }
    }

    method table {} {
        return $_table
    }

    method column {cname} {
        return [tarray::table column $_table $cname]
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

        dict set _plots $name RbcType $plot_type

        # cname set -> plotting a table column of that name
        # else -> raw data passed directly to RBC widget.
        if {[info exists cname]} {
            # We are plotting table column data
            dict set _plots $name YColumn $cname
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
                set xcolname [$self _get_xcolumn $xcolname]
                dict set _plots $name XColumn $xcolname
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
    method _cget_plot_option {plot_name optname} {
        set optname [$self _plot_option_match $plot_name $optname {
            -xcolumn -ycolumn
        }]
        switch -exact -- $optname {
            -xcolumn {
                if {[dict exists $_plots $plot_name XColumn]} {
                    return [dict get $_plots $plot_name XColumn]
                } else {
                    return ""
                }
            }
            -ycolumn {
                if {[dict exists $_plots $plot_name YColumn]} {
                    return [dict get $_plots $plot_name YColumn]
                } else {
                    return ""
                }
            }
            default {
                return [$hull [dict get $_plots $plot_name RbcType] cget $plot_name $optname]
            }
        }
    }
    method {element cget} {plot_name optname} {
        $self _cget_plot_option $plot_name $optname
    }
    method {line cget} {plot_name optname} {
        $self _cget_plot_option $plot_name $optname
    }
    method {bar cget} {plot_name optname} {
        $self _cget_plot_option $plot_name $optname
    }

    method _configure_plot_option {plot_name optname args} {
        set optname [$self _plot_option_match $plot_name $optname {
            -xcolumn -ycolumn
        }]

        if {[llength $args] == 0} {
            # Return the corresponding option record
            switch -exact -- $optname {
                -xcolumn -
                -ycolumn {
                    return [list $optname {} {} {} \
                                [$self _cget_plot_option $plot_name $optname]]
                }
                default {
                    return [$hull [dict get $_plots $plot_name RbcType] configure $plot_name $optname]
                }
            }
        }

        set optval [lindex $args 0]
        switch -exact -- $optname {
            -xcolumn -
            -ycolumn {
                error "The $optname option for a plot cannot be modified once set."
            }
            default {
                return [$hull [dict get $_plots $plot_name RbcType] configure $plot_name $optname] $optval
            }
        }
    }

    method _configure_plot {plot_name args} {

        set rbctype [dict get $_plots $plot_name RbcType]

        # If no args, return list of all options, the base RBC ones
        # as well as our added ones
        if {[llength $args] == 0} {
            set retval [$hull $rbctype configure $plot_name]
            lappend retval [$self _configure_plot_option $plot_name -xcolumn] \
                [$self _configure_plot_option $plot_name -ycolumn]
            return $retval
        }

        # Single option -> return its option record
        if {[llength $args] == 1} {
            return [$self _get_plot_option $plot_name [lindex $args 0]]
        }

        if {[llength $args] & 1} {
            error "Missing value for option [lindex $args end]."
        }

        set rbcopts {}
        foreach {optname optval} $args {
            set optname [$self _plot_option_match $plot_name $optname {
                -xcolumn -ycolumn
            }]
            switch -exact -- $optname {
                -xcolumn -
                -ycolumn {
                    $self _configure_plot_option $plot_name $optname $optval
                }
                default {
                    lappend rbcopts $optname $optval
                }
            }
        }

        $hull $rbctype configure $plot_name {*}$rbcopts
        return
    }

    method {element configure} {plot_name args} {
        $self _configure_plot $plot_name {*}$args
    }
    method {line configure} {plot_name args} {
        $self _configure_plot $plot_name {*}$args
    }
    method {bar configure} {plot_name args} {
        $self _configure_plot $plot_name {*}$args
    }
    
    delegate method {element *} to hull using "%c element %m"
    delegate method {line *} to hull using "%c line %m"
    delegate method {bar *} to hull using "%c bar %m"

    method _set_xcolumn_option {optname cname} {
        set options(-xcolumn) [$self _get_xcolumn $cname]
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

    method _get_xcolumn {{xname ""}} {
        if {$xname eq ""} {
            set xname $options(-xcolumn)
        }
        set cnames [tarray::table cnames $_table]
        if {$xname ne ""} {
            if {$xname in $cnames} {
                return $xname
            } else {
                throw {TARRAY TABLE NOTFOUND} "Column $xname not found in table."
            }
        }
        return [lindex $cnames 0]
    }

    # Returns the full option name of an element configure option
    # taking into account the core RBC element options as well as
    # additional options passed in as $args
    method _plot_option_match {plot_name optname {extra_opts {}}} {
        return [tcl::prefix match \
                    [concat \
                         [$self _rbc_plot_options $plot_name] \
                         $extra_opts \
                        ] \
                    $optname]
    }

    # Returns RBC option names for the chart plot identified by $plot_name
    method _rbc_plot_options {plot_name} {
        set rbc_elem_type [dict get $_plots $plot_name RbcType]
        if {! [info exists _rbc_plot_option_names($rbc_elem_type)]} {
            set _rbc_plot_option_names($rbc_elem_type) \
                [lmap optrec [$hull $rbc_elem_type configure $plot_name] {
                    lindex $optrec 0
                }]
        }
        return $_rbc_plot_option_names($rbc_elem_type)
    }

    # Returns 1 if the specified optname is a valid RBC option for
    # the specified plot
    method _is_rbc_plot_option {plot_name optname} {
        return [expr {$optname in [$self _rbc_plot_options $plot_name]}]
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
