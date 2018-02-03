# (c) 2018 Ashok P. Nadkarni
# All rights reserved.
# See the file license.terms for license

::snit::widgetadaptor tarray::ui::graph {
    # Specifies the 
    option -legend -configuremethod _set_legend_option

    # Specifies the name of the table column to use as the X-axis
    option -xcolumn -configuremethod _set_xcolumn_option

    delegate option * to hull

    # Holds the data table being displayed
    variable _table
    
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
                if {![info exists _plots($cname)]} {
                    $self _vectorify $cname
                }
                lappend args -xdata $_vectors($options(-xcolumn)) -ydata $_vectors($cname) 
            }
        }
        $hull element $oper $cname {*}$args
    }

    method _set_xcolumn_option {optname optval} {
        if {$optval ni [tarray::table cnames $_table]} {
            error "Table does not contain column named $optval"
        }
        set options(-xcolumn) $optval
        $self _vectorify $options(-xcolumn)
    }

    method _vectorify {cname} {
        if {[info exists _vectors($cname)]} return
        set _vectors($cname) [::rbc::vector create #auto]
        tarray::rbc::tovector $_vectors($cname) [tarray::table::column $_table $cname]
    }

    delegate method * to hull
}


set tab [tarray::table create {
    code double population double
} {
    {100 1350000000}
    {200 850}
}]
