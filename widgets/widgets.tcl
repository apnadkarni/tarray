package require Tk
package require snit
package require treectrl
package require tarray
source [file join [file dirname [info script]] color.tcl];# TBD
namespace eval tarray::ui {
    proc setup_nspath {} {
        uplevel 1 {namespace path [linsert [namespace path] end [namespace parent [namespace parent]]]}
    }
}

proc ldebug {args} {
    append ::log [concat $args]\n
}

snit::widgetadaptor tarray::ui::tableview {

    ### Type constructor

    typeconstructor {
        setup_nspath
        # TBD font create WitsFilterFont {*}[font configure WitsDefaultItalicFont] -underline 1
        # TBD
        font create WitsFilterFont {*}[font configure TkDefaultFont]
    }

    # TBD - obsolete options?
    option -highlight -default 0
    option -newhighlight -default #00ff00; # Hex because Tk changed "green"
    option -deletedhighlight -default red
    option -modifiedhighlight -default yellow

    # Command to execute when list selection changes
    option -selectcommand -default ""

    # Right mouse button click
    option -rightclickcommand -default ""

    # Double click command
    option -pickcommand -default ""


    # TBD Whether to only show changed rows (including new ones)
    # option -showchangesonly -default false -configuremethod _setshowchangesonly

    # Values to show in optional header, keyed by property name
    option -filtervalues -default {} -configuremethod _setfiltervalues

    option -undefinedfiltertext -default "<Filter>"

    option -showfilter -default 0 -configuremethod _setshowfilter

    # TBD sort order 
    option -defaultsortorder -default "-increasing"

    component _treectrl
    delegate method * to _treectrl
    delegate option * to _treectrl

    variable _datasource
    
    variable _constructed 0

    variable _columns {}
    variable _column_order {}

    variable _sort_column ""
    variable _sort_order "" ;# -increasing or -decreasing

    variable _item_style_phrase {}

    # Column of row id's where the row id identifies a row for the data source
    variable _row_ids
    # Column of tktreectrl item id's. One-to-one correspondence between
    # _row_ids and _item_ids, both indexed by physical (0-based) location
    # in table. TBD - do we really need _item_ids? We can get it from
    # $_treectrl item id [list rnc $row_index 0]
    variable _item_ids
    
    # Saves visible state like anchor, active item, selection etc
    variable _display_state {}
    
    # Stores various state info related to tooltips shown when mouse
    # hovers over an item
    variable _tooltip_state

    # Contains the name of the filter column being edited, if any
    variable _filter_column_being_edited
    
    constructor {datasource coldefs args} {
        set _datasource $datasource

        installhull using ttk::frame -borderwidth 0

        install _treectrl using treectrl $win.tbl \
            -highlightthickness 1 \
            -borderwidth 0 \
            -showroot no -showbuttons no -showlines no \
            -selectmode extended -xscrollincrement 20 -xscrollsmoothing 1 \
            -canvaspadx {2 0} -canvaspady {2 0} \
            -scrollmargin 16 -xscrolldelay "500 50" -yscrolldelay "500 50"
        # TBD -itemheight $height

        
        # item and column identify where the mouse is hovering
        # -1 indicates invalid (ie mouse is outside an item)
        array set _tooltip_state {item -1 column -1 schedule_id -1}

        $_treectrl header create -tags H2

        $_treectrl notify bind $_treectrl <ItemVisibility> [mymethod _visibilityhandler %h %v]
        $_treectrl notify bind $_treectrl <Selection> [mymethod _selecthandler %D %S ]
        bind $_treectrl <Motion> [mymethod _motionhandler %x %y]
        # See comments in _leavehandler as to why this is commented out
        # bind $_treectrl <Leave> [mymethod _leavehandler %x %y]
        # The following binding is needed because we removed the one above
        # else if you exit exactly where the tooltip was displayed
        # and reenter at the same point the tooltip is not displayed.
        # bind $_treectrl <Enter> [mymethod _cancel_tooltip]

        # Define the filter header row
        $_treectrl element create h2Elem text -lines 1 -justify left  -statedomain header -fill blue
        $_treectrl style create h2Style -orient horizontal -statedomain header
        $_treectrl style elements h2Style {h2Elem}
        $_treectrl style layout h2Style h2Elem -squeeze x -expand ns -padx 5

        # TBD Define the states used to highlight changes
        $_treectrl state define modified
        $_treectrl state define new
        $_treectrl state define deleted

        ttk::scrollbar $win.vscroll \
            -orient vertical \
            -command "$_treectrl yview" 
	$_treectrl notify bind $win.vscroll <Scroll-y> [mymethod _position_scrollbar %W %l %u]
	bind $win.vscroll <ButtonPress-1> "focus $_treectrl"
        ttk::scrollbar $win.hscroll \
            -orient horizontal \
            -command "$_treectrl xview" 
	$_treectrl notify bind $win.hscroll <Scroll-x> [mymethod _position_scrollbar %W %l %u]
	bind $win.hscroll <ButtonPress-1> "focus $_treectrl"
         
        grid columnconfigure $win 0 -weight 1
        grid rowconfigure $win 0 -weight 1
        grid configure $_treectrl -row 0 -column 0 -sticky news
        grid configure $win.hscroll -row 1 -column 0 -sticky we
        grid configure $win.vscroll -row 0 -column 1 -sticky ns
        # Do not show the scroll bars right away before widget is populated.
        # Otherwise, when there are too few rows, blank space appears in
        # the scroll bar area whereas it should have been taken up by the
        # main window.
        grid remove $win.hscroll
        grid remove $win.vscroll

        # Bind to select all
        bind $_treectrl <Control-a> [list %W selection add all]

        # Standard mouse bindings
        bind $_treectrl <Double-1> [mymethod _dblclickhandler %x %y %X %Y]
        bind $_treectrl <ButtonPress-3> [mymethod _rightclickhandler %x %y %X %Y]
        # Create the background element used for coloring
        # TBD set sel_color [get_theme_setting bar frame normal bg]
        set sel_color blue
        $_treectrl gradient create gradientSelected \
            -stops [list [list 0.0 $sel_color 0.5] [list 1.0 $sel_color 0.0]] \
            -orient vertical

        # Define states used to control selection highlighting - which
        # cell borders are merged with next cell
        $_treectrl state define openW
        $_treectrl state define openE
        $_treectrl state define openWE

        
        # Do we show plain selection highlighting or a gradient-based
        # fancy one ? Right now use plain version. If switching to fancy
        # version, note that the column drag code has to be modified
        # to change the open border settings for the columns when
        # they are moved (ie openWE etc. state assignments)
        set plain_select 1
        if {$plain_select} {
            $_treectrl element create bgElem rect \
                -fill [list lightblue selected $options(-newhighlight) new \
                           $options(-deletedhighlight) deleted \
                           $options(-modifiedhighlight) modified] \
                -outline "" -rx 0 \
                -outlinewidth 1
        } else {
            $_treectrl element create bgElem rect \
                -fill [list gradientSelected selected $options(-newhighlight) new \
                           $options(-deletedhighlight) deleted \
                           $options(-modifiedhighlight) modified] \
                -outline [list $sel_color selected] -rx 1 \
                -open [list we openWE w openW e openE] \
                -outlinewidth 1
        }

        # Create the elements for text and numbers
        $_treectrl element create leftJustifyElem text -lines 1 -justify left
        $_treectrl element create rightJustifyElem  text -lines 1 -justify right

        
        # Create the corresponding styles 
        $_treectrl style create leftJustifyStyle -orient horizontal
        $_treectrl style elements leftJustifyStyle {bgElem leftJustifyElem}
        $_treectrl style layout leftJustifyStyle leftJustifyElem -squeeze x -expand ns -padx 5
        $_treectrl style layout leftJustifyStyle bgElem -detach yes -iexpand xy

        $_treectrl style create rightJustifyStyle -orient horizontal
        $_treectrl style elements rightJustifyStyle {bgElem rightJustifyElem}
        $_treectrl style layout rightJustifyStyle rightJustifyElem -squeeze x -expand ns -padx 5
        $_treectrl style layout rightJustifyStyle bgElem -detach yes -iexpand xy

        $self configurelist $args
        
        $_treectrl notify install <Header-invoke>
        $_treectrl notify bind MyHeaderTag <Header-invoke> [mymethod _headerhandler %H %C]

        $_treectrl notify install <ColumnDrag-begin>
        $_treectrl notify install <ColumnDrag-end>
        $_treectrl notify install <ColumnDrag-indicator>
        $_treectrl notify install <ColumnDrag-receive>

        $_treectrl notify bind MyHeaderTag <ColumnDrag-receive> [mymethod _column_move_handler %C %b]

        $_treectrl header dragconfigure -enable yes
        $_treectrl header dragconfigure all -enable yes -draw yes

        $self definecolumns $coldefs
        set _constructed 1
    }

    destructor {
    }

    method setrows {row_ids args} {
        set sort_column [from args -sortcolumn ""]
        set sort_order [from args -sortorder -increasing]
        set filters    [from args -filters ""]
        if {[llength $args]} {
            error "Invalid syntax. Must be \"$win setrows ROW_IDS ?-sortcolumn COLNAME? ?-sortorder -increasing|-decreasing? ?-filters FILTERDICT?\""
        }

        $self _save_display_state; # Want to preserve view/selections etc.

        #_row_ids may be a column or a list
        set _row_ids [tarray::column::create int $row_ids]
        set count [tarray::column size $_row_ids]
        $_treectrl item delete all
        set _item_ids [tarray::column create int [$_treectrl item create -parent root -open no -count $count]]
        
        if {[dict size $_columns]} {
            set col [dict get $_columns [lindex $_column_order 0] Id]
            for {set i 0} {$i < $count} {incr i} {
                # TBD - the following comment is obsolete but the whole
                # issue of lazy updates has to be worked on.
                # We will initialize styles and contents of a row only
                # when they are actually displayed. This is to save
                # memory with large tables. However, tktreectrl does not
                # seem to call us back when an item is displayed if none
                # of the items have their styles set. Also, we have to
                # make sure sorting works correctly, so we set the style
                # and content of the sort column.
                $_treectrl item style set [tarray::column index $_item_ids $i] $col [dict get $_item_style_phrase $col]
            }
        }

        $self _update_sort_indicators $sort_column $sort_order
        $self _restore_display_state; # Want to preserve view/selections etc.
        return
    }

    method _update_sort_indicators {cname order} {
        # Reset the existing arrow indicator on the sort column
        if {$_sort_column ne ""} {
            $_treectrl column configure [$self column_name_to_id $_sort_column] -arrow none -itembackground {}
        }
        set _sort_column $cname
        set _sort_order  $order
        
        # Set the indicator on the new sort column
        if {$cname ne ""} {
            if {$order eq "-increasing"} {
                set arrow up
            } else {
                set arrow down
            }
            $_treectrl column configure [$self column_name_to_id $cname] -arrow $arrow -itembackground [color::shade [$_treectrl cget -background] black 0.05]
        }
        return
    }

    # From sbset at http://wiki.tcl.tk/950
    method _position_scrollbar {sb first last} {
        # Get infinite loop on X11
        if {$::tcl_platform(platform) ne "unix"} {
            if {$first <= 0 && $last >= 1} {
                grid remove $sb
            } else {
                grid $sb
            }
        }
        $sb set $first $last
        return
    }

    method gettreectrlpath {} {
        return $_treectrl
    }

    method _rightclickhandler {winx winy screenx screeny} {
        if {$options(-rightclickcommand) ne ""} {
            lassign [$_treectrl identify $winx $winy] type item_id col_id
            if {$type eq "" || $type eq "item"} {
                {*}$options(-rightclickcommand) $item_id $col_id $winx $winy $screenx $screeny
            } 
        }
    }

    method _dblclickhandler {winx winy screenx screeny} {
        if {$options(-pickcommand) ne ""} {
            lassign [$_treectrl identify $winx $winy] type item_id col_id
            if {$type eq "item"} {
                set id [$self _item_to_row_id $item_id]
                uplevel #0 [linsert $options(-pickcommand) end $id $item_id $col_id $winx $winy $screenx $screeny]
            }
        }
    }

    method _headerhandler {hdr_id col_id} {
        if {$hdr_id == 0} {
            set colname [$self column_id_to_name $col_id]
            if {![dict get $_columns $colname Sortable]} {
                return
            }
            # Column header, sort accordingly
            if {$colname eq $_sort_column && $_sort_order eq "-increasing"} {
                set order -decreasing
            } else {
                set order -increasing
            }
            $self _sort $colname $order
        } elseif {$hdr_id == 1} {
            #event generate $win <<FilterSelect>> -data [$self column_id_to_name $col_id]
            $self _editfilter [$self column_id_to_name $col_id]
        }
        return
    }

    method _cancel_tooltip {} {
        if {[winfo exists $win.tooltip]} {
            wm withdraw $win.tooltip
        }

        set _tooltip_state(item) -1
        set _tooltip_state(column) -1
        if {$_tooltip_state(schedule_id) != -1} {
            after cancel $_tooltip_state(schedule_id)
            set _tooltip_state(schedule_id) -1
        }
    }

    method _schedule_tooltip {item column winx winy} {
        $self _cancel_tooltip;  # Cancel pending tooltip if any
        set _tooltip_state(item) $item
        set _tooltip_state(column) $column
        set _tooltip_state(winx) $winx
        set _tooltip_state(winy) $winy
        set _tooltip_state(schedule_id) [after 100 [mymethod _show_tooltip]]
    }

    method _show_tooltip {} {
        # Called back from event loop
        set _tooltip_state(schedule_id) -1

        if {$_tooltip_state(item) == -1 || $_tooltip_state(column) == -1} {
            # No longer in an item
            return
        }

        # Get current font as it can be changed by user
        set font [$_treectrl cget -font]

        
        # Find the cell position and add to tree control position
        lassign [$_treectrl item bbox $_tooltip_state(item) $_tooltip_state(column)] xpos ypos width height
        set width [expr {$width - $xpos}]
        set height [expr {$height -$ypos}]

        # Figure out whether the cell needs a tooltip
        set text [$_treectrl item text $_tooltip_state(item) $_tooltip_state(column)]
        set required_width [font measure $font -displayof $_treectrl $text]
        # The margin "10" is to take care of ellipsis
        if {$required_width <= ($width-10)} {
            return;             # Whole text is displayed, no need for tooltip
        }

        # Position just above the row. That way we can see the 
        # whole row of interest. More important, double clicks on
        # the row work. Note we position with a gap of 5 vertical pixels
        # so that when the mouse moves, it enters the preceding row
        # thereby canceling the tooltip
        set xpos [expr {$xpos + [winfo rootx $_treectrl] + 30}]
        set ypos [expr {$ypos + [winfo rooty $_treectrl] - $height - 0}]

        # Create window if it does not exist
        if {![winfo exists $win.tooltip]} {
            toplevel $win.tooltip
            # Padding is for alignment with treectrl
            label $win.tooltip.l -background [$_treectrl cget -background] -relief solid -borderwidth 1 -padx 4 -pady 0
            # We are showing tooltips ABOVE the row now so if mouse
            # enters the tooltip, it means the row is not being hovered
            #bind $win.tooltip <Enter> [mymethod _cancel_tooltip]
            bind $win.tooltip <Enter> [mymethod _proxymouse Enter "" %X %Y]

            # Bind mouse clicks so they get passed on to parent frame
            foreach event {
                Button
                Shift-Button
                Control-Button
                Double-Button
            } {
                bind $win.tooltip <$event> [mymethod _proxymouse $event %b %X %Y]
            }
            bind $win.tooltip <MouseWheel> "event generate $_treectrl <MouseWheel> -delta %D"
            
            pack $win.tooltip.l -side left -fill y
            wm overrideredirect $win.tooltip 1
            wm withdraw $win.tooltip
        }
        
        $win.tooltip.l configure -text $text -font $font
        wm deiconify $win.tooltip
        wm geometry $win.tooltip +$xpos+$ypos
        raise $win.tooltip
    }

    method _proxymouse {event button screenx screeny} {

        if {$_tooltip_state(item) == -1} {
            return;             # Cannot happen, can it ?
        }

        set item $_tooltip_state(item); # Save before cancel
        set col  $_tooltip_state(column); # Save before cancel
        set winx  $_tooltip_state(winx); # Save before cancel
        set winy  $_tooltip_state(winy); # Save before cancel

        $self _cancel_tooltip
        focus $_treectrl
        switch -exact -- "$event-$button" {
            Enter- {
                set rootx [winfo rootx $_treectrl]
                set rooty [winfo rooty $_treectrl]
                event generate $_treectrl <Motion> -when tail -x [expr {$screenx-$rootx}] -y [expr {$screeny-$rooty}]
            }
            Button-1 {
                if {0} {
                    # Instead,  event generate below so any other actions 
                    # will also be taken (just in case)
                    $_treectrl selection clear
                    $_treectrl selection add $item
                    $_treectrl selection anchor $item
                }
                event generate $_treectrl <Button> -when mark -button 1 -x $winx -y $winy
            }
            Shift-Button-1 {
                if {[llength [$_treectrl selection get]]} {
                    $_treectrl selection add anchor $item
                } else {
                    $_treectrl selection add $item
                    $_treectrl selection anchor $item
                }
            }
            Control-Button-1 {
                $_treectrl selection add $item
            }
            Button-3 {
                if {$options(-rightclickcommand) ne ""} {
                    {*}$options(-rightclickcommand) $item $col 0 0 $screenx $screeny

                }
            }
            default {
                puts "$event-$button"
            }
        }
    }

    method _motionhandler {x y} {
        $_treectrl identify -array pos $x $y
        if {$pos(where) ne "item" || $pos(column) eq ""} {
            # Mouse moved out of an item - cancel tooltip state
            $self _cancel_tooltip
            return
        }

        # If the cell has changed, then cancel and requeue
        # the request
        if {$pos(item) != $_tooltip_state(item) ||
            $pos(column) != $_tooltip_state(column)} {
            $self _schedule_tooltip $pos(item) $pos(column) $x $y
            return
        }

        # If cell still same, nothing to do
        return
    }

    method _leavehandler {x y} {
        # We used to bind the treectrl to <Leave> so the tooltip could
        # be removed. However, this had the problem that displaying the
        # tooltip would also generate a <Leave> causing the handler
        # to immediately cancel it. So we now bind to the tooltip
        # <Leave> handler to withdraw the tooltip.
        #        $self _cancel_tooltip
    }


    method column_id_to_name {col_id} {
        dict for {colname coldef} $_columns {
            if {[dict get $coldef Id] == $col_id} {
                return $colname
            }
        }
        error "Column with id $col_id not found in _columns"
    }

    method column_name_to_id {colname} {
        return [dict get $_columns $colname Id]
    }

    method definecolumns {coldefs} {
        # For now, do not allow changes after initial construction
        if {$_constructed} {
            error "Cannot change column definitions after initialization"
        }

        # TBD - when no columns are in the table, 'item id {first visible}'
        # returns 1 (?). The 'see' command then crashes wish.
        # This is currently protected by forcing user to make
        # at least one column visible in the table editor.
        if {[dict size $coldefs] == 0} {
            error "At least one column must be included in table."
        }
        # Note that to account for this being fixed in the future,
        # the code below dows not assume coldefs is non-empty

        # Note what column we are currently sorted on
        # TBD
        set sort_col_name $_sort_column

        set _columns {}
        dict for {colname coldef} $coldefs {
            set def [dict merge {
                Type string
                Sortable 1
                Squeeze 0
            } $coldef]
            if {![dict exists $def Label]} {
                dict set def Label $colname
            }
            if {![dict exists $def Justify]} {
                if {[dict get $def Type] in {string any}} {
                    dict set def Justify left
                } else {
                    dict set def Justify right
                }
            }
            dict set _columns $colname $def
        }
        
        $_treectrl item delete all
        $_treectrl header delete all
        $_treectrl column delete all

        $_treectrl header create -tags H2

        set _item_style_phrase {}
        dict for {colname coldef} $_columns {
            set justify [dict get $coldef Justify]
            set style ${justify}JustifyStyle

            # TBD - should squeeze be 1 or 0 ? If 1, everything fits in 
            # window but has ellipses. If 0, columns scroll off the window.
            # Right now we rely on the caller to tell us whether the column
            # should be squeezable.
            # Note this also interacts with the column lock below. Locked
            # columns do not get squeezed ?
            set squeeze [dict get $coldef Squeeze]
            set minwidth [expr {$squeeze ? 80 : 40}]

            set col_id [$_treectrl column create \
                            -text [dict get $coldef Label] \
                            -arrow none \
                            -squeeze $squeeze \
                            -justify $justify \
                            -minwidth $minwidth]
            dict set _columns $colname Id $col_id
            dict set _columns $colname OutlineState {!openE !openW openWE}
            lappend _item_style_phrase $col_id $style
        }
        set _column_order [dict keys $_columns]
        
        # Locked columns do not get squeezed. This gets very confusing when
        # the first (locked) column takes up the entire window width. The
        # scroll bars do not work in this case because there is no room for
        # them to show the hidden columns. The user has to expand the window
        # to show the hidden columns which is not immediately obvious to
        # them.
        if {0} {
            $_treectrl column configure "first visible" -lock left
        }

        if {[dict size $_columns]} {
            # Set up the first and last columns to have "closed" outlines
            # for the selection rectangle.
            dict set _columns [lindex $_column_order 0] OutlineState {!openWE !openW openE}
            dict set _columns [lindex $_column_order end] OutlineState {!openWE !openE openW}
        }

        # Build the secondary table header
        $self _populatefilter
    }

    ###
    # Methods for modifying view
    
    method _initrow {item {row {}}} {
        $_treectrl item style set $item {*}$_item_style_phrase

        if {[llength $row]} {
            # It is faster to build a (colid, text) 
            # list and make a single call to $_treectrl item text
            set vals {}
            foreach col_id [$_treectrl column list] val $row {
                lappend vals $col_id $val
            }
            $_treectrl item text $item {*}$vals
        }
    }

    method insert {row_ids {pos 0} {tags {}}} {
        if {[llength $row_ids] == 0} {
            return
        }

        set current_count [$_treectrl item count]
        # Some sanity checks. Remember the treectrl count includes the
        # (implicit) root item
        if {$pos < 0 || $pos >= $current_count} {
            error "Invalid table view index $pos"
        }

        # Need to do this before treectrl operations since that can
        # call back right away
        # TBD - in case of errors below, maybe wrap in a try and revert?
        tarray::column vinject _row_ids $row_ids $pos
        
        if {$current_count == ($pos+1)} {
            # Appending to the end
            set items [$_treectrl item create -parent root -open no -count [llength $row_ids] -tags $tags]
        } else {
            set items [$_treectrl item create -nextsibling [list rnc $pos 0] -open no -count [llength $row_ids] -tags $tags]
        }
        tarray::column vinject _item_ids $items $pos

        if {[dict size $_columns]} {
            foreach item $items row_id $row_ids {
                # TBD
                # We will initialize styles and contents of a row only
                # when they are actually displayed. This is to save
                # memory with large tables. However, tktreectrl does not
                # seem to call us back when an item is displayed if none
                # of the items have their styles set. Also, we have to
                # make sure sorting works correctly, so we set the style
                # and content of the sort column.
                set col [dict get $_columns [lindex $_column_order 0] Id]
                $_treectrl item style set $item $col [dict get $_item_style_phrase $col]
            }
        }

        return 
    }

    method modify {first args} {
        if {$options(-highlight)} {
            $_treectrl item state set $item {!deleted !new modified}
        }
        
        # If $args specified, it must be a single integer value
        # as must $first. The range $first:[lindex $args 0] is deleted.
        # If $args is empty, $first can be a list of one or more
        # indices
        if {[llength $args]} {
            if {[llength $args] != 1} {
                error "wrong # args: should be \"W delete first last\""
            }
            set last [lindex $args 0]
            if {$first < 0 || $first > $last} {
                error "Invalid range $first:$last"
            }
            if {$last > [tarray::column size $_item_ids]} {
                error "Range limit $last greater than table size"
            }
            for each index that is visible update data
        } else {
            for each index that is visible update data
        }
        return
    }

    method delete {row_ids} {
        set rindices {}
        foreach row_id $row_ids {
            set rindex [tarray::column search $_row_ids $row_id]
            if {$rindex >= 0} {
                lappend rindices $rindex
            }
        }
        return [$self delete_indices $rindices]
    }
    
    method delete_indices {first args} {
        # TBD - when items are deleted is the visibility handler called?
        # TBD - handling of selection - a selection event is generated
        #   need to handle that and remove from selection
        
        set first_item [$self _first_display_item]
        if {$first_item > 0} {
            set first_rindex [$self _item_to_rindex $first_item]
        } else {
            set first_rindex -1
        }
        
        # If $args specified, it must be a single integer value
        # as must $first. The range $first:[lindex $args 0] is deleted.
        # If $args is empty, $first can be a list of one or more
        # indices
        if {[llength $args]} {
            if {[llength $args] != 1} {
                error "wrong # args: should be \"W delete first last\""
            }
            set last [lindex $args 0]
            if {$first < 0 || $first > $last} {
                error "Invalid range $first:$last"
            }
            tarray::column vdelete _row_ids $first $last
            tarray::column vdelete _item_ids $first $last
            $_treectrl item delete [list root child $first] [list root child $last]
            if {$last < $first_rindex} {
                $_treectrl see $first_item
            }
        } elseif {[llength $first]} {
            # $first is a list of one or more indices
            tarray::column vdelete _row_ids $first
            set item_ids [tarray::column get -list $_item_ids $first]
            tarray::column vdelete _item_ids $first
            $_treectrl item delete [list list $item_ids]
            if {$first_rindex >= 0 && $first_rindex ni $first} {
                $_treectrl see $first_item
            }
        }
        return
    }

    method resethighlights {} {
        set deleted_items [$_treectrl item id {state deleted}]
        if {[llength $deleted_items]} {
            foreach item $deleted_items {
                unset -nocomplain _itemvalues($item)
            }
            $_treectrl item delete [list "list" $deleted_items]
        }

        $_treectrl item state set all {!modified !new}
        if {$options(-showchangesonly)} {
            # Note this is {root children}, not "all" else root
            # and everything else becomes invisible

            # Need to check count else treectrl throws error it no items
            if {[$_treectrl item count {root children}] > 0} {
                $_treectrl item configure {root children} -visible 0
            }
        }
    }

    ###
    # Tracking of items actually displayed

    method _get_data {row_id} {
        return [{*}$_datasource get $row_id $_column_order]
    }
    
    method _visibilityhandler {invisible visible} {
        if {[llength $invisible]} {
            #TBD - delete styles and text from elements ?
            $_treectrl item tag remove [list "list" $invisible] tv-displayed
        }
        
        if {[llength $visible]} {
            $_treectrl item tag add [list "list" $visible] tv-displayed
            foreach item $visible {
                lassign [$_treectrl item rnc $item] row_index col_index
                $self _initrow $item [$self _get_data [tarray::column index $_row_ids $row_index]]
            }
        }
    }

    method _displayed_items {} {
        return [$_treectrl item id {tag tv-displayed}]
    }
    
    method _save_display_state {} {
        set _display_state [dict create]
        set item_id [$self _first_display_item]
        if {$item_id > 0} {
            dict set _display_state display_top [$self _item_to_row_id $item_id]
            ldebug "Save $_display_state"
        }
        foreach item {active anchor} {
            set item_id [$_treectrl item id $item]
            if {$item_id > 0} {
                dict set _display_state $item [$self _item_to_row_id $item_id]
            }
        }
        set selection [$self getselected]
        if {[llength $selection]} {
            dict set _display_state selection $selection
        }
    }
    
    method _restore_display_state {} {
        $_treectrl selection clear
        ldebug "Restore $_display_state"
        if {[dict exists $_display_state anchor]} {
            set item [$self _row_id_to_item [dict get $_display_state anchor]]
            if {$item != 0} {
                $_treectrl selection anchor $item
            }
        }
        
        if {[dict exists $_display_state selection]} {
            set items [lmap rid [dict get $_display_state selection] {
                set item [$self _row_id_to_item $rid]
                if {$item == 0} continue
                set item
            }]
            if {[llength $items]} {
                $_treectrl selection add [list "list" $items]
            }
        }
        
        if {[dict exists $_display_state active]} {
            set item [$self _row_id_to_item [dict get $_display_state active]]
            if {$item != 0} {
                $_treectrl activate $item
            }
        }
        
        if {0} {
            # TBD Restoring the top line commented out because (a) most
            # widgets (e.g. Windows explorer) take you back to the top after
            # sorting by a column etc., and (b) cannot get this to work
            # although the same commands typed into the console work.
            if {[dict exists $_display_state display_top]} {
                if {1} {
                    set rindex [$self _row_id_to_rindex [dict get $_display_state display_top]]
                    if {$rindex >= 0} {
                        $_treectrl yview moveto 0.0
                        $_treectrl yview scroll $rindex units
                    }
                } else {
                    # This does not work as well as above if the row
                    # happens to be already displayed in the middle
                    set item [$self _row_id_to_item [dict get $_display_state display_top]]
                    if {$item != 0} {
                        $_treectrl see $item
                    }
                }
            }
        }
        return
    }
    
    ###
    # Map row ids/item ids/row indices
    method rowids {} {return [tarray::column range -list $_row_ids 0 end]}

    method _item_to_rindex {item_id} {
        return [lindex [$_treectrl item rnc $item_id] 0]
    }
                
    method _item_to_row_id {item_id} {
        return [tarray::column index $_row_ids [$self _item_to_rindex $item_id]]
    }
    
    method _row_id_to_rindex {rid} {
        return [tarray::column::search $_row_ids $rid]
    }
    
    method _row_id_to_item {rid} {
        # 0 -> not found
        set rindex [tarray::column::search $_row_ids $rid]
        if {$rindex >= 0} {
            return [tarray::column::index $_item_ids $rindex]
        }
        return 0
    }
    
    method _first_display_item {} {
        # Note item id 0 is that of the implicit root item
        if {[scan [$_treectrl bbox content] "%d %d" x y] == 2} {
            set id [$_treectrl item id [list nearest $x $y]]
            if {$id ne ""} {
                return $id
            }
        }
        # Empty -> No items in table
        return 0
    }
        
    ###
    # Selection handling
    
    method _selecthandler {removedselections newselections} {
        # Set the state for each column for the selected items to show
        # selection highlighting. Note we do not bother with the 
        # removedselections items. They can keep their state settings
        # since those anyways are unaffected if unselected.
        if {[llength $newselections]} {
            foreach colname $_column_order {
                set col_id [dict get $_columns $colname Id]
                $_treectrl item state forcolumn [list "list" $newselections] $col_id [dict get $_columns $colname OutlineState]
            }
        }

        # Call the selection callback
        if {$options(-selectcommand) ne ""} {
            # Schedule it for later else double-clicks get lost because
            # if the callback takes longer to run than the double-click
            # time, it does not get treated as a double click
            after 200 [linsert $options(-selectcommand) end $self]
        }
    }

    method _getselecteditems {} {
        # Returns the list of currently selected item ids in the order they
        # are displayed

        # Neither [selection get], not [item id "state selected"]
        # return items in displayed order. So we have to sort that out
        # ourselves.
        set items {}
        foreach item [$_treectrl selection get] {
            lappend items [$_treectrl item order $item] $item
        }

        set item_ids {}
        foreach {pos item} [lsort -integer -stride 2 $items] {
            lappend item_ids $item
        }
        return $item_ids

    }

    method getselected {} {
        # Returns the list of currently selected row ids in the order they
        # are displayed

        # Neither [selection get], not [item id "state selected"]
        # return items in displayed order. So we have to sort that out
        # ourselves.
        set row_ids {}
        foreach item [$self _getselecteditems] {
            lappend row_ids [$self _item_to_row_id $item]
        }
        return $row_ids
    }

    method showtop {} {
        set first [$_treectrl item id "first visible"]
        # TBD - when no columns are in the table, above command still
        # returns 1 (?). The see command below then crashes wish.
        # This is currently protected by forcing user to make
        # at least one column visible in the table editor.
        if {$first ne ""} {
            $_treectrl see $first
        }
    }

    method _sort {cname order} {
        if {$_sort_column eq $cname && $_sort_order eq $order} {
            return
        }
        event generate $win <<SortColumn>> -data [list $cname $order]
        return
    }

    # Set the mode for showing all rows or changes only
    method _setshowchangesonly {opt val} {
        if {$options($opt) == $val} {
            # No change
            return
        }
        set options($opt) $val
        if {$val} {
            # Only show changes. So if any item is not in one of the
            # states indicating change, make it invisible
            # Note this is {root children}, not "all" else root
            # and everything else becomes invisible

            # Check if any items else _treectrl throws error
            if {[$_treectrl item count {root children state {!modified !new !deleted}}] > 0} {
                $_treectrl item configure {root children state {!modified !new !deleted}} -visible 0
            }
        } else {
            $_treectrl item configure all -visible 1
        }
    }

    ###
    # Filtering code
    method _populatefilter {} {
        $_treectrl header style set H2 all h2Style
        dict for {name colmeta} $_columns {
            if {[dict exists $options(-filtervalues) $name]} {
                $_treectrl header text H2 [dict get $colmeta Id] [dict get $options(-filtervalues) $name]
            } else { 
                $_treectrl header text H2 [dict get $colmeta Id] $options(-undefinedfiltertext)
            }
        }
        $_treectrl header configure H2 -visible $options(-showfilter)
    }

    method _setfiltervalues {opt val} {
        dict size $val;         # Verify valid dictionary
        set options($opt) $val
        if {$_constructed} {
            $self _populatefilter
        }
    }

    method _setshowfilter {opt val} {
        $_treectrl header configure H2 -visible $val
        set options($opt) $val
    }

    method _getfilterbbox {colname} {
        set bbox [$_treectrl header bbox H2 [dict get $_columns $colname Id]]
    }

    method _editfilter {colname} {
        set _filter_column_being_edited $colname
        lassign [$self _getfilterbbox $colname] left top right bottom

        set e $win.fedit
        if {![winfo exists $e]} {
            ttk::entry $e -font [$self cget -font] -text abc
            bind $e <Return> [mymethod _closeeditfilter %W save]
            bind $e <Tab> [mymethod _closeeditfilter %W saveandnext]
            bind $e <Shift-Tab> [mymethod _closeeditfilter %W saveandprev]
            bind $e <FocusOut> [mymethod _closeeditfilter %W save]
            bind $e <Escape> [mymethod _closeeditfilter %W discard]
            # TBD bind $e <KeyRelease-F1> [myproc _balloonpopup $e true]
        }
        place $e -x $left -y $top -width [expr {$right-$left}] -height [expr {$bottom-$top}]
        $e delete 0 end
        if {0 && [dict exists $options(-filter) properties $_filter_column_being_edited condition]} {
            $e insert 0 [dict get $options(-filter) properties $_filter_column_being_edited condition]
        }
        focus $e
        # TBD after 0 [myproc _balloonpopup $e]
    }

    method _closeeditfilter {entry action} {
        if {$_filter_column_being_edited eq "" || ![winfo exists $entry]} {
            return
        }

        if {[focus] eq ""} {
            return
        }

        # TBD _balloonburst

        set filter_col $_filter_column_being_edited
        set _filter_column_being_edited ""
        place forget $entry

        if {0 && $action in {save saveandnext saveandprev}} {
            set newcondition [string trim [$entry get]]
            set old_filter $options(-filter)
            set new_filter $old_filter
            if {$newcondition eq ""} {
                dict unset new_filter properties $filter_col
            } else {
                dict set new_filter properties $filter_col condition $newcondition
            }
            if {[catch {
                $self _setfilter -filter $new_filter
            } msg]} {
                # Restore old filter on error
                if {[catch {
                    $self _setfilter -filter $old_filter
                } msg2]} {
                    $self _setfilter -filter {}
                    after 0 [list [namespace which showerrordialog] "Error in filter definition ($msg2). Filter cleared."]
                } else {
                    after 0 [list [namespace which showerrordialog] "Error in filter definition ($msg). Original filter restored."]
                }
            } else {
                if {$action in {saveandnext saveandprev}} {
                    set colnum [lsearch -exact $options(-displaycolumns) $filter_col]
                    if {$action eq "saveandnext"} {
                        if {[incr colnum] >= [llength $options(-displaycolumns)]} {
                            set colnum 0
                        }
                    } else {
                        if {[incr colnum -1] < 0} {
                            set colnum [llength $options(-displaycolumns)]
                            incr colnum -1
                        }
                    }
                    after 0 [list $self _editfilter [lindex $options(-displaycolumns) $colnum]]
                }
            }
        }
        return
    }
    
    method _column_move_handler {col_id target_id} {

        $_treectrl column move $col_id $target_id
        set _column_order [lmap col_id [$_treectrl column list] {
            lindex $_column_order $col_id
        }]
        return
    }

}


oo::class create tarray::ui::Table {
    # Variables used in class
    variable _data;        #  Table containing data
    variable _w;        #  Widget displaying data
    variable _coldefs;  #  Column definitions
    variable _row_ids;  #  Index column containing row_ids to display
    
    variable _sort_column    # Id of column used for sorting
    variable _sort_order     # -increasing or -decreasing

    variable _filters         # Dict mapping columns to filters
    variable _filter_strings # Dict mapping columns to filter display strings

    constructor {tab w args} {
        if {[dict exists $args -coldefs]} {
            set _coldefs [dict get $args -coldefs]
            dict unset args -coldefs
        } else {
            set _coldefs [list]
            foreach cname [tarray::table::cnames $tab] col [tarray::table::columns $tab] {
                lappend _coldefs $cname [list Type [tarray::column::type $col]]
            }
        }
        if {[dict size $args]} {
            error "Unknown options [join [dict keys $args] {, }]"
        }
        set _data $tab
        set _row_ids [tarray::indexcolumn [tarray::table::size $tab]]
        
        set _sort_column ""
        set _sort_order "-increasing"

        set _filters [dict create]
        set _filter_strings [dict create]

        set _w $w
        tarray::ui::tableview $w [self] $_coldefs {*}$args
        bind $w <<SortColumn>> [list [self] sort %d]
        bind $w <Destroy> [list [self] destroy]
        my update_display 
    }

    destructor {
        if {[info exists _w]} {
            catch {destroy $_w}
        }
        return
    }
    
    method sort {cname_and_order} {
        lassign $cname_and_order cname order
        if {$cname eq "" ||
            ($cname eq $_sort_column && $order eq $_sort_order)} {
            return;             # Already in requested order
        }

        set _sort_column $cname
        set _sort_order $order
        
        # We cannot use table::sort here because
        # we only want to (potentially) sort a subset of the table since
        # displayed rows may not be the full table if filtering is
        # in effect and table::sort does not have support for indirect
        # sorting. So we extract the column and sort on that instead.
        set col [tarray::table::column $_data $cname]
        set _row_ids [tarray::column::sort $order -nocase -indirect $col $_row_ids]
        my update_display
        return
    }

    method widget {} { return $_w }

    method update_display {} {
        $_w setrows $_row_ids -sortcolumn $_sort_column -sortorder $_sort_order -filters $_filter_strings
    }

    method get {row_id cnames} {
        return [lindex [tarray::table::get -list -columns $cnames $_data $row_id] 0]
    }
}

proc test {{nrows 20}} {
    set coldefs {
        ColA {
            Label {Column A}
        }
        ColB {
            Label {Column B}
            Type int
        }
        ColC {
            Justify right
            Sortable 0
        }
    }
    set ::datatable [tarray::table create {
        ColA string ColB int ColC any
    }]
    set n -1
    time {
        set now [clock seconds]
        tarray::table vinsert ::datatable [list Row[incr n] $now [clock format $now -format %M:%S]] end
    } $nrows
    # TBD - make note of -yscrolldelay option for scrolling large tables
    tarray::ui::Table create tv $::datatable .tv -coldefs $coldefs
    pack .tv -fill both -expand 1
}

proc test2 {{nrows 20}} {
    proc datasource {cmd args} {
        switch -exact -- $cmd {
            get {
                lassign $args index cols
                return [lindex [tarray::table get -list -columns $cols $::datatable $index] 0]
            }
            rowids {
                if {[llength $args]} {
                    lassign $args col order
                    return [tarray::table::sort -indices $order $::datatable $col]
                } else {
                    return [tarray::indexcolumn [tarray::table size $::datatable]]
                }
            }
            columns {
                return {
                    ColA {
                        Label {Column A}
                    }
                    ColB {
                        Label {Column B}
                        Type int
                    }
                    ColC {
                        Justify right
                        Sortable 0
                    }
                }
            }
            default { error "Unknown command $cmd" }
        }
    }
    set ::datatable [tarray::table create {
        ColA string ColB int ColC any
    }]
    set n -1
    time {
        set now [clock seconds]
        tarray::table vinsert ::datatable [list Row[incr n] $now [clock format $now -format %M:%S]] end
    } $nrows
    # TBD - make note of -yscrolldelay option for scrolling large tables
    tarray::ui::tableview .tv datasource -showfilter 1 -yscrolldelay 500
    pack .tv -fill both -expand 1
}
