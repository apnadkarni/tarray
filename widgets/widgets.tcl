package require Tk
package require snit
package require treectrl
package require tarray
source color.tcl;# TBD
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

    option -undefinedfiltertext -default "<Edit>"

    option -showfilter -default 1 -configuremethod _setshowfilter

    # TBD sort order 
    option -defaultsortorder -default "-increasing"

    component _treectrl
    delegate method * to _treectrl
    delegate option * to _treectrl

    variable _datasource
    
    variable _constructed 0

    variable _columns {}
    variable _column_order {}

    # TBD
    variable _sort_column -1
    variable _sort_order ""

    variable _item_style_phrase {}

    # Column of row id's where the row id identifies a row for the data source
    variable _row_ids
    # Column of tktreectrl item id's. One-to-one correspondence between
    # _row_ids and _item_ids, both indexed by physical (0-based) location
    # in table. TBD - do we really need _item_ids? We can get it from
    # $_treectrl item id [list rnc $row_index 0]
    variable _item_ids
    
    # Stores various state info related to tooltips shown when mouse
    # hovers over an item
    variable _tooltip_state

    constructor {datasource args} {
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

        # Client table row id's being displayed
        set _row_ids [tarray::column create int]
        set _item_ids [tarray::column create int]
        
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
        $_treectrl element create h2Elem text -lines 1 -justify left -font WitsFilterFont -statedomain header -fill blue
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
        $_treectrl element create textElem text -lines 1 -justify left
        $_treectrl element create numericElem  text -lines 1 -justify right

        
        # Create the corresponding styles 
        $_treectrl style create textStyle -orient horizontal
        $_treectrl style elements textStyle {bgElem textElem}
        $_treectrl style layout textStyle textElem -squeeze x -expand ns -padx 5
        $_treectrl style layout textStyle bgElem -detach yes -iexpand xy

        $_treectrl style create numericStyle -orient horizontal
        $_treectrl style elements numericStyle {bgElem numericElem}
        $_treectrl style layout numericStyle numericElem -squeeze x -expand ns -padx 5
        $_treectrl style layout numericStyle bgElem -detach yes -iexpand xy

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

        set _constructed 1
    }

    destructor {
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
            # Column header, sort accordingly
            if {$col_id == $_sort_column && $_sort_order eq "-increasing"} {
                set order -decreasing
            } else {
                set order -increasing
            }
            $self _sort $col_id $order
        } elseif {$hdr_id == 1} {
            # TBD - should this be just [event generate $win <<CheckWindow>> -when tail]
            event generate $win <<FilterSelect>> -data [$self column_id_to_name $col_id]
        }
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
        dict for {colname coldata} $_columns {
            if {[dict get $coldata id] == $col_id} {
                return $colname
            }
        }
        error "Column with id $col_id not found in _columns"
    }

    method column_name_to_id {colname} {
        return [dict get $_columns $colname id]
    }

    method definecolumns {cols} {

        # TBD - when no columns are in the table, 'item id {first visible}'
        # returns 1 (?). The 'see' command then crashes wish.
        # This is currently protected by forcing user to make
        # at least one column visible in the table editor.
        if {[llength $cols] == 0} {
            error "At least one column must be included in table."
        }
        # Note that to account for this being fixed in the future,
        # the code below dows not assume cols is non-empty

        # Note what column we had sorted on
        if {$_sort_column != -1} {
            set sort_col_name [$self column_id_to_name $_sort_column]
        }
        
        $_treectrl item delete all
        $_treectrl header delete all
        $_treectrl column delete all

        $_treectrl header create -tags H2

        set _item_style_phrase {}
        set _columns {}
        set col_pos 0
        foreach col $cols {
            lassign $col name label type attrs
            if {$type eq ""} {
                set type text
            }
            dict set _columns $name meta $col
            if {$type in {int long double number}} {
                set justify right
                set style numericStyle
            } else {
                set justify left
                set style textStyle
            }

            # TBD - should squeeze be 1 or 0 ? If 1, everything fits in 
            # window but has ellipses. If 0, columns scroll off the window.
            # Right now we rely on the caller to tell us whether the column
            # should be squeezable.
            # Note this also interacts with the column lock below. Locked
            # columns do not get squeezed ?
            if {[dict exists $attrs -squeeze]} {
                set squeeze [dict get $attrs -squeeze]
                set minwidth 80
            } else {
                set squeeze 0
                set minwidth 40
            }
            set col_id [$_treectrl column create -text $label -arrow none -squeeze $squeeze  -justify $justify -minwidth $minwidth]
            dict set _columns $name id $col_id
            dict set _columns $name outline_state {!openE !openW openWE}
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

        if {[llength $cols]} {
            # Set up the first and last columns to have "closed" outlines
            # for the selection rectangle.
            dict set _columns [lindex $cols {0 0}] outline_state {!openWE !openW openE}
            dict set _columns [lindex $cols {end 0}] outline_state {!openWE !openE openW}
        }

        # Build the secondary table header
        $self _populatefilter

        if {$_constructed && [dict size $_columns]} {
            # If the original sort column exists, resort using it
            if {[info exists sort_col_name] &&
                [dict exists $_columns $sort_col_name id]} {
                $self _sort [dict get $_columns $sort_col_name id] $_sort_order
            } else {
                $self _sort_on_first_visible_column
            }
        }
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
                set col [dict get $_columns [lindex [dict keys $_columns] 0] id]
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
        
        lassign [$self _first_display_item] first_item first_rindex
        
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
            if {$first_rindex ni $first} {
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
        return [{*}$_datasource $row_id $_column_order]
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
    
    ###
    # Map row ids/item ids/row indices
    method rowids {} {return [tarray::column range -list $_row_ids 0 end]}

    method _item_to_rindex {item_id} {
        return [lindex [$_treectrl item rnc $item_id] 0]
    }
                
    method _item_to_row_id {item_id} {
        return [tarray::column index $_row_ids [$self _item_to_rindex $item_id]]
    }
    
    method _first_display_item {} {
        scan [$_treectrl bbox content] "%d %d" x y
        set id [$_treectrl item id [list nearest $x $y]]
        return [list $id [lindex [$_treectrl item rnc $id] 0]]
    }
        
    ###
    # Selection handling
    
    method _selecthandler {removedselections newselections} {
        # Set the state for each column for the selected items to show
        # selection highlighting. Note we do not bother with the 
        # removedselections items. They can keep their state settings
        # since those anyways are unaffected if unselected.
        if {[llength $newselections]} {
            foreach colname [dict keys $_columns] {
                set col_id [dict get $_columns $colname id]
                $_treectrl item state forcolumn [list "list" $newselections] $col_id [dict get $_columns $colname outline_state]
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

    # Sorts in existing order
    method resort {} {
        if {[dict size $_columns] == 0} {
            return
        }
        if {$_sort_column == -1 || $_sort_order eq ""} {
            $self _sort [dict get $_columns [lindex [dict keys $_columns] 0] id]  $options(-defaultsortorder); # Will recurse
            return
        }

        $_treectrl item sort root $_sort_order -column $_sort_column -dictionary

        if {0} {
            Commented out because if widget is scrolled we do not want
            to move displayed viewport to the top or selection

            # Make sure selection, if any is still visible
            set selected [$_treectrl selection get]
            if {[llength $selected]} {
                $_treectrl see [lindex $selected 0]
            } else {
                
                # If list is not empty, show first entry. For some reason,
                # the treectrl shows the 3rd entry (scrolled) when first
                # displayed.
                set first [$_treectrl item id "first visible"]
                if {$first ne ""} {
                    $_treectrl see $first
                }
            }
        }
    }

    method _sort_on_first_visible_column {} {
        $self _sort [lindex [$_treectrl column id "first visible"]] $options(-defaultsortorder)
    }

    method _sort {col_id order} {
        if {$_sort_column != $col_id} {
            if {$_sort_column != -1} {
                # Reset the sort arrow on existing sort column if the column
                # is still visible
                set old [$_treectrl column id $_sort_column]
                if {[llength $old]} {
                    $_treectrl column configure $_sort_column -arrow none -itembackground {}
                }
            }
            # Make sure that all cells in the sort column are updated with
            # style and value
            foreach {item row} [array get _itemvalues] {
                $_treectrl item style set $item $col_id [dict get $_item_style_phrase $col_id]
                $_treectrl item text $item $col_id [lindex $row $col_id]
            }
        }

        if {$order eq "-increasing"} {
            set arrow up
        } else {
            set arrow down
        }

        $_treectrl column configure $col_id -arrow $arrow -itembackground [color::shade [$_treectrl cget -background] black 0.05]
        set _sort_column $col_id
        set _sort_order $order

        $self resort
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

    method _populatefilter {} {
        if {$_constructed} {
            $_treectrl header style set H2 all h2Style
            dict for {name colmeta} $_columns {
                if {[dict exists $options(-filtervalues) $name]} {
                    $_treectrl header text H2 [dict get $colmeta id] [dict get $options(-filtervalues) $name]
                } else { 
                    $_treectrl header text H2 [dict get $colmeta id] $options(-undefinedfiltertext)
                }
            }
        }
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

    method getfilterbbox {colname} {
        set bbox [$_treectrl header bbox H2 [dict get $_columns $colname id]]
    }

    method _column_move_handler {col_id target_id} {

        $_treectrl column move $col_id $target_id
        set _column_order [lmap col_id [$_treectrl column list] {
            lindex $_column_order $col_id
        }]
        return
        
        if {0} {
            # Work out the new order of column names

            # Remove the column being moved from the current list
            set order [lsearch -exact -inline -not -all [$_treectrl column list] $col_id]
            # Add it to the appropriate position
            if {$target_id eq "tail"} {
                set pos end
            } else {
                set pos [lsearch -exact $order $target_id]
            }

            set colnames {}
            foreach col_id [linsert $order $pos $col_id] {
                lappend colnames [$self column_id_to_name $col_id]
            }
        }

    }

}

proc test {} {
    proc datasource {index cols} {
        set now [clock seconds]
        dict set item ColA "Row$index"
        dict set item ColB $now
        dict set item ColC [clock format $now -format %M:%S]
        return [lmap col $cols {
            dict get $item $col
        }]
    }
    tarray::ui::tableview .tv datasource
    .tv definecolumns {
        {ColA {Column A} text {}}
        {ColB {Column B} int {}}
        {ColC {Column C} text {}}
    }
    set n -1
    time {.tv insert [incr n] $n} 20
    pack .tv
}
