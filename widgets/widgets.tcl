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

# Provide a unmanaged toplevel with basic functionality like a close button
# and ability to drag using the title bar.
snit::widget tarray::ui::unmanaged {
    hulltype toplevel

    ### Option definitions

    option -title -default ""
    option -closehandler -default ""
    
    delegate option * to hull

    ### Components

    component _titlef;          # Title frame

    component _clientf;         # Client frame

    ### Variables
    
    # Position of mouse within window while it's being dragged
    variable _drag_pointer_offset_x
    variable _drag_pointer_offset_y
    
    constructor {args} {
        $hull configure -highlightthickness 1 -highlightcolor grey -highlightbackground lightgrey
        
        $self configurelist $args

        install _titlef using ttk::frame $win.f-title
        install _clientf using ttk::frame $win.f-client

        label $_titlef.l-title -textvariable [myvar options(-title)] -font TkSmallCaptionFont -anchor c
        # Bind to label for dragging window since window manager will not do
        # it for us.
        bind $_titlef.l-title <Enter> "$win configure -cursor size"
        bind $_titlef.l-title <Leave> "$win configure -cursor {}"
        bind $_titlef.l-title <Button-1> [mymethod _startdrag %W %x %y]
        bind $_titlef.l-title <Button1-Motion> [mymethod _drag %W %X %Y]
        
        label $_titlef.l-close -text X -bg [tarray::ui::color::shade [$_titlef.l-title cget -bg] black 0.2] -padx 5
        bind $_titlef.l-close <Button-1> "destroy $win"

        ttk::separator $_clientf.sep

        pack $_titlef.l-close -side right -fill none -expand 0
        pack $_titlef.l-title -side left -fill both -expand 1
         
        pack $_clientf.sep -side top -fill both -expand 1

        pack $_titlef -fill x -expand 0 -padx 5
        pack $_clientf -fill both -expand 1 -padx 5

        wm overrideredirect $win 1
    }

    method getframe {} {
        return $_clientf
    }

    #
    # Called when mouse is clicked in title bar to start dragging
    method _startdrag {w x y} {
        if {$w ne "$_titlef.l-title"} return
        set _drag_pointer_offset_x $x
        set _drag_pointer_offset_y $y
    }

    #
    # Called when mouse is dragged in title bar to move window
    method _drag {w screenx screeny} {
        if {$w ne "$_titlef.l-title"} return
        wm geometry $win +[expr {$screenx - $_drag_pointer_offset_x}]+[expr {$screeny - $_drag_pointer_offset_y}]
    }

}
                
snit::widget tarray::ui::dataview {
    hulltype ttk::frame
    
    ### Type constructor
    typevariable _select_foreground
    typevariable _select_background
    typeconstructor {
        setup_nspath
        set l .selcolorpicker
        listbox $l
        set _select_foreground [$l cget -selectforeground]
        set _select_background [$l cget -selectbackground]
        destroy $l
    }

    option -undefinedfiltertext -default "<Filter>"

    option -showfilter -default 0 -configuremethod SetShowFilter

    option -defaultsortorder -default "-increasing"

    # TBD - no need to be readonly except that we have not written the
    # code to update the display on the fly when the option is changed
    option -formatter -default "" -readonly 1
    option -visuals -default "" -readonly 1
    
    component _treectrl
    
    #delegate method * to _treectrl
    #delegate option * to _treectrl
    delegate option -yscrolldelay to _treectrl
    delegate option -xscrolldelay to _treectrl
    
    variable _datasource
    
    variable _constructed 0

    variable _columns {}
    variable _column_order {}

    variable _sort_column ""
    variable _sort_order "" ;# -increasing or -decreasing

    variable _item_style_phrase {}
    variable _visuals_reset_phrase {}; # Used to reset all user defined visual states

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

    # Dictionary containing display strings for filters
    variable _filters {}
    
    # Contains the name of the filter column being edited, if any
    variable _filter_column_being_edited
    
    constructor {datasource coldefs args} {
        $hull configure -borderwidth 0
        
        set _datasource $datasource
        
        $self _parse_visuals -visuals [from args -visuals ""]

        # TBD - do we need all these scroll options? Could they impact
        # shimmering
        install _treectrl using treectrl $win.tbl \
            -highlightthickness 1 \
            -borderwidth 0 \
            -showroot no -showbuttons no -showlines no \
            -selectmode extended -xscrollincrement 20 -xscrollsmoothing 1 \
            -canvaspadx {2 0} -canvaspady {2 0} \
            -scrollmargin 16 -xscrolldelay "500 50" -yscrolldelay "500 50"
        
        # item and column identify where the mouse is hovering
        # -1 indicates invalid (ie mouse is outside an item)
        array set _tooltip_state {item -1 column -1 schedule_id -1}

        $_treectrl header create -tags H2

        $_treectrl notify bind $_treectrl <ItemVisibility> [mymethod <ItemVisibility> %h %v]
        $_treectrl notify bind $_treectrl <Selection> [mymethod <Selection> %D %S ]
        bind $_treectrl <Motion> [mymethod <Motion> %x %y]

        # Define the filter header row
        $_treectrl element create h2Elem text -lines 1 -justify left  -statedomain header -fill blue
        $_treectrl style create h2Style -orient horizontal -statedomain header
        $_treectrl style elements h2Style {h2Elem}
        $_treectrl style layout h2Style h2Elem -squeeze x -expand ns -padx 5

        ttk::scrollbar $win.vscroll \
            -orient vertical \
            -command "$_treectrl yview" 
	$_treectrl notify bind $win.vscroll <Scroll-y> [mymethod PositionVerticalScrollbar %W %l %u]
	bind $win.vscroll <ButtonPress-1> "focus $_treectrl"
        ttk::scrollbar $win.hscroll \
            -orient horizontal \
            -command "$_treectrl xview" 
	$_treectrl notify bind $win.hscroll <Scroll-x> [mymethod PositionHorizontalScrollbar %W %l %u]
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

        # Bind common keys 
        bind $_treectrl <<SelectAll>> [list %W selection add all]
        bind $_treectrl <<Copy>> [list event generate $win <<Copy>>]
        bind $_treectrl <<Cut>> [list event generate $win <<Cut>>]
        bind $_treectrl <<SelectNone>> [list %W selection clear]
        bind $_treectrl <Escape> [list %W selection clear]

        # Standard mouse bindings
        bind $_treectrl <Double-1> [mymethod ProxyMouseClicks <<ItemDoubleClick>> %x %y]
        bind $_treectrl <ButtonPress-3> [mymethod ProxyMouseClicks <<ItemRightClick>> %x %y]
        
        # Create the gradient element used for coloring
        $_treectrl gradient create gradientSelected \
            -stops [list [list 0.0 $_select_background 0.5] [list 1.0 $_select_background 0.0]] \
            -orient vertical

        # Define states used to control selection highlighting - which
        # cell borders are merged with next cell
        $_treectrl state define openW
        $_treectrl state define openE
        $_treectrl state define openWE
        
        # Do we show plain selection highlighting or a gradient-based
        # fancy one ? Right now use plain version as the gradient one
        # does not look that great with multiple consecutive selections.
        set gradient_select 0
        #set sel_color [color::shade $_select_background white 0.7]
        set sel_color $_select_background
        set sel_color_nofocus lightgray
        
        # Define the states used for controlling visuals and collect
        # the corresponding attributes for assigning to treectrl elements
        set font_visuals {}
        set fg_visuals {}
        if {$gradient_select} {
            set bg_visuals [list gradientSelected selected]
        } else {
            set bg_visuals [list $sel_color {selected focus}]
        }
        dict for {name attrs} $options(-visuals) {
            $_treectrl state define $name
            if {[dict exists $attrs Background]} {
                lappend bg_visuals [dict get $attrs Background] $name
            }
            if {[dict exists $attrs Foreground]} {
                lappend fg_visuals [dict get $attrs Foreground] $name
            }
            if {[dict exists $attrs Font]} {
                lappend font_visuals [dict get $attrs Font] $name
            }
        }
        if {! $gradient_select} {
            # Background for selected items when widget does not have focus
            lappend bg_visuals $sel_color_nofocus {selected !focus}
        }

        if {$gradient_select} {
            # The visuals corresponding to the built-in "selected" state
            # always appear first as they override anything else.
            $_treectrl element create bgElem rect \
                -fill $bg_visuals \
                -outline [list $sel_color selected] -rx 1 \
                -open [list we openWE w openW e openE] \
                -outlinewidth 1
        } else {
            # Setting outlinewidth to 1 will show an outline around
            # selected items like Win8 but when consecutive items are
            # selected, the line between them is 2px wide unlike Win8
            # TBD - make this an option?
            set outlinewidth 0
            if {$outlinewidth} {
                $_treectrl element create bgElem rect \
                    -fill $bg_visuals \
                    -open [list we openWE w openW e openE] \
                    -outline [list $_select_background selected]  -rx 0 \
                    -outlinewidth $outlinewidth
            } else {
                # Note - -outlinewidth has to be 1 here else only one
                # item is displayed. Not sure why
                $_treectrl element create bgElem rect \
                    -fill $bg_visuals \
                    -open [list we openWE w openW e openE] \
                    -outline ""  -rx 0 \
                    -outlinewidth 1
            }
        }
        
        # Create the elements for actual text
        $_treectrl element create leftJustifyElem text -lines 1 -justify left -fill $fg_visuals -font $font_visuals
        $_treectrl element create rightJustifyElem  text -lines 1 -justify right -fill $fg_visuals -font $font_visuals
        
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
        $_treectrl notify bind MyHeaderTag <Header-invoke> [mymethod <Header-invoke> %H %C]

        $_treectrl notify install <ColumnDrag-begin>
        $_treectrl notify install <ColumnDrag-end>
        $_treectrl notify install <ColumnDrag-indicator>
        $_treectrl notify install <ColumnDrag-receive>

        $_treectrl notify bind MyHeaderTag <ColumnDrag-receive> [mymethod <ColumnDrag-receive> %C %b]

        $_treectrl header dragconfigure -enable yes
        $_treectrl header dragconfigure all -enable yes -draw yes

        $self definecolumns $coldefs
        set _constructed 1
        after 0 "focus $_treectrl"
    }

    destructor {
    }

    method _parse_visuals {opt optval} {
        set visuals_reset_phrase {}
        set visuals {}
        dict for {name attrs} $optval {
            if {![regexp {^visual[1-7]$} $name]} {
                error "Unknown visual name \"$name\"."
            }
            lappend visuals_reset_phrase "!$name"
            dict for {attr val} $attrs {
                switch -exact -- $attr {
                    "-fg" - "-foreground" {
                        dict set visuals $name Foreground $val
                    }
                    "-bg" - "-background" {
                        dict set visuals $name Background $val
                    }
                    "-font" { dict set visuals $name Font $val }
                    default {
                        error "Unknown visual attribute \"$attr\"."
                    }
                }
            }
        }
        set options(-visuals) $visuals
        set _visuals_reset_phrase [join $visuals_reset_phrase { }]
        return
    }

    method PositionHorizontalScrollbar {sb first last} {
        if {$first <= 0 && $last >= 1} {
            grid remove $sb
        } else {
            grid $sb
        }
        $sb set $first $last
        return
    }
    
    method PositionVerticalScrollbar {sb first last} {
        if {0} {
            We cannot use the PositionHorizontalScrollbar method because
            gets infinite loop due to infighting between horizontal and
            vertical scrollbars
            if {$first <= 0 && $last >= 1} {
                grid remove $sb
            } else {
                grid $sb
            }
        }

        if {0} {
            This does not work because the last item may not be fully
            visible but scroll bars do not show up
            lassign [$self DisplayItemBounds] top_item bot_item
            if {$top_item == 0 ||
                ($top_item == [tarray::column::index $_item_ids 0] &&
                 $bot_item == [tarray::column::index $_item_ids end])} {
                # Either table empty/too small display area, or
                # both first and last items are displayed in content area
                grid remove $sb
            } else {
                grid $sb
            }
        }

        # Note: The shimmering issue depends on content, font, and window
        # dimensions. When tested with the test proc, setting column filter
        # B to >60 followed by Col A filter to ~1 often (not always) produced
        # the effect. Also Setting B to > 100
        
        # Compare bottom of last item with content area bounds.
        # If not visible or greater than content area, show the scroll bar
        # We have to account for potential width of scrollbars to prevent
        # shimmering
        if {[tarray::column::size $_item_ids] == 0} {
            grid remove $sb
        } else {
            # Bounding boxes are {left top right bottom}
            set bbox [$_treectrl bbox content]
            set content_top [lindex $bbox 1]
            set content_bottom [lindex $bbox 3]
            set first_bbox [$_treectrl item bbox [tarray::column::index $_item_ids 0]]
            set first_top [lindex $first_bbox 1]
            set last_bbox [$_treectrl item bbox [tarray::column::index $_item_ids end]]
            set last_bottom [lindex $last_bbox 3]
            # To prevent infinite shimmering between horizontal and vertical
            # scrollbars, take into consideration height of the (potential)
            # horizontal scrollbar. This means scrollbar will be present
            # unnecessarily in some cases but no other workaround
            set scroll_size [winfo height $win.hscroll]

            # treectrl docs say bbox may be empty for some reason
            # so to be safe check for that and show scrollbars in that
            # case as well
            if {$first_top eq "" ||
                $last_bottom eq "" ||
                $first_top < $content_top ||
                ($last_bottom+$scroll_size) > $content_bottom} {
                # Last item not visible or partially visible
                grid $sb
            } else {
                grid remove $sb
            }
        }
        
        $sb set $first $last
        return
    }

    method gettreectrlpath {} {
        return $_treectrl
    }

    ###
    # Handling of mouse clicks within table
    method ProxyMouseClicks {event winx winy} {
         lassign [$_treectrl identify -array state $winx $winy] type item_id col_id
        if {$state(where) eq "item"} {
            event generate $win $event \
                -data [list Row [
                             $self ItemToRowId $state(item)
                            ] Column [
                             $self column_id_to_name $state(column)
                            ]]
        }
        return
    }

    ###
    # Header event handlers
    method <Header-invoke> {hdr_id col_id} {
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
            $self Sort $colname $order
        } elseif {$hdr_id == 1} {
            $self OpenEditFilter [$self column_id_to_name $col_id]
        }
        return
    }

    method UpdateSortIndicators {cname order} {
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

    ###
    # Tooltip handling
    method CancelTooltip {} {
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

    method ScheduleTooltip {item column winx winy} {
        $self CancelTooltip;  # Cancel pending tooltip if any
        set _tooltip_state(item) $item
        set _tooltip_state(column) $column
        set _tooltip_state(winx) $winx
        set _tooltip_state(winy) $winy
        set _tooltip_state(schedule_id) [after 100 [mymethod ShowTooltip]]
    }

    method ShowTooltip {} {
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
            #bind $win.tooltip <Enter> [mymethod CancelTooltip]
            bind $win.tooltip <Enter> [mymethod ProxyMouse Enter "" %X %Y]

            # Bind mouse clicks so they get passed on to parent frame
            foreach event {
                Button
                Shift-Button
                Control-Button
                Double-Button
            } {
                bind $win.tooltip <$event> [mymethod ProxyMouse $event %b %X %Y]
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

    method ProxyMouse {event button screenx screeny} {

        if {$_tooltip_state(item) == -1} {
            return;             # Cannot happen, can it ?
        }

        set item $_tooltip_state(item); # Save before cancel
        set col  $_tooltip_state(column); # Save before cancel
        set winx  $_tooltip_state(winx); # Save before cancel
        set winy  $_tooltip_state(winy); # Save before cancel

        $self CancelTooltip
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
                # puts "$event-$button"
            }
        }
    }

    method <Motion> {x y} {
        $_treectrl identify -array pos $x $y
        if {$pos(where) ne "item" || $pos(column) eq ""} {
            # Mouse moved out of an item - cancel tooltip state
            $self CancelTooltip
            return
        }

        # If the cell has changed, then cancel and requeue
        # the request
        if {$pos(item) != $_tooltip_state(item) ||
            $pos(column) != $_tooltip_state(column)} {
            $self ScheduleTooltip $pos(item) $pos(column) $x $y
            return
        }

        # If cell still same, nothing to do
        return
    }

    method <Leave> {x y} {
        # We used to bind the treectrl to <Leave> so the tooltip could
        # be removed. However, this had the problem that displaying the
        # tooltip would also generate a <Leave> causing the handler
        # to immediately cancel it. So we now bind to the tooltip
        # <Leave> handler to withdraw the tooltip.
        #        $self CancelTooltip
    }

    ###
    # Column handling
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
        set sort_col_name $_sort_column

        set _columns {}
        dict for {colname coldef} $coldefs {
            set def [dict merge {
                Type string
                Sortable 1
                Squeeze 0
            } $coldef]
            if {![dict exists $def Heading]} {
                dict set def Heading $colname
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
                            -text [dict get $coldef Heading] \
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
        $self UpdateFilterIndicators $_filters
    }

    method getcolumnorder {} {
        return $_column_order
    }
    
    ###
    # Methods related to data
    
    method setrows {row_ids args} {
        set sort_column [from args -sortcolumn ""]
        set sort_order [from args -sortorder -increasing]
        set filters    [from args -filters ""]
        if {[llength $args]} {
            error "Invalid syntax. Must be \"$win setrows ROW_IDS ?-sortcolumn COLNAME? ?-sortorder -increasing|-decreasing? ?-filters FILTERDICT?\""
        }

        $self SaveDisplayState; # Want to preserve selections etc.

        #_row_ids may be a column or a list
        set _row_ids [tarray::column::create int $row_ids]
        set count [tarray::column size $_row_ids]
        $_treectrl item delete all
        if {$count == 0} {
            set _item_ids [tarray::column::create int]
        } else {
            #TBD may be optimize by using existing item ids ?
            set _item_ids [tarray::column create int [$_treectrl item create -parent root -open no -count $count]]
            
            if {[dict size $_columns]} {
                set col [dict get $_columns [lindex $_column_order 0] Id]
                for {set i 0} {$i < $count} {incr i} {
                    # We will initialize styles and contents of a row only
                    # when they are actually displayed. This is to save
                    # memory with large tables. However, tktreectrl does not
                    # seem to call us back when an item is displayed if none
                    # of the items have their styles set.
                    $_treectrl item style set [tarray::column index $_item_ids $i] $col [dict get $_item_style_phrase $col]
                }
            }
        }

        $self UpdateSortIndicators $sort_column $sort_order
        $self UpdateFilterIndicators $filters
        
        $self RestoreDisplayState
        return
    }

    method GetData {row_id} {
        return [{*}$_datasource get $row_id $_column_order]
    }
    
    method InitRow {item {row {}}} {
        # TBD - does this have to be done every time or can we
        # keep track of which items have been init'ed and not un-inited?
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

    method SetItemVisuals {item visuals} {
        if {[dict exists $visuals ""]} {
            set visual [dict get $visuals ""]
            $_treectrl item state set $item $_visuals_reset_phrase
            if {$visual ne ""} {
                $_treectrl item state set $item $visual
            }
        }
        foreach cname $_column_order {
            if {[dict exists $visuals $cname]} {
                set visual [dict get $visuals $cname]
                set cid [$self column_name_to_id $cname]
                $_treectrl item state forcolumn $item $cid $_visuals_reset_phrase
                if {$visual ne ""} {
                    $_treectrl item state forcolumn $item $cid $visual
                }
            }
        }
    }

    method _NOT_IMPLEMENTED_YET_insert {row_ids {pos 0} {tags {}}} {
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
                # We will initialize styles and contents of a row only
                # when they are actually displayed. This is to save
                # memory with large tables. However, tktreectrl does not
                # seem to call us back when an item is displayed if none
                # of the items have their styles set.
                set col [dict get $_columns [lindex $_column_order 0] Id]
                $_treectrl item style set $item $col [dict get $_item_style_phrase $col]
            }
        }

        return 
    }

    method _NOT_IMPLEMENTED_YET_modify {first args} {
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

    method _NOT_IMPLEMENTED_YET_delete {row_ids} {
        set rindices {}
        foreach row_id $row_ids {
            set rindex [tarray::column search $_row_ids $row_id]
            if {$rindex >= 0} {
                lappend rindices $rindex
            }
        }
        return [$self delete_indices $rindices]
    }
    
    method _NOT_IMPLEMENTED_YET_delete_indices {first args} {
        # TBD - when items are deleted is the visibility handler called?
        # TBD - handling of selection - a selection event is generated
        #   need to handle that and remove from selection
        
        set first_item [$self FirstDisplayItem]
        if {$first_item > 0} {
            set first_rindex [$self ItemToRowIndex $first_item]
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

    ###
    # Tracking of items actually displayed

    method <ItemVisibility> {invisible visible} {
        if {[llength $invisible]} {
            #TBD - delete styles and text from elements ?
            $_treectrl item tag remove [list "list" $invisible] tv-displayed
        }
        
        if {[llength $visible]} {
            $_treectrl item tag add [list "list" $visible] tv-displayed
            foreach item $visible {
                lassign [$_treectrl item rnc $item] row_index col_index
                set row_id [tarray::column index $_row_ids $row_index]
                set row_data [$self GetData $row_id]
                # TBD - does doing a llength shimmer the bytecode into a list?
                if {[llength $options(-formatter)] == 0} {
                    $self InitRow $item $row_data 
                } else {
                    set keyed_data {}
                    foreach cname $_column_order val $row_data {
                        lappend keyed_data $cname $val
                    }
                    lassign [{*}$options(-formatter) $row_id $keyed_data] formatted_data visuals
                    set row_data [lmap cname $_column_order {
                        dict get $formatted_data $cname
                    }]
                    $self InitRow $item $row_data 
                    $self SetItemVisuals $item $visuals
                }
            }
        }
    }

    method DisplayedItems {} {
        return [$_treectrl item id {tag tv-displayed}]
    }
    
    # Item ids of the first and last items actually displayed
    method DisplayItemBounds {} {
        # Note item id 0 is that of the implicit root item so returning
        # 0 means empty table or too small to display any rows
        set top_id 0
        set bot_id 0
        if {[scan [$_treectrl bbox content] "%d %d %d %d" left top right bottom] == 4} {
            set top_id [$_treectrl item id [list nearest $left $top]]
            if {$top_id eq ""} {
                set top_id 0
            }
            set bot_id [$_treectrl item id [list nearest $right $bottom]]
            if {$bot_id eq ""} {
                set bot_id 0
            }
        }
        return [list $top_id $bot_id]
    }

    method FirstDisplayItem {} {
        return [lindex [$self DisplayItemBounds] 0]
    }

    ###
    # Save/restore display state after filtering/sorting etc.
    method SaveDisplayState {} {
        set _display_state [dict create]
        set item_id [$self FirstDisplayItem]
        if {$item_id > 0} {
            dict set _display_state display_top [$self ItemToRowId $item_id]
        }
        foreach item {active anchor} {
            set item_id [$_treectrl item id $item]
            if {$item_id > 0} {
                dict set _display_state $item [$self ItemToRowId $item_id]
            }
        }
        set selection [$self getselected]
        if {[llength $selection]} {
            dict set _display_state selection $selection
        }
    }
    
    method RestoreDisplayState {} {
        $_treectrl selection clear
        if {[dict exists $_display_state anchor]} {
            set item [$self RowIdToItem [dict get $_display_state anchor]]
            if {$item != 0} {
                $_treectrl selection anchor $item
            }
        }
        
        if {[dict exists $_display_state selection]} {
            set items [lmap rid [dict get $_display_state selection] {
                set item [$self RowIdToItem $rid]
                if {$item == 0} continue
                set item
            }]
            if {[llength $items]} {
                $_treectrl selection add [list "list" $items]
            }
        }
        
        if {[dict exists $_display_state active]} {
            set item [$self RowIdToItem [dict get $_display_state active]]
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
                    set rindex [$self RowIdToRowIndex [dict get $_display_state display_top]]
                    if {$rindex >= 0} {
                        $_treectrl yview moveto 0.0
                        $_treectrl yview scroll $rindex units
                    }
                } else {
                    # This does not work as well as above if the row
                    # happens to be already displayed in the middle
                    set item [$self RowIdToItem [dict get $_display_state display_top]]
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
    method ItemToRowIndex {item_id} {
        return [lindex [$_treectrl item rnc $item_id] 0]
    }
                
    method ItemToRowId {item_id} {
        return [tarray::column index $_row_ids [$self ItemToRowIndex $item_id]]
    }
    
    method RowIdToRowIndex {rid} {
        return [tarray::column::search $_row_ids $rid]
    }
    
    method RowIdToItem {rid} {
        # 0 -> not found
        set rindex [tarray::column::search $_row_ids $rid]
        if {$rindex >= 0} {
            return [tarray::column::index $_item_ids $rindex]
        }
        return 0
    }

        
    ###
    # Selection handling
    
    method <Selection> {removedselections newselections} {
        # Set the state for each column for the selected items to show
        # selection highlighting. Note we do not bother with the 
        # removedselections items. They can keep their state settings
        # since those anyways are unaffected if unselected.
        if {[llength $newselections]} {
            $self UpdateColumnOutlines $newselections
        }

        if {[llength $removedselections] || [llength $newselections]} {
            event generate $win <<ListboxSelect>> -data [list $removedselections $newselections]
        }
    }

    method GetSelectedItems {} {
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
        foreach item [$self GetSelectedItems] {
            lappend row_ids [$self ItemToRowId $item]
        }
        return $row_ids
    }

    method Sort {cname order} {
        if {$_sort_column eq $cname && $_sort_order eq $order} {
            return
        }
        event generate $win <<SortColumn>> -data [list $cname $order]
        return
    }

    ###
    # Filtering code
    method UpdateFilterIndicators {filters} {
        # TBD - does setting of style have to be done every time?
        $_treectrl header style set H2 all h2Style
        set _filters $filters
        
        foreach cname [dict keys $_columns] {
            $self UpdateColumnFilterIndicator $cname
        }
        
        # TBD - does this have to be done every time?
        $_treectrl header configure H2 -visible $options(-showfilter)
    }

    method UpdateColumnFilterIndicator cname {
        set cid [dict get $_columns $cname Id]
        if {[dict exists $_filters $cname]} {
            $_treectrl header text H2 $cid [dict get $_filters $cname]
        } else { 
            $_treectrl header text H2 $cid $options(-undefinedfiltertext)
        }
    }
    
    method SetShowFilter {opt val} {
        $_treectrl header configure H2 -visible $val
        set options($opt) $val
    }

    method GetFilterBbox {colname} {
        set bbox [$_treectrl header bbox H2 [dict get $_columns $colname Id]]
    }

    method OpenEditFilter {{colname {}}} {
        if {$colname eq ""} {
            set colname [lindex $_column_order 0]
        }
        
        set _filter_column_being_edited $colname
        lassign [$self GetFilterBbox $colname] left top right bottom

        set e $win.fedit
        set t $win.ftip
        if {![winfo exists $e]} {
            ttk::entry $e -font [$_treectrl cget -font] -text abc
            bind $e <Return> [mymethod CloseEditFilter %W save]
            bind $e <Tab> [mymethod CloseEditFilter %W saveandnext]
            bind $e <Shift-Tab> [mymethod CloseEditFilter %W saveandprev]
            bind $e <FocusOut> [mymethod CloseEditFilter %W save]
            bind $e <Escape> [mymethod CloseEditFilter %W discard]
            bind $e <Key> "+[list place forget $t]"
            label $t -text "Press F1 for filter help" -bg [$_treectrl cget -background] -borderwidth 1 -relief solid
        }
        # Bind here, not at create time because column name would change
        bind $e <F1> [list event generate $win <<FilterHelp>> -data [list entry $e column $colname]]
        place $e -x $left -y $top -width [expr {$right-$left}] -height [expr {$bottom-$top}]
        $e delete 0 end
        if {[dict exists $_filters $_filter_column_being_edited]} {
            $e insert 0 [dict get $_filters $_filter_column_being_edited]
        }
        focus $e
        place $t -x $left -y $bottom
        after 2000 [list place forget $t]
    }

    method CloseEditFilter {entry action} {
        if {$_filter_column_being_edited eq "" || ![winfo exists $entry]} {
            return
        }

        if {[focus] eq ""} {
            return
        }

        set filter_col $_filter_column_being_edited
        set _filter_column_being_edited ""
        place forget $entry
        place forget $win.ftip
        if {$action in {save saveandnext saveandprev}} {
            set newcondition [string trim [$entry get]]
            if {$newcondition eq ""} {
                dict unset _filters $filter_col
            } else {
                dict set _filters $filter_col $newcondition
            }
            $self UpdateColumnFilterIndicator $filter_col
            event generate $win <<FilterChange>> -data [list $filter_col $newcondition]
            # See if we need to tab into the next/prev filter
            # TBD - make sure the setrows does not cover the new edit box when
            # datasource call back
            if {$action in {saveandnext saveandprev}} {
                set colnum [lsearch -exact $_column_order $filter_col]
                if {$action eq "saveandnext"} {
                    if {[incr colnum] >= [llength $_column_order]} {
                        set colnum 0
                    }
                } else {
                    if {[incr colnum -1] < 0} {
                        set colnum [llength $_column_order]
                        incr colnum -1
                    }
                }
                after 0 [list $self OpenEditFilter [lindex $_column_order $colnum]]
            }
        } else {
            event generate $win <<FilterCancel>>
        }
            
        return
    }
    
    method UpdateColumnOutlines {item_ids} {
        if {[llength $item_ids] == 0} {
            return
        }
        set cols [$_treectrl column list]
        if {[llength $cols] > 1} {
            $_treectrl item state forcolumn [list "list" $item_ids] [lindex $cols 0] {openE !openW !openWE}
            $_treectrl item state forcolumn [list "list" $item_ids] [lindex $cols end] {!openE openW !openWE}
            foreach cid [lrange $cols 1 end-1] {
                $_treectrl item state forcolumn [list "list" $item_ids] $cid {!openE !openW openWE}
            }
        } elseif {[llength $cols] == 1} {
            $_treectrl item state forcolumn [list "list" $item_ids] [lindex $cols 0] {openE openW !openWE}
        }
    }
    
    method <ColumnDrag-receive> {col_id target_id} {

        $_treectrl column move $col_id $target_id
        set _column_order [lmap col_id [$_treectrl column list] {
            lindex $_column_order $col_id
        }]
        $self UpdateColumnOutlines [$_treectrl selection get]
        return
    }

}


oo::class create tarray::ui::Table {
    # Variables used in class
    variable _data;        #  Table containing data
    variable _w;        #  Widget displaying data
    variable _coldefs;  #  Column definitions
    variable _row_ids;  #  Index column containing row_ids to display
    
    variable _sort_column;    # Id of column used for sorting
    variable _sort_order;     # -increasing or -decreasing

    variable _filters;         # Dict mapping columns to filters
    variable _filter_strings;  # Dict mapping columns to filter display strings
    variable _filter_help_w; # Help widget name

    constructor {tab w args} {
        set options {}
        if {[dict exists $args -colattrs]} {
            set _coldefs [dict get $args -colattrs]
            dict unset args -colattrs
        } else {
            set _coldefs [list]
            foreach cname [tarray::table::cnames $tab] col [tarray::table::columns $tab] {
                lappend _coldefs $cname [list Type [tarray::column::type $col]]
            }
        }
        if {1} {
            set options $args;  # For now pass all unknown options to widget
        } else {
            if {[dict exists $args -showfilter]} {
                lappend options -showfilter [dict get $args -showfilter]
                dict unset args -showfilter
            }
            if {[dict size $args]} {
                error "Unknown options [join [dict keys $args] {, }]"
            }
        }
        
        set _data $tab
        set _row_ids [tarray::column::series [tarray::table::size $tab]]
        
        set _sort_column ""
        set _sort_order "-increasing"

        set _filters [dict create]
        set _filter_strings [dict create]
        set _filter_help_w ".tarrayfilterhelp"

        set _w $w
        tarray::ui::dataview $w [self] $_coldefs {*}$options
        bind $w <<SortColumn>> [list [self] <<SortColumn>> %d]
        bind $w <Destroy> [list [self] destroy]
        bind $w <<FilterHelp>> [list [self] <<FilterHelp>> %W %d]
        bind $w <<FilterChange>> [list [self] <<FilterChange>> %W %d]
        bind $w <<FilterCancel>> [list [self] <<FilterCancel>> %W]
        bind $w <<Copy>> [list [self] <<Copy>> %W]
        my UpdateData 
    }

    destructor {
        if {[info exists _w]} {
            catch {destroy $_w}
        }
        return
    }
    
    method <<Copy>> {w} {
        set sel [$_w getselected]
        set text {}
        if {[llength $sel]} {
            foreach row [tarray::table get -columns [$_w getcolumnorder] -list $_data $sel] {
                append text [join $row \t]\n
            }
            clipboard clear -displayof $w
            clipboard append -displayof $w $text
        }
        return
    }
    export <<Copy>>
    
    method <<SortColumn>> {cname_and_order} {
        lassign $cname_and_order cname order
        if {$cname eq "" ||
            ($cname eq $_sort_column && $order eq $_sort_order)} {
            return;             # Already in requested order
        }

        set _sort_column $cname
        set _sort_order $order
        
        my Sort
        my UpdateData
        return
    }
    export <<SortColumn>>

    method Sort {} {
        # We cannot use table::sort here because
        # we only want to (potentially) sort a subset of the table since
        # displayed rows may not be the full table if filtering is
        # in effect and table::sort does not have support for indirect
        # sorting. So we extract the column and sort on that instead.
        if {$_sort_column ne ""} {
            set col [tarray::table::column $_data $_sort_column]
            set _row_ids [tarray::column::sort $_sort_order -nocase -indirect $col $_row_ids]
        }
        return
    }

    method widget {} { return $_w }

    method ParseFilter {cname cond} {

        set cond [string trim $cond]
        if {$cond eq ""} {
            return {}
        }
        if {![regexp {^(==|!=|>|>=|<|<=|\*|!\*|~|!~|~\^|!~\^\s)\s*([^\s].*)$} $cond _ oper arg]} {
            # No operator specified, assume equality test
            set oper ==
            set arg $cond
            set cond "== $arg"
        }

        # Map operators to column search options
        set map {
            ==  {-eq}
            !=  {-not -eq}
            <   {-lt}
            <=  {-not -gt}
            >   {-gt}
            >=  {-not -lt}
            =^  {-nocase -eq}
            !^  {-nocase -not -eq}
            *  {-pat -nocase}
            !* {-not -pat -nocase}
            ~^  {-re}
            !~^  {-not -re}
            ~ {-nocase -re}
            !~ {-nocase -not -re}
        }
        
        return [list [dict get $map $oper] $arg $cond]
    }
        
    method update_column_filter {cname cond} {

        # Parse the string into an "executable" form
        lassign [my ParseFilter $cname $cond] oper arg cond
        
        # First, special cases:

        if {[dict exists $_filters $cname]} {
            # The filter already existed. If unchanged, nothing to do.
            if {[dict get $_filters $cname Oper] eq $oper &&
                [dict get $_filters $cname Arg] eq $arg} {
                return
            }
        } else {
            # This column was previously unfiltered.

            if {[llength $oper] == 0} {
                return;         # Still unfiltered. Naught to do
            }
            
            # We are now adding a filter no need to refilter all columns.
            # Start with what we already have and intersect with this filter
            set ids [tarray::column::search {*}$oper -all -among $_row_ids [tarray::table::column $_data $cname] $arg]
            # Reached here without errors so we can commit
            dict set _filters $cname [list Oper $oper Arg $arg]
            dict set _filter_strings $cname $cond
            set _row_ids $ids
            # Note no need to resort since we started with sorted _row_ids
            my UpdateData
            return
        }

        # In case of errors filters have to be preserved so make a copy
        set new_filters $_filters
        set new_filter_strings $_filter_strings
        if {[llength $oper] == 0} {
            dict unset new_filters $cname
            dict unset new_filter_strings $cname
        } else {
            dict set new_filters $cname [list Oper $oper Arg $arg]
            dict set new_filter_strings $cname $cond
        }

        if {[dict size $new_filters] == 0} {
            # No filters
            set _row_ids [tarray::column::series [tarray::table::size $_data]]
        } else {
            # General case. Have to rerun all filters. In search for efficiency
            # we will run all numeric filters first.
            foreach types {{byte int uint wide double} {string any boolean}} {
                dict for {cname filter} $new_filters {
                    set col [tarray::table::column $_data $cname]
                    if {[tarray::column::type $col] in $types} {
                        if {[info exists filtered_rids]} {
                            set filtered_rids [tarray::column::search {*}[dict get $new_filters $cname Oper] -all -among $filtered_rids [tarray::table::column $_data $cname] [dict get $new_filters $cname Arg]]
                        } else {
                            set filtered_rids [tarray::column::search {*}[dict get $new_filters $cname Oper] -all $col [dict get $new_filters $cname Arg]]
                        }
                        if {[info exists filtered_rids] && [llength $filtered_rids] == 0} {
                            break;      # No point looking further
                        }
                    }
                }
                if {[info exists filtered_rids] && [llength $filtered_rids] == 0} {
                    break;      # No point looking further
                }
            }
            if {[info exists filtered_rids]} {
                set _row_ids $filtered_rids
            } else {
                set _row_ids {}; # No matches
            }
        }

        set _filters $new_filters
        set _filter_strings $new_filter_strings
        my Sort;               # Need to resort after filtering
        my UpdateData
    }

    method CommitFilters {filters filter_strings rids} {
        set _filters $filters
        set _filter_strings $filter_strings
        set _row_ids $rids
        my UpdateData
        return
    }
    
    method <<FilterHelp>> {w info} {
        if {![winfo exists $_filter_help_w]} {
            tarray::ui::unmanaged $_filter_help_w -title "Filter Help"
            bind $_filter_help_w <Escape> [list destroy $_filter_help_w]
            set f [$_filter_help_w getframe]
            set l [ttk::label $f.l-msg -text "\n\
                    Filter Syntax: CONDITION VALUE\n\n\
                    Displays a row if the field value satisfies\n\
                    CONDITION. Hit Return/Enter/Tab for the filter to \n\
                    take effect. Hit Escape to cancel.\n\n\
                    CONDITION may be one of the following:\n\
                    ==\tequals VALUE\n\
                    !=\tdoes not equal VALUE\n\
                    >\tis greater than VALUE\n\
                    >=\tis greater than or equal to VALUE\n\
                    <\tis less than VALUE\n\
                    <=\tis less than or equal to VALUE\n\
                    *\tmatches VALUE pattern (case-insensitive)\n\
                    !*\tdoes not match VALUE pattern (case-insensitive)\n\
                    ~\tmatches VALUE regexp (case-insensitive)\n\
                    !~\tdoes not match VALUE regexp (case-insensitive)\n\
                    ~^\tmatches VALUE regexp (case-sensitive)\n\
                    !~^\tdoes not match VALUE regexp (case-sensitive)\n\n\
                    CONDITION defaults to == if unspecified.\n\
                    \n Examples:\n\
                    \tNew York (defaults to ==)\n\
                    \t> 100\n\
                    \t~ Mumbai|Bombay\n"]
            pack $l -expand 1 -fill both
        }

        after 0 [list after idle [list tarray::ui::place_window $_filter_help_w [dict get $info entry] {right left top bottom}]]
        raise $_filter_help_w
    }
    export <<FilterHelp>>

    method <<FilterCancel>> {w} {
        destroy $_filter_help_w
    }
    export <<FilterCancel>>
    
    method <<FilterChange>> {w cname_and_cond} {
        destroy $_filter_help_w
        if {[catch {
            lassign $cname_and_cond cname cond
            my update_column_filter $cname $cond
        } msg options]} {
            # Error - restore original data
            my UpdateData
            return -options $options $msg
        }
    }
    export <<FilterChange>>
    
    method UpdateData {} {
        $_w setrows $_row_ids -sortcolumn $_sort_column -sortorder $_sort_order -filters $_filter_strings
    }

    method get {row_id cnames} {
        return [lindex [tarray::table::get -list -columns $cnames $_data $row_id] 0]
    }
}

proc tarray::ui::tableview {w data args} {
    # TBD - make note of -yscrolldelay option for scrolling large tables
    Table new $data $w {*}$args
    return $w
}
    
#
# Places the given window at the center of the screen
# The tk::PlaceWindow does not seem to work correctly for unmanaged windows
# hence this.
proc tarray::ui::center_window {w} {
    regexp {^(\d+)x(\d+)\+(\d+)\+(\d+)$} [wm geometry $w] dontcare width height

    # With tk8.5/Tile0.8 some windows do not get redrawn properly without a
    # withdraw/deiconify - TBD
    wm withdraw $w

    set x [expr {([winfo screenwidth $w]-$width)/2}]
    set y [expr {([winfo screenheight $w]-$height)/2}]

    wm geometry $w +$x+$y
    wm deiconify $w
}

proc tarray::ui::place_window {w target {side center}} {
    update idletasks

    # TBD update idletasks needed ?
    set targetx [winfo rootx $target]
    set targety [winfo rooty $target]
    set targetwidth  [winfo width $target]
    set targetheight [winfo height $target]

    set screenx [winfo screenwidth $target]
    set screeny [winfo screenheight $target]

    set wwidth [winfo width $w]
    set wheight [winfo height $w]

    set wreqwidth [winfo reqwidth $w]
    set wreqheight [winfo reqheight $w]

    if {[llength $side] != 1} {
        lappend side center;    # If all else fails default to center
        # For each position, check if there is sufficient room on the
        # given side in order and pick the first one that has room
        foreach pos $side {
            switch -exact -- $pos {
                center - centre { break }
                top { if {$wheight < $targety} break }
                bottom {
                    if {($targety + $targetheight + $wheight) < $screeny} break
                }
                right {
                    if {($targetx + $targetwidth + $wwidth) < $screenx} break 
                }
                left { if {$wwidth < $targetx} break }
                default {
                    error "Bad position specifier \"$pos\"."
                }
            }
        }    
        set side $pos
    }

    switch -exact -- $side {
        centre -
        center { util::center_window $w $target }
        top {
            set x $targetx
            set y [expr {$targety - $wheight}]
            set adjust x
        }
        bottom {
            set x $targetx
            set y [expr {$targety + $targetheight}]
            set adjust x
        }
        left {
            set x [expr {$targetx - $wwidth}]
            set y $targety
            set adjust y
        }
        right {
            set x [expr {$targetx + $targetwidth}]
            set y $targety
            set adjust y
        }
    }

    if {$side ni {center centre}} {
        # Keep us within the screen, but only adjust in one axis
        # The routine only guarantees full visibility if a specific
        # $side is not specified.
        if {$adjust eq "x"} {
            if {$x < 0} {
                set x 0
            } elseif {($x + $wwidth) > $screenx} {
                set x [expr {$screenx - $wwidth}]
            }
        } else {
            if {$y < 0} {
                set y 0
            } elseif {($y + $wheight) > $screeny} {
                set y [expr {$screeny - $wheight}]
            }
        }
        wm geometry $w +$x+$y
    }

    wm deiconify $w
    raise $w
    return
}

proc tarray::ui::formatter {row_id data} {
    if {[dict exists $data ColB]} {
        set val [dict get $data ColB]
        if {(($val/10) % 4) == 0} {
            dict set data ColB [format 0x%x $val]
            dict set visuals ColB visual1
            dict set visuals "" visual2
        } else {
            # Have to rest highlights
            dict set visuals ColB ""
            dict set visuals "" ""
        }
    }
    return [list $data $visuals]
}

proc tarray::ui::test {args} {
    if {[llength $args] == 0} {
        set nrows 20
    } else {
        set args [lassign $args nrows]
    }
    set coldefs {
        ColA {
            Heading {Column A}
        }
        ColB {
            Heading {Column B}
            Type int
        }
        ColC {
            Justify right
            Sortable 0
        }
    }
    set ::datatable [tarray::table create {
        ColA string ColB int ColC any
    } {} $nrows]
    set n -1
    time {
        set now [clock clicks]
        tarray::table vinsert ::datatable [list Row[incr n] [expr {$n*10}] $now] end
    } $nrows
    #tarray::ui::tableview .tv $::datatable -colattrs $coldefs {*}$args
    tarray::ui::tableview .tv $::datatable  {*}$args
    pack .tv -fill both -expand 1
}
# test 20 -visuals {visual1 {-bg red -font Courier} visual2 {-fg green -bg blue}} -formatter formatter

proc tarray::ui::cities2 {args} {
    uplevel #0 source ../tests/cities.tcl
    #package require tclcsv
    tarray_cities2 ::cities ../tests/cities15000.txt
    toplevel .top
    wm title .top Cities
    tarray::ui::tableview .top.cities $::cities -showfilter 1 {*}$args
    pack .top.cities -fill both -expand 1
}

package provide tarray_ui 0.8
