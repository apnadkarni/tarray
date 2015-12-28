#
# Copyright (c) 2015, Ashok P. Nadkarni
# All rights reserved.
#
# See the file license.terms for license
#

# We do not want the end application to have the entire pt tools package
# available since at runtime we only need it to display errors. So
# we try to load it and if not available we will land up using the copied
# version below.

namespace eval xtal {}

if {![catch {
    package require pt::util
}]} {
    # pt::util is available. However...
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
    interp alias {} xtal::error2readable {} ::pt::util::error2readable
    return
}

# Rest of this file is a slightly modified (namespace changes and minor edits)
# form of pt_util.tcl from the pt package. Also the use of the char  package
# is replaced by the dumpstr routine from the wiki
#
# -*- tcl -*-
# Copyright (c) 2014 Andreas Kupries <andreas_kupries@sourceforge.net>

# Utility commands for parser syntax errors.

namespace eval xtal::pt::util {
    namespace export error2readable error2position error2text
    namespace ensemble create
}

# From http://wiki.tcl.tk/29098
#  dumpstr - dump str in the most readable way,
#  replacing non-printable characters with their hex-code written as "\xXX"
#-------------------------------------------------------------------------------
proc xtal::pt::util::dumpstr { str } {
    set result ""
    while { $str != "" } {
        if { [regexp -indices {^[-+*/%<=>.,:;|~^°`´!$&@(){}\[\]#'i\"A-Z_a-z0-9]+} $str igood] } {
            # readable characters, excluding backslash
            append result [string range $str 0 [lindex $igood 1]]
            set str [string range $str [lindex $igood 1]+1 end]
        }
        if { $str != "" } {
            set char [string index $str 0]
            set str  [string range $str 1 end]
            if { $char == "\\" } {
                append result {\\}
            } else {
                binary scan $char c hex
                append result [format "\\x%02X" [expr $hex & 0xff]]
            }
        }
    }
    append result $str
    return $result
}

# This follows the char::quote api from the char package. If args is empty
# then str is returned in printable form. Else a list is returned of
# str+args in printable form.
# The parameter $type is not currently used. It's there for compatibility
# with char::quote
proc xtal::pt::util::printable {type str args} {
    if {[llength $args] == 0} {
        return [dumpstr $str]
    }
    set printables [list [dumpstr $str]]
    foreach str $args {
        lappend printables [dumpstr $str]
    }
    return $printables
}

# # ## ### ##### ######## #############
## Public API

proc xtal::pt::util::error2readable {error text} {
    lassign $error _ location msgs
    lassign [Position $location $text] l c

    lappend map \n \\n
    lappend map \r \\r
    lappend map \t \\t

    # Get 10 chars before and after the failure point.  Depending on
    # the relative position of input beginning and end we may get less
    # back of either.  Special characters in the input (line endings,
    # tabs) are quoted to keep this on a single line.
    set prefix [string map $map [string range $text ${location}-10 $location]]
    set suffix [string map $map [string range $text ${location}+1 ${location}+10]]

    # Construct a line pointing to the failure position. By using the
    # transformed prefix as our source (length) no complex
    # calculations are required. It is implicit in the prefix/suffix
    # separation above.
    set  n [string length $prefix]
    incr n -1
    set point [string repeat - $n]
    append point ^

    # Print our results.
    lappend lines "Parse error at position $location (Line $l, column $c)."
    lappend lines "... ${prefix}${suffix} ..."
    lappend lines "    $point"
    lappend lines "Expected one of"
    lappend lines "* [join [Readables $msgs] "\n* "]"
    lappend lines ""

    return [join $lines \n]
}

proc xtal::pt::util::error2position {error text} {
    lassign $error _ location msgs
    return [Position $location $text]
}

proc xtal::pt::util::error2text {error} {
    lassign $error _ location msgs
    return [Readables $msgs]
}

# # ## ### ##### ######## #############
## Internals

proc xtal::pt::util::Position {location text} {
    incr location -1

    # Computing the line/col of a position is quite easy. Split the
    # part before the location into lines (at eol), count them, and
    # look at the length of the last line in that.

    set prefix [string range $text 0 $location]
    set lines  [split $prefix \n]
    set line   [llength $lines]
    set col    [string length [lindex $lines end]]

    return [list $line $col]
}

proc xtal::pt::util::Readables {msgs} {
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

proc xtal::pt::util::Readable {pe} {
    set details [lassign $pe tag]
    switch -exact -- $tag {
	t        {
	    set details [printable string {*}$details]
	    set m "The character '$details'"
	}
	n        { set m "The symbol $details" }
	..       {
	    set details [printable string {*}$details]
	    set m "A character in range '[join $details '-']'"
	}
	str      {
	    set details [join [printable string {*}[split $details {}]] {}]
	    set m "A string \"$details\""
	}
	cl       {
	    set details [join [printable string {*}[split $details {}]] {}]
	    set m "A character in set \{$details\}"
	}
	alpha    { set m "A unicode alphabetical character" }
	alnum    { set m "A unicode alphanumerical character" }
	ascii    { set m "An ascii character" }
	digit    { set m "A unicode digit character" }
	graph    { set m "A unicode printing character, but not space" }
	lower    { set m "A unicode lower-case alphabetical character" }
	print    { set m "A unicode printing character, including space" }
	control  { set m "A unicode control character" }
	punct    { set m "A unicode punctuation character" }
	space    { set m "A unicode space character" }
	upper    { set m "A unicode upper-case alphabetical character" }
	wordchar { set m "A unicode word character (alphanumerics + connectors)" }
	xdigit   { set m "A hexadecimal digit" }
	ddigit   { set m "A decimal digit" }
	dot      { set m "Any character" }
	default  { set m [string totitle $tag] }
    }
    return $m
}

interp alias {} xtal::error2readable {} xtal::pt::util::error2readable
return
