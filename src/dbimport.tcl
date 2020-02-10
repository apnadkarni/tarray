#
# Copyright (c) 2019, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

namespace eval tarray::table::dbimport {

    proc map_sql_type {colmeta nullable} {
        # colmeta in form returned by tdbc in column subdictionary
        # Nullable columns always returned as Tcl_Obj as we have
        # there is no way to represent them for other types.
        # Even the empty string rep for Tcl_Obj is incorrect but
        # that's life.
        if {$nullable} {
            return any
        }
        switch -exact -- $type {
            int - smallint - integer {return int}
            bigint {return wide}
            tinyint {return byte}
            float - decimal - numeric - double {return double}
            bit {return boolean}
            default {return any}
        }
    }

    proc is_primary_key {db tabname colname} {
        # Return true if the column is a primary key in the table

        # Note, tolower because tdbc seems to convert all keys in meta information
        # to lower so tabname and colname passed in are lower case
        foreach keymeta [$db primarykeys $tabname] {
            if {[dict exists $keymeta columnName] &&
                [string equal -nocase [dict get $keymeta columnName] $colname]
            } {
                return 1
            }
        }
        return 0
    }

    proc resultset {rs vtab} {
        upvar 1 $vtab tab
        set orig_size [tarray::table size $tab]
        if {[catch {
            while {[$rs nextlist row]} {
                tarray::table vinsert tab $row end
            }
        } result ropts]} {
            # Remove any entries that were added
            tarray::table vdelete tab $orig_size end
            return -options $ropts $result
        }
    }

    proc table {db tabname args} {
        set tabmeta [$db columns $tabname]
        if {[llength $args] == 0} {
            set colnames [dict keys $tabmeta]
        } elseif {[llength $args] == 1} {
            set colnames [lindex $args 0]
        } else {
            throw {TCL WRONGARGS} "wrong # args: should \"table dbimport table DBCONN TABNAME ?COLNAMES?\""
        }
        set column_defs [list ]
        foreach colname $colnames {
            if {[is_primary_key $db $tabname $colname]} {
                set nullable 0
            } else {
                set nullable [dict get $tabmeta $colname nullable]
            }
            lappend column_defs $colname [map_sql_type [dict get $tabmeta $colname] $nullable]
        }
        set result [tarray::table create $column_defs]
        set stmt [$db prepare "SELECT [join $colnames ,] FROM $tabname"]
        try {
            set rs [$stmt execute]
            resultset $rs result
        } finally {
            $stmt close
        }
        return $result
    }

    namespace export resultset table
    namespace ensemble create

}
