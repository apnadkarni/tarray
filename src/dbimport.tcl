#
# Copyright (c) 2019, Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

namespace eval tarray::table::dbimport {
    proc resultset {vtab from} {
        upvar 1 $vtab tab
        set orig_size [tarray::table size $tab]
        if {[catch {
            while {[$from nextlist row]} {
                tarray::table vinsert tab $row end
            }
        } result ropts]} {
            # Remove any entries that were added
            tarray::table vdelete tab $orig_size end
            return -options $ropts $result
        }
        return $tab
    }

    namespace export *
    namespace ensemble create

}

