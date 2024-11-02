#
# Copyright (c) 2017-2018 Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

namespace eval tarray::rbc {
    # TBD - document tovector and fromvector
    # Define our rbc vector procedures to initialize rbc and then call
    # the *redefined* commands of the same name.
    variable name
    foreach name {fromvector tovector} {
        proc $name args "init; tailcall \[namespace current\]::$name {*}\$args"
    }
    unset name

    # Initializes rbc and defines the real commands
    proc init {} {
        proc init {} {}
        
        # The uplevel is required to define C commands in global context
        if {[catch {uplevel #0 tarray::rbc::init_stubs}]} {
            # Perhaps we are running against RBC without stubs
            # Then define procedures the slow way
            
            proc fromvector {vec args} {
                if {[llength $args] == 0} {
                    set first 0
                    set last end
                } elseif {[llength $args] == 2} {
                    lassign $args first last
                } else {
                    error "wrong #args: should be \"fromvector VEC ?FIRST LAST?\""
                }
                # $vec has to be resolved in context of caller
                set vals [uplevel 1 [list [$vec range $first $last]]]
                return [tarray::column create double $vals]
            }

            proc tovector {vec col args} {
                if {[llength $args] == 0} {
                    set vals [tarray::column range -list $col 0 end]
                } elseif {[llength $args] == 1} {
                    set vals [tarray::column get -list $col [lindex $args 0]]
                } else {
                    error "wrong #args: should be \"tovector VEC COLUMN ?INDICES?\""
                }
                    
                # Note $vec has to be resolved in context of caller, hence the uplevels
                set qual_vec [uplevel 1 [list namespace which -command $vec]]
                if {$qual_vec eq "" || $qual_vec ni [rbc::vector names]} {
                    uplevel 1 [list rbc::vector create $qual_vec]
                    try {
                        $qual_vec append $vals
                    } trap {} {msg ropts} {
                        vector destroy $qual_vec
                        return $ropts $msg
                    }
                } else {
                    $qual_vec length 0
                    $qual_vec append $vals
                }
                return $vec;    # Return same name as originally passed
            }
        }
    }
}

