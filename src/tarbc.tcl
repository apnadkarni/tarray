#
# Copyright (c) 2017 Ashok P. Nadkarni
# All rights reserved.
#
# See the file LICENSE for license
#

namespace eval tarray::rbc {
    # Define our rbc vector procedures to initialize rbc and then call
    # the *redefined* commands of the same name.
    foreach name {fromvector tovector} {
        proc $name args "init; tailcall \[namespace current\]::$name {*}\$args"
    }

    # Initializes rbc and defines the real commands
    proc init {} {
        # The uplevel is required to define C commands in global context
        uplevel #0 tarray::rbc::init_stubs
        proc init {} {}
    }
}

