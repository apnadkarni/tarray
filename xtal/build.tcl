package require fileutil
package require pt::pgen
package require platform
package require critcl 3.1
package require critcl::app

proc usage {} {
    puts "Usage:\n  [info script] parser VERSION\n  [info script] extension"
    exit 1
}

switch -exact -- [lindex $argv 0] {
    parser {
        set ver [lindex $argv 1]
        if {$ver eq ""} {
            usage
        }
        set critcl_source [pt::pgen peg [fileutil::cat xtal.peg] critcl -class xtal::ParserBase -package xtalparser -name Xtal -version [lindex $argv 1]]
        # We want the xtal.tcl file to be included in the package
        # so insert it into the generated critcl parser file
        fileutil::writeFile xtalparser.critcl [regsub {return\s*$} $critcl_source "critcl::tsources xtal.tcl\n"]
    }
    ext -
    extension {
        set buildarea [file normalize [file join [pwd] .. build]]
        critcl::app::main [list -pkg -libdir [file join $buildarea lib] -includedir [file join $buildarea include] -cache [file join $buildarea cache] -clean {*}[lrange $argv 1 end] xtalparser xtalparser.critcl]
    }
    tea {
        critcl::app::main [list -tea -libdir [file join $buildarea lib] {*}[lrange $argv 1 end] xtalparser xtalparser.critcl]
    }
    default {
        usage
    }
}
