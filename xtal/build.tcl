package require fileutil
package require pt::pgen
package require platform
package require critcl 3.1
package require critcl::app

set taversion [source ../src/taversion.tcl]

proc usage {} {
    puts "Usage:\n  [info script] parser\n  [info script] extension\n  [info script] tea\n"
    exit 1
}

set buildarea [file normalize [file join [pwd] .. build]]
switch -exact -- [lindex $argv 0] {
    parser {
        set critcl_source [pt::pgen peg [fileutil::cat xtal.peg] critcl -class xtal::ParserBase -package xtal -name Xtal -version $taversion]
        # We want the xtal.tcl file to be included in the package
        # so insert it into the generated critcl parser file
        fileutil::writeFile xtal.critcl [regsub {return\s*$} $critcl_source "critcl::tsources xtal.tcl shell.tcl\n"]
    }
    ext -
    extension {
        critcl::app::main [list -pkg -libdir [file join $buildarea lib] -includedir [file join $buildarea include] -cache [file join $buildarea cache] -clean {*}[lrange $argv 1 end] xtal xtal.critcl]
    }
    tea {
        critcl::app::main [list -tea -libdir [file join $buildarea tea] {*}[lrange $argv 1 end] xtal xtal.critcl]
    }
    default {
        usage
    }
}
