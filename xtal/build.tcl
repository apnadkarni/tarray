package require fileutil
package require pt::pgen
package require platform
package require critcl 3.1
package require critcl::app

switch -exact -- [lindex $argv 0] {
    parser {
        fileutil::writeFile xtalparser.critcl [pt::pgen peg [fileutil::cat xtal.peg] critcl -class xtal::ParserBase -package xtalparser -name Xtal]
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
        puts "Usage: [info script] parser|extension"
        exit 1
    }
}
