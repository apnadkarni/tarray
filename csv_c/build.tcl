package require fileutil
package require platform
package require critcl 3.1
package require critcl::app

proc usage {} {
    puts "Usage:\n  [info script] extension\n  [info script] tea"
    exit 1
}

switch -exact -- [lindex $argv 0] {
    ext -
    extension {
        set buildarea [file normalize [file join [pwd] .. build]]
        critcl::app::main [list -pkg -libdir [file join $buildarea lib] -includedir [file join $buildarea include] -cache [file join $buildarea cache] -clean {*}[lrange $argv 1 end] csv_c csv_c.critcl]
    }
    tea {
        critcl::app::main [list -tea -libdir [file join $buildarea lib] {*}[lrange $argv 1 end] csv_c csv_c.critcl]
    }
    default {
        usage
    }
}
