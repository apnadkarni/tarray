# For articles : tclsh buildsite.tcl articles -byline 1

set target output
set adocgen_files {
    introduction guide column table tscript build
}

# file delete -force $target
file mkdir $target
file delete -force [file join $target images]
file copy images [file join $target images]
#file delete -force [file join $target scripts]
#file copy scripts [file join $target scripts]
file copy -force -- index.ad header.ad links.ad types.ad indices.ad $target
puts [exec [info nameofexecutable] c:/src/tcl-on-windows/tools/adocgen.tcl -outdir $target -maketoc toc.ad -unsafe -overwrite -author "Ashok P. Nadkarni" {*}$argv {*}[lmap fn $adocgen_files {append fn .adocgen}] 2>@1]
cd $target
puts [exec asciidoctor index.ad {*}[lmap fn $adocgen_files {append fn .ad}]]

if {0} {
    # Insert Google tags into output html files
    set snippet {
        <!-- Google Tag Manager -->
        <noscript><iframe src="//www.googletagmanager.com/ns.html?id=GTM-PNBD6P"
        height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
        <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
            new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
            j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
            '//www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
        })(window,document,'script','dataLayer','GTM-PNBD6P');</script>
        <!-- End Google Tag Manager -->
    }
    foreach fn [linsert $adocgen_files 0 index] {
        append fn .html
        set fd [open $fn r]
        if {![regsub {<body[^>]*>} [read $fd] \\0$snippet html]} {
            error "Body tag not found"
        }
        close $fd
        set fd [open $fn w]
        puts $fd $html
        close $fd
    }
}
