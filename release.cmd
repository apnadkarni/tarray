rmdir/s/q build
cmd /c "envset x86 && cd src && tclsh build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd src && tclsh build.tcl extension -target win32-x86_64-cl"
cmd /c "envset x64 && cd src && tclsh build.tcl tea"
cmd /c "envset x86 && cd xtal && tclsh build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd xtal && tclsh build.tcl extension -target win32-x86_64-cl"
cmd /c "envset x64 && cd xtal && tclsh build.tcl tea"

 
 
 
