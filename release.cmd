rmdir/s/q build
cmd /c "envset x86 && cd src && tclsh build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd src && tclsh build.tcl extension -target win32-x86_64-cl"
cmd /c "cd src && tclsh build.tcl tea"
cmd /c "envset x86 && cd xtal && tclsh build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd xtal && tclsh build.tcl extension -target win32-x86_64-cl"
cmd /c "cd xtal && tclsh build.tcl tea"

 
cd build\lib && zip -r tarray.zip tarray && move tarray.zip .. && cd ..\..
cd build\lib && zip -r xtal.zip xtal && move xtal.zip .. && cd ..\..
cd build\tea && tar cvf tarray.tar tarray && gzip tarray.tar && move tarray.tar.gz .. && cd ..\..
cd build\tea && tar cvf xtal.tar xtal && gzip xtal.tar && move xtal.tar.gz .. && cd ..\..
