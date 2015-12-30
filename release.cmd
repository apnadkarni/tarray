rmdir/s/q build
@rem Form the file names based on the version we are building
@for /f %%i in ('tclsh src/taversion.tcl') do set TANAME=tarray-%%i
@for /f %%i in ('tclsh src/taversion.tcl') do set XTALNAME=xtal-%%i
@for /f %%i in ('tclsh ui/uiversion.tcl') do set UINAME=tarray_ui-%%i

cmd /c "envset x86 && cd src && tclsh build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd src && tclsh build.tcl extension -target win32-x86_64-cl"
cmd /c "cd src && tclsh build.tcl tea"
cmd /c "envset x86 && cd xtal && tclsh build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd xtal && tclsh build.tcl extension -target win32-x86_64-cl"
cmd /c "cd xtal && tclsh build.tcl tea"
cmd /c "cd ui && tclsh build.tcl package"

cd build\lib && zip -r tarray.zip tarray && move tarray.zip ..\%TANAME%.zip  && cd ..\..
cd build\lib && zip -r xtal.zip xtal && move xtal.zip ..\%XTALNAME%.zip && cd ..\..
cd build\lib && zip -r tarray_ui.zip tarray_ui && move tarray_ui.zip ..\%UINAME%.zip && cd ..\..

cd build\tea && tar cvf tarray.tar tarray && gzip tarray.tar && move tarray.tar.gz ..\%TANAME%.tar.gz && cd ..\..
cd build\tea && tar cvf xtal.tar xtal && gzip xtal.tar && move xtal.tar.gz ..\%XTALNAME%.tar.gz && cd ..\..
cd build\lib && tar cvf tarray_ui.tar tarray_ui && gzip tarray_ui.tar && move tarray_ui.tar.gz ..\%UINAME%.tar.gz && cd ..\..
