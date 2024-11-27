setlocal
rmdir/s/q build

if not defined TCLSH set /p TCLSH=Enter path to tclsh: 
echo Using %TCLSH%
exit /B 0
set OLDTCLSH=%TCLSH%
set TCLSH=d:\tcl\magic\bin\tclsh86t.exe

@rem Form the file names based on the version we are building
@for /f %%i in ('%TCLSH% src/taversion.tcl') do set TANAME=tarray-%%i
@for /f %%i in ('%TCLSH% src/taversion.tcl') do set XTALNAME=xtal-%%i
@for /f %%i in ('%TCLSH% ui/uiversion.tcl') do set UINAME=tarray_ui-%%i

cmd /c "envset x86 && cd src && %TCLSH% build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd src && %TCLSH% build.tcl extension -target win32-x86_64-cl"
@rem cmd /c "cd src && %TCLSH% build.tcl tea"
cmd /c "envset x86 && cd xtal && %TCLSH% build.tcl extension -target win32-ix86-cl"
cmd /c "envset x64 && cd xtal && %TCLSH% build.tcl extension -target win32-x86_64-cl"
@rem cmd /c "cd xtal && %TCLSH% build.tcl tea"
cmd /c "cd ui && %TCLSH% build.tcl package"

copy doc\announce.txt build\lib\tarray\readme.txt
move build\lib\tarray build\lib\%TANAME%
cd build\lib && zip -r ../%TANAME%.zip %TANAME% && cd ..\..

copy doc\announce.txt build\lib\xtal\readme.txt
move build\lib\xtal build\lib\%XTALNAME%
cd build\lib && zip -r ../%XTALNAME%.zip %XTALNAME% && cd ..\..

copy doc\announce.txt build\lib\tarray_ui\readme.txt
move build\lib\tarray_ui build\lib\%UINAME%
cd build\lib && zip -r ../%UINAME%.zip %UINAME% && cd ..\..

copy doc\announce.txt build\tea\tarray\readme.txt
dos2unix build/tea/tarray/readme.txt
move build\tea\tarray build\tea\%TANAME%
cd build\tea && tar cvf ../%TANAME%.tar %TANAME% && gzip ../%TANAME%.tar && cd ..\..

copy build\tea\%TANAME%\readme.txt build\tea\xtal\readme.txt
move build\tea\xtal build\tea\%XTALNAME%
cd build\tea && tar cvf ../%XTALNAME%.tar %XTALNAME% && gzip ../%XTALNAME%.tar && cd ..\..

dos2unix build/lib/%UINAME%/readme.txt
cd build\lib && tar cvf ../%UINAME%.tar %UINAME% && gzip ../%UINAME%.tar && cd ..\..

set TCLSH=%OLDTCLSH%
