@REM Build ApTeX with Visual Studio 2022.

@ECHO OFF
if "%1" == "" goto set_nmake
if "%1" == "jom" goto set_jom

:set_nmake
set MAKE=nmake
goto start_build

:set_jom
set MAKE=jom
:start_build

set TL_ROOT=..\texlive
set APTEX_ROOT=..\src
set APTEX_CFLAGS=-nologo -c -O2 -Oy
set CLEAN=if exist *.obj (del *.obj)
@ECHO OFF

call :build zlib
call :build libpng
call :build kpathsea
call :build ptexenc
call :build libdpx
call :build libmd5
call :build aptex

%CLEAN%
if exist *.lib (del *.lib)
cl -nologo -MD -Feptex-ng-wrap.exe -DEXEPROG=\"ptex-ng.exe\" %TL_ROOT%\texk\texlive\windows_wrapper\callexe.c
copy ptex-ng-wrap.exe platex-ng.exe
copy ptex-ng-wrap.exe platex-dev-ng.exe
exit /B 0

:build
@ECHO ON
@ECHO Building %~1 ...
@ECHO OFF
%CLEAN%
%MAKE% -s -nologo -f makefiles\mk-%~1.nmake
exit /B 0
