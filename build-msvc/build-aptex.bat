@REM Build ApTeX with Visual Studio 2022/2026.

@ECHO OFF
if "%1" == "" goto set_nmake
if "%1" == "jom" goto set_jom

:set_nmake
set MAKE=nmake
goto start_build

:set_jom
set MAKE=jom

:start_build
set BUILD=build
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
call :build libw2c
call :build aptex

%CLEAN%
if exist *.lib (del *.lib)
cl -nologo -MD -Feaptex-wrap.exe -DEXEPROG=\"aptex.exe\" %TL_ROOT%\texk\texlive\windows_wrapper\callexe.c
copy aptex-wrap.exe aplatex.exe
copy aptex-wrap.exe aplatex-dev.exe
del callexe.obj
exit /B 0

:build
@ECHO ON
@ECHO [31mBuilding %~1 ...[0m
@ECHO OFF
%CLEAN%
%MAKE% -s -nologo -f makefiles\mk-%~1.nmake
exit /B 0
