rem Build ApTeX on Visual Studio 2015/2017/2019/2022.

echo off
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
echo on

echo Building zlib ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-zlib.nmake
echo Building libpng ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libpng.nmake
echo Building freetype ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-freetype.nmake
echo Building libotf ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libotf.nmake
echo Building lsotfea ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-lsotfea.nmake
echo Building kpathsea ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-kpathsea.nmake
echo Building ptexenc ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-ptexenc.nmake
echo Building libdpx ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libdpx.nmake
echo Building libmd5 ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libmd5.nmake
echo Building aptex/ptex-ng
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-aptex.nmake
if exist *.obj (del *.obj)
if exist *.lib (del *.lib)
