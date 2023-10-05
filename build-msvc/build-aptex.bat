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
echo Building libpixman ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libpixman.nmake
echo Building libcairo ...
if exist *.obj (del *.obj)
md build\cairo\src
copy %TL_ROOT%\libs\cairo\cairo-src\src\*.c build\cairo\src\
copy %TL_ROOT%\libs\cairo\cairo-src\src\*.h build\cairo\src\
copy %TL_ROOT%\libs\cairo\cairo-src\cairo-version.h build\cairo\cairo-version.h
del build\cairo\src\cairoint.h
%MAKE% -s -nologo -f makefiles\mk-libcairo.nmake
rd /s /q build\cairo\src
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
echo Building libyaml ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libyaml.nmake
echo Building libmd5 ...
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-libmd5.nmake
echo Building aptex/ptex-ng
copy %TL_ROOT%\libs\cairo\cairo-src\src\cairo.h build\cairo\cairo.h
copy %TL_ROOT%\libs\cairo\cairo-src\src\cairo-deprecated.h build\cairo\cairo-deprecated.h
if exist *.obj (del *.obj)
%MAKE% -s -nologo -f makefiles\mk-aptex.nmake
if exist *.obj (del *.obj)
if exist *.lib (del *.lib)
del build\cairo\cairo-version.h
del build\cairo\cairo.h
del build\cairo\cairo-deprecated.h
