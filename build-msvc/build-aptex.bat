rem Build ApTeX on Visual Studio 2015/2017/2019.

echo off
if "%1" == "" goto set_nmake
if "%1" == "jom" goto set_jom

:set_nmake
set MAKE=nmake
goto end

:set_jom
set MAKE=jom
:end
echo on

echo Building zlib ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-zlib.nmake
echo Building libpng ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-libpng.nmake
echo Building freetype ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-freetype.nmake
echo Building libotf ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-libotf.nmake
echo Building libpixman ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-libpixman.nmake
echo Building libcairo ...
del *.obj
md build\cairo\src
copy ..\texlive\libs\cairo\cairo-src\src\*.c build\cairo\src\
copy ..\texlive\libs\cairo\cairo-src\src\*.h build\cairo\src\
copy ..\texlive\libs\cairo\cairo-src\cairo-version.h build\cairo\cairo-version.h
del build\cairo\src\cairoint.h
%MAKE% -s -nologo -f makefiles\mk-libcairo.nmake
rd /s /q build\cairo\src
echo Building lsotfea ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-lsotfea.nmake
echo Building kpathsea ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-kpathsea.nmake
echo Building ptexenc ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-ptexenc.nmake
echo Building libdpx ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-libdpx.nmake
echo Building libyaml ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-libyaml.nmake
echo Building libmd5 ...
del *.obj
%MAKE% -s -nologo -f makefiles\mk-libmd5.nmake
echo Building aptex/ptex-ng
copy ..\texlive\libs\cairo\cairo-src\src\cairo.h build\cairo\cairo.h
copy ..\texlive\libs\cairo\cairo-src\src\cairo-deprecated.h build\cairo\cairo-deprecated.h
del *.obj
%MAKE% -s -nologo -f makefiles\mk-aptex.nmake
del *.obj *.lib
del build\cairo\cairo-version.h
del build\cairo\cairo.h
del build\cairo\cairo-deprecated.h
