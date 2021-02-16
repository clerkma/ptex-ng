rem Build ApTeX on Visual Studio 2015/2017/2019 with jom.
echo Building zlib ...
del *.obj
jom -s -nologo -f mk-zlib.nmake
echo Building libpng ...
del *.obj
jom -s -nologo -f mk-libpng.nmake
echo Building freetype ...
del *.obj
jom -s -nologo -f mk-freetype.nmake
echo Building libotf ...
del *.obj
jom -s -nologo -f mk-libotf.nmake
echo Building libpixman ...
del *.obj
jom -s -nologo -f mk-libpixman.nmake
echo Building libcairo ...
del *.obj
md cairo\src
copy ..\texlive\libs\cairo\cairo-src\src\*.c cairo\src\
copy ..\texlive\libs\cairo\cairo-src\src\*.h cairo\src\
copy ..\texlive\libs\cairo\cairo-src\cairo-version.h cairo\cairo-version.h
del cairo\src\cairoint.h
jom -s -nologo -f mk-libcairo.nmake
rd /s /q cairo\src
echo Building lsotfea ...
del *.obj
jom -s -nologo -f mk-lsotfea.nmake
echo Building kpathsea ...
del *.obj
jom -s -nologo -f mk-kpathsea.nmake
echo Building ptexenc ...
del *.obj
jom -s -nologo -f mk-ptexenc.nmake
echo Building libdpx ...
del *.obj
jom -s -nologo -f mk-libdpx.nmake
echo Building libmd5 ...
del *.obj
jom -s -nologo -f mk-libmd5.nmake
echo Building aptex/ptex-ng
copy ..\texlive\libs\cairo\cairo-src\src\cairo.h cairo\cairo.h
copy ..\texlive\libs\cairo\cairo-src\src\cairo-deprecated.h cairo\cairo-deprecated.h
del *.obj
jom -s -nologo -f mk-aptex.nmake
del *.obj *.lib
del cairo\cairo-version.h
del cairo\cairo.h
del cairo\cairo-deprecated.h
