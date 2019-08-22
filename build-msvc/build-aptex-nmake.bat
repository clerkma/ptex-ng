rem Build ApTeX on Visual Studio 2015/2017.
echo Building zlib ...
del *.obj
nmake -nologo -f mk-zlib.nmake
echo Building libpng ...
del *.obj
nmake -nologo -f mk-libpng.nmake
echo Building freetype ...
del *.obj
nmake -nologo -f mk-freetype.nmake
echo Building libotf ...
del *.obj
nmake -nologo -f mk-libotf.nmake
echo Building libpixman ...
del *.obj
nmake -nologo -f mk-libpixman.nmake
echo Building libcairo ...
del *.obj
md cairo\src
copy ..\texlive\libs\cairo\cairo-src\src\*.c cairo\src\
copy ..\texlive\libs\cairo\cairo-src\src\*.h cairo\src\
copy ..\texlive\libs\cairo\cairo-src\cairo-version.h cairo\cairo-version.h
del cairo\src\cairoint.h
nmake -nologo -f mk-libcairo.nmake
rd /s /q cairo\src
del cairo\cairo-version.h
echo Building lsotfea ...
del *.obj
nmake -nologo -f mk-lsotfea.nmake
echo Building kpathsea ...
del *.obj
nmake -nologo -f mk-kpathsea.nmake
echo Building ptexenc ...
del *.obj
nmake -nologo -f mk-ptexenc.nmake
echo Building libdpx ...
del *.obj
nmake -nologo -f mk-libdpx.nmake
echo Building libmd5 ...
del *.obj
nmake -nologo -f mk-libmd5.nmake
echo Building aptex/ptex-ng
del *.obj
nmake -nologo -f mk-aptex.nmake
del *.obj *.lib
