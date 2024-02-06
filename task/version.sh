# update headers of msvc (2024/02/06)
ROOT=build-msvc/build
TEXK=texlive/texk
KVER=$TEXK/kpathsea/version.ac
kver=`grep kpse_version $KVER | grep -o -e '[0-9]\.[0-9]\.[0-9]'`
sed -i 's/"[^"]\+"/"kpathsea version '$kver'"/g' $ROOT/kpathsea/c-auto.h
DVER=$TEXK/dvipdfm-x/configure.ac
dver=`grep AC_INIT $DVER | grep -o -e '[0-9]\+'`
sed -i 's/"[^"]\+"/"'$dver'"/g' $ROOT/libdpx/config.h
PVER=$TEXK/ptexenc/version.ac
pver=`grep ptexenc_version $PVER | grep -o -e '[0-9]\.[0-9]\.[0-9]'`
sed -i 's/"[^"]\+"/"ptexenc version '$pver'"/g' $ROOT/ptexenc/c-auto.h
cp texlive/libs/libpng/libpng-src/pnglibconf.h build-msvc/build/libpng/pnglibconf.h 
cp texlive/libs/zlib/zlib-src/zconf.h.in build-msvc/build/zlib/zconf.h

