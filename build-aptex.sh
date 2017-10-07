export NG_SRC_DIR=`pwd`
# libraries from TeX Live.
cd $NG_SRC_DIR/texlive/libs/zlib && ./configure && make
cp libz.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/libpng && ./configure && make
cp libpng.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/libpaper && ./configure && make
cp libpaper.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/freetype2 && ./configure && make
cp libfreetype.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/pixman && ./configure && make
cp libpixman.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/cairo && ./configure && make
cp libcairo.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/kpathsea && ./configure && make
cp .libs/libkpathsea.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/ptexenc && ./configure && make
cp .libs/libptexenc.a $NG_SRC_DIR/src
# 
cd $NG_SRC_DIR/src/libdpx && make
cp libdpx.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libotf && make
cp libotf.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src && make
