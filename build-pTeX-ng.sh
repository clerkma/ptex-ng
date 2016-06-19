export NG_SRC_DIR=`pwd`
cd $NG_SRC_DIR/libs/zlib && ./configure && make
cp libz.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/libs/libpng && ./configure && make
cp libpng.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/libs/libpaper && ./configure && make
cp libpaper.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/libs/freetype2 && ./configure && make
cp libfreetype.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/texk/kpathsea && ./configure && make
cp .libs/libkpathsea.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/texk/ptexenc && ./configure && make
cp .libs/libptexenc.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/texk/libdpx && ./configure && make
cp libdpx.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/texk/ptex-ng/libotf && make
cp libotf.a $NG_SRC_DIR/texk/ptex-ng
cd $NG_SRC_DIR/texk/ptex-ng && make
