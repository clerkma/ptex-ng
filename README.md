How to build ptex-ng:

1. set a variable:

cd ptex-ng
export NG_SRC_DIR=`pwd`

2. zlib

cd $NG_SRC_DIR/libs/zlib && ./configure && make
cp libz.a $NG_SRC_DIR/texk/ptex-ng

3. libpng

cd $NG_SRC_DIR/libs/libpng && ./configure && make
cp libpng.a $NG_SRC_DIR/texk/ptex-ng

4. kpathsea

cd $NG_SRC_DIR/texk/kpathsea && ./configure && make
cp .libs/libkpathsea.a $NG_SRC_DIR/texk/ptex-ng

5. ptexenc

cd $NG_SRC_DIR/texk/ptexenc && ./configure && make
cp .libs/libptexenc.a $NG_SRC_DIR/texk/ptex-ng

6. libdpx

cd $NG_SRC_DIR/texk/libdpx && ./configure && make
cp libdpx.a $NG_SRC_DIR/texk/ptex-ng

7. ptex-ng

cd $NG_SRC_DIR/texk/ptex-ng && make
