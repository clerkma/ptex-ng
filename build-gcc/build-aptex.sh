#!/bin/bash

export NG_SRC_DIR=`pwd`
export have_brotli=no
export with_brotli=no
# libraries from TeX Live.
cd $NG_SRC_DIR/texlive/libs/zlib && ./configure && make -j || exit 1
cp libz.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/libpng && ./configure && make -j || exit 1
cp libpng.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/libpaper && ./configure && make -j || exit 1
cp libpaper.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/freetype2 && ./configure && make -j || exit 1
cp libfreetype.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/pixman && ./configure && make -j || exit 1
cp libpixman.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/kpathsea && ./configure && make -j || exit 1
cp .libs/libkpathsea.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/ptexenc && ./configure && make -j || exit 1
cp .libs/libptexenc.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/cairo && ./configure && make -j || exit 1
cp libcairo.a $NG_SRC_DIR/src
# 
cd $NG_SRC_DIR/src/libcairo && make -j || exit 1
cp libcairo-pdf.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libdpx && make -j || exit 1
cp libdpx.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libotf && make -j || exit 1
cp libotf.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libmd5 && make -j || exit 1
cp libmd5.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src/mruby && make -j || exit 1
cp build/host/lib/libmruby.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src && make -j
