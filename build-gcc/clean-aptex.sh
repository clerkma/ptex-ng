#!/bin/sh

export NG_SRC_DIR=`pwd`
cd $NG_SRC_DIR/texlive/libs/zlib && make distclean
cd $NG_SRC_DIR/texlive/libs/libpng && make distclean
cd $NG_SRC_DIR/texlive/libs/libpaper && make distclean
cd $NG_SRC_DIR/texlive/libs/freetype2 && make distclean
cd $NG_SRC_DIR/texlive/libs/pixman && make distclean
cd $NG_SRC_DIR/texlive/texk/kpathsea && make distclean
cd $NG_SRC_DIR/texlive/texk/ptexenc && make distclean
cd $NG_SRC_DIR/texlive/libs/cairo && make distclean
cd $NG_SRC_DIR/src/libcairo && rm -f *.o *.a
cd $NG_SRC_DIR/src/libdpx && rm -f *.o *.a
cd $NG_SRC_DIR/src/libotf && rm -f *.o *.a
cd $NG_SRC_DIR/src/libmd5 && rm -f *.o *.a
cd $NG_SRC_DIR/src/mruby && make clean
cd $NG_SRC_DIR/src && rm -f *.o *.a aptex lsotfea ptex-ng
