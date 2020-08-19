#!/bin/sh

export NG_SRC_DIR=`pwd`
rm -rfv $NG_SRC_DIR/texlive/libs/cairo/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/cairo/cairo-src/src/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/libpaper/config.force
rm -rfv $NG_SRC_DIR/texlive/libs/libpaper/libpaper-src/lib/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/libpaper/libpaper-src/src/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/libpng/libpng-src/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/libpng/libpng-src/arm/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/libpng/libpng-src/powerpc/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/pixman/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/pixman/pixman-src/pixman/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/zlib/zlib-src/.deps
rm -rfv $NG_SRC_DIR/texlive/libs/zlib/zlib-src/test/.deps
rm -rfv $NG_SRC_DIR/texlive/texk/kpathsea/.deps
rm -rfv $NG_SRC_DIR/texlive/texk/kpathsea/stamp-kpathsea
rm -rfv $NG_SRC_DIR/texlive/texk/kpathsea/win32/.deps
rm -rfv $NG_SRC_DIR/texlive/texk/ptexenc/.deps

