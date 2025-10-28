#!/bin/bash

MACHINE=`uname -m`
UNIVERSAL=$1
NG_SRC_DIR=`pwd`
export TLROOT=`pwd`/texlive
export CFLAGS=-Wno-deprecated-non-prototype

export have_brotli=no
export with_brotli=no

function set_flags() {
  if [ x$UNIVERSAL == xm1 ]; then
    export CFLAGS="-arch arm64 -arch x86_64"
    export CXXFLAGS="-arch arm64 -arch x86_64"
    export LDFLAGS="-arch arm64 -arch x86_64"
  fi
}

function build_op_mruby() {
  export MRUBY_CONFIG=$NG_SRC_DIR/build-gcc/macOS-$1.rb
  make -j
  local host_mruby_lib=build/host/lib/libmruby.a
  local build_mruby_lib=build/$1/lib/libmruby.a
  lipo -create $host_mruby_lib $build_mruby_lib -output $NG_SRC_DIR/src/libmruby.a
}

function build_mruby() {
  cd $NG_SRC_DIR/src/mruby
  make -j
  if [ x$UNIVERSAL == xm1 ]; then
    if [ x$MACHINE == xarm64 ]; then
      build_op_mruby "x86_64"
    elif [ x$MACHINE == xx86_64 ]; then
      build_op_mruby "arm64"
    fi
  else
    cp build/host/lib/libmruby.a $NG_SRC_DIR/src
  fi
}
#
# build_mruby
#
set_flags
# libraries from TeX Live.
cd $NG_SRC_DIR/texlive/libs/zlib && ./configure && make -j || exit 1
cp libz.a $NG_SRC_DIR/src
# disable neon support of libpng
if [ x$UNIVERSAL == xm1 ] || [ x$MACHINE == xarm64 ]; then
  cd $NG_SRC_DIR/texlive/libs/libpng && ./configure --disable-arm-neon && make -j || exit 1
  cp libpng.a $NG_SRC_DIR/src
else
  cd $NG_SRC_DIR/texlive/libs/libpng && ./configure && make -j || exit 1
  cp libpng.a $NG_SRC_DIR/src
fi
#
cd $NG_SRC_DIR/texlive/libs/libpaper && ./configure && make -j || exit 1
cp libpaper.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/kpathsea && ./configure && make -j || exit 1
cp .libs/libkpathsea.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/ptexenc && ./configure && make -j || exit 1
cp .libs/libptexenc.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src/libdpx && make -j || exit 1
cp libdpx.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libw2c && make -j || exit 1
cp libw2c.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src && make -j
