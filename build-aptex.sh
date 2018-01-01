#!/bin/bash

export NG_SRC_DIR=`pwd`
# libraries from TeX Live.
cd $NG_SRC_DIR/texlive/libs/zlib && ./configure && make -j
cp libz.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/libpng && ./configure && make -j
cp libpng.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/libpaper && ./configure && make -j
cp libpaper.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/freetype2 && ./configure && make -j
cp libfreetype.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/libs/pixman && ./configure && make -j
cp libpixman.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/kpathsea && ./configure && make -j
cp .libs/libkpathsea.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/texlive/texk/ptexenc && ./configure && make -j
cp .libs/libptexenc.a $NG_SRC_DIR/src
# 
cd $NG_SRC_DIR/src/libcairo && make -j
cp libcairo.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libdpx && make -j
cp libdpx.a $NG_SRC_DIR/src
cd $NG_SRC_DIR/src/libotf && make -j
cp libotf.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src/libyaml
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/api.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/dumper.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/emitter.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/loader.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/parser.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/reader.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/scanner.c
gcc -I include -I win32 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -c src/writer.c
ar rcu libyaml.a api.o dumper.o emitter.o loader.o parser.o reader.o scanner.o writer.o
cp libyaml.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src/mruby && make -j
cp build/host/lib/libmruby.a $NG_SRC_DIR/src
#
cd $NG_SRC_DIR/src && make -j
