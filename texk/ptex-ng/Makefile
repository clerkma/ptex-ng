#
# Copyright (c) 2014, 2015, 2016 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
CFLAGS = -c -O2 -Wno-unused-result
INCLUDES = -I.. -I../ptexenc/ -I../../libs/zlib/include -I../../libs/freetype2/freetype2 -Ilibotf/src
LDFLAGS = libdpx.a libpng.a libpaper.a libz.a libptexenc.a libkpathsea.a libotf.a libfreetype.a -lm

objects = \
	aptex.o\
	aptex-src.o\
	aptex-synctex.o

headers = aptex.h aptex-functions.h aptex-macros.h

all: aptex lsotfea

aptex: $(objects)
	$(CC) -s -o aptex $(objects) $(LDFLAGS)
	cp aptex ptex-ng

lsotfea: lsotfea.c
	$(CC) -s -O2 -o lsotfea $(INCLUDES) lsotfea.c libotf.a libfreetype.a

aptex.o: aptex.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex.c

aptex-synctex.o: aptex-synctex.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-synctex.c

aptex-src.o: aptex-src.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-src.c

.PHONY: clean

clean:
	rm aptex ptex-ng lsotfea *.o
