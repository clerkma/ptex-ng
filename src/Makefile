#
# Copyright (c) 2014-2023 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
CDEBUG =
CFLAGS += $(CDEBUG) -Werror -c -O2
INCLUDES = \
	-I ../texlive/texk/ptexenc/ \
	-I ../texlive/texk/ \
	-I ../texlive/libs/zlib/include \
	-I ../texlive/libs/freetype2/freetype2 \
	-I ../texlive/libs/cairo/cairo \
	-I ../texlive/libs/cairo/cairo-src/src \
	-I ../texlive/texk/web2c/libmd5 \
	-I libotf/src
LDLIBS = libdpx.a libcairo-pdf.a libcairo.a libpixman.a libpng.a\
	libpaper.a libz.a libptexenc.a libkpathsea.a libotf.a libfreetype.a\
	libmd5.a -lm
LDFLAGS += -o

objects = aptex.o aptex-utils.o aptex-src.o
headers = aptex.h aptex-functions.h aptex-macros.h aptex-utils.h

lsotfea_objects = lsotfea.o lsotfea-stag.o lsotfea-ltag.o lsotfea-ftag.o

all: aptex lsotfea

aptex: $(objects)
	$(CC) $(LDFLAGS) aptex $(objects) $(LDLIBS)
	cp aptex ptex-ng

lsotfea: lsotfea-src/lsotfea.c lsotfea-src/lsotfea-stag.c lsotfea-src/lsotfea-ltag.c lsotfea-src/lsotfea-ftag.c
	$(CC) -O2 $(LDFLAGS) lsotfea $(INCLUDES) lsotfea-src/lsotfea.c lsotfea-src/lsotfea-stag.c lsotfea-src/lsotfea-ltag.c lsotfea-src/lsotfea-ftag.c libotf.a libfreetype.a

aptex.o: aptex.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex.c

aptex-utils.o: aptex-utils.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-utils.c

aptex-src.o: aptex-src.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-src.c

.PHONY: clean

clean:
	rm aptex ptex-ng lsotfea *.o
