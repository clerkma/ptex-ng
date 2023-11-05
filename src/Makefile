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
	-I ../texlive/texk/web2c/libmd5
LDLIBS = libdpx.a libpng.a libpaper.a libz.a libptexenc.a libkpathsea.a libmd5.a -lm
LDFLAGS += -o

objects = aptex.o aptex-utils.o aptex-src.o
headers = aptex.h aptex-functions.h aptex-macros.h aptex-utils.h

all: aptex

aptex: $(objects)
	$(CC) $(LDFLAGS) aptex $(objects) $(LDLIBS)
	cp aptex ptex-ng

aptex.o: aptex.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex.c

aptex-utils.o: aptex-utils.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-utils.c

aptex-src.o: aptex-src.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-src.c

.PHONY: clean

clean:
	rm aptex ptex-ng lsotfea *.o
