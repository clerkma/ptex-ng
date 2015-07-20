#
# Copyright (c) 2014, 2015 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
CFLAGS = -c -O2 -Wno-unused-result -I.. -I../ptexenc/ -I../../libs/zlib/include
LDFLAGS = libdpx.a libpng.a libz.a libptexenc.a libkpathsea.a -lm

objects = \
	aptex.o\
	aptex-initex.o\
	aptex-io.o\
	aptex-local.o\
	aptex-src.o\
	aptex-string-pool.o\
	aptex-pdf.o\
	aptex-kanji.o\
	aptex-synctex.o

headers = aptex.h aptex-functions.h aptex-macros.h aptex-synctex.h

aptex: $(objects)
	$(CC) -s -o aptex $(objects) $(LDFLAGS)
	cp aptex ptex-ng

aptex.o: aptex.c $(headers)
	$(CC) $(CFLAGS) aptex.c

aptex-pdf.o: aptex-pdf.c $(headers)
	$(CC) $(CFLAGS) aptex-pdf.c

aptex-kanji.o: aptex-kanji.c $(headers)
	$(CC) $(CFLAGS) aptex-kanji.c

aptex-synctex.o: aptex-synctex.c $(headers)
	$(CC) $(CFLAGS) aptex-synctex.c

aptex-initex.o: aptex-initex.c $(headers)
	$(CC) $(CFLAGS) aptex-initex.c

aptex-io.o: aptex-io.c $(headers)
	$(CC) $(CFLAGS) aptex-io.c

aptex-local.o: aptex-local.c $(headers)
	$(CC) $(CFLAGS) aptex-local.c

aptex-src.o: aptex-src.c $(headers)
	$(CC) $(CFLAGS) aptex-src.c

aptex-string-pool.o: aptex-string-pool.c $(headers)
	$(CC) $(CFLAGS) aptex-string-pool.c

.PHONY: clean

clean:
	rm aptex ptex-ng *.o
