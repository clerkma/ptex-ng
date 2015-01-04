#
# Copyright (c) 2014 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
CFLAGS = -c -O2 -Wno-unused-result -I.. -I../ptexenc/ -I../../libs/zlib/include
LDFLAGS = libdpx.a libpng.a libz.a libptexenc.a libkpathsea.a -lm

objects = \
	ptex-ng.o\
	ptex-ng-initex.o\
	ptex-ng-io.o\
	ptex-ng-local.o\
	ptex-ng-src.o\
	ptex-ng-pool.o\
	ptex-ng-pdf.o\
	ptex-ng-kanji.o\
	ptex-ng-synctex.o

headers = ptex-ng.h coerce.h macros.h

ptex-ng: $(objects)
	$(CC) -o ptex-ng $(objects) $(LDFLAGS)

ptex-ng.o: ptex-ng.c $(headers)
	$(CC) $(CFLAGS) ptex-ng.c

ptex-ng-pdf.o: ptex-ng-pdf.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-pdf.c

ptex-ng-kanji.o: ptex-ng-kanji.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-kanji.c

ptex-ng-synctex.o: ptex-ng-synctex.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-synctex.c

ptex-ng-initex.o: ptex-ng-initex.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-initex.c

ptex-ng-io.o: ptex-ng-io.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-io.c

ptex-ng-local.o: ptex-ng-local.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-local.c

ptex-ng-src.o: ptex-ng-src.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-src.c

ptex-ng-pool.o: ptex-ng-pool.c $(headers)
	$(CC) $(CFLAGS) ptex-ng-pool.c

.PHONY: clean

clean:
	rm ptex-ng *.o
