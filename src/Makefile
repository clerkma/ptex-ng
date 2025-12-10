#
# Copyright (c) 2014-2025 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
CDEBUG =
CFLAGS += $(CDEBUG) -Werror -c -O2 -funroll-loops -flto -fstrict-enums -fjump-tables -fshort-enums
INCLUDES = \
	-I $(TLROOT)/texk/ \
	-I $(TLROOT)/texk/ptexenc/ \
	-I $(TLROOT)/libs/zlib/include \
	-I $(TLROOT)/texk/web2c/libmd5
LDLIBS = libdpx.a libpng.a libz.a libptexenc.a libkpathsea.a libw2c.a -lm
LDFLAGS += -flto -o

objects = aptex.o aptex-utils.o aptex-src.o
headers = aptex.h aptex-funs.h aptex-defs.h aptex-utils.h aptex-dpx.h

all: aptex

aptex: $(objects)
	$(CC) $(LDFLAGS) aptex $(objects) $(LDLIBS)

aptex.o: aptex.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex.c

aptex-utils.o: aptex-utils.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-utils.c

aptex-src.o: aptex-src.c $(headers)
	$(CC) $(CFLAGS) $(INCLUDES) aptex-src.c

.PHONY: clean

clean:
	rm aptex lsotfea *.o
