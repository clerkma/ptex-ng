#
# Copyright (c) 2025-2026 LdBeth, Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
AR = ar
CFLAGS += $(CDEBUG) -c -O2
SYNCTEXINCLUDES = -I.. \
	-I $(TLROOT)/texk \
	-I $(TLROOT)/texk/ptexenc \
	-I $(TLROOT)/libs/zlib/include \
	-I $(TLROOT)/texk/web2c/synctexdir \
	-DSYNCTEX_ENGINE_H='"synctex-aptex.h"'
MD5INCLUDES = -I $(TLROOT)/texk/web2c/libmd5
objects = synctex.o md5.o

libw2c.a: $(objects)
	$(AR) cr libw2c.a $(objects)
	ranlib libw2c.a

synctex.o: $(TLROOT)/texk/web2c/synctexdir/synctex.c
	$(CC) $(CFLAGS) $(SYNCTEXINCLUDES) $<

md5.o: $(TLROOT)/texk/web2c/libmd5/md5.c
	$(CC) $(CFLAGS) $(MD5INCLUDES) $<

.PHONY: clean

clean:
	rm *.a *.o
