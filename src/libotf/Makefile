#
# Copyright (c) 2016, 2017, 2018 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#
# cvs -z3 -d:pserver:anonymous@cvs.savannah.nongnu.org:/sources/m17n co libotf
#

CC = gcc
AR = ar
CFLAGS = -c -O2 -I. -I../../texlive/libs/freetype2/freetype2 -Isrc

objects = \
	otfdrive.o\
	otferror.o\
	otfopen.o

libotf.a: $(objects)
	$(AR) cru libotf.a $(objects)

otfdrive.o: src/otfdrive.c
	$(CC) $(CFLAGS) src/otfdrive.c

otferror.o: src/otferror.c
	$(CC) $(CFLAGS) src/otferror.c

otfopen.o: src/otfopen.c
	$(CC) $(CFLAGS) src/otfopen.c

.PHONY: clean

clean:
	rm *.a *.o
