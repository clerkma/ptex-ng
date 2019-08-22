CC = gcc
AR = ar
CFLAGS = -c -O2 -I../../texlive/texk/web2c/libmd5

objects = md5.o

libmd5.a: $(objects)
	$(AR) cru libmd5.a $(objects)

md5.o: ../../texlive/texk/web2c/libmd5/md5.c
	$(CC) $(CFLAGS) ../../texlive/texk/web2c/libmd5/md5.c

.PHONY: clean

clean:
	rm *.a *.o
