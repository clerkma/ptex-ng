CC = gcc
AR = ar
CFLAGS += -c -O2
SYNCTEXINCLUDES = -I.. -I../../texlive/texk -I../../texlive/texk/ptexenc \
	-I../../texlive/texk/web2c/synctexdir \
	-DSYNCTEX_ENGINE_H='"synctex-aptex.h"'
MD5INCLUDES = -I../../texlive/texk/web2c/libmd5
objects = synctex.o md5.o

libw2c.a: $(objects)
	$(AR) cr libw2c.a $(objects)
	ranlib libw2c.a

synctex.o: ../../texlive/texk/web2c/synctexdir/synctex.c
	$(CC) $(CFLAGS) $(SYNCTEXINCLUDES) ../../texlive/texk/web2c/synctexdir/synctex.c

md5.o: ../../texlive/texk/web2c/libmd5/md5.c
	$(CC) $(CFLAGS) $(MD5INCLUDES) ../../texlive/texk/web2c/libmd5/md5.c

.PHONY: clean

clean:
	rm *.a *.o
