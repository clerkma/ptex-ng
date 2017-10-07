# makefile for libdpx
CC := gcc
CFLAGS := -I. -I./ng \
	-I../../texlive/texk \
	-I../../texlive/libs/libpaper/include \
	-I../../texlive/libs/libpng/include \
	-I../../texlive/libs/zlib/include \
	-DLIBDPX -DHAVE_CONFIG_H
objects := $(patsubst %.c, %.o, $(wildcard *.c))

libdpx.a: $(objects)
	ar rc libdpx.a $(objects)

clean:
	rm $(wildcard *.o *.a)
