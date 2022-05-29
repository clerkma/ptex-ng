#
# Copyright (c) 2022 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC = gcc
AR = ar
CFLAGS += -c -O2 -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -Iinclude

objects = api.o dumper.o emitter.o loader.o parser.o reader.o scanner.o writer.o

libyaml.a: $(objects)
	$(AR) cru libyaml.a $(objects)
	ranlib libyaml.a

api.o: src/api.c
	$(CC) $(CFLAGS) src/api.c

dumper.o: src/dumper.c
	$(CC) $(CFLAGS) src/dumper.c

emitter.o: src/emitter.c
	$(CC) $(CFLAGS) src/emitter.c

loader.o: src/loader.c
	$(CC) $(CFLAGS) src/loader.c

parser.o: src/parser.c
	$(CC) $(CFLAGS) src/parser.c

reader.o: src/reader.c
	$(CC) $(CFLAGS) src/reader.c

scanner.o: src/scanner.c
	$(CC) $(CFLAGS) src/scanner.c

writer.o: src/writer.c
	$(CC) $(CFLAGS) src/writer.c

.PHONY: clean

clean:
	rm *.a *.o
