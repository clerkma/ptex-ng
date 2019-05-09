CC = gcc
CFLAGS = -c -Wno-attributes -I../../texlive/libs/cairo -I../../texlive/libs/cairo/cairo -I../../texlive/libs/cairo/cairo-src/src -I../../texlive/libs/zlib/include -I../../texlive/libs/pixman/include -DHAVE_CONFIG_H -DCAIRO_NO_MUTEX
objects = cairo-tag-attributes.o cairo-pdf-interchange.o cairo-tag-stack.o cairo-pdf-surface.o cairo-pdf-operators.o cairo-pdf-shading.o cairo-cff-subset.o cairo-scaled-font-subsets.o cairo-truetype-subset.o cairo-type1-fallback.o cairo-type1-glyph-names.o cairo-type1-subset.o cairo-type3-glyph-surface.o cairo-deflate-stream.o
libcairo-pdf.a: $(objects)
	 ar cru libcairo-pdf.a $(objects)
cairo-tag-attributes.o : ../../texlive/libs/cairo/cairo-src/src/cairo-tag-attributes.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-tag-attributes.c
cairo-pdf-interchange.o : ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-interchange.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-interchange.c
cairo-tag-stack.o : ../../texlive/libs/cairo/cairo-src/src/cairo-tag-stack.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-tag-stack.c
cairo-pdf-surface.o : ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-surface.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-surface.c
cairo-pdf-operators.o : ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-operators.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-operators.c
cairo-pdf-shading.o : ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-shading.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-pdf-shading.c
cairo-cff-subset.o : ../../texlive/libs/cairo/cairo-src/src/cairo-cff-subset.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-cff-subset.c
cairo-scaled-font-subsets.o : ../../texlive/libs/cairo/cairo-src/src/cairo-scaled-font-subsets.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-scaled-font-subsets.c
cairo-truetype-subset.o : ../../texlive/libs/cairo/cairo-src/src/cairo-truetype-subset.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-truetype-subset.c
cairo-type1-fallback.o : ../../texlive/libs/cairo/cairo-src/src/cairo-type1-fallback.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-type1-fallback.c
cairo-type1-glyph-names.o : ../../texlive/libs/cairo/cairo-src/src/cairo-type1-glyph-names.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-type1-glyph-names.c
cairo-type1-subset.o : ../../texlive/libs/cairo/cairo-src/src/cairo-type1-subset.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-type1-subset.c
cairo-type3-glyph-surface.o : ../../texlive/libs/cairo/cairo-src/src/cairo-type3-glyph-surface.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-type3-glyph-surface.c
cairo-deflate-stream.o : ../../texlive/libs/cairo/cairo-src/src/cairo-deflate-stream.c
	 $(CC) $(CFLAGS) ../../texlive/libs/cairo/cairo-src/src/cairo-deflate-stream.c
clean:
	rm $(wildcard *.o *.a)
test:
	$(CC) -I. -I../../texlive/libs/cairo/cairo-src/src -o test test.c libcairo-pdf.a ../libcairo.a ../libpixman.a ../libz.a -lm
