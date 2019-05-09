import os

s = [
  "cairo-tag-attributes.o",
  "cairo-pdf-interchange.o",
  "cairo-tag-stack.o",
  "cairo-pdf-surface.o",
  "cairo-pdf-operators.o",
  "cairo-pdf-shading.o",
  "cairo-cff-subset.o",
  "cairo-scaled-font-subsets.o",
  "cairo-truetype-subset.o",
  "cairo-type1-fallback.o",
  "cairo-type1-glyph-names.o",
  "cairo-type1-subset.o",
  "cairo-type3-glyph-surface.o",
  "cairo-deflate-stream.o"
]

CFLAGS = "-c -Wno-attributes -I../../texlive/libs/cairo -I../../texlive/libs/cairo/cairo -I../../texlive/libs/cairo/cairo-src/src -I../../texlive/libs/zlib/include -I../../texlive/libs/pixman/include -DHAVE_CONFIG_H -DCAIRO_NO_MUTEX"
PREFIX = "../../texlive/libs/cairo/cairo-src/src/"

makefile = open("Makefile", "w")
makefile.write("CC = gcc\n")
makefile.write("CFLAGS = %s\n" % CFLAGS)
makefile.write("objects = %s\n" % " ".join(s))
makefile.write("libcairo-pdf.a: $(objects)\n")
makefile.write("\t ar cru libcairo-pdf.a $(objects)\n")
for i in s:
  makefile.write("%s : %s%s\n" % (i, PREFIX, i.replace(".o", ".c")))
  makefile.write("\t $(CC) $(CFLAGS) %s%s\n" % (PREFIX, i.replace(".o", ".c")))
makefile.write("clean:\n")
makefile.write("\trm $(wildcard *.o *.a)\n")
makefile.write("test:\n")
makefile.write("\t$(CC) -I. -I../../texlive/libs/cairo/cairo-src/src -o test test.c libcairo-pdf.a ../libcairo.a ../libpixman.a ../libz.a -lm\n")
makefile.close()
