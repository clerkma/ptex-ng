#
# Copyright (c) 2014-2026 Clerk Ma
#
# You may freely use, modify and/or distribute this file.
#

CC := gcc
ROOT := $(TLROOT)/texk/dvipdfm-x
CFLAGS += $(CDEBUG) -c -I. -I ng \
	-I $(TLROOT)/texk \
	-I $(TLROOT)/libs/libpaper/include \
	-I $(TLROOT)/libs/libpng/include \
	-I $(TLROOT)/libs/zlib/include \
	-DHAVE_CONFIG_H
objects := agl.o bmpimage.o cff.o cff_dict.o cid.o cidtype0.o cidtype2.o cmap.o cmap_read.o cmap_write.o cs_type2.o dpxconf.o dpxcrypt.o dpxfile.o dpxutil.o dvi.o dvipdfmx.o epdf.o error.o fontmap.o jp2image.o jpegimage.o mem.o mfileio.o mpost.o mt19937ar.o numbers.o otl_opt.o pdfcolor.o pdfdev.o pdfdoc.o pdfdraw.o pdfencoding.o pdfencrypt.o pdffont.o pdfnames.o pdfobj.o pdfparse.o pdfresource.o pdfximage.o pkfont.o pngimage.o pst.o pst_obj.o sfnt.o spc_color.o spc_dvipdfmx.o spc_dvips.o spc_html.o spc_misc.o spc_pdfm.o spc_tpic.o spc_util.o spc_xtx.o specials.o subfont.o t1_char.o t1_load.o tfm.o truetype.o tt_aux.o tt_cmap.o tt_glyf.o tt_gsub.o tt_post.o tt_table.o type0.o type1.o type1c.o unicode.o vf.o xbb.o

libdpx.a: $(objects)
	ar cr libdpx.a $(objects)
	ranlib libdpx.a

agl.o: $(ROOT)/agl.c
	$(CC) $(CFLAGS) $<
bmpimage.o: $(ROOT)/bmpimage.c
	$(CC) $(CFLAGS) $<
cff.o: $(ROOT)/cff.c
	$(CC) $(CFLAGS) $<
cff_dict.o: $(ROOT)/cff_dict.c
	$(CC) $(CFLAGS) $<
cid.o: $(ROOT)/cid.c
	$(CC) $(CFLAGS) $<
cidtype0.o: $(ROOT)/cidtype0.c
	$(CC) $(CFLAGS) $<
cidtype2.o: $(ROOT)/cidtype2.c
	$(CC) $(CFLAGS) $<
cmap.o: $(ROOT)/cmap.c
	$(CC) $(CFLAGS) $<
cmap_read.o: $(ROOT)/cmap_read.c
	$(CC) $(CFLAGS) $<
cmap_write.o: $(ROOT)/cmap_write.c
	$(CC) $(CFLAGS) $<
cs_type2.o: $(ROOT)/cs_type2.c
	$(CC) $(CFLAGS) $<
dpxconf.o: $(ROOT)/dpxconf.c
	$(CC) $(CFLAGS) $<
dpxcrypt.o: $(ROOT)/dpxcrypt.c
	$(CC) $(CFLAGS) $<
dpxfile.o: $(ROOT)/dpxfile.c
	$(CC) $(CFLAGS) $<
dpxutil.o: $(ROOT)/dpxutil.c
	$(CC) $(CFLAGS) $<
dvi.o: $(ROOT)/dvi.c
	$(CC) $(CFLAGS) -DLIBDPX $<
dvipdfmx.o: $(ROOT)/dvipdfmx.c
	$(CC) $(CFLAGS) -DLIBDPX $<
epdf.o: $(ROOT)/epdf.c
	$(CC) $(CFLAGS) $<
error.o: $(ROOT)/error.c
	$(CC) $(CFLAGS) $<
fontmap.o: $(ROOT)/fontmap.c
	$(CC) $(CFLAGS) $<
jp2image.o: $(ROOT)/jp2image.c
	$(CC) $(CFLAGS) $<
jpegimage.o: $(ROOT)/jpegimage.c
	$(CC) $(CFLAGS) $<
mem.o: $(ROOT)/mem.c
	$(CC) $(CFLAGS) $<
mfileio.o: $(ROOT)/mfileio.c
	$(CC) $(CFLAGS) $<
mpost.o: $(ROOT)/mpost.c
	$(CC) $(CFLAGS) $<
mt19937ar.o: $(ROOT)/mt19937ar.c
	$(CC) $(CFLAGS) $<
numbers.o: $(ROOT)/numbers.c
	$(CC) $(CFLAGS) $<
otl_opt.o: $(ROOT)/otl_opt.c
	$(CC) $(CFLAGS) $<
pdfcolor.o: $(ROOT)/pdfcolor.c
	$(CC) $(CFLAGS) $<
pdfdev.o: $(ROOT)/pdfdev.c
	$(CC) $(CFLAGS) $<
pdfdoc.o: $(ROOT)/pdfdoc.c
	$(CC) $(CFLAGS) $<
pdfdraw.o: $(ROOT)/pdfdraw.c
	$(CC) $(CFLAGS) $<
pdfencoding.o: $(ROOT)/pdfencoding.c
	$(CC) $(CFLAGS) $<
pdfencrypt.o: $(ROOT)/pdfencrypt.c
	$(CC) $(CFLAGS) $<
pdffont.o: $(ROOT)/pdffont.c
	$(CC) $(CFLAGS) $<
pdfnames.o: $(ROOT)/pdfnames.c
	$(CC) $(CFLAGS) $<
pdfobj.o: $(ROOT)/pdfobj.c
	$(CC) $(CFLAGS) -DLIBDPX $<
pdfparse.o: $(ROOT)/pdfparse.c
	$(CC) $(CFLAGS) $<
pdfresource.o: $(ROOT)/pdfresource.c
	$(CC) $(CFLAGS) $<
pdfximage.o: $(ROOT)/pdfximage.c
	$(CC) $(CFLAGS) $<
pkfont.o: $(ROOT)/pkfont.c
	$(CC) $(CFLAGS) $<
pngimage.o: $(ROOT)/pngimage.c
	$(CC) $(CFLAGS) $<
pst.o: $(ROOT)/pst.c
	$(CC) $(CFLAGS) $<
pst_obj.o: $(ROOT)/pst_obj.c
	$(CC) $(CFLAGS) $<
sfnt.o: $(ROOT)/sfnt.c
	$(CC) $(CFLAGS) $<
spc_color.o: $(ROOT)/spc_color.c
	$(CC) $(CFLAGS) $<
spc_dvipdfmx.o: $(ROOT)/spc_dvipdfmx.c
	$(CC) $(CFLAGS) $<
spc_dvips.o: $(ROOT)/spc_dvips.c
	$(CC) $(CFLAGS) $<
spc_html.o: $(ROOT)/spc_html.c
	$(CC) $(CFLAGS) $<
spc_misc.o: $(ROOT)/spc_misc.c
	$(CC) $(CFLAGS) $<
spc_pdfm.o: $(ROOT)/spc_pdfm.c
	$(CC) $(CFLAGS) $<
spc_tpic.o: $(ROOT)/spc_tpic.c
	$(CC) $(CFLAGS) $<
spc_util.o: $(ROOT)/spc_util.c
	$(CC) $(CFLAGS) $<
spc_xtx.o: $(ROOT)/spc_xtx.c
	$(CC) $(CFLAGS) $<
specials.o: $(ROOT)/specials.c
	$(CC) $(CFLAGS) $<
subfont.o: $(ROOT)/subfont.c
	$(CC) $(CFLAGS) $<
t1_char.o: $(ROOT)/t1_char.c
	$(CC) $(CFLAGS) $<
t1_load.o: $(ROOT)/t1_load.c
	$(CC) $(CFLAGS) $<
tfm.o: $(ROOT)/tfm.c
	$(CC) $(CFLAGS) $<
truetype.o: $(ROOT)/truetype.c
	$(CC) $(CFLAGS) $<
tt_aux.o: $(ROOT)/tt_aux.c
	$(CC) $(CFLAGS) $<
tt_cmap.o: $(ROOT)/tt_cmap.c
	$(CC) $(CFLAGS) -DLIBDPX $<
tt_glyf.o: $(ROOT)/tt_glyf.c
	$(CC) $(CFLAGS) $<
tt_gsub.o: $(ROOT)/tt_gsub.c
	$(CC) $(CFLAGS) $<
tt_post.o: $(ROOT)/tt_post.c
	$(CC) $(CFLAGS) $<
tt_table.o: $(ROOT)/tt_table.c
	$(CC) $(CFLAGS) $<
type0.o: $(ROOT)/type0.c
	$(CC) $(CFLAGS) -DLIBDPX $<
type1.o: $(ROOT)/type1.c
	$(CC) $(CFLAGS) -DLIBDPX $<
type1c.o: $(ROOT)/type1c.c
	$(CC) $(CFLAGS) $<
unicode.o: $(ROOT)/unicode.c
	$(CC) $(CFLAGS) $<
vf.o: $(ROOT)/vf.c
	$(CC) $(CFLAGS) -DLIBDPX $<
xbb.o: $(ROOT)/xbb.c
	$(CC) $(CFLAGS) $<

clean:
	rm $(wildcard *.o *.a)
