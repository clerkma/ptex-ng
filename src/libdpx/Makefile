# makefile for libdpx
CC := gcc
ROOT := ../../texlive/texk/dvipdfm-x
CFLAGS := -c -I. -I./ng \
	-I../../texlive/texk \
	-I../../texlive/libs/libpaper/include \
	-I../../texlive/libs/libpng/include \
	-I../../texlive/libs/zlib/include \
	-DHAVE_CONFIG_H
objects := agl.o bmpimage.o cff.o cff_dict.o cid.o cidtype0.o cidtype2.o cmap.o cmap_read.o cmap_write.o cs_type2.o dpxconf.o dpxcrypt.o dpxfile.o dpxutil.o dvi.o dvipdfmx.o epdf.o error.o fontmap.o jp2image.o jpegimage.o mem.o mfileio.o mpost.o mt19937ar.o numbers.o otl_opt.o pdfcolor.o pdfdev.o pdfdoc.o pdfdraw.o pdfencoding.o pdfencrypt.o pdffont.o pdfnames.o pdfobj.o pdfparse.o pdfresource.o pdfximage.o pkfont.o pngimage.o pst.o pst_obj.o sfnt.o spc_color.o spc_dvipdfmx.o spc_dvips.o spc_html.o spc_misc.o spc_pdfm.o spc_tpic.o spc_util.o spc_xtx.o specials.o subfont.o t1_char.o t1_load.o tfm.o truetype.o tt_aux.o tt_cmap.o tt_glyf.o tt_gsub.o tt_post.o tt_table.o type0.o type1.o type1c.o unicode.o vf.o xbb.o

libdpx.a: $(objects)
	ar cru libdpx.a $(objects)

agl.o: $(ROOT)/agl.c
	$(CC) $(CFLAGS) $(ROOT)/agl.c
bmpimage.o: $(ROOT)/bmpimage.c
	$(CC) $(CFLAGS) $(ROOT)/bmpimage.c
cff.o: $(ROOT)/cff.c
	$(CC) $(CFLAGS) $(ROOT)/cff.c
cff_dict.o: $(ROOT)/cff_dict.c
	$(CC) $(CFLAGS) $(ROOT)/cff_dict.c
cid.o: $(ROOT)/cid.c
	$(CC) $(CFLAGS) $(ROOT)/cid.c
cidtype0.o: $(ROOT)/cidtype0.c
	$(CC) $(CFLAGS) $(ROOT)/cidtype0.c
cidtype2.o: $(ROOT)/cidtype2.c
	$(CC) $(CFLAGS) $(ROOT)/cidtype2.c
cmap.o: $(ROOT)/cmap.c
	$(CC) $(CFLAGS) $(ROOT)/cmap.c
cmap_read.o: $(ROOT)/cmap_read.c
	$(CC) $(CFLAGS) $(ROOT)/cmap_read.c
cmap_write.o: $(ROOT)/cmap_write.c
	$(CC) $(CFLAGS) $(ROOT)/cmap_write.c
cs_type2.o: $(ROOT)/cs_type2.c
	$(CC) $(CFLAGS) $(ROOT)/cs_type2.c
dpxconf.o: $(ROOT)/dpxconf.c
	$(CC) $(CFLAGS) $(ROOT)/dpxconf.c
dpxcrypt.o: $(ROOT)/dpxcrypt.c
	$(CC) $(CFLAGS) $(ROOT)/dpxcrypt.c
dpxfile.o: $(ROOT)/dpxfile.c
	$(CC) $(CFLAGS) $(ROOT)/dpxfile.c
dpxutil.o: $(ROOT)/dpxutil.c
	$(CC) $(CFLAGS) $(ROOT)/dpxutil.c
dvi.o: $(ROOT)/dvi.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/dvi.c
dvipdfmx.o: $(ROOT)/dvipdfmx.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/dvipdfmx.c
epdf.o: $(ROOT)/epdf.c
	$(CC) $(CFLAGS) $(ROOT)/epdf.c
error.o: $(ROOT)/error.c
	$(CC) $(CFLAGS) $(ROOT)/error.c
fontmap.o: $(ROOT)/fontmap.c
	$(CC) $(CFLAGS) $(ROOT)/fontmap.c
jp2image.o: $(ROOT)/jp2image.c
	$(CC) $(CFLAGS) $(ROOT)/jp2image.c
jpegimage.o: $(ROOT)/jpegimage.c
	$(CC) $(CFLAGS) $(ROOT)/jpegimage.c
mem.o: $(ROOT)/mem.c
	$(CC) $(CFLAGS) $(ROOT)/mem.c
mfileio.o: $(ROOT)/mfileio.c
	$(CC) $(CFLAGS) $(ROOT)/mfileio.c
mpost.o: $(ROOT)/mpost.c
	$(CC) $(CFLAGS) $(ROOT)/mpost.c
mt19937ar.o: $(ROOT)/mt19937ar.c
	$(CC) $(CFLAGS) $(ROOT)/mt19937ar.c
numbers.o: $(ROOT)/numbers.c
	$(CC) $(CFLAGS) $(ROOT)/numbers.c
otl_opt.o: $(ROOT)/otl_opt.c
	$(CC) $(CFLAGS) $(ROOT)/otl_opt.c
pdfcolor.o: $(ROOT)/pdfcolor.c
	$(CC) $(CFLAGS) $(ROOT)/pdfcolor.c
pdfdev.o: $(ROOT)/pdfdev.c
	$(CC) $(CFLAGS) $(ROOT)/pdfdev.c
pdfdoc.o: $(ROOT)/pdfdoc.c
	$(CC) $(CFLAGS) $(ROOT)/pdfdoc.c
pdfdraw.o: $(ROOT)/pdfdraw.c
	$(CC) $(CFLAGS) $(ROOT)/pdfdraw.c
pdfencoding.o: $(ROOT)/pdfencoding.c
	$(CC) $(CFLAGS) $(ROOT)/pdfencoding.c
pdfencrypt.o: $(ROOT)/pdfencrypt.c
	$(CC) $(CFLAGS) $(ROOT)/pdfencrypt.c
pdffont.o: $(ROOT)/pdffont.c
	$(CC) $(CFLAGS) $(ROOT)/pdffont.c
pdfnames.o: $(ROOT)/pdfnames.c
	$(CC) $(CFLAGS) $(ROOT)/pdfnames.c
pdfobj.o: $(ROOT)/pdfobj.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/pdfobj.c
pdfparse.o: $(ROOT)/pdfparse.c
	$(CC) $(CFLAGS) $(ROOT)/pdfparse.c
pdfresource.o: $(ROOT)/pdfresource.c
	$(CC) $(CFLAGS) $(ROOT)/pdfresource.c
pdfximage.o: $(ROOT)/pdfximage.c
	$(CC) $(CFLAGS) $(ROOT)/pdfximage.c
pkfont.o: $(ROOT)/pkfont.c
	$(CC) $(CFLAGS) $(ROOT)/pkfont.c
pngimage.o: $(ROOT)/pngimage.c
	$(CC) $(CFLAGS) $(ROOT)/pngimage.c
pst.o: $(ROOT)/pst.c
	$(CC) $(CFLAGS) $(ROOT)/pst.c
pst_obj.o: $(ROOT)/pst_obj.c
	$(CC) $(CFLAGS) $(ROOT)/pst_obj.c
sfnt.o: $(ROOT)/sfnt.c
	$(CC) $(CFLAGS) $(ROOT)/sfnt.c
spc_color.o: $(ROOT)/spc_color.c
	$(CC) $(CFLAGS) $(ROOT)/spc_color.c
spc_dvipdfmx.o: $(ROOT)/spc_dvipdfmx.c
	$(CC) $(CFLAGS) $(ROOT)/spc_dvipdfmx.c
spc_dvips.o: $(ROOT)/spc_dvips.c
	$(CC) $(CFLAGS) $(ROOT)/spc_dvips.c
spc_html.o: $(ROOT)/spc_html.c
	$(CC) $(CFLAGS) $(ROOT)/spc_html.c
spc_misc.o: $(ROOT)/spc_misc.c
	$(CC) $(CFLAGS) $(ROOT)/spc_misc.c
spc_pdfm.o: $(ROOT)/spc_pdfm.c
	$(CC) $(CFLAGS) $(ROOT)/spc_pdfm.c
spc_tpic.o: $(ROOT)/spc_tpic.c
	$(CC) $(CFLAGS) $(ROOT)/spc_tpic.c
spc_util.o: $(ROOT)/spc_util.c
	$(CC) $(CFLAGS) $(ROOT)/spc_util.c
spc_xtx.o: $(ROOT)/spc_xtx.c
	$(CC) $(CFLAGS) $(ROOT)/spc_xtx.c
specials.o: $(ROOT)/specials.c
	$(CC) $(CFLAGS) $(ROOT)/specials.c
subfont.o: $(ROOT)/subfont.c
	$(CC) $(CFLAGS) $(ROOT)/subfont.c
t1_char.o: $(ROOT)/t1_char.c
	$(CC) $(CFLAGS) $(ROOT)/t1_char.c
t1_load.o: $(ROOT)/t1_load.c
	$(CC) $(CFLAGS) $(ROOT)/t1_load.c
tfm.o: $(ROOT)/tfm.c
	$(CC) $(CFLAGS) $(ROOT)/tfm.c
truetype.o: $(ROOT)/truetype.c
	$(CC) $(CFLAGS) $(ROOT)/truetype.c
tt_aux.o: $(ROOT)/tt_aux.c
	$(CC) $(CFLAGS) $(ROOT)/tt_aux.c
tt_cmap.o: $(ROOT)/tt_cmap.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/tt_cmap.c
tt_glyf.o: $(ROOT)/tt_glyf.c
	$(CC) $(CFLAGS) $(ROOT)/tt_glyf.c
tt_gsub.o: $(ROOT)/tt_gsub.c
	$(CC) $(CFLAGS) $(ROOT)/tt_gsub.c
tt_post.o: $(ROOT)/tt_post.c
	$(CC) $(CFLAGS) $(ROOT)/tt_post.c
tt_table.o: $(ROOT)/tt_table.c
	$(CC) $(CFLAGS) $(ROOT)/tt_table.c
type0.o: $(ROOT)/type0.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/type0.c
type1.o: $(ROOT)/type1.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/type1.c
type1c.o: $(ROOT)/type1c.c
	$(CC) $(CFLAGS) $(ROOT)/type1c.c
unicode.o: $(ROOT)/unicode.c
	$(CC) $(CFLAGS) $(ROOT)/unicode.c
vf.o: $(ROOT)/vf.c
	$(CC) $(CFLAGS) -DLIBDPX $(ROOT)/vf.c
xbb.o: $(ROOT)/xbb.c
	$(CC) $(CFLAGS) $(ROOT)/xbb.c

clean:
	rm $(wildcard *.o *.a)
