## Makefile.am for the TeX Live subdirectory texk/dvipdfm-x/
##
## Copyright (C) 2009-2014 Peter Breitenlohner <tex-live@tug.org>
## You may freely use, modify and/or distribute this file.
##

AM_CPPFLAGS = $(KPATHSEA_INCLUDES) $(LIBPNG_INCLUDES) $(ZLIB_INCLUDES)
AM_CFLAGS = $(WARNING_CFLAGS)

lib_LIBRARIES = libdpx.a

libdpx_a_CPPFLAGS = $(AM_CPPFLAGS)

libdpx_a_SOURCES = \
	agl.c \
	agl.h \
	bmpimage.c \
	bmpimage.h \
	cff.c \
	cff.h \
cff_dict.c \
	cff_dict.h \
	cff_limits.h \
	cff_stdstr.h \
	cff_types.h \
	cid_basefont.h \
	cid.c \
	cid.h \
	cid_p.h \
	cidtype0.c \
	cidtype0.h \
	cidtype2.c \
	cidtype2.h \
	cmap.h \
cmap.c \
	cmap_p.h \
cmap_read.c \
cmap_read.h \
	cmap_write.c \
	cmap_write.h \
	cs_type2.c \
	cs_type2.h \
	dpxconf.c \
	dpxconf.h \
	dpxcrypt.c \
	dpxcrypt.h \
	dpxfile.c \
	dpxfile.h \
	dpxutil.c \
	dpxutil.h \
	dvi.h dvi.c\
	dvicodes.h \
	dvipdfmx.h \
dvipdfmx.c \
	epdf.c \
	epdf.h \
	error.c \
	error.h fontmap.c fontmap.h\
	jpegimage.c \
	jpegimage.h \
	mem.c \
	mem.h \
	mfileio.c \
	mfileio.h \
	mpost.h mpost.c\
	numbers.c \
	numbers.h \
	otl_conf.c \
	otl_conf.h \
	otl_opt.c \
	otl_opt.h \
	pdfcolor.c \
	pdfcolor.h \
pdfdev.c\
	pdfdev.h \
pdfdoc.c\
	pdfdoc.h \
	pdfdraw.c \
	pdfdraw.h \
	pdfencrypt.c \
	pdfencrypt.h \
	pdfencoding.c \
	pdfencoding.h \
	pdffont.c \
	pdffont.h \
	pdflimits.h \
	pdfnames.h pdfnames.c\
	pdfobj.c \
	pdfobj.h \
	pdfparse.c \
	pdfparse.h \
	pdfresource.c \
	pdfresource.h \
	pdfximage.c \
	pdfximage.h \
	pkfont.h pkfont.c\
	pngimage.c \
	pngimage.h \
	pst.c \
	pst.h \
	pst_obj.c \
	pst_obj.h \
	sfnt.c \
	sfnt.h \
	spc_color.c \
	spc_color.h \
	spc_dvips.c \
	spc_dvips.h \
	spc_html.c \
	spc_html.h \
	spc_misc.c \
	spc_misc.h \
	spc_pdfm.h spc_pdfm.c\
	spc_tpic.c \
	spc_tpic.h \
	spc_util.h \
	spc_util.c \
	spc_xtx.h spc_xtx.c\
	specials.c \
	specials.h \
	subfont.c \
	subfont.h \
	system.h \
	t1_char.h t1_char.c\
	t1_load.c \
	t1_load.h \
	tfm.c \
	tfm.h \
	truetype.c \
	truetype.h \
	tt_aux.c \
	tt_aux.h \
	tt_cmap.c \
	tt_cmap.h \
	tt_glyf.c \
	tt_glyf.h \
	tt_gsub.c \
	tt_gsub.h \
	tt_post.c \
	tt_post.h \
	tt_table.c \
	tt_table.h \
	type0.c \
	type0.h \
	type1.h type1.c\
	type1c.c \
	type1c.h \
	unicode.c \
	unicode.h \
	vf.c \
	vf.h \
	xbb.c

$(libdpx_a_OBJECTS) : config.force

## Data
##
configdatadir = $(datarootdir)/texmf-dist/dvipdfmx
dist_configdata_DATA = data/dvipdfmx.cfg
 
glyphlistdatadir = $(datarootdir)/texmf-dist/fonts/map/glyphlist
dist_glyphlistdata_DATA = data/glyphlist.txt data/pdfglyphlist.txt

mapdatadir = $(datarootdir)/texmf-dist/fonts/map/dvipdfmx
dist_mapdata_DATA = data/cid-x.map

cmapdatadir = $(datarootdir)/texmf-dist/fonts/cmap/dvipdfmx
dist_cmapdata_DATA = data/EUC-UCS2

all-local: config.force

config.force: $(ZLIB_DEPEND) $(LIBPNG_DEPEND) $(KPATHSEA_DEPEND) $(LIBPAPER_DEPEND)
	echo timestamp >config.force
	$(SHELL) ./config.status --recheck

DISTCLEANFILES = config.force

## Rebuild libkpathsea
@KPATHSEA_RULE@
## Rebuild libpng
@LIBPNG_RULE@
## Rebuild libz
@ZLIB_RULE@

## Tests
##
TESTS =
dvipdfmx_tests = dvipdfmx.test dvipdfm-ann.test dvipdfm-bb.test dvipdfm-bkm.test dvipdfm-psz.test
dvipdfmx_tests += dvipdfm-res.test
EXTRA_DIST = $(dvipdfmx_tests)
## Test Data
EXTRA_DIST += tests/dvipdfmx.cfg tests/psfonts.map
EXTRA_DIST += tests/fullmap.dvi tests/fullmap.tex
EXTRA_DIST += tests/image.jpeg tests/imgjpeg.bb tests/imgjpeg.xbb tests/picjpeg.bb tests/picjpeg.xbb
EXTRA_DIST += tests/image.pdf tests/imgpdf.bb tests/imgpdf.xbb tests/picpdf.bb tests/picpdf.xbb
EXTRA_DIST += tests/image.png tests/imgpng.bb tests/imgpng.xbb tests/picpng.bb tests/picpng.xbb
EXTRA_DIST += tests/image.dvi tests/image.tex
EXTRA_DIST += tests/cmr10.pfb tests/cmr10.tfm tests/annot.dvi tests/annot.tex
EXTRA_DIST += tests/bookm.dvi tests/bookm.tex tests/paper.dvi tests/paper.tex
EXTRA_DIST += tests/resrc.dvi tests/resrc.tex
EXTRA_DIST += tests/xbmc.dvi tests/xbmc.tex tests/xbmc10.600pk tests/xbmc10.tfm
DISTCLEANFILES += img*.* pic*.* image*.pdf annot*.pdf bookm*.pdf paper*.pdf resrc*.pdf xbmc*.pdf
