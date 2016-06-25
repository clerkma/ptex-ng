# Copyright 2014, 2015, 2016 Clerk Ma
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.

LOCAL_PATH := $(call my-dir)

# for libz
include $(CLEAR_VARS)

ZLIB_ROOT     := ../libs/zlib/zlib-src
ZLIB_INCLUDES := $(LOCAL_PATH)/../libs/zlib/include
ZLIB_FILES := \
$(ZLIB_ROOT)/adler32.c \
$(ZLIB_ROOT)/compress.c \
$(ZLIB_ROOT)/crc32.c \
$(ZLIB_ROOT)/deflate.c \
$(ZLIB_ROOT)/infback.c \
$(ZLIB_ROOT)/gzclose.c \
$(ZLIB_ROOT)/gzlib.c \
$(ZLIB_ROOT)/gzread.c \
$(ZLIB_ROOT)/gzwrite.c \
$(ZLIB_ROOT)/inffast.c \
$(ZLIB_ROOT)/inflate.c \
$(ZLIB_ROOT)/inftrees.c \
$(ZLIB_ROOT)/trees.c \
$(ZLIB_ROOT)/uncompr.c \
$(ZLIB_ROOT)/zutil.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libz
LOCAL_CFLAGS     := -pie -fPIE -O2
LOCAL_C_INCLUDES := $(ZLIB_INCLUDES)
LOCAL_SRC_FILES  := $(ZLIB_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libpng
include $(CLEAR_VARS)

LIBPNG_ROOT     := ../libs/libpng/libpng-src
LIBPNG_INCLUDES := $(LOCAL_PATH)/../libs/libpng/include
LIBPNG_FILES := \
$(LIBPNG_ROOT)/pngerror.c \
$(LIBPNG_ROOT)/png.c \
$(LIBPNG_ROOT)/pngrio.c \
$(LIBPNG_ROOT)/pngset.c \
$(LIBPNG_ROOT)/pngwrite.c \
$(LIBPNG_ROOT)/pngget.c \
$(LIBPNG_ROOT)/pngpread.c \
$(LIBPNG_ROOT)/pngrtran.c \
$(LIBPNG_ROOT)/pngtrans.c \
$(LIBPNG_ROOT)/pngwtran.c \
$(LIBPNG_ROOT)/pngmem.c \
$(LIBPNG_ROOT)/pngread.c \
$(LIBPNG_ROOT)/pngrutil.c \
$(LIBPNG_ROOT)/pngwio.c \
$(LIBPNG_ROOT)/pngwutil.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libpng
LOCAL_CFLAGS     := -pie -fPIE -DPNG_ARM_NEON_OPT=0 -O2
LOCAL_C_INCLUDES := $(LIBPNG_INCLUDES)
LOCAL_SRC_FILES  := $(LIBPNG_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libpaper
include $(CLEAR_VARS)
LIBPAPER_ROOT    := ../libs/libpaper/libpaper-src
LIBPAPER_INCLUDES:= \
$(LOCAL_PATH)/../libs/libpaper/include \
$(LOCAL_PATH)/../libs/libpaper

LIBPAPER_FILES   := \
$(LIBPAPER_ROOT)/lib/dimen.c \
$(LIBPAPER_ROOT)/lib/paper.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libpaper
LOCAL_CFLAGS     := -pie -fPIE -DHAVE_CONFIG_H -O2
LOCAL_C_INCLUDES := $(LIBPAPER_INCLUDES)
LOCAL_SRC_FILES  := $(LIBPAPER_FILES)

include $(BUILD_STATIC_LIBRARY)

#for freetype2
include $(CLEAR_VARS)

LIBFREETYPE_ROOT    := ../libs/freetype2/freetype-src
LIBFREETYPE_INCLUDES:= \
$(LOCAL_PATH)/../libs/freetype2/freetype-src/include
LIBFREETYPE_FILES   := \
$(LIBFREETYPE_ROOT)/src/base/ftbase.c \
$(LIBFREETYPE_ROOT)/src/base/ftbbox.c \
$(LIBFREETYPE_ROOT)/src/base/ftbitmap.c \
$(LIBFREETYPE_ROOT)/src/base/ftfntfmt.c \
$(LIBFREETYPE_ROOT)/src/base/ftgasp.c \
$(LIBFREETYPE_ROOT)/src/base/ftglyph.c \
$(LIBFREETYPE_ROOT)/src/base/ftinit.c \
$(LIBFREETYPE_ROOT)/src/base/ftstroke.c \
$(LIBFREETYPE_ROOT)/src/base/ftsynth.c \
$(LIBFREETYPE_ROOT)/src/base/ftsystem.c \
$(LIBFREETYPE_ROOT)/src/base/fttype1.c \
$(LIBFREETYPE_ROOT)/src/cff/cff.c \
$(LIBFREETYPE_ROOT)/src/cid/type1cid.c \
$(LIBFREETYPE_ROOT)/src/psaux/psaux.c \
$(LIBFREETYPE_ROOT)/src/pshinter/pshinter.c \
$(LIBFREETYPE_ROOT)/src/psnames/psnames.c \
$(LIBFREETYPE_ROOT)/src/raster/raster.c \
$(LIBFREETYPE_ROOT)/src/smooth/smooth.c \
$(LIBFREETYPE_ROOT)/src/autofit/autofit.c \
$(LIBFREETYPE_ROOT)/src/sfnt/sfnt.c \
$(LIBFREETYPE_ROOT)/src/gzip/ftgzip.c \
$(LIBFREETYPE_ROOT)/src/bdf/bdf.c \
$(LIBFREETYPE_ROOT)/src/pfr/pfr.c \
$(LIBFREETYPE_ROOT)/src/pcf/pcf.c \
$(LIBFREETYPE_ROOT)/src/lzw/ftlzw.c \
$(LIBFREETYPE_ROOT)/src/winfonts/winfnt.c \
$(LIBFREETYPE_ROOT)/src/type42/type42.c \
$(LIBFREETYPE_ROOT)/src/truetype/truetype.c \
$(LIBFREETYPE_ROOT)/src/type1/type1.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libfreetype
LOCAL_CFLAGS     := -pie -fPIE -DHAVE_CONFIG_H -DFT2_BUILD_LIBRARY -O2
LOCAL_C_INCLUDES := $(LIBFREETYPE_INCLUDES)
LOCAL_SRC_FILES  := $(LIBFREETYPE_FILES)

include $(BUILD_STATIC_LIBRARY)

# for kpathsea
include $(CLEAR_VARS)

KPATHSEA_ROOT     := ../texk/kpathsea
KPATHSEA_INCLUDES := $(LOCAL_PATH)/../texk/
KPATHSEA_FILES    := \
$(KPATHSEA_ROOT)/progname.c \
$(KPATHSEA_ROOT)/readable.c \
$(KPATHSEA_ROOT)/rm-suffix.c \
$(KPATHSEA_ROOT)/absolute.c \
$(KPATHSEA_ROOT)/str-list.c \
$(KPATHSEA_ROOT)/atou.c \
$(KPATHSEA_ROOT)/str-llist.c \
$(KPATHSEA_ROOT)/cnf.c \
$(KPATHSEA_ROOT)/tex-file.c \
$(KPATHSEA_ROOT)/concat3.c \
$(KPATHSEA_ROOT)/tex-glyph.c \
$(KPATHSEA_ROOT)/concatn.c \
$(KPATHSEA_ROOT)/tex-hush.c \
$(KPATHSEA_ROOT)/concat.c \
$(KPATHSEA_ROOT)/tex-make.c \
$(KPATHSEA_ROOT)/db.c \
$(KPATHSEA_ROOT)/tilde.c \
$(KPATHSEA_ROOT)/debug.c \
$(KPATHSEA_ROOT)/uppercasify.c \
$(KPATHSEA_ROOT)/dir.c \
$(KPATHSEA_ROOT)/variable.c \
$(KPATHSEA_ROOT)/elt-dirs.c \
$(KPATHSEA_ROOT)/version.c \
$(KPATHSEA_ROOT)/expand.c \
$(KPATHSEA_ROOT)/xbasename.c \
$(KPATHSEA_ROOT)/extend-fname.c \
$(KPATHSEA_ROOT)/xcalloc.c \
$(KPATHSEA_ROOT)/file-p.c \
$(KPATHSEA_ROOT)/xdirname.c \
$(KPATHSEA_ROOT)/find-suffix.c \
$(KPATHSEA_ROOT)/xfopen.c \
$(KPATHSEA_ROOT)/fn.c \
$(KPATHSEA_ROOT)/xfseek.c \
$(KPATHSEA_ROOT)/fontmap.c \
$(KPATHSEA_ROOT)/xfseeko.c \
$(KPATHSEA_ROOT)/getopt1.c \
$(KPATHSEA_ROOT)/xftell.c \
$(KPATHSEA_ROOT)/getopt.c \
$(KPATHSEA_ROOT)/xftello.c \
$(KPATHSEA_ROOT)/hash.c \
$(KPATHSEA_ROOT)/xgetcwd.c \
$(KPATHSEA_ROOT)/kdefault.c \
$(KPATHSEA_ROOT)/xmalloc.c \
$(KPATHSEA_ROOT)/kpathsea.c \
$(KPATHSEA_ROOT)/xopendir.c \
$(KPATHSEA_ROOT)/line.c \
$(KPATHSEA_ROOT)/xputenv.c \
$(KPATHSEA_ROOT)/magstep.c \
$(KPATHSEA_ROOT)/xrealloc.c \
$(KPATHSEA_ROOT)/make-suffix.c \
$(KPATHSEA_ROOT)/xstat.c \
$(KPATHSEA_ROOT)/path-elt.c \
$(KPATHSEA_ROOT)/xstrdup.c \
$(KPATHSEA_ROOT)/pathsearch.c \
$(KPATHSEA_ROOT)/proginit.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libkpathsea
LOCAL_CFLAGS     := -pie -fPIE -Wimplicit -Wreturn-type -Wdeclaration-after-statement -Wno-unknown-pragmas -DMAKE_KPSE_DLL -O2
LOCAL_C_INCLUDES := $(KPATHSEA_INCLUDES)
LOCAL_SRC_FILES  := $(KPATHSEA_FILES)

include $(BUILD_STATIC_LIBRARY)

# for ptexenc
include $(CLEAR_VARS)

PTEXENC_ROOT     := ../texk/ptexenc
PTEXENC_INCLUDES := $(LOCAL_PATH)/../texk/ $(LOCAL_PATH)/../texk/ptexenc
PTEXENC_FILES    := \
$(PTEXENC_ROOT)/kanjicnv.c \
$(PTEXENC_ROOT)/ptexenc.c \
$(PTEXENC_ROOT)/unicode.c \
$(PTEXENC_ROOT)/unicode-jp.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libptexenc
LOCAL_CFLAGS     := -pie -fPIE -Wimplicit -Wreturn-type -Wdeclaration-after-statement -Wno-unknown-pragmas -DMAKE_KPSE_DLL -O2
LOCAL_C_INCLUDES := $(PTEXENC_INCLUDES)
LOCAL_SRC_FILES  := $(PTEXENC_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libdpx
include $(CLEAR_VARS)

LIBDPX_ROOT     := ../texk/libdpx
LIBDPX_INCLUDES := \
$(LOCAL_PATH)/../texk/ \
$(LOCAL_PATH)/../texk/ptexenc \
$(LOCAL_PATH)/../texk/libdpx \
$(LOCAL_PATH)/../libs/zlib/include \
$(LOCAL_PATH)/../libs/libpng/include \
$(LOCAL_PATH)/../libs/libpaper/include

LIBDPX_FILES := \
$(LIBDPX_ROOT)/agl.c \
$(LIBDPX_ROOT)/bmpimage.c \
$(LIBDPX_ROOT)/cff.c \
$(LIBDPX_ROOT)/cff_dict.c \
$(LIBDPX_ROOT)/cid.c \
$(LIBDPX_ROOT)/cidtype0.c \
$(LIBDPX_ROOT)/cidtype2.c \
$(LIBDPX_ROOT)/cmap.c \
$(LIBDPX_ROOT)/cmap_read.c \
$(LIBDPX_ROOT)/cmap_write.c \
$(LIBDPX_ROOT)/cs_type2.c \
$(LIBDPX_ROOT)/dpxconf.c \
$(LIBDPX_ROOT)/dpxcrypt.c \
$(LIBDPX_ROOT)/dpxfile.c \
$(LIBDPX_ROOT)/dpxutil.c \
$(LIBDPX_ROOT)/dvi.c \
$(LIBDPX_ROOT)/dvipdfmx.c \
$(LIBDPX_ROOT)/epdf.c \
$(LIBDPX_ROOT)/error.c \
$(LIBDPX_ROOT)/fontmap.c \
$(LIBDPX_ROOT)/jp2image.c \
$(LIBDPX_ROOT)/jpegimage.c \
$(LIBDPX_ROOT)/mem.c \
$(LIBDPX_ROOT)/mfileio.c \
$(LIBDPX_ROOT)/mpost.c \
$(LIBDPX_ROOT)/numbers.c \
$(LIBDPX_ROOT)/otl_conf.c \
$(LIBDPX_ROOT)/otl_opt.c \
$(LIBDPX_ROOT)/pdfcolor.c \
$(LIBDPX_ROOT)/pdfdev.c \
$(LIBDPX_ROOT)/pdfdoc.c \
$(LIBDPX_ROOT)/pdfdraw.c \
$(LIBDPX_ROOT)/pdfencrypt.c \
$(LIBDPX_ROOT)/pdfencoding.c \
$(LIBDPX_ROOT)/pdffont.c \
$(LIBDPX_ROOT)/pdfnames.c \
$(LIBDPX_ROOT)/pdfobj.c \
$(LIBDPX_ROOT)/pdfparse.c \
$(LIBDPX_ROOT)/pdfresource.c \
$(LIBDPX_ROOT)/pdfximage.c \
$(LIBDPX_ROOT)/pkfont.c \
$(LIBDPX_ROOT)/pngimage.c \
$(LIBDPX_ROOT)/pst.c \
$(LIBDPX_ROOT)/pst_obj.c \
$(LIBDPX_ROOT)/sfnt.c \
$(LIBDPX_ROOT)/spc_color.c \
$(LIBDPX_ROOT)/spc_dvipdfmx.c \
$(LIBDPX_ROOT)/spc_dvips.c \
$(LIBDPX_ROOT)/spc_html.c \
$(LIBDPX_ROOT)/spc_misc.c \
$(LIBDPX_ROOT)/spc_pdfm.c \
$(LIBDPX_ROOT)/spc_tpic.c \
$(LIBDPX_ROOT)/spc_util.c \
$(LIBDPX_ROOT)/spc_xtx.c \
$(LIBDPX_ROOT)/specials.c \
$(LIBDPX_ROOT)/subfont.c \
$(LIBDPX_ROOT)/t1_char.c \
$(LIBDPX_ROOT)/t1_load.c \
$(LIBDPX_ROOT)/tfm.c \
$(LIBDPX_ROOT)/truetype.c \
$(LIBDPX_ROOT)/tt_aux.c \
$(LIBDPX_ROOT)/tt_cmap.c \
$(LIBDPX_ROOT)/tt_glyf.c \
$(LIBDPX_ROOT)/tt_gsub.c \
$(LIBDPX_ROOT)/tt_post.c \
$(LIBDPX_ROOT)/tt_table.c \
$(LIBDPX_ROOT)/type0.c \
$(LIBDPX_ROOT)/type1.c \
$(LIBDPX_ROOT)/type1c.c \
$(LIBDPX_ROOT)/unicode.c \
$(LIBDPX_ROOT)/vf.c \
$(LIBDPX_ROOT)/xbb.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libdpx
LOCAL_CFLAGS     := -pie -fPIE -Wimplicit -Wreturn-type -Wdeclaration-after-statement -Wno-unknown-pragmas -DHAVE_CONFIG_H -DMAKE_KPSE_DLL -O2
LOCAL_C_INCLUDES := $(LIBDPX_INCLUDES)
LOCAL_SRC_FILES  := $(LIBDPX_FILES)

include $(BUILD_STATIC_LIBRARY)

#for libotf
include $(CLEAR_VARS)

LIBOTF_ROOT     := ../texk/ptex-ng/libotf
LIBOTF_INCLUDES := \
$(LOCAL_PATH)/../texk/ptex-ng/libotf \
$(LOCAL_PATH)/../texk/ptex-ng/libotf/src \
$(LOCAL_PATH)/../libs/freetype2/freetype2
LIBOTF_FILES    := \
$(LIBOTF_ROOT)/src/otfdrive.c \
$(LIBOTF_ROOT)/src/otferror.c \
$(LIBOTF_ROOT)/src/otfopen.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libotf
LOCAL_CFLAGS     := -pie -fPIE -DHAVE_CONFIG_H -O2
LOCAL_C_INCLUDES := $(LIBOTF_INCLUDES)
LOCAL_SRC_FILES  := $(LIBOTF_FILES)

include $(BUILD_STATIC_LIBRARY)

#for ptex-ng
include $(CLEAR_VARS)

PTEXNG_ROOT     := ../texk/ptex-ng
PTEXNG_INCLUDES := \
$(LOCAL_PATH)/../texk/ \
$(LOCAL_PATH)/../texk/ptexenc \
$(LOCAL_PATH)/../texk/ptex-ng \
$(LOCAL_PATH)/../libs/freetype2/freetype2 \
$(LOCAL_PATH)/../texk/ptex-ng/libotf/src \
$(LOCAL_PATH)/../libs/zlib/include
PTEXNG_FILES := \
$(PTEXNG_ROOT)/aptex.c \
$(PTEXNG_ROOT)/aptex-src.c \
$(PTEXNG_ROOT)/aptex-synctex.c

LOCAL_ARM_NEON          := false
LOCAL_STATIC_LIBRARIES  := libptexenc libdpx libpng libpaper libz libkpathsea libotf libfreetype
LOCAL_LDLIBS            := -s -lm
LOCAL_MODULE            := aptex
LOCAL_CFLAGS            := -pie -fPIE -Wimplicit -Wreturn-type -Wdeclaration-after-statement -Wno-unknown-pragmas -DHAVE_CONFIG_H -DMAKE_KPSE_DLL -O2
LOCAL_LDFLAGS           += -pie -fPIE
LOCAL_C_INCLUDES        := $(PTEXNG_INCLUDES)
LOCAL_SRC_FILES         := $(PTEXNG_FILES)

include $(BUILD_EXECUTABLE)

#for lsotfea
include $(CLEAR_VARS)

LSOTFEA_ROOT     := ../texk/ptex-ng
LSOTFEA_INCLUDES := \
$(LOCAL_PATH)/../libs/freetype2/freetype2 \
$(LOCAL_PATH)/../texk/ptex-ng/libotf/src
LSOTFEA_FILES := \
$(LSOTFEA_ROOT)/lsotfea.c

LOCAL_ARM_NEON          := false
LOCAL_STATIC_LIBRARIES  := libotf libfreetype
LOCAL_LDLIBS            := -s
LOCAL_MODULE            := lsotfea
LOCAL_CFLAGS            := -pie -fPIE -Wimplicit -Wreturn-type -Wdeclaration-after-statement -Wno-unknown-pragmas -O2
LOCAL_LDFLAGS           += -pie -fPIE
LOCAL_C_INCLUDES        := $(LSOTFEA_INCLUDES)
LOCAL_SRC_FILES         := $(LSOTFEA_FILES)

include $(BUILD_EXECUTABLE)
