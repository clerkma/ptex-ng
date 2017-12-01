# Copyright 2014, 2015, 2016, 2017 Clerk Ma
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

ZLIB_ROOT     := ../texlive/libs/zlib/zlib-src
ZLIB_INCLUDES := $(LOCAL_PATH)/zlib
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

LIBPNG_ROOT     := ../texlive/libs/libpng/libpng-src
LIBPNG_INCLUDES := \
$(LOCAL_PATH)/zlib \
$(LOCAL_PATH)/../texlive/libs/zlib/zlib-src \
$(LOCAL_PATH)/../texlive/libs/libpng/libpng-src

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
LIBPAPER_ROOT    := ../texlive/libs/libpaper/libpaper-src
LIBPAPER_INCLUDES:= \
$(LOCAL_PATH)/libpaper \
$(LOCAL_PATH)/../texlive/libs/libpaper/libpaper-src

LIBPAPER_FILES   := \
$(LIBPAPER_ROOT)/lib/dimen.c \
$(LIBPAPER_ROOT)/lib/paper.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libpaper
LOCAL_CFLAGS     := -pie -fPIE -DHAVE_CONFIG_H -O2
LOCAL_C_INCLUDES := $(LIBPAPER_INCLUDES)
LOCAL_SRC_FILES  := $(LIBPAPER_FILES)

include $(BUILD_STATIC_LIBRARY)

# for freetype2
include $(CLEAR_VARS)

LIBFREETYPE_ROOT    := ../texlive/libs/freetype2/freetype-src
LIBFREETYPE_INCLUDES:= \
$(LOCAL_PATH)/../texlive/libs/freetype2/freetype-src/include
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
LOCAL_CFLAGS     := -pie -fPIE -DFT2_BUILD_LIBRARY -O2
LOCAL_C_INCLUDES := $(LIBFREETYPE_INCLUDES)
LOCAL_SRC_FILES  := $(LIBFREETYPE_FILES)

include $(BUILD_STATIC_LIBRARY)

# for kpathsea
include $(CLEAR_VARS)

KPATHSEA_ROOT     := ../texlive/texk/kpathsea
KPATHSEA_INCLUDES := \
$(LOCAL_PATH)/kpathsea \
$(LOCAL_PATH)/../texlive/texk/
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
LOCAL_CFLAGS     := -pie -fPIE -Wimplicit -DMAKE_KPSE_DLL -O2
LOCAL_C_INCLUDES := $(KPATHSEA_INCLUDES)
LOCAL_SRC_FILES  := $(KPATHSEA_FILES)

include $(BUILD_STATIC_LIBRARY)

# for ptexenc
include $(CLEAR_VARS)

PTEXENC_ROOT     := ../texlive/texk/ptexenc
PTEXENC_INCLUDES := \
$(LOCAL_PATH)/ \
$(LOCAL_PATH)/../texlive/texk/ \
$(LOCAL_PATH)/../texlive/texk/ptexenc
PTEXENC_FILES    := \
$(PTEXENC_ROOT)/kanjicnv.c \
$(PTEXENC_ROOT)/ptexenc.c \
$(PTEXENC_ROOT)/unicode.c \
$(PTEXENC_ROOT)/unicode-jp.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libptexenc
LOCAL_CFLAGS     := -pie -fPIE -DMAKE_KPSE_DLL -O2
LOCAL_C_INCLUDES := $(PTEXENC_INCLUDES)
LOCAL_SRC_FILES  := $(PTEXENC_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libpixman
include $(CLEAR_VARS)

PIXMAN_ROOT     := ../texlive/libs/pixman/pixman-src/pixman
PIXMAN_INCLUDES := \
$(LOCAL_PATH)/pixman \
$(LOCAL_PATH)/../texlive/libs/pixman/pixman-src/pixman
PIXMAN_FILES    := \
$(PIXMAN_ROOT)/pixman.c \
$(PIXMAN_ROOT)/pixman-access.c	\
$(PIXMAN_ROOT)/pixman-access-accessors.c	\
$(PIXMAN_ROOT)/pixman-bits-image.c \
$(PIXMAN_ROOT)/pixman-combine32.c \
$(PIXMAN_ROOT)/pixman-combine-float.c \
$(PIXMAN_ROOT)/pixman-conical-gradient.c \
$(PIXMAN_ROOT)/pixman-filter.c \
$(PIXMAN_ROOT)/pixman-x86.c \
$(PIXMAN_ROOT)/pixman-mips.c \
$(PIXMAN_ROOT)/pixman-arm.c \
$(PIXMAN_ROOT)/pixman-ppc.c \
$(PIXMAN_ROOT)/pixman-edge.c \
$(PIXMAN_ROOT)/pixman-edge-accessors.c \
$(PIXMAN_ROOT)/pixman-fast-path.c \
$(PIXMAN_ROOT)/pixman-glyph.c \
$(PIXMAN_ROOT)/pixman-general.c \
$(PIXMAN_ROOT)/pixman-gradient-walker.c \
$(PIXMAN_ROOT)/pixman-image.c \
$(PIXMAN_ROOT)/pixman-implementation.c \
$(PIXMAN_ROOT)/pixman-linear-gradient.c \
$(PIXMAN_ROOT)/pixman-matrix.c \
$(PIXMAN_ROOT)/pixman-noop.c \
$(PIXMAN_ROOT)/pixman-radial-gradient.c \
$(PIXMAN_ROOT)/pixman-region16.c \
$(PIXMAN_ROOT)/pixman-region32.c \
$(PIXMAN_ROOT)/pixman-solid-fill.c \
$(PIXMAN_ROOT)/pixman-timer.c \
$(PIXMAN_ROOT)/pixman-trap.c \
$(PIXMAN_ROOT)/pixman-utils.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libpixman
LOCAL_CFLAGS     := -pie -fPIE -DPIXMAN_NO_TLS -DHAVE_CONFIG_H -O2
LOCAL_C_INCLUDES := $(PIXMAN_INCLUDES)
LOCAL_SRC_FILES  := $(PIXMAN_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libcairo
include $(CLEAR_VARS)

CAIRO_ROOT     := ../texlive/libs/cairo/cairo-src/src
CAIRO_INCLUDES := \
$(LOCAL_PATH)/cairo \
$(LOCAL_PATH)/pixman \
$(LOCAL_PATH)/../texlive/libs/pixman/pixman-src/pixman
CAIRO_FILES    := \
$(CAIRO_ROOT)/cairo-analysis-surface.c \
$(CAIRO_ROOT)/cairo-arc.c \
$(CAIRO_ROOT)/cairo-array.c \
$(CAIRO_ROOT)/cairo-atomic.c \
$(CAIRO_ROOT)/cairo-base64-stream.c \
$(CAIRO_ROOT)/cairo-base85-stream.c \
$(CAIRO_ROOT)/cairo-bentley-ottmann.c \
$(CAIRO_ROOT)/cairo-bentley-ottmann-rectangular.c \
$(CAIRO_ROOT)/cairo-bentley-ottmann-rectilinear.c \
$(CAIRO_ROOT)/cairo-botor-scan-converter.c \
$(CAIRO_ROOT)/cairo-boxes.c \
$(CAIRO_ROOT)/cairo-boxes-intersect.c \
$(CAIRO_ROOT)/cairo.c \
$(CAIRO_ROOT)/cairo-cache.c \
$(CAIRO_ROOT)/cairo-clip.c \
$(CAIRO_ROOT)/cairo-clip-boxes.c \
$(CAIRO_ROOT)/cairo-clip-polygon.c \
$(CAIRO_ROOT)/cairo-clip-region.c \
$(CAIRO_ROOT)/cairo-clip-surface.c \
$(CAIRO_ROOT)/cairo-color.c \
$(CAIRO_ROOT)/cairo-composite-rectangles.c \
$(CAIRO_ROOT)/cairo-compositor.c \
$(CAIRO_ROOT)/cairo-contour.c \
$(CAIRO_ROOT)/cairo-damage.c \
$(CAIRO_ROOT)/cairo-debug.c \
$(CAIRO_ROOT)/cairo-default-context.c \
$(CAIRO_ROOT)/cairo-device.c \
$(CAIRO_ROOT)/cairo-error.c \
$(CAIRO_ROOT)/cairo-fallback-compositor.c \
$(CAIRO_ROOT)/cairo-fixed.c \
$(CAIRO_ROOT)/cairo-font-face.c \
$(CAIRO_ROOT)/cairo-font-face-twin.c \
$(CAIRO_ROOT)/cairo-font-face-twin-data.c \
$(CAIRO_ROOT)/cairo-font-options.c \
$(CAIRO_ROOT)/cairo-freelist.c \
$(CAIRO_ROOT)/cairo-freed-pool.c \
$(CAIRO_ROOT)/cairo-gstate.c \
$(CAIRO_ROOT)/cairo-hash.c \
$(CAIRO_ROOT)/cairo-hull.c \
$(CAIRO_ROOT)/cairo-image-compositor.c \
$(CAIRO_ROOT)/cairo-image-info.c \
$(CAIRO_ROOT)/cairo-image-source.c \
$(CAIRO_ROOT)/cairo-image-surface.c \
$(CAIRO_ROOT)/cairo-line.c \
$(CAIRO_ROOT)/cairo-lzw.c \
$(CAIRO_ROOT)/cairo-matrix.c \
$(CAIRO_ROOT)/cairo-mask-compositor.c \
$(CAIRO_ROOT)/cairo-mesh-pattern-rasterizer.c \
$(CAIRO_ROOT)/cairo-mempool.c \
$(CAIRO_ROOT)/cairo-misc.c \
$(CAIRO_ROOT)/cairo-mono-scan-converter.c \
$(CAIRO_ROOT)/cairo-mutex.c \
$(CAIRO_ROOT)/cairo-no-compositor.c \
$(CAIRO_ROOT)/cairo-observer.c \
$(CAIRO_ROOT)/cairo-output-stream.c \
$(CAIRO_ROOT)/cairo-paginated-surface.c \
$(CAIRO_ROOT)/cairo-path-bounds.c \
$(CAIRO_ROOT)/cairo-path.c \
$(CAIRO_ROOT)/cairo-path-fill.c \
$(CAIRO_ROOT)/cairo-path-fixed.c \
$(CAIRO_ROOT)/cairo-path-in-fill.c \
$(CAIRO_ROOT)/cairo-path-stroke.c \
$(CAIRO_ROOT)/cairo-path-stroke-boxes.c \
$(CAIRO_ROOT)/cairo-path-stroke-polygon.c \
$(CAIRO_ROOT)/cairo-path-stroke-traps.c \
$(CAIRO_ROOT)/cairo-path-stroke-tristrip.c \
$(CAIRO_ROOT)/cairo-pattern.c \
$(CAIRO_ROOT)/cairo-pen.c \
$(CAIRO_ROOT)/cairo-polygon.c \
$(CAIRO_ROOT)/cairo-polygon-intersect.c \
$(CAIRO_ROOT)/cairo-polygon-reduce.c \
$(CAIRO_ROOT)/cairo-raster-source-pattern.c \
$(CAIRO_ROOT)/cairo-recording-surface.c \
$(CAIRO_ROOT)/cairo-rectangle.c \
$(CAIRO_ROOT)/cairo-rectangular-scan-converter.c \
$(CAIRO_ROOT)/cairo-region.c \
$(CAIRO_ROOT)/cairo-rtree.c \
$(CAIRO_ROOT)/cairo-scaled-font.c \
$(CAIRO_ROOT)/cairo-shape-mask-compositor.c \
$(CAIRO_ROOT)/cairo-slope.c \
$(CAIRO_ROOT)/cairo-spans.c \
$(CAIRO_ROOT)/cairo-spans-compositor.c \
$(CAIRO_ROOT)/cairo-spline.c \
$(CAIRO_ROOT)/cairo-stroke-dash.c \
$(CAIRO_ROOT)/cairo-stroke-style.c \
$(CAIRO_ROOT)/cairo-surface.c \
$(CAIRO_ROOT)/cairo-surface-clipper.c \
$(CAIRO_ROOT)/cairo-surface-fallback.c \
$(CAIRO_ROOT)/cairo-surface-observer.c \
$(CAIRO_ROOT)/cairo-surface-offset.c \
$(CAIRO_ROOT)/cairo-surface-snapshot.c \
$(CAIRO_ROOT)/cairo-surface-subsurface.c \
$(CAIRO_ROOT)/cairo-surface-wrapper.c \
$(CAIRO_ROOT)/cairo-time.c \
$(CAIRO_ROOT)/cairo-tor-scan-converter.c \
$(CAIRO_ROOT)/cairo-tor22-scan-converter.c \
$(CAIRO_ROOT)/cairo-clip-tor-scan-converter.c \
$(CAIRO_ROOT)/cairo-toy-font-face.c \
$(CAIRO_ROOT)/cairo-traps.c \
$(CAIRO_ROOT)/cairo-tristrip.c \
$(CAIRO_ROOT)/cairo-traps-compositor.c \
$(CAIRO_ROOT)/cairo-unicode.c \
$(CAIRO_ROOT)/cairo-user-font.c \
$(CAIRO_ROOT)/cairo-version.c \
$(CAIRO_ROOT)/cairo-wideint.c \
$(CAIRO_ROOT)/cairo-pdf-surface.c \
$(CAIRO_ROOT)/cairo-pdf-operators.c \
$(CAIRO_ROOT)/cairo-pdf-shading.c \
$(CAIRO_ROOT)/cairo-cff-subset.c \
$(CAIRO_ROOT)/cairo-scaled-font-subsets.c \
$(CAIRO_ROOT)/cairo-truetype-subset.c \
$(CAIRO_ROOT)/cairo-type1-fallback.c \
$(CAIRO_ROOT)/cairo-type1-glyph-names.c \
$(CAIRO_ROOT)/cairo-type1-subset.c \
$(CAIRO_ROOT)/cairo-type3-glyph-surface.c \
$(CAIRO_ROOT)/cairo-deflate-stream.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libcairo
LOCAL_CFLAGS     := -pie -fPIE -Wno-enum-conversion -DHAVE_CONFIG_H -DCAIRO_NO_MUTEX -O2
LOCAL_C_INCLUDES := $(CAIRO_INCLUDES)
LOCAL_SRC_FILES  := $(CAIRO_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libdpx
include $(CLEAR_VARS)

LIBDPX_ROOT     := ../src/libdpx
LIBDPX_INCLUDES := \
$(LOCAL_PATH)/ \
$(LOCAL_PATH)/zlib \
$(LOCAL_PATH)/../texlive/texk/ \
$(LOCAL_PATH)/../texlive/texk/ptexenc \
$(LOCAL_PATH)/../src/libdpx/ \
$(LOCAL_PATH)/../src/libdpx/ng \
$(LOCAL_PATH)/../texlive/libs/zlib/zlib-src \
$(LOCAL_PATH)/../texlive/libs/libpng/libpng-src \
$(LOCAL_PATH)/../texlive/libs/libpaper/libpaper-src/lib

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
LOCAL_CFLAGS     := -pie -fPIE -DHAVE_CONFIG_H -DLIBDPX -DMAKE_KPSE_DLL -O2
LOCAL_C_INCLUDES := $(LIBDPX_INCLUDES)
LOCAL_SRC_FILES  := $(LIBDPX_FILES)

include $(BUILD_STATIC_LIBRARY)

# for libotf
include $(CLEAR_VARS)

LIBOTF_ROOT     := ../src/libotf
LIBOTF_INCLUDES := \
$(LOCAL_PATH)/../src/libotf \
$(LOCAL_PATH)/../src/libotf/src \
$(LOCAL_PATH)/../texlive/libs/freetype2/freetype-src/include
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

# for libyaml
include $(CLEAR_VARS)

LIBYAML_ROOT     := ../src/libyaml
LIBYAML_INCLUDES := \
$(LOCAL_PATH)/../src/libyaml/win32 \
$(LOCAL_PATH)/../src/libyaml/include
LIBYAML_FILES    := \
$(LIBYAML_ROOT)/src/api.c \
$(LIBYAML_ROOT)/src/dumper.c \
$(LIBYAML_ROOT)/src/emitter.c \
$(LIBYAML_ROOT)/src/loader.c \
$(LIBYAML_ROOT)/src/parser.c \
$(LIBYAML_ROOT)/src/reader.c \
$(LIBYAML_ROOT)/src/scanner.c \
$(LIBYAML_ROOT)/src/writer.c

LOCAL_ARM_NEON   := false
LOCAL_MODULE     := libyaml
LOCAL_CFLAGS     := -pie -fPIE -DHAVE_CONFIG_H -DYAML_DECLARE_STATIC -O2
LOCAL_C_INCLUDES := $(LIBYAML_INCLUDES)
LOCAL_SRC_FILES  := $(LIBYAML_FILES)

include $(BUILD_STATIC_LIBRARY)

# for ptex-ng
include $(CLEAR_VARS)

PTEXNG_ROOT     := ../src
PTEXNG_INCLUDES := \
$(LOCAL_PATH)/ \
$(LOCAL_PATH)/../texlive/texk/ \
$(LOCAL_PATH)/../texlive/texk/ptexenc \
$(LOCAL_PATH)/../src \
$(LOCAL_PATH)/../texlive/libs/freetype2/freetype-src/include \
$(LOCAL_PATH)/../texlive/libs/freetype2/freetype2 \
$(LOCAL_PATH)/../src/libotf/src \
$(LOCAL_PATH)/../texlive/libs/zlib/include \
$(LOCAL_PATH)/cairo \
$(LOCAL_PATH)/../texlive/libs/cairo/cairo-src/src

PTEXNG_FILES := \
$(PTEXNG_ROOT)/aptex.c \
$(PTEXNG_ROOT)/aptex-cairo-visual-debug.c \
$(PTEXNG_ROOT)/aptex-opentype.c \
$(PTEXNG_ROOT)/aptex-unicode.c \
$(PTEXNG_ROOT)/aptex-src.c

LOCAL_ARM_NEON          := false
LOCAL_STATIC_LIBRARIES  := libptexenc libdpx libpng libpaper libcairo libpixman libz libkpathsea libotf libfreetype libyaml
LOCAL_LDLIBS            := -s -lm
LOCAL_MODULE            := aptex
LOCAL_CFLAGS            := -pie -fPIE -DHAVE_CONFIG_H -DMAKE_KPSE_DLL -O2
LOCAL_LDFLAGS           += -pie -fPIE
LOCAL_C_INCLUDES        := $(PTEXNG_INCLUDES)
LOCAL_SRC_FILES         := $(PTEXNG_FILES)

include $(BUILD_EXECUTABLE)

