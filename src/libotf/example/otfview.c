/* otfview.c -- View glyphs of OpenType fonts.

Copyright (C) 2003, 2004, 2005, 2006, 2008, 2009, 2010
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H15PRO167
Copyright (C) 2012, 2013, 2014, 2015  K. Handa  <handa@gnu.org>

This file is part of libotf.

Libotf is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

Libotf is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library, in a file named COPYING; if not,
write to the Free Software Foundation, Inc., 59 Temple Place, Suite
330, Boston, MA 02111-1307, USA.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <libgen.h>

#include "config.h"
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef HAVE_X11_XAW_COMMAND_H

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include <otf.h>

#define CAST_FROM_XTPOINTER(TYPE, DATA, VAR)	\
  do {						\
    long TYPE temp = (long TYPE) (DATA);	\
    (VAR) = temp;				\
  } while (0)

#define XtAddCallbackWithCast(TYPE, W, PROC, VAR)		\
  do {								\
    long TYPE temp = (long TYPE) (VAR);				\
    XtAddCallback (W, XtNcallback, PROC, (XtPointer) temp);	\
  } while (0)


#define DEFAULT_PIXEL_SIZE 30
int pixel_size;

#define DEFAULT_FONT_NAME "6x13"
XFontStruct *font;
#define FONT_HEIGHT (font->ascent + font->descent)
#define FONT_ASCENT (font->ascent)
#define FONT_DESCENT (font->descent)
#define FONT_WIDTH (font->max_bounds.width)

XtAppContext context;
/* Widget structure.
   +--- frame (form) -------------------------+
   | +--- command_area (box) ---------------+ |
   | | quit dump charmap ...                | |
   | +--------------------------------------+ |
   | +---- navi_area (box) -----------------+ |
   | | FIRST PREV prev range next NEXT LAST | |
   | +--------------------------------------+ |
   | +--- glyph_area (form) ----------------+ |
   | | idxh[0] glyph[0]    ...    glyph[15] | |
   | |   ...                        ...     | |
   | | idxh[7] glyph[112]  ...    glyph[127]| |
   | |         idxl[0]     ...    idxl[15]  | |
   | +--------------------------------------+ |
   | +--- script_area (box) ----------------+ |
   | | script(langsys) DFLT ...             | |
   | +--------------------------------------+ |
   | +---- uvs_area (box) (optional) -------+ |
   | | uvs[?].w ...                         | |
   | +--------------------------------------+ |
   | +--- render_area (form) ---------------+ |
   | | clear del bidi alt_subst             | |
   | | +--- raw (box) --------------------+ | |
   | | | raw_label raw_image              | | |
   | | +----------------------------------+ | |
   | | GSUB all none features...            | |
   | | GPOS all none features...            | |
   | | +--- seq (box) --------------------+ | |
   | | | seq_label seq_image              | | |
   | | +----------------------------------+ | |
   | | +--- code (box) -------------------+ | |
   | | | code_label code_list ...         | | |   
   | | +----------------------------------+ | |
   | +--------------------------------------+ |
   +------------------------------------------+ */
Widget shell, frame;
Widget command_area, quit, dump, *charmap;
Widget navi_area, FIRST, PREV, prev, range, next, NEXT, LAST;
Widget glyph_area, glyph[128], index_label[8];
Widget uvs_area, uvs_label;
Widget render_area, clear, del, bidi, alt_subst, raw, seq, code;
Widget raw_label, raw_image, seq_label, seq_image, code_label, code_list;
unsigned long foreground, background;

typedef struct
{
  OTF_Tag tag;
  int on;
  Widget w;
} FeatureElement;

typedef struct
{
  char *label;
  OTF_GSUB_GPOS *gsub_gpos;
  OTF_LangSys *langsys;
  int num_features;
  FeatureElement *features;
  Widget parent;
} FeatureRec;

FeatureRec gsub, gpos;

/* Currently selected script and langsys.  */
OTF_Tag script_tag, langsys_tag;

int glyph_char[128];

Display *display;
GC gc, gc_set, gc_or, gc_inv;

typedef struct {
  Pixmap pixmap;
  unsigned width, height;
  int x, y;
  int advance;
} BitmapRec;

BitmapRec bitmap[0x110000];

int render_width, render_height;
Pixmap raw_pixmap, seq_pixmap, gsub_pixmap, gpos_pixmap;
Pixmap none_pixmap;

FT_Face face;

struct {
  int platform_id;
  int encoding_id;
  char name[20];
} charmap_rec[10];

int charmap_index;

int reversed;
int do_alternate_subst;
unsigned glyph_width, glyph_height;
int glyph_x, glyph_y;
int glyph_index;

struct {
  int n_glyphs;
  int glyphs[64];
  int codes[64];
} glyph_rec;

OTF_EncodingSubtable14 *sub14 = NULL;

struct {
  Widget w;
  int c;
} uvs[256];

OTF *otf;
char *filename;
int fontindex;

void
create_pixmap (int index)
{
  int err = FT_Load_Glyph (face, index, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
  XImage ximage;
  Pixmap pixmap;
  
  if (err)
    {
      bitmap[index].pixmap = none_pixmap;
      return;
    }
  ximage.height = face->glyph->bitmap.rows;
  ximage.width = face->glyph->bitmap.width;
  ximage.depth = 1;
  ximage.bits_per_pixel = 1;
  ximage.xoffset = 0;
  ximage.format = XYPixmap;
  ximage.data = (char *) face->glyph->bitmap.buffer;
  ximage.byte_order = MSBFirst;
  ximage.bitmap_unit = 8;
  ximage.bitmap_bit_order = MSBFirst;
  ximage.bitmap_pad = 8;
  ximage.bytes_per_line = face->glyph->bitmap.pitch;
  XInitImage (&ximage);
  pixmap = XCreatePixmap (display, DefaultRootWindow (display),
			  glyph_width, glyph_height, 1);
  XFillRectangle (display, pixmap, gc, 0, 0, glyph_width, glyph_height);
  XPutImage (display, pixmap, gc, &ximage, 0, 0,
	     glyph_x + face->glyph->bitmap_left,
	     glyph_y - face->glyph->bitmap_top,
	     ximage.width, ximage.height);
  bitmap[index].pixmap = pixmap;
  bitmap[index].width = ximage.width;
  bitmap[index].height = ximage.height;
  bitmap[index].x = face->glyph->bitmap_left;
  bitmap[index].y = - face->glyph->bitmap_top;
  bitmap[index].advance = face->glyph->metrics.horiAdvance >> 6;
}

void
update_glyph_area ()
{
  int i;
  Arg arg[2];
  char buf[16];
  int msb;

  for (i = 0; i < 128; i++)
    {
      int index = glyph_index + i;

      if (charmap_index >= 0)
	index = FT_Get_Char_Index (face, (FT_ULong) index);
      if (charmap_index >= 0 && ! index)
	XtSetArg (arg[0], XtNbitmap, none_pixmap);
      else
	{
	  if (! bitmap[index].pixmap)
	    create_pixmap (index);
	  XtSetArg (arg[0], XtNbitmap, bitmap[index].pixmap);
	}
      XtSetValues (glyph[i], arg, 1);
    }

  msb = (glyph_index >> 7) % 2 ? 8 : 0;
  for (i = 0; i < 8; i++)
    {
      char str[3];

      sprintf (str, "%XX", i | msb );
      XtSetArg (arg[0], XtNheight, glyph_height + 5);
      XtSetArg (arg[1], XtNlabel, str);
      XtSetValues (index_label[i], arg, 2);
    }

  if (glyph_index < 0x10000)
    sprintf (buf, "  %04X-%04X  ", glyph_index, glyph_index + 0x7F);
  else
    sprintf (buf, "%06X-%06X", glyph_index, glyph_index + 0x7F);
  XtSetArg (arg[0], XtNlabel, buf);
  XtSetValues (range, arg, 1);
}

void
update_uvs_area (int c)
{
  OTF_GlyphID code[256];
  Arg arg[1];
  int i;

  OTF_get_variation_glyphs (otf, c, code);

  for (i = 0; i < 256; i++)
    if (uvs[i].w)
      {
	if (code[i])
	  XtSetArg (arg[0], XtNsensitive, True);
	else
	  XtSetArg (arg[0], XtNsensitive, False);
	XtSetValues (uvs[i].w, arg, 1);
      }
}


char *
get_features (OTF_FeatureList *list, FeatureRec *rec)
{
  int i, n;
  char *str, *p;

  if (! rec->langsys || ! rec->features)
    return NULL;
  for (i = n = 0; i < rec->langsys->FeatureCount; i++)
    if (rec->features[i].on)
      n++;
  if (n == 0)
    return NULL;
  str = malloc (n * 5);
  for (i = 0, p = str; i < rec->langsys->FeatureCount; i++)
    if (rec->features[i].on)
      {
	OTF_tag_name (rec->features[i].tag, p);
	p[4] = ',';
	p += 5;
      }
  p[-1] = '\0';
  return str;
}


#define DEVICE_DELTA(table, size)					\
  (((table).DeltaValue							\
    && ((size) >= (table).StartSize && (size) <= (table).EndSize))	\
   ? (table).DeltaValue[(size) >= (table).StartSize]			\
   : 0)

void
adjust_anchor (OTF_Anchor *anchor, FT_Face ft_face,
	       OTF_Glyph *g, int *x, int *y)
{
  if (anchor->AnchorFormat == 2)
    {
      FT_Outline *outline;
      int ap = anchor->f.f1.AnchorPoint;

      FT_Load_Glyph (ft_face, (FT_UInt) g->glyph_id, FT_LOAD_MONOCHROME);
      outline = &ft_face->glyph->outline;
      if (ap < outline->n_points)
	{
	  *x = outline->points[ap].x;
	  *y = outline->points[ap].y;
	}
    }
  else if (anchor->AnchorFormat == 3)
    {
      *x += DEVICE_DELTA (anchor->f.f2.XDeviceTable, pixel_size);
      *y += DEVICE_DELTA (anchor->f.f2.YDeviceTable, pixel_size);
    }
}

void
update_seq_area ()
{
  int i, x;
  OTF_GlyphString gstring;
  OTF_Glyph *g, *prev, *base, *mark;
  int base_width;
  int len = glyph_rec.n_glyphs;
  Arg arg[1];
  int unitsPerEm = face->units_per_EM;
  OTF_Tag *log = NULL;
  int logsize;

  gstring.size = gstring.used = len;
  gstring.glyphs = malloc (sizeof (OTF_Glyph) * len);
  memset (gstring.glyphs, 0, sizeof (OTF_Glyph) * len);
  for (i = 0; i < len; i++)
    {
      gstring.glyphs[i].c = glyph_rec.codes[i];
      if (charmap_index < 0)
	gstring.glyphs[i].glyph_id = glyph_rec.glyphs[i];
    }

  XFillRectangle (display, seq_pixmap, gc, 0, 0, render_width, render_height);
  XDrawLine (display, seq_pixmap, gc_set, 0, glyph_y, render_width, glyph_y);
  if (otf)
    {
      char *script_name = NULL, *langsys_name = NULL, buf[10];

      if (script_tag)
	{
	  script_name = buf;
	  OTF_tag_name (script_tag, script_name);
	}
      if (langsys_tag)
	{
	  langsys_name = buf + 5;
	  OTF_tag_name (langsys_tag, langsys_name);
	}

      OTF_drive_cmap (otf, &gstring);
      OTF_drive_gdef (otf, &gstring);
      if (otf->gsub)
	{
	  char *gsub_features = get_features (&otf->gsub->FeatureList, &gsub);

	  if (gsub_features)
	    {
	      if (do_alternate_subst)
		OTF_drive_gsub_alternate (otf, &gstring,
					  script_name, langsys_name,
					  gsub_features);
	      else
		{
		  OTF_drive_gsub_features (otf, &gstring,
					   script_name, langsys_name,
					   gsub_features);
		  logsize = gstring.used * 2;
		  log = alloca (sizeof (OTF_Tag) * logsize);
		  for (i = 0; i < gstring.used; i++)
		    {
		      int idx = OTF_POSITIONING_TYPE_GET_FEATURE (gstring.glyphs + i);
		      if (idx)
			log[i] = otf->gsub->FeatureList.Feature[idx - 1].FeatureTag;
		      else
			log[i] = 0;
		    }
		}
	      free (gsub_features);
	    }
	}
      if (otf->gpos)
	{
	  char *gpos_features = get_features (&otf->gpos->FeatureList, &gpos);
	  if (gpos_features)
	    {
	      OTF_drive_gpos_features (otf, &gstring,
				       script_name, langsys_name,
				       gpos_features);
	      if (log)
		{
		  if (logsize < gstring.used)
		    {
		      OTF_Tag *log2 = alloca (sizeof (OTF_Tag) * gstring.used);
		      memset (log2, 0, sizeof (OTF_Tag) * gstring.used);
		      memcpy (log2, log, sizeof (OTF_Tag) * logsize);
		      logsize = gstring.used;
		      log = log2;
		    }
		}
	      else
		{
		  logsize = gstring.used;
		  log = alloca (sizeof (OTF_Tag) * logsize);
		  memset (log, 0, sizeof (OTF_Tag) * logsize);
		}
	      for (i = 0; i < gstring.used; i++)
		{
		  int idx = OTF_POSITIONING_TYPE_GET_FEATURE (gstring.glyphs + i);
		  if (idx)
		    log[i] = otf->gpos->FeatureList.Feature[idx - 1].FeatureTag;
		}
	      free (gpos_features);
	    }
	}
    }

  prev = NULL;
  if (reversed)
    {
      OTF_Glyph temp;

      for (prev = gstring.glyphs, g = gstring.glyphs + gstring.used - 1;
	   prev < g; prev++, g--)
	temp = *prev, *prev = *g, *g = temp;
      for (g = gstring.glyphs; g < gstring.glyphs + gstring.used; g++)
	if (g->GlyphClass == 3)
	  {
	    OTF_Glyph *g0;
	    prev = g++;
	    while (g < gstring.glyphs + gstring.used && g->GlyphClass == 3)
	      g++;
	    for (g0 = g; prev < g0; prev++, g0--)
	      temp = *prev, *prev = *g, *g = temp;
	  }
    }


  mark = base = NULL;
  for (i = 0, x = glyph_x, prev = NULL, g = gstring.glyphs;
       i < gstring.used; i++, prev = g++)
    {
      BitmapRec *bmp = bitmap + gstring.glyphs[i].glyph_id;
      int xoff = 0, yoff = 0;
      int prev_width;
      int advance = bmp->advance;

      if (gstring.glyphs[i].glyph_id && ! bmp->pixmap)
	{
	  create_pixmap (gstring.glyphs[i].glyph_id);
	  if (! bmp->pixmap)
	    continue;
	  advance = bmp->advance;
	}
      if (OTF_POSITIONING_TYPE_GET_FORMAT (g))
	{
	  while (1)
	    {
	      switch (OTF_POSITIONING_TYPE_GET_FORMAT (g))
		{
		case 1: case 2:
		  {
		    int format = g->f.f1.format;

		    if (format & OTF_XPlacement)
		      xoff += g->f.f1.value->XPlacement * pixel_size / unitsPerEm;
		    if (format & OTF_XPlaDevice)
		      xoff += DEVICE_DELTA (g->f.f1.value->XPlaDevice, pixel_size);
		    if (format & OTF_YPlacement)
		      yoff += g->f.f1.value->YPlacement * pixel_size / unitsPerEm;
		    if (format & OTF_YPlaDevice)
		      yoff += DEVICE_DELTA (g->f.f1.value->YPlaDevice, pixel_size);
		    if (format & OTF_XAdvance)
		      advance += g->f.f1.value->XAdvance * pixel_size / unitsPerEm;
		    if (format & OTF_XAdvDevice)
		      advance += DEVICE_DELTA (g->f.f1.value->XAdvDevice,
					       pixel_size);
		  }
		  break;

		case 3:
		  /* Not yet supported.  */
		  break;
		case 4: case 5:
		  if (! base)
		    break;
		  prev = base;
		  prev_width = base_width;
		  goto label_adjust_anchor;
		default: 		/* i.e. case 6 */
		  if (! mark)
		    break;
		  {
		    int distance = OTF_POSITIONING_TYPE_GET_MARKDISTANCE (g);

		    if (distance > 0) 
		      {
			for (prev = g - 1; prev >= gstring.glyphs; prev--) 
			  {
			    if (prev->glyph_id > 0)
			      {
				distance--;
				if (distance == 0)
				  break;
			      }
			  }
			if (prev < gstring.glyphs)
			  prev = mark;
		      }
		    else
		      {
			prev = mark;
		      }
		    prev_width = 0;
		  }
		label_adjust_anchor:
		  {
		    int base_x, base_y, mark_x, mark_y;

		    base_x = g->f.f4.base_anchor->XCoordinate * pixel_size / unitsPerEm;
		    base_y = g->f.f4.base_anchor->YCoordinate * pixel_size / unitsPerEm;
		    mark_x = g->f.f4.mark_anchor->XCoordinate * pixel_size / unitsPerEm;
		    mark_y = g->f.f4.mark_anchor->YCoordinate * pixel_size / unitsPerEm;

		    if (g->f.f4.base_anchor->AnchorFormat != 1)
		      adjust_anchor (g->f.f4.base_anchor, face, prev, &base_x, &base_y);
		    if (g->f.f4.mark_anchor->AnchorFormat != 1)
		      adjust_anchor (g->f.f4.mark_anchor, face, g, &mark_x, &mark_y);
		    xoff = (base_x - prev_width) - mark_x;
		    yoff = base_y - mark_y;
		  }
		}
	      if (i + 1 == gstring.used
		  || gstring.glyphs[i + 1].glyph_id
		  || ! (OTF_POSITIONING_TYPE_GET_FORMAT (gstring.glyphs + i + 1)))
		break;
	      i++, g++;
	    }
	}

      XCopyArea (display, bmp->pixmap, seq_pixmap, gc_or,
		 glyph_x + bmp->x, glyph_y + bmp->y, bmp->width, bmp->height,
		 x + bmp->x + xoff, glyph_y + bmp->y - yoff);
      x += advance;

      if (g->GlyphClass == OTF_GlyphClass0)
	base = mark = g, base_width = advance;
      else if (g->GlyphClass == OTF_GlyphClassMark)
	mark = g;
      else
	base = g, base_width = advance;
    }

  XtSetArg (arg[0], XtNbitmap, seq_pixmap);
  XtSetValues (seq_image, arg, 1);

  if (gstring.used > 0)
    {
      int size = render_width / FONT_WIDTH;
      char *buf = alloca (size + 1);
      char name[5];

      sprintf (buf, "%04X", gstring.glyphs[0].glyph_id);
      if (log && log[0])
	{
	  OTF_tag_name (log[0], name);
	  sprintf (buf + 4, " (%s)", name);
	  x = 11;
	}
      else
	x = 4;
      for (i = 1; i < gstring.used && x + 5 < size; i++, x += 5)
	{
	  sprintf (buf + x, " %04X", gstring.glyphs[i].glyph_id);
	  if (log && log[i] && x + 11 < size)
	    {
	      OTF_tag_name (log[i], name);
	      sprintf (buf + x + 5, "(%s)", name);
	      x += 6;
	    }
	}
      while (x < size)
	buf[x++] = ' ';
      buf[x] = '\0';
      XtSetArg (arg[0], XtNlabel, buf);
      XtSetValues (code_list, arg, 1);
    }

  free (gstring.glyphs);
}


void
update_render_area ()
{
  int i;
  int x;
  Arg arg[1];

  XFillRectangle (display, raw_pixmap, gc, 0, 0, render_width, render_height);
  for (i = 0, x = glyph_x; i < glyph_rec.n_glyphs; i++)
    {
      if (glyph_rec.glyphs[i] >= 0)
	{
	  BitmapRec *bmp = bitmap + glyph_rec.glyphs[i];
	  char buf[5];

	  XCopyArea (display, bmp->pixmap, raw_pixmap, gc,
		     0, 0, glyph_width, glyph_height,
		     (glyph_width + 4) * i + 1, 1);
	  XDrawRectangle (display, raw_pixmap, gc_set,
			  (glyph_width + 4) * i, 0,
			  glyph_width + 1, glyph_height + 1);
	  XDrawLine (display, raw_pixmap, gc_set,
		     (glyph_width + 4) * i + 1 + glyph_x, 1,
		     (glyph_width + 4) * i + 1 + glyph_x, glyph_height + 1);
	  XDrawLine (display, raw_pixmap, gc_set,
		     (glyph_width + 4) * i + 1 + glyph_x + bmp->advance, 1,
		     (glyph_width + 4) * i + 1 + glyph_x + bmp->advance,
		     glyph_height + 1);

	  sprintf (buf, "%04X", glyph_rec.codes[i]);
	  XDrawString (display, raw_pixmap, gc_inv, 
		       (glyph_width + 4) * i + 1
		       + (glyph_width - XTextWidth (font, buf, 4)) / 2,
		       glyph_height + 2 + FONT_HEIGHT, buf, 4);
	}
      else
	{
	  /* Variation Selector */
	  int idx = - glyph_rec.glyphs[i];
	  char buf[4];

	  sprintf (buf, "%03d", idx);
	  XDrawRectangle (display, raw_pixmap, gc_set,
			  (glyph_width + 4) * i, 0,
			  glyph_width + 1, glyph_height + 1);
	  XDrawString (display, raw_pixmap, gc_set,
		       (glyph_width + 4) * i + 1
		       + (glyph_width - XTextWidth (font, "VS", 2)) / 2,
		       1 + glyph_height / 2, "VS", 2);
	  XDrawString (display, raw_pixmap, gc_set,
		       (glyph_width + 4) * i + 1
		       + (glyph_width - XTextWidth (font, buf, 3)) / 2,
		       1 + glyph_height / 2 + FONT_ASCENT, buf, 3);
	}
    }
  XtSetArg (arg[0], XtNbitmap, raw_pixmap);
  XtSetValues (raw_image, arg, 1);
  update_seq_area ();
}

void
QuitProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  XtAppSetExitFlag (XtWidgetToApplicationContext (w));
}

void
DumpProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  int g_width, g_height;
  float g_x, g_y;
  /* unit in points (1/72 inch); to fit in both US-letter and A4 */
  static int xoff = 30, yoff = 30;
  static int unit = 30;
  static int margin = 2;
  static int title_height = 20, label_height = 10;
  int total_width = (unit + margin * 2) * 16;
  int total_height = (unit + margin * 2 + label_height) * 16 + title_height;
  /* pixel size (dots) */
  int size = 128;
  int i, j, k, l;
  char *name = alloca (strlen (filename) + 10);
  FILE *fp;
  int index = (glyph_index / 0x100) * 0x100;
  float scale;

  g_width = face->bbox.xMax - face->bbox.xMin;
  g_height = face->bbox.yMax - face->bbox.yMin;
  if (g_width > g_height)
    {
      scale = g_width * size;
      g_x = face->bbox.xMin * unit / g_width;
      g_y = face->bbox.yMin * unit / g_width;
    }
  else
    {
      scale = g_height * size;
      g_x = face->bbox.xMin * unit / g_height;
      g_y = face->bbox.yMin * unit / g_height;
    }
  scale /= face->units_per_EM;

  FT_Set_Pixel_Sizes (face, 0, size);

  sprintf (name, "%s-%04X.ps", face->family_name, index);
  printf ("Writing %s ... ", name);
  fflush (stdout);
  fp = fopen (name, "w");

  fprintf (fp, "%s\n", "%!PS-Adobe-2.0 EPSF-2.0");
  fprintf (fp, "%s\n", "%%Creater: otfview");
  fprintf (fp, "%s %s(%s)-%04X\n", "%%Title:",
	   face->family_name, face->style_name, index);
  fprintf (fp, "%s\n", "%%Pages: 1");
  fprintf (fp, "%s %d %d %d %d\n", "%%BoundingBox:",
	   xoff, yoff, xoff + total_width, yoff + total_height);
  fprintf (fp, "%s\n", "%%EndComments");
  fprintf (fp, "%s\n", "%%BeginProlog");
  fprintf (fp, "/W %d def\n", unit + margin * 2);
  fprintf (fp, "/H %d def\n", unit + margin * 2 + label_height);
  fprintf (fp, "/STR 10 string def\n");
  fprintf (fp, "/DrawIndex {\n");
  fprintf (fp, "  I 16 lt { (000) show } { I 256 lt { (00) show } { I 4096 lt { (0) show} if } ifelse } ifelse I 16 STR cvrs show\n");
  fprintf (fp, "} def\n");
  fprintf (fp, "/DrawTitle {\n");
  fprintf (fp, "  /Courier findfont 20 scalefont setfont\n");
  fprintf (fp, "  %d %d 4 add moveto\n", xoff + total_width / 2,
	   yoff + total_height - title_height + 2);
  fprintf (fp, "  (%s(%s)-%04X) dup stringwidth pop 2 div neg 0 rmoveto show\n",
	   face->family_name, face->style_name, index);
  fprintf (fp, "} def\n");
  fprintf (fp, "/DrawFrame { gsave %d %d translate 0 setlinewidth\n",
	   xoff, yoff);
  fprintf (fp, "  /Courier findfont 10 scalefont setfont\n");
  fprintf (fp, "  /I %d def\n", index);
  fprintf (fp, "  0 1 16 { W mul 0 moveto 0 H 16 mul rlineto stroke } for\n");
  fprintf (fp, "  0 1 16 { H mul 0 exch moveto W 16 mul 0 rlineto stroke } for\n");
  fprintf (fp, "  0 1 15 { H mul %d add 0 exch moveto W 16 mul 0 rlineto stroke } for\n", label_height);
  fprintf (fp, "  15 -1 0 { gsave H mul 0 exch translate 0 0 moveto\n");
  fprintf (fp, "    0 1 15 { gsave W mul 0 moveto\n");
  fprintf (fp, "      4 2 rmoveto DrawIndex /I I 1 add def grestore} for\n");
  fprintf (fp, "    grestore } for grestore } def\n");
  fprintf (fp, "%s\n", "%%EndProlog");
  fprintf (fp, "DrawTitle DrawFrame\n");

  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++, index++)
      {
	int idx;

	if (charmap_index >= 0) 
	  idx = FT_Get_Char_Index (face, (FT_ULong) index);
	else
	  idx = index;
	if (idx > 0
	    && FT_Load_Glyph (face, idx, FT_LOAD_RENDER | FT_LOAD_MONOCHROME) == 0
	    && face->glyph->bitmap.rows > 0
	    && face->glyph->bitmap.width > 0)
	  {
	    unsigned char *p = face->glyph->bitmap.buffer;
	    int width = (face->glyph->bitmap.width - 1) / 8 + 1;

	    fprintf (fp, "gsave %f %f translate %d %d scale 0 0 moveto\n",
		     xoff + (unit + margin * 2) * j + margin - g_x,
		     yoff + (unit + label_height + margin * 2) * (15 - i) + label_height + margin - g_y,
		     unit, unit);
	    fprintf (fp, "%d %d true [%f 0 0 %f %d %d]\n",
		     width * 8, face->glyph->bitmap.rows,
		     scale, -scale, -face->glyph->bitmap_left,
		     face->glyph->bitmap_top);
	    fprintf (fp, "{< ");
	    for (k = 0; k < face->glyph->bitmap.rows;
		 k++, p += face->glyph->bitmap.pitch)
	      {
		for (l = 0; l < width; l++)
		  fprintf (fp, "%02X", p[l]);
		fprintf (fp, "\n");
	      }
	    fprintf (fp, ">} imagemask grestore\n");
	  }
	else
	  {
	    int boxsize = unit + margin * 2;

	    fprintf (fp, "gsave 0 setlinewidth %d %d translate\n",
		     xoff + boxsize * j,
		     yoff + (boxsize + label_height) * (15 - i) + label_height);
	    fprintf (fp, "0 0 moveto %d %d lineto stroke\n",
		     boxsize, boxsize);
	    fprintf (fp, "0 %d moveto %d 0 lineto stroke grestore\n",
		     boxsize, boxsize);
	  }
      }
  fprintf (fp, "showpage\n");
  fclose (fp);
  printf ("done\n");

  FT_Set_Pixel_Sizes (face, 0, (int) pixel_size);
}


void
GlyphProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  int old_glyph_index = glyph_index;
  int data;

  CAST_FROM_XTPOINTER (int, client_data, data);

  if (data == -3 && glyph_index > 0)
    glyph_index = 0;
  else if (data == -2 && glyph_index > 0)
    glyph_index = (glyph_index - 1) & 0x1FF000;
  else if (data == -1 && glyph_index > 0)
    glyph_index -= 0x80;
  else if (data == 1 && glyph_index < 0x10FF80)
    glyph_index += 0x80;
  else if (data == 2 && glyph_index < 0x10F000)
    glyph_index = (glyph_index + 0x1000) & 0x1FF000;
  else if (data == 3 && glyph_index < 0x10F000)
    glyph_index = 0x10FF80;
  if (glyph_index != old_glyph_index)
    update_glyph_area ();
}

void
CharmapProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  int data;

  CAST_FROM_XTPOINTER (int, client_data, data);

  if (charmap_index == data)
    return;
  charmap_index = data;
  if (charmap_index >= 0)
    FT_Set_Charmap (face, face->charmaps[charmap_index]);
  update_glyph_area ();
}

void
UVSProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  unsigned idx;
  int selector;
  OTF_VariationSelectorRecord *record;
  int i;

  CAST_FROM_XTPOINTER (unsigned, client_data, idx);
  selector = uvs[idx].c;

  if (glyph_rec.n_glyphs >= 64)
    return;
  for (i = 0; i < sub14->nRecords; i++)
    {
      record = sub14->Records + i;
      if (record->varSelector == selector)
	break;
    }
  if (i < sub14->nRecords)
    {
      if (glyph_rec.n_glyphs > 0
	  && glyph_rec.glyphs[glyph_rec.n_glyphs - 1] < 0)
	glyph_rec.n_glyphs--;
      glyph_rec.codes[glyph_rec.n_glyphs] = selector;
      glyph_rec.glyphs[glyph_rec.n_glyphs++] = - idx - 1;
      update_render_area ();
    }
}

void
RenderProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  int data;

  CAST_FROM_XTPOINTER (int, client_data, data);

  if (data < 0)
    {
      if (glyph_rec.n_glyphs > 0)
	{
	  if (data == -2)
	    glyph_rec.n_glyphs--;
	  else
	    glyph_rec.n_glyphs = 0;
	  update_render_area ();
	}
    }
  else if (glyph_rec.n_glyphs < 64)
    {
      int index = glyph_index + data;

      if (charmap_index >= 0)
	index = FT_Get_Char_Index (face, (FT_ULong) index);
      if (bitmap[index].pixmap)
	{
	  glyph_rec.codes[glyph_rec.n_glyphs] = glyph_index + data;
	  glyph_rec.glyphs[glyph_rec.n_glyphs++] = index;
	  if (otf)
	    update_uvs_area (glyph_index + data);
	  update_render_area ();
	}
    }
}

void
BidiProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  Arg arg[1];

  reversed = ! reversed;
  if (reversed)
    XtSetArg (arg[0], XtNlabel, "L<-R");
  else
    XtSetArg (arg[0], XtNlabel, "L->R");
  XtSetValues (w, arg, 1);
  update_seq_area ();
}

void
AltSubstProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  do_alternate_subst = ! do_alternate_subst;
  update_seq_area ();
}

void
FeatureProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  FeatureRec *rec = (FeatureRec *) client_data;
  int idx, i, j;
  Arg arg[4];
  char *label;

  if (! rec->langsys)
    return;
  XtSetArg (arg[0], XtNlabel, &label);
  XtGetValues (w, arg, 1);
  if (! strcmp (label, "all"))
    idx = -2;
  else if (! strcmp (label, "none"))
    idx = -1;
  else
    {
      for (idx = 0; idx < rec->langsys->FeatureCount; idx++)
	if (rec->features[idx].w == w)
	  break;
      if (idx == rec->langsys->FeatureCount)
	idx = -1;
    }
  if (idx < 0)
    {
      int on = idx == -2;
      char str[5];

      for (i = 0; i < rec->langsys->FeatureCount; i++)
	if (rec->features[i].on != on)
	  {
	    rec->features[i].on = on;
	    if (on)
	      {
		XtSetArg (arg[0], XtNborderWidth, 3);
		XtSetArg (arg[1], XtNinternalHeight, 2);
		XtSetArg (arg[2], XtNinternalWidth, 2);
	      }
	    else
	      {
		XtSetArg (arg[0], XtNborderWidth, 1);
		XtSetArg (arg[1], XtNinternalHeight, 4);
		XtSetArg (arg[2], XtNinternalWidth, 4);
	      }
	    OTF_tag_name (rec->features[i].tag, str);
	    XtSetArg (arg[3], XtNlabel, str);
	    XtSetValues (rec->features[i].w, arg, 4);
	  }
    }
  else
    {
      char str[5];

      rec->features[idx].on = ! rec->features[idx].on;
      if (rec->features[idx].on)
	{
	  XtSetArg (arg[0], XtNborderWidth, 3);
	  XtSetArg (arg[1], XtNinternalHeight, 2);
	  XtSetArg (arg[2], XtNinternalWidth, 2);
	}
      else
	{
	  XtSetArg (arg[0], XtNborderWidth, 1);
	  XtSetArg (arg[1], XtNinternalHeight, 4);
	  XtSetArg (arg[2], XtNinternalWidth, 4);
	}
      OTF_tag_name (rec->features[idx].tag, str);
      XtSetArg (arg[3], XtNlabel, str);
      XtSetValues (rec->features[idx].w, arg, 4);
    }
  update_seq_area ();
}

void
setup_feature_rec (FeatureRec *rec)
{
  int i, j;
  Arg arg[10];

  rec->langsys = NULL;
  if (! rec->gsub_gpos)
    return;
  for (i = 0; i < rec->gsub_gpos->ScriptList.ScriptCount; i++)
    if (rec->gsub_gpos->ScriptList.Script[i].ScriptTag == script_tag)
      {
	OTF_Script *script = rec->gsub_gpos->ScriptList.Script + i;

	if (langsys_tag)
	  {
	    for (j = 0; j < script->LangSysCount; j++)
	      if (script->LangSysRecord[j].LangSysTag == langsys_tag)
		{
		  rec->langsys = script->LangSys + j;
		  break;
		}
	  }
	if (! rec->langsys)
	  rec->langsys = &rec->gsub_gpos->ScriptList.Script[i].DefaultLangSys;
	break;
      }

  if (! rec->langsys)
    i = 0;
  else
    {
      XtSetArg (arg[0], XtNborderWidth, 1);
      XtSetArg (arg[1], XtNinternalHeight, 4);
      XtSetArg (arg[2], XtNinternalWidth, 4);
      XtSetArg (arg[3], XtNborderColor, foreground);
      XtSetArg (arg[4], XtNsensitive, True);
      for (i = 0; i < rec->langsys->FeatureCount; i++)
	{
	  OTF_Feature *feature = rec->gsub_gpos->FeatureList.Feature;
	  int index = rec->langsys->FeatureIndex[i];
	  char label[5];

	  if (! rec->features[i].w)
	    {
	      Widget w = XtCreateManagedWidget ("", commandWidgetClass,
						rec->parent, arg, 0);
	      XtAddCallback (w, XtNcallback, FeatureProc, (XtPointer) rec);
	      rec->features[i].w = w;
	    }

	  rec->features[i].tag = feature[index].FeatureTag;
	  rec->features[i].on = 0;
	  OTF_tag_name (rec->features[i].tag, label);
	  XtSetArg (arg[5], XtNlabel, label);
	  XtSetValues (rec->features[i].w, arg, 6);
	}
    }
  XtSetArg (arg[0], XtNborderColor, background);
  XtSetArg (arg[1], XtNsensitive, False);
  XtSetArg (arg[2], XtNlabel, "    ");
  for (; i < rec->num_features; i++)
    {
      if (! rec->features[i].w)
	{
	  Widget w = XtCreateManagedWidget ("", commandWidgetClass,
					    rec->parent, arg, 0);
	  XtAddCallback (w, XtNcallback, FeatureProc, (XtPointer) rec);
	  rec->features[i].w = w;
	}
      XtSetValues (rec->features[i].w, arg, 3);
    }
}

void
compose_script_langsys (OTF_Tag script, OTF_Tag langsys, char *name)
{
  OTF_tag_name (script, name);
  if (langsys)
    {
      name[4] = '(';
      OTF_tag_name (langsys, name + 5);
      name[9] = ')';
      name[10] = '\0';
    }
}

void
decompose_script_langsys (OTF_Tag *script, OTF_Tag *langsys, char *name)
{
  *script = OTF_tag (name);
  if (name[4])
    *langsys = OTF_tag (name + 5);
  else
    *langsys = 0;
}

void
ScriptProc (Widget w, XtPointer client_data, XtPointer call_data)
{
  char *name;
  OTF_Tag script, langsys;
  Arg arg[1];

  XtSetArg (arg[0], XtNlabel, &name);
  XtGetValues (w, arg, 1);
  decompose_script_langsys (&script, &langsys, name);
  if (script_tag == script && langsys_tag == langsys)
    return;
  script_tag = script;
  langsys_tag = langsys;
  setup_feature_rec (&gsub);
  setup_feature_rec (&gpos);
  update_seq_area ();
}

Widget
create_otf_script_widgets (Widget prev)
{
  Widget w;
  Arg arg[10];
  int n, prev_n, i, j;
  struct {
    OTF_Tag script;
    OTF_Tag langsys;
  } *script_langsys;
  char name[11];
  int nfeatures;

  XtSetArg (arg[0], XtNborderWidth, 0);
  XtSetArg (arg[1], XtNleft, XawChainLeft);
  XtSetArg (arg[2], XtNright, XawChainLeft);
  XtSetArg (arg[3], XtNtop, XawChainTop);
  XtSetArg (arg[4], XtNbottom, XawChainTop);
  XtSetArg (arg[5], XtNfromVert, prev);
  XtSetArg (arg[6], XtNorientation, XtorientHorizontal);
  prev = XtCreateManagedWidget ("Script", boxWidgetClass, render_area, arg, 7);
  XtCreateManagedWidget ("script(langsys)", labelWidgetClass, prev, arg, 1);

  n = 0;
  if (otf->gsub)
    for (i = 0; i < otf->gsub->ScriptList.ScriptCount; i++)
      n += otf->gsub->ScriptList.Script[i].LangSysCount + 1;
  if (otf->gpos)
    for (i = 0; i < otf->gpos->ScriptList.ScriptCount; i++)
      n += otf->gpos->ScriptList.Script[i].LangSysCount + 1;
  script_langsys = alloca ((sizeof script_langsys[0]) * n);
  n = 0;
  nfeatures = 0;
  if (otf->gsub)
    for (i = 0; i < otf->gsub->ScriptList.ScriptCount; i++)
      {
	OTF_Tag tag = otf->gsub->ScriptList.Script[i].ScriptTag;

	script_langsys[n].script = tag;
	script_langsys[n++].langsys = 0;
	if (nfeatures
	    < otf->gsub->ScriptList.Script[i].DefaultLangSys.FeatureCount)
	  nfeatures
	    = otf->gsub->ScriptList.Script[i].DefaultLangSys.FeatureCount;
	for (j = 0; j < otf->gsub->ScriptList.Script[i].LangSysCount; j++)
	  {
	    script_langsys[n].script = tag;
	    script_langsys[n++].langsys
	      = otf->gsub->ScriptList.Script[i].LangSysRecord[j].LangSysTag;
	    if (nfeatures
		< otf->gsub->ScriptList.Script[i].LangSys[j].FeatureCount)
	      nfeatures
		= otf->gsub->ScriptList.Script[i].LangSys[j].FeatureCount;
	  }
      }
  gsub.num_features = nfeatures;
  if (nfeatures > 0)
    {
      gsub.num_features = nfeatures;
      gsub.features = malloc ((sizeof (FeatureElement)) * nfeatures);
      memset (gsub.features, 0, (sizeof (FeatureElement)) * nfeatures);
    }
  prev_n = n;
  nfeatures = 0;
  if (otf->gpos)
    for (i = 0; i < otf->gpos->ScriptList.ScriptCount; i++)
      {
	OTF_Tag tag = otf->gpos->ScriptList.Script[i].ScriptTag;
	int k;

	if (nfeatures
	    < otf->gpos->ScriptList.Script[i].DefaultLangSys.FeatureCount)
	  nfeatures
	    = otf->gpos->ScriptList.Script[i].DefaultLangSys.FeatureCount;
	for (k = 0; k < prev_n; k++)
	  if (tag == script_langsys[k].script)
	    break;
	if (k == prev_n)
	  {
	    script_langsys[n].script = tag;
	    script_langsys[n++].langsys = 0;
	  }
	for (j = 0; j < otf->gpos->ScriptList.Script[i].LangSysCount; j++)
	  {
	    int l;

	    if (k < prev_n)
	      {
		OTF_Script *script = otf->gpos->ScriptList.Script + i;

		for (l = k; l < prev_n && tag == script_langsys[l].script; l++)
		  if (script->LangSysRecord[j].LangSysTag
		      == script_langsys[l].langsys)
		    break;
	      }
	    else
	      l = prev_n;
	    if (l == prev_n)
	      {
		script_langsys[n].script = tag;
		script_langsys[n++].langsys = 0;
	      }
	    if (nfeatures
		< otf->gpos->ScriptList.Script[i].LangSys[j].FeatureCount)
	      nfeatures
		= otf->gpos->ScriptList.Script[i].LangSys[j].FeatureCount;
	  }
      }

  if (nfeatures > 0)
    {
      gpos.num_features = nfeatures;
      gpos.features = malloc ((sizeof (FeatureElement)) * nfeatures);
      memset (gpos.features, 0, (sizeof (FeatureElement)) * nfeatures);
    }

  if (n == 0)
    return prev;

  script_tag = script_langsys[0].script;
  langsys_tag = script_langsys[0].langsys;
  compose_script_langsys (script_tag, langsys_tag, name);

  if (n == 1)
    {
      XtSetArg (arg[0], XtNforeground, background);
      XtSetArg (arg[1], XtNbackground, foreground);
      XtCreateManagedWidget (name, labelWidgetClass, prev, arg, 2);
    }
  else
    {
      Widget box;
      XtSetArg (arg[0], XtNborderWidth, 0);
      XtSetArg (arg[1], XtNwidth, render_width - (FONT_WIDTH * 15));
      XtSetArg (arg[2], XtNorientation, XtorientHorizontal);
      box = XtCreateManagedWidget ("scritp-list", boxWidgetClass, prev, arg, 2);
      XtSetArg (arg[0], XtNstate, True);
      w = XtCreateManagedWidget (name, toggleWidgetClass, box, arg, 1);
      XtAddCallback (w, XtNcallback, ScriptProc, NULL);
      XtSetArg (arg[0], XtNradioGroup, w);
      for (i = 1; i < n; i++)
	{
	  compose_script_langsys (script_langsys[i].script,
				  script_langsys[i].langsys, name);
	  w = XtCreateManagedWidget (name, toggleWidgetClass, box, arg, 1);
	  XtAddCallback (w, XtNcallback, ScriptProc, NULL);
	}	  
    }
  return prev;
}


Widget
create_otf_widgets (Widget prev, FeatureRec *rec)
{
  Arg arg[10];
  Widget w;

  XtSetArg (arg[0], XtNborderWidth, 0);
  XtSetArg (arg[1], XtNleft, XawChainLeft);
  XtSetArg (arg[2], XtNright, XawChainLeft);
  XtSetArg (arg[3], XtNtop, XawChainTop);
  XtSetArg (arg[4], XtNbottom, XawChainTop);
  XtSetArg (arg[5], XtNfromVert, prev);
  XtSetArg (arg[6], XtNorientation, XtorientHorizontal);
  prev = XtCreateManagedWidget (rec->label, boxWidgetClass, render_area,
				arg, 7);
  XtCreateManagedWidget (rec->label, labelWidgetClass, prev, arg, 1);
  XtSetArg (arg[0], XtNborderWidth, 1);
  XtSetArg (arg[1], XtNinternalHeight, 4);
  XtSetArg (arg[2], XtNinternalWidth, 4);
  w = XtCreateManagedWidget ("all", commandWidgetClass, prev, arg, 3);
  XtAddCallback (w, XtNcallback, FeatureProc, (XtPointer) rec);
  w = XtCreateManagedWidget ("none", commandWidgetClass, prev, arg, 3);
  XtAddCallback (w, XtNcallback, FeatureProc, (XtPointer) rec);

  rec->parent = prev;
  setup_feature_rec (rec);
  return prev;
}

void
create_widgets ()
{
  String quit_action = "<KeyPress>q: set() notify() unset()";
  String FIRST_action = "<KeyPress>f: set() notify() unset()\n\
			 <KeyPress>Home: set() notify() unset()";
  String PREV_action = "Shift<KeyPress>p: set() notify() unset()\n\
			 <KeyPress>Up: set() notify() unset()";
  String prev_action = "~Shift<KeyPress>p: set() notify() unset()\n\
			 <KeyPress>Left: set() notify() unset()";
  String next_action = "~Shift<KeyPress>n: set() notify() unset()\n\
			 <KeyPress>Right: set() notify() unset()";
  String NEXT_action = "Shift<KeyPress>n: set() notify() unset()\n\
			 <KeyPress>Down: set() notify() unset()";
  String LAST_action = "<KeyPress>l: set() notify() unset()\n\
			 <KeyPress>End: set() notify() unset()";
  Arg arg[10];
  int i, j;
  Widget prev, w;
  String trans = "<Expose>: Expose()";

  XtSetArg (arg[0], XtNtranslations, XtParseTranslationTable (trans));
  frame = XtCreateManagedWidget ("frame", formWidgetClass, shell, arg, 1);

  XtSetArg (arg[0], XtNleft, XawChainLeft);
  XtSetArg (arg[1], XtNright, XawChainLeft);
  XtSetArg (arg[2], XtNtop, XawChainTop);
  XtSetArg (arg[3], XtNbottom, XawChainTop);
  XtSetArg (arg[4], XtNborderWidth, 0);
  XtSetArg (arg[5], XtNorientation, XtorientHorizontal);
  command_area = XtCreateManagedWidget ("command-area", boxWidgetClass,
					frame, arg, 6);
  XtSetArg (arg[6], XtNfromVert, command_area);
  navi_area = XtCreateManagedWidget ("navi-area", boxWidgetClass,
				     frame, arg, 7);
  XtSetArg (arg[4], XtNborderWidth, 0);
  XtSetArg (arg[5], XtNfromVert, navi_area);
  XtSetArg (arg[6], XtNdefaultDistance, 0);
  glyph_area = XtCreateManagedWidget ("glyph-area", formWidgetClass,
				      frame, arg, 7);

  XtSetArg (arg[5], XtNfromVert, glyph_area);
  if (sub14)
    {
      Arg arg2[3];

      XtSetArg (arg[6], XtNorientation, XtorientHorizontal);
      uvs_area = XtCreateManagedWidget ("uvs-area", boxWidgetClass,
					frame, arg, 7);
      XtSetArg (arg2[0], XtNborderWidth, 0);
      XtSetArg (arg2[1], XtNlabel, "Variation Selector: ");
      uvs_label = XtCreateManagedWidget ("uvs-label", labelWidgetClass,
					 uvs_area, arg2, 2);
      XtSetArg (arg2[0], XtNborderWidth, 1);
      for (i = 0; i < sub14->nRecords; i++)
	{
	  OTF_VariationSelectorRecord *record = sub14->Records + i;
	  unsigned selector = record->varSelector;
	  unsigned idx;
	  char lbl[4];
	  
	  idx = (selector <= 0xFE0F ? selector - 0xFE00
		 : selector - 0xE0100 + 16);
	  if (uvs[idx].c)
	    continue;
	  uvs[idx].c = selector;
	  sprintf (lbl, "%03d", idx + 1);
	  XtSetArg (arg2[1], XtNlabel, lbl);
	  XtSetArg (arg2[2], XtNsensitive, False);
	  uvs[idx].w = XtCreateManagedWidget ("lbl", commandWidgetClass,
						   uvs_area, arg2, 3);
	  XtAddCallbackWithCast (unsigned, uvs[idx].w, UVSProc, idx);
	}
      XtSetArg (arg[5], XtNfromVert, uvs_area);
    }
  render_area = XtCreateManagedWidget ("render-area", formWidgetClass,
				       frame, arg, 6);

  XtSetArg (arg[0], XtNaccelerators, XtParseAcceleratorTable (quit_action));
  quit = XtCreateManagedWidget ("Quit", commandWidgetClass,
				command_area, arg, 1);
  XtAddCallback (quit, XtNcallback, QuitProc, NULL);

  dump = XtCreateManagedWidget ("DumpImage", commandWidgetClass,
				command_area, arg, 1);
  XtAddCallback (dump, XtNcallback, DumpProc, NULL);

  XtSetArg (arg[0], XtNborderWidth, 0);
  XtSetArg (arg[1], XtNwidth, 10);
  XtCreateManagedWidget ("spacer", boxWidgetClass, command_area, arg, 2);

  charmap = alloca (sizeof (Widget) * (face->num_charmaps + 1));
  XtSetArg (arg[0], XtNstate, True);
  charmap[0] = XtCreateManagedWidget (charmap_rec[0].name, toggleWidgetClass,
				      command_area, arg, 1);
  XtAddCallback (charmap[0], XtNcallback, CharmapProc, (XtPointer) -1);
  XtSetArg (arg[0], XtNradioGroup, charmap[0]);
  for (i = 0; i < face->num_charmaps; i++)
    {
      charmap[i + 1] = XtCreateManagedWidget (charmap_rec[i + 1].name,
					      toggleWidgetClass,
					      command_area, arg, 1);
      XtAddCallbackWithCast (int, charmap[i + 1], CharmapProc, i);
    }

  XtSetArg (arg[0], XtNlabel, " |< (f)");
  XtSetArg (arg[1], XtNaccelerators, XtParseAcceleratorTable (FIRST_action));
  FIRST = XtCreateManagedWidget ("FIRST", commandWidgetClass,
				 navi_area, arg, 2);
  XtAddCallback (FIRST, XtNcallback, GlyphProc, (XtPointer) -3);
  XtSetArg (arg[0], XtNlabel, "<< (P)");
  XtSetArg (arg[1], XtNaccelerators, XtParseAcceleratorTable (PREV_action));
  PREV = XtCreateManagedWidget ("PREV", commandWidgetClass,
				navi_area, arg, 2);
  XtAddCallback (PREV, XtNcallback, GlyphProc, (XtPointer) -2);
  XtSetArg (arg[0], XtNlabel, "< (p)");
  XtSetArg (arg[1], XtNaccelerators, XtParseAcceleratorTable (prev_action));
  prev = XtCreateManagedWidget ("prev", commandWidgetClass,
				navi_area, arg, 2);
  XtAddCallback (prev, XtNcallback, GlyphProc, (XtPointer) -1);
  XtSetArg (arg[0], XtNlabel, " 0000 ");
  range = XtCreateManagedWidget ("range", labelWidgetClass,
				 navi_area, arg, 1);
  XtSetArg (arg[0], XtNforeground, &foreground);
  XtSetArg (arg[1], XtNbackground, &background);
  XtGetValues (range, arg, 2);

  XtSetArg (arg[0], XtNlabel, "> (n)");
  XtSetArg (arg[1], XtNaccelerators, XtParseAcceleratorTable (next_action));
  next = XtCreateManagedWidget ("next", commandWidgetClass,
				navi_area, arg, 2);
  XtAddCallback (next, XtNcallback, GlyphProc, (XtPointer) 1);
  XtSetArg (arg[0], XtNlabel, ">> (N)");
  XtSetArg (arg[1], XtNaccelerators, XtParseAcceleratorTable (NEXT_action));
  NEXT = XtCreateManagedWidget ("NEXT", commandWidgetClass,
				navi_area, arg, 2);
  XtAddCallback (NEXT, XtNcallback, GlyphProc, (XtPointer) 2);
  XtSetArg (arg[0], XtNlabel, ">| (l)");
  XtSetArg (arg[1], XtNaccelerators, XtParseAcceleratorTable (LAST_action));
  LAST = XtCreateManagedWidget ("LAST", commandWidgetClass,
				navi_area, arg, 2);
  XtAddCallback (LAST, XtNcallback, GlyphProc, (XtPointer) 3);

  XtSetArg (arg[0], XtNleft, XawChainLeft);
  XtSetArg (arg[1], XtNright, XawChainLeft);
  XtSetArg (arg[2], XtNtop, XawChainTop);
  XtSetArg (arg[3], XtNbottom, XawChainTop);

  for (i = 0; i < 8; i++)
    {
      char str[3];
      int n = 4;
      Widget head;

      sprintf (str, "%XX", i);
      XtSetArg (arg[n], XtNheight, glyph_height + 5), n++;
      XtSetArg (arg[n], XtNlabel, str), n++;
      XtSetArg (arg[n], XtNborderWidth, 0), n++;
      if (i > 0)
	XtSetArg (arg[n], XtNfromVert, w), n++;
      head = XtCreateManagedWidget (str, labelWidgetClass, glyph_area, arg, n);
      index_label[i] = head;
      for (j = 0; j < 16; j++)
	{
	  int k = i * 16 + j;

	  n = 4;
	  if (i > 0)
	    XtSetArg (arg[n], XtNfromVert, w), n++;
	  if (j == 0)
	    XtSetArg (arg[n], XtNfromHoriz, head), n++;
	  else
	    XtSetArg (arg[n], XtNfromHoriz, glyph[k - 1]), n++;
	  XtSetArg (arg[n], XtNbitmap, none_pixmap), n++;
	  glyph[k] = XtCreateManagedWidget ("glyph", commandWidgetClass,
					    glyph_area, arg, n);
	  XtAddCallbackWithCast (int, glyph[k], RenderProc, k);
	}
      w = head;
    }
  /* 10 = (1 (border_width) + 4 (inner_width)) * 2 */
  XtSetArg(arg[4], XtNwidth, glyph_width + 10);
  XtSetArg (arg[5], XtNfromVert, glyph[112]);
  XtSetArg (arg[6], XtNfromHoriz, w);
  XtSetArg (arg[7], XtNborderWidth, 0);

  for (j = 0; j < 16; j++)
    {
      char str[3];

      sprintf (str, "X%X", j);
      XtSetArg (arg[8], XtNlabel, str);
      w = XtCreateManagedWidget ("idx", labelWidgetClass, glyph_area, arg, 9);
      XtSetArg (arg[6], XtNfromHoriz, w);
    }

  XtSetArg (arg[0], XtNleft, XawChainLeft);
  XtSetArg (arg[1], XtNright, XawChainLeft);
  XtSetArg (arg[2], XtNtop, XawChainTop);
  XtSetArg (arg[3], XtNbottom, XawChainTop);
  XtSetArg (arg[4], XtNorientation, XtorientHorizontal);
  XtSetArg (arg[5], XtNborderWidth, 0);
  w = XtCreateManagedWidget ("clear-box", boxWidgetClass, render_area, arg, 6);
  clear = XtCreateManagedWidget ("clear", commandWidgetClass, w, arg, 0);
  XtAddCallback (clear, XtNcallback, RenderProc, (XtPointer) -1);
  del = XtCreateManagedWidget ("delete", commandWidgetClass, w, arg, 0);
  XtAddCallback (del, XtNcallback, RenderProc, (XtPointer) -2);
  bidi = XtCreateManagedWidget ("L->R", toggleWidgetClass, w, arg, 0);
  XtAddCallback (bidi, XtNcallback, BidiProc, NULL);
  alt_subst = XtCreateManagedWidget ("AltSubst", toggleWidgetClass, w, arg, 0);
  XtAddCallback (alt_subst, XtNcallback, AltSubstProc, NULL);

  XtSetArg (arg[4], XtNorientation, XtorientHorizontal);
  XtSetArg (arg[5], XtNborderWidth, 0);
  XtSetArg (arg[6], XtNfromVert, w);
  raw = XtCreateManagedWidget ("raw", boxWidgetClass, render_area, arg, 7);

  XtSetArg (arg[0], XtNborderWidth, 0);
  XtSetArg (arg[1], XtNlabel, "raw: ");
  raw_label = XtCreateManagedWidget ("raw-label", labelWidgetClass,
				      raw, arg, 2);
  XtSetArg (arg[1], XtNbitmap, raw_pixmap);
  raw_image = XtCreateManagedWidget ("raw-image", labelWidgetClass,
				      raw, arg, 2);
  w = raw;
  if (otf)
    {
      OTF_get_table (otf, "GSUB");
      OTF_get_table (otf, "GPOS");
      w = create_otf_script_widgets (w);
      if (otf->gsub)
	{
	  gsub.label = "GSUB";
	  gsub.gsub_gpos = otf->gsub;
	  w = create_otf_widgets (w, &gsub);
	}
      if (otf->gpos)
	{
	  gpos.label = "GPOS";
	  gpos.gsub_gpos = otf->gpos;
	  w = create_otf_widgets (w, &gpos);
	}
    }

  XtSetArg (arg[6], XtNfromVert, w);
  seq = XtCreateManagedWidget ("seq", boxWidgetClass, render_area, arg, 7);
  XtSetArg (arg[0], XtNborderWidth, 0);
  XtSetArg (arg[1], XtNlabel, "seq: ");
  seq_label = XtCreateManagedWidget ("seq-label", labelWidgetClass,
				     seq, arg, 2);
  XtSetArg (arg[1], XtNbitmap, seq_pixmap);
  seq_image = XtCreateManagedWidget ("seq-image", labelWidgetClass,
				     seq, arg, 2);
  XtSetArg (arg[6], XtNfromVert, seq);
  code = XtCreateManagedWidget ("code", boxWidgetClass, render_area, arg, 7);
  XtSetArg (arg[0], XtNborderWidth, 0);
  XtSetArg (arg[1], XtNlabel, "code:");
  code_label = XtCreateManagedWidget ("code-label", labelWidgetClass,
				     code, arg, 2);
  XtSetArg (arg[1], XtNlabel, "");
  XtSetArg (arg[2], XtNwidth, render_width);
  code_list = XtCreateManagedWidget ("code-list", labelWidgetClass,
				     code, arg, 3);
  XtInstallAllAccelerators (shell, shell);
}

static void
ExposeProc (Widget w, XEvent *event, String *str, Cardinal *num)
{
  XTextProperty text_prop;
  char *pname = "otfview";
  char *fname = basename (filename);
  char *name = alloca (strlen (fname) + 3 + strlen (pname) + 1);

  sprintf (name, "%s - %s", pname, fname);
  text_prop.value = (unsigned char *) name;
  text_prop.encoding = XA_STRING;
  text_prop.format = 8;
  text_prop.nitems = strlen (name);
  XSetWMName (display, XtWindow (shell), &text_prop);
}

/* Format MSG by FMT and print the result to the stderr, and exit.  */

#define FATAL_ERROR(fmt, arg)	\
  do {				\
    fprintf (stderr, fmt, arg);	\
    exit (1);			\
  } while (0)

static int
x_error_handler (Display *display, XErrorEvent *error)
{
  return 0;
}

void
help (char **argv, int err)
{
  FILE *fp = err ? stderr : stdout;

  fprintf (fp, "Usage: %s [ X-OPTION ... ]  OTF-FILE [INDEX]\n",
	   basename (argv[0]));
  fprintf (fp, "  Environment variable PIXEL_SIZE specifies the pixel size.\n");
  fprintf (fp, "  The default pixel size is %d, but is reduced\n",
	   DEFAULT_PIXEL_SIZE);
  fprintf (fp, "  if your screen is not that wide.\n");
  exit (err);
}

int
main (int argc, char **argv)
{
  XtActionsRec actions[] = { {"Expose", ExposeProc} };
  Arg arg[10];

  FT_Library library;
  OTF_GlyphString gstring;
  OTF_Glyph *g;

  int err;
  int i;
  int fixed_pixel_size = 0;
  int display_width;

  pixel_size = DEFAULT_PIXEL_SIZE;
  {
    char *str = getenv ("PIXEL_SIZE");

    if (str && (i = atoi (str)) > 0)
      {
	pixel_size = i;
	fixed_pixel_size = 1;
      }
  }

  gstring.size = gstring.used = 256;
  g = calloc (256, sizeof (OTF_Glyph));
  gstring.glyphs = g;

  shell = XtOpenApplication (&context, "OTFView", NULL, 0, &argc, argv, NULL,
			     shellWidgetClass, arg, 0);
  display = XtDisplay (shell);
  /*XSynchronize (display, True);*/
  XSetErrorHandler (x_error_handler);
  display_width = DisplayWidth (display,
				XScreenNumberOfScreen (XtScreen (shell)));
  font = XLoadQueryFont (display, DEFAULT_FONT_NAME);
  if (! font)
    font = XLoadQueryFont (display, "fixed");

  if (argc < 2)
    help (argv, 1);
  if (!strcmp (argv[1], "-h") || !strcmp (argv[1], "--help"))
    help (argv, 0);
  filename = argv[1];
  if (argc > 2)
    {
      fontindex = atoi (argv[2]);
      if (fontindex < 0)
	FATAL_ERROR ("Invalid font index: %d\n", fontindex);
    }

  if ((err = FT_Init_FreeType (&library)))
    FATAL_ERROR ("%s\n", "FT_Init_FreeType: error");
  err = FT_New_Face (library, filename, fontindex, &face);
  if (err == FT_Err_Unknown_File_Format)
    FATAL_ERROR ("%s\n", "FT_New_Face: unknown file format");
  else if (err)
    FATAL_ERROR ("%s\n", "FT_New_Face: unknown error (invalid face index?)");
  if ((err = FT_Set_Pixel_Sizes (face, 0, pixel_size)))
    FATAL_ERROR ("%s\n", "FT_Set_Pixel_Sizes: error");

  if (strstr (filename, ".ttf")
      || strstr (filename, ".TTF")
      || strstr (filename, ".otf")
      || strstr (filename, ".OTF"))
    {
      otf = OTF_open_ft_face (face);
      if (otf)
	{
	  if (OTF_get_table (otf, "head") < 0
	      || OTF_get_table (otf, "cmap") < 0
	      || (OTF_check_table (otf, "GSUB") < 0
		  && OTF_check_table (otf, "GPOS") < 0))
	    {
	      OTF_close (otf);
	      otf = NULL;
	    }
	}
      if (otf)
	for (i = 0; i < otf->cmap->numTables; i++)
	  if (otf->cmap->EncodingRecord[i].subtable.format == 14)
	    {
	      sub14 = otf->cmap->EncodingRecord[i].subtable.f.f14;
	      break;
	    }
    }

  {
    char title[256];
    Arg arg[1];

    filename = basename (filename);
    sprintf (title, "%s family:%s style:%s",
	     filename, face->family_name, face->style_name);
    XtSetArg (arg[0], XtNtitle, title);
    XtSetValues (shell, arg, 1);
  }

  glyph_width = ((face->bbox.xMax - face->bbox.xMin)
		 * pixel_size / face->units_per_EM);
  if (! fixed_pixel_size && glyph_width * 16 > display_width * 0.8)
    {
      pixel_size = (pixel_size * display_width * 0.8 / 16 / glyph_width);
      FT_Set_Pixel_Sizes (face, 0, pixel_size);
      glyph_width = ((face->bbox.xMax - face->bbox.xMin)
		     * pixel_size / face->units_per_EM);
    }
  if (glyph_width < FONT_WIDTH * 4)
    glyph_width = FONT_WIDTH * 4;

  glyph_height = ((face->bbox.yMax - face->bbox.yMin)
		  *  pixel_size / face->units_per_EM);

  glyph_x = - (face->bbox.xMin * pixel_size / face->units_per_EM);
  glyph_y = face->bbox.yMax * pixel_size / face->units_per_EM;
  none_pixmap = XCreatePixmap (display, DefaultRootWindow (display),
			       glyph_width, glyph_height, 1);

  {
    unsigned long valuemask =  GCFunction | GCLineWidth;
    XGCValues values;

    gc = XCreateGC (display, none_pixmap, (unsigned long) 0, NULL);
    XSetFont (display, gc, font->fid);
    values.function = GXset;
    values.line_width = 1;
    gc_set = XCreateGC (display, none_pixmap, valuemask, &values);
    XSetFont (display, gc_set, font->fid);
    values.function = GXor;
    gc_or = XCreateGC (display, none_pixmap, valuemask, &values);
    values.function = GXcopyInverted;
    gc_inv = XCreateGC (display, none_pixmap, valuemask, &values);
  }

  XFillRectangle (display, none_pixmap, gc, 0, 0,
		  glyph_width, glyph_height);
  XDrawString (display, none_pixmap, gc_inv,
	       (glyph_width - XTextWidth (font, "none", 4)) / 2,
	       glyph_height / 2, "none", 4);

  render_width = (glyph_width + 4) * 15 + 1;
  render_height = glyph_height + 2;

  charmap_rec[0].platform_id = -1;
  charmap_rec[0].encoding_id = -1;
  strcpy (charmap_rec[0].name, "no charmap");

  for (i = 0; i < face->num_charmaps; i++)
    {
      charmap_rec[i + 1].platform_id = face->charmaps[i]->platform_id;
      charmap_rec[i + 1].encoding_id = face->charmaps[i]->encoding_id;
      sprintf (charmap_rec[i + 1].name, "%d-%d",
	       charmap_rec[i + 1].platform_id, charmap_rec[i + 1].encoding_id);
      if (face->charmaps[i]->platform_id == 0
	  || (face->charmaps[i]->platform_id == 3
	      && face->charmaps[i]->encoding_id == 1))
	strcat (charmap_rec[i + 1].name, " (unicode)");
      else if (face->charmaps[i]->platform_id == 1
	       && face->charmaps[i]->encoding_id == 0)
	strcat (charmap_rec[i + 1].name, " (apple-roman)");
    }

  raw_pixmap = XCreatePixmap (display, DefaultRootWindow (display),
			      render_width, render_height, 1);
  seq_pixmap = XCreatePixmap (display, DefaultRootWindow (display),
			      render_width, render_height, 1);

  memset (bitmap, 0, sizeof (bitmap));
  create_widgets ();
  glyph_index = 0;
  charmap_index = -1;
  update_glyph_area ();
  update_render_area ();

  XtAppAddActions (context, actions, XtNumber (actions));
  XtRealizeWidget (shell);
  XtAppMainLoop (context);

  exit (0);
}

#else /* not HAVE_X11_XAW_COMMAND_H */

int
main (int argc, char **argv)
{
  fprintf (stderr, 
	   "Building of this program failed (lack of some header files)\n");
  exit (1);
}

#endif
