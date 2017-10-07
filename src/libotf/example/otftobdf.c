/* otftobdf.c -- Generate BDF font from OpenType font.

Copyright (C) 2003, 2004
  National Institute of Advanced Industrial Science and Technology (AIST)
  Registration Number H15PRO167

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

#include <ft2build.h>
#include FT_FREETYPE_H

#include "config.h"
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#define DEFAULT_PIXEL_SIZE 16

FT_Face face;

/* Format MSG by FMT and print the result to the stderr, and exit.  */

#define FATAL_ERROR(fmt, arg)	\
  do {				\
    fprintf (stderr, fmt, arg);	\
    exit (1);			\
  } while (0)

char *registry;

void
dump_header (FT_Face face, char *foundry, int nchars, int pixel_size)
{
  int width = ((face->bbox.xMax - face->bbox.xMin)
	       * pixel_size / face->units_per_EM);
  int height = ((face->bbox.yMax - face->bbox.yMin)
		* pixel_size / face->units_per_EM);
  int x = face->bbox.xMin * pixel_size / face->units_per_EM;
  int y = face->bbox.yMin * pixel_size / face->units_per_EM;

  printf ("STARTFONT 2.1\n");
  printf ("FONT -%s-%s-%s-R-Normal--%d-%d-72-72-C-%d-%s\n",
	  foundry, face->family_name, face->style_name,
	  pixel_size, pixel_size * 10, pixel_size * 10, registry);
  printf ("SIZE %d 72 72\n", pixel_size);
  printf ("FONTBOUNDINGBOX %d %d %d %d\n", width, height, x, y);
  printf ("STARTPROPERTIES 2\n");
  printf ("FONT_ASCENT %d\n", y + height);
  printf ("FONT_DESCENT %d\n", -y);
  printf ("ENDPROPERTIES 0\n");
  printf ("CHARS %d\n", nchars);
}

void
dump_tailer ()
{
  printf ("ENDFONT\n");
}

void
dump_image (int pixel_size, int index, int code, int full)
{
  int err = FT_Load_Glyph (face, index, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
  int i,j;
  unsigned char *buf;
  FT_GlyphSlot glyph;
  int dwidth, x, y;

  if (err)
    return;
  glyph = face->glyph;
  if (glyph->bitmap.rows * glyph->bitmap.width == 0)
    return;
  printf ("STARTCHAR U+%04X\n", code);
  printf ("ENCODING %d\n", code);
  printf ("SWIDTH %d 0\n",
	  (int) (glyph->metrics.horiAdvance >> 6) * 1000 / pixel_size);
  if (full)
    {
      dwidth = glyph->bitmap.width;
      x = 0;
    }
  else
    {
      dwidth = glyph->metrics.horiAdvance >> 6;
      x = glyph->metrics.horiBearingX >> 6;
    }
  y = (glyph->metrics.horiBearingY - glyph->metrics.height) >> 6;
  printf ("DWIDTH %d 0\n", dwidth);
  printf ("BBX %d %d %d %d\n", glyph->bitmap.width, glyph->bitmap.rows, x, y);
  printf ("BITMAP\n");
  buf = (unsigned char *) glyph->bitmap.buffer;
  for (i = 0; i < glyph->bitmap.rows; i++)
    {
      for (j = 0; j < (glyph->bitmap.width + 7) / 8; j++)
	printf ("%02X", buf[i * glyph->bitmap.pitch + j]);
      printf ("\n");
    }
  printf ("ENDCHAR\n");
}


int
main (int argc, char **argv)
{
  FT_Library library;
  int err;
  int i;
  int pixel_size = DEFAULT_PIXEL_SIZE;
  FT_UInt code_table[0x10000];
  int nchars;
  char *filename;
  int platform_id, encoding_id;

  if (argc <= 1)
    FATAL_ERROR ("Usage: %s ENCODING OTF-FILE\n", argv[0]);
  if (sscanf (argv[1], "%d-%d", &platform_id, &encoding_id) != 2)
    {
      platform_id = -1;
      filename = argv[1];
    }
  else
    filename = argv[2];

  if ((err = FT_Init_FreeType (&library)))
    FATAL_ERROR ("%s\n", "FT_Init_FreeType: error");
  err = FT_New_Face (library, filename, 0, &face);
  if (err == FT_Err_Unknown_File_Format)
    FATAL_ERROR ("%s\n", "FT_New_Face: unknown file format");
  else if (err)
    FATAL_ERROR ("%s\n", "FT_New_Face: unknown error");
  if (platform_id >= 0)
    {
      for (i = 0; i < face->num_charmaps; i++)
	if (face->charmaps[i]->platform_id == platform_id
	    && face->charmaps[i]->encoding_id == encoding_id)
	  break;
      if (i == face->num_charmaps)
	FATAL_ERROR ("Unknown ENCODING: %s\n", argv[1]);
      FT_Set_Charmap (face, face->charmaps[i]);
      if (platform_id == 0)
	{
	  if (encoding_id == 3)
	    registry = "iso10646-1";
	  else if (face->charmaps[i]->encoding_id == 4)
	    registry = "iso10646-full";
	}
      else if (face->charmaps[i]->platform_id == 3)
	{
	  if (face->charmaps[i]->encoding_id == 1)
	    registry = "iso10646-1";
	  else if (face->charmaps[i]->encoding_id == 10)
	    registry = "iso10646-full";
	}
      else
	{
	  registry = alloca (256);
	  sprintf (registry, "%d-%d", platform_id, encoding_id);
	}
    }
  else
    {
      registry = "raw-glyph";
    }

  {
    char *str = getenv ("PIXEL_SIZE");

    if (str && (i = atoi (str)) > 0)
      pixel_size = i;
  }

  if ((err = FT_Set_Pixel_Sizes (face, 0, pixel_size)))
    FATAL_ERROR ("%s\n", "FT_Set_Pixel_Sizes: error");

  /*
  for (i = nchars = 0; i < 0x10000; i++)
    if (! FT_Load_Glyph (face, i, FT_LOAD_RENDER | FT_LOAD_MONOCHROME)
	&& face->glyph->bitmap.rows * face->glyph->bitmap.width)
      nchars++;
  */
  for (i = 0; i < 0x10000; i++)
    {
      if (platform_id >= 0)
	{
	  code_table[i] = FT_Get_Char_Index (face, (FT_ULong) i);
	  if (! code_table[i])
	    {
	      code_table[i] = -1;
	      continue;
	    }
	}
      else
	{
	  code_table[i] = i;
	}
      if (! FT_Load_Glyph (face, code_table[i],
			   FT_LOAD_RENDER | FT_LOAD_MONOCHROME)
	  && face->glyph->bitmap.rows * face->glyph->bitmap.width)
	nchars++;
      else
	code_table[i] = -1;
    }

  dump_header (face, "unknown", nchars, pixel_size);
  for (i = 0; i < 0x10000; i++)
    if (code_table[i] >= 0)
      dump_image (pixel_size, code_table[i], i, 0);
  dump_tailer ();

  exit (0);
}
