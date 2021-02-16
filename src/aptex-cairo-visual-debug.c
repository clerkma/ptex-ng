/*
   Copyright 2017, 2018, 2021 Clerk Ma

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA.
*/

#include <stdio.h>
#include <stdint.h>

#include "cairo.h"

#if CAIRO_HAS_PDF_SURFACE
#include "cairo-pdf.h"
#endif

static cairo_surface_t * aptex_cairo_surface;
static cairo_t * aptex_cairo_visual_debug;

void aptex_vdbg_ship_open (const char * out_name)
{
#if CAIRO_HAS_PDF_SURFACE
  aptex_cairo_surface = cairo_pdf_surface_create(out_name, 595.0, 842.0);
  aptex_cairo_visual_debug = cairo_create(aptex_cairo_surface);

#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1,16,0)
  cairo_pdf_surface_set_metadata(aptex_cairo_surface, CAIRO_PDF_METADATA_SUBJECT, "visual-debug");
  cairo_pdf_surface_set_metadata(aptex_cairo_surface, CAIRO_PDF_METADATA_CREATOR, "ApTeX's cairo backend (2021).");
#endif

#endif
}

void aptex_vdbg_ship_close (void)
{
#if CAIRO_HAS_PDF_SURFACE
  cairo_destroy(aptex_cairo_visual_debug);
  cairo_surface_destroy(aptex_cairo_surface);
#endif
}

void aptex_vdbg_bop (double w, double h, double x, double y)
{
#if CAIRO_HAS_PDF_SURFACE
  cairo_save(aptex_cairo_visual_debug);
  cairo_scale(aptex_cairo_visual_debug, 1 / 65536.0, 1 / 65536.0);
  cairo_translate(aptex_cairo_visual_debug, x, y);
  cairo_pdf_surface_set_size(aptex_cairo_surface, w / 65536.0, h / 65536.0);
#endif
}

void aptex_vdbg_eop (void)
{
#if CAIRO_HAS_PDF_SURFACE
  cairo_restore(aptex_cairo_visual_debug);
  cairo_show_page(aptex_cairo_visual_debug);
#endif
}

void aptex_vdbg_node_char (int32_t dir, int32_t x, int32_t y, int32_t w, int32_t h, int32_t d)
{
#if CAIRO_HAS_PDF_SURFACE
  cairo_save(aptex_cairo_visual_debug);
  cairo_rectangle(aptex_cairo_visual_debug, x, y - h, w, h + d);
  cairo_set_source_rgb(aptex_cairo_visual_debug, 1.0, 0.0, 0.0);
  cairo_set_line_width(aptex_cairo_visual_debug, 0.1);
  cairo_stroke(aptex_cairo_visual_debug);
  cairo_set_line_width(aptex_cairo_visual_debug, 0.1);
  cairo_set_source_rgb(aptex_cairo_visual_debug, 0.0, 0.0, 1.0);
  cairo_move_to(aptex_cairo_visual_debug, x, y);
  cairo_line_to(aptex_cairo_visual_debug, x + w, y);
  cairo_stroke(aptex_cairo_visual_debug);
  cairo_restore(aptex_cairo_visual_debug);
#endif
}

void aptex_vdbg_node_rule (int32_t dir, int32_t x, int32_t y, int32_t w, int32_t h)
{
#if CAIRO_HAS_PDF_SURFACE
  cairo_save(aptex_cairo_visual_debug);
  cairo_rectangle(aptex_cairo_visual_debug, x, y - h, w, h);
  cairo_set_source_rgb(aptex_cairo_visual_debug, 0.0, 0.0, 0.0);
  cairo_fill(aptex_cairo_visual_debug);
  cairo_restore(aptex_cairo_visual_debug);
#endif
}

