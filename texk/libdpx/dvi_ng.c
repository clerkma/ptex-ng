/* Copyright 2014 Clerk Ma

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
   02110-1301 USA.  */

spt_t ng_packet_width (int32_t ch, int ng_font_id)
{
  struct loaded_font * font;
  spt_t width;

  font = &loaded_fonts[ng_font_id];
  width = tfm_get_fw_width(font->tfm_id, ch);
  width = sqxfw(font->size, width);

  return width;
}

extern void ng_set_packet (int32_t ch, int vf_font, int32_t h, int32_t v);

void ng_gid (uint16_t gid, int ng_font_id, int32_t h, int32_t v)
{
  struct loaded_font *font;
  unsigned char wbuf[2];
  spt_t width, height, depth;
  unsigned advance;
  double ascent, descent;

  font = &loaded_fonts[ng_font_id];
  ascent = (double) font->ascent;
  descent = (double) font->descent;

  if (font->cffont)
  {
    cff_index * cstrings = font->cffont->cstrings;
    t1_ginfo gm;

    if (font->cffont->is_notdef_notzero)
      gid += 1;

    t1char_get_metrics(cstrings->data + cstrings->offset[gid] - 1,
        cstrings->offset[gid + 1] - cstrings->offset[gid], 
        font->cffont->subrs[0], &gm);
    advance = font->layout_dir == 0 ? gm.wx : gm.wy;
    ascent = gm.bbox.ury;
    descent = gm.bbox.lly;
  }
  else
  {
    advance = font->hvmt[gid].advance;
  }

  width = (double) font->size * (double) advance / (double) font->unitsPerEm;
  width = width * font->extend;

  if (dvi_is_tracking_boxes())
  {
    pdf_rect rect;
    height = (double) font->size * ascent / (double) font->unitsPerEm;
    depth  = (double) font->size * -descent / (double) font->unitsPerEm;
    pdf_dev_set_rect(&rect, h, v, width, height, depth);
    pdf_doc_expand_box(&rect);
  }

  wbuf[0] = gid >> 8;
  wbuf[1] = gid & 0xff;
  pdf_dev_set_string(h, v, wbuf, 2, width, font->font_id, -1);
}

void ng_layer (uint16_t gid, int ng_font_id, int32_t h, int32_t v, uint8_t r, uint8_t g, uint8_t b)
{
  struct loaded_font *font;
  unsigned char wbuf[2];
  spt_t width, height, depth;
  unsigned advance;
  double ascent, descent;

  font = &loaded_fonts[ng_font_id];
  ascent = (double) font->ascent;
  descent = (double) font->descent;
  {
    pdf_color color;
    pdf_color_rgbcolor(&color,
      (double)(r) / 255,
      (double)(g) / 255,
      (double)(b) / 255);
    pdf_color_push(&color, &color);
  }

  if (font->cffont)
  {
    cff_index * cstrings = font->cffont->cstrings;
    t1_ginfo gm;

    if (font->cffont->is_notdef_notzero)
      gid += 1;

    t1char_get_metrics(cstrings->data + cstrings->offset[gid] - 1,
        cstrings->offset[gid + 1] - cstrings->offset[gid], 
        font->cffont->subrs[0], &gm);
    advance = font->layout_dir == 0 ? gm.wx : gm.wy;
    ascent = gm.bbox.ury;
    descent = gm.bbox.lly;
  }
  else
  {
    advance = font->hvmt[gid].advance;
  }

  width = (double) font->size * (double) advance / (double) font->unitsPerEm;
  width = width * font->extend;

  if (dvi_is_tracking_boxes())
  {
    pdf_rect rect;
    height = (double) font->size * ascent / (double) font->unitsPerEm;
    depth  = (double) font->size * -descent / (double) font->unitsPerEm;
    pdf_dev_set_rect(&rect, h, v, width, height, depth);
    pdf_doc_expand_box(&rect);
  }

  wbuf[0] = gid >> 8;
  wbuf[1] = gid & 0xff;
  pdf_dev_set_string(h, v, wbuf, 2, width, font->font_id, -1);
  {
    pdf_color_pop();
  }
}

void ng_set (int32_t ch, int ng_font_id, int32_t h, int32_t v)
{
  struct loaded_font * font;
  spt_t width, height, depth;
  unsigned char wbuf[4];

  font = &loaded_fonts[ng_font_id];
  width = tfm_get_fw_width(font->tfm_id, ch);
  width = sqxfw(font->size, width);

  switch (font->type)
  {
    case PHYSICAL:
      if (ch > 65535)
      {
        wbuf[0] = (UTF32toUTF16HS(ch) >> 8) & 0xff;
        wbuf[1] =  UTF32toUTF16HS(ch)       & 0xff;
        wbuf[2] = (UTF32toUTF16LS(ch) >> 8) & 0xff;
        wbuf[3] =  UTF32toUTF16LS(ch)       & 0xff;
        pdf_dev_set_string(h, v, wbuf, 4, width, font->font_id, 2);
      }
      else if (ch > 255)
      {
        wbuf[0] = (ch >> 8) & 0xff;
        wbuf[1] =  ch & 0xff;
        pdf_dev_set_string(h, v, wbuf, 2, width, font->font_id, 2);
      }
      else if (font->subfont_id >= 0)
      {
        unsigned short uch = lookup_sfd_record(font->subfont_id, (unsigned char) ch);
        wbuf[0] = (uch >> 8) & 0xff;
        wbuf[1] =  uch & 0xff;
        pdf_dev_set_string(h, v, wbuf, 2, width, font->font_id, 2);
      }
      else
      {
        wbuf[0] = (unsigned char) ch;
        pdf_dev_set_string(h, v, wbuf, 1, width, font->font_id, 1);
      }

      if (dvi_is_tracking_boxes())
      {
        pdf_rect rect;

        height = tfm_get_fw_height(font->tfm_id, ch);
        depth  = tfm_get_fw_depth (font->tfm_id, ch);
        height = sqxfw(font->size, height);
        depth  = sqxfw(font->size, depth);
        pdf_dev_set_rect(&rect, h, v, width, height, depth);
        pdf_doc_expand_box(&rect);
      }
      break;

    case VIRTUAL:
      ng_set_packet(ch, font->font_id, h, v);
      break;
  }
}
