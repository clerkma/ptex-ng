/*
   Copyright 2017, 2018 Clerk Ma

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

#include "aptex-opentype.h"

static uint16_t parse_u16 (uint8_t * s)
{
  return (*s << 8) | (*(s + 1));
}

static uint32_t parse_u32 (uint8_t * s)
{
  return (*s << 24) | (*(s + 1) << 16) | (*(s + 2) << 8) | (*(s + 3));
}

ot_tbl_colr * ot_parse_colr (FT_Face face)
{
  FT_ULong tbl_len = 0;
  uint8_t * tbl_buf;
  uint32_t offset_base_glyph;
  uint32_t offset_layer;
  ot_tbl_colr * colr;
  uint32_t i;
  FT_ULong tag = FT_MAKE_TAG('C', 'O', 'L', 'R');

  if (FT_Load_Sfnt_Table(face, tag, 0, NULL, &tbl_len))
    return NULL;
  tbl_buf = calloc(tbl_len, sizeof(uint8_t));
  if (FT_Load_Sfnt_Table(face, tag, 0, tbl_buf, &tbl_len))
  {
    free(tbl_buf);
    return NULL;
  }
  colr = calloc(1, sizeof(ot_tbl_colr));
  colr->version             = parse_u16(tbl_buf);
  colr->numBaseGlyphRecords = parse_u16(tbl_buf + 2);
  offset_base_glyph         = parse_u32(tbl_buf + 4);
  offset_layer              = parse_u32(tbl_buf + 8);
  colr->numLayerRecords     = parse_u16(tbl_buf + 12);
  colr->base_glyphs         = malloc(sizeof(ot_base_glyph) * colr->numBaseGlyphRecords);
  colr->layers              = malloc(sizeof(ot_layer) * colr->numLayerRecords);

  for (i = 0; i < colr->numBaseGlyphRecords; i++)
  {
    colr->base_glyphs[i].GID
      = parse_u16(tbl_buf + offset_base_glyph + i * 6);
    colr->base_glyphs[i].firstLayerIndex
      = parse_u16(tbl_buf + offset_base_glyph + i * 6 + 2);
    colr->base_glyphs[i].numLayers
      = parse_u16(tbl_buf + offset_base_glyph + i * 6 + 4);
  }

  for (i = 0; i < colr->numLayerRecords; i++)
  {
    colr->layers[i].GID
      = parse_u16(tbl_buf + offset_layer + i * 4);
    colr->layers[i].paletteIndex
      = parse_u16(tbl_buf + offset_layer + i * 4 + 2);
  }

  free(tbl_buf);

  return colr;
}

void ot_delete_colr (ot_tbl_colr * colr)
{
  if (colr != NULL)
  {
    if (colr->base_glyphs != NULL)
      free(colr->base_glyphs);

    if (colr->layers != NULL)
      free(colr->layers);

    free(colr);
  }
}

ot_tbl_cpal * ot_parse_cpal (FT_Face face)
{
  FT_ULong tbl_len = 0;
  uint8_t * tbl_buf;
  uint32_t offset_first_color;
  uint32_t offset_palette_type;
  uint32_t offset_palette_label;
  uint32_t offset_palette_entry_label;
  ot_tbl_cpal * cpal;
  uint32_t i;
  FT_ULong tag = FT_MAKE_TAG('C', 'P', 'A', 'L');

  if (FT_Load_Sfnt_Table(face, tag, 0, NULL, &tbl_len))
    return NULL;
  tbl_buf = calloc(tbl_len, sizeof(uint8_t));
  if (FT_Load_Sfnt_Table(face, tag, 0, tbl_buf, &tbl_len))
  {
    free(tbl_buf);
    return NULL;
  }
  cpal = calloc(1, sizeof(ot_tbl_cpal));
  cpal->version             = parse_u16(tbl_buf);
  cpal->numPalettesEntries  = parse_u16(tbl_buf + 2);
  cpal->numPalette          = parse_u16(tbl_buf + 4);
  cpal->numColorRecords     = parse_u16(tbl_buf + 6);
  offset_first_color        = parse_u32(tbl_buf + 8);
  cpal->colorRecordIndices  = malloc(sizeof(uint16_t) * cpal->numPalette);
  cpal->colorRecords        = malloc(sizeof(ot_color) * cpal->numColorRecords);

  for (i = 0; i < cpal->numPalette; i++)
    cpal->colorRecordIndices[i] = parse_u16(tbl_buf + 12 + i * 2);

  for (i = 0; i < cpal->numColorRecords; i++)
  {
    cpal->colorRecords[i].blue
      = *(tbl_buf + offset_first_color + i * 4);
    cpal->colorRecords[i].green
      = *(tbl_buf + offset_first_color + i * 4 + 1);
    cpal->colorRecords[i].red
      = *(tbl_buf + offset_first_color + i * 4 + 2);
    cpal->colorRecords[i].alpha
      = *(tbl_buf + offset_first_color + i * 4 + 3);
  }

  if (cpal->version == 1)
  {
    offset_palette_type
      = parse_u32(tbl_buf + 12 + 2 * cpal->numPalette);
    offset_palette_label
      = parse_u32(tbl_buf + 12 + 2 * cpal->numPalette + 2);
    offset_palette_entry_label
      = parse_u32(tbl_buf + 12 + 2 * cpal->numPalette + 4);

    cpal->paletteType
      = malloc(sizeof(uint32_t) * cpal->numPalette);
    cpal->paletteLabel
      = malloc(sizeof(uint16_t) * cpal->numPalette);
    cpal->paletteEntryLabel
      = malloc(sizeof(uint16_t) * cpal->numPalettesEntries);
    
    for (i = 0; i < cpal->numPalette; i++)
      cpal->paletteType[i] = parse_u32(tbl_buf + offset_palette_type + i * 4);

    for (i = 0; i < cpal->numPalette; i++)
      cpal->paletteLabel[i] = parse_u16(tbl_buf + offset_palette_label + i * 2);

    for (i = 0; i < cpal->numPalettesEntries; i++)
      cpal->paletteEntryLabel[i] = parse_u16(tbl_buf + offset_palette_entry_label + i * 2);
  }
  else
  {
    cpal->paletteType       = NULL;
    cpal->paletteLabel      = NULL;
    cpal->paletteEntryLabel = NULL;
  }

  free(tbl_buf);

  return cpal;
}

void ot_delete_cpal (ot_tbl_cpal * cpal)
{
  if (cpal != NULL)
  {
    if (cpal->colorRecordIndices != NULL)
      free(cpal->colorRecordIndices);

    if (cpal->colorRecords != NULL)
      free(cpal->colorRecords);

    if (cpal->paletteType != NULL)
      free(cpal->paletteType);

    if (cpal->paletteLabel != NULL)
      free(cpal->paletteLabel);

    if (cpal->paletteEntryLabel != NULL)
      free(cpal->paletteEntryLabel);

    free(cpal);
  }
}

static ot_axis * parse_ot_axis (uint8_t * s, uint16_t o)
{
  uint16_t offset = parse_u16(s + o);
  uint32_t i;

  if (offset == 0)
  {
    return NULL;
  }
  else
  {
    ot_axis * axis = calloc(1, sizeof(ot_axis));
    uint16_t offset_t_list = parse_u16(s + offset);
    uint16_t offset_s_list = parse_u16(s + offset + 2);
    
    if (offset_t_list != 0)
    {
      axis->baseTagList = calloc(1, sizeof(ot_base_tag_list));
      axis->baseTagList->baseTagCount = parse_u16(s + offset + offset_t_list);
      axis->baseTagList->baselineTags = calloc(axis->baseTagList->baseTagCount, sizeof(uint32_t));

      for (i = 0; i < axis->baseTagList->baseTagCount; i++)
      {
        axis->baseTagList->baselineTags[i] = parse_u16(s + offset + offset_t_list + 2 + i * 4);
      }
    }
    else
    {
      axis->baseTagList = NULL;
    }
    
    axis->baseScriptList = calloc(1, sizeof(ot_base_script_list));
    axis->baseScriptList->baseScriptCount = parse_u16(s + offset + offset_s_list);
    axis->baseScriptList->baseScriptRecords = calloc(axis->baseScriptList->baseScriptCount, sizeof(ot_base_script_record));
    
    for (i = 0; i < axis->baseScriptList->baseScriptCount; i++)
    {
      //axis->baseScriptList->baseScriptRecords[i] = parse_ot_base_script_record(s, offset )
    }
    
    return axis;
  }
}

ot_tbl_base * ot_parse_base (FT_Face face)
{
  FT_ULong tbl_len = 0;
  uint8_t * tbl_buf;
  ot_tbl_base * base;
  uint32_t i;
  FT_ULong tag = FT_MAKE_TAG('B', 'A', 'S', 'E');

  if (FT_Load_Sfnt_Table(face, tag, 0, NULL, &tbl_len))
    return NULL;
  tbl_buf = calloc(tbl_len, sizeof(uint8_t));
  if (FT_Load_Sfnt_Table(face, tag, 0, tbl_buf, &tbl_len))
  {
    free(tbl_buf);
    return NULL;
  }
  base = calloc(1, sizeof(ot_tbl_base));
  base->majorVersion = parse_u16(tbl_buf);
  base->minorVersion = parse_u16(tbl_buf + 2);
  base->horizAxis = parse_ot_axis(tbl_buf, 4);
  base->vertAxis = parse_ot_axis(tbl_buf, 6);

  free(tbl_buf);

  return base;
}

void ot_delete_base (ot_tbl_base * cpal)
{
  /* TODO*/
}
