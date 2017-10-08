/*
   Copyright 2017 Clerk Ma

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

/* libotf and freetype for opentype support */

#include "otf.h"

#include FT_GLYPH_H
#include FT_TRUETYPE_TABLES_H

typedef struct {
  uint16_t GID;
  uint16_t firstLayerIndex;
  uint16_t numLayers;
} ot_base_glyph;

typedef struct {
  uint16_t GID;
  uint16_t paletteIndex;
} ot_layer;

typedef struct {
  uint16_t version;
  uint16_t numBaseGlyphRecords;
  uint16_t numLayerRecords;
  ot_base_glyph * base_glyphs;
  ot_layer * layers;
} ot_tbl_colr;

typedef struct {
  uint8_t blue;
  uint8_t green;
  uint8_t red;
  uint8_t alpha;
} ot_color;

typedef struct {
  uint16_t version;
  uint16_t numPalettesEntries;
  uint16_t numPalette;
  uint16_t numColorRecords;
  uint16_t * colorRecordIndices;
  ot_color * colorRecords;
  uint32_t * paletteType;
  uint16_t * paletteLabel;
  uint16_t * paletteEntryLabel;
} ot_tbl_cpal;

typedef struct {
  uint16_t baseTagCount;
  uint32_t * baselineTags;
} ot_base_tag_list;

typedef struct {
  uint16_t baseCoordFormat;
  union {
    struct {
      int16_t coordinate;
    } f1;
    struct {
      int16_t coordinate;
      uint16_t referenceGlyph;
      uint16_t baseCoordPoint;
    } f2;
    struct {
      int16_t coordinate;
      uint16_t deviceTable;
    } f3;
  };
} ot_base_coord;

typedef struct {
  uint32_t featureTableTag;
  ot_base_coord * minCoord;
  ot_base_coord * maxCoord;
} ot_feat_min_max_record;

typedef struct {
  ot_base_coord * minCoord;
  ot_base_coord * maxCoord;
  uint16_t featMinMaxCount;
  ot_feat_min_max_record * featMinMaxRecords;
} ot_min_max;

typedef struct {
  uint16_t defaultBaselineIndex;
  uint16_t baseCoordCount;
  ot_base_coord * baseCoords;
} ot_base_values;

typedef struct {
  uint32_t baseLangSysTag;
  ot_min_max * minMax;
} ot_base_lang_sys_record;

typedef struct {
  ot_base_values * baseValues;
  ot_min_max * defaultMinMax;
  uint16_t baseLangSysCount;
  ot_base_lang_sys_record * baseLangSysRecords;
} ot_base_script;

typedef struct {
  uint32_t baseScriptTag;
  ot_base_script * baseValues;
} ot_base_script_record;

typedef struct {
  uint16_t baseScriptCount;
  ot_base_script_record * baseScriptRecords;
} ot_base_script_list;

typedef struct {
  ot_base_tag_list * baseTagList;
  ot_base_script_list * baseScriptList;
} ot_axis;

typedef struct {
  uint16_t majorVersion;
  uint16_t minorVersion;
  ot_axis * horizAxis;
  ot_axis * vertAxis;
} ot_tbl_base;

ot_tbl_colr * ot_parse_colr (FT_Face face);
void ot_delete_colr (ot_tbl_colr * colr);
ot_tbl_cpal * ot_parse_cpal (FT_Face face);
void ot_delete_cpal (ot_tbl_cpal * cpal);
ot_tbl_base * ot_parse_base (FT_Face face);
void ot_delete_base (ot_tbl_base * cpal);

