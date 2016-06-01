/* otfopen.c -- OpenType font reader.

Copyright (C) 2003, 2004, 2005, 2006, 2008, 2009, 2010
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
#include <stdlib.h>
#include <string.h>
#include <config.h>

#include "otf.h"
#include "internal.h"

#include FT_TRUETYPE_TABLES_H

/***
    Table of contents (almost parallel to otf.h):

    (0) Stream handler

    (1) Readers for OTF Layout tables and OTF itself
    (1-1) Basic types and functions
    (1-2) "head" table
    (1-3) "name" table
    (1-4) "cmap" table
    (1-5) Structures common to GDEF, GSUB, and GPOS
    (1-6) "GDEF" table
    (1-7) Structures for ScriptList, FeatureList, and LookupList
    (1-8) Structures common to GSUB and GPOS
    (1-9) "GSUB" table
    (1-10) "GPOS" table
    (1-11) Structure for OTF

    (2) API for reading OTF
    (2-1) OTF_open()
    (2-2) OTF_close()
    (2-3) OTF_get_table()
    (2-4) OTF_check_table()

    (5) API miscellaneous
*/

int debug_flag = -1;

static void
set_debug_flag ()
{
  debug_flag = getenv ("LIBOTF_DEBUG") != NULL;
}


/* (0) Stream handler

   Example of typical usage of OTF_Stream.

    {
      OTF_Stream *stream;
      OTF_StreamState state;
      int offset, nbytes;

      OPEN_STREAM (_FILE_NAME_, stream);
      if (! stream)
	_ERROR_;
      SETUP_STREAM (stream, fp, 0, 256, _NAME_);
      offset = READ_OFFSET (stream);
      nbytes = READ_ULONG (stream);
      SETUP_STREAM (stream, fp, offset, nbytes, _NAME2_);
      ...;
      CLOSE_STREAM (stream);
    }

*/

typedef struct
{
  const char *name;
  long pos;
  long bufsize;
  long allocated;
  unsigned char *buf;
} OTF_Stream;

typedef long OTF_StreamState;

static OTF_Stream *
make_stream (const char *name)
{
  OTF_Stream *stream;
  char *errfmt = "stream creation%s";
  void *errret = NULL;

  stream = calloc (1, sizeof (OTF_Stream));
  if (! stream)
    OTF_ERROR (OTF_ERROR_MEMORY, "");
  stream->name = name;
  return stream;
}

static int
setup_stream (OTF_Stream *stream, FILE *fp, long offset, int nbytes)
{
  char *errfmt = "stream setup for %s";
  int errret = -1;

  stream->pos = 0;
  if (stream->allocated < nbytes)
    {
      unsigned char *buf = malloc (nbytes);

      if (! buf)
	OTF_ERROR (OTF_ERROR_MEMORY, stream->name);
      if (stream->buf)
	free (stream->buf);
      stream->buf = buf;
      stream->allocated = nbytes;
    }
  stream->bufsize = nbytes;
  if (fseek (fp, offset, SEEK_SET) < 0)
    OTF_ERROR (OTF_ERROR_FILE, stream->name);
  if (fread (stream->buf, 1, nbytes, fp) != nbytes)
    OTF_ERROR (OTF_ERROR_FILE, stream->name);
  return 0;
}

static OTF_Stream *
make_stream_from_ft_face (FT_Face face, const char *name)
{
  char *errfmt = "FT_Face stream creation for %s";
  void *errret = NULL;
  FT_ULong nbytes = 0;
  unsigned char *buf;
  OTF_Stream *stream;
  FT_ULong tag = FT_MAKE_TAG (name[0], name[1], name[2], name[3]);

  if (FT_Load_Sfnt_Table (face, tag, 0, NULL, &nbytes))
    return NULL;
  buf = malloc (nbytes);
  if (! buf)
    OTF_ERROR (OTF_ERROR_MEMORY, name);
  if (FT_Load_Sfnt_Table (face, tag, 0, buf, &nbytes))
    {
      free (buf);
      OTF_ERROR (OTF_ERROR_FT_FACE, name);
    }
  stream = make_stream (name);
  if (! stream)
    return NULL;
  stream->pos = 0;
  stream->buf = buf;
  stream->allocated = nbytes;
  stream->bufsize = nbytes;
  return stream;
}

static void
free_stream (OTF_Stream *stream)
{
  if (stream->buf)
    free (stream->buf);
  free (stream);
}

#define SAVE_STREAM(stream, state) ((state) = (stream)->pos)
#define RESTORE_STREAM(stream, state) ((stream)->pos = (state))
#define SEEK_STREAM(stream, offset) ((stream)->pos = (offset))

#define STREAM_CHECK_SIZE(stream, size)			\
  if ((stream)->pos + (size) > (stream)->bufsize)	\
    {							\
      char *errfmt = "buffer overrun in %s";		\
							\
      OTF_ERROR (OTF_ERROR_TABLE, (stream)->name);	\
      return errret;					\
    }							\
  else


#define READ_USHORT(stream, var)			\
  do {							\
    STREAM_CHECK_SIZE ((stream), 2);			\
    (var) = (((stream)->buf[(stream)->pos] << 8)	\
	     | (stream)->buf[(stream)->pos + 1]);	\
    (stream)->pos += 2;					\
  } while (0)

#define READ_SHORT(stream, var)					\
  do {								\
    STREAM_CHECK_SIZE ((stream), 2);				\
    (var) = (short) (((stream)->buf[(stream)->pos] << 8)	\
		     | (stream)->buf[(stream)->pos + 1]);	\
    (stream)->pos += 2;						\
  } while (0)

#define READ_UINT24(stream, var)			\
  do {							\
    STREAM_CHECK_SIZE ((stream), 3);			\
    (var) =  (((stream)->buf[(stream)->pos ] << 16)	\
	      | ((stream)->buf[(stream)->pos + 1] << 8)	\
	      | (stream)->buf[(stream)->pos + 2]);	\
    (stream)->pos += 3;					\
  } while (0)

#define READ_ULONG(stream, var)				\
  do {							\
    STREAM_CHECK_SIZE ((stream), 4);			\
    (var) = (((stream)->buf[(stream)->pos] << 24)	\
	     | ((stream)->buf[(stream)->pos + 1] << 16)	\
	     | ((stream)->buf[(stream)->pos + 2] << 8)	\
	     | (stream)->buf[(stream)->pos + 3]);	\
    (stream)->pos += 4;					\
  } while (0)

#define READ_LONG(stream, var)					\
  do {								\
    STREAM_CHECK_SIZE ((stream), 4);				\
    (var) = (int) (((stream)->buf[(stream)->pos] << 24)		\
		   | ((stream)->buf[(stream)->pos + 1] << 16)	\
		   | ((stream)->buf[(stream)->pos + 2] << 8)	\
		   | (stream)->buf[(stream)->pos + 3]);		\
    (stream)->pos += 4;						\
  } while (0)


#define READ_FIXED(stream, fixed)		\
  do {						\
    READ_USHORT ((stream), (fixed).high);	\
    READ_USHORT ((stream), (fixed).low);	\
  } while (0)


#define READ_BYTES(stream, p, nbytes)				\
  do {								\
    STREAM_CHECK_SIZE ((stream), (nbytes));			\
    memcpy ((p), (stream)->buf + (stream)->pos, (nbytes));	\
    (stream)->pos += (nbytes);					\
  } while (0)


#define READ_TAG READ_ULONG
#define READ_OFFSET READ_USHORT
#define READ_UINT16 READ_USHORT
#define READ_INT16 READ_SHORT
#define READ_GLYPHID READ_USHORT


/*** (1) Structures for OTF Layout tables and OTF itself */

/*** (1-1) Basic types and functions */

enum OTF_TableType
  {
    OTF_TABLE_TYPE_HEAD,
    OTF_TABLE_TYPE_NAME,
    OTF_TABLE_TYPE_CMAP,
    OTF_TABLE_TYPE_GDEF,
    OTF_TABLE_TYPE_GSUB,
    OTF_TABLE_TYPE_GPOS,
    OTF_TABLE_TYPE_MAX
  };

#define OTF_MEMORY_RECORD_SIZE 1024

struct OTF_MemoryRecord
{
  int used;
  void *memory[OTF_MEMORY_RECORD_SIZE];
  struct OTF_MemoryRecord *next;
};

typedef struct OTF_MemoryRecord OTF_MemoryRecord;

enum OTF_ReaderFlag
  {
    OTF_READ_FULL,
    OTF_READ_SCRIPTS,
    OTF_READ_FEATURES,
    OTF_READ_MAX
  };

struct _OTF_TableInfo;
typedef struct _OTF_TableInfo OTF_TableInfo;

struct _OTF_TableInfo
{
  /* Points to one of OTF->head, OTF->name, etc.  */
  void **address;
  /* Function to read one of OTF tables.  */
  void *(*reader) (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag);
  /* Stream given to <reader>.  */
  OTF_Stream *stream;
};

struct _OTF_ApplicationData
{
  char *id;
  void *data;
  void (*freer) (void *data);
  struct _OTF_ApplicationData *next;
};

typedef struct _OTF_ApplicationData OTF_ApplicationData;

struct OTF_InternalData
{
  /* Information about each OTF table.  */
  OTF_TableInfo table_info[OTF_TABLE_TYPE_MAX];

  /* Stream used to read the header part of OTF.  */
  OTF_Stream *header_stream;

  /* Records of allocated memories.  */
  OTF_MemoryRecord *memory_record;

  /* Root of application data chain.  */
  OTF_ApplicationData *app_data;
};

static OTF_MemoryRecord *
allocate_memory_record (OTF *otf)
{
  OTF_InternalData *internal_data = (OTF_InternalData *) otf->internal_data;
  OTF_MemoryRecord *memrec = malloc (sizeof (OTF_MemoryRecord));

  if (! memrec)
    return NULL;
  memrec->used = 0;
  memrec->next = internal_data->memory_record;
  internal_data->memory_record = memrec;
  return memrec;
}

/* Memory allocation macros.  */

#define OTF_MALLOC(p, size, arg)					\
  do {									\
    if (size == 0)							\
      (p) = NULL;							\
    else								\
      {									\
	OTF_MemoryRecord *memrec					\
	  = ((OTF_InternalData *) otf->internal_data)->memory_record;	\
	(p) = malloc (sizeof (*(p)) * (size));				\
	if (! (p)							\
	    || (memrec->used >= OTF_MEMORY_RECORD_SIZE			\
		&& ! (memrec = allocate_memory_record (otf))))		\
	  OTF_ERROR (OTF_ERROR_MEMORY, (arg));				\
	memrec->memory[memrec->used++] = (p);				\
      }									\
  } while (0)


#define OTF_CALLOC(p, size, arg)					\
  do {									\
    if (size == 0)							\
      (p) = NULL;							\
    else								\
      {									\
	OTF_MemoryRecord *memrec					\
	  = ((OTF_InternalData *) otf->internal_data)->memory_record;	\
	(p) = calloc ((size), sizeof (*(p)));				\
	if (! (p)							\
	    || (memrec->used >= OTF_MEMORY_RECORD_SIZE			\
		&& ! (memrec = allocate_memory_record (otf))))		\
	  OTF_ERROR (OTF_ERROR_MEMORY, (arg));				\
	memrec->memory[memrec->used++] = (p);				\
      }									\
  } while (0)


/*** (1-2) "head" table */

static void *
read_head_table (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag)
{
  OTF_Stream *stream = table->stream;
  char *errfmt = "head%s";
  void *errret = NULL;
  OTF_head *head;

  OTF_CALLOC (head, 1, "");
  READ_FIXED (stream, head->TableVersionNumber);
  READ_FIXED (stream, head->fontRevision);
  READ_ULONG (stream, head->checkSumAdjustment);
  READ_ULONG (stream, head->magicNumber);
  READ_USHORT (stream, head->flags);
  READ_USHORT (stream, head->unitsPerEm);

  *table->address = head;
  return head;
}


/*** (1-3) "name" table */

static int
read_name (OTF *otf, OTF_Stream *stream, OTF_NameRecord *rec)
{
  char errfmt[256];
  int errret = -1;
  OTF_StreamState state;
  int ucs = 0;
  int ascii = 0;
  int i;

  sprintf (errfmt, "nameID (%d)%%s", rec->nameID);
  if (rec->platformID == 0)
    ucs = (rec->encodingID <= 3) ? 2 : 4;
  else if (rec->platformID == 1 && rec->encodingID == 0)
    ascii = 1;
  else if (rec->platformID == 3)
    ucs = (rec->encodingID == 1  ? 2
	   : rec->encodingID == 10 ? 4
	   : 0);

  OTF_MALLOC (rec->name, rec->length + 1, "");
  SAVE_STREAM (stream, state);
  SEEK_STREAM (stream, stream->pos + rec->offset);
  READ_BYTES (stream, rec->name, rec->length);
  RESTORE_STREAM (stream, state);
  rec->name[rec->length] = 0;

  if (ascii)
    {
      rec->ascii = 1;
    }
  else if (ucs == 2)
    {
      rec->ascii = 1;
      for (i = 0; i < rec->length / 2; i++)
	{
	  if (rec->name[i * 2] > 0
	      || rec->name[i * 2 + 1] >= 128)
	    {
	      rec->ascii = 0;
	      break;
	    }
	}
      if (rec->ascii)
	for (i = 0; i < rec->length / 2; i++)
	  rec->name[i] = rec->name[i * 2 + 1];
      rec->name[i] = 0;
    }
  else if (ucs == 4)
    {
      rec->ascii = 1;
      for (i = 0; i < rec->length / 4; i++)
	{
	  if (rec->name[i * 4] > 0
	      || rec->name[i * 4 + 1] > 0
	      || rec->name[i * 4 + 2] > 0
	      || rec->name[i * 2 + 3] >= 128)
	    {
	      rec->ascii = 0;
	      break;
	    }
	}
      if (rec->ascii)
	for (i = 0; i < rec->length / 4; i++)
	  rec->name[i] = rec->name[i * 4 + 3];
      rec->name[i] = 0;
    }

  return 0;
}

static void *
read_name_table (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag)
{
  OTF_Stream *stream = table->stream;
  char *errfmt = "name%s";
  void *errret = NULL;
  OTF_name *name;
  int i;

  OTF_CALLOC (name, 1, "");
  READ_USHORT (stream, name->format);
  READ_USHORT (stream, name->count);
  READ_USHORT (stream, name->stringOffset);
  OTF_MALLOC (name->nameRecord, name->count, "");
  for (i = 0; i < name->count; i++)
    {
      OTF_NameRecord *rec = name->nameRecord + i;

      READ_USHORT (stream, rec->platformID);
      READ_USHORT (stream, rec->encodingID);
      READ_USHORT (stream, rec->languageID);
      READ_USHORT (stream, rec->nameID);
      READ_USHORT (stream, rec->length);
      READ_USHORT (stream, rec->offset);
    }
  for (i = 0; i < name->count; i++)
    {
      OTF_NameRecord *rec = name->nameRecord + i;
      int nameID = rec->nameID;

      read_name (otf, stream, rec);

      if (nameID >= OTF_max_nameID)
	continue;
      if (! name->name[nameID]
	  && rec->ascii)
	name->name[nameID] = (char *) rec->name;
    }

  *table->address = name;
  return name;
}


/*** (1-4) "cmap" table */

static OTF_EncodingSubtable14 *
read_cmap_uvs_table (OTF *otf, OTF_Stream *stream, OTF_Offset offset)
{
  OTF_EncodingSubtable14 *sub14;
  char *errfmt = "cmap-uvs%s";
  void *errret = NULL;
  unsigned nRecords;
  unsigned i,j;

  OTF_MALLOC (sub14, 1, " (EncodingSubtable14)");
  READ_ULONG (stream, nRecords);
  sub14->nRecords = nRecords;
  OTF_MALLOC (sub14->Records, nRecords, "(EncodingSubtable14-Records)");
  for (i = 0; i < sub14->nRecords; i++)
    {
      unsigned varSelector=0, defaultUVSOffset, nonDefaultUVSOffset;

      READ_UINT24 (stream, varSelector);
      sub14->Records[i].varSelector = varSelector;
      READ_ULONG (stream, defaultUVSOffset);
      sub14->Records[i].defaultUVSOffset = defaultUVSOffset;
      READ_ULONG (stream, nonDefaultUVSOffset);
      sub14->Records[i].nonDefaultUVSOffset = nonDefaultUVSOffset;
    }
  for (i = 0; i < sub14->nRecords; i++)
    {
      OTF_VariationSelectorRecord *record = &sub14->Records[i];
      unsigned defaultUVSOffset = record->defaultUVSOffset;
      unsigned nonDefaultUVSOffset = record->nonDefaultUVSOffset;

      if (defaultUVSOffset)
	{
	  unsigned numUnicodeValueRanges;

	  SEEK_STREAM (stream, offset+defaultUVSOffset);
	  READ_ULONG (stream, numUnicodeValueRanges);
	  record->numUnicodeValueRanges = numUnicodeValueRanges;
	  OTF_MALLOC (record->unicodeValueRanges,
		      numUnicodeValueRanges,
		      "(EncodingSubtable14-Records-unicodeValueRanges)");
	  for (j = 0; j < numUnicodeValueRanges; j++)
	    {
	      OTF_UnicodeValueRange *unicodeValueRange
		= &record->unicodeValueRanges[j];
	      unsigned startUnicodeValue;
	      char additionalCount;

	      READ_UINT24 (stream, startUnicodeValue);
	      unicodeValueRange->startUnicodeValue=startUnicodeValue;
	      READ_BYTES (stream, &additionalCount, 1);
	      unicodeValueRange->additionalCount
		= (unsigned short) additionalCount;
	    }
	}
      if (nonDefaultUVSOffset)
	{
	  unsigned numUVSMappings;

	  SEEK_STREAM (stream, offset+nonDefaultUVSOffset);
	  READ_ULONG (stream, numUVSMappings);
	  record->numUVSMappings = numUVSMappings;
	  OTF_MALLOC (record->uvsMappings, numUVSMappings,
		      "(EncodingSubtable14-Records-uvsMappings)");
	  for (j = 0; j < numUVSMappings; j++)
	    {
	      OTF_UVSMapping *uvsMapping = &record->uvsMappings[j];
	      unsigned unicodeValue;
	      unsigned short glyphID;

	      READ_UINT24 (stream, unicodeValue);
	      uvsMapping->unicodeValue = unicodeValue;
	      READ_USHORT (stream, glyphID);
	      uvsMapping->glyphID = glyphID;
	    }
	}
    }
  return sub14;
}

static void *
read_cmap_table (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag)
{
  OTF_Stream *stream = table->stream;
  char *errfmt = "cmap%s";
  void *errret = NULL;
  OTF_cmap *cmap;
  int unicode_bmp_index = -1, unicode_full_index = -1;
  int i;

  OTF_CALLOC (cmap, 1, "");
  READ_USHORT (stream, cmap->version);
  READ_USHORT (stream, cmap->numTables);
  OTF_MALLOC (cmap->EncodingRecord, cmap->numTables, "");
  for (i = 0; i < cmap->numTables; i++)
    {
      unsigned platformID, encodingID;

      READ_USHORT (stream, platformID);
      cmap->EncodingRecord[i].platformID = platformID;
      READ_USHORT (stream, encodingID);
      cmap->EncodingRecord[i].encodingID = encodingID;
      READ_ULONG (stream, cmap->EncodingRecord[i].offset);
      if (platformID == 0)
	{
	  if (encodingID <= 3)
	    unicode_bmp_index = i;
	  else
	    unicode_full_index = i;
	}
      else if (platformID == 3)
	{
	  if (encodingID == 1)
	    unicode_bmp_index = i;
	  else if (encodingID == 10)
	    unicode_full_index = i;
	}
    }
  cmap->table_index = unicode_full_index;

  for (i = 0; i < cmap->numTables; i++)
    {
      unsigned format;

      SEEK_STREAM (stream, cmap->EncodingRecord[i].offset);
      READ_USHORT (stream, format);
      cmap->EncodingRecord[i].subtable.format = format;
      if (format == 14)
	{
	  READ_ULONG (stream, cmap->EncodingRecord[i].subtable.length);
	  cmap->EncodingRecord[i].subtable.language = 0;
	}
      else
	{
	  READ_USHORT (stream, cmap->EncodingRecord[i].subtable.length);
	  if (format == 8 || format == 10 || format == 12)
	    {
	      READ_ULONG (stream, cmap->EncodingRecord[i].subtable.length);
	      READ_ULONG (stream, cmap->EncodingRecord[i].subtable.language);
	    }
	  else
	    {
	      READ_USHORT (stream, cmap->EncodingRecord[i].subtable.language);
	    }
	}
      switch (format)
	{
	case 0:
	  {
	    OTF_MALLOC (cmap->EncodingRecord[i].subtable.f.f0, 1,
			" (EncodingRecord)");
	    READ_BYTES (stream,
			cmap->EncodingRecord[i].subtable.f.f0->glyphIdArray,
			256);
	  }
	  break;

	case 2:
	  {
	    OTF_EncodingSubtable2 *sub2;
	    int j, max_key;

	    OTF_MALLOC (sub2, 1, " (EncodingSubtable2)");
	    cmap->EncodingRecord[i].subtable.f.f2 = sub2;
	    for (j = 0, max_key = 0; j < 256; j++)
	      {
		READ_USHORT (stream, sub2->subHeaderKeys[j]);
		if (max_key < sub2->subHeaderKeys[j])
		  max_key = sub2->subHeaderKeys[j];
	      }
	    max_key += 8;
	    sub2->subHeaderCount = max_key / 8;
	    OTF_MALLOC (sub2->subHeaders, max_key / 8, " (subHeaders)");
	    for (j = 0; j < sub2->subHeaderCount; j++)
	      {
		READ_USHORT (stream, sub2->subHeaders[j].firstCode);
		READ_USHORT (stream, sub2->subHeaders[j].entryCount);
		READ_SHORT (stream, sub2->subHeaders[j].idDelta);
		READ_USHORT (stream, sub2->subHeaders[j].idRangeOffset);
		/* Make it offset from sub2->glyphIndexArray.  */
		sub2->subHeaders[j].idRangeOffset -= max_key - (j * 8 + 6);
	      }
	    sub2->glyphIndexCount = (cmap->EncodingRecord[i].subtable.length
				     - 262 - max_key);
	    OTF_MALLOC (sub2->glyphIndexArray, sub2->glyphIndexCount,
			" (glyphIndexArray)");
	    READ_BYTES (stream, sub2->glyphIndexArray, sub2->glyphIndexCount);
	  }
	  break;

	case 4:
	  {
	    OTF_EncodingSubtable4 *sub4;
	    int segCount;
	    int j;
	    unsigned dummy;

	    OTF_MALLOC (sub4, 1, " (EncodingSubtable4)");
	    cmap->EncodingRecord[i].subtable.f.f4 = sub4;
	    READ_USHORT (stream, sub4->segCountX2);
	    segCount = sub4->segCountX2 / 2;
	    READ_USHORT (stream, sub4->searchRange);
	    READ_USHORT (stream, sub4->entrySelector);
	    READ_USHORT (stream, sub4->rangeShift);
	    OTF_MALLOC (sub4->segments, segCount, " (segCount)");
	    for (j = 0; j < segCount; j++)
	      READ_USHORT (stream, sub4->segments[j].endCount);
	    READ_USHORT (stream, dummy);
	    for (j = 0; j < segCount; j++)
	      READ_USHORT (stream, sub4->segments[j].startCount);
	    for (j = 0; j < segCount; j++)
	      READ_SHORT (stream, sub4->segments[j].idDelta);
	    for (j = 0; j < segCount; j++)
	      {
		unsigned off;
		unsigned rest = 2 * (segCount - j);

		READ_USHORT (stream, off);
		if (off == 0)
		  sub4->segments[j].idRangeOffset = 0xFFFF;
		else
		  sub4->segments[j].idRangeOffset = (off - rest) / 2;
	      }
	    j = (cmap->EncodingRecord[i].subtable.length
		 - (14 + 2 * (segCount * 4 + 1)));
	    sub4->GlyphCount = j / 2;
	    OTF_MALLOC (sub4->glyphIdArray, sub4->GlyphCount, " (GlyphCount)");
	    for (j = 0; j < sub4->GlyphCount; j++)
	      READ_USHORT (stream, sub4->glyphIdArray[j]);
	  }
	  break;

	case 6:
	  {
	    OTF_EncodingSubtable6 *sub6;
	    int j;

	    OTF_MALLOC (sub6, 1, " (EncodingSubtable6)");
	    cmap->EncodingRecord[i].subtable.f.f6 = sub6;
	    READ_USHORT (stream, sub6->firstCode);
	    READ_USHORT (stream, sub6->entryCount);
	    OTF_MALLOC (sub6->glyphIdArray, sub6->entryCount, " (GlyphCount)");
	    for (j = 0; j < sub6->entryCount; j++)
	      READ_USHORT (stream, sub6->glyphIdArray[j]);
	  }
	  break;

	case 8:
	  {
	    OTF_EncodingSubtable8 *sub8;
	    int j;

	    OTF_MALLOC (sub8, 1, " (EncodingSubtable8)");
	    cmap->EncodingRecord[i].subtable.f.f8 = sub8;
	    for (j = 0; j < 8192; j++)
	      READ_BYTES (stream, sub8->is32, 8192);
	    READ_ULONG (stream, sub8->nGroups);
	    OTF_MALLOC (sub8->Groups, sub8->nGroups, " (Groups)");
	    for (j = 0; j < sub8->nGroups; j++)
	      {
		READ_ULONG (stream, sub8->Groups[i].startCharCode);
		READ_ULONG (stream, sub8->Groups[i].endCharCode);
		READ_ULONG (stream, sub8->Groups[i].startGlyphID);
	      }
	  }
	  break;

	case 10:
	  {
	    OTF_EncodingSubtable10 *sub10;
	    int j;

	    OTF_MALLOC (sub10, 1, " (EncodingSubtable10)");
	    cmap->EncodingRecord[i].subtable.f.f10 = sub10;
	    READ_ULONG (stream, sub10->startCharCode);
	    READ_ULONG (stream, sub10->numChars);
	    OTF_MALLOC (sub10->glyphs, sub10->numChars, " (GlyphCount)");
	    for (j = 0; j < sub10->numChars; j++)
	      READ_USHORT (stream, sub10->glyphs[j]);
	  }
	  break;

	case 12:
	  {
	    OTF_EncodingSubtable12 *sub12;
	    int j;

	    OTF_MALLOC (sub12, 1, " (EncodingSubtable12)");
	    cmap->EncodingRecord[i].subtable.f.f12 = sub12;
	    READ_ULONG (stream, sub12->nGroups);
	    OTF_MALLOC (sub12->Groups, sub12->nGroups, " (Groups)");
	    for (j = 0; j < sub12->nGroups; j++)
	      {
		READ_ULONG (stream, sub12->Groups[j].startCharCode);
		READ_ULONG (stream, sub12->Groups[j].endCharCode);
		READ_ULONG (stream, sub12->Groups[j].startGlyphID);
	      }
	  }
	  break;

	case 14:
	  {
	    cmap->EncodingRecord[i].subtable.f.f14
	      = read_cmap_uvs_table (otf, stream,
				     cmap->EncodingRecord[i].offset);
	    break;
	  }

	default:
	  OTF_ERROR (OTF_ERROR_TABLE, " (invalid Subtable format)");
	}
    }

  if (unicode_bmp_index >= 0)
    {
      OTF_EncodingRecord *rec = cmap->EncodingRecord + unicode_bmp_index;
      OTF_GlyphID glyph_id, max_glyph_id = 0;

      OTF_CALLOC (cmap->unicode_table, 0x10000, "");
      switch (rec->subtable.format)
	{
	case 4:
	  {
	    OTF_EncodingSubtable4 *sub4 = rec->subtable.f.f4;
	    int segCount = sub4->segCountX2 / 2;

	    for (i = 0; i < segCount; i++)
	      {
		OTF_cmapSegment *seg = sub4->segments + i;
		int c;

		if (seg->idRangeOffset == 0xFFFF)
		  for (c = seg->startCount; c <= seg->endCount; c++)
		    {
		      glyph_id = (c + seg->idDelta) % 0x10000;
		      cmap->unicode_table[c] = glyph_id;
		      if (glyph_id > max_glyph_id)
			max_glyph_id = glyph_id;
		    }
		else
		  for (c = seg->startCount; c <= seg->endCount && c != 0xFFFF;
		       c++)
		    {
		      glyph_id = sub4->glyphIdArray[seg->idRangeOffset
						    + (c - seg->startCount)];
		      cmap->unicode_table[c] = glyph_id;
		      if (glyph_id > max_glyph_id)
			max_glyph_id = glyph_id;
		    }
	      }
	  }
	}

      OTF_CALLOC (cmap->decode_table, max_glyph_id + 1, "");
      for (i = 0; i < 0x10000; i++)
	if (cmap->unicode_table[i])
	  cmap->decode_table[cmap->unicode_table[i]] = i;
      cmap->max_glyph_id = max_glyph_id;
    }

  *table->address = cmap;
  return cmap;
}


/*** (1-5) Structures common to GDEF, GSUB, and GPOS */

/* Read Glyph-IDs from STREAM.  Allocate memory for IDS, and store the
   Glyph-IDs there.  If COUNT is negative, read the number of
   Glyphs-IDs at first.  MINUS if nozero is how few the actual
   Glyph-IDs are in STREAM than COUNT.  */

static int
read_glyph_ids (OTF *otf, OTF_Stream *stream, OTF_GlyphID **ids,
		int minus, int count)
{
  char *errfmt = "GlyphID List%s";
  int errret = -1;
  int i;

  if (count < 0)
    READ_UINT16 (stream, count);
  if (! count)
    return 0;
  OTF_MALLOC (*ids, count, "");
  for (i = 0; i < count + minus; i++)
    READ_GLYPHID (stream, (*ids)[i]);
  return count;
}

static unsigned
read_range_records (OTF *otf, OTF_Stream *stream, OTF_RangeRecord **record)
{
  char *errfmt = "RangeRecord%s";
  unsigned errret = 0;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    return 0;
  OTF_MALLOC (*record, count, "");
  for (i = 0; i < count; i++)
    {
      READ_GLYPHID (stream, (*record)[i].Start);
      READ_GLYPHID (stream, (*record)[i].End);
      READ_UINT16 (stream, (*record)[i].StartCoverageIndex);
    }
  return count;
}


static int
read_coverage (OTF *otf, OTF_Stream *stream, long offset,
	       OTF_Coverage *coverage)
{
  char *errfmt = "Coverage%s";
  int errret = -1;
  OTF_StreamState state;
  int count;

  READ_OFFSET (stream, coverage->offset);
  SAVE_STREAM (stream, state);
  SEEK_STREAM (stream, offset + coverage->offset);
  READ_UINT16 (stream, coverage->CoverageFormat);
  if (coverage->CoverageFormat == 1)
    count = read_glyph_ids (otf, stream, &coverage->table.GlyphArray, 0, -1);
  else if (coverage->CoverageFormat == 2)
    count = read_range_records (otf, stream, &coverage->table.RangeRecord);
  else
    OTF_ERROR (OTF_ERROR_TABLE, " (Invalid Format)");
  if (count < 0)
    return -1;
  coverage->Count = (unsigned) count;
  RESTORE_STREAM (stream, state);
  return 0;
}

/* Read list of Coverages from STREAM.  Allocate memory for COVERAGE,
   and store the Coverages there.  If COUNT is negative, read the
   number of Coverages at first.  */

static int
read_coverage_list (OTF *otf, OTF_Stream *stream, long offset,
		    OTF_Coverage **coverage, int count)
{
  char *errfmt = "Coverage List%s";
  int errret = -1;
  int i;

  if (count < 0)
    READ_UINT16 (stream, count);
  if (! count)
    return 0;
  OTF_MALLOC (*coverage, count, "");
  for (i = 0; i < count; i++)
    if (read_coverage (otf, stream, offset, (*coverage) + i) < 0)
      return -1;
  return count;
}


static int
read_class_def_without_offset (OTF *otf, OTF_Stream *stream,
			       OTF_ClassDef *class)
{
  char *errfmt = "ClassDef%s";
  int errret = -1;

  SEEK_STREAM (stream, class->offset);
  READ_UINT16 (stream, class->ClassFormat);
  if (class->ClassFormat == 1)
    {
      READ_GLYPHID (stream, class->f.f1.StartGlyph);
      class->f.f1.GlyphCount
	= (read_glyph_ids
	   (otf, stream, (OTF_GlyphID **) &class->f.f1.ClassValueArray, 0, -1));
      if (! class->f.f1.GlyphCount)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
    }
  else if (class->ClassFormat == 2)
    {
      class->f.f2.ClassRangeCount
	= (read_range_records
	   (otf, stream, (OTF_RangeRecord **) &class->f.f2.ClassRangeRecord));
      if (! class->f.f2.ClassRangeCount)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
    }
  else
    OTF_ERROR (OTF_ERROR_TABLE, " (Invalid format)");
  return 0;
}


static int
read_class_def (OTF *otf, OTF_Stream *stream, long offset, OTF_ClassDef *class)
{
  char *errfmt = "ClassDef%s";
  int errret = -1;
  OTF_StreamState state;

  READ_OFFSET (stream, class->offset);
  if (! class->offset)
    return 0;
  SAVE_STREAM (stream, state);
  SEEK_STREAM (stream, offset + class->offset);
  READ_UINT16 (stream, class->ClassFormat);
  if (class->ClassFormat == 1)
    {
      READ_GLYPHID (stream, class->f.f1.StartGlyph);
      class->f.f1.GlyphCount
	= read_glyph_ids (otf, stream,
			  (OTF_GlyphID **) &class->f.f1.ClassValueArray,
			  0, -1);
      if (! class->f.f1.GlyphCount)
	return -1;
    }
  else if (class->ClassFormat == 2)
    {
      class->f.f2.ClassRangeCount
	= read_range_records (otf, stream,
			      (OTF_RangeRecord **)
			      &class->f.f2.ClassRangeRecord);
      if (! class->f.f2.ClassRangeCount)
	return -1;
    }
  else
    OTF_ERROR (OTF_ERROR_TABLE, " (Invalid format)");

  RESTORE_STREAM (stream, state);
  return 0;
}


static int
read_device_table (OTF *otf, OTF_Stream *stream, long offset,
		   OTF_DeviceTable *table)
{
  char *errfmt = "Device Table%s";
  int errret = -1;

  int num, i;
  unsigned val;
  struct {
    int int2 : 2;
    int int4 : 4;
    int int8 : 8;
  } intval;

  SEEK_STREAM (stream, offset + table->offset);
  READ_UINT16 (stream, table->StartSize);
  READ_UINT16 (stream, table->EndSize);
  READ_UINT16 (stream, table->DeltaFormat);
  num = table->EndSize - table->StartSize + 1;
  if (num > 0 && table->DeltaFormat >= 1 && table->DeltaFormat <= 3)
    {
      OTF_MALLOC (table->DeltaValue, num, "");

      if (table->DeltaFormat == 1)
	for (i = 0; i < num; i++)
	  {
	    if ((i % 8) == 0)
	      READ_UINT16 (stream, val);
	    intval.int2 = (val >> (14 - (i % 8) * 2)) & 0x03;
	    table->DeltaValue[i] = intval.int2;
	  }
      else if (table->DeltaFormat == 2)
	for (i = 0; i < num; i++)
	  {
	    if ((i % 4) == 0)
	      READ_UINT16 (stream, val);
	    intval.int4 = (val >> (12 - (i % 4) * 4)) & 0x0F;
	    table->DeltaValue[i] = intval.int4;
	  }
      else 				/* (table->DeltaFormat == 3) */
	for (i = 0; i < num; i++)
	  {
	    if ((i % 2) == 0)
	      {
		READ_UINT16 (stream, val);
		intval.int8 = val >> 8;
		table->DeltaValue[i] = intval.int8;
	      }
	    else
	      {
		intval.int8 = val >> 8;
		table->DeltaValue[i] = intval.int8;
	      }
	  }
    }
  else
    {
      /* Invalid DeltaFormat but several fonts has such values (bug of
	 fontforge?).  So accept it with NULL delta values.  */
      table->DeltaValue = NULL;
    }
  return 0;
}


/*** (1-6) "GDEF" table */

static int
read_attach_list (OTF *otf, OTF_Stream *stream, long offset,
		  OTF_AttachList *list)
{
  char *errfmt = "AttachList%s";
  int errret = -1;
  int i, j;

  if (read_coverage (otf, stream, offset, &list->Coverage) < 0)
    return -1;
  READ_UINT16 (stream, list->GlyphCount);
  OTF_MALLOC (list->AttachPoint, list->GlyphCount, "");
  for (i = 0; i < list->GlyphCount; i++)
    READ_OFFSET (stream, list->AttachPoint[i].offset);
  for (i = 0; i < list->GlyphCount; i++)
    {
      int count;

      SEEK_STREAM (stream, offset + list->AttachPoint[i].offset);
      READ_UINT16 (stream, count);
      list->AttachPoint[i].PointCount = count;
      OTF_MALLOC (list->AttachPoint[i].PointIndex, count, " (PointIndex)");
      for (j = 0; j < count; j++)
	READ_UINT16 (stream, list->AttachPoint[i].PointIndex[j]);
    }
  return 0;
}

static int
read_caret_value (OTF *otf, OTF_Stream *stream, long offset,
		  OTF_CaretValue *caret)
{
  char *errfmt = "CaretValue%s";
  int errret = -1;

  SEEK_STREAM (stream, offset + caret->offset);
  READ_UINT16 (stream, caret->CaretValueFormat);
  if (caret->CaretValueFormat == 1)
    READ_INT16 (stream, caret->f.f1.Coordinate);
  else if (caret->CaretValueFormat == 2)
    READ_UINT16 (stream, caret->f.f2.CaretValuePoint);
  else if (caret->CaretValueFormat == 3)
    {
      READ_INT16 (stream, caret->f.f3.Coordinate);
      if (read_device_table (otf, stream, offset + caret->offset,
			     &caret->f.f3.DeviceTable) < 0)
	return -1;
    }
  else
    OTF_ERROR (OTF_ERROR_TABLE, " (Invalid format)");
  return 0;
}

static int
read_lig_caret_list (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_LigCaretList *list)
{
  char *errfmt = "LigCaretList%s";
  int errret = -1;
  int i, j;

  if (read_coverage (otf, stream, offset, &list->Coverage) < 0)
    return -1;
  READ_UINT16 (stream, list->LigGlyphCount);
  OTF_MALLOC (list->LigGlyph, list->LigGlyphCount, "");
  for (i = 0; i < list->LigGlyphCount; i++)
    READ_OFFSET (stream, list->LigGlyph[i].offset);
  for (i = 0; i < list->LigGlyphCount; i++)
    {
      int count;

      SEEK_STREAM (stream, offset + list->LigGlyph[i].offset);
      READ_UINT16 (stream, count);
      list->LigGlyph[i].CaretCount = count;
      OTF_MALLOC (list->LigGlyph[i].CaretValue, count, " (CaretValue)");
      for (j = 0; j < count; j++)
	READ_OFFSET (stream, list->LigGlyph[i].CaretValue[j].offset);
      for (j = 0; j < count; j++)
	if (read_caret_value (otf, stream, offset + list->LigGlyph[i].offset,
			      &list->LigGlyph[i].CaretValue[j]) < 0)
	  return -1;
    }
  return 0;
}

static int
read_gdef_header (OTF_Stream *stream, OTF_GDEFHeader *header)
{
  int errret = -1;

  READ_FIXED (stream, header->Version);
  READ_OFFSET (stream, header->GlyphClassDef);
  READ_OFFSET (stream, header->AttachList);
  READ_OFFSET (stream, header->LigCaretList);
  READ_OFFSET (stream, header->MarkAttachClassDef);
  return 0;
}

static void *
read_gdef_table (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag)
{
  OTF_Stream *stream = table->stream;
  char *errfmt = "GDEF%s";
  void *errret = NULL;
  OTF_GDEF *gdef;

  OTF_CALLOC (gdef, 1, "");
  if (stream->buf)
    {
      read_gdef_header (stream, (OTF_GDEFHeader *) &gdef->header);
      if (gdef->header.GlyphClassDef)
	{
	  gdef->glyph_class_def.offset = gdef->header.GlyphClassDef;
	  read_class_def_without_offset (otf, stream,
					 &gdef->glyph_class_def);
	}
      if (gdef->header.AttachList)
	read_attach_list (otf, stream, gdef->header.AttachList,
			  &gdef->attach_list);
      if (gdef->header.LigCaretList)
	read_lig_caret_list (otf, stream, gdef->header.LigCaretList,
			     &gdef->lig_caret_list);
      if (gdef->header.MarkAttachClassDef)
	{
	  gdef->mark_attach_class_def.offset = gdef->header.MarkAttachClassDef;
	  read_class_def_without_offset (otf, stream,
					 &gdef->mark_attach_class_def);
	}
    }

  *table->address = gdef;
  return gdef;
}


/*** (1-7) Structures for ScriptList, FeatureList, and LookupList */

static int
read_script_list (OTF *otf, OTF_Stream *stream, long offset,
		  OTF_ScriptList *list)
{
  char *errfmt = "Script List%s";
  int errret = -1;
  int i, j, k;

  SEEK_STREAM (stream, offset);
  READ_USHORT (stream, list->ScriptCount);
  OTF_CALLOC (list->Script, list->ScriptCount, "");

  for (i = 0; i < list->ScriptCount; i++)
    {
      READ_TAG (stream, list->Script[i].ScriptTag);
      READ_OFFSET (stream, list->Script[i].offset);
    }
  for (i = 0;  i < list->ScriptCount; i++)
    {
      OTF_Script *script = list->Script + i;
      long script_offset = offset + script->offset;

      SEEK_STREAM (stream, script_offset);
      READ_OFFSET (stream, script->DefaultLangSysOffset);
      READ_USHORT (stream, script->LangSysCount);
      OTF_MALLOC (script->LangSysRecord, script->LangSysCount, " (LangSys)");
      OTF_CALLOC (script->LangSys, script->LangSysCount, " (LangSys)");
      for (j = 0; j < script->LangSysCount; j++)
	{
	  READ_TAG (stream, script->LangSysRecord[j].LangSysTag);
	  READ_OFFSET (stream, script->LangSysRecord[j].LangSys);
	}

      if (script->DefaultLangSysOffset)
	{
	  OTF_LangSys *langsys = &script->DefaultLangSys;

	  SEEK_STREAM (stream, script_offset + script->DefaultLangSysOffset);
	  READ_OFFSET (stream, langsys->LookupOrder);
	  READ_USHORT (stream, langsys->ReqFeatureIndex);
	  READ_USHORT (stream, langsys->FeatureCount);
	  OTF_MALLOC (langsys->FeatureIndex, langsys->FeatureCount,
		      " (FeatureIndex)");
	  for (k = 0; k < langsys->FeatureCount; k++)
	    READ_USHORT (stream, langsys->FeatureIndex[k]);
	}

      for (j = 0; j < script->LangSysCount; j++)
	{
	  OTF_LangSys *langsys = script->LangSys + j;

	  SEEK_STREAM (stream,
		       script_offset + script->LangSysRecord[j].LangSys);
	  READ_OFFSET (stream, langsys->LookupOrder);
	  READ_USHORT (stream, langsys->ReqFeatureIndex);
	  READ_USHORT (stream, langsys->FeatureCount);
	  OTF_MALLOC (langsys->FeatureIndex, langsys->FeatureCount,
		      " (FeatureIndex)");
	  for (k = 0; k < langsys->FeatureCount; k++)
	    READ_USHORT (stream, langsys->FeatureIndex[k]);
	}
    }

  return 0;
}

static int
read_feature_list (OTF *otf, OTF_Stream *stream, long offset,
		   OTF_FeatureList *list)
{
  char *errfmt = "Feature List%s";
  int errret = -1;
  int i, j;

  SEEK_STREAM (stream, offset);
  READ_UINT16 (stream, list->FeatureCount);
  OTF_CALLOC (list->Feature, list->FeatureCount, "");
  for (i = 0; i < list->FeatureCount; i++)
    {
      READ_TAG (stream, list->Feature[i].FeatureTag);
      READ_OFFSET (stream, list->Feature[i].offset);
    }
  for (i = 0; i < list->FeatureCount; i++)
    {
      OTF_Feature *feature = list->Feature + i;

      SEEK_STREAM (stream, offset + feature->offset);
      READ_OFFSET (stream, feature->FeatureParams);
      READ_UINT16 (stream, feature->LookupCount);
      OTF_MALLOC (feature->LookupListIndex, feature->LookupCount,
		  " (LookupListIndex)");
      for (j = 0; j < feature->LookupCount; j++)
	READ_UINT16 (stream, feature->LookupListIndex[j]);
    }

  return 0;
}

static int read_lookup_subtable_gsub (OTF *otf, OTF_Stream *stream,
				      long offset, unsigned type,
				      OTF_LookupSubTableGSUB *subtable);
static int read_lookup_subtable_gpos (OTF *otf, OTF_Stream *stream,
				      long offset, unsigned type,
				      OTF_LookupSubTableGPOS *subtable);

static int
read_lookup_list (OTF *otf, OTF_Stream *stream, long offset,
		  OTF_LookupList *list, int gsubp)
{
  char *errfmt = "Lookup List%s";
  int errret = -1;
  int i, j;

  SEEK_STREAM (stream, offset);
  READ_UINT16 (stream, list->LookupCount);
  OTF_CALLOC (list->Lookup, list->LookupCount, "");

  for (i = 0; i < list->LookupCount; i++)
    READ_OFFSET (stream, list->Lookup[i].offset);
  for (i = 0; i < list->LookupCount; i++)
    {
      OTF_Lookup *lookup = list->Lookup + i;

      SEEK_STREAM (stream, offset + lookup->offset);
      READ_UINT16 (stream, lookup->LookupType);
      READ_UINT16 (stream, lookup->LookupFlag);
      READ_UINT16 (stream, lookup->SubTableCount);
      OTF_MALLOC (lookup->SubTableOffset, lookup->SubTableCount,
		  " (SubTableOffset)");
      if (gsubp)
	OTF_CALLOC (lookup->SubTable.gsub, lookup->SubTableCount,
		    " (SubTable)");
      else
	OTF_CALLOC (lookup->SubTable.gpos, lookup->SubTableCount,
		    " (SubTable)");
      for (j = 0; j < lookup->SubTableCount; j++)
	READ_OFFSET (stream, lookup->SubTableOffset[j]);
      for (j = 0; j < lookup->SubTableCount; j++)
	{
	  long this_offset
	    = offset + lookup->offset + lookup->SubTableOffset[j];

	  if (gsubp
	      ? read_lookup_subtable_gsub (otf, stream, this_offset,
					   lookup->LookupType,
					   lookup->SubTable.gsub + j) < 0
	      : read_lookup_subtable_gpos (otf, stream, this_offset,
					   lookup->LookupType,
					   lookup->SubTable.gpos + j) < 0)
	    return errret;
	}
    }

  return 0;
}


/*** (1-8) Structures common to GSUB and GPOS */

static int
read_lookup_record_list (OTF *otf, OTF_Stream *stream,
			 OTF_LookupRecord **record, int count)
{
  char *errfmt = "LookupRecord%s";
  int errret = -1;
  int i;

  if (count < 0)
    READ_UINT16 (stream, count);
  OTF_MALLOC (*record, count, "");
  for (i = 0; i < count; i++)
    {
      READ_UINT16 (stream, (*record)[i].SequenceIndex);
      READ_UINT16 (stream, (*record)[i].LookupListIndex);
    }
  return count;
}

static unsigned
read_rule_list (OTF *otf, OTF_Stream *stream, long offset, OTF_Rule **rule)
{
  char *errfmt = "List of Rule%s";
  unsigned errret = 0;
  OTF_StreamState state;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*rule, count, "");
  for (i = 0; i < count; i++)
    {
      READ_OFFSET (stream, (*rule)[i].offset);
      if (! (*rule)[i].offset)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero offset)");
    }
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    {
      SEEK_STREAM (stream, offset + (*rule)[i].offset);
      READ_UINT16 (stream, (*rule)[i].GlyphCount);
      if ((*rule)[i].GlyphCount == 0)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
      READ_UINT16 (stream, (*rule)[i].LookupCount);
      if (read_glyph_ids (otf, stream, &(*rule)[i].Input, 0,
			  (*rule)[i].GlyphCount) < 0)
	return errret;
      if (read_lookup_record_list (otf, stream, &(*rule)[i].LookupRecord,
				   (*rule)[i].LookupCount) < 0)
	return errret;
    }
  RESTORE_STREAM (stream, state);
  return count;
}


static unsigned
read_rule_set_list (OTF *otf, OTF_Stream *stream, long offset,
		    OTF_RuleSet **set)
{
  char *errfmt = "List of RuleSet%s";
  unsigned errret = 0;
  OTF_StreamState state;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*set, count, "");
  for (i = 0; i < count; i++)
    {
      READ_OFFSET (stream, (*set)[i].offset);
      if (! (*set)[i].offset)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero offset)");
    }
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    {
      SEEK_STREAM (stream, offset + (*set)[i].offset);
      (*set)[i].RuleCount
	= read_rule_list (otf, stream, offset + (*set)[i].offset,
			  &(*set)[i].Rule);
      if (! (*set)[i].RuleCount)
	return errret;
    }
  RESTORE_STREAM (stream, state);
  return count;
}

static unsigned
read_class_rule_list (OTF *otf, OTF_Stream *stream, long offset,
		      OTF_ClassRule **rule)
{
  char *errfmt = "ClassRule%s";
  unsigned errret = 0;
  OTF_StreamState state;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*rule, count, "");
  for (i = 0; i < count; i++)
    {
      READ_OFFSET (stream, (*rule)[i].offset);
      if (! (*rule)[i].offset)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero offset)");
    }
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    {
      SEEK_STREAM (stream, offset + (*rule)[i].offset);
      READ_USHORT (stream, (*rule)[i].GlyphCount);
      if (! (*rule)[i].GlyphCount)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
      READ_USHORT (stream, (*rule)[i].LookupCount);
      if (read_glyph_ids (otf, stream, (OTF_GlyphID **) &(*rule)[i].Class,
			  0, (*rule)[i].GlyphCount - 1) < 0)
	return errret;
      if (read_lookup_record_list (otf, stream, &(*rule)[i].LookupRecord,
				   (*rule)[i].LookupCount) < 0)
	return errret;
    }
  RESTORE_STREAM (stream, state);
  return count;
}

static unsigned
read_class_set_list (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_ClassSet **set)
{
  char *errfmt = "ClassSet%s";
  unsigned errret = 0;
  OTF_StreamState state;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_CALLOC (*set, count, "");
  for (i = 0; i < count; i++)
    /* Offset can be zero.  */
    READ_OFFSET (stream, (*set)[i].offset);
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    if ((*set)[i].offset)
      {
	SEEK_STREAM (stream, offset + (*set)[i].offset);
	(*set)[i].ClassRuleCnt
	  = read_class_rule_list (otf, stream, offset + (*set)[i].offset,
				  &(*set)[i].ClassRule);
	if (! (*set)[i].ClassRuleCnt)
	  return errret;
      }
  RESTORE_STREAM (stream, state);
  return count;
}

static unsigned
read_chain_rule_list (OTF *otf, OTF_Stream *stream, long offset,
		      OTF_ChainRule **rule)
{
  char *errfmt = "ChainRule%s";
  unsigned errret = 0;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*rule, count, "");
  for (i = 0; i < count; i++)
    READ_OFFSET (stream, (*rule)[i].offset);
  for (i = 0; i < count; i++)
    {
      int lookup_record_count;

      SEEK_STREAM (stream, offset + (*rule)[i].offset);
      (*rule)[i].BacktrackGlyphCount
	= read_glyph_ids (otf, stream, &(*rule)[i].Backtrack, 0, -1);
      (*rule)[i].InputGlyphCount
	= read_glyph_ids (otf, stream, &(*rule)[i].Input, -1, -1);
      if (! (*rule)[i].InputGlyphCount)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
      (*rule)[i].LookaheadGlyphCount
	= read_glyph_ids (otf, stream, &(*rule)[i].LookAhead, 0, -1);
      lookup_record_count 
	= read_lookup_record_list (otf, stream,
				   &(*rule)[i].LookupRecord, -1);
      if (lookup_record_count < 0)
	return errret;
      (*rule)[i].LookupCount = lookup_record_count;
    }
  return count;
}


static unsigned
read_chain_rule_set_list (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_ChainRuleSet **set)
{
  char *errfmt = "ChainRuleSet%s";
  unsigned errret = 0;
  OTF_StreamState state;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*set, count, "");
  for (i = 0; i < count; i++)
    {
      READ_OFFSET (stream, (*set)[i].offset);
      if (! (*set)[i].offset)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero offset)");
    }
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    {
      SEEK_STREAM (stream, offset + (*set)[i].offset);
      (*set)[i].ChainRuleCount
	= read_chain_rule_list (otf, stream, offset + (*set)[i].offset,
				&(*set)[i].ChainRule);
      if (! (*set)[i].ChainRuleCount)
	return errret;
    }
  RESTORE_STREAM (stream, state);
  return count;
}

static unsigned
read_chain_class_rule_list (OTF *otf, OTF_Stream *stream, long offset,
			    OTF_ChainClassRule **rule)
{
  char *errfmt = "ChainClassRule%s";
  unsigned errret = 0;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*rule, count, "");
  for (i = 0; i < count; i++)
    {
      READ_OFFSET (stream, (*rule)[i].offset);
      if (! (*rule)[i].offset)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero offset)");
    }
  for (i = 0; i < count; i++)
    {
      int lookup_record_count;

      SEEK_STREAM (stream, offset + (*rule)[i].offset);
      (*rule)[i].BacktrackGlyphCount
	= read_glyph_ids (otf, stream,
			  (OTF_GlyphID **) &(*rule)[i].Backtrack, 0, -1);
      (*rule)[i].InputGlyphCount
	= read_glyph_ids (otf, stream,
			  (OTF_GlyphID **) &(*rule)[i].Input, -1, -1);
      if (! (*rule)[i].InputGlyphCount)
	OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
      (*rule)[i].LookaheadGlyphCount
	= read_glyph_ids (otf, stream,
			  (OTF_GlyphID **) &(*rule)[i].LookAhead, 0, -1);
      lookup_record_count
	= read_lookup_record_list (otf, stream,
				   &(*rule)[i].LookupRecord, -1);
      if (lookup_record_count < 0)
	return errret;
      (*rule)[i].LookupCount = lookup_record_count;
    }
  return count;
}

static unsigned
read_chain_class_set_list (OTF *otf, OTF_Stream *stream, long offset,
			   OTF_ChainClassSet **set)
{
  char *errfmt = "ChainClassSet%s";
  unsigned errret = 0;
  OTF_StreamState state;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*set, count, "");
  for (i = 0; i < count; i++)
    /* Offset may be zero.  */
    READ_OFFSET (stream, (*set)[i].offset);
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    if ((*set)[i].offset)
      {
	SEEK_STREAM (stream, offset + (*set)[i].offset);
	(*set)[i].ChainClassRuleCnt
	  = read_chain_class_rule_list (otf, stream, offset + (*set)[i].offset,
					&(*set)[i].ChainClassRule);
	if (! (*set)[i].ChainClassRuleCnt)
	  return errret;
      }
  RESTORE_STREAM (stream, state);
  return count;
}

static int
read_context1 (OTF *otf, OTF_Stream *stream, long offset,
	       OTF_Coverage *coverage,OTF_Context1 *context1)
{
  if (read_coverage (otf, stream, offset, coverage) < 0)
    return -1;
  context1->RuleSetCount
    = read_rule_set_list (otf, stream, offset, &context1->RuleSet);
  if (! context1->RuleSetCount)
    return -1;
  return 0;
}

static int
read_context2 (OTF *otf, OTF_Stream *stream, long offset,
	       OTF_Coverage *coverage,OTF_Context2 *context2)
{
  if (read_coverage (otf, stream, offset, coverage) < 0
      || read_class_def (otf, stream, offset, &context2->ClassDef) < 0)
    return -1;
  context2->ClassSetCnt
    = read_class_set_list (otf, stream, offset, &context2->ClassSet);
  if (! context2->ClassSetCnt)
    return -1;
  return 0;
}

static int
read_context3 (OTF *otf, OTF_Stream *stream, long offset,
	       OTF_Coverage *coverage,OTF_Context3 *context3)
{
  char *errfmt = "Context1%s";
  int errret = -1;

  READ_USHORT (stream, context3->GlyphCount);
  if (context3->GlyphCount < 0)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  READ_USHORT (stream, context3->LookupCount);
  if (read_coverage_list (otf, stream, offset, &context3->Coverage,
			  context3->GlyphCount) < 0)
    return errret;
  if (read_lookup_record_list (otf, stream, &context3->LookupRecord,
			       context3->LookupCount) < 0)
    return errret;
  return 0;
}

static int
read_chain_context1 (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_Coverage *coverage, OTF_ChainContext1 *chain_context1)
{
  if (read_coverage (otf, stream, offset, coverage) < 0)
    return -1;
  chain_context1->ChainRuleSetCount
    = read_chain_rule_set_list (otf, stream, offset,
				&chain_context1->ChainRuleSet);
  if (! chain_context1->ChainRuleSetCount)
    return -1;
  return 0;
}

static int
read_chain_context2 (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_Coverage *coverage, OTF_ChainContext2 *chain_context2)
{
  if (read_coverage (otf, stream, offset, coverage) < 0
      || read_class_def (otf, stream, offset,
			 &chain_context2->BacktrackClassDef) < 0
      || read_class_def (otf, stream, offset,
			 &chain_context2->InputClassDef) < 0
      || read_class_def (otf, stream, offset,
			 &chain_context2->LookaheadClassDef) < 0)
    return -1;
  chain_context2->ChainClassSetCnt
    = read_chain_class_set_list (otf, stream, offset,
				 &chain_context2->ChainClassSet);
  if (! chain_context2->ChainClassSetCnt)
    return -1;
  return 0;
}

static int
read_chain_context3 (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_Coverage *coverage, OTF_ChainContext3 *chain_context3)
{
  int count;

  count = read_coverage_list (otf, stream, offset,
			      &chain_context3->Backtrack, -1);
  if (count < 0)
    return -1;
  chain_context3->BacktrackGlyphCount = (unsigned) count;
  count = read_coverage_list (otf, stream, offset,
			      &chain_context3->Input, -1);
  if (count <= 0)
    return -1;
  chain_context3->InputGlyphCount = (unsigned) count;
  *coverage = chain_context3->Input[0];
  count = read_coverage_list (otf, stream, offset,
			      &chain_context3->LookAhead, -1);
  chain_context3->LookaheadGlyphCount = (unsigned) count;
  count = read_lookup_record_list (otf, stream,
				   &chain_context3->LookupRecord, -1);
  if (count < 0)
    return -1;
  chain_context3->LookupCount = count;
  return 0;
}

static void *
read_gsub_gpos_table (OTF *otf, OTF_TableInfo *table, int gsubp,
		      enum OTF_ReaderFlag flag)
{
  OTF_Stream *stream = table->stream;
  char *errfmt = gsubp ? "GSUB%s" : "GPOS%s";
  void *errret = NULL;
  OTF_GSUB_GPOS *gsub_gpos = *table->address;

  if (gsub_gpos)
    SEEK_STREAM (stream, 10);
  else
    {
      SEEK_STREAM (stream, 0);
      OTF_CALLOC (gsub_gpos, 1, "");
      READ_FIXED (stream, gsub_gpos->Version);
      READ_OFFSET (stream, gsub_gpos->ScriptList.offset);
      READ_OFFSET (stream, gsub_gpos->FeatureList.offset);
      READ_OFFSET (stream, gsub_gpos->LookupList.offset);
      *table->address = gsub_gpos;
    }

  if (! gsub_gpos->ScriptList.Script
      && read_script_list (otf, stream, gsub_gpos->ScriptList.offset,
			   &gsub_gpos->ScriptList) < 0)
    return NULL;
  if (flag != OTF_READ_SCRIPTS)
    {
      if (! gsub_gpos->FeatureList.Feature
	  && read_feature_list (otf, stream, gsub_gpos->FeatureList.offset,
				&gsub_gpos->FeatureList) < 0)
	return NULL;
      if (flag != OTF_READ_FEATURES)
	{
	  if (! gsub_gpos->LookupList.Lookup
	      && read_lookup_list (otf, stream, gsub_gpos->LookupList.offset,
				   &gsub_gpos->LookupList, gsubp) < 0)
	    return NULL;
	}
    }

  return gsub_gpos;
}


/* (1-9) "GSUB" table */

static unsigned
read_sequence (OTF *otf, OTF_Stream *stream, long offset, OTF_Sequence **seq)
{
  char *errfmt = "Sequence%s";
  unsigned errret = 0;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*seq, count, "");
  for (i = 0; i < count; i++)
    READ_OFFSET (stream, (*seq)[i].offset);
  for (i = 0; i < count; i++)
    {
      SEEK_STREAM (stream, offset + (*seq)[i].offset);
      (*seq)[i].GlyphCount = read_glyph_ids (otf, stream,
					     &(*seq)[i].Substitute, 0, -1);
      if (! (*seq)[i].GlyphCount)
	return 0;
    }
  return count;
}

static int
read_ligature (OTF *otf, OTF_Stream *stream, long offset,
	       OTF_Ligature **ligature)
{
  char *errfmt = "Ligature%s";
  int errret = -1;
  int count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    return 0;
  OTF_MALLOC (*ligature, count, "");
  for (i = 0; i < count; i++)
    READ_OFFSET (stream, (*ligature)[i].offset);
  for (i = 0; i < count; i++)
    {
      SEEK_STREAM (stream, offset + (*ligature)[i].offset);
      READ_GLYPHID (stream, (*ligature)[i].LigGlyph);
      (*ligature)[i].CompCount
	= read_glyph_ids (otf, stream, &(*ligature)[i].Component, -1, -1);
      if (! (*ligature)[i].CompCount)
	return -1;
    }
  return count;
}

static unsigned
read_ligature_set_list (OTF *otf, OTF_Stream *stream, long offset,
			OTF_LigatureSet **ligset)
{
  char *errfmt = "LigatureSet%s";
  int errret = 0;
  int count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    return errret;
  OTF_MALLOC (*ligset, count, "");
  for (i = 0; i < count; i++)
    READ_OFFSET (stream, (*ligset)[i].offset);
  for (i = 0; i < count; i++)
    {
      int lig_count;

      SEEK_STREAM (stream, offset + (*ligset)[i].offset);
      lig_count = read_ligature (otf, stream, offset + (*ligset)[i].offset,
				 &(*ligset)[i].Ligature);
      if (lig_count < 0)
	return errret;
      (*ligset)[i].LigatureCount = (unsigned) lig_count;
    }
  return count;
}

static unsigned
read_alternate_set_list (OTF *otf, OTF_Stream *stream, long offset,
			 OTF_AlternateSet **altset)
{
  char *errfmt = "AlternateSet%s";
  int errret = 0;
  unsigned count;
  int i;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*altset, count, "");
  for (i = 0; i < count; i++)
    READ_OFFSET (stream, (*altset)[i].offset);
  for (i = 0; i < count; i++)
    {
      int alt_count;

      SEEK_STREAM (stream, offset + (*altset)[i].offset);
      alt_count = read_glyph_ids (otf, stream, &(*altset)[i].Alternate, 0, -1);
      if (alt_count < 0)
	return errret;
      (*altset)[i].GlyphCount = (unsigned) alt_count;
    }
  return count;
}

static int
read_reverse_chain1 (OTF *otf, OTF_Stream *stream, long offset,
		     OTF_Coverage *coverage,
		     OTF_GSUB_ReverseChain1 *reverse_chain)
{
  int count;

  if (read_coverage (otf, stream, offset, coverage) < 0)
    return -1;
  count = read_coverage_list (otf, stream, offset,
			      &reverse_chain->Backtrack, -1);
  if (count < 0)
    return -1;
  reverse_chain->BacktrackGlyphCount = (unsigned) count;
  count = read_coverage_list (otf, stream, offset,
			      &reverse_chain->LookAhead, -1);
  if (count <= 0)
    return -1;
  reverse_chain->LookaheadGlyphCount = (unsigned) count;
  count = read_glyph_ids (otf, stream, &reverse_chain->Substitute, 0, -1);
  if (count <= 0)
    return -1;
  reverse_chain->GlyphCount = count;
  return 0;
}

static int
read_lookup_subtable_gsub (OTF *otf, OTF_Stream *stream, long offset,
			   unsigned type, OTF_LookupSubTableGSUB *subtable)
{
  char errfmt[256];
  int errret = -1;

  SEEK_STREAM (stream, offset);
  READ_USHORT (stream, subtable->Format);
  sprintf (errfmt, "GSUB Lookup %d-%d%%s", type, subtable->Format);
  switch (type)
    {
    case 1:
      if (subtable->Format == 1)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  READ_INT16 (stream, subtable->u.single1.DeltaGlyphID);
	}
      else if (subtable->Format == 2)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  subtable->u.single2.GlyphCount
	    = read_glyph_ids (otf, stream, &subtable->u.single2.Substitute,
			      0, -1);
	  if (! subtable->u.single2.GlyphCount)
	    return -1;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 2:
      if (subtable->Format == 1)
	{
	  read_coverage (otf, stream, offset, &subtable->Coverage);
	  subtable->u.multiple1.SequenceCount
	    = read_sequence (otf, stream, offset,
			     &subtable->u.multiple1.Sequence);
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 3:
      if (subtable->Format == 1)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  subtable->u.alternate1.AlternateSetCount
	    = read_alternate_set_list (otf, stream, offset,
				       &subtable->u.alternate1.AlternateSet);
	  if (! subtable->u.alternate1.AlternateSetCount)
	    return -1;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 4:
      if (subtable->Format == 1)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  subtable->u.ligature1.LigSetCount
	    = read_ligature_set_list (otf, stream, offset,
				      &subtable->u.ligature1.LigatureSet);
	  if (! subtable->u.ligature1.LigSetCount)
	    return -1;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 5:
      if (subtable->Format == 1)
	{
	  if (read_context1 (otf, stream, offset, &subtable->Coverage,
			     &subtable->u.context1) < 0)
	    return errret;
	}
      else if (subtable->Format == 2)
	{
	  if (read_context2 (otf, stream, offset, &subtable->Coverage,
			     &subtable->u.context2) < 0)
	    return errret;
	}
      else if (subtable->Format == 3)
	{
	  if (read_context3 (otf, stream, offset, &subtable->Coverage,
			     &subtable->u.context3) < 0)
	    return errret;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 6:
      if (subtable->Format == 1)
	{
	  if (read_chain_context1 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.chain_context1) < 0)
	    return errret;
	}
      else if (subtable->Format == 2)
	{
	  if (read_chain_context2 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.chain_context2) < 0)
	    return errret;
	}
      else if (subtable->Format == 3)
	{
	  if (read_chain_context3 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.chain_context3) < 0)
	    return errret;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 7:
      if (subtable->Format == 1)
	{
	  unsigned ex_type;
	  long ex_offset;
	  OTF_LookupSubTableGSUB *ex_subtable;

	  READ_USHORT (stream, ex_type);
	  READ_ULONG (stream, ex_offset);
	  OTF_CALLOC (ex_subtable, 1, " (SubTable)");
	  if (read_lookup_subtable_gsub (otf, stream, offset + ex_offset,
					 ex_type, ex_subtable) < 0)
	    return errret;
	  subtable->u.extension1.ExtensionLookupType = ex_type;
	  subtable->u.extension1.ExtensionOffset = ex_offset;
	  subtable->u.extension1.ExtensionSubtable = ex_subtable;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 8:
      if (subtable->Format == 1)
	{
	  if (read_reverse_chain1 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.reverse_chain1) < 0)
	    return errret;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    default:
      OTF_ERROR (OTF_ERROR_TABLE, " (Invalid LookupType)");
    }
  return 0;
}

static void *
read_gsub_table (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag)
{
  return read_gsub_gpos_table (otf, table, 1, flag);
}


/* (1-10) "GPOS" table */

static int
read_value_record (OTF *otf, OTF_Stream *stream, long offset,
		   enum OTF_ValueFormat bit, OTF_ValueRecord *value_record)
{
  int errret = -1;
  OTF_StreamState state;
  int size, i;

  memset (value_record, 0, sizeof (OTF_ValueRecord));
  if (! bit)
    return 0;
  for (i = 0, size = 0; i < 8; i++)
    if (bit & (1 << i))
      size += 2;

  if (bit & OTF_XPlacement)
    READ_INT16 (stream, value_record->XPlacement);
  if (bit & OTF_YPlacement)
    READ_INT16 (stream, value_record->YPlacement);
  if (bit & OTF_XAdvance)
    READ_INT16 (stream, value_record->XAdvance);
  if (bit & OTF_YAdvance)
    READ_INT16 (stream, value_record->YAdvance);
  if (bit & OTF_XPlaDevice)
    READ_OFFSET (stream, value_record->XPlaDevice.offset);
  if (bit & OTF_YPlaDevice)
    READ_OFFSET (stream, value_record->YPlaDevice.offset);
  if (bit & OTF_XAdvDevice)
    READ_OFFSET (stream, value_record->XAdvDevice.offset);
  if (bit & OTF_YAdvDevice)
    READ_OFFSET (stream, value_record->YAdvDevice.offset);
  SAVE_STREAM (stream, state);
  if (value_record->XPlaDevice.offset)
    {
      if (read_device_table (otf, stream, offset, &value_record->XPlaDevice) < 0)
	return -1;
    }
  if (value_record->YPlaDevice.offset)
    {
      if (read_device_table (otf, stream, offset, &value_record->YPlaDevice) < 0)
	return -1;
    }
  if (value_record->XAdvDevice.offset)
    {
      if (read_device_table (otf, stream, offset, &value_record->XAdvDevice) < 0)
	return -1;
    }
  if (value_record->YAdvDevice.offset)
    {
      if (read_device_table (otf, stream, offset, &value_record->YAdvDevice) < 0)
	return -1;
    }
  RESTORE_STREAM (stream, state);
  return 0;
}


static int
read_anchor (OTF *otf, OTF_Stream *stream, long offset, OTF_Anchor *anchor)
{
  char *errfmt = "Anchor%s";
  int errret = -1;

  SEEK_STREAM (stream, offset + anchor->offset);
  READ_UINT16 (stream, anchor->AnchorFormat);
  READ_INT16 (stream, anchor->XCoordinate);
  READ_INT16 (stream, anchor->YCoordinate);
  if (anchor->AnchorFormat == 1)
    ;
  else if (anchor->AnchorFormat == 2)
    {
      READ_UINT16 (stream, anchor->f.f1.AnchorPoint);
    }
  else if (anchor->AnchorFormat == 3)
    {
      READ_OFFSET (stream, anchor->f.f2.XDeviceTable.offset);
      READ_OFFSET (stream, anchor->f.f2.YDeviceTable.offset);
      if (anchor->f.f2.XDeviceTable.offset)
	{
	  if (read_device_table (otf, stream, offset + anchor->offset,
				 &anchor->f.f2.XDeviceTable) < 0)
	    return -1;
	}
      if (anchor->f.f2.YDeviceTable.offset)
	{
	  if (read_device_table (otf, stream, offset + anchor->offset,
				 &anchor->f.f2.YDeviceTable) < 0)
	    return -1;
	}
    }
  else
    OTF_ERROR (OTF_ERROR_TABLE, " (invalid format)");

  return 0;
}

static int
read_mark_array (OTF *otf, OTF_Stream *stream, long offset,
		 OTF_MarkArray *array)
{
  char *errfmt = "MarkArray%s";
  int errret = -1;
  OTF_StreamState state;
  int i;

  READ_OFFSET (stream, array->offset);
  SAVE_STREAM (stream, state);
  SEEK_STREAM (stream, offset + array->offset);
  READ_UINT16 (stream, array->MarkCount);
  OTF_MALLOC (array->MarkRecord, array->MarkCount, "");
  for (i = 0; i < array->MarkCount; i++)
    {
      READ_UINT16 (stream, array->MarkRecord[i].Class);
      READ_OFFSET (stream, array->MarkRecord[i].MarkAnchor.offset);
    }
  for (i = 0; i < array->MarkCount; i++)
    if (read_anchor (otf, stream, offset + array->offset,
		     &array->MarkRecord[i].MarkAnchor) < 0)
      return -1;;
  RESTORE_STREAM (stream, state);
  return 0;
}

static int
read_anchor_array (OTF *otf, OTF_Stream *stream, long offset,
		   unsigned ClassCount, OTF_AnchorArray *array)
{
  char *errfmt = "AnchorArray%s";
  int errret = -1;
  OTF_StreamState state;
  int i, j;

  READ_OFFSET (stream, array->offset);
  SAVE_STREAM (stream, state);
  SEEK_STREAM (stream, offset + array->offset);
  READ_UINT16 (stream, array->Count);
  OTF_MALLOC (array->AnchorRecord, array->Count, "");
  for (i = 0; i < array->Count; i++)
    {
      OTF_MALLOC (array->AnchorRecord[i].Anchor, ClassCount,
		  " (AnchorRecord)");
      for (j = 0; j < ClassCount; j++)
	READ_OFFSET (stream, array->AnchorRecord[i].Anchor[j].offset);
    }
  for (i = 0; i < array->Count; i++)
    for (j = 0; j < ClassCount; j++)
      if (array->AnchorRecord[i].Anchor[j].offset > 0
	  && read_anchor (otf, stream, offset + array->offset,
			  &array->AnchorRecord[i].Anchor[j]) < 0)
	return -1;
  RESTORE_STREAM (stream, state);
  return 0;
}

static OTF_PairSet *
read_pair_set_list (OTF *otf, OTF_Stream *stream, long offset, unsigned num,
		    enum OTF_ValueFormat bit1, enum OTF_ValueFormat bit2)
{
  char *errfmt = "PairSet%s";
  void *errret = NULL;
  OTF_StreamState state;
  OTF_PairSet *set;
  int i, j;

  OTF_MALLOC (set, num, "");
  for (i = 0; i < num; i++)
    READ_OFFSET (stream, set[i].offset);
  SAVE_STREAM (stream, state);
  for (i = 0; i < num; i++)
    {
      SEEK_STREAM (stream, offset + set[i].offset);
      READ_UINT16 (stream, set[i].PairValueCount);
      OTF_MALLOC (set[i].PairValueRecord, set[i].PairValueCount, "");
      for (j = 0; j < set[i].PairValueCount; j++)
	{
	  OTF_PairValueRecord *rec = set[i].PairValueRecord + j;

	  READ_UINT16 (stream, rec->SecondGlyph);
	  read_value_record (otf, stream, offset, bit1, &rec->Value1);
	  read_value_record (otf, stream, offset, bit2, &rec->Value2);
	}
    }
  RESTORE_STREAM (stream, state);
  return set;
}

static OTF_Class1Record *
read_class1_record_list (OTF *otf, OTF_Stream *stream, long offset,
			 unsigned num1, enum OTF_ValueFormat bit1,
			 unsigned num2, enum OTF_ValueFormat bit2)
{
  char *errfmt = "Class1Record%s";
  void *errret = NULL;
  OTF_Class1Record *rec;
  int i, j;

  OTF_MALLOC (rec, num1, "");
  for (i = 0; i < num1; i++)
    {
      OTF_CALLOC (rec[i].Class2Record, num2, " (Class2Record)");
      for (j = 0; j < num2; j++)
	{
	  if (read_value_record (otf, stream, offset,
				 bit1, &rec[i].Class2Record[j].Value1) < 0
	      || read_value_record (otf, stream, offset,
				    bit2, &rec[i].Class2Record[j].Value2) < 0)
	    return NULL;
	}
    }
  return rec;
}

static unsigned
read_entry_exit_list (OTF *otf, OTF_Stream *stream, long offset,
		      OTF_EntryExitRecord **rec)
{
  char *errfmt = "EntryExitSet%s";
  int errret = 0;
  unsigned count;
  int i;
  OTF_StreamState state;

  READ_UINT16 (stream, count);
  if (! count)
    OTF_ERROR (OTF_ERROR_TABLE, " (zero count)");
  OTF_MALLOC (*rec, count, "");
  for (i = 0; i < count; i++)
    {
      READ_OFFSET (stream, (*rec)[i].EntryAnchor.offset);
      READ_OFFSET (stream, (*rec)[i].ExitAnchor.offset);
    }
  SAVE_STREAM (stream, state);
  for (i = 0; i < count; i++)
    {
      if (read_anchor (otf, stream, offset, &(*rec)[i].EntryAnchor) < 0)
	return -1;
      if (read_anchor (otf, stream, offset, &(*rec)[i].ExitAnchor) < 0)
	return -1;
    }
  RESTORE_STREAM (stream, state);
  return count;
}

static int
read_ligature_attach (OTF *otf, OTF_Stream *stream, long offset,
		      unsigned ClassCount, OTF_LigatureAttach *attach)
{
  char *errfmt = "LigatureAttach%s";
  int errret = -1;
  int i, j;

  SEEK_STREAM (stream, offset + attach->offset);
  READ_UINT16 (stream, attach->ComponentCount);
  OTF_MALLOC (attach->ComponentRecord, attach->ComponentCount, "");
  for (i = 0; i < attach->ComponentCount; i++)
    {
      OTF_MALLOC (attach->ComponentRecord[i].LigatureAnchor, ClassCount,
		  " (ComponentRecord)");
      for (j = 0; j < ClassCount; j++)
	READ_OFFSET (stream,
		     attach->ComponentRecord[i].LigatureAnchor[j].offset);
    }
  for (i = 0; i < attach->ComponentCount; i++)
    for (j = 0; j < ClassCount; j++)
      {
	if (attach->ComponentRecord[i].LigatureAnchor[j].offset)
	  {
	    if (read_anchor (otf, stream, offset + attach->offset,
			     &attach->ComponentRecord[i].LigatureAnchor[j]) < 0)
	      return -1;
	  }
	else
	  attach->ComponentRecord[i].LigatureAnchor[j].AnchorFormat = 0;
      }
  return 0;
}

static int
read_ligature_array (OTF *otf, OTF_Stream *stream, long offset,
		     unsigned class_count, OTF_LigatureArray *array)
{
  char *errfmt = "LigatureArray%s";
  int errret = -1;
  OTF_StreamState state;
  int i;

  READ_OFFSET (stream, array->offset);
  SAVE_STREAM (stream, state);
  SEEK_STREAM (stream, offset + array->offset);
  READ_UINT16 (stream, array->LigatureCount);
  OTF_MALLOC (array->LigatureAttach, array->LigatureCount, "");
  for (i = 0; i < array->LigatureCount; i++)
    READ_OFFSET (stream, array->LigatureAttach[i].offset);
  for (i = 0; i < array->LigatureCount; i++)
    if (array->LigatureAttach[i].offset > 0
	&& read_ligature_attach (otf, stream, offset + array->offset,
				 class_count, array->LigatureAttach + i) < 0)
      return -1;
  RESTORE_STREAM (stream, state);
  return 0;
}

static int
read_lookup_subtable_gpos (OTF *otf, OTF_Stream *stream,
			   long offset, unsigned type,
			   OTF_LookupSubTableGPOS *subtable)
{
  char errfmt[256];
  int errret = -1;

  SEEK_STREAM (stream, offset);
  READ_UINT16 (stream, subtable->Format);
  sprintf (errfmt, "GPOS Lookup %d-%d%%s", type, subtable->Format);
  switch (type)
    {
    case 1:
      if (subtable->Format == 1)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  READ_UINT16 (stream, subtable->u.single1.ValueFormat);
	  read_value_record (otf, stream, offset,
			     subtable->u.single1.ValueFormat,
			     &subtable->u.single1.Value);
	}
      else if (subtable->Format == 2)
	{
	  OTF_GPOS_Single2 *single2 = &subtable->u.single2;
	  int i;

	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  READ_UINT16 (stream, single2->ValueFormat);
	  READ_UINT16 (stream, single2->ValueCount);
	  OTF_CALLOC (single2->Value, single2->ValueCount," (ValueRecord)");
	  for (i = 0; i < single2->ValueCount; i++)
	    read_value_record (otf, stream, offset, single2->ValueFormat,
			       single2->Value + i);
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 2:
      if (subtable->Format == 1)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  READ_UINT16 (stream, subtable->u.pair1.ValueFormat1);
	  READ_UINT16 (stream, subtable->u.pair1.ValueFormat2);
	  READ_UINT16 (stream, subtable->u.pair1.PairSetCount);
	  subtable->u.pair1.PairSet
	    = read_pair_set_list (otf, stream, offset,
				  subtable->u.pair1.PairSetCount,
				  subtable->u.pair1.ValueFormat1,
				  subtable->u.pair1.ValueFormat2);
	  if (! subtable->u.pair1.PairSet)
	    return -1;
	}
      else if (subtable->Format == 2)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  READ_UINT16 (stream, subtable->u.pair2.ValueFormat1);
	  READ_UINT16 (stream, subtable->u.pair2.ValueFormat2);
	  if (read_class_def (otf, stream, offset,
			      &subtable->u.pair2.ClassDef1) < 0
	      || read_class_def (otf, stream, offset,
				 &subtable->u.pair2.ClassDef2) < 0)
	    return -1;
	  READ_UINT16 (stream, subtable->u.pair2.Class1Count);
	  READ_UINT16 (stream, subtable->u.pair2.Class2Count);
	  subtable->u.pair2.Class1Record
	    = read_class1_record_list (otf, stream, offset,
				       subtable->u.pair2.Class1Count,
				       subtable->u.pair2.ValueFormat1,
				       subtable->u.pair2.Class2Count,
				       subtable->u.pair2.ValueFormat2);
	  if (! subtable->u.pair2.Class1Record)
	    return -1;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 3:
      if (subtable->Format == 1)
	{
	  if (read_coverage (otf, stream, offset, &subtable->Coverage) < 0)
	    return -1;
	  subtable->u.cursive1.EntryExitCount
	    = read_entry_exit_list (otf, stream, offset,
				    &subtable->u.cursive1.EntryExitRecord);
	  if (! subtable->u.cursive1.EntryExitCount)
	    return -1;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 4:
      if (subtable->Format == 1)
	{
	  read_coverage (otf, stream, offset, &subtable->Coverage);
	  read_coverage (otf, stream, offset,
			 &subtable->u.mark_base1.BaseCoverage);
	  READ_UINT16 (stream, subtable->u.mark_base1.ClassCount);
	  read_mark_array (otf, stream, offset,
			   &subtable->u.mark_base1.MarkArray);
	  read_anchor_array (otf, stream, offset,
			     subtable->u.mark_base1.ClassCount,
			     &subtable->u.mark_base1.BaseArray);
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 5:
      if (subtable->Format == 1)
	{
	  read_coverage (otf, stream, offset, &subtable->Coverage);
	  read_coverage (otf, stream, offset,
			 &subtable->u.mark_lig1.LigatureCoverage);
	  READ_UINT16 (stream, subtable->u.mark_lig1.ClassCount);
	  read_mark_array (otf, stream, offset,
			   &subtable->u.mark_lig1.MarkArray);
	  read_ligature_array (otf, stream, offset,
			       subtable->u.mark_lig1.ClassCount,
			       &subtable->u.mark_lig1.LigatureArray);
	}
      break;

    case 6:
      if (subtable->Format == 1)
	{
	  read_coverage (otf, stream, offset, &subtable->Coverage);
	  read_coverage (otf, stream, offset,
			 &subtable->u.mark_mark1.Mark2Coverage);
	  READ_UINT16 (stream, subtable->u.mark_mark1.ClassCount);
	  read_mark_array (otf, stream, offset,
			   &subtable->u.mark_mark1.Mark1Array);
	  read_anchor_array (otf, stream, offset,
			     subtable->u.mark_mark1.ClassCount,
			     &subtable->u.mark_mark1.Mark2Array);
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 7:
      if (subtable->Format == 1)
	{
	  if (read_context1 (otf, stream, offset, &subtable->Coverage,
			     &subtable->u.context1) < 0)
	    return errret;
	}
      else if (subtable->Format == 2)
	{
	  if (read_context2 (otf, stream, offset, &subtable->Coverage,
			     &subtable->u.context2) < 0)
	    return errret;
	}
      else if (subtable->Format == 3)
	{
	  if (read_context3 (otf, stream, offset, &subtable->Coverage,
			     &subtable->u.context3) < 0)
	    return errret;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 8:
      if (subtable->Format == 1)
	{
	  if (read_chain_context1 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.chain_context1) < 0)
	    return errret;
	}
      else if (subtable->Format == 2)
	{
	  if (read_chain_context2 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.chain_context2) < 0)
	    return errret;
	}
      else if (subtable->Format == 3)
	{
	  if (read_chain_context3 (otf, stream, offset, &subtable->Coverage,
				   &subtable->u.chain_context3) < 0)
	    return errret;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    case 9:
      if (subtable->Format == 1)
	{
	  unsigned ex_type;
	  long ex_offset;
	  OTF_LookupSubTableGPOS *ex_subtable;

	  READ_USHORT (stream, ex_type);
	  READ_ULONG (stream, ex_offset);
	  OTF_CALLOC (ex_subtable, 1, " (SubTable)");
	  if (read_lookup_subtable_gpos (otf, stream, offset + ex_offset,
					 ex_type, ex_subtable) < 0)
	    return errret;
	  subtable->u.extension1.ExtensionLookupType = ex_type;
	  subtable->u.extension1.ExtensionOffset = ex_offset;
	  subtable->u.extension1.ExtensionSubtable = ex_subtable;
	}
      else
	OTF_ERROR (OTF_ERROR_TABLE, " (Invalid SubFormat)");
      break;

    default:
      OTF_ERROR (OTF_ERROR_TABLE, " (Invalid LookupType)");
    }
  return 0;
}

static void *
read_gpos_table (OTF *otf, OTF_TableInfo *table, enum OTF_ReaderFlag flag)
{
  return read_gsub_gpos_table (otf, table, 0, flag);
}


#if 0
/* BASE */

static OTF_BASE *
read_base_table (OTF_Stream *stream, long offset)
{
  OTF_BASE *base;

  OTF_MALLOC (base, 1);

  return base;
}


/* JSTF */

static OTF_JSTF *
read_jstf_table (OTF_Stream *stream, long offset)
{
  OTF_JSTF *jstf;

  OTF_MALLOC (jstf, 1);

  return jstf;
}
#endif /* 0 */

/*** (1-11) Structure for OTF */

static int
read_offset_table (OTF *otf, OTF_Stream *stream, OTF_OffsetTable *table)
{
  int errret = -1;

  READ_FIXED (stream, table->sfnt_version);
  READ_USHORT (stream, table->numTables);
  READ_USHORT (stream, table->searchRange);
  READ_USHORT (stream, table->enterSelector);
  READ_USHORT (stream, table->rangeShift);
  return 0;
}

static OTF_Tag
read_table_directory (OTF_Stream *stream, OTF_TableDirectory *table)
{
  int errret = 0;
  OTF_Tag tag;

  READ_TAG (stream, tag);
  table->tag = tag;
  table->name[0] = tag >> 24;
  table->name[1] = (tag >> 16) & 0xFF;
  table->name[2] = (tag >> 8) & 0xFF;
  table->name[3] = tag & 0xFF;
  table->name[4] = '\0';
  READ_ULONG (stream, table->checkSum);
  READ_ULONG (stream, table->offset);
  READ_ULONG (stream, table->length);
  return tag;
}

static int
read_header_part (OTF *otf, FILE *fp, FT_Face face)
{
  char *errfmt = "otf header%s";
  int errret = -1;
  int i;
  OTF_InternalData *internal_data = (OTF_InternalData *) otf->internal_data;

  internal_data->table_info[OTF_TABLE_TYPE_HEAD].address = (void *) &otf->head;
  internal_data->table_info[OTF_TABLE_TYPE_HEAD].reader = read_head_table;
  internal_data->table_info[OTF_TABLE_TYPE_NAME].address = (void *) &otf->name;
  internal_data->table_info[OTF_TABLE_TYPE_NAME].reader = read_name_table;
  internal_data->table_info[OTF_TABLE_TYPE_CMAP].address = (void *) &otf->cmap;
  internal_data->table_info[OTF_TABLE_TYPE_CMAP].reader = read_cmap_table;
  internal_data->table_info[OTF_TABLE_TYPE_GDEF].address = (void *) &otf->gdef;
  internal_data->table_info[OTF_TABLE_TYPE_GDEF].reader = read_gdef_table;
  internal_data->table_info[OTF_TABLE_TYPE_GSUB].address = (void *) &otf->gsub;
  internal_data->table_info[OTF_TABLE_TYPE_GSUB].reader = read_gsub_table;
  internal_data->table_info[OTF_TABLE_TYPE_GPOS].address = (void *) &otf->gpos;
  internal_data->table_info[OTF_TABLE_TYPE_GPOS].reader = read_gpos_table;

  if (fp)
    {
      OTF_Tag head_tag = OTF_tag ("head");
      OTF_Tag name_tag = OTF_tag ("name");
      OTF_Tag cmap_tag = OTF_tag ("cmap");
      OTF_Tag gdef_tag = OTF_tag ("GDEF");
      OTF_Tag gsub_tag = OTF_tag ("GSUB");
      OTF_Tag gpos_tag = OTF_tag ("GPOS");
      OTF_Stream *stream = make_stream ("Offset Table");
      unsigned char ttctag[4];

      if (! stream)
	return -1;
      internal_data->header_stream = stream;

      /* Size of Offset Table is 12 bytes.  Size of TTC header
	 (including only the an offset of the first font) is 16.  */
      if (setup_stream (stream, fp, 0, 16) < 0)
	return -1;
      READ_BYTES (stream, ttctag, 4);
      if (memcmp (ttctag, "ttcf", 4) == 0)
	{
	  /* This is a TrueType Collection file.  We extract the first font.  */
	  unsigned version, numfonts, offset;
	  READ_ULONG (stream, version);
	  READ_ULONG (stream, numfonts);
	  READ_ULONG (stream, offset); /* Offset of the first font.  */
	  if (setup_stream (stream, fp, offset, 12) < 0)
	    return -1;
	}
      else
	SEEK_STREAM (stream, 0L);
      if (read_offset_table (otf, stream, &otf->offset_table) < 0)
	return -1;
      /* Size of each Table Directory is 16 bytes.  */
      if (setup_stream (stream, fp, stream->pos,
			16 * otf->offset_table.numTables) < 0)
	return -1;

      OTF_CALLOC (otf->table_dirs, otf->offset_table.numTables,
		  " (OffsetTable)");
      for (i = 0; i < otf->offset_table.numTables; i++)
	{
	  OTF_Tag tag = read_table_directory (stream, otf->table_dirs + i);
	  OTF_TableInfo *table_info = NULL;

	  if (! tag)
	    return -1;
	  if (tag == head_tag)
	    table_info = internal_data->table_info + OTF_TABLE_TYPE_HEAD;
	  else if (tag == name_tag)
	    table_info = internal_data->table_info + OTF_TABLE_TYPE_NAME;
	  else if (tag == cmap_tag)
	    table_info = internal_data->table_info + OTF_TABLE_TYPE_CMAP;
	  else if (tag == gdef_tag)
	    table_info = internal_data->table_info + OTF_TABLE_TYPE_GDEF;
	  else if (tag == gsub_tag)
	    table_info = internal_data->table_info + OTF_TABLE_TYPE_GSUB;
	  else if (tag == gpos_tag)
	    table_info = internal_data->table_info + OTF_TABLE_TYPE_GPOS;

	  if (table_info)
	    {
	      table_info->stream = make_stream (otf->table_dirs[i].name);
	      if (setup_stream (table_info->stream, fp,
				otf->table_dirs[i].offset,
				otf->table_dirs[i].length) < 0)
		return -1;
	    }
	}

      internal_data->header_stream = NULL;
      free_stream (stream);
    }
  else
    {
      OTF_Stream *stream;

      internal_data->header_stream = NULL;
      if ((stream = make_stream_from_ft_face (face, "head")))
	internal_data->table_info[OTF_TABLE_TYPE_HEAD].stream = stream;
      if ((stream = make_stream_from_ft_face (face, "name")))
	internal_data->table_info[OTF_TABLE_TYPE_NAME].stream = stream;
      if ((stream = make_stream_from_ft_face (face, "cmap")))
	internal_data->table_info[OTF_TABLE_TYPE_CMAP].stream = stream;
      if ((stream = make_stream_from_ft_face (face, "GDEF")))
	internal_data->table_info[OTF_TABLE_TYPE_GDEF].stream = stream;
      if ((stream = make_stream_from_ft_face (face, "GSUB")))
	internal_data->table_info[OTF_TABLE_TYPE_GSUB].stream = stream;
      if ((stream = make_stream_from_ft_face (face, "GPOS")))
	internal_data->table_info[OTF_TABLE_TYPE_GPOS].stream = stream;
    }

  if (! internal_data->table_info[OTF_TABLE_TYPE_GDEF].stream)
    /* We can simulate the GDEF table.  */
    internal_data->table_info[OTF_TABLE_TYPE_GDEF].stream
      = make_stream ("GDEF");
  return 0;
}

static OTF_TableInfo *
get_table_info (OTF *otf, const char *name)
{
  char *errfmt = "OTF Table Read%s";
  OTF_TableInfo *errret = NULL;
  OTF_InternalData *internal_data = otf->internal_data;
  OTF_TableInfo *table_info;
  OTF_Tag tag = OTF_tag (name);

  if (! tag)
    OTF_ERROR (OTF_ERROR_TABLE, " (invalid table name)");

  if (tag == OTF_tag ("head"))
    table_info = internal_data->table_info + OTF_TABLE_TYPE_HEAD;
  else if (tag == OTF_tag ("name"))
    table_info = internal_data->table_info + OTF_TABLE_TYPE_NAME;
  else if (tag == OTF_tag ("cmap"))
    table_info = internal_data->table_info + OTF_TABLE_TYPE_CMAP;
  else if (tag == OTF_tag ("GDEF"))
    table_info = internal_data->table_info + OTF_TABLE_TYPE_GDEF;
  else if (tag == OTF_tag ("GSUB"))
    table_info = internal_data->table_info + OTF_TABLE_TYPE_GSUB;
  else if (tag == OTF_tag ("GPOS"))
    table_info = internal_data->table_info + OTF_TABLE_TYPE_GPOS;
  else
    OTF_ERROR (OTF_ERROR_TABLE, " (unsupported table name)");

  if (*table_info->address)
    /* Already read.  */
    return table_info;
  if (! table_info->stream)
    OTF_ERROR (OTF_ERROR_TABLE, " (table not found)");
  if (! table_info->reader)
    OTF_ERROR (OTF_ERROR_TABLE, " (invalid contents)");
  return table_info;
}



/*** (2) API for reading OTF */

/*** (2-1) OTF_open() */

/* Note: We can't use memory allocation macros in the following
   functions because those macros return from the functions before
   freeing memory previously allocated.  */

OTF *
OTF_open (const char *otf_name)
{
  FILE *fp;
  char *errfmt = "opening otf (%s)";
  void *errret = NULL;
  OTF *otf;
  OTF_InternalData *internal_data;
  int len = strlen (otf_name);
  const char *ext = otf_name + (len - 4);

  if (debug_flag < 0)
    set_debug_flag ();

  if (len < 4
      || ext[0] != '.'
      || (strncasecmp (ext + 1, "otf", 3)
	  && strncasecmp (ext + 1, "ttf", 3)
	  && strncasecmp (ext + 1, "ttc", 3)))
    OTF_ERROR (OTF_ERROR_FILE, otf_name);
  fp = fopen (otf_name, "rb");
  if (! fp)
    OTF_ERROR (OTF_ERROR_FILE, otf_name);
  otf = calloc (1, sizeof (OTF));
  if (! otf)
    OTF_ERROR (OTF_ERROR_MEMORY, "body allocation");
  otf->filename = strdup (otf_name);
  if (! otf->filename)
    {
      OTF_close (otf);
      fclose (fp);
      OTF_ERROR (OTF_ERROR_MEMORY, "filename allocation");
    }

  internal_data = calloc (1, sizeof (OTF_InternalData));
  if (! internal_data)
    OTF_ERROR (OTF_ERROR_MEMORY, " (InternalData");
  otf->internal_data = internal_data;
  if (! allocate_memory_record (otf))
    OTF_ERROR (OTF_ERROR_MEMORY, " (InternalData)");

  /* Here after, all pointers to allocated memory are recorded in
     otf->internal_data->memory_record except for what allocated by
     the functions allocate_memory_record and make_stream.  */

  if (read_header_part (otf, fp, NULL) < 0)
    {
      OTF_close (otf);
      fclose (fp);
      return NULL;
    }

  fclose (fp);
  return otf;
}

OTF *
OTF_open_ft_face (FT_Face face)
{
  char *errfmt = "opening otf from Freetype (%s)";
  void *errret = NULL;
  OTF *otf;
  OTF_InternalData *internal_data;

  if (debug_flag < 0)
    set_debug_flag ();

  if (! FT_IS_SFNT (face))
    OTF_ERROR (OTF_ERROR_FILE, (char *) face->family_name);
  otf = calloc (1, sizeof (OTF));
  if (! otf)
    OTF_ERROR (OTF_ERROR_MEMORY, "body allocation");
  otf->filename = NULL;

  internal_data = calloc (1, sizeof (OTF_InternalData));
  if (! internal_data)
    OTF_ERROR (OTF_ERROR_MEMORY, " (InternalData");
  otf->internal_data = internal_data;
  if (! allocate_memory_record (otf))
    OTF_ERROR (OTF_ERROR_MEMORY, " (InternalData)");

  if (read_header_part (otf, NULL, face) < 0)
    {
      OTF_close (otf);
      return NULL;
    }

  return otf;
}

/*** (2-2) OTF_close() */

void
OTF_close (OTF *otf)
{
  OTF_InternalData *internal_data = otf->internal_data;
  int i;

  if (internal_data)
    {
      OTF_MemoryRecord *memrec = internal_data->memory_record;
      OTF_ApplicationData *app_data = internal_data->app_data;

      if (internal_data->header_stream)
	free_stream (internal_data->header_stream);

      for (i = 0; i < OTF_TABLE_TYPE_MAX; i++)
	if (internal_data->table_info[i].stream)
	  free_stream (internal_data->table_info[i].stream);

      for (; app_data; app_data = app_data->next)
	if (app_data->data && app_data->freer)
	  app_data->freer (app_data->data);

      while (memrec)
	{
	  OTF_MemoryRecord *next = memrec->next;

	  for (i = memrec->used - 1; i >= 0; i--)
	    free (memrec->memory[i]);
	  free (memrec);
	  memrec = next;
	}

      free (internal_data);
    }
  if (otf->filename)
    free (otf->filename);
  free (otf);
}

/*** (2-3) OTF_get_table() */

int
OTF_get_table (OTF *otf, const char *name)
{
  OTF_TableInfo *table_info = get_table_info (otf, name);
  void *address;

  if (! table_info)
    return -1;
  if (! table_info->stream)
    /* Already fully loaded.  */
    return 0;

  address = (*table_info->reader) (otf, table_info, OTF_READ_FULL);
  free_stream (table_info->stream);
  table_info->stream = NULL;
  if (! address)
    {
      table_info->reader = NULL;
      return -1;
    }
  return 0;
}

/*** (2-4) OTF_check_table() */

int
OTF_check_table (OTF *otf, const char *name)
{
  return (get_table_info (otf, name) ? 0 : -1);
}

/*** (2-5) OTF_get_scripts()  */

int
OTF_get_scripts (OTF *otf, int gsubp)
{
  OTF_TableInfo *table_info
    = (otf->internal_data->table_info
       + (gsubp ? OTF_TABLE_TYPE_GSUB : OTF_TABLE_TYPE_GPOS));
  void *address;

  if (! table_info->reader)
    return -1;
  if (! table_info->stream)
    /* Already fully loaded.  */
    return 0;

  address = (*table_info->reader) (otf, table_info, OTF_READ_SCRIPTS);
  if (! address)
    {
      table_info->reader = NULL;
      return -1;
    }
  return 0;
}

/*** (2-6) OTF_get_features()  */

int
OTF_get_features (OTF *otf, int gsubp)
{
  OTF_TableInfo *table_info
    = (otf->internal_data->table_info
       + (gsubp ? OTF_TABLE_TYPE_GSUB : OTF_TABLE_TYPE_GPOS));
  void *address;

  if (! table_info->reader)
    return -1;
  if (! table_info->stream)
    {
      if (*table_info->address)
	/* Already fully loaded.  */
	return 0;
      return -1;
    }

  address = (*table_info->reader) (otf, table_info, OTF_READ_FEATURES);
  if (! address)
    {
      table_info->reader = NULL;
      return -1;
    }
  return 0;
}

/*** (2-7) OTF_check_features  */

int
OTF_check_features (OTF *otf, int gsubp,
		    OTF_Tag script, OTF_Tag language, const OTF_Tag *features,
		    int n_features)
{
  OTF_ScriptList *script_list;
  OTF_Script *Script = NULL;
  OTF_LangSys *LangSys = NULL;
  OTF_FeatureList *feature_list;
  int i, j;

  if (OTF_get_features (otf, gsubp) < 0)
    {
      if (gsubp ? ! otf->gsub : ! otf->gpos)
	return 0;
      for (i = 0; i < n_features; i++)
	{
	  OTF_Tag feature = features[i];

	  if (feature == 0)
	    continue;
	  if ((((unsigned) feature) & 0x80000000) == 0)
	    return -1;
	}
    }
  if (gsubp)
    {
      script_list = &otf->gsub->ScriptList;
      feature_list = &otf->gsub->FeatureList;
    }
  else
    {
      script_list = &otf->gpos->ScriptList;
      feature_list = &otf->gpos->FeatureList;
    }
  for (i = 0; i < script_list->ScriptCount && ! Script; i++)
    if (script_list->Script[i].ScriptTag == script)
      Script = script_list->Script + i;
  if (! Script)
    return 0;
  if (language)
    {
      for (i = 0; i < Script->LangSysCount && ! LangSys; i++)
	if (Script->LangSysRecord[i].LangSysTag == language)
	  LangSys = Script->LangSys + i;
      if (! LangSys)
	return 0;
    }
  else
    LangSys = &Script->DefaultLangSys;
  for (j = 0; j < n_features; j++)
    {
      OTF_Tag feature = features[j];
      int negate = 0;

      if (feature == 0)
	continue;
      if (((unsigned) feature) & 0x80000000)
	{
	  feature = (OTF_Tag) (((unsigned) feature) & 0x7FFFFFFF);
	  negate = 1;
	}
      for (i = 0; i < LangSys->FeatureCount; i++)
	if (feature_list->Feature[LangSys->FeatureIndex[i]].FeatureTag
	    == feature)
	  {
	    if (negate)
	      return 0;
	    break;
	  }
      if (i == LangSys->FeatureCount)
	return 0;
    }
  return 1;
}


/*** (5) API miscellaneous ***/

OTF_Tag
OTF_tag (const char *name)
{
  const unsigned char *p = (unsigned char *) name;

  if (! name)
    return (OTF_Tag) 0;
  return (OTF_Tag) ((p[0] << 24)
		    | (! p[1] ? 0
		       : ((p[1] << 16)
			  | (! p[2] ? 0
			     : (p[2] << 8) | p[3]))));
}

void
OTF_tag_name (OTF_Tag tag, char *name)
{
  name[0] = (char) (tag >> 24);
  name[1] = (char) ((tag >> 16) & 0xFF);
  name[2] = (char) ((tag >> 8) & 0xFF);
  name[3] = (char) (tag & 0xFF);
  name[4] = '\0';
}

int
OTF_put_data (OTF *otf, char *id, void *data, void (*freer) (void *data))
{
  char *errfmt = "appdata %s";
  int errret = -1;
  OTF_InternalData *internal_data = (OTF_InternalData *) otf->internal_data;
  OTF_ApplicationData *app_data = internal_data->app_data;
  int len = strlen (id) + 1;

  for (; app_data; app_data = app_data->next)
    if (memcmp (app_data->id, id, len) == 0)
      {
	if (app_data->data && app_data->freer)
	  app_data->freer (app_data->data);
	break;
      }
  if (! app_data)
    {
      OTF_MALLOC (app_data, sizeof (OTF_ApplicationData), id);
      app_data->next = internal_data->app_data;
      internal_data->app_data = app_data;
      OTF_MALLOC (app_data->id, len, id);
      memcpy (app_data->id, id, len);
    }
  app_data->data = data;
  app_data->freer = freer;
  return 0;
}

void *
OTF_get_data (OTF *otf, char *id)
{
  OTF_InternalData *internal_data = (OTF_InternalData *) otf->internal_data;
  OTF_ApplicationData *app_data = internal_data->app_data;

  for (; app_data; app_data = app_data->next)
    if (strcmp (app_data->id, id) == 0)
      return app_data->data;
  return NULL;
}
