/* otfdump.c -- Dump OpenType Layout Tables.

Copyright (C) 2003, 2004, 2008, 2009, 2010
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
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <libgen.h>

#include <otf.h>

/* Indented print.  */
#define IPRINT printf("\n%*s", indent * 2, ""), printf

static void
dump_tag (OTF_Tag tag)
{
  printf ("(tag \"");
  putchar (tag >> 24);
  putchar ((tag >> 16) & 0xFF);
  putchar ((tag >> 8) & 0xFF);
  putchar (tag & 0xFF);
  printf ("\" #x%04X)", tag);
}

/* HEAD */

static void
dump_offset_table (int indent, OTF_OffsetTable *table)
{
  IPRINT ("(OffsetTable");
  indent++;
  IPRINT ("(sfnt-version %d.%d)", 
	  table->sfnt_version.high, table->sfnt_version.low);
  IPRINT ("(numTables %d)", table->numTables);
  IPRINT ("(searchRange %d)", table->searchRange);
  IPRINT ("(enterSelector %d)", table->enterSelector);
  IPRINT ("(rangeShift %d))", table->rangeShift);  
}

static void
dump_table_directory (int indent, OTF_TableDirectory *table, int idx)
{
  IPRINT ("(Table %d ", idx);
  dump_tag (table->tag);
  indent++;
  IPRINT ("(checkSum %08X) (offset #x%08X) (length: #x%08X))",
	  table->checkSum, table->offset, table->length);
}



/* head */
static void
dump_head_table (int indent, OTF_head *head)
{
  IPRINT ("(head");
  indent++;
  IPRINT ("(TableVersionNumber %d.%d)",
	  head->TableVersionNumber.high, head->TableVersionNumber.low);
  IPRINT ("(fontRevision %d.%d)",
	  head->fontRevision.high, head->fontRevision.low);
  IPRINT ("(checkSumAdjustment #x%04X)", head->checkSumAdjustment);
  IPRINT ("(magicNumber #x%04X)", head->magicNumber);
  IPRINT ("(flags #x%04X)", head->flags);
  IPRINT ("(unitsPerEm %d)", head->unitsPerEm);
  printf (")");
}


/* COMMON */

static void
dump_glyph_ids (int indent, char *title, OTF_GlyphID *ids, unsigned count)
{
  IPRINT ("(%s (count %d)", title, count);
  while (count-- > 0)
    {
      printf (" #x%04X", *ids);
      ids++;
    }
  printf (")");
}

static int *
dump_coverage (int indent, char *title, OTF_Coverage *coverage)
{
  int i;
  int *char_list;

  IPRINT ("(%sCoverage (CoverageFormat %d)",
	  (title ? title : ""), coverage->CoverageFormat);
  indent++;
  if (coverage->CoverageFormat == 1)
    {
      dump_glyph_ids (indent, "GlyphArray", coverage->table.GlyphArray,
		      coverage->Count);
      char_list = malloc (sizeof (int) * (coverage->Count + 1));
      for (i = 0; i < coverage->Count; i++)
	char_list[i] = coverage->table.GlyphArray[i];
      char_list[i] = -1;
    }
  else
    {
      int n, c;

      IPRINT ("(RangeCount %d)", coverage->Count);
      indent++;
      for (i = n = 0; i < coverage->Count; i++)
	{
	  IPRINT ("(Range (%d) (Start #x%04X) (End #x%04X)", i,
		  coverage->table.RangeRecord[i].Start,
		  coverage->table.RangeRecord[i].End);
	  indent++;
	  IPRINT ("(StartCoverageIndex %d))",
		  coverage->table.RangeRecord[i].StartCoverageIndex);
	  indent--;
	  n += (coverage->table.RangeRecord[i].End
		- coverage->table.RangeRecord[i].Start + 1);
	}
      char_list = malloc (sizeof (int) * (n + 1));
      for (i = n = 0; i < coverage->Count; i++)
	for (c = coverage->table.RangeRecord[i].Start;
	     c <= coverage->table.RangeRecord[i].End;
	     c++)
	  char_list[n++] = c;
      char_list[n] = -1;
    }
  printf (")");
  return char_list;
}

static void
dump_coverage_list (int indent, char *title,
		    OTF_Coverage *coverage, unsigned num)
{
  int i;

  IPRINT ("(%s %d)", title, num);
  for (i = 0; i < num; i++)
    free (dump_coverage (indent, NULL, coverage + i));
}


static void
dump_language_system (int indent, int index, OTF_Tag tag, OTF_Offset offset,
		      OTF_LangSys *langsys)
{
  int i;

  IPRINT ("(LangSys ");
  if (index >= 0)
    printf ("(%d) ", index);
  if (tag)
    dump_tag (tag);
  else
    printf ("DefaultLangSys");
  printf (" (Offset #x%04X)", offset);
  indent++;
  IPRINT ("(LookupOrder #x%04X)", langsys->LookupOrder);
  IPRINT ("(ReqFeatureIndex %d)", langsys->ReqFeatureIndex);
  IPRINT ("(FeatureCount %d)", langsys->FeatureCount);
  if (langsys->FeatureCount)
    {
      IPRINT ("(FeatureIndex");
      for (i = 0; i < langsys->FeatureCount; i++)
	printf (" %d", langsys->FeatureIndex[i]);
      printf (")");
    }
  printf (")");
}

static void
dump_script_list (int indent, OTF_ScriptList *list)
{
  int i, j;

  IPRINT ("(ScriptList (count %d)", list->ScriptCount);
  indent++;
  for (i = 0; i < list->ScriptCount; i++)
    {
      OTF_Script *script = list->Script + i;

      IPRINT ("(Script (%d) ", i);
      dump_tag (list->Script[i].ScriptTag);
      printf (" (Offset #x%04X)", list->Script[i].offset);
      indent++;
      IPRINT ("(DefaultLangSysOffset #x%04X)",
	      script->DefaultLangSysOffset);
      if (script->DefaultLangSysOffset)
	dump_language_system (indent, -1, 0,
			      script->DefaultLangSysOffset,
			      &script->DefaultLangSys);
      IPRINT ("(LangSysCount %d)", script->LangSysCount);
      for (j = 0; j < script->LangSysCount; j++)
	dump_language_system (indent, j,
			      script->LangSysRecord[j].LangSysTag,
			      script->LangSysRecord[j].LangSys,
			      script->LangSys + j);
      printf (")");
      indent--;
    }
  printf (")");
}

static void
dump_feature_list (int indent, OTF_FeatureList *list)
{
  int i, j;
  
  IPRINT ("(FeatureList (count %d)", list->FeatureCount);
  indent++;
  for (i = 0; i < list->FeatureCount; i++)
    {
      OTF_Feature *feature = list->Feature + i;

      IPRINT ("(Feature (%d) ", i);
      dump_tag (list->Feature[i].FeatureTag);
      printf (" (Offset #x%04X)", list->Feature[i].offset);
      printf (" (LookupCount %d)", feature->LookupCount);
      if (feature->LookupCount)
	{
	  indent++;
	  IPRINT ("(LookupListIndex");
	  for (j = 0; j < feature->LookupCount; j++)
	    printf (" %d", feature->LookupListIndex[j]);
	  printf (")");
	  indent--;
	}
      printf (")");
    }
  printf (")");
}

static void
dump_class_def (int indent, char *title, OTF_ClassDef *class)
{
  IPRINT ("(%s (offset #x%04X) (ClassFormat %d)",
	  (title ? title : "ClassDef"),
	  class->offset, class->ClassFormat);
  if (class->offset)
    {
      indent++;
      if (class->ClassFormat == 1)
	{
	  IPRINT ("(StartGlyph #x%04X)", class->f.f1.StartGlyph);
	  dump_glyph_ids (indent, "ClassValueArray",
			  (OTF_GlyphID *) class->f.f1.ClassValueArray,
			  class->f.f1.GlyphCount);
	}
      else if (class->ClassFormat == 2)
	{
	  int i;

	  IPRINT ("(ClassRangeCount %d)", class->f.f2.ClassRangeCount);
	  IPRINT ("(ClassRangeRecord");
	  indent++;
	  for (i = 0; i < class->f.f2.ClassRangeCount; i++)
	    IPRINT ("((Start #x%04X) (End #x%04X) (class %d))",
		    class->f.f2.ClassRangeRecord[i].Start,
		    class->f.f2.ClassRangeRecord[i].End,
		    class->f.f2.ClassRangeRecord[i].Class);
	  printf (")");
	}
      else
	printf (" UnknownClassFormat");
    }
  printf (")");
}

static void
dump_device_table (int indent, char *title, OTF_DeviceTable *table)
{
  int i;

  if (! table->offset)
    return;
  IPRINT ("(%s (offset #x%04X)", title, table->offset);
  indent++;
  IPRINT ("(StartSize %d) (EndSize %d) (DeltaFormat %d)",
	  table->StartSize, table->EndSize, table->DeltaFormat);
  if (table->DeltaValue)
    {
      IPRINT ("(DeltaValue");
      for (i = 0; i < table->EndSize - table->StartSize + 1; i++)
	printf (" %d", table->DeltaValue[i]);
      printf (")");
    }
  printf (")");
}



static void
dump_value_record (int indent, char *title, OTF_ValueRecord *rec)
{
  IPRINT ("(%s %d %d %d %d", title,
	  rec->XPlacement, rec->YPlacement, rec->XAdvance, rec->YAdvance);
  indent++;
  if (rec->XPlaDevice.offset)
    dump_device_table (indent, "XPlaDevice", &rec->XPlaDevice);
  if (rec->YPlaDevice.offset)
    dump_device_table (indent, "YPlaDevice", &rec->YPlaDevice);
  if (rec->XAdvDevice.offset)
    dump_device_table (indent, "XAdvDevice", &rec->XAdvDevice);
  if (rec->YAdvDevice.offset)
    dump_device_table (indent, "YAdvDevice", &rec->YAdvDevice);
  printf (")");
}


static void
dump_sequence_list (int indent, OTF_Sequence *sequence, unsigned num)
{
  int i;
  IPRINT ("(SequenceCount %d)", num);

  for (i = 0; i < num; i++)
    {
      IPRINT ("(Sequence (%d) (offset #x%04X)",
	      i, sequence[i].offset);
      dump_glyph_ids (indent + 1, "Substitute", sequence[i].Substitute,
		      sequence[i].GlyphCount);
      printf (")");
    }
}

static void
dump_alternate_set_list (int indent, OTF_AlternateSet *altset, unsigned num)
{
  int i;

  IPRINT ("(AlternateSetCount %d)", num);
  for (i = 0; i < num; i++)
    {
      IPRINT ("(AlternateSet (%d) (offset #x%04X)",
	      i, altset[i].offset);
      dump_glyph_ids (indent + 1, "Alternate", altset[i].Alternate,
		      altset[i].GlyphCount);
      printf (")");
    }
}


static void
dump_ligature_set_list (int indent, int *char_list,
			OTF_LigatureSet *ligset, unsigned num)
{
  int i, j, k;

  IPRINT ("(LigSetCount %d)", num);
  for (i = 0; i < num; i++)
    {
      IPRINT ("(LigatureSet (%d) (offset #x%04X) (count %d)",
	      i, ligset[i].offset, ligset[i].LigatureCount);
      indent++;
      for (j = 0; j < ligset[i].LigatureCount; j++)
	{
	  IPRINT ("(Ligature (%d) (offset #x%04X)",
		  j, ligset[i].Ligature[j].offset);
	  indent++;
	  IPRINT ("(LigGlyph #x%04X)",
		  ligset[i].Ligature[j].LigGlyph);
	  dump_glyph_ids (indent, "Component", ligset[i].Ligature[j].Component,
			  ligset[i].Ligature[j].CompCount - 1);
	  IPRINT ("(i.e. #x%04X", char_list[i]);
	  for (k = 0; k < ligset[i].Ligature[j].CompCount - 1; k++)
	    printf (" #x%04X", ligset[i].Ligature[j].Component[k]);
	  printf (" = #x%04X)", ligset[i].Ligature[j].LigGlyph);
	  printf (")");
	  indent--;
	}
      indent--;
      printf (")");
    }
}

static void
dump_pair_set_list (int indent, unsigned count, OTF_PairSet *set)
{
  int i, j;

  for (i = 0; i < count; i++)
    {
      IPRINT ("(PairSet (%d)", i);
      indent++;
      for (j = 0; j < set[i].PairValueCount; j++)
	{
	  IPRINT ("(PairValueRecord (%d)", j);
	  indent++;
	  IPRINT ("(SecondGlyph #x%04X)",
		  set[i].PairValueRecord[j].SecondGlyph);
	  dump_value_record (indent, "Value1",
			     &set[i].PairValueRecord[j].Value1);
	  dump_value_record (indent, "Value2",
			     &set[i].PairValueRecord[j].Value2);
	  printf (")");
	  indent--;
	}
      printf (")");
      indent--;
    }
}

static void
dump_class1_record_list (int indent,
			 unsigned Class1Count, unsigned Class2Count,
			 OTF_Class1Record *rec)
{
  int i, j;

  for (i = 0; i < Class1Count; i++)
    {
      IPRINT ("(Class1Record (%d)", i);
      indent++;
      for (j = 0; j < Class2Count; j++)
	{
	  IPRINT ("(Class2Record (%d)", j);
	  indent++;
	  dump_value_record (indent, "Value1", &rec[i].Class2Record[j].Value1);
	  dump_value_record (indent, "Value2", &rec[i].Class2Record[j].Value2);
	  printf (")");
	  indent--;
	}
      printf (")");
      indent--;
    }
}

static void
dump_anchor (int indent, OTF_Anchor *anchor)
{
  if (anchor->offset == 0)
    return;
  IPRINT ("(Anchor (offset #x%04X) (AnchorFormat %d)",
	  anchor->offset, anchor->AnchorFormat);
  indent++;
  IPRINT ("(XCoordinate %d) (YCoordinate %d)",
	  anchor->XCoordinate, anchor->YCoordinate);
  if (anchor->AnchorFormat == 1)
    ;
  else if (anchor->AnchorFormat == 2)
    IPRINT ("(AnchorPoint %d)", anchor->f.f1.AnchorPoint);
  else
    {
      dump_device_table (indent, "XDeviceTable", &anchor->f.f2.XDeviceTable);
      dump_device_table (indent, "YDeviceTable", &anchor->f.f2.YDeviceTable);
    }
  printf (")");
}

static void
dump_entry_exit_list (int indent, unsigned count, OTF_EntryExitRecord *rec)
{
  int i;

  for (i = 0; i < count; i++)
    {
      IPRINT ("(EntryExitRecord (%d)", i);
      indent++;
      dump_anchor (indent, &rec[i].EntryAnchor);
      dump_anchor (indent, &rec[i].EntryAnchor);
      printf (")");
      indent--;
    }
}

static void
dump_mark_array (int indent, OTF_MarkArray *array)
{
  int i;

  IPRINT ("(MarkArray (MarkCount %d)", array->MarkCount);
  indent++;
  for (i = 0; i < array->MarkCount; i++)
    {
      IPRINT ("(MarkRecord (%d) (Class %d)", i, array->MarkRecord[i].Class);
      dump_anchor (indent + 1, &array->MarkRecord[i].MarkAnchor);
      printf (")");
    }
  printf (")");
}

static void
dump_anchor_array (int indent, unsigned ClassCount, OTF_AnchorArray *array)
{
  int i, j;

  IPRINT ("(AnchorArray (Count %d)", array->Count);
  indent++;
  for (i = 0; i < array->Count; i++)
    {
      IPRINT ("(AnchorRecord (%d) ", i);
      for (j = 0; j < ClassCount; j++)
	dump_anchor (indent + 1, array->AnchorRecord[i].Anchor + j);
      printf (")");
    }
  printf (")");
}


static void
dump_lookup_record_list (int indent, OTF_LookupRecord *rec, unsigned num)
{
  int i;

  IPRINT ("(LookupCount %d)", num);
  for (i = 0; i < num; i++)
    {
      IPRINT ("(LookupRecord (%d)", i);
      indent++;
      IPRINT ("(SequenceIndex %d)", rec[i].SequenceIndex);
      IPRINT ("(LookupListIndex %d))", rec[i].LookupListIndex);
      indent--;
    }
}


static void dump_lookup_subtable_gsub (int indent, int index, unsigned type,
				       OTF_LookupSubTableGSUB *subtable);
static void dump_lookup_subtable_gpos (int indent, int index, unsigned type,
				       OTF_LookupSubTableGPOS *subtable);


static void
dump_lookup_list (int indent, OTF_LookupList *list, int gsub)
{
  int i, j;

  IPRINT ("(LookupList (count %d)", list->LookupCount);
  indent++;
  for (i = 0; i < list->LookupCount; i++)
    {
      OTF_Lookup *lookup = list->Lookup + i;

      IPRINT ("(Lookup (%d) (Offset #x%04X)",
	      i, lookup->offset);
      printf (" (Type %d) (Flag #x%04X) (SubTableCount %d)",
	      lookup->LookupType, lookup->LookupFlag, lookup->SubTableCount);
      if (gsub)
	for (j = 0; j < lookup->SubTableCount; j++)
	  dump_lookup_subtable_gsub (indent + 1, j,
				     lookup->LookupType,
				     lookup->SubTable.gsub + j);
      else
	for (j = 0; j < lookup->SubTableCount; j++)
	  dump_lookup_subtable_gpos (indent + 1, j,
				     lookup->LookupType,
				     lookup->SubTable.gpos + j);

      printf (")");
    }
  printf (")");
}

static void
dump_rule_list (int indent, OTF_Rule *rule, int count)
{
  int i;

  IPRINT ("(RuleCount %d)", count);
  for (i = 0; i < count; i++)
    {
      IPRINT ("(Rule (%d)", i);
      indent++;
      IPRINT ("(GlyphCount %d)", rule[i].GlyphCount);
      IPRINT ("(LookupCount %d)", rule[i].LookupCount);
      dump_glyph_ids (indent, "Input", rule[i].Input, rule[i].GlyphCount - 1);
      dump_lookup_record_list (indent, rule[i].LookupRecord,
			       rule[i].LookupCount);
      printf (")");
      indent--;
    }
}

static void
dump_rule_set_list (int indent, OTF_RuleSet *set, int count)
{
  int i;

  IPRINT ("(RuleSetCount %d)", count);
  for (i = 0; i < count; i++)
    {
      IPRINT ("(RuleSet (%d)", i);
      dump_rule_list (indent + 1, set[i].Rule, set[i].RuleCount);
      printf (")");
    }
}

static void
dump_class_rule_list (int indent, OTF_ClassRule *rule, int count)
{
  int i, j;

  IPRINT ("(ClassRuleCnt %d)", count);
  for (i = 0; i < count; i++)
    {
      IPRINT ("(ClassRule (%d)", i);
      indent++;
      IPRINT ("(GlyphCount %d)", rule[i].GlyphCount);
      IPRINT ("(LookupCount %d)", rule[i].LookupCount);
      IPRINT ("(Class");
      for (j = 0; j < rule[i].GlyphCount - 1; j++)
	printf (" %d", rule[i].Class[j]);
      printf (")");
      dump_lookup_record_list (indent, rule[i].LookupRecord,
			       rule[i].LookupCount);
      printf (")");
      indent--;
    }
}

static void
dump_class_set_list (int indent, OTF_ClassSet *set, int count)
{
  int i;

  IPRINT ("(ClassSetCount %d)", count);
  for (i = 0; i < count; i++)
    if (set[i].offset)
      {
	IPRINT ("(ClassSet (%d)", i);
	dump_class_rule_list (indent + 1, set[i].ClassRule,
			      set[i].ClassRuleCnt);
	printf (")");
      }
}

static void
dump_chain_rule_list (int indent, OTF_ChainRule *rule, int count)
{
  int i;

  IPRINT ("(ChainRuleCount %d)", count);
  for (i = 0; i < count; i++)
    {
      IPRINT ("(ChainRule (%d)", i);
      dump_glyph_ids (indent + 1, "Backtrack",
		      rule[i].Backtrack, rule[i].BacktrackGlyphCount);
      dump_glyph_ids (indent + 1, "Input",
		      rule[i].Input, rule[i].InputGlyphCount - 1);
      dump_glyph_ids (indent + 1, "LookAhead",
		      rule[i].LookAhead, rule[i].LookaheadGlyphCount);
      dump_lookup_record_list (indent + 1, rule[i].LookupRecord,
			       rule[i].LookupCount);
      printf (")");
    }
}

static void
dump_chain_rule_set_list (int indent, OTF_ChainRuleSet *set, int count)
{
  int i;

  IPRINT ("(ChainRuleSetCount %d)", count);
  for (i = 0; i < count; i++)
    {
      IPRINT ("(ChainRuleSet (%d)", i);
      dump_chain_rule_list (indent + 1,
			    set[i].ChainRule, set[i].ChainRuleCount);
      printf (")");
    }
}

static void
dump_chain_class_rule_list (int indent, OTF_ChainClassRule *rule, int count)
{
  int i;

  IPRINT ("(ChainClassRuleCount %d)", count);
  for (i = 0; i < count; i++)
    {
      IPRINT ("(ChainClassRule (%d)", i);
      dump_glyph_ids (indent + 1, "Backtrack",
		      rule[i].Backtrack, rule[i].BacktrackGlyphCount);
      dump_glyph_ids (indent + 1, "Input",
		      rule[i].Input, rule[i].InputGlyphCount - 1);
      dump_glyph_ids (indent + 1, "LookAhead",
		      rule[i].LookAhead, rule[i].LookaheadGlyphCount);
      dump_lookup_record_list (indent + 1, rule[i].LookupRecord,
			       rule[i].LookupCount);
      printf (")");
    }
}

static void
dump_chain_class_set_list (int indent, OTF_ChainClassSet *set, int count)
{
  int i;

  IPRINT ("(ChainClassSetCount %d)", count);
  for (i = 0; i < count; i++)
    if (set[i].offset)
      {
	IPRINT ("(ChainClassSet (%d)", i);
	dump_chain_class_rule_list (indent + 1,
				    set[i].ChainClassRule,
				    set[i].ChainClassRuleCnt);
	printf (")");
      }
}





/* GSUB */

static void
dump_lookup_subtable_gsub (int indent, int index, unsigned type,
			   OTF_LookupSubTableGSUB *subtable)
{
  IPRINT ("(SubTable (%d) (Format %d)", index, subtable->Format);
  indent++;
  switch (type)
    {
    case 1:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  IPRINT ("(DeltaGlyhpID #x%04X)",
		  subtable->u.single1.DeltaGlyphID);
	}
      else if (subtable->Format == 2)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_glyph_ids (indent, "Substitute", subtable->u.single2.Substitute,
			  subtable->u.single2.GlyphCount);
	}
      else
	printf (" invalid");
      break;

    case 2:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_sequence_list (indent,
			      subtable->u.multiple1.Sequence,
			      subtable->u.multiple1.SequenceCount);
	}
      else
	printf (" invalid");
      break;
      
    case 3:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_alternate_set_list (indent, subtable->u.alternate1.AlternateSet,
				   subtable->u.alternate1.AlternateSetCount);
	}
      else
	printf (" invalid");
      break;

    case 4:
      if (subtable->Format == 1)
	{
	  int *char_list = dump_coverage (indent, NULL, &subtable->Coverage);
	  dump_ligature_set_list (indent, char_list,
				  subtable->u.ligature1.LigatureSet,
				  subtable->u.ligature1.LigSetCount);
	  free (char_list);
	}
      else
	printf (" invalid");
      break;

    case 5:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_rule_set_list (indent, subtable->u.context1.RuleSet,
			      subtable->u.context1.RuleSetCount); 
	}
      else if (subtable->Format == 2)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_class_def (indent, NULL, &subtable->u.context2.ClassDef);
	  dump_class_set_list (indent, subtable->u.context2.ClassSet,
			       subtable->u.context2.ClassSetCnt);
	}
      else if (subtable->Format == 3)
	{
	  dump_coverage_list (indent, "Coverage",
			      subtable->u.context3.Coverage,
			      subtable->u.context3.GlyphCount);
	  dump_lookup_record_list (indent,
				   subtable->u.context3.LookupRecord,
				   subtable->u.context3.LookupCount);
	}
      else
	printf (" invalid");
      break;

    case 6:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_chain_rule_set_list
	    (indent,
	     subtable->u.chain_context1.ChainRuleSet,
	     subtable->u.chain_context1.ChainRuleSetCount);
	}
      else if (subtable->Format == 2)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_class_def (indent, "BacktrackClassDef",
			  &subtable->u.chain_context2.BacktrackClassDef);
	  dump_class_def (indent, "InputClassDef",
			  &subtable->u.chain_context2.InputClassDef);
	  dump_class_def (indent, "LookaheadClassDef",
			  &subtable->u.chain_context2.LookaheadClassDef);
	  dump_chain_class_set_list
	    (indent,
	     subtable->u.chain_context2.ChainClassSet,
	     subtable->u.chain_context2.ChainClassSetCnt);
	}
      else if (subtable->Format == 3)
	{
	  dump_coverage_list
	    (indent, "BackTrackGlyphCount",
	     subtable->u.chain_context3.Backtrack,
	     subtable->u.chain_context3.BacktrackGlyphCount);
	  dump_coverage_list
	    (indent, "InputGlyphCount",
	     subtable->u.chain_context3.Input,
	     subtable->u.chain_context3.InputGlyphCount);
	  dump_coverage_list
	    (indent, "LookaheadGlyphCount",
	     subtable->u.chain_context3.LookAhead,
	     subtable->u.chain_context3.LookaheadGlyphCount);
	  dump_lookup_record_list
	    (indent,
	     subtable->u.chain_context3.LookupRecord,
	     subtable->u.chain_context3.LookupCount);
	}
      else
	printf (" invalid");
      break;

    case 7:
      IPRINT ("(ExtensionLookupType %d)",
	      subtable->u.extension1.ExtensionLookupType);
      IPRINT ("(ExtensionOffset %d)",
	      subtable->u.extension1.ExtensionOffset);
      dump_lookup_subtable_gsub (indent, index, 
				 subtable->u.extension1.ExtensionLookupType,
				 subtable->u.extension1.ExtensionSubtable);
      break;

    case 8:
      printf (" not-yet-supported");
      break;

    default:
      printf (" invalid");
    }
  printf (")");
}

static void
dump_gsub_table (int indent, OTF_GSUB *gsub)
{
  IPRINT ("(GSUB");
  indent++;
  IPRINT ("(Header");
  indent++;
  IPRINT ("(Version %d.%d)", gsub->Version.high, gsub->Version.low);
  IPRINT ("(ScriptList #x%04X)", gsub->ScriptList.offset);
  IPRINT ("(FeatureList #x%04X)", gsub->FeatureList.offset);
  IPRINT ("(LookupList #x%04X))", gsub->LookupList.offset);
  indent--;
  dump_script_list (indent, &gsub->ScriptList);
  dump_feature_list (indent, &gsub->FeatureList);
  dump_lookup_list (indent, &gsub->LookupList, 1);
  printf (")");
}


/* GPOS */

static void
dump_lookup_subtable_gpos (int indent, int index, unsigned type,
			   OTF_LookupSubTableGPOS *subtable)
{
  IPRINT ("(SubTable (%d) (Format %d)", index, subtable->Format);
  indent++;
  switch (type)
    {
    case 1:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  IPRINT ("(ValueFormat #x%04X)",
		  subtable->u.single1.ValueFormat);
	  dump_value_record (indent, "Value", &subtable->u.single1.Value);
	}
      else if (subtable->Format == 2)
	{
	  int i;

	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  IPRINT ("(ValueFormat #x%04X)",
		  subtable->u.single2.ValueFormat);
	  IPRINT ("(ValueCount %d)",
		  subtable->u.single2.ValueCount);
	  for (i = 0; i < subtable->u.single2.ValueCount; i++)
	    dump_value_record (indent, "Value", &subtable->u.single2.Value[i]);
	}
      else
	printf (" invalid");
      break;

    case 2:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  IPRINT ("(ValueFormat1 #x%04X)",
		  subtable->u.pair1.ValueFormat1);
	  IPRINT ("(ValueFormat2 #x%04X)",
		  subtable->u.pair1.ValueFormat2);
	  dump_pair_set_list (indent, subtable->u.pair1.PairSetCount,
			      subtable->u.pair1.PairSet);
	}
      else if (subtable->Format == 2)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  IPRINT ("(ValueFormat1 #x%04X)",
		  subtable->u.pair2.ValueFormat1);
	  IPRINT ("(ValueFormat2 #x%04X)",
		  subtable->u.pair2.ValueFormat2);
	  dump_class_def (indent, "ClassDef1",
			  &subtable->u.pair2.ClassDef1);
	  dump_class_def (indent, "ClassDef2",
			  &subtable->u.pair2.ClassDef2);
	  IPRINT ("(Class1Count %d)",
		  subtable->u.pair2.Class1Count);
	  IPRINT ("(Class2Count %d)",
		  subtable->u.pair2.Class2Count);
	  dump_class1_record_list (indent,
				   subtable->u.pair2.Class1Count,
				   subtable->u.pair2.Class2Count,
				   subtable->u.pair2.Class1Record);
	}
      else
	printf (" invalid");
      break;
      
    case 3:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_entry_exit_list (indent, subtable->u.cursive1.EntryExitCount,
				subtable->u.cursive1.EntryExitRecord);
	}
      else
	printf (" invalid");
      break;

    case 4:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, "Mark", &subtable->Coverage));
	  free (dump_coverage (indent, "Base",
			       &subtable->u.mark_base1.BaseCoverage));
	  IPRINT ("(ClassCount %d)",
		  subtable->u.mark_base1.ClassCount);
	  dump_mark_array (indent, &subtable->u.mark_base1.MarkArray);
	  dump_anchor_array (indent, subtable->u.mark_base1.ClassCount,
			   &subtable->u.mark_base1.BaseArray);
	}
      break;

    case 5:
      if (subtable->Format == 1)
	{
	  OTF_GPOS_MarkLig1 *mark_lig1 = &subtable->u.mark_lig1;
	  int i, j, k;

	  free (dump_coverage (indent, "Mark", &subtable->Coverage));
	  free (dump_coverage (indent, "Ligature",
			       &mark_lig1->LigatureCoverage));
	  IPRINT ("(ClassCount %d)", mark_lig1->ClassCount);
	  dump_mark_array (indent, &mark_lig1->MarkArray);
	  IPRINT ("(LigatureArray (%d)",
		  mark_lig1->LigatureArray.LigatureCount);
	  indent++;
	  for (i = 0; i < mark_lig1->LigatureArray.LigatureCount; i++)
	    {
	      OTF_LigatureAttach *attach
		= mark_lig1->LigatureArray.LigatureAttach + i;

	      IPRINT ("(LigatureAttach (%d)", attach->ComponentCount);
	      indent++;
	      for (j = 0; j < attach->ComponentCount; j++)
		{
		  OTF_ComponentRecord *rec = attach->ComponentRecord + j;

		  IPRINT ("(LigatureAnchor (%d)", mark_lig1->ClassCount);
		  for (k = 0; k < mark_lig1->ClassCount; k++)
		    if (rec->LigatureAnchor[k].AnchorFormat)
		      dump_anchor (indent + 1, rec->LigatureAnchor + k);
		  printf (")");
		}
	      printf (")");
	      indent--;
	    }
	  printf (")");
	}
      else
	printf (" invalid");
      break;

    case 6:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, "Mark1", &subtable->Coverage));
	  free (dump_coverage (indent, "Mark2",
			       &subtable->u.mark_mark1.Mark2Coverage));
	  IPRINT ("(ClassCount %d)",
		  subtable->u.mark_mark1.ClassCount);
	  dump_mark_array (indent, &subtable->u.mark_mark1.Mark1Array);
	  dump_anchor_array (indent, subtable->u.mark_mark1.ClassCount,
			   &subtable->u.mark_mark1.Mark2Array);
	}
      else
	printf (" invalid");
      break;

    case 7:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_rule_set_list (indent, subtable->u.context1.RuleSet,
			      subtable->u.context1.RuleSetCount); 
	}
      else if (subtable->Format == 2)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_class_def (indent, NULL, &subtable->u.context2.ClassDef);
	  dump_class_set_list (indent, subtable->u.context2.ClassSet,
			       subtable->u.context2.ClassSetCnt);
	}
      else if (subtable->Format == 3)
	{
	  dump_coverage_list (indent, "Coverage",
			      subtable->u.context3.Coverage,
			      subtable->u.context3.GlyphCount);
	  dump_lookup_record_list (indent,
				   subtable->u.context3.LookupRecord,
				   subtable->u.context3.LookupCount);
	}
      else
	printf (" invalid");
      break;

    case 8:
      if (subtable->Format == 1)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_chain_rule_set_list
	    (indent,
	     subtable->u.chain_context1.ChainRuleSet,
	     subtable->u.chain_context1.ChainRuleSetCount);
	}
      else if (subtable->Format == 2)
	{
	  free (dump_coverage (indent, NULL, &subtable->Coverage));
	  dump_class_def (indent, "BacktrackClassDef",
			  &subtable->u.chain_context2.BacktrackClassDef);
	  dump_class_def (indent, "InputClassDef",
			  &subtable->u.chain_context2.InputClassDef);
	  dump_class_def (indent, "LookaheadClassDef",
			  &subtable->u.chain_context2.LookaheadClassDef);
	  dump_chain_class_set_list
	    (indent,
	     subtable->u.chain_context2.ChainClassSet,
	     subtable->u.chain_context2.ChainClassSetCnt);
	}
      else if (subtable->Format == 3)
	{
	  dump_coverage_list
	    (indent, "BackTrackGlyphCount",
	     subtable->u.chain_context3.Backtrack,
	     subtable->u.chain_context3.BacktrackGlyphCount);
	  dump_coverage_list
	    (indent, "InputGlyphCount",
	     subtable->u.chain_context3.Input,
	     subtable->u.chain_context3.InputGlyphCount);
	  dump_coverage_list
	    (indent, "LookaheaGlyphCount",
	     subtable->u.chain_context3.LookAhead,
	     subtable->u.chain_context3.LookaheadGlyphCount);
	  dump_lookup_record_list
	    (indent,
	     subtable->u.chain_context3.LookupRecord,
	     subtable->u.chain_context3.LookupCount);
	}
      else
	printf (" invalid");
      break;

    case 9:
      if (subtable->Format == 1)
	{
	  IPRINT ("(ExtensionLookupType %d)",
		  subtable->u.extension1.ExtensionLookupType);
	  IPRINT ("(ExtensionOffset %d)",
		  subtable->u.extension1.ExtensionOffset);
	  dump_lookup_subtable_gpos
	    (indent, index, 
	     subtable->u.extension1.ExtensionLookupType,
	     subtable->u.extension1.ExtensionSubtable);
	}
      else
	printf (" invalid");
    }
  printf (")");
}


static void
dump_gpos_table (int indent, OTF_GPOS *gpos)
{
  if (! gpos)
    return;
  IPRINT ("(GPOS");
  indent++;
  IPRINT ("(Header");
  indent++;
  IPRINT ("(Version %d.%d)", gpos->Version.high, gpos->Version.low);
  IPRINT ("(ScriptList #x%04X)", gpos->ScriptList.offset);
  IPRINT ("(FeatureList #x%04X)", gpos->FeatureList.offset);
  IPRINT ("(LookupList #x%04X))", gpos->LookupList.offset);
  indent--;
  dump_script_list (indent, &gpos->ScriptList);
  dump_feature_list (indent, &gpos->FeatureList);
  dump_lookup_list (indent, &gpos->LookupList, 0);
  printf (")");
}

#if 0
static void
dump_base_table (OTF_BASE *base)
{
}

static void
dump_jstf_table (OTF_JSTF *jstf)
{
}
#endif


/* GDEF */
static void
dump_gdef_header (int indent, OTF_GDEFHeader *header)
{
  IPRINT ("(Header");
  indent++;
  IPRINT ("(Version %d.%d)",
	  header->Version.high, header->Version.low);
  IPRINT ("(GlyphClassDef #x%04X)", header->GlyphClassDef);
  IPRINT ("(AttachList #x%04X)", header->AttachList);
  IPRINT ("(LigCaretList #x%04X)", header->LigCaretList);
  IPRINT ("(MarkAttachClassDef #x%04X))",
	  header->MarkAttachClassDef);
}

static void
dump_attach_list (int indent, OTF_AttachList *list)
{
}

static void
dump_lig_caret_list (int indent, OTF_LigCaretList *list)
{
  int i, j;

  IPRINT ("(LigCaretList");
  indent++;
  free (dump_coverage (indent, NULL, &list->Coverage));
  IPRINT ("(LigGlyphCount %d)", list->LigGlyphCount);
  for (i = 0; i < list->LigGlyphCount; i++)
    {
      IPRINT ("(LigGlyph (%d) (offset #x%04X)",
	      i, list->LigGlyph[i].offset);
      indent++;
      IPRINT ("(CaretCount %d)", list->LigGlyph[i].CaretCount);
      for (j = 0; j < list->LigGlyph[i].CaretCount; j++)
	{
	  unsigned format = list->LigGlyph[i].CaretValue[j].CaretValueFormat;

	  IPRINT ("(Caret (%d) (CaretValueFormat %d)", j, format);
	  if (format == 1)
	    {
	      printf ("(Coordinate %d)",
		      list->LigGlyph[i].CaretValue[j].f.f1.Coordinate);
	    }
	  else if (format == 2)
	    {
	      printf ("(CaretValuePoint %d)",
		      list->LigGlyph[i].CaretValue[j].f.f2.CaretValuePoint);
	    }
	  else if (format == 3)
	    {
	      printf ("(Coodinate %d)",
		      list->LigGlyph[i].CaretValue[j].f.f3.Coordinate);
	      indent++;
	      dump_device_table
		(indent, "DeviceTable", 
		 &list->LigGlyph[i].CaretValue[j].f.f3.DeviceTable);
	      indent--;
	    }
	  printf (")");
	}
      printf (")");
    }
  printf (")");
}


static void
dump_gdef_table (int indent, OTF_GDEF *gdef)
{
  if (! gdef)
    return;
  IPRINT ("(GDEF");
  indent++;
  dump_gdef_header (indent, &gdef->header);
  if (gdef->header.GlyphClassDef)
    dump_class_def (indent, "GlyphClassDef", &gdef->glyph_class_def);
  if (gdef->header.AttachList)
    dump_attach_list (indent, &gdef->attach_list);
  if (gdef->header.LigCaretList)
    dump_lig_caret_list (indent, &gdef->lig_caret_list);
  if (gdef->header.MarkAttachClassDef)
    dump_class_def (indent, "MarkAttachClassDef",
		    &gdef->mark_attach_class_def);
  printf (")");
}


/* cmap */
static void
dump_cmap_table (int indent, OTF_cmap *cmap)
{
  int i;

  IPRINT ("(cmap");
  indent++;
  IPRINT ("(version %d)", cmap->version);
  IPRINT ("(numTables %d)", cmap->numTables);
  for (i = 0; i < cmap->numTables; i++)
    {
      IPRINT ("(EncodingRecord (%d) (platformID %d) (encodingID %d)",
	      i,
	      cmap->EncodingRecord[i].platformID,
	      cmap->EncodingRecord[i].encodingID);
      indent++;
      IPRINT ("(Subtable (offset #x%04X) (format %d) (length #x%04X) (language %d)",
	      cmap->EncodingRecord[i].offset,
	      cmap->EncodingRecord[i].subtable.format,
	      cmap->EncodingRecord[i].subtable.length,
	      cmap->EncodingRecord[i].subtable.language);
      indent++;
      switch (cmap->EncodingRecord[i].subtable.format)
	{
	case 0:
	  {
	    int j, k;
	    unsigned char *array
	      = cmap->EncodingRecord[i].subtable.f.f0->glyphIdArray;

	    IPRINT ("(glyphIdArray");
	    for (j = 0; j < 16; j++)
	      {
		IPRINT (" ");
		for (k = 0; k < 16; k++)
		  printf (" %3d", array[j * 16 + k]);
	      }
	    printf (")");
	  }
	  break;

	case 4:
	  {
	    OTF_EncodingSubtable4 *sub4
	      = cmap->EncodingRecord[i].subtable.f.f4;
	    int j;

	    IPRINT ("(segCountX2 %d) (searchRange %d)",
		    sub4->segCountX2, sub4->searchRange);
	    IPRINT ("(entrySelector %d) (rangeShift %d)",
		    sub4->entrySelector, sub4->rangeShift);
	    for (j = 0; j < sub4->segCountX2 / 2; j++)
	      {
		IPRINT ("(Segment (%d)", j);
		indent++;
		IPRINT ("(startCount #x%04X) (endCount #x%04X)",
			sub4->segments[j].startCount,
			sub4->segments[j].endCount);
		IPRINT ("(idDelta %d) (idRangeOffset #x%04X))",
			sub4->segments[j].idDelta,
			sub4->segments[j].idRangeOffset);
		indent--;
	      }
	    IPRINT ("(glyphIdArray");
	    for (j = 0; j < sub4->GlyphCount; j++)
	      {
		if ((j % 16) == 0)
		  IPRINT (" ");
		printf (" %3d", sub4->glyphIdArray[j]);
	      }
	    printf (")");
	  }
	  break;

	case 6:
	  {
	    OTF_EncodingSubtable6 *sub6
	      = cmap->EncodingRecord[i].subtable.f.f6;
	    int j;

	    IPRINT ("(firstCode %d) (entryCount %d)",
		    sub6->firstCode, sub6->entryCount);
	    IPRINT ("(glyphIdArray");
	    for (j = 0; j < sub6->entryCount; j++)
	      {
		if ((j % 16) == 0)
		  IPRINT (" ");
		printf (" %3d", sub6->glyphIdArray[j]);
	      }
	    printf (")");
	  }
	  break;

	case 12:
	  {
	    OTF_EncodingSubtable12 *sub12
	      = cmap->EncodingRecord[i].subtable.f.f12;
	    int j;

	    for (j = 0; j < sub12->nGroups; j++)
	      {
		IPRINT ("(Group (#x%X) (startChar #x%04X) (endChar #x%04X) (startGlyphID #x%X))",
			j,
			sub12->Groups[j].startCharCode,
			sub12->Groups[j].endCharCode,
			sub12->Groups[j].startGlyphID);
	      }
	  }
	  break;

	case 14:
	  {
	    OTF_EncodingSubtable14 *sub14
	      = cmap->EncodingRecord[i].subtable.f.f14;
	    unsigned j,k;

	    IPRINT ("(VariationSelectorRecords %d)",sub14->nRecords);
	    for (j = 0; j < sub14->nRecords; j++)
	      {
		OTF_VariationSelectorRecord *record = sub14->Records + j;
		IPRINT ("(VariationSelectorRecord (varSelector #x%x)",
			record->varSelector);
		indent += 1;
		IPRINT ("(defaultUVSOffset #x%x)",
			record->defaultUVSOffset);
		if (record->defaultUVSOffset) 
		  {
		    IPRINT ("(defaultUVS");
		    indent += 1;
		    for (k = 0 ; k < record->numUnicodeValueRanges; k++)
		      {
			OTF_UnicodeValueRange *unicodeValueRange
			  = &record->unicodeValueRanges[k];
			IPRINT("(startUnicodeValue #x%x) (additionalCount %d)",
			       unicodeValueRange->startUnicodeValue,
			       unicodeValueRange->additionalCount);
		      }
		    printf (")");
		    indent -= 1;
		  }
		IPRINT ("(nonDefaultUVSOffset #x%x)",
			record->nonDefaultUVSOffset);
		if (record->nonDefaultUVSOffset) 
		  {
		    IPRINT ("(NonDefaultUVS");
		    indent += 1;
		    for (k=0; k < record->numUVSMappings; k++)
		      {
			OTF_UVSMapping *uvsMapping
			  = &record->uvsMappings[k];
			IPRINT("(unicodeValue #x%x) (glyphID %d)",
			       uvsMapping->unicodeValue,
			       uvsMapping->glyphID);
		      }
		    printf (")");
		    indent -= 1;
		  }
		printf (")");
		indent -= 1;
	      }
	  }
	}

      indent -= 2;
      printf ("))");
    }
  printf (")");
}


/* name */
static void
dump_name_table (int indent, OTF_name *name)
{
  int i;

  IPRINT ("(name");
  indent++;
  IPRINT ("(format %d)", name->format);
  IPRINT ("(count %d)", name->count);
  IPRINT ("(stringOffset %d)", name->stringOffset);
  for (i = 0; i < name->count; i++)
    {
      OTF_NameRecord *rec = name->nameRecord + i; 

      IPRINT ("(nameRecord (%d)", i);
      indent++;
      IPRINT ("(platformID %d) (encodingID %d) (languageID %d) (nameID %d)",
	      rec->platformID, rec->encodingID, rec->languageID, rec->nameID);
      IPRINT ("(length %d) (offset #x%04X))", rec->length, rec->offset);
      indent--;
    }
  for (i = 0; i <= OTF_max_nameID; i++)
    if (name->name[i])
      IPRINT ("(nameID %d \"%s\")", i, name->name[i]);

  printf (")");
}



static void
otf_dump (OTF *otf)
{
  int i;

  printf ("(OTF");

  dump_offset_table (1, &otf->offset_table);
  for (i = 0; i < otf->offset_table.numTables; i++)
    dump_table_directory (1, otf->table_dirs + i, i);

  if (otf->head)
    dump_head_table (1, otf->head);
  if (otf->name)
    dump_name_table (1, otf->name);
  if (otf->cmap)
    dump_cmap_table (1, otf->cmap);
  if (otf->gdef)
    dump_gdef_table (1, otf->gdef);
  if (otf->gsub)
    dump_gsub_table (1, otf->gsub);
  if (otf->gpos)
    dump_gpos_table (1, otf->gpos);
#if 0
  if (otf->base)
    dump_base_table (1, otf->base);
  if (otf->jstf)
    dump_jstf_table (1, otf->jstf);
#endif
  printf (")\n");
}


int
main (int argc, char **argv)
{
  OTF *otf;

  if (argc != 2 || !strcmp (argv[1], "-h") || !strcmp (argv[1], "--help"))
    {
      fprintf (stderr, "Usage: %s OTF-FILE\n", basename (argv[0]));
      exit (argc != 2);
    }
  
  otf = OTF_open (argv[1]);
  if (! otf)
    {
      OTF_perror ("otfdump");
      exit (1);
    }
  OTF_get_table (otf, "head");
  OTF_get_table (otf, "name");
  OTF_get_table (otf, "cmap");
  OTF_get_table (otf, "GDEF");
  OTF_get_table (otf, "GSUB");
  OTF_get_table (otf, "GPOS");
#if 0
  OTF_get_table (otf, "BASE");
  OTF_get_table (otf, "JSTF");
#endif
  otf_dump (otf);
  OTF_close (otf);
  exit (0);
}
