/*
   Copyright 2016, 2017 Clerk Ma

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

   A simple tool to dump OpenType's features
*/

#ifdef _WIN32
  #pragma warning(disable:4819)
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <otf.h>

struct otlex { char * dict; char * mean; };

// feature tags
extern const struct otlex *
  lookup_ftag (register const char *str, register unsigned int len);
// script tags
extern const struct otlex *
  lookup_stag (register const char *str, register unsigned int len);
// language tags
extern const struct otlex *
  lookup_ltag (register const char *str, register unsigned int len);

void list_GSUB_features(OTF * ot)
{
  int si, li, fi, di;
  char facture[5];
  const struct otlex * meaning;

  for (si = 0; si < ot->gsub->ScriptList.ScriptCount; si++)
  {
    OTF_tag_name(ot->gsub->ScriptList.Script[si].ScriptTag, facture);
    meaning = lookup_stag(facture, 4);
    
    if (meaning != NULL)
      printf("script '%s' (%s):\n", facture, meaning->mean);
    else
      printf("script '%s':\n", facture);

    printf("  default features:\n");
    if (ot->gsub->ScriptList.Script[si].DefaultLangSys.FeatureCount == 0)
      printf("    (none)");
    else
      for (di = 0; di < ot->gsub->ScriptList.Script[si].DefaultLangSys.FeatureCount; di++)
      {
        unsigned idx = ot->gsub->ScriptList.Script[si].DefaultLangSys.FeatureIndex[di];
        OTF_Tag tag  = ot->gsub->FeatureList.Feature[idx].FeatureTag;
        OTF_tag_name(tag, facture);
        if (di == 0) printf("    ");
        printf("'%s', ", facture);
        if (di % 4 == 3) printf("\n    ");
      }

    printf("\n");
    
    for (li = 0; li < ot->gsub->ScriptList.Script[si].LangSysCount; li++)
    {
      OTF_tag_name(ot->gsub->ScriptList.Script[si].LangSysRecord[li].LangSysTag, facture);
      meaning = lookup_ltag(facture, 4);
      
      if (meaning != NULL)
        printf("  language '%s' (%s):\n", facture, meaning->mean);
      else
        printf("  language '%s':\n", facture);

      if (ot->gsub->ScriptList.Script[si].LangSys[li].FeatureCount == 0)
        printf("    (none)");
      else
        for (fi = 0; fi < ot->gsub->ScriptList.Script[si].LangSys[li].FeatureCount; fi++)
        {
          unsigned idx = ot->gsub->ScriptList.Script[si].LangSys[li].FeatureIndex[fi];
          OTF_Tag tag  = ot->gsub->FeatureList.Feature[idx].FeatureTag;
          OTF_tag_name(tag, facture);
          if (fi == 0) printf("    ");
          printf("'%s', ", facture);
          if (fi % 4 == 3) printf("\n    ");
        }

      printf("\n");
    }
  }
}

void list_GPOS_features(OTF * ot)
{
  int si, li, fi, di;
  char facture[5];
  const struct otlex * meaning;

  for (si = 0; si < ot->gpos->ScriptList.ScriptCount; si++)
  {
    OTF_tag_name(ot->gpos->ScriptList.Script[si].ScriptTag, facture);
    meaning = lookup_stag(facture, 4);
    
    if (meaning != NULL)
      printf("script '%s' (%s):\n", facture, meaning->mean);
    else
      printf("script '%s':\n", facture);

    printf("  default features:\n");

    if (ot->gpos->ScriptList.Script[si].DefaultLangSys.FeatureCount == 0)
      printf("    (none)");
    else
      for (di = 0; di < ot->gpos->ScriptList.Script[si].DefaultLangSys.FeatureCount; di++)
      {
        unsigned idx = ot->gpos->ScriptList.Script[si].DefaultLangSys.FeatureIndex[di];
        OTF_Tag tag  = ot->gpos->FeatureList.Feature[idx].FeatureTag;
        OTF_tag_name(tag, facture);
        if (di == 0) printf("    ");
        printf("'%s', ", facture);
        if (di % 4 == 3) printf("\n    ");
      }

    printf("\n");

    for (li = 0; li < ot->gpos->ScriptList.Script[si].LangSysCount; li++)
    {
      OTF_tag_name(ot->gpos->ScriptList.Script[si].LangSysRecord[li].LangSysTag, facture);
      meaning = lookup_ltag(facture, 4);
      
      if (meaning != NULL)
        printf("  language '%s' (%s):\n", facture, meaning->mean);
      else
        printf("  language '%s':\n", facture);

      if (ot->gpos->ScriptList.Script[si].LangSys[li].FeatureCount == 0)
        printf("    (none)");
      else
        for (fi = 0; fi < ot->gpos->ScriptList.Script[si].LangSys[li].FeatureCount; fi++)
        {
          unsigned idx = ot->gpos->ScriptList.Script[si].LangSys[li].FeatureIndex[fi];
          OTF_Tag tag  = ot->gpos->FeatureList.Feature[idx].FeatureTag;
          OTF_tag_name(tag, facture);
          if (fi == 0) printf("    ");
          printf("'%s', ", facture);
          if (fi % 4 == 3) printf("\n    ");
        }

      printf("\n");
    }
  }
}

int main (int ac, char ** av)
{
  if (ac < 2)
  {
    printf("usage: lsotfea <your OpenType file name>\n");
    return 0;
  }
  else
  {
    OTF * ot;

    ot = OTF_open(av[1]);

    if (!ot)
    {
      OTF_perror("lsotfea");
      return 0;
    }

    if (OTF_get_table(ot, "GSUB") == 0 && ot->gsub->ScriptList.ScriptCount > 0)
    {
      printf("All GSUB (Glyph Substitution) Infomation\n");
      list_GSUB_features(ot);
    }

    if (OTF_get_table(ot, "GPOS") == 0 && ot->gpos->ScriptList.ScriptCount > 0)
    {
      printf("All GPOS (Glyph Positioning) Infomation\n");
      list_GPOS_features(ot);
    }

    OTF_close(ot);
  }

  return 0;
}
