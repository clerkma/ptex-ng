/* otflist.c -- List OpenType fonts.

Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
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
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#include "config.h"
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <otf.h>

/* Format MSG by FMT and print the result to the stderr, and exit.  */

#define FATAL_ERROR(fmt, arg)	\
  do {				\
    fprintf (stderr, fmt, arg);	\
    exit (1);			\
  } while (0)


void
print_tag (OTF_Tag tag)
{
  char name[5];

  OTF_tag_name (tag, name);
  printf ("%s", name);
}

void
print_gsub_gpos_info (OTF *otf, char *table)
{
  int i, j;

  if (OTF_get_table (otf, table) == 0)
    {
      OTF_ScriptList *scripts;
      OTF_FeatureList *features;

      if (! strcmp (table, "GSUB"))
	scripts = &otf->gsub->ScriptList, features = &otf->gsub->FeatureList;
      else
	scripts = &otf->gpos->ScriptList, features = &otf->gpos->FeatureList;

      printf ("  %s scripts: ", table);
      for (i = 0; i < scripts->ScriptCount; i++)
	{
	  OTF_Script *script = scripts->Script + i;

	  if (i > 0)
	    printf (", ");
	  print_tag (script->ScriptTag);
	  if (script->LangSysCount > 0)
	    {
	      printf (" (");
	      for (j = 0; j < script->LangSysCount; j++)
		{
		  if (j > 0)
		    printf (", ");
		  print_tag (script->LangSysRecord[j].LangSysTag);
		}
	      printf (")");
	    }
	}
      printf ("\n");

      printf ("  %s features: ", table);
      for (i = 0; i < features->FeatureCount; i++)
	{
	  if (i > 0)
	    printf (",");
	  print_tag (features->Feature[i].FeatureTag);
	}
      printf ("\n");
    }
}
void
help_and_exit (char *prog)
{
  printf ("otflist %s\n", LIBOTF_VERSION);
  printf ("Usage: %s [-l] [-h] [DIR]\n", prog);
  printf ("List information about OpenType font files in the directory DIR.\n");
  printf ("It actually lists all fonts that can be handled by Freetype.\n");
  printf ("  -h		print this help, then exit\n");
  printf ("  -l		use a long listing mode\n");
  exit (0);
}

int
filter (const struct dirent *direntry)
{
  int len = strlen (direntry->d_name);
  const char *ext = direntry->d_name + (len - 4);

  return (len >= 5
	  && (! strncmp (ext, ".ttf", 4)
	      || ! strncmp (ext, ".TTF", 4)
	      || ! strncmp (ext, ".ttc", 4)
	      || ! strncmp (ext, ".TTC", 4)
	      || ! strncmp (ext, ".otf", 4)
	      || ! strncmp (ext, ".OTF", 4)
	      || ! strncmp (ext, ".PFA", 4)
	      || ! strncmp (ext, ".pfa", 4)
	      || ! strncmp (ext, ".PFB", 4)
	      || ! strncmp (ext, ".pfb", 4)));
}

int dir_index = 0;

#ifdef HAVE_SCANDIR

struct dirent **namelist = NULL;
int num_files = 0;

char *
next_file (char *dirname)
{
  if (dir_index == 0)
    {
#ifdef HAVE_ALPHASORT
      num_files = scandir (".", &namelist, filter, alphasort);
#else
      num_files = scandir (".", &namelist, filter, NULL);
#endif
    }
  if (dir_index == num_files)
    return NULL;
  return namelist[dir_index++]->d_name;
}

#else  /* not HAVE_SCANDIR */

DIR *dirp;

char *
next_file (char *dirname)
{
  struct dirent *dirent;

  if (dir_index == 0)
    dirp = opendir (dirname);
  while ((dirent = readdir (dirp))
	 && (strcmp (dirent->d_name, ".") == 0
	     || strcmp (dirent->d_name, "..") == 0));
  if (! dirent)
    return NULL;
  dir_index++;
  return dirent->d_name;
}

#endif /* not HAVE_SCANDIR */

int
main (int argc, char **argv)
{ 
  FT_Library ft_library;
  FT_Face face;
  char *filename;
  int long_format = 0;
  int i;

  if (FT_Init_FreeType (&ft_library))
    FATAL_ERROR ("%s\n", "!! Freetype initialization failed.");

  if (argc > 1)
    {
      if (! strcmp (argv[1], "-h") || ! strcmp (argv[1], "--help"))
	help_and_exit (argv[0]);
      if (! strcmp (argv[1], "-l"))
	long_format = 1, argc--, argv++;
    }
  if (argc == 2)
    {
      if (chdir (argv[1]) < 0)
	FATAL_ERROR ("Can't change directory to %s\n", argv[1]);
    }

  while ((filename = next_file (".")) != NULL)
    {
      if (! FT_New_Face (ft_library, filename, 0, &face))
	{
	  OTF *otf = OTF_open (filename);
	  char *name, *family = NULL, *style = NULL;

	  if (otf && OTF_get_table (otf, "name") == 0)
	    {
	      if (! (family = otf->name->name[16]))
		family = otf->name->name[1];
	      if (! (style = otf->name->name[17]))
		style = otf->name->name[2];
	    }
	  if (! family)
	    family = face->family_name;
	  if (! style)
	    style = face->style_name;

	  name = alloca (strlen (filename)
			 + strlen (family)
			 + 4);
	  sprintf (name, "%s (%s)", filename, family);
	  printf ("%-40s %s", name, style);
	  for (i = 0; i < face->num_charmaps; i++)
	    printf (" %d-%d", face->charmaps[i]->platform_id,
		    face->charmaps[i]->encoding_id);
	  printf ("\n");
	  if (otf && long_format)
	    {
	      print_gsub_gpos_info (otf, "GSUB");
	      print_gsub_gpos_info (otf, "GPOS");
	    }
	  if (otf)
	    OTF_close (otf);
	}
      else
	{
	  printf ("%s fail to open\n", filename);
	}
    }
  exit (0);
}
