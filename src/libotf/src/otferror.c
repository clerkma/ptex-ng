/* otferror.c -- Error handling.

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
#include <stdlib.h>
#include <string.h>
#include <config.h>

#include "otf.h"

static char error_message[1024];
int OTF_error;

static char *error_string[] =
  {
    "No error",
    "Memory shortage",
    "File error",
    "Invalid table",
    "CMAP drive",
    "GDEF drive",
    "GSUB drive",
    "GPOS drive"
  };

int
otf__error (int err, const char *fmt, const void *arg)
{
  sprintf (error_message, "OTF-Error (%s): ", error_string[err]);
  sprintf (error_message + strlen (error_message), fmt, arg);
  OTF_error = err;
  return 0;
}

void
OTF_perror (const char *prefix)
{
  if (OTF_error == 0)
    sprintf (error_message, "%s", error_string[0]);
  if (prefix)
    fprintf (stderr, "%s: %s\n", prefix, error_message);
  else
    fprintf (stderr, "%s\n", error_message);
}
