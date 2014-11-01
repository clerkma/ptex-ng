/* This is (x)dvipdfmx, an extended version of...

    DVIPDFMx, an eXtended version of DVIPDFM by Mark A. Wicks.

    Copyright (C) 2002-2014 by Jin-Hwan Cho, Matthias Franz, and Shunsaku Hirata,
    the DVIPDFMx project team.
    
    Copyright (c) 2006 SIL. (xdvipdfmx extensions for XeTeX support)

    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "system.h"
#include "mem.h"

#include "dpxconf.h"
#include "dpxfile.h"
#include "dpxutil.h"

#include "dvi.h"

#include "pdflimits.h"
#include "pdfdoc.h"
#include "pdfdev.h"
#include "pdfparse.h"
#include "pdfencrypt.h"

#include "spc_tpic.h"
#include "specials.h"

#include "mpost.h"

#include "fontmap.h"
#include "pdffont.h"
#include "pdfximage.h"
#include "cid.h"

#include "dvipdfmx.h"
#include "tt_aux.h"

#include "error.h"

int is_xdv = 0;

#ifdef XETEX
const char *my_name = "xdvipdfmx";
#else
const char *my_name = "dvipdfmx";
#endif

int compat_mode = 0;     /* 0 = dvipdfmx, 1 = dvipdfm */

/* Page device */
double paper_width  = 595.0;
double paper_height = 842.0;
char   landscape_mode    = 0;

int always_embed = 0; /* always embed fonts, regardless of licensing flags */

char *dvi_filename = NULL, *pdf_filename = NULL;

void
error_cleanup (void)
{
  pdf_close_images();  /* delete temporary files */
  pdf_error_cleanup();
  if (pdf_filename) {
    remove(pdf_filename);
    fprintf(stderr, "\nOutput file removed.\n");
  }
}