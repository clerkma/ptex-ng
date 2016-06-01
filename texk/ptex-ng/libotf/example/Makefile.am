# Makefile.am -- example level Makefile for libotf.
# Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
#   National Institute of Advanced Industrial Science and Technology (AIST)
#   Registration Number H15PRO167

# This file is part of libotf.

# Libotf is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or
# (at your option) any later version.

# Libotf is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.

# You should have received a copy of the GNU Lesser General Public
# License along with this library, in a file named COPYING; if not,
# write to the Free Software Foundation, Inc., 59 Temple Place, Suite
# 330, Boston, MA 02111-1307, USA

## Process this file with Automake to create Makefile.in

bin_PROGRAMS = otfdump otflist otftobdf

if !X11_UNAVAILABLE
bin_PROGRAMS += otfview
endif

CommonLDADD = ${top_builddir}/src/libotf.la

otflist_SOURCE = otflist.c
otflist_LDADD = ${CommonLDADD}

otfdump_SOURCE = otfdump.c
otfdump_LDADD = ${CommonLDADD}

otfview_SOURCE = otfview.c
otfview_LDADD = ${CommonLDADD}
otfview_LDFLAGS = ${X_LIBS} ${X_PRE_LIBS} @X11_XT_XAW_XMU@ ${X_EXTRA_LIBS}

otftobdf_SOURCE = otftobdf.c
otftobdf_LDADD = ${CommonLDADD}
