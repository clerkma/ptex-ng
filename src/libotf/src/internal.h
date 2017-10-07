/* private.h -- Private header file for libotf

Copyright (C) 2009, 2010
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

#ifndef _PRIVATE_H_
#define _PRIVATE_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#define OTF_ERROR(err, arg)	\
  return (otf__error ((err), errfmt, (arg)), errret)

extern int otf__error (int err, const char *fmt, const void *arg);

#ifdef __cplusplus
}
#endif

#endif /* not _PRIVATE_H__ */
