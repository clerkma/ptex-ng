/* c-auto.h.  Generated from c-auto.in by configure.  */
/* c-auto.in.  Generated from configure.ac by autoheader.  */

/* c-auto.h: defines for kpathsea, as determined by configure.

   Copyright 1994-97, 2008-2015 Karl Berry.
   Copyright 1997-99, 2002, 2005 Olaf Weber.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, see <http://www.gnu.org/licenses/>.  */

/* Guard against double inclusion. */
#ifndef KPATHSEA_C_AUTO_H
#define KPATHSEA_C_AUTO_H

/* kpathsea: the version string. */
#define KPSEVERSION "kpathsea version 6.3.2/dev"

/* Define to 1 if the `closedir' function returns void instead of `int'. */
/* #undef CLOSEDIR_VOID */

/* Define to 1 if getcwd is implemented using fork or vfork. Let me know if
   you have to add this by hand because configure failed to detect it. */
/* #undef GETCWD_FORKS */

/* Define to 1 if you have the <assert.h> header file. */
#define HAVE_ASSERT_H 1

/* Define to 1 if you have the declaration of `isascii', and to 0 if you
   don't. */
#define HAVE_DECL_ISASCII 1

/* Define to 1 if you have the declaration of `putenv', and to 0 if you don't.
   */
#define HAVE_DECL_PUTENV 1

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <float.h> header file. */
#define HAVE_FLOAT_H 1

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
#define HAVE_FSEEKO 1

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the `getwd' function. */
#define HAVE_GETWD 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the `memcmp' function. */
#define HAVE_MEMCMP 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkstemp' function. */
/* #undef HAVE_MKSTEMP */

/* Define to 1 if you have the `mktemp' function. */
#define HAVE_MKTEMP 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define to 1 if you have the <pwd.h> header file. */
/* #undef HAVE_PWD_H */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strrchr' function. */
#define HAVE_STRRCHR 1

/* Define to 1 if `st_mtim' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_MTIM 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
/* #undef HAVE_UNISTD_H */

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* Define to 1 if you want to run mkocp if OCP file is missing, and to 0 if
   you don't */
#define MAKE_OMEGA_OCP_BY_DEFAULT 1

/* Define to 1 if you want to run mkofm if OFM file is missing, and to 0 if
   you don't */
#define MAKE_OMEGA_OFM_BY_DEFAULT 1

/* Define to 1 if you want to run mktexfmt if format file is missing, and to 0
   if you don't */
#define MAKE_TEX_FMT_BY_DEFAULT 1

/* Define to 1 if you want to run mktexmf if MF source is missing, and to 0 if
   you don't */
#define MAKE_TEX_MF_BY_DEFAULT 1

/* Define to 1 if you want to run mktexpk if PK font is missing, and to 0 if
   you don't */
#define MAKE_TEX_PK_BY_DEFAULT 1

/* Define to 1 if you want to run mktextex if TeX source is missing, and to 0
   if you don't */
#define MAKE_TEX_TEX_BY_DEFAULT 0

/* Define to 1 if you want to run mktextfm if TFM file is missing, and to 0 if
   you don't */
#define MAKE_TEX_TFM_BY_DEFAULT 1

/* Name of package */
#define KPSE_PACKAGE "kpathsea"

/* Define to the address where bug reports for this package should be sent. */
#define KPSE_PACKAGE_BUGREPORT "tex-k@tug.org"

/* Define to the full name of this package. */
#define KPSE_PACKAGE_NAME "Kpathsea"

/* Define to the full name and version of this package. */
#define KPSE_PACKAGE_STRING "Kpathsea 6.3.2/dev"

/* Define to the one symbol short name of this package. */
#define KPSE_PACKAGE_TARNAME "kpathsea"

/* Define to the home page for this package. */
#define KPSE_PACKAGE_URL ""

/* Define to the version of this package. */
#define KPSE_PACKAGE_VERSION "6.3.2/dev"

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define KPSE_VERSION "6.3.2/dev"

/* Define to 1 if we need (v)snprintf wrapper functions. */
/* #undef WRAP_SNPRINTF */

/* Enable large inode numbers on Mac OS X 10.5.  */
#ifndef _DARWIN_USE_64_BIT_INODE
# define _DARWIN_USE_64_BIT_INODE 1
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
/* #undef _LARGEFILE_SOURCE */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define for Solaris 2.5.1 so the uint64_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef were allowed, the
   #define below would cause a syntax error. */
/* #undef _UINT64_T */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to the type of a signed integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
/* #undef int64_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to the type of an unsigned integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
/* #undef uint64_t */

#endif /* !KPATHSEA_C_AUTO_H */
