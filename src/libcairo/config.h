/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to 1 if egl functions enabled. */
/* #undef CAIRO_HAS_EGL_FUNCTIONS */

/* Define to 1 if fc font enabled. */
/* #undef CAIRO_HAS_FC_FONT */

/* Define to 1 if ft font enabled. */
/* #undef CAIRO_HAS_FT_FONT */

/* Define to 1 if glx functions enabled. */
/* #undef CAIRO_HAS_GLX_FUNCTIONS */

/* Define to 1 if gobject functions enabled. */
/* #undef CAIRO_HAS_GOBJECT_FUNCTIONS */

/* Define to 1 if image surface enabled. */
#define CAIRO_HAS_IMAGE_SURFACE 1

/* Define to 1 if mime surface enabled. */
#define CAIRO_HAS_MIME_SURFACE 1

/* Define to 1 if observer surface enabled. */
#define CAIRO_HAS_OBSERVER_SURFACE 1

/* Define to 1 if pdf surface enabled. */
#define CAIRO_HAS_PDF_SURFACE 1

/* Define to 1 if png functions enabled. */
/* #undef CAIRO_HAS_PNG_FUNCTIONS */

/* Define to 1 if ps surface enabled. */
/* #undef CAIRO_HAS_PS_SURFACE */

/* Define to 1 if quartz font enabled. */
/* #undef CAIRO_HAS_QUARTZ_FONT */

/* Define to 1 if quartz surface enabled. */
/* #undef CAIRO_HAS_QUARTZ_SURFACE */

/* Define to 1 if recording surface enabled. */
#define CAIRO_HAS_RECORDING_SURFACE 1

/* Define to 1 if script surface enabled. */
/* #undef CAIRO_HAS_SCRIPT_SURFACE */

/* Define to 1 if svg surface enabled. */
/* #undef CAIRO_HAS_SVG_SURFACE */

/* Define to 1 if user font enabled. */
#define CAIRO_HAS_USER_FONT 1

/* Define to 1 if wgl functions enabled. */
/* #undef CAIRO_HAS_WGL_FUNCTIONS */

/* Define to 1 if win32 font enabled. */
/* #undef CAIRO_HAS_WIN32_FONT */

/* Define to 1 if win32 functions enabled. */
/* #undef CAIRO_HAS_WIN32_FUNCTIONS */

/* Define to 1 if win32 surface enabled. */
/* #undef CAIRO_HAS_WIN32_SURFACE */

/* Define to 1 if xcb shm functions enabled. */
/* #undef CAIRO_HAS_XCB_SHM_FUNCTIONS */

/* Define to 1 if xcb surface enabled. */
/* #undef CAIRO_HAS_XCB_SURFACE */

/* Define to 1 if xlib surface enabled. */
/* #undef CAIRO_HAS_XLIB_SURFACE */

/* Define to 1 if xlib xrender surface enabled. */
/* #undef CAIRO_HAS_XLIB_XRENDER_SURFACE */

/* Define to 1 if your system stores words within floats with the most
   significant word first */
/* #undef FLOAT_WORDS_BIGENDIAN */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/int_types.h> header file. */
/* #undef HAVE_SYS_INT_TYPES_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if the system has the type `uint128_t'. */
/* #undef HAVE_UINT128_T */

/* Define to 1 if the system has the type `uint64_t'. */
#define HAVE_UINT64_T 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if the system has the type `__uint128_t'. */
#define HAVE___UINT128_T 1

/* Name of package */
#define PACKAGE "cairo--tex-live-"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "tex-k@tug.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "cairo (TeX Live)"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "cairo (TeX Live) 1.16.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "cairo--tex-live-"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.16.0"

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Version number of package */
#define VERSION "1.16.0"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif


/* Deal with multiple architecture compiles on Mac OS X */
#ifdef __APPLE_CC__
#ifdef __BIG_ENDIAN__
#define WORDS_BIGENDIAN 1
#define FLOAT_WORDS_BIGENDIAN 1
#else
/* #undef WORDS_BIGENDIAN */
/* #undef FLOAT_WORDS_BIGENDIAN */
#endif
#endif


/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */
