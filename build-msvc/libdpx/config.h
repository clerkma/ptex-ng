/* Define to 1 if you have the `getenv' function. */
#define HAVE_GETENV 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define if you have libpng and its headers. */
#define HAVE_LIBPNG 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/wait.h> header file. */
#define HAVE_SYS_WAIT_H 1

/* Define if <time.h> has timezone as an external variable. */
#define HAVE_TIMEZONE 1

/* Define if you have zlib and its headers. */
#define HAVE_ZLIB 1

/* Define if your zlib has the compress2 function. */
#define HAVE_ZLIB_COMPRESS2 1

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* Version number of package */
#define VERSION "20180217"

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

/* Define to 1 if <zlib.h> declares 'z_const'. */
#define ZLIB_CONST 1

/* Enable large inode numbers on Mac OS X 10.5.  */
#ifndef _DARWIN_USE_64_BIT_INODE
# define _DARWIN_USE_64_BIT_INODE 1
#endif
