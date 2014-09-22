/*
   Copyright 2014 Clerk Ma

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
*/

#ifndef _PTEX_NG_TEXD_H
#define _PTEX_NG_TEXD_H

/* headers and pragmas */
#ifdef _WIN32
  #pragma warning(disable:4201) // nameless struct/union
  #pragma warning(disable:4267)
  #pragma warning(disable:4996) // a function that was marked with deprecated
  #pragma warning(disable:4701) // potentially uninitialized local variable 'name' used
  #pragma warning(disable:4135) // conversion between different integral types
  #pragma warning(disable:4127) // conditional expression is constant
#endif
// for GCC
#ifdef __GNUC__
  #pragma GCC diagnostic ignored "-Wunused-result"
#endif
// for Clang
#ifdef __clang__
  #pragma clang diagnostic ignored "-Wdangling-else"
#endif
// standard C headers
#include <stdarg.h>
#include <setjmp.h>
#include <time.h>
#include <math.h>
#include <signal.h>
// TeX Live's kpathsea and ptexenc
#include <kpathsea/c-auto.h>
#include <kpathsea/c-std.h>
#include <kpathsea/c-pathmx.h>
#include <kpathsea/c-pathch.h>
#include <kpathsea/c-fopen.h>
#include <kpathsea/c-ctype.h>
#include <kpathsea/c-proto.h>
#include <kpathsea/config.h>
#include <kpathsea/getopt.h>
#include <kpathsea/lib.h>
#include <kpathsea/line.h>
#include <kpathsea/readable.h>
#include <kpathsea/types.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/variable.h>
#include <kpathsea/absolute.h>
#include <ptexenc/ptexenc.h>
#include <ptexenc/unicode.h>
#ifdef _WIN32
  #include <kpathsea/win32lib.h>
  #include <kpathsea/concatn.h>
  #include <kpathsea/knj.h>
#endif
#include "zlib.h"
// typedefs
typedef long long integer;
typedef double    glue_ratio;
typedef double    real;
typedef FILE * alpha_file;
typedef FILE * byte_file;
typedef FILE * word_file;
typedef unsigned char  ASCII_code;
typedef int32_t        KANJI_code;
typedef unsigned char  eight_bits;
typedef unsigned short sixteen_bits;
typedef integer pool_pointer;
typedef integer str_number;
typedef unsigned char packed_ASCII_code;
typedef integer scaled;
typedef integer nonnegative_integer;
typedef unsigned char small_number;

#ifdef link
  #undef link
#endif

//#define abs(x)   ((integer)(x) >= 0 ? (integer)(x) : (integer)-(x))
//#define fabs(x)  ((x) >= 0.0 ? (x) : -(x))
#define chr(x)   (x)
#define odd(x)   ((x) % 2)
#define round(x) web2c_round((double) (x))
#define decr(x)  --(x)
#define incr(x)  ++(x)
#define toint(x) ((integer) (x))

EXTERN integer web2c_round (double r);
EXTERN boolean open_input  (FILE ** f, kpse_file_format_type file_fmt, const char * fopen_mode);
EXTERN boolean open_output (FILE ** f, const char * fopen_mode);
EXTERN int check_fclose    (FILE * f);

#define show_line(str, flag) (void) fputs(str, stdout)

#define wterm(s)    (void) putc(s, stdout)
#define wlog(s)     (void) putc(s, log_file)
#define wterm_cr()  (void) putc('\n', stdout);
#define wlog_cr()   (void) putc('\n', log_file);

EXTERN boolean input_line (FILE * f);
#define input_ln(stream, flag) input_line(stream)
/* sec 0027 */
#define a_open_in(f)    open_input  (&(f), kpse_tex_format, FOPEN_R_MODE)
#define a_open_out(f)   open_output (&(f), FOPEN_W_MODE)
#define b_open_in(f)    open_input  (&(f), kpse_tfm_format, FOPEN_RBIN_MODE)
#define b_open_out(f)   open_output (&(f), FOPEN_WBIN_MODE)
#define w_open_in(f)    open_input  (&(f), kpse_fmt_format, FOPEN_RBIN_MODE)
#define w_open_out(f)   open_output (&(f), FOPEN_WBIN_MODE)
#define a_close(f)	    (void) check_fclose(f)
#define b_close         a_close
#define w_close         a_close
#define gz_w_close      gzclose

/* If we're running under Unix, use system calls instead of standard I/O
to read and write the output files; also, be able to make a core dump. */
#ifndef unix
  #define dumpcore() exit(1)
#else /* unix */
  #define dumpcore abort
#endif

#ifdef COMPACTFORMAT
EXTERN int do_dump(char * p, int item_size, int nitems, gzFile out_file);
EXTERN int do_undump(char * p, int item_size, int nitems, gzFile out_file);
#define dump_file gz_fmt_file
#else
EXTERN int do_dump(char * p, int item_size, int nitems, FILE * out_file);
EXTERN int do_undump(char * p, int item_size, int nitems, FILE * out_file);
#define dump_file fmt_file
#endif

#define dump_things(base, len)    do_dump  ((char *) &(base), sizeof (base), (int) (len), dump_file)
#define undump_things(base, len)  do_undump((char *) &(base), sizeof (base), (int) (len), dump_file)

/* Use the above for all the other dumping and undumping. */
#define generic_dump(x)   dump_things(x, 1)
#define generic_undump(x) undump_things(x, 1)

#define dump_wd     generic_dump
#define undump_wd   generic_undump
#define dump_hh     generic_dump
#define undump_hh   generic_undump
#define dump_qqqq   generic_dump
#define undump_qqqq generic_undump

#define dump_int(x)       \
do                        \
  {                       \
    integer x_val = (x);  \
    generic_dump (x_val); \
  }                       \
while (0)

#define undump_int  generic_undump

#define undump_size(arg1, arg2, arg3, arg4)                     \
do                                                              \
{                                                               \
  undump_int(x);                                                \
                                                                \
  if (x < arg1)                                                 \
    goto bad_fmt;                                               \
                                                                \
  if (x > arg2)                                                 \
  {                                                             \
    fprintf(stdout, "%s%s\n", "---! Must increase the " , arg3);\
    goto bad_fmt;                                               \
  }                                                             \
  else                                                          \
    arg4 = x;                                                   \
}                                                               \
while (0)

// pTeX-ng's macros and functions

#define XXHi(x) BYTE1(x)
#define XHi(x)  BYTE2(x)
#define Hi(x)   BYTE3(x)
#define Lo(x)   BYTE4(x)

extern boolean check_kanji  (integer c);
extern boolean is_char_ascii(integer c);
extern boolean is_char_kanji(integer c);
extern boolean ismultiprn   (integer c);
extern integer calc_pos     (integer c);
extern integer kcatcodekey  (integer c);
extern integer multilenbuffchar(integer c);
extern void init_default_kanji(const_string file_str, const_string internal_str);
#define init_kanji() init_default_kanji("utf8", "uptex")
#define nrestmultichr(x)  ( (x)!=0 ? ((x) / 8) + 2 - ((x) % 8) : -1 )
#define max_cjk_val 0x1000000

#endif
