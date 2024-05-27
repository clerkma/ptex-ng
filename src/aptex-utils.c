/*
   Copyright 2019-2024 Clerk Ma

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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#else
#include <sys/time.h>
#include <unistd.h>
#endif

#include "aptex-utils.h"
#include "md5.h"

void aptex_utils_get_seconds_and_micros (int64_t * s, int64_t * m)
{
#ifdef _WIN32
  uint64_t intervals;
  FILETIME ft;

  GetSystemTimeAsFileTime(&ft);
  intervals = ((uint64_t) ft.dwHighDateTime << 32) | ft.dwLowDateTime;
  intervals -= 116444736000000000ULL;
  *s = intervals / 10000000L;
  *m = (intervals % 10000000L) / 10;
#else
  struct timeval tv;

  gettimeofday(&tv, NULL);
  *s = tv.tv_sec;
  *m = tv.tv_usec;
#endif
}

static int32_t start_time_set = 0;
static time_t start_time = 0;

static char start_time_str[38];
static char time_str[38];

static uint32_t SOURCE_DATE_EPOCH_set = 0;
static uint32_t FORCE_SOURCE_DATE_set = 0;

/* see PDF-1.7-8.3.1, PDF-2.0-7.9.4 */
static void make_asn1_date (time_t t, char * time_str, int32_t utc)
{
  struct tm gmt, lt;
  size_t size;
  int off, off_hours, off_mins;

  if (utc)
    lt = *gmtime(&t);
  else
    lt = *localtime(&t);

  size = strftime(time_str, 30, "D:%Y%m%d%H%M%S", &lt);

  if (size == 0)
    time_str[0] = '\0';

  if (time_str[14] == '6')
  {
    time_str[14] = '5';
    time_str[15] = '9';
    time_str[16] = '\0';
  }

  gmt = *gmtime(&t);
  off = 60 * (lt.tm_hour - gmt.tm_hour) + lt.tm_min - gmt.tm_min;

  if (lt.tm_year != gmt.tm_year)
  {
    off += (lt.tm_year > gmt.tm_year) ? 1440 : -1440;
  }
  else if (lt.tm_yday != gmt.tm_yday)
  {
    off += (lt.tm_yday > gmt.tm_yday) ? 1440 : -1440;
  }

  if (off == 0)
  {
    time_str[size++] = 'Z';
    time_str[size] = 0;
  }
  else
  {
    off_hours = off / 60;
    off_mins = abs(off - off_hours * 60);
    snprintf(&time_str[size], 22, "%+03d'%02d'", off_hours, off_mins);
  }
}

void aptex_utils_init_start_time (void)
{
  char * source_date_epoch = NULL;
  char * endptr;
  unsigned long long epoch;

  if (!start_time_set)
  {
    start_time_set = 1;
    source_date_epoch = getenv("SOURCE_DATE_EPOCH");

    if (source_date_epoch)
    {
      errno = 0;
      epoch = strtoull(source_date_epoch, &endptr, 10);

      if (*endptr != '\0' || errno != 0)
      {
        fprintf(stderr, "invalid $SOURCE_DATE_EPOCH: %s", source_date_epoch);
        exit(EXIT_FAILURE);
      }

      start_time = epoch;
      SOURCE_DATE_EPOCH_set = 1;
    }
    else
    {
      start_time = time((time_t *) NULL);
    }

    if (source_date_epoch)
      make_asn1_date(start_time, start_time_str, 1);
    else
      make_asn1_date(start_time, start_time_str, 0);
  }
}

char * aptex_utils_get_creation_date(void)
{
  if (!start_time_set)
  {
    char * source_date_epoch = NULL;

    source_date_epoch = getenv("SOURCE_DATE_EPOCH");
    aptex_utils_init_start_time();

    if (source_date_epoch)
      make_asn1_date(start_time, start_time_str, 1);
    else
      make_asn1_date(start_time, start_time_str, 0);
  }

  return start_time_str;
}

char * aptex_utils_get_file_mod_date (char * file_name)
{
  struct stat file_stat;

  if (stat(file_name, &file_stat) == 0)
    make_asn1_date(file_stat.st_mtime, time_str, 0);
  else
    time_str[0] = '\0';

  return time_str;
}

char * aptex_utils_get_file_size (char * file_name)
{
  struct stat file_stat;
  char * file_size_str;

  if (stat(file_name, &file_stat) == 0)
  {
    file_size_str = calloc(sizeof(char), 20);
    snprintf(file_size_str, 20, "%lu", (long unsigned int) file_stat.st_size);
  }
  else
  {
    file_size_str = NULL;
  }  

  return file_size_str;
}

static char * gen_hex_dump (unsigned char * in, uint32_t len)
{
  char * output_buffer;
  uint32_t step;

  output_buffer = calloc(sizeof(char), len * 2 + 1);

  if (output_buffer != NULL)
  {
    for (step = 0; step < len; step++)
    {
      sprintf(&output_buffer[2 * step], "%02X", in[step]);
    }
  }

  return output_buffer;
}

char * aptex_utils_get_md5_sum (char * file_name, uint32_t file_or_str)
{
  md5_state_t state;
  md5_byte_t digest[16];

  if (file_or_str == 1)
  {
    char file_buffer[1024];
    size_t buffer_len;
    FILE * f;

    f = fopen(file_name, "rb");

    if (f == NULL)
      return NULL;
    else
    {
      md5_init(&state);

      while ((buffer_len = fread(&file_buffer, sizeof(char), 1024, f)) > 0)
        md5_append(&state, (const md5_byte_t *) file_buffer, buffer_len);

      md5_finish(&state, digest);
      fclose(f);
      return gen_hex_dump(digest, 16);
    }
  }
  else
  {
    md5_init(&state);
    md5_append(&state, (const md5_byte_t *) file_name, strlen(file_name));
    md5_finish(&state, digest);
    return gen_hex_dump(digest, 16);
  }

  return NULL;
}

char * aptex_utils_get_file_dump (char * file_name, uint32_t s, uint32_t l)
{
  struct stat file_stat;
  int readable = 0;

#ifdef _WIN32
  readable = (stat(file_name, &file_stat) == 0 && _access(file_name, 4) == 0);
#else
  readable = (stat(file_name, &file_stat) == 0 && access(file_name, R_OK) == 0);
#endif

  if (readable != 0 && (file_stat.st_size > s + l))
  {
    FILE * f;
    unsigned char * file_buffer;
    char * buffer_hex_dump;

    f = fopen(file_name, "rb");
    file_buffer = calloc(sizeof(unsigned char), l);

    if (file_buffer != NULL)
    {
      fseek(f, s, SEEK_SET);

      if (fread(file_buffer, sizeof(unsigned char), l, f) == l)
      {
        buffer_hex_dump = gen_hex_dump(file_buffer, l);
        free(file_buffer);
        fclose(f);

        return buffer_hex_dump;
      }
    }
  }

  return NULL;
}

/*
  JIS X 4051 character classes
  
  1  Opening parentheses and quotation marks
    1-1-42  0x214A U+0028 # LEFT PARENTHESIS Fullwidth: U+FF08
    1-1-46  0x214E U+005B # LEFT SQUARE BRACKET Fullwidth: U+FF3B
    1-1-48  0x2150 U+007B # LEFT CURLY BRACKET Fullwidth: U+FF5B
    1-1-44  0x214C U+3014 # LEFT TORTOISE SHELL BRACKET
    1-1-50  0x2152 U+3008 # LEFT ANGLE BRACKET
    1-1-52  0x2154 U+300A # LEFT DOUBLE ANGLE BRACKET
    1-1-54  0x2156 U+300C # LEFT CORNER BRACKET
    1-1-56  0x2158 U+300E # LEFT WHITE CORNER BRACKET
    1-1-58  0x215A U+3010 # LEFT BLACK LENTICULAR BRACKET
    1-13-64 0x2D60 U+301D # REVERSED DOUBLE PRIME QUOTATION MARK 
    1-1-38  0x2146 U+2018 # LEFT SINGLE QUOTATION MARK
    1-1-40  0x2148 U+201C # LEFT DOUBLE QUOTATION MARK
    1-2-54  0x2256 U+FF5F # FULLWIDTH LEFT WHITE PARENTHESIS 
    1-9-8   0x2928 U+00AB # LEFT-POINTING DOUBLE ANGLE QUOTATION MARK  
  2  Closing parentheses and quotation marks
    1-1-4   0x2124 U+002C # COMMA Fullwidth: U+FF0C
    1-1-43  0x214B U+0029 # RIGHT PARENTHESIS Fullwidth: U+FF09
    1-1-47  0x214F U+005D # RIGHT SQUARE BRACKET Fullwidth: U+FF3D
    1-1-49  0x2151 U+007D # RIGHT CURLY BRACKET Fullwidth: U+FF5D
    1-1-2   0x2122 U+3001 # IDEOGRAPHIC COMMA
    1-1-45  0x214D U+3015 # RIGHT TORTOISE SHELL BRACKET
    1-1-51  0x2153 U+3009 # RIGHT ANGLE BRACKET
    1-1-53  0x2155 U+300B # RIGHT DOUBLE ANGLE BRACKET
    1-1-55  0x2157 U+300D # RIGHT CORNER BRACKET
    1-1-57  0x2159 U+300F # RIGHT WHITE CORNER BRACKET
    1-1-59  0x215B U+3011 # RIGHT BLACK LENTICULAR BRACKET
    1-2-57  0x2259 U+3019 # RIGHT WHITE TORTOISE SHELL BRACKET 
    1-2-59  0x225B U+3017 # RIGHT WHITE LENTICULAR BRACKET 
    1-13-65 0x2D61 U+301F # LOW DOUBLE PRIME QUOTATION MARK 
    1-1-39  0x2147 U+2019 # RIGHT SINGLE QUOTATION MARK
    1-1-41  0x2149 U+201D # RIGHT DOUBLE QUOTATION MARK
    1-2-55  0x2257 U+FF60 # FULLWIDTH RIGHT WHITE PARENTHESIS 
    1-9-18  0x2932 U+00BB # RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK  
  3  Japanese characters prohibited from starting lines
    1-1-19  0x2133 U+30FD # KATAKANA ITERATION MARK
    1-1-20  0x2134 U+30FE # KATAKANA VOICED ITERATION MARK
    1-1-28  0x213C U+30FC # KATAKANA-HIRAGANA PROLONGED SOUND MARK
    1-5-1   0x2521 U+30A1 # KATAKANA LETTER SMALL A
    1-5-3   0x2523 U+30A3 # KATAKANA LETTER SMALL I
    1-5-5   0x2525 U+30A5 # KATAKANA LETTER SMALL U
    1-5-7   0x2527 U+30A7 # KATAKANA LETTER SMALL E
    1-5-9   0x2529 U+30A9 # KATAKANA LETTER SMALL O
    1-5-35  0x2543 U+30C3 # KATAKANA LETTER SMALL TU
    1-5-67  0x2563 U+30E3 # KATAKANA LETTER SMALL YA
    1-5-69  0x2565 U+30E5 # KATAKANA LETTER SMALL YU
    1-5-71  0x2567 U+30E7 # KATAKANA LETTER SMALL YO
    1-5-78  0x256E U+30EE # KATAKANA LETTER SMALL WA
    1-5-85  0x2575 U+30F5 # KATAKANA LETTER SMALL KA
    1-5-86  0x2576 U+30F6 # KATAKANA LETTER SMALL KE
    1-1-21  0x2135 U+309D # HIRAGANA ITERATION MARK
    1-1-22  0x2136 U+309E # HIRAGANA VOICED ITERATION MARK
    1-4-1   0x2421 U+3041 # HIRAGANA LETTER SMALL A
    1-4-3   0x2423 U+3043 # HIRAGANA LETTER SMALL I
    1-4-5   0x2425 U+3045 # HIRAGANA LETTER SMALL U
    1-4-7   0x2427 U+3047 # HIRAGANA LETTER SMALL E
    1-4-9   0x2429 U+3049 # HIRAGANA LETTER SMALL O
    1-4-35  0x2443 U+3063 # HIRAGANA LETTER SMALL TU
    1-4-67  0x2463 U+3083 # HIRAGANA LETTER SMALL YA
    1-4-69  0x2465 U+3085 # HIRAGANA LETTER SMALL YU
    1-4-71  0x2467 U+3087 # HIRAGANA LETTER SMALL YO
    1-4-78  0x246E U+308E # HIRAGANA LETTER SMALL WA
    1-4-85  0x2475 U+3095 # HIRAGANA LETTER SMALL KA 
    1-4-86  0x2476 U+3096 # HIRAGANA LETTER SMALL KE 
    1-6-78  0x266E U+31F0 # KATAKANA LETTER SMALL KU 
    1-6-79  0x266F U+31F1 # KATAKANA LETTER SMALL SI 
    1-6-80  0x2670 U+31F2 # KATAKANA LETTER SMALL SU 
    1-6-81  0x2671 U+31F3 # KATAKANA LETTER SMALL TO 
    1-6-82  0x2672 U+31F4 # KATAKANA LETTER SMALL NU 
    1-6-83  0x2673 U+31F5 # KATAKANA LETTER SMALL HA 
    1-6-84  0x2674 U+31F6 # KATAKANA LETTER SMALL HI 
    1-6-85  0x2675 U+31F7 # KATAKANA LETTER SMALL HU 
    1-6-86  0x2676 U+31F8 # KATAKANA LETTER SMALL HE 
    1-6-87  0x2677 U+31F9 # KATAKANA LETTER SMALL HO 
    1-6-89  0x2679 U+31FA # KATAKANA LETTER SMALL MU 
    1-6-90  0x267A U+31FB # KATAKANA LETTER SMALL RA 
    1-6-91  0x267B U+31FC # KATAKANA LETTER SMALL RI 
    1-6-92  0x267C U+31FD # KATAKANA LETTER SMALL RU 
    1-6-93  0x267D U+31FE # KATAKANA LETTER SMALL RE 
    1-6-94  0x267E U+31FF # KATAKANA LETTER SMALL RO 
    1-1-25  0x2139 U+3005 # IDEOGRAPHIC ITERATION MARK
    1-2-22  0x2236 U+303B # VERTICAL IDEOGRAPHIC ITERATION MARK 
  4  Hyphens and hyphen-like characters
    1-1-30  0x213E U+2010 # HYPHEN
    1-3-91  0x237B U+30A0 # KATAKANA-HIRAGANA DOUBLE HYPHEN 
    1-3-92  0x237C U+2013 # EN DASH 
    1-1-33  0x2141 U+301C # WAVE DASH Windows: U+FF5E
  5  Question and exclamation marks
    1-1-9   0x2129 U+003F # QUESTION MARK Fullwidth: U+FF1F
    1-1-10  0x212A U+0021 # EXCLAMATION MARK Fullwidth: U+FF01
    1-8-75  0x286B U+203C # DOUBLE EXCLAMATION MARK 
    1-8-76  0x286C U+2047 # DOUBLE QUESTION MARK 
    1-8-77  0x286D U+2048 # QUESTION EXCLAMATION MARK 
    1-8-78  0x286E U+2049 # EXCLAMATION QUESTION MARK 
  6  Bullets, colons, and semicolons
    1-1-6   0x2126 U+30FB # KATAKANA MIDDLE DOT
    1-1-7   0x2127 U+003A # COLON Fullwidth: U+FF1A
    1-1-8   0x2128 U+003B # SEMICOLON Fullwidth: U+FF1B
  7  Periods
    1-1-3   0x2123 U+3002 # IDEOGRAPHIC FULL STOP
    1-1-5   0x2125 U+002E # FULL STOP Fullwidth: U+FF0E
  8  Inseparable characters
    1-1-29  0x213D U+2014 # EM DASH Windows: U+2015
    1-1-36  0x2144 U+2026 # HORIZONTAL ELLIPSIS
    1-1-37  0x2145 U+2025 # TWO DOT LEADER
    1-2-19  0x2233 U+3033 # VERTICAL KANA REPEAT MARK UPPER HALF 
    1-2-20  0x2234 U+3034 # VERTICAL KANA REPEAT WITH VOICED SOUND MARK UPPER HALF 
    1-2-21  0x2235 U+3035 # VERTICAL KANA REPEAT MARK LOWER HALF 
  9  Prefixed abbreviated symbols
    1-1-79  0x216F U+00A5 # YEN SIGN Windows: U+FFE5
    1-1-82  0x2172 U+00A3 # POUND SIGN Windows: U+FFE1
    1-1-80  0x2170 U+0024 # DOLLAR SIGN Fullwidth: U+FF04
    1-1-84  0x2174 U+0023 # NUMBER SIGN Fullwidth: U+FF03
    1-9-1   0x2921 U+20AC # EURO SIGN 
    1-13-66 0x2D62 U+2116 # NUMERO SIGN 
  10 Suffixed abbreviated symbols
    1-1-75  0x216B U+00B0 # DEGREE SIGN
    1-1-81  0x2171 U+00A2 # CENT SIGN Windows: U+FFE0
    1-1-76  0x216C U+2032 # PRIME
    1-1-77  0x216D U+2033 # DOUBLE PRIME
    1-2-82  0x2272 U+212B # ANGSTROM SIGN 
    1-1-78  0x216E U+2103 # DEGREE CELSIUS
    1-3-63  0x235F U+2113 # SCRIPT SMALL L 
    1-1-83  0x2173 U+0025 # PERCENT SIGN Fullwidth: U+FF05
    1-3-62  0x235E U+33CB # SQUARE HP 
  11 Ideographic space
    1-1-1   0x2121 U+3000 # IDEOGRAPHIC SPACE
  12 Hiragana
  13 Japanese characters other than those in classes 1-12
  14 Characters used in note references
  15 Body characters of an attached sequence
  16 Body characters of an attached ruby other than a compound ruby
  17 Body characters of an attached compound ruby
  18 Characters used in numeric sequences
  19 Unit symbols
  20 Latin space
  21 Latin characters other than a space
  22 Opening parentheses for inline notes
  23 Closing parentheses for inline notes
*/

const char * aptex_unicode_version (void)
{
  return "15.0";
}

uint32_t aptex_get_jis4051_class (uint32_t codepoint)
{
  uint32_t jis4051_class = -1;

  switch (codepoint)
  {
    case 0xff08:
    case 0xff3b:
    case 0xff5b:
    case 0x3014:
    case 0x3008:
    case 0x300a:
    case 0x300c:
    case 0x300e:
    case 0x3010:
    case 0x301d:
    case 0x2018:
    case 0x201c:
    case 0xff5f:
    case 0x00ab:
      jis4051_class = 1;
      break;
    case 0xff0c:
    case 0xff09:
    case 0xff3d:
    case 0xff5d:
    case 0x3001:
    case 0x3015:
    case 0x3009:
    case 0x300b:
    case 0x300d:
    case 0x300f:
    case 0x3011:
    case 0x3019:
    case 0x3017:
    case 0x301f:
    case 0x2019:
    case 0x201d:
    case 0xff60:
    case 0x00bb:
      jis4051_class = 2;
      break;
    case 0xff1f:
    case 0xff01:
    case 0x203c:
    case 0x2047:
    case 0x2048:
    case 0x2049:
      jis4051_class = 5;
      break;
    case 0x30fb:
    case 0xff1a:
    case 0xff1b:
      jis4051_class = 6;
      break;
    case 0x3002:
    case 0xff0e:
      jis4051_class = 7;
      break;
    case 0x2014:
    case 0x2026:
    case 0x2025:
    case 0x3033:
    case 0x3034:
    case 0x3035:
      jis4051_class = 8;
      break;
    case 0x3000:
      jis4051_class = 11;
      break;
  }

  return jis4051_class;
}
