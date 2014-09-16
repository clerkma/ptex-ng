/*
   Copyright 1992 Karl Berry
   Copyright 2007 TeX Users Group
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

/*
  meaning      structure                      TeX                 Y&Y TeX
               ----------------------------------------------------------------------
  integer      |            int            || 4: long           | 8: long long      | min_quarterword 0
               ---------------------------------------------------------------------- max_quarterword FFFF
  scaled       |            sc             || 4: long           | 8: long long      | min_halfword
               ----------------------------------------------------------------------
  glue_ratio   |            gr             || 4: float          | 8: double         |
               ----------------------------------------------------------------------
  halfword     |     lh      |     rh      || 2: unsigned short | 4: unsigned long  |
               ----------------------------------------------------------------------
  half+quarter |  b0  |  b1  |     rh      ||                                       |
               ----------------------------------------------------------------------
  quarter      |  b0  |  b1  |  b2  |  b3  || 1: unsigned char  | 2: unsigned short |
               ----------------------------------------------------------------------
*/
#ifndef _YANDYTEX_MEMORY_H
#define _YANDYTEX_MEMORY_H

typedef struct
{
#ifdef WORDS_BIGENDIAN
  halfword rh;

  union
  {
    halfword lh;

    struct
    {
      quarterword b0, b1;
    };
  };
#endif
} two_halves;

typedef struct
{
#ifdef WORDS_BIGENDIAN
  quarterword b0, b1, b2, b3;
#else
  quarterword b3, b2, b1, b0;
#endif
} four_quarters;

typedef union
{
  glue_ratio gr;
  two_halves hh;
  integer cint;
  four_quarters qqqq;
} memory_word;

#endif