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

#define EXTERN extern
#include "ptex.h"

boolean check_kanji (integer c)
{
  return is_char_kanji(c);
}

boolean is_char_ascii (integer c)
{
  return (0 <= c && c < 0x100);
}

boolean is_char_kanji (integer c)
{
  return (iskanji1(Hi(c)) && iskanji2(Lo(c)));
}

boolean ismultiprn (integer c)
{
  if (iskanji1(c) || iskanji2(c))
    return true;
  return false;
}

integer calc_pos(integer c)
{
  unsigned char c1, c2;

  if (c >= 0 && c <= 255)
    return(c);

  c1 = Hi(c);
  c2 = Lo(c);

  if (iskanji1(c1))
  {
    if (is_internalSJIS())
    {
      c1 = ((c1 - 0x81) % 4) * 64;  /* c1 = 0, 64, 128, 192 */
      c2 = c2 % 64;                 /* c2 = 0..63 */
    }
    else
    {
      c1 = ((c1 - 0xa1) % 4) * 64;  /* c1 = 0, 64, 128, 192 */
      c2 = c2 % 64;                 /* c2 = 0..63 */
    }

    return (c1 + c2);              /* ret = 0..255 */
  }
  else
  {
    return (c2);
  }
}

integer kcatcodekey(integer c)
{
  return Hi(toDVI(c));
}

void init_default_kanji(const_string file_str, const_string internal_str)
{
  char *p;

  enable_UPTEX(false); /* disable */

  if (!set_enc_string(file_str, internal_str))
  {
    fprintf(stderr, "Bad kanji encoding \"%s\" or \"%s\".\n",
      file_str ? file_str : "NULL",
      internal_str ? internal_str : "NULL");
    uexit(1);
  }

  p = getenv("PTEX_KANJI_ENC");

  if (p)
  {
    if (!set_enc_string(p, NULL))
      fprintf(stderr, "Ignoring bad kanji encoding \"%s\".\n", p);
  }

#ifdef _WIN32
  p = kpse_var_value("guess_input_kanji_encoding");

  if (p)
  {
    if (*p == '1' || *p == 'y' || *p == 't')
      infile_enc_auto = 1;

    free(p);
  }
#endif
}
