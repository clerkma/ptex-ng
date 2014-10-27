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
#include "ptex-ng.h"

#undef start
#undef name

/*
#define max_fonts 300

typedef struct
{
  integer font_number_start;
  integer font_number_end;
  integer packet_start;
  integer packet_end;
} virtual_font;

typedef struct
{
  integer font_number;
  integer size;
  str_number font_name;
} vf_fnt_def;

typedef struct
{
  integer pl;
  integer cc;
  integer idx;
} vf_packet;

ASCII_code * vf_info;
vf_fnt_def * vf_fnt_info;
vf_packet  * vf_packet_info;
integer vf_info_ptr = 0;
byte_file vf_file;
virtual_font vf[font_max + 1];
int temp_byte;
int current_vf_info_size;

#define four_cases(a)       a: case a + 1: case a + 2: case a + 3
#define eight_cases(a)      four_cases(a): case four_cases(a + 4)
#define sixteen_cases(a)    eight_cases(a): case eight_cases(a + 8)
#define thirty_two_cases(a) sixteen_cases(a): case sixteen_cases(a + 16)
#define sixty_four_cases(a) thirty_two_cases(a): case thirty_two_cases(a + 32)
#define set_char_0 0
#define vf_id_byte 202
#define long_char  242

#define read_vf(a) a = getc(vf_file)

#define get_vf(a)             \
  do {                        \
    if (feof(vf_file)) a = 0; \
    else read_vf(a);          \
  } while (0)

void vf_store (integer s)
{
  integer k;

  if (vf_info_ptr + s > current_vf_info_size)
    vf_info = realloc_vf_info(10000);

  for (k = vf_info_ptr; k < vf_info_ptr + s - 1; k++)
    read_vf(vf_info[k]);

  vf_info_ptr = vf_info_ptr + s;
}

integer vf_read (integer k)
{
  integer a;

  a = getc(vf_file);

  if (k == 4)
    if (a >= 128)
      a = a - 256;

  while (k > 1)
  {
    a = 256 * a + getc(vf_file);
    decr(k);
  }

  return a;
}

four_quarters vf_read_checksum (void)
{
  four_quarters cs;

  cs.b0 = getc(vf_file);
  cs.b1 = getc(vf_file);
  cs.b2 = getc(vf_file);
  cs.b3 = getc(vf_file);
  return cs;
}

void vf_read_preamble (internal_font_number f)
{
  integer k;
  integer skip_len;
  four_quarters cs;
  scaled ds;

  read_vf(temp_byte);

  if (temp_byte != pre)
    jump_out();

  read_vf(temp_byte);

  if (temp_byte != vf_id_byte)
    jump_out();

  read_vf(temp_byte);
  skip_len = temp_byte;

  for (k = 0; k < skip_len; k++)
    read_vf(temp_byte);

  cs = vf_read_checksum();

  if ((font_check[f].b0 != cs.b0) || (font_check[f].b1 != cs.b1) ||
    (font_check[f].b2 != cs.b2) || (font_check[f].b3 != cs.b3))
    jump_out();

  ds = vf_read(4) / 020;

  if (ds != font_dsize[f])
    jump_out();
}

scaled vf_store_scaled (scaled z)
{
  eight_bits a, b, c, d;
  scaled sw;
  integer alpha;
  eight_bits beta;

  a = getc(vf_file); printf("VF: a = %d\n", a);
  b = getc(vf_file);
  c = getc(vf_file);
  d = getc(vf_file);

  alpha = 16; // 2^4

  while (z >= 040000000)
  {
    z = z / 2;
    alpha = alpha + alpha; // 2^{4+e}
  }

  beta = 256 / alpha;
  alpha = alpha * z;
  sw = (((((d * z) / 0400) + (c * z)) / 0400) + (b * z)) / beta;

  if (a == 0)
    return sw;
  else if (a == 255)
    return sw - alpha;
  else
    return 0;
}

internal_font_number vf_seek_tfm (str_number s, scaled fs)
{
  internal_font_number k;

  if (fs != 0 )
  {
    for (k = font_base + 1; k <= font_ptr; k++)
    {
      if (str_eq_str(font_name[k], s) && (font_size[k] == fs))
      {
        flush_string();
        return k;
      }
    }
  }

  return null_font;
}

void vf_read_fntdef (internal_font_number f)
{
  integer k;
  integer vf_fnt_num;
  four_quarters cs;
  scaled ds, fs;
  integer a, l;
  str_number s;
  
  vf_fnt_num = vf_read(temp_byte - fnt_def1 + 1);
  cs = vf_read_checksum();
  fs = vf_store_scaled(font_size[f]);
  ds = vf_read(4) / 020;
  a = vf_read(1);
  l = vf_read(1);

  while (a > 0)
  {
    decr(a);
    getc(vf_file);
  }

  str_room(l);

  while (l > 0)
  {
    decr(l);
    append_char(getc(vf_file));
  }

  s = make_string();
  k = vf_seek_tfm(s, fs);

  if (k == 0)
    k = read_font_info(null_cs, s, 256, fs);
}

void vf_read_packet (internal_font_number f)
{
  integer c;
  integer pl;

  if (temp_byte == long_char)
  {
    pl = vf_read(4);
    c = vf_read(4);
    vf_read(4);
  }
  else
  {
    pl = temp_byte;
    c = vf_read(1);
    vf_read(3);
  }
}

boolean read_vf_info (internal_font_number f)
{
  pack_file_name(font_name[f], 256, make_str_string(".vf"));

  if (!vf_open_in(vf_file))
  {
    return false;
  }
  else
  {
    vf_read_preamble(f);
    update_terminal();

    do {
      read_vf(temp_byte);

      if ((temp_byte > long_char) && (temp_byte != post))
        vf_read_fntdef(f);
      else
        vf_read_packet(f);
    } while (temp_byte != post);

    vf_close(vf_file);
    return true;
  }
}

void pdf_packet_out (internal_font_number f, ASCII_code c)
{
  ASCII_code op_code;
  integer packet_ptr;
  integer stack_top = 0;
  scaled save_cur_h = cur_h;
  scaled save_cur_v = cur_v;
  scaled w = 0, x = 0, y = 0, z = 0;
  scaled hstack[51];
  scaled vstack[51];
  scaled wstack[51];
  scaled xstack[51];
  scaled ystack[51];
  scaled zstack[51];
  integer i;

  switch (op_code)
  {
    case nop:
      do_nothing(); i+=1;
      break;

    case push:
      {
        hstack[stack_top] = cur_h;
        vstack[stack_top] = cur_v;
        wstack[stack_top] = w;
        xstack[stack_top] = x;
        ystack[stack_top] = y;
        zstack[stack_top] = z;
        incr(stack_top);
      }i+=1;
      break;

    case pop:
      {
        decr(stack_top);
        cur_h = hstack[stack_top];
        cur_v = vstack[stack_top];
        w = wstack[stack_top];
        x = xstack[stack_top];
        y = ystack[stack_top];
        z = zstack[stack_top];
      }i+=1;
      break;

    case set_rule:
    case put_rule:
      {
        rule_ht = 0;
        rule_wd = 0;

        if ((rule_wd > 0) && (rule_ht > 0))
        {
          pdf_rule_out(rule_wd, rule_ht);

          if (op_code == set_rule)
            cur_h = cur_h + rule_wd;
        }
      } i+=9;
      break;

    case four_cases(right1):
      cur_h = cur_h + 0;
      i+=(op_code - right1 + 2);
      break;

    case w0:
    case four_cases(w1):
      i+=(op_code - w0 + 1);
      break;

    case x0:
    case four_cases(x1):
      i += (op_code - x0 + 1);
      break;

    case y0:
    case four_cases(y1):
      i += (op_code - y0 + 1);
      break;

    case z0:
    case four_cases(z1):
      i += (op_code - z0 + 1);
      break;

    case four_cases(xxx1):
      {
      }
      break;

    case sixty_four_cases(fnt_num_0):
    case four_cases(fnt1):
      break;

    case sixty_four_cases(set_char_0):
    case sixty_four_cases(set_char_0 + 64):
    case four_cases(set1):
    case four_cases(put1):
      {
        pdf_char_out(f, c);

        if ((op_code != put1) && (op_code != put2) || (op_code != put3) || (op_code != put4))
          cur_h = cur_h + char_width(f, char_info(f, c));
      }
      break;
  }

  cur_h = save_cur_h;
  cur_v = save_cur_v;
}
*/
