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

pointer new_dir_node(pointer b, eight_bits dir)
{
  pointer p;

  if (type(b) > vlist_node)
    confusion("new_dir_node:not box");

  p = new_null_box();
  type(p) = dir_node;
  set_box_dir(p, dir);

  switch (box_dir(b))
  {
    case dir_yoko:
      switch (dir)
      {
        case dir_tate:
          {
            width(p) = height(b) + depth(b);
            depth(p) = width(b) / 2;
            height(p) = width(b) - depth(p);
          }
          break;
 
        case dir_dtou:
          {
            width(p) = height(b) + depth(b);
            depth(p) = 0;
            height(p) = width(b);
          }
          break;
 
        default:
          confusion("new_dir_node:y->?");
          break;
      }
      break;

    case dir_tate:
      switch (dir)
      {
        case dir_yoko:
          {
            width(p) = height(b) + depth(b);
            depth(p) = 0;
            height(p) = width(b);
          }
          break;

        case dir_dtou:
          {
            width(p) = width(b);
            depth(p) = height(b);
            height(p) = depth(b);
          }
          break;

        default:
          confusion("new_dir_node:t->?");
          break;
      }
      break;

    case dir_dtou:
      switch (dir)
      {
        case dir_yoko:
          {
            width(p) = height(b) + depth(b);
            depth(p) = 0;
            height(p) = width(b);
          }
          break;

        case dir_tate:
          {
            width(p) = width(b);
            depth(p) = height(b);
            height(p) = depth(b);
          }
          break;

        default:
          confusion("new_dir_node:d->?");
          break;
      }
      break;

    default:
      confusion("new_dir_node:illegal dir");
      break;
  }

  link(b) = null;
  list_ptr(p) = b;

  return p;
}

void prev_append_(pointer val)
{
  link(prev_node) = val;
  link(link(prev_node)) = tail;
  prev_node = link(prev_node);
}

/* sec 1416 */
eight_bits get_jfm_pos(KANJI_code kcode, internal_font_number f)
{
  KANJI_code jc;
  pointer sp, mp, ep;

  if (f == null_font)
  {
    return kchar_type(null_font, 0);
  }

  jc = toDVI(kcode);
  sp = 1;
  ep = font_num_ext[f] - 1;

  if ((ep >= 1) && (kchar_code(f, sp) <= jc) && (jc <= kchar_code(f, ep)))
  {
    while (sp <= ep)
    {
      mp = sp + ((ep - sp) / 2);

      if (jc < kchar_code(f, mp))
        ep = mp - 1;
      else if (jc > kchar_code(f, mp))
        sp = mp + 1;
      else
      {
        return kchar_type(f, mp);
      }
    }
  }

  return kchar_type(f, 0);
}
/* sec 1425 */
void print_kansuji(integer n)
{
  char k;
  KANJI_code cx;

  k = 0;

  if (n < 0)
    return;

  do {
    dig[k] = n % 10;
    n = n / 10;
    incr(k);
  } while (!(n == 0));

  while (k > 0)
  {
    decr(k);
    cx = kansuji_char(dig[k]);
    print_kanji(fromDVI(cx));
  }
}
/* sec 1435 */
pointer get_inhibit_pos(KANJI_code c, small_number n)
{
  pointer p, s;

  s = calc_pos(c);
  p = s;

  if (n == new_pos)
  {
    do {
      if ((inhibit_xsp_code(p) == 0) || (inhibit_xsp_code(p) == c))
        goto done;
      
      incr(p);
      
      if (p > 255)
        p = 0;
    } while (!(s == p));

    p = no_entry;
  }
  else
  {
    do {
      if (inhibit_xsp_code(p) == 0)
        goto done1;

      if (inhibit_xsp_code(p) == c)
        goto done;

      incr(p);

      if (p > 255)
        p = 0;
    } while (!(s == p));
done1:
    p = no_entry;
  }
done:
  return p;
}
/* sec 1440 */
pointer get_kinsoku_pos(KANJI_code c, small_number n)
{
  pointer p, s;

  s = calc_pos(c);
  p = s;

  if (n == new_pos)
  {
    do {
      if ((kinsoku_type(p) == 0) || (kinsoku_code(p) == c))
        goto done;

      incr(p);

      if (p > 255)
        p = 0;
    } while (!(s == p));

    p = no_entry;
  }
  else
  {
    do {
      if (kinsoku_type(p) == 0)
        goto done1;

      if (kinsoku_code(p) == c)
        goto done;

      incr(p);

      if (p > 255)
        p = 0;
    } while (!(s == p));
done1:
    p = no_entry;
  }
done:
  return p;
}
/* sec 1448 */
void pdf_synch_dir(void)
{
  scaled tmp;

  switch (cur_dir_hv)
  {
    case dir_yoko:
      if (dvi_dir != cur_dir_hv)
      {
        pdf_synch_h();
        pdf_synch_v();
        dir_used = true;

        switch (dvi_dir)
        {
          case dir_tate:
            {
              tmp = cur_h;
              cur_h = -cur_v;
              cur_v = tmp;
            }
            break;
          case dir_dtou:
            {
              tmp = cur_h;
              cur_h = cur_v;
              cur_v = -tmp;
            }
            break;
        }

        dvi_h = cur_h;
        dvi_v = cur_v;
        dvi_dir = cur_dir_hv;
      }
      break;
    case dir_tate:
      if (dvi_dir != cur_dir_hv)
      {
        pdf_synch_h();
        pdf_synch_v();
        dir_used = true;

        switch (dvi_dir)
        {
          case dir_yoko:
            {
              tmp = cur_h;
              cur_h = cur_v;
              cur_v = -tmp;
            }
            break;
          case dir_dtou:
            {
              cur_h = -cur_h;
              cur_v = -cur_v;
            }
            break;
        }

        dvi_h = cur_h;
        dvi_v = cur_v;
        dvi_dir = cur_dir_hv;
      }
      break;
    case dir_dtou:
      if (dvi_dir != cur_dir_hv)
      {
        pdf_synch_h();
        pdf_synch_v();
        dir_used = true;

        switch (dvi_dir)
        {
          case dir_yoko:
            {
              tmp = cur_h;
              cur_h = -cur_v;
              cur_v = tmp;
            }
            break;
          case dir_tate:
            {
              cur_h = -cur_h;
              cur_v = -cur_v;
            }
            break;
        }

        dvi_h = cur_h;
        dvi_v = cur_v;
        dvi_dir = cur_dir_hv;
      }
      break;
    default:
      confusion("synch_dir");
      break;
  }
}
/* sec 1450 */
boolean check_box(pointer box_p)
{
  pointer p;
  boolean flag;

  flag = false;
  p = box_p;

  while (p != null)
  {
    if (is_char_node(p))
    {
      do
      {
        if (find_first_char)
        {
          first_char = p;
          find_first_char = false;
        }

        last_char = p;
        flag = true;

        if (font_dir[font(p)] != dir_default)
          p = link(p);
          
        p = link(p);

        if (p == null)
          goto done;
      }
      while (!(!is_char_node(p)));
    }

    switch (type(p))
    {
      case hlist_node:
        {
          flag = true;

          if (shift_amount(p) == 0)
          {
            if (check_box(list_ptr(p)))
              flag = true;
          }
          else if (find_first_char)
            find_first_char = false;
          else
            last_char = null;
        }
        break;
      case ligature_node:
        if (check_box(lig_ptr(p)))
          flag = true;
        break;
      case ins_node:
      case disp_node:
      case mark_node:
      case adjust_node:
      case whatsit_node:
      case penalty_node:
        do_nothing();
        break;
      case math_node:
        if ((subtype(p) == before) || (subtype(p) == after))
        {
          if (find_first_char)
          {
            find_first_char = false;
            first_char = p;
          }

          last_char = p;
          flag = true;
        }
        else
          do_nothing();
        break;
      default:
        {
          flag = true;

          if (find_first_char)
            find_first_char = false;
          else
            last_char = null;
        }
        break;
    }

    p = link(p);
  }
done:
  return flag;
}
/* sec 1451 */
void adjust_hlist(pointer p, boolean pf)
{
  pointer q, s, t, u, v, x, z;
  halfword i, k;
  pointer a;
  int insert_skip;
  KANJI_code cx;
  ASCII_code ax;
  boolean do_ins;

  if (link(p) == null)
    goto exit;

  if (auto_spacing > 0)
  {
    delete_glue_ref(space_ptr(p));
    space_ptr(p) = kanji_skip;
    add_glue_ref(kanji_skip);
  }

  if (auto_xspacing > 0)
  {
    delete_glue_ref(xspace_ptr(p));
    xspace_ptr(p) = xkanji_skip;
    add_glue_ref(xkanji_skip);
  }

  u = space_ptr(p);
  add_glue_ref(u);
  s = xspace_ptr(p);
  add_glue_ref(s);

  if (!is_char_node(link(p)) && (type(link(p) == glue_node)) &&
    (subtype(link(p)) == jfm_skip + 1))
  {
    v = link(p);
    link(p) = link(v);
    fast_delete_glue_ref(glue_ptr(v));
    free_node(v, small_node_size);
  }

  i = 0;
  insert_skip = no_skip;
  p = link(p);
  v = p;
  q = p;

  while (p != null)
  {
    if (is_char_node(p))
    {
      do 
      {
        insert_space_around_char();
        q = p;
        p = link(p);
        incr(i);

        if ((i > 5) && pf)
        {
          if (is_char_node(v))
            if (font_dir[font(v)] != dir_default)
              v = link(v);

          v = link(v);
        }
      }
      while (!(!is_char_node(p)));
    }
    else
    {
      switch (type(p))
      {
        case hlist_node:
          insert_hbox_surround_spacing();
          break;

        case ligature_node:
          insert_ligature_surround_spacing();
          break;

        case penalty_node:
        case disp_node:
          insert_penalty_or_displace_surround_spacing();
          break;

        case kern_node:
          if (subtype(p) == explicit)
            insert_skip = no_skip;
          else if (subtype(p) == acc_kern)
          {
            if (q == p)
            {
              t = link(p);

              if (is_char_node(t))
                if (font_dir[font(t)] != dir_default)
                  t = link(t);

              p = link(link(t));

              if (font_dir[font(p)] != dir_default)
              {
                p = link(p);
                insert_skip = after_wchar;
              }
              else
                insert_skip = after_schar;
            }
            else
            {
              a = p;
              t = link(p);

              if (is_char_node(t))
                if (font_dir[font(t)] != dir_default)
                  t = link(t);

              t = link(link(t));
              link(q) = t;
              p = t;
              insert_space_around_char();
              incr(i);

              if ((i > 5) && pf)
              {
                if (is_char_node(v))
                  if (font_dir[font(v)] != dir_default)
                    v = link(v);

                v = link(v);
              }

              if (link(q) != t)
                link(link(q)) = a;
              else
                link(q) = a;
            }
          }
          break;

        case math_node:
          insert_math_surround_spacing();
          break;

        case mark_node:
        case adjust_node:
        case ins_node:
        case whatsit_node:
          do_nothing();
          break;

        default:
          insert_skip = no_skip;
          break;
      }

      q = p;
      p = link(p);
    }
  }

  if (!is_char_node(q) && (type(q) == glue_node) && (subtype(q) == jfm_skip + 1))
  {
    fast_delete_glue_ref(glue_ptr(q));
    glue_ptr(q) = zero_glue;
    add_glue_ref(zero_glue);
  }

  delete_glue_ref(u);
  delete_glue_ref(s);

  if ((v != null) && pf && (i > 5))
    make_jchr_widow_penalty_node();
exit:;
}
/* sec 1467 */
void print_dir(eight_bits dir)
{
  if (dir == dir_yoko)
    print_char('Y');
  else if (dir == dir_tate)
    print_char('T');
  else if (dir == dir_dtou)
    print_char('D');
}
void print_direction(integer d)
{
  switch (abs(d))
  {
    case dir_yoko: prints("yoko"); break;
    case dir_tate: prints("tate"); break;
    case dir_dtou: prints("dtou"); break;
  }

  if (d < 0)
    prints("(math)");

  prints(" direction");
}
/* sec 1465 */
void dir_out(void)
{
  pointer this_box;

  this_box = temp_ptr;
  temp_ptr = list_ptr(this_box);

  if ((type(temp_ptr) != hlist_node) && (type(temp_ptr) != vlist_node))
    confusion("dir_out");

  switch (box_dir(this_box))
  {
    case dir_yoko:
      switch (box_dir(temp_ptr))
      {
        case dir_tate:
          {
            cur_v = cur_v - height(this_box);
            cur_h = cur_h + depth(temp_ptr);
          }
          break;
        case dir_dtou:
          {
            cur_v = cur_v + depth(this_box);
            cur_h = cur_h + height(temp_ptr);
          }
          break;
      }
      break;
    case dir_tate:
      switch (box_dir(temp_ptr))
      {
        case dir_yoko:
          {
            cur_v = cur_v + depth(this_box);
            cur_h = cur_h + height(temp_ptr);
          }
          break;
        case dir_dtou:
          {
            cur_v = cur_v + depth(this_box) - height(temp_ptr);
            cur_h = cur_h + width(temp_ptr);
          }
          break;
      }
    case dir_dtou:
      switch (box_dir(temp_ptr))
      {
        case dir_yoko:
          {
            cur_v = cur_v - height(this_box);
            cur_h = cur_h + depth(temp_ptr);
          }
          break;
        case dir_tate:
          {
            cur_v = cur_v + depth(this_box) - height(temp_ptr);
            cur_h = cur_h + width(temp_ptr);
          }
          break;
      }
  }

  cur_dir_hv = box_dir(temp_ptr);

  if (type(temp_ptr) == vlist_node)
    vlist_out();
  else
    hlist_out();
}
/* sec 1468 */
void set_math_kchar(integer c)
{
  pointer p;

  p = new_noad();
  math_type(nucleus(p)) = math_jchar;
  inhibit_glue_flag = false;
  character(nucleus(p)) = 0;
  math_kcode(p) = c;
  fam(nucleus(p)) = cur_jfam;

  if (font_dir[fam_fnt(fam(nucleus(p)) + cur_size)] == dir_default)
  {
    print_err("Not two-byte family");
    help1("IGNORE.");
    error();
  }

  type(p) = ord_noad;
  link(tail) = p;
  tail = p;
}
/* sec 1473 */
void print_kanji(KANJI_code s)
{
  s = toBUFF(s % max_cjk_val);
  if (BYTE1(s) != 0) print_char(BYTE1(s));
  if (BYTE2(s) != 0) print_char(BYTE2(s));
  if (BYTE3(s) != 0) print_char(BYTE3(s));
  print_char(BYTE4(s));
}

integer check_kcat_code(integer ct)
{
  if (((ct >= kanji) && (enable_cjk_token == 0)) || (enable_cjk_token == 2))
    return 1;
  else
  return 0;
}

integer check_echar_range(integer c)
{
  if ((c >= 0) && (c < 256))
    return 1;
  else
    return 0;
}