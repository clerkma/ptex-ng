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

/* sec 0581 */
void char_warning_(internal_font_number f, eight_bits c)
{
  ASCII_code l;

  if (tracing_lost_chars > 0)
  {
    if (show_missing == 0)
      begin_diagnostic();

    if (show_missing)
    {
      print_nl("! ");
      prints("Missing character: there is no ");
    }
    else
      print_nl("Missing character: there is no ");

    if ((c < ' ') || (c > '-'))
    {
      print_char('^');
      print_char('^');

      if (c < 64)
        print_char(c + 64);
      else if (c < 128)
        print_char(c - 64);
      else
      {
        print_lc_hex(c / 16);
        print_lc_hex(c % 16);
      }
    }
    else
      print(c);

    if (show_numeric)
    {
      print_char(' ');
      print_char('(');

      if (c / 100 > 0)
      {
        print_char('0' + c / 100);
        c = c - (c / 100) * 100;
        print_char('0' + c / 10);
      }
      else
      {
        c = c - (c / 100) * 100;

        if (c / 10 > 0)
          print_char('0' + c / 10);
      }

      print_char('0' + c % 10);
      print_char(')');
    }

    prints(" in font ");
    slow_print(font_name[f]);
    print_char('!');

    if (show_missing)
    {
      if (f != null_font)
        show_context();
    }

    if (show_missing == 0)
      end_diagnostic(false);

    missing_characters++;
  }
}
/* sec 0582 */
pointer new_character_(internal_font_number f, eight_bits c)
{
  pointer p;

  if (font_bc[f] <= c)
    if (font_ec[f] >= c)
      if (char_exists(char_info(f, c)))
      {
        p = get_avail();
        font(p) = f;
        character(p) = c;
        return p;
      }

  char_warning(f, c);
  return 0;
}
/* sec 0598 */
void dvi_swap (void)
{ 
  if (trace_flag)
  {
    wterm_cr();
    printf("dvi_swap() %lld", dvi_gone);
  }

  if (dvi_limit == dvi_buf_size)
  {
    write_dvi(0, half_buf - 1);
    dvi_limit = half_buf;
    dvi_offset = dvi_offset + dvi_buf_size;
    dvi_ptr = 0;
  }
  else
  {
    write_dvi(half_buf, dvi_buf_size - 1);
    dvi_limit = dvi_buf_size;
  }

  dvi_gone = dvi_gone + half_buf;
}
/* sec 0600 */
void dvi_four_(integer x)
{ 
  if (x >= 0)
    dvi_out(x / 0100000000);
  else
  {
    x = x + 010000000000;
    x = x + 010000000000;
    dvi_out((x / 0100000000) + 128);
  }

  x = x % 0100000000;
  dvi_out(x / 0200000);
  x = x % 0200000;
  dvi_out(x / 0400);
  dvi_out(x % 0400);
}
/* sec 0601 */
void dvi_pop_(integer l)
{
  if ((l == dvi_offset + dvi_ptr) && (dvi_ptr > 0))
    decr(dvi_ptr);
  else
    dvi_out(pop);
}
/* sec 0602 */
void dvi_font_def (internal_font_number f)
{
  pool_pointer k;

#ifdef INCREASEFONTS
  if (f <= 256)
  {
    dvi_out(fnt_def1);
    dvi_out(f - 1);
  }
  else
  {
    dvi_out(fnt_def2);
    dvi_out(((f - 1) >> 8));
    dvi_out(((f - 1) & 255));
  }
#else
  dvi_out(fnt_def1);
  dvi_out(f - 1);
#endif

  dvi_out(font_check[f].b0);
  dvi_out(font_check[f].b1);
  dvi_out(font_check[f].b2);
  dvi_out(font_check[f].b3);
  dvi_four(font_size[f]); 
  dvi_four(font_dsize[f]);
  dvi_out(length(font_area[f]));
  dvi_out(length(font_name[f]));

  for (k = str_start[font_area[f]]; k <= str_start[font_area[f] + 1] - 1; k++)
    dvi_out(str_pool[k]);

  for (k = str_start[font_name[f]]; k <= str_start[font_name[f] + 1] - 1; k++)
    dvi_out(str_pool[k]);
}
/* sec 0607 */
void movement (scaled w, eight_bits o)
{
  small_number mstate;
  pointer p, q;
  integer k;

  q = get_node(movement_node_size);
  width(q) = w;
  location(q) = dvi_offset + dvi_ptr;

  if (o == down1)
  {
    link(q) = down_ptr;
    down_ptr = q;
  }
  else
  {
    link(q) = right_ptr;
    right_ptr = q;
  }

  p = link(q);
  mstate = none_seen;

  while (p != 0)
  {
    if (width(p) == w)
      switch (mstate + info(p))
      {
        case none_seen + yz_OK:
        case none_seen + y_OK:
        case z_seen + yz_OK:
        case z_seen + y_OK:
          if (location(p) < dvi_gone)
            goto not_found;
          else
          {
            k = location(p) - dvi_offset;

            if (k < 0)
              k = k + dvi_buf_size;

            dvi_buf[k] = dvi_buf[k] + y1 - down1;
            info(p) = y_here;
            goto found;
          }
          break;

        case none_seen + z_OK:
        case y_seen + yz_OK:
        case y_seen + z_OK:
          if (location(p) < dvi_gone)
            goto not_found;
          else
          {
            k = location(p) - dvi_offset;

            if (k < 0)
              k = k + dvi_buf_size;

            dvi_buf[k] = dvi_buf[k] + z1 - down1;
            info(p) = z_here;
            goto found;
          }
          break;

        case none_seen + y_here:
        case none_seen + z_here:
        case y_seen + z_here:
        case z_seen + y_here:
          goto found;
          break;

        default:
          break;
      }
    else
      switch (mstate + info(p))
      {
        case none_seen + y_here:
          mstate = y_seen;
          break;

        case none_seen + z_here:
          mstate = z_seen;
          break;

        case y_seen + z_here:
        case z_seen + y_here:
          goto not_found;
          break;

        default:
          break;
      }

    p = link(p);
  }

not_found:

  info(q) = yz_OK;

  if (abs(w) >= 8388608L) /* 2^23 */
  {
    dvi_out(o + 3);
    dvi_four(w);
    return;
  }

  if (abs(w) >= 32768L)
  {
    dvi_out(o + 2);

    if (w < 0)
      w = w + 16777216L;  /* 2^24 */
    //dvi_out(w / 65536L);
    dvi_out((w >> 16));
    //w = w % 65536L;
    w = w & 65535L;
    goto lab2;
  }

  if (abs(w) >= 128)
  {
    dvi_out(o + 1);

    if (w < 0)
      w = w + 65536L;

    goto lab2;
  }

  dvi_out(o);

  if (w < 0)
    w = w + 256;

  goto lab1;

lab2:
  dvi_out(w / 256);

lab1:
  dvi_out(w % 256);
  return;

found:
  info(q) = info(p);

  if (info(q) == y_here)
  {
    dvi_out(o + y0 - down1);

    while (link(q) != p)
    {
      q = link(q);

      switch (info(q))
      {
        case yz_OK:
          info(q) = z_OK;
          break;

        case y_OK:
          info(q) = d_fixed;
          break;

        default:
          break;
      }
    }
  }
  else
  {
    dvi_out(o + z0 - down1);

    while (link(q) != p)
    {
      q = link(q);

      switch (info(q))
      {
        case yz_OK:
          info(q) = y_OK;
          break;

        case z_OK:
          info(q) = d_fixed;
          break;

        default:
          break;
      }
    }
  }
}
/* sec 0615 */
void prune_movements (integer l)
{
  pointer p;

  while (down_ptr != 0)
  {
    if (location(down_ptr) < l)
      goto done;

    p = down_ptr;
    down_ptr = link(p);
    free_node(p, movement_node_size);
  }

done:
  while (right_ptr != 0)
  {
    if (location(right_ptr) < l)
      return;

    p = right_ptr;
    right_ptr = link(p);
    free_node(p, movement_node_size);
  }
}
/* sec 1368 */
void special_out (pointer p)
{
  char old_setting;
  //pool_pointer k;

  pdf_synch_h();//synch_h();
  pdf_synch_h();//synch_v();
  old_setting = selector;
  selector = new_string;

#ifdef ALLOCATESTRING
  if (pool_ptr + 32000 > current_pool_size)
    str_pool = realloc_str_pool (increment_pool_size);

  show_token_list(link(write_tokens(p)), 0, 10000000L);
#else
  show_token_list(link(write_tokens(p)), 0, pool_size - pool_ptr);
#endif

  selector = old_setting;
  str_room(1);
  graphics_mode();
  spc_exec_special((const char *)str_pool + str_start[str_ptr], cur_length, cur_h * 0.000015202, -cur_v * 0.000015202, 1.0);
/*
  if (cur_length < 256)
  {
    dvi_out(xxx1);
    dvi_out(cur_length);
  }
  else
  {
    dvi_out(xxx4);
    dvi_four(cur_length); 
  } 

  for (k = str_start[str_ptr]; k <= pool_ptr - 1; k++)
    dvi_out(str_pool[k]);
*/
  pool_ptr = str_start[str_ptr];
}
/* sec 1370 */
void write_out (pointer p)
{
  char old_setting;
  /* integer old_mode; */
  int old_mode;
  /* small_number j; */
  int j;
  pointer q, r;

  q = get_avail();
  info(q) = right_brace_token + '}';
  r = get_avail();
  link(q) = r;
  info(r) = end_write_token;
  ins_list(q);
  begin_token_list(write_tokens(p), write_text);
  q = get_avail();
  info(q) = left_brace_token + '{';
  ins_list(q);
  old_mode = mode;
  mode = 0;
  cur_cs = write_loc;
  q = scan_toks(false, true);
  get_token();

  if (cur_tok != end_write_token)
  {
    print_err("Unbalanced write command");
    help2("On this page there's a \\write with fewer real {'s than }'s.",
        "I can't handle that very well; good luck.");
    error();

    do
      {
        get_token();
      }
    while (!(cur_tok == end_write_token));
  }

  mode = old_mode;
  end_token_list();
  old_setting = selector;
  j = write_stream(p);

  if (write_open[j])
    selector = j;
  else
  {
    if ((j == 17) && (selector == term_and_log))
      selector = log_only;

    print_nl("");
  }

  token_show(def_ref);
  print_ln();
  flush_list(def_ref);
  selector = old_setting;
}
/* sec 1373 */
void out_what (pointer p)
{
  /* small_number j; */
  int j;

  switch (subtype(p))
  {
    case open_node:
    case write_node:
    case close_node:
      if (!doing_leaders)
      {
        j = write_stream(p);

        if (subtype(p) == write_node)
          write_out(p);
        else
        {
          if (write_open[j])
            a_close(write_file[j]); 

          if (subtype(p) == close_node)
            write_open[j]= false;
          else if (j < 16)
          {
            cur_name = open_name(p);
            cur_area = open_area(p);
            cur_ext = open_ext(p); 

            if (cur_ext == 335) /* "" */
              cur_ext = 785;    /* ".tex" */

            pack_cur_name();

            while (!a_open_out(write_file[j]))
              prompt_file_name("output file name", ".tex");

            write_open[j] = true;
          }
        }
      }
      break;

    case special_node:
      special_out(p); 
      break;

    case language_node:
      do_nothing();
      break;

    default:
      {
        confusion("ext4");
        return;
      }
      break;
  }
}
/* sec 0638 */
void ship_out (pointer p)
{
  pdf_ship_out(p);
}
/* sec 0645 */
void scan_spec (group_code c, boolean three_codes)
{
  integer s;
  char spec_code;

  if (three_codes)
    s = saved(0);

  if (scan_keyword("to"))
    spec_code = exactly;
  else if (scan_keyword("spread"))
    spec_code = additional;
  else
  {
    spec_code = additional;
    cur_val = 0;
    goto found;
  }

  scan_normal_dimen();

found:
  if (three_codes)
  {
    saved(0) = s;
    incr(save_ptr);
  }

  saved(0) = spec_code;
  saved(1) = cur_val;
  save_ptr = save_ptr + 2;
  new_save_level(c);
  scan_left_brace();
}
/* sec 0649 */
pointer hpack_(pointer p, scaled w, small_number m)
{
  pointer r;
  pointer k;
  scaled disp;
  pointer q;
  scaled h, d, x;
  scaled s;
  pointer g;
  /* glue_ord o; */
  int o;
  internal_font_number f;
  four_quarters i;
  eight_bits hd;

  last_badness = 0;
  r = get_node(box_node_size);
  type(r) = hlist_node;
  subtype(r) = 0;
  shift_amount(r) = 0;
  space_ptr(r) = cur_kanji_skip;
  xspace_ptr(r) = cur_xkanji_skip;
  add_glue_ref(cur_kanji_skip);
  add_glue_ref(cur_xkanji_skip);
  k = cur_kanji_skip;
  q = r + list_offset;
  link(q) = p;
  h = 0;
  d = 0;
  x = 0;
  total_stretch[normal] = 0;
  total_shrink[normal] = 0;
  total_stretch[fil] = 0;
  total_shrink[fil] = 0;
  total_stretch[fill] = 0;
  total_shrink[fill] = 0;
  total_stretch[filll] = 0;
  total_shrink[filll] = 0;
  disp = 0;

  while (p != 0)
  {
reswitch:
    chain = false;

    while (is_char_node(p))
    {
      f = font(p);
      i = char_info(f, character(p));
      hd = height_depth(i);
      x = x + char_width(f, i);
      s = char_height(f, hd) - disp;

      if (s > h)
        h = s;

      s = char_depth(f, hd) + disp;

      if (s > d)
        d = s;

      if (font_dir[f] != dir_default)
      {
        p = link(p);

        if (chain)
        {
          x = x + width(k);
          o = stretch_order(k);
          total_stretch[o] = total_stretch[o] + stretch(k);
          o = shrink_order(k);
          total_shrink[o] = total_shrink[o] + shrink(k);
        }
        else
          chain = true;
      }
      else
        chain = false;

      p = link(p);
    }

    if (p != 0)
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case rule_node:
        case unset_node:
          {
            x = x + width(p);

            if (type(p) >= rule_node)
              s = 0 + disp;
            else
              s = shift_amount(p) + disp;

            if (height(p) - s > h)
              h = height(p) - s;

            if (depth(p) + s > d)
              d = depth(p) + s;
          }
          break;

        case ins_node:
        case mark_node:
        case adjust_node:
          if (adjust_tail != 0)
          {
            while (link(q) != p)
              q = link(q);

            if (type(p) == adjust_node)
            {
              link(adjust_tail) = adjust_ptr(p);

              while (link(adjust_tail) != 0)
                adjust_tail = link(adjust_tail);

              p = link(p);
              free_node(link(q), small_node_size);
            }
            else
            {
              link(adjust_tail) = p;
              adjust_tail = p;
              p = link(p);
            }

            link(q) = p;
            p = q;
          }
          break;

        case whatsit_node:
          break;

        case disp_node:
          disp = disp_dimen(p);
          break;

        case glue_node:
          {
            g = glue_ptr(p);
            x = x + width(g);
            o = stretch_order(g);
            total_stretch[o] = total_stretch[o] + stretch(g);
            o = shrink_order(g);
            total_shrink[o] = total_shrink[o] + shrink(g);

            if (subtype(p) >= a_leaders)
            {
              g = leader_ptr(p);

              if (height(g) > h)
                h = height(g);

              if (depth(g) > d)
                d = depth(g);
            }
          }
          break;

        case kern_node:
        case math_node:
          x = x + width(p);
          break;

        case ligature_node:
          {
            mem[lig_trick] = mem[lig_char(p)];
            link(lig_trick) = link(p);
            p = lig_trick;
            goto reswitch;
          }
          break;

        default:
          break;
      }
      p = link(p);
    }
  }

  if (adjust_tail != 0)
    link(adjust_tail) = 0;

  height(r) = h;
  depth(r) = d;

  if (m == additional)
    w = x + w;

  width(r) = w;
  x = w - x;

  if (x == 0)
  {
    glue_sign(r) = normal;
    glue_order(r) = normal;
    glue_set(r) = 0.0;
    goto exit;
  }
  else if (x > 0)
  {
    if (total_stretch[filll] != 0)
      o = filll;
    else if (total_stretch[fill] != 0)
      o = fill;
    else if (total_stretch[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = stretching;

    if (total_stretch[o] != 0)
      glue_set(r) = x / ((double) total_stretch[o]);
    else
    {
      glue_sign(r) = normal;
      glue_set(r) = 0.0;
    }

    if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(x, total_stretch[normal]);

        if (last_badness > hbadness)
        {
          print_ln();

          if (last_badness > 100)
            print_nl("Underfull");
          else
            print_nl("Loose");

          prints(" \\hbox (badness ");
          print_int(last_badness);

          if (last_badness > 100)
            underfull_hbox++;

          goto common_ending;
        }
      }

    goto exit;
  }
  else
  {
    if (total_shrink[filll] != 0)
      o = filll;
    else if (total_shrink[fill] != 0)
      o = fill;
    else if (total_shrink[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = shrinking;

    if (total_shrink[o] != 0)
      glue_set(r) = ((- (integer) x) / ((double) total_shrink[o]));
    else
    {
      glue_sign(r) = normal;
      glue_set(r) = 0.0;
    }

    if ((total_shrink[o] < - (integer) x) && (o == 0) && (list_ptr(r) != 0))
    {
      last_badness = 1000000L;
      glue_set(r) = 1.0;

      if ((- (integer) x - total_shrink[normal] > hfuzz) || (hbadness < 100))
      {
        if ((overfull_rule > 0) && (- (integer) x - total_shrink[0] > hfuzz))
        {
          while (link(q) != 0)
            q = link(q);
          
          link(q) = new_rule();
          width(link(q)) = overfull_rule;
        }
        
        print_ln();
        print_nl("Overfull \\hbox (");
        print_scaled(- (integer) x - total_shrink[normal]);
        prints("pt too wide");
        
        overfull_hbox++;
        goto common_ending;
      }
    }
    else if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(- (integer) x, total_shrink[normal]);

        if (last_badness > hbadness)
        {
          print_ln();
          print_nl("Tight \\hbox (badness ");
          print_int(last_badness);
          goto common_ending;
        }
      }

    goto exit;
  }

common_ending:

  if (output_active)
    prints(") has occurred while \\output is active");
  else
  {
    if (pack_begin_line != 0)
    {
      if (pack_begin_line > 0)
        prints(") in paragraph at lines ");
      else
        prints(") in alignment at lines ");

      print_int(abs(pack_begin_line));
      prints("--");
    }
    else
      prints(") detected at line ");

    print_int(line);
  }

  print_ln();
  font_in_short_display = null_font;
  short_display(list_ptr(r));
  print_ln();
  begin_diagnostic();
  show_box(r);
  end_diagnostic(true);

exit:
  last_disp = disp;
  return r;
}
/* sec 0668 */
pointer vpackage_(pointer p, scaled h, small_number m, scaled l)
{
  pointer r;
  scaled w, d, x;
  scaled s;
  pointer g;
  /* glue_ord o; */
  int o;

  last_badness = 0;
  r = get_node(box_node_size);
  type(r) = vlist_node;
  subtype(r) = min_quarterword;
  shift_amount(r) = 0;
  space_ptr(r) = zero_glue;
  xspace_ptr(r) = zero_glue;
  add_glue_ref(zero_glue);
  add_glue_ref(zero_glue);
  list_ptr(r) = p;
  w = 0;
  d = 0;
  x = 0;
  total_stretch[normal] = 0;
  total_shrink[normal] = 0;
  total_stretch[fil] = 0;
  total_shrink[fil] = 0;
  total_stretch[fill] = 0;
  total_shrink[fill] = 0;
  total_stretch[filll] = 0;
  total_shrink[filll] = 0;

  while (p != 0)
  {
    if (is_char_node(p))
    {
      confusion("vpack");
      return 0;
    }
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case rule_node:
      case unset_node:
        {
          x = x + d + height(p);
          d = depth(p);

          if (type(p) >= rule_node)
            s = 0;
          else
            s = shift_amount(p);

          if (width(p) + s > w)
            w = width(p) + s;
        }
        break;

      case whatsit_node:
        break;

      case glue_node:
        {
          x = x + d;
          d = 0;
          g = glue_ptr(p);
          x = x + width(g);
          o = stretch_order(g);
          total_stretch[o] = total_stretch[o] + stretch(g);
          o = shrink_order(g);
          total_shrink[o] = total_shrink[o] + shrink(g);

          if (subtype(p) >= a_leaders)
          {
            g = leader_ptr(p);

            if (width(g) > w)
              w = width(g);
          }
        }
        break;

      case kern_node:
        {
          x = x + d + width(p);
          d = 0;
        }
        break;

      default:
        break;
    }

    p = link(p);
  }

  width(r) = w;

  if (d > l)
  {
    x = x + d - l;
    depth(r) = l;
  }
  else
    depth(r) = d;

  if (m == additional)
    h = x + h;

  height(r) = h;
  x = h - x;

  if (x == 0)
  {
    glue_sign(r) = normal;
    glue_order(r) = normal;
    glue_set(r) = 0.0;
    goto exit;
  }
  else if (x > 0)
  {
    if (total_stretch[filll] != 0)
      o = filll;
    else if (total_stretch[fill] != 0)
      o = fill;
    else if (total_stretch[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = stretching;

    if (total_stretch[o] != 0)
      glue_set(r) = x / ((double) total_stretch[o]);
    else
    {
      glue_sign(r) = normal;
      glue_set(r) = 0.0;
    }

    if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(x, total_stretch[normal]);

        if (last_badness > vbadness)
        {
          print_ln();

          if (last_badness > 100)
            print_nl("Underfull");
          else
            print_nl("Loose");

          prints(" \\vbox (badness ");
          print_int(last_badness);

          if (last_badness > 100)
            underfull_vbox++;

          goto common_ending;
        }
      }

    goto exit;
  }
  else
  {
    if (total_shrink[filll] != 0)
      o = filll;
    else if (total_shrink[fill] != 0)
      o = fill;
    else if (total_shrink[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = shrinking;

    if (total_shrink[o] != 0)
      glue_set(r) = (- (integer) x) / ((double) total_shrink[o]);
    else
    {
      glue_sign(r) = normal;
      glue_set(r) = 0.0;
    }

    if ((total_shrink[o] < - (integer) x) && (o == 0) && (list_ptr(r) != 0))
    {
      last_badness = 1000000L;
      glue_set(r) = 1.0;

      if ((- (integer) x - total_shrink[0] > vfuzz) || (vbadness < 100))
      {
        print_ln();
        print_nl("Overfull \\vbox (");
        print_scaled(- (integer) x - total_shrink[0]);
        prints("pt too high");

        overfull_vbox++;

        goto common_ending;
      }
    }
    else if (o == 0)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(- (integer) x, total_shrink[normal]);
        if (last_badness > vbadness)
        {
          print_ln();
          print_nl("Tight \\vbox (badness ");
          print_int(last_badness);
          goto common_ending;
        }
      }

    goto exit;
  }

common_ending:

  if (output_active)
    prints(") has occurred while \\output is active");
  else
  {
    if (pack_begin_line != 0)
    {
      prints(") in alignment at lines ");
      print_int(abs(pack_begin_line));
      prints("--");
    }
    else
      prints(") detected at line ");

    print_int(line);
    print_ln();
  }

  begin_diagnostic();
  show_box(r);
  end_diagnostic(true);

exit:
  return r;
}
/* sec 0679 */
void append_to_vlist (pointer b)
{
  scaled d;
  pointer p;

  if (prev_depth > ignore_depth)
  {
    d = width(baseline_skip) - prev_depth - height(b);

    if (d < line_skip_limit)
      p = new_param_glue(line_skip_code);
    else
    {
      p = new_skip_param(baseline_skip_code);
      width(temp_ptr) = d;
    }

    link(tail) = p;
    tail = p;
  }

  link(tail) = b;
  tail = b;
  prev_depth = depth(b);
}
/* sec 0686 */
pointer new_noad (void)
{
  pointer p;

  p = get_node(noad_size);
  type(p) = ord_noad;
  subtype(p) = normal;
  mem[nucleus(p)].hh = empty_field;
  mem[subscr(p)].hh = empty_field;
  mem[supscr(p)].hh = empty_field;
  mem[kcode_noad(p)].hh = empty_field;

  return p;
}
/* sec 0688 */
pointer new_style (small_number s)
{
  pointer p;

  p = get_node(style_node_size);
  type(p) = style_node;
  subtype(p) = s;
  width(p) = 0;
  depth(p) = 0;

  return p;
}
/* sec 0689 */
pointer new_choice (void)
{
  pointer p;

  p = get_node(style_node_size);
  type(p) = choice_node;
  subtype(p) = 0;
  display_mlist(p) = 0;
  text_mlist(p) = 0;
  script_mlist(p) = 0;
  script_script_mlist(p) = 0;

  return p;
}
/* sec 0693 */
void show_info (void)
{
  show_node_list(info(temp_ptr));
}
/* sec 0704 */
pointer fraction_rule (scaled t)
{
  pointer p;

  p = new_rule();
  height(p) = t;
  depth(p) = 0;

  return p;
}
/* sec 0705 */
pointer overbar (pointer b, scaled k, scaled t)
{
  pointer p, q;

  p = new_kern(k);
  link(p) = b;
  q = fraction_rule(t);
  link(q) = p;
  p = new_kern(t);
  link(p) = q;

  return vpackage(p, 0, 1, max_dimen);
}
/* sec 0709 */
pointer char_box (internal_font_number f, quarterword c)
{
  four_quarters q;
  eight_bits hd;
  pointer b, p;

  q = char_info(f, c);
  hd = height_depth(q);
  b = new_null_box();
  width(b) = char_width(f, q) + char_italic(f, q);
  height(b) = char_height(f, hd);
  depth(b) = char_depth(f, hd);
  p = get_avail();
  character(p) = c;
  font(p) = f;
  list_ptr(b) = p;

  return b;
}
/* sec 0711 */
void stack_into_box (pointer b, internal_font_number f, quarterword c)
{
  pointer p;

  p = char_box(f, c);
  link(p) = list_ptr(b);
  list_ptr(b) = p;
  height(b) = height(p);
}
/* sec 0712 */
scaled height_plus_depth (internal_font_number f, quarterword c)
{
  four_quarters q;
  eight_bits hd;

  q = char_info(f, c);
  hd = height_depth(q);

  return char_height(f, hd) + char_depth(f, hd);
}
/* sec 0706 */
pointer var_delimiter (pointer d, small_number s, scaled v)
{
  pointer b;
  internal_font_number f, g;
  quarterword c, x, y;
  integer m, n;
  scaled u;
  scaled w;
  four_quarters q;
  four_quarters r;
  eight_bits hd;
  /* small_number z; */
  int z;
  boolean large_attempt;

  f = null_font;
  w = 0;
  large_attempt = false;
  z = small_fam(d);
  x = small_char(d);

  while (true)
  {
    if ((z != 0) || (x != 0))
    {
      z = z + s + 16;

      do
        {
          z = z - 16;
          g = fam_fnt(z);

          if (g != null_font)
          {
            y = x;

            if ((y >= font_bc[g]) && (y <= font_ec[g]))
            {
continu:
              q = char_info(g, y);
              
              if (char_exists(q))
              {
                if (char_tag(q) == ext_tag)
                {
                  f = g;
                  c = y;
                  goto found;
                }

                hd = height_depth(q);
                u = char_height(g, hd) + char_depth(g, hd);

                if (u > w)
                {
                  f = g;
                  c = y;
                  w = u;

                  if (u >= v)
                    goto found;
                }

                if (char_tag(q) == list_tag)
                {
                  y = rem_byte(q);
                  goto continu;
                }
              }
            }
          }
        }
      while (!(z < 16));
    }

    if (large_attempt)
      goto found;

    large_attempt = true;
    z = large_fam(d);
    x = large_char(d);
  }

found:
  if (f != null_font)
    if (char_tag(q) == ext_tag)
    {
      b = new_null_box();
      type(b) = vlist_node;
      r = font_info[exten_base[f] + rem_byte(q)].qqqq;
      c = ext_rep(r);
      u = height_plus_depth(f, c);
      w = 0;
      q = char_info(f, c);
      width(b) = char_width(f, q) + char_italic(f, q);
      c = ext_bot(r);

      if (c != min_quarterword)
        w = w + height_plus_depth(f, c);

      c = ext_mid(r);

      if (c != min_quarterword)
        w = w + height_plus_depth(f, c);

      c = ext_top(r);

      if (c != min_quarterword)
        w = w + height_plus_depth(f, c);

      n = 0;

      if (u > 0)
        while (w < v)
        {
          w = w + u;
          incr(n);

          if (ext_mid(r) != min_quarterword)
            w = w + u;
        }

      c = ext_bot(r);

      if (c != min_quarterword)
        stack_into_box(b, f, c);

      c = ext_rep(r);

      for (m = 1; m <= n; m++)
        stack_into_box(b, f, c);

      c = ext_mid(r);

      if (c != min_quarterword)
      {
        stack_into_box(b, f, c);
        c = ext_rep(r);

        for (m = 1; m <= n; m++)
          stack_into_box(b, f, c);
      }

      c = ext_top(r);

      if (c != 0)
        stack_into_box(b, f, c);
      
      depth(b) = w - height(b);
    }
    else
      b = char_box(f, c);
  else
  {
    b = new_null_box();
    width(b) = null_delimiter_space;
  }

  shift_amount(b) = half(height(b) - depth(b)) - axis_height(s);

  return b;
}
