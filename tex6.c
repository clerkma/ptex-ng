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

/* sec 0785 */
void align_peek (void)
{
restart:
  align_state = 1000000L;

  do
    {
      get_x_token();
    }
  while (!(cur_cmd != spacer));

  if (cur_cmd == no_align)
  {
    scan_left_brace();

    new_save_level(no_align_group);

    if (mode == -vmode)
      normal_paragraph();
  }
  else if (cur_cmd == right_brace)
    fin_align();
  else if ((cur_cmd == car_ret) && (cur_chr == cr_cr_code))
    goto restart;
  else
  {
    init_row();
    init_col();
  }
}
/* sec 0826 */
pointer finite_shrink (pointer p)
{
  pointer q;

  if (no_shrink_error_yet)
  {
    no_shrink_error_yet = false;
    print_err("Infinite glue shrinkage found in a paragraph");
    help5("The paragraph just ended includes some glue that has",
        "infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.",
        "Such glue doesn't belong there---it allows a paragraph",
        "of any length to fit on one line. But it's safe to proceed,",
        "since the offensive shrinkability has been made finite.");
    error();
  }

  q = new_spec(p);
  shrink_order(q) = normal;
  delete_glue_ref(p);

  return q;
}
/* sec 0829 */
void try_break (integer pi, small_number break_type)
{
  pointer r;
  pointer prev_r;
  halfword old_l;
  boolean no_break_yet;
  pointer prev_prev_r;
  pointer s;
  pointer q;
  pointer v;
  integer t;
  internal_font_number f;
  halfword l;
  boolean node_r_stays_active;
  scaled line_width;
  char fit_class;
  halfword b;
  integer d;
  boolean artificial_demerits;
  pointer save_link;
  scaled shortfall;

  if (abs(pi) >= inf_penalty)
    if (pi > 0)
      goto exit;
    else
      pi = eject_penalty;

  no_break_yet = true;
  prev_r = active;
  old_l = 0;
  do_all_six(copy_to_cur_active);

  while (true)
  {
continu:
    r = link(prev_r);

    if (type(r) == delta_node)
    {
      do_all_six(update_width);
      prev_prev_r = prev_r;
      prev_r = r;
      goto continu;
    }

    {
      l = line_number(r);

      if (l > old_l)
      {
        if ((minimum_demerits < awful_bad) && ((old_l != easy_line) || (r == active)))
        {
          if (no_break_yet)
          {
            no_break_yet = false;
            do_all_six(set_break_width_to_background);
            s = cur_p;

            if (break_type > unhyphenated)
              if (cur_p != 0)
              {
                t = replace_count(cur_p);
                v = cur_p;
                s = post_break(cur_p);

                while (t > 0)
                {
                  decr(t);
                  v = link(v);

                  if (is_char_node(v))
                  {
                    f = font(v);
                    break_width[1] = break_width[1] - char_width(f, char_info(f, character(v)));

                    if (font_dir[f] != dir_default)
                    {
                      v = link(v);
                    }
                  }
                  else switch (type(v))
                  {
                    case ligature_node:
                      {
                        f = font(lig_char(v));
                        break_width[1] = break_width[1] - char_width(f, char_info(f, character(lig_char(v))));
                      }
                      break;

                    case hlist_node:
                    case vlist_node:
                    case dir_node:
                    case rule_node:
                    case kern_node:
                      break_width[1] = break_width[1] - width(v);
                      break;

                    case disp_node:
                      do_nothing();
                      break;

                    default:
                      {
                        confusion("disc1");
                        return;
                      }
                      break;
                  }
                }

                while (s != 0)
                {
                  if (is_char_node(s))
                  {
                    f = font(s);
                    break_width[1] = break_width[1] + char_width(f, char_info(f, character(s)));

                    if (font_dir[f] != dir_default)
                    {
                      s = link(s);
                    }
                  }
                  else switch (type(s))
                  {
                    case ligature_node:
                      {
                        f = font(lig_char(s));
                        break_width[1] = break_width[1] + char_width(f, char_info(f, character(lig_char(s))));
                      }
                      break;

                    case hlist_node:
                    case vlist_node:
                    case dir_node:
                    case rule_node:
                    case kern_node:
                      break_width[1] = break_width[1] + width(s);
                      break;

                    case disp_node:
                      do_nothing();
                      break;

                    default:
                      {
                        confusion("disc2");
                        return;
                      }
                      break;
                  }

                  s = link(s);
                }

                break_width[1] = break_width[1] + disc_width;

                if (post_break(cur_p) == 0)
                  s = link(v);
              }

              while (s != 0)
              {
                if (is_char_node(s))
                {
                  if (chain)
                  {
                    break_width[1] = break_width[1] - width(cur_kanji_skip);
                    break_width[2 + stretch_order(cur_kanji_skip)] =
                      break_width[2 + stretch_order(cur_kanji_skip)] - stretch(cur_kanji_skip);
                    break_width[6] = break_width[6] - shrink(cur_kanji_skip);
                  }

                  goto done;
                }

                switch (type(s))
                {
                  case glue_node:
                    {
                      v = glue_ptr(s);
                      break_width[1] = break_width[1] - width(v);
                      break_width[2 + stretch_order(v)] = break_width[2 + stretch_order(v)] - stretch(v);
                      break_width[6] = break_width[6] - shrink(v);
                    }
                    break;

                  case penalty_node:
                    do_nothing();
                    break;

                  case math_node:
                    break_width[1] = break_width[1] - width(s);
                    break;

                  case kern_node:
                    if ((subtype(s) != explicit) && (subtype(s) != ita_kern))
                      goto done;
                    else
                      break_width[1] = break_width[1] - width(s);
                    break;

                  default:
                    goto done;
                    break;
                }

                s = link(s);
              }
done:;
          }

          if (type(prev_r) == delta_node)
            do_all_six(convert_to_break_width);
          else if (prev_r == active)
            do_all_six(store_break_width);
          else
          {
            q = get_node(delta_node_size);
            link(q) = r;
            type(q) = delta_node;
            subtype(q) = 0;
            do_all_six(new_delta_to_break_width);
            link(prev_r) = q;
            prev_prev_r = prev_r;
            prev_r = q;
          }

          if (abs(adj_demerits) >= awful_bad - minimum_demerits)
            minimum_demerits = awful_bad - 1;
          else
            minimum_demerits = minimum_demerits + abs(adj_demerits);

          for (fit_class = very_loose_fit; fit_class <= tight_fit; fit_class++)
          {
            if (minimal_demerits[fit_class] <= minimum_demerits)
            {
              q = get_node(passive_node_size);
              link(q) = passive;
              passive = q;
              cur_break(q) = cur_p;
#ifdef STAT
              incr(pass_number);
              serial(q) = pass_number;
#endif
              prev_break(q) = best_place[fit_class];
              q = get_node(active_node_size);
              break_node(q) = passive;
              line_number(q) = best_pl_line[fit_class] + 1;
              fitness(q) = fit_class;
              type(q) = break_type;
              total_demerits(q) = minimal_demerits[fit_class];
              link(q) = r;
              link(prev_r) = q;
              prev_r = q;
#ifdef STAT
              if (tracing_paragraphs > 0)
              {
                print_nl("@@");
                print_int(serial(passive));
                prints(": line ");
                print_int(line_number(q) - 1);
                print_char('.');
                print_int(fit_class);

                if (break_type == hyphenated)
                  print_char('-');

                prints(" t=");
                print_int(total_demerits(q));
                prints(" -> @@");

                if (prev_break(passive) == 0)
                  print_char('0');
                else
                  print_int(serial(prev_break(passive)));
              }
#endif
            }

            minimal_demerits[fit_class] = awful_bad;
          }

          minimum_demerits = awful_bad;

          if (r != active)
          {
            q = get_node(delta_node_size);
            link(q) = r;
            type(q) = delta_node;
            subtype(q) = 0;
            do_all_six(new_delta_from_break_width);
            link(prev_r) = q;
            prev_prev_r = prev_r;
            prev_r = q;
          }
        }

        if (r == active)
          goto exit;

        if (l > easy_line)
        {
          line_width = second_width;
          old_l = max_halfword - 1;
        }
        else
        {
          old_l = l;

          if (l > last_special_line)
            line_width = second_width;
          else if (par_shape_ptr == 0)
            line_width = first_width;
          else
            line_width = mem[par_shape_ptr + 2 * l].cint;
        }
      }
    }

    {
      artificial_demerits = false;
      shortfall = line_width - cur_active_width[1];

      if (shortfall > 0)
        if ((cur_active_width[3] != 0) || (cur_active_width[4] != 0) || (cur_active_width[5] != 0))
        {
          b = 0;
          fit_class = decent_fit;
        }
        else
        {
          if (shortfall > 7230584L)
            if (cur_active_width[2] < 1663497L)
            {
              b = 10000;
              fit_class = very_loose_fit;
              goto done1;
            }

          b = badness(shortfall, cur_active_width[2]);

          if (b > 12)
            if (b > 99)
              fit_class = very_loose_fit;
            else
              fit_class = loose_fit;
          else
            fit_class = decent_fit;
done1:;
        }
      else
      {
        if (- (integer) shortfall > cur_active_width[6])
          b = inf_bad + 1;
        else
          b = badness(- (integer) shortfall, cur_active_width[6]);

        if (b > 12)
          fit_class = tight_fit;
        else
          fit_class = decent_fit;
      }

      if ((b > inf_bad) || (pi == eject_penalty))
      {
        if (final_pass && (minimum_demerits == awful_bad) && (link(r) == active) && (prev_r == active))
          artificial_demerits = true;
        else if (b > threshold)
          goto deactivate;

        node_r_stays_active = false;
      }
      else
      {
        prev_r = r;

        if (b > threshold)
          goto continu;

        node_r_stays_active = true;
      }

      if (artificial_demerits)
        d = 0;
      else
      {
        d = line_penalty + b;

        if (abs(d) >= 10000)
          d = 100000000L;
        else
          d = d * d;

        if (pi != 0)
          if (pi > 0)
            d = d + pi * pi;
          else if (pi > -10000)
            d = d - pi * pi;

        if ((break_type == hyphenated) && (type(r) == hyphenated))
          if (cur_p != 0)
            d = d + double_hyphen_demerits;
          else
            d = d + final_hyphen_demerits;

        if (abs(toint(fit_class) - toint(fitness(r))) > 1)
          d = d + adj_demerits;
      }

#ifdef STAT
      if (tracing_paragraphs > 0)
      {
        if (printed_node != cur_p)
        {
          print_nl("");

          if (cur_p == 0)
            short_display(link(printed_node));
          else
          {
            save_link = link(cur_p);
            link(cur_p) = 0;
            print_nl("");
            short_display(link(printed_node));
            link(cur_p) = save_link;
          }

          printed_node = cur_p;
        }

        print_nl("@");

        if (cur_p == 0)
          print_esc("par");
        else if ((type(cur_p) != glue_node) && (!is_char_node(cur_p)))
        {
          if (type(cur_p) == penalty_node)
            print_esc("penalty");
          else if (type(cur_p) == disc_node)
            print_esc("discretionary");
          else if (type(cur_p) == kern_node)
            print_esc("kern");
          else
            print_esc("math");
        }

        prints(" via @@");

        if (break_node(r) == 0)
          print_char('0');
        else
          print_int(serial(break_node(r)));

        prints(" b=");

        if (b > inf_bad)
          print_char('*');
        else
          print_int(b);

        prints(" p=");
        print_int(pi);
        prints(" d=");

        if (artificial_demerits)
          print_char('*');
        else
          print_int(d);
      }
#endif

      d = d + total_demerits(r);

      if (d <= minimal_demerits[fit_class])
      {
        minimal_demerits[fit_class] = d;
        best_place[fit_class] = break_node(r);
        best_pl_line[fit_class] = l;

        if (d < minimum_demerits)
          minimum_demerits = d;
      }

      if (node_r_stays_active)
        goto continu;

deactivate:
      link(prev_r) = link(r);
      free_node(r, active_node_size);

      if (prev_r == active)
      {
        r = link(active);

        if (type(r) == delta_node)
        {
          do_all_six(update_active);
          do_all_six(copy_to_cur_active);
          link(active) = link(r);
          free_node(r, delta_node_size);
        }
      }
      else if (type(prev_r) == delta_node)
      {
        r = link(prev_r);

        if (r == active)
        {
          do_all_six(downdate_width);
          link(prev_prev_r) = active;
          free_node(prev_r, delta_node_size);
          prev_r = prev_prev_r;
        }
        else if (type(r) == delta_node)
        {
          do_all_six(update_width);
          do_all_six(combine_two_deltas);
          link(prev_r) = link(r);
          free_node(r, delta_node_size);
        }
      }
    }
  }

exit:;
#ifdef STAT
  if (cur_p == printed_node)
    if (cur_p != 0)
      if (type(cur_p) == disc_node)
      {
        t = replace_count(cur_p);

        while (t > 0)
        {
          decr(t);
          printed_node = link(printed_node);
        }
      }
#endif
}
/* sec 0877 */
void post_line_break (integer final_widow_penalty)
{
  pointer q, r, s;
  boolean disc_break;
  boolean post_disc_break;
  scaled cur_width;
  scaled cur_indent;
  quarterword t;
  integer pen;
  halfword cur_line;

  q = break_node(best_bet);
  cur_p = 0;

  do
    {
      r = q;
      q = prev_break(q);
      next_break(r) = cur_p;
      cur_p = r;
    }
  while (!(q == 0));

  cur_line = prev_graf + 1;
  last_disp = 0;

  do
    {
      q = cur_break(cur_p);
      disc_break = false;
      post_disc_break = false;

      if (q != 0)
      {
        if (!is_char_node(q))
          if (type(q) == glue_node)
          {
            delete_glue_ref(glue_ptr(q));
            glue_ptr(q) = right_skip;
            subtype(q) = right_skip_code + 1;
            add_glue_ref(right_skip);
            goto done;
          }
          else
          {
            if (type(q) == disc_node)
            {
              t = replace_count(q);

              if (t == 0)
                r = link(q);
              else
              {
                r = q;

                while (t > 1)
                {
                  r = link(r);
                  decr(t);
                }

                s = link(r);
                r = link(s);
                link(s) = 0;
                flush_node_list(link(q));
                replace_count(q) = 0;
              }

              if (post_break(q) != 0)
              {
                s = post_break(q);

                while (link(s) != 0)
                  s = link(s);

                link(s) = r;
                r = post_break(q);
                post_break(q) = 0;
                post_disc_break = true;
              }

              if (pre_break(q) != 0)
              {
                s = prev_break(q);
                link(q) = s;

                while (link(s) != 0)
                  s = link(s);

                prev_break(q) = 0;
                q = s;
              }

              link(q) = r;
              disc_break = true;
            }
            else if ((type(q) == math_node) || (type(q) == kern_node))
              width(q) = 0;
          }
      }
      else
      {
        q = temp_head;

        while (link(q) != 0)
          q = link(q);
      }

      r = new_param_glue(right_skip_code);
      link(r) = link(q);
      link(q) = r;
      q = r;

done:

      r = link(q);
      link(q) = 0;
      q = link(temp_head);
      link(temp_head) = r;

      if (last_disp != 0)
      {
        r = get_node(small_node_size);
        type(r) = disp_node;
        disp_dimen(r) = last_disp;
        link(r) = q;
        q = r;
      }

      if (left_skip != 0)
      {
        r = new_param_glue(left_skip_code);
        link(r) = q;
        q = r;
      }

      if (cur_line > last_special_line)
      {
        cur_width = second_width;
        cur_indent = second_indent;
      }
      else if (par_shape_ptr == 0)
      {
        cur_width = first_width;
        cur_indent = first_indent;
      }
      else
      {
        cur_width = mem[par_shape_ptr + 2 * cur_line].cint;
        cur_indent = mem[par_shape_ptr + 2 * cur_line - 1].cint;
      }

      adjust_tail = adjust_head;
      just_box = hpack(q, cur_width, 0);
      shift_amount(just_box) = cur_indent;
      append_to_vlist(just_box);

      if (adjust_head != adjust_tail)
      {
        link(tail) = link(adjust_head);
        tail = adjust_tail;
      }

      adjust_tail = 0;

      if (cur_line + 1 != best_line)
      {
        pen = inter_line_penalty;

        if (cur_line == prev_graf + 1)
          pen = pen + club_penalty;

        if (cur_line + 2 == best_line)
          pen = pen + final_widow_penalty;

        if (disc_break)
          pen = pen + broken_penalty;

        if (pen != 0)
        {
          r = new_penalty(pen);
          link(tail) = r;
          tail = r;
        }
      }

      incr(cur_line);
      cur_p = next_break(cur_p);

      if (cur_p != 0)
        if (!post_disc_break)
        {
          r = temp_head;

          while (true)
          {
            q = link(r);

            if (q == cur_break(cur_p))
              goto done1;

            if (is_char_node(q))
              goto done1;

            if (non_discardable(q))
              goto done1;

            if (type(q) == kern_node)
              if ((subtype(q) != explicit) && (subtype(q) != ita_kern))
                goto done1;

            r = q;
          }
done1:
          if (r != temp_head)
          {
            link(r) = 0;
            flush_node_list(link(temp_head));
            link(temp_head) = q;
          }
        }
    }
  while (!(cur_p == 0));

  if ((cur_line != best_line) || (link(temp_head) != 0))
  {
    confusion("line breaking");
    return;
  }

  prev_graf = best_line - 1;
}
/* sec 0906 */
small_number reconstitute (small_number j, small_number n, halfword bchar, halfword hchar)
{
  pointer p;
  pointer t;
  four_quarters q;
  halfword cur_rh;
  halfword test_char;
  scaled w;
  font_index k;

  hyphen_passed = 0;
  t = hold_head;
  w = 0;
  link(hold_head) = 0;
  cur_l = hu[j];
  cur_q = t;

  if (j == 0)
  {
    ligature_present = init_lig;
    p = init_list; 

    if (ligature_present)
      lft_hit = init_lft; 

    while (p != 0)
    {
      append_charnode_to_t(character(p));
      p = link(p);
    }
  }
  else if (cur_l < 256)
  {
    append_charnode_to_t(cur_l);
  }

  lig_stack = 0;
  set_cur_r();

continu:
  if (cur_l == non_char)
  {
    k = bchar_label[hf];

    if (k == non_address)
      goto done;
    else
      q = font_info[k].qqqq;
  }
  else
  {
    q = char_info(hf, cur_l);

    if (char_tag(q) != lig_tag)
      goto done;

    k = lig_kern_start(hf, q);
    q = font_info[k].qqqq;

    if (skip_byte(q) > stop_flag)
    {
      k = lig_kern_restart(hf, q);
      q = font_info[k].qqqq;
    }
  }

  if (cur_rh < non_char)
    test_char = cur_rh;
  else
    test_char = cur_r;

  while (true)
  {
    if (next_char(q) == test_char)
      if (skip_byte(q) <= 128)
        if (cur_rh < non_char)
        {
          hyphen_passed = j;
          hchar = non_char;
          cur_rh = non_char;
          goto continu;
        }
        else
        {
          if (hchar < non_char)
            if (odd(hyf[j]))
            {
              hyphen_passed = j;
              hchar = non_char;
            }

          if (op_byte(q) < kern_flag)
          {
            if (cur_l == non_char)
              lft_hit = true;

            if (j == n)
              if (lig_stack == 0)
                rt_hit = true;

            check_interrupt();

            switch (op_byte(q))
            {
              case 1:
              case 5:
                {
                  cur_l = rem_byte(q);
                  ligature_present = true;
                }
                break;

              case 2:
              case 6:
                {
                  cur_r = rem_byte(q);

                  if (lig_stack != 0)
                    character(lig_stack) = cur_r;
                  else
                  {
                    lig_stack = new_lig_item(cur_r);

                    if (j == n)
                      bchar = non_char;
                    else
                    {
                      p = get_avail();
                      list_ptr(lig_stack) = p;
                      character(p) = hu[j + 1];
                      font(p) = hf;
                    }
                  }
                }
                break;

              case 3:
                {
                  cur_r = rem_byte(q);
                  p = lig_stack;
                  lig_stack = new_lig_item(cur_r);
                  link(lig_stack) = p;
                }
                break;

              case 7:
              case 11:
                {
                  wrap_lig(false);
                  cur_q = t;
                  cur_l = rem_byte(q);
                  ligature_present = true;
                }
                break;

              default:
                {
                  cur_l = rem_byte(q);
                  ligature_present = true;

                  if (lig_stack != 0)
                  {
                    if (lig_ptr(lig_stack) != 0)
                    {
                      link(t) = lig_ptr(lig_stack);
                      t = link(t);
                      incr(j);
                    }

                    p = lig_stack;
                    lig_stack = link(p);
                    free_node(p, small_node_size);

                    if (lig_stack == 0)
                    {
                      if (j < n)
                        cur_r = hu[j + 1];
                      else
                        cur_r = bchar;

                      if (odd(hyf[j]))
                        cur_rh = hchar;
                      else
                        cur_rh = 256;
                    }
                    else
                      cur_r = character(lig_stack);
                  }
                  else if (j == n)
                    goto done;
                  else
                  {
                    append_charnode_to_t(cur_r);
                    incr(j);
                    set_cur_r();
                  }
                }
                break;
            }

            if (op_byte(q) > 4)
              if (op_byte(q) != 7)
                goto done;

            goto continu;
          }

          w = char_kern(hf, q);
          goto done;
        }

    if (skip_byte(q) >= stop_flag)
      if (cur_rh == non_char)
        goto done;
      else
      {
        cur_rh = non_char;
        goto continu;
      }
      
    k = k + skip_byte(q) + 1;
    q = font_info[k].qqqq;
  }

done:
  wrap_lig(rt_hit);

  if (w != 0)
  {
    link(t) = new_kern(w);
    t = link(t);
    w = 0;
  }

  if (lig_stack != 0)
  {
    cur_q = t;
    cur_l = character(lig_stack);
    ligature_present = true;
    pop_lig_stack();
    goto continu;
  }

  return j;
}
/* sec 0895 */
void hyphenate (void)
{
  /* char i, j, l; */
  char i, j;
  int l;
  pointer q, r, s;
  halfword bchar;
  pointer major_tail, minor_tail;
  /* ASCII_code c; */
  int c;
  char c_loc;
  /* integer r_count; */
  int r_count;
  pointer hyf_node;
  trie_pointer z;
  integer v;
  hyph_pointer h;
  str_number k;
  pool_pointer u;

  for (j = 0; j <= hn; j++)
    hyf[j] = 0;

  h = hc[1];
  incr(hn);
  hc[hn] = cur_lang;

  for (j = 2; j <= hn; j++)
    h = (h + h + hc[j]) % hyphen_prime;

  while (true)
  {
    k = hyph_word[h];

    if (k == 0)
      goto not_found;

    if (length(k) < hn)
      goto not_found;

    if (length(k) == hn)
    {
      j = 1;
      u = str_start[k];

      do
        {
          if (str_pool[u] < hc[j])
            goto not_found;

          if (str_pool[u] > hc[j])
            goto done;

          incr(j);
          incr(u);
        }
      while (!(j > hn));

      s = hyph_list[h];

      while (s != 0)
      {
        hyf[info(s)] = 1;
        s = link(s);
      }

      decr(hn);
      goto found;
    }

done:
    if (h > 0)
      decr(h);
    else
      h = hyphen_prime;
  }

not_found:
  decr(hn);

  if (trie_trc[cur_lang + 1] != cur_lang)
    return;

  hc[0] = 0;
  hc[hn + 1] = 0;
  hc[hn + 2] = 256;

  for (j = 0; j <= hn - r_hyf + 1; j++)
  {
    z = trie_trl[cur_lang + 1] + hc[j];
    l = j;

    while (hc[l] == trie_trc[z])
    {
      if (trie_tro[z] != min_trie_op)
      {
        v = trie_tro[z];

        do
          {
            v = v + op_start[cur_lang];
            i = l - hyf_distance[v];

            if (hyf_num[v] > hyf[i])
              hyf[i]= hyf_num[v];

            v = hyf_next[v];
          }
        while (!(v == min_trie_op));
      }

      incr(l);
      z = trie_trl[z] + hc[l];
    }
  }

found:
  for (j = 0; j <= l_hyf - 1; j++)
    hyf[j] = 0;

  for (j = 0; j <= r_hyf - 1; j++)
    hyf[hn - j]= 0;

  for (j = l_hyf; j <= hn - r_hyf; j++)
    if (odd(hyf[j]))
      goto found1;

  return;

found1:
  q = link(hb);
  link(hb) = 0;
  r = link(ha);
  link(ha) = 0;
  bchar = hyf_bchar;

  if (is_char_node(ha))
    if (font(ha) != hf)
      goto found2;
    else
    {
      init_list = ha;
      init_lig = false;
      hu[0] = character(ha);
    }
  else if (type(ha) == ligature_node)
    if (font(lig_char(ha)) != hf)
      goto found2;
    else
    {
      init_list = lig_ptr(ha);
      init_lig = true;
      init_lft = (subtype(ha) > 1);
      hu[0] = character(lig_char(ha));

      if (init_list == 0)
        if (init_lft)
        {
          hu[0] = 256;
          init_lig = false;
        }

      free_node(ha, small_node_size);
    }
  else
  {
    if (!is_char_node(r))
      if (type(r) == ligature_node)
        if (subtype(r) > 1)
          goto found2;

    j = 1;
    s = ha;
    init_list = 0;
    goto common_ending;
  }

  s = cur_p;

  while (link(s) != ha)
    s = link(s);

  j = 0;
  goto common_ending;

found2:
  s = ha;
  j = 0;
  hu[0] = 256;
  init_lig = false;
  init_list = 0;

common_ending:
  flush_node_list(r);

  do
    {
      l = j;
      j = reconstitute(j, hn, bchar, hyf_char) + 1;

      if (hyphen_passed == 0)
      {
        link(s) = link(hold_head);

        while (link(s) != 0)
          s = link(s);

        if (odd(hyf[j - 1]))
        {
          l = j;
          hyphen_passed = j - 1;
          link(hold_head) = 0;
        }
      }

      if (hyphen_passed > 0)
        do
          {
            r = get_node(small_node_size);
            link(r) = link(hold_head);
            type(r) = disc_node;
            major_tail = r;
            r_count = 0;

            while (link(major_tail) != 0)
            {
              major_tail = link(major_tail);
              incr(r_count);
            }

            i = hyphen_passed;
            hyf[i] = 0;
            minor_tail = 0;
            pre_break(r) = 0;
            hyf_node = new_character(hf, hyf_char);

            if (hyf_node != 0)
            {
              incr(i);
              c = hu[i];
              hu[i] = hyf_char;
              free_avail(hyf_node);
            }

            while (l <= i)
            {
              l = reconstitute(l, i, font_bchar[hf], non_char) + 1;

              if (link(hold_head) != 0) /* BUG FIX ??? */
              {
                if (minor_tail == 0)
                  pre_break(r) = link(hold_head);
                else
                  link(minor_tail) = link(hold_head);

                minor_tail = link(hold_head);

                while (link(minor_tail) != 0)  /* BUG FIX */
                  minor_tail = link(minor_tail);
              }
            }

            if (hyf_node != 0)
            {
              hu[i] = c;
              l = i;
              decr(i);
            }

            minor_tail = 0;
            post_break(r) = 0;
            c_loc = 0;

            if (bchar_label[hf] != non_address)
            {
              decr(l);
              c = hu[l];
              c_loc = l;
              hu[l]= 256;
            }

            while (l < j)
            {
              do
                {
                  l = reconstitute(l, hn, bchar, 256) + 1;

                  if (c_loc > 0)
                  {
                    hu[c_loc] = c;
                    c_loc = 0;
                  }

                  if (link(hold_head) != 0)     /* BUG FIX */
                  {
                    if (minor_tail == 0)
                      post_break(r) = link(hold_head);
                    else
                      link(minor_tail) = link(hold_head);

                    minor_tail = link(hold_head);

                    while (link(minor_tail) != 0)
                      minor_tail = link(minor_tail);
                  }
                }
              while (!(l >= j));

              while (l > j)
              {
                j = reconstitute(j, hn, bchar, non_char) + 1;
                link(major_tail) = link(hold_head);

                while (link(major_tail) != 0)
                {
                  major_tail = link(major_tail);
                  incr(r_count);
                }
              }
            }

            if (r_count > 127)
            {
              link(s) = link(r);
              link(r) = 0;
              flush_node_list(r);
            }
            else
            {
              link(s) = r;
              replace_count(r) = r_count;
            }

            s = major_tail;
            hyphen_passed = j - 1;
            link(hold_head) = 0;
          }
        while (!(!odd(hyf[j - 1])));
    }
  while (!(j > hn));

  link(s) = q;
  flush_list(init_list);
}
/* sec 0934 */
void new_hyph_exceptions (void)
{
  char n;
  char j;
  hyph_pointer h;
  str_number k;
  pointer p;
  pointer q;
  str_number s, t;
  pool_pointer u, v;

  scan_left_brace();
  set_cur_lang();
  n = 0;
  p = 0;

  while (true)
  {
    get_x_token();

reswitch:
    switch (cur_cmd)
    {
      case letter:
      case other_char:
      case char_given:
        if (cur_chr == '-')
        {
          if (n < 63)
          {
            q = get_avail();
            link(q) = p;
            info(q) = n;
            p = q;
          }
        }
        else
        {
          if (lc_code(cur_chr) == 0)
          {
            print_err("Not a letter");
            help2("Letters in \\hyphenation words must have \\lccode>0.",
                "Proceed; I'll ignore the character I just read.");
            error();
          }
          else if (n < 63)
          {
            incr(n);
            hc[n] = lc_code(cur_chr);
          }
        }
        break;

      case char_num:
        {
          scan_char_num();
          cur_chr = cur_val;
          cur_cmd = char_given;
          goto reswitch;
        }
        break;

      case spacer:
      case right_brace:
        {
          if (n > 1)
          {
            incr(n);
            hc[n] = cur_lang;
            str_room(n);
            h = 0;

            for (j = 1; j <= n; j++)
            {
              h = (h + h + hc[j]) % hyphen_prime;
              append_char(hc[j]);
            }

            s = make_string();

            if (hyph_count == hyphen_prime)
            {
              overflow("exception dictionary", hyphen_prime);
              return;
            }

            incr(hyph_count);

            while (hyph_word[h] != 0)
            {
              k = hyph_word[h];

              if (length(k) < length(s))
                goto found;

              if (length(k) > length(s))
                goto not_found;

              u = str_start[k];
              v = str_start[s];

              do
                {
                  if (str_pool[u] < str_pool[v])
                    goto found;

                  if (str_pool[u] > str_pool[v])
                    goto not_found;

                  incr(u);
                  incr(v);
                }
              while (!(u == str_start[k + 1]));

found:
              q = hyph_list[h];
              hyph_list[h] = p;
              p = q;
              t = hyph_word[h];
              hyph_word[h] = s;
              s = t;

not_found:
              if (h > 0)
                decr(h);
              else
                h = hyphen_prime;
            }

            hyph_word[h] = s;
            hyph_list[h] = p;
          }

          if (cur_cmd == right_brace)
            return;

          n = 0;
          p = 0;
        }
        break;

      default:
        {
          print_err("Improper ");
          print_esc("hyphenation");
          prints(" will be flushed");
          help2("Hyphenation exceptions must contain only letters",
              "and hyphens. But continue; I'll forgive and forget.");
          error();
        }
        break;
    }
  }
}
/* sec 0968 */
pointer prune_page_top (pointer p)
{
  pointer prev_p;
  pointer q;

  prev_p = temp_head;
  link(temp_head) = p;

  while (p != 0)
    switch (type(p))
    {
      case dir_node:
      case hlist_node:
      case vlist_node:
      case rule_node:
        {
          q = new_skip_param(split_top_skip_code);
          link(prev_p) = q;
          link(q) = p;

          if (width(temp_ptr) > height(p))
            width(temp_ptr) = width(temp_ptr) - height(p);
          else
            width(temp_ptr) = 0;

          p = 0;
        }
        break;

      case whatsit_node:
      case mark_node:
      case ins_node:
        {
          prev_p = p;
          p = link(prev_p);
        }
        break;

      case glue_node:
      case kern_node:
      case penalty_node:
        {
          q = p;
          p = link(q);
          link(q) = 0;
          link(prev_p) = p;
          flush_node_list(q);
        }
        break;

      default:
        {
          confusion("pruning");
          return 0;
        }
        break;
    }

  return link(temp_head);
}
/* sec 0970 */
pointer vert_break (pointer p, scaled h, scaled d)
{
  pointer prev_p;
  pointer q, r;
  integer pi;
  integer b;
  integer least_cost;
  pointer best_place;
  scaled prev_dp; 
  /* small_number t; */
  int t;

  prev_p = p;
  least_cost = awful_bad;
  do_all_six(set_height_zero);
  prev_dp = 0;

  while (true)
  {
    if (p == 0)
      pi = eject_penalty;
    else switch (type(p))
    {
      case dir_node:
      case hlist_node:
      case vlist_node:
      case rule_node:
        {
          cur_height = cur_height + prev_dp + height(p);
          prev_dp = depth(p);
          goto not_found;
        }
        break;

      case whatsit_node:
        goto not_found;
        break;

      case glue_node:
        if (precedes_break(prev_p))
          pi = 0;
        else
          goto update_heights;
        break;

      case kern_node:
        {
          if (link(p) == 0)
            t = penalty_node;
          else
            t = type(link(p));

          if (t == glue_node)
            pi = 0;
          else
            goto update_heights;
        }
        break;

      case penalty_node:
        pi = penalty(p);
        break;

      case mark_node:
      case ins_node:
        goto not_found;
        break;

      default:
        {
          confusion("vertbreak");
          return 0;
        }
        break;
    }

    if (pi < inf_penalty)
    {
      if (cur_height < h)
        if ((active_width[3] != 0) || (active_width[4] != 0) || (active_width[5] != 0))
          b = 0;
        else
          b = badness(h - cur_height, active_width[2]);
      else if (act_width - h > active_width[6])
        b = awful_bad;
      else
        b = badness(cur_height - h, active_width[6]);

      if (b < awful_bad)
        if (pi <= eject_penalty)
          b = pi;
        else if (b < inf_bad)
          b = b + pi;
        else
          b = deplorable;

      if (b <= least_cost)
      {
        best_place = p;
        least_cost = b;
        best_height_plus_depth = cur_height + prev_dp;
      }

      if ((b == awful_bad) || (pi <= eject_penalty))
        goto done;
    }

    if ((type(p) < glue_node) || (type(p) > kern_node))
      goto not_found;

update_heights:

    if (type(p) == kern_node)
      q = p;
    else
    {
      q = glue_ptr(p);
      active_width[2 + stretch_order(q)] = active_width[2 + stretch_order(q)] + stretch(q);
      active_width[6] = active_width[6] + shrink(q);

      if ((shrink_order(q) != normal) && (shrink(q) != 0))
      {
        print_err("Infinite glue shrinkage found in box being split");
        help4("The box you are \\vsplitting contains some infinitely",
            "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
            "Such glue doesn't belong there; but you can safely proceed,",
            "since the offensive shrinkability has been made finite.");
        error();
        r = new_spec(q);
        shrink_order(r) = normal;
        delete_glue_ref(q);
        glue_ptr(p) = r;
        q = r;
      }
    }

    cur_height = cur_height + prev_dp + width(q);
    prev_dp = 0;

not_found:

    if (prev_dp > d)
    {
      cur_height = cur_height + prev_dp - d;
      prev_dp = d;
    }

    prev_p = p;
    p = link(prev_p);
  }

done:
  return best_place;
}
/* sec 0977 */
pointer vsplit (eight_bits n, scaled h)
{
  pointer v;
  pointer w;
  pointer p;
  pointer q;

  v = box(n);

  if (split_first_mark != 0)
  {
    delete_token_ref(split_first_mark);
    split_first_mark = 0;
    delete_token_ref(split_bot_mark);
    split_bot_mark = 0;
  }

  if (v == 0)
  {
    return 0;
  }

  if (type(v) == dir_node)
  {
    w = v;
    v =list_ptr(v);
    delete_glue_ref(space_ptr(w));
    delete_glue_ref(xspace_ptr(w));
    free_node(w, box_node_size);
  }

  if (type(v) != vlist_node)
  {
    print_err("");
    print_esc("vsplit");
    prints(" needs a ");
    print_esc("vbox");
    help2("The box you are trying to split is an \\hbox.",
        "I can't split such a box, so I'll leave it alone.");
    error();
    return 0;
  }

  flush_node_list(link(v));
  link(v) = null;
  q = vert_break(list_ptr(v), h, split_max_depth);
  p = list_ptr(v);

  if (p == q)
    list_ptr(v) = 0;
  else while (true)
  {
    if (type(p) == mark_node)
      if (split_first_mark == 0)
      {
        split_first_mark = mark_ptr(p);
        split_bot_mark = split_first_mark;
        token_ref_count(split_first_mark) = token_ref_count(split_first_mark) + 2;
      }
      else
      {
        delete_token_ref(split_bot_mark);
        split_bot_mark = mark_ptr(p);
        add_token_ref(split_bot_mark);
      }

    if (link(p) == q)
    {
      link(p) = 0;
      goto done;
    }

    p = link(p);
  }

done:
  q = prune_page_top(q);
  p = list_ptr(v);
 
  if (q == 0)
    box(n) = 0;
  else
  {
    box(n) = vpackage(q, 0, 1, max_dimen);
    set_box_dir(box(n), box_dir(v));
  }

  q = vpackage(p, h, exactly, split_max_depth);
  set_box_dir(q, box_dir(v));
  delete_glue_ref(space_ptr(v));
  delete_glue_ref(xspace_ptr(v));
  free_node(v, box_node_size);

  return q;
}
/* sec 0985 */
void print_totals (void)
{
  print_scaled(page_so_far[1]);
  print_plus(2, "");
  print_plus(3, "fil");
  print_plus(4, "fill");
  print_plus(5, "filll");

  if (page_shrink != 0)
  {
    prints(" minus ");
    print_scaled(page_shrink);
  }
}
/* sec 0987 */
void freeze_page_specs (small_number s)
{
  page_contents = s;
  page_goal = vsize;
  page_max_depth = max_depth;
  page_depth = 0;
  do_all_six(set_page_so_far_zero);
  least_page_cost = awful_bad;

#ifdef STAT
  if (tracing_pages > 0)
  {
    begin_diagnostic();
    print_nl("%% goal height=");
    print_scaled(page_goal);
    prints(", max depth=");
    print_scaled(page_max_depth);
    end_diagnostic(false);
  }
#endif
}
/* sec 0992 */
void box_error (eight_bits n)
{
  error();
  begin_diagnostic();
  print_nl("The following box has been deleted:");
  show_box(box(n));
  end_diagnostic(true);
  flush_node_list(box(n));
  box(n) = 0;
}
/* sec 0993 */
void ensure_vbox_(eight_bits n)
{
  pointer p;

  p = box(n);

  if (p != null)
    if (type(p) == dir_node)
    {
      p = list_ptr(p);
      delete_glue_ref(space_ptr(box(n)));
      delete_glue_ref(xspace_ptr(box(n)));
      free_node(box(n), box_node_size);
      box(n) = p;
    }

  if (p != 0)
    if (type(p) != vlist_node)
    {
      print_err("Insertions can only be added to a vbox");
      help3("Tut tut: You're trying to \\insert into a",
          "\\box register that now contains an \\hbox.",
          "Proceed, and I'll discard its present contents.");
      box_error(n);
    }
}
/* sec 1012 */
void fire_up (pointer c)
{
  pointer p, q, r, s;
  pointer prev_p;
  /* unsigned char n; */
  unsigned int n;
  boolean wait;
  integer save_vbadness;
  scaled save_vfuzz;
  pointer save_split_top_skip;

  if (type(best_page_break) == penalty_node)
  {
    geq_word_define(int_base + output_penalty_code, penalty(best_page_break));
    penalty(best_page_break) = inf_penalty;
  }
  else
    geq_word_define(int_base + output_penalty_code, inf_penalty);

  if (bot_mark != 0)
  {
    if (top_mark != 0)
      delete_token_ref(top_mark);

    top_mark = bot_mark;
    add_token_ref(top_mark);
    delete_token_ref(first_mark);
    first_mark = 0;
  }

  if (c == best_page_break)
    best_page_break = 0;

  if (box(255) != 0)
  {
    print_err("");
    print_esc("box");
    prints("255 is not void");
    help2("You shouldn't use \\box255 except in \\output routines.",
        "Proceed, and I'll discard its present contents.");
    box_error(255);
  }

  insert_penalties = 0;
  save_split_top_skip = split_top_skip;

  if (holding_inserts <= 0)
  {
    r = link(page_ins_head);

    while (r != page_ins_head)
    {
      if (best_ins_ptr(r) != 0)
      {
        n = subtype(r);
        ensure_vbox(n);

        if (box(n) == 0)
          box(n) = new_null_box();

        p = box(n) + list_offset;

        while (link(p) != 0)
          p = link(p);

        last_ins_ptr(r) = p;
      }

      r = link(r);
    }
  }

  q = hold_head;
  link(q) = 0;
  prev_p = page_head;
  p = link(prev_p);

  while (p != best_page_break)
  {
    if (type(p) == ins_node)
    {
      if (holding_inserts <= 0)
      {
        r = link(page_ins_head);

        while (subtype(r) != subtype(p))
          r = link(r);

        if (best_ins_ptr(r) == 0)
          wait = true;
        else
        {
          wait = false;
          n = subtype(p);

          switch (box_dir(box(n)))
          {
            case any_dir:
              if (ins_dir(p) != box_dir(box(n)))
              {
                print_err("Insertions can only be added to a same direction vbox");
                help3("Tut tut: You're trying to \\insert into a",
                  "\\box register that now have a different direction.",
                  "Proceed, and I'll discard its present contents.");
                box_error(n);
                box(n) = new_null_box();
                last_ins_ptr(r) = box(n) + list_offset;
              }
              break;
            default:
              set_box_dir(box(n), ins_dir(p));
              break;
          }

          s = last_ins_ptr(r);
          link(s) = ins_ptr(p);

          if (best_ins_ptr(r) == p)
          {
            if (type(r) == split_up)
              if ((broken_ins(r) == p) && (broken_ins(r) != 0))
              {
                while (link(s) != broken_ptr(r))
                  s = link(s);

                link(s) = 0;
                split_top_skip = split_top_ptr(p);
                ins_ptr(p) = prune_page_top(broken_ptr(r));

                if (ins_ptr(p) != 0)
                {
                  temp_ptr = vpackage(ins_ptr(p), 0, 1, max_dimen);
                  height(p) = height(temp_ptr) + depth(temp_ptr);
                  delete_glue_ref(space_ptr(temp_ptr));
                  delete_glue_ref(xspace_ptr(temp_ptr));
                  free_node(temp_ptr, box_node_size);
                  wait = true;
                }
              }

            best_ins_ptr(r) = 0;
            n = subtype(r);
            temp_ptr = list_ptr(box(n));
            delete_glue_ref(space_ptr(box(n)));
            delete_glue_ref(xspace_ptr(box(n)));
            flush_node_list(link(box(n)));
            free_node(box(n), box_node_size);
            box(n) = vpackage(temp_ptr, 0, 1, max_dimen);
            set_box_dir(box(n), ins_dir(p));
          }
          else
          {
            while (link(s) != 0)
              s = link(s);

            last_ins_ptr(r) = s;
          }
        }

        link(prev_p) = link(p);
        link(p) = 0;

        if (wait)
        {
          link(q) = p;
          q = p;
          incr(insert_penalties);
        }
        else
        {
          delete_glue_ref(split_top_ptr(p));
          free_node(p, ins_node_size);
        }

        p = prev_p;
      }
    }
    else if (type(p) == mark_node)
    {
      if (first_mark == 0)
      {
        first_mark = mark_ptr(p);
        add_token_ref(first_mark);
      }

      if (bot_mark != 0)
        delete_token_ref(bot_mark);

      bot_mark = mark_ptr(p);
      add_token_ref(bot_mark);
    }

    prev_p = p;
    p = link(prev_p);
  }

  split_top_skip = save_split_top_skip;

  if (p != 0)
  {
    if (link(contrib_head) == 0)
      if (nest_ptr == 0)
        tail = page_tail;
      else
        nest[0].tail_field = page_tail;

    link(page_tail) = link(contrib_head);
    link(contrib_head) = p;
    link(prev_p) = 0;
  }

  save_vbadness = vbadness;
  vbadness = inf_bad;
  save_vfuzz = vfuzz;
  vfuzz = max_dimen;
  box(255) = vpackage(link(page_head), best_size, 0, page_max_depth);
  set_box_dir(box(255), page_dir);
  vbadness = save_vbadness;
  vfuzz = save_vfuzz;

  if (last_glue != empty_flag)
    delete_glue_ref(last_glue);

  page_contents = 0;
  page_tail = page_head;
  link(page_head) = 0;
  last_glue = empty_flag;
  last_penalty = 0;
  last_kern = 0;
  page_depth = 0;
  page_max_depth = 0;

  if (q != hold_head)
  {
    link(page_head) = link(hold_head);
    page_tail = q;
  }

  r = link(page_ins_head);

  while (r != page_ins_head)
  {
    q = link(r);
    free_node(r, page_ins_node_size);
    r = q;
  }
 
  link(page_ins_head) = page_ins_head;

  if ((top_mark != 0) && (first_mark == 0))
  {
    first_mark = top_mark;
    add_token_ref(top_mark);
  }

  if (output_routine != 0)
    if (dead_cycles >= max_dead_cycles)
    {
      print_err("Output loop---");
      print_int(dead_cycles);
      prints(" consecutive dead cycles");
      help3("I've concluded that your \\output is awry; it never does",
          "\\ship_out, so I'm shipping \box255 out myself. Next ",
          "increase \\maxdeadcycles if you want me to be more patient!");
      error();
    }
    else
    {
      output_active = true;
      incr(dead_cycles);
      push_nest();
      mode = -vmode;
      prev_depth = ignore_depth;
      mode_line = - (integer) line;
      begin_token_list(output_routine, output_text);
      new_save_level(output_group);
      normal_paragraph();
      scan_left_brace();
      return;
    }

  {
    if (link(page_head) != 0)
    {
      if (link(contrib_head) == 0)
        if (nest_ptr == 0)
          tail = page_tail;
        else
          nest[0].tail_field = page_tail;
      else
        link(page_tail) = link(contrib_head);

      link(contrib_head) = link(page_head);
      link(page_head) = 0;
      page_tail = page_head;
    }

    ship_out(box(255));
    box(255) = 0;
  }
}