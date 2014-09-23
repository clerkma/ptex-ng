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

/* sec 0994 */
void build_page (void)
{
  pointer p;
  pointer q, r;
  integer b, c;
  integer pi;
  /* unsigned char n; */
  unsigned int n;
  scaled delta, h, w;

  if ((link(contrib_head) == 0) || output_active)
    return;

  do
    {
continu:
      p = link(contrib_head);

      if (last_glue != max_halfword)
        delete_glue_ref(last_glue);

      last_penalty = 0;
      last_kern = 0;

      if (type(p) == glue_node)
      {
        last_glue = glue_ptr(p);
        add_glue_ref(last_glue);
      }
      else
      {
        last_glue = max_halfword;

        if (type(p) == penalty_node)
          last_penalty = penalty(p);
        else if (type(p) == kern_node)
          last_kern = width(p);
      }

      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case rule_node:
          if (page_contents < box_there)
          {
            if (page_contents == 0)
              freeze_page_specs(box_there);
            else
              page_contents = box_there;

            q = new_skip_param(top_skip_code);

            if (width(temp_ptr) > height(p))
              width(temp_ptr) = width(temp_ptr) - height(p);
            else
              width(temp_ptr) = 0;

            link(q) = p;
            link(contrib_head) = q;
            goto continu;
          }
          else
          {
            page_total = page_total + page_depth + height(p);
            page_depth = depth(p);
            goto contribute;
          }
          break;

        case whatsit_node:
          goto contribute;
          break;

        case glue_node:
          if (page_contents < box_there)
            goto done1;
          else if (precedes_break(page_tail))
            pi = 0;
          else
            goto update_heights;
          break;

        case kern_node:
          if (page_contents < box_there)
            goto done1;
          else if (link(p) == 0)
            return;
          else if (type(link(p)) == glue_node)
            pi = 0;
          else
            goto update_heights;
          break;

        case penalty_node:
          if (page_contents < box_there)
            goto done1;
          else
            pi = penalty(p);
          break;

        case mark_node:
          goto contribute;
          break;

        case ins_node:
          {
            if (page_contents == 0)
              freeze_page_specs(inserts_only);

            n = subtype(p);
            r = page_ins_head;

            while (n >= subtype(link(r)))
              r = link(r);

            n = n;

            if (subtype(r) != n)
            {
              q = get_node(page_ins_node_size);
              link(q) = link(r);
              link(r) = q;
              r = q;
              subtype(r) = n;
              type(r) = inserting;
              ensure_vbox(n);

              if (box(n) == 0)
                height(r) = 0;
              else
              {
                if (ins_dir(p) != box_dir(box(n)))
                {
                  print_err("Insertions can only be added to a same direction vbox");
                  help3("Tut tut: You're trying to \\insert into a",
                    "\\box register that now have a different direction.",
                    "Proceed, and I'll discard its present contents.");
                  box_error(n);
                }
                else
                  height(r) = height(box(n)) + depth(box(n));
              }

              best_ins_ptr(r) = 0;
              q = skip(n);

              if (count(n) == 1000)
                h = height(r);
              else
                h = x_over_n(height(r), 1000) * count(n);

              page_goal = page_goal - h - width(q);
              page_so_far[2 + stretch_order(q)] = page_so_far[2 + stretch_order(q)] + stretch(q);
              page_shrink = page_shrink + shrink(q);

              if ((shrink_order(q) != normal) && (shrink(q) != 0))
              {
                print_err("Infinite glue shrinkage inserted from ");
                print_esc("skip");
                print_int(n);
                help3("The correction glue for page breaking with insertions",
                    "must have finite shrinkability. But you may proceed,",
                    "since the offensive shrinkability has been made finite.");
                error();
              }
            }

            if (type(r) == split_up)
              insert_penalties = insert_penalties + float_cost(p);
            else
            {
              last_ins_ptr(r) = p;
              delta = page_goal - page_total - page_depth + page_shrink;

              if (count(n) == 1000)
                h = height(p);
              else
                h = x_over_n(height(p), 1000) * count(n);

              if (((h <= 0) || (h <= delta)) && (height(p) + height(r) <= dimen(n)))
              {
                page_goal = page_goal - h;
                height(r) = height(r) + height(p);
              }
              else
              {
                if (count(n) <= 0)
                  w = max_dimen;
                else
                {
                  w = page_goal - page_total - page_depth;

                  if (count(n) != 1000)
                    w = x_over_n(w, count(n)) * 1000;
                }

                if (w > dimen(n) - height(r))
                  w = dimen(n) - height(r);

                q = vert_break(ins_ptr(p), w, depth(p));
                height(r) = height(r) + best_height_plus_depth;
#ifdef STAT
                if (tracing_pages > 0)
                {
                  begin_diagnostic();
                  print_nl("% split");
                  print_int(n);
                  prints(" to");
                  print_scaled(w);
                  print_char(',');
                  print_scaled(best_height_plus_depth);
                  prints(" p=");

                  if (q == 0)
                    print_int(eject_penalty);
                  else if (type(q) == penalty_node)
                    print_int(penalty(q));
                  else
                    print_char('0');

                  end_diagnostic(false);
                }
#endif
                if (count(n) != 1000)
                  best_height_plus_depth = x_over_n(best_height_plus_depth, 1000) * count(n);

                page_goal = page_goal - best_height_plus_depth;
                type(r) = split_up;
                broken_ptr(r) = q;
                broken_ins(r) = p;

                if (q == 0)
                  insert_penalties = insert_penalties + (eject_penalty);
                else if (type(q) == penalty_node)
                  insert_penalties = insert_penalties + penalty(q);
              }
            }
            goto contribute;
          }
          break;

        default:
          {
            confusion("page");
            return;
          }
          break;
      }

      if (pi < inf_penalty)
      {
        if (page_total < page_goal)
          if ((page_so_far[3] != 0) || (page_so_far[4] != 0) || (page_so_far[5] != 0))
            b = 0;
          else
            b = badness(page_goal - page_total, page_so_far[2]);
        else if (page_total - page_goal > page_shrink)
          b = awful_bad;
        else
          b = badness(page_total - page_goal, page_shrink);
  
        if (b < awful_bad)
          if (pi <= eject_penalty)
            c = pi; 
          else if (b < inf_bad)
            c = b + pi + insert_penalties;
          else
            c = deplorable;
        else
          c = b;

        if (insert_penalties >= 10000)
          c = awful_bad;

#ifdef STAT
        if (tracing_pages > 0)
        {
          begin_diagnostic();
          print_nl("%");
          prints(" t=");
          print_totals();
          prints(" g=");
          print_scaled(page_goal);
          prints(" b=");

          if (b == awful_bad)
            print_char('*');
          else
            print_int(b);

          prints(" p=");
          print_int(pi);
          prints(" c=");

          if (c == awful_bad)
            print_char('*');
          else
            print_int(c);

          if (c <= least_page_cost)
            print_char('#');

          end_diagnostic(false);
        }
#endif

        if (c <= least_page_cost)
        {
          best_page_break = p;
          best_size = page_goal;
          least_page_cost = c;
          r = link(page_ins_head);

          while (r != page_ins_head)
          {
            best_ins_ptr(r) = last_ins_ptr(r);
            r = link(r);
          }
        }

        if ((c == awful_bad) || (pi <= eject_penalty))
        {
          fire_up(p);

          if (output_active)
            return;

          goto done;
        }
      }

      if ((type(p) < glue_node) || (type(p) > kern_node))
        goto contribute;

update_heights:
      if (type(p) == kern_node)
        q = p;
      else
      {
        q = glue_ptr(p);
        page_so_far[2 + stretch_order(q)] = page_so_far[2 + stretch_order(q)] + stretch(q);
        page_shrink = page_shrink + shrink(q);

        if ((shrink_order(q) != normal) && (shrink(q) != 0))
        {
          print_err("Infinite glue shrinkage found on current page");
          help4("The page about to be output contains some infinitely",
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

      page_total = page_total + page_depth + width(q);
      page_depth = 0;

contribute:
      if (page_depth > page_max_depth)
      {
        page_total = page_total + page_depth - page_max_depth;
        page_depth = page_max_depth;
      }

      link(page_tail) = p;
      page_tail = p;
      link(contrib_head) = link(p);
      link(p) = 0;
      goto done;

done1:
      link(contrib_head) = link(p);
      link(p) = 0;
      flush_node_list(p);
done:;
    }
  while (!(link(contrib_head) == 0));

  if (nest_ptr == 0)
    tail = contrib_head;
  else
    nest[0].tail_field = contrib_head;
} 
/* sec 1043 */
void app_space (void)
{
  pointer q;

  if ((space_factor >= 2000) && (xspace_skip != zero_glue))
    q = new_param_glue(xspace_skip_code);
  else
  {
    if (space_skip != zero_glue)
      main_p = space_skip;
    else
    {
      main_p = font_glue[cur_font];

      if (main_p == 0)
      {
        main_p = new_spec(zero_glue);
        main_k = param_base[cur_font] + space_code;
        width(main_p) = font_info[main_k].cint;
        stretch(main_p) = font_info[main_k + 1].cint;
        shrink(main_p) = font_info[main_k + 2].cint;
        font_glue[cur_font] = main_p;
      }
    }

    main_p = new_spec(main_p);

    if (space_factor >= 2000)
      width(main_p) = width(main_p) + extra_space(cur_font);

    stretch(main_p) = xn_over_d(stretch(main_p), space_factor, 1000);
    shrink(main_p) = xn_over_d(shrink(main_p), 1000, space_factor);
    q = new_glue(main_p);
    glue_ref_count(main_p) = 0;
  }

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    link(prev_node) = q;
    link(q) = tail;
    prev_node = q;
  }
  else
  {
    link(tail) = q;
    tail = q;
  }
}
/* sec 1047 */
void insert_dollar_sign (void)
{
  back_input();
  cur_tok = math_shift_token + '$';
  print_err("Missing $ inserted");
  help2("I've inserted a begin-math/end-math symbol since I think",
      "you left one out. Proceed, with fingers crossed.");
  ins_error();
}
/* sec 1049 */
void you_cant (void)
{
  print_err("You can't use `");
  print_cmd_chr(cur_cmd, cur_chr);
  prints("' in ");
  print_mode(mode);
}
/* sec 1050 */
void report_illegal_case (void)
{
  you_cant();
  help4("Sorry, but I'm not programmed to handle this case;",
      "I'll just pretend that you didn't ask for it.",
      "If you're in the wrong mode, you might be able to",
      "return to the right one by typing `I}' or `I$' or `I\\par'.");
  error();
}
/* sec 1051 */
boolean privileged (void)
{
  if (mode > 0)
    return true;
  else
  {
    report_illegal_case();
    return false;
  }
}
/* sec 1054 */
boolean its_all_over (void)
{
  if (privileged())
  {
    if ((page_head == page_tail) && (head == tail) && (dead_cycles == 0))
    {
      return true;
    }

    back_input();
    tail_append(new_null_box());
    width(tail) = hsize;
    tail_append(new_glue(fill_glue));
    tail_append(new_penalty(-1073741824L));
    build_page();
  }

  return false;
}
/* sec 1060 */
void append_glue (void)
{
  small_number s;

  s = cur_chr;

  switch (s)
  {
    case fil_code:
      cur_val = fil_glue;
      break;

    case fill_code:
      cur_val = fill_glue;
      break;

    case ss_code:
      cur_val = ss_glue;
      break;

    case fil_neg_code:
      cur_val = fil_neg_glue;
      break;

    case skip_code:
      scan_glue(glue_val);
      break;

    case mskip_code:
      scan_glue(mu_val);
      break;
  }

  tail_append(new_glue(cur_val));
  inhibit_glue_flag = false;

  if (s >= skip_code)
  {
    decr(glue_ref_count(cur_val));

    if (s > skip_code)
      subtype(tail) = mu_glue;
  }
}
/* sec 1061 */
void append_kern (void)
{ 
  quarterword s;

  s = cur_chr;
  scan_dimen((s == mu_glue), false, false);
  inhibit_glue_flag = false;

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    prev_append(new_kern(cur_val));
    subtype(prev_node) = s;
  }
  else
  {
    tail_append(new_kern(cur_val));
    subtype(tail) = s;
  }
}
/* sec 1064 */
void off_save (void)
{
  pointer p;

  if (cur_group == bottom_level)
  {
    print_err("Extra ");
    print_cmd_chr(cur_cmd, cur_chr);
    help1("Things are pretty mixed up, but I think the worst is over.");
    error();
  }
  else
  {
    back_input();
    p = get_avail();
    link(temp_head) = p;
    print_err("Missing ");

    switch (cur_group)
    {
      case semi_simple_group:
        {
          info(p) = cs_token_flag + frozen_end_group;
          print_esc("endgroup");
        }
        break;

      case math_shift_group:
        {
          info(p) = math_shift_token + '$';
          print_char('$');
        }
        break;

      case math_left_group:
        {
          info(p) = cs_token_flag + frozen_right;
          link(p) = get_avail();
          p = link(p);
          info(p) = other_token + '.';
          print_esc("right.");
        }
        break;

      default:
        {
          info(p) = right_brace_token + '}';
          print_char('}');
        }
        break;
    }

    prints(" inserted");
    ins_list(link(temp_head));
    help5("I've inserted something that you may have forgotten.",
        "(See the <inserted text> above.)",
        "With luck, this will get me unwedged. But if you",
        "really didn't forget anything, try typing `2' now; then",
        "my insertion and my current dilemma will both disappear.");
    error();
  }
}
/* sec 1069 */
void extra_right_brace (void)
{
  print_err("Extra }, or forgotten ");

  switch (cur_group)
  {
    case semi_simple_group:
      print_esc("endgroup");
      break;

    case math_shift_group:
      print_char('$');
      break;

    case math_left_group:
      print_esc("right");
      break;
  }

  help5("I've deleted a group-closing symbol because it seems to be",
      "spurious, as in `$x}$'. But perhaps the } is legitimate and",
      "you forgot something else, as in `\\hbox{$x}'. In such cases",
      "the way to recover is to insert both the forgotten and the",
      "deleted material, e.g., by typing `I$}'.");
  error();
  incr(align_state);
}
/* sec 1070 */
void normal_paragraph (void)
{
  if (looseness != 0)
    eq_word_define(int_base + looseness_code, 0);

  if (hang_indent != 0)
    eq_word_define(dimen_base + hang_indent_code, 0);

  if (hang_after != 1)
    eq_word_define(int_base + hang_after_code, 1);

  if (par_shape_ptr != 0)
    eq_define(par_shape_loc, shape_ref, 0);
}
/* sec 1075 */
void box_end (integer box_context)
{
  pointer p;
  pointer q;

  if (box_context < box_flag)
  {
    if (cur_box != 0)
    {
      p = link(cur_box);
      link(cur_box) = null;

      while (p != null)
      {
        q = p;
        p = link(p);

        if (box_dir(q) == abs(direction))
        {
          list_ptr(q) = cur_box;
          cur_box = q;
          link(cur_box) = null;
        }
        else
        {
          delete_glue_ref(space_ptr(q));
          delete_glue_ref(xspace_ptr(q));
          free_node(q, box_node_size);
        }
      }

      if (box_dir(cur_box) != abs(direction))
      {
        cur_box = new_dir_node(cur_box, abs(direction));
      }

      shift_amount(cur_box) = box_context;

      if (abs(mode) == vmode)
      {
        append_to_vlist(cur_box);

        if (adjust_tail != 0)
        {
          if (adjust_head != adjust_tail)
          {
            link(tail) = link(adjust_head);
            tail = adjust_tail;
          }

          adjust_tail = 0;
        }

        if (mode > 0)
          build_page();
      }
      else
      {
        if (abs(mode) == hmode)
          space_factor = 1000;
        else
        {
          p = new_noad();
          math_type(nucleus(p)) = sub_box;
          info(nucleus(p)) = cur_box;
          cur_box = p;
        }

        link(tail) = cur_box;
        tail = cur_box;
      }
    }
  }
  else if (box_context < ship_out_flag)
    if (box_context < (box_flag + 256))
      eq_define((box_base - box_flag) + box_context, box_ref, cur_box);
    else
      geq_define((box_base - box_flag - 256) + box_context, box_ref, cur_box);
  else if (cur_box != 0)
    if (box_context > ship_out_flag)
    {
      do
        {
          get_x_token();
        }
      while (!((cur_cmd != spacer) && (cur_cmd != relax)));

      if (((cur_cmd == hskip) && (abs(mode) != vmode)) ||
        ((cur_cmd == vskip) && (abs(mode) == vmode)))
      {
        append_glue();
        subtype(tail) = box_context - (leader_flag - a_leaders);

        if (type(cur_box) <= dir_node)
        {
          p = link(cur_box);
          link(cur_box) = null;

          while (p != null)
          {
            q = p;
            p = link(p);

            if (box_dir(q) == abs(direction))
            {
              list_ptr(q) = cur_box;
              cur_box = q;
              link(cur_box) = null;
            }
            else
            {
              delete_glue_ref(space_ptr(q));
              delete_glue_ref(xspace_ptr(q));
              free_node(q, box_node_size);
            }
          }

          if (box_dir(cur_box) != abs(direction))
          {
            cur_box = new_dir_node(cur_box, abs(direction));
          }
        }

        leader_ptr(tail) = cur_box;
      }
      else
      {
        print_err("Leaders not followed by proper glue");
        help3("You should say `\\leaders <box or rule><hskip or vskip>'.",
            "I found the <box or rule>, but there's no suitable",
            "<hskip or vskip>, so I'm ignoring these leaders.");
        back_error();
        flush_node_list(cur_box);
      }
    }
    else
      ship_out(cur_box);
}
/* sec 1079 */
void begin_box (integer box_context)
{
  pointer p, q;
  pointer r;
  boolean fd;
  scaled disp, pdisp;
  eight_bits a_dir;
  pointer tx;
  quarterword m;
  halfword k;
  eight_bits n;

  switch (cur_chr)
  {
    case box_code:
      {
        scan_eight_bit_int();
        cur_box = box(cur_val);
        box(cur_val) = 0;
      }
      break;

    case copy_code:
      {
        scan_eight_bit_int();
        cur_box = copy_node_list(box(cur_val));
      }
      break;

    case last_box_code:
      {
        cur_box = 0;

        if (abs(mode) == mmode)
        {
          you_cant();
          help1("Sorry; this \\lastbox will be void.");
          error();
        }
        else if ((mode == vmode) && (head == cur_list.tail_field))
        {
          you_cant();
          help2("Sorry...I usually can't take things from the current page.",
              "This \\lastbox will therefore be void.");
          error();
        }
        else
        {
          check_effective_tail(goto done);

          if (!is_char_node(tx) && (head != tx))
            if ((type(tx) == hlist_node) || (type(tx) == vlist_node) ||
              (type(tx) == dir_node))
            {
              fetch_effective_tail(goto done);
              cur_box = tx;
              shift_amount(cur_box) = 0;

              if (type(cur_box) == dir_node)
              {
                link(list_ptr(cur_box)) = cur_box;
                cur_box = list_ptr(cur_box);
                list_ptr(link(cur_box)) = null;
              }
              else if (box_dir(cur_box) == dir_default)
              {
                set_box_dir(cur_box, abs(direction));
              }
done:;
            }
        }
      }
      break;

    case vsplit_code:
      {
        scan_eight_bit_int();
        n = cur_val;

        if (!scan_keyword("to"))
        {
          print_err("Missing `to' inserted");
          help2("I'm working on `\\vsplit<box number> to <dimen>';",
              "will look for the <dimen> next.");
          error();
        }

        scan_normal_dimen();
        cur_box = vsplit(n, cur_val);
      }
      break;

    default:
      {
        k = cur_chr - vtop_code;
        saved(0) = box_context;
        a_dir = adjust_dir;

        if (k == hmode)
          if ((box_context < box_flag) && (abs(mode) == vmode))
          {
            a_dir = abs(direction);
            scan_spec(adjust_hbox_group, true);
          }
          else
            scan_spec(hbox_group, true);
        else
        {
          if (k == vmode)
            scan_spec(vbox_group, true);
          else
          {
            scan_spec(vtop_group, true);
            k = vmode;
          }

          normal_paragraph();
        }

        push_nest();
        mode = - (integer) k;
        adjust_dir = a_dir;

        if (k == vmode)
        {
          prev_depth = ignore_depth;

          if (every_vbox != 0)
            begin_token_list(every_vbox, every_vbox_text);
        }
        else
        {
          space_factor = 1000;

          if (every_hbox != 0)
            begin_token_list(every_hbox, every_vbox_text);
        }

        return;
      }
      break;
  }

  box_end(box_context);
}
/* sec 1084 */
void scan_box_(integer box_context)
{
  do
    {
      get_x_token(); 
    }
  while (!((cur_cmd != spacer) && (cur_cmd != relax)));

  if (cur_cmd == make_box)
  {
    begin_box(box_context);
  }
  else if ((box_context >= leader_flag) && ((cur_cmd == hrule) || (cur_cmd == vrule)))
  {
    cur_box = scan_rule_spec();
    box_end(box_context);
  }
  else
  {
    print_err("A <box> was supposed to be here");
    help3("I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
        "something like that. So you might find something missing in",
        "your output. But keep trying; you can fix this later.");
    back_error();
  }
}
/* sec 1091 */
small_number norm_min (integer h)
{
  if (h <= 0)
    return 1;
  else if (h >= 63)
    return 63;
  else
    return h;
}
/* sec 1091 */
void new_graf (boolean indented)
{
  prev_graf = 0;

  if ((mode == vmode) || (head != tail))
    tail_append(new_param_glue(par_skip_code));

  inhibit_glue_flag = false;
  push_nest();
  adjust_dir = abs(direction);
  mode = hmode;
  space_factor = 1000;
  set_cur_lang();
  clang = cur_lang;
  prev_graf = (norm_min(left_hyphen_min) * 64 + norm_min(right_hyphen_min)) * 65536L + cur_lang;

  if (indented)
  {
    tail = new_null_box();
    link(head) = tail;
    width(tail) = par_indent;
  }

  if (every_par != 0)
    begin_token_list(every_par, every_par_text);

  if (nest_ptr == 1)
    build_page();
}
/* sec 1093 */
void indent_in_hmode (void)
{
  pointer p, q;

  if (cur_chr > 0)
  {
    p = new_null_box();
    width(p) = par_indent;

    if (abs(mode) == hmode)
      space_factor = 1000;
    else
    {
      q = new_noad();
      math_type(nucleus(q)) = sub_box;
      info(nucleus(q)) = p;
      p = q;
    }

    tail_append(p);
  }
}
/* sec 1095 */
void head_for_vmode (void)
{
  if (mode < 0)
  {
    if (cur_cmd != hrule)
      off_save();
    else
    {
      print_err("You can't use `");
      print_esc("hrule");
      prints("' here except with leaders");
      help2("To put a horizontal rule in an hbox or an alignment,",
          "you should use \\leaders or \\hrulefill (see The TeXbook).");
      error();
    }
  }
  else
  {
    back_input();
    cur_tok = par_token;
    back_input();
    token_type = inserted;
  }
}
/* sec 1096 */
void end_graf (void)
{
  if (mode == hmode)
  {
    if (head == tail)
      pop_nest();
    else
    {
      adjust_hlist(head, true);
      line_break(widow_penalty);
    }

    normal_paragraph();
    error_count = 0;
  }
}
/* sec 1099 */
void begin_insert_or_adjust (void)
{
  if (cur_cmd == vadjust)
    cur_val = 255;
  else
  {
    scan_eight_bit_int();

    if (cur_val == 255)
    {
      print_err("You can't ");
      print_esc("insert");
      print_int(255);
      help1("I'm changing to \\insert0; box 255 is special.");
      error();
      cur_val = 0;
    }
  }

  saved(0) = cur_val;
  incr(save_ptr);
  new_save_level(insert_group);
  scan_left_brace();
  normal_paragraph();
  push_nest();
  mode = -vmode;
  direction = adjust_dir;
  prev_depth = ignore_depth;
}
/* sec 1101 */
void make_mark (void)
{
  pointer p;

  p = scan_toks(false, true);
  p = get_node(small_node_size);
  type(p) = mark_node;
  subtype(p) = 0;
  mark_ptr(p) = def_ref;

  if (!is_char_node(tail) && (type(tail) == disp_node))
    prev_append(p);
  else
    tail_append(p);
}
/* sec 1103 */
void append_penalty (void)
{
  scan_int();

  if (!is_char_node(tail) && (type(tail) == disp_node))
    prev_append(new_penalty(cur_val));
  else
    tail_append(new_penalty(cur_val));

  if (mode == vmode)
    build_page();
}
/* sec 1105 */
void delete_last (void)
{
  pointer p, q;
  pointer r;
  boolean fd;
  scaled disp, pdisp;
  pointer tx;
  quarterword m;

  if ((mode == vmode) && (tail == head))
  {
    if ((cur_chr != glue_node) || (last_glue != max_halfword))
    {
      you_cant();
      help2("Sorry...I usually can't take things from the current page.",
          "Try `I\\vskip-\\lastskip' instead.");

      if (cur_chr == kern_node)
        help_line[0] = "Try `I\\kern-\\last_kern' instead.";
      else if (cur_chr != glue_node)
        help_line[0] = "Perhaps you can make the output routine do it.";
      error();
    }
  }
  else
  {
    check_effective_tail(return);

    if (!is_char_node(tx))
      if (type(tx) == cur_chr)
      {
        fetch_effective_tail(return);
        flush_node_list(tx);
      }
  }
}
/* sec 1110 */
void unpackage (void)
{
  pointer p;
  char c;
  scaled disp;

  c = cur_chr;
  scan_eight_bit_int();
  p = box(cur_val);

  if (p == 0)
    return;

  if (type(p) == dir_node)
    p = list_ptr(p);

  if ((abs(mode) == mmode) || ((abs(mode) == vmode) && (type(p) != vlist_node)) ||
    ((abs(mode) == hmode) && (type(p) != hlist_node)))
  {
    print_err("Incompatible list can't be unboxed");
    help3("Sorry, Pandora. (You sneaky devil.)",
        "I refuse to unbox an \\hbox in vertical mode or vice versa.",
        "And I can't open any boxes in math mode.");
    error();
    return;
  }

  switch (box_dir(p))
  {
    case any_dir:
      if (abs(direction) != box_dir(p))
      {
        print_err("Incompatible direction list can't be unboxed");
        help2("Sorry, Pandora. (You sneaky devil.)",
          "I refuse to unbox a box in differrent direction.");
        error();
        return;
      }
      break;
  }

  disp = 0;

  if (c == copy_code)
    link(tail) = copy_node_list(list_ptr(p));
  else
  {
    if (type(box(cur_val)) == dir_node)
    {
      delete_glue_ref(space_ptr(box(cur_val)));
      delete_glue_ref(xspace_ptr(box(cur_val)));
      free_node(box(cur_val), box_node_size);
    }

    flush_node_list(link(p));
    link(tail) = list_ptr(p);
    box(cur_val) = 0;
    delete_glue_ref(space_ptr(p));
    delete_glue_ref(xspace_ptr(p));
    free_node(p, box_node_size);
  }

  while (link(tail) != 0)
  {
    p = tail;
    tail = link(tail);

    if (!is_char_node(tail))
    {
      switch (type(tail))
      {
        case glue_node:
          if ((subtype(tail) == kanji_skip_code + 1) ||
            (subtype(tail) == xkanji_skip_code + 1))
          {
            link(p) = link(tail);
            delete_glue_ref(glue_ptr(tail));
            free_node(tail, small_node_size);
            tail = p;
          }
          break;
        case penalty_node:
          if (subtype(tail) == widow_pena)
          {
            link(p) = link(tail);
            free_node(tail, small_node_size);
            tail = p;
          }
          break;
        case disp_node:
          {
            prev_disp = disp;
            disp = disp_dimen(tail);
            prev_node = p;
          }
          break;
      }
    }
  }
}
/* sec 1113 */
void append_italic_correction (void)
{
  pointer p;
  internal_font_number f;
  pointer d;

  if (tail != head)
  {
    if (!is_char_node(tail) && (type(tail) == disp_node))
    {
      d = tail;
      tail = prev_node;
    }
    else
    {
      d = null;
    }

    if ((last_jchr != null) && (link(last_jchr) == tail) && is_char_node(tail))
      p = last_jchr;
    else if (is_char_node(tail))
      p = tail;
    else if (type(tail) == ligature_node)
      p = lig_char(tail);
    else
      return;

    f = font(p);
    tail_append(new_kern(char_italic(f, char_info(f, character(p)))));
    subtype(tail) = ita_kern;

    if (d != null)
    {
      prev_node = tail;
      tail_append(d);
    }
  }
}
/* sec 1117 */
void append_discretionary (void)
{
  integer c;

  tail_append(new_disc());

  if (cur_chr == 1)
  {
    c = hyphen_char[cur_font];

    if (c >= 0)
      if (c < 256)
        pre_break(tail) = new_character(cur_font, c);
  }
  else
  {
    incr(save_ptr);
    saved(-1) = 0;
    new_save_level(disc_group);
    scan_left_brace();
    push_nest();
    mode = -hmode;
    space_factor = 1000;
  }
}
/* sec 1119 */
void build_discretionary (void)
{
  pointer p, q;
  integer n;

  unsave();
  q = head;
  p = link(q);
  n = 0;

  while (p != 0)
  {
    if (!is_char_node(p))
      if ((type(p) > rule_node) && (type(p) != kern_node) &&
        (type(p) != ligature_node) && (type(p) != disp_node))
        if ((type(p) == penalty_node) && (subtype(p) != normal))
        {
          link(q) = link(p);
          free_node(p, small_node_size);
          p = q;
        }
        else
        {
          print_err("Improper discretionary list");
          help1("Discretionary lists must contain only boxes and kerns.");
          error();
          begin_diagnostic();
          print_nl("The following discretionary sublist has been deleted:");
          show_box(p);
          end_diagnostic(true);
          flush_node_list(p);
          link(q) = 0;
          goto done;
        }

    q = p;
    p = link(q);
    incr(n);
  }

done:
  p = link(head);
  pop_nest();

  switch (saved(-1))
  {
    case 0:
      pre_break(tail) = p;
      break;

    case 1:
      post_break(tail) = p;
      break;

    case 2:
      {
        if ((n > 0) && (abs(mode) == mmode))
        {
          print_err("Illegal math ");
          print_esc("discretionary");
          help2("Sorry: The third part of a discretionary break must be",
              "empty, in math formulas. I had to delete your third part.");
          flush_node_list(p);
          n = 0;
          error();
        }
        else
          link(tail) = p;

        if (n <= max_quarterword)
          replace_count(tail) = n;
        else
        {
          print_err("Discretionary list is too long");
          help2("Wow---I never thought anybody would tweak me here.",
              "You can't seriously need such a huge discretionary list?");
          error();
        }

        if (n > 0)
          tail = q;

        decr(save_ptr);
        prev_node = tail;
        tail_append(get_node(small_node_size));
        type(tail) = disp_node;
        disp_dimen(tail) = 0;
        prev_disp = 0;
        return;
      }
      break;
  }

  incr(saved(-1));
  new_save_level(disc_group);
  scan_left_brace();
  push_nest();
  mode = -hmode;
  space_factor = 1000;
}
/* sec 1123 */
void make_accent (void)
{
  real s, t;
  scaled disp;
  KANJI_code cx;
  pointer p, q, r;
  internal_font_number f;
  scaled a, h, x, w, delta;
  four_quarters i;

  scan_char_num();

  if (check_echar_range(cur_val) == 0)
  {
    KANJI(cx) = cur_val;

    if (direction == dir_tate)
      f = cur_tfont;
    else
      f = cur_jfont;

    p = new_character(f, get_jfm_pos(KANJI(cx), f));

    if (p != null)
    {
      link(p) = get_avail();
      info(link(p)) = KANJI(cx) + kcat_code(kcatcodekey(cx)) * max_cjk_val;
    }
  }
  else
  {
    f = cur_font;
    p = new_character(f, cur_val);
  }

  if (p != 0)
  {
    x = x_height(f);
    s = slant(f) / ((double) 65536.0);
    a = char_width(f, char_info(f, character(p)));
    do_assignments();
    q = 0;
    f = cur_font;
    KANJI(cx) = 0;

    if ((cur_cmd == letter) || (cur_cmd == other_char))
      q = new_character(f, cur_chr);
    else if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
    {
      if (direction == dir_tate)
        f = cur_tfont;
      else
        f = cur_jfont;

      cx = cur_chr;
    }
    else if (cur_cmd == char_given)
    {
      if (check_echar_range(cur_chr))
        q = new_character(f, cur_chr);
      else
      {
        if (direction == dir_tate)
          f = cur_tfont;
        else
          f = cur_jfont;

        KANJI(cx) = cur_chr;
      }
    }
    else if (cur_cmd == char_num)
    {
      scan_char_num();

      if (check_echar_range(cur_val))
        q = new_character(f, cur_val);
      else
      {
        if (direction == dir_tate)
          f = cur_tfont;
        else
          f = cur_jfont;

        KANJI(cx) = cur_chr;
      }
    }
    else if (cur_cmd == kchar_given)
    {
      if (direction == dir_tate)
        f = cur_tfont;
      else
        f = cur_jfont;
      KANJI(cx) = cur_chr;
    }
    else if (cur_cmd == kchar_num)
    {
      scan_char_num();

      if (direction == dir_tate)
        f = cur_tfont;
      else
        f = cur_jfont;
      KANJI(cx) = cur_val;
    }
    else
      back_input();

    if (direction == dir_tate)
    {
      if (font_dir[f] == dir_tate)
        disp = 0;
      else if (font_dir[f] == dir_yoko)
        disp = t_baseline_shift - y_baseline_shift;
      else
        disp = t_baseline_shift;
    }
    else
    {
      if (font_dir[f] == dir_yoko)
        disp = 0;
      else if (font_dir[f] == dir_tate)
        disp = y_baseline_shift - t_baseline_shift;
      else
        disp = y_baseline_shift;
    }

    append_disp_node_at_begin();

    if (KANJI(cx) != 0)
    {
      q = new_character(f, get_jfm_pos(KANJI(cx), f));
      link(q) = get_avail();
      info(link(q)) = KANJI(cx) + kcat_code(kcatcodekey(cx)) * max_cjk_val;
      last_jchr = q;
    }

    if (q != 0)
    {
      t = slant(f) / ((double) 65536.0);
      i = char_info(f, character(q));
      w = char_width(f, i);
      h = char_height(f, height_depth(i));

      if (h != x)
      {
        delete_glue_ref(cur_kanji_skip);
        delete_glue_ref(cur_xkanji_skip);
        cur_kanji_skip = zero_glue;
        cur_xkanji_skip = zero_glue;
        add_glue_ref(cur_kanji_skip);
        add_glue_ref(cur_xkanji_skip);
        p = hpack(p, 0, 1);
        shift_amount(p) = x - h;
      }

      delta = round((w - a) / ((double) 2.0) + h * t - x * s);
      r = new_kern(delta);
      subtype(r) = acc_kern;
      link(tail) = r;
      link(r) = p;
      tail = new_kern(- (integer) a - delta);
      subtype(tail) = acc_kern;

      if (h == x)
      {
        if (font_dir[font(p)] != dir_default)
          link(link(p)) = tail;
        else
          link(p) = tail;
      }
      else
        link(p) = tail;

      p = q;
    }

    link(tail) = p;

    if (link(p) != null)
      tail = link(p);
    else
      tail = p;

    append_disp_node_at_end();
    space_factor = 1000;
  }
}
/* sec 1127 */
void align_error (void)
{
  if (abs(align_state) > 2)
  {
    print_err("Misplaced ");
    print_cmd_chr(cur_cmd, cur_chr);

    if (cur_tok == tab_token + '&')
    {
      help6("I can't figure out why you would want to use a tab mark",
          "here. If you just want an ampersand, the remedy is",
          "simple: Just type `I\\&' now. But if some right brace",
          "up above has ended a previous alignment prematurely,",
          "you're probably due for more error messages, and you",
          "might try typing `S' now just to see what is salvageable.");
    }
    else
    {
      help5("I can't figure out why you would want to use a tab mark",
          "or \\cr or \\span just now. If something like a right brace",
          "up above has ended a previous alignment prematurely,",
          "you're probably due for more error messages, and you",
          "might try typing `S' now just to see what is salvageable.");
    }

    error();
  }
  else
  {
    back_input();

    if (align_state < 0)
    {
      print_err("Missing { inserted");
      incr(align_state);
      cur_tok = left_brace_token + '{';
    }
    else
    {
      print_err("Missing } inserted");
      decr(align_state);
      cur_tok = right_brace_token + '}';
    }

    help3("I've put in what seems to be necessary to fix",
        "the current column of the current alignment.",
        "Try to go on, since this might almost work.");
    ins_error();
  }
}
/* sec 1129 */
void noalign_error (void)
{
  print_err("Misplaced ");
  print_esc("noalign");
  help2("I expect to see \\noalign only after the \\cr of",
      "an alignment. Proceed, and I'll ignore this case.");
  error();
}
/* sec 1129 */
void omit_error (void)
{
  print_err("Misplaced ");
  print_esc("omit");
  help2("I expect to see \\omit only after tab marks or the \\cr of",
      "an alignment. Proceed, and I'll ignore this case.");
  error();
}
/* sec 1131 */
void do_endv (void)
{
  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;

  while ((input_stack[base_ptr].index_field != v_template) &&
    (input_stack[base_ptr].loc_field == 0) &&
    (input_stack[base_ptr].state_field == token_list))
    decr(base_ptr);

  if ((input_stack[base_ptr].index_field != v_template) ||
    (input_stack[base_ptr].loc_field != 0) ||
    (input_stack[base_ptr].state_field != token_list))
    fatal_error("(interwoven alignment preambles are not allowed)");

  if (cur_group == align_group)
  {
    end_graf();

    if (fin_col())
      fin_row();
  }
  else
    off_save();
}
/* sec 1135 */
void cs_error (void)
{
  print_err("Extra ");
  print_esc("endcsname");
  help1("I'm ignoring this, since I wasn't doing a \\csname."); 
  error();
}
/* sec 1136 */
void push_math (group_code c)
{
  push_nest();
  mode = -mmode;
  incompleat_noad = 0;
  new_save_level(c);
}
/* sec 1138 */
void init_math (void)
{
  scaled w;
  scaled l;
  scaled s;
  pointer p;
  pointer q;
  internal_font_number f;
  integer n;
  scaled v;
  scaled d;

  get_token();

  if ((cur_cmd == math_shift) && (mode > 0))
  {
    if (head == tail)
    {
      pop_nest();
      w = -max_dimen;
    }
    else
    {
      adjust_hlist(head, true);
      line_break(display_widow_penalty);
      v = shift_amount(just_box) + 2 * quad(cur_font);
      w = -max_dimen;
      p = list_ptr(just_box);

      while (p != 0)
      {
reswitch:
        if (is_char_node(p))
        {
          f = font(p);
          d = char_width(f, char_info(f, character(p)));

          if (font_dir[f] != dir_default)
            p = link(p);

          goto found;
        }

        switch (type(p))
        {
          case hlist_node:
          case vlist_node:
          case dir_node:
          case rule_node:
            {
              d = width(p);
              goto found;
            }
            break;

          case ligature_node:
            {
              mem[lig_trick] = mem[lig_char(p)];
              link(lig_trick) = link(p);
              p = lig_trick;
              goto reswitch;
            }
            break;

          case kern_node:
          case math_node:
            d = width(p);
            break;

          case glue_node:
            {
              q = glue_ptr(p);
              d = width(q);

              if (glue_sign(just_box) == stretching)
              {
                if ((glue_order(just_box) == stretch_order(q)) && (stretch(q) != 0))
                  v = max_dimen;
              }
              else if (glue_sign(just_box) == shrinking)
              {
                if ((glue_order(just_box) == shrink_order(q)) && (shrink(q) != 0))
                  v = max_dimen;
              }

              if (subtype(p) >= a_leaders)
                goto found;
            }
            break;

          case whatsit_node:
            d = 0;
            break;

          default:
            d = 0;
            break;
        }

        if (v < max_dimen)
          v = v + d;

        goto not_found;

found:
        if (v < max_dimen)
        {
          v = v + d;
          w = v;
        }
        else
        {
          w = max_dimen;
          goto done;
        }

not_found:
        p = link(p);
      }
done:;
    }

    if (par_shape_ptr == 0)
      if ((hang_indent != 0) && (((hang_after >= 0) &&
        (prev_graf + 2 > hang_after)) || (prev_graf + 1 < - (integer) hang_after)))
      {
        l = hsize - abs(hang_indent);

        if (hang_indent > 0)
          s = hang_indent;
        else
          s = 0;
      }
      else
      {
        l = hsize;
        s = 0;
      }
    else
    {
      n = info(par_shape_ptr);

      if (prev_graf + 2 >= n)
        p = par_shape_ptr + 2 * n;
      else
        p = par_shape_ptr + 2 * (prev_graf + 2);

      s = mem[p - 1].cint;
      l = mem[p].cint;
    }

    push_math(math_shift_group);
    mode = mmode;
    eq_word_define(int_base + cur_fam_code, -1);
    eq_word_define(dimen_base + pre_display_size_code, w);
    eq_word_define(dimen_base + display_width_code, l);
    eq_word_define(dimen_base + display_indent_code, s);

    if (every_display != 0)
      begin_token_list(every_display, every_display_text);

    if (nest_ptr == 1)
    {
      build_page();
    }
  }
  else
  {
    back_input();

    {
      push_math(math_shift_group);
      eq_word_define(int_base + cur_fam_code, -1);

      if (every_math != 0)
        begin_token_list(every_math, every_math_text);
    }
  }

  direction = -abs(direction);
}
/* sec 1142 */
void start_eq_no (void)
{
  saved(0) = cur_chr;
  incr(save_ptr);

  {
    push_math(math_shift_group);
    eq_word_define(int_base + cur_fam_code, -1);

    if (every_math != 0)
      begin_token_list(every_math, every_math_text);
  }
}
/* sec 1151 */
void scan_math (pointer p, pointer q)
{
  integer c;
  KANJI_code cx;

  KANJI(cx) = 0;

restart:
  do
    {
      get_x_token();
    }
  while (!((cur_cmd != spacer) && (cur_cmd != relax)));

reswitch:
  switch (cur_cmd)
  {
    case letter:
    case other_char:
    case char_given:
      if ((cur_chr >= 0) || (cur_chr <= 256))
      {
        c = math_code(cur_chr);

        if (c == 32768L)
        {
          {
            cur_cs = cur_chr + active_base;
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);
            x_token();
            back_input();
          }

          goto restart;
        }
      }
      else
      {
        KANJI(cx) = cur_chr;
      }
      break;

    case kchar_given:
      KANJI(cx) = cur_chr;
      break;

    case kanji:
    case kana:
    case other_kchar:
      cx = cur_chr;
      break;

    case char_num:
      {
        scan_char_num();
        cur_chr = cur_val;
        cur_cmd = char_given;
        goto reswitch;
      }
      break;

    case math_char_num:
      {
        scan_fifteen_bit_int();
        c = cur_val;
      }
      break;

    case math_given:
      c = cur_chr;
      break;

    case delim_num:
      {
        scan_twenty_seven_bit_int();
        c = cur_val / 4096;
      }
      break;

    default:
      {
        back_input();
        scan_left_brace();
        saved(0) = p;
        incr(save_ptr);
        push_math(math_group);
        return;
      }
      break;
  }

  if (KANJI(cx) == 0)
  {
    math_type(p) = math_char;
    character(p) = c % 256;

    if ((c >= var_code) && ((cur_fam >= 0) && (cur_fam < 16)))
      fam(p) = cur_fam;
    else
      fam(p) = (c / 256) % 16;

    if (font_dir[fam_fnt(fam(p) + cur_size)] != dir_default)
    {
      print_err("Not one-byte family");
      help1("IGNORE.");
      error();
    }
  }
  else
  {
    if (q == null)
    {
      math_type(p) = sub_mlist;
      info(p) = new_noad();
      p = nucleus(info(p));
      q = kcode_noad_nucleus(p);
    }

    math_type(p) = math_jchar;
    fam(p) = cur_jfam;
    character(p) = 0;
    math_kcode(p - 1) = KANJI(cx) + kcat_code(kcatcodekey(cx)) * max_cjk_val;

    if (font_dir[fam_fnt(fam(p) + cur_size)] == dir_default)
    {
      print_err("Not two-byte family");
      help1("IGNORE.");
      error();
    }
  }
}
/* sec 1155 */
void set_math_char_(integer c)
{
  pointer p;

  if (c >= 32768L)
  {
    cur_cs = cur_chr + active_base;
    cur_cmd = eq_type(cur_cs);
    cur_chr = equiv(cur_cs);
    x_token();
    back_input();
  }
  else
  {
    p = new_noad();
    math_type(nucleus(p)) = math_char;
    character(nucleus(p)) = c % 256;
    fam(nucleus(p)) = (c / 256) % 16;

    if (c >= var_code)
    {
      if (((cur_fam >= 0) && (cur_fam < 16)))
        fam(nucleus(p)) = cur_fam;

      type(p) = ord_noad;
    }
    else
      type(p) = ord_noad + (c / 4096);

    link(tail) = p;
    tail = p;

    if (font_dir[fam_fnt(fam(nucleus(p)) + cur_size)] != dir_default)
    {
      print_err("Not one-byte family");
      help1("IGNORE.");
      error();
    }

    inhibit_glue_flag = false;
  }
}
/* sec 1159 */
void math_limit_switch (void)
{
  if (head != tail)
    if (type(tail) == op_noad)
    {
      subtype(tail) = cur_chr;
      return;
    }

  print_err("Limit controls must follow a math operator");
  help1("I'm ignoring this misplaced \\limits or \\nolimits command.");
  error();
}
/* sec 1160 */
void scan_delimiter_(pointer p, boolean r)
{
   if (r)
     scan_twenty_seven_bit_int();
   else
   {
     do
      {
        get_x_token();
      }
     while (!((cur_cmd != spacer) && (cur_cmd != relax)));

     switch (cur_cmd)
     {
       case letter:
       case other_char:
         cur_val = del_code(cur_chr);
         break;

       case delim_num:
         scan_twenty_seven_bit_int();
         break;

       default:
         cur_val = -1;
         break;
     }
   }

   if (cur_val < 0)
   {
     print_err("Missing delimiter (. inserted)");
     help6("I was expecting to see something like `(' or `\\{' or",
         "`\\}' here. If you typed, e.g., `{' instead of `\\{', you",
         "should probably delete the `{' by typing `1' now, so that",
         "braces don't get unbalanced. Otherwise just proceed.",
         "Acceptable delimiters are characters whose \\delcode is",
         "nonnegative, or you can use `\\delimiter <delimiter code>'.");
     back_error();
     cur_val = 0;
   }

   small_fam(p) = (cur_val / 1048576L) % 16;
   small_char(p) = (cur_val / 4096) % 256;
   large_fam(p) = (cur_val / 256) % 16;
   large_char(p) = cur_val % 256;
}
/* sec 1163 */
void math_radical (void)
{
  tail_append(get_node(radical_noad_size));
  type(tail) = radical_noad;
  subtype(tail) = normal;
  mem[nucleus(tail)].hh = empty_field;
  mem[subscr(tail)].hh = empty_field;
  mem[supscr(tail)].hh = empty_field;
  scan_delimiter(left_delimiter(tail), true);
  scan_math(nucleus(tail), kcode_noad(tail));
}
/* sec 1165 */
void math_ac (void)
{
  if (cur_cmd == accent)
  {
    print_err("Please use ");
    print_esc("mathaccent");
    prints(" for accents in math mode");
    help2("I'm changing \\accent to \\mathaccent here; wish me luck.",
      "(Accents are not the same in formulas as they are in text.)");
    error();
  }

  tail_append(get_node(accent_noad_size));
  type(tail) = accent_noad;
  subtype(tail) = normal;
  mem[nucleus(tail)].hh = empty_field;
  mem[subscr(tail)].hh = empty_field;
  mem[supscr(tail)].hh = empty_field;
  math_type(accent_chr(tail)) = math_char;
  scan_fifteen_bit_int();
  character(accent_chr(tail)) = cur_val % 256;

  if ((cur_val >= var_code) && ((cur_fam >= 0) && (cur_fam < 16)))
    fam(accent_chr(tail)) = cur_fam;
  else
    fam(accent_chr(tail)) = (cur_val / 256) % 16;

  scan_math(nucleus(tail), kcode_noad(tail));
}
/* sec 1172 */
void append_choices (void)
{
  tail_append(new_choice());
  incr(save_ptr);
  saved(-1) = 0;
  push_math(math_choice_group);
  scan_left_brace();
}
/* sec 1184 */
pointer fin_mlist (pointer p)
{
  pointer q;

  if (incompleat_noad != 0)
  {
    math_type(denominator(incompleat_noad)) = sub_mlist;
    info(denominator(incompleat_noad)) = link(head);

    if (p == 0)
      q = incompleat_noad;
    else
    {
      q = info(numerator(incompleat_noad));

      if (type(q) != left_noad)
      {
        confusion("right");
        return 0;
      }

      info(numerator(incompleat_noad)) = link(q);
      link(q) = incompleat_noad;
      link(incompleat_noad) = p;
    }
  }
  else
  {
    link(tail) = p;
    q = link(head);
  }

  pop_nest();

  return q;
}
/* sec 1174 */
void build_choices (void)
{
  pointer p;

  unsave();
  p = fin_mlist(0);

  switch (saved(-1))
  {
    case 0:
      display_mlist(tail) = p;
      break;

    case 1:
      text_mlist(tail) = p;
      break;

    case 2:
      script_mlist(tail) = p;
      break;

    case 3:
      {
        script_script_mlist(tail) = p;
        decr(save_ptr);
        return;
      }
      break;
  }

  incr(saved(-1));
  push_math(math_choice_group);
  scan_left_brace();
}
/* sec 1176 */
void sub_sup (void)
{
  /* small_number t; */
  int t;
  pointer p;

  t = 0;
  p = 0;
  inhibit_glue_flag = false;

  if (tail != head)
    if (script_allowed(tail))
    {
      p = supscr(tail) + cur_cmd - sup_mark;
      t = math_type(p);
    }

  if ((p == 0) || (t != 0))
  {
    tail_append(new_noad());
    p = supscr(tail) + cur_cmd - sup_mark;

    if (t != 0)
    {
      if (cur_cmd == sup_mark)
      {
        print_err("Double superscript");
        help1("I treat `x^1^2' essentially like `x^1{}^2'.");
      }
      else
      {
        print_err("Double subscript");
        help1("I treat `x_1_2' essentially like `x_1{}_2'.");
      }

      error();
    }
  }

  scan_math(p, null);
}
/* sec 1086 */
void package (small_number c)
{
  scaled h;
  pointer p;
  scaled d;

  d = box_max_depth;
  delete_glue_ref(cur_kanji_skip);
  delete_glue_ref(cur_xkanji_skip);

  if (auto_spacing > 0)
    cur_kanji_skip = kanji_skip;
  else
    cur_kanji_skip = zero_glue;

  if (auto_xspacing > 0)
    cur_xkanji_skip = xkanji_skip;
  else
    cur_xkanji_skip = zero_glue;

  add_glue_ref(cur_kanji_skip);
  add_glue_ref(cur_xkanji_skip);
  unsave();
  save_ptr = save_ptr - 3;

  if (mode == -hmode)
  {
    cur_box = hpack(link(head), saved(2), saved(1));
    set_box_dir(cur_box, abs(direction));
    pop_nest();
  }
  else
  {
    cur_box = vpackage(link(head), saved(2), saved(1), d);
    set_box_dir(cur_box, abs(direction));
    pop_nest();

    if (c == vtop_code)
    {
      h = 0;
      p = list_ptr(cur_box);

      if (p != 0)
        if (type(p) <= rule_node)
          h = height(p);

      depth(cur_box) = depth(cur_box) - h + height(cur_box);
      height(cur_box) = h;
    }
  }

  box_end(saved(0));
}