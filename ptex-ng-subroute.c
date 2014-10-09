/*
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

#define EXTERN extern
#include "ptex-ng.h"

// texk/web2c/lib/uexit.c
void uexit (int unix_code)
{
  int final_code;

  update_terminal();

  if (unix_code == 0)
    final_code = EXIT_SUCCESS;
  else if (unix_code == 1)
    final_code = EXIT_FAILURE;
  else
    final_code = unix_code;

  if (jump_used)
  {
    printf("Jump Buffer already used.\n");
    exit(1);
  }

  jump_used++;
  exit(final_code);
}
// texk/web2c/lib/zround.c
integer web2c_round (double r)
{
  integer i;

  if (r > 2147483647.0)
    i = 2147483647;
  else if (r < -2147483647.0)
    i = -2147483647;
  else if (r >= 0.0)
    i = (integer) (r + 0.5);
  else
    i = (integer) (r - 0.5);

  return i;
}
// Unixify filename and path (turn \ into /)
// --- assumes null terminated
char * unixify (char * t)
{
  char * s = t;

  if (s == NULL)
    return s;

  if (t != '\0')
  {
    while (*s != '\0')
    {
      if (*s == '\\')
        *s = '/';

      s++;
    }
  }

  if (trace_flag)
    printf("Unixified name: %s\n", t);

  return t;
}
// for eTeX
boolean eTeX_enabled(boolean b, quarterword j, halfword k)
{
  if (!b)
  {
    print_err("Improper ");
    print_cmd_chr(j, k);
    help1("Sorry, this optional e-TeX feature has been disabled.");
    error();
  }
  
  return b;
}

void print_group(boolean e)
{
  switch (cur_group)
  {
    case bottom_level:
      {
        prints("bottom level");
        return;
      }
      break;

    case simple_group:
    case semi_simple_group:
      {
        if (cur_group == semi_simple_group)
          prints("semi ");
        prints("simple");
      }
      break;

    case hbox_group:
    case adjusted_hbox_group:
      {
        if (cur_group == adjusted_hbox_group)
          prints("adjusted ");
        prints("hbox");
      }
      break;

    case vbox_group:
      prints("vbox");
      break;

    case vtop_group:
      prints("vtop");
      break;

    case align_group:
    case no_align_group:
      {
        if (cur_group == no_align_group)
          prints("no ");
        prints("align");
      }
      break;

    case output_group:
      prints("output");
      break;

    case disc_group:
      prints("disc");
      break;

    case insert_group:
      prints("insert");
      break;

    case vcenter_group:
      prints("vcenter");
      break;

    case math_group:
    case math_choice_group:
    case math_shift_group:
    case math_left_group:
      {
        prints("math");
        if (cur_group == math_choice_group) prints(" choice");
        else if (cur_group == math_shift_group) prints(" shift");
        else if (cur_group == math_left_group) prints(" left");
      }
      break;
  }

  prints(" group (level ");
  print_int(cur_level);
  print_char(')');

  if (saved(-1) != 0)
  {
    if (e)
      prints(" entered at line ");
    else
      prints(" at line ");

    print_int(saved(-1));
  }
}

#ifdef STAT
void group_trace(boolean e)
{
  begin_diagnostic();
  print_char('{');

  if (e)
    prints("leaving ");
  else
    prints("entering ");

  print_group(e);
  print_char('}');
  end_diagnostic(false);
}
#endif

void show_save_groups(void)
{
  int p;
  int m;
  pointer v;
  quarterword l;
  group_code c;
  int a;
  integer i;
  quarterword j;
  char *s;

  p = nest_ptr;
  nest[p] = cur_list;
  v = save_ptr;
  l = cur_level;
  c = cur_group;
  save_ptr = cur_boundary;
  decr(cur_level);
  a = 1;
  print_nl("");
  print_ln();

  while (true)
  {
    print_nl("### ");
    print_group(true);

    if (cur_group == bottom_level)
      goto done;

    do {
      m = nest[p].mode_field;

      if (p>0)
        decr(p);
      else
        m = vmode;
    } while (!(m != hmode));

    prints(" (");

    switch (cur_group)
    {
      case simple_group:
        {
          incr(p);
          goto found2;
        }
        break;
        
      case hbox_group:
      case adjusted_hbox_group:
        s = "hbox";
        break;

      case vbox_group:
        s = "vbox";
        break;
      case vtop_group:
        s = "vtop";
        break;
      case align_group:
        if (a == 0)
        {
          if (m==-vmode)
            s = "halign";
          else
            s = "valign";

          a = 1;
          goto found1;
        }
        else
        {
          if (a == 1)
            prints("align entry");
          else
            print_esc("cr");

          if (p>=a)
            p = p-a;

          a = 0; goto found;
        }
        break;

      case no_align_group:
        {
          incr(p); a = -1; print_esc("noalign"); goto found2;
        }
        break;

      case output_group:
        {
          print_esc("output"); goto found;
        }
        break;
    
      case math_group:
        goto found2;
        break;
    
      case disc_group:
      case math_choice_group:
        {
          if (cur_group==disc_group)
            print_esc("discretionary");
          else
            print_esc("mathchoice");

          for (i = 1; i <= 3; ++i)
            if (i<=saved(-2))
              prints("{}");
      
          goto found2;
        }
        break;

      case insert_group:
        {
          if (saved(-2)==255)
            print_esc("vadjust");
          else
          {
            print_esc("insert");
            print_int(saved(-2));
          }

          goto found2;
        }
        break;

      case vcenter_group:
        {
          s = "vcenter";
          goto found1;
        }
    
      case semi_simple_group:
        {
          incr(p); print_esc("begingroup"); goto found;
        }
        break;
        
      case math_shift_group:
        {
          if (m ==mmode)
            print_char('$');
          else if (nest[p].mode_field==mmode)
          {
            print_cmd_chr(eq_no,saved(-2)); goto found;
          }
  
          print_char('$'); goto found;
        }
        break;

      case math_left_group:
        {
          if (type(nest[p+1].eTeX_aux_field)==left_noad)
            print_esc("left");
          else
            print_esc("middle");
          goto found;
        }
        break;
    }
  }

i = saved(-4);
if (i != 0)
  if (i<box_flag)
  {
    if (abs(nest[p].mode_field) == vmode)
      j = hmove;
    else
      j = vmove;
    
    if (i>0)
      print_cmd_chr(j,0);
    else
      print_cmd_chr(j,1);
    
    print_scaled(abs(i));
    prints("pt");
  }
  else if (i < ship_out_flag)
  {
    if (i>=global_box_flag)
    {
      print_esc("global");
      i = i-(global_box_flag-box_flag);
    }

    print_esc("setbox");
    print_int(i-box_flag);
    print_char('=');
  }
  else
    print_cmd_chr(leader_ship,i-(leader_flag-a_leaders));

found1:
  print_esc(s);

  if (saved(-2) != 0)
  {
    print_char(' ');

    if (saved(-3)==exactly)
      prints("to");
    else
      prints("spread");

    print_scaled(saved(-2));
    prints("pt");
  }

found2:
  print_char('{');

found:
  print_char(')');
  decr(cur_level);
  cur_group = save_level(save_ptr);
  save_ptr = save_index(save_ptr);

done:
  save_ptr = v;
  cur_level = l;
  cur_group = c;
}

void scan_general_text(void)
{
  int s;
  pointer w;
  pointer d;
  pointer p;
  pointer q;
  halfword unbalance;

  s = scanner_status; w = warning_index; d = def_ref;
  scanner_status = absorbing; warning_index = cur_cs;
  def_ref = get_avail(); token_ref_count(def_ref) = null; p = def_ref;
  scan_left_brace();
  unbalance = 1;

  while (true)
  {
    get_token();

    if (cur_tok < right_brace_limit)
      if (cur_cmd < right_brace)
        incr(unbalance);
      else
      {
        decr(unbalance);

        if (unbalance == 0)
          goto found;
      }

    store_new_token(cur_tok);
  }

found:
  q = link(def_ref);
  free_avail(def_ref);

  if (q == null)
    cur_val = temp_head;
  else
    cur_val = p;

  link(temp_head) = q;
  scanner_status = s;
  warning_index = w;
  def_ref = d;
}

pointer new_edge(small_number s, scaled w)
{
  pointer p;

  p = get_node(edge_node_size);
  type(p) = edge_node;
  subtype(p) = s;
  width(p) = w;
  edge_dist(p) = 0;
  return p;
}

pointer reverse(pointer this_box, pointer t, scaled cur_g, real cur_glue)
{
  pointer l, la;
  scaled disp, disp2;
  boolean disped;
  pointer p;
  pointer q;
  glue_ord g_order;
  int g_sign;
  real glue_temp;
  halfword m, n;

  g_order = glue_order(this_box);
  g_sign = glue_sign(this_box);
  disp = revdisp; disped = false;
  l = t; p = temp_ptr; m = min_halfword; n = min_halfword;

  while (true)
  {
    while (p != null)
reswitch:
    if (is_char_node(p))
      do {
        f = font(p); c = character(p);
        cur_h = cur_h + char_width(f, char_info(f, c));

        if (font_dir[f] != dir_default)
        {
          q = link(p); la = l; l = p; p = link(q); link(q) = la;
        }
        else
        {
          q = link(p); link(p) = l; l = p; p = q;
        }
      } while (!(!is_char_node(p)));
    else
    {
      q = link(p);

      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case rule_node:
        case kern_node:
          rule_wd = width(p);
          break;

        case glue_node:
          {
            round_glue();
            handle_a_glue_node();
          }
          break;

        case ligature_node:
          {
            flush_node_list(lig_ptr(p));
            temp_ptr = p;
            p = get_avail();
            mem[p] = mem[lig_char(temp_ptr)];
            link(p) = q;
            free_node(temp_ptr, small_node_size);
            goto reswitch;
          }
          break;

        case math_node:
          {
            rule_wd = width(p);

            if (end_LR(p))
              if (info(LR_ptr) != end_LR_type(p))
              {
                type(p) = kern_node;
                incr(LR_problems);
              }
              else
              {
                pop_LR();

                if (n>min_halfword)
                {
                  decr(n);
                  decr(subtype(p));
                }
                else
                {
                  type(p) = kern_node;

                  if (m>min_halfword)
                    decr(m);
                  else
                  {
                    free_node(p, small_node_size);
                    link(t) = q;
                    width(t) = rule_wd;
                    edge_dist(t) = -cur_h - rule_wd;
                    goto done;
                  }
                }
              }
            else
            {
              push_LR(p);

              if ((n>min_halfword) || (LR_dir(p) != cur_dir))
              {
                incr(n);
                incr(subtype(p));
              }
              else
              {
                type(p) = kern_node;
                incr(m);
              }
            }
          }
          break;

        case edge_node:
          confusion("LR2");
          break;
          
        case disp_node:
          {
            disp2 = disp_dimen(p);
            disp_dimen(p) = disp;
            disp = disp2;
          
            if (!disped)
              disped = true;
          }
          break;

        default:
          goto next_p;
          break;
      }

      cur_h = cur_h + rule_wd;
next_p:
      link(p) = l;

      if (type(p) == kern_node)
        if ((rule_wd == 0) || (l == null))
        {
          free_node(p, small_node_size);
          p = l;
        }

      l = p;
      p = q;
    }

    if ((t == null) && (m == min_halfword) && (n == min_halfword))
      goto done;

    p = new_math(0, info(LR_ptr));
    LR_problems = LR_problems + 10000;
  }

done:
  if ((l != null) && (type(l) != disp_node))
  {
    p = get_node(small_node_size); type(p) = disp_node;
    disp_dimen(p) = disp; link(p) = l; return p;
  }
  else
    return l;
}

pointer new_segment(small_number s, pointer f)
{
  pointer p;

  p = get_node(segment_node_size);
  type(p) = segment_node;
  subtype(p) = s;
  width(p) = 0;
  segment_first(p) = f;
  segment_last(p) = f;

  return p;
}

void just_copy(pointer p, pointer h, pointer t)
{
  pointer r;
  int words;

  while (p != null)
  {
    words = 1;

    if (is_char_node(p))
      r = get_avail();
    else switch (type(p))
    {
      case dir_node:
      case hlist_node:
      case vlist_node:
        {
          r = get_node(box_node_size);
          mem[r + 7] = mem[p + 7];
          mem[r + 6] = mem[p + 6];
          mem[r + 5] = mem[p + 5];
          add_glue_ref(space_ptr(r));
          add_glue_ref(xspace_ptr(r));
          words = 5;
          list_ptr(r) = null;
        }
        break;

      case rule_node:
        {
          r = get_node(rule_node_size);
          words = rule_node_size;
        }
        break;

      case ligature_node:
        {
          r = get_avail();
          mem[r] = mem[lig_char(p)];
          goto found;
        }
        break;

      case kern_node:
      case math_node:
        {
          r = get_node(small_node_size);
          words = small_node_size;
        }
        break;

      case glue_node:
        {
          r = get_node(small_node_size);
          add_glue_ref(glue_ptr(p));
          glue_ptr(r) = glue_ptr(p);
          leader_ptr(r) = null;
        }
        break;

      case whatsit_node:
        switch (subtype(p))
        {
          case open_node:
            {
              r = get_node(open_node_size);
              words = open_node_size;
            }
            break;

          case write_node:
          case special_node:
            {
              r = get_node(write_node_size);
              add_token_ref(write_tokens(p));
              words = write_node_size;
            }
            break;

          case close_node:
          case language_node:
            {
              r = get_node(small_node_size);
              words = small_node_size;
            }
            break;

          default:
            confusion("ext2");
            break;
        }
        break;

      default:
        goto not_found;
        break;
    }

    while (words>0)
    {
      decr(words);
      mem[r + words] = mem[p + words];
    }

found:
    link(h) = r;
    h = r;
not_found:
    p = link(p);
  }

  link(h) = t;
}

void just_reverse(pointer p)
{
  pointer l;
  pointer t;
  pointer q;
  halfword m, n;

  m = min_halfword;
  n = min_halfword;

  if (link(temp_head) == null)
  {
    just_copy(link(p), temp_head, null);
    q = link(temp_head);
  }
  else
  {
    q = link(p);
    link(p) = null;
    flush_node_list(link(temp_head));
  }

  t = new_edge(cur_dir, 0);
  l = t;
  cur_dir = reflected;

  while (q != null)
    if (is_char_node(q))
      do {
        p = q; q = link(p); link(p) = l; l = p;
      } while (!(!is_char_node(q)));
    else
    {
      p = q; q = link(p);

      if (type(p) == math_node)
        adjust_the_LR_stack_j();

      link(p) = l; l = p;
    }

  goto done;

found:
  width(t) = width(p); link(t) = q;
  free_node(p, small_node_size);
done:
  link(temp_head) = l;
}

void app_display(pointer j, pointer b, scaled d)
{
  scaled z;
  scaled s;
  scaled e;
  integer x;
  pointer p, q, r, t, u;

  s = display_indent;
  x = pre_display_direction;

  if (x == 0)
    shift_amount(b) = s + d;
  else
  {
    z = display_width; p = b;

    if (x>0)
      e = z - d - width(p);
    else
    {
      e = d;
      d = z - e - width(p);
    }

    if (j != null)
    {
      b = copy_node_list(j);
      height(b) = height(p);
      depth(b) = depth(p);
      s = s - shift_amount(b); d = d + s; e = e + width(b) - z - s;
    }

    if (box_lr(p) == dlist)
      q = p;
    else
    {
      r = list_ptr(p); free_node(p, box_node_size);

      if (r == null)
        confusion("LR4");

      if (x>0)
      {
        p = r;

        do {
          q = r; r = link(r);
        } while (!(r == null));
      }
      else
      {
        p = null; q = r;

        do {
          t = link(r); link(r) = p; p = r; r = t;
        } while (!(r == null));
      }
    }

    if (j == null)
    {
      r = new_kern(0);
      t = new_kern(0);
    }
    else
    {
      r = list_ptr(b);
      t = link(r);
    }

    u = new_math(0, end_M_code);

    if (type(t) == glue_node)
    {
      cancel_glue(right_skip_code, q, u, t, e);
      link(u) = t;
    }
    else
    {
      width(t) = e;
      link(t) = u;
      link(q) = t;
    }

    u = new_math(0, begin_M_code);

    if (type(r) == glue_node)
    {
      cancel_glue(left_skip_code, u, p, r, d);
      link(r) = u;
    }
    else
    {
      width(r) = d; link(r) = p;
      link(u) = r;

      if (j == null)
      {
        b = hpack(u, 0, 1);
        shift_amount(b) = s;
      }
      else
        list_ptr(b) = u;
    }
  }

  append_to_vlist(b);
}

void pseudo_start(void)
{
  int old_setting;
  str_number s;
  pool_pointer l, m;
  pointer p, q, r;
  four_quarters w;
  integer nl, sz;

  scan_general_text();
  old_setting = selector;
  selector = new_string;
  token_show(temp_head);
  selector = old_setting;
  flush_list(link(temp_head));
  str_room(1); s = make_string();
  str_pool[pool_ptr] = ' ';
  l = str_start[s];
  nl = new_line_char;
  p = get_avail(); q = p;

  while (l<pool_ptr)
  {
    m = l;

    while ((l<pool_ptr) && (str_pool[l] != nl))
      incr(l);

    sz = (l - m + 7) / 4;

    if (sz == 1)
      sz = 2;

    r = get_node(sz);
    link(q) = r;
    q = r;
    info(q) = sz;

    while (sz>2)
    {
      decr(sz); incr(r);
      w.b0 = str_pool[m]; w.b1 = str_pool[m + 1];
      w.b2 = str_pool[m + 2]; w.b3 = str_pool[m + 3];
      mem[r].qqqq = w; m = m + 4;
    }

    w.b0 = ' '; w.b1 = ' '; w.b2 = ' '; w.b3 = ' ';

    if (l>m)
    {
      w.b0 = str_pool[m];

      if (l>m + 1)
      {
        w.b1 = str_pool[m + 1];

        if (l>m + 2)
        {
          w.b2 = str_pool[m + 2];

          if (l>m + 3)
            w.b3 = str_pool[m + 3];
        }
      }
    }

    mem[r + 1].qqqq = w;

    if (str_pool[l] == nl)
      incr(l);
  }

  info(p) = link(p);
  link(p) = pseudo_files;
  pseudo_files = p;
  flush_string();
  begin_file_reading();
  line = 0; limit = start; loc = limit + 1;

  if (tracing_scan_tokens>0)
  {
    if (term_offset>max_print_line - 3)
      print_ln();
    else if ((term_offset>0) || (file_offset>0))
      print_char(' ');

    name = 19; prints("( "); incr(open_parens);
    update_terminal();
  }
  else
    name = 18;
}

boolean pseudo_input()
{
  pointer p;
  integer sz;
  four_quarters w;
  pointer r;

  last = first;
  p = info(pseudo_files);

  if (p == null)
    return false;
  else
  {
    info(pseudo_files) = link(p);
    sz = info(p);

    if (4 * sz - 3 >= buf_size - last)
      ;//@<Report overflow of the input buffer, and abort@>;

    last = first;

    for (r = p + 1; r <= p + sz - 1; ++r)
    {
      w = mem[r].qqqq;
      buffer[last] = w.b0; buffer[last + 1] = w.b1;
      buffer[last + 2] = w.b2; buffer[last + 3] = w.b3;
      last = last + 4;
    }

    if (last >= max_buf_stack)
      max_buf_stack = last + 1;

    while ((last>first) && (buffer[last - 1] == ' '))
      decr(last);

    free_node(p, sz);
    return true;
  }
}

void pseudo_close(void)
{
  pointer p, q;

  p = link(pseudo_files);
  q = info(pseudo_files);
  free_avail(pseudo_files);
  pseudo_files = p;

  while (q != null)
  {
    p = q; q = link(p); free_node(p, info(p));
  }
}

void get_x_or_protected(void)
{
  while (true)
  {
    get_token();

    if (cur_cmd <= max_command)
      return;

    if ((cur_cmd >= call) && (cur_cmd<end_template))
      if (info(link(cur_chr)) == protected_token)
        return;

    expand();
  }
}

void group_warning(void)
{
  int i;
  boolean w;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  i = in_open; w = false;

  while ((grp_stack[i] == cur_boundary) && (i>0))
  {
    if (tracing_nesting>0)
    {
      while ((input_stack[base_ptr].state_field == token_list) ||
        (input_stack[base_ptr].index_field>i))
        decr(base_ptr);

      if (input_stack[base_ptr].name_field>17)
        w = true;
    }

    grp_stack[i] = save_index(save_ptr);
    decr(i);
  }

  if (w)
  {
    print_nl("Warning: end of ");
    print_group(true);
    prints(" of a different file");
    print_ln();

    if (tracing_nesting>1)
      show_context();

    if (history == spotless)
      history = warning_issued;
  }
}

void if_warning(void)
{
  int i;
  boolean w;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  i = in_open; w = false;

  while (if_stack[i] == cond_ptr)
  {
    if (tracing_nesting>0)
    {
      while ((input_stack[base_ptr].state_field == token_list) ||
        (input_stack[base_ptr].index_field>i))
        decr(base_ptr);

      if (input_stack[base_ptr].name_field>17)
        w = true;
    }

    if_stack[i] = link(cond_ptr); decr(i);
  }

  if (w)
  {
    print_nl("Warning: end of ");
    print_cmd_chr(if_test, cur_if);
    print_if_line(if_line);
    prints(" of a different file");
    print_ln();

    if (tracing_nesting>1)
      show_context();

    if (history == spotless)
      history = warning_issued;
  }
}

void file_warning(void)
{
  pointer p;
  quarterword l;
  quarterword c;
  integer i;

  p = save_ptr; l = cur_level;
  c = cur_group; save_ptr = cur_boundary;

  while (grp_stack[in_open] != save_ptr)
  {
    decr(cur_level);
    print_nl("Warning: end of file when ");
    print_group(true); prints(" is incomplete");
    cur_group = save_level(save_ptr); save_ptr = save_index(save_ptr);
  }

  save_ptr = p; cur_level = l; cur_group = c;
  p = cond_ptr; l = if_limit; c = cur_if; i = if_line;

  while (if_stack[in_open] != cond_ptr)
  {
    print_nl("Warning: end of file when ");
    print_cmd_chr(if_test, cur_if);

    if (if_limit == fi_code)
      print_esc("else");

    print_if_line(if_line); prints(" is incomplete");
    if_line = if_line_field(cond_ptr); cur_if = subtype(cond_ptr);
    if_limit = type(cond_ptr); cond_ptr = link(cond_ptr);
  }

  cond_ptr = p; if_limit = l; cur_if = c; if_line = i;
  print_ln();

  if (tracing_nesting>1)
    show_context();

  if (history == spotless)
    history = warning_issued;
}

void scan_expr(void)
{
  boolean a, b;
  small_number l;
  small_number r;
  small_number s;
  small_number o;
  integer e;
  integer t;
  integer f;
  integer n;
  pointer p;
  pointer q;

  l = cur_val_level; a = arith_error; b = false; p = null;

restart:
  r = expr_none; e = 0; s = expr_none; t = 0; n = 0;

continu:
  if (s == expr_none) o = l; else o = int_val;

  do
  {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok == other_token + '(')
  {
    q = get_node(expr_node_size); link(q) = p; type(q) = l;
    subtype(q) = 4 * s + r;
    expr_e_field(q) = e; expr_t_field(q) = t; expr_n_field(q) = n;
    p = q; l = o; goto restart;
  }

  back_input();

  if (o == int_val) scan_int();
  else if (o == dimen_val) scan_normal_dimen();
  else if (o == glue_val) scan_normal_glue();
  else scan_mu_glue();

  f = cur_val;

found:
  do
  {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok == other_token + '+') o = expr_add;
  else if (cur_tok == other_token + '-') o = expr_sub;
  else if (cur_tok == other_token + '*') o = expr_mult;
  else if (cur_tok == other_token + '/') o = expr_div;
  else
  {
    o = expr_none;

    if (p == null)
    {
      if (cur_cmd != relax)
        back_input();
    }
    else if (cur_tok != other_token + ')')
    {
      print_err("Missing ) inserted for expression");
      help1("I was expecting to see `+', `-', `*', `/', or `)'. Didn't.");
      back_error();
    }
  }

  arith_error = b;

  if ((l == int_val) || (s > expr_sub))
  {
    if ((f>infinity) || (f<-infinity))
      num_error(f);
  }
  else if (l == dimen_val)
  {
    if (abs(f)>max_dimen)
      num_error(f);
  }
  else
  {
    if ((abs(width(f))>max_dimen) ||
      (abs(stretch(f))>max_dimen) ||
      (abs(shrink(f))>max_dimen))
      glue_error(f);
  }

  switch (s)
  {
    case expr_none:
      if ((l >= glue_val) && (o != expr_none))
      {
        t = new_spec(f);
        delete_glue_ref(f);
        normalize_glue(t);
      }
      else
        t = f;
      break;

    case expr_mult:
      if (o == expr_div)
      {
        n = f;
        o = expr_scale;
      }
      else if (l == int_val)
        t = mult_integers(t, f);
      else if (l == dimen_val)
        expr_m(t);
      else
      {
        expr_m(width(t));
        expr_m(stretch(t));
        expr_m(shrink(t));
      }
      break;

    case expr_div:
      if (l < glue_val)
        expr_d(t);
      else
      {
        expr_d(width(t));
        expr_d(stretch(t));
        expr_d(shrink(t));
      }
      break;

    case expr_scale:
      if (l == int_val)
        t = fract(t, n, f, infinity);
      else if (l == dimen_val)
        expr_s(t);
      else
      {
        expr_s(width(t));
        expr_s(stretch(t));
        expr_s(shrink(t));
      }
      break;
  }

  if (o>expr_sub)
    s = o;
  else
  {
    s = expr_none;

    if (r == expr_none) e = t;
    else if (l == int_val) e = expr_add_sub(e, t, infinity);
    else if (l == dimen_val) e = expr_a(e, t);
    else
    {
      width(e) = expr_a(width(e), width(t));

      if (stretch_order(e) == stretch_order(t))
        stretch(e) = expr_a(stretch(e), stretch(t));
      else if ((stretch_order(e)<stretch_order(t)) && (stretch(t) != 0))
      {
        stretch(e) = stretch(t);
        stretch_order(e) = stretch_order(t);
      }

      if (shrink_order(e) == shrink_order(t))
        shrink(e) = expr_a(shrink(e), shrink(t));
      else if ((shrink_order(e)<shrink_order(t)) && (shrink(t) != 0))
      {
        shrink(e) = shrink(t);
        shrink_order(e) = shrink_order(t);
      }

      delete_glue_ref(t);
      normalize_glue(e);
    }

    r = o;
  }

  b = arith_error;

  if (o != expr_none)
    goto continu;

  if (p != null)
  {
    f = e; q = p;
    e = expr_e_field(q); t = expr_t_field(q); n = expr_n_field(q);
    s = subtype(q) / 4; r = subtype(q) % 4;
    l = type(q); p = link(q); free_node(q, expr_node_size);
    goto found;
  }

  if (b)
  {
    print_err("Arithmetic overflow");
    help2("I can't evaluate this expression,",
      "since the result is out of range.");
    error();

    if (l >= glue_val)
    {
      delete_glue_ref(e);
      e = zero_glue; add_glue_ref(e);
    }
    else
      e = 0;
  }

  arith_error = a; cur_val = e; cur_val_level = l;
}

void scan_normal_glue(void)
{
  scan_glue(glue_val);
}

void scan_mu_glue(void)
{
  scan_glue(mu_val);
}

integer add_or_sub(integer x, integer y, integer max_answer, boolean negative)
{
  integer a;

  if (negative)
    negate(y);

  if (x >= 0)
    if (y <= max_answer - x)
      a = x + y;
    else
      num_error(a);
  else if (y >= -max_answer - x)
    a = x + y;
  else
    num_error(a);

  return a;
}

integer quotient(integer n, integer d)
{
  boolean negative;
  integer a;

  if (d == 0)
    num_error(a);
  else
  {
    if (d > 0)
      negative = false;
    else
    {
      negate(d);
      negative = true;
    }

    if (n < 0)
    {
      negate(n);
      negative = !negative;
    }

    a = n / d; n = n - a * d; d = n - d;

    if (d + n >= 0)
      incr(a);

    if (negative)
      negate(a);;
  }

  return a;
}

integer fract(integer x, integer n, integer d, integer max_answer)
{
  boolean negative;
  integer a;
  integer f;
  integer h;
  integer r;
  integer t;

  if (d == 0)
    goto too_big;

  a = 0;

  if (d > 0)
    negative = false;
  else
  {
    negate(d); negative = true;
  }

  if (x < 0)
  {
    negate(x); negative = !negative;
  }
  else if (x == 0)
    goto done;

  if (n < 0)
  {
    negate(n); negative = !negative;
  }

  t = n / d;

  if (t > max_answer / x)
    goto too_big;

  a = t*x; n = n - t*d;

  if (n == 0)
    goto found;

  t = x / d;

  if (t>(max_answer - a) / n)
    goto too_big;

  a = a + t*n; x = x - t*d;

  if (x == 0)
    goto found;

  if (x < n)
  {
    t = x; x = n; n = t;
  }

  f = 0; r = (d / 2) - d; h = -r;

  while (true)
  {
    if (odd(n))
    {
      r = r + x;

      if (r >= 0)
      {
        r = r - d; incr(f);
      }
    }

    n = n / 2;

    if (n == 0)
      goto found1;

    if (x < h)
      x = x + x;
    else
    {
      t = x - d; x = t + x; f = f + n;

      if (x < n)
      {
        if (x == 0)
          goto found1;

        t = x; x = n; n = t;
      }
    }
  }
found1:

  if (f > (max_answer - a))
    goto too_big;

  a = a + f;
found:
  if (negative)
    negate(a);

  goto done;
too_big:
  num_error(a);
done:
  return a;
}

void scan_register_num(void)
{
  scan_int();

  if ((cur_val<0) || (cur_val>max_reg_num))
  {
    print_err("Bad register code");
    help2(max_reg_help_line, "I changed this one to zero.");
    int_error(cur_val); cur_val = 0;
  }
}

void new_index(quarterword i, pointer q)
{
  small_number k;

  cur_ptr = get_node(index_node_size);
  sa_index(cur_ptr) = i;
  sa_used(cur_ptr) = 0;
  link(cur_ptr) = q;

  for (k = 1; k <= index_node_size - 1; ++k)
    mem[cur_ptr + k] = sa_null;
}

void find_sa_element(small_number t, halfword n, boolean w)
{
  pointer q;
  small_number i;

  cur_ptr = sa_root[t];
  if_cur_ptr_is_null_then_return_or_goto(not_found);
  q = cur_ptr; i = hex_dig1(n); get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found1);
  q = cur_ptr; i = hex_dig2(n); get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found2);
  q = cur_ptr; i = hex_dig3(n); get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found3);
  q = cur_ptr; i = hex_dig4(n); get_sa_ptr();

  if ((cur_ptr == null) && w)
    goto not_found4;

  goto exit;

not_found:
  new_index(t, null);
  sa_root[t] = cur_ptr; q = cur_ptr; i = hex_dig1(n);

not_found1:
  new_index(i, q);
  add_sa_ptr(); q = cur_ptr; i = hex_dig2(n);

not_found2:
  new_index(i, q);
  add_sa_ptr(); q = cur_ptr; i = hex_dig3(n);

not_found3:
  new_index(i, q);
  add_sa_ptr(); q = cur_ptr; i = hex_dig4(n);

not_found4:
  if (t == mark_val)
  {
    cur_ptr = get_node(mark_class_node_size);
    mem[cur_ptr + 1] = sa_null; mem[cur_ptr + 2] = sa_null; mem[cur_ptr + 3] = sa_null;
  }
  else
  {
    if (t <= dimen_val)
    {
      cur_ptr = get_node(word_node_size); sa_int(cur_ptr) = 0;
      sa_num(cur_ptr) = n;
    }
    else
    {
      cur_ptr = get_node(pointer_node_size);

      if (t <= mu_val)
      {
        sa_ptr(cur_ptr) = zero_glue; add_glue_ref(zero_glue);
      }
      else sa_ptr(cur_ptr) = null;
    }

    sa_ref(cur_ptr) = null;
  }

  sa_index(cur_ptr) = 16 * t + i; sa_lev(cur_ptr) = level_one;
  link(cur_ptr) = q; add_sa_ptr();
exit:;
}

void delete_sa_ref(pointer q)
{
  pointer p;
  small_number i;
  small_number s;

  decr(sa_ref(q));

  if (sa_ref(q) != null)
    return;

  if (sa_index(q)<dimen_val_limit)
    if (sa_int(q) == 0)
      s = word_node_size;
    else
      return;
  else
  {
    if (sa_index(q)<mu_val_limit)
      if (sa_ptr(q) == zero_glue)
        delete_glue_ref(zero_glue);
      else
        return;
    else if (sa_ptr(q) != null)
      return;

    s = pointer_node_size;
  }

  do {
    i = hex_dig4(sa_index(q));
    p = q; q = link(p); free_node(p, s);

    if (q == null)
    {
      sa_root[i] = null;
      return;
    }

    delete_sa_ptr(); s = index_node_size;
  } while (!(sa_used(q) > 0));
}

void print_sa_num(pointer q)
{
  halfword n;

  if (sa_index(q)<dimen_val_limit)
    n = sa_num(q);
  else
  {
    n = hex_dig4(sa_index(q)); q = link(q); n = n + 16 * sa_index(q);
    q = link(q); n = n + 256 * (sa_index(q) + 16 * sa_index(link(q)));
  }

  print_int(n);
}

#ifdef STAT
void show_sa(pointer p, const char * s)
{
  small_number t;

  begin_diagnostic(); print_char('{'); prints(s); print_char(' ');

  if (p == null)
    print_char('?');
  else
  {
    t = sa_type(p);

    if (t < box_val)
      print_cmd_chr(tex_register, p);
    else if (t == box_val)
    {
      print_esc("box"); print_sa_num(p);
    }
    else if (t == tok_val)
      print_cmd_chr(toks_register, p);
    else
      print_char('?');

    print_char('=');

    if (t == int_val)
      print_int(sa_int(p));
    else if (t == dimen_val)
    {
      print_scaled(sa_dim(p)); prints("pt");
    }
    else
    {
      p = sa_ptr(p);

      if (t == glue_val) print_spec(p, "pt");
      else if (t == mu_val) print_spec(p, "mu");
      else if (t == box_val)
        if (p == null)
          prints("void");
        else
        {
          depth_threshold = 0;
          breadth_max = 1;
          show_node_list(p);
        }
      else if (t == tok_val)
      {
        if (p != null)
          show_token_list(link(p), null, 32);
      }
      else print_char('?');
    }
  }

  print_char('}');
  end_diagnostic(false);
}
#endif

boolean do_marks(small_number a, small_number l, pointer q)
{
  small_number i;

  if (l < 4)
  {
    for (i = 0; i <= 15; ++i)
    {
      get_sa_ptr();

      if (cur_ptr != null)
        if (do_marks(a, l + 1, cur_ptr))
          delete_sa_ptr();
    }

    if (sa_used(q) == 0)
    {
      free_node(q, index_node_size);
      q = null;
    }
  }
  else
  {
    switch (a)
    {
      case fire_up_init:
        if (sa_bot_mark(q) != null)
        {
          if (sa_top_mark(q) != null)
            delete_token_ref(sa_top_mark(q));

          delete_token_ref(sa_first_mark(q));
          sa_first_mark(q) = null;

          if (link(sa_bot_mark(q)) == null)
          {
            delete_token_ref(sa_bot_mark(q));
            sa_bot_mark(q) = null;
          }
          else add_token_ref(sa_bot_mark(q));
          sa_top_mark(q) = sa_bot_mark(q);
        }
        break;

      case fire_up_done:
        if ((sa_top_mark(q) != null) && (sa_first_mark(q) == null))
        {
          sa_first_mark(q) = sa_top_mark(q); add_token_ref(sa_top_mark(q));
        }
        break;

      case destroy_marks:
        for (i = top_mark_code; i <= split_bot_mark_code; ++i)
        {
          get_sa_ptr();

          if (cur_ptr != null)
          {
            delete_token_ref(cur_ptr); put_sa_ptr(null);
          }
        }
        break;
    }

    if (sa_bot_mark(q) == null)
      if (sa_split_bot_mark(q) == null)
      {
        free_node(q, mark_class_node_size);
        q = null;
      }
  }

  return (q == null);
}

void sa_save(pointer p)
{
  pointer q;
  quarterword i;

  if (cur_level != sa_level)
  {
    check_full_save_stack(); save_type(save_ptr) = restore_sa;
    save_level(save_ptr) = sa_level; save_index(save_ptr) = sa_chain;
    incr(save_ptr); sa_chain = null; sa_level = cur_level;
  }

  i = sa_index(p);

  if (i < dimen_val_limit)
  {
    if (sa_int(p) == 0)
    {
      q = get_node(pointer_node_size); i = tok_val_limit;
    }
    else
    {
      q = get_node(word_node_size); sa_int(q) = sa_int(p);
    }

    sa_ptr(q) = null;
  }
  else
  {
    q = get_node(pointer_node_size); sa_ptr(q) = sa_ptr(p);
  }

  sa_loc(q) = p; sa_index(q) = i; sa_lev(q) = sa_lev(p);
  link(q) = sa_chain; sa_chain = q; add_sa_ref(p);
}

void sa_destroy(pointer p)
{
  if (sa_index(p)<mu_val_limit)
    delete_glue_ref(sa_ptr(p));
  else if (sa_ptr(p) != null)
  if (sa_index(p)<box_val_limit)
    flush_node_list(sa_ptr(p));
  else
    delete_token_ref(sa_ptr(p));
}

void sa_def(pointer p, halfword e)
{
  add_sa_ref(p);

  if (sa_ptr(p) == e)
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "reassigning");
#endif

    sa_destroy(p);
  }
  else
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "changing");
#endif

    if (sa_lev(p) == cur_level)
      sa_destroy(p);
    else
      sa_save(p);

    sa_lev(p) = cur_level; sa_ptr(p) = e;

#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "into");
#endif
  }

  delete_sa_ref(p);
}

void sa_w_def(pointer p, integer w)
{
  add_sa_ref(p);

  if (sa_int(p) == w)
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "reassigning");
#endif
  }
  else
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "changing");
#endif

    if (sa_lev(p) != cur_level)
      sa_save(p);

    sa_lev(p) = cur_level; sa_int(p) = w;
#ifdef STAT 
    if (tracing_assigns > 0)
      show_sa(p, "into");
#endif
  }

  delete_sa_ref(p);
}

void gsa_def(pointer p, halfword e)
{
  add_sa_ref(p);
#ifdef STAT
  if (tracing_assigns>0)
    show_sa(p, "globally changing");
#endif

  sa_destroy(p); sa_lev(p) = level_one; sa_ptr(p) = e;

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "into");
#endif
  delete_sa_ref(p);
}

void gsa_w_def(pointer p, integer w)
{
  add_sa_ref(p);
#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "globally changing");
#endif

  sa_lev(p) = level_one; sa_int(p) = w;

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "into");
#endif
  delete_sa_ref(p);
}

void sa_restore(void)
{
  pointer p;

  do {
    p = sa_loc(sa_chain);

    if (sa_lev(p) == level_one)
    {
      if (sa_index(p) >= dimen_val_limit)
        sa_destroy(sa_chain);

#ifdef STAT
      if (tracing_restores>0)
        show_sa(p, "retaining");
#endif
    }
    else
    {
      if (sa_index(p)<dimen_val_limit)
        if (sa_index(sa_chain)<dimen_val_limit)
          sa_int(p) = sa_int(sa_chain);
        else
          sa_int(p) = 0;
      else
        {
        sa_destroy(p); sa_ptr(p) = sa_ptr(sa_chain);
        }

      sa_lev(p) = sa_lev(sa_chain);

#ifdef STAT
      if (tracing_restores>0)
        show_sa(p, "restoring");
#endif
    }

    delete_sa_ref(p);
    p = sa_chain; sa_chain = link(p);

    if (sa_index(p)<dimen_val_limit)
      free_node(p, word_node_size);
    else
      free_node(p, pointer_node_size);
  } while (!(sa_chain == null));
}
