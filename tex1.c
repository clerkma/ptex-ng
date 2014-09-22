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

/* sec 0198 */
void show_box_(pointer p)
{
  depth_threshold = show_box_depth;
  breadth_max = show_box_breadth;

  if (breadth_max <= 0)
    breadth_max = 5;

#ifdef ALLOCATESTRING
  if (pool_ptr + depth_threshold >= current_pool_size)
    str_pool = realloc_str_pool(increment_pool_size);

  if (pool_ptr + depth_threshold >= current_pool_size)
    depth_threshold = current_pool_size - pool_ptr - 1;
#else
  if (pool_ptr + depth_threshold >= pool_size)
    depth_threshold = pool_size - pool_ptr - 1;
#endif

  show_node_list(p);
  print_ln();
}
/* sec 0200 */
void delete_token_ref_(pointer p)
{
  if (token_ref_count(p) == 0)
    flush_list(p);
  else
    decr(token_ref_count(p));
}
/* sec 0201 */
void delete_glue_ref_(pointer p)
{
  if (glue_ref_count(p) == 0)
    free_node(p, glue_spec_size);
  else
    decr(glue_ref_count(p));
}
/* sec 0202 */
void flush_node_list_(pointer p)
{
  pointer q;

  while (is_char_node(p))
  {
    q = link(p);

    if (is_char_node(p))
      free_avail(p);
    else
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case unset_node:
          {
            flush_node_list(list_ptr(p));
            fast_delete_glue_ref(space_ptr(p));
            fast_delete_glue_ref(xspace_ptr(p));
            free_node(p, box_node_size);
            goto done;
          }
          break;

        case rule_node:
          {
            free_node(p, rule_node_size);
            goto done;
          }
          break;

        case ins_node:
          {
            flush_node_list(ins_ptr(p));
            delete_glue_ref(split_top_ptr(p));
            free_node(p, ins_node_size);
            goto done;
          }
          break;

        case whatsit_node:
          {
            switch (subtype(p))
            {
              case open_node:
                free_node(p, open_node_size);
                break;

              case write_node:
              case special_node:
                {
                  delete_token_ref(write_tokens(p));
                  free_node(p, write_node_size);
                  goto done;
                }
                break;

              case close_node:
              case set_language_code:
                free_node(p, small_node_size);
                break;

              default:
                {
                  confusion("ext3");
                  return;
                }
                break;
            }

            goto done;
          }
          break;

        case glue_node:
          {
            fast_delete_glue_ref(p);

            if (leader_ptr(p) != 0)
              flush_node_list(leader_ptr(p));
          }
          break;

        case disp_node:
        case kern_node:
        case math_node:
        case penalty_node:
          do_nothing();
          break;

        case ligature_node:
          flush_node_list(lig_ptr(p));
          break;

        case mark_node:
          delete_token_ref(mark_ptr(p));
          break;

        case disc_node:
          {
            flush_node_list(pre_break(p));
            flush_node_list(post_break(p));
          }
          break;

        case adjust_node:
          flush_node_list(adjust_ptr(p));
          break;

        case style_node:
          {
            free_node(p, style_node_size);
            goto done;
          }
          break;

        case choice_node:
          {
            flush_node_list(display_mlist(p));
            flush_node_list(text_mlist(p));
            flush_node_list(script_mlist(p));
            flush_node_list(script_script_mlist(p));
            free_node(p, style_node_size);
            goto done;
          }
          break;

        case ord_noad:
        case op_noad:
        case bin_noad:
        case rel_noad:
        case open_noad:
        case close_noad:
        case punct_noad:
        case inner_noad:
        case radical_noad:
        case over_noad:
        case under_noad:
        case vcenter_noad:
        case accent_noad:
          {
            if (math_type(nucleus(p)) >= sub_box)
              flush_node_list(info(nucleus(p)));

            if (math_type(supscr(p)) >= sub_box)
              flush_node_list(info(supscr(p)));

            if (math_type(subscr(p)) >= sub_box)
              flush_node_list(info(subscr(p)));

            if (type(p) == radical_noad)
              free_node(p, radical_noad_size);
            else
            {
              if (type(p) == accent_noad)
                free_node(p, accent_noad_size);
              else
                free_node(p, noad_size);
            }

            goto done;
          }
          break;

        case left_noad:
        case right_noad:
          {
            free_node(p, noad_size);
            goto done;
          }
          break;

        case fraction_noad:
          {
            flush_node_list(info(numerator(p)));
            flush_node_list(info(denominator(p)));
            free_node(p, fraction_noad_size);
            goto done;
          }
          break;

        default:
          {
            confusion("flushing");
            return;
          }
          break;
      }

      free_node(p, small_node_size);
done:;
    }

    p = q;
  }
}
/* sec 0204 */
pointer copy_node_list_(pointer p)
{
  pointer h;
  pointer q;
  pointer r;
  char words;

  h = get_avail();
  q = h;

  while (p != 0)
  {
    words = 1;

    if (is_char_node(p)) 
      r = get_avail();
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case unset_node:
        {
          r = get_node(box_node_size);
          mem[r + 7] = mem[p + 7];
          mem[r + 6] = mem[p + 6];
          mem[r + 5] = mem[p + 5];
          add_glue_ref(space_ptr(r));
          add_glue_ref(xspace_ptr(r));
          list_ptr(r) = copy_node_list(list_ptr(p));
          words = 5;
        }
        break;

      case rule_node:
        {
          r = get_node(rule_node_size);
          words = rule_node_size;
        }
        break;

      case ins_node:
        {
          r = get_node(ins_node_size);
          mem[r + 5] = mem[p + 5];
          mem[r + 4] = mem[p + 4];
          add_glue_ref(split_top_ptr(p));
          ins_ptr(r) = copy_node_list(ins_ptr(p));
          words = ins_node_size - 2;
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
            {
              confusion("ext2");
              return 0;
            }
            break;
        }
        break;

      case glue_node:
        {
          r = get_node(small_node_size);
          add_glue_ref(glue_ptr(p));
          glue_ptr(r) = glue_ptr(p);
          leader_ptr(r) = copy_node_list(leader_ptr(p));
        }
        break;

      case disp_node:
      case kern_node:
      case math_node:
      case penalty_node:
        {
          r = get_node(small_node_size);
          words = small_node_size;
        }
        break;

      case ligature_node:
        {
          r = get_node(small_node_size);
          mem[lig_char(r)] = mem[lig_char(p)];
          lig_ptr(r) = copy_node_list(lig_ptr(p));
        }
        break;

      case disc_node:
        {
          r = get_node(small_node_size);
          pre_break(r) = copy_node_list(pre_break(p));
          post_break(r) = copy_node_list(pre_break(p));
        }
        break;

      case mark_node:
        {
          r = get_node(small_node_size);
          add_token_ref(mark_ptr(p));
          words = small_node_size;
        }
        break;

      case adjust_node:
        {
          r = get_node(small_node_size);
          adjust_ptr(r) = copy_node_list(adjust_ptr(p));
        }
        break;

      default:
        {
          confusion("copying");
          return 0;
        }
        break;
    }

    while (words > 0)
    {
      decr(words);
      mem[r + words] = mem[p + words];
    }

    link(q) = r;
    q = r;
    p = link(p);
  }

  link(q) = 0;
  q = link(h);
  free_avail(h);

  return q;
}
/* sec 0211 */
void print_mode_(integer m)
{ 
  if (m > 0)
  {
    switch (m / (max_command + 1))
    {
      case 0:
        prints("vertical");
        break;
      case 1:
        prints("horizontal");
        break;
      case 2:
        prints("display math");
        break;
    }
  }
  else
  {
    if (m == 0)
      prints("no");
    else
    {
      switch ((- (integer) m) / (max_command + 1))
      {
        case 0:
          prints("internal vertical");
          break;
        case 1:
          prints("restricted horizontal");
          break;
        case 2:
          prints("math");
          break;
      }
    }
  }

  prints(" mode");
}
/* sec 0216 */
void push_nest (void) 
{
  if (nest_ptr > max_nest_stack)
  {
    max_nest_stack = nest_ptr;

#ifdef ALLOCATEINPUTSTACK
    if (nest_ptr == current_nest_size)
      nest = realloc_nest_stack(increment_nest_size);

    if (nest_ptr == current_nest_size)
    {
      overflow("semantic nest size", current_nest_size);
      return;
    }
#else
    if (nest_ptr == nest_size)
    {
      overflow("semantic nest size", nest_size);
      return;
    }
#endif
  }

  nest[nest_ptr]= cur_list;
  incr(nest_ptr);
  head = new_null_box();
  tail = head;
  prev_node = tail;
  prev_graf = 0;
  prev_disp = 0;
  last_jchr = null;
  mode_line = line;
}
/* sec 0217 */
void pop_nest (void) 
{
  fast_delete_glue_ref(space_ptr(head));
  fast_delete_glue_ref(xspace_ptr(head));
  free_node(head, box_node_size);
  decr(nest_ptr);
  cur_list = nest[nest_ptr];
}
/* sec 0218 */
void show_activities (void)
{
  integer p;
  short m;
  memory_word a;
  halfword q, r;
  integer t;

  nest[nest_ptr] = cur_list;
  print_nl("");
  print_ln();

  for (p = nest_ptr; p >= 0; p--)
  {
    m = nest[p].mode_field;
    a = nest[p].aux_field;
    print_nl("### ");
    print_direction(nest[p].dir_field);
    prints(", ");
    print_mode(m);
    prints(" entered at line ");
    print_int(abs(nest[p].ml_field));

    if (m == hmode)
    {
      if (nest[p].pg_field != 040600000)
      {
        prints(" (language");
        print_int(nest[p].pg_field % 65536L);
        prints(":hyphenmin");
        print_int(nest[p].pg_field / 4194304L);
        print_char(',');
        print_int((nest[p].pg_field / 65536L) % 64);
        print_char(')');
      }
    }

    if (nest[p].ml_field < 0)
      prints(" (\\output routine)");

    if (p == 0)
    {
      if (page_head != page_tail)
      {
        print_nl("### current page:");
        
        if (output_active)
          prints(" (held over for next output)");

        show_box(link(page_head));

        if (page_contents > 0)
        {
          print_nl("total height ");
          print_totals();
          print_nl(" goal height ");
          print_scaled(page_so_far[0]);
          r = link(page_ins_head);
          
          while (r != mem_top)
          {
            print_ln();
            print_esc("insert");
            t = subtype(r);
            print_int(t);
            prints(" adds ");

            if (count(t) == 1000)
              t = height(r);
            else
              t = x_over_n(height(r), 1000) * count(t);

            print_scaled(t);

            if (type(r) == split_up)
            {
              q = page_head;
              t = 0;

              do
                {
                  q = link(q);

                  if ((type(q) == ins_node) && (subtype(q) == subtype(r)))
                    incr(t);
                }
              while (!(q == broken_ins(r)));

              prints(", #");
              print_int(t);
              prints(" might split");
            }
            r = link(r);
          }
        }
      }

      if (link(contrib_head) != 0)
        print_nl("### recent contributions:");
    }

    show_box(link(nest[p].head_field));

    switch (abs(m) / (max_command + 1))
    {
      case 0:
        {
          print_nl("prevdepth ");

          if  (a.cint <= ignore_depth)
            prints("ignored");
          else
            print_scaled(a.cint);

          if (nest[p].pg_field != 0)
          {
            prints(", prevgraf ");
            print_int(nest[p].pg_field);
            prints(" line");

            if (nest[p].pg_field != 1)
              print_char('s');
          }
        }
        break;

      case 1:
        {
          print_nl("spacefactor ");
          print_int(a.hh.lh);

          if (m > 0)
          {
            if (a.hh.rh > 0)
            {
              prints(", current language ");
              print_int(a.hh.rh);
            }
          }
        }
        break;

      case 2:
        if (a.cint != 0)
        {
          prints("this will be denominator of:");
          show_box(a.cint);
        }
        break;
    }
  }
}
/* sec 0237 */
void print_param_(integer n)
{
  switch (n)
  {
    case pretolerance_code:
      print_esc("pretolerance");
      break;

    case tolerance_code:
      print_esc("tolerance");
      break;

    case line_penalty_code:
      print_esc("linepenalty");
      break;

    case hyphen_penalty_code:
      print_esc("hyphenpenalty");
      break;

    case ex_hyphen_penalty_code:
      print_esc("exhyphenpenalty");
      break;

    case club_penalty_code:
      print_esc("clubpenalty");
      break;

    case widow_penalty_code:
      print_esc("widowpenalty");
      break;

    case display_widow_penalty_code:
      print_esc("displaywidowpenalty");
      break;

    case broken_penalty_code:
      print_esc("brokenpenalty");
      break;

    case bin_op_penalty_code:
      print_esc("binoppenalty");
      break;

    case rel_penalty_code:
      print_esc("relpenalty");
      break;

    case pre_display_penalty_code:
      print_esc("predisplaypenalty");
      break;

    case post_display_penalty_code:
      print_esc("postdisplaypenalty");
      break;

    case inter_line_penalty_code:
      print_esc("interlinepenalty");
      break;

    case double_hyphen_demerits_code:
      print_esc("doublehyphendemerits");
      break;

    case final_hyphen_demerits_code:
      print_esc("finalhyphendemerits");
      break;

    case adj_demerits_code:
      print_esc("adjdemerits");
      break;

    case mag_code:
      print_esc("mag");
      break;

    case delimiter_factor_code:
      print_esc("delimiterfactor");
      break;

    case looseness_code:
      print_esc("looseness");
      break;

    case time_code:
      print_esc("time");
      break;

    case day_code:
      print_esc("day");
      break;

    case month_code:
      print_esc("month");
      break;

    case year_code:
      print_esc("year");
      break;

    case show_box_breadth_code:
      print_esc("showboxbreadth");
      break;

    case show_box_depth_code:
      print_esc("showboxdepth");
      break;

    case hbadness_code:
      print_esc("hbadness");
      break;

    case vbadness_code:
      print_esc("vbadness");
      break;

    case pausing_code:
      print_esc("pausing");
      break;

    case tracing_online_code:
      print_esc("tracingonline");
      break;

    case tracing_macros_code:
      print_esc("tracingmacros");
      break;

    case tracing_stats_code:
      print_esc("tracingstats");
      break;

    case tracing_paragraphs_code:
      print_esc("tracingparagraphs");
      break;

    case tracing_pages_code:
      print_esc("tracingpages");
      break;

    case tracing_output_code:
      print_esc("tracingoutput");
      break;

    case tracing_lost_chars_code:
      print_esc("tracinglostchars");
      break;

    case tracing_commands_code:
      print_esc("tracingcommands");
      break;

    case tracing_restores_code:
      print_esc("tracingrestores");
      break;

    case uc_hyph_code:
      print_esc("uchyph");
      break;

    case output_penalty_code:
      print_esc("outputpenalty");
      break;

    case max_dead_cycles_code:
      print_esc("maxdeadcycles");
      break;

    case hang_after_code:
      print_esc("hangafter");
      break;

    case floating_penalty_code:
      print_esc("floatingpenalty");
      break;

    case global_defs_code:
      print_esc("globaldefs");
      break;

    case cur_fam_code:
      print_esc("fam");
      break;

    case cur_jfam_code:
      print_esc("jfam");
      break;

    case escape_char_code:
      print_esc("escapechar");
      break;

    case default_hyphen_char_code:
      print_esc("defaulthyphenchar");
      break;

    case default_skew_char_code:
      print_esc("defaultskewchar");
      break;

    case end_line_char_code:
      print_esc("endlinechar");
      break;

    case new_line_char_code:
      print_esc("newlinechar");
      break;

    case language_code:
      print_esc("language");
      break;

    case left_hyphen_min_code:
      print_esc("lefthyphenmin");
      break;

    case right_hyphen_min_code:
      print_esc("righthyphenmin");
      break;

    case holding_inserts_code:
      print_esc("holdinginserts");
      break;

    case error_context_lines_code:
      print_esc("errorcontextlines");
      break;

    case jchr_widow_penalty_code:
      print_esc("jcharwidowpenalty");
      break;

    default:
      prints("[unknown integer parameter!]");
      break;
  }
}
/* sec 0245 */
void begin_diagnostic (void)
{
  old_setting = selector;

  if ((tracing_online <= 0) && (selector == term_and_log))
  {
    decr(selector);

    if (history == spotless)
      history = warning_issued;
  }
}
/* sec 0245 */
void end_diagnostic(boolean blank_line)
{
  print_nl("");

  if (blank_line)
    print_ln();

  selector = old_setting;
}
/* sec 0247 */
void print_length_param_ (integer n)
{
  switch (n)
  {
    case par_indent_code:
      print_esc("parindent");
      break;

    case math_surround_code:
      print_esc("mathsurround");
      break;

    case line_skip_limit_code:
      print_esc("lineskiplimit");
      break;

    case hsize_code:
      print_esc("hsize");
      break;

    case vsize_code:
      print_esc("vsize");
      break;

    case max_depth_code:
      print_esc("maxdepth");
      break;

    case split_max_depth_code:
      print_esc("splitmaxdepth");
      break;

    case box_max_depth_code:
      print_esc("boxmaxdepth");
      break;

    case hfuzz_code:
      print_esc("hfuzz");
      break;

    case vfuzz_code:
      print_esc("vfuzz");
      break;

    case delimiter_shortfall_code:
      print_esc("delimitershortfall");
      break;

    case null_delimiter_space_code:
      print_esc("nulldelimiterspace");
      break;

    case script_space_code:
      print_esc("scriptspace");
      break;

    case pre_display_size_code:
      print_esc("predisplaysize");
      break;

    case display_width_code:
      print_esc("displaywidth");
      break;

    case display_indent_code:
      print_esc("displayindent");
      break;

    case overfull_rule_code:
      print_esc("overfullrule");
      break;

    case hang_indent_code:
      print_esc("hangindent");
      break;

    case h_offset_code:
      print_esc("hoffset");
      break;

    case v_offset_code:
      print_esc("voffset");
      break;

    case t_baseline_shift_code:
      print_esc("tbaselineshift");
      break;

    case y_baseline_shift_code:
      print_esc("ybaselineshift");
      break;

    case emergency_stretch_code:
      print_esc("emergencystretch");
      break;

    default:
      prints("[unknown dimen parameter!]");
      break;
  }
}
/* sec 0298 */
void print_cmd_chr_ (quarterword cmd, halfword chr_code)
{
  switch (cmd)
  {
    case left_brace:
      chr_cmd("begin-group character ");
      break;

    case right_brace:
      chr_cmd("end-group character ");
      break;

    case math_shift:
      chr_cmd("math shift character ");
      break;

    case mac_param:
      chr_cmd("macro parameter character ");
      break;

    case sup_mark:
      chr_cmd("superscript character ");
      break;

    case sub_mark:
      chr_cmd("subscript character ");
      break;

    case endv:
      chr_cmd("end of alignment template");
      break;

    case spacer:
      chr_cmd("blank space ");
      break;

    case letter:
      chr_cmd("the letter ");
      break;

    case other_char:
      chr_cmd("the character ");
      break;

    case kanji:
    case kana:
    case other_kchar:
    case hangul:
      prints("kanji character ");
      print_kanji(KANJI(chr_code));
      break;

    case assign_glue:
    case assign_mu_glue:
      if (chr_code < skip_base)
        print_skip_param(chr_code - glue_base);
      else if (chr_code < mu_skip_base)
      {
        print_esc("skip");
        print_int(chr_code - skip_base);
      }
      else
      {
        print_esc("muskip");
        print_int(chr_code - mu_skip_base);
      }
      break;

    case assign_toks:
      if (chr_code >= toks_base)
      {
        print_esc("toks");
        print_int(chr_code - toks_base);
      }
      else
      {
        switch (chr_code)
        {
          case output_routine_loc:
            print_esc("output");
            break;

          case every_par_loc:
            print_esc("everypar");
            break;

          case every_math_loc:
            print_esc("everymath");
            break;

          case every_display_loc:
            print_esc("everydisplay");
            break;

          case every_hbox_loc:
            print_esc("everyhbox");
            break;

          case every_vbox_loc:
            print_esc("everyvbox");
            break;

          case every_job_loc:
            print_esc("everyjob");
            break;

          case every_cr_loc:
            print_esc("everycr");
            break;

          default:
            print_esc("errhelp");
            break;
        }
      }
      break;

    case assign_int:
      if (chr_code < count_base)
        print_param(chr_code - int_base);
      else
      {
        print_esc("count");
        print_int(chr_code - count_base);
      }
      break;

    case assign_dimen:
      if (chr_code < scaled_base)
        print_length_param(chr_code - dimen_base);
      else
      {
        print_esc("dimen");
        print_int(chr_code - scaled_base);
      }
      break;

    case accent:
      print_esc("accent");
      break;

    case advance:
      print_esc("advance");
      break;

    case after_assignment:
      print_esc("afterassignment");
      break;

    case after_group:
      print_esc("aftergroup");
      break;

    case assign_font_dimen:
      print_esc("fontdimen");
      break;

    case begin_group:
      print_esc("begingroup");
      break;

    case break_penalty:
      print_esc("penalty");
      break;

    case char_num:
      print_esc("char");
      break;

    case cs_name:
      print_esc("csname");
      break;

    case def_font:
      print_esc("font");
      break;

    case def_jfont:
      print_esc("jfont");
      break;

    case def_tfont:
      print_esc("tfont");
      break;

    case delim_num:
      print_esc("delimiter");
      break;

    case divide:
      print_esc("divide");
      break;

    case end_cs_name:
      print_esc("endcsname");
      break;

    case end_group:
      print_esc("endgroup");
      break;

    case ex_space:
      print_esc(" ");
      break;

    case expand_after:
      print_esc("expandafter");
      break;

    case halign:
      print_esc("halign");
      break;

    case hrule:
      print_esc("hrule");
      break;

    case ignore_spaces:
      print_esc("ignorespaces");
      break;

    case insert:
      print_esc("insert");
      break;

    case ital_corr:
      print_esc("/");
      break;

    case kchar_num:
      print_esc("kchar");
      break;

    case mark:
      print_esc("mark");
      break;

    case math_accent:
      print_esc("mathaccent");
      break;

    case math_char_num:
      print_esc("mathchar");
      break;

    case math_choice:
      print_esc("mathchoice");
      break;

    case multiply:
      print_esc("multiply");
      break;

    case no_align:
      print_esc("noalign");
      break;

    case no_boundary:
      print_esc("noboundary");
      break;

    case no_expand:
      print_esc("noexpand");
      break;

    case non_script:
      print_esc("nonscript");
      break;

    case omit:
      print_esc("omit");
      break;

    case radical:
      print_esc("radical");
      break;

    case read_to_cs:
      print_esc("read");
      break;

    case relax:
      print_esc("relax");
      break;

    case set_box:
      print_esc("setbox");
      break;

    case set_prev_graf:
      print_esc("prevgraf");
      break;

    case set_shape:
      print_esc("parshape");
      break;

    case the:
      print_esc("the");
      break;

    case toks_register:
      print_esc("toks");
      break;

    case vadjust:
      print_esc("vadjust");
      break;

    case valign:
      print_esc("valign");
      break;

    case vcenter:
      print_esc("vcenter");
      break;

    case vrule:
      print_esc("vrule");
      break;

    case par_end:
      print_esc("par");
      break;

    case input:
      if (chr_code == 0)
        print_esc("input");
      else
        print_esc("endinput");
      break;

    case top_bot_mark:
      switch (chr_code)
      {
        case first_mark_code:
          print_esc("firstmark");
          break;

        case bot_mark_code:
          print_esc("botmark");
          break;

        case split_first_mark_code:
          print_esc("splitfirstmark");
          break;

        case split_bot_mark_code:
          print_esc("splitbotmark");
          break;

        default:
          print_esc("topmark");
          break;
      }
      break;

    case tex_register:
      if (chr_code == int_val)
        print_esc("count");
      else if (chr_code == dimen_val)
        print_esc("dimen");
      else if (chr_code == glue_val)
        print_esc("skip");
      else
        print_esc("muskip");
      break;

    case set_aux:
      if (chr_code == vmode)
        print_esc("prevdepth");
      else
        print_esc("spacefactor");
      break;

    case set_page_int:
      if (chr_code == 0)
        print_esc("deadcycles");
      else
        print_esc("insertpenalties");
      break;

    case set_box_dimen:
      if (chr_code == width_offset)
        print_esc("wd");
      else if (chr_code == height_offset)
        print_esc("ht");
      else
        print_esc("dp");
      break;

    case last_item:
      switch (chr_code)
      {
        case int_val:
          print_esc("lastpenalty");
          break;

        case dimen_val:
          print_esc("lastkern");
          break;

        case glue_val:
          print_esc("lastskip");
          break;

        case input_line_no_code:
          print_esc("inputlineno");
          break;

        default:
          print_esc("badness");
          break;
      }
      break;

    case convert:
      switch (chr_code)
      {
        case number_code:
          print_esc("number");
          break;

        case roman_numeral_code:
          print_esc("romannumeral");
          break;

        case string_code:
          print_esc("string");
          break;

        case meaning_code:
          print_esc("meaning");
          break;

        case font_name_code:
          print_esc("fontname");
          break;
        
        case kansuji_code:
          print_esc("kansuji");
          break;

        case euc_code:
          print_esc("euc");
          break;

        case sjis_code:
          print_esc("sjis");
          break;

        case jis_code:
          print_esc("jis");
          break;
        
        case kuten_code:
          print_esc("kuten");
          break;

        case ucs_code:
          print_esc("ucs");
          break;

        default:
          print_esc("jobname");
          break;
      }
      break;

    case if_test:
      switch (chr_code)
      {
        case if_cat_code:
          print_esc("ifcat");
          break;

        case if_int_code:
          print_esc("ifnum");
          break;

        case if_dim_code:
          print_esc("ifdim");
          break;

        case if_odd_code:
          print_esc("ifodd");
          break;

        case if_vmode_code:
          print_esc("ifvmode");
          break;

        case if_hmode_code:
          print_esc("ifhmode");
          break;

        case if_mmode_code:
          print_esc("ifmmode");
          break;

        case if_inner_code:
          print_esc("ifinner");
          break;

        case if_void_code:
          print_esc("ifvoid");
          break;

        case if_hbox_code:
          print_esc("ifhbox");
          break;

        case if_vbox_code:
          print_esc("ifvbox");
          break;

        case ifx_code:
          print_esc("ifx");
          break;

        case if_eof_code:
          print_esc("ifeof");
          break;

        case if_true_code:
          print_esc("iftrue");
          break;

        case if_false_code:
          print_esc("iffalse");
          break;

        case if_case_code:
          print_esc("ifcase");
          break;
        
        case if_tdir_code:
          print_esc("iftdir");
          break;

        case if_ydir_code:
          print_esc("ifydir");
          break;

        case if_ddir_code:
          print_esc("ifddir");
          break;

        case if_mdir_code:
          print_esc("ifmdir");
          break;

        case if_tbox_code:
          print_esc("iftbox");
          break;

        case if_ybox_code:
          print_esc("ifybox");
          break;

        case if_dbox_code:
          print_esc("ifdbox");
          break;

        default:
          print_esc("if");
          break;
      }
      break;

    case fi_or_else:
      if (chr_code == fi_code)
        print_esc("fi");
      else if (chr_code == or_code)
        print_esc("or");
      else
        print_esc("else");
      break;

    case tab_mark:
      if (chr_code == span_code)
        print_esc("span");
      else
      {
        prints("alignment tab character ");
        print(chr_code);
      }
      break;

    case car_ret:
      if (chr_code == cr_code)
        print_esc("cr");
      else
        print_esc("crcr");
      break;

    case set_page_dimen:
      switch (chr_code)
      {
        case 0:
          print_esc("pagegoal");
          break;

        case 1:
          print_esc("pagetotal");
          break;

        case 2:
          print_esc("pagestretch");
          break;

        case 3:
          print_esc("pagefilstretch");
          break;

        case 4:
          print_esc("pagefillstretch");
          break;

        case 5:
          print_esc("pagefilllstretch");
          break;

        case 6:
          print_esc("pageshrink");
          break;

        default:
          print_esc("pagedepth");
          break;
      }
      break;

    case stop:
      if (chr_code == 1)
        print_esc("dump");
      else
        print_esc("end");
      break;

    case hskip:
      switch (chr_code)
      {
        case skip_code:
          print_esc("hskip");
          break;

        case fil_code:
          print_esc("hfil");
          break;

        case fill_code:
          print_esc("hfill");
          break;

        case ss_code:
          print_esc("hss");
          break;

        default:
          print_esc("hfilneg");
          break;
      }
      break;

    case vskip:
      switch (chr_code)
      {
        case skip_code:
          print_esc("vskip");
          break;

        case fil_code:
          print_esc("vfil");
          break;

        case fill_code:
          print_esc("vfill");
          break;

        case ss_code:
          print_esc("vss");
          break;

        default:
          print_esc("vfilneg");
          break;
      }
      break;

    case mskip:
      print_esc("mskip");
      break;

    case kern:
      print_esc("kern");
      break;

    case mkern:
      print_esc("mkern");
      break;

    case hmove:
      if (chr_code == 1)
        print_esc("moveleft");
      else
        print_esc("moveright");
      break;

    case vmove:
      if (chr_code == 1)
        print_esc("raise");
      else
        print_esc("lower");
      break;

    case make_box:
      switch (chr_code)
      {
        case box_code:
          print_esc("box");
          break;

        case copy_code:
          print_esc("copy");
          break;

        case last_box_code:
          print_esc("lastbox");
          break;

        case vsplit_code:
          print_esc("vsplit");
          break;

        case vtop_code:
          print_esc("vtop");
          break;

        case vtop_code + vmode:
          print_esc("vbox");
          break;

        default:
          print_esc("hbox");
          break;
      }
      break;

    case chg_dir:
      switch (chr_code)
      {
        case dir_yoko:
          print_esc("yoko");
          break;
        case dir_tate:
          print_esc("tate");
          break;
        case dir_dtou:
          print_esc("dtou");
          break;
      }
      break;

    case leader_ship:
      if (chr_code == a_leaders)
        print_esc("leaders");
      else if (chr_code == c_leaders)
        print_esc("cleaders");
      else if (chr_code == x_leaders)
        print_esc("xleaders");
      else
        print_esc("shipout");
      break;

    case start_par:
      if (chr_code == 0)
        print_esc("noindent");
      else
        print_esc("indent");
      break;

    case remove_item:
      if (chr_code == glue_node)
        print_esc("unskip");
      else if (chr_code == kern_node)
        print_esc("unkern");
      else
        print_esc("unpenalty");
      break;

    case un_hbox:
      if (chr_code == copy_code)
        print_esc("unhcopy");
      else
        print_esc("unhbox");
      break;

    case un_vbox:
      if (chr_code == copy_code)
        print_esc("unvcopy");
      else
        print_esc("unvbox");
      break;

    case discretionary:
      if (chr_code == 1)
        print_esc("-");
      else
        print_esc("discretionary");
      break;

    case eq_no:
      if (chr_code == 1)
        print_esc("leqno");
      else
        print_esc("eqno");
      break;

    case math_comp:
      switch (chr_code)
      {
        case ord_noad:
          print_esc("mathord");
          break;

        case op_noad:
          print_esc("mathop");
          break;

        case bin_noad:
          print_esc("mathbin");
          break;

        case rel_noad:
          print_esc("mathrel");
          break;

        case open_noad:
          print_esc("mathopen");
          break;

        case close_noad:
          print_esc("mathclose");
          break;

        case punct_noad:
          print_esc("mathpunct");
          break;

        case inner_noad:
          print_esc("mathinner");
          break;

        case under_noad:
          print_esc("underline");
          break;

        default:
          print_esc("overline");
          break;
      }
      break;

    case limit_switch:
      if (chr_code == limits)
        print_esc("limits");
      else if (chr_code == no_limits)
        print_esc("nolimits");
      else
        print_esc("displaylimits");
      break;

    case math_style:
      print_style(chr_code);
      break;

    case above:
      switch (chr_code)
      {
        case over_code:
          print_esc("over");
          break;

        case atop_code:
          print_esc("atop");
          break;

        case delimited_code + above_code:
          print_esc("abovewithdelims");
          break;

        case delimited_code + over_code:
          print_esc("overwithdelims");
          break;

        case delimited_code + atop_code:
          print_esc("atopwithdelims");
          break;

        default:
          print_esc("above");
          break;
      }
      break;

    case left_right:
      if (chr_code == left_noad)
        print_esc("left");
      else
        print_esc("right");
      break;

    case prefix:
      if (chr_code == 1)
        print_esc("long");
      else if (chr_code == 2)
        print_esc("outer");
      else
        print_esc("global");
      break;

    case def:
      if (chr_code == 0)
        print_esc("def");
      else if (chr_code == 1)
        print_esc("gdef");
      else if (chr_code == 2)
        print_esc("edef");
      else
        print_esc("xdef");
      break;

    case let:
      if (chr_code != normal)
        print_esc("futurelet");
      else
        print_esc("let");
      break;

    case shorthand_def:
      switch (chr_code)
      {
        case char_def_code:
          print_esc("chardef");
          break;

        case kchar_def_code:
          print_esc("kchardef");
          break;

        case math_char_def_code:
          print_esc("mathchardef");
          break;

        case count_def_code:
          print_esc("countdef");
          break;

        case dimen_def_code:
          print_esc("dimendef");
          break;

        case skip_def_code:
          print_esc("skipdef");
          break;

        case mu_skip_def_code:
          print_esc("muskipdef");
          break;

        default:
          print_esc("toksdef");
          break;
      }
      break;

    case char_given:
      print_esc("char");
      print_hex(chr_code);
      break;

    case kchar_given:
      print_esc("kchar");
      print_hex(chr_code);
      break;

    case math_given:
      print_esc("mathchar");
      print_hex(chr_code);
      break;

    case def_code:
      if (chr_code == cat_code_base)
        print_esc("catcode");
      else if (chr_code == kcat_code_base)
        print_esc("kcatcode");
      else if (chr_code == auto_xsp_code_base)
        print_esc("xspcode");
      else if (chr_code == math_code_base)
        print_esc("mathcode");
      else if (chr_code == lc_code_base)
        print_esc("lccode");
      else if (chr_code == uc_code_base)
        print_esc("uccode");
      else if (chr_code == sf_code_base)
        print_esc("sfcode");
      else
        print_esc("delcode");
      break;

    case def_family:
      print_size(chr_code - math_font_base);
      break; 

    case hyph_data:
      if (chr_code == 1)
        print_esc("patterns");
      else
        print_esc("hyphenation");
      break;

    case assign_font_int:
      if (chr_code == 0)
        print_esc("hyphenchar");
      else
        print_esc("skewchar");
      break;

    case set_font:
      prints("select font ");
      slow_print(font_name[chr_code]);

      if (font_size[chr_code] != font_dsize[chr_code])
      {
        prints(" at ");
        print_scaled(font_size[chr_code]);
        prints("pt");
      }
      break;

    case set_interaction:
      switch (chr_code)
      {
        case batch_mode:
          print_esc("batchmode");
          break;

        case nonstop_mode:
          print_esc("nonstopmode");
          break;

        case scroll_mode:
          print_esc("scrollmode");
          break;

        default:
          print_esc("errorstopmode");
          break;
      }
      break;

    case in_stream:
      if (chr_code == 0)
        print_esc("closein");
      else
        print_esc("openin");
      break;

    case message:
      if (chr_code == 0)
        print_esc("message");
      else
        print_esc("errmessage");
      break;

    case case_shift:
      if (chr_code == lc_code_base)
        print_esc("lowercase");
      else
        print_esc("uppercase");
      break;

    case xray:
      switch (chr_code)
      {
        case show_box_code:
          print_esc("showbox");
          break;

        case show_the_code:
          print_esc("showthe");
          break;

        case show_lists:
          print_esc("showlists");
          break;

        case show_mode:
          print_esc("showmode");
          break;

        default:
          print_esc("show");
          break;
      }
      break;

    case undefined_cs:
      prints("undefined");
      break;

    case call:
      prints("macro");
      break;

    case long_call:
      print_esc("long macro");
      break;

    case outer_call:
      print_esc("outer macro");
      break;

    case long_outer_call:
      print_esc("long");
      print_esc("outer macro");
      break;

    case end_template:
      print_esc("outer endtemplate");
      break;

    case extension:
      switch (chr_code)
      {
        case open_node:
          print_esc("openout");
          break;

        case write_node:
          print_esc("write");
          break;

        case close_node:
          print_esc("closeout");
          break;

        case special_node:
          print_esc("special");
          break;

        case immediate_code:
          print_esc("immediate");
          break;

        case set_language_code:
          print_esc("setlanguage");
          break;

        default:
          prints("[unknown extension!]");
          break;
      }
      break;

    case set_kansuji_char:
      print_esc("kansujichar");
      break;

    case set_auto_spacing:
      {
        if ((chr_code % 2) == 0)
          print_esc("noauto");
        else
          print_esc("auto");

        if (chr_code < 2)
          prints("spacing");
        else
          prints("xspacing");
      }
      break;

    case set_enable_cjk_token:
      {
      if (chr_code == 0)
        print_esc("enable");
      else if (chr_code == 1)
        print_esc("disable");
      else
        print_esc("force");

      prints("cjktoken");
      }
      break;

    case inhibit_glue:
      print_esc("inhibitglue");
      break;
    
    case assign_inhibit_xsp_code:
      print_esc("inhibitxspcode");
      break;

    case assign_kinsoku:
      switch (chr_code)
      {
        case pre_break_penalty_code:
          print_esc("prebreakpenalty");
          break;
        case post_break_penalty_code:
          print_esc("postbreakpenalty");
          break;
      }
      break;

    default:
      prints("[unknown command code!]");
      break;
  }
}
#ifdef STAT
/* sec 0252 */
void show_eqtb (pointer n)
{ 
  if (n < active_base)
    print_char('?');
  else if (n < glue_base)
  {
    sprint_cs(n);
    print_char('=');
    print_cmd_chr(eq_type(n), equiv(n));
    
    if (eq_type(n) >= call)
    {
      print_char(':');
      show_token_list(link(equiv(n)), 0, 32);
    }
  }
  else if (n < local_base)
    if (n < skip_base)
    {
      print_skip_param(n - glue_base);
      print_char('=');
      
      if (n < glue_base + thin_mu_skip_code)
        print_spec(equiv(n), "pt");
      else
        print_spec(equiv(n), "mu");
    }
    else if (n < mu_skip_base)
    {
      print_esc("skip");
      print_int(n - skip_base);
      print_char('=');
      print_spec(equiv(n), "pt");
    }
    else
    {
      print_esc("muskip");
      print_int(n - mu_skip_base);
      print_char('=');
      print_spec(equiv(n), "mu");
    }
  else if (n < int_base)
    if (n == par_shape_loc)
    {
      print_esc("parshape");
      print_char('=');
      
      if (par_shape_ptr == 0)
        print_char('0');
      else
        print_int(info(par_shape_ptr));
    }
    else if (n < toks_base)
    {
      print_cmd_chr(assign_toks, n);
      print_char('=');
      
      if (equiv(n) != 0)
        show_token_list(link(equiv(n)), 0, 32);
    }
    else if (n < box_base)
    {
      print_esc("toks");
      print_int(n - toks_base);
      print_char('=');
      
      if (equiv(n) != 0)
        show_token_list(link(equiv(n)), 0, 32);
    }
    else if (n < cur_font_loc)
    {
      print_esc("box");
      print_int(n - box_base);
      print_char('=');
      
      if (equiv(n) == 0)
        prints("void");
      else
      {
        depth_threshold = 0;
        breadth_max = 1;
        show_node_list(equiv(n));
      }
    }
    else if (n < cat_code_base)
    {
      if (n == cur_font_loc)
        prints("current font");
      else if (n < math_font_base + 16)
      {
        print_esc("textfont");
        print_int(n - math_font_base);
      }
      else if (n < math_font_base + 32)
      {
        print_esc("scriptfont");
        print_int(n - math_font_base - 16);
      }
      else
      {
        print_esc("scriptscriptfont");
        print_int(n - math_font_base - 32);
      }
      
      print_char('=');
      print_esc("");
      print(hash[font_id_base + equiv(n)].rh);
    }
    else if (n < math_code_base)
    {
      if (n < kcat_code_base)
      {
        print_esc("catcode");
        print_int(n - cat_code_base);
      }
      else if (n < auto_xsp_code_base)
      {
        print_esc("kcatcode");
        print_int(n - kcat_code_base);
      }
      else if (n < inhibit_xsp_code_base)
      {
        print_esc("xspcode");
        print_int(n - auto_xsp_code_base);
      }
      else if (n < kinsoku_base)
      {
        print_esc("(inhibitxspcode table) ");
        print_int(n - inhibit_xsp_code_base);
      }
      else if (n < kansuji_base)
      {
        print_esc("(kinsoku table) ");
        print_int(n - kinsoku_base);
      }
      else if (n < lc_code_base)
      {
        print_esc("kansujichar");
        print_int(n - kansuji_base);
      }
      else if (n < uc_code_base)
      {
        print_esc("lccode");
        print_int(n - lc_code_base);
      }
      else if (n < sf_code_base)
      {
        print_esc("uccode");
        print_int(n - uc_code_base);
      }
      else
      {
        print_esc("sfcode");
        print_int(n - sf_code_base);
      }
      
      print_char('=');
      print_int(equiv(n));
    }
    else
    {
      print_esc("mathcode");
      print_int(n - math_code_base);
      print_char('=');
      print_int(equiv(n));
    }
  else if (n < dimen_base)
  {
    if (n < count_base)
      print_param(n - int_base);
    else if (n < del_code_base)
    {
      print_esc("count");
      print_int(n - count_base);
    }
    else
    {
      print_esc("delcode");
      print_int(n - del_code_base);
    }
    
    print_char('=');
    print_int(eqtb[n].cint);
  }
  else if (n <= kinsoku_penalty_base)
  {
    if (n < scaled_base)
      print_length_param(n - dimen_base);
    else
    {
      print_esc("dimen");
      print_int(n - scaled_base);
    }
    
    print_char('=');
    print_scaled(eqtb[n].cint);
    prints("pt");
  }
  else if (n <= eqtb_size)
  {
    prints("kinsoku");
  }
  else
  {
    print_char('?');
  }
}
#endif
/* sec 0259 */
pointer id_lookup_(integer j, integer l)
{
  integer h;
  integer d;
  pointer p;
  pointer k;

  h = buffer[j];

  for (k = j + 1; k <= j + l - 1; k++)
  {
    h = h + h + buffer[k];

    while (h >= hash_prime)
      h = h - hash_prime;
  }

  p = h + hash_base;

  while (true)
  {
    if (text(p) > 0)
      if (length(text(p)) == l)
        if (str_eq_buf(text(p), j))
          goto found;

    if (next(p) == 0)
    {
      if (no_new_control_sequence)
        p = undefined_control_sequence;
      else
      {
        if (text(p) > 0)
        {
          do
            {
              if (hash_is_full)
              {
                overflow("hash size", hash_size + hash_extra);
                /* not dynamic        ^~~~~~~~~~~~~~~~~~~~~~*/
                return 0;
              }

              decr(hash_used);
            }
          while (!(text(hash_used) == 0));

          next(p) = hash_used;
          p = hash_used;
        }

        str_room(l);
        d = cur_length;

        while (pool_ptr > str_start[str_ptr])
        {
          decr(pool_ptr);
          str_pool[pool_ptr + l] = str_pool[pool_ptr];
        }

        for (k = j; k <= j + l - 1; k++)
          append_char(buffer[k]);

        text(p) = make_string();
        pool_ptr = pool_ptr + d;

#ifdef STAT
        incr(cs_count);

        if (trace_flag)
        {
          str_pool[pool_ptr] = '\0';
          printf(" tex1.c cs_count: '%s' ", &str_pool[pool_ptr - l - d]);
        }
#endif
      }

      goto found;
    }

    p = next(p);
  } 

found:
  return p;
}
/* sec 0274 */
void new_save_level (group_code c)
{
  check_full_save_stack();
  save_type(save_ptr) = level_boundary;
  save_level(save_ptr) = (quarterword) cur_group; 
  save_index(save_ptr) = cur_boundary;

  if (cur_level == max_quarterword)
  {
    overflow("grouping levels", max_quarterword - min_quarterword);
    return;
  }

  cur_boundary = save_ptr;
  incr(cur_level);
  incr(save_ptr);
  cur_group = c;
}
/* sec 0275 */
void eq_destroy (memory_word w)
{
  pointer q;

  switch (eq_type_field(w))
  {
    case call:
    case long_call:
    case outer_call:
    case long_outer_call:
      delete_token_ref(equiv_field(w));
      break;

    case glue_ref:
      delete_glue_ref(equiv_field(w));
      break;

    case shape_ref:
      q = equiv_field(w);

      if (q != 0)
        free_node(q, info(q) + info(q) + 1);
      break;

    case box_ref:
      flush_node_list(equiv_field(w));
      break;

    default:
      break;
  }
}
/* sec 0276 */
void eq_save (pointer p, quarterword l)
{
  check_full_save_stack();

  if (l == level_zero)
    save_type(save_ptr) = restore_zero;
  else
  {
    save_stack[save_ptr] = eqtb[p];
    incr(save_ptr);
    save_type(save_ptr) = restore_old_value;
  }

  save_level(save_ptr) = l;
  save_index(save_ptr) = p;
  incr(save_ptr);
}
/* sec 0277 */
void eq_define_(pointer p, quarterword t, halfword e)
{
  if (eq_level(p) == cur_level)
    eq_destroy(eqtb[p]);
  else if (cur_level > level_one)
    eq_save(p, eq_level(p));

  eq_level(p) = cur_level;
  eq_type(p) = t;
  equiv(p) = e;
}
/* sec 0278 */
void eq_word_define_(pointer p, integer w)
{
  if (xeq_level[p] != cur_level)
  {
    eq_save(p, xeq_level[p]);
    xeq_level[p] = cur_level;
  }

  eqtb[p].cint = w;
}
/* sec 0279 */
void geq_define_(pointer p, quarterword t, halfword e)
{
  eq_destroy(eqtb[p]);
  eq_level(p) = level_one;
  eq_type(p) = t;
  equiv(p) = e;
}
/* sec 0279 */
void geq_word_define_(pointer p, integer w)
{
  eqtb[p].cint = w;
  xeq_level[p]= level_one;
}
/* sec 0280 */
void save_for_after (halfword t)
{ 
  if (cur_level > level_one)
  {
    check_full_save_stack();
    save_type(save_ptr) = insert_token;
    save_level(save_ptr) = level_zero;
    save_index(save_ptr) = t;
    incr(save_ptr);
  }
}