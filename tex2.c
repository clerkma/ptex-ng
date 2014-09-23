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

/* following bit used to be end of tex1.c */
#ifdef STAT
/* sec 0284 */
void restore_trace (pointer p, const char * s)
{
  begin_diagnostic();
  print_char('{');
  prints(s);
  print_char(' ');
  show_eqtb(p);
  print_char('}');
  end_diagnostic(false);
}
#endif
/* sec 0281 */
void unsave (void)
{
  pointer p;
  quarterword l;
  halfword t;

  if (cur_level > level_one)
  {
    decr(cur_level);

    while (true)
    {
      decr(save_ptr);

      if (save_type(save_ptr) == level_boundary)
        goto done;

      p = save_index(save_ptr);

      if (save_type(save_ptr) == insert_token)
      {
        t = cur_tok;
        cur_tok = p;
        back_input();
        cur_tok = t;
      }
      else
      {
        if (save_type(save_ptr) == restore_old_value)
        {
          l = save_level(save_ptr);
          decr(save_ptr);
        }
        else
          save_stack[save_ptr] = eqtb[undefined_control_sequence];
        
        if (p < int_base)
          if (eq_level(p) == level_one)
          {
            eq_destroy(save_stack[save_ptr]);
#ifdef STAT
            if (tracing_restores > 0)
              restore_trace(p, "retaining");
#endif
          }
          else
          {
            eq_destroy(eqtb[p]);
            eqtb[p] = save_stack[save_ptr];
#ifdef STAT
            if (tracing_restores > 0)
              restore_trace(p, "restoring");
#endif
          }
        else if (xeq_level[p] != level_one)
        {
          eqtb[p] = save_stack[save_ptr];
          xeq_level[p] = l;
#ifdef STAT
          if (tracing_restores > 0)
            restore_trace(p, "restoring");
#endif
        }
        else
        {
#ifdef STAT
          if (tracing_restores > 0)
            restore_trace(p, "retaining");
#endif
        }
      }
    }

done:
    cur_group = save_level(save_ptr);
    cur_boundary = save_index(save_ptr);
  }
  else
  {
    confusion("curlevel");
    return;
  }
}
/* sec 0288 */
void prepare_mag (void) 
{
  if ((mag_set > 0) && (mag != mag_set))
  {
    print_err("Incompatible magnification (");
    print_int(mag);
    prints(");");
    print_nl(" the previous value will be retained");
    help2("I can handle only one magnification ratio per job. So I've",
        "reverted to the magnification you used earlier on this run.");
    int_error(mag_set);
    geq_word_define(int_base + mag_code, mag_set);
  }

  if ((mag <= 0) || (mag > 32768L))
  {
    print_err("Illegal magnification has been changed to 1000");
    help1("The magnification ratio must be between 1 and 32768.");
    int_error(mag);
    geq_word_define(int_base + mag_code, 1000);
  }

  mag_set = mag;
}
/* sec 0295 */
void token_show (pointer p)
{
  if (p != 0)
    show_token_list(link(p), 0, 10000000L);
}
/* sec 0296 */
void print_meaning (void) 
{
  print_cmd_chr(cur_cmd, cur_chr);

  if (cur_cmd >= call)
  {
    print_char(':');
    print_ln();
    token_show(cur_chr);
  }
  else if (cur_cmd == top_bot_mark)
  {
    print_char(':');
    print_ln();
    token_show(cur_mark[cur_chr]);
  }
}
/* sec 0299 */
void show_cur_cmd_chr (void)
{ 
  begin_diagnostic();
  print_nl("{");

  if (mode != shown_mode)
  {
    print_mode(mode);
    prints(": ");
    shown_mode = mode;
  }

  print_cmd_chr(cur_cmd, cur_chr);
  print_char('}');
  end_diagnostic(false);
}
/* sec 0311 */
void show_context (void)
{
  char old_setting;
  pointer s;
  integer nn;
  boolean bottom_line;
  integer i;
  integer j;
  integer l;
  integer m;
  integer n;
  integer p;
  integer q;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  nn = -1;
  bottom_line = false;

  while (true)
  {
    cur_input = input_stack[base_ptr];

    if ((state != token_list))
      if ((name > 17) || (base_ptr == 0))
        bottom_line = true;

    if ((base_ptr == input_ptr) || bottom_line || (nn < error_context_lines))
    {
      if ((base_ptr == input_ptr) || (state != token_list) ||
          (token_type != backed_up) || (loc != 0))
      {
        tally = 0;
        old_setting = selector;

        if (state != token_list)
        {
          if (name <= 17)
            if (name == 0)
              if (base_ptr == 0)
                print_nl("<*>");
              else
                print_nl("<insert> ");
            else
            {
              print_nl("<read ");

              if (name == 17)
                print_char('*');
              else
                print_int(name - 1);

              print_char('>');
            }
          else
          {
            if (c_style_flag)
            {
              print_ln();

              if (name > 17)
                print(name);

              print_char('(');
              print_int(line);
              prints(") :");
            }
            else
            {
              print_nl("l.");
              print_int(line);
            }
          }

          print_char(' ');
          begin_pseudoprint();

          if (buffer[limit] == end_line_char)
            j = limit;
          else
            j = limit + 1;

          if (j > 0)
            for (i = start; i <= j - 1; i++)
            {
              if (i == loc)
                set_trick_count();

              print(buffer[i]);
            }
        }
        else
        {
          switch (token_type)
          {
            case parameter:
              print_nl("<argument> ");
              break;

            case u_template:
            case v_template:
              print_nl("<template> ");
              break;

            case backed_up:
              if (loc == 0)
                print_nl("<recently read> ");
              else
                print_nl("<to be read again> ");
              break;

            case inserted:
              print_nl("<inserted text> ");
              break;

            case macro:
              print_ln();
              print_cs(name);
              break;

            case output_text:
              print_nl("<output> ");
              break;

            case every_par_text:
              print_nl("<everypar> ");
              break;

            case every_math_text:
              print_nl("<everymath> ");
              break;

            case every_display_text:
              print_nl("<everydisplay> ");
              break;

            case every_hbox_text:
              print_nl("<everyhbox> ");
              break;

            case every_vbox_text:
              print_nl("<everyvbox> ");
              break;

            case every_job_text:
              print_nl("<everyjob> ");
              break;

            case every_cr_text:
              print_nl("<everycr> ");
              break;

            case mark_text:
              print_nl("<mark> ");
              break;

            case write_text:
              print_nl("<write> ");
              break;

            default:
              print_nl("?");
              break;
          }

          begin_pseudoprint();

          if (token_type < macro)
          {
            if ((token_type == backed_up) && (loc != null))
            {
              if ((link(start) == null) && (check_kanji(info(start))))
              {
                cur_input = input_stack[base_ptr - 1];
                s = get_avail();
                info(s) = (info(loc) % max_char_val);
                cur_input = input_stack[base_ptr];
                link(start) = s;
                show_token_list(start, loc, 100000L);
                free_avail(s);
                link(start) = null;
                goto done1;
              }
            }

            show_token_list(start, loc, 100000);
          }
          else
          {
            show_token_list(link(start), loc, 100000L);
          }
done1:;
        }

        selector = old_setting;

        if (trick_count == 1000000L)
          set_trick_count();
        
        if (tally < trick_count)
          m = tally - first_count;
        else
          m = trick_count - first_count;

        if (l + first_count <= half_error_line)
        {
          p = 0;
          n = l + first_count;
        }
        else
        {
          prints("...");
          p = l + first_count - half_error_line + 3;
          n = half_error_line;
        }

        if (trick_buf2[p % error_line] == 2)
        {
          p = p + 1;
          n = n - 1;
        }

        for (q = p; q <= first_count - 1; q++)
          print_char(trick_buf[q % error_line]);

        print_ln();

        for (q = 1; q <= n; q++)
          print_char(' ');

        if (m + n <= error_line)
          p = first_count + m;
        else
          p = first_count +(error_line - n - 3);

        kcp = trick_buf2[(p - 1) % error_line];

        if (((kcp % 010) > 0) && (nrestmultichr(kcp) > 0))
          p = p - (kcp % 010);


        for (q = first_count; q <= p - 1; q++)
          print_char(trick_buf[q % error_line]);

        if (m + n > error_line)
          prints("...");

        incr(nn);
      }
    }
    else if (nn == error_context_lines)
    {
      print_nl("...");
      incr(nn); 
    }

    if (bottom_line)
      goto done;

    decr(base_ptr);
  }

done:
  cur_input = input_stack[input_ptr];
}
/* sec 0323 */
void begin_token_list_ (pointer p, quarterword t)
{
  push_input();
  state = token_list;
  start = p;
  token_type = t;

  if (t >= macro)
  {
    add_token_ref(p);

    if (t == macro)
      limit = param_ptr;
    else
    {
      loc = link(p);

      if (tracing_macros > 1)
      {
        begin_diagnostic(); 
        print_nl("");

        switch (t)
        {
          case mark_text:
            print_esc("mark");
            break;

          case write_text:
            print_esc("write");
            break;

          default:
            print_cmd_chr(assign_toks, t - output_text + output_routine_loc);
            break;
        }

        prints("->");
        token_show(p);
        end_diagnostic(false);
      }
    }
  }
  else
    loc = p;
}
/* sec 0324 */
void end_token_list (void) 
{ 
  if (token_type >= backed_up)
  {
    if (token_type <= inserted)
      flush_list(start); 
    else
    {
      delete_token_ref(start);

      if (token_type == macro)
        while (param_ptr > limit)
        {
          decr(param_ptr);
          flush_list(param_stack[param_ptr]);
        }
    }
  }
  else if (token_type == u_template)
    if (align_state > 500000L)
      align_state = 0;
    else
    {
      fatal_error("(interwoven alignment preambles are not allowed)");
      return;
    }

  pop_input();
  check_interrupt();
}
/* sec 0325 */
void back_input (void)
{
  pointer p;

  while ((state == 0) && (loc == 0) && (token_type != v_template))
  {
    end_token_list();
  }

  p = get_avail();
  info(p) = cur_tok;

  if (cur_tok < right_brace_limit)
    if (cur_tok < left_brace_limit)
      decr(align_state);
    else
      incr(align_state);

  push_input();
  state = token_list;
  start = p;
  token_type = backed_up;
  loc = p;
}
/* sec 0327 */
void back_error (void)
{
  OK_to_interrupt = false;
  back_input();
  OK_to_interrupt = true;
  error();
}
/* sec 0327 */
void ins_error (void) 
{
  OK_to_interrupt = false;
  back_input();
  token_type = inserted;
  OK_to_interrupt = true;
  error();
}
/* sec 0328 */
void begin_file_reading (void)
{
  if (in_open == max_in_open)
  {
    overflow("text input levels", max_in_open);
    return;
  }

#ifdef ALLOCATEBUFFER
  if (first == current_buf_size)
    buffer = realloc_buffer(increment_buf_size);

  if (first == current_buf_size)
  {
    overflow("buffer size", current_buf_size);
    return;
  }
#else
  if (first == buf_size)
  {
    overflow("buffer size", buf_size);
    return;
  }
#endif

  incr(in_open);

  if (in_open > high_in_open)
    high_in_open = in_open;

  push_input();
  index = in_open;
  line_stack[index] = line;
  start = first;
  state = mid_line;
  name = 0;
}
/* sec 0329 */
void end_file_reading (void)
{
  first = start;
  line = line_stack[index];

  if (name > 17)
    a_close(cur_file);

  pop_input();
  decr(in_open);
}
/* sec 0330 */
void clear_for_error_prompt (void) 
{
  while ((state != 0) && (name == 0) && (input_ptr > 0) && (loc > limit))
    end_file_reading();

  print_ln();
}
/* sec 0336 */
void check_outer_validity (void)
{
  pointer p;
  pointer q;

  if (scanner_status != 0)
  {
    deletions_allowed = false;

    if (cur_cs != 0)
    {
      if ((state == 0) || (name < 1) || (name > 17))
      {
        p = get_avail();
        info(p) = cs_token_flag + cur_cs;
        back_list(p);
      }

      cur_cmd = spacer;
      cur_chr = ' ';
    }

    if (scanner_status > skipping)
    {
      runaway();

      if (cur_cs == 0)
        print_err("File ended");
      else
      {
        cur_cs = 0;
        print_err("Forbidden control sequence found");
      }

      prints(" while scanning ");
      p = get_avail();

      switch (scanner_status)
      {
        case defining:
          prints("definition");
          info(p) = right_brace_token + '}';
          break;

        case matching:
          prints("use");
          info(p) = par_token;
          long_state = outer_call;
          break;

        case aligning:
          prints("preamble");
          info(p) = right_brace_token + '}';
          q = p;
          p = get_avail();
          link(p) = q;
          info(p) = cs_token_flag + frozen_cr;
          align_state = -1000000L;
          break;

        case absorbing:
          prints("text");
          info(p) = right_brace_token + '}';
          break;
      }

      ins_list(p);
      prints(" of ");
      sprint_cs(warning_index);
      help4("I suspect you have forgotten a `}', causing me",
          "to read past where you wanted me to stop.",
          "I'll try to recover; but if the error is serious,",
          "you'd better type `E' or `X' now and fix your file.");
      error();
    }
    else
    {
      print_err("Incomplete ");
      print_cmd_chr(if_test, cur_if);
      prints("; all text was ignored after line ");
      print_int(skip_line);
      help3("A forbidden control sequence occurred in skipped text.",
          "This kind of error happens when you say `\\if...' and forget",
          "the matching `\\fi'. I've inserted a `\\fi'; this might work.");

      if (cur_cs != 0)
        cur_cs = 0; 
      else
        help_line[2] = "The file ended while I was skipping conditional text.";

      cur_tok = cs_token_flag + frozen_fi;
      ins_error();
    }

    deletions_allowed = true;
  }
}
/* sec 0363 */
void firm_up_the_line (void)
{
  integer k;

  limit = last;

  if (pausing > 0)
    if (interaction > nonstop_mode)
    {
      wake_up_terminal();
      print_ln();

      if (start < limit)
        for (k = start; k <= limit - 1; k++)
          print(buffer[k]);

      first = limit;
      prompt_input("=>");

      if (last > first)
      {
        for (k = first; k <= last - 1; k++)
          buffer[k + start - first] = buffer[k];

        limit = start + last - first;
      }
    }
}
/* sec 0365 */
void get_token (void)
{ 
  no_new_control_sequence = false;
  get_next();
  no_new_control_sequence = true;

  if (cur_cs == 0)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  }
  else
  {
    cur_tok = cs_token_flag + cur_cs;
  }
}
/* sec 0389 */
void macro_call (void)
{
  pointer r;
  pointer p;
  pointer q;
  pointer s;
  pointer t;
  pointer u, v;
  pointer rbrace_ptr;
  small_number n;
  halfword unbalance;
  halfword m;
  pointer ref_count;
  small_number save_scanner_status;
  pointer save_warning_index;
  ASCII_code match_chr;

  save_scanner_status = scanner_status;
  save_warning_index = warning_index;
  warning_index = cur_cs;
  ref_count = cur_chr;
  r = link(ref_count);
  n = 0;

  if (tracing_macros > 0)
  {
    begin_diagnostic();
    print_ln();
    print_cs(warning_index);
    token_show(ref_count);
    end_diagnostic(false);
  }

  if (info(r) != end_match_token)
  {
    scanner_status = matching;
    unbalance = 0;
    long_state = eq_type(cur_cs);

    if (long_state >= outer_call)
      long_state = long_state - 2;

    do
      {
        link(temp_head) = 0;

        if ((info(r) > match_token + 255) || (info(r) < match_token))
          s = 0;
        else
        {
          match_chr = info(r) - match_token;
          s = link(r);
          r = s;
          p = temp_head;
          m = 0;
        }

continu:
        get_token();

        if (cur_tok == info(r))
        {
          r = link(r);

          if ((info(r) >= match_token) && (info(r) <= end_match_token))
          {
            if (cur_tok < left_brace_limit)
              decr(align_state);

            goto found;
          }
          else
            goto continu;
        }

        if (s != r)
          if (s == 0)
          {
            print_err("Use of ");
            sprint_cs(warning_index);
            prints(" doesn't match its definition");
            help4("If you say, e.g., `\\def\\a1{...}', then you must always",
              "put `1' after `\\a', since control sequence names are",
              "made up of letters only. The macro here has not been",
              "followed by the required stuff, so I'm ignoring it.");
            error();
            goto exit;
          }
          else
          {
            t = s;

            do
              {
                store_new_token(info(t));
                incr(m);
                u = link(t);
                v = s;

                while (true)
                {
                  if (u == r)
                    if (cur_tok != info(v))
                      goto done;
                    else
                    {
                      r = link(v);
                      goto continu;
                    }

                    if (info(u) != info(v))
                      goto done;

                    u = link(u);
                    v = link(v);
                }
done:
                t = link(t);
              }
            while (!(t == r));

            r = s;
          }

        if (cur_tok == par_token)
          if (long_state != long_call)
          {
            if (long_state == call)
            {
              runaway();
              print_err("Paragraph ended before ");
              sprint_cs(warning_index);
              prints("was complete");
              help3("I suspect you've forgotten a `}', causing me to apply this",
                  "control sequence to too much text. How can we recover?",
                  "My plan is to forget the whole thing and hope for the best.");
              back_error();
            }

            pstack[n] = link(temp_head);
            align_state = align_state - unbalance;

            for (m = 0; m <= n; m++)
              flush_list(pstack[m]);

            goto exit;
          }

        if (cur_tok < right_brace_limit)
          if (cur_tok < left_brace_limit)
          {
            unbalance = 1;

            while (true)
            {
              fast_store_new_token(cur_tok);
              get_token();

              if (cur_tok == par_token)
                if (long_state != long_call)
                {
                  if (long_state == call)
                  {
                    runaway();
                    print_err("Paragraph ended before ");
                    sprint_cs(warning_index);
                    prints(" was complete");
                    help3("I suspect you've forgotten a `}', causing me to apply this",
                        "control sequence to too much text. How can we recover?",
                        "My plan is to forget the whole thing and hope for the best.");
                    back_error();
                  }

                  pstack[n] = link(temp_head);
                  align_state = align_state - unbalance;

                  for (m = 0; m <= n; m++)
                    flush_list(pstack[m]);
                  goto exit;
                }

              if (cur_tok < right_brace_limit)
                if (cur_tok < left_brace_limit)
                  incr(unbalance);
                else
                {
                  decr(unbalance);

                  if (unbalance == 0)
                    goto done1;
                }
            }
done1:
            rbrace_ptr = p;
            store_new_token(cur_tok);
          }
          else
          {
            back_input();
            print_err("Argument of ");
            sprint_cs(warning_index);
            prints(" has an extra }");
            help6("I've run across a `}' that doesn't seem to match anything.",
                "For example, `\\def\\a#1{...}' and `\\a}' would produce",
                "this error. If you simply proceed now, the `\\par' that",
                "I've just inserted will cause me to report a runaway",
                "argument that might be the root of the problem. But if",
                "your `}' was spurious, just type `2' and it will go away.");
            incr(align_state);
            long_state = call;
            cur_tok = par_token;
            ins_error();
            goto continu;
          }
        else
        {
          if (cur_tok == space_token)
            if (info(r) <= end_match_token)
              if (info(r) >= match_token)
                goto continu;

          store_new_token(cur_tok);
        }

        incr(m);

        if (info(r) > end_match_token)
          goto continu;

        if (info(r) < match_token)
          goto continu;

found:
        if (s != 0)
        {
          if ((m == 1) && (info(p) < right_brace_limit) && (p != temp_head))
          {
            link(rbrace_ptr) = 0;
            free_avail(p);
            p = link(temp_head);
            pstack[n] = link(p);
            free_avail(p);
          }
          else
            pstack[n] = link(temp_head);

          incr(n);

          if (tracing_macros > 0)
          {
            begin_diagnostic();
            //print_nl(match_chr);
            print_nl(""); print(match_chr);
            print_int(n);
            prints("<-");
            show_token_list(pstack[n - 1], 0, 1000);
            end_diagnostic(false);
          }
        }
      }
    while (!(info(r) == end_match_token));
  }

  while ((state == token_list) && (loc == 0) && (token_type != v_template))
    end_token_list();

  begin_token_list(ref_count, macro);
  name = warning_index;
  loc = link(r);

  if (n > 0)
  {
    if (param_ptr + n > max_param_stack)
    {
      max_param_stack = param_ptr + n;

#ifdef ALLOCATEPARAMSTACK
      if (max_param_stack > current_param_size)
        param_stack = realloc_param_stack(increment_param_size);

      if (max_param_stack > current_param_size)
      {
        overflow("parameter stack size", current_param_size);
        return;
      }
#else
      if (max_param_stack > param_size)
      {
        overflow("parameter stack size", param_size);
        return;
      }
#endif
    }

    for (m = 0; m <= n - 1; m++)
      param_stack[param_ptr + m] = pstack[m];

    param_ptr = param_ptr + n;
  }

exit:
  scanner_status = save_scanner_status;
  warning_index = save_warning_index;
}
/* sec 0379 */
void insert_relax (void)
{
  cur_tok = cs_token_flag + cur_cs;
  back_input();
  cur_tok = cs_token_flag + frozen_relax;
  back_input();
  token_type = inserted;
}
/* sec 0366 */
void expand (void)
{
  halfword t;
  pointer p, q, r;
  integer j;
  integer cv_backup;
  small_number cvl_backup, radix_backup, co_backup;
  pointer backup_backup;
  small_number save_scanner_status;

  cv_backup = cur_val;
  cvl_backup = cur_val_level;
  radix_backup = radix;
  co_backup = cur_order;
  backup_backup = link(backup_head);

  if (cur_cmd < call)
  {
    if (tracing_commands > 1)
      show_cur_cmd_chr();

    switch (cur_cmd)
    {
      case top_bot_mark:
        if (cur_mark[cur_chr] != 0)
          begin_token_list(cur_mark[cur_chr], mark_text);
        break;

      case expand_after:
        get_token();
        t = cur_tok;
        get_token();

        if (cur_cmd > max_command)
          expand();
        else
          back_input();

        cur_tok = t;
        back_input();
        break;

      case no_expand:
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_token();
        scanner_status = save_scanner_status;
        t = cur_tok;
        back_input();

        if (t >= cs_token_flag)
        {
          p = get_avail();
          info(p) = cs_token_flag + frozen_dont_expand;
          link(p) = loc;
          start = p;
          loc = p;
        }
        break;

      case cs_name:
        r = get_avail();
        p = r;

        do
          {
            get_x_token();
  
            if (cur_cs == 0)
              store_new_token(cur_tok);
          }
        while (!(cur_cs != 0));

        if (cur_cmd != end_cs_name)
        {
          print_err("Missing ");
          print_esc("endcsname");
          prints(" inserted");
          help2("The control sequence marked <to be read again> should",
              "not appear between \\csname and \\endcsname.");
          back_error();
        }

        j = first;
        p = link(r);

        while (p != 0)
        {
          if (j >= max_buf_stack)
          {
            max_buf_stack = j + 1;

#ifdef ALLOCATEBUFFER
            if (max_buf_stack == current_buf_size)
              buffer = realloc_buffer (increment_buf_size);

            if (max_buf_stack == current_buf_size)
            {
              overflow("buffer size", current_buf_size);
              return;
            }
#else
            if (max_buf_stack == buf_size)
            {
              overflow("buffer size", buf_size);
              return;
            }
#endif
          }

          if (check_kanji(info(p)))
          {
            t = toBUFF(info(p) % max_cjk_val);
            if (BYTE1(t) != 0) { buffer[j] = BYTE1(t); incr(j); };
            if (BYTE2(t) != 0) { buffer[j] = BYTE2(t); incr(j); };
            if (BYTE3(t) != 0) { buffer[j] = BYTE3(t); incr(j); };
            buffer[j] = BYTE4(t); incr(j);
            p = link(p);
          }
          else
          {
            buffer[j] = info(p) % max_char_val;
            incr(j);
            p = link(p);
          }

        }

        if (j > first + 1)
        {
          no_new_control_sequence = false;
          cur_cs = id_lookup(first, j - first);
          no_new_control_sequence = true;
        }
        else if (j == first)
          cur_cs = null_cs;
        else
          cur_cs = single_base + buffer[first];

        flush_list(r);

        if (eq_type(cur_cs) == undefined_cs)
        {
          eq_define(cur_cs, relax, 256);
        }

        cur_tok = cur_cs + cs_token_flag;
        back_input();
        break;

      case convert:
        conv_toks();
        break;

      case the:
        ins_the_toks();
        break;

      case if_test:
        conditional();
        break;

      case fi_or_else:
        if (cur_chr > if_limit)
          if (if_limit == if_code)
            insert_relax();
          else
          {
            print_err("Extra ");
            print_cmd_chr(fi_or_else, cur_chr);
            help1("I'm ignoring this; it doesn't match any \\if.");
            error();
          }
        else
        {
          while (cur_chr != fi_code)
            pass_text();

          {
            p = cond_ptr;
            if_line = if_line_field(p);
            cur_if = subtype(p);
            if_limit = type(p);
            cond_ptr = link(p);
            free_node(p, if_node_size);
          }
        }
        break;

      case input:
        if (cur_chr > 0)
          force_eof = true;
        else if (name_in_progress)
          insert_relax();
        else
          start_input();
        break;

      default:
        print_err("Undefined control sequence");
        help5("The control sequence at the end of the top line",
            "of your error message was never \\def'ed. If you have",
            "misspelled it (e.g., `\\hobx'), type `I' and the correct",
            "spelling (e.g., `I\\hbox'). Otherwise just continue,",
            "and I'll forget about whatever was undefined.");
        error();
        break;
    }
  }
  else if (cur_cmd < end_template)
  {
    macro_call();
  }
  else
  {
    cur_tok = cs_token_flag + frozen_endv;
    back_input();
  }

  cur_val = cv_backup;
  cur_val_level = cvl_backup;
  radix = radix_backup;
  cur_order = co_backup;
  link(backup_head) = backup_backup;
}
/* sec 0380 */
void get_x_token (void)
{
restart:
  get_next();

  if (cur_cmd <= max_command)
    goto done;

  if (cur_cmd >= call)
    if (cur_cmd < end_template)
      macro_call();
    else
    {
      cur_cs = frozen_endv;
      cur_cmd = endv;
      goto done;
    }
  else
    expand();

  goto restart;

done:
  if (cur_cs == 0)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  }
  else
  {
    cur_tok = cs_token_flag + cur_cs;
  }
}
/* sec 0381 */
void x_token (void)
{
  while (cur_cmd > max_command)
  {
    expand();
    get_next();
  }

  if (cur_cs == 0)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  }
  else
  {
    cur_tok = cs_token_flag + cur_cs;
  }
}
/* sec 0403 */
void scan_left_brace (void)
{
  do
    {
      get_x_token();
    }
  while (!((cur_cmd != spacer) && (cur_cmd != relax)));

  if (cur_cmd != left_brace)
  {
    print_err("Missing { inserted");
    help4("A left brace was mandatory here, so I've put one in.",
        "You might want to delete and/or insert some corrections",
        "so that I will find a matching right brace soon.",
        "(If you're confused by all this, try typing `I}' now.)");
    back_error();
    cur_tok = left_brace_token + '{';
    cur_cmd = left_brace;
    cur_chr = '{';
    incr(align_state);
  }
}
/* sec 0405 */
void scan_optional_equals (void)
{
  do
    {
      get_x_token();
    }
  while (!(cur_cmd != spacer));

  if (cur_tok != other_token + '=')
    back_input();
}
/* sec 0407 */
boolean scan_keyword (const char * s)
{
  pointer p;
  pointer q;
  const char * k;

  p = backup_head;
  link(p) = 0;
  k = s;

  while (*k)
  {
    get_x_token(); 

    if ((cur_cs == 0) && ((cur_chr == (*k)) || (cur_chr == (*k) - 'a' + 'A')))
    {
      store_new_token(cur_tok);
      incr(k);
    }
    else if ((cur_cmd != spacer) || (p != backup_head))
    {
      back_input();

      if (p != backup_head)
        back_list(link(backup_head));

      return false;
    }
  }

  flush_list(link(backup_head));

  return true;
}
/* sec 0408 */
void mu_error (void)
{
  print_err("Incompatible glue units");
  help1("I'm going to assume that 1mu=1pt when they're mixed.");
  error();
}
/* sec 0433 */
void scan_eight_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 255))
  {
    print_err("Bad register code");
    help2("A register number must be between 0 and 255.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}
/* sec 0434 */
void scan_char_num (void)
{
  scan_int();

  if (!(is_char_ascii(cur_val)) && !(is_char_kanji(cur_val)))
  {
    print_err("Bad character code");
    help2("A character number must be between 0 and 255, or KANJI code.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}
/* sec 0435 */
void scan_four_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 15))
  {
    print_err("Bad number");
    help2("Since I expected to read a number between 0 and 15,",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}
/* sec 0436 */
void scan_fifteen_bit_int (void) 
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 32767))
  {
    print_err("Bad mathchar");
    help2("A mathchar number must be between 0 and 32767.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}
/* sec 0437 */
void scan_twenty_seven_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 134217727L))
  {
    print_err("Bad delimiter code");
    help2("A numeric delimiter code must be between 0 and 2^{27}-1.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}
/* sec 0577 */
void scan_font_ident (void) 
{
  internal_font_number f;
  halfword m;

  do
    {
      get_x_token();
    }
  while (!(cur_cmd != spacer));

  if (cur_cmd == def_font)
    f = cur_font;
  else if (cur_cmd == def_tfont)
    f = cur_tfont;
  else if (cur_cmd == def_jfont)
    f = cur_jfont;
  else if (cur_cmd == set_font)
    f = cur_chr; 
  else if (cur_cmd == def_family)
  {
    m = cur_chr;
    scan_four_bit_int();
    f = equiv(m + cur_val);
  }
  else
  {
    print_err("Missing font identifier");
    help2("I was looking for a control sequence whose",
        "current meaning has been defined by \\font.");
    back_error();
    f = null_font;
  }

  cur_val = f;
}
/* sec 0578 */
void find_font_dimen (boolean writing)
{
  internal_font_number f;
  integer n;

  scan_int();
  n = cur_val;
  scan_font_ident();
  f = cur_val;

  if (n < 0)
    cur_val = fmem_ptr;
  else
  {
    if (writing && (n <= space_shrink_code) && (n >= space_code) && (font_glue[f] != 0)) 
    {
      delete_glue_ref(font_glue[f]);
      font_glue[f] = 0;
    }

    if (n > font_params[f])
      if (f < font_ptr)
        cur_val = fmem_ptr;
      else
      {
        do
          {
 #ifdef ALLOCATEFONT
            if (fmem_ptr == current_font_mem_size)
              font_info = realloc_font_info(increment_font_mem_size);

            if (fmem_ptr == current_font_mem_size)
            {
              overflow("font memory", current_font_mem_size);
              return;
            }
#else
            if (fmem_ptr == font_mem_size)
            {
              overflow("font memory", font_mem_size);
              return;
            }
#endif
            font_info[fmem_ptr].cint = 0;
            incr(fmem_ptr);
            incr(font_params[f]);
          }
        while (!(n == font_params[f]));

        cur_val = fmem_ptr - 1;
      }
    else if (n > 0)
      cur_val = n + param_base[f];
  }

  if (cur_val == fmem_ptr)
  {
    print_err("Font ");
    print_esc(""); print(font_id_text(f));
    prints(" has only ");
    print_int(font_params[f]);
    prints(" fontdimen parameters");
    help2("To increase the number of font parameters, you must",
      "use \\fontdimen immediately after the \\font is loaded.");
    error();
  }
}
/* sec 0413 */
void scan_something_internal (small_number level, boolean negative)
{
  halfword m;
  pointer tx;
  halfword qx;
  integer p;
  pointer q, r;

  m = cur_chr;

  switch (cur_cmd)
  {
    case assign_kinsoku:
      {
        scan_int();
        q = get_kinsoku_pos(tokanji(cur_val), cur_pos);
        cur_val_level = int_val;
        cur_val = 0;

        if ((q != no_entry) && (m == kinsoku_type(q)))
          scanned_result(kinsoku_penalty(q), int_val);
      }
      break;

    case assign_inhibit_xsp_code:
      {
        scan_int();
        q = get_inhibit_pos(tokanji(cur_val), cur_pos);
        cur_val_level = int_val;
        cur_val = 3;

        if (q != no_entry)
          cur_val = inhibit_xsp_type(q);
      }
      break;

    case def_code:
      {
        scan_char_num();

        if (m == math_code_base)
          scanned_result(math_code(cur_val), int_val);
        else if (m == kcat_code_base)
          scanned_result(equiv(m + kcatcodekey(cur_val)), int_val);
        else if (m < math_code_base)
        {
          if (!is_char_ascii(cur_val))
            scanned_result(equiv(m + Hi(cur_val)), int_val);
          else
            scanned_result(equiv(m + cur_val), int_val);
        }
        else
        {
          if (!is_char_ascii(cur_val))
            scanned_result(eqtb[m + Hi(cur_val)].cint, int_val);
          else
            scanned_result(eqtb[m + cur_val].cint, int_val);
        }
      }
      break;

    case toks_register:
    case assign_toks:
    case def_family:
    case set_font:
    case def_font:
      if (level != tok_val)
      {
        print_err("Missing number, treated as zero");
        help3("A number should have been here; I inserted `0'.",
            "(If you can't figure out why I needed to see a number,",
            "look up `weird error' in the index to The TeXbook.)");
        back_error();
        scanned_result(0, dimen_val);
      }
      else if (cur_cmd <= assign_toks)
      {
        if (cur_cmd < assign_toks)
        {
          scan_eight_bit_int();
          m = toks_base + cur_val;
        }

        scanned_result(equiv(m), tok_val);
      }
      else
      {
        back_input();
        scan_font_ident();
        scanned_result(font_id_base + cur_val, ident_val);
      }
      break;

    case assign_int:
      scanned_result(eqtb[m].cint, int_val);
      break;

    case assign_dimen:
      scanned_result(eqtb[m].cint, dimen_val);
      break; 

    case assign_glue:
      scanned_result(equiv(m), glue_val);
      break;

    case assign_mu_glue:
      scanned_result(equiv(m), mu_val);
      break;

    case set_aux:
      if (abs(mode) != m)
      {
        print_err("Improper ");
        print_cmd_chr(set_aux, m);
        help4("You can refer to \\spacefactor only in horizontal mode;",
            "you can refer to \\prevdepth only in vertical mode; and",
            "neither of these is meaningful inside \\write. So",
            "I'm forgetting what you said and using zero instead.");
        error();

        if (level != tok_val)
          scanned_result(0, dimen_val);
        else
          scanned_result(0, int_val);
      }
      else if (m == vmode)
        scanned_result(prev_depth, dimen_val);
      else
        scanned_result(space_factor, int_val);
      break;

    case set_prev_graf:
      if (mode == 0)
        scanned_result(0, int_val);
      else
      {
        nest[nest_ptr] = cur_list;
        p = nest_ptr;

        while (abs(nest[p].mode_field) != vmode)
          decr(p);

        scanned_result(nest[p].pg_field, int_val);
      }
      break;

    case set_page_int:
      {
        if (m == 0)
          cur_val = dead_cycles; 
        else
          cur_val = insert_penalties;

        cur_val_level = 0;
      }
      break;

    case set_page_dimen:
      {
        if ((page_contents == 0) && (! output_active))
          if (m == 0)
            cur_val = max_dimen;
          else
            cur_val = 0;
        else
          cur_val = page_so_far[m];

        cur_val_level = dimen_val;
      }
      break;

    case set_shape:
      {
        if (par_shape_ptr == 0)
          cur_val = 0; 
        else
          cur_val = info(par_shape_ptr);

        cur_val_level = int_val;
      }
      break;

    case set_box_dimen:
      {
        scan_eight_bit_int();
        q = box(cur_val);

        if (q == 0)
          cur_val = 0;
        else
        {
          qx = q;

          while ((q != null) && box_dir(q) != abs(direction))
            q = link(q);

          if (q == 0)
          {
            r = link(qx);
            link(qx) = null;
            q = new_dir_node(qx, abs(direction));
            link(qx) = r;
            cur_val = mem[q + m].cint;
            delete_glue_ref(space_ptr(q));
            delete_glue_ref(xspace_ptr(q));
            free_node(q, box_node_size);
          }
          else
          {
            cur_val = mem[q + m].cint;
          }
        }

        cur_val_level = dimen_val;
      }
      break;

    case kchar_given:
    case char_given:
    case math_given:
      scanned_result(cur_chr, int_val);
      break;

    case assign_font_dimen:
      {
        find_font_dimen(false);
        font_info[fmem_ptr].cint = 0;
        scanned_result(font_info[cur_val].cint, dimen_val);
      }
      break;

    case assign_font_int:
      {
        scan_font_ident();

        if (m == 0)
          scanned_result(hyphen_char[cur_val], int_val);
        else
          scanned_result(skew_char[cur_val], int_val);
      }
      break;

    case tex_register:
      {
        scan_eight_bit_int();

        switch (m)
        {
          case int_val:
            cur_val = count(cur_val);
            break;

          case dimen_val:
            cur_val = dimen(cur_val);
            break;

          case glue_val:
            cur_val = skip(cur_val);
            break;

          case mu_val:
            cur_val = mu_skip(cur_val);
            break;
        }
        
        cur_val_level = m;
      }
      break;

    case last_item:
      if (cur_chr > glue_val)
      {
        if (cur_chr == input_line_no_code)
          cur_val = line;
        else
          cur_val = last_badness;

        cur_val_level = int_val;
      }
      else
      {
        if (cur_chr == glue_val)
          cur_val = zero_glue;
        else
          cur_val = 0;

        find_effective_tail();
        cur_val_level = cur_chr;

        if (!is_char_node(tx) && (tx != head) && (mode != 0))
          switch (cur_chr)
          {
            case int_val:
              if (type(tx) == penalty_node)
                cur_val = penalty(tx);
              break;

            case dimen_val:
              if (type(tx) == kern_node)
                cur_val = width(tx);
              break;

            case glue_val:
              if (type(tx) == glue_node)
              {
                cur_val = glue_ptr(tx);

                if (subtype(tx) == mu_glue)
                  cur_val_level = mu_val;
              }
              break;
          }
        else if ((mode == vmode) && (tx == head))
          switch (cur_chr)
          {
            case int_val:
              cur_val = last_penalty;
              break;

            case dimen_val:
              cur_val = last_kern;
              break;

            case glue_val:
              if (last_glue != empty_flag)
                cur_val = last_glue;
              break;
          }
      }
      break;

    default:
      {
        print_err("You can't use `");
        print_cmd_chr(cur_cmd, cur_chr);
        prints("' after ");
        print_esc("the");
        help1("I'm forgetting what you said and using zero instead.");
        error();

        if (level != tok_val)
          scanned_result(0, dimen_val);
        else
          scanned_result(0, int_val);
      }
      break;
  }

  while (cur_val_level > level)
  {
    if (cur_val_level == glue_val)
      cur_val = width(cur_val);
    else if (cur_val_level == mu_val)
      mu_error();

    decr(cur_val_level);
  }

  if (negative)
    if (cur_val_level >= glue_val)
    {
      cur_val = new_spec(cur_val);

      {
        width(cur_val) = -width(cur_val);
        stretch(cur_val) = -stretch(cur_val);
        shrink(cur_val) = -shrink(cur_val);
      }
    }
    else
      cur_val = -cur_val;
  else if ((cur_val_level >= glue_val) && (cur_val_level <= mu_val))
    add_glue_ref(cur_val);
}
/* sec 0341 */
void get_next (void)
{
  integer k;
  halfword t;
  /* char cat; */
  int cat;
  int l;
  ASCII_code c, cc;
  char d;

restart:
  cur_cs = 0;

  if (state != token_list)
  {
lab_switch:
    if (loc <= limit)
    {
      cur_chr = fromBUFF(buffer, limit, loc);
      cur_cmd = kcat_code(kcatcodekey(cur_chr));

      if ((multistrlen(buffer, limit, loc) > 1) && check_kcat_code(cur_cmd))
      {
        if (cur_cmd == not_cjk)
          cur_cmd = other_kchar;

        loc = loc + multistrlen(buffer, limit, loc);
      }
      else
      {
        cur_chr = buffer[loc];
        incr(loc);
reswitch:
        cur_cmd = cat_code(cur_chr);
      };


      switch (state + cur_cmd)
      {
        case any_state_plus(ignore):
        case skip_blanks + spacer:
        case new_line + spacer:
          goto lab_switch;
          break;

        case any_state_plus(escape):
          {
            if (loc > limit)
              cur_cs = null_cs;
            else
            {
              k = loc;
              cur_chr = fromBUFF(buffer, limit, k);
              cat = kcat_code(kcatcodekey(cur_chr));

              if ((multistrlen(buffer, limit, k) > 1) && check_kcat_code(cat))
              {
                if (cat == not_cjk)
                  cat = other_kchar;

                k = k + multistrlen(buffer, limit, k);
              }
              else
              {
                cur_chr = buffer[k];
                cat = cat_code(cur_chr);
                incr(k);
              };

start_cs:
              if ((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul))
                state = skip_blanks;
              else if (cat == spacer)
                state = skip_blanks;
              else
                state = mid_line;

              if (cat == other_kchar)
              {
                cur_cs = id_lookup(loc, k - loc);
                loc = k;
                goto found;
              }
              else if (((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)) && (k <= limit))
              {
                do {
                  cur_chr = fromBUFF(buffer, limit, k);
                  cat = kcat_code(kcatcodekey(cur_chr));

                  if ((multistrlen(buffer, limit, k)>1) && check_kcat_code(cat))
                  {
                    if (cat == not_cjk)
                      cat = other_kchar;
                    k = k + multistrlen(buffer, limit, k);
                  }
                  else
                  {
                    cur_chr = buffer[k];
                    cat = cat_code(cur_chr);
                    incr(k);
                  }

                  
                  while ((buffer[k] == cur_chr) && (cat == sup_mark) && (k < limit))
                  {
                    c = buffer[k + 1];
                    
                    if (c < 0200)
                    {
                      d = 2;

                      if (is_hex(c))
                        if (k + 2 <= limit)
                        {
                          cc = buffer[k + 2];

                          if (is_hex(cc))
                            incr(d);
                        }

                      if (d > 2)
                      {
                        hex_to_cur_chr();
                      }
                      else if (c < 0100)
                        cur_chr = c + 0100;
                      else
                        cur_chr = c - 0100;

                      cat = cat_code(cur_chr);

                      if ((cat == letter) || (cat == sup_mark))
                      {
                        buffer[k - 1] = cur_chr;
                        limit = limit - d;
                        first = first - d;
                        l = k;

                        while (l <= limit)
                        {
                          buffer[l] = buffer[l + d];
                          incr(l);
                        }
                      }
                    }
                  }
                }
                while (!(!((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)) || (k > limit)));

                if (!((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)))
                  decr(k);

                if (cat == other_kchar)
                  k = k - multilenbuffchar(cur_chr) + 1;

                if (k > loc + 1)
                {
                  cur_cs = id_lookup(loc, k - loc);
                  loc = k;
                  goto found;
                }
              }
              else
              {
                if (buffer[k] == cur_chr)
                  if (cat == sup_mark)
                    if (k < limit)
                    {
                      c = buffer[k + 1];

                      if (c < 128)
                      {
                        d = 2;

                        if (is_hex(c))
                          if (k + 2 <= limit)
                          {
                            cc = buffer[k + 2];

                            if (is_hex(cc))
                              incr(d);
                          }

                        if (d > 2)
                        {
                          hex_to_cur_chr();
                          buffer[k - 1] = cur_chr;
                        }
                        else if (c < 64)
                          buffer[k - 1] = c + 64;
                        else
                          buffer[k - 1] = c - 64;

                        limit = limit - d;
                        first = first - d;
                        l = k;
                        cur_chr = buffer[k - 1];
                        cat = cat_code(cur_chr);

                        while (l <= limit)
                        {
                          buffer[l] = buffer[l + d];
                          incr(l);
                        }

                        goto start_cs;
                      }
                    }
              }

              cur_cs = single_base + buffer[loc];
              incr(loc);
            }
found:
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);
            
            if (cur_cmd >= outer_call)
              check_outer_validity();
          }
          break;

        case any_state_plus(active_char):
          {
            cur_cs = cur_chr + active_base;
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);
            state = mid_line;
            
            if (cur_cmd >= outer_call)
              check_outer_validity();
          }
          break;

        case any_state_plus(sup_mark):
          {
            if (cur_chr == buffer[loc])
              if (loc < limit)
              {
                c = buffer[loc + 1];

                if (c < 128)
                {
                  loc = loc + 2;

                  if (is_hex(c))
                    if (loc <= limit)
                    {
                      cc = buffer[loc];

                      if (is_hex(cc))
                      {
                        incr(loc);
                        hex_to_cur_chr();
                        goto reswitch;
                      }
                    }

                  if (c < 64)
                    cur_chr = c + 64;
                  else
                    cur_chr = c - 64;

                  goto reswitch;
                }
              }

              state = mid_line;
          }
          break;

        case any_state_plus(invalid_char):
          {
            print_err("Text line contains an invalid character");
            help2("A funny symbol that I can't read has just been input.",
                "Continue, and I'll forget that it ever happened.");
            deletions_allowed = false;
            error();
            deletions_allowed = true;
            goto restart;
          }
          break;

        case mid_kanji + spacer:
        case mid_line + spacer:
          {
            state = skip_blanks;
            cur_chr = ' ';
          }
          break;

        case mid_line + car_ret:
          {
            loc = limit + 1;
            cur_cmd = spacer;
            cur_chr = ' ';
          }
          break;

        case mid_kanji + car_ret:
          if (skip_mode)
          {
            loc = limit + 1;
            goto lab_switch;
          }
          else
          {
            loc = limit + 1;
            cur_cmd = spacer;
            cur_chr = ' ';
          }
          break;

        case skip_blanks + car_ret:
        case any_state_plus(comment):
          {
            loc = limit + 1;
            goto lab_switch;
          }
          break;

        case new_line + car_ret:
          {
            loc = limit + 1;
            cur_cs = par_loc;
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);
            
            if (cur_cmd >= outer_call)
              check_outer_validity();
          }
          break;

        case mid_line + left_brace:
        case mid_kanji + left_brace:
          incr(align_state);
          break;

        case skip_blanks + left_brace:
        case new_line + left_brace:
          {
            state = mid_line;
            incr(align_state);
          }
          break;

        case mid_line + right_brace:
        case mid_kanji + right_brace:
          decr(align_state);
          break;

        case skip_blanks + right_brace:
        case new_line + right_brace:
          {
            state = mid_line;
            decr(align_state);
          }
          break;

        case add_delims_to(skip_blanks):
        case add_delims_to(new_line):
        case add_delims_to(mid_kanji):
          state = mid_line;
          break;

        case all_jcode(skip_blanks):
        case all_jcode(new_line):
        case all_jcode(mid_kanji):
          state = mid_kanji;
          break;

        case hangul_code(skip_blanks):
        case hangul_code(new_line):
        case hangul_code(mid_kanji):
          state = mid_line;
          break;

        default:
          break;
      }
    }
    else
    {
      state = new_line;

      if (name > 17)
      {
        incr(line);
        first = start;

        if (!force_eof)
        {
          if (input_ln(cur_file, true))
            firm_up_the_line();
          else
            force_eof = true;
        }

        if (force_eof)
        {
          print_char(')');
          decr(open_parens);
          update_terminal();
          force_eof = false;
          end_file_reading();
          check_outer_validity();
          goto restart;
        }

        if (end_line_char_inactive())
          decr(limit);
        else
          buffer[limit] = end_line_char;

        first = limit + 1;
        loc = start;
      }
      else
      {
        if (!(name == 0))
        {
          cur_cmd = 0;
          cur_chr = 0;
          return;
        }

        if (input_ptr > 0)
        {
          end_file_reading();
          goto restart;
        }

        if (selector < log_only)
          open_log_file();

        if (interaction > nonstop_mode)
        {
          if (end_line_char_inactive())
            incr(limit);

          if (limit == start)
            print_nl("(Please type a command or say `\\end')");

          print_ln();
          first = start;
          prompt_input("*");
          limit = last;

          if (end_line_char_inactive())
            decr(limit);
          else
            buffer[limit]= end_line_char;

          first = limit + 1;
          loc = start;
        }
        else
        {
          fatal_error("*** (job aborted, no legal \\end found)");
          return;
        }
      }

      check_interrupt();
      goto lab_switch;
    }
  }
  else if (loc != 0)
  {
    t = info(loc);
    loc = link(loc);

    if (t >= cs_token_flag)
    {
      cur_cs = t - cs_token_flag;
      cur_cmd = eq_type(cur_cs);
      cur_chr = equiv(cur_cs);

      if (cur_cmd >= outer_call)
        if (cur_cmd == dont_expand)
        {
          cur_cs = info(loc) - cs_token_flag;
          loc = 0;
          cur_cmd = eq_type(cur_cs);
          cur_chr = equiv(cur_cs);

          if (cur_cmd > max_command)
          {
            cur_cmd = relax;
            cur_chr = no_expand_flag;
          }
        }
        else
        {
          check_outer_validity();
        }
    }
    else if (check_kanji(t))
    {
      cur_cmd = t / max_cjk_val;
      cur_chr = t % max_cjk_val;
    }
    else
    {
      cur_cmd = t / max_char_val;
      cur_chr = t % max_char_val;

      switch (cur_cmd)
      {
        case left_brace:
          incr(align_state);
          break;

        case right_brace:
          decr(align_state);
          break;

        case out_param:
          {
            begin_token_list(param_stack[limit + cur_chr - 1], parameter);
            goto restart;
          }
          break;

        default:
          break;
      }
    }
  }
  else
  {
    end_token_list();
    goto restart;
  }

  if (cur_cmd <= car_ret)
    if (cur_cmd >= tab_mark)
      if (align_state == 0)
      {
        if ((scanner_status == aligning) && (cur_align == 0))
        {
          fatal_error("(interwoven alignment preambles are not allowed)");
          return;
        }

        cur_cmd = extra_info(cur_align);
        extra_info(cur_align) = cur_chr;

        if (cur_cmd == omit)
          begin_token_list(omit_template, v_template);
        else
          begin_token_list(v_part(cur_align), v_template);

        align_state = 1000000L;
        goto restart;
      }
}
