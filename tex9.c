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

/* sec 1284 */
void give_err_help (void)
{
  token_show(err_help);
}
/* sec 0524 */
boolean open_fmt_file (void)
{
  integer j;

  j = loc;

  if (buffer[loc] == '&' || buffer[loc] == '+')
  {
    incr(loc);
    j = loc;
    buffer[last] = ' ';

    while (buffer[j] != ' ')
      incr(j);

    pack_buffered_name(0, loc, j - 1);

    if (w_open_in(fmt_file))
      goto found;
  
    if (knuth_flag)
    {
      wake_up_terminal();
      printf("%s;%s\n", "Sorry, I can't find that format", " will try the default.");
    }
    else
    {
      name_of_file[name_length + 1] = '\0';
      printf("%s (%s);%s\n", "Sorry, I can't find that format", name_of_file + 1, " will try the default.");
      name_of_file[name_length + 1] = ' ';
      printf("(Perhaps your Y&Y TeX's environment variable is not set correctly)\n");
    }

    update_terminal();
  }

  pack_buffered_name(format_default_length - 4, 1, 0);

  if (!w_open_in(fmt_file))
  {
    if (knuth_flag)
    {
      wake_up_terminal();
      printf("%s!\n", "I can't find the default format file");
    }
    else
    {
      name_of_file[name_length + 1] = '\0';
      printf("%s (%s)!\n", "I can't find the default format file", name_of_file + 1);
      name_of_file[name_length + 1] = ' ';
      printf("(Perhaps your Y&Y TeX's environment variable is not set correctly)\n");
    }

    return false;
  }

found:
  loc = j;

  return true;
}
/* sec 1333 */
void close_files_and_terminate (void)
{
  integer k; 

  if (trace_flag)
    puts("\nclose_files_and_terminate() ");

  for (k = 0; k <= 15; k++)
    if (write_open[k])
      a_close(write_file[k]);

#ifdef STAT
  if (tracing_stats > 0 || verbose_flag != 0)
    if (log_opened)
    {
      log_printf("%c\n", ' ');
      log_printf("\n");
      log_printf("%s%s\n", "Here is how much of TeX's memory", " you used:");
      log_printf("%c%lld%s", ' ', (integer)(str_ptr - init_str_ptr), " string");

      if (str_ptr != init_str_ptr + 1)
        wlog_cr();

#ifdef ALLOCATESTRING
      if (show_current)
        log_printf("%s%d\n", " out of ", (int)(current_max_strings - init_str_ptr));
      else
#endif
        log_printf("%s%d\n", " out of ", (int)(max_strings - init_str_ptr));

#ifdef ALLOCATESTRING
      if (show_current)
        log_printf("%c%lld%s%lld\n", ' ', (integer)(pool_ptr - init_pool_ptr), " string characters out of ", current_pool_size - init_pool_ptr);
      else
#endif
        log_printf("%c%lld%s%lld\n", ' ', (integer)(pool_ptr - init_pool_ptr), " string characters out of ", pool_size - init_pool_ptr);

#ifdef ALLOCATEMAIN
      if (show_current)
        log_printf("%c%lld%s%d\n", ' ', (integer)(lo_mem_max - mem_min + mem_end - hi_mem_min + 2), " words of memory out of ", current_mem_size);
      else
#endif
        log_printf("%c%lld%s%lld\n", ' ', (integer)(lo_mem_max - mem_min + mem_end - hi_mem_min + 2), " words of memory out of ", mem_end + 1 - mem_min);

      log_printf("%c%lld%s%d\n", ' ', (cs_count), " multiletter control sequences out of ", (hash_size + hash_extra));
      log_printf("%c%lld%s%lld%s", ' ', (fmem_ptr), " words of font info for ", (font_ptr - font_base), " font");

      if (font_ptr != 1)
        wlog('s');

#ifdef ALLOCATEFONT
      if (show_current)
        log_printf("%s%d%s%d\n", ", out of ", current_font_mem_size, " for ", font_max - font_base);
      else
#endif
        log_printf("%s%lu%s%d\n", ", out of ", font_mem_size, " for ", font_max - font_base);

      log_printf("%c%lld%s", ' ', hyph_count, " hyphenation exception");

      if (hyph_count != 1)
        wlog('s');

      log_printf("%s%lld\n", " out of ", hyphen_prime);
      log_printf(" ");
      log_printf("%d%s", (int) max_in_stack, "i,");
      log_printf("%d%s", (int) max_nest_stack, "n,");
      log_printf("%d%s", (int) max_param_stack, "p,");
      log_printf("%d%s", (int) max_buf_stack + 1, "b,");
      log_printf("%d%s", (int) max_save_stack + 6, "s");
      log_printf(" stack positions out of ");

#ifdef ALLOCATESAVESTACK
      if (show_current)
        log_printf("%d%s", current_stack_size, "i,");
      else
#endif
        log_printf("%d%s", stack_size, "i,");

#ifdef ALLOCATENESTSTACK
      if (show_current)
        log_printf("%d%s", current_nest_size, "n,");
      else
#endif
        log_printf("%d%s", nest_size, "n,");

#ifdef ALLOCATEPARAMSTACK
      if (show_current)
        log_printf("%d%s", current_param_size, "p,");
      else
#endif
        log_printf("%d%s", param_size, "p,");

#ifdef ALLOCATEBUFFER
      if (show_current)
        log_printf("%d%s", current_buf_size, "b,");
      else
#endif
        log_printf("%ld%s", buf_size, "b,");

#ifdef ALLOCATESAVESTACK
      if (show_current)
        log_printf("%d%s", current_save_size, "s");
      else
#endif
        log_printf("%d%s", save_size, "s");

      log_printf("\n");

      if (!knuth_flag)
      {
        log_printf(" (i = in_stack, n = nest_stack, p = param_stack, b = buf_stack, s = save_stack)\n");
        log_printf(" %lld inputs open max out of %d\n", high_in_open, max_in_open);
      }

      if (show_line_break_stats && first_pass_count > 0)
      {
        int first_count, second_count, third_count;

        log_printf("\nSuccess at breaking %d paragraph%s:", first_pass_count, (first_pass_count == 1) ? "" : "s");

        if (single_line > 0)
          log_printf("\n %d single line `paragraph%s'", single_line, (single_line == 1) ? "" : "s");

        first_count = first_pass_count - single_line - second_pass_count;

        if (first_count < 0)
          first_count = 0;

        second_count = second_pass_count - final_pass_count;
        third_count = final_pass_count - paragraph_failed;

        if (first_pass_count > 0)
          log_printf("\n %d first pass (\\pretolerance = %lld)", first_pass_count, pretolerance);

        if (second_pass_count > 0)
          log_printf("\n %d second pass (\\tolerance = %lld)", second_pass_count, tolerance);

        if (final_pass_count > 0 || emergency_stretch > 0)
          log_printf("\n %d third pass (\\emergencystretch = %lgpt)",
            final_pass_count, (double) emergency_stretch / 65536.0);

        if (paragraph_failed > 0)
          log_printf("\n %d failed", paragraph_failed);

        wlog_cr();

        if (overfull_hbox > 0)
          log_printf("\n %d overfull \\hbox%s", overfull_hbox, (overfull_hbox > 1) ? "es" : "");

        if (underfull_hbox > 0)
          log_printf("\n %d underfull \\hbox%s", underfull_hbox, (underfull_hbox > 1) ? "es" : "");

        if (overfull_vbox > 0)
          log_printf("\n %d overfull \\vbox%s", overfull_vbox, (overfull_vbox > 1) ? "es" : "");

        if (underfull_vbox > 0)
          log_printf("\n %d underfull \\vbox%s", underfull_vbox, (underfull_vbox > 1) ? "es" : "");

        if (overfull_hbox || underfull_hbox || overfull_vbox || underfull_vbox)
          wlog_cr();
      }
  }
#endif
  
  wake_up_terminal();

  {
    if (total_pages == 0)
      print_nl("No pages of output.");
    else
    {
      spc_exec_at_end_document();
      pdf_close_document();
      pdf_close_device();
      pdf_files_close();
      pdf_close_fontmaps();
      print_nl("Output written on ");

      if (full_file_name_flag && pdf_file_name != NULL)
        prints(pdf_file_name);
      else
        slow_print(output_file_name);

      prints(" (");
      print_int(total_pages);
      prints(" page");

      if (total_pages != 1)
        print_char('s');

      prints(", ");
      print_int(pdf_output_stats());
      prints(" bytes).");
      b_close(pdf_file);
    }
  }
/*
  {
    while (cur_s > -1)
    {
      if (cur_s > 0)
        dvi_out(pop);
      else
      {
        dvi_out(eop);
        incr(total_pages);
      }

      decr(cur_s);
    }

    if (total_pages == 0)
      print_nl("No pages of output.");
    else
    {
      dvi_out(post);
      dvi_four(last_bop);
      last_bop = dvi_offset + dvi_ptr - 5;
      dvi_four(25400000L);
      dvi_four(473628672L);
      prepare_mag();
      dvi_four(mag);
      dvi_four(max_v);
      dvi_four(max_h);
      dvi_out(max_push / 256);
      dvi_out(max_push % 256);

      if (total_pages >= 65536)
      {
        sprintf(log_line, "\nWARNING: page count (dvi_t) in DVI file will be %lld not %lld\n",
          (total_pages % 65536), total_pages);

        if (log_opened)
          fputs(log_line, log_file);

        show_line(log_line, 1);
      }

      dvi_out((total_pages / 256) % 256);
      dvi_out(total_pages % 256);

      if (show_fonts_used && log_opened)
        show_font_info();

      while (font_ptr > 0)
      {
        if (font_used[font_ptr])
          dvi_font_def(font_ptr);

        decr(font_ptr);
      }

      dvi_out(post_post);
      dvi_four(last_bop);
      dvi_out(id_byte);
      k = 4 + ((dvi_buf_size - dvi_ptr) % 4);

      while (k > 0)
      {
        dvi_out(223);
        decr(k);
      }

      if (trace_flag)
        printf("\ndvi_write %lld", dvi_gone);

      if (dvi_limit == half_buf)
        write_dvi(half_buf, dvi_buf_size - 1);

      if (dvi_ptr > 0)
        write_dvi(0, dvi_ptr - 1);
      
      print_nl("Output written on ");

      if (full_file_name_flag && dvi_file_name != NULL)
        prints(dvi_file_name);
      else
        slow_print(output_file_name);

      prints(" (");
      print_int(total_pages);
      prints(" page");

      if (total_pages != 1)
        print_char('s');

      prints(", ");
      print_int(dvi_offset + dvi_ptr);
      prints(" bytes).");
      b_close(dvi_file);
    }
  }
*/
  if (log_opened)
  {
    wlog_cr();
    a_close(log_file);
    selector = selector - 2;

    if (selector == term_only)
    {
      print_nl("Transcript written on ");

      if (full_file_name_flag && log_file_name != NULL)
        prints(log_file_name);
      else
        slow_print(log_name);

      print_char('.');
    }
  }

  print_ln();

  if ((edit_name_start != 0) && (interaction > 0))
    call_edit(str_pool, edit_name_start, edit_name_length, edit_line);
}
#ifdef DEBUG
/* sec 1338 */
void debug_help (void) 
{
  integer k, l, m, n;

  while (true)
  {
    wake_up_terminal();
    print_nl(" debug # (-1 to exit):");
    update_terminal();
    read(stdin, m);  // ???

    if (m < 0)
      return;
    else if (m == 0)
      dumpcore();
    else
    {
      read(stdin, n);

      switch (m)
      {
        case 1:
          print_word(mem[n]);
          break;

        case 2:
          print_int(info(n));
          break;
          
        case 3:
          print_int(link(n));
          break;
        
        case 4:
          print_word(eqtb[n]);
          break;

        case 5:
          print_word(font_info[n]);
          break;
        
        case 6:
          print_word(save_stack[n]);
          break;
          
        case 7:
          show_box(n);
          break;
        
        case 8:
          {
            breadth_max = 10000;
#ifdef ALLOCATESTRING
            if (pool_ptr + 32000 > current_pool_size)
              str_pool = realloc_str_pool (increment_pool_size);

            depth_threshold = current_pool_size - pool_ptr - 10;
#else
            depth_threshold = pool_size - pool_ptr - 10;
#endif
            show_node_list(n);
          }
          break;
        
        case 9:
          show_token_list(n, 0, 1000);
          break;
        
        case 10:
          slow_print(n);
          break;
        
        case 11:
          check_mem(n > 0);
          break;
        
        case 12:
          search_mem(n);
          break;
        
        case 13:
          {
            read(stdin, l);
            print_cmd_chr(n, l);
          }
          break;
        
        case 14:
          {
            for (k = 0; k <= n; k++)
              print(buffer[k]);
          }
          break;
        
        case 15:
          {
            font_in_short_display = 0;
            short_display(n);
          }
          break;
        
        case 16:
          panicking = !panicking;
          break;
        
        default:
          print('?');
          break;
      }
    }
  }
}
#endif