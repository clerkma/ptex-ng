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

/* sec 0058 */
void print_ln (void)
{
  integer ii;

  switch (selector)
  {
    case term_and_log:
      if (nrestmultichr(kcode_pos) > 0)
        for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
        {
          wterm(' ');
          wlog(' ');
        }
      wterm_cr();
      term_offset = 0;
      wlog_cr();
      file_offset = 0;
      break;

    case log_only:
      if (nrestmultichr(kcode_pos) > 0)
        for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
        {
          wlog(' ');
        }
      wlog_cr();
      file_offset = 0;
      break;

    case term_only:
      if (nrestmultichr(kcode_pos) > 0)
        for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
        {
          wterm(' ');
        }
      wterm_cr();
      term_offset = 0;
      break;

    case no_print:
    case pseudo:
    case new_string:
      do_nothing();
      break;

    default:
      putc('\n', write_file[selector]);
      break;
  }

  kcode_pos = 0;
}
/* sec 0058 */
void print_char_ (ASCII_code s)
{
  if (s == new_line_char)
    if (selector < pseudo)
    {
      print_ln();
      return;
    }

  if ((kcode_pos == 1) ||
    ((kcode_pos >= 011) && (kcode_pos <= 012)) ||
    ((kcode_pos >= 021) && (kcode_pos <= 023)))
    incr(kcode_pos);
  else if (iskanji1(xchr[s]))
  {
    if (ismultichr(4, 1, xchr[s]))
      kcode_pos = 021;
    else if (ismultichr(3, 1, xchr[s]))
      kcode_pos = 011;
    else
      kcode_pos = 1;

    if ((selector == term_and_log) || (selector == log_only))
    {
      if (file_offset >= max_print_line - nrestmultichr(kcode_pos))
      {
        wlog_cr();
        file_offset = 0;
      }
    }

    if ((selector == term_and_log) || (selector == term_only))
    {
      if (term_offset >= max_print_line - nrestmultichr(kcode_pos))
      {
        wterm_cr();
        term_offset = 0;
      }
    }
  }
  else
  {
    kcode_pos = 0;
  }

  switch (selector)
  {
    case term_and_log:
      wterm(xchr[s]);
      incr(term_offset);
      wlog(xchr[s]);
      incr(file_offset);

      if (term_offset == max_print_line)
      {
        wterm_cr();
        term_offset = 0;
      }
      
      if (file_offset == max_print_line)
      {
        wlog_cr();
        file_offset = 0;
      }

      break;

    case log_only:
      wlog(xchr[s]);
      incr(file_offset);

      if (file_offset == max_print_line)
        print_ln();

      break;

    case term_only:
      wterm(xchr[s]);
      incr(term_offset);

      if (term_offset == max_print_line)
        print_ln();

      break;

    case no_print:
      do_nothing();
      break;

    case pseudo:
      if (tally < trick_count)
      {
        trick_buf[tally % error_line] = s;
        trick_buf2[tally % error_line] = kcode_pos;
      }
      break;

    case new_string:
#ifdef ALLOCATESTRING
      if (pool_ptr + 1 > current_pool_size)
        str_pool = realloc_str_pool(increment_pool_size);
      
      if (pool_ptr < current_pool_size)
        append_char(s);
#else
      if (pool_ptr < pool_size)
        append_char(s);
#endif
      break;

    default:
      putc(xchr[s], write_file[selector]);
      break;
  }

  incr(tally);
}
/* sec 0059 */
void print_ (integer s)
{
  pool_pointer j;
  integer nl;

  if (s >= str_ptr)
    s = 259; /* ??? */
  else
  {
    if (s < 256)
    {
      if (s < 0)
        s = 259; /* ??? */
      else
      {
        if (selector > pseudo)
        {
          print_char(s);
          return;
        }

        if (s == new_line_char)
          if (selector < pseudo)
          {
            print_ln();
            return;
          }
          
        nl = new_line_char;
        new_line_char = -1;

        if (!show_in_hex && s < 256 && s >= 32)
        {
          print_char(s);       /* don't translate to hex */
        }
        else
        {                       /* not just a character */
          j = str_start[s];

          while (j < str_start[s + 1])
          {
            print_char(str_pool[j]);
            incr(j);
          }
        }

        new_line_char = nl;
        return;
      }
    }
  }

  j = str_start[s];

  while (j < str_start[s + 1])
  {
    print_char(str_pool[j]);
    incr(j);
  }
}
/* string version print. */
void prints_ (const char * s)
{
  while (*s)
    print_char(*s++);
}
/* sec 0060 */
void slow_print_ (integer s)
{
  pool_pointer j;

  if ((s >= str_ptr) || (s < 256))
    print(s);
  else
  {
    j = str_start[s];

    while (j < str_start[s + 1])
    {
      print(str_pool[j]);
      incr(j);
    }
  }
}
/* sec 0062 */
void print_nl (const char * s)
{
  if (((term_offset > 0) && (odd(selector))) ||
      ((file_offset > 0) && (selector >= log_only)))
    print_ln();

  prints(s);
}
/* sec 0063 */
void print_esc (const char * s)
{
  integer c;

  c = escape_char;

  if (c >= 0)
    if (c < 256)
      print(c);

  prints(s);
}
/* sec 0064 */
void print_the_digs (eight_bits k)
{
  while (k > 0)
  {
    decr(k);

    if (dig[k] < 10)
      print_char('0' + dig[k]);
    else
      print_char('A' + dig[k]);
  }
}
/* sec 0065 */
void print_int_ (integer n)
{
  char k;
  integer m;

  k = 0;

  if (n < 0)
  {
    print_char('-');

    if (n > -100000000L)
      n = - (integer) n;
    else
    {
      m = -1 - n;
      n = m / 10;
      m = (m % 10) + 1;
      k = 1;

      if (m < 10)
        dig[0] = (char) m;
      else
      {
        dig[0] = 0;
        incr(n);
      }
    }
  }

  do
    {
      dig[k] = (char) (n % 10);
      n = n / 10;
      incr(k);
    }
  while (!(n == 0));

  print_the_digs(k);
}
/* sec 0262 */
void print_cs_ (integer p)
{
  if (p < hash_base)
    if (p >= single_base)
      if (p == null_cs)
      {
        print_esc("csname");
        print_esc("endcsname");
        print_char(' ');
      }
      else
      {
        print_esc(""); print(p - single_base);

        if (cat_code(p - single_base) == letter)
          print_char(' ');
      }
    else if (p < active_base)
      print_esc("IMPOSSIBLE.");
    else
      print(p - active_base);
  else if (p >= undefined_control_sequence)
    print_esc("IMPOSSIBLE.");
  else if ((text(p) >= str_ptr))
    print_esc("NONEXISTENT.");
  else
  {
    print_esc("");
    print(text(p));
    print_char(' ');
  }
}
/* sec 0263 */
void sprint_cs (pointer p)
{ 
  if (p < hash_base)
    if (p < single_base)
      print(p - active_base);
    else if (p < null_cs)
    {
      print_esc("");
      print(p - single_base);
    }
    else
    {
      print_esc("csname");
      print_esc("endcsname");
    }
  else
  {
    print_esc(""); print(text(p));
  }
}
/* sec 0518 */
void print_file_name (integer n, integer a, integer e)
{
  slow_print(a);
  slow_print(n);
  slow_print(e);
}
/* sec 0699 */
void print_size (integer s)
{ 
  if (s == 0)
    print_esc("textfont");
  else if (s == 16)
    print_esc("scriptfont");
  else
    print_esc("scriptscriptfont");
} 
/* sec 1355 */
void print_write_whatsit_(const char * s, pointer p)
{
  print_esc(s);

  if (write_stream(p) < 16)
    print_int(write_stream(p)); 
  else if (write_stream(p) == 16)
    print_char('*');
  else
    print_char('-');
}
/* sec 0081 */
void jump_out (void) 
{
  close_files_and_terminate();

  {
    int code;

    fflush(stdout); 
    ready_already = 0;

    if (trace_flag)
      puts("EXITING at JUMPOUT");

    if ((history != spotless) && (history != warning_issued))
      code = 1;
    else
      code = 0;

    uexit(code);
  }
}
/* sec 0082 */
void error (void)
{
  ASCII_code c;
  integer s1, s2, s3, s4;

  if (history < error_message_issued)
    history = error_message_issued;

  print_char('.');
  show_context();

  if (interaction == error_stop_mode)
    while (true)
    {
continu:
      clear_for_error_prompt();
      prompt_input("? ");

      if (last == first)
        return; // no input

      c = buffer[first];

      if (c >= 'a')
        c = (c + 'A' - 'a'); 

      switch (c)
      {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          if (deletions_allowed)
          {
            s1 = cur_tok;
            s2 = cur_cmd;
            s3 = cur_chr;
            s4 = align_state;
            align_state = 1000000L;
            OK_to_interrupt = false;

            if ((last > first + 1) && (buffer[first + 1] >= '0') && (buffer[first + 1] <= '9'))
              c = (c * 10 + buffer[first + 1] - '0' * 11);
            else
              c = (c - 48);
            
            while (c > 0)
            {
              get_token();
              decr(c);
            }

            cur_tok = s1;
            cur_cmd = s2;
            cur_chr = s3;
            align_state = s4;
            OK_to_interrupt = true;
            help2("I have just deleted some text, as you asked.",
                "You can now delete more, or insert, or whatever.");
            show_context();
            goto continu;
          }
          break;

#ifdef DEBUG
        case 'D':
          {
            debug_help();
            goto continu;
          }
          break;
#endif

        case 'E':
          if (base_ptr > 0)
          {
            edit_name_start = str_start[input_stack[base_ptr].name_field];
            edit_name_length = length(input_stack[base_ptr].name_field);
            edit_line = line;
            jump_out();
          }
          break;

        case 'H':
          {
            if (use_err_help)
            {
              give_err_help();
              use_err_help = false;
            }
            else
            {
              if (help_ptr == 0)
                help2("Sorry, I don't know how to help in this situation.",
                    "Maybe you should try asking a human?");
              do
                {
                  decr(help_ptr);
                  prints(help_line[help_ptr]);
                  print_ln();
                }
              while (!(help_ptr == 0));
            }

            help4("Sorry, I already gave what help I could...",
                "Maybe you should try asking a human?",
                "An error might have occurred before I noticed any problems.",
                "``If all else fails, read the instructions.''");
            goto continu;
          }
          break;

        case 'I':
          {
            begin_file_reading();

            if (last > first + 1)
            {
              loc = first + 1;
              buffer[first] = ' ';
            }
            else
            {
              prompt_input("insert>");
              loc = first;
            }

            first = last;
            limit = last - 1;

            return;
          }
          break;

        case 'Q':
        case 'R':
        case 'S':
          {
            error_count = 0; 
            interaction = 0 + c - 'Q';
            prints("OK, entering ");

            switch (c)
            {
              case 'Q':
                print_esc("batchmode");
                decr(selector);
                break;

              case 'R':
                print_esc("nonstopmode");
                break;

              case 'S':
                print_esc("scrollmode");
                break;
            }

            prints("...");
            print_ln();
            update_terminal();
            return;
          }
          break;

        case 'X':
          {
            interaction = scroll_mode;
            jump_out();
          }
          break;

        default:
          break;
      }

      {
        prints("Type <return> to proceed, S to scroll future error messages,");
        print_nl("R to run without stopping, Q to run quietly,");
        print_nl("I to insert something, ");

        if (base_ptr > 0)
          prints("E to edit your file,");

        if (deletions_allowed)
          print_nl("1 or ... or 9 to ignore the next 1 to 9 tokens of input,");

        print_nl("H for help, X to quit.");
      }
    }

  incr(error_count);

  if (error_count == 100)
  {
    print_nl("(That makes 100 errors; please try again.)");
    history = fatal_error_stop;
    jump_out();
  }

  if (interaction > batch_mode)
    decr(selector);

  if (use_err_help)
  {
    print_ln();
    give_err_help();
  }
  else while (help_ptr > 0)
  {
    decr(help_ptr);
    print_nl(help_line[help_ptr]);
  }

  print_ln();

  if (interaction > batch_mode)
    incr(selector);
  
  print_ln();
}
/* sec 0093 */
void fatal_error (const char * s)
{
  normalize_selector();
  print_err("Emergency stop");
  help1(s);
  succumb();
}
/* sec 0094 */
void overflow_(const char * s, integer n)
{
  normalize_selector();
  print_err("TeX capacity exceeded, sorry [");
  prints(s);
  print_char('=');
  print_int(n);
  print_char(']');
  help2("If you really absolutely need more capacity,",
      "you can ask a wizard to enlarge me.");

  if (!knuth_flag)
  {
    if (!strcmp(s, "pattern memory") && (n == trie_size))
      printf("\n  (Maybe use -h=... on command line in initex)\n");
    else if (!strcmp(s, "exception dictionary") && (n == hyphen_prime))
      printf("\n  (Maybe use -e=... on command line in initex)\n");
  }

  succumb();
}
/* sec 0095 */
void confusion (const char * s)
{
  normalize_selector();

  if (history < error_message_issued)
  {
    print_err("This can't happen (");
    prints(s);
    print_char(')');
    help1("I'm broken. Please show this to someone who can fix can fix");
  }
  else
  {
    print_err("I can't go on meeting you like this");
    help2("One of your faux pas seems to have wounded me deeply...",
        "in fact, I'm barely conscious. Please fix it and try again.");
  }

  succumb();
}
/* sec 0037 */
boolean init_terminal (void)
{
  boolean flag;

  t_open_in();

  if (last > first)
  {
    loc = first;

    while ((loc < last) && (buffer[loc]== ' '))
      incr(loc);

    if (loc < last)
      return true;
  }

// failed to find input file name
  while (true)
  {
    wake_up_terminal();
    fputs("**", stdout);
    update_terminal();
    flag = input_ln(stdin, true);

    if (!flag)
    {
      wterm_cr();
      puts("! End of file on the terminal... why?\n");
      return false;
    }

    loc = first;

    while ((loc < last) && (buffer[loc]== ' '))
      incr(loc);    // step over intial white space

    if (loc < last)
      return true;

    printf("%s\n", "Please type the name of your input file.");
  }
}
/* sec 0043 */
str_number make_string (void)
{
#ifdef ALLOCATESTRING
  if (str_ptr == current_max_strings)
    str_start = realloc_str_start(increment_max_strings);

  if (str_ptr == current_max_strings)
  {
    overflow("number of strings", current_max_strings - init_str_ptr);
    return 0;
  }
#else
  if (str_ptr == max_strings)
  {
    overflow("number of strings", max_strings - init_str_ptr);
    return 0;
  }
#endif

  incr(str_ptr);
  str_start[str_ptr] = pool_ptr;

  return (str_ptr - 1);
}
/* sec 0044 */
boolean str_eq_buf_ (str_number s, integer k)
{
  pool_pointer j;
  boolean result;

  j = str_start[s];

  while (j < str_start[s + 1])
  {
    if (str_pool[j] != buffer[k])
    {
      result = false;
      goto not_found;
    }

    incr(j);
    incr(k);
  }

  result = true;

not_found:
  return result;
}
/* sec 0045 */
boolean str_eq_str_ (str_number s, str_number t)
{
  pool_pointer j, k;
  boolean result;

  result = false;

  if (length(s) != length(t))
    goto not_found;

  j = str_start[s];
  k = str_start[t];

  while (j < str_start[s + 1])
  {
    if (str_pool[j] != str_pool[k])
      goto not_found;

    incr(j);
    incr(k);
  }

  result = true;

not_found:
  return result;
}
/* sec 0066 */
void print_two_(integer n)
{ 
  n = abs(n) % 100;
  print_char('0' + (n / 10));
  print_char('0' + (n % 10));
} 
/* sec 0067 */
void print_hex_(integer n)
{
  char k;

  k = 0;
  print_char('"');

  do
    {
      dig[k] = (unsigned char) (n % 16);
      n = n / 16;
      incr(k);
    }
  while (!(n == 0));

  print_the_digs(k);
}
/* sec 0069 */
void print_roman_int_(integer n)
{
  pool_pointer j, k;
  nonnegative_integer u, v;

  j = str_start[260]; /* m2d5c2l5x2v5i */
  v = 1000;

  while (true)
  {
    while (n >= v)
    {
      print_char(str_pool[j]);
      n = n - v;
    }

    if (n <= 0)
      return;

    k = j + 2;
    u = v / (str_pool[k - 1] - '0');

    if (str_pool[k - 1] == 50)
    {
      k = k + 2;
      u = u / (str_pool[k - 1] - '0');
    }

    if (n + u >= v)
    {
      print_char(str_pool[k]);
      n = n + u;
    }
    else
    {
      j = j + 2;
      v = v / (str_pool[j - 1] - '0');
    }
  }
}
/* sec 0070 */
void print_current_string (void)
{
  pool_pointer j;

  j = str_start[str_ptr];

  while (j < pool_ptr)
  {
    print_char(str_pool[j]);
    incr(j);
  }
}

/* sec 0071 */
void term_input (void)
{ 
  integer k;
  
  if (!knuth_flag)
    show_line("\n", 0);

  update_terminal();

  if (!input_ln(stdin, true))
  {
    fatal_error("End of file on the terminal!");
    return;
  }

  term_offset = 0;
  decr(selector);

  if (last != first)
    for (k = first; k <= last - 1; k++)
      print(buffer[k]);

  print_ln();
  incr(selector);
}
/* sec 0091 */
void int_error_ (integer n)
{
  prints(" (");
  print_int(n);
  print_char(')');
  error();
}
/* sec 0092 */
void normalize_selector (void)
{
  if (log_opened)
    selector = term_and_log;
  else
    selector = term_only;

  if (job_name == 0)
    open_log_file();

  if (interaction == batch_mode)
    decr(selector);
}
/* sec 0098 */
void pause_for_instructions (void)
{
  if (OK_to_interrupt)
  {
    interaction = error_stop_mode;

    if ((selector == log_only) || (selector == no_print))
      incr(selector);

    print_err("Interruption");
    help3("You rang?",
        "Try to insert some instructions for me (e.g.,`I\\showlists'),",
        "unless you just want to quit by typing `X'.");
    deletions_allowed = false;
    error();
    deletions_allowed = true;
    interrupt = 0;
  }
}
/* sec 0100 */
integer half_(integer x)
{
  if (odd(x))
    return ((x + 1) / 2);
  else
    return (x / 2);
}
/* sec 0102 */
scaled round_decimals_(small_number k)
{
  integer a;

  a = 0;

  while (k > 0)
  {
    decr(k);
    a = (a + dig[k] * 131072L) / 10; /* 2^17 */
  }
  
  return ((a + 1) / 2);
}
/* sec 0103 */
void print_scaled_(scaled s)
{
  scaled delta;

  if (s < 0)
  {
    print_char('-');
    s = - (integer) s;
  }

  print_int(s / 65536L);
  print_char('.');
  s = 10 * (s % 65536L) + 5;
  delta = 10;

  do
    {
      if (delta > 65536L)
        s = s - 17232; /* 2^15 - 50000 - rounding */

      print_char('0' + (s / 65536L));
      s = 10 * (s % 65536L);
      delta = delta * 10;
    }
  while (!(s <= delta));
}
/* sec 0105 */
scaled mult_and_add_(integer n, scaled x, scaled y, scaled max_answer)
{
  if (n < 0)
  {
    x = - (integer) x;
    n = - (integer) n;
  }

  if (n == 0)
    return y;
  else if (((x <= (max_answer - y) / n) && (- (integer) x <= (max_answer + y) / n)))
    return (n * x + y); 
  else
  {
    arith_error = true;
    return 0;
  }
}
/* sec 0106 */
scaled x_over_n_(scaled x, integer n)
{
  register scaled Result;
  boolean negative;

  negative = false;

  if (n == 0)
  {
    arith_error = true;
    Result = 0;
    tex_remainder = x;
  }
  else
  {
    if (n < 0)
    {
      x = - (integer) x;
      n = - (integer) n;
      negative = true;
    }

    if (x >= 0)
    {
      Result = x / n;
      tex_remainder = x % n;
    }
    else
    {
      Result = - (integer) ((- (integer) x) / n);
      tex_remainder = - (integer) ((- (integer) x) % n);
    }
  }

  if (negative)
    tex_remainder = - (integer) tex_remainder;

  return Result;
}
/* sec 0107 */
scaled xn_over_d_(scaled x, integer n, integer d)
{
  register scaled Result;
  boolean positive;
  nonnegative_integer t, u, v;

  if (x >= 0)
    positive = true; 
  else
  {
    x = - (integer) x;
    positive = false;
  }

  t = (x % 32767L) * n;
  u = (x / 32768L) * n + (t / 32768L);
  v = (u % d) * 32768L + (t % 32768L); 

  if (u / d >= 32768L)
    arith_error = true; 
  else
    u = 32768L * (u / d) + (v / d);

  if (positive)
  {
    Result = u;
    tex_remainder = v % d;
  }
  else
  {
    Result = - (integer) u;
    tex_remainder = - (integer)(v % d);
  }

  return Result;
}
/* sec 0108 */
halfword badness_(scaled t, scaled s)
{
  integer r;

  if (t == 0)
    return 0;
  else if (s <= 0)
    return 10000;
  else
  {
    if (t <= 7230584L)
      r = (t * 297) / s;
    else if (s >= 1663497L)
      r = t / (s / 297);
    else
      r = t;

    if (r > 1290)
      return 10000; 
    else
      return (r * r * r + 131072L) / 262144L;  /* 2^17 */
  }
}
/* sec 0114 */
#ifdef DEBUG
void print_word (memory_word w)
{ 
  print_int(w.cint); 
  print_char(' ');
  print_scaled(w.cint); 
  print_char(' ');
  print_scaled(round(unity * w.gr));
  print_ln();
  print_int(w.hh.lh);
  print_char('=');
  print_int(w.hh.b0);
  print_char(':');
  print_int(w.hh.b1);
  print_char(';');
  print_int(w.hh.rh);
  print_char(' ');
  print_int(w.qqqq.b0); 
  print_char(':');
  print_int(w.qqqq.b1); 
  print_char(':');
  print_int(w.qqqq.b2); 
  print_char(':');
  print_int(w.qqqq.b3);
}
#endif
/* sec 0292 */
void show_token_list_(integer p, integer q, integer l)
{
  integer m, c;
  ASCII_code match_chr;
  ASCII_code n;

  match_chr = '#';
  n = '0';
  tally = 0;

  while ((p != 0) && (tally < l))
  {
    if (p == q)
    {
      first_count = tally;
      trick_count = tally + 1 + error_line - half_error_line;

      if (trick_count < error_line)
        trick_count = error_line;
    }

    if ((p < hi_mem_min) || (p > mem_end))
    {
      print_esc("CLOBBERED.");
      return;
    }

    if (info(p) >= cs_token_flag)
      print_cs(info(p) - cs_token_flag);
    else
    {
      if (check_kanji(info(p)))
      {
        m = info(p) / max_cjk_val;
        c = info(p) % max_cjk_val;
      }
      else
      {
        m = info(p) / max_char_val;
        c = info(p) % max_char_val;
      }

      if ((m < kanji) && (c > 256))
        print_esc("BAD.");
      else switch (m)
      {
        case kanji:
        case kana:
        case other_kchar:
        case hangul:
          print_kanji(KANJI(c));
          break;

        case left_brace:
        case right_brace:
        case math_shift:
        case tab_mark:
        case sup_mark:
        case sub_mark:
        case spacer:
        case letter:
        case other_char:
          print(c);
          break;
        
        case mac_param:
          print(c);
          print(c);
          break;
        
        case out_param:
          print(match_chr);
          
          if (c <= 9)
            print_char(c + '0');
          else
          {
            print_char('!');
            return;
          }
          break;
        
        case match:
          match_chr = (ASCII_code) c;
          print(c);
          incr(n);
          print_char(n);
          
          if (n > '9')
            return;
          break;
        
        case end_match:
          if (c == 0)
            prints("->");
          break;
        
        default:
          print_esc("BAD.");
          break;
      }
    }

    p = link(p);
  }

  if (p != 0)
    print_esc("ETC.");
}
/* sec 0306 */
void runaway (void)
{
  pointer p;

  if (scanner_status > skipping)
  {
    print_nl("Runaway ");

    switch (scanner_status)
    {
      case defining:
        prints("definition");
        p = def_ref;
        break;

      case matching:
        prints("argument");
        p = temp_head;
        break;

      case aligning:
        prints("preamble");
        p = hold_head;
        break;

      case absorbing:
        prints("text");
        p = def_ref;
        break;
    }

    print_char('?');
    print_ln();
    show_token_list(link(p), 0, error_line - 10); 
  }
}
/* sec 0120 */
/* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** */
/* first try list of available nodes (avail != NULL)                   */
/* then see if can go upwards (mem_end < mem_max)                      */
/* then see if can go downwards (hi_mem_min > lo_mem_max)              */
/* if not, extend memory at the top and grab from there --- new        */
/* else fail ! paragraph 120                                           */
/* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** */
pointer get_avail (void)
{
  pointer p;

  p = avail;

  if (p != 0)
    avail = link(avail);
  else if (mem_end < mem_max)
  {
    incr(mem_end);
    p = mem_end;
  }
  else
  {
    decr(hi_mem_min);
    p = hi_mem_min;

    if (hi_mem_min <= lo_mem_max) /* have we run out in middle ? */
    {
      incr(hi_mem_min);
      mem = realloc_main (0, mem_top / 2);  /* zzzaa = zmem = mem */

      if (mem == NULL)
        return 0;

      if (mem_end >= mem_max)
      {
        runaway();
        overflow("main memory size", mem_max + 1 - mem_min);
        return 0;
      }

      incr(mem_end);        /* then grab from new area */
      p = mem_end;          /* 1993/Dec/14 */
    }
  }

  link(p) = 0;       /* link(p) = null !!! */

#ifdef STAT
  incr(dyn_used); 
#endif

  return p; 
} 
/* sec 0123 */
void flush_list_(pointer p)
{ 
  pointer q, r;

  if (p != 0)
  {
    r = p;

    do
      {
        q = r;
        r = link(r);
#ifdef STAT
        decr(dyn_used);
#endif
      }
    while (!(r == 0));

    link(q) = avail;
    avail = p;
  }
}
/* sec 0125 */
pointer get_node (integer s)
{
  pointer p;
  pointer q;
  integer r;
  integer t;

restart:
  p = rover;

  do
    {
      q = p + node_size(p);

      while (is_empty(q))
      {
        t = rlink(q);

        if (q == rover)
          rover = t;

        llink(t) = llink(q);
        rlink(llink(q)) = t;
        q = q + node_size(q);
      }

      r = q - s;

      if (r > toint(p + 1)) 
      {
        node_size(p) = r - p;
        rover = p;
        goto found;
      }

      if (r == p)
        if (rlink(p) != p)
        {
          rover = rlink(p);
          t = llink(p);
          llink(rover) = t;
          rlink(t) = rover;
          goto found;
        }

      node_size(p) = q - p;
      p = rlink(p);
    }
  while (!(p == rover));

  if (s == 1073741824L)    /* 2^30 - special case - merge adjacent */
  {
    if (trace_flag)
      puts("Merged adjacent multi-word nodes");

    return max_halfword;
  }

/*  maybe try downward epxansion first instead ? */
  if (lo_mem_max + 2 < hi_mem_min)
    if (lo_mem_max + 2 <= mem_bot + max_halfword)  /* silly ? flush 93/Dec/16 */
    {
      if (hi_mem_min - lo_mem_max >= (block_size + block_size - 2))
        t = lo_mem_max + block_size;
      else
        t = lo_mem_max + 1 + (hi_mem_min - lo_mem_max) / 2;

      p = llink(rover);
      q = lo_mem_max;
      rlink(p) = q;
      llink(rover) = q;

      if (t > mem_bot + max_halfword)
        t = mem_bot + max_halfword;     /* silly ? flush 93/Dec/16 */

      rlink(q) = rover;
      llink(q) = p;
      link(q) = empty_flag;
      node_size(q) = t - lo_mem_max; /* block size */
      lo_mem_max = t;
      link(lo_mem_max) = 0;
      info(lo_mem_max) = 0;
      rover = q;
      goto restart;
    }

/* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** */
/* we've run out of space in the middle for variable length blocks */
/* try and add new block from below mem_bot *//* first check if space ! */
  if (mem_min - (block_size + 1) <= mem_start) /* extend lower memory downwards */
  {
    mem = realloc_main (mem_top / 2 + block_size, 0);  /* zzzaa = zmem = mem */

    if (mem == NULL)
    {
      return 0;
    }
  }

  if (mem_min - (block_size + 1) <= mem_start) /* check again */
  {
    if (trace_flag)
      printf("mem_min %lld, mem_start %d, block_size %d\n", mem_min, mem_start, block_size);

    overflow("main memory size", mem_max + 1 - mem_min); /* darn: allocation failed ! */
    return 0;
  }

  add_variable_space(block_size);
  goto restart; /* go try get_node again */

found:
  link(r) = 0;

#ifdef STAT
  var_used = var_used + s; 
#endif

  return r; 
} 
/* sec 0130 */
void free_node (pointer p, halfword s)
{ 
  pointer q;

  node_size(p) = s;
  link(p) = empty_flag;
  q = llink(rover);
  llink(p) = q;
  rlink(p) = rover;
  llink(rover) = p;
  rlink(q) = p;

#ifdef STAT
  var_used = var_used - s; 
#endif
}
/* sec 0136 */
pointer new_null_box (void) 
{
  pointer p;

  p = get_node(box_node_size);
  type(p) = hlist_node;
  subtype(p) = min_quarterword;
  width(p) = 0;
  depth(p) = 0;
  height(p) = 0;
  shift_amount(p) = 0;
  list_ptr(p) = 0;
  glue_sign(p) = normal;
  glue_order(p) = normal;
  set_glue_ratio_zero(glue_set(p));
  space_ptr(p) = zero_glue;
  xspace_ptr(p) = zero_glue;
  add_glue_ref(zero_glue);
  add_glue_ref(zero_glue);

  return p;
}
/* sec 0139 */
pointer new_rule (void) 
{
  pointer p;

  p = get_node(rule_node_size);
  type(p) = rule_node;
  subtype(p) = 0;
  width(p) = null_flag;
  depth(p) = null_flag;
  height(p) = null_flag;

  return p;
}
/* sec 0144 */
pointer new_ligature_(quarterword f, quarterword c, pointer q)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = ligature_node;
  font(lig_char(p)) = f;
  character(lig_char(p)) = c;
  lig_ptr(p) = q;
  subtype(p) = 0;

  return p;
}
/* sec 0144 */
pointer new_lig_item_(quarterword c)
{
  pointer p;

  p = get_node(small_node_size);
  character(p) = c;
  lig_ptr(p) = 0;

  return p;
}
/* sec 0145 */
pointer new_disc (void) 
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = disc_node;
  replace_count(p) = 0;
  pre_break(p) = 0;
  post_break(p) = 0;

  return p;
}
/* sec 0147 */
pointer new_math (scaled w, small_number s)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = math_node;
  subtype(p) = s;
  width(p) = w;

  return p;
}
/* sec 0151 */
pointer new_spec_(pointer p)
{
  pointer q;

  q = get_node(glue_spec_size);
  mem[q] = mem[p];
  glue_ref_count(q) = 0;
  width(q) = width(p);
  stretch(q) = stretch(p);
  shrink(q) = shrink(p);

  return q;
}
/* se 0152 */
pointer new_param_glue (small_number n)
{
  pointer p;
  pointer q;

  p = get_node(small_node_size);
  type(p) = glue_node;
  subtype(p) = n + 1;
  leader_ptr(p) = 0;
  q = glue_par(n);
  glue_ptr(p) = q;
  incr(glue_ref_count(q));

  return p;
}
/* sec 0153 */
pointer new_glue (pointer q)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = glue_node;
  subtype(p) = normal;
  leader_ptr(p) = 0; 
  glue_ptr(p) = q;
  incr(glue_ref_count(q));

  return p;
}
/* sec 0154 */
pointer new_skip_param (small_number n)
{
  pointer p;

  temp_ptr = new_spec(glue_par(n));
  p = new_glue(temp_ptr);
  glue_ref_count(temp_ptr) = 0;
  subtype(p) = n + 1;

  return p;
}
/* sec 0155 */
pointer new_kern(scaled w)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = kern_node;
  subtype(p) = normal;
  width(p) = w;

  return p;
}
/* sec 0158 */
pointer new_penalty(integer m)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = penalty_node;
  subtype(p) = 0;
  penalty(p) = m;

  return p;
}

#ifdef DEBUG
/* sec 0167 */
void check_mem (boolean print_locs)
{
  pointer p, q;
  boolean clobbered;

  for (p = mem_min; p <= lo_mem_max; p++)
    freearr[p] = false;

  for (p = hi_mem_min; p <= mem_end; p++)
    freearr[p] = false;

  p = avail;
  q = 0;
  clobbered = false;

  while (p != 0)
  {
    if ((p > mem_end) || (p < hi_mem_min))
      clobbered = true;
    else if (freearr[p])
      clobbered = true;

    if (clobbered)
    {
      print_nl("AVAIL list clobbered at ");
      print_int(q);
      goto done1;
    }

    freearr[p] = true;
    q = p;
    p = link(q);
  }

done1:
  p = rover;
  q = 0;
  clobbered = false;

  do
    {
      if ((p >= lo_mem_max) || (p < mem_min))
        clobbered = true;
      else if ((rlink(p) >= lo_mem_max) || (rlink(p) < mem_min))
        clobbered = true;
      else if (!(is_empty(p)) || (node_size(p) < 2) ||
          (p + node_size(p) > lo_mem_max) || (llink(rlink(p)) != p))
        clobbered = true;
      
      if (clobbered)
      {
        print_nl("Double-AVAIL list clobbered at ");
        print_int(q);
        goto done2;
      }

      for (q = p; q <= p + node_size(p) - 1; q++)
      {
        if (freearr[q])
        {
          print_nl("Doubly free location at ");
          print_int(q);
          goto done2;
        }

        freearr[q] = true;
      }

      q = p;
      p = rlink(p);
    }
  while (!(p == rover));

done2:
  p = mem_min;

  while (p <= lo_mem_max)
  {
    if (is_empty(p))
    {
      print_nl("Bad flag at ");
      print_int(p);
    }

    while ((p <= lo_mem_max) && !freearr[p])
      incr(p);

    while ((p <= lo_mem_max) && freearr[p])
      incr(p);
  }

  if (print_locs)
  {
    print_nl("New busy locs:");

    for (p = mem_min; p <= lo_mem_max; p++)
      if (!freearr[p] && ((p > was_lo_max) || wasfree[p]))
      {
        print_char(' ');
        print_int(p);
      }

    for (p = hi_mem_min; p <= mem_end; p++)
      if (!freearr[p] && ((p < was_hi_min) || (p > was_mem_end) || wasfree[p]))
      {
        print_char(' ');
        print_int(p);
      }
  }

  for (p = mem_min; p <= lo_mem_max; p++)
    wasfree[p] = freearr[p];

  for (p = hi_mem_min; p <= mem_end; p++)
    wasfree[p] = freearr[p];

  was_mem_end = mem_end;
  was_lo_max = lo_mem_max;
  was_hi_min = hi_mem_min;
}
/* sec 0172 */
void search_mem_(pointer p)
{
  integer q;

  for (q = mem_min; q <= lo_mem_max; q++)
  {
    if (link(q) == p)
    {
      print_nl("LINK(");
      print_int(q);
      print_char(')');
    }

    if (info(q) == p)
    {
      print_nl("INFO(");
      print_int(q);
      print_char(')');
    }
  }

  for (q = hi_mem_min; q <= mem_end; q++)
  {
    if (link(q) == p)
    {
      print_nl("LINK(");
      print_int(q);
      print_char(')');
    }

    if (info(q) == p)
    {
      print_nl("INFO(");
      print_int(q);
      print_char(')');
    }
  }

  for (q = active_base; q <= box_base + 255; q++)
    if (equiv(q) == p)
    {
      print_nl("EQUIV(");
      print_int(q);
      print_char(')');
    }

  if (save_ptr > 0)
    for (q = 0; q <= save_ptr - 1; q++)
    {
      if (equiv_field(save_stack[q]) == p)
      {
        print_nl("SAVE(");
        print_int(q);
        print_char(')');
      }
    }

  for (q = 0; q <= hyphen_prime; q++)
    if (hyph_list[q] == p)
    {
      print_nl("HYPH(");
      print_int(q);
      print_char(')');
    }
}
#endif
/* sec 0174 */
void short_display_(integer p)
{
  integer n; 

  while (p != 0) /* want p != null here ! */
  {
    if (is_char_node(p))
    {
      if (p <= mem_end)
      {
        if (font(p) != font_in_short_display)
        {
          if ((font(p) > font_max))
            print_char('*');
          else
          {
            print_esc("");
            print(font_id_text(font(p)));
          }
          
          print_char(' ');
          font_in_short_display = font(p);
        }
        
        if (font_dir[font(p)] != dir_default)
        {
          p = link(p);
          print_kanji(info(p));
        }
        else
        {
          print(character(p));
        }
      }
    }
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case ins_node:
      case whatsit_node:
      case mark_node:
      case adjust_node:
      case unset_node:
        prints("[]");
        break;

      case rule_node:
        print_char('|');
        break;

      case glue_node:
        if (glue_ptr(p) != 0)
          print_char(' ');
        break;

      case math_node:
        if (subtype(p) >= L_code)
          prints("[]");
        else
          print_char('$');
        break;

      case ligature_node:
        short_display(lig_ptr(p));
        break;

      case disc_node:
        short_display(pre_break(p));
        short_display(post_break(p));
        n = replace_count(p);

        while (n > 0)
        {
          if (link(p) != 0)
            p = link(p);

          decr(n);
        }
        break;

      default:
        break;
    }
    
    p = link(p);
  }
}
/* sec 0176 */
void print_font_and_char (integer p)
{
  if (p > mem_end)
    print_esc("CLOBBERED.");
  else
  {
    if ((font(p) > font_max))
      print_char('*');
    else
    {
      print_esc("");
      print(font_id_text(font(p)));
    }

    print_char(' ');

    if (font_dir[font(p)] != dir_default)
    {
      p = link(p);
      print_kanji(info(p));
    }
    else
    {
      print(character(p));
    }
  }
}
/* sec 0176 */
void print_mark (integer p)
{ 
  print_char('{');

  if ((p < hi_mem_min) || (p > mem_end))
    print_esc("CLOBBERED.");
  else
    show_token_list(link(p), 0, max_print_line - 10);

  print_char('}');
}
/* sec 0176 */
void print_rule_dimen(scaled d)
{
  if (is_running(d))
    print_char('*');
  else
    print_scaled(d);
}
/* sec 0177 */
void print_glue_(scaled d, integer order, const char * s)
{
  print_scaled(d); 

  if ((order < normal) || (order > filll))
    prints("foul");
  else if (order > normal)
  {
    prints("fil");

    while (order > fil)
    {
      print_char('l');
      decr(order);
    }
  }
  else if (*s != '\0')
    prints(s);
}
/* sec 0178 */
void print_spec_(integer p, const char * s)
{
  if ((p < mem_min) || (p >= lo_mem_max)) 
    print_char('*');
  else
  {
    print_scaled(width(p));

    if (*s != '\0')
      prints(s);

    if (stretch(p) != 0)
    {
      prints(" plus ");
      print_glue(stretch(p), stretch_order(p), s);
    }

    if (shrink(p) != 0)
    {
      prints(" minus ");
      print_glue(shrink(p), shrink_order(p), s);
    }
  }
}
/* sec 0691 */
void print_fam_and_char_(pointer p, small_number t)
{
  KANJI_code cx;

  print_esc("fam");
  print_int(fam(p));
  print_char(' ');

  if (t == math_char)
    print(character(p));
  else
  {
    KANJI(cx) = math_kcode_nucleus(p);
    print_kanji(cx);
  }
}
/* sec 0691 */
void print_delimiter_(pointer p)
{
  integer a;

  a = small_fam(p) * 256 + small_char(p);
  a = a * 0x1000 + large_fam(p) * 256 + large_char(p);

  if (a < 0)
    print_int(a);
  else
    print_hex(a);
}
/* sec 0692 */
void print_subsidiary_data_(pointer p, ASCII_code c)
{
  if ((pool_ptr - str_start[str_ptr]) >= depth_threshold)
  {
    if (math_type(p) != 0)
      prints(" []");
  }
  else
  {
    append_char(c);
    temp_ptr = p;

    switch (math_type(p))
    {
      case math_char:
      case math_jchar:
        print_ln();
        print_current_string();
        print_fam_and_char(p, math_type(p));
        break;

      case sub_box:
        show_info();
        break;

      case sub_mlist:
        if (info(p) == 0)
        {
          print_ln();
          print_current_string();
          prints("{}");
        }
        else
          show_info();
        break;

      default:
        break;
    }

    decr(pool_ptr);
  }
}
/* sec 0694 */
void print_style_(integer c)
{
  switch (c / 2)
  {
    case 0:
      print_esc("displaystyle");
      break;
    case 1:
      print_esc("textstyle");
      break;
    case 2:
      print_esc("scriptstyle");
      break;
    case 3:
      print_esc("scriptscriptstyle");
      break;
    default:
      prints("Unknown style!");
      break;
  }
}
/* sec 0225 */
void print_skip_param_(integer n)
{
  switch (n)
  {
    case line_skip_code:
      print_esc("lineskip");
      break;

    case baseline_skip_code:
      print_esc("baselineskip");
      break; 

    case par_skip_code:
      print_esc("parskip");
      break;

    case above_display_skip_code:
      print_esc("abovedisplayskip");
      break;

    case below_display_skip_code:
      print_esc("belowdisplayskip");
      break;

    case above_display_short_skip_code:
      print_esc("abovedisplayshortskip");
      break;

    case below_display_short_skip_code:
      print_esc("belowdisplayshortskip");
      break;

    case left_skip_code:
      print_esc("leftskip");
      break;

    case right_skip_code:
      print_esc("rightskip");
      break;

    case top_skip_code:
      print_esc("topskip");
      break;

    case split_top_skip_code:
      print_esc("splittopskip");
      break;

    case tab_skip_code:
      print_esc("tabskip");
      break;

    case space_skip_code:
      print_esc("spaceskip");
      break;

    case xspace_skip_code:
      print_esc("xspaceskip");
      break;

    case par_fill_skip_code:
      print_esc("parfillskip");
      break;

    case kanji_skip_code:
      print_esc("kanjiskip");
      break;

    case xkanji_skip_code:
      print_esc("xkanjiskip");
      break;

    case thin_mu_skip_code:
      print_esc("thinmuskip");
      break;

    case med_mu_skip_code:
      print_esc("medmuskip");
      break;

    case thick_mu_skip_code:
      print_esc("thickmuskip");
      break;

    case jfm_skip:
      prints("refer from jfm");
      break;

    default:
      prints("[unknown glue parameter!]");
      break;
  }
}
/* sec 0182 */
void show_node_list_(integer p)
{
  integer n;
  real g;

  if (cur_length > depth_threshold)
  {
    if (p != 0)
      prints(" []");

    return; 
  }

  n = 0;

  while (p != 0)
  {
    print_ln(); 
    print_current_string(); 

    if (p > mem_end)
    {
      prints("Bad link, display aborted.");
      return;
    }

    incr(n);

    if (n > breadth_max)
    {
      prints("etc.");
      return;
    }

    if (is_char_node(p))
    {
      print_font_and_char(p);

      if (font_dir[font(p)] != dir_default)
        p = link(p);
    }
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case unset_node:
        {
          switch (type(p))
          {
            case hlist_node:
              print_esc("h");
              break;
            case vlist_node:
              print_esc("v");
              break;
            case dir_node:
              print_esc("dir");
              break;
            default:
              print_esc("unset");
              break;
          }

          prints("box(");
          print_scaled(height(p));
          print_char('+');
          print_scaled(depth(p));
          prints(")x");
          print_scaled(width(p));

          if (type(p) == unset_node)
          {
            if (span_count(p) != 0)
            {
              prints(" (");
              print_int(span_count(p) + 1);
              prints(" columns)");
            }

            if (glue_stretch(p) != 0)
            {
              prints(", stretch ");
              print_glue(glue_stretch(p), glue_order(p), "");
            }

            if (glue_shrink(p) != 0)
            {
              prints(", shrink ");
              print_glue(glue_shrink(p), glue_sign(p), "");
            }
          }
          else
          {
            g = glue_set(p);

            if ((g != 0.0) && (glue_sign(p) != 0))
            {
              prints(", glue set ");

              if (glue_sign(p) == shrinking)
                prints("- ");

              if (fabs(g) > 20000.0)
              {
                if (g > 0.0)
                  print_char('>');
                else
                  prints("< -");

                print_glue(20000 * unity, glue_order(p), "");
              }
              else
                print_glue(round(unity * g), glue_order(p), "");
            }

            if (shift_amount(p) != 0)
            {
              prints(", shifted ");
              print_scaled(shift_amount(p));
            }

            if (eTeX_ex)
              if ((type(p) == hlist_node) && (box_lr(p) == dlist))
                prints(", display");

            if (box_dir(p) != dir_default)
            {
              print_direction_alt(box_dir(p));
            }
          }

          node_list_display(list_ptr(p));
        }
        break;

      case rule_node:
        {
          print_esc("rule(");
          print_rule_dimen(height(p));
          print_char('+');
          print_rule_dimen(depth(p));
          prints(")x");
          print_rule_dimen(width(p));
        }
        break;

      case ins_node:
        {
          print_esc("insert");
          print_int(subtype(p));
          print_dir(ins_dir(p));
          prints(", natural size ");
          print_scaled(height(p));
          prints("; split(");
          print_spec(split_top_ptr(p), "");
          print_char(',');
          print_scaled(depth(p));
          prints("); float cost ");
          print_int(float_cost(p));
          node_list_display(ins_ptr(p));
        }
        break;

      case whatsit_node:
        switch (subtype(p))
        {
          case open_node:
            {
              print_write_whatsit("openout", p);
              print_char('=');
              print_file_name(open_name(p), open_area(p), open_ext(p));
            }
            break;

          case write_node:
            {
              print_write_whatsit("write", p);
              print_mark(write_tokens(p));
            }
            break;

          case close_node:
            print_write_whatsit("closeout", p);
            break;

          case special_node:
            {
              print_esc("special");
              print_mark(write_tokens(p));
            }
            break;

          case language_node:
            {
              print_esc("setlanguage");
              print_int(what_lang(p));
              prints(" (hyphenmin ");
              print_int(what_lhm(p));
              print_char(',');
              print_int(what_rhm(p));
              print_char(')');
            }
            break;

          default:
            prints("whatsit?");
            break;
        }
        break;

      case disp_node:
        {
          print_esc("displace ");
          print_scaled(disp_dimen(p));
        }
        break;

      case glue_node:
        if (subtype(p) >= a_leaders)
        {
          print_esc("");

          if (subtype(p) == c_leaders)
            print_char('c');
          else if (subtype(p) == x_leaders)
            print_char('x');

          prints("leaders ");
          print_spec(glue_ptr(p), "");
          node_list_display(leader_ptr(p));
        }
        else
        {
          print_esc("glue");

          if (subtype(p) != normal)
          {
            print_char('(');

            if (subtype(p) < cond_math_glue)
              print_skip_param(subtype(p) - 1);
            else if (subtype(p) == cond_math_glue)
              print_esc("nonscript");
            else print_esc("mskip");

            print_char(')');
          }

          if (subtype(p) != cond_math_glue)
          {
            print_char(' ');

            if (subtype(p) < cond_math_glue)
              print_spec(glue_ptr(p), "");
            else
              print_spec(glue_ptr(p), "mu");
          }
        }
        break;

      case kern_node:
        if (subtype(p) != mu_glue)
        {
          print_esc("kern");

          if (subtype(p) != normal)
            print_char(' ');

          print_scaled(width(p));

          if (subtype(p) == acc_kern)
            prints(" (for accent)");
        }
        else
        {
          print_esc("mkern");
          print_scaled(width(p));
          prints("mu");
        }
        break;

      case math_node:
        if (subtype(p) > after)
        {
          if (end_LR(p))
            print_esc("end");
          else
            print_esc("begin");
          
          if (subtype(p) > R_code)
            print_char('R');
          else if (subtype(p) > L_code)
            print_char('L');
          else
            print_char('M');
        }
        else
        {
          print_esc("math");

          if (subtype(p) == before)
            prints("on");
          else
            prints("off");

          if (width(p) != 0)
          {
            prints(", surrounded ");
            print_scaled(width(p));
          }
        }
        break;

      case ligature_node:
        {
          print_font_and_char(lig_char(p));
          prints("(ligature ");

          if (subtype(p) > 1)
            print_char('|');

          font_in_short_display = font(lig_char(p)); 
          short_display(lig_ptr(p));

          if (odd(subtype(p)))
            print_char('|');

          print_char(')');
        }
        break;

      case penalty_node:
        {
          print_esc("penalty ");
          print_int(penalty(p));

          if (subtype(p) == widow_pena)
            prints("(for \\jchrwidowpenalty)");
          else if (subtype(p) == kinsoku_pena)
            prints("(for kinsoku)");
        }
        break;

      case disc_node:
        {
          print_esc("discretionary");

          if (replace_count(p) > 0)
          {
            prints(" replacing ");
            print_int(replace_count(p));
          }

          node_list_display(pre_break(p));
          append_char('|');
          show_node_list(post_break(p));
          decr(pool_ptr);
        }
        break;

      case mark_node:
        {
          print_esc("mark");

          if (mark_class(p) != 0)
          {
            print_char('s');
            print_int(mark_class(p));
          }

          print_mark(mark_ptr(p));
        }
        break;

      case adjust_node:
        {
          print_esc("vadjust");
          node_list_display(adjust_ptr(p));
        }
        break;

      case style_node:
        print_style(subtype(p));
        break;

      case choice_node:
        {
          print_esc("mathchoice");
          append_char('D');
          show_node_list(display_mlist(p));
          decr(pool_ptr);
          append_char('T');
          show_node_list(text_mlist(p));
          decr(pool_ptr);
          append_char('S');
          show_node_list(script_mlist(p));
          decr(pool_ptr);
          append_char('s');
          show_node_list(script_script_mlist(p)); 
          decr(pool_ptr); 
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
      case left_noad:
      case right_noad:
        {
          switch (type(p))
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

            case over_noad:
              print_esc("overline");
              break;

            case under_noad:
              print_esc("underline");
              break;

            case vcenter_noad:
              print_esc("vcenter");
              break;

            case radical_noad:
              {
                print_esc("radical");
                print_delimiter(left_delimiter(p));
              }
              break;

            case accent_noad:
              {
                print_esc("accent");
                print_fam_and_char(accent_chr(p), math_char);
              }
              break;

            case left_noad:
              {
                print_esc("left");
                print_delimiter(delimiter(p));
              }
              break;

            case right_noad:
              {
                if (subtype(p) == normal)
                  print_esc("right");
                else
                  print_esc("middle");

                print_delimiter(delimiter(p));
              }
              break;
          }

          if (type(p) < left_noad)
          {
            if (subtype(p) != normal)
              if (subtype(p) == limits)
                print_esc("limits");
              else
                print_esc("nolimits");

            print_subsidiary_data(nucleus(p), '.');
          }

          print_subsidiary_data(supscr(p), '^');
          print_subsidiary_data(subscr(p), '_');
        }
        break;

      case fraction_noad:
        {
          print_esc("fraction, thickness ");

          if (thickness(p) == 1073741824L)  /* 2^30 */
            prints("= default");
          else
            print_scaled(thickness(p));

          if ((small_fam(left_delimiter(p)) != 0) || (small_char(left_delimiter(p)) != 0) ||
              (large_fam(left_delimiter(p)) != 0) || (large_char(left_delimiter(p)) != 0))
          {
            prints(", left-delimiter ");
            print_delimiter(left_delimiter(p));
          }

          if ((small_fam(right_delimiter(p)) != 0) || (small_char(right_delimiter(p)) != 0) ||
              (large_fam(right_delimiter(p)) != 0) || (large_char(right_delimiter(p)) != 0))
          {
            prints(", right-delimiter ");
            print_delimiter(right_delimiter(p));
          }

          print_subsidiary_data(numerator(p), '\\');
          print_subsidiary_data(denominator(p), '/');
        }
        break;

      default:
        prints("Unknown node type!");
        break;
    }

    p = link(p);
  }
}