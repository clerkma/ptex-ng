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

/* sec 0058 */
// prints an end-of-line
void print_ln (void)
{
  integer ii;

  switch (selector)
  {
    case term_and_log:
      {
        if (nrestmultichr(kcode_pos) > 0)
          for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
          {
            wterm(' ');
            wlog(' ');
          }
      
        wterm_cr();
        wlog_cr();
        term_offset = 0;
        file_offset = 0;
      }
      break;

    case log_only:
      {
        if (nrestmultichr(kcode_pos) > 0)
          for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
            wlog(' ');

        wlog_cr();
        file_offset = 0;
      }
      break;

    case term_only:
      {
        if (nrestmultichr(kcode_pos) > 0)
          for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
          wterm(' ');

        wterm_cr();
        term_offset = 0;
      }
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
// prints a single character
void print_char (ASCII_code s)
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
      if (file_offset >= max_print_line - nrestmultichr(kcode_pos))
      {
        wlog_cr();
        file_offset = 0;
      }

    if ((selector == term_and_log) || (selector == term_only))
      if (term_offset >= max_print_line - nrestmultichr(kcode_pos))
      {
        wterm_cr();
        term_offset = 0;
      }
  }
  else
    kcode_pos = 0;

  switch (selector)
  {
    case term_and_log:
      {
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
      }
      break;

    case log_only:
      {
        wlog(xchr[s]);
        incr(file_offset);

        if (file_offset == max_print_line)
          print_ln();
      }
      break;

    case term_only:
      {
        wterm(xchr[s]);
        incr(term_offset);

        if (term_offset == max_print_line)
          print_ln();
      }
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
// prints string |s|
void print_ (integer s)
{
  pool_pointer j;
  integer nl;

  if (s >= str_ptr)
    s = 259;
  else
  {
    if (s < 256)
    {
      if (s < 0)
        s = 259;
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
        j = str_start[s];

        while (j < str_start[s + 1])
        {
          print_char(str_pool[j]);
          incr(j);
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
// string version print.
void prints_ (const char * s)
{
  while (*s)
    print_char(*s++);
}
/* sec 0060 */
// prints string |s|
void slow_print (integer s)
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
// prints string |s| at beginning of line
void print_nl (const char * s)
{
  if (((term_offset > 0) && (odd(selector))) ||
      ((file_offset > 0) && (selector >= log_only)))
    print_ln();

  prints(s);
}
/* sec 0063 */
// prints escape character, then |s|
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
// prints |dig[k-1]|$\,\ldots\,$|dig[0]|
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
// prints an integer in decimal form
void print_int (integer n)
{
  char k;
  integer m;

  k = 0;

  if (n < 0)
  {
    print_char('-');

    if (n > -100000000)
      negate(n);
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

  do {
    dig[k] = (char) (n % 10);
    n = n / 10;
    incr(k);
  } while (!(n == 0));

  print_the_digs(k);
}
/* sec 0262 */
// prints a purported control sequence
void print_cs (integer p)
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
// prints a control sequence
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
  if (s == text_size)
    print_esc("textfont");
  else if (s == script_size)
    print_esc("scriptfont");
  else
    print_esc("scriptscriptfont");
} 
/* sec 1355 */
void print_write_whatsit (const char * s, pointer p)
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
// todo: noreturn
void jump_out (void) 
{
  close_files_and_terminate();

  if (trace_flag)
    puts("Exiting at jump_out().");

  exit(do_final_end());
}
/* sec 0082 */
// completes the job of error reporting
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
            align_state = 1000000;
            OK_to_interrupt = false;

            if ((last > first + 1) && (buffer[first + 1] >= '0') &&
                (buffer[first + 1] <= '9'))
              c = (c * 10 + buffer[first + 1] - '0' * 11);
            else
              c = (c - '0');
            
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

#ifdef NG_DEBUG
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
            print_nl("You want to edit file ");
            slow_print(input_stack[base_ptr].name_field);
            prints(" at line ");
            print_int(line);
            interaction = scroll_mode;
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

              do {
                decr(help_ptr);
                prints(help_line[help_ptr]);
                print_ln();
              } while (!(help_ptr == 0));
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
            cur_input.limit_field = last - 1;

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
                {
                  print_esc("batchmode");
                  decr(selector);
                }
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
          do_nothing();
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
// todo: noreturn
// prints |s|, and that's it
void fatal_error (const char * s)
{
  normalize_selector();
  print_err("Emergency stop");
  help1(s);
  succumb();
}
/* sec 0094 */
// todo: noreturn
// stop due to finiteness
void overflow (const char * s, integer n)
{
  normalize_selector();
  print_err("TeX capacity exceeded, sorry [");
  prints(s);
  print_char('=');
  print_int(n);
  print_char(']');
  help2("If you really absolutely need more capacity,",
      "you can ask a wizard to enlarge me.");

  if (!tex82_flag)
  {
    if (!strcmp(s, "pattern memory") && (n == trie_size))
      printf("\n  (Maybe use -h=... on command line in initex)\n");
    else if (!strcmp(s, "exception dictionary") && (n == hyphen_prime))
      printf("\n  (Maybe use -e=... on command line in initex)\n");
  }

  succumb();
}
/* sec 0095 */
// todo: noreturn
// consistency check violated; |s| tells where
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
// gets the terminal input started
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
// current string enters the pool
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
// test equality of strings
boolean str_eq_buf (str_number s, integer k)
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
// test equality of strings
boolean str_eq_str (str_number s, str_number t)
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
// prints two least significant digits
void print_two (integer n)
{
  n = abs(n) % 100;
  print_char('0' + (n / 10));
  print_char('0' + (n % 10));
}
/* sec 0067 */
// prints a positive integer in hexadecimal form
void print_hex (integer n)
{
  char k;

  k = 0;
  print_char('"');

  do {
    dig[k] = (unsigned char) (n % 16);
    n = n / 16;
    incr(k);
  } while (!(n == 0));

  print_the_digs(k);
}
/* sec 0069 */
void print_roman_int (integer n)
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

    if (str_pool[k - 1] == '2')
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
// prints a yet-unmade string
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
// gets a line from the terminal
void term_input (void)
{ 
  integer k;

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
void int_error (integer n)
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
integer half (integer x)
{
  if (odd(x))
    return ((x + 1) / 2);
  else
    return (x / 2);
}
/* sec 0102 */
scaled round_decimals (small_number k)
{
  integer a;

  a = 0;

  while (k > 0)
  {
    decr(k);
    a = (a + dig[k] * two) / 10;
  }
  
  return ((a + 1) / 2);
}
/* sec 0103 */
// prints scaled real, rounded to five digits
void print_scaled (scaled s)
{
  scaled delta;

  if (s < 0)
  {
    print_char('-');
    negate(s);
  }

  print_int(s / unity);
  print_char('.');
  s = 10 * (s % unity) + 5;
  delta = 10;

  do {
    if (delta > unity)
      s = s - 17232; /* 2^15 - 50000 - rounding */

    print_char('0' + (s / unity));
    s = 10 * (s % unity);
    delta = delta * 10;
  } while (!(s <= delta));
}
/* sec 0105 */
scaled mult_and_add (integer n, scaled x, scaled y, scaled max_answer)
{
  if (n < 0)
  {
    negate(x);
    negate(n);
  }

  if (n == 0)
    return y;
  else if (((x <= (max_answer - y) / n) &&
    (-x <= (max_answer + y) / n)))
    return (n * x + y); 
  else
  {
    arith_error = true;
    return 0;
  }
}
/* sec 0106 */
scaled x_over_n (scaled x, integer n)
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
      negate(x);
      negate(n);
      negative = true;
    }

    if (x >= 0)
    {
      Result = x / n;
      tex_remainder = x % n;
    }
    else
    {
      Result = -((-x) / n);
      tex_remainder = -((-x) % n);
    }
  }

  if (negative)
    negate(tex_remainder);

  return Result;
}
/* sec 0107 */
scaled xn_over_d (scaled x, integer n, integer d)
{
  register scaled Result;
  boolean positive;
  nonnegative_integer t, u, v;

  if (x >= 0)
    positive = true; 
  else
  {
    negate(x);
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
    Result = -u;
    tex_remainder = -(v % d);
  }

  return Result;
}
/* sec 0108 */
// compute badness, given |t>=0|
halfword badness (scaled t, scaled s)
{
  integer r;

  if (t == 0)
    return 0;
  else if (s <= 0)
    return inf_bad;
  else
  {
    if (t <= 7230584L)
      r = (t * 297) / s;
    else if (s >= 1663497L)
      r = t / (s / 297);
    else
      r = t;

    if (r > 1290)
      return inf_bad; 
    else
      return (r * r * r + 131072L) / 262144L;  /* 2^17 */
  }
}
/* sec 0114 */
#ifdef NG_DEBUG
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
void show_token_list (integer p, integer q, integer l)
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
      set_trick_count();

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
          {
            print(c);
            print(c);
          }
          break;
        
        case out_param:
          {
            print(match_chr);
          
            if (c <= 9)
              print_char(c + '0');
            else
            {
              print_char('!');
              return;
            }
          }
          break;
        
        case match:
          {
            match_chr = (ASCII_code) c;
            print(c);
            incr(n);
            print_char(n);
          
            if (n > '9')
              return;
          }
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
        {
          prints("definition");
          p = def_ref;
        }
        break;

      case matching:
        {
          prints("argument");
          p = temp_head;
        }
        break;

      case aligning:
        {
          prints("preamble");
          p = hold_head;
        }
        break;

      case absorbing:
        {
          prints("text");
          p = def_ref;
        }
        break;
    }

    print_char('?');
    print_ln();
    show_token_list(link(p), 0, error_line - 10); 
  }
}
/* sec 0120 */
// single-word node allocation
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

  link(p) = 0;

#ifdef STAT
  incr(dyn_used); 
#endif

  return p; 
} 
/* sec 0123 */
// makes list of single-word nodes
void flush_list (pointer p)
{ 
  pointer q, r;

  if (p != 0)
  {
    r = p;

    do {
      q = r;
      r = link(r);
#ifdef STAT
      decr(dyn_used);
#endif
    } while (!(r == 0));

    link(q) = avail;
    avail = p;
  }
}
/* sec 0125 */
// variable-size node allocation
pointer get_node (integer s)
{
  pointer p;
  pointer q;
  integer r;
  integer t;

restart:
  p = rover;

  do {
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
  } while (!(p == rover));

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
// variable-size node liberation
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
// creates a new box node
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
pointer new_ligature (quarterword f, quarterword c, pointer q)
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
pointer new_lig_item (quarterword c)
{
  pointer p;

  p = get_node(small_node_size);
  character(p) = c;
  lig_ptr(p) = 0;

  return p;
}
/* sec 0145 */
// creates an empty |disc_node|
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
// duplicates a glue specification
pointer new_spec (pointer p)
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
pointer new_kern (scaled w)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = kern_node;
  subtype(p) = normal;
  width(p) = w;

  return p;
}
/* sec 0158 */
pointer new_penalty (integer m)
{
  pointer p;

  p = get_node(small_node_size);
  type(p) = penalty_node;
  subtype(p) = 0;
  penalty(p) = m;

  return p;
}

#ifdef NG_DEBUG
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
void search_mem (pointer p)
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
// prints highlights of list |p|
void short_display (integer p)
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
          print(character(p));
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
        if (glue_ptr(p) != zero_glue)
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
        {
          short_display(pre_break(p));
          short_display(post_break(p));
          n = replace_count(p);

          while (n > 0)
          {
            if (link(p) != 0)
              p = link(p);

            decr(n);
          }
        }
        break;

      default:
        do_nothing();
        break;
    }
    
    p = link(p);
  }
}
/* sec 0176 */
// prints |char_node| data
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
      print(character(p));
  }
}
/* sec 0176 */
// prints token list data in braces
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
// prints dimension in rule node
void print_rule_dimen (scaled d)
{
  if (is_running(d))
    print_char('*');
  else
    print_scaled(d);
}
/* sec 0177 */
void print_glue (scaled d, integer order, const char * s)
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
void print_spec (integer p, const char * s)
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
// prints family and character
void print_fam_and_char (pointer p, small_number t)
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
// prints a delimiter as 24-bit hex value
void print_delimiter (pointer p)
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
// display a noad field
void print_subsidiary_data (pointer p, ASCII_code c)
{
  if (cur_length >= depth_threshold)
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
        {
          print_ln();
          print_current_string();
          print_fam_and_char(p, math_type(p));
        }
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
        do_nothing();
        break;
    }

    flush_char();
  }
}
/* sec 0694 */
void print_style (integer c)
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
void print_skip_param (integer n)
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
// prints a node list symbolically
void show_node_list (integer p)
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
            if (span_count(p) != min_quarterword)
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

            if ((g != 0.0) && (glue_sign(p) != normal))
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
            else
              print_esc("mskip");

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
          flush_char();
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
          flush_char();
          append_char('T');
          show_node_list(text_mlist(p));
          flush_char();
          append_char('S');
          show_node_list(script_mlist(p));
          flush_char();
          append_char('s');
          show_node_list(script_script_mlist(p)); 
          flush_char(); 
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

          if (thickness(p) == default_code)
            prints("= default");
          else
            print_scaled(thickness(p));

          if ((small_fam(left_delimiter(p)) != 0) ||
              (small_char(left_delimiter(p)) != min_quarterword) ||
              (large_fam(left_delimiter(p)) != 0) ||
              (large_char(left_delimiter(p)) != min_quarterword))
          {
            prints(", left-delimiter ");
            print_delimiter(left_delimiter(p));
          }

          if ((small_fam(right_delimiter(p)) != 0) ||
              (small_char(right_delimiter(p)) != min_quarterword) ||
              (large_fam(right_delimiter(p)) != 0) ||
              (large_char(right_delimiter(p)) != min_quarterword))
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
/* sec 0198 */
void show_box (pointer p)
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
// |p| points to the reference count
// of a token list that is losing one reference
void delete_token_ref (pointer p)
{
  if (token_ref_count(p) == 0)
    flush_list(p);
  else
    decr(token_ref_count(p));
}
/* sec 0201 */
// |p| points to a glue specification
void delete_glue_ref (pointer p)
{
  if (glue_ref_count(p) == 0)
    free_node(p, glue_spec_size);
  else
    decr(glue_ref_count(p));
}
/* sec 0202 */
// erase list of nodes starting at |p|
void flush_node_list (pointer p)
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
            else if (type(p) == accent_noad)
              free_node(p, accent_noad_size);
            else
              free_node(p, noad_size);

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
// makes a duplicate of the node list that starts
// at |p| and returns a pointer to the new lis
pointer copy_node_list (pointer p)
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
      case dir_node:
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
          post_break(r) = copy_node_list(post_break(p));
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
// prints the mode represented by |m|
void print_mode (integer m)
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
      switch ((-m) / (max_command + 1))
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
// enter a new semantic level, save the old
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
  eTeX_aux = 0;
}
/* sec 0217 */
// leave a semantic level, re-enter the old
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
  pointer q, r;
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
          print_scaled(page_goal);
          r = link(page_ins_head);
          
          while (r != page_ins_head)
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

              do {
                q = link(q);

                if ((type(q) == ins_node) && (subtype(q) == subtype(r)))
                  incr(t);
              } while (!(q == broken_ins(r)));

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

          if (a.sc <= ignore_depth)
            prints("ignored");
          else
            print_scaled(a.sc);

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
            if (a.hh.rh > 0)
            {
              prints(", current language ");
              print_int(a.hh.rh);
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
void print_param (integer n)
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

    case tracing_assigns_code:
      print_esc("tracingassigns");
      break;

    case tracing_groups_code:
      print_esc("tracinggroups");
      break;

    case tracing_ifs_code:
      print_esc("tracingifs");
      break;
    
    case tracing_scan_tokens_code:
      print_esc("tracingscantokens");
      break;
    
    case tracing_nesting_code:
      print_esc("tracingnesting");
      break;
    
    case pre_display_direction_code:
      print_esc("predisplaydirection");
      break;

    case last_line_fit_code:
      print_esc("lastlinefit");
      break;

    case saving_vdiscards_code:
      print_esc("savingvdiscards");
      break;
    
    case saving_hyph_codes_code:
      print_esc("savinghyphcodes");
      break;

    case eTeX_state_code + TeXXeT_code:
      print_esc("TeXXeTstate");
      break;

    default:
      prints("[unknown integer parameter!]");
      break;
  }
}
/* sec 0245 */
// prepare to do some tracing
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
// restore proper conditions after tracing
void end_diagnostic (boolean blank_line)
{
  print_nl("");

  if (blank_line)
    print_ln();

  selector = old_setting;
}
/* sec 0247 */
void print_length_param (integer n)
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

    case pdf_h_origin_code:
      print_esc("pdfhorigin");
      break;

    case pdf_v_origin_code:
      print_esc("pdfvorigin");
      break;

    case pdf_page_width_code:
      print_esc("pdfpagewidth");
      break;

    case pdf_page_height_code:
      print_esc("pdfpageheight");
      break;

    default:
      prints("[unknown dimen parameter!]");
      break;
  }
}
/* sec 0298 */
void print_cmd_chr (quarterword cmd, halfword chr_code)
{
  integer n;

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
      {
        prints("kanji character ");
        print_kanji(KANJI(chr_code));
      }
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
      else switch (chr_code)
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

        case every_eof_loc:
          print_esc("everyeof");
          break;

        default:
          print_esc("errhelp");
          break;
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
      if (chr_code == 0)
        print_esc("expandafter");
      else
        print_esc("unless");
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
      {
        print_esc("mark");

        if (chr_code > 0)
          print_char('s');
      }
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
      if (chr_code == 0)
        print_esc("read");
      else
        print_esc("readline");
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
      switch (chr_code)
      {
        case par_shape_loc:
          print_esc("parshape");
          break;

        case inter_line_penalties_loc:
          print_esc("interlinepenalties");
          break;

        case club_penalties_loc:
          print_esc("clubpenalties");
          break;

        case widow_penalties_loc:
          print_esc("widowpenalties");
          break;

        case display_widow_penalties_loc:
          print_esc("displaywidowpenalties");
          break;
      }
      break;

    case the:
      if (chr_code == 0)
        print_esc("the");
      else if (chr_code == 1)
        print_esc("unexpanded");
      else
        print_esc("detokenize");
      break;

    case toks_register:
      {
        print_esc("toks");
      
        if (chr_code != mem_bot)
          print_sa_num(chr_code);
      }
      break;

    case vadjust:
      print_esc("vadjust");
      break;

    case valign:
      if (chr_code == 0)
        print_esc("valign");
      else
      {
        switch (chr_code)
        {
          case begin_L_code:
            print_esc("beginL");
            break;

          case end_L_code:
            print_esc("endL");
            break;

          case begin_R_code:
            print_esc("beginR");
            break;

          default:
            print_esc("endR");
            break;
        }
      }
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
      else if (chr_code == 2)
        print_esc("scantokens");
      else
        print_esc("endinput");
      break;

    case top_bot_mark:
      switch (chr_code % marks_code)
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

      if (chr_code >= marks_code)
        print_char('s');
      break;

    case tex_register:
      {
        if ((chr_code<mem_bot) || (chr_code>lo_mem_stat_max))
          cmd = sa_type(chr_code);
        else
        {
          cmd = chr_code - mem_bot;
          chr_code = null;
        }

        if (cmd == int_val)
          print_esc("count");
        else if (cmd == dimen_val)
          print_esc("dimen");
        else if (cmd == glue_val)
          print_esc("skip");
        else
          print_esc("muskip");

        if (chr_code != 0)
          print_sa_num(chr_code);
      }
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
      else if (chr_code == 2)
        print_esc("interactionmode");
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

        case last_node_type_code:
          print_esc("lastnodetype");
          break;

        case eTeX_version_code:
          print_esc("eTeXversion");
          break;

        case current_group_level_code:
          print_esc("currentgrouplevel");
          break;

        case current_group_type_code:
          print_esc("currentgrouptype");
          break;

        case current_if_level_code:
          print_esc("currentiflevel");
          break;

        case current_if_type_code:
          print_esc("currentiftype");
          break;

        case current_if_branch_code:
          print_esc("currentifbranch");
          break;

        case font_char_wd_code:
          print_esc("fontcharwd");
          break;

        case font_char_ht_code:
          print_esc("fontcharht");
          break;

        case font_char_dp_code:
          print_esc("fontchardp");
          break;

        case font_char_ic_code:
          print_esc("fontcharic");
          break;

        case par_shape_length_code:
          print_esc("parshapelength");
          break;

        case par_shape_indent_code:
          print_esc("parshapeindent");
          break;

        case par_shape_dimen_code:
          print_esc("parshapedimen");
          break;

        case eTeX_expr - int_val + int_val:
          print_esc("numexpr");
          break;

        case eTeX_expr - int_val + dimen_val:
          print_esc("dimexpr");
          break;

        case eTeX_expr - int_val + glue_val:
          print_esc("glueexpr");
          break;

        case eTeX_expr - int_val + mu_val:
          print_esc("muexpr");
          break;

        case glue_stretch_order_code:
          print_esc("gluestretchorder");
          break;

        case glue_shrink_order_code:
          print_esc("glueshrinkorder");
          break;

        case glue_stretch_code:
          print_esc("gluestretch");
          break;

        case glue_shrink_code:
          print_esc("glueshrink");
          break;

        case mu_to_glue_code:
          print_esc("mutoglue");
          break;

        case glue_to_mu_code:
          print_esc("gluetomu");
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

        case eTeX_revision_code:
          print_esc("eTeXrevision");
          break;

        default:
          print_esc("jobname");
          break;
      }
      break;

    case if_test:
      {
        if (chr_code >= unless_code)
          print_esc("unless");

        switch (chr_code % unless_code)
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
          
          case if_def_code:
            print_esc("ifdefined");
            break;

          case if_cs_code:
            print_esc("ifcsname");
            break;

          case if_font_char_code:
            print_esc("iffontchar");
            break;

          default:
            print_esc("if");
            break;
        }
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
      else if (chr_code == last_box_code)
        print_esc("pagediscards");
      else if (chr_code == vsplit_code)
        print_esc("splitdiscards");
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
      else if (chr_code == middle_noad)
        print_esc("middle");
      else
        print_esc("right");
      break;

    case prefix:
      if (chr_code == 1)
        print_esc("long");
      else if (chr_code == 2)
        print_esc("outer");
      else if (chr_code == 8)
        print_esc("protected");
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
      {
        print_esc("char");
        print_hex(chr_code);
      }
      break;

    case kchar_given:
      {
        print_esc("kchar");
        print_hex(chr_code);
      }
      break;

    case math_given:
      {
        print_esc("mathchar");
        print_hex(chr_code);
      }
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
      {
        prints("select font ");
        slow_print(font_name[chr_code]);

        if (font_size[chr_code] != font_dsize[chr_code])
        {
          prints(" at ");
          print_scaled(font_size[chr_code]);
          prints("pt");
        }
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

        case show_groups:
          print_esc("showgroups");
          break;

        case show_tokens:
          print_esc("showtokens");
          break;

        case show_ifs:
          print_esc("showifs");
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
    case long_call:
    case outer_call:
    case long_outer_call:
      {
        n = cmd - call;

        if (info(link(chr_code)) == protected_token)
          n = n + 4;

        if (odd(n / 4))
          print_esc("protected");

        if (odd(n))
          print_esc("long");

        if (odd(n / 2))
          print_esc("outer");

        if (n > 0)
          print_char(' ');

        prints("macro");
      }
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
    if ((n == par_shape_loc) || ((n >= etex_pen_base) && (n < etex_pens)))
    {
      print_cmd_chr(set_shape, n);
      print_char('=');

      if (equiv(n) == 0)
        print_char('0');
      else if (n > par_shape_loc)
      {
        print_int(penalty(equiv(n)));
        print_char(' ');
        print_int(penalty(equiv(n) + 1));
        
        if (penalty(equiv(n)) > 1)
          print_esc("ETC.");
      }
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
    prints("kinsoku");
  else
    print_char('?');
}
#endif
/* sec 0259 */
// search the hash table
pointer id_lookup (integer j, integer l)
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
          do {
            if (hash_is_full)
            {
              overflow("hash size", hash_size);
              /* not dynamic        ^~~~~~~~~*/
              return 0;
            }

            decr(hash_used);
          } while (!(text(hash_used) == 0));

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
          printf(" (cs_count[%d]: '%s') ", cs_count, &str_pool[pool_ptr - l - d]);
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
// begin a new level of grouping
void new_save_level (group_code c)
{
  check_full_save_stack();

  if (eTeX_ex)
  {
    saved(0) = line;
    incr(save_ptr);
  }

  save_type(save_ptr) = level_boundary;
  save_level(save_ptr) = (quarterword) cur_group; 
  save_index(save_ptr) = cur_boundary;

  if (cur_level == max_quarterword)
  {
    overflow("grouping levels", max_quarterword - min_quarterword);
    return;
  }

  cur_boundary = save_ptr;
  cur_group = c;

#ifdef STAT
  if (tracing_groups > 0)
    group_trace(false);
#endif

  incr(cur_level);
  incr(save_ptr);
}
/* sec 0275 */
// gets ready to forget |w|
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
      {
        q = equiv_field(w);

        if (q != 0)
          free_node(q, info(q) + info(q) + 1);
      }
      break;

    case box_ref:
      flush_node_list(equiv_field(w));
      break;

    case toks_register:
    case tex_register:
      if ((equiv_field(w) < mem_bot) || (equiv_field(w) > lo_mem_stat_max))
        delete_sa_ref(equiv_field(w));

    default:
      do_nothing();
      break;
  }
}
/* sec 0276 */
// saves |eqtb[p]|
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
// new data for |eqtb|
void eq_define (pointer p, quarterword t, halfword e)
{
  if (eTeX_ex && (eq_type(p) == t) && (equiv(p) == e))
  {
    assign_trace(p, "reassigning");
    eq_destroy(eqtb[p]);
    return;
  }

  assign_trace(p, "changing");

  if (eq_level(p) == cur_level)
    eq_destroy(eqtb[p]);
  else if (cur_level > level_one)
    eq_save(p, eq_level(p));

  eq_level(p) = cur_level;
  eq_type(p) = t;
  equiv(p) = e;
  assign_trace(p, "into");
}
/* sec 0278 */
void eq_word_define (pointer p, integer w)
{
  if (eTeX_ex && (eqtb[p].cint == w))
  {
    assign_trace(p, "reassigning");
    return;
  }

  assign_trace(p, "changing");

  if (xeq_level[p] != cur_level)
  {
    eq_save(p, xeq_level[p]);
    xeq_level[p] = cur_level;
  }

  eqtb[p].cint = w;
  assign_trace(p, "into");
}
/* sec 0279 */
// global |eq_define|
void geq_define (pointer p, quarterword t, halfword e)
{
  assign_trace(p, "globally changing");
  eq_destroy(eqtb[p]);
  eq_level(p) = level_one;
  eq_type(p) = t;
  equiv(p) = e;
  assign_trace(p, "into");
}
/* sec 0279 */
// global |eq_word_define|
void geq_word_define (pointer p, integer w)
{
  assign_trace(p, "globally changing");
  eqtb[p].cint = w;
  xeq_level[p]= level_one;
  assign_trace(p, "into");
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

#ifdef STAT
/* sec 0284 */
// |eqtb[p]| has just been restored or retained
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
// macros -> function
void assign_trace (pointer p, const char * s)
{
  if (tracing_assigns > 0)
    restore_trace(p, s);
}
#endif
/* sec 0281 */
// pops the top level off the save stack
void unsave (void)
{
  pointer p;
  quarterword l;
  halfword t;
  boolean a;

  a = false;

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

        if (a)
        {
          p = get_avail();
          info(p) = cur_tok;
          link(p) = loc;
          loc = p;
          start = p;

          if (cur_tok < right_brace_limit)
            if (cur_tok < left_brace_limit)
              decr(align_state);
            else
              incr(align_state);
        }
        else
        {
          back_input();
          a = eTeX_ex;
        };

        cur_tok = t;
      }
      else if (save_type(save_ptr) == restore_sa)
      {
        sa_restore();
        sa_chain = p;
        sa_level = save_level(save_ptr);
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
#ifdef STAT
    if (tracing_groups > 0)
      group_trace(true);
#endif

    if (grp_stack[in_open] == cur_boundary)
      group_warning();

    cur_group = save_level(save_ptr);
    cur_boundary = save_index(save_ptr);

    if (eTeX_ex)
      decr(save_ptr);
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
    show_token_list(link(p), 0, 10000000);
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
  else if ((cur_cmd == top_bot_mark) && (cur_chr < marks_code))
  {
    print_char(':');
    print_ln();
    token_show(cur_mark[cur_chr]);
  }
}
/* sec 0299 */
void show_cur_cmd_chr (void)
{
  integer n;
  integer l;
  pointer p;

  begin_diagnostic();
  print_nl("{");

  if (mode != shown_mode)
  {
    print_mode(mode);
    prints(": ");
    shown_mode = mode;
  }

  print_cmd_chr(cur_cmd, cur_chr);

  if (tracing_ifs > 0)
    if (cur_cmd >= if_test)
      if (cur_cmd <= fi_or_else)
      {
        prints(": ");

        if (cur_cmd == fi_or_else)
        {
          print_cmd_chr(if_test, cur_if);
          print_char(' ');
          n = 0;
          l = if_line;
        }
        else
        {
          n = 1;
          l = line;
        }

        p = cond_ptr;

        while (p != 0)
        {
          incr(n);
          p = link(p);
        }

        prints("(level ");
        print_int(n);
        print_char(')');
        print_if_line(l);
      }

  print_char('}');
  end_diagnostic(false);
}
/* sec 0311 */
// prints where the scanner is
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
      if ((name > 19) || (base_ptr == 0))
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

              if (index == in_open)
                print_int(line);
              else
                print_int(line_stack[index + 1]);

              prints(") :");
            }
            else
            {
              print_nl("l.");

              if (index == in_open)
                print_int(line);
              else
                print_int(line_stack[index + 1]);
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
              {
                print_ln();
                print_cs(name);
              }
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

            case every_eof_text:
              print_nl("<everyeof> ");
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
            show_token_list(link(start), loc, 100000L);

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

        kcp = trick_buf2[p % error_line];

        if (kcp % 010 > 1)
        {
          p = p + nrestmultichr(kcp) + 1;
          n = n - nrestmultichr(kcp) - 1;
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
void begin_token_list (pointer p, quarterword t)
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
// leave a token-list input level
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
    if (align_state > 500000)
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
// undoes one token of input
void back_input (void)
{
  pointer p;

  while ((state == token_list) && (loc == 0) && (token_type != v_template))
    end_token_list();

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
// back up one token and call |error|
void back_error (void)
{
  OK_to_interrupt = false;
  back_input();
  OK_to_interrupt = true;
  error();
}
/* sec 0327 */
// back up one inserted token and call |error|
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
  eof_seen[index] = false;
  grp_stack[index] = cur_boundary;
  if_stack[index] = cond_ptr;
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

  if ((name == 18) || (name == 19))
    pseudo_close();
  else if (name > 17)
    a_close(cur_file);

  pop_input();
  decr(in_open);
}
/* sec 0330 */
void clear_for_error_prompt (void) 
{
  while ((state != token_list) && (name == 0) &&
      (input_ptr > 0) && (loc > limit))
    end_file_reading();

  print_ln();
}
/* sec 0336 */
void check_outer_validity (void)
{
  pointer p;
  pointer q;

  if (scanner_status != normal)
  {
    deletions_allowed = false;

    if (cur_cs != 0)
    {
      if ((state == token_list) || (name < 1) || (name > 17))
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
          {
            prints("definition");
            info(p) = right_brace_token + '}';
          }
          break;

        case matching:
          {
            prints("use");
            info(p) = par_token;
            long_state = outer_call;
          }
          break;

        case aligning:
          {
            prints("preamble");
            info(p) = right_brace_token + '}';
            q = p;
            p = get_avail();
            link(p) = q;
            info(p) = cs_token_flag + frozen_cr;
            align_state = -1000000;
          }
          break;

        case absorbing:
          {
            prints("text");
            info(p) = right_brace_token + '}';
          }
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
// sets |cur_cmd|, |cur_chr|, |cur_tok|
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
    cur_tok = cs_token_flag + cur_cs;
}
/* sec 0389 */
// invokes a user-defined control sequence
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

  if (info(r) == protected_token)
    r = link(r);

  if (info(r) != end_match_token)
  {
    scanner_status = matching;
    unbalance = 0;
    long_state = eq_type(cur_cs);

    if (long_state >= outer_call)
      long_state = long_state - 2;

    do {
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

          do {
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
          } while (!(t == r));

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
    } while (!(info(r) == end_match_token));
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

reswitch:
  if (cur_cmd < call)
  {
    if (tracing_commands > 1)
      show_cur_cmd_chr();

    switch (cur_cmd)
    {
      case top_bot_mark:
        {
          t = cur_chr % marks_code;

          if (cur_chr >= marks_code)
            scan_register_num();
          else
            cur_val = 0;

          if (cur_val == 0)
            cur_ptr = cur_mark[t];
          else
          {
            find_sa_element(mark_val, cur_val, false);

            if (cur_ptr != 0)
              if (odd(t))
                cur_ptr = link(cur_ptr + (t / 2) + 1);
              else
                cur_ptr = info(cur_ptr + (t / 2) + 1);
          }

          if (cur_ptr != 0)
            begin_token_list(cur_ptr, mark_text);
        }
        break;

      case expand_after:
        if (cur_chr == 0)
        {
          get_token();
          t = cur_tok;
          get_token();

          if (cur_cmd > max_command)
            expand();
          else
            back_input();

          cur_tok = t;
          back_input();
        }
        else
        {
          get_token();

          if ((cur_cmd == if_test) && (cur_chr != if_case_code))
          {
            cur_chr = cur_chr + unless_code;
            goto reswitch;
          }

          print_err("You can't use `");
          print_esc("unless");
          print("' before `");
          print_cmd_chr(cur_cmd, cur_chr);
          print_char('\'');
          help1("Continue, and I'll forget that it ever happened.");
          back_error();
        }
        break;

      case no_expand:
        {
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
        }
        break;

      case cs_name:
        {
          r = get_avail();
          p = r;

          do {
            get_x_token();
  
            if (cur_cs == 0)
              store_new_token(cur_tok);
          } while (!(cur_cs != 0));
          
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

              if (BYTE1(t) != 0)
              {
                buffer[j] = BYTE1(t);
                incr(j);
              }
              
              if (BYTE2(t) != 0)
              {
                buffer[j] = BYTE2(t);
                incr(j);
              }
              
              if (BYTE3(t) != 0)
              {
                buffer[j] = BYTE3(t);
                incr(j);
              }
              
              buffer[j] = BYTE4(t);
              incr(j);
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
        }
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
        {
          if (tracing_ifs > 0)
            if (tracing_commands <= 1)
              show_cur_cmd_chr();

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
              if (if_stack[in_open] == cond_ptr)
                if_warning();

              p = cond_ptr;
              if_line = if_line_field(p);
              cur_if = subtype(p);
              if_limit = type(p);
              cond_ptr = link(p);
              free_node(p, if_node_size);
            }
          }
        }
        break;

      case input:
        if (cur_chr == 1)
          force_eof = true;
        else if (cur_chr == 2)
          pseudo_start();
        else if (name_in_progress)
          insert_relax();
        else
          start_input();
        break;

      default:
        {
          print_err("Undefined control sequence");
          help5("The control sequence at the end of the top line",
              "of your error message was never \\def'ed. If you have",
              "misspelled it (e.g., `\\hobx'), type `I' and the correct",
              "spelling (e.g., `I\\hbox'). Otherwise just continue,",
              "and I'll forget about whatever was undefined.");
          error();
        }
        break;
    }
  }
  else if (cur_cmd < end_template)
    macro_call();
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
// sets |cur_cmd|, |cur_chr|, |cur_tok|, and expands macros
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
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  else
    cur_tok = cs_token_flag + cur_cs;
}
/* sec 0381 */
// |get_x_token| without the initial |get_next|
void x_token (void)
{
  while (cur_cmd > max_command)
  {
    expand();
    get_next();
  }

  if (cur_cs == 0)
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  else
    cur_tok = cs_token_flag + cur_cs;
}
/* sec 0403 */
// reads a mandatory |left_brace|
void scan_left_brace (void)
{
  do {
    get_x_token();
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

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
  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok != other_token + '=')
    back_input();
}
/* sec 0407 */
// look for a given string
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

  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

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
// sets |cur_val| to |font_info| location
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
    if (writing && (n <= space_shrink_code) &&
      (n >= space_code) && (font_glue[f] != 0)) 
    {
      delete_glue_ref(font_glue[f]);
      font_glue[f] = 0;
    }

    if (n > font_params[f])
      if (f < font_ptr)
        cur_val = fmem_ptr;
      else
      {
        do {
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
        } while (!(n == font_params[f]));

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
// fetch an internal parameter
void scan_something_internal (small_number level, boolean negative)
{
  halfword m;
  pointer tx;
  halfword qx;
  four_quarters i;
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
          if (m == mem_bot)
          {
            scan_register_num();

            if (cur_val < 256)
              cur_val = equiv(toks_base + cur_val);
            else
            {
              find_sa_element(tok_val, cur_val, false);

              if (cur_ptr == null)
                cur_val = null;
              else
                cur_val = sa_ptr(cur_ptr);
            }
          }
          else
            cur_val = sa_ptr(m);
        else
          cur_val = equiv(m);

        cur_val_level = tok_val;
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
      scanned_result(eqtb[m].sc, dimen_val);
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
        else if (m == 2)
          cur_val = interaction;
        else
          cur_val = insert_penalties;

        cur_val_level = int_val;
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
        if (m > par_shape_loc)
        {
          scan_int();

          if ((equiv(m) == null) || (cur_val < 0))
            cur_val = 0;
          else
          {
            if (cur_val > penalty(equiv(m)))
              cur_val = penalty(equiv(m));

            cur_val = penalty(equiv(m) + cur_val);
          }
        }
        else if (par_shape_ptr == 0)
          cur_val = 0;
        else
          cur_val = info(par_shape_ptr);

        cur_val_level = int_val;
      }
      break;

    case set_box_dimen:
      {
        scan_register_num();
        fetch_box(q);

        if (q == 0)
          cur_val = 0;
        else
        {
          qx = q;

          while ((q != null) && (box_dir(q) != abs(direction)))
            q = link(q);

          if (q == 0)
          {
            r = link(qx);
            link(qx) = null;
            q = new_dir_node(qx, abs(direction));
            link(qx) = r;
            cur_val = mem[q + m].sc;
            delete_glue_ref(space_ptr(q));
            delete_glue_ref(xspace_ptr(q));
            free_node(q, box_node_size);
          }
          else
            cur_val = mem[q + m].cint;
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
        font_info[fmem_ptr].sc = 0;
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
        if ((m < mem_bot) || (m > lo_mem_stat_max))
        {
          cur_val_level = sa_type(m);

          if (cur_val_level < glue_val)
            cur_val = sa_int(m);
          else
            cur_val = sa_ptr(m);
        }
        else
        {
          scan_register_num();
          cur_val_level = m - mem_bot;

          if (cur_val > 255)
          {
            find_sa_element(cur_val_level, cur_val, false);

            if (cur_ptr == null)
              if (cur_val_level < glue_val)
                cur_val = 0;
              else
                cur_val = zero_glue;
            else if (cur_val_level < glue_val)
              cur_val = sa_int(cur_ptr);
            else
              cur_val = sa_ptr(cur_ptr);
          }
          else switch (cur_val_level)
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
        }
      }
      break;

    case last_item:
      if (cur_chr >= input_line_no_code)
      {
        if (m >= eTeX_glue)
        {
          if (m < eTeX_mu)
          {
            switch (m)
            {
              case mu_to_glue_code:
                scan_mu_glue();
                break;
            }

            cur_val_level = glue_val;
          }
          else if (m < eTeX_expr)
          {
            switch (m)
            {
              case glue_to_mu_code:
                scan_normal_glue();
                break;
            }

            cur_val_level = mu_val;
          }
          else
          {
            cur_val_level = m - eTeX_expr + int_val;
            scan_expr();
          }

          while (cur_val_level > level)
          {
            if (cur_val_level == glue_val)
            {
              m = cur_val;
              cur_val = width(m);
              delete_glue_ref(m);
            }
            else if (cur_val_level == mu_val)
              mu_error();

            decr(cur_val_level);
          }

          if (negative)
            if (cur_val_level >= glue_val)
            {
              m = cur_val;
              cur_val = new_spec(m);
              delete_glue_ref(m);

              {
                negate(width(cur_val));
                negate(stretch(cur_val));
                negate(shrink(cur_val));
              }
            }
            else
              negate(cur_val);

          return;
        }
        else if (m >= eTeX_dim)
        {
          switch (m)
          {
            case font_char_wd_code:
            case font_char_ht_code:
            case font_char_dp_code:
            case font_char_ic_code:
              {
                scan_font_ident();
                q = cur_val;
                scan_char_num();

                if (font_dir[q] != dir_default)
                {
                  i = char_info(q, get_jfm_pos(KANJI(cur_val), q));

                  switch (m)
                  {
                    case font_char_wd_code:
                      cur_val = char_width(q, i);
                      break;

                    case font_char_ht_code:
                      cur_val = char_height(q, height_depth(i));
                      break;

                    case font_char_dp_code:
                      cur_val = char_depth(q, height_depth(i));
                      break;

                    case font_char_ic_code:
                      cur_val = char_italic(q, i);
                      break;
                  }
                }
                else if ((font_bc[q] <= cur_val) && (font_ec[q] >= cur_val))
                {
                  i = char_info(q, cur_val);

                  switch (m)
                  {
                    case font_char_wd_code:
                      cur_val = char_width(q, i);
                      break;

                    case font_char_ht_code:
                      cur_val = char_height(q, height_depth(i));
                      break;

                    case font_char_dp_code:
                      cur_val = char_depth(q, height_depth(i));
                      break;

                    case font_char_ic_code:
                      cur_val = char_italic(q, i);
                      break;
                  }
                }
                else
                  cur_val = 0;
              }
              break;

            case par_shape_length_code:
            case par_shape_indent_code:
            case par_shape_dimen_code:
              {
                q = cur_chr - par_shape_length_code;
                scan_int();

                if ((par_shape_ptr == null) || (cur_val <= 0))
                  cur_val = 0;
                else
                {
                  if (q == 2)
                  {
                    q = cur_val % 2;
                    cur_val = (cur_val + q) / 2;
                  }

                  if (cur_val > info(par_shape_ptr))
                    cur_val = info(par_shape_ptr);

                  cur_val = mem[par_shape_ptr + 2 * cur_val - q].sc;
                }

                cur_val_level = dimen_val;
              }
              break;

            case glue_stretch_code:
            case glue_shrink_code:
              {
                scan_normal_glue();
                q = cur_val;

                if (m == glue_stretch_code)
                  cur_val = stretch(q);
                else
                  cur_val = shrink(q);

                delete_glue_ref(q);
              }
              break;
          }
          
          cur_val_level = dimen_val;
        }
        else
        {
          switch (m)
          {
            case input_line_no_code:
              cur_val = line;
              break;

            case badness_code:
              cur_val = last_badness;
              break;

            case eTeX_version_code:
              cur_val = eTeX_version;
              break;

            case current_group_level_code:
              cur_val = cur_level - level_one;
              break;

            case current_group_type_code:
              cur_val = cur_group;
              break;

            case current_if_level_code:
              {
                q = cond_ptr;
                cur_val = 0;

                while (q != null)
                {
                  incr(cur_val);
                  q = link(q);
                }
              }
              break;

            case current_if_type_code:
              if (cond_ptr == null)
                cur_val = 0;
              else if (cur_if < unless_code)
                cur_val = cur_if + 1;
              else
                cur_val = -(cur_if - unless_code + 1);
              break;

            case current_if_branch_code:
              if ((if_limit == or_code) || (if_limit == else_code))
                cur_val = 1;
              else if (if_limit == fi_code)
                cur_val = -1;
              else
                cur_val = 0;
              break;

            case glue_stretch_order_code:
            case glue_shrink_order_code:
              {
                scan_normal_glue();
                q = cur_val;

                if (m == glue_stretch_order_code)
                  cur_val = stretch_order(q);
                else
                  cur_val = shrink_order(q);

                delete_glue_ref(q);
              }
              break;
          }

          cur_val_level = int_val;
        }
      }
      else
      {
        if (cur_chr == glue_val)
          cur_val = zero_glue;
        else
          cur_val = 0;

        find_effective_tail();

        if (cur_chr == last_node_type_code)
        {
          cur_val_level = int_val;

          if ((tx == head) || (mode == 0))
            cur_val = -1;
        }
        else
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

            case last_node_type_code:
              if (type(tx) <= unset_node)
              {
                if (type(tx) == dir_node)
                  tx = list_ptr(tx);

                cur_val = type(tx);

                if (cur_val < dir_node)
                  cur_val = cur_val + 1;
                else if (cur_val > disp_node)
                  cur_val = cur_val - 1;
              }
              else
                cur_val = unset_node;
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
              if (last_glue != max_halfword)
                cur_val = last_glue;
              break;

            case last_node_type_code:
              cur_val = last_node_type;
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
        negate(width(cur_val));
        negate(stretch(cur_val));
        negate(shrink(cur_val));
      }
    }
    else
      negate(cur_val);
  else if ((cur_val_level >= glue_val) && (cur_val_level <= mu_val))
    add_glue_ref(cur_val);
}
/* sec 0341 */
// sets |cur_cmd|, |cur_chr|, |cur_cs| to next token
void get_next (void)
{
  integer k;
  halfword t;
  /* char cat; */
  int cat;
  integer l;
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
                } while (!(!((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)) || (k > limit)));

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
          do_nothing();
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
          if (name <= 19)
          {
            if (pseudo_input())
              firm_up_the_line();
            else if ((every_eof != null) && !eof_seen[index])
            {
              limit = first - 1;
              eof_seen[index] = true;
              begin_token_list(every_eof, every_eof_text);
              goto restart;
            }
            else
              force_eof = true;
          }
          else
          {
            if (input_ln(cur_file, true))
              firm_up_the_line();
            else if ((every_eof != null) && !eof_seen[index])
            {
              limit = first - 1;
              eof_seen[index] = true;
              begin_token_list(every_eof, every_eof_text);
              goto restart;
            }
            else
              force_eof = true;
          }

        if (force_eof)
        {
          if (tracing_nesting > 0)
            if ((grp_stack[in_open] != cur_boundary) ||
              (if_stack[in_open] != cond_ptr))
              file_warning();

          if (name >= 19)
          {
            print_char(')');
            decr(open_parens);
            update_terminal();
          }

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
          check_outer_validity();
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
            begin_token_list(param_stack[param_start + cur_chr - 1], parameter);
            goto restart;
          }
          break;

        default:
          do_nothing();
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

        align_state = 1000000;
        goto restart;
      }
}
/* sec 0440 */
// sets |cur_val| to an integer
void scan_int (void)
{
  boolean negative;
  integer m;
  small_number d;
  boolean vacuous;
  boolean OK_so_far;

  radix = 0;
  OK_so_far = true;
  negative = false;

  do {
    do {
      get_x_token();
    } while (!(cur_cmd != spacer));

    if (cur_tok == other_token + '-')
    {
      negative = !negative;
      cur_tok = other_token + '+';
    }
  } while (!(cur_tok != other_token + '+'));

  if (cur_tok == alpha_token)
  {
    get_token();

    if (cur_tok < cs_token_flag)
      if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      {
        skip_mode = false;
        cur_val = tonum(cur_chr);
      }
      else
      {
        cur_val = cur_chr;

        if (cur_cmd <= right_brace)
          if (cur_cmd == right_brace)
            incr(align_state);
          else
            decr(align_state);
      }
    else if (cur_tok < cs_token_flag + single_base)
      cur_val = cur_tok - cs_token_flag - active_base;
    else
      cur_val = cur_tok - cs_token_flag - single_base;

    if ((cur_val > 255) && ((cur_cmd < kanji) || (cur_cmd > max_char_code)))
    {
      print_err("Improper alphabetic or KANJI constant");
      help2("A one-character control sequence belongs after a ` mark.",
        "So I'm essentially inserting \\0 here.");
      cur_val = '0';
      back_error();
    }
    else
    {
      get_x_token();

      if (cur_cmd != spacer)
        back_input();
    }

    skip_mode = true;
  }
  else if ((cur_cmd >= min_internal) && (cur_cmd <= max_internal))
    scan_something_internal(int_val, false);
  else
  {
    radix = 10;
    m = 214748364;

    if (cur_tok == octal_token)
    {
      radix = 8;
      m = 268435456;   /* 2^28 */
      get_x_token();
    }
    else if (cur_tok == hex_token)
    {
      radix = 16;
      m = 134217728;   /* 2^27 8000000 hex */
      get_x_token();
    }

    vacuous = true;
    cur_val = 0;

    while (true)
    {
      if ((cur_tok < zero_token + radix) && (cur_tok >= zero_token) &&
        (cur_tok <= zero_token + 9))
        d = cur_tok - zero_token;
      else if (radix == 16)
        if ((cur_tok <= A_token + 5) && (cur_tok >= A_token))
          d = cur_tok - A_token + 10;
        else if ((cur_tok <= other_A_token + 5) && (cur_tok >= other_A_token))
          d = cur_tok - other_A_token + 10;
        else
          goto done;
      else
        goto done;

      vacuous = false;

      if ((cur_val >= m) && ((cur_val > m) || (d > 7) || (radix != 10)))
      {
        if (OK_so_far)
        {
          print_err("Number too big");
          help2("I can only go up to 2147483647='17777777777=\"7FFFFFFF,",
            "so I'm using that number instead of yours.");
          error();
          cur_val = infinity;
          OK_so_far = false;
        }
      }
      else
        cur_val = cur_val * radix + d;

      get_x_token();
    }

done:
    if (vacuous)
    {
      print_err("Missing number, treated as zero");
      help3("A number should have been here; I inserted `0'.",
        "(If you can't figure out why I needed to see a number,",
        "look up `weird error' in the index to The TeXbook.)");
      back_error();
    } 
    else if (cur_cmd != spacer)
      back_input();
  }

  if (negative)
    negate(cur_val);
}
/* sec 0448 */
// sets |cur_val| to a dimension
void scan_dimen (boolean mu, boolean inf, boolean shortcut)
{
  boolean negative;
  integer f;
  integer num, denom;
  small_number k, kk;
  halfword p, q;
  scaled v;
  integer save_cur_val;
  eight_bits t;

  f = 0;
  arith_error = false;
  cur_order = normal;
  negative = false;

  if (!shortcut)
  {
    negative = false;

    do {
      do {
        get_x_token();
      } while (!(cur_cmd != spacer));

      if (cur_tok == other_token + '-')
      {
        negative = !negative;
        cur_tok = other_token + '+';
      }
    } while (!(cur_tok != other_token + '+'));

    if ((cur_cmd >= min_internal) && (cur_cmd <= max_internal))
    {
      if (mu)
      {
        scan_something_internal(mu_val, false);

        if (cur_val_level >= glue_val)
        {
          v = width(cur_val);
          delete_glue_ref(cur_val);
          cur_val = v;
        }

        if (cur_val_level == mu_val)
          goto attach_sign;

        if (cur_val_level != int_val)
          mu_error();
      }
      else
      {
        scan_something_internal(dimen_val, false);

        if (cur_val_level == dimen_val)
          goto attach_sign;
      }
    }
    else
    {
      back_input();

      if (cur_tok == continental_point_token)
        cur_tok = point_token;

      if (cur_tok != point_token)
        scan_int();
      else
      {
        radix = 10;
        cur_val = 0;
      }

      if (cur_tok == continental_point_token)
        cur_tok = point_token;

      if ((radix == 10) && (cur_tok == point_token))
      {
        k = 0;
        p = 0;
        get_token();

        while (true)
        {
          get_x_token();

          if ((cur_tok > zero_token + 9) || (cur_tok < zero_token))
            goto done1;

          if (k < 17)
          {
            q = get_avail();
            link(q) = p;
            info(q) = cur_tok - zero_token;
            p = q;
            incr(k);
          }
        }

done1:
        for (kk = k; kk >= 1; kk--)
        {
          dig[kk - 1] = info(p);
          q = p;
          p = link(p);
          free_avail(q);
        }

        f = round_decimals(k);
        
        if (cur_cmd != spacer)
          back_input();
      }
    }
  }

  if (cur_val < 0)
  {
    negative = !negative;
    negate(cur_val);
  }

  if (inf)
  {
    if (scan_keyword("fil"))
    {
      cur_order = fil;

      while (scan_keyword("l"))
      {
        if (cur_order == filll)
        {
          print_err("Illegal unit of measure (");
          prints("replaced by filll)");
          help1("I dddon't go any higher than filll.");
          error();
        }
        else
          incr(cur_order);
      }

      goto attach_fraction;
    }
  }

  save_cur_val = cur_val;

  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if ((cur_cmd < min_internal) || (cur_cmd > max_internal))
    back_input();
  else
  {
    if (mu)
    {
      scan_something_internal(mu_val, false);

      if (cur_val_level >= glue_val)
      {
        v = width(cur_val);
        delete_glue_ref(cur_val);
        cur_val = v;
      }

      if (cur_val_level != mu_val)
        mu_error();
    }
    else
      scan_something_internal(dimen_val, false);

    v = cur_val;
    goto found;
  }

  if (mu)
    goto not_found;

  if (scan_keyword("em"))
    v = quad(cur_font);
  else if (scan_keyword("ex"))
    v = x_height(cur_font);
  else if (scan_keyword("zw"))
  {
    if (direction == dir_tate)
      v = char_width(cur_tfont, char_info(cur_tfont, 0));
    else
      v = char_width(cur_jfont, char_info(cur_jfont, 0));
  }
  else if (scan_keyword("zh"))
  {
    if (direction == dir_tate)
    {
      t = height_depth(char_info(cur_tfont, 0));
      v = char_height(cur_tfont, t) + char_depth(cur_tfont, t);
    }
    else
    {
      t = height_depth(char_info(cur_jfont, 0));
      v = char_height(cur_jfont, t) + char_depth(cur_jfont, t);
    }
  }
  else
    goto not_found;

  {
    get_x_token();

    if (cur_cmd != spacer)
      back_input();
  }

found:
  cur_val = nx_plus_y(save_cur_val, v, xn_over_d(v, f, 65536L));
  goto attach_sign;

not_found:
  if (mu)
  {
    if (scan_keyword("mu"))
      goto attach_fraction;
    else
    {
      print_err("Illegal unit of measure (");
      prints("mu inserted)");
      help4("The unit of measurement in math glue must be mu.",
          "To recover gracefully from this error, it's best to",
          "delete the erroneous units; e.g., type `2' to delete",
          "two letters. (See Chapter 27 of The TeXbook.)");
      error();
      goto attach_fraction;
    }
  }

  if (scan_keyword("true"))
  {
    prepare_mag();

    if (mag != 1000)
    {
      cur_val = xn_over_d(cur_val, 1000, mag);
      f = (1000 * f + 65536 * tex_remainder) / mag;
      cur_val = cur_val + (f / 65536);
      f = f % 65536;
    }
  }

  if (scan_keyword("pt"))
    goto attach_fraction;

  if (scan_keyword("in"))
    set_conversion(7227, 100);
  else if (scan_keyword("pc"))
    set_conversion(12, 1);
  else if (scan_keyword("cm"))
    set_conversion(7227, 254);
  else if (scan_keyword("mm"))
    set_conversion(7227, 2540);
  else if (scan_keyword("bp"))
    set_conversion(7227, 7200);
  else if (scan_keyword("dd"))
    set_conversion(1238, 1157);
  else if (scan_keyword("cc"))
    set_conversion(14856, 1157);
  else if (scan_keyword("Q"))
    set_conversion(7227, 10160);
  else if (scan_keyword("H"))
    set_conversion(7227, 10160);
  else if (scan_keyword("twip"))
    set_conversion(1, 20);
  else if (scan_keyword("sp"))
    goto done;
  else
  {
    print_err("Illegal unit of measure (");
    prints("pt inserted)");
    help6("Dimensions can be in units of em, ex, in, pt, pc,",
      "cm, mm, dd, cc, bp, or sp; but yours is a new one!",
      "I'll assume that you meant to say pt, for printer's points.",
      "To recover gracefully from this error, it's best to",
      "delete the erroneous units; e.g., type `2' to delete",
      "two letters. (See Chapter 27 of The TeXbook.)");
    error();
    goto done2;
  }

  cur_val = xn_over_d(cur_val, num, denom);
  f = (num * f + 65536L * tex_remainder) / denom;
  cur_val = cur_val + (f / 65536L);
  f = f % 65536L;

done2:
attach_fraction:
  if (cur_val >= 16384)     /* 2^14 */
    arith_error = true;
  else
    cur_val = cur_val * unity + f;

done:
  {
    get_x_token();

    if (cur_cmd != spacer)
      back_input();
  }

attach_sign:
  if (arith_error || (abs(cur_val) >= 1073741824L)) /* 2^30 */
  {
    print_err("Dimension too large");
    help2("I can't work with sizes bigger than about 19 feet.",
        "Continue and I'll use the largest value I can.");
    error();
    cur_val = max_dimen;
    arith_error = false;
  }

  if (negative)
    negate(cur_val);
}
/* sec 0461 */
// sets |cur_val| to a glue spec pointer
void scan_glue (small_number level)
{
  boolean negative;
  pointer q;
  boolean mu;

  mu = (level == mu_val);
  negative = false;

  do {
    do {
      get_x_token();
    } while (!(cur_cmd != spacer));

    if (cur_tok == other_token + '-')
    {
      negative = !negative;
      cur_tok = other_token + '+';
    }
  } while (!(cur_tok != other_token + '+'));

  if ((cur_cmd >= min_internal) && (cur_cmd <= max_internal))
  {
    scan_something_internal(level, negative);

    if (cur_val_level >= glue_val)
    {
      if (cur_val_level != level)
        mu_error();

      return;
    }

    if (cur_val_level == int_val)
      scan_dimen(mu, false, true);
    else if (level == mu_val)
      mu_error();
  }
  else
  {
    back_input();
    scan_dimen(mu, false, false);

    if (negative)
      negate(cur_val);
  }

  q = new_spec(zero_glue);
  width(q) = cur_val;

  if (scan_keyword("plus"))
  {
    scan_dimen(mu, true, false);
    stretch(q) = cur_val;
    stretch_order(q) = cur_order;
  }

  if (scan_keyword("minus"))
  {
    scan_dimen(mu, true, false);
    shrink(q) = cur_val;
    shrink_order(q) = cur_order;
  }

  cur_val = q;
}
/* sec 0463 */
pointer scan_rule_spec (void)
{
  pointer q;

  q = new_rule();

  if (cur_cmd == vrule)
    width(q) = default_rule;
  else
  {
    height(q) = default_rule;
    depth(q) = 0;
  }

reswitch:

  if (scan_keyword("width"))
  {
    scan_normal_dimen();
    width(q) = cur_val;
    goto reswitch;
  }

  if (scan_keyword("height"))
  {
    scan_normal_dimen();
    height(q) = cur_val;
    goto reswitch;
  }

  if (scan_keyword("depth"))
  {
    scan_normal_dimen();
    depth(q) = cur_val;
    goto reswitch;
  }

  return q;
}
/* sec 0464 */
// changes the string |str_pool[b..pool_ptr]| to a token list
pointer str_toks (pool_pointer b)
{
  pointer p;
  pointer q;
  halfword t;
  pool_pointer k;
  int cc;

  str_room(1);
  p = temp_head;
  link(p) = 0;
  k = b;

  while (k < pool_ptr)
  {
    t = fromBUFF(str_pool, pool_ptr, k);
    cc = kcat_code(kcatcodekey(t));

    if ((multistrlen(str_pool, pool_ptr, k) > 1) && check_kcat_code(cc))
    {
      if (cc == not_cjk)
        cc = other_kchar;

      t = t + cc * max_cjk_val;
      k = k + multistrlen(str_pool, pool_ptr, k) - 1;
    }

    else
    {
      t = str_pool[k];

      if (t == ' ')
        t = space_token;
      else
        t = other_token + t;
    }

    fast_store_new_token(t);
    incr(k);
  }

  pool_ptr = b;

  return p;
}
/* sec 0465 */
pointer the_toks (void)
{
  char old_setting;
  pointer p, q, r;
  pool_pointer b;
  small_number c;

  if (odd(cur_chr))
  {
    c = cur_chr;
    scan_general_text();

    if (c == 1)
      return cur_val;
    else
    {
      old_setting = selector;
      selector = new_string;
      b = pool_ptr;
      p = get_avail();
      link(p) = link(temp_head);
      token_show(p);
      flush_list(p);
      selector = old_setting;
      return str_toks(b);
    }
  }

  get_x_token();
  scan_something_internal(tok_val, false);

  if (cur_val_level >= ident_val)
  {
    p = temp_head;
    link(p) = 0;

    if (cur_val_level == ident_val)
      store_new_token(cs_token_flag + cur_val);
    else if (cur_val != 0)
    {
      r = link(cur_val);

      while (r != 0)
      {
        fast_store_new_token(info(r));
        r = link(r);
      }
    }

    return p;
  }
  else
  {
    old_setting = selector;
    selector = new_string;
    b = pool_ptr;

    switch (cur_val_level)
    {
      case int_val:
        print_int(cur_val);
        break;

      case dimen_val:
        {
          print_scaled(cur_val);
          prints("pt");
        }
        break;

      case glue_val:
        {
          print_spec(cur_val, "pt");
          delete_glue_ref(cur_val);
        }
        break;

      case mu_val:
        {
          print_spec(cur_val, "mu");
          delete_glue_ref(cur_val);
        }
        break;
    }

    selector = old_setting;
    return str_toks(b);
  }
}
/* sec 0467 */
void ins_the_toks (void) 
{ 
  link(garbage) = the_toks();
  ins_list(link(temp_head));
}
/* sec 0470 */
void conv_toks (void)
{
  char old_setting;
  KANJI_code cx;
  char c;
  small_number save_scanner_status;
  pool_pointer b;

  c = cur_chr;
  KANJI(cx) = 0;

  switch (c)
  {
    case number_code:
    case roman_numeral_code:
    case kansuji_code:
    case euc_code:
    case sjis_code:
    case jis_code:
    case kuten_code:
    case ucs_code:
      scan_int();
      break;

    case string_code:
    case meaning_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_token();

        if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
          KANJI(cx) = cur_tok;

        scanner_status = save_scanner_status;
      }
      break;

    case font_name_code:
      scan_font_ident();
      break;

    case eTeX_revision_code:
      do_nothing();
      break;

    case job_name_code:
      if (job_name == 0)
        open_log_file();
      break;
  }

  old_setting = selector;
  selector = new_string;
  b = pool_ptr;

  switch (c)
  {
    case number_code:
      print_int(cur_val);
      break;

    case roman_numeral_code:
      print_roman_int(cur_val);
      break;

    case jis_code:
      print_int(fromJIS(cur_val));
      break;

    case euc_code:
      print_int(fromEUC(cur_val));
      break;

    case sjis_code:
      print_int(fromSJIS(cur_val));
      break;

    case kuten_code:
      print_int(fromKUTEN(cur_val));
      break;

    case ucs_code:
      print_int(fromUCS(cur_val));
      break;

    case kansuji_code:
      print_kansuji(cur_val);
      break;

    case string_code:
      if (cur_cs != 0)
        sprint_cs(cur_cs);
      else if (KANJI(cx) == 0)
        print_char(cur_chr);
      else
        print_kanji(cx);
      break;

    case meaning_code:
      print_meaning();
      break;

    case font_name_code:
      {
        print(font_name[cur_val]);

        if (font_size[cur_val] != font_dsize[cur_val])
        {
          prints(" at ");
          print_scaled(font_size[cur_val]);
          prints("pt");
        }
      }
      break;

    case eTeX_revision_code:
      prints(eTeX_revision);
      break;

    case job_name_code:
      print(job_name);
      break;
  }

  selector = old_setting;
  link(garbage) = str_toks(b);
  ins_list(link(temp_head));
}
/* sec 0473 */
pointer scan_toks (boolean macro_def, boolean xpand)
{
  halfword t;
  halfword s;
  pointer p;
  pointer q;
  halfword unbalance;
  halfword hash_brace;

  if (macro_def)
    scanner_status = defining;
  else
    scanner_status = absorbing;

  warning_index = cur_cs;
  def_ref = get_avail();
  token_ref_count(def_ref) = 0;
  p = def_ref;
  hash_brace = 0;
  t = zero_token;

  if (macro_def)
  {
    while (true)
    {
      get_token();

      if (cur_tok < right_brace_limit)
        goto done1;

      if (cur_cmd == mac_param)
      {
        s = match_token + cur_chr;
        get_token();

        if (cur_cmd == left_brace)
        {
          hash_brace = cur_tok;
          store_new_token(cur_tok);
          store_new_token(end_match_token);
          goto done;
        }

        if (t == zero_token + 9)
        {
          print_err("You already have nine parameters");
          help1("I'm going to ignore the # sign you just used.");
          error();
        }
        else
        {
          incr(t);

          if (cur_tok != t)
          {
            print_err("Parameters must be numbered consecutively");
            help2("I've inserted the digit you should have used after the #.",
                "Type `1' to delete what you did use.");
            back_error();
          }

          cur_tok = s;
        }
      }

      store_new_token(cur_tok);
    }

done1:
    store_new_token(end_match_token);

    if (cur_cmd == right_brace)
    {
      print_err("Missing { inserted");
      incr(align_state);
      help2("Where was the left brace? You said something like `\\def\\a}',",
          "which I'm going to interpret as `\\def\\a{}'.");
      error();
      goto found;
    }
done:;
  }
  else
    scan_left_brace();

  unbalance = 1;

  while (true)
  {
    if (xpand)
    {
      while (true)
      {
        get_next();

        if (cur_cmd >= call)
          if (info(link(cur_chr)) == protected_token)
          {
            cur_cmd = relax;
            cur_chr = no_expand_flag;
          }

        if (cur_cmd <= max_command)
          goto done2;

        if (cur_cmd != the)
          expand();
        else
        {
          q = the_toks();

          if (link(temp_head) != 0)
          {
            link(p) = link(temp_head);
            p = q;
          }
        }
      }
done2:
      x_token();
    }
    else
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
    else if (cur_cmd == mac_param)
      if (macro_def)
      {
        s = cur_tok;

        if (xpand)
          get_x_token();
        else
          get_token();

        if (cur_cmd != mac_param)
          if ((cur_tok <= zero_token) || (cur_tok > t))
          {
            print_err("Illegal parameter number in definition of ");
            sprint_cs(warning_index);
            help3("You meant to type ## instead of #, right?",
                "Or maybe a } was forgotten somewhere earlier, and things",
                "are all screwed up? I'm going to assume that you meant ##.");
            back_error();
            cur_tok = s;
          }
          else
            cur_tok = out_param_token - '0' + cur_chr;
      }

    store_new_token(cur_tok);
  }

found:
  scanner_status = normal;

  if (hash_brace != 0)
    store_new_token(hash_brace);

  return p;
}
/* sec 0482 */
void read_toks (integer n, pointer r, halfword j)
{
  pointer p;
  pointer q;
  integer s;
  /* small_number m; */
  int m;

  scanner_status = defining;
  warning_index = r;
  def_ref = get_avail();
  token_ref_count(def_ref) = 0;
  p = def_ref;
  store_new_token(end_match_token);

  if ((n < 0) || (n > 15))
    m = 16;
  else
    m = n;

  s = align_state;
  align_state = 1000000L;

  do {
    begin_file_reading();
    name = m + 1;

    if (read_open[m] == closed)
      if (interaction > nonstop_mode)
        if (n < 0)
          prompt_input("");
        else
        {
          wake_up_terminal();
          print_ln();
          sprint_cs(r);
          prompt_input("=");
          n = -1;
        }
      else
      {
        fatal_error("*** (cannot \\read from terminal in nonstop modes)");
        return;
      }
    else if (read_open[m] == just_open)
      if (input_ln(read_file[m], false))
        read_open[m] = normal;
      else
      {
        a_close(read_file[m]);
        read_open[m] = closed;
      }
    else
    {
      if (!input_ln(read_file[m], true))
      {
        a_close(read_file[m]);
        read_open[m] = closed;

        if (align_state != 1000000L)
        {
          runaway();
          print_err("File ended within ");
          print_esc("read");
          help1("This \\read has unbalanced braces.");
          align_state = 1000000L;
          error();
        }
      }
    }

    limit = last;

    if (end_line_char_inactive())
      decr(limit);
    else
      buffer[limit] = end_line_char;

    first = limit + 1;
    loc = start;
    state = new_line;

    if (j == 1)
    {
      while (loc <= limit)
      {
        cur_chr = buffer[loc];
        incr(loc);

        if (cur_chr == ' ')
          cur_tok = space_token;
        else
          cur_tok = cur_chr + other_token;

        store_new_token(cur_tok);
      }

      goto done;
    }

    while (true)
    {
      get_token();

      if (cur_tok == 0)
        goto done;

      if (align_state < 1000000L)
      {
        do {
          get_token();
        } while (!(cur_tok == 0));

        align_state = 1000000L;
        goto done;
      }

      store_new_token(cur_tok);
    }

done:
    end_file_reading();
  } while (!(align_state == 1000000L));

  cur_val = def_ref;
  scanner_status = normal;
  align_state = s;
}
/* sec 0494 */
void pass_text (void)
{
  integer l;
  small_number save_scanner_status;

  save_scanner_status = scanner_status;
  scanner_status = skipping;
  l = 0;
  skip_line = line;

  while (true)
  {
    get_next();

    if (cur_cmd == fi_or_else)
    {
      if (l == 0)
        goto done;

      if (cur_chr == fi_code)
        decr(l);
    }
    else if (cur_cmd == if_test)
      incr(l);
  }

done:
  scanner_status = save_scanner_status;

  if (tracing_ifs > 0)
    show_cur_cmd_chr();
}
/* sec 0497 */
void change_if_limit (small_number l, pointer p)
{
  pointer q;

  if (p == cond_ptr)
    if_limit = l;
  else
  {
    q = cond_ptr;

    while (true)
    {
      if (q == 0)
      {
        confusion("if");
        return;
      }

      if (link(q) == p)
      {
        type(p) = l;
        return;
      }

      q = link(q);
    }
  }
}
/* sec 0498 */
void conditional (void)
{
  boolean b;
  char r;
  integer m, n;
  pointer p, q;
  small_number save_scanner_status;
  pointer save_cond_ptr;
  small_number this_if;
  boolean is_unless;

  if (tracing_ifs > 0)
    if (tracing_commands <= 1)
      show_cur_cmd_chr();

  {
    p = get_node(if_node_size);
    link(p) = cond_ptr;
    type(p) = if_limit;
    subtype(p) = cur_if;
    if_line_field(p) = if_line;
    cond_ptr = p;
    cur_if = cur_chr;
    if_limit = if_code;
    if_line = line;
  }

  save_cond_ptr = cond_ptr;
  is_unless = (cur_chr >= unless_code);
  this_if = cur_chr % unless_code;

  switch (this_if)
  {
    case if_char_code:
    case if_cat_code:
      {
        get_x_token_or_active_char();

        if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
        {
          m = cur_cmd;
          n = cur_chr;
        }
        else if ((cur_cmd > active_char) || (cur_chr > 255))
        {
          m = relax;
          n = 256;
        }
        else
        {
          m = cur_cmd;
          n = cur_chr;
        }

        get_x_token_or_active_char();

        if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
        {
          cur_cmd = cur_cmd;
        }
        else if ((cur_cmd > active_char) || (cur_chr > 255))
        {
          cur_cmd = relax;
          cur_chr = 256;
        }

        if (this_if == if_char_code)
          b = (n == cur_chr); 
        else
          b = (m == cur_cmd);
      }
      break;

    case if_int_code:
    case if_dim_code:
      {
        if (this_if == if_int_code)
          scan_int();
        else
          scan_normal_dimen();

        n = cur_val;
        
        do {
          get_x_token();
        } while (!(cur_cmd != spacer));

        if ((cur_tok >= other_token + '<') && (cur_tok <= other_token + '>'))
          r = cur_tok - other_token;
        else
        {
          print_err("Missing = inserted for ");
          print_cmd_chr(if_test, this_if);
          help1("I was expecting to see `<', `=', or `>'. Didn't.");
          back_error();
          r = '=';
        }

        if (this_if == if_int_code)
          scan_int();
        else 
          scan_normal_dimen();

        switch (r)
        {
          case '<':
            b = (n < cur_val);
            break;

          case '=':
            b = (n == cur_val);
            break;

          case '>':
            b = (n > cur_val);
            break;
        }
      }
      break;

    case if_odd_code:
      {
        scan_int();
        b = odd(cur_val);
      }
      break;

    case if_vmode_code:
      b = (abs(mode) == vmode);
      break;

    case if_hmode_code:
      b = (abs(mode) == hmode);
      break;

    case if_mmode_code:
      b = (abs(mode) == mmode);
      break;

    case if_inner_code:
      b = (mode < 0);
      break;

    case if_tdir_code:
      b = (abs(direction) == dir_tate);
      break;

    case if_ydir_code:
      b = (abs(direction) == dir_yoko);
      break;

    case if_ddir_code:
      b = (abs(direction) == dir_dtou);
      break;

    case if_mdir_code:
      b = (direction < 0);
      break;

    case if_void_code:
    case if_hbox_code:
    case if_vbox_code:
    case if_tbox_code:
    case if_ybox_code:
    case if_dbox_code:
      {
        scan_register_num();
        fetch_box(p);

        if (this_if == if_void_code)
          b = (p == 0);
        else if (p == 0)
          b = false;
        else
        {
          if (type(p) == dir_node)
            p = list_ptr(p);

          if (this_if == if_hbox_code)
            b = (type(p) == hlist_node);
          else if (this_if == if_vbox_code)
            b = (type(p) == vlist_node);
          else if (this_if == if_tbox_code)
            b = (box_dir(p) == dir_tate);
          else if (this_if == if_ybox_code)
            b = (box_dir(p) == dir_yoko);
          else
            b = (box_dir(p) == dir_dtou);
        }
      }
      break;

    case ifx_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_next();
        n = cur_cs;
        p = cur_cmd;
        q = cur_chr;
        get_next();

        if (cur_cmd != p)
          b = false;
        else if (cur_cmd < call)
          b = (cur_chr == q);
        else
        {
          p = link(cur_chr);
          q = link(equiv(n));

          if (p == q)
            b = true;
          else
          {
            while ((p != 0) && (q != 0))
              if (info(p) != info(q))
                p = 0;
              else
              {
                p = link(p);
                q = link(q);
              }

            b = ((p == 0) && (q == 0));
          }
        }

        scanner_status = save_scanner_status;
      }
      break;

    case if_eof_code:
      {
        scan_four_bit_int();
        b = (read_open[cur_val] == closed);
      }
      break;

    case if_true_code:
      b = true;
      break;

    case if_false_code:
      b = false;
      break;

    case if_def_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_next();
        b = (cur_cmd != undefined_cs);
        scanner_status = save_scanner_status;
      }
      break;

    case if_cs_code:
      {
        n = get_avail();
        p = n;

        do {
          get_x_token();

          if (cur_cs == 0)
            store_new_token(cur_tok);
        } while (!(cur_cs != 0));

        if (cur_cmd != end_cs_name)
        {
          print_err("Missing ");
          print_esc("endcsname");
          prints(" inserted");
          help2("The control sequence marked <to be read again> should",
            "not appear between \\csname and \\endcsname.");
          back_error();
        }

        m = first;
        p = link(n);

        while (p != null)
        {
          if (m >= max_buf_stack)
          {
            max_buf_stack = m + 1;

#ifdef ALLOCATEBUFFER
            if (max_buf_stack == current_buf_size)
              buffer = realloc_buffer(increment_buf_size);

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
            if (BYTE1(toBUFF(info(p) % max_cjk_val)) != 0)
            {
              buffer[m] = BYTE1(toBUFF(info(p) % max_cjk_val));
              incr(m);
            }

            if (BYTE2(toBUFF(info(p) % max_cjk_val)) != 0)
            {
              buffer[m] = BYTE2(toBUFF(info(p) % max_cjk_val));
              incr(m);
            }

            if (BYTE3(toBUFF(info(p) % max_cjk_val)) != 0)
            {
              buffer[m] = BYTE3(toBUFF(info(p) % max_cjk_val));
              incr(m);
            }
          
            buffer[m] = BYTE4(toBUFF(info(p) % max_cjk_val));
            incr(m);
            p = link(p);
          }
          else
          {
            buffer[m] = info(p) % max_char_val; incr(m);
            p = link(p);
          }
        }

        if (m > first + 1)
          cur_cs = id_lookup(first, m - first);
        else if (m == first)
          cur_cs = null_cs;
        else
          cur_cs = single_base + buffer[first];

        flush_list(n);
        b = (eq_type(cur_cs) != undefined_cs);
      }
      break;

    case if_font_char_code:
      {
        scan_font_ident();
        n = cur_val;
        scan_char_num();

        if ((font_bc[n] <= cur_val) && (font_ec[n] >= cur_val))
          b = char_exists(char_info(n, cur_val));
        else
          b = false;
      }
      break;

    case if_case_code:
      {
        scan_int();
        n = cur_val;

        if (tracing_commands > 1)
        {
          begin_diagnostic();
          prints("{case ");
          print_int(n); 
          print_char('}');
          end_diagnostic(false);
        }

        while (n != 0)
        {
          pass_text();

          if (cond_ptr == save_cond_ptr)
            if (cur_chr == or_code)
              decr(n);
            else 
              goto common_ending;
          else if (cur_chr == fi_code)
          {
            if (if_stack[in_open] == cond_ptr)
              if_warning();

            p = cond_ptr;
            if_line = if_line_field(p);
            cur_if = subtype(p);
            if_limit = type(p);
            cond_ptr = link(p);
            free_node(p, if_node_size);
          }
        }

        change_if_limit(or_code, save_cond_ptr);
        return;
      }
      break;
  }

  if (is_unless)
    b = !b;

  if (tracing_commands > 1)
  {
    begin_diagnostic();

    if (b)
      prints("{true}");
    else
      prints("{false}");

    end_diagnostic(false);
  }

  if (b)
  {
    change_if_limit(else_code, save_cond_ptr);
    return;
  }

  while (true)
  {
    pass_text();

    if (cond_ptr == save_cond_ptr)
    {
      if (cur_chr != or_code)
        goto common_ending;

      print_err("Extra ");
      print_esc("or");
      help1("I'm ignoring this; it doesn't match any \\if.");
      error();
    }
    else if (cur_chr == fi_code)
    {
      if (if_stack[in_open] == cond_ptr)
        if_warning();

      p = cond_ptr;
      if_line = if_line_field(p);
      cur_if = subtype(p);
      if_limit = type(p);
      cond_ptr = link(p);
      free_node(p, if_node_size);
    }
  }

common_ending:
  if (cur_chr == fi_code)
  {
    if (if_stack[in_open] == cond_ptr)
      if_warning();

    p = cond_ptr;
    if_line = if_line_field(p);
    cur_if = subtype(p);
    if_limit = type(p);
    cond_ptr = link(p);
    free_node(p, if_node_size);
  }
  else
    if_limit = fi_code;
}
/* sec 0515 */
void begin_name (void)
{
  area_delimiter = 0;
  ext_delimiter = 0;
  prev_char = 0;
}
/* sec 0516 */
boolean more_name (ASCII_code c)
{
  if (!quoted_file_name && c == ' ')
    return false;
  else if (quoted_file_name && c == '"')
  {
    quoted_file_name = false; // catch next space character 
    return true;     // accept ending quote, but throw away
  }
  else
  {   
    str_room(1);
    append_char(c);

    //  for DOS/Windows
    if ((c == '/' || c == '\\' || c == ':')) 
    {
      area_delimiter = cur_length;
      ext_delimiter = 0;
    } 
    else if (c == '.')
      ext_delimiter = cur_length;

    return true;
  }
}
/* sec 0517 */
void end_name (void) 
{
#ifdef ALLOCATESTRING
  if (str_ptr + 3 > current_max_strings)
    str_start = realloc_str_start(increment_max_strings + 3);

  if (str_ptr + 3 > current_max_strings)
  {
    overflow("number of strings", current_max_strings - init_str_ptr);
    return;
  }
#else
  if (str_ptr + 3 > max_strings)
  {
    overflow("number of strings", max_strings - init_str_ptr);
    return;
  }
#endif

  if (area_delimiter == 0)
    cur_area = 335;
  else
  {
    cur_area = str_ptr;
    str_start[str_ptr + 1] = str_start[str_ptr] + area_delimiter;
    incr(str_ptr);
  }

  if (ext_delimiter == 0)
  {
    cur_ext = 335;
    cur_name = make_string();
  } 
  else
  {
    cur_name = str_ptr;
    str_start[str_ptr + 1] = str_start[str_ptr] + ext_delimiter - area_delimiter - 1;
    incr(str_ptr);
    cur_ext = make_string();
  }
}
/* sec 0519 */
void pack_file_name (str_number n, str_number a, str_number e)
{
  integer k;
  ASCII_code c;
  pool_pointer j;

  k = 0;

  for (j = str_start[a]; j <= str_start[a + 1] - 1; j++)
    append_to_name(str_pool[j]);

  for (j = str_start[n]; j <= str_start[n + 1] - 1; j++)
    append_to_name(str_pool[j]);

  for (j = str_start[e]; j <= str_start[e + 1] - 1; j++)
    append_to_name(str_pool[j]);

  if (k < file_name_size)
    name_length = k;
  else
    name_length = file_name_size - 1;

  for (k = name_length + 1; k <= file_name_size; k++)
    name_of_file[k] = ' ';

  name_of_file[file_name_size] = '\0';
}
/* sec 0523 */
void pack_buffered_name (small_number n, integer a, integer b)
{
  integer k;
  ASCII_code c;
  integer j;

  if (n + b - a + 5 > file_name_size)
    b = a + file_name_size - n - 5;

  k = 0;

  for (j = 1; j <= n; j++)
    append_to_name(xord[TEX_format_default[j]]);

  for (j = a; j <= b; j++)
    append_to_name(buffer[j]);

  for (j = format_default_length - 3; j <= format_default_length; j++)
    append_to_name(xord[TEX_format_default[j]]);

  if (k < file_name_size)
    name_length = k;
  else
    name_length = file_name_size - 1;

  for (k = name_length + 1; k <= file_name_size; k++)
    name_of_file[k]= ' ';

  name_of_file[file_name_size] = '\0';
}
/* sec 0525 */
str_number make_name_string (void)
{
  integer k;

#ifdef ALLOCATESTRING
  if (pool_ptr + name_length > current_pool_size)
    str_pool = realloc_str_pool(increment_pool_size + name_length);

  if (str_ptr == current_max_strings)
    str_start = realloc_str_start(increment_max_strings);

  if ((pool_ptr + name_length > current_pool_size) || (str_ptr == current_max_strings) || (cur_length > 0))
#else
  if ((pool_ptr + name_length > pool_size) || (str_ptr == max_strings) || (cur_length > 0))
#endif
  {
    return '?';
  }
  else
  {
    for (k = 1; k <= name_length; k++)
      append_char(xord[name_of_file[k]]);

    return make_string();
  }
}
/* sec 0525 */
//str_number a_make_name_string (alpha_file * f)
str_number a_make_name_string_(void)
{
  return make_name_string();
}
/* sec 0525 */
//str_number b_make_name_string_(byte_file * f)
str_number b_make_name_string_(void)
{
  return make_name_string(); 
}
/* sec 0525 */
//str_number w_make_name_string_(word_file * f)
str_number w_make_name_string_(void)
{
  return make_name_string();
}
/* sec 0526 */
void scan_file_name (void)
{
  name_in_progress = true;
  begin_name();

  do {
    get_x_token(); 
  } while (!(cur_cmd != spacer));

  quoted_file_name = false;

  if (allow_quoted_names)
  {
    if (cur_chr == '"')
    {
      quoted_file_name = true;
      get_x_token();
    }
  }

  skip_mode = false;

  while (true)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
    {
      str_room(4);
      cur_chr = toBUFF(cur_chr);

      if (BYTE1(cur_chr) != 0)
        append_char(BYTE1(cur_chr));

      if (BYTE2(cur_chr) != 0)
        append_char(BYTE2(cur_chr));

      if (BYTE3(cur_chr) != 0)
        append_char(BYTE3(cur_chr));

      append_char(BYTE4(cur_chr));
    }
    else if ((cur_cmd > other_char) || (cur_chr > 255)) 
    {
      back_input();
      goto done; 
    }
    else if (((cur_chr == ' ') && (state != token_list) && (loc > limit)) || !more_name(cur_chr))
      goto done;

    get_x_token();
  }

done:
  end_name();
  name_in_progress = false;
  skip_mode = true;
}
/* sec 0529 */
void pack_job_name_(str_number s)
{
  cur_area = 335; /* "" */
  cur_ext  = s;
  cur_name = job_name;
  pack_cur_name();
}
/* sec 0530 */
void prompt_file_name_(const char * s, str_number e) 
{
  integer k;

  if (interaction == scroll_mode)
    wake_up_terminal();

  if (!strcmp("input file name", s))
    print_err("I can't find file `");
  else
    print_err("I can't write on file `");

  print_file_name(cur_name, cur_area, cur_ext);
  prints("'.");

  if (e == 785)    /* .tex */
    show_context();

  print_nl("Please type another ");
  prints(s);

  if (interaction < scroll_mode)
  {
    fatal_error("*** (job aborted, file error in nonstop mode)");
    return;
  }

  if (!tex82_flag)
    show_line(" (or Ctrl-Z to exit)", 0);

  prompt_input(": ");

  {
    begin_name();
    k = first;

    while ((buffer[k] == ' ') && (k < last))
      incr(k);

    quoted_file_name = false;

    if (allow_quoted_names && k < last)
    {
      if (buffer[k]== '"')
      {
        quoted_file_name = true;
        incr(k);
      }
    }

    while (true)
    {
      if (k == last)
        goto done;

      if (!more_name(buffer[k]))
        goto done;

      incr(k);
    }

done:
    end_name();
  }

  if (cur_ext == 335) /* "" */
    cur_ext = e;      /* use default extension */

  pack_cur_name();
}
/* sec 0534 */
void open_log_file (void)
{
  char old_setting;
  integer k;
  integer l;
  const char * months;

  old_setting = selector;

  if (job_name == 0)
    job_name = get_job_name(790);

  pack_job_name(".log");

  while (!a_open_out(log_file))
  {
    selector = term_only;
    prompt_file_name("transcript file name", ".log");
  }

  log_name = a_make_name_string(log_file);
  selector = log_only;
  log_opened = true;

  {
    log_printf("%s", banner);

    if (format_ident > 0)
      slow_print(format_ident);

    prints("  ");

    if (civilize_flag)
      print_int(year);
    else
      print_int(day);

    print_char(' ');
    months = " JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";

    for (k = 3 * month - 2; k <= 3 * month; k++)
      wlog(months[k]);

    print_char(' ');

    if (civilize_flag)
      print_int(day);
    else
      print_int(year);

    print_char(' ');
    print_two(tex_time / 60);
    print_char(':');
    print_two(tex_time % 60);

    if (eTeX_ex)
    {
      wlog_cr();
      fputs("entering extended mode", log_file);
    }
  }

  input_stack[input_ptr] = cur_input;
  print_nl("**");
  l = input_stack[0].limit_field;

  if (buffer[l] == end_line_char)
    decr(l);

  for (k = 1; k <= l; k++)
    print(buffer[k]);

  print_ln(); 

  if (show_fmt_flag)
  {
    if (format_file != NULL)
    {
      log_printf("(%s)\n", format_file);
      free(format_file);
      format_file = NULL;
    }
  }

  selector = old_setting + 2;
}
/* sec 0537 */
// \TeX\ will \.{\\input} something
void start_input (void)
{
  scan_file_name();
  pack_cur_name();

  while (true)
  {
    begin_file_reading();
    
    if (a_open_in(cur_file))
      goto done;

    end_file_reading();
    prompt_file_name("input file name", ".tex");
  }

done: 
  name = a_make_name_string(cur_file);

  if (job_name == 0)
  {
    job_name = get_job_name(cur_name);
    open_log_file();
  }

  if (term_offset + length(name) > max_print_line - 2)
    print_ln();
  else if ((term_offset > 0) || (file_offset > 0))
    print_char(' ');

  print_char('(');
  incr(open_parens);

  if (open_parens > max_open_parens)
    max_open_parens = open_parens;

  slow_print(name);
  update_terminal();
  state = new_line;

  {
    line = 1;

    if (input_ln(cur_file, false))
      do_nothing();

    firm_up_the_line();

    if (end_line_char_inactive())
      decr(limit);
    else
      buffer[limit] = end_line_char;

    first = limit + 1;
    loc = start;
  }
}
/* sec 0560 */
// input a \.{TFM} file
internal_font_number read_font_info (pointer u, str_number nom, str_number aire, scaled s)
{
  font_index k;
  int jfm_flag;
  halfword nt;
  KANJI_code cx;
  boolean file_opened;
  halfword lf, lh, nw, nh, nd, ni, nl, nk, ne, np;
  int bc, ec;
  internal_font_number f;
  internal_font_number g;
  eight_bits a, b, c, d;
  four_quarters qw;
  scaled sw;
  integer bch_label;
  short bchar;
  scaled z;
  integer alpha;
  char beta;

  g = null_font;
  file_opened = false;
  pack_file_name(nom, aire, 805); /* .tfm */

  if (!b_open_in(tfm_file))
    goto bad_tfm;

  file_opened = true;

  {
    read_sixteen(lf);
    fget();
    read_sixteen(lh);

    if (lf == yoko_jfm_id)
    {
      jfm_flag = dir_yoko;
      nt = lh;
      fget();
      read_sixteen(lf);
      fget();
      read_sixteen(lh);
    }
    else if (lf == tate_jfm_id)
    {
      jfm_flag = dir_tate;
      nt = lh;
      fget();
      read_sixteen(lf);
      fget();
      read_sixteen(lh);
    }
    else
    {
      jfm_flag = dir_default;
      nt = 0;
    }

    fget();
    read_sixteen(bc);
    fget();
    read_sixteen(ec);

    if ((bc > ec + 1) || (ec > 255))
      goto bad_tfm;

    if (bc > 255)
    {
      bc = 1;
      ec = 0;
    }

    fget();
    read_sixteen(nw);
    fget();
    read_sixteen(nh);
    fget();
    read_sixteen(nd);
    fget();
    read_sixteen(ni);
    fget();
    read_sixteen(nl);
    fget();
    read_sixteen(nk);
    fget();
    read_sixteen(ne);
    fget();
    read_sixteen(np);

    if (jfm_flag != dir_default)
    {
      if (lf != 7 + lh + nt + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np)
        goto bad_tfm;
    }
    else
    {
      if (lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np)
        goto bad_tfm;
    }

    if ((nw == 0) || (nh == 0) || (nd == 0) || (ni == 0))
      goto bad_tfm;
  }

  if (jfm_flag != dir_default)
    lf = lf - 7 - lh;
  else
    lf = lf - 6 - lh;

  if (np < 7)
    lf = lf + 7 - np;

#ifdef ALLOCATEFONT
  if ((fmem_ptr + lf > current_font_mem_size))
    font_info = realloc_font_info (increment_font_mem_size + lf);

  if ((font_ptr == font_max) || (fmem_ptr + lf > current_font_mem_size))
#else
  if ((font_ptr == font_max) || (fmem_ptr + lf > font_mem_size))
#endif
  {
    if (trace_flag)
      printf("font_ptr %lld font_max %d fmem_ptr %lld lf %d font_mem_size %ld\n",
          font_ptr, font_max, fmem_ptr, lf, font_mem_size);

    start_font_error_message();
    prints(" not loaded: Not enough room left");
    help4("I'm afraid I won't be able to make use of this font,",
        "because my memory for character-size data is too small.",
        "If you're really stuck, ask a wizard to enlarge me.",
        "Or maybe try `I\\font<same font id>=<name of loaded font>'.");
    error();
    goto done;
  }

  f = font_ptr + 1;
  font_dir[f] = jfm_flag;
  font_num_ext[f] = nt;
  ctype_base[f] = fmem_ptr;
  font_cmap[f] = cur_cmap;
  font_spec[f] = cur_spec;
  char_base[f] = ctype_base[f] + nt - bc;
  width_base[f] = char_base[f] + ec + 1;
  height_base[f] = width_base[f] + nw;
  depth_base[f] = height_base[f] + nh;
  italic_base[f] = depth_base[f] + nd;
  lig_kern_base[f] = italic_base[f] + ni;
  kern_base[f] = lig_kern_base[f] + nl - kern_base_offset;
  exten_base[f] = kern_base[f] + kern_base_offset + nk;
  param_base[f] = exten_base[f] + ne;

  {
    if (lh < 2)
      goto bad_tfm;
    
    store_four_quarters(font_check[f]);
    fget();
    read_sixteen(z);
    fget();
    z = z * 256 + fbyte;
    fget();
    z = (z * 16) + (fbyte / 16);

    if (z < unity)
      goto bad_tfm; 

    while (lh > 2)
    {
      fget();
      fget();
      fget();
      fget();
      decr(lh);
    }

    font_dsize[f] = z;

    if (s != -1000)
      if (s >= 0)
        z = s;
      else
        z = xn_over_d(z, -s, 1000);

    font_size[f] = z;
  }

  if (jfm_flag != dir_default)
  {
    for (k = ctype_base[f]; k <= ctype_base[f] + nt - 1; k++)
    {
      fget();
      read_sixteenx(cx);
      font_info[k].hh.rh = tokanji(cx);
      fget();
      read_sixteen(cx);
      font_info[k].hh.lh = tonum(cx);
    }
  }

  for (k = char_base[f] + bc; k <= width_base[f] - 1; k++)
  {
    store_four_quarters(font_info[k].qqqq);

    if ((a >= nw) || (b / 16 >= nh) || (b % 16 >= nd) || (c / 4 >= ni))
      goto bad_tfm;

    switch (c % 4)
    {
      case lig_tag:
        if (d >= nl)
          goto bad_tfm;
        break;

      case ext_tag:
        if (d >= ne)
          goto bad_tfm;
        break;

      case list_tag:
        {
          check_byte_range(d);

          while (d < current_character_being_worked_on)
          {
            qw = char_info(f, d);
 
            if (char_tag(qw) != list_tag)
              goto not_found;

            d = rem_byte(qw);
          }

          if (d == current_character_being_worked_on)
            goto bad_tfm;
not_found:;
        }
        break;

      default:
        do_nothing();
        break;
    }
  }

  {
    {
      alpha = 16;

      while (z >= 8388608L)   /* 2^23 */
      {
        z = z / 2;
        alpha = alpha + alpha;
      }

      beta = (char) (256 / alpha);
      alpha = alpha * z;
    }

    for (k = width_base[f]; k <= lig_kern_base[f] - 1; k++)
      store_scaled(font_info[k].cint);

    if (font_info[width_base[f]].cint != 0)
      goto bad_tfm;

    if (font_info[height_base[f]].cint != 0)
      goto bad_tfm;

    if (font_info[depth_base[f]].cint != 0)
      goto bad_tfm;

    if (font_info[italic_base[f]].cint != 0)
      goto bad_tfm;
  }

  bch_label = 32767;     /* '77777 */
  bchar = 256;

  if (nl > 0)
  {
    for (k = lig_kern_base[f]; k <= kern_base[f] + kern_base_offset - 1; k++)
    {
      store_four_quarters(font_info[k].qqqq);

      if (a > 128)
      {
        if (256 * c + d >= nl)
          goto bad_tfm;

        if (a == 255)
          if (k == lig_kern_base[f])
            bchar = b;
      }
      else
      {
        if (b != bchar)
          check_existence(b);

        if (c < 128)
        {
          if (jfm_flag != dir_default)
          {
            if (d >= ne)
              goto bad_tfm;
          }
          else
           check_existence(d);
        }
        else if (256 * (c - 128) + d >= nk)
          goto bad_tfm;

        if (a < 128)
          if (k - lig_kern_base[f] + a + 1 >= nl)
            goto bad_tfm;
      }
    }

    if (a == 255)
      bch_label = 256 * c + d;
  }

  for (k = kern_base[f] + kern_base_offset; k <= exten_base[f] - 1; k++)
    store_scaled(font_info[k].cint);

  if (jfm_flag != dir_default)
  {
    for (k = exten_base[f]; k <= param_base[f] - 1; k++)
      store_scaled(font_info[k].cint);
  }
  else for (k = exten_base[f]; k <= param_base[f] - 1; k++)
  {
    store_four_quarters(font_info[k].qqqq);

    if (a != 0)
      check_existence(a);

    if (b != 0)
      check_existence(b);

    if (c != 0)
      check_existence(c);

    check_existence(d);
  }

  {
    for (k = 1; k <= np; k++)
      if (k == 1)
      {
        fget();
        sw = fbyte;

        if (sw > 127)
          sw = sw - 256;

        fget();
        sw = sw * 256 + fbyte;
        fget();
        sw = sw * 256 + fbyte;
        fget();
        font_info[param_base[f]].cint = (sw * 16) + (fbyte / 16);
      }
      else
        store_scaled(font_info[param_base[f] + k - 1].cint);

    if (feof(tfm_file))
      goto bad_tfm;

    for (k = np + 1; k <= 7; k++)
      font_info[param_base[f] + k - 1].cint = 0;
  }

  if (np >= 7)
    font_params[f] = np;
  else
    font_params[f] = 7;

  hyphen_char[f] = default_hyphen_char;
  skew_char[f] = default_skew_char;

  if (bch_label < nl)
    bchar_label[f] = bch_label + lig_kern_base[f];
  else
    bchar_label[f] = non_address;

  font_bchar[f] = bchar;
  font_false_bchar[f] = bchar;

  if (bchar <= ec)
    if (bchar >= bc)
    {
      qw = char_info(f, bchar);

      if (char_exists(qw))
        font_false_bchar[f] = non_char;
    }

  font_name[f] = nom;
  font_area[f] = aire;
  font_bc[f] = bc;
  font_ec[f] = ec;
  font_glue[f] = 0;
  adjust(ctype_base);
  adjust(char_base);
  adjust(width_base);
  adjust(lig_kern_base);
  adjust(kern_base);
  adjust(exten_base);
  decr(param_base[f]);
  fmem_ptr = fmem_ptr + lf;
  font_ptr = f;
  g = f;
  goto done;

bad_tfm:
  start_font_error_message();

  if (file_opened)
    prints(" not loadable: Bad metric (TFM) file");
  else
    prints(" not loadable: Metric (TFM) file not found");

  help5("I wasn't able to read the size data for this font,",
      "so I will ignore the font specification.",
      "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
      "You might try inserting a different font spec;",
      "e.g., type `I\\font<same font id>=<substitute font name>'.");
  error();

done:
  if (file_opened)
    b_close(tfm_file);

  return g;
}
/* sec 0581 */
void char_warning (internal_font_number f, eight_bits c)
{
  ASCII_code l;
  integer old_setting;

  if (tracing_lost_chars > 0)
  {
    old_setting = tracing_online;

    if (eTeX_ex && (tracing_lost_chars > 1))
      tracing_online = 1;

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
    tracing_online = old_setting;
  }
}
/* sec 0582 */
pointer new_character (internal_font_number f, eight_bits c)
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
  pdf_special_exec(cur_h, cur_v);
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

    do {
      get_token();
    } while (!(cur_tok == end_write_token));
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

            if (log_opened)
            {
              old_setting = selector;
              
              if (tracing_online <= 0)
                selector = log_only;
              else
                selector = term_and_log;

              print_nl("\\openout");
              print_int(j);
              prints(" = `");
              print_file_name(cur_name, cur_area, cur_ext);
              prints("'.");
              print_nl("");
              print_ln();
              selector = old_setting;
            }
          }
        }
      }
      break;

    case special_node:
      pdf_special_out(p); 
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
// scans a box specification and left brace
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
pointer hpack (pointer p, scaled w, small_number m)
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
  subtype(r) = min_quarterword;
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

  if (TeXXeT_en)
    put_LR(before);

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
              s = disp;
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
          do_nothing();
          break;

        case disp_node:
          {
            disp = disp_dimen(p);
            revdisp = disp;
          }
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
          x = x + width(p);
          break;

        case math_node:
          {
            x = x + width(p);

            if (TeXXeT_en)
              if (end_LR(p))
                if (info(LR_ptr) == end_LR_type(p))
                  pop_LR();
                else
                {
                  incr(LR_problems);
                  type(p) = kern_node;
                  subtype(p) = explicit;
                }
              else
                push_LR(p);
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

        default:
          do_nothing();
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
    set_glue_ratio_zero(glue_set(r));
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
      set_glue_ratio_zero(glue_set(r));
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
      glue_set(r) = ((-x) / ((double) total_shrink[o]));
    else
    {
      glue_sign(r) = normal;
      set_glue_ratio_zero(glue_set(r));
    }

    if ((total_shrink[o] < -x) && (o == normal) && (list_ptr(r) != 0))
    {
      last_badness = 1000000L;
      glue_set(r) = 1.0;

      if ((-x - total_shrink[normal] > hfuzz) || (hbadness < 100))
      {
        if ((overfull_rule > 0) && (-x - total_shrink[normal] > hfuzz))
        {
          while (link(q) != 0)
            q = link(q);
          
          link(q) = new_rule();
          width(link(q)) = overfull_rule;
        }
        
        print_ln();
        print_nl("Overfull \\hbox (");
        print_scaled(-x - total_shrink[normal]);
        prints("pt too wide");
        
        overfull_hbox++;
        goto common_ending;
      }
    }
    else if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(-x, total_shrink[normal]);

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

  if (TeXXeT_en)
  {
    if (info(LR_ptr) != before)
    {
      while (link(q) != null)
        q = link(q);

      do {
        temp_ptr = q;
        q = new_math(0, info(LR_ptr));
        link(temp_ptr) = q;
        LR_problems = LR_problems + 10000;
        pop_LR();
      } while (!(info(LR_ptr) == before));
    }

    if (LR_problems > 0)
    {
      report_LR_problems();
      goto common_ending;
    }

    pop_LR();

    if (LR_ptr != null)
      confusion("LR1");
  }

  return r;
}
/* sec 0668 */
pointer vpackage (pointer p, scaled h, small_number m, scaled l)
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
        do_nothing();
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
        do_nothing();
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
    set_glue_ratio_zero(glue_set(r));
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
      set_glue_ratio_zero(glue_set(r));
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
      glue_set(r) = (-x) / ((double) total_shrink[o]);
    else
    {
      glue_sign(r) = normal;
      glue_set(r) = 0.0;
    }

    if ((total_shrink[o] < -x) && (o == 0) && (list_ptr(r) != 0))
    {
      last_badness = 1000000L;
      set_glue_ratio_one(glue_set(r));

      if ((-x - total_shrink[0] > vfuzz) || (vbadness < 100))
      {
        print_ln();
        print_nl("Overfull \\vbox (");
        print_scaled(-x - total_shrink[0]);
        prints("pt too high");

        overfull_vbox++;

        goto common_ending;
      }
    }
    else if (o == 0)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(-x, total_shrink[normal]);

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
// create a style node
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
// create a choice node
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
// the reader will kindly forgive this
void show_info (void)
{
  show_node_list(info(temp_ptr));
}
/* sec 0704 */
// construct the bar for a fraction
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

      do {
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
      } while (!(z < 16));
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
/* sec 0715 */
pointer rebox (pointer b, scaled w)
{
  pointer p;
  internal_font_number f;
  scaled v;

  if ((width(b) != w) && (list_ptr(b) != 0))
  {
    if (type(b) != hlist_node)
      b = hpack(b, 0, 1);

    p = list_ptr(b);

    if (is_char_node(p))
    {
      if (font_dir[font(p)] != dir_default)
      {
        if (link(link(p)) == null)
        {
          f = font(p);
          v = char_width(f, char_info(f, character(p)));

          if (v != width(b))
            link(link(p)) = new_kern(width(b) - v);
        }
      }
      else if (link(p) == null)
      {
        f = font(p);
        v = char_width(f, char_info(f, character(p)));
        
        if (v != width(b))
          link(p) = new_kern(width(b) - v);
      }
    }

    delete_glue_ref(space_ptr(b));
    delete_glue_ref(xspace_ptr(b));
    free_node(b, box_node_size);
    b = new_glue(ss_glue);
    link(b) = p;

    while (link(p) != 0)
      p = link(p);

    link(p) = new_glue(ss_glue);
    return hpack(b, w, exactly);
  }
  else
  {
    width(b) = w;
    return b;
  }
}
/* sec 0716 */
pointer math_glue (pointer g, scaled m)
{
  pointer p;
  integer n;
  scaled f;

  n = x_over_n(m, 65536);
  f = tex_remainder;

  if (f < 0)
  {
    decr(n);
    f = f + 65536;
  }

  p = get_node(glue_spec_size);
  width(p) = mu_mult(width(g));
  stretch_order(p) = stretch_order(g);

  if (stretch_order(p) == normal)
    stretch(p) = mu_mult(stretch(g));
  else
    stretch(p) = stretch(g);

  shrink_order(p) = shrink_order(g);

  if (shrink_order(p) == normal)
    shrink(p) = mu_mult(shrink(g));
  else
    shrink(p) = shrink(g);

  return p;
}
/* sec 0717 */
void math_kern (pointer p, scaled m)
{
  integer n;
  scaled f;

  if (subtype(p) == mu_glue)
  {
    n = x_over_n(m, 65536);
    f = tex_remainder;

    if (f < 0)
    {
      decr(n);
      f = f + 65536;
    }

    width(p) = mu_mult(width(p));
    subtype(p) = explicit;
  }
}
/* sec 0718 */
void flush_math (void)
{
  flush_node_list(link(head));
  flush_node_list(incompleat_noad);
  link(head) = 0;
  tail = head;
  incompleat_noad = 0;
}
/* sec 0720 */
pointer clean_box (pointer p, small_number s, halfword jc)
{
  pointer q;
  small_number save_style;
  pointer x;
  pointer r;

  switch (math_type(p))
  {
    case math_char:
      {
        cur_mlist = new_noad();
        mem[nucleus(cur_mlist)] = mem[p];
      }
      break;

    case math_jchar:
      {
        cur_mlist = new_noad();
        mem[nucleus(cur_mlist)] = mem[p];
        math_kcode(cur_mlist) = jc;
      }
      break;

    case sub_box:
      {
        q = info(p);
        goto found;
      }
      break;

    case sub_mlist:
      cur_mlist = info(p);
      break;

    default:
      {
        q = new_null_box();
        goto found;
      }
      break;
  }

  save_style = cur_style;
  cur_style = s;
  mlist_penalties = false;
  mlist_to_hlist();
  q = link(temp_head);
  cur_style = save_style;

  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

found:
  if (is_char_node(q) || (q == 0))
    x = hpack(q, 0, 1);
  else if ((link(q) == 0) && (type(q) <= dir_node) && (shift_amount(q) == 0))
    x = q;
  else
    x = hpack(q, 0, 1);

  q = list_ptr(x);

  if (is_char_node(q))
  {
    if (font_dir[font(q)] != dir_default)
      q = link(q);

    r = link(q);

    if (r != 0)
      if (link(r) == 0)
        if (!is_char_node(r))
          if (type(r) == kern_node)
          {
            free_node(r, small_node_size);
            link(q) = 0;
          }
  }

  return x;
}
/* sec 0722 */
// unpack the |math_char| field |a|
void fetch (pointer a)
{
  cur_c = character(a);
  cur_f = fam_fnt(fam(a) + cur_size);

  if (cur_f == null_font)
  {
    print_err("");
    print_size(cur_size);
    print_char(' ');
    print_int(fam(a));
    prints(" is undefined (character ");
    print(cur_c);
    print_char(')');
    help4("Somewhere in the math formula just ended, you used the",
        "stated character from an undefined font family. For example,",
        "plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,",
        "and I'll try to forget that I needed that character.");
    error();
    cur_i = null_character;
    math_type(a) = 0;
  }
  else
  {
    if (font_dir[cur_f] != dir_default)
      cur_c = get_jfm_pos(KANJI(math_kcode_nucleus(a)), cur_f);

    if ((cur_c >= font_bc[cur_f]) && (cur_c <= font_ec[cur_f]))
      cur_i = char_info(cur_f, cur_c);
    else
      cur_i = null_character;

    if (!char_exists(cur_i))
    {
      char_warning(cur_f, cur_c);
      math_type(a) = 0;
    }
  }
}
/* sec 0734 */
void make_over (pointer q)
{
  info(nucleus(q)) = overbar(clean_box(nucleus(q), cramped_style(cur_style), math_kcode(q)),
      3 * default_rule_thickness, default_rule_thickness);
  math_type(nucleus(q)) = sub_box;
}
/* sec 0735 */
void make_under (pointer q)
{
  pointer p, x, y;
  scaled delta;

  x = clean_box(nucleus(q), cur_style, math_kcode(q));
  p = new_kern(3 * default_rule_thickness);
  link(x) = p;
  link(p) = fraction_rule(default_rule_thickness);
  y = vpackage(x, 0, 1, max_dimen);
  delta = height(y) + depth(y) + default_rule_thickness;
  height(y) = height(x);
  depth(y) = delta - height(y);
  info(nucleus(q)) = y;
  math_type(nucleus(q)) = sub_box;
}
/* sec 0736 */
void make_vcenter (pointer q)
{ 
  pointer v;
  scaled delta;

  v = info(nucleus(q));

  if (type(v) == dir_node)
  {
    if (type(list_ptr(v)) != vlist_node)
    {
      confusion("dircenter");
      return;
    }
  }
  else
  {
    if (type(v) != vlist_node)
    {
      confusion("vcenter");
      return;
    }
  }

  delta = height(v) + depth(v);
  height(v) = axis_height(cur_size) + half(delta);
  depth(v) = delta - height(v);
}
/* sec 0737 */
void make_radical (pointer q)
{
  pointer x, y;
  scaled delta, clr;

  x = clean_box(nucleus(q), cramped_style(cur_style), math_kcode(q));

  if (cur_style < text_style)
    clr = default_rule_thickness + (abs(math_x_height(cur_size)) / 4);
  else
  {
    clr = default_rule_thickness;
    clr = clr + (abs(clr) / 4);
  }

  y = var_delimiter(left_delimiter(q), cur_size, height(x) + depth(x) + clr + default_rule_thickness);
  delta = depth(y) - (height(x) + depth(x) + clr);

  if (delta > 0)
    clr = clr + half(delta);

  shift_amount(y) = -(height(x) + clr);
  link(y) = overbar(x, clr, height(y));
  info(nucleus(q)) = hpack(y, 0, 1);
  math_type(nucleus(q)) = sub_box;
}
/* sec 0738 */
void make_math_accent (pointer q)
{
  pointer p, x, y;
  integer a;
  quarterword c;
  internal_font_number f;
  four_quarters i;
  scaled s;
  scaled h;
  scaled delta;
  scaled w;

  fetch(accent_chr(q));

  if (char_exists(cur_i))
  {
    i = cur_i;
    c = cur_c;
    f = cur_f;
    s = 0;

    if (math_type(nucleus(q)) == math_char)
    {
      fetch(nucleus(q));

      if (char_tag(cur_i) == lig_tag)
      {
        a = lig_kern_start(cur_f, cur_i);
        cur_i = font_info[a].qqqq;

        if (skip_byte(cur_i) > stop_flag)
        {
          a = lig_kern_restart(cur_f, cur_i);
          cur_i = font_info[a].qqqq;
        }

        while (true)
        {
          if (next_char(cur_i) == skew_char[cur_f])
          {
            if (op_byte(cur_i) >= kern_flag)
              if (skip_byte(cur_i) <= stop_flag)
                s = char_kern(cur_f, cur_i);

            goto done1;
          }

          if (skip_byte(cur_i) >= stop_flag)
            goto done1;

          a = a + skip_byte(cur_i) + 1;
          cur_i = font_info[a].qqqq;
        }
      }
    }

done1:
    x = clean_box(nucleus(q), cramped_style(cur_style), math_kcode(q));
    w = width(x);
    h = height(x);

    while (true)
    {
      if (char_tag(i) != list_tag)
        goto done;

      y = rem_byte(i);
      i = char_info(f, y);

      if (!char_exists(i))
        goto done;

      if (char_width(f, i) > w)
        goto done;

      c = y;
    }

done:
    if (h < x_height(f))
      delta = h;
    else
      delta = x_height(f);

    if ((math_type(supscr(q)) != 0) || (math_type(subscr(q)) != 0))
      if (math_type(nucleus(q)) == math_char)
      {
        flush_node_list(x);
        x = new_noad();
        mem[nucleus(x)] = mem[nucleus(q)];
        mem[supscr(x)] = mem[supscr(q)];
        mem[subscr(x)] = mem[subscr(q)];
        mem[supscr(q)].hh = empty_field;
        mem[subscr(q)].hh = empty_field;
        math_type(nucleus(q)) = sub_mlist;
        info(nucleus(q)) = x;
        x = clean_box(nucleus(q), cur_style, math_kcode(q));
        delta = delta + height(x) - h;
        h = height(x);
      }

    y = char_box(f, c);
    shift_amount(y) = s + half(w - width(y));
    width(y) = 0;
    p = new_kern(-delta);
    link(p) = x;
    link(y) = p;
    y = vpackage(y, 0, 1, max_dimen);
    width(y) = width(x);

    if (height(y) < h)
    {
      p = new_kern(h - height(y));
      link(p) = list_ptr(y);
      list_ptr(y) = p;
      height(y) = h;
    }

    info(nucleus(q)) = y;
    math_type(nucleus(q)) = sub_box;
  }
}
/* sec 0743 */
void make_fraction (pointer q)
{
  pointer p, v, x, y, z;
  scaled delta, delta1, delta2, shift_up, shift_down, clr;
  
  if (thickness(q) == default_code)
    thickness(q) = default_rule_thickness;

  x = clean_box(numerator(q), num_style(cur_style), math_kcode(q));
  z = clean_box(denominator(q), denom_style(cur_style), math_kcode(q));

  if (width(x) < width(z))
    x = rebox(x, width(z));
  else
    z = rebox(z, width(x));

  if (cur_style < text_style)
  {
    shift_up = num1(cur_size);
    shift_down = denom1(cur_size);
  }
  else
  {
    shift_down = denom2(cur_size);

    if (thickness(q) != 0)
      shift_up = num2(cur_size);
    else
      shift_up = num3(cur_size);
  }

  if (thickness(q) == 0)
  {
    if (cur_style < text_style)
      clr = 7 * default_rule_thickness;
    else
      clr = 3 * default_rule_thickness;

    delta = half(clr - ((shift_up - depth(x)) - (height(z) - shift_down)));

    if (delta > 0)
    {
      shift_up = shift_up + delta;
      shift_down = shift_down + delta;
    }
  }
  else
  {
    if (cur_style < text_style)
      clr = 3 * thickness(q);
    else
      clr = thickness(q);

    delta = half(thickness(q));
    delta1 = clr - ((shift_up - depth(x)) - (axis_height(cur_size) + delta));
    delta2 = clr - ((axis_height(cur_size) - delta) - (height(z) - shift_down));

    if (delta1 > 0)
      shift_up = shift_up + delta1;

    if (delta2 > 0)
      shift_down = shift_down + delta2;
  }

  v = new_null_box();
  type(v) = vlist_node;
  height(v) = shift_up + height(x);
  depth(v) = depth(z) + shift_down;
  width(v) = width(x);

  if (thickness(q) == 0)
  {
    p = new_kern((shift_up - depth(x)) - (height(z) - shift_down));
    link(p) = z;
  }
  else
  {
    y = fraction_rule(thickness(q));
    p = new_kern((axis_height(cur_size) - delta) - (height(z) - shift_down));
    link(y) = p;
    link(p) = z;
    p = new_kern((shift_up - depth(x)) - (axis_height(cur_size) + delta));
    link(p) = y;
  }

  link(x) = p;
  list_ptr(v) = x;

  if (cur_style < text_style)
    delta = delim1(cur_size);
  else
    delta = delim2(cur_size);

  x = var_delimiter(left_delimiter(q), cur_size, delta);
  link(x) = v;
  z = var_delimiter(right_delimiter(q), cur_size, delta);
  link(v) = z;
  new_hlist(q) = hpack(x, 0, 1);
}
/* sec 0752 */
void make_ord (pointer q)
{
  integer a;
  pointer gp, gq, p, r;
  halfword rr;

restart:
  if ((math_type(subscr(q)) == 0) && (math_type(supscr(q)) == 0) &&
    ((math_type(nucleus(q)) == math_char) || (math_type(nucleus(q)) == math_jchar)))
  {
    p = link(q);

    if (p != 0)
      if ((type(p) >= ord_noad) && (type(p) <= punct_noad))
        if (fam(nucleus(p)) == fam(nucleus(q)))
          if (math_type(nucleus(p)) == math_char)
          {
            math_type(nucleus(q)) = math_text_char;
            fetch(nucleus(q));
            
            if (char_tag(cur_i) == lig_tag)
            {
              a = lig_kern_start(cur_f, cur_i);
              cur_c = character(nucleus(p));
              cur_i = font_info[a].qqqq;
              
              if (skip_byte(cur_i) > stop_flag)
              {
                a = lig_kern_restart(cur_f, cur_i);
                cur_i = font_info[a].qqqq;
              }
              
              while (true)
              {
                if (next_char(cur_i) == cur_c)
                  if (skip_byte(cur_i) <= stop_flag)
                    if (op_byte(cur_i) >= kern_flag)
                    {
                      p = new_kern(char_kern(cur_f, cur_i));
                      link(p) = link(q);
                      link(q) = p;
                      return;
                    }
                    else
                    {
                      check_interrupt();
                      
                      switch (op_byte(cur_i))
                      {
                        case 1:
                        case 5:
                          character(nucleus(q)) = rem_byte(cur_i);
                          break;
                        
                        case 2:
                        case 6:
                          character(nucleus(p)) = rem_byte(cur_i);
                          break;

                        case 3:
                        case 7:
                        case 11:
                          {
                            r = new_noad();
                            character(nucleus(r)) = rem_byte(cur_i);
                            fam(nucleus(r)) = fam(nucleus(q));
                            link(q) = r;
                            link(r) = p;
                          
                            if (op_byte(cur_i) < 11)
                              math_type(nucleus(r)) = math_char;
                            else
                              math_type(nucleus(r)) = math_text_char;
                          }
                          break;
                          
                        default:
                          {
                            link(q) = link(p);
                            character(nucleus(q)) = rem_byte(cur_i);
                            mem[subscr(q)] = mem[subscr(p)];
                            mem[supscr(q)] = mem[supscr(p)];
                            free_node(p, noad_size);
                          }
                          break;
                      }
                      
                      if (op_byte(cur_i) > 3)
                        return;
                      
                      math_type(nucleus(q)) = math_char;
                      goto restart;
                    }

                if (skip_byte(cur_i) >= stop_flag)
                  return;

                a = a + skip_byte(cur_i) + 1;
                cur_i = font_info[a].qqqq;
              }
            }
          }
          else if (math_type(nucleus(p)) == math_jchar)
          {
            math_type(nucleus(q)) = math_text_jchar;
            fetch(nucleus(p));
            a = cur_c;
            fetch(nucleus(q));

            if (char_tag(cur_i) == gk_tag)
            {
              cur_c = a;
              a = glue_kern_start(cur_f, cur_i);

              do {
                cur_i = font_info[a].qqqq;

                if (next_char(cur_i) == cur_c)
                  if (op_byte(cur_i) < kern_flag)
                  {
                    gp = font_glue[cur_f];
                    rr = rem_byte(cur_i);
                      
                    if (gp != null)
                    {
                      while ((type(gp) != rr) && (link(gp) != null))
                      {
                        gp = link(gp);
                      }

                      gq = glue_ptr(gp);
                    }
                    else
                    {
                      gp = get_node(small_node_size);
                      font_glue[cur_f] = gp;
                      gq = null;
                    }

                    if (gq == null)
                    {
                      type(gp) = rr;
                      gq = new_spec(zero_glue);
                      glue_ptr(gp) = gq;
                      a = exten_base[cur_f] + (((rr)) * 3);
                      width(gq) = font_info[a].cint;
                      stretch(gq) = font_info[a + 1].cint;
                      shrink(gq) = font_info[a + 2].cint;
                      add_glue_ref(gq);
                      link(gp) = get_node(small_node_size);
                      gp = link(gp);
                      glue_ptr(gp) = null;
                      link(gp) = null;
                    }

                    p = new_glue(gq);
                    link(p) = link(q);
                    link(q) = p;
                    return;
                  }
                  else
                  {
                    p = new_kern(char_kern(cur_f, cur_i));
                    link(p) = link(q);
                    link(q) = p;
                    return;
                  }

                incr(a);
              } while (!(skip_byte(cur_i) >= stop_flag));
            }
          }
      }
}
/* sec 0762 */
small_number make_left_right (pointer q, small_number style, scaled max_d, scaled max_h)
{
  scaled delta, delta1, delta2;

  cur_style = style;

  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

  delta2 = max_d + axis_height(cur_size);
  delta1 = max_h + max_d - delta2;

  if (delta2 > delta1)
    delta1 = delta2;

  delta = (delta1 / 500) * delimiter_factor;
  delta2 = delta1 + delta1 - delimiter_shortfall;

  if (delta < delta2)
    delta = delta2;

  new_hlist(q) = var_delimiter(delimiter(q), cur_size, delta);

  return type(q) - (left_noad - open_noad);
}
/* sec 0726 */
void mlist_to_hlist (void)
{
  pointer mlist;
  boolean penalties;
  small_number style;
  pointer u;
  small_number save_style;
  pointer q;
  pointer r;
  /* small_number r_type; */
  int r_type;
  /* small_number t; */
  int t;
  pointer p, x, y, z;
  integer pen;
  small_number s;
  scaled max_h, max_d;
  scaled delta;

  mlist = cur_mlist;
  penalties = mlist_penalties;
  style = cur_style;
  q = mlist;
  r = 0;
  r_type = op_noad;
  max_h = 0;
  max_d = 0;

  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

  while (q != 0)
  {
reswitch:
    delta = 0;

    switch (type(q))
    {
      case bin_noad:
        switch (r_type)
        {
          case bin_noad:
          case op_noad:
          case rel_noad:
          case open_noad:
          case punct_noad:
          case left_noad:
            {
              type(q) = ord_noad;
              goto reswitch;
            }
            break;

          default:
            do_nothing();
            break;
        }
        break;

      case rel_noad:
      case close_noad:
      case punct_noad:
      case right_noad:
        {
          if (r_type == bin_noad)
            type(r) = ord_noad;

          if (type(q) == right_noad)
            goto done_with_noad;
        }
        break;

      case left_noad:
        goto done_with_noad;
        break;

      case fraction_noad:
        {
          make_fraction(q);
          goto check_dimensions;
        }
        break;

      case op_noad:
        {
          delta = make_op(q);

          if (subtype(q) == limits)
            goto check_dimensions;
        }
        break;

      case ord_noad:
        make_ord(q);
        break;

      case open_noad:
      case inner_noad:
        do_nothing();
        break;

      case radical_noad:
        make_radical(q);
        break;

      case over_noad:
        make_over(q);
        break;

      case under_noad:
        make_under(q);
        break;

      case accent_noad:
        make_math_accent(q);
        break;

      case vcenter_noad:
        make_vcenter(q);
        break;

      case style_node:
        {
          cur_style = subtype(q);

          {
            if (cur_style < script_style)
              cur_size = text_size;
            else
              cur_size = 16 * ((cur_style - text_style) / 2);

            cur_mu = x_over_n(math_quad(cur_size), 18);
          }

          goto done_with_node;
        }
        break;

      case choice_node:
        {
          switch (cur_style / 2)
          {
            case 0:
              choose_mlist(display_mlist);
              break;

            case 1:
              choose_mlist(text_mlist);
              break;

            case 2:
              choose_mlist(script_mlist);
              break;

            case 3:
              choose_mlist(script_script_mlist);
              break;
          }

          flush_node_list(display_mlist(q));
          flush_node_list(text_mlist(q));
          flush_node_list(script_mlist(q));
          flush_node_list(script_script_mlist(q));
          type(q) = style_node;
          subtype(q) = cur_style;
          width(q) = 0;
          depth(q) = 0;

          if (p != 0)
          {
            z = link(q);
            link(q) = p;

            while (link(p) != 0)
              p = link(p);

            link(p) = z;
          }

          goto done_with_node;
        }
        break;

      case ins_node:
      case mark_node:
      case adjust_node:
      case whatsit_node:
      case penalty_node:
      case disc_node:
        goto done_with_node;
        break;

      case rule_node:
        {
          if (height(q) > max_h)
            max_h = height(q);

          if (depth(q) > max_d)
            max_d = depth(q);

          goto done_with_node;
        }
        break;

      case glue_node:
        {
          if (subtype(q) == mu_glue)
          {
            x = glue_ptr(q);
            y = math_glue(x, cur_mu);
            delete_glue_ref(x);
            glue_ptr(q) = y;
            subtype(q) = normal;
          }
          else if ((cur_size != text_size) && (subtype(q) == cond_math_glue))
          {
            p = link(q);

            if (p != 0)
              if ((type(q) == glue_node) || (type(p) == kern_node))
              {
                link(q) = link(p);
                link(p) = 0;
                flush_node_list(p);
              }
          }

          goto done_with_node;
        }
        break;

      case kern_node:
        {
          math_kern(q, cur_mu);
          goto done_with_node;
        }
        break;

      case disp_node:
        goto done_with_node;
        break;

      default:
        {
          confusion("mlist1");
          return;
        }
        break;
    }

    switch (math_type(nucleus(q)))
    {
      case math_char:
      case math_text_char:
      case math_jchar:
      case math_text_jchar:
        {
          fetch(nucleus(q));

          if (char_exists(cur_i))
          {
            delta = char_italic(cur_f, cur_i);
            p = new_character(cur_f, cur_c);
            u = p;

            if (font_dir[cur_f] != dir_default)
            {
              link(u) = get_avail();
              u = link(u);
              info(u) = math_kcode(q);
            }

            if (((math_type(nucleus(q)) == math_text_char) ||
              (math_type(nucleus(q)) == math_text_jchar))
              && (space(cur_f) != 0))
              delta = 0;

            if ((math_type(subscr(q)) == 0) && (delta != 0))
            {
              link(u) = new_kern(delta);
              delta = 0;
            }
          }
          else
            p = 0;
        }
        break;

      case 0:
        p = 0;
        break;

      case sub_box:
        p = info(nucleus(q));
        break;

      case sub_mlist:
        {
          cur_mlist = info(nucleus(q));
          save_style = cur_style;
          mlist_penalties = false;
          mlist_to_hlist();
          cur_style = save_style;

          {
            if (cur_style < script_style)
              cur_size = text_size;
            else
              cur_size = 16 * ((cur_style - text_style) / 2);

            cur_mu = x_over_n(math_quad(cur_size), 18);
          }

          p = hpack(link(temp_head), 0, 1);
        }
        break;

      default:
        {
          confusion("mlist2");
          return;
        }
        break;
    }
  
    new_hlist(q) = p;

    if ((math_type(subscr(q)) == 0) && (math_type(supscr(q)) == 0))
      goto check_dimensions;

    make_scripts(q, delta);

check_dimensions:
    z = hpack(new_hlist(q), 0, 1);

    if (height(z) > max_h)
      max_h = height(z);

    if (depth(z) > max_d)
      max_d = depth(z);

    delete_glue_ref(space_ptr(z));
    delete_glue_ref(xspace_ptr(z));
    free_node(z, box_node_size);

done_with_noad:
    r = q;
    r_type = type(r);

    if (r_type == right_noad)
    {
      r_type = left_noad;
      cur_style = style;

      {
        if (cur_style < script_style)
          cur_size = text_size;
        else
          cur_size = 16 * ((cur_style - text_style) / 2);

        cur_mu = x_over_n(math_quad(cur_size), 18);
      }
    }

done_with_node:
    q = link(q);
  }

  if (r_type == bin_noad)
    type(r) = ord_noad;

  p = temp_head;
  link(p) = 0;
  q = mlist;
  r_type = 0;
  cur_style = style;

  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 *((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

  while (q != 0)
  {
    t = ord_noad;
    s = noad_size;
    pen = inf_penalty;

    switch (type(q))
    {
      case op_noad:
      case open_noad:
      case close_noad:
      case punct_noad:
      case inner_noad:
        t = type(q);
        break;

      case bin_noad:
        {
          t = bin_noad;
          pen = bin_op_penalty;
        }
        break;

      case rel_noad:
        {
          t = rel_noad;
          pen = rel_penalty;
        }
        break;

      case ord_noad:
      case vcenter_noad:
      case over_noad:
      case under_noad:
        do_nothing();
        break;

      case radical_noad:
        s = radical_noad_size;
        break;

      case accent_noad:
        s = accent_noad_size;
        break;

      case fraction_noad:
        {
          t = inner_noad;
          s = fraction_noad_size;
        }
        break;

      case left_noad:
      case right_noad:
        t = make_left_right(q, style, max_d, max_h);
        break;

      case style_node:
        {
          cur_style = subtype(q);
          s = style_node_size;

          {
            if (cur_style < script_style)
              cur_size = text_size;
            else
              cur_size = 16 *((cur_style - text_style) / 2);

            cur_mu = x_over_n(math_quad(cur_size), 18);
          }

          goto delete_q;
        }
        break;

      case whatsit_node:
      case penalty_node:
      case rule_node:
      case disc_node:
      case adjust_node:
      case ins_node:
      case mark_node:
      case glue_node:
      case kern_node:
        {
          link(p) = q;
          p = q;
          q = link(q);
          link(p) = 0;
          goto done;
        }
        break;

      case disp_node:
        {
          link(p) = q;
          p = q;
          q = link(q);
          link(p) = null;
          goto done;
        }
        break;

      default:
        {
          confusion("mlist3");
          return;
        }
        break;
    }

    if (r_type > 0)
    {
      switch (str_pool[r_type * 8 + t + magic_offset])
      {
        case '0':
          x = 0;
          break;

        case '1':
          if (cur_style < script_style)
            x = thin_mu_skip_code;
          else
            x = 0;
          break;

        case '2':
          x = thin_mu_skip_code;
          break;

        case '3':
          if (cur_style < script_style)
            x = med_mu_skip_code;
          else
            x = 0;
          break;

        case '4':
          if (cur_style < script_style)
            x = thick_mu_skip_code;
          else
            x = 0;
          break;

        default:
          {
            confusion("mlist4");
            return;
          }
          break;
      }

      if (x != 0)
      {
        y = math_glue(glue_par(x), cur_mu);
        z = new_glue(y);
        glue_ref_count(y) = 0;
        link(p) = z;
        p = z;
        subtype(z) = x + 1;
      }
    }

    if (new_hlist(q) != 0)
    {
      link(p) = new_hlist(q);

      do {
        p = link(p);
      } while (!(link(p) == 0));
    }

    if (penalties)
      if (link(q) != 0)
        if (pen < inf_penalty)
        {
          r_type = type(link(q));

          if (r_type != penalty_node)
            if (r_type != rel_noad)
            {
              z = new_penalty(pen);
              link(p) = z;
              p = z;
            }
        }

    if (type(q) == right_noad)
      t = open_noad;

    r_type = t;

delete_q:
    r = q;
    q = link(q);
    free_node(r, s);
done:;
  }
  
  p = new_null_box();
  link(p) = link(temp_head);
  adjust_hlist(p, false);
  link(temp_head) = link(p);
  delete_glue_ref(space_ptr(p));
  delete_glue_ref(xspace_ptr(p));
  free_node(p, box_node_size);
}
/* sec 0772 */
void push_alignment (void)
{
  pointer p;

  p = get_node(align_stack_node_size);
  link(p) = align_ptr;
  info(p) = cur_align;
  llink(p) = preamble;
  rlink(p) = cur_span;
  mem[p + 2].cint = cur_loop;
  mem[p + 3].cint = align_state;
  info(p + 4) = cur_head;
  link(p + 4) = cur_tail;
  align_ptr = p;
  cur_head = get_avail();
}
/* sec 0772 */
void pop_alignment (void)
{
  pointer p;

  free_avail(cur_head);
  p = align_ptr;
  cur_tail = link(p + 4);
  cur_head = info(p + 4);
  align_state = mem[p + 3].cint;
  cur_loop = mem[p + 2].cint;
  cur_span = rlink(p);
  preamble = llink(p);
  cur_align = info(p);
  align_ptr = link(p);
  free_node(p, align_stack_node_size);
}
/* sec 0782 */
void get_preamble_token (void)
{
restart:
  get_token();

  while ((cur_chr == span_code) && (cur_cmd == tab_mark))
  {
    get_token();

    if (cur_cmd > max_command)
    {
      expand();
      get_token();
    }
  }

  if (cur_cmd == endv)
  {
    fatal_error("(interwoven alignment preambles are not allowed)");
    return;
  }

  if ((cur_cmd == assign_glue) && (cur_chr == glue_base + tab_skip_code))
  {
    scan_optional_equals();
    scan_glue(glue_val);

    if (global_defs > 0)
      geq_define(glue_base + tab_skip_code, glue_ref, cur_val);
    else
      eq_define(glue_base + tab_skip_code, glue_ref, cur_val);

    goto restart;
  }
}
/* sec 0774 */
void init_align (void)
{
  pointer save_cs_ptr;
  pointer p;

  save_cs_ptr = cur_cs;
  push_alignment();
  align_state = -1000000L;

  if ((mode == mmode) && ((tail != head) || (incompleat_noad != 0)))
  {
    print_err("Improper ");
    print_esc("halign");
    prints(" inside $$'s");
    help3("Displays can use special alignments (like \\eqalignno)",
        "only if nothing but the alignment itself is between $$'s.",
        "So I've deleted the formulas that preceded this alignment.");
    error();
    flush_math();
  }

  push_nest();

  if (mode == mmode)
  {
    mode = -vmode;
    prev_depth = nest[nest_ptr - 2].aux_field.cint;
  }
  else if (mode > 0)
    negate(mode);

  scan_spec(align_group, false);
  preamble = 0;
  cur_align = align_head;
  cur_loop = 0;
  scanner_status = aligning;
  warning_index = save_cs_ptr;
  align_state = -1000000L;

  while (true)
  {
    link(cur_align) = new_param_glue(tab_skip_code);
    cur_align = link(cur_align);

    if (cur_cmd == car_ret)
      goto done;

    p = hold_head;
    link(p) = 0;

    while (true)
    {
      get_preamble_token();

      if (cur_cmd == mac_param)
        goto done1;

      if ((cur_cmd <= car_ret) && (cur_cmd >= tab_mark) && (align_state == -1000000L))
        if ((p == hold_head) && (cur_loop == 0) && (cur_cmd == tab_mark))
          cur_loop = cur_align;
        else
        {
          print_err("Missing # inserted in alignment preamble");
          help3("There should be exactly one # between &'s, when an",
              "\\halign or \\valign is being set up. In this case you had",
              "none, so I've put one in; maybe that will work.");
          back_error();
          goto done1;
        }
      else if ((cur_cmd != spacer) || (p != hold_head))
      {
        link(p) = get_avail();
        p = link(p);
        info(p) = cur_tok;
      }
    }

done1:
    link(cur_align) = new_null_box();
    cur_align = link(cur_align);
    info(cur_align) = end_span;
    width(cur_align) = null_flag;
    u_part(cur_align) = link(hold_head);
    p = hold_head;
    link(p) = 0;

    while (true)
    {
continu:
      get_preamble_token();

      if ((cur_cmd <= car_ret) && (cur_cmd >= tab_mark) && (align_state == -1000000L))
        goto done2;

      if (cur_cmd == mac_param)
      {
        print_err("Only one # is allowed per tab");
        help3("There should be exactly one # between &'s, when an",
            "\\halign or \\valign is being set up. In this case you had",
            "more than one, so I'm ignoring all but the first.");
        error();
        goto continu;
      }

      link(p) = get_avail();
      p = link(p);
      info(p) = cur_tok;
    }

done2:
    link(p) = get_avail();
    p = link(p);
    info(p) = end_template_token;
    v_part(cur_align) = link(hold_head);
  }

done:
  scanner_status = normal;
  new_save_level(align_group);

  if (every_cr != 0)
    begin_token_list(every_cr, every_cr_text);

  align_peek();
}
/* sec 0787 */
void init_span (pointer p)
{
  push_nest();

  if (mode == -hmode)
    space_factor = 1000;
  else
  {
    prev_depth = ignore_depth;
    normal_paragraph();
  }

  cur_span = p;
}
/* sec 0786 */
void init_row (void)
{
  push_nest();
  mode = (-hmode - vmode) - mode;

  if (mode == -hmode)
    space_factor = 0;
  else
    prev_depth = 0;

  tail_append(new_glue(glue_ptr(preamble)));
  subtype(tail) = tab_skip_code + 1;
  cur_align = link(preamble);
  cur_tail = cur_head;
  init_span(cur_align);
}
/* sec 0788 */
void init_col (void)
{
  extra_info(cur_align) = cur_cmd;

  if (cur_cmd == omit)
    align_state = 0;
  else
  {
    back_input();
    begin_token_list(u_part(cur_align), u_template);
  }
}
/* sec 0799 */
void fin_row (void)
{
  pointer p;

  if (mode == -hmode)
  {
    adjust_hlist(head, false);
    delete_glue_ref(cur_kanji_skip);
    delete_glue_ref(cur_xkanji_skip);
    cur_kanji_skip = space_ptr(head);
    cur_xkanji_skip = xspace_ptr(head);
    add_glue_ref(cur_kanji_skip);
    add_glue_ref(cur_xkanji_skip);
    p = hpack(link(head), 0, 1);
    pop_nest();
    append_to_vlist(p);

    if (cur_head != cur_tail)
    {
      link(tail) = link(cur_head);
      tail = cur_tail;
    }
  }
  else
  {
    p = vpackage(link(head), 0, 1, max_dimen);
    pop_nest();
    link(tail) = p;
    tail = p;
    space_factor = 1000;
  }

  type(p) = unset_node;
  glue_stretch(p) = 0;

  if (every_cr != 0)
    begin_token_list(every_cr, every_cr_text);

  align_peek();
}
/* sec 0800 */
void fin_align (void)
{
  pointer p, q, r, s, u, v, z;
  scaled t, w;
  scaled o;
  halfword n;
  scaled rule_save;
  memory_word aux_save;

  if (cur_group != align_group)
  {
    confusion("align1");
    return;
  }

  unsave();

  if (cur_group != align_group)
  {
    confusion("align0");
    return;
  }

  unsave();

  if (nest[nest_ptr - 1].mode_field == mmode)
    o = display_indent;
  else
    o = 0;

  q = link(preamble);

  do {
    flush_list(u_part(q));
    flush_list(v_part(q));
    p = link(link(q));

    if (width(q) == null_flag)
    {
      width(q) = 0;
      r = link(q);
      s = glue_ptr(r);

      if (s != zero_glue)
      {
        add_glue_ref(zero_glue);
        delete_glue_ref(s);
        glue_ptr(c) = zero_glue;
      }
    }

    if (info(q) != end_span)
    {
      t = width(q) + width(glue_ptr(link(q)));
      r = info(q);
      s = end_span;
      info(s) = p;
      n = min_quarterword + 1;

      do {
        width(r) = width(r) - t;
        u = info(r);

        while (link(r) > n)
        {
          s = info(s);
          n = link(info(s)) + 1;
        }

        if (link(r) < n)
        {
          info(r) = info(s);
          info(s) = r;
          decr(link(r));
          s = r;
        }
        else
        {
          if (width(r) > width(info(s)))
            width(info(s)) = width(r);

          free_node(r, span_node_size);
        }

        r = u;
      } while (!(r == end_span));
    }

    type(q) = unset_node;
    span_count(q) = min_quarterword;
    height(q) = 0;
    depth(q) = 0;
    glue_order(q) = normal;
    glue_sign(q) = normal;
    glue_stretch(q) = 0;
    glue_shrink(q) = 0;
    q = p;
  } while (!(q == 0));

  save_ptr = save_ptr - 2;
  pack_begin_line = -mode_line;

  if (mode == -vmode)
  {
    rule_save = overfull_rule;
    overfull_rule = 0;
    z = new_null_box();
    link(z) = preamble;
    adjust_hlist(z, false);
    delete_glue_ref(cur_kanji_skip);
    delete_glue_ref(cur_xkanji_skip);
    cur_kanji_skip = space_ptr(z);
    cur_xkanji_skip = xspace_ptr(z);
    add_glue_ref(cur_kanji_skip);
    add_glue_ref(cur_xkanji_skip);
    p = hpack(preamble, saved(1), saved(0));
    overfull_rule = rule_save;
    delete_glue_ref(space_ptr(z));
    delete_glue_ref(xspace_ptr(z));
    free_node(z, box_node_size);
  }
  else
  {
    q = link(preamble);

    do {
      height(q) = width(q);
      width(q) = 0;
      q = link(link(q));
    } while (!(q == 0));

    p = vpackage(preamble, saved(1), saved(0), max_dimen);
    q = link(preamble);

    do {
      width(q) = height(q);
      height(q) = 0;
      q = link(link(q));
    } while (!(q == 0));
  }

  pack_begin_line = 0;
  q = link(head);
  s = head;

  while (q != 0)
  {
    if (!is_char_node(q))
      if (type(q) == unset_node)
      {
        if (mode == -vmode)
        {
          type(q) = hlist_node;
          width(q) = width(p);

          if (nest[nest_ptr - 1].mode_field == mmode)
            set_box_lr(q, dlist);
        }
        else
        {
          type(q) = vlist_node;
          height(q) = height(p);
        }

        set_box_dir(q, abs(direction));
        glue_order(q) = glue_order(p);
        glue_sign(q) = glue_sign(p);
        glue_set(q) = glue_set(p);
        shift_amount(q) = o;
        r = link(list_ptr(q));
        s = link(list_ptr(p));

        do {
          n = span_count(r);
          t = width(s);
          w = t;
          u = hold_head;
          set_box_lr(r, 0);

          while (n > min_quarterword)
          {
            decr(n);
            s = link(s);
            v = glue_ptr(s);
            link(u) = new_glue(v);
            u = link(u);
            subtype(u) = tab_skip_code + 1;
            t = t + width(v);

            if (glue_sign(p) == stretching)
            {
              if (stretch_order(v) == glue_order(p))
                t = t + round(glue_set(p) * stretch(v));
            }
            else if (glue_sign(p) == shrinking)
            {
              if (shrink_order(v) == glue_order(p))
                t = t - round(glue_set(p) * shrink(v));
            }

            s = link(s);
            link(u) = new_null_box();
            u = link(u);
            t = t + width(s);

            if (mode == -vmode)
              width(u) = width(s);
            else
            {
              type(u) = vlist_node;
              height(u) = width(s);
            }

            set_box_dir(u, abs(direction));
          }
            

          if (mode == -vmode)
          {
            height(r) = height(q);
            depth(r) = depth(q);

            if (t == width(r))
            {
              glue_sign(r) = normal;
              glue_order(r) = normal;
              set_glue_ratio_zero(glue_set(r));
            }
            else if (t > width(r))
            {
              glue_sign(r) = stretching;

              if (glue_stretch(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else
                glue_set(r) = (t - width(r)) / ((double) glue_stretch(r));
            }
            else
            {
              glue_order(r) = glue_sign(r);
              glue_sign(r) = shrinking;

              if (glue_shrink(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else if ((glue_order(r) == normal) && (width(r) - t > glue_shrink(r)))
                set_glue_ratio_one(glue_set(r));
              else
                glue_set(r) = (width(r) - t)/ ((double) glue_shrink(r));
            }

            width(r) = w;
            type(r) = hlist_node;
            set_box_dir(r, abs(direction));
          }
          else
          {
            width(r) = width(q);

            if (t == height(r))
            {
              glue_sign(r) = normal;
              glue_order(r) = normal;
              set_glue_ratio_zero(glue_set(r));
            }
            else if (t > height(r))
            {
              glue_sign(r) = stretching;

              if (glue_stretch(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else
                glue_set(r) = (t - height(r)) / ((double) glue_stretch(r));
            }
            else
            {
              glue_order(r) = glue_sign(r);
              glue_sign(r) = shrinking;

              if (glue_shrink(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else if ((glue_order(r) == normal) && (height(r) - t > glue_shrink(r)))
                set_glue_ratio_one(glue_set(r));
              else
                glue_set(r) = (height(r) - t) / ((double) glue_shrink(r));
            }

            height(r) = w;
            type(r) = vlist_node;
            set_box_dir(r, abs(direction));
          }

          shift_amount(r) = 0;

          if (u != hold_head)
          {
            link(u) = link(r);
            link(r) = link(hold_head);
            r = u;
          }

          r = link(link(r));
          s = link(link(s));
        } while (!(r == 0));
      }
      else if (type(q) == rule_node)
      {
        if (is_running(width(q)))
          width(q) = width(p);

        if (is_running(height(q)))
          height(q) = height(p);

        if (is_running(depth(q)))
          depth(q) = depth(p);

        if (o != 0)
        {
          r = link(q);
          link(q) = 0;
          q = hpack(q, 0, 1);
          shift_amount(q) = o;
          link(q) = r;
          link(s) = q;
        }
      }

    s = q;
    q = link(q);
  }

  flush_node_list(p);
  pop_alignment();
  aux_save = aux;
  p = link(head);
  q = tail;
  pop_nest();

  if (mode == mmode)
  {
    do_assignments();

    if (cur_cmd != math_shift)
    {
      print_err("Missing $$ inserted");
      help2("Displays can use special alignments (like \\eqalignno)",
          "only if nothing but the alignment itself is between $$'s.");
      back_error();
    }
    else
    {
      get_x_token();

      if (cur_cmd != math_shift)
      {
        print_err("Display math should end with $$");
        help2("The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.");
        back_error();
      }
    }

    flush_node_list(LR_box);
    pop_nest();
    tail_append(new_penalty(pre_display_penalty));
    tail_append(new_param_glue(above_display_skip_code));
    link(tail) = p;

    if (p != 0)
      tail = q;

    tail_append(new_penalty(post_display_penalty));
    tail_append(new_param_glue(below_display_skip_code));
    prev_depth = aux_save.cint;
    resume_after_display();
  }
  else
  {
    aux = aux_save;
    link(tail) = p;

    if (p != 0)
      tail = q;

    if (mode == vmode)
      build_page();
  }
}
/* sec 0791 */
boolean fin_col (void)
{
  pointer p;
  pointer q, r;
  pointer s;
  pointer u;
  scaled w;
  glue_ord o;
  halfword n;

  if (cur_align == 0)
  {
    confusion("endv");
    return 0;
  }

  q = link(cur_align);

  if (q == 0)
  {
    confusion("endv");
    return 0;
  }

  if (align_state < 500000L)
  {
    fatal_error("(interwoven alignment preambles are not allowed)");
    return 0;
  }

  p = link(q);

  if ((p == 0) && (extra_info(cur_align) < cr_code))
    if (cur_loop != 0)
    {
      link(q) = new_null_box();
      p = link(q);
      info(p) = end_span;
      width(p) = null_flag;
      cur_loop = link(cur_loop);
      q = hold_head;
      r = u_part(cur_loop);

      while (r != 0)
      {
        link(q) = get_avail();
        q = link(q);
        info(q) = info(r);
        r = link(r);
      }

      link(q) = 0;
      u_part(p) = link(hold_head);
      q = hold_head;
      r = v_part(cur_loop);

      while (r != 0)
      {
        link(q) = get_avail();
        q = link(q);
        info(q) = info(r);
        r = link(r);
      }

      link(q) = 0;
      v_part(p) = link(hold_head);
      cur_loop = link(cur_loop);
      link(p) = new_glue(glue_ptr(cur_loop));
    }
    else
    {
      print_err("Extra alignment tab has been changed to ");
      print_esc("cr");
      help3("You have given more \\span or & marks than there were",
          "in the preamble to the \\halign or \\valign now in progress.",
          "So I'll assume that you meant to type \\cr instead.");
      extra_info(cur_align) = cr_code;
      error();
    }

  if (extra_info(cur_align) != span_code)
  {
    unsave();
    new_save_level(align_group);

    {
      if (mode == -hmode)
      {
        adjust_tail = cur_tail;
        adjust_hlist(head, false);
        delete_glue_ref(cur_kanji_skip);
        delete_glue_ref(cur_xkanji_skip);
        cur_kanji_skip = space_ptr(head);
        cur_xkanji_skip = xspace_ptr(head);
        add_glue_ref(cur_kanji_skip);
        add_glue_ref(cur_xkanji_skip);
        u = hpack(link(head), 0, 1);
        w = width(u);
        cur_tail = adjust_tail;
        adjust_tail = 0;
      }
      else
      {
        u = vpackage(link(head), 0, 1, 0);
        w = height(u);
      }

      n = min_quarterword;

      if (cur_span != cur_align)
      {
        q = cur_span;

        do {
          incr(n);
          q = link(link(q));
        } while (!(q == cur_align));

        if (n > max_quarterword)
        {
          confusion("256 spans");
          return 0;
        }

        q = cur_span;

        while (link(info(q)) < n)
          q = info(q);

        if (link(info(q)) > n)
        {
          s = get_node(span_node_size);
          info(s) = info(q);
          link(s) = n;
          info(q) = s;
          width(s) = w;
        }
        else if (width(info(q)) < w)
          width(info(q)) = w;
      }
      else if (w > width(cur_align))
        width(cur_align) = w;

      type(u) = unset_node;
      span_count(u) = n;

      if (total_stretch[filll] != 0)
        o = filll;
      else if (total_stretch[fill] != 0)
        o = fill;
      else if (total_stretch[fil] != 0)
        o = fil;
      else
        o = normal;

      glue_order(u) = o;
      glue_stretch(u) = total_stretch[o];

      if (total_shrink[filll] != 0)
        o = filll;
      else if (total_shrink[fill] != 0)
        o = fill;
      else if (total_shrink[fil] != 0)
        o = fil;
      else
        o = normal;

      glue_sign(u) = o;
      glue_shrink(u) = total_shrink[o];
      pop_nest();
      link(tail) = u;
      tail = u;
    }

    tail_append(new_glue(glue_ptr(link(cur_align))));
    subtype(tail) = tab_skip_code + 1;

    if (extra_info(cur_align) >= cr_code)
    {
      return true;
    }

    init_span(p);
  }

  align_state = 1000000L;

  do {
    get_x_or_protected();
  } while (!(cur_cmd != spacer));

  cur_align = p;
  init_col();

  return false;
}
/* sec 0749 */
scaled make_op (pointer q)
{
  scaled delta;
  pointer p, v, x, y, z;
  quarterword c;
  four_quarters i;
  scaled shift_up, shift_down;

  if ((subtype(q) == normal) && (cur_style < text_style))
    subtype(q) = limits;

  if (math_type(nucleus(q)) == math_char)
  {
    fetch(nucleus(q));

    if ((cur_style < text_style) && (char_tag(cur_i) == list_tag))
    {
      c = rem_byte(cur_i);
      i = char_info(cur_f, c);

      if (char_exists(i))
      {
        cur_c = c;
        cur_i = i;
        character(nucleus(q)) = c;
      }
    }

    delta = char_italic(cur_f, cur_i);
    x = clean_box(nucleus(q), cur_style, math_kcode(q));

    if ((math_type(subscr(q)) != 0) && (subtype(q) != limits))
      width(x) = width(x) - delta;

    shift_amount(x) = half(height(x) - depth(x)) - axis_height(cur_size);
    math_type(nucleus(q)) = sub_box;
    info(nucleus(q)) = x;
  }
  else
    delta = 0;

  if (subtype(q) == limits)
  {
    x = clean_box(supscr(q), sup_style(cur_style), math_kcode(q));
    y = clean_box(nucleus(q), cur_style, math_kcode(q));
    z = clean_box(subscr(q), sub_style(cur_style), math_kcode(q));
    v = new_null_box();
    type(v) = vlist_node;
    width(v) = width(y);

    if (width(x) > width(v))
      width(v) = width(x);

    if (width(z) > width(v))
      width(v) = width(z);

    x = rebox(x, width(v));
    y = rebox(y, width(v));
    z = rebox(z, width(v));
    shift_amount(x) = half(delta);
    shift_amount(z) = -shift_amount(x);
    height(v) = height(y);
    depth(v) = depth(y);

    if (math_type(supscr(q)) == 0)
    {
      delete_glue_ref(space_ptr(x));
      delete_glue_ref(xspace_ptr(x));
      free_node(x, box_node_size);
      list_ptr(v) = y;
    }
    else
    {
      shift_up = big_op_spacing3 - depth(x);

      if (shift_up < big_op_spacing1)
        shift_up = big_op_spacing1;

      p = new_kern(shift_up);
      link(p) = y;
      link(x) = p;
      p = new_kern(big_op_spacing5);
      link(p) = x;
      list_ptr(v) = p;
      height(v) = height(v) + big_op_spacing5 + height(x) + depth(x) + shift_up;
    }

    if (math_type(subscr(q)) == 0)
    {
      delete_glue_ref(space_ptr(z));
      delete_glue_ref(xspace_ptr(z));
      free_node(z, box_node_size);
    }
    else
    {
      shift_down = big_op_spacing4 - height(z);

      if (shift_down < big_op_spacing2)
        shift_down = big_op_spacing2;

      p = new_kern(shift_down);
      link(y) = p;
      link(p) = z;
      p = new_kern(big_op_spacing5);
      link(z) = p;
      depth(v) = depth(v) + big_op_spacing5 + height(z) + depth(z) + shift_down;
    }

    new_hlist(q) = v;
  }

  return delta;
}
/* sec 0756 */
void make_scripts (pointer q, scaled delta)
{
  pointer p, x, y, z;
  scaled shift_up, shift_down, clr;
  small_number t;

  p = new_hlist(q);

  if (is_char_node(p))
  {
    shift_up = 0;
    shift_down = 0;
  }
  else
  {
    z = hpack(p, 0, 1);

    if (cur_style < script_style)
      t = script_size;
    else
      t = script_script_size;

    shift_up = height(z) - sup_drop(t);
    shift_down = depth(z) + sub_drop(t);
    delete_glue_ref(space_ptr(z));
    delete_glue_ref(xspace_ptr(z));
    free_node(z, box_node_size);
  }

  if (math_type(supscr(q)) == 0)
  {
    x = clean_box(subscr(q), sub_style(cur_style), math_kcode(q));
    width(x) = width(x) + script_space;

    if (shift_down < sub1(cur_size))
      shift_down = sub1(cur_size);

    clr = height(x) - (abs(math_x_height(cur_size) * 4) / 5);

    if (shift_down < clr)
      shift_down = clr;

    shift_amount(x) = shift_down;
  }
  else
  {
    {
      x = clean_box(supscr(q), sup_style(cur_style), math_kcode(q));
      width(x) = width(x) + script_space;

      if (odd(cur_style))
        clr = sup3(cur_size);
      else if (cur_style < text_style)
        clr = sup1(cur_size);
      else
        clr = sup2(cur_size);

      if (shift_up < clr)
        shift_up = clr;

      clr = depth(x) + (abs(math_x_height(cur_size)) / 4);

      if (shift_up < clr)
        shift_up = clr;
    }

    if (math_type(subscr(q)) == 0)
      shift_amount(x) = -shift_up;
    else
    {
      y = clean_box(subscr(q), sub_style(cur_style), math_kcode(q));
      width(y) = width(y) + script_space;

      if (shift_down < sub2(cur_size))
        shift_down = sub2(cur_size);

      clr = 4 * default_rule_thickness - ((shift_up - depth(x)) - (height(y) - shift_down));

      if (clr > 0)
      {
        shift_down = shift_down + clr;

        clr = (abs(math_x_height(cur_size) * 4) / 5) - (shift_up - depth(x));

        if (clr > 0)
        {
          shift_up = shift_up + clr;
          shift_down = shift_down - clr;
        }
      }

      shift_amount(x) = delta;
      p = new_kern((shift_up - depth(x)) - (height(y) - shift_down));
      link(x) = p;
      link(p) = y;
      x = vpackage(x, 0, 1, max_dimen);
      shift_amount(x) = shift_down;
    }
  }

  if (new_hlist(q) == 0)
    new_hlist(q) = x;
  else
  {
    p = new_hlist(q);

    while (link(p) != 0)
      p = link(p);

    link(p) = x;
  }
}
/* sec 0785 */
void align_peek (void)
{
restart:
  align_state = 1000000L;

  do {
    get_x_or_protected();
  } while (!(cur_cmd != spacer));

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
// finite_shrink
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
  scaled g;

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
        if ((minimum_demerits < awful_bad) &&
          ((old_l != easy_line) || (r == active)))
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
                      v = link(v);
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
                      s = link(s);
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
          {
            do_all_six(convert_to_break_width);
          }
          else if (prev_r == active)
          {
            do_all_six(store_break_width);
          }
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

              if (do_last_line_fit)
              {
                active_short(q) = best_pl_short[fit_class];
                active_glue(q) = best_pl_glue[fit_class];
              }

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

                if (do_last_line_fit)
                {
                  prints(" s=");
                  print_scaled(active_short(q));

                  if (cur_p == null)
                    prints(" a=");
                  else
                    prints(" g=");

                  print_scaled(active_glue(q));
                }

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
        if ((cur_active_width[3] != 0) || (cur_active_width[4] != 0) ||
          (cur_active_width[5] != 0))
        {
          if (do_last_line_fit)
          {
            if (cur_p == null)
            {
              if ((active_short(r) == 0) || (active_glue(r) <= 0))
                goto not_found;

              if ((cur_active_width[3] != fill_width[0]) ||
                (cur_active_width[4] != fill_width[1]) ||
                (cur_active_width[5] != fill_width[2]))
                goto not_found;

              if (active_short(r) > 0)
                g = cur_active_width[2];
              else
                g = cur_active_width[6];

              if (g <= 0)
                goto not_found;

              arith_error = false;
              g = fract(g, active_short(r), active_glue(r), max_dimen);

              if (last_line_fit < 1000)
                g = fract(g, last_line_fit, 1000, max_dimen);

              if (arith_error)
                if (active_short(r)>0)
                  g = max_dimen;
                else
                  g = -max_dimen;

              if (g > 0)
              {
                if (g > shortfall)
                  g = shortfall;

                if (g > 7230584)
                  if (cur_active_width[2] < 1663497)
                  {
                    b = inf_bad;
                    fit_class = very_loose_fit;
                    goto found;
                  }

                b = badness(g, cur_active_width[2]);
                  
                if (b > 12)
                  if (b > 99)
                    fit_class = very_loose_fit;
                  else
                    fit_class = loose_fit;
                else
                  fit_class = decent_fit;

                goto found;
              }
              else if (g < 0)
              {
                if (-g > cur_active_width[6])
                  g = -cur_active_width[6];

                b = badness(-g, cur_active_width[6]);

                if (b > 12)
                  fit_class = tight_fit;
                else
                  fit_class = decent_fit;

                goto found;
              }

not_found:;
            }

            shortfall = 0;
          }

          b = 0;
          fit_class = decent_fit;
        }
        else
        {
          if (shortfall > 7230584L)
            if (cur_active_width[2] < 1663497L)
            {
              b = inf_bad;
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
        if (-shortfall > cur_active_width[6])
          b = inf_bad + 1;
        else
          b = badness(-shortfall, cur_active_width[6]);

        if (b > 12)
          fit_class = tight_fit;
        else
          fit_class = decent_fit;
      }

      if (do_last_line_fit)
      {
        if (cur_p == null)
          shortfall = 0;

        if (shortfall > 0)
          g = cur_active_width[2];
        else if (shortfall < 0)
          g = cur_active_width[6];
        else
          g = 0;
      }

found:
      if ((b > inf_bad) || (pi == eject_penalty))
      {
        if (final_pass && (minimum_demerits == awful_bad) &&
          (link(r) == active) && (prev_r == active))
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

        if (do_last_line_fit)
        {
          best_pl_short[fit_class] = shortfall;
          best_pl_glue[fit_class] = g;
        }

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
void post_line_break (boolean d)
{
  pointer q, r, s;
  boolean disc_break;
  boolean post_disc_break;
  scaled cur_width;
  scaled cur_indent;
  quarterword t;
  integer pen;
  halfword cur_line;
  pointer LR_ptr;

  LR_ptr = LR_save;
  q = break_node(best_bet);
  cur_p = 0;

  do {
    r = q;
    q = prev_break(q);
    next_break(r) = cur_p;
    cur_p = r;
  } while (!(q == 0));

  cur_line = prev_graf + 1;
  last_disp = 0;

  do {
    if (TeXXeT_en)
    {
      q = link(temp_head);

      if (LR_ptr != null)
      {
        temp_ptr = LR_ptr;
        r = q;

        do {
          s = new_math(0, begin_LR_type(info(temp_ptr)));
          link(s) = r;
          r = s;
          temp_ptr = link(temp_ptr);
        } while (!(temp_ptr == null));

        link(temp_head) = r;
      }

      while (q != cur_break(cur_p))
      {
        if (!is_char_node(q))
          if (type(q) == math_node)
            adjust_the_LR_stack_p();

        q = link(q);
      }
    }

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
          else if (type(q) == kern_node)
            width(q) = 0;
          else if (type(q) == math_node)
          {
            width(q) = 0;

            if (TeXXeT_en)
              adjust_the_LR_stack_p();
          }
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
    if (TeXXeT_en)
      if (LR_ptr != null)
      {
        s = temp_head;
        r = link(s);

        while (r != q)
        {
          s = r;
          r = link(s);
        }

        r = LR_ptr;

        while (r != null)
        {
          temp_ptr = new_math(0, info(r));
          link(s) = temp_ptr;
          s = temp_ptr;
          r = link(r);
        }

        link(s) = q;
      }

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

    if (left_skip != zero_glue)
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
    just_box = hpack(q, cur_width, exactly);
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
      q = inter_line_penalties_ptr;

      if (q != null)
      {
        r = cur_line;

        if (r > penalty(q))
          r = penalty(q);

        pen = penalty(q + r);
      }
      else
        pen = inter_line_penalty;

      q = club_penalties_ptr;

      if (q != null)
      {
        r = cur_line - prev_graf;

        if (r > penalty(q))
          r = penalty(q);

        pen = pen + penalty(q + r);
      }
      else if (cur_line == prev_graf + 1)
        pen = pen + club_penalty;

      if (d)
        q = display_widow_penalties_ptr;
      else
        q = widow_penalties_ptr;

      if (q != null)
      {
        r = best_line - cur_line - 1;

        if (r > penalty(q))
          r = penalty(q);

        pen = pen + penalty(q + r);
      }
      else if (cur_line + 2 == best_line)
        if (d)
          pen = pen + display_widow_penalty;
        else
          pen = pen + widow_penalty;

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

          if (type(q) == math_node)
            if (TeXXeT_en)
              adjust_the_LR_stack_p();
        }

done1:
        if (r != temp_head)
        {
          link(r) = 0;
          flush_node_list(link(temp_head));
          link(temp_head) = q;
        }
      }
  } while (!(cur_p == 0));

  if ((cur_line != best_line) || (link(temp_head) != 0))
  {
    confusion("line breaking");
    return;
  }

  prev_graf = best_line - 1;
  LR_save = LR_ptr;
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
  else if (cur_l < non_char)
    append_charnode_to_t(cur_l);

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
      if (skip_byte(q) <= stop_flag)
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
                    pop_lig_stack();
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

      do {
        if (str_pool[u] < hc[j])
          goto not_found;

        if (str_pool[u] > hc[j])
          goto done;

        incr(j);
        incr(u);
      } while (!(j > hn));

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

  if (trie_char(cur_lang + 1) != cur_lang)
    return;

  hc[0] = 0;
  hc[hn + 1] = 0;
  hc[hn + 2] = 256;

  for (j = 0; j <= hn - r_hyf + 1; j++)
  {
    z = trie_link(cur_lang + 1) + hc[j];
    l = j;

    while (hc[l] == trie_char(z))
    {
      if (trie_op(z) != min_trie_op)
      {
        v = trie_op(z);

        do {
          v = v + op_start[cur_lang];
          i = l - hyf_distance[v];

          if (hyf_num[v] > hyf[i])
            hyf[i]= hyf_num[v];

          v = hyf_next[v];
        } while (!(v == min_trie_op));
      }

      incr(l);
      z = trie_link(z) + hc[l];
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

  do {
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
      do {
        r = get_node(small_node_size);
        link(r) = link(hold_head);
        type(r) = disc_node;
        major_tail = r;
        r_count = 0;

        while (link(major_tail) != 0)
          advance_major_tail();

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
          do {
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
          } while (!(l >= j));

          while (l > j)
          {
            j = reconstitute(j, hn, bchar, non_char) + 1;
            link(major_tail) = link(hold_head);

            while (link(major_tail) != 0)
              advance_major_tail();
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
      } while (!(!odd(hyf[j - 1])));
  } while (!(j > hn));

  link(s) = q;
  flush_list(init_list);
}
/* sec 0934 */
// enters new exceptions
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

  if (is_initex)
  {
    hyph_index = 0;
    goto not_found1;
  }

  set_hyph_index();

not_found1:
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
      case kchar_given:
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
          set_lc_code(cur_chr);

          if (hc[0] == 0)
          {
            print_err("Not a letter");
            help2("Letters in \\hyphenation words must have \\lccode>0.",
                "Proceed; I'll ignore the character I just read.");
            error();
          }
          else if (n < 63)
          {
            incr(n);
            hc[n] = hc[0];
          }
        }
        break;

      case char_num:
      case kchar_num:
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

              do {
                if (str_pool[u] < str_pool[v])
                  goto found;

                if (str_pool[u] > str_pool[v])
                  goto not_found;

                incr(u);
                incr(v);
              } while (!(u == str_start[k + 1]));

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
pointer prune_page_top (pointer p, boolean s)
{
  pointer prev_p;
  pointer q, r;

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

          if (s)
          {
            if (split_disc == null)
              split_disc = q;
            else
              link(r) = q;

            r = q;
          }
          else
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
// finds optimum page break
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
        if ((active_width[3] != 0) || (active_width[4] != 0) ||
          (active_width[5] != 0))
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
// extracts a page of height |h| from box |n|
pointer vsplit (halfword n, scaled h)
{
  pointer v;
  pointer w;
  pointer p;
  pointer q;

  cur_val = n;
  fetch_box(v);
  flush_node_list(split_disc);
  split_disc = null;

  if (sa_mark != null)
    if (do_marks(vsplit_init, 0, sa_mark))
      sa_mark = null;

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
      if (mark_class(p) != 0)
      {
        find_sa_element(mark_val, mark_class(p), true);

        if (sa_split_first_mark(cur_ptr) == null)
        {
          sa_split_first_mark(cur_ptr) = mark_ptr(p);
          add_token_ref(mark_ptr(p));
        }
        else
          delete_token_ref(sa_split_bot_mark(cur_ptr));

        sa_split_bot_mark(cur_ptr) = mark_ptr(p);
        add_token_ref(mark_ptr(p));
      }
      else if (split_first_mark == 0)
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
  q = prune_page_top(q, saving_vdiscards > 0);
  p = list_ptr(v);
 
  if (q != 0)
  {
    q = vpackage(q, 0, 1, max_dimen);
    set_box_dir(q, box_dir(v));
  }

  change_box(q);
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

  if (sa_mark != null)
    if (do_marks(fire_up_init, 0, sa_mark))
      sa_mark = null;

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
                ins_ptr(p) = prune_page_top(broken_ptr(r), false);

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
      if (mark_class(p) != 0)
      {
        find_sa_element(mark_val, mark_class(p), true);

        if (sa_first_mark(cur_ptr) == null)
        {
          sa_first_mark(cur_ptr) = mark_ptr(p);
          add_token_ref(mark_ptr(p));
        }

        if (sa_bot_mark(cur_ptr) != null)
          delete_token_ref(sa_bot_mark(cur_ptr));

        sa_bot_mark(cur_ptr) = mark_ptr(p);
        add_token_ref(mark_ptr(p));
      }
      else
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
        contrib_tail = page_tail;

    link(page_tail) = link(contrib_head);
    link(contrib_head) = p;
    link(prev_p) = 0;
  }

  save_vbadness = vbadness;
  vbadness = inf_bad;
  save_vfuzz = vfuzz;
  vfuzz = max_dimen;
  box(255) = vpackage(link(page_head), best_size, exactly, page_max_depth);
  set_box_dir(box(255), page_dir);
  vbadness = save_vbadness;
  vfuzz = save_vfuzz;

  if (last_glue != empty_flag)
    delete_glue_ref(last_glue);

  page_contents = 0;
  page_tail = page_head;
  link(page_head) = 0;
  last_glue = max_halfword;
  last_penalty = 0;
  last_kern = 0;
  last_node_type = -1;
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

  if (sa_mark != null)
    if (do_marks(fire_up_done, 0, sa_mark))
      sa_mark = null;

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
      mode_line = -line;
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
          contrib_tail = page_tail;
      else
        link(page_tail) = link(contrib_head);

      link(contrib_head) = link(page_head);
      link(page_head) = 0;
      page_tail = page_head;
    }

    flush_node_list(page_disc);
    page_disc = null;
    ship_out(box(255));
    box(255) = 0;
  }
}
/* sec 0994 */
// append contributions to the current page
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

  do {
continu:
    p = link(contrib_head);

    if (last_glue != max_halfword)
      delete_glue_ref(last_glue);

    last_penalty = 0;
    last_kern = 0;

    if (type(p) < dir_node)
      last_node_type = type(p) + 1;
    else if (type(p) == dir_node)
      last_node_type = type(list_ptr(p)) + 1;
    else if (type(p) < disp_node)
      last_node_type = type(p);
    else
      last_node_type = type(p) - 1;

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
        if ((page_so_far[3] != 0) || (page_so_far[4] != 0) ||
          (page_so_far[5] != 0))
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

    if (saving_vdiscards > 0)
    {
      if (page_disc == null)
        page_disc = p;
      else
        link(tail_page_disc) = p;

      tail_page_disc = p;
    }
    else
      flush_node_list(p);
done:;
  } while (!(link(contrib_head) == 0));

  if (nest_ptr == 0)
    tail = contrib_head;
  else
    contrib_tail = contrib_head;
} 
/* sec 1043 */
// handle spaces when |space_factor<>1000
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
// do this when \.{\\end} or \.{\\dump} occurs
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

  if (inter_line_penalties_ptr != 0)
    eq_define(inter_line_penalties_loc, shape_ref, null);
}
/* sec 1075 */
void box_end (integer box_context)
{
  pointer p;
  small_number a;
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
        cur_box = new_dir_node(cur_box, abs(direction));

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
  {
    if (box_context < global_box_flag)
    {
      cur_val = box_context - box_flag;
      a = 0;
    }
    else
    {
      cur_val = box_context - global_box_flag;
      a = 4;
    }

    if (cur_val < 256)
      define(box_base + cur_val, box_ref, cur_box);
    else
      sa_def_box();
  }
  else if (cur_box != 0)
    if (box_context > ship_out_flag)
    {
      do {
        get_x_token();
      } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

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
            cur_box = new_dir_node(cur_box, abs(direction));
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
  pointer s;
  pointer t;
  integer fm;
  integer gm;
  boolean fd, gd;
  scaled disp, pdisp;
  eight_bits a_dir;
  pointer tx;
  quarterword m;
  halfword k;
  halfword n;

  switch (cur_chr)
  {
    case box_code:
      {
        scan_register_num();
        fetch_box(cur_box);
        change_box(null);
      }
      break;

    case copy_code:
      {
        scan_register_num();
        fetch_box(q);
        cur_box = copy_node_list(q);
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
        else if ((mode == vmode) && (head == tail))
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
                set_box_dir(cur_box, abs(direction));
done:;
            }
        }
      }
      break;

    case vsplit_code:
      {
        scan_register_num();
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
            scan_spec(adjusted_hbox_group, true);
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
        mode = -k;
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
// the next input should specify a box or perhaps a rule
void scan_box (integer box_context)
{
  do {
    get_x_token(); 
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

  if (cur_cmd == make_box)
    begin_box(box_context);
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
      line_break(false);
    }

    if (LR_save != null)
    {
      flush_list(LR_save);
      LR_save = null;
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
  halfword c;

  if (cur_chr == 0)
    c = 0;
  else
  {
    scan_register_num();
    c = cur_val;
  }

  p = scan_toks(false, true);
  p = get_node(small_node_size);
  mark_class(p) = c;
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
  pointer s;
  pointer t;
  integer fm;
  integer gm;
  boolean fd, gd;
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

  if (cur_chr > copy_code)
  {
    link(tail) = disc_ptr[cur_chr];
    disc_ptr[cur_chr] = null;
    goto done;
  }

  c = cur_chr;
  scan_register_num();
  fetch_box(p);

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
    if (type(p) == dir_node)
    {
      delete_glue_ref(space_ptr(p));
      delete_glue_ref(xspace_ptr(p));
      free_node(p, box_node_size);
    }

    flush_node_list(link(p));
    link(tail) = list_ptr(p);
    change_box(null);
    delete_glue_ref(space_ptr(p));
    delete_glue_ref(xspace_ptr(p));
    free_node(p, box_node_size);
  }

done:
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
      d = null;

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
      tail = new_kern(-a - delta);
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
void no_align_error (void)
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
  pointer j;
  integer x;
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
    j = null;
    w = -max_dimen;

    if (head == tail)
    {
      pop_nest();
      set_value_of_x();
    }
    else
    {
      adjust_hlist(head, true);
      line_break(true);

      if (eTeX_ex)
      {
        if (right_skip == zero_glue)
          j = new_kern(0);
        else
          j = new_param_glue(right_skip_code);

        if (left_skip == zero_glue)
          p = new_kern(0);
        else
          p = new_param_glue(left_skip_code);

        link(p) = j;
        j = new_null_box();
        width(j) = width(just_box);
        shift_amount(j) = shift_amount(just_box);
        list_ptr(j) = p;
        glue_order(j) = glue_order(just_box);
        glue_sign(j) = glue_sign(just_box);
        glue_set(j) = glue_set(just_box);
      }

      v = shift_amount(just_box);
      set_value_of_x();

      if (x >= 0)
      {
        p = list_ptr(just_box);
        link(temp_head) = null;
      }
      else
      {
        v = -v - width(just_box);
        p = new_math(0, begin_L_code);
        link(temp_head) = p;
        just_copy(list_ptr(just_box), p, new_math(0, end_L_code));
        cur_dir = right_to_left;
      }

      v = v + 2 * quad(cur_font);

      if (TeXXeT_en)
        put_LR(before);

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
            d = width(p);
            break;

          case math_node:
            {
              d = width(p);

              if (TeXXeT_en)
                if (end_LR(p))
                {
                  if (info(LR_ptr) == end_LR_type(p))
                    pop_LR();
                  else if (subtype(p) > L_code)
                  {
                    w = max_dimen;
                    goto done;
                  }
                }
                else
                {
                  push_LR(p);

                  if (LR_dir(p) != cur_dir)
                  {
                    just_reverse(p);
                    p = temp_head;
                  }
                }
              else if (subtype(p) >= L_code)
              {
                w = max_dimen;
                goto done;
              }
            }
            break;

          case edge_node:
            {
              d = width(p);
              cur_dir = subtype(p);
            }
            break;

          case glue_node:
            {
              q = glue_ptr(p);
              d = width(q);

              if (glue_sign(just_box) == stretching)
              {
                if ((glue_order(just_box) == stretch_order(q)) &&
                  (stretch(q) != 0))
                  v = max_dimen;
              }
              else if (glue_sign(just_box) == shrinking)
              {
                if ((glue_order(just_box) == shrink_order(q)) &&
                  (shrink(q) != 0))
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
done:
      if (TeXXeT_en)
      {
        while (LR_ptr != null)
          pop_LR();

        if (LR_problems != 0)
        {
          w = max_dimen;
          LR_problems = 0;
        }
      }

      cur_dir = left_to_right;
      flush_node_list(link(temp_head));
    }

    if (par_shape_ptr == 0)
      if ((hang_indent != 0) && (((hang_after >= 0) &&
        (prev_graf + 2 > hang_after)) || (prev_graf + 1 < -hang_after)))
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
    LR_box = j;

    if (eTeX_ex)
      eq_word_define(int_base + pre_display_direction_code, x);

    eq_word_define(dimen_base + display_width_code, l);
    eq_word_define(dimen_base + display_indent_code, s);

    if (every_display != 0)
      begin_token_list(every_display, every_display_text);

    if (nest_ptr == 1)
      build_page();
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
  do {
    get_x_token();
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

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
        KANJI(cx) = cur_chr;
      break;

    case kchar_given:
      KANJI(cx) = cur_chr;
      break;

    case kanji:
    case kana:
    case other_kchar:
    case hangul:
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

    if ((c >= var_code) && fam_in_range)
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
void set_math_char (integer c)
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
      if (fam_in_range)
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
void scan_delimiter (pointer p, boolean r)
{
   if (r)
     scan_twenty_seven_bit_int();
   else
   {
     do {
      get_x_token();
     } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

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

  if ((cur_val >= var_code) && fam_in_range)
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

      if ((type(q) != left_noad) || (delim_ptr == null))
      {
        confusion("right");
        return 0;
      }

      info(numerator(incompleat_noad)) = link(delim_ptr);
      link(delim_ptr) = incompleat_noad;
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
/* sec 1181 */
void math_fraction (void)
{
  small_number c;

  c = cur_chr;
  inhibit_glue_flag = false;

  if (incompleat_noad != 0)
  {
    if (c >= delimited_code)
    {
      scan_delimiter(garbage, false);
      scan_delimiter(garbage, false);
    }

    if (c % delimited_code == above_code)
      scan_normal_dimen();

    print_err("Ambiguous; you need another { and }");
    help3("I'm ignoring this fraction specification, since I don't",
      "know whether a construction like `x \\over y \\over z'",
      "means `{x \\over y} \\over z' or `x \\over {y \\over z}'.");
    error();
  }
  else
  {
    incompleat_noad = get_node(fraction_noad_size);
    type(incompleat_noad) = fraction_noad;
    subtype(incompleat_noad) = normal;
    math_type(numerator(incompleat_noad)) = sub_mlist;
    info(numerator(incompleat_noad)) = link(head);
    mem[denominator(incompleat_noad)].hh = empty_field;
    mem[left_delimiter(incompleat_noad)].qqqq = null_delimiter;
    mem[right_delimiter(incompleat_noad)].qqqq = null_delimiter;
    link(head) = 0;
    tail = head;

    if (c >= delimited_code)
    {
      scan_delimiter(left_delimiter(incompleat_noad), false);
      scan_delimiter(right_delimiter(incompleat_noad), false);
    }

    switch (c % delimited_code)
    {
      case above_code:
        scan_normal_dimen();
        thickness(incompleat_noad) = cur_val;
        break;

      case over_code:
        thickness(incompleat_noad) = default_code;
        break;

      case atop_code:
        thickness(incompleat_noad) = 0;
        break;
    }
  }
}
/* sec 1191 */
void math_left_right (void)
{
  small_number t;
  pointer p;
  pointer q;

  t = cur_chr;
  inhibit_glue_flag = false;

  if ((t != left_noad) && (cur_group != math_left_group))
  {
    if (cur_group == math_shift_group)
    {
      scan_delimiter(garbage, false);
      print_err("Extra ");

      if (t == middle_noad)
      {
        print_esc("middle");
        help1("I'm ignoring a \\middle that had no matching \\left.");
      }
      else
      {
        print_esc("right");
        help1("I'm ignoring a \\right that had no matching \\left.");
      }

      error();
    }
    else
      off_save();
  }
  else
  {
    p = new_noad();
    type(p) = t;
    scan_delimiter(delimiter(p), false);

    if (t == middle_noad)
    {
      type(p) = right_noad;
      subtype(p) = middle_noad;
    }

    if (t == left_noad)
      q = p;
    else
    {
      q = fin_mlist(p);
      unsave();
    }

    if (t != right_noad)
    {
      push_math(math_left_group);
      link(head) = q;
      tail = p;
      delim_ptr = p;
    }
    else
    {
      tail_append(new_noad());
      type(tail) = inner_noad;
      math_type(nucleus(tail)) = sub_mlist;
      info(nucleus(tail)) = q;
    }
  }
}
/* sec 1194 */
void after_math (void)
{
  boolean l;
  scaled disp;
  boolean danger;
  integer m;
  pointer p;
  pointer a;
  pointer b;
  scaled w;
  scaled z;
  scaled e;
  scaled q;
  scaled d;
  scaled s;
  small_number g1, g2;
  pointer r;
  pointer t;
  pointer j;

  danger = false;

  if (mode == mmode)
    j = LR_box;
  
  if ((font_params[fam_fnt(2 + text_size)] < total_mathsy_params) ||
    (font_params[fam_fnt(2 + script_size)] < total_mathsy_params) ||
    (font_params[fam_fnt(2 + script_script_size)] < total_mathsy_params))
  {
    print_err("Math formula deleted: Insufficient symbol fonts");
    help3("Sorry, but I can't typeset math unless \\textfont 2",
        "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
        "the \\fontdimen values needed in math symbol fonts.");
    error();
    flush_math();
    danger = true;
  }
  else if ((font_params[fam_fnt(3 + text_size)] < total_mathex_params) ||
    (font_params[fam_fnt(3 + script_size)] < total_mathex_params) ||
    (font_params[fam_fnt(3 + script_script_size)] < total_mathex_params))
  {
    print_err("Math formula deleted: Insufficient extension fonts");
    help3("Sorry, but I can't typeset math unless \\textfont 3",
        "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
        "the \\fontdimen values needed in math extension fonts.");
    error();
    flush_math();
    danger = true;
  }

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
  m = mode;
  l = false;
  p = fin_mlist(0);

  if (mode == -m)
  {
    {
      get_x_token();

      if (cur_cmd != math_shift)
      {
        print_err("Display math should end with $$");
        help2("The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.");
        back_error();
      }
    }

    cur_mlist = p;
    cur_style = text_style;
    mlist_penalties = false;
    mlist_to_hlist();
    a = hpack(link(temp_head), 0, 1);
    set_box_lr(a, dlist);
    unsave();
    decr(save_ptr);

    if (saved(0) == 1)
      l = true;

    danger = false;

    if (mode == mmode)
      j = LR_box;

    if ((font_params[fam_fnt(2 + text_size)] < total_mathsy_params) ||
      (font_params[fam_fnt(2 + script_size)] < total_mathsy_params) ||
      (font_params[fam_fnt(2 + script_script_size)] < total_mathsy_params))
    {
      print_err("Math formula deleted: Insufficient symbol fonts");
      help3("Sorry, but I can't typeset math unless \\textfont 2",
          "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
          "the \\fontdimen values needed in math symbol fonts.");
      error();
      flush_math();
      danger = true;
    }
    else if ((font_params[fam_fnt(3 + text_size)] < total_mathex_params) ||
      (font_params[fam_fnt(3 + script_size)] < total_mathex_params) ||
      (font_params[fam_fnt(3 + script_script_size)] < total_mathex_params))
    {
      print_err("Math formula deleted: Insufficient extension fonts");
      help3("Sorry, but I can't typeset math unless \\textfont 3",
        "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
        "the \\fontdimen values needed in math extension fonts.");
      error();
      flush_math();
      danger = true;
    }

    m = mode;
    p = fin_mlist(0);
  }
  else
    a = 0;

  if (m < 0)
  {
    if (direction == dir_tate)
      disp = t_baseline_shift;
    else
      disp = y_baseline_shift;

    append_disp_node_at_begin();
    tail_append(new_math(math_surround, before));
    cur_mlist = p;
    cur_style = text_style;
    mlist_penalties = (mode > 0);
    mlist_to_hlist();
    link(tail) = link(temp_head);

    while (link(tail) != 0)
      tail = link(tail);

    tail_append(new_math(math_surround, after));
    append_disp_node_at_end();
    space_factor = 1000;
    unsave();
  }
  else
  {
    if (a == 0)
    {
      get_x_token();

      if (cur_cmd != math_shift)
      {
        print_err("Display math should end with $$");
        help2("The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.");
        back_error();
      }
    }

    cur_mlist = p;
    cur_style = display_style;
    mlist_penalties = false;
    mlist_to_hlist();
    p = link(temp_head);
    adjust_tail = adjust_head;
    b = hpack(p, 0, 1);
    p = list_ptr(b);
    t = adjust_tail;
    adjust_tail = 0;
    w = width(b);
    z = display_width;
    s = display_indent;

    if (pre_display_direction < 0)
      s = -s - z;

    if ((a == 0) || danger)
    {
      e = 0;
      q = 0;
    }
    else
    {
      e = width(a);
      q = e + math_quad(text_size);
    }

    if (w + q > z)
    {
      if ((e != 0) && ((w - total_shrink[normal] + q <= z) || (total_shrink[fil] != 0) ||
        (total_shrink[fill] != 0) || (total_shrink[filll] != 0)))
      {
        delete_glue_ref(space_ptr(b));
        delete_glue_ref(xspace_ptr(b));
        free_node(b, box_node_size);
        b = hpack(p, z - q, 0);
      }
      else
      {
        e = 0;

        if (w > z)
        {
          delete_glue_ref(space_ptr(b));
          delete_glue_ref(xspace_ptr(b));
          free_node(b, box_node_size);
          b = hpack(p, z, 0);
        }
      }

      w = width(b);
    }

    set_box_lr(b, dlist);
    d = half(z - w);

    if ((e > 0) && (d < 2 * e))
    {
      d = half(z - w - e);

      if (p != 0)
        if (!is_char_node(p))
          if (type(p) == glue_node)
            d = 0;
    }

    tail_append(new_penalty(pre_display_penalty));

    if ((d + s <= pre_display_size) || l)
    {
      g1 = above_display_skip_code;
      g2 = below_display_skip_code;
    }
    else
    {
      g1 = above_display_short_skip_code;
      g2 = below_display_short_skip_code;
    }

    if (l && (e == 0))
    {
      app_display(j, a, 0);
      tail_append(new_penalty(10000));
    }
    else
      tail_append(new_param_glue(g1));

    if (e != 0)
    {
      r = new_kern(z - w - e - d);

      if (l)
      {
        link(a) = r;
        link(r) = b;
        b = a;
        d = 0;
      }
      else
      {
        link(b) = r;
        link(r) = a;
      }

      b = hpack(b, 0, 1);
    }

    app_display(j, b, d);

    if ((a != 0) && (e == 0) && !l)
    {
      tail_append(new_penalty(inf_penalty));
      app_display(j, a, z - width(a));
      g2 = 0;
    }

    if (t != adjust_head)
    {
      link(tail) = link(adjust_head);
      tail = t;
    }

    tail_append(new_penalty(post_display_penalty));

    if (g2 > 0)
      tail_append(new_param_glue(g2));

    flush_node_list(j);
    resume_after_display();
  }
}
/* sec 1200 */
void resume_after_display (void)
{
  if (cur_group != math_shift_group)
  {
    confusion("display");
    return;
  }

  unsave();
  prev_graf = prev_graf + 3;
  push_nest();
  adjust_dir = abs(direction);
  mode = hmode;
  space_factor = 1000;
  set_cur_lang();
  clang = cur_lang;
  prev_graf =(norm_min(left_hyphen_min) * 64 + norm_min(right_hyphen_min)) * 65536L + cur_lang;

  {
    get_x_token();

    if (cur_cmd != spacer)
      back_input();
  }

  if (nest_ptr == 1)
    build_page();
}
/* sec 1215 */
void get_r_token (void)
{
restart:
  do {
    get_token();
  } while (!(cur_tok != space_token));

  if ((cur_cs == 0) || (cur_cs > frozen_control_sequence))
  {
    print_err("Missing control sequence inserted");
    help5("Please don't say `\\def cs{...}', say `\\def\\cs{...}'.",
      "I've inserted an inaccessible control sequence so that your",
      "definition will be completed without mixing me up too badly.",
      "You can recover graciously from this error, if you're",
      "careful; see exercise 27.2 in The TeXbook.");

    if (cur_cs == 0)
      back_input();

    cur_tok = cs_token_flag + frozen_protection;
    ins_error();
    goto restart;
  }
}
/* sec 1229 */
void trap_zero_glue (void)
{
  if ((width(cur_val) == 0) && (stretch(cur_val) == 0) && (shrink(cur_val) == 0))
  {
    add_glue_ref(zero_glue);
    delete_glue_ref(cur_val);
    cur_val = zero_glue;
  }
}
/* sec 1236 */
void do_register_command (small_number a)
{
  pointer l, q, r, s;
  char p;
  boolean e;
  integer w;

  q = cur_cmd;
  e = false;

  {
    if (q != tex_register)
    {
      get_x_token();

      if ((cur_cmd >= assign_int) && (cur_cmd <= assign_mu_glue))
      {
        l = cur_chr;
        p = cur_cmd - assign_int;
        goto found;
      }

      if (cur_cmd != tex_register)
      {
        print_err("You can't use `");
        print_cmd_chr(cur_cmd, cur_chr);
        prints("' after ");
        print_cmd_chr(q, 0);
        help1("I'm forgetting what you said and not changing anything.");
        error();
        return;
      }
    }

    if ((cur_chr < mem_bot) || (cur_chr > lo_mem_stat_max))
    {
      l = cur_chr;
      p = sa_type(l);
      e = true;
    }
    else
    {
      p = cur_chr - mem_bot;
      scan_register_num();

      if (cur_val > 255)
      {
        find_sa_element(p, cur_val, true);
        l = cur_ptr;
        e = true;
      }
      else switch (p)
      {
        case int_val:
          l = cur_val + count_base;
          break;

        case dimen_val:
          l = cur_val + scaled_base;
          break;

        case glue_val:
          l = cur_val + skip_base;
          break;

        case mu_val:
          l = cur_val + mu_skip_base;
          break;
      }
    }
  }

found:
  if (p < glue_val)
    if (e)
      w = sa_int(l);
    else
      w = eqtb[l].cint;
  else if (e)
    s = sa_ptr(l);
  else
    s = equiv(l);

  if (q == tex_register)
    scan_optional_equals();
  else if (scan_keyword("by"))
    do_nothing();

  arith_error = false;

  if (q < multiply)
    if (p < glue_val)
    {
      if (p == int_val)
        scan_int();
      else
        scan_normal_dimen();

      if (q == advance)
        cur_val = cur_val + w;
    }
    else
    {
      scan_glue(p);

      if (q == advance)
      {
        q = new_spec(cur_val);
        r = s;
        delete_glue_ref(cur_val);
        width(q) = width(q) + width(r);

        if (stretch(q) == 0)
          stretch_order(q) = normal;

        if (stretch_order(q) == stretch_order(r))
          stretch(q) = stretch(q) + stretch(r);
        else if ((stretch_order(q) < stretch_order(r)) && (stretch(r) != 0))
        {
          stretch(q) = stretch(r);
          stretch_order(q) = stretch_order(r);
        }

        if (shrink(q) == 0)
          shrink_order(q) = normal;

        if (shrink_order(q) == shrink_order(r))
          shrink(q) = shrink(q) + shrink(r);
        else if ((shrink_order(q) < shrink_order(r)) && (shrink(r) != 0))
        {
          shrink(q) = shrink(r);
          shrink_order(q) = shrink_order(r);
        }

        cur_val = q;
      }
    }
  else
  {
    scan_int();

    if (p < glue_val)
      if (q == multiply)
        if (p == int_val)
          cur_val = mult_integers(w, cur_val);
        else
          cur_val = nx_plus_y(w, cur_val, 0);
      else
        cur_val = x_over_n(w, cur_val);
    else
    {
      r = new_spec(s);

      if (q == multiply)
      {
        width(r) = nx_plus_y(width(s), cur_val, 0);
        stretch(r) = nx_plus_y(stretch(s), cur_val, 0);
        shrink(r) = nx_plus_y(shrink(s), cur_val, 0);
      }
      else
      {
        width(r) = x_over_n(width(s), cur_val);
        stretch(r) = x_over_n(stretch(s), cur_val);
        shrink(r) = x_over_n(shrink(s), cur_val);
      }

      cur_val = r;
    }
  }

  if (arith_error)
  {
    print_err("Arithmetic overflow");
    help2("I can't carry out that multiplication or division,",
        "since the result is out of range.");

    if (p >= glue_val)
      delete_glue_ref(cur_val);

    error();
    return;
  }

  if (p < glue_val)
    sa_word_define(l, cur_val);
  else
  {
    trap_zero_glue();
    sa_define(l, cur_val, l, glue_ref, cur_val);
  }
}
/* sec 1243 */
void alter_aux (void)
{
  halfword c;

  if (cur_chr != abs(mode))
    report_illegal_case();
  else
  {
    c = cur_chr;
    scan_optional_equals();

    if (c == vmode)
    {
      scan_normal_dimen();
      prev_depth = cur_val;
    }
    else
    {
      scan_int();

      if ((cur_val <= 0) || (cur_val > 32767))
      {
        print_err("Bad space factor");
        help1("I allow only values in the range 1..32767 here.");
        int_error(cur_val);
      }
      else
        space_factor = cur_val;
    }
  }
}
/* sec 1244 */
void alter_prev_graf (void)
{
  integer p;

  nest[nest_ptr] = cur_list;
  p = nest_ptr;

  while (abs(nest[p].mode_field) != vmode)
    decr(p);

  scan_optional_equals();
  scan_int();

  if (cur_val < 0)
  {
    print_err("Bad ");
    print_esc("prevgraf");
    help1("I allow only nonnegative values here.");
    int_error(cur_val);
  }
  else
  {
    nest[p].pg_field = cur_val;
    cur_list = nest[nest_ptr];
  }
}
/* sec 1245 */
void alter_page_so_far (void)
{
  char c;

  c = cur_chr;
  scan_optional_equals();
  scan_normal_dimen();
  page_so_far[c] = cur_val;
}
/* sec 1246 */
void alter_integer (void)
{
  small_number c;

  c = cur_chr;
  scan_optional_equals();
  scan_int();

  if (c == 0)
    dead_cycles = cur_val;
  else if (c == 2)
  {
    if ((cur_val < batch_mode) || (cur_val > error_stop_mode))
    {
      print_err("Bad interaction mode");
      help2("Modes are 0=batch, 1=nonstop, 2=scroll, and",
        "3=errorstop. Proceed, and I'll ignore this case.");
      int_error(cur_val);
    }
    else
    {
      cur_chr = cur_val;
      new_interaction();
    }
  }
  else
    insert_penalties = cur_val;
}
/* sec 1247 */
void alter_box_dimen (void)
{
  small_number c;
  pointer p, q;
  pointer b;

  c = cur_chr;
  scan_register_num();
  fetch_box(b);
  scan_optional_equals();
  scan_normal_dimen();

  if (b != null)
  {
    q = b;
    p = link(q);

    while (p != null)
    {
      if (abs(direction) == box_dir(p))
        q = p;

      p = link(p);
    }

    if (box_dir(q) != abs(direction))
    {
      p = link(b);
      link(b) = null;
      q = new_dir_node(q, abs(direction));
      list_ptr(q) = null;
      link(q) = p;
      link(b) = q;
    }

    mem[q + c].cint = cur_val;
  }
}
/* sec 1257 */
void new_font (small_number a)
{
  pointer u;
  scaled s;
  internal_font_number f;
  str_number t;
  char old_setting;
  str_number flushable_string;

  if (job_name == 0)
    open_log_file();

  get_r_token();
  u = cur_cs;

  if (u >= hash_base)
    t = text(u);
  else if (u >= single_base)
    if (u == null_cs)
      t = 1213; /* FONT */
    else
      t = u - single_base;
  else
  {
    old_setting = selector;
    selector = new_string;
    prints("FONT");
    print(u - active_base);
    selector = old_setting;
    str_room(1);
    t = make_string();
  }

  define(u, set_font, null_font);
  scan_optional_equals();
  scan_file_name();
  name_in_progress = true;

  if (scan_keyword("at"))
  {
    scan_normal_dimen();
    s = cur_val; 

    if ((s <= 0) || (s >= 134217728L)) /* 2^27 */
    {
      print_err("Improper `at' size (");
      print_scaled(s);
      prints("pt), replaced by 10pt");
      help2("I can only handle fonts at positive sizes that are",
        "less than 2048pt, so I've changed what you said to 10pt.");
      error();
      s = 10 * unity;
    }
  }
  else if (scan_keyword("scaled"))
  {
    scan_int();
    s = -cur_val;

    if ((cur_val <= 0) || (cur_val > 32768))
    {
      print_err("Illegal magnification has been changed to 1000");
      help1("The magnification ratio must be between 1 and 32768.");
      int_error(cur_val);
      s = -1000;
    }
  }
  else
    s = -1000;

  name_in_progress = false;
  flushable_string = str_ptr - 1;

  for (f = font_base + 1; f < font_ptr; f++)
  {
    if (str_eq_str(font_name[f], cur_name) && str_eq_str(font_area[f], cur_area))
    {
      if (cur_name == flushable_string)
      {
        flush_string();
        cur_name = font_name[f];
      }

      if (s > 0)
      {
        if (s == font_size[f])
          goto common_ending;
      }
      else if (font_size[f] == xn_over_d(font_dsize[f], -s, 1000))
        goto common_ending;
    }
  }

  f = read_font_info(u, cur_name, cur_area, s);

common_ending:
  define(u, set_font, f);
  eqtb[font_id_base + f] = eqtb[u];
  font_id_text(f) = t;
}
/* sec 1265 */
void new_interaction (void)
{
  print_ln();
  interaction = cur_chr;

  if (interaction == batch_mode)
    selector = no_print;
  else
    selector = term_only;

  if (log_opened)
    selector = selector + 2;
}
/* sec 1270 */
void do_assignments (void)
{
  while (true)
  {
    do {
      get_x_token();
    } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

    if (cur_cmd <= max_non_prefixed_command)
      return;

    set_box_allowed = false;
    prefixed_command();
    set_box_allowed = true;
  }
}
/* sec 1275 */
void open_or_close_in (void)
{
  char c;
  char n;

  c = cur_chr;
  scan_four_bit_int();
  n = cur_val;

  if (read_open[n] != closed)
  {
    a_close(read_file[n]);
    read_open[n] = closed;
  }

  if (c != 0)
  {
    scan_optional_equals();
    scan_file_name();
    pack_cur_name();

    if ((cur_ext != 335) && a_open_in(read_file[n]))
      read_open[n] = just_open;
    else if ((cur_ext != 785) && (name_length + 5 < file_name_size))
    {
      strncpy((char *) name_of_file + name_length + 1, ".tex ", 5);
      name_length = name_length + 4;

      if (a_open_in(read_file[n]))
        read_open[n] = just_open;
      else
      {
        name_length = name_length - 4;
        name_of_file[name_length + 1] = ' ';

        if ((cur_ext == 335) && a_open_in(read_file[n]))
          read_open[n] = just_open;
      }
    }
  }
}
/* sec 1279 */
void issue_message (void)
{
  char old_setting;
  char c;
  str_number s;

  c = cur_chr;
  link(garbage) = scan_toks(false, true);
  old_setting = selector;
  selector = new_string;
  token_show(def_ref);
  selector = old_setting;
  flush_list(def_ref);
  str_room(1);
  s = make_string();

  if (c == 0)
  {
    if (term_offset + length(s) > max_print_line - 2)
      print_ln();
    else if ((term_offset > 0) || (file_offset > 0))
      print_char(' ');

    slow_print(s);
    update_terminal();
  }
  else
  {
    print_err("");
    slow_print(s);

    if (err_help != 0)
      use_err_help = true;
    else if (long_help_seen)
      help1("(That was another \\errmessage.)");
    else
    {
      if (interaction < error_stop_mode)
        long_help_seen = true;

      help4("This error message was generated by an \\errmessage",
        "command, so I can't give any explicit help.",
        "Pretend that you're Hercule Poirot: Examine all clues,",
        "and deduce the truth by order and method.");
    }

    error();
    use_err_help = false;
  }

  flush_string();
}
/* sec 1288 */
void shift_case (void)
{
  pointer b;
  pointer p;
  halfword t;
  eight_bits c;

  b = cur_chr;
  p = scan_toks(false, false);
  p = link(def_ref);

  while (p != 0)
  {
    t = info(p); 

    if ((t < cs_token_flag + single_base) && !check_kanji(t))
    {
      c = t % max_char_val;

      if (equiv(b + c) != 0)
        info(p) = t - c + equiv(b + c);
    }

    p = link(p);
  }

  back_list(link(def_ref));
  free_avail(def_ref);
}
/* sec 1293 */
void show_whatever (void)
{
  pointer p;
  small_number t;
  int m;
  integer l;
  integer n;

  switch (cur_chr)
  {
    case show_lists:
      {
        begin_diagnostic();
        show_activities();
      }
      break;

    case show_box_code:
      {
        scan_register_num();
        fetch_box(p);
        begin_diagnostic();
        print_nl("> \\box");
        print_int(cur_val);
        print_char('=');

        if (p == 0)
          prints("void");
        else
          show_box(p);
      }
      break;

    case show_code:
      {
        get_token();

        if (interaction == error_stop_mode)
          wake_up_terminal();

        print_nl("> ");

        if (cur_cs != 0)
        {
          sprint_cs(cur_cs);
          print_char('=');
        }

        print_meaning();
        goto common_ending;
      }
      break;

    case show_mode:
      {
        print_nl("> ");

        if (auto_spacing > 0)
          prints("auto spacing mode; ");
        else
          prints("no auto spacing mode; ");

        print_nl("> ");

        if (auto_xspacing > 0)
          prints("auto xspacing mode; ");
        else
          prints("no auto xspacing mode; ");

        goto common_ending;
      }
      break;

    case show_groups:
      {
        begin_diagnostic();
        show_save_groups();
      }
      break;

    case show_ifs:
      {
        begin_diagnostic();
        print_nl("");
        print_ln();

        if (cond_ptr == null)
        {
          print_nl("### ");
          prints("no active conditionals");
        }
        else
        {
          p = cond_ptr;
          n = 0;

          do {
            incr(n);
            p = link(p);
          } while (!(p == null));

          p = cond_ptr;
          t = cur_if;
          l = if_line;
          m = if_limit;

          do {
            print_nl("### level ");
            print_int(n);
            prints(": ");
            print_cmd_chr(if_test, t);

            if (m == fi_code)
              print_esc("else");

            print_if_line(l);
            decr(n);
            t = subtype(p);
            l = if_line_field(p);
            m = type(p);
            p = link(p);
          } while (!(p == null));
        }
      }
      break;

    default:
      {
        p = the_toks();

        if (interaction == error_stop_mode)
          wake_up_terminal();

        print_nl("> ");
        token_show(temp_head);
        flush_list(link(temp_head));
        goto common_ending;
      }
      break;
  }

  end_diagnostic(true);
  print_err("OK");

  if (selector == term_and_log)
    if (tracing_online <= 0)
    {
      selector = term_only;
      prints(" (see the transcript file)");
      selector = term_and_log;
    }

common_ending:

  if (interaction < error_stop_mode)
  {
    help0();
    decr(error_count);
  }
  else if (tracing_online > 0)
  {
    help3("This isn't an error message; I'm just \\showing something.",
      "Type `I\\show...' to show more (e.g., \\show\\cs,",
      "\\showthe\\count10, \\showbox255, \\showlists).");
  }
  else
  {
    help5("This isn't an error message; I'm just \\showing something.",
      "Type `I\\show...' to show more (e.g., \\show\\cs,",
      "\\showthe\\count10, \\showbox255, \\showlists).",
      "And type `I\\tracingonline=1\\show...' to show boxes and",
      "lists on your terminal as well as in the transcript file.");
  }

  error();
}
/* sec 1349 */
void new_whatsit (small_number s, small_number w)
{
  pointer p;

  p = get_node(w);
  type(p) = whatsit_node;
  subtype(p) = s;
  link(tail) = p;
  tail = p;
}
/* sec 1350 */
void new_write_whatsit (small_number w)
{
  new_whatsit(cur_chr, w);

  if (w != write_node_size)
    scan_four_bit_int();
  else
  {
    scan_int();

    if (cur_val < 0)
      cur_val = 17;
    else if ((cur_val > 15) && (cur_val != 18))
      cur_val = 16;
  }

  write_stream(tail) = cur_val;
}
/* sec 1348 */
void do_extension (void)
{
  integer k;
  pointer p;

  switch (cur_chr)
  {
    case open_node:
      {
        new_write_whatsit(open_node_size);
        scan_optional_equals();
        scan_file_name();
        open_name(tail) = cur_name;
        open_area(tail) = cur_area;
        open_ext(tail) = cur_ext;
      }
      break;

    case write_node:
      {
        k = cur_cs;
        new_write_whatsit(write_node_size);
        cur_cs = k;
        p = scan_toks(false, false);
        write_tokens(tail) = def_ref;
      }
      break;

    case close_node:
      {
        new_write_whatsit(write_node_size);
        write_tokens(tail) = 0;
      }
      break;

    case special_node:
      {
        new_whatsit(special_node, write_node_size);
        write_stream(tail) = 0;
        p = scan_toks(false, true);
        write_tokens(tail) = def_ref;
      }
      break;

    case immediate_code:
      {
        get_x_token();

        if ((cur_cmd == extension) && (cur_chr <= close_node))
        {
          p = tail;
          do_extension();
          out_what(tail);
          flush_node_list(tail);
          tail = p;
          link(p) = 0;
        }
        else
          back_input();
      }
      break;

    case set_language_code:
      if (abs(mode) != hmode)
        report_illegal_case();
      else
      {
        new_whatsit(language_node, small_node_size);
        scan_int();

        if (cur_val <= 0)
          clang = 0;
        else if (cur_val > 255)
          clang = 0;
        else
          clang = cur_val;

        what_lang(tail) = clang;
        what_lhm(tail) = norm_min(left_hyphen_min);
        what_rhm(tail) = norm_min(right_hyphen_min);
      }
      break;

    default:
      {
        confusion("ext1");
        return;
      }
      break;
  }
}
/* sec 1376 */
void fix_language (void)
{
  /* ASCII_code l; */
  int l;

  if (language <= 0)
    l = 0; 
  else if (language > 255)
    l = 0;
  else
    l = language;

  if (l != clang)
  {
    new_whatsit(language_node, small_node_size);
    what_lang(tail) = l;
    clang = l;
    what_lhm(tail) = norm_min(left_hyphen_min);
    what_rhm(tail) = norm_min(right_hyphen_min);
  }
}
/* sec 1068 */
void handle_right_brace (void)
{
  pointer p, q;
  pointer r;
  scaled d;
  integer f;

  switch (cur_group)
  {
    case simple_group:
      unsave();
      break;

    case bottom_level:
      {
        print_err("Too many }'s");
        help2("You've closed more groups than you opened.",
          "Such booboos are generally harmless, so keep going.");
        error();
      }
      break;

    case semi_simple_group:
    case math_shift_group:
    case math_left_group:
      extra_right_brace();
      break;

    case hbox_group:
      {
        adjust_hlist(head, false);
        package(0);
      }
      break;

    case adjusted_hbox_group:
      {
        adjust_hlist(head, false);
        adjust_tail = adjust_head;
        package(0);
      }
      break;

    case vbox_group:
      {
        end_graf();
        package(0);
      }
      break;

    case vtop_group:
      {
        end_graf();
        package(vtop_code);
      }
      break;

    case insert_group:
      {
        end_graf();
        q = split_top_skip;
        add_glue_ref(q);
        d = split_max_depth;
        f = floating_penalty;
        unsave();
        decr(save_ptr);
        p = vpackage(link(head), 0, 1, max_dimen);
        set_box_dir(p, abs(direction));
        pop_nest();

        if (saved(0) < 255)
        {
          r = get_node(ins_node_size);
          type(r) = ins_node;
          subtype(r) = saved(0);
          height(r) = height(p) + depth(p);
          ins_ptr(r) = list_ptr(p);
          split_top_ptr(r) = q;
          depth(r) = d;
          float_cost(r) = f;
          ins_dir(r) = box_dir(p);

          if (!is_char_node(tail) && (type(tail) == disp_node))
            prev_append(r);
          else
            tail_append(r);
        }
        else
        {
          if (box_dir(p) != adjust_dir)
          {
            print_err("Direction Incompatible.");
            help1("\\vadjust's argument and outer vlist must have same direction.");
            error();
            flush_node_list(list_ptr(p));
          }
          else
          {
            r = get_node(small_node_size);
            type(r) = adjust_node;
            adjust_ptr(r) = list_ptr(p);
            delete_glue_ref(q);

            if (!is_char_node(tail) && (type(tail) == disp_node))
              prev_append(r);
            else
              tail_append(r);
          }
        }

        delete_glue_ref(space_ptr(p));
        delete_glue_ref(xspace_ptr(p));
        free_node(p, box_node_size);

        if (nest_ptr == 0)
          build_page();
      }
      break;

    case output_group:
      {
        if ((loc != 0) ||
          ((token_type != output_text) && (token_type != backed_up)))
        {
          print_err("Unbalanced output routine");
          help2("Your sneaky output routine has problematic {'s and/or }'s.",
            "I can't handle that very well; good luck.");
          error();

          do {
            get_token();
          } while (!(loc == 0));
        }

        end_token_list();
        end_graf();
        unsave();
        output_active = false;
        insert_penalties = 0;

        if (box(255) != 0)
        {
          print_err("Output routine didn't use all of ");
          print_esc("box");
          print_int(255);
          help3("Your \\output commands should empty \\box255,",
            "e.g., by saying `\\shipout\\box255'.",
            "Proceed; I'll discard its present contents.");
          box_error(255);
        }

        if (tail != head)
        {
          link(page_tail) = link(head);
          page_tail = tail;
        }

        if (link(page_head) != 0)
        {
          if (link(contrib_head) == 0)
            contrib_tail = page_tail;

          link(page_tail) = link(contrib_head);
          link(contrib_head) = link(page_head);
          link(page_head) = 0;
          page_tail = page_head;
        }

        flush_node_list(page_disc);
        page_disc = null;
        pop_nest();
        build_page();
      }
      break;

    case disc_group:
      build_discretionary();
      break;

    case align_group:
      {
        back_input();
        cur_tok = cs_token_flag + frozen_cr;
        print_err("Missing ");
        print_esc("cr");
        prints("inserted");
        help1("I'm guessing that you meant to end an alignment here.");
        ins_error();
      }
      break;

    case no_align_group:
      {
        end_graf();
        unsave();
        align_peek();
      }
      break;

    case vcenter_group:
      {
        end_graf();
        unsave();
        save_ptr = save_ptr - 2;
        p = vpackage(link(head), saved(1), saved(0), max_dimen);
        set_box_dir(p, abs(direction));
        pop_nest();

        if (box_dir(p) != abs(direction))
          p = new_dir_node(p, abs(direction));

        tail_append(new_noad());
        type(tail) = vcenter_noad;
        math_type(nucleus(tail)) = sub_box;
        info(nucleus(tail)) = p;
      }
      break;

    case math_choice_group:
      build_choices();
      break;

    case math_group:
      {
        unsave();
        decr(save_ptr);
        math_type(saved(0)) = sub_mlist;
        p = fin_mlist(0);
        info(saved(0)) = p;

        if (p != 0)
          if (link(p) == 0)
            if (type(p) == ord_noad)
            {
              if (math_type(subscr(p)) == 0)
                if ((math_type(supscr(p)) == 0) && (math_kcode(p) == null))
                {
                  mem[saved(0)].hh = mem[nucleus(p)].hh;
                  free_node(p, noad_size);
                }
            }
            else if (type(p) == accent_noad)
              if (saved(0) == nucleus(tail))
                if (type(tail) == ord_noad)
                {
                  q = head;

                  while (link(q) != tail)
                    q = link(q);

                  link(q) = p;
                  free_node(tail, noad_size);
                  tail = p;
                }
      }
      break;

    default:
      {
        confusion("rightbrace");
        return;
      }
      break;
  }
}
/* sec 1030 */
// governs \TeX's activities
void main_control (void) 
{
  integer t;
  KANJI_code cx;
  pointer kp;
  pointer gp, gq;
  scaled disp;
  boolean ins_kp;
  boolean bSuppress;

  if (every_job != 0)
    begin_token_list(every_job, every_job_text);

big_switch:
  get_x_token();

reswitch:
  if (interrupt != 0)
    if (OK_to_interrupt)
    {
      back_input();
      check_interrupt();
      goto big_switch;
    }

#ifdef NG_DEBUG
  if (panicking)
    check_mem(false);
#endif

  if (tracing_commands > 0)
    show_cur_cmd_chr();

  ins_kp = false;

  switch (abs(mode) + cur_cmd)
  {
    case hmode + letter:
    case hmode + other_char:
      goto main_loop;
      break;

    case hmode + kanji:
    case hmode + kana:
    case hmode + other_kchar:
    case hmode + hangul:
    case hmode + kchar_given:
      goto main_loop_j;
      break;

    case hmode + char_given:
      if (check_echar_range(cur_chr))
        goto main_loop;
      else
        goto main_loop_j;
      break;

    case hmode + char_num:
      {
        scan_char_num();
        cur_chr = cur_val;

        if (check_echar_range(cur_chr))
          goto main_loop;
        else
          goto main_loop_j;
      }
      break;

    case hmode + kchar_num:
      {
        scan_char_num();
        cur_chr = cur_val;
        goto main_loop_j;
      }
      break;

    case hmode + no_boundary:
      {
        get_x_token();

        if ((cur_cmd == letter) || (cur_cmd == other_char) ||
          ((cur_cmd >= kanji) && (cur_cmd <= hangul)) ||
          (cur_cmd == char_given) || (cur_cmd == char_num) ||
          (cur_cmd == kchar_given) || (cur_cmd == kchar_num))
          cancel_boundary = true;

        goto reswitch;
      }
      break;

    case hmode + spacer:
      if (space_factor == 1000)
        goto append_normal_space;
      else
        app_space();
      break;

    case hmode + ex_space:
    case mmode + ex_space:
      goto append_normal_space;
      break;

    case any_mode(relax):
    case vmode + spacer:
    case mmode + spacer:
    case mmode + no_boundary:
      do_nothing();
      break;

    case any_mode(ignore_spaces):
      {
        do {
          get_x_token();
        } while (!(cur_cmd != spacer));

        goto reswitch;
      }
      break;

    case vmode + stop:
      if (its_all_over())
        return;
      break;

    case vmode + vmove:
    case hmode + hmove:
    case mmode + hmove:
    case any_mode(last_item):
    case vmode + vadjust:
    case vmode + ital_corr:
    case non_math(eq_no):
    case any_mode(mac_param):
      report_illegal_case();
      break;

    case non_math(sup_mark):
    case non_math(sub_mark):
    case non_math(math_char_num):
    case non_math(math_given):
    case non_math(math_comp):
    case non_math(delim_num):
    case non_math(left_right):
    case non_math(above):
    case non_math(radical):
    case non_math(math_style):
    case non_math(math_choice):
    case non_math(vcenter):
    case non_math(non_script):
    case non_math(mkern):
    case non_math(limit_switch):
    case non_math(mskip):
    case non_math(math_accent):
    case mmode + endv:
    case mmode + par_end:
    case mmode + stop:
    case mmode + vskip:
    case mmode + un_vbox:
    case mmode + valign:
    case mmode + hrule:
      insert_dollar_sign();
      break;

    case vmode + hrule:
    case hmode + vrule:
    case mmode + vrule:
      {
        tail_append(scan_rule_spec());

        if (abs(mode) == vmode)
          prev_depth = ignore_depth;
        else if (abs(mode) == hmode)
          space_factor = 1000;
      }
      break;

    case vmode + vskip:
    case hmode + hskip:
    case mmode + hskip:
    case mmode + mskip:
      append_glue();
      break;

    case any_mode(kern):
    case mmode + mkern:
      append_kern();
      break;

    case non_math(left_brace):
      new_save_level(simple_group);
      break;

    case any_mode(begin_group):
      new_save_level(semi_simple_group);
      break;

    case any_mode(end_group):
      if (cur_group == semi_simple_group)
        unsave();
      else
        off_save();
      break;

    case any_mode(right_brace):
      handle_right_brace();
      break;

    case vmode + hmove:
    case hmode + vmove:
    case mmode + vmove:
      {
        t = cur_chr;
        scan_normal_dimen();

        if (t == 0)
          scan_box(cur_val);
        else
          scan_box(-cur_val);
      }
      break;

    case any_mode(leader_ship):
      scan_box(leader_flag - a_leaders + cur_chr);
      break;

    case any_mode(make_box):
      begin_box(0);
      break;

    case any_mode(chg_dir):
      {
        if (cur_group != align_group)
          if (head == tail)
          {
            direction = cur_chr;

            if (mode == vmode)
              page_dir = cur_chr;
          }
          else
          {
            print_err("Use `");
            print_cmd_chr(cur_cmd, cur_chr);
            prints("' at top of list");
            help2("Direction change command is available only while",
              "current list is null.");
            error();
          }
        else
        {
          print_err("You can't use `");
          print_cmd_chr(cur_cmd, cur_chr);
          prints("' in an align");
          help2("To change direction in an align,",
            "you shold use \\hbox or \\vbox with \\tate or \\yoko.");
          error();
        }
      }
      break;

    case vmode + start_par:
      new_graf(cur_chr > 0);
      break;

    case vmode + letter:
    case vmode + other_char:
    case vmode + char_num:
    case vmode + char_given:
    case vmode + kchar_num:
    case vmode + kchar_given:
    case vmode + math_shift:
    case vmode + un_hbox:
    case vmode + vrule:
    case vmode + accent:
    case vmode + discretionary:
    case vmode + hskip:
    case vmode + valign:
    case vmode + kanji:
    case vmode + kana:
    case vmode + other_kchar:
    case vmode + hangul:
    case vmode + ex_space:
    case vmode + no_boundary:
      {
        back_input();
        new_graf(true);
      }
      break;

    case hmode + start_par:
    case mmode + start_par:
      indent_in_hmode();
      break;

    case vmode + par_end:
      {
        normal_paragraph();

        if (mode > 0)
          build_page();
      }
      break;

    case hmode + par_end:
      {
        if (align_state < 0)
          off_save();

        end_graf();

        if (mode == vmode)
          build_page();
      }
      break;

    case hmode + stop:
    case hmode + vskip:
    case hmode + hrule:
    case hmode + un_vbox:
    case hmode + halign:
      head_for_vmode();
      break;

    case any_mode(insert):
    case hmode + vadjust:
    case mmode + vadjust:
      begin_insert_or_adjust();
      break;

    case any_mode(mark):
      make_mark();
      break;

    case any_mode(break_penalty):
      append_penalty();
      break;

    case any_mode(remove_item):
      delete_last();
      break;

    case vmode + un_vbox:
    case hmode + un_hbox:
    case mmode + un_hbox:
      unpackage();
      break;

    case hmode + ital_corr:
      append_italic_correction();
      break;

    case mmode + ital_corr:
      tail_append(new_kern(0));
      break;

    case hmode + discretionary:
    case mmode + discretionary:
      append_discretionary();
      break;

    case hmode + accent:
      make_accent();
      break;

    case any_mode(car_ret):
    case any_mode(tab_mark):
      align_error();
      break;

    case any_mode(no_align):
      no_align_error();
      break;

    case any_mode(omit):
      omit_error();
      break;

    case vmode + halign:
      init_align();
      break;

    case hmode + valign:
      if (cur_chr > 0)
      {
        if (eTeX_enabled(TeXXeT_en, cur_cmd, cur_chr))
          tail_append(new_math(0, cur_chr));
      }
      else
        init_align();
      break;

    case mmode + halign:
      if (privileged())
        if (cur_group == math_shift_group)
          init_align();
        else
          off_save();
      break;

    case vmode + endv:
    case hmode + endv:
      do_endv();
      break;

    case any_mode(end_cs_name):
      cs_error();
      break;

    case hmode + math_shift:
      init_math();
      break;

    case mmode + eq_no:
      if (privileged())
        if (cur_group == math_shift_group)
          start_eq_no();
        else
          off_save();
      break;

    case mmode + left_brace:
      {
        tail_append(new_noad());
        back_input();
        scan_math(nucleus(tail), kcode_noad(tail));
      }
      break;

    case mmode + letter:
    case mmode + other_char:
    case mmode + char_given:
      if (check_echar_range(cur_chr))
        if (cur_chr < 128)
          set_math_char(math_code(cur_chr));
        else
          set_math_char(cur_chr);
      else
        set_math_kchar(cur_chr);
      break;

    case mmode + kanji:
    case mmode + kana:
    case mmode + other_kchar:
    case mmode + hangul:
      {
        cx = cur_chr;
        set_math_kchar(KANJI(cx));
      }
      break;

    case mmode + char_num:
      {
        scan_char_num();
        cur_chr = cur_val;

        if (check_echar_range(cur_chr))
          if (cur_chr < 128)
            set_math_char(math_code(cur_chr));
          else
            set_math_char(cur_chr);
        else
          set_math_kchar(cur_chr);
      }
      break;

      case mmode + kchar_given:
        set_math_kchar(cur_chr);
        break;

      case mmode + kchar_num: 
        {
          scan_char_num();
          cur_chr = cur_val;
          set_math_kchar(cur_chr);
        }
        break;

    case mmode + math_char_num:
      {
        scan_fifteen_bit_int();
        set_math_char(cur_val);
      }
      break;

    case mmode + math_given:
      set_math_char(cur_chr);
      break;

    case mmode + delim_num:
      {
        scan_twenty_seven_bit_int();
        set_math_char(cur_val / 4096);
      }
      break;

    case mmode + math_comp:
      {
        tail_append(new_noad());
        type(tail) = cur_chr;
        scan_math(nucleus(tail), kcode_noad(tail));
      }
      break;

    case mmode + limit_switch:
      math_limit_switch();
      break;

    case mmode + radical:
      math_radical();
      break;

    case mmode + accent:
    case mmode + math_accent:
      math_ac();
      break;

    case mmode + vcenter:
      {
        scan_spec(vcenter_group, false);
        normal_paragraph();
        inhibit_glue_flag = false;
        push_nest();
        mode = -vmode;
        prev_depth = ignore_depth;

        if (every_vbox != 0)
          begin_token_list(every_vbox, every_vbox_text);
      }
      break;

    case mmode + math_style:
      tail_append(new_style(cur_chr));
      break;

    case mmode + non_script:
      {
        tail_append(new_glue(zero_glue));
        subtype(tail) = cond_math_glue;
      }
      break;

    case mmode + math_choice:
      append_choices();
      break;

    case mmode + sub_mark:
    case mmode + sup_mark:
      sub_sup();
      break;

    case mmode + above:
      math_fraction();
      break;

    case mmode + left_right:
      math_left_right();
      break;

    case mmode + math_shift:
      if (cur_group == math_shift_group)
        after_math();
      else
        off_save();
      break;

    case any_mode(assign_kinsoku):
    case any_mode(assign_inhibit_xsp_code):
    case any_mode(set_auto_spacing):
    case any_mode(set_enable_cjk_token):
    case any_mode(set_kansuji_char):
    case any_mode(toks_register):
    case any_mode(assign_toks):
    case any_mode(assign_int):
    case any_mode(def_jfont):
    case any_mode(def_tfont):
    case any_mode(assign_dimen):
    case any_mode(assign_glue):
    case any_mode(assign_mu_glue):
    case any_mode(assign_font_dimen):
    case any_mode(assign_font_int):
    case any_mode(set_aux):
    case any_mode(set_prev_graf):
    case any_mode(set_page_dimen):
    case any_mode(set_page_int):
    case any_mode(set_box_dimen):
    case any_mode(set_shape):
    case any_mode(def_code):
    case any_mode(def_family):
    case any_mode(set_font):
    case any_mode(def_font):
    case any_mode(tex_register):
    case any_mode(advance):
    case any_mode(multiply):
    case any_mode(divide):
    case any_mode(prefix):
    case any_mode(let):
    case any_mode(shorthand_def):
    case any_mode(read_to_cs):
    case any_mode(def):
    case any_mode(set_box):
    case any_mode(hyph_data):
    case any_mode(set_interaction):
      prefixed_command();
      break;

    case any_mode(after_assignment):
      {
        get_token();
        after_token = cur_tok;
      }
      break;

    case any_mode(after_group):
      {
        get_token();
        save_for_after(cur_tok);
      }
      break;

    case any_mode(in_stream):
      open_or_close_in();
      break;

    case any_mode(message):
      issue_message();
      break;

    case any_mode(case_shift):
      shift_case();
      break;

    case any_mode(xray):
      show_whatever();
      break;

    case any_mode(inhibit_glue):
      inhibit_glue_flag = true;
      break;

    case any_mode(extension):
      do_extension();
      break;
  }

  goto big_switch;

main_loop_j:
  append_kanji_to_hlist();

main_loop:
  inhibit_glue_flag = false;
  adjust_space_factor();

  if (direction == dir_tate)
    disp = t_baseline_shift;
  else
    disp = y_baseline_shift;

  append_disp_node_at_begin();
  main_f = cur_font;
  bchar = font_bchar[main_f];
  false_bchar = font_false_bchar[main_f];

  if (mode > 0)
    if (language != clang)
      fix_language();

  fast_get_avail(lig_stack);
  font(lig_stack) = main_f;
  cur_l = cur_chr;
  character(lig_stack) = cur_l;
  cur_q = tail;

  if (cancel_boundary)
  {
    cancel_boundary = false;
    main_k = non_address;
  }
  else
    main_k = bchar_label[main_f];

  if (main_k == non_address)
    goto main_loop_move_2;

  cur_r = cur_l;
  cur_l = non_char;
  goto main_lig_loop_1;

main_loop_wrapup: 
  wrapup(rt_hit);

main_loop_move:
  if (lig_stack == 0)
  {
    append_disp_node_at_end();
    goto reswitch;
  }

  cur_q = tail;
  cur_l = character(lig_stack);

main_loop_move_1:
  if (!is_char_node(lig_stack))
    goto main_loop_move_lig;

main_loop_move_2:
  if ((cur_chr < font_bc[main_f]) || (cur_chr > font_ec[main_f]))
  {
    char_warning(main_f, cur_chr);
    free_avail(lig_stack);
    goto big_switch;
  }

  main_i = char_info(main_f, cur_l);

  if (!char_exists(main_i))
  {
    char_warning(main_f, cur_chr);
    free_avail(lig_stack);
    goto big_switch; 
  }

  link(tail) = lig_stack;
  tail = lig_stack;

main_loop_lookahead:
  get_next();

  if (cur_cmd == letter)
    goto main_loop_lookahead_1;

  if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
  {
    goto_main_lig_loop();
  }

  if (cur_cmd == other_char)
    goto main_loop_lookahead_1;

  if (cur_cmd == char_given)
  {
    if (check_echar_range(cur_chr))
      goto main_loop_lookahead_1;
    else
      goto_main_lig_loop();
  }

  if (cur_cmd == kchar_given)
  {
    goto_main_lig_loop();
  }

  x_token();

  if (cur_cmd == letter)
    goto main_loop_lookahead_1;

  if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
  {
    goto_main_lig_loop();
  }

  if (cur_cmd == other_char)
    goto main_loop_lookahead_1;

  if (cur_cmd == char_given)
  {
    if (check_echar_range(cur_chr))
      goto main_loop_lookahead_1;
    else
      goto_main_lig_loop();
  }

  if (cur_cmd == char_num)
  {
    scan_char_num();
    cur_chr = cur_val;

    if (check_echar_range(cur_chr))
      goto main_loop_lookahead_1;
    else
      goto_main_lig_loop();
  }

  if (cur_cmd == kchar_num)
  {
    scan_char_num();
    cur_chr = cur_val;
    goto_main_lig_loop();
  }

  if (cur_cmd == inhibit_glue)
  {
    inhibit_glue_flag = true;
    goto main_loop_lookahead;
  }

  if (cur_cmd == no_boundary)
    bchar = non_char;

  cur_r = bchar;
  lig_stack = 0;
  goto main_lig_loop;

main_loop_lookahead_1:
  adjust_space_factor();
  inhibit_glue_flag = false;
  fast_get_avail(lig_stack);
  font(lig_stack) = main_f;
  cur_r = cur_chr;
  character(lig_stack) = cur_r;

  if (cur_r == false_bchar)
    cur_r = non_char;

main_lig_loop:
  if (char_tag(main_i) != lig_tag)
    goto main_loop_wrapup;

  if (cur_r == non_char)
    goto main_loop_wrapup;

  main_k = lig_kern_start(main_f, main_i);
  main_j = font_info[main_k].qqqq;

  if (skip_byte(main_j) <= stop_flag)
    goto main_lig_loop_2;

  main_k = lig_kern_restart(main_f, main_j);

main_lig_loop_1:
  main_j = font_info[main_k].qqqq;

main_lig_loop_2:
  bSuppress = false;

  if (suppress_f_ligs && next_char(main_j) == cur_r && op_byte(main_j) == no_tag)
  {
    if (cur_l == 'f')
      bSuppress = true;
  }

  if (next_char(main_j) == cur_r && bSuppress == false)
    if (skip_byte(main_j) <= stop_flag)
    {
      if (op_byte(main_j) >= kern_flag)
      {
        wrapup(rt_hit);
        tail_append(new_kern(char_kern(main_f, main_j)));
        goto main_loop_move;
      }

      if (cur_l == non_char)
        lft_hit = true;
      else if (lig_stack == 0)
        rt_hit = true;

      check_interrupt();

      switch (op_byte(main_j))
      {
        case 1:
        case 5:
          {
            cur_l = rem_byte(main_j);
            main_i = char_info(main_f, cur_l);
            ligature_present = true;
          }
          break;

        case 2:
        case 6:
          {
            cur_r = rem_byte(main_j);

            if (lig_stack == 0)
            {
              lig_stack = new_lig_item(cur_r);
              bchar = non_char;
            }
            else if (is_char_node(lig_stack))
            {
              main_p = lig_stack;
              lig_stack = new_lig_item(cur_r);
              lig_ptr(lig_stack) = main_p;
            }
            else
              character(lig_stack) = cur_r;
          }
          break;

        case 3:
          {
            cur_r = rem_byte(main_j);
            main_p = lig_stack;
            lig_stack = new_lig_item(cur_r);
            link(lig_stack) = main_p;
          }
          break;

        case 7:
        case 11:
          {
            wrapup(false);
            cur_q = tail;
            cur_l = rem_byte(main_j);
            main_i = char_info(main_f, cur_l);
            ligature_present = true;
          }
          break;

        default:
          {
            cur_l = rem_byte(main_j);
            ligature_present = true;
 
            if (lig_stack == 0)
              goto main_loop_wrapup;
            else
              goto main_loop_move_1;
          }
          break;
      }

      if (op_byte(main_j) > 4)
        if (op_byte(main_j) != 7)
          goto main_loop_wrapup;

      if (cur_l < non_char)
        goto main_lig_loop;

      main_k = bchar_label[main_f];
      goto main_lig_loop_1;
    }

    if (skip_byte(main_j) == 0)
      incr(main_k);
    else
    {
      if (skip_byte(main_j) >= stop_flag)
        goto main_loop_wrapup;

      main_k = main_k + skip_byte(main_j) + 1;
    }

    goto main_lig_loop_1;

main_loop_move_lig:
  main_p = lig_ptr(lig_stack);

  if (main_p != 0)
    tail_append(main_p);

  temp_ptr = lig_stack;
  lig_stack = link(temp_ptr);
  free_node(temp_ptr, small_node_size);
  main_i = char_info(main_f, cur_l);
  ligature_present = true;

  if (lig_stack == 0)
    if (main_p != 0)
      goto main_loop_lookahead;
    else
      cur_r = bchar;
  else
    cur_r = character(lig_stack);

  goto main_lig_loop;

append_normal_space:
  if (space_skip == zero_glue)
  {
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

    temp_ptr = new_glue(main_p);
  }
  else
    temp_ptr = new_param_glue(space_skip_code);

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    link(prev_node) = temp_ptr;
    link(temp_ptr) = tail;
    prev_node = temp_ptr;
  }
  else
  {
    link(tail) = temp_ptr;
    tail = temp_ptr;
  }

  goto big_switch;
}
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
  
    if (tex82_flag)
    {
      wake_up_terminal();
      printf("%s;%s\n", "Sorry, I can't find that format", " will try the default.");
    }
    else
    {
      name_of_file[name_length + 1] = '\0';
      printf("%s (%s);%s\n", "Sorry, I can't find that format", name_of_file + 1, " will try the default.");
      name_of_file[name_length + 1] = ' ';
      printf("(Perhaps your pTeX-ng's environment variable is not set correctly)\n");
    }

    update_terminal();
  }

  pack_buffered_name(format_default_length - 4, 1, 0);

  if (!w_open_in(fmt_file))
  {
    if (tex82_flag)
    {
      wake_up_terminal();
      printf("%s!\n", "I can't find the default format file");
    }
    else
    {
      name_of_file[name_length + 1] = '\0';
      printf("%s (%s)!\n", "I can't find the default format file", name_of_file + 1);
      name_of_file[name_length + 1] = ' ';
      printf("(Perhaps your pTeX-ng's environment variable is not set correctly)\n");
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
        wlog('s');

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

      log_printf("%c%lld%s%d\n", ' ', (cs_count), " multiletter control sequences out of ", hash_size);
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

      if (!tex82_flag)
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
}
#ifdef NG_DEBUG
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

pointer new_dir_node (pointer b, eight_bits dir)
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

void prev_append (pointer val)
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
void print_kansuji (integer n)
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
pointer get_inhibit_pos (KANJI_code c, small_number n)
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
pointer get_kinsoku_pos (KANJI_code c, small_number n)
{
  pointer p, s;

  s = calc_pos(c);
  p = s;

#ifdef NG_DEBUG
  print_ln();
  prints("c:=");
  print_int(c);
  prints(", p:=");
  print_int(s);

  if (p + kinsoku_base < 0)
  {
    prints("p is negative value");
    print_ln();
  }
#endif

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
void pdf_synch_dir (void)
{
  scaled tmp;

  switch (cur_dir_hv)
  {
    case dir_yoko:
      if (dvi_dir != cur_dir_hv)
      {
        pdf_synch_h();
        pdf_synch_v();

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
              cur_v = -cur_v;
              cur_h = -cur_h;
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
boolean check_box (pointer box_p)
{
  pointer p;
  boolean flag;

  flag = false;
  p = box_p;

  while (p != null)
  {
    if (is_char_node(p))
    {
      do {
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
      } while (!(!is_char_node(p)));
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
void adjust_hlist (pointer p, boolean pf)
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
      do {
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
      } while (!(!is_char_node(p)));
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

  if (!is_char_node(q) && (type(q) == glue_node) &&
    (subtype(q) == jfm_skip + 1))
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
// prints |dir| data
void print_dir (eight_bits dir)
{
  if (dir == dir_yoko)
    print_char('Y');
  else if (dir == dir_tate)
    print_char('T');
  else if (dir == dir_dtou)
    print_char('D');
}

void print_direction_alt (integer d)
{
  boolean x;

  x = false;

  switch (abs(d))
  {
    case dir_yoko:
      {
        prints(", yoko");
        x = true;
      }
      break;

    case dir_tate:
      {
        prints(", tate");
        x = true;
      }
      break;

    case dir_dtou:
      {
        prints(", dtou");
        x = true;
      }
      break;
  }

  if (x)
  {
    if (d < 0)
      prints("(math)");

    prints(" direction");
  }
}
// print the direction represented by d
void print_direction (integer d)
{
  switch (abs(d))
  {
    case dir_yoko:
      prints("yoko");
      break;

    case dir_tate:
      prints("tate");
      break;

    case dir_dtou:
      prints("dtou");
      break;
  }

  if (d < 0)
    prints("(math)");

  prints(" direction");
}
/* sec 1468 */
void set_math_kchar (integer c)
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
// prints a single character
void print_kanji (KANJI_code s)
{
  s = toBUFF(s % max_cjk_val);

  if (BYTE1(s) != 0)
    print_char(BYTE1(s));

  if (BYTE2(s) != 0)
    print_char(BYTE2(s));

  if (BYTE3(s) != 0)
    print_char(BYTE3(s));

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

// for eTeX
boolean eTeX_enabled (boolean b, quarterword j, halfword k)
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

void print_group (boolean e)
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

        if (cur_group == math_choice_group)
          prints(" choice");
        else if (cur_group == math_shift_group)
          prints(" shift");
        else if (cur_group == math_left_group)
          prints(" left");
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
void group_trace (boolean e)
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

void show_save_groups (void)
{
  int p;
  int m;
  pointer v;
  quarterword l;
  group_code c;
  int a;
  integer i;
  quarterword j;
  const char * s;

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

      if (p > 0)
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
          if (m == -vmode)
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

          if (p >= a)
            p = p - a;

          a = 0;
          goto found;
        }
        break;

      case no_align_group:
        {
          incr(p);
          a = -1;
          print_esc("noalign");
          goto found2;
        }
        break;

      case output_group:
        {
          print_esc("output");
          goto found;
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
            if (i <= saved(-2))
              prints("{}");
      
          goto found2;
        }
        break;

      case insert_group:
        {
          if (saved(-2) == 255)
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
          incr(p);
          print_esc("begingroup");
          goto found;
        }
        break;
        
      case math_shift_group:
        {
          if (m ==mmode)
            print_char('$');
          else if (nest[p].mode_field == mmode)
          {
            print_cmd_chr(eq_no, saved(-2));
            goto found;
          }
  
          print_char('$');
          goto found;
        }
        break;

      case math_left_group:
        {
          if (type(nest[p + 1].eTeX_aux_field) == left_noad)
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
    if (i < box_flag)
    {
      if (abs(nest[p].mode_field) == vmode)
        j = hmove;
      else
        j = vmove;
    
      if (i > 0)
        print_cmd_chr(j, 0);
      else
        print_cmd_chr(j, 1);
    
      print_scaled(abs(i));
      prints("pt");
    }
    else if (i < ship_out_flag)
    {
      if (i >= global_box_flag)
      {
        print_esc("global");
        i = i - (global_box_flag - box_flag);
      }

      print_esc("setbox");
      print_int(i - box_flag);
      print_char('=');
    }
    else
      print_cmd_chr(leader_ship, i - (leader_flag - a_leaders));

found1:
  print_esc(s);

  if (saved(-2) != 0)
  {
    print_char(' ');

    if (saved(-3) == exactly)
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

void scan_general_text (void)
{
  int s;
  pointer w;
  pointer d;
  pointer p;
  pointer q;
  halfword unbalance;

  s = scanner_status;
  w = warning_index;
  d = def_ref;
  scanner_status = absorbing;
  warning_index = cur_cs;
  def_ref = get_avail();
  token_ref_count(def_ref) = null;
  p = def_ref;
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
// create an edge nod
pointer new_edge (small_number s, scaled w)
{
  pointer p;

  p = get_node(edge_node_size);
  type(p) = edge_node;
  subtype(p) = s;
  width(p) = w;
  edge_dist(p) = 0;
  return p;
}

pointer reverse (pointer this_box, pointer t, scaled cur_g, real cur_glue)
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
  disp = revdisp;
  disped = false;
  l = t;
  p = temp_ptr;
  m = min_halfword;
  n = min_halfword;

  while (true)
  {
    while (p != null)
reswitch:
    if (is_char_node(p))
      do {
        f = font(p);
        c = character(p);
        cur_h = cur_h + char_width(f, char_info(f, c));

        if (font_dir[f] != dir_default)
        {
          q = link(p);
          la = l;
          l = p;
          p = link(q);
          link(q) = la;
        }
        else
        {
          q = link(p);
          link(p) = l;
          l = p;
          p = q;
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

                  if (m > min_halfword)
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

              if ((n > min_halfword) || (LR_dir(p) != cur_dir))
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
    p = get_node(small_node_size);
    type(p) = disp_node;
    disp_dimen(p) = disp;
    link(p) = l;
    return p;
  }
  else
    return l;
}
// create a segment node
pointer new_segment (small_number s, pointer f)
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

void just_copy (pointer p, pointer h, pointer t)
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

    while (words > 0)
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

void just_reverse (pointer p)
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
        p = q;
        q = link(p);
        link(p) = l;
        l = p;
      } while (!(!is_char_node(q)));
    else
    {
      p = q;
      q = link(p);

      if (type(p) == math_node)
        adjust_the_LR_stack_j();

      link(p) = l;
      l = p;
    }

  goto done;

found:
  width(t) = width(p);
  link(t) = q;
  free_node(p, small_node_size);

done:
  link(temp_head) = l;
}

void app_display (pointer j, pointer b, scaled d)
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
    z = display_width;
    p = b;

    if (x > 0)
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
      s = s - shift_amount(b);
      d = d + s;
      e = e + width(b) - z - s;
    }

    if (box_lr(p) == dlist)
      q = p;
    else
    {
      r = list_ptr(p);
      free_node(p, box_node_size);

      if (r == null)
        confusion("LR4");

      if (x > 0)
      {
        p = r;

        do {
          q = r;
          r = link(r);
        } while (!(r == null));
      }
      else
      {
        p = null;
        q = r;

        do {
          t = link(r);
          link(r) = p;
          p = r;
          r = t;
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
      width(r) = d;
      link(r) = p;
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

void pseudo_start (void)
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
  str_room(1);
  s = make_string();
  str_pool[pool_ptr] = ' ';
  l = str_start[s];
  nl = new_line_char;
  p = get_avail();
  q = p;

  while (l < pool_ptr)
  {
    m = l;

    while ((l < pool_ptr) && (str_pool[l] != nl))
      incr(l);

    sz = (l - m + 7) / 4;

    if (sz == 1)
      sz = 2;

    r = get_node(sz);
    link(q) = r;
    q = r;
    info(q) = sz;

    while (sz > 2)
    {
      decr(sz);
      incr(r);
      w.b0 = str_pool[m];
      w.b1 = str_pool[m + 1];
      w.b2 = str_pool[m + 2];
      w.b3 = str_pool[m + 3];
      mem[r].qqqq = w;
      m = m + 4;
    }

    w.b0 = ' ';
    w.b1 = ' ';
    w.b2 = ' ';
    w.b3 = ' ';

    if (l > m)
    {
      w.b0 = str_pool[m];

      if (l > m + 1)
      {
        w.b1 = str_pool[m + 1];

        if (l > m + 2)
        {
          w.b2 = str_pool[m + 2];

          if (l > m + 3)
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
  line = 0; limit = start;
  loc = limit + 1;

  if (tracing_scan_tokens > 0)
  {
    if (term_offset > max_print_line - 3)
      print_ln();
    else if ((term_offset > 0) || (file_offset > 0))
      print_char(' ');

    name = 19;
    prints("( ");
    incr(open_parens);
    update_terminal();
  }
  else
    name = 18;
}

boolean pseudo_input (void)
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
      buffer[last] = w.b0;
      buffer[last + 1] = w.b1;
      buffer[last + 2] = w.b2;
      buffer[last + 3] = w.b3;
      last = last + 4;
    }

    if (last >= max_buf_stack)
      max_buf_stack = last + 1;

    while ((last > first) && (buffer[last - 1] == ' '))
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
    p = q;
    q = link(p);
    free_node(p, info(p));
  }
}

void get_x_or_protected (void)
{
  while (true)
  {
    get_token();

    if (cur_cmd <= max_command)
      return;

    if ((cur_cmd >= call) && (cur_cmd < end_template))
      if (info(link(cur_chr)) == protected_token)
        return;

    expand();
  }
}

void group_warning (void)
{
  int i;
  boolean w;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  i = in_open;
  w = false;

  while ((grp_stack[i] == cur_boundary) && (i > 0))
  {
    if (tracing_nesting>0)
    {
      while ((input_stack[base_ptr].state_field == token_list) ||
        (input_stack[base_ptr].index_field > i))
        decr(base_ptr);

      if (input_stack[base_ptr].name_field > 17)
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

void if_warning (void)
{
  int i;
  boolean w;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  i = in_open;
  w = false;

  while (if_stack[i] == cond_ptr)
  {
    if (tracing_nesting > 0)
    {
      while ((input_stack[base_ptr].state_field == token_list) ||
        (input_stack[base_ptr].index_field > i))
        decr(base_ptr);

      if (input_stack[base_ptr].name_field > 17)
        w = true;
    }

    if_stack[i] = link(cond_ptr);
    decr(i);
  }

  if (w)
  {
    print_nl("Warning: end of ");
    print_cmd_chr(if_test, cur_if);
    print_if_line(if_line);
    prints(" of a different file");
    print_ln();

    if (tracing_nesting > 1)
      show_context();

    if (history == spotless)
      history = warning_issued;
  }
}

void file_warning (void)
{
  pointer p;
  quarterword l;
  quarterword c;
  integer i;

  p = save_ptr;
  l = cur_level;
  c = cur_group;
  save_ptr = cur_boundary;

  while (grp_stack[in_open] != save_ptr)
  {
    decr(cur_level);
    print_nl("Warning: end of file when ");
    print_group(true);
    prints(" is incomplete");
    cur_group = save_level(save_ptr);
    save_ptr = save_index(save_ptr);
  }

  save_ptr = p;
  cur_level = l;
  cur_group = c;
  p = cond_ptr;
  l = if_limit;
  c = cur_if;
  i = if_line;

  while (if_stack[in_open] != cond_ptr)
  {
    print_nl("Warning: end of file when ");
    print_cmd_chr(if_test, cur_if);

    if (if_limit == fi_code)
      print_esc("else");

    print_if_line(if_line);
    prints(" is incomplete");
    if_line = if_line_field(cond_ptr);
    cur_if = subtype(cond_ptr);
    if_limit = type(cond_ptr);
    cond_ptr = link(cond_ptr);
  }

  cond_ptr = p;
  if_limit = l;
  cur_if = c;
  if_line = i;
  print_ln();

  if (tracing_nesting>1)
    show_context();

  if (history == spotless)
    history = warning_issued;
}

void scan_expr (void)
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

  l = cur_val_level;
  a = arith_error;
  b = false;
  p = null;

restart:
  r = expr_none;
  e = 0;
  s = expr_none;
  t = 0;
  n = 0;

continu:
  if (s == expr_none)
    o = l;
  else
    o = int_val;

  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok == other_token + '(')
  {
    q = get_node(expr_node_size);
    link(q) = p;
    type(q) = l;
    subtype(q) = 4 * s + r;
    expr_e_field(q) = e;
    expr_t_field(q) = t;
    expr_n_field(q) = n;
    p = q;
    l = o;
    goto restart;
  }

  back_input();

  if (o == int_val)
    scan_int();
  else if (o == dimen_val)
    scan_normal_dimen();
  else if (o == glue_val)
    scan_normal_glue();
  else
    scan_mu_glue();

  f = cur_val;

found:
  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok == other_token + '+')
    o = expr_add;
  else if (cur_tok == other_token + '-')
    o = expr_sub;
  else if (cur_tok == other_token + '*')
    o = expr_mult;
  else if (cur_tok == other_token + '/')
    o = expr_div;
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
    if ((f > infinity) || (f < -infinity))
      num_error(f);
  }
  else if (l == dimen_val)
  {
    if (abs(f) > max_dimen)
      num_error(f);
  }
  else
  {
    if ((abs(width(f)) > max_dimen) ||
      (abs(stretch(f)) > max_dimen) ||
      (abs(shrink(f)) > max_dimen))
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

  if (o > expr_sub)
    s = o;
  else
  {
    s = expr_none;

    if (r == expr_none)
      e = t;
    else if (l == int_val)
      e = expr_add_sub(e, t, infinity);
    else if (l == dimen_val)
      e = expr_a(e, t);
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
    e = expr_e_field(q);
    t = expr_t_field(q);
    n = expr_n_field(q);
    s = subtype(q) / 4;
    r = subtype(q) % 4;
    l = type(q);
    p = link(q);
    free_node(q, expr_node_size);
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
      e = zero_glue;
      add_glue_ref(e);
    }
    else
      e = 0;
  }

  arith_error = a;
  cur_val = e;
  cur_val_level = l;
}

void scan_normal_glue (void)
{
  scan_glue(glue_val);
}

void scan_mu_glue (void)
{
  scan_glue(mu_val);
}

integer add_or_sub (integer x, integer y, integer max_answer, boolean negative)
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

integer quotient (integer n, integer d)
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

    a = n / d;
    n = n - a * d;
    d = n - d;

    if (d + n >= 0)
      incr(a);

    if (negative)
      negate(a);
  }

  return a;
}

integer fract (integer x, integer n, integer d, integer max_answer)
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
    negate(d);
    negative = true;
  }

  if (x < 0)
  {
    negate(x);
    negative = !negative;
  }
  else if (x == 0)
    goto done;

  if (n < 0)
  {
    negate(n);
    negative = !negative;
  }

  t = n / d;

  if (t > max_answer / x)
    goto too_big;

  a = t * x;
  n = n - t * d;

  if (n == 0)
    goto found;

  t = x / d;

  if (t > (max_answer - a) / n)
    goto too_big;

  a = a + t * n;
  x = x - t * d;

  if (x == 0)
    goto found;

  if (x < n)
  {
    t = x;
    x = n;
    n = t;
  }

  f = 0;
  r = (d / 2) - d;
  h = -r;

  while (true)
  {
    if (odd(n))
    {
      r = r + x;

      if (r >= 0)
      {
        r = r - d;
        incr(f);
      }
    }

    n = n / 2;

    if (n == 0)
      goto found1;

    if (x < h)
      x = x + x;
    else
    {
      t = x - d;
      x = t + x;
      f = f + n;

      if (x < n)
      {
        if (x == 0)
          goto found1;

        t = x;
        x = n;
        n = t;
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

void scan_register_num (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > max_reg_num))
  {
    print_err("Bad register code");
    help2(max_reg_help_line, "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void new_index (quarterword i, pointer q)
{
  small_number k;

  cur_ptr = get_node(index_node_size);
  sa_index(cur_ptr) = i;
  sa_used(cur_ptr) = 0;
  link(cur_ptr) = q;

  for (k = 1; k <= index_node_size - 1; k++)
    mem[cur_ptr + k] = sa_null;
}

void find_sa_element (small_number t, halfword n, boolean w)
{
  pointer q;
  small_number i;

  cur_ptr = sa_root[t];
  if_cur_ptr_is_null_then_return_or_goto(not_found);
  q = cur_ptr;
  i = hex_dig1(n);
  get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found1);
  q = cur_ptr;
  i = hex_dig2(n);
  get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found2);
  q = cur_ptr;
  i = hex_dig3(n);
  get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found3);
  q = cur_ptr;
  i = hex_dig4(n);
  get_sa_ptr();

  if ((cur_ptr == null) && w)
    goto not_found4;

  goto exit;

not_found:
  new_index(t, null);
  sa_root[t] = cur_ptr;
  q = cur_ptr;
  i = hex_dig1(n);

not_found1:
  new_index(i, q);
  add_sa_ptr();
  q = cur_ptr;
  i = hex_dig2(n);

not_found2:
  new_index(i, q);
  add_sa_ptr();
  q = cur_ptr;
  i = hex_dig3(n);

not_found3:
  new_index(i, q);
  add_sa_ptr();
  q = cur_ptr;
  i = hex_dig4(n);

not_found4:
  if (t == mark_val)
  {
    cur_ptr = get_node(mark_class_node_size);
    mem[cur_ptr + 1] = sa_null;
    mem[cur_ptr + 2] = sa_null;
    mem[cur_ptr + 3] = sa_null;
  }
  else
  {
    if (t <= dimen_val)
    {
      cur_ptr = get_node(word_node_size);
      sa_int(cur_ptr) = 0;
      sa_num(cur_ptr) = n;
    }
    else
    {
      cur_ptr = get_node(pointer_node_size);

      if (t <= mu_val)
      {
        sa_ptr(cur_ptr) = zero_glue;
        add_glue_ref(zero_glue);
      }
      else
        sa_ptr(cur_ptr) = null;
    }

    sa_ref(cur_ptr) = null;
  }

  sa_index(cur_ptr) = 16 * t + i;
  sa_lev(cur_ptr) = level_one;
  link(cur_ptr) = q;
  add_sa_ptr();
exit:;
}

void delete_sa_ref (pointer q)
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
    p = q;
    q = link(p);
    free_node(p, s);

    if (q == null)
    {
      sa_root[i] = null;
      return;
    }

    delete_sa_ptr(); s = index_node_size;
  } while (!(sa_used(q) > 0));
}

void print_sa_num (pointer q)
{
  halfword n;

  if (sa_index(q) < dimen_val_limit)
    n = sa_num(q);
  else
  {
    n = hex_dig4(sa_index(q));
    q = link(q);
    n = n + 16 * sa_index(q);
    q = link(q);
    n = n + 256 * (sa_index(q) + 16 * sa_index(link(q)));
  }

  print_int(n);
}

#ifdef STAT
void show_sa (pointer p, const char * s)
{
  small_number t;

  begin_diagnostic();
  print_char('{');
  prints(s);
  print_char(' ');

  if (p == null)
    print_char('?');
  else
  {
    t = sa_type(p);

    if (t < box_val)
      print_cmd_chr(tex_register, p);
    else if (t == box_val)
    {
      print_esc("box");
      print_sa_num(p);
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
      print_scaled(sa_dim(p));
      prints("pt");
    }
    else
    {
      p = sa_ptr(p);

      if (t == glue_val)
        print_spec(p, "pt");
      else if (t == mu_val)
        print_spec(p, "mu");
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
      else
        print_char('?');
    }
  }

  print_char('}');
  end_diagnostic(false);
}
#endif

boolean do_marks (small_number a, small_number l, pointer q)
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
          else
            add_token_ref(sa_bot_mark(q));

          sa_top_mark(q) = sa_bot_mark(q);
        }
        break;

      case fire_up_done:
        if ((sa_top_mark(q) != null) && (sa_first_mark(q) == null))
        {
          sa_first_mark(q) = sa_top_mark(q);
          add_token_ref(sa_top_mark(q));
        }
        break;

      case destroy_marks:
        for (i = top_mark_code; i <= split_bot_mark_code; ++i)
        {
          get_sa_ptr();

          if (cur_ptr != null)
          {
            delete_token_ref(cur_ptr);
            put_sa_ptr(null);
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

void sa_save (pointer p)
{
  pointer q;
  quarterword i;

  if (cur_level != sa_level)
  {
    check_full_save_stack();
    save_type(save_ptr) = restore_sa;
    save_level(save_ptr) = sa_level;
    save_index(save_ptr) = sa_chain;
    incr(save_ptr);
    sa_chain = null;
    sa_level = cur_level;
  }

  i = sa_index(p);

  if (i < dimen_val_limit)
  {
    if (sa_int(p) == 0)
    {
      q = get_node(pointer_node_size);
      i = tok_val_limit;
    }
    else
    {
      q = get_node(word_node_size);
      sa_int(q) = sa_int(p);
    }

    sa_ptr(q) = null;
  }
  else
  {
    q = get_node(pointer_node_size);
    sa_ptr(q) = sa_ptr(p);
  }

  sa_loc(q) = p;
  sa_index(q) = i;
  sa_lev(q) = sa_lev(p);
  link(q) = sa_chain;
  sa_chain = q;
  add_sa_ref(p);
}

void sa_destroy (pointer p)
{
  if (sa_index(p) < mu_val_limit)
    delete_glue_ref(sa_ptr(p));
  else if (sa_ptr(p) != null)
  if (sa_index(p) < box_val_limit)
    flush_node_list(sa_ptr(p));
  else
    delete_token_ref(sa_ptr(p));
}

void sa_def (pointer p, halfword e)
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

    sa_lev(p) = cur_level;
    sa_ptr(p) = e;

#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "into");
#endif
  }

  delete_sa_ref(p);
}

void sa_w_def (pointer p, integer w)
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

    sa_lev(p) = cur_level;
    sa_int(p) = w;

#ifdef STAT 
    if (tracing_assigns > 0)
      show_sa(p, "into");
#endif
  }

  delete_sa_ref(p);
}

void gsa_def (pointer p, halfword e)
{
  add_sa_ref(p);

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "globally changing");
#endif

  sa_destroy(p);
  sa_lev(p) = level_one;
  sa_ptr(p) = e;

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "into");
#endif

  delete_sa_ref(p);
}

void gsa_w_def (pointer p, integer w)
{
  add_sa_ref(p);

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "globally changing");
#endif

  sa_lev(p) = level_one;
  sa_int(p) = w;

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
      if (tracing_restores > 0)
        show_sa(p, "retaining");
#endif
    }
    else
    {
      if (sa_index(p) < dimen_val_limit)
        if (sa_index(sa_chain) < dimen_val_limit)
          sa_int(p) = sa_int(sa_chain);
        else
          sa_int(p) = 0;
      else
      {
        sa_destroy(p);
        sa_ptr(p) = sa_ptr(sa_chain);
      }

      sa_lev(p) = sa_lev(sa_chain);

#ifdef STAT
      if (tracing_restores > 0)
        show_sa(p, "restoring");
#endif
    }

    delete_sa_ref(p);
    p = sa_chain;
    sa_chain = link(p);

    if (sa_index(p) < dimen_val_limit)
      free_node(p, word_node_size);
    else
      free_node(p, pointer_node_size);
  } while (!(sa_chain == null));
}