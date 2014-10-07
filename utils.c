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

/* sec 0016 */
void do_nothing(void)
{
  /* todo */
}
void wake_up_terminal (void)
{
  /* todo */
}
/* sec 0034 */
void update_terminal(void)
{
  fflush(stdout);
}
/* sec 0042 */
void append_char (ASCII_code c)
{
  str_pool[pool_ptr] = c;
  incr(pool_ptr);
}
/* sec 0042 */
void str_room(int val)
{
#ifdef ALLOCATESTRING
  if (pool_ptr + val > current_pool_size)
    str_pool = realloc_str_pool(increment_pool_size);

  if (pool_ptr + val > current_pool_size)
    overflow("pool size", current_pool_size - init_pool_ptr);
#else
  if (pool_ptr + val > pool_size)
    overflow("pool size", pool_size - init_pool_ptr);
#endif
}
/* sec 0044 */
void flush_string (void)
{
  decr(str_ptr);
  pool_ptr = str_start[str_ptr];
}
/* sec 0048 */
void append_lc_hex (ASCII_code c)
{
  if (c < 10)
    append_char(c + '0');
  else
    append_char(c - 10 + 'a');
}
/* sec 0056 */
/* sec 0073 */
void print_err (const char * s)
{
  if (interaction == error_stop_mode)
    wake_up_terminal();
  
  print_nl("! ");
  prints(s);
}
/* sec 0071 */
void prompt_input(const char * s)
{
  wake_up_terminal();
  prints(s);
  term_input();
}
/* sec 0079 */
void tex_help (unsigned int n, ...)
{
  int i;
  va_list help_arg;

  if (n > 6)
    n = 6;

  help_ptr = n;
  va_start(help_arg, n);

  for (i = n - 1; i > -1; --i)
    help_line[i] = va_arg(help_arg, char *);

  va_end(help_arg);
}
/* sec 0093 */
void succumb (void)
{
  if (interaction == error_stop_mode)
    interaction = scroll_mode;

  if (log_opened)
    error();

#ifdef DEBUG
  if (interaction > 0)
    debug_help();
#endif

  history = error_stop_mode;
  jump_out();
}
/* sec 0121 */
void free_avail_(halfword p)
{
  link(p) = avail;
  avail = p;
#ifdef STAT
  decr(dyn_used);
#endif
}
/* sec 0180 */
void node_list_display (integer p)
{
  append_char('.');
  show_node_list(p);
  decr(pool_ptr);
}
/* sec 0214 */
void tail_append_ (pointer val)
{
  link(tail) = val;
  tail = link(tail);
}
/* sec 0273 */
void check_full_save_stack(void)
{
  if (save_ptr > max_save_stack)
  {
    max_save_stack = save_ptr;

#ifdef ALLOCATESAVESTACK
    if (max_save_stack > current_save_size - 7)
      save_stack = realloc_save_stack(increment_save_size);

    if (max_save_stack > current_save_size - 7)
    {
      overflow("save size", current_save_size);
      return;
    }
#else
    if (max_save_stack > save_size - 7)
    {
      overflow("save size", save_size);
      return;
    }
#endif
  }
}
/* sec 0321 */
void push_input(void)
{
  if (input_ptr > max_in_stack)
  {
    max_in_stack = input_ptr;

#ifdef ALLOCATEINPUTSTACK
    if (input_ptr == current_stack_size)
      input_stack = realloc_input_stack(increment_stack_size);
    
    if (input_ptr == current_stack_size)
    {
      overflow("input stack size", current_stack_size);
      return;
    }
#else
    if (input_ptr == stack_size)
    {
      overflow("input stack size", stack_size);
      return;
    }
#endif
  }
  
  input_stack[input_ptr] = cur_input;
  incr(input_ptr);
}
/* sec 0322 */
void pop_input(void)
{
  decr(input_ptr);
  cur_input = input_stack[input_ptr];
}
/* sec 0532 */
void ensure_dvi_open (void)
{
  if (output_file_name == 0)
  {
    if (job_name == 0)
      open_log_file();

    pack_job_name(".dvi");

    while (!b_open_out(dvi_file))
      prompt_file_name("file name for output", ".dvi");

    output_file_name = b_make_name_string(dvi_file);
  }
}
/* sec 0564 */
void fget (void)
{
  fbyte = getc(tfm_file);
}
/* sec 0597 */
void write_dvi(size_t a, size_t b)
{
  if (fwrite((char *)&dvi_buf[a], sizeof(dvi_buf[a]),
    (b - a + 1), dvi_file) != (b - a + 1))
    FATAL_PERROR("\n! dvi file");
}
/* sec 0598 */
void dvi_out_(ASCII_code op)
{
  dvi_buf[dvi_ptr] = op;
  incr(dvi_ptr);

  if (dvi_ptr == dvi_limit)
    dvi_swap();
}
/* sec 0616 */
void synch_h (void)
{
  if (cur_h != dvi_h)
  {
    movement(cur_h - dvi_h, right1);
    dvi_h = cur_h;
  }
}
/* sec 0616 */
void synch_v (void)
{
  if (cur_v != dvi_v)
  {
    movement(cur_v - dvi_v, down1);
    dvi_v = cur_v;
  }
}
/* sec 0934 */
void set_cur_lang (void)
{
  if (language <= 0)
    cur_lang = 0;
  else if (language > 255)
    cur_lang = 0;
  else
    cur_lang = language;
}
/* sec 0985 */
void print_plus (int i, const char * s)
{
  if (page_so_far[i] != 0)
  {
    prints(" plus ");
    print_scaled(page_so_far[i]);
    prints(s);
  }
}
// eTeX
void print_if_line(integer val)
{
  if (val != 0)
  {
    prints(" entered on line ");
    print_int(val);
  }
}