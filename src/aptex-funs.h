/*
   Copyright 2014-2024 Clerk Ma

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

#ifndef APTEX_FUNCTIONS_H
#define APTEX_FUNCTIONS_H

// functions of reallocation
static packed_ASCII_code *  realloc_str_pool (int size);
static pool_pointer *       realloc_str_start (int size);
static memory_word *        realloc_save_stack (int size);
static list_state_record *  realloc_nest_stack (int size);
static in_state_record *    realloc_input_stack (int size);
static halfword *           realloc_param_stack (int size);
static ASCII_code *         realloc_buffer (int size);
static memory_word *        realloc_font_info (int size);
static int                  realloc_hyphen (int hyphen_prime);

// functions of string pool
static str_number make_str_string (const char * s);
static char *     take_str_string (str_number s);

// functions of I/O
static boolean b_open_output (byte_file * f);

#define a_open_in(f)    a_open_input(&(f))
#define b_open_in(f)    b_open_input(&(f))
#define w_open_in(f)    w_open_input(&(f))
//
#define a_open_out(f)   a_open_output(&(f))
#define b_open_out(f)   b_open_output(&(f))
#define w_open_out(f)   w_open_output(&(f))
//
#define w_eof(f)        (aptex_env.flag_compact_fmt == true ? gzeof((gzFile) f) : feof((FILE *) f))

// functions of SyncTeX
void synctex_init (void);
void synctex_terminate (void);
void synctex_start_input (void);
void synctex_sheet (integer sync_mag);
void synctex_teehs (void);
void synctex_vlist (pointer this_box);
void synctex_tsilv (pointer this_box);
void synctex_void_vlist (halfword p, halfword this_box);
void synctex_hlist (halfword this_box);
void synctex_tsilh (halfword this_box);
void synctex_void_hlist (halfword p, halfword this_box);
void synctex_math (halfword p, halfword this_box);
void synctex_horizontal_rule_or_glue (halfword p, halfword this_box);
void synctex_kern (halfword p, halfword this_box);
void synctex_char (halfword p, halfword this_box);
void synctex_node (halfword p, halfword this_box);
void synctex_current (void);

// functions of TeX82
void print_ln (void);
void print_char (ASCII_code s);
void print_(integer s);
#define print(s) print_((integer) (s))
void prints_(const char * s);
#define prints(s) prints_((const char *) s)
void slow_print (integer s);
void print_nl (const char * s);
void print_esc (const char * s);
void sprint_esc (str_number s);
void print_int (integer n);
void print_file_name (integer n, integer a, integer e);
void jump_out (void);
void error (void);
void overflow (const char * s, integer n);

boolean init_terminal (void);
str_number make_string (void);

void term_input (void);
void normalize_selector (void);
void pause_for_instructions (void);
void print_scaled (scaled s);

void show_token_list (integer p, integer q, integer l);

void flush_list (pointer p);
pointer get_node (integer s);
void free_node (pointer p, halfword s);

#ifdef APTEX_DEBUG
void check_mem (boolean print_locs);
void search_mem (pointer p);
#endif

void show_node_list (integer p);
void delete_token_ref (pointer p);
void delete_glue_ref (pointer p);
#define fast_delete_glue_ref(p) delete_glue_ref((pointer) (p))
void flush_node_list (pointer p);
void print_cmd_chr (quarterword cmd, halfword chr_code);
pointer id_lookup (integer j, integer l);
#define primitive(s, c, o) primitive_(make_str_string((const char *) s), (quarterword) (c), (halfword) (o))

void assign_trace (pointer p, const char * s);

void show_context (void);
void end_token_list (void);
void back_input (void);
void begin_file_reading (void);
void end_file_reading (void);
void clear_for_error_prompt (void);
void get_next (void);
void get_token (void);
void get_x_token (void);
void scan_left_brace (void);

void scan_int (void);
void scan_dimen (boolean mu, boolean inf, boolean shortcut);
void scan_glue (small_number level);
void ins_the_toks (void);
void conv_toks (void);
pointer scan_toks (boolean macro_def, boolean xpand);
void pass_text (void);
void conditional (void);
void pack_file_name (str_number n, str_number a, str_number e);
str_number b_make_name_string (byte_file f);
str_number w_make_name_string (word_file f);
void pack_job_name_(str_number s);
#define pack_job_name(s) pack_job_name_(make_str_string((const char *) (s)))
void prompt_file_name_(const char * s, str_number e);
#define prompt_file_name(s, e) prompt_file_name_((const char *) s, make_str_string((const char*) e))
void open_log_file (void);
void start_input (void);
void dvi_swap (void);

void movement (scaled w, eight_bits o);
void hlist_out (void);
void vlist_out (void);

void prune_movements (integer l);
void out_what (pointer p);
void show_info (void);

void mlist_to_hlist (void);
void align_peek (void);
pointer finite_shrink (pointer p);
void try_break (integer pi, small_number breaktype);
void post_line_break (boolean d);
void hyphenate (void);
void print_totals (void);
void build_page (void);
void normal_paragraph (void);
void resume_after_display (void);
void get_r_token (void);
void trap_zero_glue (void);
void do_register_command (small_number a);
void alter_aux (void);
void alter_prev_graf (void);
void alter_page_so_far (void);
void alter_integer (void);
void alter_box_dimen (void);
void new_font (small_number a);
void new_interaction (void);
void do_assignments (void);
void main_control (void);
void give_err_help (void);
boolean open_fmt_file (void);
void close_files_and_terminate (void);

#ifdef APTEX_DEBUG
void debug_help (void);
#endif

// functions of pTeX and upTeX
pointer new_dir_node (pointer b, eight_bits dir);
eight_bits get_jfm_pos (KANJI_code kcode, internal_font_number f);

pointer get_inhibit_pos (KANJI_code c, small_number n);
pointer get_kinsoku_pos (KANJI_code c, small_number n);

boolean check_box (pointer box_p);
void adjust_hlist(pointer p, boolean pf);

void dir_out(void);
void set_math_kchar(integer c);
void print_kanji(KANJI_code s);
void change_page_direction(halfword d);
boolean check_kcat_code(integer ct);
boolean check_echar_range(integer c);

// functions of eTeX
boolean eTeX_enabled (boolean b, quarterword j, halfword k);
void group_trace (boolean e);
void show_save_groups (void);
void scan_general_text (void);

static inline void print_if_line (integer val)
{
  if (val != 0)
  {
    prints(" entered on line ");
    print_int(val);
  }
}

pointer new_edge(small_number s, scaled w);
pointer reverse(pointer this_box, pointer t, scaled cur_g, real cur_glue);
pointer new_segment(small_number s, pointer f);
void just_copy(pointer p, pointer h, pointer t);
void just_reverse(pointer p);
void app_display(pointer j, pointer b, scaled d);
void pseudo_start(void);
boolean pseudo_input();
void pseudo_close(void);
void get_x_or_protected(void);
void group_warning(void);
void if_warning(void);
void file_warning(void);
void scan_expr(void);
void scan_normal_glue(void);
void scan_mu_glue(void);
integer add_or_sub(integer x, integer y, integer max_answer, boolean negative);
integer quotient(integer n, integer d);
integer fract(integer x, integer n, integer d, integer max_answer);
void scan_register_num(void);
void new_index(quarterword i, pointer q);
void find_sa_element(small_number t, halfword n, boolean w);
void delete_sa_ref(pointer q);
//void print_sa_num(pointer q);
void show_sa(pointer p, const char * s);
boolean do_marks(small_number a, small_number l, pointer q);
void sa_save(pointer p);
void sa_destroy(pointer p);
void sa_def(pointer p, halfword e);
void sa_w_def(pointer p, integer w);
void gsa_def(pointer p, halfword e);
void gsa_w_def(pointer p, integer w);
void sa_restore(void);

// functions of encodings
static void init_default_kanji (const_string file_str, const_string internal_str);
static char * mbcs_utf8 (const char * mbcs_str);
static char * utf8_mbcs (const char * utf8_str);

// inline functions
/* sec 0016 */
static inline void do_nothing (void)
{
  /* todo */
}

/* sec 0034 */
static inline void update_terminal (void)
{
  fflush(stdout);
}

static inline void clear_terminal (void)
{
  /* todo */
}

static inline void wake_up_terminal(void)
{
  /* todo */
}

// functions of WEB2C
static inline int do_final_end (void)
{
  update_terminal();
  ready_already = 0;

  if ((history != spotless) && (history != warning_issued))
    return 1;
  else
    return 0;
}
/* sec 0073 */
static inline void print_err (const char * s)
{
  if (interaction == error_stop_mode)
    wake_up_terminal();

  print_nl("! ");
  prints(s);
}
/* sec 0042 */
static inline void append_char (ASCII_code c)
{
  str_pool[pool_ptr] = c;
  incr(pool_ptr);
}
/* sec 0042 */
static inline void str_room (int val)
{
#ifdef APTEX_EXTENSION
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
static inline void flush_string (void)
{
  decr(str_ptr);
  pool_ptr = str_start[str_ptr];
}
/* sec 0048 */
static inline void append_lc_hex (ASCII_code c)
{
  if (c < 10)
    append_char(c + '0');
  else
    append_char(c - 10 + 'a');
}
/* sec 0071 */
static inline void prompt_input (const char * s)
{
  wake_up_terminal();
  prints(s);
  term_input();
}
/* sec 0079 */
static inline void tex_help (unsigned int n, ...)
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
static inline void succumb (void)
{
  if (interaction == error_stop_mode)
    interaction = scroll_mode;

  if (log_opened)
    error();

#ifdef APTEX_DEBUG
  if (interaction > 0)
    debug_help();
#endif

  history = error_stop_mode;
  jump_out();
}
/* sec 0096 */
static inline void check_interrupt (void)
{
  if (interrupt != 0)
    pause_for_instructions();
}
/* sec 0273 */
static inline void check_full_save_stack (void)
{
  if (save_ptr > max_save_stack)
  {
    max_save_stack = save_ptr;

#ifdef APTEX_EXTENSION
    if (max_save_stack > current_save_size - 7)
      save_stack = realloc_save_stack(increment_save_size);

    if (max_save_stack > current_save_size - 7)
      overflow("save size", current_save_size);
#else
    if (max_save_stack > save_size - 7)
      overflow("save size", save_size);
#endif
  }
}
/* sec 0321 */
static inline void push_input (void)
{
  if (input_ptr > max_in_stack)
  {
    max_in_stack = input_ptr;

#ifdef APTEX_EXTENSION
    if (input_ptr == current_stack_size)
      input_stack = realloc_input_stack(increment_stack_size);

    if (input_ptr == current_stack_size)
      overflow("input stack size", current_stack_size);
#else
    if (input_ptr == stack_size)
      overflow("input stack size", stack_size);
#endif
  }

  input_stack[input_ptr] = cur_input;
  incr(input_ptr);
}
/* sec 0322 */
static inline void pop_input (void)
{
  decr(input_ptr);
  cur_input = input_stack[input_ptr];
}
/* sec 0532 */
static inline void ensure_dvi_open (void)
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
/* sec 0616 */
static inline void synch_h (void)
{
  if (cur_h != dvi_h)
  {
    movement(cur_h - dvi_h, right1);
    dvi_h = cur_h;
  }
}
/* sec 0616 */
static inline void synch_v (void)
{
  if (cur_v != dvi_v)
  {
    movement(cur_v - dvi_v, down1);
    dvi_v = cur_v;
  }
}
/* sec 0121 */
static inline void free_avail (halfword p)
{
  link(p) = avail;
  avail = p;

#ifdef STAT
  decr(dyn_used);
#endif
}
/* sec 0180 */
static inline void node_list_display (integer p)
{
  append_char('.');
  show_node_list(p);
  decr(pool_ptr);
}
/* sec 0214 */
static inline void tail_append (pointer val)
{
  link(tail) = val;
  tail = link(tail);
}
/* sec 0214 */
static inline void prev_append (pointer val)
{
  link(prev_node) = val;
  link(link(prev_node)) = tail;
  prev_node = link(prev_node);
}
/* sec 0564 */
static inline void fget (void)
{
  fbyte = fgetc(tfm_file);
}
/* sec 0597 */
static inline void write_dvi (size_t a, size_t b)
{
  if (fwrite((char *) &dvi_buf[a], sizeof(dvi_buf[a]), (b - a + 1), dvi_file) != (b - a + 1))
    fprintf(stderr, "!Fatal Error, write_dvi()\n");
}
/* sec 0598 */
static inline void dvi_out (ASCII_code op)
{
  dvi_buf[dvi_ptr] = op;
  incr(dvi_ptr);

  if (dvi_ptr == dvi_limit)
    dvi_swap();
}
/* sec 0934 */
static inline void set_cur_lang (void)
{
  if (language <= 0)
    cur_lang = 0;
  else if (language > 255)
    cur_lang = 0;
  else
    cur_lang = language;
}
/* sec 0985 */
static inline void print_plus (int i, const char * s)
{
  if (page_so_far[i] != 0)
  {
    prints(" plus ");
    print_scaled(page_so_far[i]);
    prints(s);
  }
}

static inline void aptex_error (const char * t, const char * p)
{
  normalize_selector();
  print_err("Asiatic pTeX error");

  if (t != NULL)
  {
    prints(" (");
    prints(t);
    prints(")");
  }

  prints(": ");
  prints(p);
  succumb();
}

static inline integer get_microinterval()
{
  integer s, m;

  aptex_utils_get_seconds_and_micros(&s, &m);

  if ((s - epochseconds) > 32767)
    return 0x7FFFFFFF;
  else if (microseconds > m)
    return ((s - 1 - epochseconds) * 65536) + (((m + 1000000 - microseconds) / 100) * 65536) / 10000;
  else
    return ((s - epochseconds) * 65536) + (((m - microseconds) / 100) * 65536) / 10000;
}

static inline str_number tokens_to_string (pointer p)
{
  if (selector == new_string)
    aptex_error("tokens", "tokens_to_string() called while selector = new_string");

  old_setting = selector;
  selector = new_string;
  show_token_list(link(p), null, pool_size - pool_ptr);
  selector = old_setting;
  last_tokens_string = make_string();
  return last_tokens_string;
}

#define flushable(a) (a == str_ptr - 1)

static inline void flush_str (str_number s)
{
  if (flushable(s))
    flush_string();
}

static inline void compare_strings (void)
{
  str_number s1, s2;
  pool_pointer i1, i2, j1, j2;

  scan_toks(false, true);
  is_print_utf8 = true;
  s1 = tokens_to_string(def_ref);
  is_print_utf8 = false;
  delete_token_ref(def_ref);
  scan_toks(false, true);
  is_print_utf8 = true;
  s2 = tokens_to_string(def_ref);
  is_print_utf8 = false;
  delete_token_ref(def_ref);
  i1 = str_start[s1];
  j1 = str_start[s1 + 1];
  i2 = str_start[s2];
  j2 = str_start[s2 + 1];

  while ((i1 < j1) && (i2 < j2))
  {
    if (str_pool[i1] < str_pool[i2])
    {
      cur_val = -1;
      goto done;
    }

    if (str_pool[i1] > str_pool[i2])
    {
      cur_val = 1;
      goto done;
    }

    incr(i1);
    incr(i2);
  }

  if ((i1 == j1) && (i2 == j2))
    cur_val = 0;
  else if (i1 < j1)
    cur_val = 1;
  else
    cur_val = -1;

done:
  flush_str(s2);
  flush_str(s1);
  cur_val_level = int_val;
}

static inline void write_log (const char * fmt, ...)
{
  va_list m_ptr;
  va_start(m_ptr, fmt);
  vfprintf(log_file.file_data, fmt, m_ptr);
  va_end(m_ptr);
}

static inline void wlog_ln (const char * fmt, ...)
{
  va_list m_ptr;
  va_start(m_ptr, fmt);
  vfprintf(log_file.file_data, fmt, m_ptr);
  fprintf(log_file.file_data, "\n");
  va_end(m_ptr);
}

static inline void wterm (ASCII_code s)
{
  (void) fputc(s, stdout);
}

static inline void wlog (ASCII_code s)
{
  (void) fputc(s, log_file.file_data);
}

static inline void wterm_cr (void)
{
  (void) fputc('\n', stdout);
}

static inline void wlog_cr (void)
{
  (void) fputc('\n', log_file.file_data);
}

static inline void write_ln (alpha_file f)
{
  (void) fputc('\n', f.file_data);
}

#endif
