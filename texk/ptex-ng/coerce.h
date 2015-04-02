/*
   Copyright 2014, 2015 Clerk Ma

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

#ifndef _PTEX_NG_COERCE_H
#define _PTEX_NG_COERCE_H
// functions of TeX's mainbody
extern int main_program (void);
extern void main_init (int ac, char ** av);
// functions of reallocation
extern memory_word * allocate_main_memory (int size);
extern memory_word * realloc_main (int lo_size, int hi_size);
extern packed_ASCII_code * realloc_str_pool (int size);
extern pool_pointer * realloc_str_start (int size);
extern memory_word * realloc_save_stack (int size);
extern list_state_record * realloc_nest_stack (int size);
extern in_state_record * realloc_input_stack (int size);
extern halfword * realloc_param_stack (int size);
extern ASCII_code * realloc_buffer (int size);
extern memory_word * realloc_font_info (int size);
extern int realloc_hyphen (int hyphen_prime);
extern int allocate_tries (int trie_max);
extern void print_cs_names (FILE * output, boolean pass);
// functions of string pool
extern str_number load_pool_strings (integer spare_size);
extern str_number make_str_string (const char * s);
extern char * get_str_string (str_number s);
extern str_number get_job_name (str_number job);
extern void main_exit (void);
extern void uexit (int unix_code);
extern void t_open_in (void);
extern void add_variable_space (int size);
extern char * unixify (char * t);
// functions of I/O

extern integer web2c_round (real r);

#ifndef NG_FILE
extern boolean input_ln (FILE * f, boolean bypass_eoln);
extern boolean open_input  (void ** f, kpse_file_format_type file_fmt, const char * fopen_mode);
extern boolean open_output (void ** f, kpse_file_format_type file_fmt, const char * fopen_mode);
extern void    close_file  (FILE * f);
#define a_open_in(f)    open_input  ((void **) &(f), kpse_tex_format, FOPEN_R_MODE)
#define a_open_out(f)   open_output ((void **) &(f), kpse_tex_format, FOPEN_W_MODE)
#define b_open_in(f)    open_input  ((void **) &(f), kpse_tfm_format, FOPEN_RBIN_MODE)
#define b_open_out(f)   open_output ((void **) &(f), kpse_tfm_format, FOPEN_WBIN_MODE)
#define w_open_in(f)    open_input  ((void **) &(f), kpse_fmt_format, FOPEN_RBIN_MODE)
#define w_open_out(f)   open_output ((void **) &(f), kpse_fmt_format, FOPEN_WBIN_MODE)
#define a_close(f)      close_file(f)
#define b_close(f)      a_close(f)
#define w_close(f)      gzclose(fmt_file)
#define w_eof(f)        (flag_compact_fmt == true ? gzeof((gzFile) f) : feof((FILE *) f))
#else
extern boolean input_ln (EFILE f, boolean bypass_eoln);
extern boolean open_input  (EFILE * f, kpse_file_format_type file_fmt, const char * fopen_mode);
extern boolean open_output (EFILE * f, kpse_file_format_type file_fmt, const char * fopen_mode);
extern void    close_file  (EFILE f);
#define a_open_in(f)    open_input  ((EFILE *) &(f), kpse_tex_format, FOPEN_R_MODE)
#define a_open_out(f)   open_output ((EFILE *) &(f), kpse_tex_format, FOPEN_W_MODE)
#define b_open_in(f)    open_input  ((EFILE *) &(f), kpse_tfm_format, FOPEN_RBIN_MODE)
#define b_open_out(f)   open_output ((EFILE *) &(f), kpse_tfm_format, FOPEN_WBIN_MODE)
#define w_open_in(f)    open_input  ((EFILE *) &(f), kpse_fmt_format, FOPEN_RBIN_MODE)
#define w_open_out(f)   open_output ((EFILE *) &(f), kpse_fmt_format, FOPEN_WBIN_MODE)
#define a_close(f)      close_file(f)
#define b_close(f)      close_file(f)
#define w_close(f)      close_file(f)
#define w_eof(f)        (flag_compact_fmt == true ? gzeof((gzFile) f) : feof((FILE *) f))
#endif
// functions of synctex
void synctex_init (void);
void synctex_terminate (void);
void synctex_start_input (void);
void synctex_sheet (integer sync_mag);
void synctex_teehs (void);
void synctex_vlist (pointer this_box);
void synctex_tsilv (pointer this_box);
void synctex_void_vlist (pointer p, pointer this_box);
void synctex_hlist (pointer this_box);
void synctex_tsilh (pointer this_box);
void synctex_void_hlist (pointer p, pointer this_box);
void synctex_math (pointer p, pointer this_box);
void synctex_horizontal_rule_or_glue (pointer p, pointer this_box);
void synctex_kern (pointer p, pointer this_box);
void synctex_char (pointer p, pointer this_box);
void synctex_node (pointer p, pointer this_box);
void synctex_current (void);
// functions of TeX82
void initialize (void);
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
void print_the_digs (eight_bits k);
void print_int (integer n);
void print_cs (integer p);
void sprint_cs (pointer p);
void print_file_name (integer n, integer a, integer e);
void print_size (integer s);
void print_write_whatsit (const char * s, pointer p);
void jump_out(void);
void error (void);
void fatal_error (const char * s);
void overflow (const char * s, integer n);
void confusion (const char * s);
boolean init_terminal (void);
str_number make_string (void);
boolean str_eq_buf (str_number s, integer k);
boolean str_eq_str (str_number s, str_number t);
boolean get_strings_started (void);
void print_two (integer n);
void print_hex (integer n);
void print_roman_int (integer n);
void print_current_string (void);
void term_input (void);
void int_error (integer n);
void normalize_selector (void);
void pause_for_instructions (void);
integer half (integer x);
scaled round_decimals (small_number k);
void print_scaled (scaled s);
scaled mult_and_add (integer n, scaled x, scaled y, scaled max_answer);
scaled x_over_n (scaled x, integer n);
scaled xn_over_d (scaled x, integer n, integer d);
halfword badness (scaled t, scaled s);
void print_word (memory_word w);
void show_token_list (integer p, integer q, integer l);
void runaway (void);
pointer get_avail (void);
void flush_list (pointer p);
pointer get_node (integer s);
void free_node (pointer p, halfword s);
void sort_avail (void);
pointer new_null_box (void);
pointer new_rule (void);
pointer new_ligature (quarterword f, quarterword c, pointer q);
pointer new_lig_item (quarterword c);
pointer new_disc (void);
pointer new_math (scaled w, small_number s);
pointer new_spec (pointer p);
pointer new_param_glue (small_number n);
pointer new_glue (pointer q);
pointer new_skip_param (small_number n);
pointer new_kern (scaled w);
pointer new_penalty (integer m);
#ifdef NG_DEBUG
void check_mem (boolean print_locs);
void search_mem (pointer p);
#endif
void short_display (integer p);
void print_font_and_char (integer p);
void print_mark (integer p);
void print_rule_dimen (scaled d);
void print_glue (scaled d, integer order, const char * s);
void print_spec (integer p, const char * s);
void print_fam_and_char (pointer p, small_number t);
void print_delimiter (pointer p);
void print_subsidiary_data (pointer p, ASCII_code c);
void print_style (integer c);
void print_skip_param (integer n);
void show_node_list (integer p);
void show_box (pointer p);
void delete_token_ref (pointer p);
void delete_glue_ref (pointer p);
#define fast_delete_glue_ref(p) delete_glue_ref((pointer) (p))
void flush_node_list (pointer p);
pointer copy_node_list (pointer p);
void print_mode (integer m);
void push_nest (void);
void pop_nest (void);
void show_activities (void);
void print_param (integer n);
void begin_diagnostic (void);
void end_diagnostic (boolean blank_line);
void print_length_param (integer n);
void print_cmd_chr (quarterword cmd, halfword chr_code);
void show_eqtb (pointer n);
pointer id_lookup (integer j, integer l);
void primitive_(str_number s, quarterword c, halfword o);
#define primitive(s, c, o) primitive_(make_str_string((const char *) s), (quarterword) (c), (halfword) (o))
void new_save_level (group_code c);
void eq_destroy (memory_word w);
void eq_save (pointer p, quarterword l);
void eq_define (pointer p, quarterword t, halfword e);
void eq_word_define (pointer p, integer w);
void geq_define (pointer p, quarterword t, halfword e);
void geq_word_define (pointer p, integer w);
void save_for_after (halfword t);
void restore_trace (pointer p, const char * s);
void assign_trace (pointer p, const char * s);
void unsave (void);
void prepare_mag (void);
void token_show (pointer p);
void print_meaning (void);
void show_cur_cmd_chr (void);
void show_context (void);
void begin_token_list (pointer p, quarterword t);
void end_token_list (void);
void back_input (void);
void back_error (void);
void ins_error (void);
void begin_file_reading (void);
void end_file_reading (void);
void clear_for_error_prompt (void);
void check_outer_validity (void);
void get_next (void);
void firm_up_the_line (void);
void get_token (void);
void macro_call (void);
void insert_relax (void);
void expand (void);
void get_x_token (void);
void x_token (void);
void scan_left_brace (void);
void scan_optional_equals (void);
boolean scan_keyword (const char * s);
void mu_error (void);
void scan_eight_bit_int (void);
void scan_char_num (void);
void scan_four_bit_int (void);
void scan_fifteen_bit_int (void);
void scan_twenty_seven_bit_int (void);
void scan_font_ident (void);
void find_font_dimen (boolean writing);
void scan_something_internal (small_number level, boolean negative);
void scan_int (void);
void scan_dimen (boolean mu, boolean inf, boolean shortcut);
void scan_glue (small_number level);
pointer scan_rule_spec (void);
pointer str_toks (pool_pointer b);
pointer the_toks (void);
void ins_the_toks (void);
void conv_toks (void);
pointer scan_toks (boolean macro_def, boolean xpand);
void read_toks (integer n, pointer r, halfword j);
void pass_text (void);
void change_if_limit (small_number l, pointer p);
void conditional (void);
void begin_name (void);
boolean more_name (ASCII_code c);
void end_name (void);
void pack_file_name (str_number n, str_number a, str_number e);
void pack_buffered_name (small_number n, integer a, integer b);
str_number make_name_string (void);
str_number a_make_name_string_(void);
#define a_make_name_string(f) a_make_name_string_()
str_number b_make_name_string_(void);
#define b_make_name_string(f) b_make_name_string_()
str_number w_make_name_string_(void);
#define w_make_name_string(f) w_make_name_string_()
void scan_file_name (void);
void pack_job_name_(str_number s);
#define pack_job_name(s) pack_job_name_(make_str_string((const char *) (s)))
void prompt_file_name_(const char * s, str_number e);
#define prompt_file_name(s, e) prompt_file_name_((const char *) s, make_str_string((const char*) e))
void open_log_file (void);
void start_input (void);
internal_font_number read_font_info (pointer u, str_number nom, str_number arie, scaled s);
void char_warning (internal_font_number f, eight_bits c);
pointer new_character (internal_font_number f, eight_bits c);
void dvi_swap (void);
void dvi_four_(integer x);
#define dvi_four(x) dvi_four_((integer) (x))
void dvi_pop_(integer l);
#define dvi_pop(l) dvi_pop_((integer) (l))
void dvi_font_def (internal_font_number f);
void movement (scaled w, eight_bits o);
void pdf_special_out (pointer p);
void pdf_hlist_out (void);
void pdf_vlist_out (void);
void pdf_ship_out (pointer p);
void ship_out (pointer p);
void prune_movements (integer l);
void write_out (pointer p);
void out_what (pointer p);
void scan_spec (group_code c, boolean three_codes);
pointer hpack (pointer p, scaled w, small_number m);
pointer vpackage (pointer p, scaled h, small_number m, scaled l);
void append_to_vlist (pointer b);
pointer new_noad (void);
pointer new_style (small_number s);
pointer new_choice (void);
void show_info (void);
pointer fraction_rule (scaled t);
pointer overbar (pointer b, scaled k, scaled t);
pointer char_box (internal_font_number f, quarterword c);
void stack_into_box (pointer b, internal_font_number f, quarterword c);
scaled height_plus_depth (internal_font_number f, quarterword c);
pointer var_delimiter (pointer d, small_number s, scaled v);
pointer rebox (pointer b, scaled w);
pointer math_glue (pointer g, scaled m);
void math_kern (pointer p, scaled m);
void flush_math (void);
pointer clean_box (pointer p, small_number s, halfword jc);
void fetch (pointer a);
void make_over (pointer q);
void make_under (pointer q);
void make_vcenter (pointer q);
void make_radical (pointer q);
void make_math_accent (pointer q);
void make_fraction (pointer q);
scaled make_op (pointer q);
void make_ord (pointer q);
void make_scripts (pointer q, scaled delta);
small_number make_left_right (pointer q, small_number style, scaled max_d, scaled max_h);
void mlist_to_hlist (void);
void push_alignment (void);
void pop_alignment (void);
void get_preamble_token (void);
void init_align (void);
void init_span (pointer p);
void init_row (void);
void init_col (void);
boolean fin_col (void);
void fin_row (void);
void fin_align (void);
void align_peek (void);
pointer finite_shrink (pointer p);
void try_break (integer pi, small_number breaktype);
void post_line_break (boolean d);
small_number reconstitute (small_number j, small_number n, halfword bchar, halfword hchar);
void hyphenate (void);
trie_op_code new_trie_op (small_number d, small_number n, trie_op_code v);
trie_pointer trie_node (trie_pointer p);
trie_pointer compress_trie (trie_pointer p);
void first_fit (trie_pointer p);
void trie_pack (trie_pointer p);
void trie_fix (trie_pointer p);
void new_patterns (void);
void init_trie (void);
void line_break (boolean d);
void new_hyph_exceptions (void);
pointer prune_page_top (pointer p, boolean s);
pointer vert_break (pointer p, scaled h, scaled d);
pointer vsplit (halfword n, scaled h);
void print_totals (void);
void freeze_page_specs (small_number s);
void box_error (eight_bits n);
void ensure_vbox (eight_bits n);
void fire_up (pointer c);
void build_page (void);
void app_space (void);
void insert_dollar_sign (void);
void you_cant (void);
void report_illegal_case (void);
boolean privileged (void);
boolean its_all_over (void);
void append_glue (void);
void append_kern (void);
void off_save (void);
void extra_right_brace (void);
void normal_paragraph (void);
void box_end (integer box_content);
void begin_box (integer box_content);
void scan_box (integer box_content);
void package (small_number c);
small_number norm_min (integer h);
void new_graf (boolean indented);
void indent_in_hmode (void);
void head_for_vmode (void);
void end_graf (void);
void begin_insert_or_adjust (void);
void make_mark (void);
void append_penalty (void);
void delete_last (void);
void unpackage (void);
void append_italic_correction (void);
void append_discretionary (void);
void build_discretionary (void);
void make_accent (void);
void align_error (void);
void no_align_error (void);
void omit_error (void);
void do_endv (void);
void cs_error (void);
void push_math (group_code c);
void init_math (void);
void start_eq_no (void);
void scan_math (pointer p, pointer q);
void set_math_char (integer c);
void math_limit_switch(void);
void scan_delimiter (pointer p, boolean r);
void math_radical (void);
void math_ac (void);
void append_choices (void);
pointer fin_mlist (pointer p);
void build_choices (void);
void sub_sup (void);
void math_fraction (void);
void math_left_right (void);
void after_math (void);
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
void prefixed_command (void);
void do_assignments (void);
void open_or_close_in (void);
void issue_message (void);
void shift_case (void);
void show_whatever (void);
void store_fmt_file (void);
void new_whatsit (small_number s, small_number w);
void new_write_whatsit (small_number w);
void do_extension (void);
void fix_language (void);
void handle_right_brace (void);
void main_control (void);
void give_err_help (void);
boolean open_fmt_file (void);
boolean load_fmt_file (void);
void close_files_and_terminate (void);
void final_cleanup (void);
void init_prim (void);
void debug_help (void);
void fix_date_and_time (void);
// ptex
pointer new_dir_node(pointer b, eight_bits dir);
eight_bits get_jfm_pos(KANJI_code kcode, internal_font_number f);
void print_kansuji(integer n);
pointer get_inhibit_pos(KANJI_code c, small_number n);
pointer get_kinsoku_pos(KANJI_code c, small_number n);
void pdf_synch_dir(void);
boolean check_box(pointer box_p);
void adjust_hlist(pointer p, boolean pf);
void print_dir(eight_bits dir);
void print_direction(integer d);
void print_direction_alt(integer d);
void pdf_dir_out(void);
void set_math_kchar(integer c);
void print_kanji(KANJI_code s);
boolean check_kcat_code(integer ct);
boolean check_echar_range(integer c);
// etex
boolean eTeX_enabled (boolean b, quarterword j, halfword k);
void print_group(boolean e);
void group_trace(boolean e);
void show_save_groups(void);
void scan_general_text(void);
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
void print_sa_num(pointer q);
void show_sa(pointer p, const char * s);
boolean do_marks(small_number a, small_number l, pointer q);
void sa_save(pointer p);
void sa_destroy(pointer p);
void sa_def(pointer p, halfword e);
void sa_w_def(pointer p, integer w);
void gsa_def(pointer p, halfword e);
void gsa_w_def(pointer p, integer w);
void sa_restore(void);
void pdf_synch_h(void);
void pdf_synch_v(void);
// for pdf backend.
extern void pdf_init_fontmaps(void);
extern void pdf_close_fontmaps(void);
extern void pdf_doc_set_creator(const char * creator);
extern void pdf_doc_set_producer(const char * producer);
extern void pdf_set_version(unsigned version);
extern void pdf_set_compression(int level);
extern void pdf_files_init(void);
extern void pdf_files_close(void);
extern void graphics_mode (void);
extern long pdf_output_stats (void);
extern void pdf_init_device(double dvi2pts, int precision, int black_and_white);
extern void pdf_close_device(void);
extern void pdf_open_document(const char *filename,
                              int do_encryption,
                              double media_width,
                              double media_height,
                              double annot_grow_amount,
                              int bookmark_open_depth,
                              int check_gotos);
extern void pdf_close_document(void);
extern void pdf_doc_begin_page(double scale, double x_origin, double y_origin);
extern void pdf_doc_end_page(void);
extern int spc_exec_at_begin_document(void);
extern int spc_exec_at_end_document(void);
extern int spc_exec_at_begin_page(void);
extern int spc_exec_at_end_page(void);
typedef signed long spt_t;
extern int spc_exec_special (const char *buffer, long size, double x_user, double y_user, double dpx_mag);
extern int pdf_dev_locate_font(const char *font_name, spt_t ptsize);
extern int dvi_locate_font(const char *tfm_name, spt_t ptsize);
typedef long UNSIGNED_TRIPLE, SIGNED_TRIPLE, SIGNED_QUAD;
extern void ng_set(SIGNED_QUAD ch, int ng_font_id, SIGNED_QUAD h, SIGNED_QUAD v);
extern void pdf_dev_set_rule(spt_t xpos, spt_t ypos, spt_t width, spt_t height);
extern void pdf_dev_set_string (spt_t xpos,
                                spt_t ypos,
                                const void *instr_ptr,
                                int instr_len,
                                spt_t width,
                                int font_id,
                                int ctype);
extern void pdf_synch_h (void);
extern void pdf_synch_h (void);
typedef struct pdf_rect
{
  double llx, lly, urx, ury;
} pdf_rect;
extern void pdf_dev_set_rect(pdf_rect *rect,
                  spt_t x_user, spt_t y_user,
                  spt_t width,  spt_t height, spt_t depth);
extern void pdf_doc_expand_box(const pdf_rect *rect);
extern void pdf_doc_set_mediabox(unsigned page_no, const pdf_rect *mediabox);
extern void pdf_enc_compute_id_string(char *dviname, char *pdfname);
extern void pdf_dev_set_dirmode(int dir_mode);
extern int pdf_load_fontmap_file(const char *filename, int map_mode);
// special out
extern void pdf_special_exec(scaled h, scaled v);
// kanji processing
extern boolean check_kanji(integer c);
extern boolean is_char_ascii(integer c);
extern boolean is_char_kanji(integer c);
extern boolean ismultiprn(integer c);
extern integer calc_pos(integer c);
extern integer kcatcodekey(integer c);
extern integer multilenbuffchar(integer c);
extern void init_default_kanji(const_string file_str, const_string internal_str);
extern char * mbcs_utf8(const char * mbcs_str);
extern char * utf8_mbcs(const char * utf8_str);
/* sec 79 */
// inline functions
/* sec 0016 */
static inline void do_nothing (void)
{
  /* todo */
}
static inline void wake_up_terminal (void)
{
  /* todo */
}
/* sec 0034 */
static inline void update_terminal (void)
{
  fflush(stdout);
}
// web2c's specific
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
#ifdef NG_EXTENSION
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

#ifdef NG_DEBUG
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

#ifdef NG_EXTENSION
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

#ifdef NG_EXTENSION
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
  if (fwrite((char *) &dvi_buf[a], sizeof(dvi_buf[a]),
    (b - a + 1), dvi_file) != (b - a + 1))
    FATAL_PERROR("\n! dvi file");
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

static inline void ptex_ng_error (const char * t, const char * p)
{
  normalize_selector();
  print_err("pTeX-ng error");

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
static inline str_number tokens_to_string (pointer p)
{
  if (selector == new_string)
    ptex_ng_error("tokens", "tokens_to_string() called while selector = new_string");

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

static inline void print_out_name (const char * str, str_number num)
{
  if (str != NULL)
    prints(str);
  else
    slow_print(num);
}

static inline void write_log (const char * fmt, ...)
{
  va_list m_ptr;
  va_start(m_ptr, fmt);
  vfprintf(log_file, fmt, m_ptr);
  va_end(m_ptr);
}

static inline void dump_int (integer x)
{
  generic_dump(x);
}

static inline void dump_hh (two_halves x)
{
  generic_dump(x);
}

static inline void dump_wd (memory_word x)
{
  generic_dump(x);
}

static inline void wterm (ASCII_code s)
{
  (void) fputc(s, stdout);
}

static inline void wlog (ASCII_code s)
{
  (void) fputc(s, log_file);
}

static inline void wterm_cr (void)
{
  (void) fputc('\n', stdout);
}

static inline void wlog_cr (void)
{
  (void) fputc('\n', log_file);
}
#endif
