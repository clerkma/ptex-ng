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

#ifndef _PTEX_NG_COERCE_H
#define _PTEX_NG_COERCE_H

void initialize (void);
void print_ln (void);
void print_char (ASCII_code s);
void print_(integer s);
#define print(s) print_((integer) (s))
void prints_(const char * s);
#define prints(s) prints_((const char *) s)
void slow_print_(integer s);
#define slow_print(s) slow_print_((integer) (s))
void print_nl (const char * s);
void print_esc (const char * s);
void print_the_digs (eight_bits k);
void print_int_(integer n);
#define print_int(n) print_int_((integer) (n))
void print_cs_(integer p);
#define print_cs(p) print_cs_((integer) (p))
void sprint_cs (pointer p);
void print_file_name (integer n, integer a, integer e);
void print_size (integer s);
void print_write_whatsit_(const char * s, pointer p);
#define print_write_whatsit(s, p) print_write_whatsit_((const char *) (s), (pointer) (p))
void jump_out (void);
void error (void);
void fatal_error (const char * s);
void overflow_(const char * s, integer n);
#define overflow(s, n) overflow_((const char *) (s), (integer) (n))
void confusion (const char * s);
boolean init_terminal (void);
str_number make_string (void);
boolean str_eq_buf (str_number s, integer k);
boolean str_eq_str (str_number s, str_number t);
boolean get_strings_started (void);
void print_two (integer n);
void print_hex_(integer n);
#define print_hex(n) print_hex_((integer) (n))
void print_roman_int (integer n);
void print_current_string (void);
void term_input (void);
void int_error (integer n);
void normalize_selector (void);
void pause_for_instructions (void);
integer half (integer x);
scaled round_decimals_(small_number k);
void print_scaled (scaled s);
scaled mult_and_add_(integer n, scaled x, scaled y, scaled max_answer);
#define mult_and_add(n, x, y, max_answer) mult_and_add_((integer) (n), (scaled) (x), (scaled) (y), (scaled) (max_answer))
scaled x_over_n_(scaled x, integer n);
#define x_over_n(x, n) x_over_n_((scaled) (x), (integer) (n))
scaled xn_over_d_(scaled x, integer n, integer d);
#define xn_over_d(x, n, d) xn_over_d_((scaled) (x), (integer) (n), (integer) (d))
halfword badness_(scaled t, scaled s);
#define badness(t, s) badness_((scaled) (t), (scaled) (s))
void print_word (memory_word w);
void show_token_list_(integer p, integer q, integer l);
#define show_token_list(p, q, l) show_token_list_((integer) (p), (integer) (q), (integer) (l))
void runaway (void);
pointer get_avail (void);
void flush_list (pointer p);
pointer get_node (integer s);
void free_node (pointer p, halfword s);
void sort_avail (void);
pointer new_null_box (void);
pointer new_rule (void);
pointer new_ligature_(quarterword f, quarterword c, pointer q);
#define new_ligature(f, c, q) new_ligature_((quarterword) (f), (quarterword) (c), (pointer) (q))
pointer new_lig_item_(quarterword c);
#define new_lig_item(c) new_lig_item_((quarterword) (c))
pointer new_disc (void);
pointer new_math (scaled w, small_number s);
pointer new_spec_(pointer p);
#define new_spec(p) new_spec_((pointer) (p))
pointer new_param_glue (small_number n);
pointer new_glue (pointer q);
pointer new_skip_param (small_number n);
pointer new_kern (scaled w);
pointer new_penalty (integer m);
void check_mem (boolean print_locs);
void search_mem_(pointer p);
#define search_mem(p) search_mem_((pointer) (p))
void short_display_(integer p);
#define short_display(p) short_display_((integer) (p))
void print_font_and_char(integer p);
void print_mark (integer p);
void print_rule_dimen (scaled d);
void print_glue (scaled d, integer order, const char * s);
void print_spec (integer p, const char * s);
void print_fam_and_char_(pointer p, small_number t);
#define print_fam_and_char(p, t) print_fam_and_char_((pointer) (p), (small_number) t)
void print_delimiter_(pointer p);
#define print_delimiter(p) print_delimiter_((pointer) (p))
void print_subsidiary_data_(pointer p, ASCII_code c);
#define print_subsidiary_data(p, c) print_subsidiary_data_((pointer) (p), (ASCII_code) (c))
void print_style_(integer c);
#define print_style(c) print_style_((integer) (c))
void print_skip_param_(integer n);
#define print_skip_param(n) print_skip_param_((integer) (n))
void show_node_list_(integer p);
#define show_node_list(p) show_node_list_((integer) (p))
void show_box_(pointer p);
#define show_box(p) show_box_((pointer) (p))
void delete_token_ref_(pointer p);
#define delete_token_ref(p) delete_token_ref_((pointer) (p))
void delete_glue_ref_(pointer p);
#define delete_glue_ref(p)      delete_glue_ref_((pointer) (p))
#define fast_delete_glue_ref(p) delete_glue_ref_((pointer) (p))
void flush_node_list_(pointer p);
#define flush_node_list(p) flush_node_list_((pointer) (p))
pointer copy_node_list_(pointer p);
#define copy_node_list(p) copy_node_list_((pointer) (p))
void print_mode_(integer m);
#define print_mode(m) print_mode_((integer) (m))
void push_nest (void);
void pop_nest (void);
void show_activities (void);
void print_param_(integer n);
#define print_param(n) print_param_((integer) (n))
void begin_diagnostic (void);
void end_diagnostic (boolean blank_line);
void print_length_param_(integer n);
#define print_length_param(n) print_length_param_((integer) (n))
void print_cmd_chr_(quarterword cmd, halfword chr_code);
#define print_cmd_chr(cmd, chr_code) print_cmd_chr_((quarterword) (cmd), (halfword) (chr_code))
void show_eqtb (pointer n);
pointer id_lookup (integer j, integer l);
void primitive_(str_number s, quarterword c, halfword o);
#define primitive(s, c, o) primitive_(make_string_pool((const char *) s), (quarterword) (c), (halfword) (o))
void new_save_level (group_code c);
void eq_destroy (memory_word w);
void eq_save (pointer p, quarterword l);
void eq_define_(pointer p, quarterword t, halfword e);
#define eq_define(p, t, e) eq_define_((pointer) (p), (quarterword) (t), (halfword) (e))
void eq_word_define_(pointer p, integer w);
#define eq_word_define(p, w) eq_word_define_((pointer) (p), (integer) (w))
void geq_define_(pointer p, quarterword t, halfword e);
#define geq_define(p, t, e) geq_define_((pointer) (p), (quarterword) (t), (halfword) (e))
void geq_word_define_(pointer p, integer w);
#define geq_word_define(p, w) geq_word_define_((pointer) (p), (integer) (w))
void save_for_after (halfword t);
void restore_trace (pointer p, const char * s);
void assign_trace (pointer p, const char * s);
void unsave (void);
void prepare_mag (void);
void token_show (pointer p);
void print_meaning (void);
void show_cur_cmd_chr (void);
void show_context (void);
void begin_token_list_(pointer p, quarterword t);
#define begin_token_list(p, t) begin_token_list_((pointer) (p), (quarterword) (t))
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
void pack_buffered_name_(small_number n, integer a, integer b);
#define pack_buffered_name(n, a, b) pack_buffered_name_((small_number) (n), (integer) (a), (integer) (b))
str_number make_name_string (void);
str_number a_make_name_string_(void);
#define a_make_name_string(f) a_make_name_string_()
str_number b_make_name_string_(void);
#define b_make_name_string(f) b_make_name_string_()
str_number w_make_name_string_(void);
#define w_make_name_string(f) w_make_name_string_()
void scan_file_name (void);
void pack_job_name_(str_number s);
#define pack_job_name(s) pack_job_name_(make_string_pool((const char *) (s)))
void prompt_file_name_(const char * s, str_number e);
#define prompt_file_name(s, e) prompt_file_name_((const char *) s, make_string_pool((const char*) e))
void open_log_file (void);
void start_input (void);
internal_font_number read_font_info (pointer u, str_number nom, str_number arie, scaled s);
void char_warning_(internal_font_number f, eight_bits c);
#define char_warning(f, c) char_warning_((internal_font_number) (f), (eight_bits) (c))
pointer new_character_(internal_font_number f, eight_bits c);
#define new_character(f, c) new_character_((internal_font_number) (f), (eight_bits) (c))
void dvi_swap (void);
void dvi_four_(integer x);
#define dvi_four(x) dvi_four_((integer) (x))
void dvi_pop_(integer l);
#define dvi_pop(l) dvi_pop_((integer) (l))
void dvi_font_def (internal_font_number f);
void movement (scaled w, eight_bits o);
void special_out (pointer p);
void hlist_out (void);
void vlist_out (void);
void pdf_ship_out (pointer p);
void ship_out (pointer p);
void prune_movements (integer l);
void write_out (pointer p);
void out_what (pointer p);
void scan_spec (group_code c, boolean three_codes);
pointer hpack_(pointer p, scaled w, small_number m);
#define hpack(p, w, m) hpack_((pointer) (p), (scaled) (w), (small_number) (m))
pointer vpackage_(pointer p, scaled h, small_number m, scaled l);
#define vpackage(p, h, m, l) vpackage_((pointer) (p), (scaled) (h), (small_number) (m), (scaled) (l))
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
void ensure_vbox_(eight_bits n);
#define ensure_vbox(n) ensure_vbox_((eight_bits) (n))
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
void scan_box_(integer box_content);
#define scan_box(box_context) scan_box_((integer) (box_context))
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
void set_math_char_(integer c);
#define set_math_char(c) set_math_char_((integer) (c))
void math_limit_switch(void);
void scan_delimiter_(pointer p, boolean r);
#define scan_delimiter(p, r) scan_delimiter_((pointer) (p), (boolean) (r))
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
void new_whatsit_(small_number s, small_number w);
#define new_whatsit(s, w) new_whatsit_((small_number) (s), (small_number) (w))
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
void dir_out(void);
void set_math_kchar(integer c);
void print_kanji(KANJI_code s);
integer check_kcat_code(integer ct);
integer check_echar_range(integer c);
boolean eTeX_enabled(boolean b, quarterword j, halfword k);
void print_group(boolean e);
void group_trace(boolean e);
void show_save_groups(void);
void scan_general_text(void);
void print_if_line(integer val);
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

int main_program (void);
int main_init (int ac, char ** av);

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
extern int  pdf_dev_locate_font(const char *font_name, spt_t ptsize);
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
extern void pdf_dev_set_rect (pdf_rect *rect,
                  spt_t x_user, spt_t y_user,
                  spt_t width,  spt_t height, spt_t depth);
extern void pdf_doc_expand_box (const pdf_rect *rect);
extern void pdf_doc_set_mediabox(unsigned page_no, const pdf_rect *mediabox);
extern void pdf_enc_compute_id_string(char *dviname, char *pdfname);
extern void pdf_dev_set_dirmode(int dir_mode);
extern int pdf_load_fontmap_file(const char *filename, int map_mode);
#endif
