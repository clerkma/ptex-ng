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

#ifndef _PTEX_NG_H
#define _PTEX_NG_H

#ifndef EXTERN
  #define EXTERN extern
#endif

// macro for dynamic allocation
#define NG_EXTENSION
// tex's infrastructure
#define STAT
#define INITEX

// headers and pragmas
#if defined (_WIN32)
  #define _CRT_DISABLE_PERFCRIT_LOCKS
  #define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES 1
  #pragma warning(disable:4201) // nameless struct/union
  #pragma warning(disable:4996) // a function that was marked with deprecated
  #pragma warning(disable:4701) // potentially uninitialized local variable 'name' used
  #pragma warning(disable:4135) // conversion between different integral types
  #pragma warning(disable:4127) // conditional expression is constant
#elif defined (__clang__)
  #pragma clang diagnostic ignored "-Wdangling-else"
#elif defined (__GNUC__) || defined (__GNUG__)
  #pragma GCC diagnostic ignored "-Wunused-result"
#endif

// standard C headers
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>
#include <signal.h>
#include <assert.h>
// TeX Live's kpathsea
#include <kpathsea/config.h>
#include <kpathsea/c-pathmx.h> // PATH_MAX
#include <kpathsea/c-pathch.h> // ISBLANK
#include <kpathsea/c-fopen.h>  // FOPEN_WBIN_MODE
#include <kpathsea/getopt.h>   // get_opt
#include <kpathsea/tex-file.h> // kpse_find_file
#include <kpathsea/variable.h> // kpse_var_value
#include <kpathsea/types.h>    // RPId64
#include <kpathsea/version.h>  // kpathsea_version_string
// ptexenc for kanji processing
#include <ptexenc/ptexenc.h>
#include <ptexenc/unicode.h>
// zlib for fmt file and synctex
#include "zlib.h"

#ifdef __cplusplus
extern "C" {
#endif

// integers
typedef uint8_t ASCII_code;
typedef int32_t KANJI_code;
typedef uint8_t eight_bits;
typedef uint8_t packed_ASCII_code;
typedef uint8_t small_number;

// files
typedef FILE * alpha_file;
typedef FILE * byte_file;
typedef void * word_file;

#ifdef link
  #undef link
#endif

/* sec 0027 */
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

EXTERN int block_dump   (char * p, int item_size, int nitems, void * out_file);
EXTERN int block_undump(char * p, int item_size, int nitems, void * out_file);

#define dump_things(base, len)    block_dump  ((char *) &(base), sizeof (base), (int) (len), fmt_file)
#define undump_things(base, len)  block_undump((char *) &(base), sizeof (base), (int) (len), fmt_file)

#define generic_dump(x)   dump_things(x, 1)
#define generic_undump(x) undump_things(x, 1)

#define undump_hh   generic_undump
#define undump_int  generic_undump

#define too_small(a)                        \
do {                                        \
  wake_up_terminal();                       \
  printf("---! Must increase the %s", a);   \
  goto bad_fmt;                             \
} while (0)

#define undump_size(arg1, arg2, arg3, arg4) \
do {                                        \
  undump_int(x);                            \
                                            \
  if (x < arg1)                             \
    goto bad_fmt;                           \
                                            \
  if (x > arg2)                             \
    too_small(arg3);                        \
  else                                      \
    arg4 = x;                               \
} while (0)

// pTeX-ng's macros
#define Hi(x) BYTE3(x)
#define Lo(x) BYTE4(x)
#define nrestmultichr(x)  ((x)!=0 ? ((x) / 8) + 2 - ((x) % 8) : -1)
#define max_cjk_val       0x1000000

#ifdef NG_EXTENSION
  #define def_const(sym, val0, val1) \
    enum {sym = val1}
  #define def_alloc(sym, val0, val1, init, incr) \
    enum {sym = val1, initial_##sym = init, increment_##sym = incr}
  #define def_type(sym, t0, t1) \
    typedef t1 sym
  #define def_array(sym, type, size) \
    EXTERN type * sym
  #define def_alter(sym, type, val) \
    EXTERN type sym
#else
  #define def_const(sym, val0, val1) \
    enum {sym = val0}
  #define def_alloc(sym, val0, val1, init, incr) \
    enum {sym = val0}
  #define def_type(sym, t0, t1) \
    typedef t0 sym
  #define def_array(sym, type, size) \
    EXTERN type sym[size]
  #define def_alter(sym, type, val) \
    enum {sym = val}
#endif

def_const(file_name_size,     PATH_MAX, PATH_MAX);
def_const(min_halfword,       0,      0);
def_const(max_halfword,       65536,  2147483647);
def_const(max_in_open,        15,     15); // 127
def_const(block_size,         1000,   1000);
def_const(min_quarterword,    0,      0);
def_const(max_quarterword,    255,    65535);
def_const(default_mem_top,    0,      262140);
def_const(error_line,         79,     79);
def_const(half_error_line,    50,     50);
def_const(max_print_line,     79,     79);
def_const(mem_bot,            0,      0);
def_const(int_size,           4,      8);
def_const(font_max,           255,    65535);
def_const(string_vacancies,   100000, 100000);
def_const(trie_op_size,       751,    35111);
def_const(neg_trie_op_size,   -751,   -35111);
def_const(min_trie_op,        0,      0);
def_const(max_trie_op,        500,    1000);
def_const(dvi_buf_size,       16384,  16384);
def_const(hash_prime,         55711,  445631);
def_const(hash_size,          65536,  524288);

def_alloc(buf_size,       200000, 2000000,                        1000,   2000);
def_alloc(font_mem_size,  100000, (max_halfword / int_size - 1),  20000,  40000);
def_alloc(pool_size,      124000, (max_halfword - 1),             40000,  80000);
def_alloc(max_strings,    16384,  (max_halfword / int_size - 1),  5000,   10000);
def_alloc(save_size,      8000,   65536,                          1000,   2000);
def_alloc(nest_size,      200,    65536,                          100,    200);
def_alloc(param_size,     500,    65536,                          100,    200);
def_alloc(stack_size,     800,    65536,                          100,    200);

def_type(integer,     int32_t, int64_t);
def_type(halfword,    int16_t, int32_t);
def_type(quarterword, uint8_t, uint16_t);
def_type(glue_ratio,  float,   double);
def_type(real,        float,   double);

typedef integer pool_pointer;
typedef integer str_number;
typedef integer scaled;
typedef integer nonnegative_integer;

def_alter(mem_top, integer, 262140);
def_alter(mem_max, integer, 262140);
def_alter(mem_min, integer, 0);

#ifdef NG_EXTENSION
  #define max_mem_size (max_halfword / sizeof(memory_word) - 1)
#endif

def_array(buffer, ASCII_code, buf_size + 4);

EXTERN integer first;
EXTERN integer last;
EXTERN integer max_buf_stack;

def_alter(trie_size, integer, 30000);

#ifdef NG_EXTENSION
  #define default_trie_size 1000000 // 60000
#endif

/* mem_min may be < 0 */
/* sec 0113 */
typedef halfword pointer;
/* sec 0113 */
typedef struct
{
  halfword rh;

  union
  {
    halfword lh;

    struct
    {
      quarterword b0, b1;
    };
  };
} two_halves;

typedef struct
{
  quarterword b0, b1, b2, b3;
} four_quarters;

typedef union
{
  integer cint;
  glue_ratio gr;
  two_halves hh;
  four_quarters qqqq;
} memory_word;

#include "macros.h"

/* sec 0150 */
typedef char glue_ord;
/* sec 0212 */
typedef struct
{
  int mode_field;
  int dir_field, adj_dir_field;
  scaled pdisp_field;
  pointer head_field, tail_field, pnode_field, last_jchr_field;
  pointer eTeX_aux_field;
  integer pg_field, ml_field;
  memory_word aux_field;
} list_state_record;
/* sec 0269 */
typedef char group_code;
/* sec 0300 */
typedef struct
{
  quarterword state_field, index_field;
  halfword start_field, loc_field, limit_field, name_field;
  integer synctex_tag_field;
} in_state_record;
/* sec 0548 */
typedef integer internal_font_number;
typedef integer font_index;
/* sec 0594 */
typedef integer dvi_index;
/* sec 0920 */
typedef integer trie_op_code;
/* sec 0925 */
typedef integer trie_pointer;
typedef integer hyph_pointer;

EXTERN integer bad;
EXTERN ASCII_code xord[256];
EXTERN ASCII_code xchr[256];
EXTERN ASCII_code name_of_file[file_name_size + 4];
EXTERN integer name_length;

def_array(str_pool, packed_ASCII_code, pool_size + 1);
def_array(str_start, pool_pointer, max_strings + 1);

EXTERN pool_pointer pool_ptr;
EXTERN str_number   str_ptr;
EXTERN pool_pointer init_pool_ptr;
EXTERN str_number   init_str_ptr;
EXTERN alpha_file log_file; 
EXTERN int selector;
EXTERN char dig[23 + 1];
EXTERN integer tally;
EXTERN integer term_offset;
EXTERN integer file_offset;
EXTERN ASCII_code trick_buf[error_line + 1];
EXTERN ASCII_code trick_buf2[error_line + 1];
EXTERN ASCII_code kcode_pos;
EXTERN ASCII_code kcp;
EXTERN ASCII_code prev_char;
EXTERN integer trick_count;
EXTERN integer first_count;
EXTERN int interaction;
EXTERN boolean deletions_allowed;
EXTERN boolean set_box_allowed;
EXTERN int history;
EXTERN int error_count;
EXTERN char * help_line[6];
EXTERN int help_ptr;
EXTERN boolean use_err_help;
EXTERN integer interrupt;
EXTERN boolean OK_to_interrupt;
EXTERN boolean arith_error;
EXTERN scaled ng_remainder;
EXTERN halfword temp_ptr;

/* sec 0116 */
def_array(mem, memory_word, mem_max - mem_bot + 1);

EXTERN memory_word * main_memory;
EXTERN pointer lo_mem_max;
EXTERN pointer hi_mem_min;
EXTERN integer var_used, dyn_used;
/* sec 0118 */
EXTERN pointer avail;
EXTERN pointer mem_end;
EXTERN pointer mem_start;
/* sec 0124 */
EXTERN pointer rover;

/* sec 0165 */
#ifdef NG_DEBUG
  define_array(zzzab, char, mem_max - mem_bot + 1);
  define_array(zzzac, char, mem_max - mem_bot + 1);
  #define freearr (zzzab - (int)(mem_bot))
  #define wasfree (zzzac - (int)(mem_bot))
  EXTERN pointer was_mem_end, was_lo_max, was_hi_min;
  EXTERN boolean panicking;
#endif

EXTERN integer font_in_short_display;
EXTERN integer depth_threshold;
EXTERN integer breadth_max;
EXTERN int shown_mode;
EXTERN int old_setting;

EXTERN memory_word eqtb[eqtb_size + 1];
EXTERN quarterword zzzad[eqtb_size - int_base + 1];
#define xeq_level (zzzad - int_base)
EXTERN two_halves zzzae[undefined_control_sequence - hash_base];
#define hash (zzzae - hash_base)

EXTERN pointer hash_used;
EXTERN boolean no_new_control_sequence;
EXTERN integer cs_count;

def_array(save_stack, memory_word, save_size + 1);

EXTERN integer save_ptr;
EXTERN integer max_save_stack;
EXTERN int cur_level;
EXTERN int cur_group;
EXTERN integer cur_boundary;
EXTERN integer mag_set;
EXTERN int cur_cmd;
EXTERN halfword cur_chr;
EXTERN pointer cur_cs;
EXTERN halfword cur_tok;

def_array(nest, list_state_record, nest_size + 1);

EXTERN integer nest_ptr;
EXTERN integer max_nest_stack;
EXTERN list_state_record cur_list;

def_array(param_stack, pointer, param_size + 1);

EXTERN integer param_ptr;
EXTERN integer max_param_stack;

def_array(input_stack, in_state_record, stack_size + 1);

EXTERN integer input_ptr;
EXTERN integer max_in_stack;
EXTERN integer high_in_open;
EXTERN in_state_record cur_input;
EXTERN int in_open;
EXTERN integer open_parens;
EXTERN alpha_file input_file[max_in_open + 1];
EXTERN integer line;
EXTERN integer line_stack[max_in_open + 1];
EXTERN int scanner_status;
EXTERN pointer warning_index;
EXTERN pointer def_ref;
EXTERN integer align_state;
EXTERN integer base_ptr;
EXTERN pointer par_loc;
EXTERN halfword par_token;
EXTERN boolean skip_mode;
EXTERN boolean force_eof;
EXTERN pointer cur_mark[6];
EXTERN int long_state;
EXTERN pointer pstack[10];
EXTERN integer cur_val;
EXTERN int cur_val_level;
EXTERN int radix;
EXTERN int cur_order;
EXTERN alpha_file read_file[16];
EXTERN char read_open[17];
EXTERN pointer cond_ptr;
EXTERN int if_limit;
EXTERN int cur_if;
EXTERN integer if_line;
EXTERN integer skip_line;
EXTERN str_number cur_name;
EXTERN str_number cur_area;
EXTERN str_number cur_ext;
EXTERN pool_pointer area_delimiter;
EXTERN pool_pointer ext_delimiter;
EXTERN integer format_default_length;
EXTERN char * TEX_format_default;
EXTERN boolean name_in_progress;
EXTERN boolean log_opened;
EXTERN boolean quoted_file_name;
EXTERN str_number job_name;
EXTERN const char * job_name_str;
EXTERN str_number output_file_name;
EXTERN str_number log_name;
EXTERN byte_file dvi_file;
EXTERN byte_file tfm_file;
EXTERN byte_file pdf_file;
EXTERN char * dvi_file_name;
EXTERN char * pdf_file_name;
EXTERN char * log_file_name;
EXTERN char * fmt_file_name;

def_array(font_info, memory_word, font_mem_size + 1);

EXTERN font_index fmem_ptr;
EXTERN internal_font_number font_ptr;
EXTERN four_quarters font_check[font_max + 1];
EXTERN eight_bits font_dir[font_max + 1];
EXTERN integer font_num_ext[font_max + 1];
EXTERN integer font_id[font_max + 1];
EXTERN scaled font_size[font_max + 1];
EXTERN scaled font_dsize[font_max + 1];
EXTERN font_index font_params[font_max + 1];
EXTERN str_number font_name[font_max + 1];
EXTERN str_number font_area[font_max + 1];
EXTERN eight_bits font_bc[font_max + 1];
EXTERN eight_bits font_ec[font_max + 1];
EXTERN pointer font_glue[font_max + 1];
EXTERN boolean font_used[font_max + 1];
EXTERN integer hyphen_char[font_max + 1];
EXTERN integer skew_char[font_max + 1];
EXTERN font_index bchar_label[font_max + 1];
EXTERN short font_bchar[font_max + 1];
EXTERN short font_false_bchar[font_max + 1];
EXTERN integer char_base[font_max + 1];
EXTERN integer ctype_base[font_max + 1];
EXTERN integer width_base[font_max + 1];
EXTERN integer height_base[font_max + 1];
EXTERN integer depth_base[font_max + 1];
EXTERN integer italic_base[font_max + 1];
EXTERN integer lig_kern_base[font_max + 1];
EXTERN integer kern_base[font_max + 1];
EXTERN integer exten_base[font_max + 1];
EXTERN integer param_base[font_max + 1];
EXTERN four_quarters null_character;
EXTERN integer total_pages;
EXTERN scaled max_v;
EXTERN scaled max_h;
EXTERN integer max_push;
EXTERN integer last_bop;
EXTERN integer dead_cycles;
EXTERN boolean doing_leaders;
EXTERN quarterword c, f;
EXTERN scaled rule_ht, rule_dp, rule_wd;
EXTERN pointer g;
EXTERN integer lq, lr;
EXTERN eight_bits dvi_buf[dvi_buf_size + 4];
EXTERN dvi_index half_buf;
EXTERN dvi_index dvi_limit;
EXTERN dvi_index dvi_ptr;
EXTERN integer dvi_offset;
EXTERN integer dvi_gone;
EXTERN pointer down_ptr, right_ptr;
EXTERN scaled dvi_h, dvi_v;
EXTERN scaled cur_h, cur_v;
EXTERN internal_font_number dvi_f;
EXTERN integer cur_s;
EXTERN scaled total_stretch[4], total_shrink[4];
EXTERN integer last_badness;
EXTERN pointer adjust_tail;
EXTERN scaled last_disp;
EXTERN pointer cur_kanji_skip;
EXTERN pointer cur_xkanji_skip;
EXTERN integer pack_begin_line;
EXTERN two_halves empty_field;
EXTERN four_quarters null_delimiter;
EXTERN pointer cur_mlist;
EXTERN small_number cur_style;
EXTERN small_number cur_size;
EXTERN scaled cur_mu;
EXTERN boolean mlist_penalties;
EXTERN internal_font_number cur_f;
EXTERN quarterword cur_c;
EXTERN four_quarters cur_i;
EXTERN integer magic_offset;
EXTERN pointer cur_align;
EXTERN pointer cur_span;
EXTERN pointer cur_loop;
EXTERN pointer align_ptr;
EXTERN pointer cur_head, cur_tail;
EXTERN pointer just_box;
EXTERN pointer passive;
EXTERN pointer printed_node;
EXTERN halfword pass_number;
EXTERN scaled active_width[8];
EXTERN scaled cur_active_width[8];
EXTERN scaled background[8];
EXTERN scaled break_width[8];
EXTERN boolean no_shrink_error_yet;
EXTERN pointer cur_p;
EXTERN boolean chain;
EXTERN boolean second_pass;
EXTERN boolean final_pass;
EXTERN integer threshold;
EXTERN integer minimal_demerits[4];
EXTERN integer minimum_demerits;
EXTERN pointer best_place[4];
EXTERN halfword best_pl_line[4];
EXTERN scaled disc_width;
EXTERN halfword easy_line;
EXTERN halfword last_special_line;
EXTERN scaled first_width;
EXTERN scaled second_width;
EXTERN scaled first_indent;
EXTERN scaled second_indent;
EXTERN pointer best_bet;
EXTERN integer fewest_demerits;
EXTERN halfword best_line;
EXTERN integer actual_looseness;
EXTERN integer line_diff;
EXTERN int hc[66];
EXTERN int hn;
EXTERN halfword ha, hb;
EXTERN int hf;
EXTERN int hu[66];
EXTERN int hyf_char;
EXTERN int cur_lang, init_cur_lang;
EXTERN integer l_hyf, r_hyf;
EXTERN integer init_l_hyf, init_r_hyf;
EXTERN halfword hyf_bchar;
EXTERN char hyf[68];
EXTERN pointer init_list;
EXTERN boolean init_lig;
EXTERN boolean init_lft;
EXTERN int hyphen_passed;
EXTERN halfword cur_l, cur_r;
EXTERN pointer cur_q;
EXTERN pointer lig_stack;
EXTERN boolean ligature_present;
EXTERN boolean lft_hit, rt_hit;

def_array(trie_trl, halfword, trie_size + 1);
def_array(trie_tro, halfword, trie_size + 1);
def_array(trie_trc, quarterword, trie_size + 1);

EXTERN small_number hyf_distance[trie_op_size + 1];
EXTERN small_number hyf_num[trie_op_size + 1];
EXTERN trie_op_code hyf_next[trie_op_size + 1];
EXTERN integer op_start[256];

def_array(hyph_word, str_number, hyphen_prime + 1);
def_array(hyph_list, pointer, hyphen_prime + 1);

def_alter(hyphen_prime, integer, 607);

#ifdef NG_EXTENSION
  #define default_hyphen_prime 8191 // 1009
#endif

EXTERN hyph_pointer hyph_count;

#ifdef INITEX
  EXTERN integer trie_op_hash_C[trie_op_size - neg_trie_op_size + 1];
  #define trie_op_hash (trie_op_hash_C - (int)(neg_trie_op_size))
  EXTERN trie_op_code trie_used[256];
  EXTERN ASCII_code trie_op_lang[trie_op_size + 1];
  EXTERN trie_op_code trie_op_val[trie_op_size + 1];
  EXTERN integer trie_op_ptr;
#endif

EXTERN trie_op_code max_op_used;

#ifdef INITEX
  def_array(trie_c, packed_ASCII_code, trie_size + 1); /* characters to match */
  def_array(trie_o, trie_op_code, trie_size + 1); /* operations to perform */
  def_array(trie_l, trie_pointer, trie_size + 1); /* left subtrie links */
  def_array(trie_r, trie_pointer, trie_size + 1); /* right subtrie links */
  def_array(trie_hash, trie_pointer, trie_size + 1); /* used to identify equivlent subtries */
  def_array(trie_taken, char, trie_size + 1); // char / boolean
  EXTERN trie_pointer trie_ptr;
  EXTERN trie_pointer trie_min[256];
  EXTERN trie_pointer trie_max;
  EXTERN boolean trie_not_ready;
#endif

EXTERN scaled best_height_plus_depth;
EXTERN pointer page_tail;
EXTERN int page_contents;
EXTERN scaled page_max_depth;
EXTERN pointer best_page_break;
EXTERN integer least_page_cost;
EXTERN scaled best_size;
EXTERN scaled page_so_far[8];
EXTERN pointer last_glue;
EXTERN integer last_penalty;
EXTERN scaled last_kern;
EXTERN integer last_node_type;
EXTERN integer insert_penalties;
EXTERN boolean output_active;
/* sec 1032 */
EXTERN internal_font_number main_f;
EXTERN four_quarters main_i;
EXTERN four_quarters main_j;
EXTERN font_index main_k;
EXTERN pointer main_p;
EXTERN integer main_s;
EXTERN halfword bchar;
EXTERN halfword false_bchar;
EXTERN boolean cancel_boundary;
EXTERN boolean ins_disc;
/* sec 1074 */
EXTERN pointer cur_box;
EXTERN halfword after_token;
EXTERN boolean long_help_seen;
EXTERN str_number format_ident;
EXTERN word_file fmt_file;
/* sec 1331 */
EXTERN integer ready_already;
/* sec 1342 */
EXTERN alpha_file write_file[16];
EXTERN boolean write_open[18];
/* sec 1345 */
EXTERN pointer write_loc;
EXTERN boolean inhibit_glue_flag;
EXTERN integer dvi_dir;
EXTERN integer cur_dir_hv;
EXTERN eight_bits page_dir;
EXTERN pointer first_char;
EXTERN pointer last_char;
EXTERN boolean find_first_char;
EXTERN int fbyte;

//eTeX
EXTERN boolean eTeX_mode;
EXTERN boolean eof_seen[max_in_open + 1];
EXTERN pointer LR_ptr;
EXTERN scaled revdisp;
EXTERN integer LR_problems;
EXTERN small_number cur_dir;
EXTERN pointer pseudo_files;
EXTERN pointer grp_stack[max_in_open + 1];
EXTERN pointer if_stack[max_in_open + 1];
EXTERN halfword max_reg_num;
EXTERN const char * max_reg_help_line;
EXTERN pointer sa_root[7];
EXTERN pointer cur_ptr;
EXTERN memory_word sa_null;
EXTERN pointer sa_chain;
EXTERN quarterword sa_level;
EXTERN pointer last_line_fill;
EXTERN boolean do_last_line_fit;
EXTERN small_number active_node_size;
EXTERN scaled fill_width[3];
EXTERN scaled best_pl_short[4];
EXTERN scaled best_pl_glue[4];
EXTERN trie_pointer hyph_start;
EXTERN trie_pointer hyph_index;
EXTERN pointer disc_ptr[4];
EXTERN boolean is_print_utf8;
EXTERN str_number last_tokens_string;

/* new variables defined in ptex-ng-local.c */
EXTERN boolean flag_initex;
EXTERN boolean flag_verbose;
EXTERN boolean flag_trace;
EXTERN boolean flag_tex82;
EXTERN boolean flag_compact_fmt;
EXTERN boolean flag_c_style;
EXTERN boolean flag_deslash;
EXTERN boolean flag_allow_patterns;
EXTERN boolean flag_allow_quoted;
EXTERN boolean flag_reset_exceptions;
EXTERN boolean flag_civilize;
EXTERN boolean flag_show_current;
EXTERN boolean flag_show_numeric;
EXTERN boolean flag_show_missing;
EXTERN boolean flag_show_in_hex;
EXTERN boolean flag_show_tfm;
EXTERN boolean flag_show_csnames;
EXTERN boolean flag_show_lb_stats;
EXTERN boolean flag_suppress_f_ligs;
EXTERN int mem_initex;
EXTERN int new_hyphen_prime;
EXTERN int missing_characters;
EXTERN int tab_step;
EXTERN scaled default_rule;
EXTERN char * format_name;
EXTERN int count_first_pass;
EXTERN int count_second_pass;
EXTERN int count_final_pass;
EXTERN int count_underfull_hbox;
EXTERN int count_overfull_hbox;
EXTERN int count_underfull_vbox;
EXTERN int count_overfull_vbox;
EXTERN int count_paragraph_failed;
EXTERN int count_single_line;
EXTERN int current_pool_size;
EXTERN int current_max_strings;
EXTERN int current_mem_size;
EXTERN int current_font_mem_size;
EXTERN int current_save_size;
EXTERN int current_stack_size;
EXTERN int current_nest_size;
EXTERN int current_param_size;
EXTERN int current_buf_size;
extern const char * banner;
extern const char * dist;
EXTERN char log_line[256];
extern char * dvi_directory;
extern char * log_directory;
extern char * aux_directory;
extern char * fmt_directory;
extern char * pdf_directory;
EXTERN clock_t time_start, time_main, time_finish;
// for synctex
EXTERN integer synctex_option;

#include "coerce.h"

#ifdef __cplusplus
}
#endif

#endif
