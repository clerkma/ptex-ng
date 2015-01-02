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

#ifndef _PTEX_H
#define _PTEX_H
// macros for dynamic allocation
#define ALLOCATEINI
#define ALLOCATEMAIN       /* allocate main memory for TeX (2 Meg) */
#define ALLOCATEFONT       /* allocate font_info (800 k) (dynamically now) */
#define ALLOCATETRIES      /* allocate hyphenation trie stuff (270 k) trie_trl, trie_tro, trie_trc */
#define ALLOCATEHYPHEN     /* allocate hyphenation exception tables */
#define VARIABLETRIESIZE   /* allow trie_size to be variable */
#define ALLOCATESTRING     /* allocate strings and string pointers (184 k) str_pool & str_start */
#define ALLOCATESAVESTACK  /* experiment to dynamically deal with save_stack   */
#define ALLOCATEINPUTSTACK /* experiment to dynamically deal with input_stack  */
#define ALLOCATENESTSTACK  /* experiment to dynamically deal with nest_stack   */
#define ALLOCATEPARAMSTACK /* experiment to dynamically deal with param_stack  */
#define ALLOCATEBUFFER     /* experiment to dynamically deal with input buffer */
#define INCREASEFONTS
#define INCREASETRIEOP     /* tire_* */
#define COMPACTFORMAT      /* .fmt file with zlib */
#define STAT               /* TeX's statistics (tex82) */
#define INITEX             /* invoke initex */
#define WORDS_BIGENDIAN 0  /* about format file */

/* headers and pragmas */
#if defined (_WIN32)
  #define _CRT_DISABLE_PERFCRIT_LOCKS
  #define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES 1
  #pragma warning(disable:4201) // nameless struct/union
  #pragma warning(disable:4267)
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
#include <stdint.h>
#include <stdarg.h>
#include <setjmp.h>
#include <time.h>
#include <math.h>
#include <signal.h>
// TeX Live's kpathsea
#include <kpathsea/config.h>
#include <kpathsea/c-pathmx.h> // PATH_MAX
#include <kpathsea/c-pathch.h> // ISBLANK
#include <kpathsea/c-fopen.h>  // FOPEN_WBIN_MODE
#include <kpathsea/getopt.h>   // get_opt
#include <kpathsea/tex-file.h> // kpse_find_file
#include <kpathsea/variable.h> // kpse_var_value
// ptexenc for kanji processing
#include <ptexenc/ptexenc.h>
#include <ptexenc/unicode.h>
// zlib for fmt file and synctex
#include "zlib.h"
// integers
typedef int64_t integer;
typedef double  glue_ratio;
typedef double  real;
typedef uint8_t ASCII_code;
typedef int32_t KANJI_code;
typedef uint8_t eight_bits;
typedef integer pool_pointer;
typedef integer str_number;
typedef uint8_t packed_ASCII_code;
typedef integer scaled;
typedef integer nonnegative_integer;
typedef uint8_t small_number;
// files
typedef FILE * alpha_file;
typedef FILE * byte_file;
typedef FILE * word_file;

#ifdef link
  #undef link
#endif

#define wterm(s)    (void) fputc(s, stdout)
#define wlog(s)     (void) fputc(s, log_file)
#define wterm_cr()  (void) fputc('\n', stdout)
#define wlog_cr()   (void) fputc('\n', log_file)
/* sec 0027 */
#define a_open_in(f)    open_input  (&(f), kpse_tex_format, FOPEN_R_MODE)
#define a_open_out(f)   open_output (&(f), FOPEN_W_MODE)
#define b_open_in(f)    open_input  (&(f), kpse_tfm_format, FOPEN_RBIN_MODE)
#define b_open_out(f)   open_output (&(f), FOPEN_WBIN_MODE)
#define w_open_in(f)    open_input  (&(f), kpse_fmt_format, FOPEN_RBIN_MODE)
#define w_open_out(f)   open_output (&(f), FOPEN_WBIN_MODE)
#define a_close(f)      close_file(f)
#define b_close(f)      a_close(f)
#define w_close(f)      gzclose(gz_fmt_file)

#ifdef COMPACTFORMAT
  EXTERN int do_dump(char * p, int item_size, int nitems, gzFile out_file);
  EXTERN int do_undump(char * p, int item_size, int nitems, gzFile out_file);
  #define dump_file gz_fmt_file
#else
  EXTERN int do_dump(char * p, int item_size, int nitems, FILE * out_file);
  EXTERN int do_undump(char * p, int item_size, int nitems, FILE * out_file);
  #define dump_file fmt_file
#endif

#define dump_things(base, len)    do_dump  ((char *) &(base), sizeof (base), (int) (len), dump_file)
#define undump_things(base, len)  do_undump((char *) &(base), sizeof (base), (int) (len), dump_file)

/* Use the above for all the other dumping and undumping. */
#define generic_dump(x)   dump_things(x, 1)
#define generic_undump(x) undump_things(x, 1)

#define dump_wd     generic_dump
#define undump_wd   generic_undump
#define dump_hh     generic_dump
#define undump_hh   generic_undump
#define dump_qqqq   generic_dump
#define undump_qqqq generic_undump

#define dump_int(x)     \
do {                    \
  integer x_val = (x);  \
  generic_dump (x_val); \
} while (0)

#define undump_int  generic_undump

#define undump_size(arg1, arg2, arg3, arg4)                     \
do {                                                            \
  undump_int(x);                                                \
                                                                \
  if (x < arg1)                                                 \
    goto bad_fmt;                                               \
                                                                \
  if (x > arg2)                                                 \
  {                                                             \
    fprintf(stdout, "%s%s\n", "---! Must increase the ", arg3); \
    goto bad_fmt;                                               \
  }                                                             \
  else                                                          \
    arg4 = x;                                                   \
} while (0)

// pTeX-ng's macros
#define Hi(x) BYTE3(x)
#define Lo(x) BYTE4(x)
#define nrestmultichr(x)  ((x)!=0 ? ((x) / 8) + 2 - ((x) % 8) : -1)
#define max_cjk_val       0x1000000

#define file_name_size  PATH_MAX
#define min_halfword    0
#define max_halfword    2147483647
#define block_size      1000

#ifdef INCREASEFONTS
  #define min_quarterword 0
  #define max_quarterword 65535
#else
  #define min_quarterword 0
  #define max_quarterword 255
#endif

#define default_mem_top 262140
#define mem_bot 0

#ifdef ALLOCATEMAIN
  EXTERN integer mem_top;
  EXTERN integer mem_max;
  EXTERN integer mem_min;
  #define max_mem_size (max_halfword / sizeof(memory_word) - 1)
#else
  #define mem_top 262140L
  #define mem_max mem_top
  #define mem_min 0
#endif

#ifdef ALLOCATEBUFFER
  #define initial_buf_size   1000
  #define increment_buf_size 2000
  #define buf_size           2000000L
  EXTERN ASCII_code *        buffer;
#else
  #define buf_size           20000L
  EXTERN ASCII_code          buffer[buf_size + 4];
#endif

EXTERN integer first;
EXTERN integer last;
EXTERN integer max_buf_stack;

#define error_line      79
#define half_error_line 50
#define max_print_line  79

#ifdef INCREASEFIXED
  #define max_in_open 127
#else
  #define max_in_open 15
#endif

#ifdef INCREASEFONTS
  #define font_max 65535
#else
  #define font_max 255
#endif

#ifdef ALLOCATEFONT
  #define font_mem_size (max_halfword / sizeof(memory_word) - 1)
  #define initial_font_mem_size   20000
  #define increment_font_mem_size 40000
#else
  #define font_mem_size 100000
#endif

#ifdef ALLOCATESTRING
  #define max_strings (max_halfword / sizeof(pool_pointer) - 1)
  #define pool_size   (max_halfword - 1)
#else
  #define max_strings 16384
  #define pool_size   124000
#endif

#define string_vacancies 100000

#ifdef VARIABLETRIESIZE
  EXTERN integer trie_size;
  #define default_trie_size 1000000 // 60000
#else
  #define trie_size 30000
#endif

#ifdef INCREASETRIEOP
  #define trie_op_size      35111 //  3001
  #define neg_trie_op_size -35111 // -3001
  #define min_trie_op       0
  #define max_trie_op       1000
#else
  #define trie_op_size      751
  #define neg_trie_op_size -751
  #define min_trie_op       0
  #define max_trie_op       500
#endif

#define dvi_buf_size 16384
#define ng_huge

#if !defined (ng_huge)
  #define hash_prime  55711 // 27197 (prime ~ 85% * hash_size)
  #define hash_size   65536 // 32000 32768 9500 25000
#else
  #define hash_prime  445631
  #define hash_size   524288
#endif

/* sec 0113 */
#ifdef INCREASEFONTS
  typedef unsigned short quarterword;
#else
  typedef unsigned char  quarterword;
#endif

/* mem_min may be < 0 */
/* sec 0113 */
typedef int32_t halfword;
typedef halfword pointer;
/* sec 0113 */
typedef struct
{
#ifdef WORDS_BIGENDIAN
  halfword rh;

  union
  {
    halfword lh;

    struct
    {
      quarterword b0, b1;
    };
  };
#else
  union
  {
    struct
    {
      quarterword b1, b0;
    };

    halfword lh;
  };

  halfword rh;
#endif
} two_halves;

typedef struct
{
#ifdef WORDS_BIGENDIAN
  quarterword b0, b1, b2, b3;
#else
  quarterword b3, b2, b1, b0;
#endif
} four_quarters;

typedef union
{
  glue_ratio gr;
  two_halves hh;
  integer cint;
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

#ifdef ALLOCATESTRING
  #define initial_pool_size     40000
  #define increment_pool_size   80000
  EXTERN packed_ASCII_code *    str_pool;
  #define initial_max_strings   5000
  #define increment_max_strings 10000
  EXTERN pool_pointer *         str_start;
#else
  EXTERN packed_ASCII_code      str_pool[pool_size + 1];
  EXTERN pool_pointer           str_start[max_strings + 1];
#endif

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
#ifdef ALLOCATEMAIN
  EXTERN memory_word * main_memory;
  EXTERN memory_word * mem;
#else
  EXTERN memory_word zzzaa[mem_max - mem_bot + 1];
  #define mem (zzzaa - (int)(mem_bot))
#endif

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
/* NOTE: the following really also need to be dynamically allocated */
#ifdef NG_DEBUG
  #ifdef ALLOCATEMAIN
    EXTERN char * zzzab;
    EXTERN char * zzzac;
  #else
    EXTERN char zzzab[mem_max - mem_bot + 1];
    EXTERN char zzzac[mem_max - mem_bot + 1];
  #endif

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

#ifdef INCREASEFONTS
  EXTERN memory_word eqtb[eqtb_size + 1];
  #define xeq_level (zzzad - (int_base))
  EXTERN two_halves zzzae[undefined_control_sequence - hash_base];
#else
  EXTERN memory_word eqtb[eqtb_size + 1];
  #define xeq_level (zzzad - (int_base))
  EXTERN two_halves zzzae[undefined_control_sequence - hash_base];
#endif

EXTERN quarterword zzzad[eqtb_size - int_base + 1];
#define hash (zzzae - hash_base)

EXTERN pointer hash_used;
EXTERN boolean no_new_control_sequence;
EXTERN integer cs_count;

#ifdef ALLOCATESAVESTACK
  #define save_size           65536
  #define initial_save_size   1000
  #define increment_save_size 2000
  EXTERN memory_word * save_stack;
#else
  #define save_size 8000
  EXTERN memory_word save_stack[save_size + 1];
#endif

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

#ifdef ALLOCATENESTSTACK
  #define nest_size           65536
  #define initial_nest_size   100
  #define increment_nest_size 200
  EXTERN list_state_record * nest;
#else
  #define nest_size 200
  EXTERN list_state_record nest[nest_size + 1];
#endif

EXTERN integer nest_ptr;
EXTERN integer max_nest_stack;
EXTERN list_state_record cur_list;

#ifdef ALLOCATEPARAMSTACK
  #define param_size           65536
  #define initial_param_size   100
  #define increment_param_size 200
  EXTERN pointer * param_stack;
#else
  #define param_size 500
  EXTERN pointer param_stack[param_size + 1];
#endif

EXTERN integer param_ptr;
EXTERN integer max_param_stack;

#ifdef ALLOCATEINPUTSTACK
  #define stack_size           65536
  #define initial_stack_size   100
  #define increment_stack_size 200
  EXTERN in_state_record * input_stack;
#else
  #define stack_size 800
  EXTERN in_state_record input_stack[stack_size + 1];
#endif

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
EXTERN const char * c_job_name;
EXTERN str_number output_file_name;
EXTERN str_number log_name;
EXTERN byte_file dvi_file;
EXTERN byte_file tfm_file;
EXTERN byte_file pdf_file;
EXTERN char * dvi_file_name;
EXTERN char * pdf_file_name;
EXTERN char * log_file_name;

#ifdef ALLOCATEFONT
  EXTERN memory_word * font_info;
#else
  EXTERN memory_word font_info[font_mem_size + 1];
#endif

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

#ifdef ALLOCATETRIES
  EXTERN halfword * trie_trl;
  EXTERN halfword * trie_tro;
  EXTERN quarterword * trie_trc;
#else
  EXTERN halfword trie_trl[trie_size + 1];
  EXTERN halfword trie_tro[trie_size + 1];
  EXTERN quarterword trie_trc[trie_size + 1];
#endif

EXTERN small_number hyf_distance[trie_op_size + 1];
EXTERN small_number hyf_num[trie_op_size + 1];
EXTERN trie_op_code hyf_next[trie_op_size + 1];
EXTERN integer op_start[256];

#ifdef ALLOCATEHYPHEN
  #define default_hyphen_prime 8191 // 1009
  EXTERN str_number * hyph_word;
  EXTERN pointer * hyph_list;
  EXTERN integer hyphen_prime;
#else
  #define hyphen_prime 607
  EXTERN str_number hyph_word[hyphen_prime + 1];
  EXTERN pointer hyph_list[hyphen_prime + 1];
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
  #ifdef ALLOCATEINI
    EXTERN packed_ASCII_code * trie_c; /* characters to match */
    EXTERN trie_op_code * trie_o;      /* operations to perform */
    EXTERN trie_pointer * trie_l;      /* left subtrie links */
    EXTERN trie_pointer * trie_r;      /* right subtrie links */
    EXTERN trie_pointer * trie_hash;   /* used to identify equivlent subtries */
    EXTERN char * trie_taken;
  #else
    EXTERN packed_ASCII_code trie_c[trie_size + 1];
    EXTERN trie_op_code trie_o[trie_size + 1];
    EXTERN trie_pointer trie_l[trie_size + 1];
    EXTERN trie_pointer trie_r[trie_size + 1];
    EXTERN trie_pointer trie_hash[trie_size + 1];
    EXTERN boolean trie_taken[trie_size + 1];
  #endif

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
EXTERN gzFile gz_fmt_file;
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
EXTERN boolean flag_open_trace;
EXTERN boolean flag_tex82;
EXTERN boolean flag_c_style;
EXTERN boolean flag_deslash;
EXTERN boolean flag_allow_patterns;
EXTERN boolean flag_reset_exceptions;
EXTERN boolean flag_show_current;
EXTERN boolean flag_civilize;
EXTERN boolean flag_show_numeric;
EXTERN boolean flag_show_missing;
EXTERN boolean flag_show_in_hex;
EXTERN boolean flag_show_tfm;
EXTERN boolean flag_show_csnames;
EXTERN boolean flag_allow_quoted;
EXTERN boolean flag_show_linebreak_stats;
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
EXTERN int jump_used;
EXTERN jmp_buf ng_env;
extern int current_pool_size;
extern int current_max_strings;
extern int current_mem_size;
extern int current_font_mem_size;
extern int current_vf_info_size;
extern int current_save_size;
extern int current_stack_size;
extern int current_nest_size;
extern int current_param_size;
extern int current_buf_size;
extern const char * banner;
extern const char * dist;
extern char log_line[256];
extern char * dvi_directory;
extern char * log_directory;
extern char * aux_directory;
extern char * fmt_directory;
extern char * pdf_directory;
extern clock_t start_time, main_time, finish_time;
// for synctex
EXTERN integer synctex_option;

#include "coerce.h"
#define log_printf(...) fprintf(log_file, __VA_ARGS__)
#endif