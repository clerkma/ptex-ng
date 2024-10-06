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

#ifndef APTEX_H
#define APTEX_H

#ifndef EXTERN
  #define EXTERN static
#endif

// macro for dynamic allocation
#define APTEX_EXTENSION

// tex's infrastructure
#define STAT
#define INITEX

// headers and pragmas
#if defined (_MSC_VER)
  // MSVC
  //  https://msdn.microsoft.com/en-us/library/d9x1s805.aspx

  #if _MSC_VER == 1900
    #pragma warning(disable:4459) // declaration of 'identifier' hides global declaration
    #pragma warning(disable:4311) // 'variable': pointer truncation from 'type1' to 'type2'
  #else
    // https://msdn.microsoft.com/en-us/library/ms235356.aspx
    // Disables performance-critical locking in I/O operations.
    #define _CRT_DISABLE_PERFCRIT_LOCKS
  #endif

  // https://msdn.microsoft.com/en-us/library/ms175759.aspx
  // Secure Template Overloads
  #define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES 1
  #define _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT 1
  #define _USE_MATH_DEFINES 1

  // https://msdn.microsoft.com/en-us/library/8x5x43k7.aspx
  // C/C++ Build Errors
  #pragma warning(disable:4201) // nameless struct/union
  #pragma warning(disable:4996) // a function that was marked with deprecated
  #pragma warning(disable:4701) // potentially uninitialized local variable 'name' used
  #pragma warning(disable:4135) // conversion between different integral types
  #pragma warning(disable:4127) // conditional expression is constant

#elif defined (__clang__)
  // Clang
  //  http://clang.llvm.org/docs/UsersManual.html
  #pragma clang diagnostic ignored "-Wdangling-else"
#endif

// standard C headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>
#include <signal.h>
#include <assert.h>
#include <ctype.h>

// TeX Live's kpathsea
#include <kpathsea/config.h>
#include <kpathsea/c-pathmx.h> // PATH_MAX
#if defined (__APPLE__)
#define __GNU_LIBRARY__
#endif
#include <kpathsea/getopt.h>   // getopt_long_only
#include <kpathsea/tex-file.h> // kpse_find_file
#include <kpathsea/types.h>    // RPId64
#include <kpathsea/version.h>  // kpathsea_version_string
#define USE_KPATHSEA

// ptexenc for kanji processing
#include <ptexenc/ptexenc.h>
#include <ptexenc/unicode.h>

// zlib for format file and SyncTeX
#include "zlib.h"

// libmruby
#if defined (USE_MRUBY)
#include "mruby.h"
#include "mruby/compile.h"
#include "aptex-mruby.h"
#endif

// internal headers
#include "aptex-utils.h"

#ifdef __cplusplus
extern "C" {
#endif

// types: integers
typedef uint8_t ASCII_code;
typedef int32_t KANJI_code;
typedef uint8_t eight_bits;
typedef uint8_t packed_ASCII_code;
typedef uint8_t small_number;

// types: files
typedef struct {
  void * file_data;
  uint32_t file_type;
} alpha_file; // fopen or popen
typedef FILE * byte_file;  // stdio
typedef void * word_file;  // stdio/zlib

#ifdef link
  #undef link
#endif

#ifdef APTEX_EXTENSION
  #define def_const(sym, val0, val1) \
    enum {sym = val1}
  #define def_alloc(sym, val0, val1, init, incr) \
    EXTERN uint32_t current_##sym; enum {sym = val1, initial_##sym = init, increment_##sym = incr}
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
    EXTERN int current_##sym; enum {sym = val0}
  #define def_type(sym, t0, t1) \
    typedef t0 sym
  #define def_array(sym, type, size) \
    EXTERN type sym[size]
  #define def_alter(sym, type, val) \
    enum {sym = val}
#endif

def_const(file_name_size,     PATH_MAX, PATH_MAX);

def_const(min_quarterword,    0,      0);           // {smallest allowable value in a |quarterword|}
def_const(max_quarterword,    255,    65535);       // {largest allowable value in a |quarterword|}
def_const(min_halfword,       0,      0);           // {smallest allowable value in a |halfword|}
def_const(max_halfword,       65536,  2147483647);  // {largest allowable value in a |halfword|}
def_const(max_cjk_val,        0x1000000, 0x1000000);// {to separate wchar and kcatcode}

def_const(max_in_open,        15,     15);          // {maximum number of input files and error insertions that can be going on simultaneously}
def_const(block_size,         1000,   1000);

def_const(default_mem_top,    0,      2621400);
def_const(error_line,         79,     79);          // {width of context lines on terminal error messages}
def_const(half_error_line,    50,     50);          // {width of first lines of contexts in terminal error messages; should be between 30 and |error_line-15|}
def_const(max_print_line,     79,     79);          // {width of longest text lines output; should be at least 60}
def_const(mem_bot,            0,      0);

def_const(font_max,           255,    65535);
def_const(string_vacancies,   100000, 100000);
def_const(trie_op_size,       751,    35111);
def_const(neg_trie_op_size,   -751,   -35111);
def_const(min_trie_op,        0,      0);
def_const(max_trie_op,        500,    1000);
def_const(dvi_buf_size,       16384,  16384);
def_const(hash_prime,         55711,  445631);
def_const(hash_size,          65536,  524288);

def_alloc(buf_size,       200000, 2000000,                10000,  2000);
def_alloc(font_mem_size,  100000, (max_halfword / 8 - 1), 20000,  40000);
def_alloc(pool_size,      124000, (max_halfword - 1),     40000,  80000);
def_alloc(max_strings,    16384,  (max_halfword / 8 - 1), 5000,   10000);
def_alloc(save_size,      8000,   65536,                  1000,   2000);
def_alloc(nest_size,      200,    65536,                  100,    200);
def_alloc(param_size,     500,    65536,                  100,    200);
def_alloc(stack_size,     800,    65536,                  100,    200);

EXTERN int current_mem_size;

def_type(integer,     int32_t, int64_t);
def_type(halfword,    int16_t, int32_t);
def_type(quarterword, uint8_t, uint16_t);
def_type(glue_ratio,  float,   double);
def_type(real,        float,   double);

typedef integer pool_pointer;
typedef integer str_number;
typedef integer scaled;
typedef integer nonnegative_integer;

def_alter(mem_top, integer, 2621400);
def_alter(mem_max, integer, 2621400);
def_alter(mem_min, integer, 0);

def_const(max_mem_size, 0, (max_halfword / 8 - 1));

def_alter(trie_size, integer, 30000);

def_const(default_trie_size, 0, 1000000); // 60000

/* mem_min may be < 0 */
/* sec 0113 */
typedef halfword pointer; // {a flag or a location in |mem| or |eqtb|}
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
  integer       cint;
  glue_ratio    gr;
  two_halves    hh;
  four_quarters qqqq;
} memory_word;

#include "aptex-defs.h"

/* sec 0150 */
typedef uint32_t glue_ord;
/* sec 0212 */
typedef struct
{
  int mode_field;
  int dir_field, adj_dir_field;
  scaled pdisp_field;
  pointer head_field, tail_field, pnode_field, last_jchr_field;
  boolean disp_called_field;
  integer inhibit_glue_flag_field;
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

EXTERN integer bad;                           // {is some ``constant'' wrong?}

EXTERN ASCII_code xord[256];                  // {specifies conversion of input characters}
EXTERN ASCII_code xchr[256];                  // { specifies conversion of output characters }

EXTERN ASCII_code name_of_file[file_name_size + 4];
EXTERN integer name_length;                   // {this many characters are actually relevant in |name_of_file| (the rest are blank)}

def_array(buffer, ASCII_code, buf_size + 4);  // {lines of characters being read}

EXTERN uint32_t first;                         // {the first unused position in |buffer|}
EXTERN uint32_t last;                          // {end of the line just input to |buffer|}
EXTERN uint32_t max_buf_stack;                 // {largest index used in |buffer|}

EXTERN alpha_file term_in;                    // {the terminal as an input file}
EXTERN alpha_file term_out;                   // {the terminal as an output file}

def_array(str_pool, packed_ASCII_code, pool_size + 1);  // {the characters}
def_array(str_start, pool_pointer, max_strings + 1);    // {the starting pointers}

EXTERN pool_pointer pool_ptr;                 // {first unused position in |str_pool|}
EXTERN str_number   str_ptr;                  // {number of the current string being created}
EXTERN pool_pointer init_pool_ptr;            // {the starting value of |pool_ptr|}
EXTERN str_number   init_str_ptr;             // {the starting value of |str_ptr|}

EXTERN alpha_file log_file;                   // {transcript of \TeX\ session}
EXTERN uint32_t   selector;                   // {where to print a message}
EXTERN uint32_t   dig[23 + 1];                // {digits in a number being output}
EXTERN integer    tally;                      // {the number of characters recently printed}
EXTERN integer    term_offset;                // {the number of characters on the current terminal line}
EXTERN integer    file_offset;                // {the number of characters on the current file line}
EXTERN ASCII_code trick_buf[error_line + 1];  // {circular buffer for pseudoprinting}
EXTERN ASCII_code trick_buf2[error_line + 1]; // {pTeX: buffer for KANJI}
EXTERN ASCII_code kcode_pos;                  // {pTeX: denotes whether first byte or second byte of KANJI 1..2:2byte-char, 11..13:3byte-char, 21..24:4byte-char}
EXTERN ASCII_code kcp;                        // {temporary kcode_pos}
EXTERN ASCII_code prev_char;
EXTERN integer trick_count;                   // {threshold for pseudoprinting, explained later}
EXTERN integer first_count;                   // {another variable for pseudoprinting}

EXTERN int interaction;                       // {current level of interaction}

EXTERN boolean deletions_allowed;             // {is it safe for |error| to call |get_token|? }
EXTERN boolean set_box_allowed;               // {is it safe to do a \.{\\setbox} assignment?}
EXTERN int history;                           // {has the source input been clean so far?}
EXTERN int error_count;                       // {the number of scrolled errors since the last paragraph ended}

EXTERN char * help_line[6];                   // {helps for the next |error|}
EXTERN int help_ptr;                          // {the number of help lines present}
EXTERN boolean use_err_help;                  // {should the |err_help| list be shown?}

EXTERN integer interrupt;                     // {should \TeX\ pause for instructions?}
EXTERN boolean OK_to_interrupt;               // {should interrupts be observed?}

EXTERN boolean arith_error;                   // {has arithmetic overflow occurred recently?}
EXTERN scaled ng_remainder;                   // {amount subtracted to get an exact division}

EXTERN integer randoms[55];                   // {the last 55 random values generated}
EXTERN uint32_t j_random;                     // {the number of unused |randoms|}
EXTERN scaled random_seed;                    // {the default random seed}
EXTERN integer two_to_the[32];                // {powers of two}
EXTERN integer spec_log[29];                  // {special logarithms}

EXTERN halfword temp_ptr;                     // {a pointer variable for occasional emergency use}

def_array(mem, memory_word, mem_max - mem_bot + 1);

EXTERN memory_word * main_memory;
EXTERN pointer lo_mem_max;                    // {the largest location of variable-size memory in use}
EXTERN pointer hi_mem_min;                    // {the smallest location of one-word memory in use}
EXTERN integer var_used, dyn_used;            // {how much memory is in use}

EXTERN pointer avail;                         // {head of the list of available one-word nodes}
EXTERN pointer mem_end;                       // {the last one-word node used in |mem|}
EXTERN pointer mem_start;
/* sec 0124 */
EXTERN pointer rover;                         // {points to some node in the list of empties}

/* sec 0165 */
#ifdef APTEX_DEBUG
  //define_array(zzzab, char, mem_max - mem_bot + 1);
  EXTERN char zzzab[mem_max - mem_bot + 1];
  //define_array(zzzac, char, mem_max - mem_bot + 1);
  EXTERN char zzzac[mem_max - mem_bot + 1];
  #define freearr (zzzab - mem_bot)
  #define wasfree (zzzac - mem_bot)
#endif

EXTERN pointer was_mem_end, was_lo_max, was_hi_min;
EXTERN boolean panicking;

EXTERN integer font_in_short_display;         // {an internal font number}

EXTERN integer depth_threshold;               // {maximum nesting depth in box displays}
EXTERN integer breadth_max;                   // {maximum number of items shown at the same list level}

def_array(nest, list_state_record, nest_size + 1);

EXTERN integer nest_ptr;                      // {first unused location of |nest|}
EXTERN integer max_nest_stack;                // {maximum of |nest_ptr| when pushing}
EXTERN list_state_record cur_list;            // {the ``top'' semantic state}
EXTERN int shown_mode;                        // {most recent mode shown by \.{\\tracingcommands}}

EXTERN int old_setting;
EXTERN integer sys_time, sys_day, sys_month, sys_year; // {date and time supplied by external system}

EXTERN memory_word eqtb[eqtb_size + 1];
EXTERN quarterword zzzad[eqtb_size - int_base + 1];
#define xeq_level (zzzad - int_base)
EXTERN two_halves zzzae[undefined_control_sequence - hash_base];
#define hash (zzzae - hash_base)              // {the hash table}
EXTERN pointer hash_used;                     // {allocation pointer for |hash|}
EXTERN boolean no_new_control_sequence;       // {are new identifiers legal?}
EXTERN integer cs_count;                      // {total number of known identifiers}
// #
EXTERN two_halves prim[prim_size + 1]; // {the primitives table}
EXTERN pointer prim_used; // {allocation pointer for |prim|}

def_array(save_stack, memory_word, save_size + 1);

EXTERN integer save_ptr;                      // {first unused entry on |save_stack|}
EXTERN integer max_save_stack;                // {maximum usage of save stack}
EXTERN int cur_level;                         // {current nesting level for groups}
EXTERN int cur_group;                         // {current group type}
EXTERN integer cur_boundary;                  // {where the current level begins}

EXTERN integer mag_set;                       // {if nonzero, this magnification should be used henceforth}

EXTERN int cur_cmd;                           // {current command set by |get_next|}
EXTERN halfword cur_chr;                      // {operand of current command}
EXTERN pointer cur_cs;                        // {control sequence found here, zero if none found}
EXTERN halfword cur_tok;                      // {packed representative of |cur_cmd| and |cur_chr|}

def_array(input_stack, in_state_record, stack_size + 1);

EXTERN integer input_ptr;                     // {first unused location of |input_stack|}
EXTERN integer max_in_stack;                  // {largest value of |input_ptr| when pushing}
EXTERN integer high_in_open;
EXTERN in_state_record cur_input;             // {the ``top'' input state, according to convention (1)}

EXTERN int in_open;                           // {the number of lines in the buffer, less one}
EXTERN integer open_parens;                   // {the number of open text files}
EXTERN alpha_file input_file[max_in_open + 1];
EXTERN integer line;                          // {current line number in the current source file}
EXTERN integer line_stack[max_in_open + 1];

EXTERN int scanner_status;                    // {can a subfile end now?}
EXTERN pointer warning_index;                 // {identifier relevant to non-|normal| scanner status}
EXTERN pointer def_ref;                       // {reference count of token list being defined}

def_array(param_stack, pointer, param_size + 1); // {token list pointers for parameters}

EXTERN integer param_ptr;                     // {first unused entry in |param_stack|}
EXTERN integer max_param_stack;               // {largest value of |param_ptr|, will be |<=param_size+9|}

EXTERN integer align_state;                   // {group level with respect to current alignment}

EXTERN integer base_ptr;                      // {shallowest level shown by |show_context|}

EXTERN pointer par_loc;                       // {location of `\.{\\par}' in |eqtb|}
EXTERN halfword par_token;                    // {token representing `\.{\\par}'}

EXTERN boolean skip_mode;
EXTERN boolean force_eof;                     // {should the next \.{\\input} be aborted early?}
EXTERN pointer cur_mark[6];                   // {token lists for marks}

EXTERN int long_state;                        // {governs the acceptance of \.{\\par}}
EXTERN pointer pstack[10];                    // {arguments supplied to a macro}

EXTERN integer cur_val;                       // {value returned by numeric scanners}
EXTERN int cur_val_level;                     // {the ``level'' of this value}

EXTERN int radix;                             // {|scan_int| sets this to 8, 10, 16, or zero}

EXTERN int cur_order;                         // {order of infinity found by |scan_dimen|}

EXTERN alpha_file read_file[16];              // {used for \.{\\read}}
EXTERN char read_open[17];                    // {state of |read_file[n]|}

EXTERN pointer cond_ptr;                      // {top of the condition stack}
EXTERN int if_limit;                          // {upper bound on |fi_or_else| codes}
EXTERN int cur_if;                            // {type of conditional being worked on}
EXTERN integer if_line;                       // {line where that conditional began}

EXTERN integer skip_line;                     // {skipping began here}

EXTERN str_number cur_name;                   // {name of file just scanned}
EXTERN str_number cur_area;                   // {file area just scanned, or \.{""}}
EXTERN str_number cur_ext;                    // {file extension just scanned, or \.{""}}

EXTERN pool_pointer area_delimiter;           // {the most recent `\./', if any}
EXTERN pool_pointer ext_delimiter;            // {the most recent `\..', if any}

EXTERN integer format_default_length;
EXTERN char * TEX_format_default;

EXTERN boolean name_in_progress;              // {is a file name being scanned?}
EXTERN str_number job_name;                   // {principal file name}
EXTERN boolean log_opened;                    // {has the transcript file been opened?}
EXTERN boolean quoted_file_name;

EXTERN byte_file dvi_file;                    // {the device-independent output goes here}
EXTERN str_number output_file_name;           // {full name of the output file}
EXTERN str_number log_name;                   // {full name of the log file}

EXTERN byte_file tfm_file;

def_array(font_info, memory_word, font_mem_size + 1); // {pTeX: use halfword for |char_type| table.}

EXTERN eight_bits font_dir[font_max + 1];     // {pTeX: direction of fonts, 0 is default, 1 is Yoko, 2 is Tate}
EXTERN eight_bits font_enc[font_max + 1];     // {pTeX: encoding of fonts, 0 is default, 1 is JIS, 2 is Unicode}
EXTERN integer font_num_ext[font_max + 1];    // {pTeX: number of the |char_type| table.}
EXTERN eight_bits jfm_enc;                    // {pTeX: holds scanned result of encoding}
EXTERN font_index fmem_ptr;                   // {first unused word of |font_info|}
EXTERN internal_font_number font_ptr;         // {largest internal font number in use}
EXTERN four_quarters font_check[font_max + 1];// {check sum}
EXTERN scaled font_size[font_max + 1];        // {``at'' size}
EXTERN scaled font_dsize[font_max + 1];       // {``design'' size}
EXTERN font_index font_params[font_max + 1];  // {how many font parameters are present}
EXTERN str_number font_name[font_max + 1];    // {name of the font}
EXTERN str_number font_area[font_max + 1];    // {area of the font}
EXTERN eight_bits font_bc[font_max + 1];      // {beginning (smallest) character code}
EXTERN eight_bits font_ec[font_max + 1];      // {ending (largest) character code}
EXTERN pointer font_glue[font_max + 1];       // {glue specification for interword space, |null| if not allocated}
EXTERN boolean font_used[font_max + 1];       // {has a character from this font actually appeared in the output?}
EXTERN integer hyphen_char[font_max + 1];     // {current \.{\\hyphenchar} values}
EXTERN integer skew_char[font_max + 1];       // {current \.{\\skewchar} values}
EXTERN font_index bchar_label[font_max + 1];  // {start of |lig_kern| program for left boundary character, |non_address| if there is none}
EXTERN short font_bchar[font_max + 1];        // {right boundary character, |non_char| if there is none}
EXTERN short font_false_bchar[font_max + 1];  // {|font_bchar| if it doesn't exist in the font, otherwise |non_char|}

EXTERN integer char_base[font_max + 1];       // {base addresses for |char_info|}
EXTERN integer ctype_base[font_max + 1];      // {pTeX: base addresses for KANJI character type parameters}
EXTERN integer width_base[font_max + 1];      // {base addresses for widths}
EXTERN integer height_base[font_max + 1];     // {base addresses for heights}
EXTERN integer depth_base[font_max + 1];      // {base addresses for depths}
EXTERN integer italic_base[font_max + 1];     // {base addresses for italic corrections}
EXTERN integer lig_kern_base[font_max + 1];   // {base addresses for ligature/kerning programs}
EXTERN integer kern_base[font_max + 1];       // {base addresses for kerns}
EXTERN integer exten_base[font_max + 1];      // {base addresses for extensible recipes}
EXTERN integer param_base[font_max + 1];      // {base addresses for font parameters}

EXTERN four_quarters null_character;          // {nonexistent character information}

EXTERN integer total_pages;                   // {the number of pages that have been shipped out}
EXTERN scaled max_v;                          // {maximum height-plus-depth of pages shipped so far}
EXTERN scaled max_h;                          // {maximum width of pages shipped so far}
EXTERN integer max_push;                      // {deepest nesting of |push| commands encountered so far}
EXTERN integer last_bop;                      // {location of previous |bop| in the \.{DVI} output}
EXTERN integer dead_cycles;                   // {recent outputs that didn't ship anything out}
EXTERN boolean doing_leaders;                 // {are we inside a leader box?}

EXTERN quarterword c, f;
EXTERN boolean dir_used;                      // {Is this dvi extended?}
EXTERN scaled rule_ht, rule_dp, rule_wd;      // {size of current rule being output}
EXTERN pointer g;                             // {current glue specification}
EXTERN integer lq, lr;                        // {quantities used in calculations for leaders}
EXTERN eight_bits dvi_buf[dvi_buf_size + 4];  // {buffer for \.{DVI} output}
EXTERN dvi_index half_buf;                    // {half of |dvi_buf_size|}
EXTERN dvi_index dvi_limit;                   // {end of the current half buffer}
EXTERN dvi_index dvi_ptr;                     // {the next available buffer address}
EXTERN integer dvi_offset;                    // {|dvi_buf_size| times the number of times the output buffer has been fully emptied}
EXTERN integer dvi_gone;                      // {the number of bytes already output to |dvi_file|}

EXTERN pointer down_ptr, right_ptr;           // {heads of the down and right stacks}

EXTERN scaled dvi_h, dvi_v;                   // {a \.{DVI} reader program thinks we are here}
EXTERN scaled cur_h, cur_v;                   // {\TeX\ thinks we are here}
EXTERN internal_font_number dvi_f;            // {the current font}
EXTERN integer cur_s;                         // {current depth of output box nesting, initially $-1$}

EXTERN scaled total_stretch[4], total_shrink[4]; // {glue found by |hpack| or |vpack|}
EXTERN integer last_badness;                  // {badness of the most recently packaged box}

EXTERN integer pack_begin_line;               // {source file line where the current paragraph or alignment began; a negative value denotes alignment}

EXTERN pointer adjust_tail;                   // {tail of adjustment list}
EXTERN scaled last_disp;                      // {displacement at end of list}
EXTERN pointer cur_kanji_skip;
EXTERN pointer cur_xkanji_skip;

EXTERN two_halves empty_field;
EXTERN four_quarters null_delimiter;

EXTERN pointer cur_mlist;                     // {beginning of mlist to be translated}
EXTERN small_number cur_style;                // {style code at current place in the list}
EXTERN small_number cur_size;                 // {size code corresponding to |cur_style|}
EXTERN scaled cur_mu;                         // {the math unit width corresponding to |cur_size|}
EXTERN boolean mlist_penalties;               // {should |mlist_to_hlist| insert penalties?}

EXTERN internal_font_number cur_f;            // {the |font| field of a |math_char|}
EXTERN quarterword cur_c;                     // {the |character| field of a |math_char|}
EXTERN four_quarters cur_i;                   // {the |char_info| of a |math_char|, or a lig/kern instruction}

EXTERN integer magic_offset;                  // {used to find inter-element spacing}

EXTERN pointer cur_align;                     // {current position in preamble list}
EXTERN pointer cur_span;                      // {start of currently spanned columns in preamble list}
EXTERN pointer cur_loop;                      // {place to copy when extending a periodic preamble}
EXTERN pointer align_ptr;                     // {most recently pushed-down alignment stack node}
EXTERN pointer cur_head, cur_tail;            // {adjustment list pointers}

EXTERN pointer just_box;                      // {the |hlist_node| for the last line of the new paragraph}

EXTERN pointer passive;                       // {most recent node on passive list}
EXTERN pointer printed_node;                  // {most recent node that has been printed}
EXTERN halfword pass_number;                  // {the number of passive nodes allocated on this pass}

EXTERN scaled active_width[8];                // {distance from first active node to~|cur_p|}
EXTERN scaled cur_active_width[8];            // {distance from current active node}
EXTERN scaled background[8];                  // {length of an ``empty'' line}
EXTERN scaled break_width[8];                 // {length being computed after current break}

EXTERN boolean no_shrink_error_yet;           // {have we complained about infinite shrinkage?}

EXTERN pointer cur_p;                         // {the current breakpoint under consideration}
EXTERN boolean chain;                         // {chain current line and next line?}
EXTERN boolean second_pass;                   // {is this our second attempt to break this paragraph?}
EXTERN boolean final_pass;                    // {is this our final attempt to break this paragraph?}
EXTERN integer threshold;                     // {maximum badness on feasible lines}

EXTERN scaled disc_width;                     // {the length of discretionary material preceding a break}

EXTERN halfword easy_line;                    // {line numbers |>easy_line| are equivalent in break nodes}
EXTERN halfword last_special_line;            // {line numbers |>last_special_line| all have the same width}
EXTERN scaled first_width;                    // {the width of all lines |<=last_special_line|, if no \.{\\parshape} has been specified}
EXTERN scaled second_width;                   // {the width of all lines |>last_special_line|}
EXTERN scaled first_indent;                   // {left margin to go with |first_width|}
EXTERN scaled second_indent;                  // {left margin to go with |second_width|}

EXTERN pointer best_bet;                      // {use this passive node and its predecessors}
EXTERN integer fewest_demerits;               // {the demerits associated with |best_bet|}
EXTERN halfword best_line;                    // {line number following the last line of the new paragraph}
EXTERN integer actual_looseness;              // {the difference between |line_number(best_bet)| and the optimum |best_line|}
EXTERN integer line_diff;                     // {the difference between the current line number and the optimum |best_line|}

EXTERN int hc[66];                            // {word to be hyphenated}
EXTERN int hn;                                // {the number of positions occupied in |hc|}
EXTERN halfword ha, hb;                       // {nodes |ha..hb| should be replaced by the hyphenated result}
EXTERN int hf;                                // {font number of the letters in |hc|}
EXTERN int hu[66];                            // {like |hc|, before conversion to lowercase}
EXTERN int hyf_char;                          // {hyphen character of the relevant font}
EXTERN int cur_lang, init_cur_lang;           // {current hyphenation table of interest}
EXTERN integer l_hyf, r_hyf;                  // {limits on fragment sizes}
EXTERN integer init_l_hyf, init_r_hyf;
EXTERN halfword hyf_bchar;                    // {boundary character after $c_n$}

EXTERN char hyf[68];                          // {odd values indicate discretionary hyphens}
EXTERN pointer init_list;                     // {list of punctuation characters preceding the word}
EXTERN boolean init_lig;                      // {does |init_list| represent a ligature?}
EXTERN boolean init_lft;                      // {if so, did the ligature involve a left boundary?}

EXTERN int hyphen_passed;                     // {first hyphen in a ligature, if any}

EXTERN halfword cur_l, cur_r;                 // {characters before and after the cursor}
EXTERN pointer cur_q;                         // {where a ligature should be detached}
EXTERN pointer lig_stack;                     // {unfinished business to the right of the cursor}
EXTERN boolean ligature_present;              // {should a ligature node be made for |cur_l|?}
EXTERN boolean lft_hit, rt_hit;               // {did we hit a ligature with a boundary character?}

def_array(trie_trl, halfword, trie_size + 1);
def_array(trie_tro, halfword, trie_size + 1);
def_array(trie_trc, quarterword, trie_size + 1);

EXTERN small_number hyf_distance[trie_op_size + 1]; // {position |k-j| of $n_j$}
EXTERN small_number hyf_num[trie_op_size + 1];  // {value of $n_j$}
EXTERN trie_op_code hyf_next[trie_op_size + 1]; // {continuation code}
EXTERN integer op_start[256];                   // {offset for current language}

def_array(hyph_word, str_number, hyphen_prime + 1); // {exception words}
def_array(hyph_list, pointer, hyphen_prime + 1);    // {lists of hyphen positions}

def_alter(hyphen_prime, integer, 607);

#ifdef APTEX_EXTENSION
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
  def_array(trie_c, packed_ASCII_code, trie_size + 1);  /* characters to match */
  def_array(trie_o, trie_op_code, trie_size + 1);       /* operations to perform */
  def_array(trie_l, trie_pointer, trie_size + 1);       /* left subtrie links */
  def_array(trie_r, trie_pointer, trie_size + 1);       /* right subtrie links */
  def_array(trie_hash, trie_pointer, trie_size + 1);    /* used to identify equivlent subtries */
  def_array(trie_taken, char, trie_size + 1); // char / boolean
  EXTERN trie_pointer trie_ptr;
  EXTERN trie_pointer trie_min[256];
  EXTERN trie_pointer trie_max;
  EXTERN boolean trie_not_ready;
#endif

EXTERN scaled best_height_plus_depth;         // {height of the best box, without stretching or shrinking}

EXTERN pointer page_tail;                     // {the final node on the current page}
EXTERN int page_contents;                     // {what is on the current page so far?}
EXTERN scaled page_max_depth;                 // {maximum box depth on page being built}
EXTERN pointer best_page_break;               // {break here to get the best page known so far}
EXTERN integer least_page_cost;               // {the score for this currently best page}
EXTERN scaled best_size;                      // {its |page_goal|}

EXTERN scaled page_so_far[8];                 // {height and glue of the current page}
EXTERN pointer last_glue;                     // {used to implement \.{\\lastskip}}
EXTERN integer last_penalty;                  // {used to implement \.{\\lastpenalty}}
EXTERN scaled last_kern;                      // {used to implement \.{\\lastkern}}
EXTERN integer last_node_type;                // {used to implement \.{\\lastnodetype}}
EXTERN integer last_node_subtype;             // {used to implement \.{\\lastnodesubtype}}
EXTERN integer insert_penalties;              // {sum of the penalties for held-over insertions}

EXTERN boolean output_active;                 // {are we in the midst of an output routine?}

EXTERN internal_font_number main_f;           // {the current font}
EXTERN four_quarters main_i;                  // {character information bytes for |cur_l|}
EXTERN four_quarters main_j;                  // {ligature/kern command}
EXTERN font_index main_k;                     // {index into |font_info|}
EXTERN pointer main_p;                        // {temporary register for list manipulation}
EXTERN integer main_s;                        // {space factor value}
EXTERN halfword bchar;                        // {right boundary character of current font, or |non_char|}
EXTERN halfword false_bchar;                  // {nonexistent character matching |bchar|, or |non_char|}
EXTERN boolean cancel_boundary;               // {should the left boundary be ignored?}
EXTERN boolean ins_disc;                      // {should we insert a discretionary node?}

EXTERN pointer cur_box;                       // {box to be placed into its context}

EXTERN halfword after_token;                  // {zero, or a saved token}

EXTERN boolean long_help_seen;                // {has the long \.{\\errmessage} help been used?}

EXTERN str_number format_ident;

EXTERN word_file fmt_file;                    // {for input or output of format information}

EXTERN integer ready_already;                 // {a sacrifice of purity for economy}

EXTERN alpha_file write_file[16];
EXTERN boolean write_open[18];

EXTERN pointer write_loc;                     // {|eqtb| address of \.{\\write}}

EXTERN scaled cur_page_width;                 // {"physical" width of page being shipped}
EXTERN scaled cur_page_height;                // {"physical" height of page being shipped}
EXTERN integer pdf_last_x_pos;
EXTERN integer pdf_last_y_pos;

EXTERN boolean eTeX_mode;                     // {identifies compatibility and extended mode}

EXTERN boolean eof_seen[max_in_open + 1];     // {has eof been seen?}

EXTERN scaled revdisp;                        // {temporary value of displacement}
EXTERN pointer LR_ptr;                        // {stack of LR codes for |hpack|, |ship_out|, and |init_math|}
EXTERN integer LR_problems;                   // {counts missing begins and ends}
EXTERN small_number cur_dir;                  // {current text direction}

EXTERN pointer pseudo_files;                  // {stack of pseudo files}

EXTERN pointer grp_stack[max_in_open + 1];    // {initial |cur_boundary|}
EXTERN pointer if_stack[max_in_open + 1];     // {initial |cond_ptr|}

EXTERN halfword max_reg_num;                  // {largest allowed register number}
EXTERN const char * max_reg_help_line;        // {first line of help message}

EXTERN pointer sa_root[7];                    // {roots of sparse arrays}
EXTERN pointer cur_ptr;                       // {value returned by |new_index| and |find_sa_element|}
EXTERN memory_word sa_null;                   // {two |null| pointers}

EXTERN pointer sa_chain;                      // {chain of saved sparse array entries}
EXTERN quarterword sa_level;                  // {group level for |sa_chain|}

EXTERN pointer last_line_fill;                // {the |par_fill_skip| glue node of the new paragraph}
EXTERN boolean do_last_line_fit;              // {special algorithm for last line of paragraph?}
EXTERN small_number active_node_size;         // {number of words in active nodes}
EXTERN scaled fill_width[3];                  // {infinite stretch components of |par_fill_skip|}
EXTERN scaled best_pl_short[4];               // {|shortfall| corresponding to |minimal_demerits|}
EXTERN scaled best_pl_glue[4];                // {corresponding glue stretch or shrink}

EXTERN trie_pointer hyph_start;               // {root of the packed trie for |hyph_codes|}
EXTERN trie_pointer hyph_index;               // {pointer to hyphenation codes for |cur_lang|}

EXTERN pointer disc_ptr[4];                   // {list pointers}

EXTERN integer minimal_demerits[4];           // {best total demerits known for current line class and position, given the fitness}
EXTERN integer minimum_demerits;              // {best total demerits known for current line class and position}
EXTERN pointer best_place[4];                 // {how to achieve |minimal_demerits|}
EXTERN halfword best_pl_line[4];              // {corresponding line number}

EXTERN integer dvi_dir;                       // {a \.{DVI} reader program thinks we direct to}
EXTERN integer cur_dir_hv;                    // {\TeX\ thinks we direct to}
EXTERN eight_bits page_dir;

EXTERN pointer first_char;                    // {first printable character}
EXTERN pointer last_char;                     // {last printable character}
EXTERN boolean find_first_char;               // {find for a first printable character?}
EXTERN int fbyte;

// eTeX
EXTERN boolean is_print_utf8;
EXTERN str_number last_tokens_string;
EXTERN integer epochseconds;
EXTERN integer microseconds;

// for SyncTeX
EXTERN integer synctex_option;
EXTERN boolean stop_at_space;
EXTERN boolean is_in_csname;

EXTERN struct {
  // enviroment in UTF-8
  int     argc;
  char ** argv;
  char * aptex_fmt;
  char * aptex_src;
  char * aptex_job;
  char * aptex_map;
  // time variables
  clock_t time_start;
  clock_t time_main;
  clock_t time_finish;
  // tracing options
  boolean trace_mem;
  boolean trace_realloc;
  boolean trace_lbrk;
  // flags
  boolean flag_initex;
  boolean flag_tex82;
  boolean flag_shell_escape;
  boolean flag_compact_fmt;
  boolean flag_reset_trie;
  boolean flag_reset_hyphen;
  boolean flag_suppress_f_ligs;
  boolean flag_merge_kanji_baseline;
} aptex_env;

#include "aptex-funs.h"

#ifdef __cplusplus
}
#endif

#endif
