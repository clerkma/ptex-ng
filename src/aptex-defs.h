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

#ifndef APTEX_MACROS_H
#define APTEX_MACROS_H

#define pTeX_version 4
#define pTeX_minor_version 1
#define pTeX_revision ".1"
#define pTeX_version_string "-p4.1.1"

#define upTeX_version 1
#define upTeX_revision ".30"
#define upTeX_version_string "-u1.30"

#define eTeX_version        2         // { \.{\\eTeXversion} }
#define eTeX_revision       ".6"      // { \.{\\eTeXrevision} }
#define eTeX_version_string "-2.6"    // {current \eTeX\ version}

#define epTeX_version_string "-210218"
#define epTeX_version_number 210218

#define TeXXeT_code         0         // {the \TeXXeT\ feature is optional}
#define eTeX_states         1         // {number of \eTeX\ state variables in |eqtb|}

#define eTeX_ex             (eTeX_mode == true)

// predefined macro
#define abs(x)      ((integer)(x) >= 0 ? (integer)(x) : (integer)-(x))
#define chr(x)      (x)
#define odd(x)      ((x) % 2)
#define round(x)    aptex_utils_round((real) (x))
#define decr(x)     --(x)
#define incr(x)     ++(x)
#define negate(x)   x = -x
#define empty       0
#define KANJI(x)    x
#define tokanji(x)  x
#define tonum(x)    x
// |scaled| data is equivalent to integer
#define sc cint
//#
#define font_base 0
/* sec 0022 */
#define null_code       0     // 0
#define carriage_return 015   // 13
#define invalid_code    0177  // 127
/* sec 0036 */
#define loc cur_input.loc_field
/* sec 0040 */
#define length(s) (str_start[(s) + 1] - str_start[(s)])
/* sec 0041 */
#define cur_length    (pool_ptr - str_start[str_ptr])
#define flush_char()  decr(pool_ptr)
/* sec 0054 */
enum
{
  no_print     = 16,  // {|selector| setting that makes data disappear}
  term_only    = 17,  // {printing is destined for the terminal only}
  log_only     = 18,  // {printing is destined for the transcript file only}
  term_and_log = 19,  // {normal |selector| setting}
  pseudo       = 20,  // {special |selector| setting for |show_context|}
  new_string   = 21,  // {printing is deflected to the string pool}
  max_selector = 21,  // {highest selector setting}
};
/* sec 0073 */
enum
{
  batch_mode      = 0,  // {omits all stops and omits terminal output}
  nonstop_mode    = 1,  // {omits all stops}
  scroll_mode     = 2,  // {omits error stops}
  error_stop_mode = 3,  // {stops at every opportunity to interact}
};
/* sec 0076 */
enum
{
  spotless             = 0, // {|history| value when nothing has been amiss yet}
  warning_issued       = 1, // {|history| value when |begin_diagnostic| has been called}
  error_message_issued = 2, // {|history| value when |error| has been called}
  fatal_error_stop     = 3, // {|history| value when termination was premature}
};
/* sec 0079 */
#define help0()     tex_help(0)
#define help1(...)  tex_help(1, __VA_ARGS__)
#define help2(...)  tex_help(2, __VA_ARGS__)
#define help3(...)  tex_help(3, __VA_ARGS__)
#define help4(...)  tex_help(4, __VA_ARGS__)
#define help5(...)  tex_help(5, __VA_ARGS__)
#define help6(...)  tex_help(6, __VA_ARGS__)
/* sec 0101 */
#define unity 0200000 // {$2^{16}$, represents 1.00000}
#define two   0400000 // {$2^{17}$, represents 2.00000}
/* sec 0095 @ metafont */
#define el_gordo 017777777777
/* sec 0105 @ metafont */
#define fraction_half 01000000000  // {$2^{27}$, represents 0.50000000}
#define fraction_one  02000000000  // {$2^{28}$, represents 1.00000000}
#define fraction_four 010000000000 // {$2^{30}$, represents 4.00000000}
/* sec 0105 */
#define nx_plus_y(a, b, c)  mult_and_add(a, b, c, 07777777777)
#define mult_integers(a, b) mult_and_add(a, b, 0, 017777777777)
/* sec 0108 */
#define inf_bad 10000
/* sec 0109 */
#define set_glue_ratio_zero(a) (a) = 0.0
#define set_glue_ratio_one(a)  (a) = 1.0
#define tex_float(a)           (a)
#define unfloat(a)             (a)
#define float_constant(a)      (real) (a)
/* sec 0115 */
#define null min_halfword  // {the null pointer}
/* sec 0118 */
#define link(p) mem[p].hh.rh  // {the |link| field of a memory word}
#define info(p) mem[p].hh.lh  // {the |info| field of a memory word}
/* sec 0122 */
#ifdef STAT
#define fast_get_avail(a) \
  do                      \
    {                     \
      a = avail;          \
                          \
      if (a == null)      \
        a = get_avail();  \
      else                \
      {                   \
        avail = link(a);  \
        link(a) = null;   \
        incr(dyn_used);   \
      }                   \
    }                     \
  while (0)
#else
#define fast_get_avail(a) \
  do                      \
    {                     \
      a = avail;          \
                          \
      if (a == null)      \
        a = get_avail();  \
      else                \
      {                   \
        avail = link(a);  \
        link(a) = null;   \
      }                   \
    }                     \
  while (0)
#endif
/* sec 0124 */
#define empty_flag  max_halfword            // {the |link| of an empty variable-size node}
#define is_empty(a) (link(a) == empty_flag) // {tests for empty node}
#define node_size   info                    // {the size field in empty variable-size nodes}
#define llink(a)    info(a + 1)             // {left link in doubly-linked list of empty nodes}
#define rlink(a)    link(a + 1)             // {right link in doubly-linked list of empty nodes}
/* sec 0133 */
#define type(a)    mem[a].hh.b0 // {identifies what kind of node this is}
#define subtype(a) mem[a].hh.b1 // {secondary identification in some cases}
/* sec 0134 */
#define is_char_node(a) (a >= hi_mem_min) // {does the argument point to a |char_node|?}
#define font            type              // {the font code in a |char_node|}
#define character       subtype           // {the character code in a |char_node|}
/* for SyncTeX */
#define synctex_field_size 2                                    // {Declare the {\sl Sync\TeX} field size to store the {\sl Sync\TeX} information: 2 integers for file tag and line}
#define sync_tag(a)        mem[a - synctex_field_size].cint     // {The tag subfield}
#define sync_line(a)       mem[a - synctex_field_size + 1].cint // {The line subfield}
/* sec 0135 */
#define hlist_node        0                         // {|type| of hlist nodes}
#define box_node_size     (8 + synctex_field_size)  // {number of words to allocate for a box node}
//#
#define dir_max           5
#define box_dir(a)        (subtype(a) % 16 - dir_max)          // {direction of a box}
#define set_box_dir(a,b)  subtype(a) = box_lr(a) * 16 + b + dir_max
//#
#define dir_default       0 // {direction of the box, default Left to Right}
#define dir_dtou          1 // {direction of the box, Bottom to Top}
#define dir_tate          3 // {direction of the box, Top to Bottom}
#define dir_yoko          4 // {direction of the box, equal default}
#define any_dir           dir_yoko: case dir_tate: case dir_dtou
//#
#define width_offset      1 // {position of |width| field in a box node}
#define depth_offset      2 // {position of |depth| field in a box node}
#define height_offset     3 // {position of |height| field in a box node}
#define width(a)          mem[a + width_offset].sc  // {width of the box, in sp}
#define depth(a)          mem[a + depth_offset].sc  // {depth of the box, in sp}
#define height(a)         mem[a + height_offset].sc // {height of the box, in sp}
#define shift_amount(a)   mem[a + 4].sc             // {repositioning distance, in sp}
#define list_offset       5                         // {position of |list_ptr| field in a box node}
#define list_ptr(a)       link(a + list_offset)     // {beginning of the list inside the box}
#define glue_order(a)     subtype(a + list_offset)  // {applicable order of infinity}
#define glue_sign(a)      type(a + list_offset)     // {stretching or shrinking}
#define normal            0                         // {the most common case when several cases are named}
#define stretching        1                         // {glue setting applies to the stretch components}
#define shrinking         2                         // {glue setting applies to the shrink components}
#define glue_offset       6                         // {position of |glue_set| in a box node}
#define glue_set(a)       mem[a + glue_offset].gr   // {a word of type |glue_ratio| for glue setting}
#define space_offset      7                         // {position of |glue_set| in a box node}
#define space_ptr(a)      link(a + space_offset)
#define xspace_ptr(a)     info(a + space_offset)
/* sec 0137 */
#define vlist_node 1  // {|type| of vlist nodes}
#define dir_node   2  // {|type| of dir nodes}
/* sec 0138 */
#define rule_node      3                        // {|type| of rule nodes}
#define rule_node_size (4 + synctex_field_size) // {number of words to allocate for a rule node}
#define null_flag      -010000000000            // {$-2^{30}$, signifies a missing item}
#define is_running(a)  (a == null_flag)         // {tests for a running dimension}
/* sec 0140 */
#define ins_node         4                      // {|type| of insertion nodes}
#define ins_node_size    6                      // {number of words to allocate for an insertion}
#define float_cost(a)    mem[a + 1].cint        // {the |floating_penalty| to be used}
#define ins_ptr(a)       info(a + 4)            // {the vertical list to be inserted}
#define split_top_ptr(a) link(a + 4)            // {the |split_top_skip| to be used}
#define ins_dir(a)       (subtype(a + 5) - dir_max)         // {direction of |ins_node|}
#define set_ins_dir(a, b)   subtype(a + 5) = b + dir_max
//#
#define disp_node        5                      // {|type| of a displace node}
#define disp_dimen(a)    mem[a + 1].cint
/* sec 0141 */
#define mark_node         6                     // {|type| of a mark node}
#define small_node_size   2                     // {number of words to allocate for most node types}
#define medium_node_size  (small_node_size + synctex_field_size)
// {number of words to allocate for synchronized node types like math, kern, glue and penalty nodes}
#define mark_ptr(a)       link(a + 1)           // {head of the token list for a mark}
#define mark_class(a)     info(a + 1)           // {the mark class}
/* sec 0142 */
#define adjust_node     7                       // {|type| of an adjust node}
#define adjust_ptr(a)   mem[a + 1].cint         // {vertical list to be moved out of horizontal list}
/* sec 0143 */
#define ligature_node   8                       // {|type| of a ligature node}
#define lig_char(a)     (a + 1)                 // {the word where the ligature is to be found}
#define lig_ptr(a)      link(lig_char(a))       // {the list of characters}
/* sec 0145 */
#define disc_node     9                         // {|type| of a discretionary node}
#define replace_count subtype                   // {how many subsequent nodes to replace}
#define pre_break     llink                     // {text that precedes a discretionary break}
#define post_break    rlink                     // {text that follows a discretionary break}
/* sec 0146 */
#define whatsit_node 10                         // {|type| of special extension nodes}
/* sec 0147 */
#define math_node 11                            // {|type| of a math node}
#define before    0                             // {|subtype| for math node that introduces a formula}
#define after     1                             // {|subtype| for math node that winds up a formula}

#define M_code    2
#define begin_M_code  (M_code + before)         // 2 {|subtype| for \.{\\beginM} node}
#define end_M_code    (M_code + after)          // 3 {|subtype| for \.{\\endM} node}
#define L_code    4
#define begin_L_code  (L_code + begin_M_code)   // 6 {|subtype| for \.{\\beginL} node}
#define end_L_code    (L_code + end_M_code)     // 7 {|subtype| for \.{\\endL} node}
#define R_code    8
#define begin_R_code  (R_code + begin_M_code)   // 10 {|subtype| for \.{\\beginR} node}
#define end_R_code    (R_code + end_M_code)     // 11 {|subtype| for \.{\\endR} node}

#define end_LR(a)         odd(subtype(a))
#define end_LR_type(a)    (L_code * (subtype(a) / L_code) + end_M_code)
#define begin_LR_type(a)  (a - after + before)
//
#define precedes_break(a)  (type(a) < math_node)
#define non_discardable(a) (type(a) < math_node)
//
#define glue_node      12                       // {|type| of node that points to a glue specification}
#define cond_math_glue 98                       // {special |subtype| to suppress glue in the next node}
#define mu_glue        99                       // {|subtype| for math glue}
#define a_leaders      100                      // {|subtype| for aligned leaders}
#define c_leaders      101                      // {|subtype| for centered leaders}
#define x_leaders      102                      // {|subtype| for expanded leaders}
#define glue_ptr       llink                    // {pointer to a glue specification}
#define leader_ptr     rlink                    // {pointer to box or rule node for leaders}
/* sec 0150 */
#define glue_spec_size    4                     // {number of words to allocate for a glue specification}
#define glue_ref_count(a) link(a)               // {reference count of a glue specification}
#define stretch(a)        mem[a + 2].sc         // {the stretchability of this glob of glue}
#define shrink(a)         mem[a + 3].sc         // {the shrinkability of this glob of glue}
#define stretch_order     type                  // {order of infinity for stretching}
#define shrink_order      subtype               // {order of infinity for shrinking}
#define fil               1                     // {first-order infinity}
#define fill              2                     // {second-order infinity}
#define filll             3                     // {third-order infinity}
/* sec 0155 */
#define kern_node 13                            // {|type| of a kern node}
#define explicit  1                             // {|subtype| of kern nodes from \.{\\kern}}
#define acc_kern  2                             // {|subtype| of kern nodes from accents}
#define ita_kern  3                             // {|subtype| of kern nodes from \.{\\/}}
/* sec 0157 */
#define penalty_node  14                        // {|type| of a penalty node}
#define widow_pena    1                         // {|subtype| of penalty nodes from \.{\\jcharwidowpenalty}}
#define kinsoku_pena  2                         // {|subtype| of penalty nodes from kinsoku}
#define inf_penalty   inf_bad                   // {``infinite'' penalty value}
#define eject_penalty -inf_penalty              // {``negatively infinite'' penalty value}
#define penalty(a)    mem[a + 1].cint           // {the added cost of breaking a list here}
/* sec 0159 */
#define unset_node      15                      // {|type| for an unset node}
#define glue_stretch(a) mem[a + glue_offset].sc // {total stretch in an unset node}
#define glue_shrink     shift_amount            // {total shrink in an unset node}
#define span_count      subtype                 // {indicates the number of spanned columns}
/* sec 0162 */
#define zero_glue         mem_bot                             // {specification for \.{0pt plus 0pt minus 0pt}}
#define fil_glue          (zero_glue + glue_spec_size)        // {\.{0pt plus 1fil minus 0pt}}
#define fill_glue         (fil_glue + glue_spec_size)         // {\.{0pt plus 1fill minus 0pt}}
#define ss_glue           (fill_glue + glue_spec_size)        // {\.{0pt plus 1fil minus 1fil}}
#define fil_neg_glue      (ss_glue + glue_spec_size)          // {\.{0pt plus -1fil minus 0pt}}
#define lo_mem_stat_max   (fil_neg_glue + glue_spec_size - 1) // {largest statically allocated word in the variable-size |mem|}
// #
#define page_ins_head     mem_top                             // {list of insertion data for current page}
#define contrib_head      (mem_top - 1)                       // {vlist of items not yet on current page}
#define page_head         (mem_top - 2)                       // {vlist for current page}
#define temp_head         (mem_top - 3)                       // {head of a temporary list of some kind}
#define hold_head         (mem_top - 4)                       // {head of a temporary list of another kind}
#define adjust_head       (mem_top - 5)                       // {head of adjustment list returned by |hpack|}
#define active            (mem_top - 7)                       // {head of active list in |line_break|, needs two words}
#define align_head        (mem_top - 8)                       // {head of preamble list for alignments}
#define end_span          (mem_top - 9)                       // {tail of spanned-width lists}
#define omit_template     (mem_top - 10)                      // {a constant token list}
#define null_list         (mem_top - 11)                      // {permanently empty list}
#define lig_trick         (mem_top - 12)                      // {a ligature masquerading as a |char_node|}
#define garbage           (mem_top - 12)                      // {used for scrap information}
#define backup_head       (mem_top - 13)                      // {head of token list built by |scan_keyword|}
#define hi_mem_stat_min   (mem_top - 13)                      // {smallest statically allocated word in the one-word |mem|}
#define hi_mem_stat_usage 14                                  // {the number of one-word nodes always present}
/* sec 0200 */
#define token_ref_count(a) info(a)                            // {reference count preceding a token list}
/* sec 0203 */
#define add_token_ref(a) incr(token_ref_count(a))             // {new reference to a token list}
#define add_glue_ref(a)  incr(glue_ref_count(a))              // {new reference to a glue spec}
/* sec 0207 */
#define escape        0   // {escape delimiter (called \.\\ in {\sl The \TeX book\/})}
#define relax         0   // {do nothing ( \.{\\relax} )}
#define left_brace    1   // {beginning of a group ( \.\{ )}
#define right_brace   2   // {ending of a group ( \.\} )}
#define math_shift    3   // {mathematics shift character ( \.\$ )}
#define tab_mark      4   // {alignment delimiter ( \.\&, \.{\\span} )}
#define car_ret       5   // {end of line ( |carriage_return|, \.{\\cr}, \.{\\crcr} )}
#define out_param     5   // {output a macro parameter}
#define mac_param     6   // {macro parameter symbol ( \.\# )}
#define sup_mark      7   // {superscript ( \.{\char'136} )}
#define sub_mark      8   // {subscript ( \.{\char'137} )}
#define ignore        9   // {characters to ignore ( \.{\^\^@@} )}
#define endv          9   // {end of \<v_j> list in alignment template}
#define spacer        10  // {characters equivalent to blank space ( \.{\ } )}
#define letter        11  // {characters regarded as letters ( \.{A..Z}, \.{a..z} )}
#define other_char    12  // {none of the special character types}
#define active_char   13  // {characters that invoke macros ( \.{\char`\~} )}
#define par_end       13  // {end of paragraph ( \.{\\par} )}
#define match         13  // {match a macro parameter}
#define comment       14  // {characters that introduce comments ( \.\% )}
#define end_match     14  // {end of parameters to macro}
#define stop          14  // {end of job ( \.{\\end}, \.{\\dump} )}
#define invalid_char  15  // {characters that shouldn't appear ( \.{\^\^?} )}
#define delim_num     15  // {specify delimiter numerically ( \.{\\delimiter} )}
#define not_cjk       15  // {is not cjk characters}
#define kanji         16  // {kanji}
#define kana          17  // {hiragana, katakana, alphabet}
#define other_kchar   18  // {cjk symbol codes}
#define hangul        19  // {hangul codes}
#define max_char_code 19  // {largest catcode for individual characters}
/* sec 0208 */
#define char_num          (max_char_code + 1) // {character specified numerically ( \.{\\char} )}
#define kchar_num         (char_num + 1)      // {cjk character specified numerically ( \.{\\kchar} )}
#define math_char_num     (kchar_num + 1)      // {explicit math code ( \.{\\mathchar} )}
#define mark              (math_char_num + 1) // {mark definition ( \.{\\mark} )}
#define xray              (mark + 1)          // {peek inside of \TeX\ ( \.{\\show}, \.{\\showbox}, etc.~)}
#define make_box          (xray + 1)          // {make a box ( \.{\\box}, \.{\\copy}, \.{\\hbox}, etc.~)}
#define hmove             (make_box + 1)      // {horizontal motion ( \.{\\moveleft}, \.{\\moveright} )}
#define vmove             (hmove + 1)         // {vertical motion ( \.{\\raise}, \.{\\lower} )}
#define un_hbox           (vmove + 1)         // {unglue a box ( \.{\\unhbox}, \.{\\unhcopy} )}
#define un_vbox           (un_hbox + 1)       // {unglue a box ( \.{\\unvbox}, \.{\\unvcopy} )}
                                              // {( or \.{\\pagediscards}, \.{\\splitdiscards})}
#define remove_item       (un_vbox + 1)       // {nullify last item ( \.{\\unpenalty}, \.{\\unkern}, \.{\\unskip} )}
#define hskip             (remove_item + 1)   // {horizontal glue ( \.{\\hskip}, \.{\\hfil}, etc.~)}
#define vskip             (hskip + 1)         // {vertical glue ( \.{\\vskip}, \.{\\vfil}, etc.~)}
#define mskip             (vskip + 1)         // {math glue ( \.{\\mskip} )}
#define kern              (mskip + 1)         // {fixed space ( \.{\\kern})}
#define mkern             (kern + 1)          // {math kern ( \.{\\mkern} )}
#define leader_ship       (mkern + 1)         // {use a box ( \.{\\shipout}, \.{\\leaders}, etc.~)}
#define halign            (leader_ship + 1)   // {horizontal table alignment ( \.{\\halign} )}
#define valign            (halign + 1)        // {vertical table alignment ( \.{\\valign} )}
                                              // {or text direction directives ( \.{\\beginL}, etc.~)}
#define no_align          (valign + 1)        // {temporary escape from alignment ( \.{\\noalign} )}
#define vrule             (no_align + 1)      // {vertical rule ( \.{\\vrule} )}
#define hrule             (vrule + 1)         // {horizontal rule ( \.{\\hrule} )}
#define insert            (hrule + 1)         // {vlist inserted in box ( \.{\\insert} )}
#define vadjust           (insert + 1)        // {vlist inserted in enclosing paragraph ( \.{\\vadjust} )}
#define ignore_spaces     (vadjust + 1)       // {gobble |spacer| tokens ( \.{\\ignorespaces} )}
#define after_assignment  (ignore_spaces + 1) // {save till assignment is done ( \.{\\afterassignment} )}
#define after_group       (after_assignment + 1)  // {save till group is done ( \.{\\aftergroup} )}
#define break_penalty     (after_group + 1)   // {additional badness ( \.{\\penalty} )}
#define start_par         (break_penalty + 1) // {begin paragraph ( \.{\\indent}, \.{\\noindent} )}
#define ital_corr         (start_par + 1)     // {italic correction ( \.{\\/} )}
#define accent            (ital_corr + 1)     // {attach accent in text ( \.{\\accent} )}
#define math_accent       (accent + 1)        // {attach accent in math ( \.{\\mathaccent} )}
#define discretionary     (math_accent + 1)   // {discretionary texts ( \.{\\-}, \.{\\discretionary} )}
#define eq_no             (discretionary + 1) // {equation number ( \.{\\eqno}, \.{\\leqno} )}
#define left_right        (eq_no + 1)         // {variable delimiter ( \.{\\left}, \.{\\right} )}
                                              // {( or \.{\\middle})}
#define math_comp         (left_right + 1)    // {component of formula ( \.{\\mathbin}, etc.~)}
#define limit_switch      (math_comp + 1)     // {diddle limit conventions ( \.{\\displaylimits}, etc.~)}
#define above             (limit_switch + 1)  // {generalized fraction ( \.{\\above}, \.{\\atop}, etc.~)}
#define math_style        (above + 1)         // {style specification ( \.{\\displaystyle}, etc.~)}
#define math_choice       (math_style + 1)    // {choice specification ( \.{\\mathchoice} )}
#define non_script        (math_choice + 1)   // {conditional math glue ( \.{\\nonscript} )}
#define vcenter           (non_script + 1)    // {vertically center a vbox ( \.{\\vcenter} )}
#define case_shift        (vcenter + 1)       // {force specific case ( \.{\\lowercase}, \.{\\uppercase}~)}
#define message           (case_shift + 1)    // {send to user ( \.{\\message}, \.{\\errmessage} )}
#define extension         (message + 1)       // {extensions to \TeX\ ( \.{\\write}, \.{\\special}, etc.~)}
#define in_stream         (extension + 1)     // {files for reading ( \.{\\openin}, \.{\\closein} )}
#define begin_group       (in_stream + 1)     // {begin local grouping ( \.{\\begingroup} )}
#define end_group         (begin_group + 1)   // {end local grouping ( \.{\\endgroup} )}
#define omit              (end_group + 1)     // {omit alignment template ( \.{\\omit} )}
#define ex_space          (omit + 1)          // {explicit space ( \.{\\\ } )}
#define no_boundary       (ex_space + 1)      // {suppress boundary ligatures ( \.{\\noboundary} )}
#define radical           (no_boundary + 1)   // {square root and similar signs ( \.{\\radical} )}
#define end_cs_name       (radical + 1)       // {end control sequence ( \.{\\endcsname} )}
#define min_internal      (end_cs_name + 1)   // {the smallest code that can follow \.{\\the}}
#define char_given        (min_internal + 1)  // {character code defined by \.{\\chardef}}
#define kchar_given       (char_given + 1)    // {cjk character code defined by \.{\\kchardef}}
#define math_given        (kchar_given + 1)   // {math code defined by \.{\\mathchardef}}
#define last_item         (math_given + 1)    // {most recent item ( \.{\\lastpenalty}, \.{\\lastkern}, \.{\\lastskip} )}
#define inhibit_glue      (last_item + 1)     // {inhibit adjust glue ( \.{\\inhibitglue} )}
#define chg_dir           (inhibit_glue + 1)  // {change dir mode by \.{\\tate}, \.{\\yoko}}
#define max_non_prefixed_command (chg_dir)    // {largest command code that can't be \.{\\global}}
/* sec 0209 */
#define toks_register           (max_non_prefixed_command + 1)  // {token list register ( \.{\\toks} )}
#define assign_toks             (toks_register + 1)             // {special token list ( \.{\\output}, \.{\\everypar}, etc.~)}
#define assign_int              (assign_toks + 1)               // {user-defined integer ( \.{\\tolerance}, \.{\\day}, etc.~)}
#define assign_dimen            (assign_int + 1)                // {user-defined length ( \.{\\hsize}, etc.~)}
#define assign_glue             (assign_dimen + 1)              // {user-defined glue ( \.{\\baselineskip}, etc.~)}
#define assign_mu_glue          (assign_glue + 1)               // {user-defined muglue ( \.{\\thinmuskip}, etc.~)}
#define assign_font_dimen       (assign_mu_glue + 1)            // {user-defined font dimension ( \.{\\fontdimen} )}
#define assign_font_int         (assign_font_dimen + 1)         // {user-defined font integer ( \.{\\hyphenchar}, \.{\\skewchar} )}
#define assign_kinsoku          (assign_font_int + 1)           // {user-defined kinsoku character ( \.{\\prebreakpenalty}, \.{\\postbreakpenalty} )}
#define assign_inhibit_xsp_code (assign_kinsoku + 1)            // {user-defined inhibit xsp character ( \.{\\inhibitxspcode} )}
#define set_kansuji_char        (assign_inhibit_xsp_code + 1)   // {user-defined kansuji character ( \.{\\kansujichar} )}
#define set_aux                 (set_kansuji_char + 1)          // {specify state info ( \.{\\spacefactor}, \.{\\prevdepth} )}
#define set_prev_graf           (set_aux + 1)                   // {specify state info ( \.{\\prevgraf} )}
#define set_page_dimen          (set_prev_graf + 1)             // {specify state info ( \.{\\pagegoal}, etc.~)}
#define set_page_int            (set_page_dimen + 1)            // {specify state info ( \.{\\deadcycles}, \.{\\insertpenalties} )}
                                                                // {(or \.{\\interactionmode})}
#define set_box_dimen           (set_page_int + 1)              // {change dimension of box ( \.{\\wd}, \.{\\ht}, \.{\\dp} )}
#define set_shape               (set_box_dimen + 1)             // {specify fancy paragraph shape ( \.{\\parshape} )}
                                                                // {(or \.{\\interlinepenalties}, etc.~)}
#define def_code                (set_shape + 1)                 // {define a character code ( \.{\\catcode}, etc.~)}
#define def_family              (def_code + 1)                  // {declare math fonts ( \.{\\textfont}, etc.~)}
#define set_font                (def_family + 1)                // {set current font ( font identifiers )}
#define def_font                (set_font + 1)                  // {define a font file ( \.{\\font} )}
#define def_jfont               (def_font + 1)                  // {define a font file ( \.{\\jfont} )}
#define def_tfont               (def_jfont + 1)                 // {define a font file ( \.{\\tfont} )}
#define tex_register            (def_tfont + 1)                 // {internal register ( \.{\\count}, \.{\\dimen}, etc.~)}
#define max_internal            tex_register                    // {the largest code that can follow \.{\\the}}
#define advance                 (max_internal + 1)              // {advance a register or parameter ( \.{\\advance} )}
#define multiply                (advance + 1)                   // {multiply a register or parameter ( \.{\\multiply} )}
#define divide                  (multiply + 1)                  // {divide a register or parameter ( \.{\\divide} )}
#define prefix                  (divide + 1)                    // {qualify a definition ( \.{\\global}, \.{\\long}, \.{\\outer} )}
                                                                // {( or \.{\\protected})}
#define let                     (prefix + 1)                    // {assign a command code ( \.{\\let}, \.{\\futurelet} )}
#define shorthand_def           (let + 1)                       // {code definition ( \.{\\chardef}, \.{\\countdef}, etc.~)}
#define read_to_cs              (shorthand_def + 1)             // {read into a control sequence ( \.{\\read} )}
                                                                // {( or \.{\\readline})}
#define def                     (read_to_cs + 1)                // {macro definition ( \.{\\def}, \.{\\gdef}, \.{\\xdef}, \.{\\edef} )}
#define set_box                 (def + 1)                       // {set a box ( \.{\\setbox} )}
#define hyph_data               (set_box + 1)                   // {hyphenation data ( \.{\\hyphenation}, \.{\\patterns} )}
#define set_interaction         (hyph_data + 1)                 // {define level of interaction ( \.{\\batchmode}, etc.~)}
#define set_auto_spacing        (set_interaction + 1)           // {set auto spaceing mode ( \.{\\autospacing}, \.{\\noautospacing}, (\.{\\autoxspacing}, \.{\\noautoxspacing})}
#define set_enable_cjk_token    (set_auto_spacing + 1)          // {set cjk mode ( \.{\\enablecjktoken}, \.{\\disablecjktoken}, \.{\\forcecjktoken} )}
#define partoken_name           (set_enable_cjk_token + 1)      // {set |par_token| name}
#define max_command             partoken_name                   // {the largest command code seen at |big_switch|}
/* sec 0210 */
#define undefined_cs    (max_command + 1 )  // {initial state of most |eq_type| fields}
#define expand_after    (max_command + 2 )  // {special expansion ( \.{\\expandafter} )}
#define no_expand       (max_command + 3 )  // {special nonexpansion ( \.{\\noexpand} )}
#define input           (max_command + 4 )  // {input a source file ( \.{\\input}, \.{\\endinput} )}
                                            // {( or \.{\\scantokens})}
#define if_test         (max_command + 5 )  // {conditional text ( \.{\\if}, \.{\\ifcase}, etc.~)}
#define fi_or_else      (max_command + 6 )  // {delimiters for conditionals ( \.{\\else}, etc.~)}
#define cs_name         (max_command + 7 )  // {make a control sequence from tokens ( \.{\\csname} )}
#define convert         (max_command + 8 )  // {convert to text ( \.{\\number}, \.{\\string}, etc.~)}
#define the             (max_command + 9 )  // {expand an internal quantity ( \.{\\the} )}
                                            // {( or \.{\\unexpanded}, \.{\\detokenize})}
#define top_bot_mark    (max_command + 10)  // {inserted mark ( \.{\\topmark}, etc.~)}
#define call            (max_command + 11)  // {non-long, non-outer control sequence}
#define long_call       (max_command + 12)  // {long, non-outer control sequence}
#define outer_call      (max_command + 13)  // {non-long, outer control sequence}
#define long_outer_call (max_command + 14)  // {long, outer control sequence}
#define end_template    (max_command + 15)  // {end of an alignment template}
#define dont_expand     (max_command + 16)  // {the following token was marked by \.{\\noexpand}}
#define glue_ref        (max_command + 17)  // {the equivalent points to a glue specification}
#define shape_ref       (max_command + 18)  // {the equivalent points to a parshape specification}
#define box_ref         (max_command + 19)  // {the equivalent points to a box node, or is |null|}
#define data            (max_command + 20)  // {the equivalent is simply a halfword number}
/* sec 0211 */
#define vmode 1                         // {vertical mode}
#define hmode (vmode + max_command + 1) // {horizontal mode}
#define mmode (hmode + max_command + 1) // {math mode}
/* sec 0212 */
#define ignore_depth -65536000          // {|prev_depth| value that is ignored}
/* sec 0213 */
#define mode            cur_list.mode_field         // {current mode}
#define direction       cur_list.dir_field          // {current direction}
#define adjust_dir      cur_list.adj_dir_field      // {current adjust direction}
#define head            cur_list.head_field         // {header node of current list}
#define tail            cur_list.tail_field         // {final node on current list}
#define prev_node       cur_list.pnode_field        // {previous to last |disp_node|}
#define prev_disp       cur_list.pdisp_field        // {displacemant at |prev_node|}
#define last_jchr       cur_list.last_jchr_field    // {final jchar node on current list}
#define disp_called     cur_list.disp_called_field  // {is a |disp_node| present in the current list?}
#define inhibit_glue_flag cur_list.inhibit_glue_flag_field // {is \.{inhibitglue} is specified at the current list?}
#define eTeX_aux        cur_list.eTeX_aux_field     // {auxiliary data for \eTeX}
#define LR_save         eTeX_aux                    // {LR stack when a paragraph is interrupted}
#define LR_box          eTeX_aux                    // {prototype box for display}
#define delim_ptr       eTeX_aux                    // {most recent left or right noad of a math left group}
#define prev_graf       cur_list.pg_field           // {number of paragraph lines accumulated}
#define aux             cur_list.aux_field          // {auxiliary data about the current list}
#define prev_depth      aux.sc                      // {the name of |aux| in vertical mode}
#define space_factor    aux.hh.lh                   // {part of |aux| in horizontal mode}
#define clang           aux.hh.rh                   // {the other part of |aux| in horizontal mode}
#define incompleat_noad aux.cint                    // {the name of |aux| in math mode}
#define mode_line       cur_list.ml_field           // {source file line number at beginning of list}
/* sec 0221 */
#define eq_level_field(a) a.hh.b1
#define eq_type_field(a)  a.hh.b0
#define equiv_field(a)    a.hh.rh
#define eq_level(a)       eq_level_field(eqtb[a]) // {level of definition}
#define eq_type(a)        eq_type_field(eqtb[a])  // {command code for equivalent}
#define equiv(a)          equiv_field(eqtb[a])    // {equivalent value}
#define level_zero        min_quarterword         // {level for undefined quantities}
#define level_one         (level_zero + 1)        // {outermost level for defined quantities}
/* sec 0222 */
#define active_base                   1                                 // {beginning of region 1, for active character equivalents}
#define single_base                   (active_base + 256)               // {equivalents of one-character control sequences}
#define null_cs                       (single_base + 256)               // {equivalent of \.{\\csname\\endcsname}}
#define hash_base                     (null_cs + 1)                     // {beginning of region 2, for the hash table}
#define frozen_control_sequence       (hash_base + hash_size)           // {for error recovery}
#define frozen_protection             frozen_control_sequence           // {inaccessible but definable}
#define frozen_cr                     (frozen_control_sequence + 1)     // {permanent `\.{\\cr}'}
#define frozen_end_group              (frozen_control_sequence + 2)     // {permanent `\.{\\endgroup}'}
#define frozen_right                  (frozen_control_sequence + 3)     // {permanent `\.{\\right}'}
#define frozen_fi                     (frozen_control_sequence + 4)     // {permanent `\.{\\fi}'}
#define frozen_end_template           (frozen_control_sequence + 5)     // {permanent `\.{\\endtemplate}'}
#define frozen_endv                   (frozen_control_sequence + 6)     // {second permanent `\.{\\endtemplate}'}
#define frozen_relax                  (frozen_control_sequence + 7)     // {permanent `\.{\\relax}'}
#define end_write                     (frozen_control_sequence + 8)     // {permanent `\.{\\endwrite}'}
#define frozen_dont_expand            (frozen_control_sequence + 9)     // {permanent `\.{\\notexpanded:}'}
#define frozen_primitive              (frozen_control_sequence + 10)    // {permanent `\.{\\pdfprimitive}'}
#define prim_eqtb_base                (frozen_primitive + 1)
#define prim_size                     2100                              // {maximum number of primitives }
#define frozen_null_font              (prim_eqtb_base + prim_size + 1)  // {permanent `\.{\\nullfont}'}
#define font_id_base                  (frozen_null_font - font_base)    // {begins table of 257 permanent font identifiers}
#define undefined_control_sequence    (frozen_null_font + font_max + 2) // {dummy location}
#define glue_base                     (undefined_control_sequence + 1)  // {beginning of region 3}
/* sec 0224 */
#define line_skip_code                0   // {interline glue if |baseline_skip| is infeasible}
#define baseline_skip_code            1   // {desired glue between baselines}
#define par_skip_code                 2   // {extra glue just above a paragraph}
#define above_display_skip_code       3   // {extra glue just above displayed math}
#define below_display_skip_code       4   // {extra glue just below displayed math}
#define above_display_short_skip_code 5   // {glue above displayed math following short lines}
#define below_display_short_skip_code 6   // {glue below displayed math following short lines}
#define left_skip_code                7   // {glue at left of justified lines}
#define right_skip_code               8   // {glue at right of justified lines}
#define top_skip_code                 9   // {glue at top of main pages}
#define split_top_skip_code           10  // {glue at top of split pages}
#define tab_skip_code                 11  // {glue between aligned entries}
#define space_skip_code               12  // {glue between words (if not |zero_glue|)}
#define xspace_skip_code              13  // {glue after sentences (if not |zero_glue|)}
#define par_fill_skip_code            14  // {glue on last line of paragraph}
#define kanji_skip_code               15  // {between kanji-kanji space}
#define xkanji_skip_code              16  // {between latin-kanji or kanji-latin space}
#define thin_mu_skip_code             17  // {thin space in math formula}
#define med_mu_skip_code              18  // {medium space in math formula}
#define thick_mu_skip_code            19  // {thick space in math formula}
#define jfm_skip                      20  // {space refer from JFM}
#define glue_pars                     21  // {total number of glue parameters}
#define skip_base                     (glue_base + glue_pars) // {table of 256 ``skip'' registers}
#define mu_skip_base                  (skip_base + 256)       // {table of 256 ``muskip'' registers}
#define local_base                    (mu_skip_base + 256)    // {beginning of region 4}
// #
#define skip(a)                       equiv(skip_base + a)    // {|mem| location of glue specification}
#define mu_skip(a)                    equiv(mu_skip_base + a) // {|mem| location of math glue spec}
#define glue_par(a)                   equiv(glue_base + a)    // {|mem| location of glue specification}
#define line_skip                     glue_par(line_skip_code)
#define baseline_skip                 glue_par(baseline_skip_code)
#define par_skip                      glue_par(par_skip_code)
#define above_display_skip            glue_par(above_display_skip_code)
#define below_display_skip            glue_par(below_display_skip_code)
#define above_display_short_skip      glue_par(above_display_short_skip_code)
#define below_display_short_skip      glue_par(below_display_short_skip_code)
#define left_skip                     glue_par(left_skip_code)
#define right_skip                    glue_par(right_skip_code)
#define top_skip                      glue_par(top_skip_code)
#define split_top_skip                glue_par(split_top_skip_code)
#define tab_skip                      glue_par(tab_skip_code)
#define space_skip                    glue_par(space_skip_code)
#define xspace_skip                   glue_par(xspace_skip_code)
#define par_fill_skip                 glue_par(par_fill_skip_code)
#define kanji_skip                    glue_par(kanji_skip_code)
#define xkanji_skip                   glue_par(xkanji_skip_code)
#define thin_mu_skip                  glue_par(thin_mu_skip_code)
#define med_mu_skip                   glue_par(med_mu_skip_code)
#define thick_mu_skip                 glue_par(thick_mu_skip_code)
/* sec 0230 */
#define par_shape_loc                 local_base          // {specifies paragraph shape}
#define output_routine_loc            (local_base + 1)    // {points to token list for \.{\\output}}
#define every_par_loc                 (local_base + 2)    // {points to token list for \.{\\everypar}}
#define every_math_loc                (local_base + 3)    // {points to token list for \.{\\everymath}}
#define every_display_loc             (local_base + 4)    // {points to token list for \.{\\everydisplay}}
#define every_hbox_loc                (local_base + 5)    // {points to token list for \.{\\everyhbox}}
#define every_vbox_loc                (local_base + 6)    // {points to token list for \.{\\everyvbox}}
#define every_job_loc                 (local_base + 7)    // {points to token list for \.{\\everyjob}}
#define every_cr_loc                  (local_base + 8)    // {points to token list for \.{\\everycr}}
#define err_help_loc                  (local_base + 9)    // {points to token list for \.{\\errhelp}}
#define tex_toks                      (local_base + 10)   // {end of \TeX's token list parameters}
//#
#define etex_toks_base                tex_toks              // {base for \eTeX's token list parameters}
#define every_eof_loc                 etex_toks_base        // {points to token list for \.{\\everyeof}}
#define etex_toks                     (etex_toks_base + 1)  // {end of \eTeX's token list parameters}
//#
#define toks_base                     etex_toks             // {table of 256 token list registers}
//#
#define etex_pen_base                 (toks_base + 256)     // {start of table of \eTeX's penalties}
#define inter_line_penalties_loc      etex_pen_base         // {additional penalties between lines}
#define club_penalties_loc            (etex_pen_base + 1)   // {penalties for creating club lines}
#define widow_penalties_loc           (etex_pen_base + 2)   // {penalties for creating widow lines}
#define display_widow_penalties_loc   (etex_pen_base + 3)   // {ditto, just before a display}
#define etex_pens                     (etex_pen_base + 4)   // {end of table of \eTeX's penalties}
//#
#define box_base                      etex_pens                     // {table of 256 box registers}
#define cur_font_loc                  (box_base + 256)              // {internal font number outside math mode}
#define math_font_base                (cur_font_loc + 1)            // {table of 48 math font numbers}
#define cur_jfont_loc                 (math_font_base + 48)
#define cur_tfont_loc                 (cur_jfont_loc + 1)
#define auto_spacing_code             (cur_tfont_loc + 1)
#define auto_xspacing_code            (auto_spacing_code + 1)
#define enable_cjk_token_code         (auto_xspacing_code + 1)
#define cat_code_base                 (enable_cjk_token_code + 1)   // {table of 256 command codes (the ``catcodes'')}
#define kcat_code_base                (cat_code_base + 256)         // {table of 512 command codes for the wchar's catcodes }
#define auto_xsp_code_base            (kcat_code_base + 512)        // {table of 256 auto spacer flag}
#define inhibit_xsp_code_base         (auto_xsp_code_base + 256)
#define kinsoku_base                  (inhibit_xsp_code_base + 1024) // {table of 1024 kinsoku mappings}
#define kansuji_base                  (kinsoku_base + 1024)          // {table of 10 kansuji mappings}
#define lc_code_base                  (kansuji_base + 10)            // {table of 256 lowercase mappings}
#define uc_code_base                  (lc_code_base + 256)           // {table of 256 uppercase mappings}
#define sf_code_base                  (uc_code_base + 256)           // {table of 256 spacefactor mappings}
#define math_code_base                (sf_code_base + 256)           // {table of 256 math mode mappings}
#define int_base                      (math_code_base + 256)         // {table of character substitutions}
// #
#define par_shape_ptr                 equiv(par_shape_loc)
#define output_routine                equiv(output_routine_loc)
#define every_par                     equiv(every_par_loc)
#define every_math                    equiv(every_math_loc)
#define every_display                 equiv(every_display_loc)
#define every_hbox                    equiv(every_hbox_loc)
#define every_vbox                    equiv(every_vbox_loc)
#define every_job                     equiv(every_job_loc)
#define every_cr                      equiv(every_cr_loc)
#define err_help                      equiv(err_help_loc)
#define every_eof                     equiv(every_eof_loc)
#define inter_line_penalties_ptr      equiv(inter_line_penalties_loc)
#define club_penalties_ptr            equiv(club_penalties_loc)
#define widow_penalties_ptr           equiv(widow_penalties_loc)
#define display_widow_penalties_ptr   equiv(display_widow_penalties_loc)
#define toks(a)                       equiv(toks_base + a)
#define box(a)                        equiv(box_base + a)
#define cur_font                      equiv(cur_font_loc)
#define fam_fnt(a)                    equiv(math_font_base + a)
#define cur_jfont                     equiv(cur_jfont_loc)
#define cur_tfont                     equiv(cur_tfont_loc)
#define auto_spacing                  equiv(auto_spacing_code)
#define auto_xspacing                 equiv(auto_xspacing_code)
#define enable_cjk_token              equiv(enable_cjk_token_code)
#define kcat_code(a)                  equiv(kcat_code_base + a)
#define auto_xsp_code(a)              equiv(auto_xsp_code_base + a)
#define inhibit_xsp_type(a)           eq_type(inhibit_xsp_code_base + a)
#define inhibit_xsp_code(a)           equiv(inhibit_xsp_code_base + a)
#define kinsoku_type(a)               eq_type(kinsoku_base + a)
#define kinsoku_code(a)               equiv(kinsoku_base + a)
#define kansuji_char(a)               equiv(kansuji_base + a)
#define cat_code(a)                   equiv(cat_code_base + a)
#define lc_code(a)                    equiv(lc_code_base + a)
#define uc_code(a)                    equiv(uc_code_base + a)
#define sf_code(a)                    equiv(sf_code_base + a)
#define math_code(a)                  equiv(math_code_base + a)
/* sec 0232 */
#define null_font font_base
#define var_code  070000  // {math code meaning ``use the current family''}
/* sec 0236 */
#define pretolerance_code             0   // {badness tolerance before hyphenation}
#define tolerance_code                1   // {badness tolerance after hyphenation}
#define line_penalty_code             2   // {added to the badness of every line}
#define hyphen_penalty_code           3   // {penalty for break after discretionary hyphen}
#define ex_hyphen_penalty_code        4   // {penalty for break after explicit hyphen}
#define club_penalty_code             5   // {penalty for creating a club line}
#define widow_penalty_code            6   // {penalty for creating a widow line}
#define display_widow_penalty_code    7   // {ditto, just before a display}
#define broken_penalty_code           8   // {penalty for breaking a page at a broken line}
#define bin_op_penalty_code           9   // {penalty for breaking after a binary operation}
#define rel_penalty_code              10  // {penalty for breaking after a relation}
#define pre_display_penalty_code      11  // {penalty for breaking just before a displayed formula}
#define post_display_penalty_code     12  // {penalty for breaking just after a displayed formula}
#define inter_line_penalty_code       13  // {additional penalty between lines}
#define double_hyphen_demerits_code   14  // {demerits for double hyphen break}
#define final_hyphen_demerits_code    15  // {demerits for final hyphen break}
#define adj_demerits_code             16  // {demerits for adjacent incompatible lines}
#define mag_code                      17  // {magnification ratio}
#define delimiter_factor_code         18  // {ratio for variable-size delimiters}
#define looseness_code                19  // {change in number of lines for a paragraph}
#define time_code                     20  // {current time of day}
#define day_code                      21  // {current day of the month}
#define month_code                    22  // {current month of the year}
#define year_code                     23  // {current year of our Lord}
#define show_box_breadth_code         24  // {nodes per level in |show_box|}
#define show_box_depth_code           25  // {maximum level in |show_box|}
#define hbadness_code                 26  // {hboxes exceeding this badness will be shown by |hpack|}
#define vbadness_code                 27  // {vboxes exceeding this badness will be shown by |vpack|}
#define pausing_code                  28  // {pause after each line is read from a file}
#define tracing_online_code           29  // {show diagnostic output on terminal}
#define tracing_macros_code           30  // {show macros as they are being expanded}
#define tracing_stats_code            31  // {show memory usage if \TeX\ knows it}
#define tracing_paragraphs_code       32  // {show line-break calculations}
#define tracing_pages_code            33  // {show page-break calculations}
#define tracing_output_code           34  // {show boxes when they are shipped out}
#define tracing_lost_chars_code       35  // {show characters that aren't in the font}
#define tracing_commands_code         36  // {show command codes at |big_switch|}
#define tracing_restores_code         37  // {show equivalents when they are restored}
#define uc_hyph_code                  38  // {hyphenate words beginning with a capital letter}
#define output_penalty_code           39  // {penalty found at current page break}
#define max_dead_cycles_code          40  // {bound on consecutive dead cycles of output}
#define hang_after_code               41  // {hanging indentation changes after this many lines}
#define floating_penalty_code         42  // {penalty for insertions heldover after a split}
#define global_defs_code              43  // {override \.{\\global} specifications}
#define cur_fam_code                  44  // {current family}
#define cur_jfam_code                 45  // {current kanji family}
#define escape_char_code              46  // {escape character for token output}
#define default_hyphen_char_code      47  // {value of \.{\\hyphenchar} when a font is loaded}
#define default_skew_char_code        48  // {value of \.{\\skewchar} when a font is loaded}
#define end_line_char_code            49  // {character placed at the right end of the buffer}
#define new_line_char_code            50  // {character that prints as |print_ln|}
#define language_code                 51  // {current hyphenation table}
#define left_hyphen_min_code          52  // {minimum left hyphenation fragment size}
#define right_hyphen_min_code         53  // {minimum right hyphenation fragment size}
#define holding_inserts_code          54  // {do not remove insertion nodes from \.{\\box255}}
#define error_context_lines_code      55  // {maximum intermediate line pairs shown}
#define jchr_widow_penalty_code       56  // {penalty for creating a widow KANJI character line}
#define text_baseline_shift_factor_code 57
#define script_baseline_shift_factor_code 58
#define scriptscript_baseline_shift_factor_code 59
#define ptex_lineend_code 60
#define ptex_tracing_fonts_code 61
#define tracing_assigns_code          62  // {show assignments}
#define tracing_groups_code           63  // {show save/restore groups}
#define tracing_ifs_code              64  // {show conditionals}
#define tracing_scan_tokens_code      65  // {show pseudo file open and close}
#define tracing_nesting_code          66  // {show incomplete groups and ifs within files}
#define pre_display_direction_code    67  // {text direction preceding a display}
#define last_line_fit_code            68  // {adjustment for last line of paragraph}
#define saving_vdiscards_code         69  // {save items discarded from vlists}
#define saving_hyph_codes_code        70  // {save hyphenation codes for languages}
#define eTeX_state_code               71  // {\eTeX\ state variables}
#define tracing_fontloaders_code      72
#define pdf_compress_level_code       73
#define pdf_major_version_code        74
#define pdf_minor_version_code        75
#define synctex_code                  76
#define tracing_stack_levels_code     77
#define partoken_context_code         78
#define show_stream_code              79
#define int_pars                      80
#define count_base                    (int_base + int_pars) // {256 user \.{\\count} registers}
#define del_code_base                 (count_base + 256)    // {256 delimiter code mappings}
#define dimen_base                    (del_code_base + 256) // {beginning of region 6}
// #
#define del_code(a)                   eqtb[del_code_base + a].cint
#define count(a)                      eqtb[count_base + a].cint
#define int_par(a)                    eqtb[int_base + a].cint
#define eTeX_state_base               (int_base + eTeX_state_code)
#define eTeX_state(a)                 eqtb[eTeX_state_base + a].cint
#define pretolerance                  int_par(pretolerance_code)
#define tolerance                     int_par(tolerance_code)
#define line_penalty                  int_par(line_penalty_code)
#define hyphen_penalty                int_par(hyphen_penalty_code)
#define ex_hyphen_penalty             int_par(ex_hyphen_penalty_code)
#define club_penalty                  int_par(club_penalty_code)
#define widow_penalty                 int_par(widow_penalty_code)
#define display_widow_penalty         int_par(display_widow_penalty_code)
#define broken_penalty                int_par(broken_penalty_code)
#define bin_op_penalty                int_par(bin_op_penalty_code)
#define rel_penalty                   int_par(rel_penalty_code)
#define pre_display_penalty           int_par(pre_display_penalty_code)
#define post_display_penalty          int_par(post_display_penalty_code)
#define inter_line_penalty            int_par(inter_line_penalty_code)
#define double_hyphen_demerits        int_par(double_hyphen_demerits_code)
#define final_hyphen_demerits         int_par(final_hyphen_demerits_code)
#define adj_demerits                  int_par(adj_demerits_code)
#define mag                           int_par(mag_code)
#define delimiter_factor              int_par(delimiter_factor_code)
#define looseness                     int_par(looseness_code)
#define tex_time                      int_par(time_code)
#define day                           int_par(day_code)
#define month                         int_par(month_code)
#define year                          int_par(year_code)
#define show_box_breadth              int_par(show_box_breadth_code)
#define show_box_depth                int_par(show_box_depth_code)
#define hbadness                      int_par(hbadness_code)
#define vbadness                      int_par(vbadness_code)
#define pausing                       int_par(pausing_code)
#define tracing_online                int_par(tracing_online_code)
#define tracing_macros                int_par(tracing_macros_code)
#define tracing_stats                 int_par(tracing_stats_code)
#define tracing_paragraphs            int_par(tracing_paragraphs_code)
#define tracing_pages                 int_par(tracing_pages_code)
#define tracing_output                int_par(tracing_output_code)
#define tracing_lost_chars            int_par(tracing_lost_chars_code)
#define tracing_commands              int_par(tracing_commands_code)
#define tracing_restores              int_par(tracing_restores_code)
#define tracing_fontloaders           int_par(tracing_fontloaders_code)
#define uc_hyph                       int_par(uc_hyph_code)
#define output_penalty                int_par(output_penalty_code)
#define max_dead_cycles               int_par(max_dead_cycles_code)
#define hang_after                    int_par(hang_after_code)
#define floating_penalty              int_par(floating_penalty_code)
#define global_defs                   int_par(global_defs_code)
#define cur_fam                       int_par(cur_fam_code)
#define cur_jfam                      int_par(cur_jfam_code)
#define escape_char                   int_par(escape_char_code)
#define default_hyphen_char           int_par(default_hyphen_char_code)
#define default_skew_char             int_par(default_skew_char_code)
#define end_line_char                 int_par(end_line_char_code)
#define new_line_char                 int_par(new_line_char_code)
#define language                      int_par(language_code)
#define left_hyphen_min               int_par(left_hyphen_min_code)
#define right_hyphen_min              int_par(right_hyphen_min_code)
#define holding_inserts               int_par(holding_inserts_code)
#define error_context_lines           int_par(error_context_lines_code)
#define jchr_widow_penalty            int_par(jchr_widow_penalty_code)
#define text_baseline_shift_factor    int_par(text_baseline_shift_factor_code)
#define script_baseline_shift_factor  int_par(script_baseline_shift_factor_code)
#define scriptscript_baseline_shift_factor  int_par(scriptscript_baseline_shift_factor_code)
#define ptex_lineend                  int_par(ptex_lineend_code)
#define ptex_tracing_fonts            int_par(ptex_tracing_fonts_code)
#define tracing_assigns               int_par(tracing_assigns_code)
#define tracing_groups                int_par(tracing_groups_code)
#define tracing_ifs                   int_par(tracing_ifs_code)
#define tracing_scan_tokens           int_par(tracing_scan_tokens_code)
#define tracing_nesting               int_par(tracing_nesting_code)
#define pre_display_direction         int_par(pre_display_direction_code)
#define last_line_fit                 int_par(last_line_fit_code)
#define saving_vdiscards              int_par(saving_vdiscards_code)
#define saving_hyph_codes             int_par(saving_hyph_codes_code)
#define pdf_compress_level            int_par(pdf_compress_level_code)
#define pdf_major_version             int_par(pdf_major_version_code)
#define pdf_minor_version             int_par(pdf_minor_version_code)
#define synctex                       int_par(synctex_code)
#define tracing_stack_levels          int_par(tracing_stack_levels_code)
#define partoken_context              int_par(partoken_context_code)
#define show_stream                   int_par(show_stream_code)
/* sec 0247 */
#define par_indent_code               0   // {indentation of paragraphs}
#define math_surround_code            1   // {space around math in text}
#define line_skip_limit_code          2   // {threshold for |line_skip| instead of |baseline_skip|}
#define hsize_code                    3   // {line width in horizontal mode}
#define vsize_code                    4   // {page height in vertical mode}
#define max_depth_code                5   // {maximum depth of boxes on main pages}
#define split_max_depth_code          6   // {maximum depth of boxes on split pages}
#define box_max_depth_code            7   // {maximum depth of explicit vboxes}
#define hfuzz_code                    8   // {tolerance for overfull hbox messages}
#define vfuzz_code                    9   // {tolerance for overfull vbox messages}
#define delimiter_shortfall_code      10  // {maximum amount uncovered by variable delimiters}
#define null_delimiter_space_code     11  // {blank space in null delimiters}
#define script_space_code             12  // {extra space after subscript or superscript}
#define pre_display_size_code         13  // {length of text preceding a display}
#define display_width_code            14  // {length of line for displayed equation}
#define display_indent_code           15  // {indentation of line for displayed equation}
#define overfull_rule_code            16  // {width of rule that identifies overfull hboxes}
#define hang_indent_code              17  // {amount of hanging indentation}
#define h_offset_code                 18  // {amount of horizontal offset when shipping pages out}
#define v_offset_code                 19  // {amount of vertical offset when shipping pages out}
#define emergency_stretch_code        20  // {reduces badnesses on final pass of line-breaking}
#define t_baseline_shift_code         21  // {shift amount when mixing TATE-kumi and Alphabet}
#define y_baseline_shift_code         22  // {shift amount when mixing YOKO-kumi and Alphabet}
#define pdf_h_origin_code             23
#define pdf_v_origin_code             24
#define pdf_page_width_code           25
#define pdf_page_height_code          26
#define dimen_pars                    27
#define scaled_base                   (dimen_base + dimen_pars)     // {table of 256 user-defined \.{\\dimen} registers}
#define kinsoku_penalty_base          (scaled_base + 256)           // {table of 256 kinsoku registers}
#define eqtb_size                     (kinsoku_penalty_base + 255)  // {largest subscript of |eqtb|}
// #
#define dimen(a)                      eqtb[scaled_base + a].sc
#define dimen_par(a)                  eqtb[dimen_base + a].sc   // {a scaled quantity}
#define kinsoku_penalty(a)            eqtb[kinsoku_penalty_base + a].cint
#define par_indent                    dimen_par(par_indent_code)
#define math_surround                 dimen_par(math_surround_code)
#define line_skip_limit               dimen_par(line_skip_limit_code)
#define hsize                         dimen_par(hsize_code)
#define vsize                         dimen_par(vsize_code)
#define max_depth                     dimen_par(max_depth_code)
#define split_max_depth               dimen_par(split_max_depth_code)
#define box_max_depth                 dimen_par(box_max_depth_code)
#define hfuzz                         dimen_par(hfuzz_code)
#define vfuzz                         dimen_par(vfuzz_code)
#define delimiter_shortfall           dimen_par(delimiter_shortfall_code)
#define null_delimiter_space          dimen_par(null_delimiter_space_code)
#define script_space                  dimen_par(script_space_code)
#define pre_display_size              dimen_par(pre_display_size_code)
#define display_width                 dimen_par(display_width_code)
#define display_indent                dimen_par(display_indent_code)
#define overfull_rule                 dimen_par(overfull_rule_code)
#define hang_indent                   dimen_par(hang_indent_code)
#define h_offset                      dimen_par(h_offset_code)
#define v_offset                      dimen_par(v_offset_code)
#define t_baseline_shift              dimen_par(t_baseline_shift_code)
#define y_baseline_shift              dimen_par(y_baseline_shift_code)
#define emergency_stretch             dimen_par(emergency_stretch_code)
#define pdf_h_origin                  dimen_par(pdf_h_origin_code)
#define pdf_v_origin                  dimen_par(pdf_v_origin_code)
#define pdf_page_width                dimen_par(pdf_page_width_code)
#define pdf_page_height               dimen_par(pdf_page_height_code)
/* sec 0256 */
#define next(a)         hash[a].lh  // {link for coalesced lists}
#define text(a)         hash[a].rh  // {string number for control sequence name}
#define hash_is_full    (hash_used == hash_base)  // {test if all positions are occupied}
#define font_id_text(a) text(font_id_base + a)    // {a frozen font identifier's name}
// #
#define prim_prime 1777 // {about 85\pct! of |primitive_size|}
#define prim_base 1
#define prim_next(a) prim[a].lh // {link for coalesced lists}
#define prim_text(a) prim[a].rh // {string number for control sequence name, plus one}
#define prim_is_full (prim_used == prim_base) // {test if all positions are occupied}
#define prim_eq_level_field(a) a.hh.b1
#define prim_eq_type_field(a) a.hh.b0
#define prim_equiv_field(a) a.hh.rh
#define prim_eq_level(a) prim_eq_level_field(eqtb[prim_eqtb_base+a]) // {level of definition}
#define prim_eq_type(a) prim_eq_type_field(eqtb[prim_eqtb_base+a]) // {command code for equivalent}
#define prim_equiv(a) prim_equiv_field(eqtb[prim_eqtb_base+a]) // {equivalent value}
#define undefined_primitive 0
#define biggest_char 255 // { 65535 in XeTeX }
#define print_the_font_identifier_for_font_p()  \
do {                                            \
  sprint_esc(font_id_text(font(p)));            \
  if (ptex_tracing_fonts > 0)                   \
  {                                             \
    prints(" (");                               \
    print_font_name_and_size(font(p));          \
    if (ptex_tracing_fonts > 1)                 \
    {                                           \
      print_font_dir_and_enc(font(p));          \
    }                                           \
    prints(")");                                \
  }                                             \
} while (0)
/* sec 0268 */
#define save_type(a)      save_stack[a].hh.b0 // {classifies a |save_stack| entry}
#define save_level(a)     save_stack[a].hh.b1 // {saved level for regions 5 and 6, or group code}
#define save_index(a)     save_stack[a].hh.rh // {|eqtb| location or token or |save_stack| location}
#define restore_old_value 0 // {|save_type| when a value should be restored later}
#define restore_zero      1 // {|save_type| when an undefined entry should be restored}
#define insert_token      2 // {|save_type| when a token is being saved for later use}
#define level_boundary    3 // {|save_type| corresponding to beginning of group}
#define restore_sa        4 // {|save_type| when sparse array entries should be restored}
/* sec 0269 */
#define bottom_level        0   // {group code for the outside world}
#define simple_group        1   // {group code for local structure only}
#define hbox_group          2   // {code for `\.{\\hbox}\grp'}
#define adjusted_hbox_group 3   // {code for `\.{\\hbox}\grp' in vertical mode}
#define vbox_group          4   // {code for `\.{\\vbox}\grp'}
#define vtop_group          5   // {code for `\.{\\vtop}\grp'}
#define align_group         6   // {code for `\.{\\halign}\grp', `\.{\\valign}\grp'}
#define no_align_group      7   // {code for `\.{\\noalign}\grp'}
#define output_group        8   // {code for output routine}
#define math_group          9   // {code for, e.g., `\.{\char'136}\grp'}
#define disc_group          10  // {code for `\.{\\discretionary}\grp\grp\grp'}
#define insert_group        11  // {code for `\.{\\insert}\grp', `\.{\\vadjust}\grp'}
#define vcenter_group       12  // {code for `\.{\\vcenter}\grp'}
#define math_choice_group   13  // {code for `\.{\\mathchoice}\grp\grp\grp\grp'}
#define semi_simple_group   14  // {code for `\.{\\begingroup...\\endgroup}'}
#define math_shift_group    15  // {code for `\.{\$...\$}'}
#define math_left_group     16  // {code for `\.{\\left...\\right}'}
#define max_group_code      16
/* sec 0274 */
#define saved(a) save_stack[save_ptr + (a)].cint
/* sec 0289 */
#define cs_token_flag     0x1FFFFFFF  // {amount added to the |eqtb| location in a
// token that stands for a control sequence; is a multiple of~@"1000000, less~1}
#define max_char_val      0x100       // {to separate char and command code}
#define left_brace_token  0x100       // {$2^8\cdot|left_brace|$}
#define left_brace_limit  0x200       // {$2^8\cdot(|left_brace|+1)$}
#define right_brace_token 0x200       // {$2^8\cdot|right_brace|$}
#define right_brace_limit 0x300       // {$2^8\cdot(|right_brace|+1)$}
#define math_shift_token  0x300       // {$2^8\cdot|math_shift|$}
#define tab_token         0x400       // {$2^8\cdot|tab_mark|$}
#define out_param_token   0x500       // {$2^8\cdot|out_param|$}
#define space_token       0xA20       // {$2^8\cdot|spacer|+|" "|$}
#define letter_token      0xB00       // {$2^8\cdot|letter|$}
#define other_token       0xC00       // {$2^8\cdot|other_char|$}
#define match_token       0xD00       // {$2^8\cdot|match|$}
#define end_match_token   0xE00       // {$2^8\cdot|end_match|$}
#define protected_token   0xE01       // {$2^8\cdot|end_match|+1$}
/* sec 0298 */
#define chr_cmd(s)  \
do {                \
  prints(s);        \
  print(chr_code);  \
} while (0)
/* sec 0302 */
#define state       cur_input.state_field // {current scanner state}
#define index       cur_input.index_field // {reference for buffer information}
#define start       cur_input.start_field // {starting position in |buffer|}
#define limit       cur_input.limit_field // {end of current line in |buffer|}
#define name        cur_input.name_field  // {name of the current file}
#define synctex_tag cur_input.synctex_tag_field // {{\sl Sync\TeX} tag of the current file}
/* sec 0303 */
#define mid_line    1 // {|state| code when scanning a line of characters}
#define mid_kanji   (2 + max_char_code) // {|state| code when scanning a line of characters}
#define skip_blanks (3 + max_char_code + max_char_code) // {|state| code when ignoring blanks}
#define skip_blanks_kanji (4 + max_char_code + max_char_code + max_char_code) // {|state| code when ignoring blanks}
#define new_line    (5 + max_char_code + max_char_code + max_char_code + max_char_code) // {|state| code at start of line}
/* sec 0304 */
#define terminal_input (name == 0)  // {are we reading from the terminal?}
#define cur_file input_file[index]  // {the current |alpha_file| variable}
/* sec 0305 */
#define skipping  1 // {|scanner_status| when passing conditional text}
#define defining  2 // {|scanner_status| when reading a macro definition}
#define matching  3 // {|scanner_status| when reading macro arguments}
#define aligning  4 // {|scanner_status| when reading an alignment preamble}
#define absorbing 5 // {|scanner_status| when reading a balanced text}
/* sec 0307 */
#define token_list         0      // {|state| code when scanning a token list}
#define token_type         index  // {type of current token list}
#define param_start        limit  // {base of macro parameters in |param_stack|}
#define parameter          0      // {|token_type| code for parameter}
#define u_template         1      // {|token_type| code for \<u_j> template}
#define v_template         2      // {|token_type| code for \<v_j> template}
#define backed_up          3      // {|token_type| code for text to be reread}
#define inserted           4      // {|token_type| code for inserted texts}
#define macro              5      // {|token_type| code for defined control sequences}
#define output_text        6      // {|token_type| code for output routines}
#define every_par_text     7      // {|token_type| code for \.{\\everypar}}
#define every_math_text    8      // {|token_type| code for \.{\\everymath}}
#define every_display_text 9      // {|token_type| code for \.{\\everydisplay}}
#define every_hbox_text    10     // {|token_type| code for \.{\\everyhbox}}
#define every_vbox_text    11     // {|token_type| code for \.{\\everyvbox}}
#define every_job_text     12     // {|token_type| code for \.{\\everyjob}}
#define every_cr_text      13     // {|token_type| code for \.{\\everycr}}
#define mark_text          14     // {|token_type| code for \.{\\topmark}, etc.}
//#
#define eTeX_text_offset   (output_routine_loc - output_text)
#define every_eof_text     (every_eof_loc - eTeX_text_offset)
//#
#define write_text         (toks_base - eTeX_text_offset)
/* sec 0316 */
#define begin_pseudoprint() \
do {                        \
  l = tally;                \
  tally = 0;                \
  selector = pseudo;        \
  kcode_pos = 0;            \
  trick_count = 1000000;    \
} while (0)
//
#define set_trick_count()                                       \
do {                                                            \
  first_count = tally;                                          \
  kcp = trick_buf2[(first_count - 1) % error_line];             \
                                                                \
  if ((first_count > 0) && (kcp > 0))                           \
    first_count = first_count + nrestmultichr(kcp);             \
                                                                \
  trick_count = first_count + 1 + error_line - half_error_line; \
                                                                \
  if (trick_count < error_line)                                 \
    trick_count = error_line;                                   \
} while (0)
/* sec 0323 */
#define back_list(a) begin_token_list(a, backed_up) // {backs up a simple token list}
#define ins_list(a)  begin_token_list(a, inserted)  // {inserts a simple token list}
/* sec 0344 */
#define any_state_plus(a)       \
  mid_line + (a):               \
  case mid_kanji + (a):         \
  case skip_blanks + (a):       \
  case skip_blanks_kanji + (a): \
  case new_line + (a)
/* sec 0347 */
#define add_delims_to(a) \
  (a) + math_shift:      \
  case (a) + tab_mark:   \
  case (a) + mac_param:  \
  case (a) + sub_mark:   \
  case (a) + letter:     \
  case (a) + other_char
//#
#define all_jcode(a)     \
  (a) + kanji:           \
  case (a) + kana:       \
  case (a) + other_kchar
#define hangul_code(a)  \
  (a) + hangul
/* sec 0352 */
#define is_hex(a) \
  (((a >= '0') && (a <= '9')) || ((a >= 'a') && (a <= 'f')))
//
#define hex_to_cur_chr()                    \
do {                                        \
  if (c <= '9')                             \
    cur_chr = c - '0';                      \
  else                                      \
    cur_chr = c - 'a' + 10;                 \
                                            \
  if (cc <= '9')                            \
    cur_chr = 16 * cur_chr + cc - '0';      \
  else                                      \
    cur_chr = 16 * cur_chr + cc - 'a' + 10; \
} while (0)
/* sec 0360 */
#define end_line_char_inactive()  \
  (end_line_char < 0) || (end_line_char > 255)
/* sec 0358 */
/* sec 0371 */
#define store_new_token(a)  \
do {                        \
  q = get_avail();          \
  link(p) = q;              \
  info(q) = a;              \
  p = q;                    \
} while (0)
//
#define fast_store_new_token(a) \
do {                            \
  fast_get_avail(q);            \
  link(p) = q;                  \
  info(q) = a;                  \
  p = q;                        \
} while (0)
#define no_expand_flag 257  // {this characterizes a special variant of |relax|}
/* sec 0382 */
#define marks_code            5 // {add this for \.{\\topmarks} etc.}
//#
#define top_mark_code         0 // {the mark in effect at the previous page break}
#define first_mark_code       1 // {the first mark between |top_mark| and |bot_mark|}
#define bot_mark_code         2 // {the mark in effect at the current page break}
#define split_first_mark_code 3 // {the first mark found by \.{\\vsplit}}
#define split_bot_mark_code   4 // {the last mark found by \.{\\vsplit}}
#define top_mark              cur_mark[top_mark_code]
#define first_mark            cur_mark[first_mark_code]
#define bot_mark              cur_mark[bot_mark_code]
#define split_first_mark      cur_mark[split_first_mark_code]
#define split_bot_mark        cur_mark[split_bot_mark_code]
/* sec 0400 */
#define int_val   0 // {integer values}
#define dimen_val 1 // {dimension values}
#define glue_val  2 // {glue specifications}
#define mu_val    3 // {math glue specifications}
#define ident_val 4 // {font identifier}
#define tok_val   5 // {token lists}
/* sec 0413 */
#define scanned_result(va, vb)  \
do {                            \
  cur_val = va;                 \
  cur_val_level = vb;           \
} while (0)
/* sec 0416 */
#define last_node_type_code       (glue_val + 1)            // {code for \.{\\lastnodetype}}
#define last_node_char_code       (glue_val + 2)            // {code for \.{\\lastnodechar}}
#define last_node_subtype_code    (glue_val + 3)            // {code for \.{\\lastnodesubtype}}
#define input_line_no_code        (glue_val + 4)            // {code for \.{\\inputlineno}}
#define badness_code              (input_line_no_code + 1)  // {code for \.{\\badness}}
#define shell_escape_code         (badness_code + 1)        // {code for \.{\\shellescape}}
#define ptex_version_code         (shell_escape_code + 1)   // {code for \.{\\ptexversion}}
#define ptex_minor_version_code   (ptex_version_code + 1)   // {code for \.{\\ptexminorversion}}
#define uptex_version_code        (ptex_minor_version_code + 1) // {code for \.{\\uptexversion}}
#define eptex_version_code        (uptex_version_code + 1)      // {code for \.{\\epTeXversion}}
#define pdf_last_x_pos_code       (eptex_version_code + 1)      // {code for \.{\\pdflastxpos}}
#define pdf_last_y_pos_code       (pdf_last_x_pos_code + 1)     // {code for \.{\\pdflastypos}}
#define elapsed_time_code         (pdf_last_y_pos_code + 1)     // {code for \.{\\elapsedtime}}
#define random_seed_code          (elapsed_time_code + 1)       // {code for \.{\\randomseed}}
//
#define eTeX_int                   (badness_code + 10)        // {first of \eTeX\ codes for integers}
#define eTeX_version_code          eTeX_int
#define current_group_level_code   (eTeX_int + 1)
#define current_group_type_code    (eTeX_int + 2)
#define current_if_level_code      (eTeX_int + 3)
#define current_if_type_code       (eTeX_int + 4)
#define current_if_branch_code     (eTeX_int + 5)
#define glue_stretch_order_code    (eTeX_int + 6)
#define glue_shrink_order_code     (eTeX_int + 7)
#define current_spacing_mode_code  (eTeX_int + 8)
#define current_xspacing_mode_code (eTeX_int + 9)
#define current_cjk_token_code     (eTeX_int + 10)
#define eTeX_dim                   (eTeX_int + 11)
#define font_char_wd_code          eTeX_dim
#define font_char_ht_code          (eTeX_dim + 1)
#define font_char_dp_code          (eTeX_dim + 2)
#define font_char_ic_code          (eTeX_dim + 3)
#define par_shape_length_code      (eTeX_dim + 4)
#define par_shape_indent_code      (eTeX_dim + 5)
#define par_shape_dimen_code       (eTeX_dim + 6)
#define glue_stretch_code          (eTeX_dim + 7)
#define glue_shrink_code           (eTeX_dim + 8)
#define eTeX_glue                  (eTeX_dim + 9)
#define eTeX_mu                    (eTeX_glue + 1)
#define mu_to_glue_code            eTeX_glue
#define glue_to_mu_code            eTeX_mu
#define eTeX_expr                  (eTeX_mu + 1)
/* sec 0421 */
#define max_dimen 07777777777
/* sec 0438 */
#define octal_token             (other_token + '\'')  // {apostrophe, indicates an octal constant}
#define hex_token               (other_token + '"' )  // {double quote, indicates a hex constant}
#define alpha_token             (other_token + '`' )  // {reverse apostrophe, precedes alpha constants}
#define point_token             (other_token + '.' )  // {decimal point}
#define continental_point_token (other_token + ',' )  // {decimal point, Eurostyle}
/* sec 0445 */
#define infinity      017777777777                    // {the largest positive value that \TeX\ knows}
#define zero_token    (other_token  + '0')            // {zero, the smallest digit}
#define A_token       (letter_token + 'A')            // {the smallest special hex digit}
#define other_A_token (other_token  + 'A')            // {special hex digit of type |other_char|}
/* sec 0448 */
#define scan_normal_dimen() scan_dimen(false, false, false)
/* sec 0458 */
#define set_conversion(a, b)  \
do {                          \
  num = a;                    \
  denom = b;                  \
} while (0)
/* sec 0468 */
#define number_code              0  // {command code for \.{\\number}}
#define roman_numeral_code       1  // {command code for \.{\\romannumeral}}
#define kansuji_code             2  // {command code for \.{\\kansuji}}
#define string_code              3  // {command code for \.{\\string}}
#define meaning_code             4  // {command code for \.{\\meaning}}
#define font_name_code           5  // {command code for \.{\\fontname}}
#define euc_code                 6  // {command code for \.{\\euc}}
#define sjis_code                7  // {command code for \.{\\sjis}}
#define jis_code                 8  // {command code for \.{\\jis}}
#define kuten_code               9  // {command code for \.{\\kuten}}
#define ucs_code                 10 // {command code for \.{\\ucs}}
#define toucs_code               11 // {command code for \.{\\toucs}}
#define tojis_code               12 // {command code for \.{\\tojis}}
#define ptex_font_name_code      13 // {command code for \.{\\ptexfontname}}
#define eTeX_revision_code       14 // {base for \eTeX's command codes}
#define ng_strcmp_code           15 // {command code for \.{\\pdfstrcmp}}
#define ng_banner_code           16 // {command code for \.{\\ngbanner}}
#define ng_os_type_code          17 // {command code for \.{\\ngostype}}
#define ptex_revision_code       18 // {command code for \.{\\ptexrevision}}
#define uptex_revision_code      19 // {command code for \.{\\uptexrevision}}
#define pdf_creation_date_code   20 // {command code for \.{\\pdfcreationdate}}
#define pdf_file_mod_date_code   21 // {command code for \.{\\pdffilemodedate}}
#define pdf_file_size_code       22 // {command code for \.{\\pdffilesize}}
#define pdf_mdfive_sum_code      23 // {command code for \.{\\pdfmdfivesum}}
#define pdf_file_dump_code       24 // {command code for \.{\\pdffiledump}}
#define pdf_uniform_deviate_code 25 // {command code for \.{\\pdfuniformdeviate}}
#define pdf_normal_deviate_code  26 // {command code for \.{\\pdfnormaldeviate}}
#define expanded_code            27 // {command code for \.{\\expanded}}
#define Uchar_convert_code       28 // {command code for \.{\\Uchar}}
#define Ucharcat_convert_code    29 // {command code for \.{\\Ucharcat}}
#define job_name_code            30 // {command code for \.{\\jobname}}
/* sec 0480 */
#define closed    2
#define just_open 1
/* sec 0487 */
#define unless_code       32
#define if_char_code      0
#define if_cat_code       1
#define if_int_code       2
#define if_dim_code       3
#define if_odd_code       4
#define if_vmode_code     5
#define if_hmode_code     6
#define if_mmode_code     7
#define if_inner_code     8
#define if_void_code      9
#define if_hbox_code      10
#define if_vbox_code      11
#define ifx_code          12
#define if_eof_code       13
#define if_true_code      14
#define if_false_code     15
#define if_case_code      16
#define if_def_code       17
#define if_cs_code        18
#define if_font_char_code 19
//#
#define if_in_csname_code     (if_case_code + 4)
#define if_pdfprimitive_code  (if_in_csname_code + 1)
#define if_tdir_code          (if_pdfprimitive_code + 1)
#define if_ydir_code          (if_tdir_code + 1)
#define if_ddir_code          (if_ydir_code + 1)
#define if_mdir_code          (if_ddir_code + 1)
#define if_tbox_code          (if_mdir_code + 1)
#define if_ybox_code          (if_tbox_code + 1)
#define if_dbox_code          (if_ybox_code + 1)
#define if_mbox_code          (if_dbox_code + 1)
#define if_jfont_code         (if_mbox_code + 1)
#define if_tfont_code         (if_jfont_code + 1)
/* sec 0489 */
#define if_node_size     2                  // {number of words in stack entry for conditionals}
#define if_line_field(a) mem[(a) + 1].cint
#define if_code          1  // {code for \.{\\if...} being evaluated}
#define fi_code          2  // {code for \.{\\fi}}
#define else_code        3  // {code for \.{\\else}}
#define or_code          4  // {code for \.{\\or}}
/* sec 0506 */
#define get_x_token_or_active_char()                    \
do {                                                    \
  get_x_token();                                        \
                                                        \
  if (cur_cmd == relax)                                 \
  {                                                     \
    if (cur_chr == no_expand_flag)                      \
    {                                                   \
      cur_cmd = active_char;                            \
      cur_chr = cur_tok - cs_token_flag - active_base;  \
    }                                                   \
  }                                                     \
} while (0)
/* sec 0519 */
#define append_to_name(a)       \
do {                            \
  c = a; incr(k);               \
                                \
  if (k < file_name_size)       \
    name_of_file[k] = xchr[c];  \
} while (0)
/* sec 0529 */
#define pack_cur_name() pack_file_name(cur_name, cur_area, cur_ext)
/* sec 0544 */
#define yoko_jfm_id 11
#define tate_jfm_id 9
//#
#define no_tag   0 // {vanilla character}
#define lig_tag  1 // {character has a ligature/kerning program}
#define gk_tag   1
#define list_tag 2 // {character has a successor in a charlist}
#define ext_tag  3 // {character is extensible}
/* sec 0545 */
#define stop_flag    128  // {value indicating `\.{STOP}' in a lig/kern program}
#define kern_flag    128  // {op code for a kern step}
#define skip_byte(a) a.b0
#define next_char(a) a.b1
#define op_byte(a)   a.b2
#define rem_byte(a)  a.b3
/* sec 0546 */
#define ext_top(a) a.b0 // {|top| piece in a recipe}
#define ext_mid(a) a.b1 // {|mid| piece in a recipe}
#define ext_bot(a) a.b2 // {|bot| piece in a recipe}
#define ext_rep(a) a.b3 // {|rep| piece in a recipe}
/* sec 0547 */
#define slant_code         1
#define space_code         2
#define space_stretch_code 3
#define space_shrink_code  4
#define x_height_code      5
#define quad_code          6
#define extra_space_code   7
/* sec 0549 */
#define non_char    256
#define non_address 0
/* sec 0554 */
#define char_info(a, b)   font_info[char_base[a] + b].qqqq
#define kchar_code(a, b)  font_info[ctype_base[a] + b].hh.rh
#define kchar_type(a, b)  font_info[ctype_base[a] + b].hh.lh
#define char_width(a, b)  font_info[width_base[a] + b.b0].cint
#define char_exists(a)    (a.b0 > min_quarterword)
#define char_italic(a, b) font_info[italic_base[a] + (b.b2) / 4].sc
#define height_depth(a)   (a.b1)
#define char_height(a, b) font_info[height_base[a] + (b) / 16].sc
#define char_depth(a, b)  font_info[depth_base[a] + (b) % 16].sc
#define char_tag(a)       (a.b2 % 4)
/* sec 0557 */
#define char_kern(a, b)         font_info[kern_base[a] + 256 * op_byte(b) + rem_byte(b)].sc
#define kern_base_offset        (256 * (128 + min_quarterword))
#define lig_kern_start(a, b)    lig_kern_base[a] + rem_byte(b)
#define glue_kern_start(a, b)   lig_kern_base[a] + rem_byte(b)
#define glue_kern_restart(a, b) lig_kern_base[a] + 256 * op_byte(b) + rem_byte(b) + 32768 - kern_base_offset
#define lig_kern_restart(a, b)  lig_kern_base[a] + 256 * op_byte(b) + rem_byte(b) + 32768 - kern_base_offset
/* sec 0558 */
#define param(a, b)      font_info[a + param_base[b]].sc
#define slant(f)         param(slant_code, f)
#define space(f)         param(space_code, f)
#define space_stretch(f) param(space_stretch_code, f)
#define space_shrink(f)  param(space_shrink_code, f)
#define x_height(f)      param(x_height_code, f)
#define quad(f)          param(quad_code, f)
#define extra_space(f)   param(extra_space_code, f)
/* sec 0561 */
#define start_font_error_message()  \
do {                                \
  print_err("Font ");               \
  sprint_cs(u);                     \
  print_char('=');                  \
  print_file_name(nom, aire, 335);  \
                                    \
  if (s >= 0)                       \
  {                                 \
    prints(" at ");                 \
    print_scaled(s);                \
    prints("pt");                   \
  }                                 \
  else if (s != -1000)              \
  {                                 \
    prints(" scaled ");             \
    print_int(-s);                  \
  }                                 \
} while (0)
/* sec 0564 */
#define read_sixteen(a) \
do {                    \
  a = fbyte;            \
                        \
  if (a > 127)          \
    goto bad_tfm;       \
                        \
  fget();               \
  a = a * 256 + fbyte;  \
} while (0)
// #
#define read_twentyfourx(a) \
do {                        \
  a = fbyte;                \
  fget();                   \
  a = a * 0x100 + fbyte;    \
  fget();                   \
  a = a + fbyte * 0x10000;  \
} while (0)
// #
#define store_four_quarters(val)  \
do {                              \
  fget(); a = fbyte; qw.b0 = a;   \
  fget(); b = fbyte; qw.b1 = b;   \
  fget(); c = fbyte; qw.b2 = c;   \
  fget(); d = fbyte; qw.b3 = d;   \
  val = qw;                       \
} while (0)
/* sec 0570 */
#define check_byte_range(a) \
do {                        \
  if ((a < bc) || (a > ec)) \
    goto bad_tfm;           \
} while (0)
/* sec 0571 */
#define store_scaled(a)                                       \
do {                                                          \
  fget(); a = fbyte; fget(); b = fbyte;                       \
  fget(); c = fbyte; fget(); d = fbyte;                       \
  sw = (((((d * z) / 256) + (c * z)) / 256) + (b * z)) / beta;\
                                                              \
  if (a == 0)                                                 \
    a = sw;                                                   \
  else if (a == 255)                                          \
    a = sw - alpha;                                           \
  else                                                        \
    goto bad_tfm;                                             \
} while (0)
/* sec 0573 */
#define check_existence(a)  \
do {                        \
  check_byte_range(a);      \
  qw = char_info(f, a);     \
                            \
  if (!char_exists(qw))     \
    goto bad_tfm;           \
} while (0)
/* sec 0576 */
#define adjust(a) a[f] = a[f]
/* sec 0585 */
#define set1      128 // {typeset a character and move right}
#define set2      129 //
#define set3      130 //
#define set_rule  132 // {typeset a rule and move right}
#define put_rule  137 // {typeset a rule}
#define nop       138 // {no operation}
#define bop       139 // {beginning of page}
#define eop       140 // {ending of page}
#define push      141 // {save the current positions}
#define pop       142 // {restore previous positions}
#define right1    143 // {move right}
#define w0        147 // {move right by |w|}
#define w1        148 // {move right and set |w|}
#define x0        152 // {move right by |x|}
#define x1        153 // {move right and set |x|}
#define down1     157 // {move down}
#define y0        161 // {move down by |y|}
#define y1        162 // {move down and set |y|}
#define z0        166 // {move down by |z|}
#define z1        167 // {move down and set |z|}
#define fnt_num_0 171 // {set current font to 0}
#define fnt1      235 // {set current font}
#define fnt2      236 // 
#define xxx1      239 // {extension to \.{DVI} primitives}
#define xxx4      242 // {potentially long extension to \.{DVI} primitives}
#define fnt_def1  243 // {define the meaning of a font number}
#define fnt_def2  244 //
#define pre       247 // {preamble}
#define post      248 // {postamble beginning}
#define post_post 249 // {postamble ending}
#define dirchg    255
/* sec 0587 */
#define id_byte    2
#define ex_id_byte 3
/* sec 0605 */
#define movement_node_size  3 // {number of words per entry in the down and right stacks} 
#define location(a)         mem[a + 2].cint // {\.{DVI} byte number for a movement command}
/* sec 0608 */
#define y_here  1 // {|info| when the movement entry points to a |y| command}
#define z_here  2 // {|info| when the movement entry points to a |z| command}
#define yz_OK   3 // {|info| corresponding to an unconstrained \\{down} command}
#define y_OK    4 // {|info| corresponding to a \\{down} that can't become a |z|}
#define z_OK    5 // {|info| corresponding to a \\{down} that can't become a |y|}
#define d_fixed 6 // {|info| corresponding to a \\{down} that can't change}
/* sec 0611 */
#define none_seen 0   // {no |y_here| or |z_here| nodes have been encountered yet}
#define y_seen    6   // {we have seen |y_here| but not |z_here|}
#define z_seen    12  // {we have seen |z_here| but not |y_here|}
/* sec 0625 */
#define billion 1000000000.0
#define vet_glue(a)             \
do {                            \
  glue_temp = a;                \
                                \
  if (glue_temp > billion)      \
    glue_temp = billion;        \
  else if (glue_temp < -billion)\
    glue_temp = -billion;       \
} while (0)
/* sec 0644 */
#define exactly     0 // {a box dimension is pre-specified}
#define additional  1 // {a box dimension is increased from the natural one}
#define natural     0, additional // {shorthand for parameters to |hpack| and |vpack|}
/* sec 0769 */
#define u_part(a)     mem[(a) + height_offset].cint // {pointer to \<u_j> token list}
#define v_part(a)     mem[(a) + depth_offset].cint  // {pointer to \<v_j> token list}
#define extra_info(a) info((a) + list_offset) // {info to remember during template}
/* sec 0681 */
#define noad_size             5             // {number of words in a normal noad}
#define nucleus(a)            ((a) + 1)     // {the |nucleus| field of a noad}
#define supscr(a)             ((a) + 2)     // {the |supscr| field of a noad}
#define subscr(a)             ((a) + 3)     // {the |subscr| field of a noad}
#define kcode_noad(a)         ((a) + 4)
#define math_kcode(a)         info((a) + 4) // {the |kanji character| field of a noad}
#define kcode_noad_nucleus(a) ((a) + 3)
#define math_kcode_nucleus(a) info((a) + 3) // {the |kanji character| field offset from nucleus}
//#
#define math_jchar            6
#define math_text_jchar       7
#define math_type             link  // {a |halfword| in |mem|}
#define fam                   font  // {a |quarterword| in |mem|}
#define math_char             1     // {|math_type| when the attribute is simple}
#define sub_box               2     // {|math_type| when the attribute is a box}
#define sub_exp_box           3     // {|math_type| when the attribute is an explicit created box}
#define sub_mlist             4     // {|math_type| when the attribute is a formula}
#define math_text_char        5     // {|math_type| when italic correction is dubious}
/* sec 0682 */
#define ord_noad   (unset_node + 3) // 18 {|type| of a noad classified Ord}
#define op_noad    (ord_noad + 1  ) // 19 {|type| of a noad classified Op}
#define bin_noad   (ord_noad + 2  ) // 20 {|type| of a noad classified Bin}
#define rel_noad   (ord_noad + 3  ) // 21 {|type| of a noad classified Rel}
#define open_noad  (ord_noad + 4  ) // 22 {|type| of a noad classified Ope}
#define close_noad (ord_noad + 5  ) // 23 {|type| of a noad classified Clo}
#define punct_noad (ord_noad + 6  ) // 24 {|type| of a noad classified Pun}
#define inner_noad (ord_noad + 7  ) // 25 {|type| of a noad classified Inn}
#define limits    1 // {|subtype| of |op_noad| whose scripts are to be above, below}
#define no_limits 2 // {|subtype| of |op_noad| whose scripts are to be normal}
/* sec 0683 */
#define left_delimiter(a)  ((a) + 5)
#define right_delimiter(a) ((a) + 4)
#define radical_noad       (inner_noad + 1) // 24
#define radical_noad_size  6
#define fraction_noad      (radical_noad + 1) // 25
#define fraction_noad_size 6
#define small_fam(a)       mem[(a)].qqqq.b0
#define small_char(a)      mem[(a)].qqqq.b1
#define large_fam(a)       mem[(a)].qqqq.b2
#define large_char(a)      mem[(a)].qqqq.b3
#define thickness          width
#define default_code       010000000000
#define numerator          supscr
#define denominator        subscr
/* sec 0687 */
#define under_noad        (fraction_noad + 1) // 26
#define over_noad         (under_noad + 1   ) // 27
#define accent_noad       (over_noad + 1    ) // 28
#define accent_noad_size  6
#define accent_chr(a)     ((a) + 5)
#define vcenter_noad      (accent_noad + 1  ) // 29
#define left_noad         (vcenter_noad + 1 ) // 30
#define right_noad        (left_noad + 1    ) // 31
#define delimiter         nucleus
#define middle_noad       1
#define script_allowed(a) ((type(a) >= ord_noad) && (type(a) < left_noad))
/* sec 0688 */
#define style_node          (unset_node + 1)  // 16 {|type| of a style node}
#define style_node_size     3 // {number of words in a style node}
#define display_style       0 // {|subtype| for \.{\\displaystyle}}
#define text_style          2 // {|subtype| for \.{\\textstyle}}
#define script_style        4 // {|subtype| for \.{\\scriptstyle}}
#define script_script_style 6 // {|subtype| for \.{\\scriptscriptstyle}}
#define cramped             1 // {add this to an uncramped style if you want to cramp it}
/* sec 0689 */
#define choice_node            (unset_node + 2) // 17 {|type| of a choice node}
#define display_mlist(a)       info((a) + 1)  // {mlist to be used in display style}
#define text_mlist(a)          link((a) + 1)  // {mlist to be used in text style}
#define script_mlist(a)        info((a) + 2)  // {mlist to be used in script style}
#define script_script_mlist(a) link((a) + 2)  // {mlist to be used in scriptscript style}
/* sec 0699 */
#define text_size          0  // {size code for the largest size in a family}
#define script_size        16 // {size code for the medium size in a family}
#define script_script_size 32 // {size code for the smallest size in a family}
/* sec 0700 */
#define mathsy(a, b)        font_info[a + param_base[fam_fnt(2 + b)]].cint
#define math_x_height(a)    mathsy(5, a)  // {height of `\.x'}
#define math_quad(a)        mathsy(6, a)  // {\.{18mu}}
#define num1(a)             mathsy(8, a)  // {numerator shift-up in display styles}
#define num2(a)             mathsy(9, a)  // {numerator shift-up in non-display, non-\.{\\atop}}
#define num3(a)             mathsy(10, a) // {numerator shift-up in non-display \.{\\atop}}
#define denom1(a)           mathsy(11, a) // {denominator shift-down in display styles}
#define denom2(a)           mathsy(12, a) // {denominator shift-down in non-display styles}
#define sup1(a)             mathsy(13, a) // {superscript shift-up in uncramped display style}
#define sup2(a)             mathsy(14, a) // {superscript shift-up in uncramped non-display}
#define sup3(a)             mathsy(15, a) // {superscript shift-up in cramped styles}
#define sub1(a)             mathsy(16, a) // {subscript shift-down if superscript is absent}
#define sub2(a)             mathsy(17, a) // {subscript shift-down if superscript is present}
#define sup_drop(a)         mathsy(18, a) // {superscript baseline below top of large box}
#define sub_drop(a)         mathsy(19, a) // {subscript baseline below bottom of large box}
#define delim1(a)           mathsy(20, a) // {size of \.{\\atopwithdelims} delimiters in display styles}
#define delim2(a)           mathsy(21, a) // {size of \.{\\atopwithdelims} delimiters in non-displays}
#define axis_height(a)      mathsy(22, a) // {height of fraction lines above the baseline}
#define total_mathsy_params 22
/* sec 0701 */
#define mathex(a)              font_info[(a) + param_base[fam_fnt(3 + cur_size)]].cint
#define default_rule_thickness mathex(8)  // {thickness of \.{\\over} bars}
#define big_op_spacing1        mathex(9)  // {minimum clearance above a displayed op}
#define big_op_spacing2        mathex(10) // {minimum clearance below a displayed op}
#define big_op_spacing3        mathex(11) // {minimum baselineskip above displayed op}
#define big_op_spacing4        mathex(12) // {minimum baselineskip below displayed op}
#define big_op_spacing5        mathex(13) // {padding above and below displayed limits}
#define total_mathex_params    13
/* sec 0702 */
#define cramped_style(a) (2 * ((a) / 2) + cramped)  // {cramp the style}
#define sub_style(a)     (2 * ((a) / 4) + script_style + cramped) // {smaller and cramped}
#define sup_style(a)     (2 * ((a) / 4) + script_style + ((a) % 2)) // {smaller}
#define num_style(a)     ((a) + 2 - 2 * ((a) / 6))  // {smaller unless already script-script}
#define denom_style(a)   (2 * ((a) / 2) + cramped + 2 - 2 * ((a) / 6))  // {smaller, cramped}
/* sec 0716 */
#define mu_mult(a) nx_plus_y(n, a, xn_over_d(a, f, 0200000))
/* sec 0725 */
#define new_hlist(a) mem[nucleus(a)].cint
/* sec 0731 */
#define choose_mlist(a) \
do {                    \
  p = a(q);             \
  a(q) = null;          \
} while (0)
/* sec 0770 */
#define preamble              link(align_head)
#define align_stack_node_size 5
/* sec 0780 */
#define span_code          256
#define cr_code            257
#define cr_cr_code         (cr_code + 1)
#define end_template_token (cs_token_flag + frozen_end_template)
/* sec 0797 */
#define span_node_size 2
/* sec 0817 */
#define tight_fit      3
#define loose_fit      1
#define very_loose_fit 0
#define decent_fit     2
/* sec 0819 */
#define active_node_size_normal   3
#define active_node_size_extended 5
#define active_short(a)           mem[a + 3].cint
#define active_glue(a)            mem[a + 4].cint
#define fitness                   subtype
#define break_node                rlink
#define line_number               llink
#define total_demerits(a)         mem[a + 2].cint
#define unhyphenated              0
#define hyphenated                1
#define last_active               active
/* sec 0821 */
#define passive_node_size 2     // {number of words in passive nodes}
#define cur_break         rlink // {in passive node, points to position of this breakpoint}
#define prev_break        llink // {points to passive node that should precede this one}
#define serial            info  // {serial number for symbolic identification}
/* sec 0822 */
#define delta_node_size 7 // {number of words in a delta node}
#define delta_node      2 // {|type| field in a delta node}
/* sec 0823 */
#define do_all_six(a) \
do {                  \
  a(1); a(2); a(3);   \
  a(4); a(5); a(6);   \
} while (0)
/* sec 0825 */
#define check_shrinkage(s)                            \
do {                                                  \
  if ((shrink_order(s) != normal) && (shrink(s) != 0))\
    s = finite_shrink(s);                             \
} while (0)
/* sec 0829 */
#define copy_to_cur_active(a) cur_active_width[a] = active_width[a]
/* sec 0832 */
#define update_width(a) cur_active_width[a] = cur_active_width[a] + mem[r + (a)].cint
/* sec 0833 */
#define awful_bad 07777777777
/* sec 0837 */
#define set_break_width_to_background(a) break_width[a] = background[a]
/* sec 0843 */
#define convert_to_break_width(a)   mem[prev_r + (a)].cint = mem[prev_r + (a)].cint - cur_active_width[a] + break_width[a]
#define store_break_width(a)        active_width[a] = break_width[a]
#define new_delta_to_break_width(a) mem[q + (a)].cint = break_width[(a)] - cur_active_width[(a)]
/* sec 0844 */
#define new_delta_from_break_width(a) mem[q + (a)].cint = cur_active_width[(a)] - break_width[(a)]
/* sec 0860 */
#define combine_two_deltas(a) mem[prev_r + (a)].cint = mem[prev_r + (a)].cint + mem[r + (a)].cint
#define downdate_width(a)     cur_active_width[(a)] = cur_active_width[(a)] - mem[prev_r + (a)].cint
/* sec 0861 */
#define update_active(a) active_width[(a)] = active_width[(a)] + mem[r + (a)].cint
/* sec 0864 */
#define store_background(a) active_width[a] = background[a]
/* sec 0866 */
#define act_width active_width[1]
#define kern_break()                                \
do {                                                \
  if (!is_char_node(link(cur_p)) && auto_breaking)  \
    if (type(link(cur_p)) == glue_node)             \
      try_break(0, unhyphenated);                   \
                                                    \
  act_width = act_width + width(cur_p);             \
} while (0)
/* sec 0877 */
#define next_break prev_break
/* sec 0908 */
#define append_charnode_to_t(a) \
do {                            \
  link(t) = get_avail();        \
  t = link(t);                  \
  font(t) = hf;                 \
  character(t) = (a);           \
} while (0)
//
#define set_cur_r()     \
do {                    \
  if (j < n)            \
    cur_r = hu[j + 1];  \
  else                  \
    cur_r = bchar;      \
                        \
  if (odd(hyf[j]))      \
    cur_rh = hchar;     \
  else                  \
    cur_rh = non_char;  \
} while (0)
/* sec 0910 */
#define wrap_lig(a)                           \
do {                                          \
  if (ligature_present)                       \
  {                                           \
    p = new_ligature(hf, cur_l, link(cur_q)); \
                                              \
    if (lft_hit)                              \
    {                                         \
      subtype(p) = 2;                         \
      lft_hit = false;                        \
    }                                         \
                                              \
    if ((a))                                  \
      if (lig_stack == 0)                     \
      {                                       \
        incr(subtype(p));                     \
        rt_hit = false;                       \
      }                                       \
                                              \
    link(cur_q) = p;                          \
    t = p;                                    \
    ligature_present = false;                 \
  }                                           \
} while (0)
//
#define pop_lig_stack()                       \
do {                                          \
  if (lig_ptr(lig_stack) != 0)                \
  {                                           \
    link(t) = lig_ptr(lig_stack);             \
    t = link(t);                              \
    incr(j);                                  \
  }                                           \
                                              \
  p = lig_stack;                              \
  lig_stack = link(p);                        \
  free_node(p, small_node_size);              \
                                              \
  if (lig_stack == 0)                         \
  {                                           \
    set_cur_r();                              \
  }                                           \
  else                                        \
    cur_r = character(lig_stack);             \
} while (0)
/* sec 0914 */
#define advance_major_tail()      \
do {                              \
  major_tail = link(major_tail);  \
  incr(r_count);                  \
} while (0)
/* sec 0947 */
#define trie_root trie_l[0]
/* sec 0970 */
#define active_height      active_width
#define cur_height         active_height[1]
#define set_height_zero(a) active_width[(a)] = 0
/* sec 0974 */
#define deplorable 100000
/* sec 0980 */
#define inserts_only 1
#define box_there    2
/* sec 0981 */
#define page_ins_node_size 4            // {number of words for a page insertion node}
#define inserting          0            // {an insertion class that has not yet overflowed}
#define split_up           1            // {an overflowed insertion class}
#define broken_ptr(a)      link(a + 1)  // {an insertion for this class will break here if anywhere}
#define broken_ins(a)      info(a + 1)  // {this insertion might break at |broken_ptr|}
#define last_ins_ptr(a)    link(a + 2)  // {the most recent insertion for this |subtype|}
#define best_ins_ptr(a)    info(a + 2)  // {the optimum most recent insertion}
/* sec 0982 */
#define page_goal   page_so_far[0]  // {desired height of information on page being built}
#define page_total  page_so_far[1]  // {height of the current page}
#define page_shrink page_so_far[6]  // {shrinkability of the current page}
#define page_depth  page_so_far[7]  // {depth of the current page}
/* sec 0987 */
#define set_page_so_far_zero(a) page_so_far[(a)] = 0
/* sec 0995 */
#define contrib_tail nest[0].tail_field // {tail of the contribution list}
/* sec 1034 */
#define adjust_space_factor()   \
do {                            \
  main_s = sf_code(cur_chr);    \
                                \
  if (main_s == 1000)           \
    space_factor = 1000;        \
  else if (main_s < 1000)       \
  {                             \
    if (main_s > 0)             \
      space_factor = main_s;    \
  }                             \
  else if (space_factor < 1000) \
    space_factor = 1000;        \
  else                          \
    space_factor = main_s;      \
} while (0)
/* sec 1035 */
// {the parameter is either |rt_hit| or |false|}
#define pack_lig(a)                                   \
do {                                                  \
  main_p = new_ligature(main_f, cur_l, link(cur_q));  \
                                                      \
  if (lft_hit)                                        \
  {                                                   \
    subtype(main_p) = 2;                              \
    lft_hit = false;                                  \
  }                                                   \
                                                      \
  if (a)                                              \
    if (lig_stack == 0)                               \
    {                                                 \
      incr(subtype(main_p));                          \
      rt_hit = false;                                 \
    }                                                 \
                                                      \
  link(cur_q) = main_p;                               \
  tail = main_p;                                      \
  ligature_present = false;                           \
} while (0)
//
#define wrapup(a)                                         \
do {                                                      \
  if (cur_l < non_char)                                   \
  {                                                       \
    if (link(cur_q) != 0)                                 \
      if (character(tail) == hyphen_char[main_f])         \
        ins_disc = true;                                  \
                                                          \
    if (ligature_present)                                 \
      pack_lig(a);                                        \
                                                          \
    if (ins_disc)                                         \
    {                                                     \
      ins_disc = false;                                   \
                                                          \
      if (mode > 0)                                       \
      {                                                   \
        tail_append(new_disc());                          \
      }                                                   \
    }                                                     \
  }                                                       \
} while (0)
/* sec 1045 */
#define any_mode(a) vmode + a: case hmode + a: case mmode + a
/* sec 1046 */
#define non_math(a) vmode + a: case hmode + a
/* sec 1058 */
#define fil_code     0  // {identifies \.{\\hfil} and \.{\\vfil}}
#define fill_code    1  // {identifies \.{\\hfill} and \.{\\vfill}}
#define ss_code      2  // {identifies \.{\\hss} and \.{\\vss}}
#define fil_neg_code 3  // {identifies \.{\\hfilneg} and \.{\\vfilneg}}
#define skip_code    4  // {identifies \.{\\hskip} and \.{\\vskip}}
#define mskip_code   5  // {identifies \.{\\mskip}}
/* sec 1071 */
#define box_flag        010000000000  // {context code for `\.{\\setbox0}'}
#define global_box_flag 010000100000  // {context code for `\.{\\global\\setbox0}'}
#define ship_out_flag   010000200000  // {context code for `\.{\\shipout}'}
#define leader_flag     010000200001  // {context code for `\.{\\leaders}'}
#define box_code      0 // {|chr_code| for `\.{\\box}'}
#define copy_code     1 // {|chr_code| for `\.{\\copy}'}
#define last_box_code 2 // {|chr_code| for `\.{\\lastbox}'}
#define vsplit_code   3 // {|chr_code| for `\.{\\vsplit}'}
#define vtop_code     4 // {|chr_code| for `\.{\\vtop}'}
/* sec 1151 */
#define fam_in_range ((cur_fam >= 0) && (cur_fam < 16))
/* sec 1178 */
#define above_code     0  // { `\.{\\above}' }
#define over_code      1  // { `\.{\\over}' }
#define atop_code      2  // { `\.{\\atop}' }
#define delimited_code 3  // { `\.{\\abovewithdelims}', etc.}
/* sec 1214 */
#define global (a >= 4)
#define define(p, t, e) \
do                      \
{                       \
  if (global)           \
    geq_define(p, t, e);\
  else                  \
    eq_define(p, t, e); \
}                       \
while (0)
#define word_define(p, w) \
do                        \
{                         \
  if (global)             \
    geq_word_define(p, w);\
  else                    \
    eq_word_define(p, w); \
}                         \
while (0)
/* sec 1222 */
#define char_def_code      0  // {|shorthand_def| for \.{\\chardef}}
#define math_char_def_code 1  // {|shorthand_def| for \.{\\mathchardef}}
#define count_def_code     2  // {|shorthand_def| for \.{\\countdef}}
#define dimen_def_code     3  // {|shorthand_def| for \.{\\dimendef}}
#define skip_def_code      4  // {|shorthand_def| for \.{\\skipdef}}
#define mu_skip_def_code   5  // {|shorthand_def| for \.{\\muskipdef}}
#define toks_def_code      6  // {|shorthand_def| for \.{\\toksdef}}
#define kchar_def_code     7  // {|shorthand_def| for \.{\\kchardef}}
/* sec 1290 */
#define show_code       0 // { \.{\\show} }
#define show_box_code   1 // { \.{\\showbox} }
#define show_the_code   2 // { \.{\\showthe} }
#define show_lists_code 3 // { \.{\\showlists} }
#define show_groups     4 // { \.{\\showgroups} }
#define show_tokens     5 // { \.{\\showtokens} , must be odd! }
#define show_ifs        6 // { \.{\\showifs} }
#define show_mode       7 // { \.{\\showmode} }
/* sec 1306 */
#define undump(va, vb, vc)        \
do {                              \
  undump_int(x);                  \
                                  \
  if ((x < (va)) || (x > (vb)))   \
    goto bad_fmt;                 \
  else                            \
    vc = x;                       \
} while (0)


#define dump_things(base, len)    aptex_dump_put (fmt_file, &(base), sizeof(base) * (int) (len))
#define undump_things(base, len)  aptex_dump_get (fmt_file, &(base), sizeof(base) * (int) (len))
#define generic_dump(x)           aptex_dump_put (fmt_file, &(x),    sizeof(x)) //dump_things(x, 1)
#define generic_undump(x)         aptex_dump_get (fmt_file, &(x),    sizeof(x)) //undump_things(x, 1)

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
/* sec 1342 */
#define write_node_size 2 // {number of words in a write/whatsit node}
#define open_node_size  3 // {number of words in an open/whatsit node}
#define open_node       0 // {|subtype| in whatsits that represent files to \.{\\openout}}
#define write_node      1 // {|subtype| in whatsits that represent things to \.{\\write}}
#define close_node      2 // {|subtype| in whatsits that represent streams to \.{\\closeout}}
#define special_node    3 // {|subtype| in whatsits that represent \.{\\special} things}
#define language_node   4 // {|subtype| in whatsits that change the current language}
#define what_lang(s)    link(s + 1)     // {language number, in the range |0..255|}
#define what_lhm(s)     type(s + 1)     // {minimum left fragment, in the range |1..63|}
#define what_rhm(s)     subtype(s + 1)  // {minimum right fragment, in the range |1..63|}
#define write_tokens(s) link(s + 1)     // {reference count of token list to write}
#define write_stream(s) info(s + 1)     // {stream number (0 to 17)}
#define open_name(s)    link(s + 1)     // {string number of file name to open}
#define open_area(s)    info(s + 2)     // {string number of file area for |open_name|}
#define open_ext(s)     link(s + 2)     // {string number of file extension for |open_name|}

/* sec 1344 */
#define immediate_code    4 // {command modifier for \.{\\immediate}}
#define set_language_code 5 // {command modifier for \.{\\setlanguage}}
#define pdf_save_pos_node 6
#define reset_timer_code  7
#define set_random_seed_code 8
/* sec 1371 */
#define end_write_token (cs_token_flag + end_write)
// macros of pTeX
#define find_effective_tail_epTeX()                               \
do {                                                              \
  tx = tail;                                                      \
                                                                  \
  if (!is_char_node(tx))                                          \
  {                                                               \
    if (type(tx) == disp_node)                                    \
      tx = prev_node;                                             \
  }                                                               \
                                                                  \
  if (!is_char_node(tx))                                          \
  {                                                               \
    if ((type(tx) == disp_node) ||                                \
      ((type(tx) == math_node) && (subtype(tx) == end_M_code)))   \
    {                                                             \
      r = head;                                                   \
      q = link(head);                                             \
                                                                  \
      while (q != tx)                                             \
      {                                                           \
        if (is_char_node(q))                                      \
          r = q;                                                  \
        else if ((type(q) != disp_node) &&                        \
          ((type(q) != math_node) || (subtype(q) != end_M_code))) \
          r = q;                                                  \
                                                                  \
        q = link(q);                                              \
      }                                                           \
                                                                  \
      tx = r;                                                     \
    }                                                             \
  }                                                               \
} while (0)
//#
#define find_effective_tail() find_effective_tail_epTeX()
#define find_last_char()                            \
do {                                                \
  if (font_dir[font(tx)] != dir_default)            \
    cur_val = KANJI(info(link(tx))) % max_cjk_val;  \
  else                                              \
    cur_val = character(tx);                        \
} while (0)
#define ignore_font_kerning()                                       \
do {                                                                \
  if (((type(tx) == glue_node) && (subtype(tx) == jfm_skip + 1)) || \
    ((type(tx) == penalty_node) && (subtype(tx) == kinsoku_pena)))  \
    tx = last_jchr;                                                 \
  else if ((type(tx) == kern_node) && (subtype(tx) == normal))      \
  {                                                                 \
    r = head; q = link(head);                                       \
    while (q != tx)                                                 \
    {                                                               \
      r = q;                                                        \
      if (is_char_node(q))                                          \
        if (font_dir[font(q)] != dir_default)                       \
          q = link(q);                                              \
      q = link(q);                                                  \
    }                                                               \
    if ((type(r) == penalty_node) && (subtype(r) == kinsoku_pena))  \
      tx = last_jchr;                                               \
    else                                                            \
      tx = r;                                                       \
  }                                                                 \
  if (!is_char_node(tx))                                            \
  {                                                                 \
    if (type(tx) == ligature_node)                                  \
    {                                                               \
      r = lig_ptr(tx);                                              \
      while (link(r) != null)                                       \
        r = link(r);                                                \
      cur_val = character(r);                                       \
    }                                                               \
  }                                                                 \
  else                                                              \
    find_last_char();                                               \
} while (0)
#define current_character_being_worked_on (k - char_base[f])
#define print_lc_hex(a)       \
do {                          \
  l = (a);                    \
                              \
  if (l < 10)                 \
    print_char(l + '0');      \
  else                        \
    print_char(l - 10 + 'a'); \
} while (0)
//
#define goto_main_lig_loop()  \
do {                          \
  bchar = non_char;           \
  cur_r = bchar;              \
  lig_stack = 0;              \
                              \
  if (ligature_present)       \
    pack_lig(rt_hit);         \
                              \
  if (ins_kp == true)         \
  {                           \
    cx = cur_l;               \
    insert_kinsoku_penalty(); \
  }                           \
                              \
  ins_kp = false;             \
  goto main_loop_j;           \
}                             \
while (0)
//
/*
{extract |tx|, drop \.{\\beginM} \.{\\endM} pair and\slash or merge |disp_node| pair}
*/
#define fetch_effective_tail_epTeX(a)           \
do                                              \
{                                               \
  q = head;                                     \
  p = null; r = null; fm = 0; fd = 0;           \
  disp = 0;                                     \
  pdisp = 0;                                    \
                                                \
  do {                                          \
    s = r; r = p;                               \
    p = q; fm = fm / 2; fd = fd / 2;            \
    fd = false;                                 \
                                                \
    if (!is_char_node(q))                       \
      if (type(q) == disc_node)                 \
      {                                         \
        for (m = 1; m <= replace_count(q); m++) \
          p = link(p);                          \
                                                \
        if (p == tx)                            \
        {                                       \
          a;                                    \
        }                                       \
      }                                         \
      else if ((type(q) == math_node) &&        \
        (subtype(q) == begin_M_code))           \
        fm = 2;                                 \
      else if (type(q) == disp_node)            \
      {                                         \
        pdisp = disp;                           \
        disp = disp_dimen(q);                   \
        fd = 2;                                 \
      }                                         \
                                                \
    q = link(p);                                \
  } while (!(q == tx));                         \
                                                \
  q = link(tx); link(p) = q; link(tx) = null;   \
                                                \
  if (q == null)                                \
  {                                             \
    tail = p; gm = 0; gd = 0;                   \
  }                                             \
  else                                          \
  {                                             \
    if (type(q) == math_node)                   \
    {                                           \
      gm = 2;                                   \
                                                \
      if (link(q) == null)                      \
        gd = 0;                                 \
      else if (type(link(q)) == disp_node)      \
        gd = 1;                                 \
      else                                      \
        confusion("tail3");                     \
    }                                           \
    else if (type(q) == disp_node)              \
    {                                           \
      prev_node = p; gd = 2;                    \
                                                \
      if (link(q) == null)                      \
        gm = 0;                                 \
      else if (type(link(q)) == math_node)      \
        gm = 1;                                 \
      else                                      \
        confusion("tail4");                     \
    }                                           \
    else                                        \
      confusion("tail5");                       \
  }                                             \
                                                \
  if (gm == 0) if (fm == 2) confusion("tail1"); \
  else if (fm == 1) confusion("tail2");         \
  if ((fm + fd) == 1) { fm = 0; fd = 0; }       \
  if (gm == 0) fm = 0;                          \
  if (gd == 0) fd = 0;                          \
                                                \
  if (fd > 0)                                   \
  {                                             \
    if (gm == 0)                                \
    {                                           \
      t = q; q = null; link(p) = q; tail = p;   \
    }                                           \
    else if (gm == 1)                           \
    {                                           \
      t = q; q = link(q); link(p) = q; gm = 2;  \
    }                                           \
    else                                        \
    {                                           \
      t = link(q); link(q) = null; tail = q;    \
    }                                           \
                                                \
    if (fd == 1)                                \
    {                                           \
      prev_node = s;                            \
      disp_dimen(r) = disp_dimen(t);            \
    }                                           \
    else                                        \
    {                                           \
      prev_node = r;                            \
      disp_dimen(p) = disp_dimen(t);            \
    }                                           \
                                                \
    prev_disp = pdisp;                          \
    free_node(t, small_node_size);              \
    gd = 0;                                     \
  }                                             \
                                                \
  if (fm > 0)                                   \
  {                                             \
    if (gd == 0)                                \
    {                                           \
      t = q; q = null; link(p) = q; tail = p;   \
    }                                           \
    else if (gd == 1)                           \
    {                                           \
      t = q; q = link(q); link(p) = q;          \
      prev_node = p; link(t) = null;            \
    }                                           \
    else                                        \
    {                                           \
      t = link(q); link(q) = null; tail = q;    \
    }                                           \
                                                \
    if (fm == 1)                                \
    {                                           \
      link(s) = p; link(r) = t; t = r;          \
      prev_node = s;                            \
    }                                           \
    else                                        \
    {                                           \
      link(r) = q; link(p) = t; t = p;          \
                                                \
      if (q == null) tail = r;                  \
      else prev_node = r;                       \
    }                                           \
                                                \
    flush_node_list(t);                         \
  }                                             \
}                                               \
while (0)

#define check_effective_tail(a) find_effective_tail_epTeX()
#define fetch_effective_tail    fetch_effective_tail_epTeX

#define reset_auto_spacing_code   0
#define set_auto_spacing_code     1
#define reset_auto_xspacing_code  2
#define set_auto_xspacing_code    3

#define reset_enable_cjk_token_code 0
#define set_enable_cjk_token_code   1
#define set_force_cjk_token_code    2

#define inhibit_both      0 // {disable to insert space before 2byte-char and after it}
#define inhibit_previous  1 // {disable to insert space before 2byte-char}
#define inhibit_after     2 // {disable to insert space after 2byte-char}
#define inhibit_none      3 // {enable to insert space before/after 2byte-char}
#define inhibit_unused    4 // {unused entry}
#define no_entry          10000
#define new_pos           0
#define cur_pos           1

#define pre_break_penalty_code  1
#define post_break_penalty_code 2
#define kinsoku_unused_code     3

#define dvi_yoko 0
#define dvi_tate 1
#define dvi_dtou 3

#define no_skip     0
#define after_schar 1
#define after_wchar 2

// @<Insert a space around the character |p|@>
#define insert_space_around_char()            \
do {                                          \
  if (font_dir[font(p)] != dir_default)       \
  {                                           \
    KANJI(cx) = info(link(p)) % max_cjk_val;  \
                                              \
    if (insert_skip == after_schar)           \
      insert_ascii_kanji_spacing();           \
                                              \
    p = link(p);                              \
    insert_skip = after_wchar;                \
  }                                           \
  else                                        \
  {                                           \
    ax = character(p);                        \
                                              \
    if (insert_skip == after_wchar)           \
      insert_kanji_ascii_spacing();           \
                                              \
    if (auto_xsp_code(ax) >= 2)               \
      insert_skip = after_schar;              \
    else                                      \
      insert_skip = no_skip;                  \
  }                                           \
} while (0)
// @<Insert hbox surround spacing@>
#define insert_hbox_surround_spacing()    \
do {                                      \
  find_first_char = true;                 \
  first_char = null;                      \
  last_char = null;                       \
                                          \
  if (shift_amount(p) == 0)               \
  {                                       \
    if (check_box(list_ptr(p)))           \
    {                                     \
      if (first_char != null)             \
        insert_space_before_first_char(); \
                                          \
      if (last_char != null)              \
        insert_space_after_last_char();   \
      else                                \
        insert_skip = no_skip;            \
    }                                     \
    else                                  \
      insert_skip = no_skip;              \
  }                                       \
  else                                    \
    insert_skip = no_skip;                \
} while (0)
// @<Insert a space before the |first_char|@>
#define insert_space_before_first_char()              \
do {                                                  \
  if (type(first_char) == math_node)                  \
  {                                                   \
    ax = '0';                                         \
                                                      \
    if (insert_skip == after_wchar)                   \
      insert_kanji_ascii_spacing();                   \
  }                                                   \
  else if (font_dir[font(first_char)] != dir_default) \
  {                                                   \
    KANJI(cx) = info(link(first_char)) % max_cjk_val; \
                                                      \
    if (insert_skip == after_schar)                   \
      insert_ascii_kanji_spacing();                   \
    else if (insert_skip == after_wchar)              \
      insert_kanji_kanji_spacing();                   \
  }                                                   \
  else                                                \
  {                                                   \
    ax = character(first_char);                       \
                                                      \
    if (insert_skip == after_wchar)                   \
      insert_kanji_ascii_spacing();                   \
  }                                                   \
} while (0)
// @<Insert a space after the |last_char|@>
#define insert_space_after_last_char()                \
do {                                                  \
  if (type(last_char) == math_node)                   \
  {                                                   \
    ax = '0';                                         \
                                                      \
    if (auto_xsp_code(ax) >= 2)                       \
      insert_skip = after_schar;                      \
    else                                              \
      insert_skip = no_skip;                          \
  }                                                   \
  else if (font_dir[font(last_char)] != dir_default)  \
  {                                                   \
    insert_skip = after_wchar;                        \
    KANJI(cx) = info(link(last_char)) % max_cjk_val;  \
                                                      \
    if (is_char_node(link(p)) &&                      \
      (font_dir[font(link(p))] != dir_default))       \
    {                                                 \
      append_kanji_kanji_spacing();                   \
      p = link(p);                                    \
    }                                                 \
  }                                                   \
  else                                                \
  {                                                   \
    ax = character(last_char);                        \
                                                      \
    if (auto_xsp_code(ax) >= 2)                       \
      insert_skip = after_schar;                      \
    else                                              \
      insert_skip = no_skip;                          \
  }                                                   \
} while (0)
// @<Insert math surround spacing@>
#define insert_math_surround_spacing()                        \
do {                                                          \
  if ((subtype(p) == before) && (insert_skip == after_wchar)) \
  {                                                           \
    ax = '0';                                                 \
    insert_kanji_ascii_spacing();                             \
    insert_skip = no_skip;                                    \
  }                                                           \
  else if (subtype(p) == after)                               \
  {                                                           \
    ax = '0';                                                 \
                                                              \
    if (auto_xsp_code(ax) >= 2)                               \
      insert_skip = after_schar;                              \
    else                                                      \
      insert_skip = no_skip;                                  \
  }                                                           \
  else                                                        \
    insert_skip = no_skip;                                    \
} while (0)
// @<Insert ligature surround spacing@>
#define insert_ligature_surround_spacing()  \
do {                                        \
  t = lig_ptr(p);                           \
                                            \
  if (is_char_node(t))                      \
  {                                         \
    ax = character(t);                      \
                                            \
    if (insert_skip == after_wchar)         \
      insert_kanji_ascii_spacing();         \
                                            \
    while (link(t) != null)                 \
      t = link(t);                          \
                                            \
    if (is_char_node(t))                    \
    {                                       \
      ax = character(t);                    \
                                            \
      if (auto_xsp_code(ax) >= 2)           \
        insert_skip = after_schar;          \
      else                                  \
        insert_skip = no_skip;              \
    }                                       \
  }                                         \
} while (0)
// @<Insert penalty or displace surround spacing@>
#define insert_penalty_or_displace_surround_spacing() \
do {                                                  \
  if (is_char_node(link(p)))                          \
  {                                                   \
    q = p;                                            \
    p = link(p);                                      \
                                                      \
    if (font_dir[font(p)] != dir_default)             \
    {                                                 \
      KANJI(cx) = info(link(p)) % max_cjk_val;        \
                                                      \
      if (insert_skip == after_schar)                 \
        insert_ascii_kanji_spacing();                 \
      else if (insert_skip == after_wchar)            \
        insert_kanji_kanji_spacing();                 \
                                                      \
      p = link(p);                                    \
      insert_skip = after_wchar;                      \
    }                                                 \
    else                                              \
    {                                                 \
      ax = character(p);                              \
                                                      \
      if (insert_skip == after_wchar)                 \
        insert_kanji_ascii_spacing();                 \
                                                      \
      if (auto_xsp_code(ax) >= 2)                     \
        insert_skip = after_schar;                    \
      else                                            \
        insert_skip = no_skip;                        \
    }                                                 \
  }                                                   \
} while (0)
// @<Insert ASCII-KANJI spacing@>
#define insert_ascii_kanji_spacing()                \
do {                                                \
  {                                                 \
    x = get_inhibit_pos(cx, cur_pos);               \
                                                    \
    if (x != no_entry)                              \
      if ((inhibit_xsp_type(x) == inhibit_both) ||  \
        (inhibit_xsp_type(x) == inhibit_previous))  \
        do_ins = false;                             \
      else                                          \
        do_ins = true;                              \
    else                                            \
      do_ins = true;                                \
  }                                                 \
                                                    \
  if (do_ins)                                       \
  {                                                 \
    z = new_glue(s);                                \
    subtype(z) = xkanji_skip_code + 1;              \
    link(z) = link(q);                              \
    link(q) = z;                                    \
    q = z;                                          \
  }                                                 \
} while (0)
// @<Insert KANJI-ASCII spacing@>
#define insert_kanji_ascii_spacing()                \
do {                                                \
  if ((auto_xsp_code(ax) % 2) == 1)                 \
  {                                                 \
    x = get_inhibit_pos(cx, cur_pos);               \
                                                    \
    if (x != no_entry)                              \
      if ((inhibit_xsp_type(x) == inhibit_both) ||  \
        (inhibit_xsp_type(x) == inhibit_after))     \
        do_ins = false;                             \
      else                                          \
        do_ins = true;                              \
    else                                            \
      do_ins = true;                                \
  }                                                 \
  else                                              \
    do_ins = false;                                 \
                                                    \
  if (do_ins)                                       \
  {                                                 \
    z = new_glue(s);                                \
    subtype(z) = xkanji_skip_code + 1;              \
    link(z) = link(q);                              \
    link(q) = z;                                    \
    q = z;                                          \
  }                                                 \
} while (0)
// @<Insert KANJI-KANJI spacing@>
#define insert_kanji_kanji_spacing()  \
do {                                  \
  z = new_glue(u);                    \
  subtype(z) = kanji_skip_code + 1;   \
  link(z) = link(q);                  \
  link(q) = z;                        \
  q = z;                              \
} while (0)
// @<Append KANJI-KANJI spacing@>
#define append_kanji_kanji_spacing()  \
do {                                  \
  z = new_glue(u);                    \
  subtype(z) = kanji_skip_code + 1;   \
  link(z) = link(p);                  \
  link(p) = z;                        \
  p = link(z);                        \
  q = z;                              \
} while (0)
// @<Make |jchr_widow_penalty| node@>
#define make_jchr_widow_penalty_node()                        \
do {                                                          \
  q = v;                                                      \
  p = link(v);                                                \
                                                              \
  if (is_char_node(v) && (font_dir[font(v)] != dir_default))  \
  {                                                           \
    q = p;                                                    \
    p = link(p);                                              \
  }                                                           \
                                                              \
  t = q;                                                      \
  s = null;                                                   \
  seek_list_and_make();                                       \
                                                              \
  if (s != null)                                              \
  {                                                           \
    s = link(t);                                              \
                                                              \
    if (!is_char_node(s) && (type(s) == penalty_node))        \
      penalty(s) = penalty(s) + jchr_widow_penalty;           \
    else if (jchr_widow_penalty != 0)                         \
    {                                                         \
      s = new_penalty(jchr_widow_penalty);                    \
      subtype(s) = widow_pena;                                \
      link(s) = link(t);                                      \
      link(t) = s;                                            \
      t = link(s);                                            \
                                                              \
      while (!is_char_node(t))                                \
      {                                                       \
        if ((type(t) == glue_node) || (type(t) == kern_node)) \
          goto exit;                                          \
                                                              \
        t = link(t);                                          \
      }                                                       \
                                                              \
      z = new_glue(u);                                        \
      subtype(z) = kanji_skip_code + 1;                       \
      link(z) = link(s);                                      \
      link(s) = z;                                            \
    }                                                         \
  }                                                           \
} while (0)
// @<Seek list and make |t| pointing widow penalty position@>
#define seek_list_and_make()                              \
do {                                                      \
  k = 0;                                                  \
  while (p != null)                                       \
  {                                                       \
    if (is_char_node(p))                                  \
    {                                                     \
      if (font_dir[font(p)] != dir_default)               \
      {                                                   \
        KANJI(cx) = info(link(p)) % max_cjk_val;          \
        i = info(link(p)) / max_cjk_val;                  \
        k = 0;                                            \
                                                          \
        if ((i == kanji) || (i == kana) || (i == hangul)) \
        {                                                 \
          t = q;                                          \
          s = p;                                          \
        }                                                 \
                                                          \
        p = link(p);                                      \
        q = p;                                            \
      }                                                   \
      else                                                \
      {                                                   \
        k = k + 1;                                        \
                                                          \
        if (k > 1)                                        \
        {                                                 \
          q = p;                                          \
          s = null;                                       \
        }                                                 \
      }                                                   \
    }                                                     \
    else                                                  \
    {                                                     \
      switch (type(p))                                    \
      {                                                   \
        case penalty_node:                                \
        case mark_node:                                   \
        case adjust_node:                                 \
        case whatsit_node:                                \
        case glue_node:                                   \
        case kern_node:                                   \
        case math_node:                                   \
        case disp_node:                                   \
          do_nothing();                                   \
          break;                                          \
                                                          \
        default:                                          \
          {                                               \
            q = p;                                        \
            s = null;                                     \
          }                                               \
          break;                                          \
      }                                                   \
    }                                                     \
                                                          \
    p = link(p);                                          \
  }                                                       \
} while (0)
// @<Insert kinsoku penalty@>
#define insert_kinsoku_penalty()                                \
do {                                                            \
  kp = get_kinsoku_pos(cx, cur_pos);                            \
                                                                \
  if (kp != no_entry) if (kinsoku_penalty(kp) != 0)             \
  {                                                             \
    if (kinsoku_type(kp) == pre_break_penalty_code)             \
    {                                                           \
      if (!is_char_node(cur_q) && (type(cur_q) == penalty_node))\
        penalty(cur_q) = penalty(cur_q) + kinsoku_penalty(kp);  \
      else                                                      \
      {                                                         \
        main_p = link(cur_q);                                   \
        link(cur_q) = new_penalty(kinsoku_penalty(kp));         \
        subtype(link(cur_q)) = kinsoku_pena;                    \
        link(link(cur_q)) = main_p;                             \
      }                                                         \
    }                                                           \
    else if (kinsoku_type(kp) == post_break_penalty_code)       \
    {                                                           \
      tail_append(new_penalty(kinsoku_penalty(kp)));            \
      subtype(tail) = kinsoku_pena;                             \
    }                                                           \
  }                                                             \
} while (0)
// @<Insert |pre_break_penalty| of |cur_chr|@>
#define insert_pre_break_penalty()                              \
do {                                                            \
  kp = get_kinsoku_pos(cur_chr, cur_pos);                       \
                                                                \
  if (kp != no_entry) if (kinsoku_penalty(kp) != 0)             \
  {                                                             \
    if (kinsoku_type(kp) == pre_break_penalty_code)             \
      if (!is_char_node(tail) && (type(tail) == penalty_node))  \
        penalty(tail) = penalty(tail) + kinsoku_penalty(kp);    \
      else                                                      \
      {                                                         \
        tail_append(new_penalty(kinsoku_penalty(kp)));          \
        subtype(tail) = kinsoku_pena;                           \
      }                                                         \
  }                                                             \
} while (0)
// @<Insert |post_break_penalty|@>
#define insert_post_break_penalty()                           \
do {                                                          \
  kp = get_kinsoku_pos(cx, cur_pos);                          \
                                                              \
  if (kp != no_entry) if (kinsoku_penalty(kp) != 0)           \
  {                                                           \
    if (kinsoku_type(kp) == post_break_penalty_code)          \
    {                                                         \
      tail_append(new_penalty(kinsoku_penalty(kp)));          \
      subtype(tail) = kinsoku_pena;                           \
    }                                                         \
  }                                                           \
} while (0)

/*
  @<Append KANJI-character |cur_chr|
  to the current hlist in the current font; |goto reswitch| when
  a non-character has been fetched@>
*/

#define append_kanji_to_hlist()                               \
do {                                                          \
  if (is_char_node(tail))                                     \
  {                                                           \
    if (!((last_jchr != null) && (link(last_jchr) == tail)))  \
    {                                                         \
      cx = character(tail);                                   \
      insert_post_break_penalty();                            \
    }                                                         \
  }                                                     \
  else if (type(tail) == ligature_node)                 \
  {                                                     \
    cx = character(lig_char(tail));                     \
    insert_post_break_penalty();                        \
  }                                                     \
                                                        \
  if (direction == dir_tate)                            \
  {                                                     \
    if (font_dir[main_f] == dir_tate)                   \
      disp = 0;                                         \
    else if (font_dir[main_f] == dir_yoko)              \
      disp = t_baseline_shift - y_baseline_shift;       \
    else                                                \
      disp = t_baseline_shift;                          \
                                                        \
    main_f = cur_tfont;                                 \
  }                                                     \
  else                                                  \
  {                                                     \
    if (font_dir[main_f] == dir_yoko)                   \
      disp = 0;                                         \
    else if (font_dir[main_f] == dir_tate)              \
      disp = y_baseline_shift - t_baseline_shift;       \
    else                                                \
      disp = y_baseline_shift;                          \
                                                        \
    main_f = cur_jfont;                                 \
  }                                                     \
                                                        \
  append_disp_node_at_end();                            \
  ins_kp = false;                                       \
  ligature_present = false;                             \
  cur_l = get_jfm_pos(KANJI(cur_chr), main_f);          \
  main_i = char_info(main_f, 0);                        \
  goto main_loop_j_3;                                   \
                                                        \
main_loop_j_1:                                          \
  space_factor = 1000;                                  \
                                                        \
  if (main_f != null_font)                              \
  {                                                     \
    if (!disp_called)                                   \
    {                                                   \
      prev_node = tail;                                 \
      tail_append(get_node(small_node_size));           \
      type(tail) = disp_node;                           \
      disp_dimen(tail) = 0;                             \
      disp_called = true;                               \
    }                                                   \
    fast_get_avail(main_p);                             \
    font(main_p) = main_f;                              \
    character(main_p) = cur_l;                          \
    link(tail) = main_p;                                \
    tail = main_p;                                      \
    last_jchr = tail;                                   \
    fast_get_avail(main_p);                             \
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))      \
      info(main_p) =                                    \
          KANJI(cur_chr) + cur_cmd * max_cjk_val;       \
    else if (cur_cmd == not_cjk)                        \
      info(main_p) =                                    \
          KANJI(cur_chr) + other_kchar * max_cjk_val;   \
    else                                                \
       info(main_p) = KANJI(cur_chr) +                      \
       kcat_code(kcatcodekey(KANJI(cur_chr))) * max_cjk_val;\
    link(tail) = main_p;                                \
    tail = main_p;                                      \
    cx = cur_chr;                                       \
    insert_kinsoku_penalty();                           \
  }                                                     \
                                                        \
  ins_kp = false;                                       \
                                                        \
again_2:                                                \
  get_next();                                           \
  main_i = char_info(main_f, cur_l);                    \
                                                        \
  switch (cur_cmd)                                      \
  {                                                     \
    case kanji:                                         \
    case kana:                                          \
    case other_kchar:                                   \
    case hangul:                                        \
      {                                                 \
        cur_l = get_jfm_pos(KANJI(cur_chr), main_f);    \
        goto main_loop_j_3;                             \
      }                                                 \
      break;                                            \
                                                        \
    case letter:                                        \
    case other_char:                                    \
      {                                                 \
        ins_kp = true;                                  \
        cur_l = 0;                                      \
        goto main_loop_j_3;                             \
      }                                                 \
      break;                                            \
  }                                                     \
                                                        \
  x_token();                                            \
                                                        \
  switch (cur_cmd)                                      \
  {                                                     \
    case kanji:                                         \
    case kana:                                          \
    case other_kchar:                                   \
    case hangul:                                        \
      cur_l = get_jfm_pos(KANJI(cur_chr), main_f);      \
      break;                                            \
                                                        \
    case letter:                                        \
    case other_char:                                    \
      {                                                 \
        ins_kp = true;                                  \
        cur_l = 0;                                      \
      }                                                 \
      break;                                            \
                                                        \
    case char_given:                                    \
      {                                                 \
        if (check_echar_range(cur_chr))                 \
        {                                               \
          ins_kp = true;                                \
          cur_l = 0;                                    \
        }                                               \
        else                                            \
          cur_l = get_jfm_pos(KANJI(cur_chr), main_f);  \
        cur_cmd = kcat_code(kcatcodekey(cur_chr));      \
      }                                                 \
      break;                                            \
                                                        \
    case char_num:                                      \
      {                                                 \
        scan_char_num();                                \
        cur_chr = cur_val;                              \
                                                        \
        if (check_echar_range(cur_chr))                 \
        {                                               \
          ins_kp = true;                                \
          cur_l = 0;                                    \
        }                                               \
        else                                            \
          cur_l = get_jfm_pos(KANJI(cur_chr), main_f);  \
        cur_cmd = kcat_code(kcatcodekey(cur_chr));      \
      }                                                 \
      break;                                            \
                                                        \
    case kchar_given:                                   \
      {                                                 \
        cur_l = (get_jfm_pos(KANJI(cur_chr), main_f));  \
        cur_cmd = kcat_code(kcatcodekey(cur_chr));      \
      }                                                 \
      break;                                            \
                                                        \
    case kchar_num:                                     \
      {                                                 \
        scan_char_num();                                \
        cur_chr = cur_val;                              \
        cur_l = (get_jfm_pos(KANJI(cur_chr), main_f));  \
        cur_cmd = kcat_code(kcatcodekey(cur_chr));      \
      }                                                 \
      break;                                            \
                                                        \
    case inhibit_glue:                                  \
      {                                                 \
        inhibit_glue_flag = (cur_chr == 0);             \
        goto again_2;                                   \
      }                                                 \
      break;                                            \
                                                        \
    default:                                            \
      {                                                 \
        ins_kp = max_halfword;                          \
        cur_l = -1;                                     \
        cur_r = non_char;                               \
        lig_stack = null;                               \
      }                                                 \
      break;                                            \
  }                                                     \
                                                        \
main_loop_j_3:                                          \
  if (ins_kp == true)                                   \
    insert_pre_break_penalty();                         \
                                                        \
  if (main_f != null_font)                              \
    look_ahead_for_glue_or_kerning();                   \
  else                                                  \
    inhibit_glue_flag = false;                          \
                                                        \
  if (ins_kp == false)                                  \
    goto main_loop_j_1;                                 \
  else if (ins_kp == true)                              \
  {                                                     \
    ins_kp = false;                                     \
    goto main_loop;                                     \
  }                                                     \
  else                                                  \
    goto reswitch;                                      \
} while (0)
// @<Append |disp_node| at begin of displace area@>
#define append_disp_node_at_begin()                     \
do {                                                    \
  if (!is_char_node(tail) && (type(tail) == disp_node)) \
  {                                                     \
    if (prev_disp == disp)                              \
    {                                                   \
      free_node(tail, small_node_size);                 \
      tail = prev_node;                                 \
      link(tail) = null;                                \
    }                                                   \
    else                                                \
      disp_dimen(tail) = disp;                          \
  }                                                     \
  else if (disp != 0 || !disp_called)                   \
  {                                                     \
    prev_node = tail;                                   \
    tail_append(get_node(small_node_size));             \
    type(tail) = disp_node;                             \
    disp_dimen(tail) = disp;                            \
    prev_disp = disp;                                   \
    disp_called = true;                                 \
  }                                                     \
} while (0)
// @<Append |disp_node| at end of displace area@>
#define append_disp_node_at_end()                         \
do {                                                      \
  if (disp != 0)                                          \
  {                                                       \
    if (!is_char_node(tail) && (type(tail) == disp_node)) \
    {                                                     \
      disp_dimen(tail) = 0;                               \
    }                                                     \
    else                                                  \
    {                                                     \
      prev_node = tail;                                   \
      tail_append(get_node(small_node_size));             \
      type(tail) = disp_node;                             \
      disp_dimen(tail) = 0;                               \
      prev_disp = disp;                                   \
      disp_called = true;                                 \
    }                                                     \
  }                                                       \
} while (0)
// @<Look ahead for glue or kerning@>
#define look_ahead_for_glue_or_kerning()                          \
do {                                                              \
  cur_q = tail;                                                   \
                                                                  \
  if (inhibit_glue_flag != true)                                  \
  {                                                               \
    /*{ prints("IF"); print_int(cur_l); }*/                       \
    if (cur_l < 0) cur_l = 0; else inhibit_glue_flag = false;     \
    if ((tail == link(head)) && (!is_char_node(tail))             \
      && (type(tail) == disp_node))                               \
      goto skip_loop;                                             \
    else                                                          \
    {                                                             \
      if (char_tag(main_i) == gk_tag)                             \
      {                                                           \
        main_k = glue_kern_start(main_f, main_i);                 \
        main_j = font_info[main_k].qqqq;                          \
                                                                  \
        if (skip_byte(main_j)>stop_flag)                          \
        {                                                         \
          main_k = glue_kern_restart(main_f, main_j);             \
          main_j = font_info[main_k].qqqq;                        \
        }                                                         \
                                                                  \
        while (true) {                                            \
          if (next_char(main_j) == cur_l)                         \
            if (skip_byte(main_j) <= stop_flag)                   \
          {                                                       \
            if (op_byte(main_j) < kern_flag)                      \
            {                                                     \
              gp = font_glue[main_f];                             \
              cur_r = op_byte(main_j) * 256 + rem_byte(main_j);   \
                                                                  \
              if (gp != null)                                     \
              {                                                   \
                while ((type(gp) != cur_r) && (link(gp) != null)) \
                {                                                 \
                  gp = link(gp);                                  \
                }                                                 \
                                                                  \
                gq = glue_ptr(gp);                                \
              }                                                   \
              else                                                \
              {                                                   \
                gp = get_node(small_node_size);                   \
                font_glue[main_f] = gp;                           \
                gq = null;                                        \
              }                                                   \
                                                                  \
              if (gq == null)                                     \
              {                                                   \
                type(gp) = cur_r;                                 \
                gq = new_spec(zero_glue);                         \
                glue_ptr(gp) = gq;                                \
                main_k = exten_base[main_f] + (cur_r * 3);        \
                width(gq) = font_info[main_k].cint;               \
                stretch(gq) = font_info[main_k + 1].cint;         \
                shrink(gq) = font_info[main_k + 2].cint;          \
                add_glue_ref(gq);                                 \
                link(gp) = get_node(small_node_size);             \
                gp = link(gp);                                    \
                glue_ptr(gp) = null;                              \
                link(gp) = null;                                  \
              }                                                   \
                                                                  \
              tail_append(new_glue(gq));                          \
              subtype(tail) = jfm_skip + 1;                       \
              goto skip_loop;                                     \
            }                                                     \
            else                                                  \
            {                                                     \
              tail_append(new_kern(char_kern(main_f, main_j)));   \
              goto skip_loop;                                     \
            }                                                     \
          }                                                       \
                                                                  \
          if (skip_byte(main_j) >= stop_flag) goto skip_loop;     \
          main_k = main_k + (skip_byte(main_j)) + 1;              \
          main_j = font_info[main_k].qqqq;                        \
        }                                                         \
      }                                                           \
    }                                                             \
  }                                                               \
  else                                                            \
  {                                                               \
    /*{ prints("IF"); print_int(cur_l); }*/                       \
    if (cur_l < 0) cur_l = 0; else inhibit_glue_flag = false;     \
  }                                                               \
skip_loop: do_nothing();                                          \
} while (0)

// eTeX
#define reversed          1 // {subtype for an |hlist_node| whose hlist has been reversed}
#define dlist             2 // {subtype for an |hlist_node| from display math mode}
#define box_lr(a)         (subtype(a) / 16)  // {direction mode of a box}
#define set_box_lr(a, b)  subtype(a) = box_dir(a) + dir_max + 16 * b
//#
#define left_to_right 0
#define right_to_left 1
#define reflected     (1 - cur_dir) // {the opposite of |cur_dir|}

#define round_glue()                              \
do{                                               \
  g = glue_ptr(p);                                \
  rule_wd = width(g) - cur_g;                     \
                                                  \
  if (g_sign != normal)                           \
  {                                               \
    if (g_sign == stretching)                     \
    {                                             \
      if (stretch_order(g) == g_order)            \
      {                                           \
        cur_glue = cur_glue + stretch(g);         \
        vet_glue(glue_set(this_box) * cur_glue);  \
        cur_g = round(glue_temp);                 \
      }                                           \
    }                                             \
    else if (shrink_order(g) == g_order)          \
    {                                             \
      cur_glue = cur_glue - shrink(g);            \
      vet_glue(glue_set(this_box) * cur_glue);    \
      cur_g = round(glue_temp);                   \
    }                                             \
  }                                               \
                                                  \
  rule_wd = rule_wd + cur_g;                      \
} while (0)

#define handle_a_glue_node()                                      \
do {                                                              \
  if (((g_sign == stretching) && (stretch_order(g) == g_order)) ||\
    ((g_sign == shrinking) && (shrink_order(g) == g_order)))      \
  {                                                               \
    fast_delete_glue_ref(g);                                      \
                                                                  \
    if (subtype(p) < a_leaders)                                   \
    {                                                             \
      type(p) = kern_node;                                        \
      width(p) = rule_wd;                                         \
    }                                                             \
    else                                                          \
    {                                                             \
      g = get_node(glue_spec_size);                               \
      stretch_order(g) = filll + 1;                               \
      shrink_order(g) = filll + 1;                                \
      width(g) = rule_wd;                                         \
      stretch(g) = 0;                                             \
      shrink(g) = 0;                                              \
      glue_ptr(p) = g;                                            \
    }                                                             \
  }                                                               \
} while (0)

#define report_LR_problems()              \
do {                                      \
  print_ln();                             \
  print_nl("\\endL or \\endR problem ("); \
  print_int(LR_problems / 10000);         \
  prints(" missing, ");                   \
  print_int(LR_problems % 10000);         \
  prints(" extra");                       \
  LR_problems = 0;                        \
} while (0)

#define put_LR(a)         \
do {                      \
  temp_ptr = get_avail(); \
  info(temp_ptr) = a;     \
  link(temp_ptr) = LR_ptr;\
  LR_ptr = temp_ptr;      \
} while (0)

#define push_LR(a) put_LR(end_LR_type(a))

#define pop_LR()          \
do {                      \
  temp_ptr = LR_ptr;      \
  LR_ptr = link(temp_ptr);\
  free_avail(temp_ptr);   \
} while (0)

#define LR_dir(a) (subtype(a) / R_code)

#define edge_node       style_node      // {a |style_node| does not occur in hlists}
#define edge_node_size  style_node_size // {number of words in an edge node}
#define edge_dist(a)    depth(a)        // {new |left_edge| position relative to |cur_h| (after |width| has been taken into account)}

#define adjust_the_LR_stack_p()           \
do {                                      \
  if (end_LR(q))                          \
  {                                       \
    if (LR_ptr != null)                   \
      if (info(LR_ptr) == end_LR_type(q)) \
        pop_LR();                         \
  }                                       \
  else                                    \
    push_LR(q);                           \
} while (0)

#define adjust_the_LR_stack_j()           \
do {                                      \
  if (end_LR(p))                          \
    if (info(LR_ptr) != end_LR_type(p))   \
    {                                     \
      type(p) = kern_node;                \
      incr(LR_problems);                  \
    }                                     \
    else                                  \
    {                                     \
      pop_LR();                           \
                                          \
      if (n > min_halfword)               \
      {                                   \
        decr(n);                          \
        decr(subtype(p));                 \
      }                                   \
      else                                \
      {                                   \
        if (m > min_halfword)             \
          decr(m);                        \
        else                              \
          goto found;                     \
                                          \
        type(p) = kern_node;              \
      }                                   \
    }                                     \
  else                                    \
  {                                       \
    push_LR(p);                           \
                                          \
    if ((n > min_halfword) ||             \
      (LR_dir(p) != cur_dir))             \
    {                                     \
      incr(n);                            \
      incr(subtype(p));                   \
    }                                     \
    else                                  \
    {                                     \
      type(p) = kern_node;                \
      incr(m);                            \
    }                                     \
  }                                       \
} while (0)

#define set_value_of_x()  \
do {                                \
  if (LR_save == null)              \
    x = 0;                          \
  else if (info(LR_save) >= R_code) \
    x = -1;                         \
  else                              \
    x = 1;                          \
} while (0)

#define TeXXeT_state eTeX_state(TeXXeT_code)
#define TeXXeT_en    (TeXXeT_state > 0)

#define segment_node      style_node
#define segment_node_size style_node_size // {number of words in a segment node}
#define segment_first(a)  info(a+2)       // {first node of the segment}
#define segment_last(a)   link(a+2)       // {last node of the segment}

#define cancel_glue(a,b,c,d,e)                \
do {                                          \
  j = new_skip_param(a);                      \
  link(b) = j; link(j) = c;                   \
  j = glue_ptr(d);                            \
  stretch_order(temp_ptr) = stretch_order(j); \
  shrink_order(temp_ptr) = shrink_order(j);   \
  width(temp_ptr) = e - width(j);             \
  stretch(temp_ptr) = -stretch(j);            \
  shrink(temp_ptr) = -shrink(j);              \
} while (0)

#define expr_none   0 // {\.( seen, or \.( $\langle\it expr\rangle$ \.) seen}
#define expr_add    1 // {\.( $\langle\it expr\rangle$ \.+ seen}
#define expr_sub    2 // {\.( $\langle\it expr\rangle$ \.- seen}
#define expr_mult   3 // {$\langle\it term\rangle$ \.* seen}
#define expr_div    4 // {$\langle\it term\rangle$ \./ seen}
#define expr_scale  5 // {$\langle\it term\rangle$ \.* $\langle\it factor\rangle$ \./ seen}

#define expr_node_size  4               // {number of words in stack entry for subexpressions}
#define expr_e_field(a) mem[a + 1].cint // {saved expression so far}
#define expr_t_field(a) mem[a + 2].cint // {saved term so far}
#define expr_n_field(a) mem[a + 3].cint // {saved numerator}

#define num_error(a)  \
do {                  \
  arith_error = true; \
  a = 0;              \
} while (0)

#define glue_error(a)     \
do {                      \
  arith_error = true;     \
  delete_glue_ref(a);     \
  a = new_spec(zero_glue);\
} while (0)

#define normalize_glue(a)     \
do {                          \
  if (stretch(a) == 0)        \
    stretch_order(a) = normal;\
                              \
  if (shrink(a) == 0)         \
    shrink_order(a) = normal; \
} while (0)

#define expr_add_sub(a,b,c) add_or_sub(a, b, c, r==expr_sub)
#define expr_a(a,b)         expr_add_sub(a,b, max_dimen)
#define expr_m(a)           a = nx_plus_y(a, f, 0)
#define expr_d(a)           a = quotient(a, f)
#define expr_s(a)           a = fract(a, n, f, max_dimen)

// for sparse array
#define box_val   4
#define mark_val  6
//
#define dimen_val_limit 0x20
#define mu_val_limit    0x40
#define box_val_limit   0x50
#define tok_val_limit   0x60
//
#define index_node_size 9
#define sa_index        type
#define sa_used         subtype
//
#define sa_mark sa_root[mark_val]
//
#define if_cur_ptr_is_null_then_return_or_goto(a) \
do {                    \
  if (cur_ptr == null)  \
  {                     \
    if (w)              \
      goto a;           \
    else                \
      return;           \
  }                     \
} while (0)
//
#define hex_dig1(a) (a / 4096)
#define hex_dig2(a) (a / 256) % 16
#define hex_dig3(a) (a / 16) % 16
#define hex_dig4(a) (a % 16)
//
#define get_sa_ptr()                \
do {                                \
  if odd(i)                         \
    cur_ptr = link(q + (i / 2) + 1);\
  else                              \
    cur_ptr = info(q + (i / 2) + 1);\
} while (0)

#define put_sa_ptr(a)           \
do {                            \
  if odd(i)                     \
    link(q + (i / 2) + 1) = a;  \
  else                          \
    info(q + (i / 2) + 1) = a;  \
} while (0)

#define add_sa_ptr()    \
do {                    \
  put_sa_ptr(cur_ptr);  \
  incr(sa_used(q));     \
} while (0)
 
#define delete_sa_ptr() \
do {                    \
  put_sa_ptr(null);     \
  decr(sa_used(q));     \
} while (0)
//
#define sa_lev            sa_used
#define pointer_node_size 2 
#define sa_type(a)        (sa_index(a) / 16) 
#define sa_ref(a)         info(a + 1) 
#define sa_ptr(a)         link(a + 1)
//
#define word_node_size  3
#define sa_num          sa_ptr
#define sa_int(a)       mem[a + 2].cint
#define sa_dim(a)       mem[a + 2].cint
//
#define mark_class_node_size 4
//
#define fetch_box(a)                          \
do {                                          \
  if (cur_val < 256)                          \
    a = box(cur_val);                         \
  else                                        \
  {                                           \
    find_sa_element(box_val, cur_val, false); \
                                              \
    if (cur_ptr == null)                      \
      a = null;                               \
    else                                      \
      a = sa_ptr(cur_ptr);                    \
  }                                           \
} while (0)
//
#define add_sa_ref(a) incr(sa_ref(a))
//
#define change_box(a) \
do {                  \
  if (cur_val<256)    \
    box(cur_val) = a; \
  else                \
    set_sa_box(a);    \
} while (0)

#define set_sa_box(a)                     \
do {                                      \
  find_sa_element(box_val,cur_val,false); \
                                          \
  if (cur_ptr != null)                    \
  {                                       \
    sa_ptr(cur_ptr) = a;                  \
    add_sa_ref(cur_ptr);                  \
    delete_sa_ref(cur_ptr);               \
  }                                       \
} while (0)
//
#define vsplit_init   0
#define fire_up_init  1
#define fire_up_done  2
#define destroy_marks 3
//
#define sa_top_mark(a)          info(a + 1)
#define sa_first_mark(a)        link(a + 1)
#define sa_bot_mark(a)          info(a + 2)
#define sa_split_first_mark(a)  link(a + 2)
#define sa_split_bot_mark(a)    info(a + 3)
//
#define sa_loc sa_ref
//
#define sa_define(a1,a2,a3,a4,a5) \
do {                    \
  if (e)                \
  {                     \
    if (global)         \
      gsa_def(a1, a2);  \
    else                \
      sa_def(a1, a2);   \
  }                     \
  else                  \
    define(a3, a4, a5); \
} while (0)
//
#define sa_def_box()                      \
do {                                      \
  find_sa_element(box_val, cur_val, true);\
                                          \
  if (global)                             \
    gsa_def(cur_ptr,cur_box);             \
  else                                    \
    sa_def(cur_ptr,cur_box);              \
} while (0)
//
#define sa_word_define(a1,a2) \
do {                          \
  if (e)                      \
  {                           \
    if (global)               \
      gsa_w_def(a1, a2);      \
    else                      \
      sa_w_def(a1, a2);       \
  }                           \
  else word_define(a1, a2);   \
} while (0)
//
#define trie_link(a) trie_trl[a]
#define trie_char(a) trie_trc[a]
#define trie_op(a)   trie_tro[a]

#define set_hyph_index()                            \
do {                                                \
  if (trie_char(hyph_start + cur_lang) != cur_lang) \
    hyph_index = 0;                                 \
  else                                              \
    hyph_index = trie_link(hyph_start+cur_lang);    \
} while (0)

#define set_lc_code(a)                      \
do {                                        \
  if (hyph_index == 0)                      \
    hc[0] = lc_code(a);                     \
  else if (trie_char(hyph_index + a) != a)  \
    hc[0] = 0;                              \
  else                                      \
    hc[0] = trie_op(hyph_index + a);        \
} while (0)

#define hyph_root       trie_r[0]
#define tail_page_disc  disc_ptr[copy_code]
#define page_disc       disc_ptr[last_box_code]
#define split_disc      disc_ptr[vsplit_code]

#define illegal_Ucharcat_ascii_catcode(a) \
  (a<left_brace)||(a>active_char)||(a==out_param)||(a==ignore)
#define illegal_Ucharcat_wchar_catcode(a) \
  (a<kanji)||(a>hangul)

#define adjust_selector_based_on_show_stream()  \
do {                                            \
  if ((show_stream >= 0) &&                     \
      (show_stream < no_print) &&               \
      write_open[show_stream])                  \
    selector = show_stream;                     \
} while (0)
#endif
