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

/* sec 0440 */
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

  do
    {
      do 
        {
          get_x_token();
        }
      while (!(cur_cmd != spacer));

      if (cur_tok == other_token + '-')
      {
        negative = !negative;
        cur_tok = other_token + '+';
      }
    }
  while (!(cur_tok != other_token + '+'));

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
  {
    scan_something_internal(int_val, false);
  }
  else
  {
    radix = 10;
    m = 214748364L;   /* 7FFFFFFF hex */

    if (cur_tok == octal_token)
    {
      radix = 8;
      m = 268435456L;   /* 2^28 */
      get_x_token();
    }
    else if (cur_tok == hex_token)
    {
      radix = 16;
      m = 134217728L;   /* 2^27 8000000 hex */
      get_x_token();
    }

    vacuous = true;
    cur_val = 0;

    while (true)
    {
      if ((cur_tok < zero_token + radix) && (cur_tok >= zero_token) && (cur_tok <= zero_token + 9))
        d = cur_tok - zero_token;
      else if (radix == 16)
        if ((cur_tok <= A_token + 5) && (cur_tok >= A_token))
          d = cur_tok - A_token + 10;
        else if ((cur_tok <= other_A_token + 5) && (cur_tok >= other_A_token))
          d = cur_tok - other_A_token;
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
          cur_val = 2147483647L;    /* 7FFFFFFF hex */
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
    cur_val = - (integer) cur_val;
}
/* sec 0448 */
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

    do
      {
        do
          {
            get_x_token();
          }
        while (!(cur_cmd != spacer));

        if (cur_tok == other_token + '-')
        {
          negative = ! negative;
          cur_tok = other_token + '+';
        }
      }
    while (!(cur_tok != other_token + '+'));

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
      {
        scan_int();
      }
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
    cur_val = - (integer) cur_val;
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

  do
    {
      get_x_token();
    }
  while (!(cur_cmd != spacer));

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
      {
        mu_error();
      }
    }
    else
    {
      scan_something_internal(dimen_val, false);
    }

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
      f = (1000 * f + 65536L * tex_remainder) / mag;
      cur_val = cur_val + (f / 65536L);
      f = f % 65536L;
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
    cur_val = - (integer) cur_val;
}
/* sec 0461 */
void scan_glue (small_number level)
{
  boolean negative;
  pointer q;
  boolean mu;

  mu = (level == mu_val);
  negative = false;

  do
    {
      do
        {
          get_x_token();
        }
      while (!(cur_cmd != spacer));

      if (cur_tok == other_token + '-')
      {
        negative = !negative;
        cur_tok = other_token + '+';
      }
    }
  while (!(cur_tok != other_token + '+'));

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
      cur_val = - (integer) cur_val;
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

    if ((multistrlen(str_pool, pool_ptr, k)>1) && check_kcat_code(cc))
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
      save_scanner_status = scanner_status;
      scanner_status = normal;
      get_token();

      if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      {
        KANJI(cx) = cur_tok;
      }

      scanner_status = save_scanner_status;
      break;

    case font_name_code:
      scan_font_ident();
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
      print(font_name[cur_val]);

      if (font_size[cur_val] != font_dsize[cur_val])
      {
        prints(" at ");
        print_scaled(font_size[cur_val]);
        prints("pt");
      }
      break;

    case job_name_code:
      print(job_name);
      break;
  }

  selector = old_setting;
  link(garbage) = str_toks(b);
  begin_token_list(link(temp_head), 4);
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
  {
    scan_left_brace();
  }

  unbalance = 1;

  while (true)
  {
    if (xpand)
    {
      while (true)
      {
        get_next();

        if (cur_cmd <= max_command)
          goto done2;

        if (cur_cmd != the)
        {
          expand();
        }
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
  scanner_status = 0;

  if (hash_brace != 0)
    store_new_token(hash_brace);

  return p;
}
/* sec 0482 */
void read_toks (integer n, pointer r)
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

  do
    {
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

      while (true)
      {
        get_token();

        if (cur_tok == 0)
          goto done;

        if (align_state < 1000000L)
        {
          do
            {
              get_token();
            }
          while (!(cur_tok == 0));

          align_state = 1000000L;
          goto done;
        }

        store_new_token(cur_tok);
      }

done:
      end_file_reading();
    }
  while (!(align_state == 1000000L));

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
  this_if = cur_chr;

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
        
        do
          {
            get_x_token();
          }
        while (!(cur_cmd != spacer));

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
      scan_int();
      b = odd(cur_val);
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
        scan_eight_bit_int();
        p = box(cur_val);

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
        scanner_status = 0;
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
  if (quoted_file_name == false && c == ' ')
    return false;
  else if (quoted_file_name != false && c == '"')
  {
    quoted_file_name = false; /* catch next space character */
    return true;     /* accept ending quote, but throw away */
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

  if (area_delimiter == 0) // no area delimiter ':' '/' or '\' found
    cur_area = 335;        // "" default area 
  else
  {
    cur_area = str_ptr;
    str_start[str_ptr + 1] = str_start[str_ptr] + area_delimiter;
    incr(str_ptr);
  }

  if (ext_delimiter == 0) // no extension delimiter '.' found
  {
    cur_ext = 335;        // "" default extension 
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

  {
    name_of_file [name_length + 1] = '\0';

    if (trace_flag)
      printf(" pack_file_name -> `%s' (%lld) ", name_of_file + 1, name_length);

    name_of_file [name_length + 1] = ' ';
  }
}
/* sec 0523 */
void pack_buffered_name_(small_number n, integer a, integer b)
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

  do
    {
      get_x_token(); 
    }
  while (!(cur_cmd != spacer));

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
      if (BYTE1(cur_chr) != 0) append_char(BYTE1(cur_chr));
      if (BYTE2(cur_chr) != 0) append_char(BYTE2(cur_chr));
      if (BYTE3(cur_chr) != 0) append_char(BYTE3(cur_chr));
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
/* argument is string .fmt, .log, .pdf, or .dvi */
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

  if (!knuth_flag)
    show_line(" (or Ctrl-Z to exit)", 0);

  prompt_input(": ");

  {
    begin_name();
    k = first;

    while ((buffer[k] == ' ') && (k < last))
      incr(k);

    quoted_file_name = false;

    if (allow_quoted_names && k < last) /* check whether quoted name */
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

      /* convert tilde '~' to pseudo tilde */
      if (pseudo_tilde != 0 && buffer[k]== '~')
        buffer[k] = pseudo_tilde;

      /* convert space ' ' to pseudo space */
      if (pseudo_space != 0 && buffer[k]== ' ')
        buffer[k] = pseudo_space;

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
  char * months;

  old_setting = selector;

  if (job_name == 0)
    job_name = get_job_name(790);
    //job_name = 790;

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
      putc(months[k], log_file);

    print_char(' ');

    if (civilize_flag)
      print_int(day);
    else
      print_int(year);

    print_char(' ');
    print_two(tex_time / 60);
    print_char(':');
    print_two(tex_time % 60);
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
    //job_name = cur_name;
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

  g = 0;
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
        z = xn_over_d(z, - (integer) s, 1000);

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
    {
      store_scaled(font_info[k].cint);
    }
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
        font_false_bchar[f] = 256;
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