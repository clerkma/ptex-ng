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

static const double sp2bp = 0.000015202;
static scaled cur_page_width;
static scaled cur_page_height;

void ensure_pdf_open(void)
{
  if (output_file_name == 0)
  {
    if (job_name == 0)
      open_log_file();

    pack_job_name(".pdf");

    while (!b_open_out(pdf_file))
      prompt_file_name("file name for output", ".pdf");

    output_file_name = b_make_name_string(pdf_file);
  }
}

void pdf_set_cur_page(scaled cur_wd, scaled cur_ht)
{
  pdf_rect mediabox;

  mediabox.llx = 0.0;
  mediabox.lly = 0.0;
  mediabox.urx = cur_wd * sp2bp;
  mediabox.ury = cur_ht * sp2bp;
  pdf_doc_set_mediabox(total_pages + 1, &mediabox);
}

void pdf_ship_out (pointer p)
{
  integer page_loc;
  pointer del_node;
  char j, k;

  if (tracing_output > 0)
  {
    print_nl("");
    print_ln();
    prints("Completed box being shipped out");
  }

  if (term_offset > max_print_line - 9)
    print_ln();
  else if ((term_offset > 0) || (file_offset > 0))
    print_char(' ');

  print_char('[');
  j = 9;

  while ((count(j) == 0) && (j > 0))
    decr(j);

  for (k = 0; k <= j; k++)
  {
    print_int(count(k));

    if (k < j)
      print_char('.');
  }

  update_terminal();

  if (tracing_output > 0)
  {
    print_char(']');
    begin_diagnostic();
    show_box(p);
    end_diagnostic(true);
  }

  if (type(p) == dir_node)
  {
    del_node = p;
    p = list_ptr(p);
    delete_glue_ref(space_ptr(del_node));
    delete_glue_ref(xspace_ptr(del_node));
    free_node(del_node, box_node_size);
  }

  flush_node_list(link(p));
  link(p) = null;

  if (box_dir(p) != dir_yoko)
    p = new_dir_node(p, dir_yoko);

  if ((height(p) > max_dimen) || (depth(p) > max_dimen) ||
      (height(p) + depth(p) + v_offset > max_dimen) ||
      (width(p) + h_offset > max_dimen))
  {
    print_err("Huge page cannot be shipped out");
    help2("The page just created is more than 18 feet tall or",
      "more than 18 feet wide, so I suspect something went wrong.");
    error();

    if (tracing_output <= 0)
    {
      begin_diagnostic();
      print_nl("The following box has been deleted:");
      show_box(p);
      end_diagnostic(true);
    }

    goto done;
  }

  if (height(p) + depth(p) + v_offset > max_v)
    max_v = height(p) + depth(p) + v_offset;

  if (width(p) + h_offset > max_h)
    max_h = width(p) + h_offset;

  dvi_h = 0;
  dvi_v = 0;
  cur_h = h_offset;
  dvi_f = null_font;
  dvi_dir = dir_yoko;
  cur_dir_hv = dvi_dir;
  ensure_pdf_open();

  if (total_pages == 0)
  {
    pdf_set_version(5);
    pdf_set_compression(9);
    pdf_init_fontmaps();
    pdf_load_fontmap_file("pdftex.map", '+');
    //pdf_load_fontmap_file("kanjix.map", '+');
    //pdf_load_fontmap_file("ckx.map", '+');
    pdf_doc_set_producer("pTeX-ng@2014");
    pdf_doc_set_creator("pTeX-ng");
    pdf_files_init();
    pdf_init_device(sp2bp, 2, 0);
    pdf_open_document(pdf_file_name, 0, 595.0, 842.0, 0, 0, (1 << 4));
    spc_exec_at_begin_document();
  }

  page_loc = dvi_offset + dvi_ptr;

  if (pdf_page_width != 0)
    cur_page_width = pdf_page_width;
  else
    cur_page_width = width(p) + 2 * (pdf_h_origin + h_offset + 4736286);

  if (pdf_page_height != 0)
    cur_page_height = pdf_page_height;
  else
    cur_page_height = height(p) + depth(p) + 2 * (pdf_v_origin + v_offset + 4736286);

  pdf_set_cur_page(cur_page_width, cur_page_height);
  pdf_doc_begin_page(1.0, pdf_h_origin * sp2bp, (cur_page_height - pdf_v_origin) * sp2bp);
  spc_exec_at_begin_page();
  last_bop = page_loc;
  cur_v = height(p) + v_offset;
  temp_ptr = p;

  switch (type(p))
  {
    case hlist_node:
      hlist_out();
      break;
    case vlist_node:
      vlist_out();
      break;
    case dir_node:
      dir_out();
      break;
  }

  spc_exec_at_end_page();
  pdf_doc_end_page();
  incr(total_pages);
  cur_s = -1;

  if (eTeX_ex)
  {
    if (LR_problems > 0)
    {
      report_LR_problems();
      print_char(')');
      print_ln();
    }

    if ((LR_ptr != null) || (cur_dir != left_to_right))
      confusion("LR3");
  }

done:
  if (tracing_output <= 0)
    print_char(']');

  dead_cycles = 0;
  update_terminal();

#ifdef STAT
  if (tracing_stats > 1)
  {
    print_nl("Memory usage before: ");
    print_int(var_used);
    print_char('&');
    print_int(dyn_used);
    print_char(';');
  }
#endif

  flush_node_list(p);

#ifdef STAT
  if (tracing_stats > 1)
  {
    prints(" after: ");
    print_int(var_used);
    print_char('&');
    print_int(dyn_used);
    prints("; still utouched: ");
    print_int(hi_mem_min - lo_mem_max - 1);
    print_ln();
  }
#endif
}

void pdf_synch_h (void)
{
  if (cur_h != dvi_h)
    dvi_h = cur_h;
}

void pdf_synch_v (void)
{
  if (cur_v != dvi_v)
    dvi_v = cur_v;
}

static int number_of_fonts = -1;

void pdf_get_font (internal_font_number f)
{
  char * sbuf = malloc(length(font_name[f]) + 1);
  memset(sbuf, 0, length(font_name[f]) + 1);
  memcpy(sbuf, str_pool + str_start[font_name[f]], length(font_name[f]));

  if (pdf_dev_locate_font(sbuf, font_size[f]) >= 0)
  {
    number_of_fonts += 1;
    font_id[f] = number_of_fonts;
  }
  else
  {
    font_id[f] = -1;
  }

  free(sbuf);
}

void pdf_out_char (internal_font_number f, ASCII_code c)
{
  pdf_rect rect;
  char cbuf[2];
  cbuf[0] = c; cbuf[1] = 0;
  switch (cur_dir_hv)
  {
    case dir_yoko:
      pdf_dev_set_dirmode(dvi_yoko);
      pdf_dev_set_string(cur_h, -cur_v, cbuf, 1, char_width(f, char_info(f, c)), font_id[f], 1);
      break;
    case dir_tate:
      pdf_dev_set_dirmode(dvi_tate);
      pdf_dev_set_string(-cur_v, -cur_h, cbuf, 1, char_width(f, char_info(f, c)), font_id[f], 1);
      break;
    case dir_dtou:
      pdf_dev_set_dirmode(dvi_dtou);
      pdf_dev_set_string(cur_v, cur_h, cbuf, 1, char_width(f, char_info(f, c)), font_id[f], 1);
      break;
  }

  pdf_dev_set_rect(&rect, cur_h, -cur_v, char_width(f, char_info(f, c)),
      char_height(f, height_depth(char_info(f, c))),
      char_depth(f, height_depth(char_info(f, c))));
  pdf_doc_expand_box(&rect);
}

#define jfm_zw(f) char_width(f, char_info(f, 0))

void mojikumi_before_kanji (internal_font_number f, KANJI_code k, ASCII_code d)
{
  switch (k)
  {
    case 0xFF08: /* （ */
    case 0x3014: /* 〔 */
    case 0xFF3B: /* ［ */
    case 0xFF5B: /* ｛ */
    case 0x3008: /* 〈 */
    case 0x300A: /* 《 */
    case 0x300C: /* 「 */
    case 0x300E: /* 『 */
    case 0x3010: /* 【 */
    case 0xFF5F: /* JIS X 0213  1-02-54 始め二重バーレーン */
    case 0x3018: /* JIS X 0213  1-02-56 始め二重亀甲括弧 */
    case 0x3016: /* JIS X 0213  1-02-58 始めすみ付き括弧(白) */
    case 0x301D: /* JIS X 0213  1-13-64 始めダブルミニュート */
      cur_h = cur_h - (jfm_zw(f) - char_width(f, char_info(f, d)));
      break;
    case 0x3000: /* spc */
    case 0x3001: /* 、 */
    case 0x3002: /* 。 */
    case 0xFF0C: /* ， */
    case 0xFF0E: /* ． */
    case 0x309B: /* ゛ */
    case 0x309C: /* ゜ */
    case 0xFF09: /* ） */
    case 0x3015: /* 〕 */
    case 0xFF3D: /* ］ */
    case 0xFF5D: /* ｝ */
    case 0x3009: /* 〉 */
    case 0x300B: /* 》 */
    case 0x300D: /* 」 */
    case 0x300F: /* 』 */
    case 0x3011: /* 】 */
    case 0xFF60: /* JIS X 0213  1-02-55 終わり二重バーレーン */
    case 0x3019: /* JIS X 0213  1-02-57 終わり二重亀甲括弧 */
    case 0x3017: /* JIS X 0213  1-02-59 終わりすみ付き括弧(白) */
    case 0x301F: /* JIS X 0213  1-13-65 終わりダブルミニュート */
    case 0x00B0: /* ° */
    case 0x2032: /* ′ */
    case 0x2033: /* ″ */
      break;
    default:
      if (jfm_zw(f) != char_width(f, char_info(f, d)))
        cur_h = cur_h - (jfm_zw(f) - char_width(f, char_info(f, d))) / 2;
      break;
  }
}

void mojikumi_after_kanji (internal_font_number f, KANJI_code k, ASCII_code d)
{
  switch (k)
  {
    case 0xFF08: /* （ */
    case 0x3014: /* 〔 */
    case 0xFF3B: /* ［ */
    case 0xFF5B: /* ｛ */
    case 0x3008: /* 〈 */
    case 0x300A: /* 《 */
    case 0x300C: /* 「 */
    case 0x300E: /* 『 */
    case 0x3010: /* 【 */
    case 0xFF5F: /* JIS X 0213  1-02-54 始め二重バーレーン */
    case 0x3018: /* JIS X 0213  1-02-56 始め二重亀甲括弧 */
    case 0x3016: /* JIS X 0213  1-02-58 始めすみ付き括弧(白) */
    case 0x301D: /* JIS X 0213  1-13-64 始めダブルミニュート */
      cur_h = cur_h + char_width(f, char_info(f, d));
      break;
    case 0x3000: /* spc */
    case 0x3001: /* 、 */
    case 0x3002: /* 。 */
    case 0xFF0C: /* ， */
    case 0xFF0E: /* ． */
    case 0x309B: /* ゛ */
    case 0x309C: /* ゜ */
    case 0xFF09: /* ） */
    case 0x3015: /* 〕 */
    case 0xFF3D: /* ］ */
    case 0xFF5D: /* ｝ */
    case 0x3009: /* 〉 */
    case 0x300B: /* 》 */
    case 0x300D: /* 」 */
    case 0x300F: /* 』 */
    case 0x3011: /* 】 */
    case 0xFF60: /* JIS X 0213  1-02-55 終わり二重バーレーン */
    case 0x3019: /* JIS X 0213  1-02-57 終わり二重亀甲括弧 */
    case 0x3017: /* JIS X 0213  1-02-59 終わりすみ付き括弧(白) */
    case 0x301F: /* JIS X 0213  1-13-65 終わりダブルミニュート */
    case 0x00B0: /* ° */
    case 0x2032: /* ′ */
    case 0x2033: /* ″ */
      //cur_h = cur_h - (jfm_zw(f) - char_width(f, char_info(f, d)));
      break;
    default:
      if (jfm_zw(f) != char_width(f, char_info(f, d)))
        cur_h = cur_h + (jfm_zw(f) - char_width(f, char_info(f, d))) / 2;
      break;
  }
}

void pdf_out_kanji(internal_font_number f, KANJI_code k, ASCII_code d)
{
  pdf_rect rect;
  char cbuf[4];

  mojikumi_before_kanji(f, k, d);

  if (k < 0x10000)
  {
    cbuf[0] = Hi(k);
    cbuf[1] = Lo(k);

    if (font_id[f] >= 0)
    {
      switch (cur_dir_hv)
      {
        case dir_yoko:
          pdf_dev_set_dirmode(dvi_yoko);
          pdf_dev_set_string(cur_h, -cur_v, cbuf, 2, jfm_zw(f), font_id[f], 2);
          break;
        case dir_tate:
          pdf_dev_set_dirmode(dvi_tate);
          pdf_dev_set_string(-cur_v, -cur_h, cbuf, 2, jfm_zw(f), font_id[f], 2);
          break;
        case dir_dtou:
          pdf_dev_set_dirmode(dvi_dtou);
          pdf_dev_set_string(cur_v, cur_h, cbuf, 2, jfm_zw(f), font_id[f], 2);
          break;
      }
    }
  }
  else
  {
    cbuf[0] = (UTF32toUTF16HS(k) >> 8) & 0xff;
    cbuf[1] = UTF32toUTF16HS(k)        & 0xff;
    cbuf[2] = (UTF32toUTF16LS(k) >> 8) & 0xff;
    cbuf[3] = UTF32toUTF16LS(k)        & 0xff;

    if (font_id[f] >= 0)
    {
      switch (cur_dir_hv)
      {
        case dir_yoko:
          pdf_dev_set_dirmode(dvi_yoko);
          pdf_dev_set_string(cur_h, -cur_v, cbuf, 4, jfm_zw(f), font_id[f], 2);
          break;
        case dir_tate:
          pdf_dev_set_dirmode(dvi_tate);
          pdf_dev_set_string(-cur_v, -cur_h, cbuf, 4, jfm_zw(f), font_id[f], 2);
          break;
        case dir_dtou:
          pdf_dev_set_dirmode(dvi_dtou);
          pdf_dev_set_string(-cur_v, -cur_h, cbuf, 4, jfm_zw(f), font_id[f], 2);
          break;
      }
    }
  }

  pdf_dev_set_rect(&rect, cur_h, -cur_v,
    char_width(f, char_info(f, d)),
    char_height(f, height_depth(char_info(f, d))),
    char_depth(f, height_depth(char_info(f, d))));
  pdf_doc_expand_box(&rect);
  mojikumi_after_kanji(f, k, d);
}

void pdf_output_rule(scaled rule_wd, scaled rule_ht)
{
  switch (cur_dir_hv)
  {
    case dir_yoko:
      pdf_dev_set_rule(cur_h, -cur_v, rule_wd, rule_ht);
      break;
    case dir_tate:
      pdf_dev_set_rule(-cur_v, -cur_h - rule_wd, rule_ht, rule_wd);
      break;
    case dir_dtou:
      pdf_dev_set_rule(cur_v, cur_h, rule_ht, rule_wd);
      break;
  }
}

void hlist_out (void)
{
  scaled base_line;
  scaled disp;
  eight_bits save_dir;
  KANJI_code jc;
  pointer ksp_ptr;
  scaled left_edge;
  scaled save_h, save_v;
  pointer this_box;
  // glue_ord g_order;
  int g_order;
  // char g_sign;
  int g_sign;
  pointer p;
  integer save_loc;
  pointer leader_box;
  scaled leader_wd;
  scaled lx;
  boolean outer_doing_leaders;
  scaled edge;
  pointer prev_p;
  real glue_temp;
  real cur_glue;
  scaled cur_g;

  cur_g = 0;
  cur_glue = 0.0;
  this_box = temp_ptr;
  g_order = glue_order(this_box);
  g_sign = glue_sign(this_box);
  p = list_ptr(this_box);
  ksp_ptr = space_ptr(this_box);
  incr(cur_s);

  if (cur_s > max_push)
    max_push = cur_s;

  save_loc = dvi_offset + dvi_ptr;
  pdf_synch_dir();
  base_line = cur_v;
  disp = 0;
  revdisp = 0;
  prev_p = this_box + list_offset;

  if (eTeX_ex)
  {
    put_LR(before);
  
    if (box_lr(this_box) == dlist)
      if (cur_dir == right_to_left)
      {
        cur_dir = left_to_right;
        cur_h = cur_h - width(this_box);
      }
      else
        set_box_lr(this_box, 0);
      
      if ((cur_dir == right_to_left) && (box_lr(this_box) != reversed))
      {
        save_h = cur_h; temp_ptr = p; p = new_kern(0); link(prev_p) = p;
        cur_h = 0; link(p) = reverse(this_box, null, cur_g, cur_glue); width(p) = -cur_h;
        cur_h = save_h; set_box_lr(this_box, reversed);
      }
  }

  left_edge = cur_h;

  while (p != 0)
reswitch:
  if (is_char_node(p))
  {
    pdf_synch_h();
    pdf_synch_v();
    chain = false;

    do
    {
      f = font(p);
      c = character(p);

      if (f != dvi_f)
      {
        if (!font_used[f])
        {
          pdf_get_font(f);
          font_used[f] = true;
        }

        dvi_f = f;
      }

      if (font_dir[f] == dir_default)
      {
        chain = false;
        pdf_out_char(dvi_f, c);
        cur_h = cur_h + char_width(f, char_info(f, c));
      }
      else
      {
        if (chain == false)
          chain = true;
        else
        {
          cur_h = cur_h + width(ksp_ptr);

          if (g_sign != normal)
          {
            if (g_sign == stretching)
            {
              if (stretch_order(ksp_ptr) == g_order)
                cur_h = cur_h + round(tex_float(glue_set(this_box)) * stretch(ksp_ptr));
            }
            else
            {
              if (shrink_order(ksp_ptr) == g_order)
                cur_h = cur_h - round(tex_float(glue_set(this_box)) * shrink(ksp_ptr));
            }
          }

          pdf_synch_h();
        }

        p = link(p);
        jc = toDVI(KANJI(info(p)) % max_cjk_val);
        pdf_out_kanji(f, jc, c);
        cur_h = cur_h + char_width(f, char_info(f, c));
      }

      dvi_h = cur_h;
      prev_p = link(prev_p);
      p = link(p);
    } while (!(!is_char_node(p)));

    chain = false;
  }
  else
  {
    switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
        if (list_ptr(p) == 0)
          cur_h = cur_h + width(p);
        else
        {
          save_h = dvi_h;
          save_v = dvi_v;
          save_dir = dvi_dir;
          cur_v = base_line + disp + shift_amount(p);
          temp_ptr = p;
          edge = cur_h + width(p);

          if (cur_dir == right_to_left)
            cur_h = edge;

          switch (type(p))
          {
            case hlist_node:
              hlist_out();
              break;
            case vlist_node:
              vlist_out();
              break;
            case dir_node:
              dir_out();
              break;
          }

          dvi_h = save_h;
          dvi_v = save_v;
          dvi_dir = save_dir;
          cur_h = edge;
          cur_v = base_line + disp;
          cur_dir_hv = save_dir;
        }
        break;

      case rule_node:
        {
          rule_ht = height(p);
          rule_dp = depth(p);
          rule_wd = width(p);
          goto fin_rule;
        }
        break;

      case whatsit_node:
        out_what(p);
        break;

      case disp_node:
        disp = disp_dimen(p);
        revdisp = disp;
        cur_v = base_line + disp;
        break;

      case glue_node:
        {
          round_glue();

          if (eTeX_ex)
            handle_a_glue_node();

          if (subtype(p) >= a_leaders)
          {
            leader_box = leader_ptr(p);

            if (type(leader_box) == rule_node)
            {
              rule_ht = height(leader_box);
              rule_dp = depth(leader_box);
              goto fin_rule;
            }

            leader_wd = width(leader_box);

            if ((leader_wd > 0) && (rule_wd > 0))
            {
              rule_wd = rule_wd + 10;

              if (cur_dir == right_to_left)
                cur_h = cur_h - 10;

              edge = cur_h + rule_wd;
              lx = 0;

              if (subtype(p) == a_leaders)
              {
                save_h = cur_h;
                cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);

                if (cur_h < save_h)
                  cur_h = cur_h + leader_wd;
              }
              else
              {
                lq = rule_wd / leader_wd;
                lr = rule_wd % leader_wd;

                if (subtype(p) == c_leaders)
                  cur_h = cur_h + (lr / 2);
                else
                {
                  lx = (2 * lr + lq + 1) / (2 * lq + 2);
                  cur_h = cur_h + ((lr - (lq - 1)* lx) / 2);
                }
              }

              while (cur_h + leader_wd <= edge)
              {
                cur_v = base_line + disp + shift_amount(leader_box);
                pdf_synch_v();
                save_v = dvi_v;
                pdf_synch_h();
                save_h = dvi_h;
                save_dir = dvi_dir;
                temp_ptr = leader_box;

                if (cur_dir == right_to_left)
                  cur_h = cur_h + leader_wd;

                outer_doing_leaders = doing_leaders;
                doing_leaders = true;

                switch (type(p))
                {
                  case hlist_node:
                    hlist_out();
                    break;
                  case vlist_node:
                    vlist_out();
                    break;
                  case dir_node:
                    dir_out();
                    break;
                }

                doing_leaders = outer_doing_leaders;
                dvi_v = save_v;
                dvi_h = save_h;
                dvi_dir = save_dir;
                cur_v = base_line;
                cur_h = save_h + leader_wd + lx;
                cur_dir_hv = save_dir;
              }

              if (cur_dir == right_to_left)
                cur_h = edge;
              else
                cur_h = edge - 10;

              goto next_p;
            }
          }

          goto move_past;
        }
        break;

      case kern_node:
        cur_h = cur_h + width(p);
        break;

      case math_node:
        {
          if (eTeX_ex)
          {
            if (end_LR(p))
              if (info(LR_ptr) == end_LR_type(p))
                pop_LR();
              else
              {
                if (subtype(p)>L_code)
                  incr(LR_problems);
              }
            else
            {
              push_LR(p);

              if (LR_dir(p) != cur_dir)
              {
                save_h = cur_h;
                temp_ptr = link(p);
                rule_wd = width(p);
                free_node(p, small_node_size);
                cur_dir = reflected;
                p = new_edge(cur_dir, rule_wd);
                link(prev_p) = p;
                cur_h = cur_h - left_edge + rule_wd;
                link(p) = reverse(this_box, new_edge(reflected, 0), cur_g, cur_glue);
                edge_dist(p) = cur_h;
                cur_dir = reflected;
                cur_h = save_h;
                goto reswitch;
              }
            }

            type(p) = kern_node;
          }

          cur_h = cur_h + width(p);
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

      case edge_node:
        {
          cur_h = cur_h + width(p);
          left_edge = cur_h + edge_dist(p);
          cur_dir = subtype(p);
        }

      default:
        do_nothing();
        break;
    }

    goto next_p;

fin_rule:
    if (is_running(rule_ht))
      rule_ht = height(this_box) + disp;

    if (is_running(rule_dp))
      rule_dp = depth(this_box) - disp;

    rule_ht = rule_ht + rule_dp;

    if ((rule_ht > 0) && (rule_wd > 0))
    {
      pdf_synch_h();
      cur_v = base_line + rule_dp;
      pdf_synch_v();
      pdf_output_rule(rule_wd, rule_ht);
      cur_v = base_line;
      dvi_h = dvi_h + rule_wd;
    }

move_past:
    cur_h = cur_h + rule_wd;

next_p:
    prev_p = p;
    p = link(p);
  }

  if (eTeX_ex)
  {
    {
      while (info(LR_ptr) != before)
      {
        if (info(LR_ptr) > L_code)
          LR_problems = LR_problems + 10000;

        pop_LR();
      }

      pop_LR();
    }

    if (box_lr(this_box) == dlist)
      cur_dir = right_to_left;
  }

  prune_movements(save_loc);
  decr(cur_s);
}

void vlist_out (void)
{
  scaled left_edge;
  scaled top_edge;
  scaled save_h, save_v;
  pointer this_box;
  // glue_ord g_order;
  int g_order;
  // char g_sign;
  int g_sign;
  pointer p;
  integer save_loc;
  pointer leader_box;
  scaled leader_ht;
  scaled lx;
  boolean outer_doing_leaders;
  scaled edge;
  real glue_temp;
  real cur_glue;
  scaled cur_g;
  integer save_dir;

  cur_g = 0;
  cur_glue = 0.0;
  this_box = temp_ptr;
  g_order = glue_order(this_box);
  g_sign = glue_sign(this_box);
  p = list_ptr(this_box);
  incr(cur_s);

  if (cur_s > max_push)
    max_push = cur_s;

  save_loc = dvi_offset + dvi_ptr;
  pdf_synch_dir();
  left_edge = cur_h;
  cur_v = cur_v - height(this_box);
  top_edge = cur_v;

  while (p != 0)
  {
    if (is_char_node(p))
    {
      confusion("vlistout");
      return;
    }
    else
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
          if (list_ptr(p) == 0)
            cur_v = cur_v + height(p) + depth(p);
          else
          {
            cur_v = cur_v + height(p);
            pdf_synch_v();
            save_h = dvi_h;
            save_v = dvi_v;
            save_dir = dvi_dir;

            if (cur_dir == right_to_left)
              cur_h = left_edge - shift_amount(p);
            else
              cur_h = left_edge + shift_amount(p);

            temp_ptr = p;

            switch (type(p))
            {
              case hlist_node:
                hlist_out();
                break;
              case vlist_node:
                vlist_out();
                break;
              case dir_node:
                dir_out();
                break;
            }

            dvi_h = save_h;
            dvi_v = save_v;
            dvi_dir = save_dir;
            cur_v = save_v + depth(p);
            cur_h = left_edge;
            cur_dir_hv = save_dir;
          }
          break;

        case rule_node:
          {
            rule_ht = height(p);
            rule_dp = depth(p);
            rule_wd = width(p);
            goto fin_rule;
          }
          break;

        case whatsit_node:
          out_what(p);
          break;

        case glue_node:
          {
            g = glue_ptr(p);
            rule_ht = width(g) - cur_g;

            if (g_sign != normal)
            {
              if (g_sign == stretching)
              {
                if (stretch_order(g) == g_order)
                {
                  cur_glue = cur_glue + stretch(g);
                  vet_glue(glue_set(this_box) * cur_glue);
                  cur_g = round(glue_temp);
                }
              }
              else if (shrink_order(g) == g_order)
              {
                cur_glue = cur_glue - shrink(g);
                vet_glue(glue_set(this_box) * cur_glue);
                cur_g = round(glue_temp);
              }
            }

            rule_ht = rule_ht + cur_g;

            if (subtype(p) >= a_leaders)
            {
              leader_box = leader_ptr(p);

              if (type(leader_box) == rule_node)
              {
                rule_wd = width(leader_box);
                rule_dp = 0;
                goto fin_rule;
              }

              leader_ht = height(leader_box) + depth(leader_box);

              if ((leader_ht > 0) && (rule_ht > 0))
              {
                rule_ht = rule_ht + 10;
                edge = cur_v + rule_ht;
                lx = 0;

                if (subtype(p) == a_leaders)
                {
                  save_v = cur_v;
                  cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);

                  if (cur_v < save_v)
                    cur_v = cur_v + leader_ht;
                }
                else
                {
                  lq = rule_ht / leader_ht;
                  lr = rule_ht % leader_ht;

                  if (subtype(p) == c_leaders)
                    cur_v = cur_v + (lr / 2);
                  else
                  {
                    lx = (2 * lr + lq + 1) / (2 * lq + 2);
                    cur_v = cur_v + ((lr - (lq - 1) * lx) / 2);
                  }
                }

                while (cur_v + leader_ht <= edge)
                {
                  if (cur_dir == right_to_left)
                    cur_h = left_edge - shift_amount(leader_box);
                  else
                    cur_h = left_edge + shift_amount(leader_box);

                  pdf_synch_h();
                  save_h = dvi_h;
                  cur_v = cur_v + height(leader_box);
                  pdf_synch_v();
                  save_v = dvi_v;
                  save_dir = dvi_dir;
                  temp_ptr = leader_box;
                  outer_doing_leaders = doing_leaders;
                  doing_leaders = true;

                  switch (type(p))
                  {
                    case hlist_node:
                      hlist_out();
                      break;
                    case vlist_node:
                      vlist_out();
                      break;
                    case dir_node:
                      dir_out();
                      break;
                  }

                  doing_leaders = outer_doing_leaders;
                  dvi_v = save_v;
                  dvi_h = save_h;
                  dvi_dir = save_dir;
                  cur_h = left_edge;
                  cur_v = save_v - height(leader_box) + leader_ht + lx;
                  cur_dir_hv = save_dir;
                }

                cur_v = edge - 10;
                goto next_p;
              }
            }

            goto move_past;
          }
          break;

        case kern_node:
          cur_v = cur_v + width(p);
          break;

        default:
          break;
      }

      goto next_p;

fin_rule:
      if (is_running(rule_wd))
        rule_wd = width(this_box);

      rule_ht = rule_ht + rule_dp;
      cur_v = cur_v + rule_ht;

      if ((rule_ht > 0) && (rule_wd > 0))
      {
        if (cur_dir == right_to_left)
          cur_h = cur_h - rule_wd;

        pdf_synch_h();
        pdf_synch_v();
        pdf_output_rule(rule_wd, rule_ht);
        cur_h = left_edge;
      }

      goto next_p;

move_past:
      cur_v = cur_v + rule_ht;
    }

next_p:
    p = link(p);
  }

  prune_movements(save_loc);
  decr(cur_s);
}