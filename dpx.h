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

#ifndef _DPX_H
#define _DPX_H

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
extern int dvi_locate_font (const char * name, spt_t ptsize);
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
extern void read_config_file (const char *config);
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
extern void pdf_dev_set_dirmode(int dir_mode);
extern int pdf_load_fontmap_file(const char *filename, int mode);
#endif