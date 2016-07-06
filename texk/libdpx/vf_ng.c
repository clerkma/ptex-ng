/* Copyright 2014 Clerk Ma

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
   02110-1301 USA.  */

/* output a vf packet */
#define SIGNED_QUAD int32_t
static int ng_stack_ptr = 0;
static SIGNED_QUAD ng_dvi_h[101];
static SIGNED_QUAD ng_dvi_v[101];
static SIGNED_QUAD ng_dvi_w[101];
static SIGNED_QUAD ng_dvi_x[101];
static SIGNED_QUAD ng_dvi_y[101];
static SIGNED_QUAD ng_dvi_z[101];

static int ng_packet_font (SIGNED_QUAD font_id, unsigned long vf_font)
{
  int i;

  for (i = 0; i < vf_fonts[vf_font].num_dev_fonts; i++)
  {
    if (font_id == ((vf_fonts[vf_font].dev_fonts)[i]).font_id)
      break;
  }

  if (i < vf_fonts[vf_font].num_dev_fonts)
  {
    return ((vf_fonts[vf_font].dev_fonts[i]).dev_id);
  }
  else
  {
    fprintf(stderr, "Font_id: %d not found in VF\n", font_id);
    return 0;
  }
}

extern int spc_exec_special (const char *buffer, long size, double x_user, double y_user, double dpx_mag);

static void ng_special_out (SIGNED_QUAD len, unsigned char **start, unsigned char *end, SIGNED_QUAD h, SIGNED_QUAD v)
{
  uint8_t *buffer;

  if (*start <= end - len)
  {
    buffer = NEW(len+1, uint8_t);
    memcpy(buffer, *start, len);
    buffer[len] = '\0';

    {
      uint8_t *p = buffer;

      while (p < buffer + len && *p == ' ')
        p++;
      /*
       * Warning message from virtual font.
       */
      if (!memcmp((char *)p, "Warning:", 8))
      {
        if (verbose)
          WARN("VF:%s", p+8);
      }
      else
      {
        double spc_h = 0.000015202 * h;
        double spc_v = 0.000015202 * v;
        graphics_mode();
        spc_exec_special(buffer, len, spc_h, spc_v, 1.0);
      }
    }

    RELEASE(buffer);
  }
  else
  {
    ERROR ("Premature end of DVI byte stream in VF font.");
  }

  *start += len;
}

typedef long long scaled;
extern void pdf_rule_out (scaled rule_wd, scaled rule_ht);
extern spt_t ng_packet_width (SIGNED_QUAD ch, int ng_font_id);
extern void ng_set (SIGNED_QUAD ch, int ng_font_id, SIGNED_QUAD h, SIGNED_QUAD v);

#define dvi_yoko 0
#define dvi_tate 1
#define dvi_dtou 3

static void ng_adjust_hpos (SIGNED_QUAD * h, SIGNED_QUAD * v, SIGNED_QUAD d)
{
  int ng_cur_dir = pdf_dev_get_dirmode();

  switch (ng_cur_dir)
  {
    case dvi_yoko:
      *h += d;
      break;
    case dvi_tate:
      *v -= d;
      break;
    case dvi_dtou:
      *v += d;
      break;
  }
}

static void ng_adjust_vpos (SIGNED_QUAD * h, SIGNED_QUAD * v, SIGNED_QUAD d)
{
  int ng_cur_dir = pdf_dev_get_dirmode();

  switch (ng_cur_dir)
  {
    case dvi_yoko:
      *v -= d;
      break;
    case dvi_tate:
      *h -= d;
      break;
    case dvi_dtou:
      *h += d;
      break;
  }
}

void ng_set_packet (SIGNED_QUAD ch, int vf_font, SIGNED_QUAD h, SIGNED_QUAD v)
{
  unsigned char opcode;
  unsigned char *start, *end;
  spt_t ptsize;
  SIGNED_QUAD packet_word;
  SIGNED_QUAD packet_h, packet_v;
  SIGNED_QUAD packet_w, packet_x;
  SIGNED_QUAD packet_y, packet_z;
  int packet_font = -1;

  if (vf_font < num_vf_fonts)
  {
    ptsize = vf_fonts[vf_font].ptsize;
    packet_h = h;
    packet_v = v;
    packet_w = 0;
    packet_x = 0;
    packet_y = 0;
    packet_z = 0;

    if (vf_fonts[vf_font].num_dev_fonts > 0)
      packet_font = ((vf_fonts[vf_font].dev_fonts)[0]).dev_id;

    if (ch >= vf_fonts[vf_font].num_chars || !(start = (vf_fonts[vf_font].ch_pkt)[ch]))
    {
      fprintf(stderr, "\nchar=0x%x(%d)\n", ch, ch);
      fprintf(stderr, "Tried to set a nonexistent character in a virtual font");
      start = end = NULL;
    }
    else
    {
      end = start + (vf_fonts[vf_font].pkt_len)[ch];
    }

    while (start && start < end)
    {
      opcode = *(start++);

      switch (opcode)
      {
        case SET1:
        case SET2:
        case SET3:
          packet_word = get_pkt_unsigned_num(&start, end, opcode - SET1);
          ng_set(packet_word, packet_font, packet_h, packet_v);
          ng_adjust_hpos(&packet_h, &packet_v, ng_packet_width(packet_word, packet_font));
          break;

        case PUT1:
        case PUT2:
        case PUT3:
          packet_word = get_pkt_unsigned_num(&start, end, opcode - PUT1);
          ng_set(packet_word, packet_font, packet_h, packet_v);
          break;

        case SET4:
        case PUT4:
          ERROR("Multibyte (>24 bits) character in VF packet.\nI can't handle this!");
          break;

        case SET_RULE:
          {
            SIGNED_QUAD width, height, s_width;
            height = get_pkt_signed_num(&start, end, 3);
            width = get_pkt_signed_num(&start, end, 3);
            s_width = sqxfw(ptsize, width);
            pdf_rule_out(s_width, sqxfw(ptsize, height));
            ng_adjust_hpos(&packet_h, &packet_v, s_width);
          }
          break;

        case PUT_RULE:
          {
            SIGNED_QUAD width, height, s_width;
            height = get_pkt_signed_num(&start, end, 3);
            width = get_pkt_signed_num(&start, end, 3);
            pdf_rule_out(sqxfw(ptsize, width), sqxfw(ptsize, height));
          }
          break;

        case NOP:
          break;

        case PUSH:
          {
            ng_dvi_h[ng_stack_ptr] = packet_h;
            ng_dvi_v[ng_stack_ptr] = packet_v;
            ng_dvi_w[ng_stack_ptr] = packet_w;
            ng_dvi_x[ng_stack_ptr] = packet_x;
            ng_dvi_y[ng_stack_ptr] = packet_y;
            ng_dvi_z[ng_stack_ptr] = packet_z;
            ng_stack_ptr += 1;
          }
          break;

        case POP:
          {
            ng_stack_ptr -= 1;
            packet_h = ng_dvi_h[ng_stack_ptr];
            packet_v = ng_dvi_v[ng_stack_ptr];
            packet_w = ng_dvi_w[ng_stack_ptr];
            packet_x = ng_dvi_x[ng_stack_ptr];
            packet_y = ng_dvi_y[ng_stack_ptr];
            packet_z = ng_dvi_z[ng_stack_ptr];
          }
          break;

        case RIGHT1:
        case RIGHT2:
        case RIGHT3:
        case RIGHT4:
          packet_word = get_pkt_signed_num(&start, end, opcode - RIGHT1);
          ng_adjust_hpos(&packet_h, &packet_v, sqxfw(ptsize, packet_word));
          break;

        case W0:
        case W1:
        case W2:
        case W3:
        case W4:
          if (opcode != W0)
          {
            packet_word = get_pkt_signed_num(&start, end, opcode - W1);
            packet_w = sqxfw(ptsize, packet_word);
          }

          ng_adjust_hpos(&packet_h, &packet_v, packet_w);
          break;

        case X0:
        case X1:
        case X2:
        case X3:
        case X4:
          if (opcode != X0)
          {
            packet_word = get_pkt_signed_num(&start, end, opcode - X1);
            packet_x = sqxfw(ptsize, packet_word);
          }

          ng_adjust_hpos(&packet_h, &packet_v, packet_x);
          break;

        case DOWN1:
        case DOWN2:
        case DOWN3:
        case DOWN4:
          packet_word = get_pkt_signed_num(&start, end, opcode - DOWN1);
          ng_adjust_vpos(&packet_h, &packet_v, sqxfw(ptsize, packet_word));
          break;

        case Y0:
        case Y1:
        case Y2:
        case Y3:
        case Y4:
          if (opcode != Y0)
          {
            packet_word =get_pkt_signed_num(&start, end, opcode - Y1);
            packet_y = sqxfw(ptsize, packet_word);
          }

          ng_adjust_vpos(&packet_h, &packet_v, packet_y);
          break;

        case Z0:
        case Z1:
        case Z2:
        case Z3:
        case Z4:
          if (opcode != Z0)
          {
            packet_word = get_pkt_signed_num(&start, end, opcode - Z1);
            packet_z = sqxfw(ptsize, packet_word);
          }

          ng_adjust_vpos(&packet_h, &packet_v, packet_z);
          break;

        case FNT1:
        case FNT2:
        case FNT3:
        case FNT4:
          packet_word = get_pkt_unsigned_num(&start, end, opcode - FNT1);
          packet_font = ng_packet_font(packet_word, vf_font);
          break;

        case XXX1:
        case XXX2:
        case XXX3:
        case XXX4:
          packet_word = get_pkt_unsigned_num(&start, end, opcode - XXX1);
          ng_special_out(packet_word, &start, end, packet_h, packet_v);
          break;

        case PTEXDIR:
          packet_word = unsigned_byte(&start, end);
          break;

        default:
          if (opcode <= SET_CHAR_127)
            ng_set(opcode, packet_font, packet_h, packet_v),
            ng_adjust_hpos(&packet_h, &packet_v, ng_packet_width(opcode, packet_font));
          else if (opcode >= FNT_NUM_0 && opcode <= FNT_NUM_63)
            packet_font = ng_packet_font(opcode - FNT_NUM_0, vf_font);
          else
          {
            fprintf(stderr, "Unexpected opcode: %d\n", opcode);
            ERROR("Unexpected opcode in vf file\n");
          }
      }
    }
  }
  else
  {
    fprintf(stderr, "ng_set_packet: font: %d", vf_font);
    ERROR("Font not loaded\n");
  }
}
