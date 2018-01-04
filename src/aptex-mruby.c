/*
   Copyright 2017, 2018 Clerk Ma
 
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

#include "mruby.h"
#include <kpathsea/progname.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/variable.h>

static mrb_value mrb_kpse_set_program_name (mrb_state * mrb, mrb_value self)
{
  char * argv0, * program;
  mrb_get_args(mrb, "z!z!", &argv0, &program);
  kpse_set_program_name((const string) argv0, (const string) program);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_var_value (mrb_state * mrb, mrb_value self)
{
  char * var;
  mrb_get_args(mrb, "z!", &var);
  return mrb_str_new_cstr(mrb, kpse_var_value((const string) var));
}

static mrb_value mrb_kpse_var_expand (mrb_state * mrb, mrb_value self)
{
  char * src;
  mrb_get_args(mrb, "z!", &src);
  return mrb_str_new_cstr(mrb, kpse_var_expand((const string) src));
}

static mrb_value mrb_kpse_find_file (mrb_state * mrb, mrb_value self)
{
  char * name;
  mrb_int format;
  mrb_bool must_exist;
  printf(">>>");
  mrb_get_args(mrb, "z!ib", &name, &format, &must_exist);
  return mrb_str_new_cstr(mrb, kpse_find_file(name, format, must_exist));
}

void mrb_mruby_aptex_gem_init (mrb_state * mrb)
{
  struct RClass * klass = mrb_define_module(mrb, "Kpathsea");
  /* types.h */
  mrb_define_const(mrb, klass, "GF", mrb_fixnum_value(0));
  mrb_define_const(mrb, klass, "PK", mrb_fixnum_value(1));
  mrb_define_const(mrb, klass, "ANY_GLYPH", mrb_fixnum_value(2));
  mrb_define_const(mrb, klass, "TFM", mrb_fixnum_value(3));
  mrb_define_const(mrb, klass, "AFM", mrb_fixnum_value(4));
  mrb_define_const(mrb, klass, "BASE", mrb_fixnum_value(5));
  mrb_define_const(mrb, klass, "BIB", mrb_fixnum_value(6));
  mrb_define_const(mrb, klass, "BST", mrb_fixnum_value(7));
  mrb_define_const(mrb, klass, "CNF", mrb_fixnum_value(8));
  mrb_define_const(mrb, klass, "DB", mrb_fixnum_value(9));
  mrb_define_const(mrb, klass, "FMT", mrb_fixnum_value(10));
  mrb_define_const(mrb, klass, "FONTMAP", mrb_fixnum_value(11));
  mrb_define_const(mrb, klass, "MEM", mrb_fixnum_value(12));
  mrb_define_const(mrb, klass, "MF", mrb_fixnum_value(13));
  mrb_define_const(mrb, klass, "MFPOOL", mrb_fixnum_value(14));
  mrb_define_const(mrb, klass, "MFT", mrb_fixnum_value(15));
  mrb_define_const(mrb, klass, "MP", mrb_fixnum_value(16));
  mrb_define_const(mrb, klass, "MPPOOL", mrb_fixnum_value(17));
  mrb_define_const(mrb, klass, "MPSUPPORT", mrb_fixnum_value(18));
  mrb_define_const(mrb, klass, "OCP", mrb_fixnum_value(19));
  mrb_define_const(mrb, klass, "OFM", mrb_fixnum_value(20));
  mrb_define_const(mrb, klass, "OPL", mrb_fixnum_value(21));
  mrb_define_const(mrb, klass, "OTP", mrb_fixnum_value(22));
  mrb_define_const(mrb, klass, "OVF", mrb_fixnum_value(23));
  mrb_define_const(mrb, klass, "OVP", mrb_fixnum_value(24));
  mrb_define_const(mrb, klass, "PICT", mrb_fixnum_value(25));
  mrb_define_const(mrb, klass, "TEX", mrb_fixnum_value(26));
  mrb_define_const(mrb, klass, "TEXDOC", mrb_fixnum_value(27));
  mrb_define_const(mrb, klass, "TEXPOOL", mrb_fixnum_value(28));
  mrb_define_const(mrb, klass, "TEXSOURCE", mrb_fixnum_value(29));
  mrb_define_const(mrb, klass, "TEX_PS_HEADER", mrb_fixnum_value(30));
  mrb_define_const(mrb, klass, "TROFF_FONT", mrb_fixnum_value(31));
  mrb_define_const(mrb, klass, "TYPE1", mrb_fixnum_value(32));
  mrb_define_const(mrb, klass, "VF", mrb_fixnum_value(33));
  mrb_define_const(mrb, klass, "DVIPS_CONFIG", mrb_fixnum_value(34));
  mrb_define_const(mrb, klass, "IST", mrb_fixnum_value(35));
  mrb_define_const(mrb, klass, "TRUETYPE", mrb_fixnum_value(36));
  mrb_define_const(mrb, klass, "TYPE42", mrb_fixnum_value(37));
  mrb_define_const(mrb, klass, "WEB2C", mrb_fixnum_value(38));
  mrb_define_const(mrb, klass, "PROGRAM_TEXT", mrb_fixnum_value(39));
  mrb_define_const(mrb, klass, "PROGRAM_BINARY", mrb_fixnum_value(40));
  mrb_define_const(mrb, klass, "MISCFONTS", mrb_fixnum_value(41));
  mrb_define_const(mrb, klass, "WEB", mrb_fixnum_value(42));
  mrb_define_const(mrb, klass, "CWEB", mrb_fixnum_value(43));
  mrb_define_const(mrb, klass, "ENC", mrb_fixnum_value(44));
  mrb_define_const(mrb, klass, "CMAP", mrb_fixnum_value(45));
  mrb_define_const(mrb, klass, "SFD", mrb_fixnum_value(46));
  mrb_define_const(mrb, klass, "OPENTYPE", mrb_fixnum_value(47));
  mrb_define_const(mrb, klass, "PDFTEX_CONFIG", mrb_fixnum_value(48));
  mrb_define_const(mrb, klass, "LIG", mrb_fixnum_value(49));
  mrb_define_const(mrb, klass, "TEXMFSCRIPTS", mrb_fixnum_value(50));
  mrb_define_const(mrb, klass, "LUA", mrb_fixnum_value(51));
  mrb_define_const(mrb, klass, "FEA", mrb_fixnum_value(52));
  mrb_define_const(mrb, klass, "CID", mrb_fixnum_value(53));
  mrb_define_const(mrb, klass, "MLBIB", mrb_fixnum_value(54));
  mrb_define_const(mrb, klass, "MLBST", mrb_fixnum_value(55));
  mrb_define_const(mrb, klass, "CLUA", mrb_fixnum_value(56));
  mrb_define_const(mrb, klass, "RIS", mrb_fixnum_value(57));
  mrb_define_const(mrb, klass, "BLTXML", mrb_fixnum_value(58));
  mrb_define_const(mrb, klass, "LAST", mrb_fixnum_value(59));
  /* progname.h */
  mrb_define_class_method(mrb, klass, "set_program_name", mrb_kpse_set_program_name, MRB_ARGS_REQ(2));
  /* tex-file.h */
  mrb_define_class_method(mrb, klass, "find_file", mrb_kpse_find_file, MRB_ARGS_REQ(3));
  /* variable.h */
  mrb_define_class_method(mrb, klass, "var_value", mrb_kpse_var_value, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "var_expand", mrb_kpse_var_expand, MRB_ARGS_REQ(1));
}

void mrb_mruby_aptex_gem_final (mrb_state * mrb)
{
}
