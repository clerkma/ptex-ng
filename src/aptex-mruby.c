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
#include <kpathsea/expand.h>
#include <kpathsea/progname.h>
#include <kpathsea/proginit.h>
#include <kpathsea/readable.h>
#include <kpathsea/tex-file.h>
#include <kpathsea/tex-hush.h>
#include <kpathsea/tex-make.h>
#include <kpathsea/lib.h>
#include <kpathsea/variable.h>

#include <ptexenc/ptexenc.h>
#include <ptexenc/unicode.h>

static mrb_value mrb_kpse_brace_expand (mrb_state * mrb, mrb_value self)
{
  char * path;
  mrb_get_args(mrb, "z!", &path);
  return mrb_str_new_cstr(mrb, kpse_brace_expand(path));
}

static mrb_value mrb_kpse_path_expand (mrb_state * mrb, mrb_value self)
{
  char * path;
  mrb_get_args(mrb, "z!", &path);
  return mrb_str_new_cstr(mrb, kpse_path_expand(path));
}

static mrb_value mrb_kpse_xputenv (mrb_state * mrb, mrb_value self)
{
  char * var;
  char * value;
  mrb_get_args(mrb, "z!z!", &var, &value);
  xputenv(var, value);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_xputenv_int (mrb_state * mrb, mrb_value self)
{
  char * var;
  mrb_int value;
  mrb_get_args(mrb, "z!i", &var, &value);
  xputenv_int(var, value);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_init_prog (mrb_state * mrb, mrb_value self)
{
  char * prefix;
  mrb_int dpi;
  char * mode;
  char * fallback;
  mrb_get_args(mrb, "z!iz!z!", &prefix, &dpi, &mode, &fallback);
  kpse_init_prog(prefix, dpi, mode, fallback);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_readable_file (mrb_state * mrb, mrb_value self)
{
  char * name;
  mrb_get_args(mrb, "z!", &name);
  return mrb_str_new_cstr(mrb, kpse_readable_file(name));
}

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

static mrb_value mrb_kpse_set_program_enabled (mrb_state * mrb, mrb_value self)
{
  mrb_int fmt;
  mrb_bool value;
  mrb_int level;
  mrb_get_args(mrb, "ibi", &fmt, &value, &level);
  kpse_set_program_enabled(fmt, value, level);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_maketex_option (mrb_state * mrb, mrb_value self)
{
  char * fmtname;
  mrb_bool value;
  mrb_get_args(mrb, "z!b", &fmtname, &value);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_init_format (mrb_state * mrb, mrb_value self)
{
  mrb_int fmt;
  mrb_get_args(mrb, "i", &fmt);
  return mrb_str_new_cstr(mrb, kpse_init_format(fmt));
}

static mrb_value mrb_kpse_find_file (mrb_state * mrb, mrb_value self)
{
  char * name;
  mrb_int format;
  mrb_bool must_exist;
  mrb_get_args(mrb, "z!ib", &name, &format, &must_exist);
  return mrb_str_new_cstr(mrb, kpse_find_file(name, format, must_exist));
}

static mrb_value mrb_kpse_in_name_ok (mrb_state * mrb, mrb_value self)
{
  char * fname;
  mrb_get_args(mrb, "z!", &fname);
  return mrb_bool_value(kpse_in_name_ok(fname));
}

static mrb_value mrb_kpse_out_name_ok (mrb_state * mrb, mrb_value self)
{
  char * fname;
  mrb_get_args(mrb, "z!", &fname);
  return mrb_bool_value(kpse_out_name_ok(fname));
}

static mrb_value mrb_kpse_reset_program_name (mrb_state * mrb, mrb_value self)
{
  char * progname;
  mrb_get_args(mrb, "z!", &progname);
  kpse_reset_program_name(progname);
  return mrb_nil_value();
}

static mrb_value mrb_kpse_tex_hush (mrb_state * mrb, mrb_value self)
{
  char * what;
  mrb_get_args(mrb, "z!", &what);
  return mrb_bool_value(kpse_tex_hush(what));
}

static mrb_value mrb_kpse_make_tex (mrb_state * mrb, mrb_value self)
{
  mrb_int format;
  char * base_file;
  mrb_get_args(mrb, "iz!", &format, &base_file);
  return mrb_str_new_cstr(mrb, kpse_make_tex(format, base_file));
}

static void mrb_mruby_kpathsea_gem_init (mrb_state * mrb)
{
  struct RClass * klass = mrb_define_module(mrb, "Kpathsea");
  /* types.h, kpse_file_format_type */
  mrb_define_const(mrb, klass, "GF_FORMAT", mrb_fixnum_value(kpse_gf_format));
  mrb_define_const(mrb, klass, "PK_FORMAT", mrb_fixnum_value(kpse_pk_format));
  mrb_define_const(mrb, klass, "ANY_GLYPH_FORMAT", mrb_fixnum_value(kpse_any_glyph_format));
  mrb_define_const(mrb, klass, "TFM_FORMAT", mrb_fixnum_value(kpse_tfm_format));
  mrb_define_const(mrb, klass, "AFM_FORMAT", mrb_fixnum_value(kpse_afm_format));
  mrb_define_const(mrb, klass, "BASE_FORMAT", mrb_fixnum_value(kpse_base_format));
  mrb_define_const(mrb, klass, "BIB_FORMAT", mrb_fixnum_value(kpse_bib_format));
  mrb_define_const(mrb, klass, "BST_FORMAT", mrb_fixnum_value(kpse_bst_format));
  mrb_define_const(mrb, klass, "CNF_FORMAT", mrb_fixnum_value(kpse_cnf_format));
  mrb_define_const(mrb, klass, "DB_FORMAT", mrb_fixnum_value(kpse_db_format));
  mrb_define_const(mrb, klass, "FMT_FORMAT", mrb_fixnum_value(kpse_fmt_format));
  mrb_define_const(mrb, klass, "FONTMAP_FORMAT", mrb_fixnum_value(kpse_fontmap_format));
  mrb_define_const(mrb, klass, "MEM_FORMAT", mrb_fixnum_value(kpse_mem_format));
  mrb_define_const(mrb, klass, "MF_FORMAT", mrb_fixnum_value(kpse_mf_format));
  mrb_define_const(mrb, klass, "MFPOOL_FORMAT", mrb_fixnum_value(kpse_mfpool_format));
  mrb_define_const(mrb, klass, "MFT_FORMAT", mrb_fixnum_value(kpse_mft_format));
  mrb_define_const(mrb, klass, "MP_FORMAT", mrb_fixnum_value(kpse_mp_format));
  mrb_define_const(mrb, klass, "MPPOOL_FORMAT", mrb_fixnum_value(kpse_mppool_format));
  mrb_define_const(mrb, klass, "MPSUPPORT_FORMAT", mrb_fixnum_value(kpse_mpsupport_format));
  mrb_define_const(mrb, klass, "OCP_FORMAT", mrb_fixnum_value(kpse_ocp_format));
  mrb_define_const(mrb, klass, "OFM_FORMAT", mrb_fixnum_value(kpse_ofm_format));
  mrb_define_const(mrb, klass, "OPL_FORMAT", mrb_fixnum_value(kpse_opl_format));
  mrb_define_const(mrb, klass, "OTP_FORMAT", mrb_fixnum_value(kpse_otp_format));
  mrb_define_const(mrb, klass, "OVF_FORMAT", mrb_fixnum_value(kpse_ovf_format));
  mrb_define_const(mrb, klass, "OVP_FORMAT", mrb_fixnum_value(kpse_ovp_format));
  mrb_define_const(mrb, klass, "PICT_FORMAT", mrb_fixnum_value(kpse_pict_format));
  mrb_define_const(mrb, klass, "TEX_FORMAT", mrb_fixnum_value(kpse_tex_format));
  mrb_define_const(mrb, klass, "TEXDOC_FORMAT", mrb_fixnum_value(kpse_texdoc_format));
  mrb_define_const(mrb, klass, "TEXPOOL_FORMAT", mrb_fixnum_value(kpse_texpool_format));
  mrb_define_const(mrb, klass, "TEXSOURCE_FORMAT", mrb_fixnum_value(kpse_texsource_format));
  mrb_define_const(mrb, klass, "TEX_PS_HEADER_FORMAT", mrb_fixnum_value(kpse_tex_ps_header_format));
  mrb_define_const(mrb, klass, "TROFF_FONT_FORMAT", mrb_fixnum_value(kpse_troff_font_format));
  mrb_define_const(mrb, klass, "TYPE1_FORMAT", mrb_fixnum_value(kpse_type1_format));
  mrb_define_const(mrb, klass, "VF_FORMAT", mrb_fixnum_value(kpse_vf_format));
  mrb_define_const(mrb, klass, "DVIPS_CONFIG_FORMAT", mrb_fixnum_value(kpse_dvips_config_format));
  mrb_define_const(mrb, klass, "IST_FORMAT", mrb_fixnum_value(kpse_ist_format));
  mrb_define_const(mrb, klass, "TRUETYPE_FORMAT", mrb_fixnum_value(kpse_truetype_format));
  mrb_define_const(mrb, klass, "TYPE42_FORMAT", mrb_fixnum_value(kpse_type42_format));
  mrb_define_const(mrb, klass, "WEB2C_FORMAT", mrb_fixnum_value(kpse_web2c_format));
  mrb_define_const(mrb, klass, "PROGRAM_TEXT_FORMAT", mrb_fixnum_value(kpse_program_text_format));
  mrb_define_const(mrb, klass, "PROGRAM_BINARY_FORMAT", mrb_fixnum_value(kpse_program_binary_format));
  mrb_define_const(mrb, klass, "MISCFONTS_FORMAT", mrb_fixnum_value(kpse_miscfonts_format));
  mrb_define_const(mrb, klass, "WEB_FORMAT", mrb_fixnum_value(kpse_web_format));
  mrb_define_const(mrb, klass, "CWEB_FORMAT", mrb_fixnum_value(kpse_cweb_format));
  mrb_define_const(mrb, klass, "ENC_FORMAT", mrb_fixnum_value(kpse_enc_format));
  mrb_define_const(mrb, klass, "CMAP_FORMAT", mrb_fixnum_value(kpse_cmap_format));
  mrb_define_const(mrb, klass, "SFD_FORMAT", mrb_fixnum_value(kpse_sfd_format));
  mrb_define_const(mrb, klass, "OPENTYPE_FORMAT", mrb_fixnum_value(kpse_opentype_format));
  mrb_define_const(mrb, klass, "PDFTEX_CONFIG_FORMAT", mrb_fixnum_value(kpse_pdftex_config_format));
  mrb_define_const(mrb, klass, "LIG_FORMAT", mrb_fixnum_value(kpse_lig_format));
  mrb_define_const(mrb, klass, "TEXMFSCRIPTS_FORMAT", mrb_fixnum_value(kpse_texmfscripts_format));
  mrb_define_const(mrb, klass, "LUA_FORMAT", mrb_fixnum_value(kpse_lua_format));
  mrb_define_const(mrb, klass, "FEA_FORMAT", mrb_fixnum_value(kpse_fea_format));
  mrb_define_const(mrb, klass, "CID_FORMAT", mrb_fixnum_value(kpse_cid_format));
  mrb_define_const(mrb, klass, "MLBIB_FORMAT", mrb_fixnum_value(kpse_mlbib_format));
  mrb_define_const(mrb, klass, "MLBST_FORMAT", mrb_fixnum_value(kpse_mlbst_format));
  mrb_define_const(mrb, klass, "CLUA_FORMAT", mrb_fixnum_value(kpse_clua_format));
  mrb_define_const(mrb, klass, "RIS_FORMAT", mrb_fixnum_value(kpse_ris_format));
  mrb_define_const(mrb, klass, "BLTXML_FORMAT", mrb_fixnum_value(kpse_bltxml_format));
  mrb_define_const(mrb, klass, "LAST_FORMAT", mrb_fixnum_value(kpse_last_format));
  /* types.h, kpse_src_type */
  mrb_define_const(mrb, klass, "SRC_IMPLICIT", mrb_fixnum_value(kpse_src_implicit));
  mrb_define_const(mrb, klass, "SRC_COMPILE", mrb_fixnum_value(kpse_src_compile));
  mrb_define_const(mrb, klass, "SRC_TEXMF_CNF", mrb_fixnum_value(kpse_src_texmf_cnf));
  mrb_define_const(mrb, klass, "SRC_CLIENT_CNF", mrb_fixnum_value(kpse_src_client_cnf));
  mrb_define_const(mrb, klass, "SRC_ENV", mrb_fixnum_value(kpse_src_env));
  mrb_define_const(mrb, klass, "SRC_X", mrb_fixnum_value(kpse_src_x));
  mrb_define_const(mrb, klass, "SRC_CMDLINE", mrb_fixnum_value(kpse_src_cmdline));
  /* expand.h */
  mrb_define_class_method(mrb, klass, "brace_expand", mrb_kpse_brace_expand, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "path_expand", mrb_kpse_path_expand, MRB_ARGS_REQ(1));
  /* lib.h */
  mrb_define_class_method(mrb, klass, "xputenv", mrb_kpse_xputenv, MRB_ARGS_REQ(2));
  mrb_define_class_method(mrb, klass, "xputenv_int", mrb_kpse_xputenv_int, MRB_ARGS_REQ(2));
  /* proginit.h */
  mrb_define_class_method(mrb, klass, "init_prog", mrb_kpse_init_prog, MRB_ARGS_REQ(4));
  /* progname.h */
  mrb_define_class_method(mrb, klass, "set_program_name", mrb_kpse_set_program_name, MRB_ARGS_REQ(2));
  /* readable.h */
  mrb_define_class_method(mrb, klass, "readable_file", mrb_kpse_readable_file, MRB_ARGS_REQ(1));
  /* tex-file.h */
  mrb_define_class_method(mrb, klass, "set_program_enabled", mrb_kpse_set_program_enabled, MRB_ARGS_REQ(3));
  mrb_define_class_method(mrb, klass, "maketex_option", mrb_kpse_maketex_option, MRB_ARGS_REQ(2));
  mrb_define_class_method(mrb, klass, "init_format", mrb_kpse_init_format, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "in_name_ok", mrb_kpse_in_name_ok, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "out_name_ok", mrb_kpse_out_name_ok, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "find_file", mrb_kpse_find_file, MRB_ARGS_REQ(3));
  mrb_define_class_method(mrb, klass, "reset_program_name", mrb_kpse_reset_program_name, MRB_ARGS_REQ(1));
  /* tex-make.h */
  mrb_define_class_method(mrb, klass, "tex_hush", mrb_kpse_tex_hush, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "make_tex", mrb_kpse_make_tex, MRB_ARGS_REQ(2));
  /* variable.h */
  mrb_define_class_method(mrb, klass, "var_value", mrb_kpse_var_value, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "var_expand", mrb_kpse_var_expand, MRB_ARGS_REQ(1));
}

static void mrb_mruby_kpathsea_gem_final (mrb_state * mrb)
{
}

static mrb_value mrb_ptexenc_enable_UPTEX (mrb_state * mrb, mrb_value self)
{
  mrb_bool enable;
  mrb_get_args(mrb, "b", &enable);
  return mrb_nil_value();
}

static mrb_value mrb_ptexenc_get_enc_string (mrb_state * mrb, mrb_value self)
{
  return mrb_str_new_cstr(mrb, get_enc_string());
}

static mrb_value mrb_ptexenc_set_enc_string (mrb_state * mrb, mrb_value self)
{
  char * file;
  char * inter;
  mrb_get_args(mrb, "z!z!", &file, &inter);
  return mrb_bool_value(set_enc_string(file, inter));
}

static mrb_value mrb_ptexenc_toDVI (mrb_state * mrb, mrb_value self)
{
  mrb_int kcode;
  mrb_get_args(mrb, "i", &kcode);
  return mrb_fixnum_value(toDVI(kcode));
}

static mrb_value mrb_ptexenc_fromDVI (mrb_state * mrb, mrb_value self)
{
  mrb_int kcode;
  mrb_get_args(mrb, "i", &kcode);
  return mrb_fixnum_value(fromDVI(kcode));
}

static mrb_value mrb_ptexenc_toJIS (mrb_state * mrb, mrb_value self)
{
  mrb_int kcode;
  mrb_get_args(mrb, "i", &kcode);
  return mrb_fixnum_value(toJIS(kcode));
}

static mrb_value mrb_ptexenc_fromJIS (mrb_state * mrb, mrb_value self)
{
  mrb_int jis;
  mrb_get_args(mrb, "i", &jis);
  return mrb_fixnum_value(fromJIS(jis));
}

static mrb_value mrb_ptexenc_fromEUC (mrb_state * mrb, mrb_value self)
{
  mrb_int euc;
  mrb_get_args(mrb, "i", &euc);
  return mrb_fixnum_value(fromEUC(euc));
}

static mrb_value mrb_ptexenc_fromSJIS (mrb_state * mrb, mrb_value self)
{
  mrb_int sjis;
  mrb_get_args(mrb, "i", &sjis);
  return mrb_fixnum_value(fromSJIS(sjis));
}

static mrb_value mrb_ptexenc_fromKUTEN (mrb_state * mrb, mrb_value self)
{
  mrb_int kuten;
  mrb_get_args(mrb, "i", &kuten);
  return mrb_fixnum_value(fromKUTEN(kuten));
}

static mrb_value mrb_ptexenc_fromUCS (mrb_state * mrb, mrb_value self)
{
  mrb_int ucs;
  mrb_get_args(mrb, "i", &ucs);
  return mrb_fixnum_value(fromUCS(ucs));
}

static mrb_value mrb_ptexenc_toUCS (mrb_state * mrb, mrb_value self)
{
  mrb_int kcode;
  mrb_get_args(mrb, "i", &kcode);
  return mrb_fixnum_value(toUCS(kcode));
}

static mrb_value mrb_ptexenc_UCStoUTF8 (mrb_state * mrb, mrb_value self)
{
  mrb_int ucs;
  mrb_get_args(mrb, "i", &ucs);
  return mrb_fixnum_value(UCStoUTF8(ucs));
}

static void mrb_mruby_ptexenc_gem_init (mrb_state * mrb)
{
  struct RClass * klass = mrb_define_module(mrb, "PTEXENC");
  mrb_define_class_method(mrb, klass, "enable_UPTEX", mrb_ptexenc_enable_UPTEX, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "get_enc_string", mrb_ptexenc_get_enc_string, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, klass, "set_enc_string", mrb_ptexenc_set_enc_string, MRB_ARGS_REQ(2));
  mrb_define_class_method(mrb, klass, "toDVI", mrb_ptexenc_toDVI, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "fromDVI", mrb_ptexenc_fromDVI, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "toJIS", mrb_ptexenc_toJIS, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "fromJIS", mrb_ptexenc_fromJIS, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "fromEUC", mrb_ptexenc_fromEUC, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "fromSJIS", mrb_ptexenc_fromSJIS, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "fromKUTEN", mrb_ptexenc_fromKUTEN, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "fromUCS", mrb_ptexenc_fromUCS, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "toUCS", mrb_ptexenc_toUCS, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, klass, "UCStoUTF8", mrb_ptexenc_UCStoUTF8, MRB_ARGS_REQ(1));
}

static void mrb_mruby_ptexenc_gem_final (mrb_state * mrb)
{
}

void mrb_mruby_aptex_gem_init (mrb_state * mrb)
{
  mrb_mruby_kpathsea_gem_init(mrb);
  mrb_mruby_ptexenc_gem_init(mrb);
}
void mrb_mruby_aptex_gem_final (mrb_state * mrb)
{
  mrb_mruby_kpathsea_gem_final(mrb);
  mrb_mruby_ptexenc_gem_final(mrb);
}
