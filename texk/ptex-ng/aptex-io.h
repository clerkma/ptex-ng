/*
  Copyright (c) 2015 Clerk Ma

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE

  Except as contained in this notice, the name of the copyright holder
  shall not be used in advertising or otherwise to promote the sale,
  use or other dealings in this Software without prior written
  authorization from the copyright holder.
*/

#ifndef APTEX_IO
#define APTEX_IO

extern boolean input_ln (FILE * f, boolean bypass_eoln);
extern boolean open_input  (void ** f, kpse_file_format_type file_fmt, const char * fopen_mode);
extern boolean open_output (void ** f, kpse_file_format_type file_fmt, const char * fopen_mode);
extern void    close_file  (FILE * f);

#define a_open_in(f)    open_input  ((void **) &(f), kpse_tex_format, FOPEN_R_MODE)
#define a_open_out(f)   open_output ((void **) &(f), kpse_tex_format, FOPEN_W_MODE)
#define b_open_in(f)    open_input  ((void **) &(f), kpse_tfm_format, FOPEN_RBIN_MODE)
#define b_open_out(f)   open_output ((void **) &(f), kpse_tfm_format, FOPEN_WBIN_MODE)
#define w_open_in(f)    open_input  ((void **) &(f), kpse_fmt_format, FOPEN_RBIN_MODE)
#define w_open_out(f)   open_output ((void **) &(f), kpse_fmt_format, FOPEN_WBIN_MODE)
#define a_close(f)      close_file(f)
#define b_close(f)      a_close(f)
#define w_close(f)      gzclose(fmt_file)
#define w_eof(f)        (aptex_env.flag_compact_fmt == true ? gzeof((gzFile) f) : feof((FILE *) f))

#endif /* APTEX_IO */
