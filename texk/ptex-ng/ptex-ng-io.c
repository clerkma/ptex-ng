/*
   Copyright 1992 Karl Berry
   Copyright 2007 TeX Users Group
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

#define EXTERN extern

#include "ptex-ng.h"
#undef name

char * mbcs_utf8 (const char * mbcs_str)
{
#ifdef WIN32
  int    utf8_len;
  int    utf16_len;
  size_t mbcs_len;
  LPWSTR utf16_str;
  char * utf8_str;
  int    codepage;

  mbcs_len = strlen(mbcs_str);
  codepage = AreFileApisANSI() ? CP_ACP : CP_OEMCP;
  utf16_len = MultiByteToWideChar(codepage, 0, mbcs_str, -1, NULL, 0);

  if (utf16_len == 0)
    return 0;

  utf16_str = (LPWSTR) malloc((utf16_len + 1) * sizeof(utf16_str[0]));

  if (utf16_str == NULL)
    return NULL;

  MultiByteToWideChar(codepage, 0, mbcs_str, -1, utf16_str, utf16_len);
  utf8_len = WideCharToMultiByte(CP_UTF8, 0, utf16_str, utf16_len, 0, 0, 0, 0);
  utf8_str = utf8_len ? (char*) malloc(utf8_len + 1) : 0;

  if (utf8_str)
  {
    WideCharToMultiByte(CP_UTF8, 0, utf16_str, utf16_len, utf8_str, utf8_len, 0, 0);
  }

  free(utf16_str);

  return utf8_str;
#else
  return xstrdup(mbcs_str);
#endif
}

char * utf8_mbcs (const char * utf8_str)
{
#ifdef WIN32
  size_t utf8_len;
  int    utf16_len;
  int    mbcs_len;
  LPWSTR utf16_str;
  char * mbcs_str;
  int    codepage;

  utf8_len = strlen(utf8_str);
  utf16_len = MultiByteToWideChar(CP_UTF8, 0, utf8_str, -1, NULL, 0);

  if (utf16_len == 0)
    return 0;

  utf16_str = (LPWSTR) malloc((utf16_len + 1) * sizeof(utf16_str[0]));

  if (utf16_str == NULL)
    return NULL;

  MultiByteToWideChar(CP_UTF8, 0, utf8_str, -1, utf16_str, utf16_len);
  codepage = AreFileApisANSI() ? CP_ACP : CP_OEMCP;
  mbcs_len = WideCharToMultiByte(codepage, 0, utf16_str, utf16_len, 0, 0, 0, 0);
  mbcs_str = mbcs_len ? (char*) malloc(mbcs_len + 1) : 0;

  if (mbcs_str)
  {
    WideCharToMultiByte(codepage, 0, utf16_str, utf16_len, mbcs_str, mbcs_len, 0, 0);
  }

  free(utf16_str);

  return mbcs_str;
#else
  return xstrdup(utf8_str);
#endif  
}

static char * xconcat3 (char * buffer, char * s1, char * s2, char * s3)
{
  size_t n1 = strlen(s1);
  size_t n2 = strlen(s2);
  size_t n3 = strlen(s3);

  if (buffer == s3)
  {
    memmove(buffer + n1 + n2, buffer, n3 + 1);
    strncpy(buffer, s1, n1);
    strncpy(buffer + n1, s2, n2);
  }
  else
  {
    strcpy(buffer, s1);
    strcat(buffer + n1, s2);
    strcat(buffer + n1 + n2, s3);
  }

  return buffer;
}

static void patch_in_path (ASCII_code * buffer, ASCII_code * name, ASCII_code * path)
{
  if (*path == '\0')
    strcpy((char *) buffer, (char *) name);
  else
    xconcat3((char *) buffer, (char *) path, "/", (char *) name);
}

static boolean qualified (ASCII_code * name)
{
  if (strchr((char *) name, '/') != NULL ||
      strchr((char *) name, '\\') != NULL ||
      strchr((char *) name, ':') != NULL)
    return true;
  else
    return false;
}
/* patch path if 
    (i)   path not empty
    (ii)  name not qualified
    (iii) ext match
*/
static boolean prepend_path_if (ASCII_code * buffer, ASCII_code * name, const char * ext, char * path)
{
  if ((path == NULL) || (*path == '\0') || qualified(name) || (strstr((char *) name, ext) == NULL))
    return false;
  else
  {
    patch_in_path(buffer, name, (ASCII_code *) path);
    return true;
  }
}

static inline boolean check_path_sep (ASCII_code c)
{
#ifdef _WIN32
  return (c == DIR_SEP || c == '\\');
#else
  return (c == DIR_SEP);
#endif
}

static inline void strip_file_name (char * s)
{
  strcpy((char *) name_of_file + 1, s);

  if (name_of_file[1] == '.' && check_path_sep(name_of_file[2]))
  {
    unsigned i = 1;

    while (name_of_file[i + 2] != '\0')
    {
      name_of_file[i] = name_of_file[i + 2];
      i++;
    }

    name_of_file[i] = '\0';
    name_length = i - 1;
  }
  else
    name_length = strlen((char *) name_of_file + 1);
}

static void * (*open_file) (const char * f, const char * m);

static inline void set_open_file (kpse_file_format_type file_fmt)
{
  if ((file_fmt == kpse_fmt_format) && (flag_compact_fmt == true))
    open_file = (void * (*) (const char *, const char *)) &gzopen;
  else
    open_file = (void * (*) (const char *, const char *)) &fopen;
}

boolean open_input (void ** f, kpse_file_format_type file_fmt, const char * fopen_mode)
{
  boolean openable = false;
  boolean must_exist = (file_fmt != kpse_tex_format);
  char * file_name_kpse = NULL;
  char * file_name_mbcs = NULL;
  char * file_name_utf8 = NULL;

  set_open_file(file_fmt);

  if (strcmp(fopen_mode, "r") == 0)
    fopen_mode = "rb";

  name_of_file[name_length + 1] = '\0';
  file_name_mbcs = utf8_mbcs((const char *) name_of_file + 1);
  file_name_kpse = kpse_find_file((const_string) file_name_mbcs, file_fmt, must_exist);

  if (file_name_kpse != NULL)
  {
    file_name_utf8 = mbcs_utf8(file_name_kpse);
    *f = open_file((char *) file_name_kpse, fopen_mode);

    if (file_fmt == kpse_tfm_format)
      fbyte = getc((FILE *) *f);

    if (*f)
      strip_file_name(file_name_utf8);

    openable = true;
  }

  name_of_file[name_length + 1] = ' ';

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  if (file_name_kpse != NULL)
    free(file_name_kpse);

  return openable;
}

void close_file (FILE * f)
{
  if (f == NULL)
    return;

  if (ferror(f) || fclose (f))
  {
    perror("\n! I/O Error");
    uexit(EXIT_FAILURE);
  }
}

static char * xstrdup_name (void)
{
  if (qualified(name_of_file + 1))
    *log_line = '\0';
  else
  {
    (void) getcwd(log_line, sizeof(log_line));
    strcat(log_line, DIR_SEP_STRING);
  }

  strcat(log_line, (char *) name_of_file + 1);
  unixify(log_line);

  return xstrdup(log_line);
}

boolean open_output (void ** f, kpse_file_format_type file_fmt, const char * fopen_mode)
{
  char * file_name_utf8 = NULL;
  char * file_name_mbcs = NULL;

  name_of_file[name_length + 1] = '\0';

  prepend_path_if(name_of_file + 1, name_of_file + 1, ".dvi", dvi_directory);
  prepend_path_if(name_of_file + 1, name_of_file + 1, ".log", log_directory);
  prepend_path_if(name_of_file + 1, name_of_file + 1, ".aux", aux_directory);
  prepend_path_if(name_of_file + 1, name_of_file + 1, ".fmt", fmt_directory);
  prepend_path_if(name_of_file + 1, name_of_file + 1, ".pdf", pdf_directory);

  set_open_file(file_fmt);

  file_name_mbcs = utf8_mbcs ((const char *) name_of_file + 1);
  *f = open_file(file_name_mbcs, fopen_mode);

  if (*f == NULL)
  {
    string temp_dir = kpse_var_value("TEXMFOUTPUT");

    if (temp_dir != NULL)
    {
      unsigned char temp_name[file_name_size];
      xconcat3((char *) temp_name, temp_dir, DIR_SEP_STRING, file_name_mbcs);

      if (flag_deslash)
        unixify((char *) temp_name);
      
      *f = open_file((char *) temp_name, fopen_mode);

      if (*f)
      {
        file_name_utf8 = mbcs_utf8((char *) temp_name);
        strcpy((char *) name_of_file + 1, file_name_utf8);
        name_length = strlen((char *) name_of_file + 1);
      }
    }
  }

  if (strstr((char *) name_of_file + 1, ".fmt") != NULL)
    fmt_file_name = xstrdup_name();
  else if (strstr((char *) name_of_file + 1, ".dvi") != NULL)
    dvi_file_name = xstrdup_name();
  else if (strstr((char *) name_of_file + 1, ".pdf") != NULL)
    pdf_file_name = xstrdup_name();
  else if (strstr((char *) name_of_file + 1, ".log") != NULL)
    log_file_name = xstrdup_name();

  name_of_file[name_length + 1] = ' ';

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);
  
  return (*f != NULL);
}
