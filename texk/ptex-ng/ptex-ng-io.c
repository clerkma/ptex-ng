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

  patch_in_path(buffer, name, (ASCII_code *) path);

  return true;
}

static inline boolean check_path_sep (ASCII_code c)
{
#ifdef _WIN32
  return (c == DIR_SEP || c == '\\');
#else
  return (c == DIR_SEP);
#endif
}

boolean open_input (FILE ** f, kpse_file_format_type file_fmt, const char * fopen_mode)
{
  boolean openable = false;
  boolean must_exist;
  char * file_name = NULL;

  if (strcmp(fopen_mode, "r") == 0)
    fopen_mode = "rb";

  name_of_file[name_length + 1] = '\0';
  
  if (flag_open_trace)
    printf(" Open `%s' for input ", name_of_file + 1);

  must_exist = (file_fmt != kpse_tex_format);
  file_name = kpse_find_file((const_string) name_of_file + 1, file_fmt, must_exist);

  if (file_name != NULL)
  {
    strcpy ((char *) name_of_file + 1, file_name);
    *f = fopen((char *) file_name, fopen_mode);

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
      
    if (file_fmt == kpse_tfm_format)
    {
      fbyte = getc(*f);
    } 

    if (strstr((char *) name_of_file + 1, ".fmt") != NULL)
    {
#ifdef COMPACTFORMAT
      gz_fmt_file = gzdopen(fileno(*f), FOPEN_RBIN_MODE);
#endif
    }

    openable = true;
  }

  {
    size_t temp_length = strlen((char *) name_of_file + 1);
    name_of_file[temp_length + 1] = ' ';
  }

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

boolean open_output (FILE ** f, const char * fopen_mode)
{
  size_t temp_length;

  name_of_file[name_length + 1] = '\0';

  if (prepend_path_if(name_of_file + 1, name_of_file + 1, ".dvi", dvi_directory) ||
      prepend_path_if(name_of_file + 1, name_of_file + 1, ".log", log_directory) ||
      prepend_path_if(name_of_file + 1, name_of_file + 1, ".aux", aux_directory) ||
      prepend_path_if(name_of_file + 1, name_of_file + 1, ".fmt", fmt_directory) ||
      prepend_path_if(name_of_file + 1, name_of_file + 1, ".pdf", pdf_directory))
  {
    if (flag_open_trace)
      printf("After prepend %s\n", name_of_file + 1);
  }

  if (flag_open_trace)
    printf(" Open `%s' for output ", name_of_file + 1);

  *f = fopen((char *) name_of_file + 1, fopen_mode);

  if (*f == NULL)
  {
    string temp_dir = kpse_var_value("TEXMFOUTPUT");

    if (temp_dir != NULL)
    {
      unsigned char temp_name[file_name_size];
      xconcat3((char *) temp_name, temp_dir, DIR_SEP_STRING, (char *) name_of_file + 1);

      if (flag_deslash)
        unixify((char *) temp_name);
      
      *f = fopen((char *) temp_name, fopen_mode);

      if (*f)
        strcpy((char *) name_of_file + 1, (char *) temp_name);
    }
  }

#ifdef COMPACTFORMAT
  if (strstr((char *) name_of_file + 1, ".fmt") != NULL)
    gz_fmt_file = gzdopen(fileno(*f), FOPEN_WBIN_MODE);
#endif

  if (strstr((char *) name_of_file + 1, ".dvi") != NULL)
    dvi_file_name = xstrdup_name();
  else if (strstr((char *) name_of_file + 1, ".pdf") != NULL)
    pdf_file_name = xstrdup_name();
  else if (strstr((char *) name_of_file + 1, ".log") != NULL)
    log_file_name = xstrdup_name();

  temp_length = strlen((char *) name_of_file + 1);
  name_of_file[temp_length + 1] = ' ';

  if (*f)
    name_length = temp_length;
  
  return (*f != NULL);
}
