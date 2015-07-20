/*
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

#define EXTERN
#include "aptex.h"

static void aptex_set_signal (void);

void aptex_env_init (int argc, char ** argv)
{
#ifdef WIN32
  int i;

  aptex_env.argc = argc;
  aptex_env.argv = (char **) malloc(argc * sizeof(char *));

  for (i = 0; i < argc; i++)
    aptex_env.argv[i] = mbcs_utf8(argv[i]);

  _setmaxstdio(2048);
  setmode(fileno(stdin), _O_BINARY);
#else
  aptex_env.argc = argc;
  aptex_env.argv = argv;
#endif
}

#define safe_free(a)  \
do {                  \
  if (a != NULL)      \
    free(a);          \
  a = NULL;           \
} while (0)

void aptex_env_fini (void)
{
#ifdef WIN32
  int i;

  for (i = 0; i < aptex_env.argc; i++)
  {
    if (aptex_env.argv[i] != NULL)
      free(aptex_env.argv[i]);

    aptex_env.argv[i] = NULL;
  }

  free(aptex_env.argv);
#endif

  safe_free(aptex_env.aptex_fmt);
  safe_free(aptex_env.aptex_src);
  safe_free(aptex_env.aptex_job);
}

void aptex_set_fmt (char * fmt);
void aptex_set_src (char * src);
void aptex_set_job (char * src);

int main (int argc, char *argv[])
{
  aptex_env_init(argc, argv);
  aptex_set_signal();
  aptex_init();
  aptex_program();
  aptex_fini();
  aptex_env_fini();

  return 0;
}

void t_open_in (void)
{
  int i;

  buffer[first] = 0;

  if (aptex_env.argc > optind && optind > 0)
  {
    for (i = optind; i < aptex_env.argc; i++)
    {
      if (flag_allow_quoted && (strchr(aptex_env.argv[i], ' ') != NULL))
        sprintf((char *) &buffer[first], "\"%s\" ", aptex_env.argv[i]);
      else
        sprintf((char *) &buffer[first], "%s ", aptex_env.argv[i]);
    }
  }

  for (last = first; buffer[last]; ++last)
    do_nothing();

  for (--last; last >= first && ISBLANK(buffer[last]) && buffer[last] != '\r'; --last)
    do_nothing();

  last++;
}

static void catch_interrupt (int err)
{
  (void) err;
  (void) signal(SIGINT, SIG_IGN);

  if (interrupt++ >= 3)
    exit(EXIT_FAILURE);

  (void) signal(SIGINT, catch_interrupt);
}

static void aptex_set_signal (void)
{
#ifdef _WIN32
  if (signal(SIGINT, catch_interrupt) == SIG_ERR)
  {
    puts(" CTRL-C handler not installed");
    aptex_utils_exit(EXIT_FAILURE);
  }
#else
  void (*old_handler)();

  old_handler = signal(SIGINT, catch_interrupt);

  if (old_handler != SIG_DFL)
    (void) signal(SIGINT, old_handler);
#endif
}

void fix_date_and_time (void)
{
  time_t clock;
  struct tm * tm_ptr;

  if ((clock = time(NULL)) < 0)
    puts("Time not available!");

  tm_ptr = localtime(&clock);

  if (tm_ptr == NULL)
  {
    year     = 2038;
    month    = 1;
    day      = 18;
    tex_time = 22 * 60 + 14;
  }
  else
  {
    tex_time = tm_ptr->tm_hour * 60 + tm_ptr->tm_min;
    day      = tm_ptr->tm_mday;
    month    = tm_ptr->tm_mon + 1;
    year     = tm_ptr->tm_year + 1900;
  }
}

static inline void input_char (ASCII_code i)
{
  buffer[last++] = i;

#ifdef APTEX_EXTENSION
  if (last >= current_buf_size)
    buffer = realloc_buffer(increment_buf_size);
#endif
}

/* sec 0031 */
// inputs the next line or returns |false|
boolean input_ln (FILE * f, boolean bypass_eoln)
{
  int i = '\0';

  (void) bypass_eoln;
  last = first;

#ifdef APTEX_EXTENSION
  while (true)
#else
  while (last < buf_size)
#endif
  {
    i = fgetc(f);

    if ((i == EOF) || (i == '\n') || (i == '\r'))
      break;
    else switch (i)
    {
      case '\t':
        if (tab_step <= 0)
          input_char('\t');
        else
        {
          int j;

          for (j = 0; j < tab_step; j++)
            input_char(' ');
        }
        break;

      default:
        input_char(i);
        break;
    }
  }

  if (i == '\r')
  {
    i = fgetc(f);

    if (i != '\n')
    {
      ungetc(i, f);
      i = '\r';
    }
  }

  if (i == EOF && last == first)
    return false;

  buffer[last] = ' ';

  if (last >= max_buf_stack)
    max_buf_stack = last;

  while (last > first)
  {
    i = buffer[last - 1];

    if (i == ' ' || i == '\t')
      --last;
    else
      break;
  }

  return true;
}

int block_dump (char * p, int item_size, int nitems, void * out_file)
{
  boolean flag_fmt_failed;

  if (aptex_env.flag_compact_fmt)
    flag_fmt_failed = (gzwrite(out_file, p, (item_size * nitems)) != (item_size * nitems));
  else
    flag_fmt_failed = (fwrite(p, item_size, nitems, out_file) != nitems);

  if (flag_fmt_failed)
  {
    printf("\n! ApTeX: Could not write %d %d-byte item%s.\n", nitems, item_size, (nitems > 1) ? "s" : "");
    aptex_utils_exit(EXIT_FAILURE);
  }

  return 0;
}

int block_undump (char * p, int item_size, int nitems, void * in_file)
{
  boolean flag_fmt_failed;

  if (aptex_env.flag_compact_fmt)
    flag_fmt_failed = (gzread(in_file, (void *) p, (item_size * nitems)) <= 0);
  else
    flag_fmt_failed = (fread((void *) p, item_size, nitems, in_file) != nitems);

  if (flag_fmt_failed)
  {
    printf("\n! ApTeX: Could not read %d %d-byte item%s.\n", nitems, item_size, (nitems > 1) ? "s" : "");
    aptex_utils_exit(EXIT_FAILURE);
  }

  return 0;
}

void aptex_utils_exit (int unix_code)
{
  int final_code;

  update_terminal();

  if (unix_code == 0)
    final_code = EXIT_SUCCESS;
  else if (unix_code == 1)
    final_code = EXIT_FAILURE;
  else
    final_code = unix_code;

  exit(final_code);
}

integer aptex_utils_round (real r)
{
  integer i;

  if (r > 2147483647.0)
    i = 2147483647;
  else if (r < -2147483647.0)
    i = -2147483647;
  else if (r >= 0.0)
    i = (integer) (r + 0.5);
  else
    i = (integer) (r - 0.5);

  return i;
}
