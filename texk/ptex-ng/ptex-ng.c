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

#define EXTERN
#include "ptex-ng.h"

static int    gargc;
static char **gargv;

int main (int argc, char *argv[])
{
  int flag = 0;

  gargc = argc;
  gargv = argv;

#ifdef WIN32
  _setmaxstdio(2048);
  setmode(fileno(stdin), _O_BINARY);
#endif

  main_init(argc, argv);
  flag = main_program();
  main_exit(flag);

  return 0;
}

void t_open_in (void)
{
  int i;
  buffer[first] = 0;

  if (gargc > optind && optind > 0)
  {
    for (i = optind; i < gargc; i++)
    {
      char * name_from_cli = mbcs_utf8(gargv[i]);

      if (flag_allow_quoted && (strchr(gargv[i], ' ') != NULL))
        sprintf((char *) &buffer[first], "\"%s\" ", name_from_cli);
      else
        sprintf((char *) &buffer[first], "%s ", name_from_cli);

      free(name_from_cli);
    }

    gargc = 0;
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

void fix_date_and_time (void)
{
  time_t clock;
  struct tm *tmptr;

  (void) time(&clock);

  if (clock < 0)
    puts("Time not available!");

  tmptr = localtime(&clock);

  if (tmptr == NULL)
  {
    printf("Cannot convert time (%0lld)!\n", (long long) clock);
    year     = 2038;
    month    = 1;
    day      = 18;
    tex_time = 22 * 60 + 14;
  }
  else
  {
    tex_time = tmptr->tm_hour * 60 + tmptr->tm_min;
    day      = tmptr->tm_mday;
    month    = tmptr->tm_mon + 1;
    year     = tmptr->tm_year + 1900;
  }

  {
#ifdef _WIN32
    if (signal(SIGINT, catch_interrupt) == SIG_ERR)
    {
      puts(" CTRL-C handler not installed");
      uexit(EXIT_FAILURE);
    }
#else
    void (*old_handler)();

    old_handler = signal(SIGINT, catch_interrupt);

    if (old_handler != SIG_DFL)
      (void) signal(SIGINT, old_handler);
#endif
  }
}

/* sec 0031 */
// inputs the next line or returns |false|
boolean input_ln (FILE * f, boolean bypass_eoln)
{
  int i = '\0';

  (void) bypass_eoln;
  last = first;

#ifdef NG_EXTENSION
  while (true)
#else
  while (last < buf_size)
#endif
  {
    i = fgetc(f);

    if (i < ' ')
    {
      if ((i == EOF) || (i == '\n') || (i == '\r'))
        break;
      else if ((i == '\t') && (tab_step != 0))
      {
        buffer[last++] = (ASCII_code) ' ';

#ifdef NG_EXTENSION
        if (last >= current_buf_size)
        {
          buffer = realloc_buffer(increment_buf_size);

          if (last >= current_buf_size)
            break;
        }
#endif

#ifdef NG_EXTENSION
        while ((last - first) % tab_step != 0)
#else
        while ((last < buf_size) && ((last - first) % tab_step != 0))
#endif
        {
          buffer[last++] = (ASCII_code) ' ';

#ifdef NG_EXTENSION
          if (last >= current_buf_size)
          {
            buffer = realloc_buffer(increment_buf_size);

            if (last >= current_buf_size)
              break;
          }
#endif
        }

        continue;
      }
    }

    {
      buffer[last++] = (ASCII_code) i;

#ifdef NG_EXTENSION
      if (last >= current_buf_size)
      {
        buffer = realloc_buffer(increment_buf_size);

        if (last >= current_buf_size)
          break;
      }
#endif
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

  if (flag_compact_fmt)
    flag_fmt_failed = (gzwrite(out_file, p, (item_size * nitems)) != (item_size * nitems));
  else
    flag_fmt_failed = (fwrite(p, item_size, nitems, out_file) != nitems);

  if (flag_fmt_failed)
  {
    printf("\n! Could not write %d %d-byte item%s.\n",
               nitems, item_size, (nitems > 1) ? "s" : "");
    uexit(EXIT_FAILURE);
  }

  return 0;
}

int block_undump (char * p, int item_size, int nitems, void * in_file)
{
  boolean flag_fmt_failed;

  if (flag_compact_fmt)
    flag_fmt_failed = (gzread(in_file, (void *) p, (item_size * nitems)) <= 0);
  else
    flag_fmt_failed = (fread((void *) p, item_size, nitems, in_file) != nitems);

  if (flag_fmt_failed)
  {
    printf("\n! Could not read %d %d-byte item%s.\n",
               nitems, item_size, (nitems > 1) ? "s" : "");
    uexit(EXIT_FAILURE);
  }

  return 0;
}

void uexit (int unix_code)
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

// texk/web2c/lib/zround.c
integer web2c_round (double r)
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

// Unixify filename and path (turn \ into /)
char * unixify (char * t)
{
  char * s = t;

  if (s == NULL)
    return s;

  if (t != '\0')
  {
    while (*s != '\0')
    {
      if (*s == '\\')
        *s = '/';

      s++;
    }
  }

  if (flag_trace)
    printf("Unixified name: %s\n", t);

  return t;
}
