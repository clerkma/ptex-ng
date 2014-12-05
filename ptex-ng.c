/*
   Copyright 1992 Karl Berry
   Copyright 2007 TeX Users Group
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

#define EXTERN
#include "ptex-ng.h"

static int    gargc;
static char **gargv;

int main (int ac, char *av[])
{
  int flag = 0, ret = 0;

  gargc = ac;
  gargv = av;

  if (main_init(gargc, gargv))
    return -1;

  TEX_format_default = " plain.fmt";
  format_default_length = strlen(TEX_format_default + 1);

  jump_used = 0;

  ret = setjmp(ng_env);

  if (ret == 0)
  {
    flag = main_program();

    if (trace_flag)
      printf("EXITING at %s: flag = %d, ret = %d, jump_used = %d\n", "main", flag, ret, jump_used);
  }
  else
  {
    if (trace_flag)
      printf("EXITING at %s: flag = %d, ret = %d, jump_used = %d\n", "jump_out", flag, ret, jump_used);
  }

  if (endit(flag) != 0)
    flag = 1;

  if (flag == 0)
    return 0;
  else
    exit(flag);
}
/* texk/web2c/lib/texmfmp.c */
void t_open_in (void)
{
  int i;

  buffer[first] = 0;

  if (gargc > optind && optind > 0)
  {
    for (i = optind; i < gargc; i++)
    {
      if (allow_quoted_names && strchr(gargv[i], ' ') != NULL)
      {
        (void) strcat ((char *) &buffer[first], "\"");
        (void) strcat ((char *) &buffer[first], gargv[i]);
        (void) strcat ((char *) &buffer[first], "\"");
      }
      else
        (void) strcat ((char *) &buffer[first], gargv[i]);

      (void) strcat ((char *) &buffer[first], " ");
    }
    
    gargc = 0;
  }

  /* Find the end of the buffer.  */
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

  tmptr = localtime (&clock);

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

#ifdef ALLOCATEBUFFER
  while (true) 
#else
  while (last < buf_size)
#endif
  {
    i = getc(f);

    if (i < ' ')
    {
      if ((i == EOF) || (i == '\n') || (i == '\r'))
        break;
      else if ((i == '\t') && (tab_step != 0))
      {
        buffer[last++] = (ASCII_code) ' ';

#ifdef ALLOCATEBUFFER
        if (last >= current_buf_size)
        {
          buffer = realloc_buffer(increment_buf_size);

          if (last >= current_buf_size)
            break;
        }
#endif

#ifdef ALLOCATEBUFFER
        while ((last - first) % tab_step != 0) 
#else
        while ((last < buf_size) && ((last - first) % tab_step != 0))
#endif
        {
          buffer[last++] = (ASCII_code) ' ';

#ifdef ALLOCATEBUFFER
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

#ifdef ALLOCATEBUFFER
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
    i = getc(f);

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

#if !defined (WORDS_BIGENDIAN)
// for swap
#define SWAP(x, y) temp = (x); (x) = (y); (y) = temp;

static int swap_items (char *p, int nitems, int size)
{
  char temp;

  switch (size)
  {
    case 8:
      while (nitems--)
      {
        SWAP (p[0], p[7]);
        SWAP (p[1], p[6]);
        SWAP (p[2], p[5]);
        SWAP (p[3], p[4]);
        p += size;
      }
      break;

    case 4:
      while (nitems--)
      {
        SWAP (p[0], p[3]);
        SWAP (p[1], p[2]);
        p += size;
      }
      break;

    case 2:
      while (nitems--)
      {
        SWAP (p[0], p[1]);
        p += size;
      }
      break;

    case 1:
      do_nothing();
      break;

    default:
      printf("\n! I can't (un)dump a %d byte item.\n", size);
      uexit(EXIT_FAILURE);
  }

  return 0;
}
#endif

#ifdef COMPACTFORMAT
int do_dump (char *p, int item_size, int nitems, gzFile out_file)
#else
int do_dump (char *p, int item_size, int nitems, FILE *out_file)
#endif
{
#if !defined (WORDS_BIGENDIAN)
  swap_items(p, nitems, item_size);
#endif

#ifdef COMPACTFORMAT
  if (gzwrite(out_file, p, (item_size * nitems)) != (item_size * nitems))
#else
  if ((int) fwrite(p, item_size, nitems, out_file) != nitems)
#endif
  {
    printf("\n! Could not write %d %d-byte item%s.\n",
               nitems, item_size, (nitems > 1) ? "s" : "");
    uexit(EXIT_FAILURE);
  }

/* Have to restore the old contents of memory, since some of it might get used again.  */
#if !defined (WORDS_BIGENDIAN)
  swap_items(p, nitems, item_size);
#endif

  return 0;
}

#ifdef COMPACTFORMAT
int do_undump (char *p, int item_size, int nitems, gzFile in_file)
#else
int do_undump (char *p, int item_size, int nitems, FILE *in_file)
#endif
{
#ifdef COMPACTFORMAT
  if (gzread(in_file, (void *) p, (unsigned int) (item_size * nitems)) <= 0)
#else
  if ((int) fread((void *) p, item_size, nitems, in_file) != nitems)
#endif
  {
    printf("\n! Could not read %d %d-byte item%s.\n",
               nitems, item_size, (nitems > 1) ? "s" : "");
    uexit(EXIT_FAILURE);
  }

#if !defined (WORDS_BIGENDIAN)
  swap_items (p, nitems, item_size);
#endif

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

  if (jump_used)
  {
    printf("Jump Buffer already used.\n");
    exit(EXIT_FAILURE);
  }

  jump_used++;
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
    i = (integer)(r + 0.5);
  else
    i = (integer)(r - 0.5);

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

  if (trace_flag)
    printf("Unixified name: %s\n", t);

  return t;
}
