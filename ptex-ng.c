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

#define dump_ext_length 4
#define edit_value      tex_edit_value

int    gargc;
char **gargv;

int jump_used = 0;

jmp_buf jumpbuffer;

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

  ret = setjmp(jumpbuffer);

  if (ret == 0)
  {
    flag = main_program();

    if (trace_flag)
      printf("EXITING at %s: flag = %d, ret = %d, jump_used = %d\n", "main", flag, ret, jump_used);
  }
  else
  {
    if (trace_flag)
      printf("EXITING at %s: flag = %d, ret = %d, jump_used =  %d\n", "jump_out", flag, ret, jump_used);
  }

  if (endit(flag) != 0)
    flag = 1; /* do final clean up in local.c */

  if (flag == 0)
    return 0;
  else
    exit (flag);
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
    exit(1);

  (void) signal(SIGINT, catch_interrupt);
}

void fix_date_and_time (void)
{
  time_t clock;
  struct tm *tmptr;

  (void) time(&clock);

  if (trace_flag)
    printf("The time is %lld\n", (long long)clock);

  if (clock < 0)
    puts("Time not available!");

  tmptr = localtime (&clock);

  if (tmptr == NULL)
  {
    printf("Cannot convert time (%0lld)!\n", (long long)clock);
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

    if (trace_flag)
      printf("%d-%d-%d %d:%d\n",
        tmptr->tm_year + 1900,
        tmptr->tm_mon + 1,
        tmptr->tm_mday,
        tmptr->tm_hour,
        tmptr->tm_min);
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

/* I/O for TeX and Metafont. */
void complain_line (FILE * output)
{
  show_line("\n", 0);

#ifdef ALLOCATEBUFFER
  sprintf(log_line, "! Unable to read an entire line---buf_size=%d.\n", current_buf_size);
#else
  sprintf(log_line, "! Unable to read an entire line---buf_size=%d.\n", buf_size);
#endif

  fputs(log_line, output);
  puts("  (File may have a line termination problem.)");
}

void show_bad_line (FILE * output, int first, int last)
{
  int i, c, d, ch;
  char *s = log_line;

  for (i = first; i <= last; i++)
  {
    ch = buffer[i];

    if (show_in_hex && (ch > 127))
    {
      c = ch >> 4;
      d = ch & 15;

      if (c > 9)
        c = c + 'a' - 10;
      else
        c = c + '0';

      if (d > 9)
        d = d + 'a' - 10;
      else
        d = d + '0';

      *s++ = '^';
      *s++ = '^';

      *s++ = (char) c;
      *s++ = (char) d;
    }
    else if (ch < 32)
    {
      *s++ = '^';
      *s++ = '^';
      *s++ = (char) (ch + 64);
    }
    else if (ch == 127)
    {
      *s++ = '^';
      *s++ = '^';
      *s++ = (char) (ch - 64);
    }
    else
    {
      *s++ = (char) ch;
    }
  }

  *s++ = ' ';
  *s++ = '\0';

  fputs(log_line, output);   // log_file
}

boolean input_line_finish (void)
{
  int i = '\0';
  int ch, flag;

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

  if (restrict_to_ascii)
  {
    flag = 0;

    for (i = first; i <= last; i++)
    {
      ch = buffer[i];

      if (ch > 126 ||  (ch < ' ' && ch != '\t' && ch != '\f' && ch != '\r' && ch != '\n'))
      {
        sprintf(log_line, "\n! non ASCII char (%d) in line: ", ch);
        show_line(log_line, 1);

        if (log_opened)
          fprintf(log_file, "\n! non ASCII char (%d) in line: ", ch);

        flag = 1;
        break;
      }
    }

    if (flag)
    {
      show_bad_line(errout, first, last);

      if (log_opened)
        show_bad_line(log_file, first, last);
    }
  }

  return true;
}
/* sec 0031 */
boolean input_line (FILE * f)
{
  int i = '\0';

  last = first;

/*  different versions depending on return_flag / tabexpand */
/*  while (last < buf_size && (i = getc (f)) != EOF)  */
#ifdef ALLOCATEBUFFER
  for ( ; ; ) 
#else
  while (last < buf_size) 
#endif
  {
    i = getc(f);

    if (i < ' ')
    {
      if (i == EOF || i == '\n' || (i == '\r' && return_flag))
        break;
      else if (i == '\t' && tab_step != 0)
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
        while (last < buf_size && (last - first) % tab_step != 0)
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

  if (return_flag)  /* let return terminate line as well as newline */
  {
    if (i == '\r')  /* see whether return followed by newline */
    {
      i = getc(f);  /* in which case throw away the newline */

      if (i != '\n')
      {
        ungetc(i, f);
        i = '\r';
      }
/*      else  buffer[last-1] = (ASCII_code) i; */
    }
  }

  //  Turn Ctrl-Z at end of file into newline 2000 June 22
  if (i == EOF && trimeof && buffer[last - 1] == 26)
  {
    last--;
  }

  if (i == EOF && last == first)
    return false;

/*  Didn't get the whole line because buffer was too small?  */
/*  This shouldn't happen anymore 99/Jan/23 */
  if (i != EOF && i != '\n' && i != '\r')
  {
    complain_line(errout);

    if (log_opened)
      complain_line(log_file);

    /* This may no longer be needed ... now that we grow it */
    if (truncate_long_lines)
    {
      while (i != EOF && i != '\n' && i != '\r')
      {
        i = getc (f);     // discard rest of line
      }

      last--;       /* just in case */
    }
    else
      uexit(EXIT_FAILURE);      /* line too long */
  }

  return input_line_finish();
}

static char * edit_value = "c:\\yandy\\WinEdt\\WinEdt.exe [Open('%s');SelLine(%d,7)]";

#ifdef WIN32
static inline int Isspace (char c)
{
  return (c == ' ' || c == '\t');
}
#endif

void call_edit (ASCII_code * filename, pool_pointer fnstart, integer fnlength, integer linenumber)
{
  char *temp, *command, *fullcmd;
  char c;
  int sdone, ddone, i;

#ifdef WIN32
  char *fp, *ffp, *env, editorname[256], buffer[256];
  int cnt = 0;
  int dontchange = 0;
#endif

  sdone = ddone = 0;
  filename += fnstart;

  /* Close any open input files, since we're going to kill the job. */
  for (i = 1; i <= in_open; i++)
#ifdef XeTeX
    xfclose(input_file[i]->f, "inputfile");
#else
    xfclose(input_file[i], "inputfile");
#endif

  /* Replace the default with the value of the appropriate environment
     variable or config file value, if it's set.  */
  temp = kpse_var_value("TEXEDIT");

  if (temp != NULL)
    edit_value = temp;

  /* Construct the command string.  The `11' is the maximum length an
     integer might be.  */
  command = (char *) xmalloc (strlen (edit_value) + fnlength + 11);

  /* So we can construct it as we go.  */
  temp = command;

#ifdef WIN32
  fp = editorname;
  if ((isalpha(*edit_value) && *(edit_value + 1) == ':'
        && IS_DIR_SEP (*(edit_value + 2)))
      || (*edit_value == '"' && isalpha(*(edit_value + 1))
        && *(edit_value + 2) == ':'
        && IS_DIR_SEP (*(edit_value + 3)))
     )
    dontchange = 1;
#endif

  while ((c = *edit_value++) != 0)
  {
    if (c == '%')
    {
      switch (c = *edit_value++)
      {
        case 'd':
          if (ddone)
            FATAL ("call_edit: `%%d' appears twice in editor command");
          sprintf (temp, "%ld", (long int)linenumber);
          while (*temp != '\0')
            temp++;
          ddone = 1;
          break;
        
        case 's':
          if (sdone)
            FATAL ("call_edit: `%%s' appears twice in editor command");
          for (i = 0; i < fnlength; i++)
            *temp++ = xchr[(filename[i])];
          sdone = 1;
          break;
        
        case '\0':
          *temp++ = '%';
          /* Back up to the null to force termination.  */
          edit_value--;
          break;
        
        default:
          *temp++ = '%';
          *temp++ = c;
          break;
      }
    }
    else
    {
#ifdef WIN32
      if (dontchange)
        *temp++ = c;
      else 
      { 
        if(Isspace(c) && cnt == 0)
        {
          cnt++;
          temp = command;
          *temp++ = c;
          *fp = '\0';
        }
        else if(!Isspace(c) && cnt == 0)
        {
          *fp++ = c;
        }
        else
        {
          *temp++ = c;
        }
      }
#else
      *temp++ = c;
#endif
    }
  }

  *temp = 0;

#ifdef WIN32
  if (dontchange == 0) {
    if(editorname[0] == '.' ||
       editorname[0] == '/' ||
       editorname[0] == '\\') {
      fprintf(stderr, "%s is not allowed to execute.\n", editorname);
      uexit(1);
    }
    env = (char *)getenv("PATH");
    if(SearchPath(env, editorname, ".exe", 256, buffer, &ffp)==0) {
      if(SearchPath(env, editorname, ".bat", 256, buffer, &ffp)==0) {
        fprintf(stderr, "I cannot find %s in the PATH.\n", editorname);
        uexit(1);
      }
    }
    fullcmd = (char *)xmalloc(strlen(buffer)+strlen(command)+5);
    strcpy(fullcmd, "\"");
    strcat(fullcmd, buffer);
    strcat(fullcmd, "\"");
    strcat(fullcmd, command);
  } else
#endif
  fullcmd = command;

  /* Execute the command.  */
  if (system (fullcmd) != 0)
    fprintf(stderr, "! Trouble executing `%s'.\n", command);

  /* Quit, since we found an error.  */
  uexit(1);
}


#if !defined (WORDS_BIGENDIAN) && !defined (NO_FMTBASE_SWAP)
   
#define SWAP(x, y) temp = (x); (x) = (y); (y) = temp;


/* Make the NITEMS items pointed at by P, each of size SIZE, be the
   opposite-endianness of whatever they are now.  */

static int swap_items (char *p, int nitems, int size)
{
  char temp;

  /* Since `size' does not change, we can write a while loop for each
     case, and avoid testing `size' for each time.  */
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
    /* Nothing to do.  */
      break;

    default:
      show_line("\n", 0);
      sprintf(log_line, "! I can't (un)dump a %d byte item.\n", size);
      show_line(log_line, 1);
      uexit(EXIT_FAILURE);
  }
  return 0;
}
#endif /* not WORDS_BIGENDIAN and not NO_FMTBASE_SWAP */

#ifdef COMPACTFORMAT
int do_dump (char *p, int item_size, int nitems, gzFile out_file)
#else
int do_dump (char *p, int item_size, int nitems, FILE *out_file)
#endif
{
#if !defined (WORDS_BIGENDIAN) && !defined (NO_FMTBASE_SWAP)
  swap_items (p, nitems, item_size);
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
#if !defined (WORDS_BIGENDIAN) && !defined (NO_FMTBASE_SWAP)
  swap_items (p, nitems, item_size);
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

#if !defined (WORDS_BIGENDIAN) && !defined (NO_FMTBASE_SWAP)
  swap_items (p, nitems, item_size);
#endif

  return 0;
}
