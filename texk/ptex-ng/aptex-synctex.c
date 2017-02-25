/*
  Copyright (c) 2008, 2009, 2010, 2011 jerome DOT laurens AT u-bourgogne DOT fr
  Copyright (c) 2014, 2015, 2016, 2017 Clerk Ma

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

#define EXTERN
#include "aptex.h"

#define SYNCTEX_VERSION       1
#define SYNCTEX_NOERR         0
#define SYNCTEX_NO_OPTION     INT_MAX
#define SYNCTEX_NO_ERROR      0
#define synctex_free(x)       free(x)
#define SYNCTEX_OUTPUT        "pdf"
#define SYNCTEX_OFFSET_IS_PDF 0

static int  synctex_record_preamble (void);
static int  synctex_record_input (integer tag, char * fname);
static int  synctex_record_postamble (void);
static int  synctex_record_content (void);
static int  synctex_record_settings (void);
static int  synctex_record_sheet (integer sheet);
static int  synctex_record_teehs (integer sheet);
static void synctex_record_vlist (pointer p);
static void synctex_record_tsilv (pointer p);
static void synctex_record_void_vlist (pointer p);
static void synctex_record_hlist (pointer p);
static void synctex_record_tsilh (pointer p);
static void synctex_record_void_hlist (pointer p);
static void synctex_math_recorder (pointer p);
static void synctex_record_glue (pointer p);
static void synctex_record_kern (pointer p);
static void synctex_record_rule (pointer p);
static void synctex_kern_recorder (pointer p);
static void synctex_char_recorder (pointer p);
static void synctex_node_recorder (pointer p);
static int  synctex_record_anchor (void);
static int  synctex_record_count (void);

typedef void  (*synctex_recorder_t) (pointer);
typedef int   (*synctex_writer_t)   (void *, const char *, ...);
synctex_recorder_t  synctex_recorder;
synctex_writer_t    synctex_printf;
#define synctex_writer (*synctex_printf)

static char       * synctex_busy_name;
static char       * synctex_root_name;
static const char * synctex_suffix       = ".synctex";
static const char * synctex_suffix_gz    = ".gz";
static const char * synctex_suffix_busy  = "(busy)";

static integer synctex_pointer      = 0;
static integer synctex_h            = 0;
static integer synctex_v            = 0;
static integer synctex_count        = 0;
static integer synctex_cur_tag      = 0;
static integer synctex_cur_line     = 0;
static integer synctex_mag          = 0;
static integer synctex_unit         = 0;
static void *  synctex_file         = NULL;
static integer synctex_total_length = 0;
static boolean synctex_flag_read    = false;
static boolean synctex_flag_off     = false;
static boolean synctex_flag_flate   = false;
static boolean synctex_flag_shipable= false;
static boolean synctex_flag_warn    = false;
static boolean synctex_flag_quoted  = false;
static boolean synctex_flag_redir   = false;

void synctex_init (void)
{
  if (synctex_flag_read)
    return;

  switch (synctex_option)
  {
    case SYNCTEX_NO_OPTION:
      synctex = 0;
      break;

    case 0:
      synctex_flag_off = true;
      synctex = 0;
      break;

    default:
      {
        if (synctex_option < 0)
          synctex_flag_flate = true;

        synctex = synctex_option;
      }
      break;
  }

  synctex_flag_read = true;
}

static void synctex_abort (void)
{
  if (synctex_file)
  {
    if (synctex_flag_flate)
      xfclose((FILE *) synctex_file, synctex_busy_name);
    else
      gzclose((gzFile) synctex_file);

    synctex_file = NULL;
    remove(synctex_busy_name);
    synctex_free(synctex_busy_name);
    synctex_busy_name = NULL;
  }

  if (NULL != synctex_root_name)
  {
    synctex_free(synctex_root_name);
    synctex_root_name = NULL;
  }

  synctex_flag_off = true;
}

static void * synctex_dot_open (void)
{
  if (synctex_flag_off || !synctex)
    return NULL;

  if (synctex_file)
    return synctex_file;

  {
    char * tmp = utf8_mbcs(take_str_string(job_name));
    size_t len = strlen(tmp);

    if (len > 0)
    {
      char * the_busy_name = xmalloc((size_t)
          (len
           + strlen(synctex_suffix)
           + strlen(synctex_suffix_gz)
           + strlen(synctex_suffix_busy)
           + 1));

      if (!the_busy_name)
      {
        synctex_free(tmp);
        tmp = NULL;
        synctex_abort();
        return NULL;
      }

      the_busy_name[0] = (char) 0;

      if (tmp[0] == '"' && tmp[len - 1] == '"')
      {
        synctex_flag_quoted = true;
        tmp[len - 1] = (char) 0;
        strcat(the_busy_name, tmp + 1);
      }
      else
      {
        synctex_flag_quoted = false;
        strcat(the_busy_name, tmp);
      }

      synctex_free(tmp);
      tmp = NULL;
      strcat(the_busy_name, synctex_suffix);
      synctex_flag_flate = synctex < 0 ? true : false;

      if (!synctex_flag_flate)
        strcat(the_busy_name, synctex_suffix_gz);

      strcat(the_busy_name, synctex_suffix_busy);

      if (synctex_flag_flate)
      {
        synctex_file = fopen(the_busy_name, "wb");
        synctex_printf = (synctex_writer_t) (&fprintf);
      }
      else
      {
        synctex_file = gzopen(the_busy_name, "wb");
        synctex_printf = (synctex_writer_t) (&gzprintf);
      }

      if (synctex_file)
      {
        if (SYNCTEX_NO_ERROR == synctex_record_preamble())
        {
          synctex_mag = 1000;
          synctex_unit = 1;
          synctex_busy_name = the_busy_name;
          the_busy_name = NULL;

          if (NULL != synctex_root_name)
          {
            synctex_record_input(1, synctex_root_name);
            synctex_free(synctex_root_name);
            synctex_root_name = NULL;
          }

          synctex_count = 0;
          synctex_free(the_busy_name);
          the_busy_name = NULL;

          return synctex_file;
        }
        else
        {
          print_ln();
          prints("SyncTeX warning: no synchronization, problem with");
          prints(the_busy_name);
          print_ln();
        }
      }

      synctex_free(the_busy_name);
      the_busy_name = NULL;
    }
    else
    {
      print_ln();
      prints("SyncTeX information: no synchronization with keyboard input");
      print_ln();
    }

    synctex_free(tmp);
    tmp = NULL;
    synctex_abort();
    return NULL;
  }
}

static int synctex_record_input (integer tag, char * fname)
{
  int len = 0;

  len = synctex_writer(synctex_file, "Input:%"PRId64":%s\n", tag, fname);

  if (len > 0)
  {
    synctex_total_length += len;
    return SYNCTEX_NOERR;
  }

  synctex_abort();
  return -1;
}

void synctex_start_input (void)
{
  static unsigned int synctex_tag_counter = 0;

  if (synctex_flag_off)
    return;

  if (~synctex_tag_counter > 0)
    ++synctex_tag_counter;
  else
  {
    synctex_tag = 0;
    return;
  }

  synctex_tag = (int) synctex_tag_counter;

  if (synctex_tag_counter == 1)
  {
    char * name_mbcs = utf8_mbcs(take_str_string(name));
    synctex_root_name = kpse_find_file(name_mbcs, kpse_tex_format, false);
    free(name_mbcs);

    if (!strlen(synctex_root_name))
    {
      synctex_root_name = xrealloc(synctex_root_name, strlen("texput") + 1);
      strcpy(synctex_root_name, "texput");
    }

    return;
  }

  if (synctex_file || (synctex_dot_open() != NULL))
  {
    char * tmp = calloc(1, name_length + 1);
    strncpy(tmp, (const char *) name_of_file + 1, name_length);
    synctex_record_input(synctex_tag, tmp);
    synctex_free(tmp);
  }
}

void synctex_terminate (void)
{
  char * tmp = utf8_mbcs(take_str_string(job_name));
  char * the_real_syncname = NULL;

  if (log_opened && (tmp != NULL))
  {
    the_real_syncname = xmalloc((unsigned)
        (strlen(tmp) + strlen(synctex_suffix) +
         strlen(synctex_suffix_gz) + 1));

    if (!the_real_syncname)
    {
      synctex_free(tmp);
      synctex_abort();
      return;
    }

    strcpy(the_real_syncname, tmp);
    synctex_free(tmp);
    tmp = NULL;
    tmp = the_real_syncname + strlen(the_real_syncname);

    while (tmp > the_real_syncname)
    {
      --tmp;

      if (*tmp == '.')
      {
        *tmp = (char) 0;
        break;
      }
    }

    strcat(the_real_syncname, synctex_suffix);

    if (!synctex_flag_flate)
    {
      remove(the_real_syncname);
      strcat(the_real_syncname, synctex_suffix_gz);
    }

    if (0 != remove(the_real_syncname) && errno == EACCES)
      fprintf(stderr, "SyncTeX: Can't remove %s (file is open or read only)\n",
          the_real_syncname);

    if (synctex_file)
    {
      if (synctex_flag_shipable)
      {
        synctex_record_postamble();

        if (synctex_flag_flate)
          xfclose((FILE *) synctex_file, synctex_busy_name);
        else
          gzclose((gzFile) synctex_file);

        synctex_file = NULL;

        if (0 == rename(synctex_busy_name, the_real_syncname))
        {
          if (log_opened)
          {
            char * synctex_file_name = mbcs_utf8(the_real_syncname);
            print_ln();
            prints("SyncTeX written on ");
            prints(synctex_file_name);
            print('.');
            free(synctex_file_name);
          }
        }
        else
        {
          fprintf(stderr, "SyncTeX: Can't rename %s to %s\n",
              synctex_busy_name, the_real_syncname);
          remove(synctex_busy_name);
        }
      }
      else
      {
        if (synctex_flag_flate)
          xfclose((FILE *) synctex_file, synctex_busy_name);
        else
          gzclose((gzFile) synctex_file);

        synctex_file = NULL;
        remove(synctex_busy_name);
      }
    }

    if (synctex_flag_flate)
    {
      strcat(the_real_syncname, synctex_suffix_gz);
      remove(the_real_syncname);
    }
  }
  else if ((tmp = take_str_string(job_name)) != NULL)
  {
    size_t len = strlen(tmp);
    the_real_syncname = xmalloc((size_t)
        (len + strlen(synctex_suffix)
         + strlen(synctex_suffix_gz) + 1));

    if (!the_real_syncname)
    {
      synctex_free(tmp);
      synctex_abort();
      return;
    }

    if (len > 0 && tmp[0] == '"' && tmp[len - 1] == '"')
    {
      strcpy(the_real_syncname, tmp + 1);
      len = strlen(the_real_syncname);

      if ((len > 0) && (the_real_syncname[len - 1] == '"'))
        the_real_syncname[len - 1] = '\0';
    }
    else
      strcpy(the_real_syncname, tmp);

    synctex_free(tmp);
    tmp = NULL;
    strcat(the_real_syncname, synctex_suffix);
    remove(the_real_syncname);
    strcat(the_real_syncname, synctex_suffix_gz);
    remove(the_real_syncname);

    if (synctex_file)
    {
      if (synctex_flag_flate)
        xfclose((FILE *) synctex_file, synctex_busy_name);
      else
        gzclose((gzFile) synctex_file);

      synctex_file = NULL;
      remove(synctex_busy_name);
    }
  }

  synctex_free(synctex_busy_name);
  synctex_busy_name = NULL;
  synctex_free(the_real_syncname);
  the_real_syncname = NULL;
  synctex_abort();
}

void synctex_sheet (integer sync_mag)
{
  if (synctex_flag_off)
  {
    if (synctex && !synctex_flag_warn)
    {
      synctex_flag_warn = true;
      print_ln();
      prints("SyncTeX warning: Synchronization was disabled from");
      print_ln();
      prints("the command line with -synctex=0");
      print_ln();
      prints("Changing the value of \\synctex has no effect.");
    }

    return;
  }

  if (synctex_file || (synctex && (synctex_dot_open() != NULL)))
  {
    if (total_pages == 0)
    {
      if (sync_mag > 0)
        synctex_mag = sync_mag;

      if (SYNCTEX_NO_ERROR != synctex_record_settings()
          || SYNCTEX_NO_ERROR != synctex_record_content())
      {
        synctex_abort();
        return;
      }
    }

    synctex_record_sheet(total_pages + 1);
  }
}

void synctex_teehs (void)
{
  if (synctex_flag_off || !synctex_file)
    return;

  synctex_record_teehs(total_pages);
}

static int synctex_record_preamble (void)
{
  int len = 0;
  len = synctex_writer(synctex_file, "SyncTeX Version:%"PRId64"\n", (integer) SYNCTEX_VERSION);

  if (len > 0)
  {
    synctex_total_length = len;
    return SYNCTEX_NOERR;
  }

  synctex_abort();
  return -1;
}

static int synctex_record_postamble (void)
{
  if (SYNCTEX_NOERR == synctex_record_anchor())
  {
    int len = synctex_writer(synctex_file, "Postamble:\n");

    if (len > 0)
    {
      synctex_total_length += len;

      if (synctex_record_count() || synctex_record_anchor())
        do_nothing();
      else
      {
        len = synctex_writer(synctex_file, "Post scriptum:\n");

        if (len > 0)
        {
          synctex_total_length += len;
          return SYNCTEX_NOERR;
        }
      }
    }
  }

  synctex_abort();
  return -1;
}

#define SYNCTEX_IGNORE(NODE) synctex_flag_off || !synctex || !synctex_file

void synctex_vlist (pointer this_box)
{
  if (SYNCTEX_IGNORE(this_box))
    return;

  synctex_pointer = this_box;
  synctex_recorder = NULL;
  synctex_cur_tag = sync_tag(this_box + box_node_size);
  synctex_cur_line = sync_line(this_box + box_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_record_vlist(this_box);
}

void synctex_tsilv (pointer this_box)
{
  if (SYNCTEX_IGNORE(this_box))
    return;

  synctex_pointer = this_box;
  synctex_cur_tag = sync_tag(this_box + box_node_size);
  synctex_cur_line = sync_line(this_box + box_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;
  synctex_record_tsilv(this_box);
}

void synctex_void_vlist (halfword p, halfword this_box)
{
  (void) this_box;

  if (SYNCTEX_IGNORE(p))
    return;

  synctex_pointer = p;
  synctex_cur_tag = sync_tag(p + box_node_size);
  synctex_cur_line = sync_line(p + box_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;
  synctex_record_void_vlist(p);
}

void synctex_hlist (halfword this_box)
{
  if (SYNCTEX_IGNORE(this_box))
    return;

  synctex_pointer = this_box;
  synctex_cur_tag = sync_tag(this_box + box_node_size);
  synctex_cur_line = sync_line(this_box + box_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;
  synctex_record_hlist(this_box);
}

void synctex_tsilh (halfword this_box)
{
  if (SYNCTEX_IGNORE(this_box))
    return;

  synctex_pointer = this_box;
  synctex_cur_tag = sync_tag(this_box + box_node_size);
  synctex_cur_line = sync_line(this_box + box_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;
  synctex_record_tsilh(this_box);
}

void synctex_void_hlist (halfword p, halfword this_box)
{
  (void) this_box;

  if (SYNCTEX_IGNORE(p))
    return;

  if (synctex_recorder != NULL)
  {
    (*synctex_recorder) (synctex_pointer);
  }

  synctex_pointer = p;
  synctex_cur_tag = sync_tag(p + box_node_size);
  synctex_cur_line = sync_line(p + box_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;
  synctex_record_void_hlist(p);
}


#define SYNCTEX_IGNORE_NODE(NODE,TYPE) synctex_flag_off || !synctex \
  || (0 >= sync_tag(NODE + TYPE)) \
  || (0 >= sync_line(NODE + TYPE))

#define SYNCTEX_CONTEXT_DID_CHANGE(NODE,TYPE) ((0 == synctex_pointer)\
  || (sync_tag(NODE + TYPE) != synctex_cur_tag)\
  || (sync_line(NODE + TYPE) != synctex_cur_line))

void synctex_math (halfword p, halfword this_box)
{
  (void) this_box;

  if (SYNCTEX_IGNORE(p))
    return;

  if ((synctex_recorder != NULL) && SYNCTEX_CONTEXT_DID_CHANGE(p, medium_node_size))
  {
    (*synctex_recorder) (synctex_pointer);
  }

  synctex_pointer = p;
  synctex_cur_tag = sync_tag(p + medium_node_size);
  synctex_cur_line = sync_line(p + medium_node_size);
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;
  synctex_math_recorder(p);
}

#undef SYNCTEX_IGNORE
#define SYNCTEX_IGNORE(NODE,TYPE) synctex_flag_off || !synctex \
  || (0 >= sync_tag(NODE + TYPE)) \
  || (0 >= sync_line(NODE + TYPE))

void synctex_horizontal_rule_or_glue (halfword p, halfword this_box)
{
  (void) this_box;

  switch (type(p))
  {
    case rule_node:
      if (SYNCTEX_IGNORE(p, rule_node_size))
        return;
      break;

    case glue_node:
    case kern_node:
      if (SYNCTEX_IGNORE(p, medium_node_size))
        return;
      break;

    default:
      print_ln();
      printf("Synchronize ERROR: unknown node type %d", type(p));
      print_ln();
      break;
  }

  synctex_pointer = p;
  synctex_h = cur_h;
  synctex_v = cur_v;
  synctex_recorder = NULL;

  switch (type(p))
  {
    case rule_node:
      synctex_cur_tag = sync_tag(p + rule_node_size);
      synctex_cur_line = sync_line(p + rule_node_size);
      synctex_record_rule(p);
      break;

    case glue_node:
    case kern_node:
      synctex_cur_tag = sync_tag(p + medium_node_size);
      synctex_cur_line = sync_line(p + medium_node_size);
      synctex_record_rule(p);
      break;

    default:
      print_ln();
      printf("Synchronize ERROR: unknown node type %d", type(p));
      print_ln();
      break;
  }
}

void synctex_kern (halfword p, halfword this_box)
{
  (void) this_box;

  if (SYNCTEX_IGNORE(p, medium_node_size))
    return;

  if (SYNCTEX_CONTEXT_DID_CHANGE(p, medium_node_size))
  {
    if (synctex_recorder != NULL)
    {
      (*synctex_recorder) (synctex_pointer);
    }

    if (synctex_pointer == this_box)
    {
      synctex_pointer = p;
      synctex_cur_tag = sync_tag(p + medium_node_size);
      synctex_cur_line = sync_line(p + medium_node_size);
      synctex_recorder = &synctex_kern_recorder;
    }
    else
    {
      synctex_pointer = p;
      synctex_cur_tag = sync_tag(p + medium_node_size);
      synctex_cur_line = sync_line(p + medium_node_size);
      synctex_recorder = NULL;
      synctex_kern_recorder(p);
    }
  }
  else
  {
    synctex_pointer = p;
    synctex_cur_tag = sync_tag(p + medium_node_size);
    synctex_cur_line = sync_line(p + medium_node_size);
    synctex_recorder = &synctex_kern_recorder;
  }
}

#undef SYNCTEX_IGNORE
#define SYNCTEX_IGNORE(NODE) synctex_flag_off || !synctex || !synctex_file \
  || (synctex_count > 2000)

void synctex_char (halfword p, halfword this_box)
{
  (void) this_box;

  if (SYNCTEX_IGNORE(p))
    return;

  if (synctex_recorder != NULL)
  {
    (*synctex_recorder) (synctex_pointer);
  }

  synctex_pointer = p;
  synctex_cur_tag = 0;
  synctex_cur_line = 0;
  synctex_recorder = NULL;
  synctex_char_recorder(p);
}

#undef SYNCTEX_IGNORE
#define SYNCTEX_IGNORE(NODE) (synctex_flag_off || !synctex || !synctex_file)

void synctex_node (halfword p, halfword this_box)
{
  (void) this_box;

  if (SYNCTEX_IGNORE(p))
    return;

  synctex_node_recorder(p);
}

void synctex_current (void)
{
  if (SYNCTEX_IGNORE(nothing))
    return;
  else
  {
    int len = synctex_writer(synctex_file, "x%"PRId64",%"PRId64":%"PRId64",%"PRId64"\n",
        synctex_cur_tag, synctex_cur_line,
        cur_h, cur_v);

    if (len > 0)
    {
      synctex_total_length += len;
      return;
    }
  }

  synctex_abort();
}

static int synctex_record_settings (void)
{
  if (NULL == synctex_file)
    return SYNCTEX_NOERR;

  if (synctex_file)
  {
    int len = synctex_writer(synctex_file,
        "Output:%s\nMagnification:%"PRId64"\nUnit:%"PRId64"\nX Offset:%"PRId64"\nY Offset:%"PRId64"\n",
        SYNCTEX_OUTPUT, synctex_mag, synctex_unit,
        (integer) ((SYNCTEX_OFFSET_IS_PDF != 0) ? 0 : 4736287),
        (integer) ((SYNCTEX_OFFSET_IS_PDF != 0) ? 0 : 4736287));

    if (len > 0)
    {
      synctex_total_length += len;
      return SYNCTEX_NOERR;
    }
  }

  synctex_abort();
  return -1;
}

static int synctex_record_anchor (void)
{
  int len = 0;

  len = synctex_writer(synctex_file, "!%"PRId64"\n", synctex_total_length);

  if (len > 0)
  {
    synctex_total_length = len;
    incr(synctex_count);
    return SYNCTEX_NOERR;
  }

  synctex_abort();
  return -1;
}

static int synctex_record_content (void)
{
  int len = 0;

  len = synctex_writer(synctex_file, "Content:\n");

  if (len > 0)
  {
    synctex_total_length += len;
    return SYNCTEX_NOERR;
  }

  synctex_abort();
  return -1;
}

static int synctex_record_sheet (integer sheet)
{
  if (SYNCTEX_NOERR == synctex_record_anchor())
  {
    int len = synctex_writer(synctex_file, "{%"PRId64"\n", sheet);

    if (len > 0)
    {
      synctex_total_length += len;
      incr(synctex_count);
      return SYNCTEX_NOERR;
    }
  }

  synctex_abort();
  return -1;
}

static int synctex_record_teehs (integer sheet)
{
  if (SYNCTEX_NOERR == synctex_record_anchor())
  {
    int len = synctex_writer(synctex_file, "}%"PRId64"\n", sheet);

    if (len > 0)
    {
      synctex_total_length += len;
      incr(synctex_count);
      return SYNCTEX_NOERR;
    }
  }

  synctex_abort();
  return -1;
}

static void synctex_record_void_vlist (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "v%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64",%"PRId64",%"PRId64"\n",
      sync_tag(p + box_node_size),
      sync_line(p + box_node_size),
      synctex_h, synctex_v,
      width(p), height(p), depth(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static void synctex_record_vlist (pointer p)
{
  int len = 0;
  synctex_flag_shipable = true;

  len = synctex_writer(synctex_file, "[%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64",%"PRId64",%"PRId64"\n",
      sync_tag(p + box_node_size),
      sync_line(p + box_node_size),
      synctex_h, synctex_v,
      width(p), height(p), depth(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static void synctex_record_tsilv (pointer p)
{
  int len = 0;
  (void) p;

  len = synctex_writer(synctex_file, "]\n");

  if (len > 0)
  {
    synctex_total_length += len;
    return;
  }

  synctex_abort();
}

static void synctex_record_void_hlist (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "h%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64",%"PRId64",%"PRId64"\n",
      sync_tag(p + box_node_size),
      sync_line(p + box_node_size),
      synctex_h, synctex_v,
      width(p), height(p), depth(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static void synctex_record_hlist (pointer p)
{
  int len = 0;
  synctex_flag_shipable = true;

  len = synctex_writer(synctex_file, "(%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64",%"PRId64",%"PRId64"\n",
      sync_tag(p + box_node_size),
      sync_line(p + box_node_size),
      synctex_h, synctex_v,
      width(p), height(p), depth(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static void synctex_record_tsilh (pointer p)
{
  int len = 0;
  (void) p;


  len = synctex_writer(synctex_file, ")\n");

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static int synctex_record_count (void)
{
  int len = 0;

  len = synctex_writer(synctex_file, "Count:%"PRId64"\n", synctex_count);

  if (len > 0)
  {
    synctex_total_length += len;
    return SYNCTEX_NOERR;
  }

  synctex_abort();
  return -1;
}

static void synctex_record_glue (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "g%"PRId64",%"PRId64":%"PRId64",%"PRId64"\n",
      sync_tag(p + medium_node_size),
      sync_line(p + medium_node_size),
      synctex_h, synctex_v);

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static void synctex_record_kern (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "k%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64"\n",
      sync_tag(p + medium_node_size),
      sync_line(p + medium_node_size),
      synctex_h, synctex_v,
      width(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

static void synctex_record_rule (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "r%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64",%"PRId64",%"PRId64"\n",
      sync_tag(p + medium_node_size),
      sync_line(p + medium_node_size),
      synctex_h, synctex_v,
      rule_wd, rule_ht, rule_dp);

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

void synctex_math_recorder (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "$%"PRId64",%"PRId64":%"PRId64",%"PRId64"\n",
      sync_tag(p + medium_node_size),
      sync_line(p + medium_node_size),
      synctex_h, synctex_v);

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

void synctex_kern_recorder (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "k%"PRId64",%"PRId64":%"PRId64",%"PRId64":%"PRId64"\n",
      sync_tag(p + medium_node_size),
      sync_line(p + medium_node_size),
      synctex_h, synctex_v,
      width(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

void synctex_char_recorder (pointer p)
{
  int len = 0;
  (void) p;

  len = synctex_writer(synctex_file, "c%"PRId64",%"PRId64"\n",
      synctex_h, synctex_v);

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}

void synctex_node_recorder (pointer p)
{
  int len = 0;

  len = synctex_writer(synctex_file, "?%"PRId64",%"PRId64":%"PRId64",%"PRId64"\n",
    synctex_h, synctex_v,
    type(p), subtype(p));

  if (len > 0)
  {
    synctex_total_length += len;
    incr(synctex_count);
    return;
  }

  synctex_abort();
}
