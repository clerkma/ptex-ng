/*
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

#if   defined (__ANDROID__)
  #define malloc_usable_size dlmalloc_usable_size
#elif defined (__APPLE__)
  #include <malloc/malloc.h>
  #define malloc_usable_size malloc_size
#endif

#define EXTERN extern
#include "ptex.h"

#define USEOUREALLOC
#define USEMEMSET

#ifdef USEOUREALLOC
  #define REALLOC ourrealloc
#else
  #define REALLOC realloc
#endif

#if   defined (__clang__)
const char * compiler = "Clang";
#elif defined (__GNUC__) || defined(__GNUG__)
const char * compiler = "GCC";
#elif defined (_MSC_VER)
const char * compiler = "MSVC";
#else
const char * compiler = "Unknown";
#endif

#if   defined (_WIN64)
const char * dist = "Win64";
#elif defined (_WIN32)
const char * dist = "Win32";
#elif defined (__ANDROID__)
const char * dist = "Android";
#elif defined (__APPLE__)
const char * dist = "Darwin";
#elif defined (__gnu_linux__)
const char * dist = "Linux";
#else
const char * dist = "Unknown";
#endif

const char * compiletime  = __TIME__;
const char * compiledate  = __DATE__;
const char * yandyversion = "2.3.0";
const char * application  = "pTeX";
const char * banner       = "This is pTeX-ng, Version 3.14159265";

void print_banner (void)
{
  prints(banner);
}

clock_t start_time, main_time, finish_time;

char * dvi_directory = "";
char * log_directory = "";
char * aux_directory = "";
char * fmt_directory = "";
char * pdf_directory = "";

char log_line[256];

boolean mem_spec_flag     = false;
boolean format_spec       = false;
boolean reorder_arg_flag  = true;  /* put command line flags/arguments first */

void show_usage (void)
{
  printf("\n"
      "Useage: ptex [OPTION]... [+format_file] [file]\n\n"
      "--help       -?  show this usage summary\n"
      "--initex     -i  start up as initex (create format file)\n"
      "--verbose    -v  be verbose (show implementation version number)\n"
      "--ascii      -n  do not allow `non ASCII' characters in input files\n"
      "                    (complain instead)\n"
      "--showhex    -w  do not show `non ASCII' characters in hexadecimal\n"
      "                    (show as is)\n"
      "--patterns   -p  allow use of \\patterns after loading format (initex only)\n"
      "--knuthify   -K  disable all extensions to basic TeX\n"
      "--main-mem   -m  initial main memory size in kilo words (initex only)\n"
      "--hyph-size  -e  hyphenation exception dictionary size (initex only)\n"
      "--trie-size  -h  hyphenation pattern trie size (initex only)\n"
      "--xchr-file  -x  use `non ASCII' character mapping (xchr[]) defined in file\n"
      "--key-file   -k  use `key replacement' defined in file\n"
      "--dvi-dir    -o  write DVI file in specified directory (default '.')\n"
      "--log-dir    -l  write LOG file in specified directory (default '.')\n"
      "--aux-dir    -a  write AUX file in specified directory (default '.')\n");
  uexit(EXIT_FAILURE);
}

// Sep 27 1990 => 1990 Sep 27
// 0123456789     0123456789
void scivilize (char * date)
{
  int k;
  char pyear[6];

  strcpy(pyear, date + 7);

  for (k = 5; k >= 0; k--)
    date[k + 5] = date[k];

  for (k = 0; k < 4; k++)
    date[k] = pyear[k];

  date[4] = ' ';

  if (date[9] == ' ')
    date[9] = '0';
}

void stamp_it (char * s)
{
  char date[11 + 1];

  strcpy(date, compiledate);
  scivilize(date);
  sprintf(s, "%s %s (compiled time: %s %s with %s/%s)",
    application, yandyversion, date, compiletime, dist, compiler);
  s += strlen(s);
}

#define MAXCHRS 256
#define NOTDEF  127

void read_xchr_sub (FILE * xchr_input)
{
  char buffer[file_name_size];
  int k, from, to, count = 0;
  char * s;

  memset(xchr, NOTDEF, MAXCHRS);
  memset(xord, NOTDEF, MAXCHRS);

#ifdef ALLOCATEBUFFER
  while (fgets(buffer, current_buf_size, xchr_input) != NULL)
#else
  while (fgets(buffer, sizeof(buffer), xchr_input) != NULL)
#endif
  {
    if (*buffer == '%' || *buffer == ';' || *buffer == '\n')
      continue;

    from = (int) strtol (buffer, &s, 0);
    to = (int) strtol (s, NULL, 0);

    if (from >= 0 && from < MAXCHRS && to >= 0 && to < MAXCHRS)
    {
      if (xchr[from] == NOTDEF)
        xchr[from] = (unsigned char) to;
      else
        printf("NOTE: %s collision: %d => %d, %d\n", "xchr", from, xchr[from], to);

      if (xord[to] == NOTDEF)
        xord[to] = (unsigned char) from;
      else
        printf("NOTE: %s collision: %d => %d, %d\n", "xord", to, xord[to], from);

      count++;
    }
  }

  for (k = 0; k < MAXCHRS; k++)
  {
    if (xchr[k] == NOTDEF)   /* if it has not been filled */
    {
      if (xord[k] == NOTDEF) /* see whether used already */
      {
        xchr[k] = (unsigned char) k; /* no, so make identity */
        xord[k] = (unsigned char) k; /* no, so make identity */
      }
    }
  }

  xchr[NOTDEF] = NOTDEF;         /* fixed point of mapping */

  if (trace_flag)
  {
    printf("Read %d xchr[] pairs:\n", count);

    for (k = 0; k < MAXCHRS; k++)
    {
      if (xchr[k] != NOTDEF)
        printf("%d => %d\n", k, xchr[k]);
    }
  }
}

char * replacement[MAXCHRS];     /* pointers to replacement strings */

void read_repl_sub (FILE * repl_input)
{
  int k, n, m, chrs;
  char buffer[file_name_size];
  char charname[128];
  int charnum[10];
  char * s, * t;
  
  memset(replacement, 0, MAXCHRS * sizeof(replacement[0]));

  while (fgets(buffer, file_name_size, repl_input) != NULL)
  {
    if (*buffer == '%' || *buffer == ';' || *buffer == '\n')
      continue;

    if ((m = sscanf(buffer, "%d%n %s", &chrs, &n, (char *)&charname)) == 0)
      continue;
    else if (m == 2)
    {
      if (*charname == '"')   /* deal with quoted string "..." */
      {
        s = buffer + n;
        t = charname;

        while (*s != '"' && *s != '\0')
          s++;  /* step up to " */

        if (*s++ == '\0')
          continue;       /* sanity check */

        while (*s != '\0')
        {
          if (*s == '"')
          {
            s++;            /* is it "" perhaps ? */

            if (*s != '"')
              break;   /* no, end of string */
          }

          *t++ = *s++;          /* copy over */
        }

        *t = '\0';              /* and terminate */
      }

      if (chrs >= 0 && chrs < MAXCHRS)
        replacement[chrs] = xstrdup(charname);
    }
/*    presently the following can never get triggered */
/*    which is good, because it is perhaps not right ... */
    else if ((m = sscanf (buffer, "%d %d %d %d %d %d %d %d %d %d %d",
      &chrs, charnum, charnum+1, charnum+2, charnum+3, charnum+4,
        charnum+5, charnum+6, charnum+7, charnum+8, charnum+9)) > 1) {
/*      for (k = 0; k < n-1; k++) charname[k] = (char) charnum; */
      for (k = 0; k < n-1; k++) charname[k] = (char) charnum[k];
      charname[m] = '\0';
      if (chrs >= 0 && chrs < MAXCHRS)
        replacement[chrs] = xstrdup(charname);      
    }
    else
      printf("ERROR: don't understand %s", buffer);
  }

  if (trace_flag)
  {
    puts("Key replacement table");

    for (k = 0; k < MAXCHRS; k++)
    {
      if (replacement[k] != NULL)
        printf("%d\t%s\n", k, replacement[k]);
    }
  }
}

/* Following used both to read xchr[] file and key replacement file */
/* the flag is 0 for -x=... and the flag is 1 for -k=... */
int read_xchr_file (char *filename, int flag, char *argv[])
{
  FILE *xchr_input;
  char infile[file_name_size];
  char *s;

  if (filename == NULL)
    return -1;

  if (trace_flag)
    printf("Reading xchr/repl %s\n", filename);

  /* first try using file as specified */
  strcpy(infile, filename);

  if (trace_flag)
    printf("Trying %s\n", infile);

  xchr_input = fopen (infile, "r");

  if (xchr_input == NULL)
  {
    if (strrchr(infile, '.') == NULL)
    {
      if (flag == 0)
        strcat(infile, ".map");
      else
        strcat(infile, ".key");

      if (trace_flag)
        printf("Trying %s\n", infile);
      
      xchr_input = fopen(infile, "r");
    }
  }

  if (xchr_input == NULL)
  {
    strcpy(infile, argv[0]);     /* try TeX program path */

    if ((s = strrchr (infile, '\\')) != NULL)
      *(s+1) = '\0';
    else if ((s = strrchr (infile, '/')) != NULL)
      *(s+1) = '\0';
    else if ((s = strrchr (infile, ':')) != NULL)
      *(s+1) = '\0';

    strcat (infile, filename);

    if (trace_flag)
      printf("Trying %s\n", infile);

    xchr_input = fopen (infile, "r");

    if (xchr_input == NULL)
    {
      if (strchr(infile, '.') == NULL)
      {
        if (flag == 0)
          strcat(infile, ".map");
        else
          strcat(infile, ".key");

        if (trace_flag)
          printf("Trying %s\n", infile);

        xchr_input = fopen (infile, "r");
      }
    }
  }

  if (xchr_input == NULL)
  {
    strcpy(infile, argv[0]);     /* try TeX program path */

    if ((s = strrchr (infile, '\\')) != NULL)
      *(s + 1) = '\0';
    else if ((s = strrchr (infile, '/')) != NULL)
      *(s + 1) = '\0';
    else if ((s = strrchr (infile, ':')) != NULL)
      *(s + 1) = '\0';

    strcat(infile, "keyboard\\");
    strcat(infile, filename);

    if (trace_flag)
      printf("Trying %s\n", infile);

    xchr_input = fopen (infile, "r");

    if (xchr_input == NULL)
    {
      if (strchr(infile, '.') == NULL)
      {
        if (flag == 0)
          strcat(infile, ".map");
        else
          strcat(infile, ".key");

        if (trace_flag)
          printf("Trying %s\n", infile);

        xchr_input = fopen (infile, "r");
      }
    }
  }

  /* Note: can't look in TeX source file dir, since that is not known yet */
  if (xchr_input == NULL)
  {
    printf("ERROR: Sorry, cannot find %s file %s",
        flag ? " xchr[]" : "key mapping", filename);
    perrormod (filename);
    return 0;
  }

  if (flag == 0)
    read_xchr_sub(xchr_input);
  else
    read_repl_sub(xchr_input);

  (void) fclose(xchr_input);

  return 1;
}

/* need to also set `key_replace' here based on command line */
/* need to also allocate `buffercopy' here and free at end */
/* need to call `readreplace' in appropriate place */

#define MAXSPLITS 3

/* ad hoc default minimum growth in memory realloc is 62% */
/* golden ratio (1 + \sqrt{5}) / 2 = 1.618033989... */
int percent_grow    = 62; /* default minimum growth in memory realloc is 62% */
int total_allocated = 0;  /* total memory allocated so far */
int ini_max_address = 0;  /* maximum address when starting */
int max_address     = 0;  /* maximum address seen in allocated memory */


void show_maximums (FILE * output)
{
  sprintf(log_line, "Max allocated %d --- max address %d\n", total_allocated, max_address);
  fputs(log_line, output);
}

/* our own version of realloc --- avoid supposed MicroSoft version bug */
/* also tries _expand first, which can avoid address growth ... */

#ifdef USEOUREALLOC 
void * ourrealloc (void * old, size_t new_size)
{
  void * mnew;
  size_t old_size, overlap;
  
  /* round up to nearest multiple of four bytes */
  /* avoid unlikely alignment */
  if ((new_size % 4) != 0)
    new_size = ((new_size / 4) + 1) * 4;

  if (old == NULL)
    return malloc (new_size);  /* no old block - use malloc */

#ifdef _WIN32
  old_size = _msize (old);
#else
  old_size = malloc_usable_size (old);
#endif

  if (old_size >= new_size && old_size < new_size + 4)
    return old;

#ifdef _WIN32
  mnew = _expand (old, new_size); /* first try and expand in place MSVC */
#else
  mnew = realloc (old, new_size);
#endif

  if (mnew != NULL)
  {
    if (trace_flag)
      printf("EXPANDED! %p (%ld) == %p (%ld)\n",
          mnew, new_size, old, old_size);

    return mnew;
  }

  /* do this if you want to call the real realloc next -  */
  mnew = realloc (old, new_size);

  if (mnew != NULL)
    return mnew;

  /*  we are screwed typically if we ever drop through here - no more space */
  mnew = malloc (new_size); /* otherwise find new space */

  if (mnew == NULL)
    return mnew;        /* if unable to allocate */

  if (old_size < new_size)
    overlap = old_size;
  else
    overlap = new_size;

  memcpy (mnew, old, overlap); /* copy old data to new area */
  free(old); /* free the old area */

  return mnew;
}
#endif

void memory_error (const char * s, int n)
{
  if (log_opened)
  {
    log_printf("\n! Unable to allocate %d bytes for %s\n", n, s);
    show_maximums(log_file);
  }

  printf("\n! Unable to allocate %d bytes for %s\n", n, s);
  show_maximums(stderr);
}

void trace_memory (const char * s, int n)
{
  printf("Allocating %d bytes for %s\n", n, s);
}

void update_statistics (long address, int size, int old_size)
{
  if (address + size > max_address)
    max_address = address + size;

  total_allocated =  total_allocated + size - old_size;
}

void probe_memory (void)
{
  char * s = (char *) malloc(sizeof(void *));
  free(s);
  update_statistics ((long) s, 0, 0);
}

void probe_show (void)
{
  probe_memory();
  show_maximums(stdout);
}

size_t roundup (size_t n)
{
  if ((n % sizeof(void *)) == 0)
    return n;
  else
    return ((n / sizeof(void *)) + 1) * sizeof(void *);
}

#ifdef ALLOCATETRIES
/* returns -1 if it fails */

int allocate_tries (int trie_max)
{
  int n, nl, no, nc;

  if (trie_max > 1000000)
    trie_max = 1000000;

  nl = (trie_max + 1) * sizeof(halfword);
  no = (trie_max + 1) * sizeof(halfword);
  nc = (trie_max + 1) * sizeof(quarterword);
  n = nl + no + nc;

  if (trace_flag)
    trace_memory("hyphen trie", n);

  trie_trl = (halfword *) malloc(roundup(nl));
  trie_tro = (halfword *) malloc(roundup(no));
  trie_trc = (quarterword *) malloc(roundup(nc));

  if (trie_trl == NULL || trie_tro == NULL || trie_trc == NULL)
  {
    memory_error("hyphen trie", n);
    return -1;
  }

  if (trace_flag)
    printf("Addresses trie_trl %p trie_tro %p trie_trc %p\n", trie_trl, trie_tro, trie_trc);

  update_statistics((long) trie_trl, nl, 0);
  update_statistics((long) trie_tro, no, 0);
  update_statistics((long) trie_trc, nc, 0);

  trie_size = trie_max;

  if (trace_flag)
    probe_show();

  return 0; // success
}
#endif

#ifdef ALLOCATEHYPHEN
int current_prime = 0; /* remember in case reallocated later */

/* we don't return an address here, since TWO memory regions allocated */
/* plus, we don't really reallocate, we FLUSH the old information totally */
/* returns -1 if it fails */

int realloc_hyphen (int hyphen_prime)
{
  int n, nw, nl;

  if (!prime(hyphen_prime))
  {
    printf("ERROR: non-prime hyphen exception number (%d)\n", hyphen_prime);
    return -1;
  }

/*  need not/cannot preserve old contents when hyphen prime is changed */
/*  if (hyph_list != NULL) free(hyph_list); */
/*  if (hyph_word != NULL) free(hyph_word); */
  nw = (hyphen_prime + 1) * sizeof(str_number);
  nl = (hyphen_prime + 1) * sizeof(halfword);
  n = nw + nl;

  if (trace_flag)
    trace_memory("hyphen exception", n);

  hyph_word = (str_number *) REALLOC (hyph_word, nw);
  hyph_list = (halfword *) REALLOC (hyph_list, nl);

  if (hyph_word == NULL || hyph_list == NULL)
  {
    memory_error("hyphen exception", n);
    return -1;
  }

  if (trace_flag)
    printf("Addresses hyph_word %p hyph_list %p\n", hyph_word, hyph_list);

/*  cannot preserve old contents when hyphen prime is changed */
#ifdef USEMEMSET
  memset(hyph_word, 0, (hyphen_prime + 1) * sizeof (hyph_word[0]));
#else
  for (k = 0; k <= hyphen_prime; k++)
    hyph_word[k]= 0;
#endif

#ifdef USEMEMSET
  memset(hyph_list, 0, (hyphen_prime + 1) * sizeof (hyph_list[0]));
#else
  for (k = 0; k <= hyphen_prime; k++)
    hyph_list[k]= 0;
#endif

  hyph_count = 0;

  if (current_prime != 0)
  {
    update_statistics((long) hyph_word, nw, (current_prime + 1) * sizeof(str_number));
    update_statistics((long) hyph_list, nl, (current_prime + 1) * sizeof(halfword));
  }
  else
  {
    update_statistics((long) hyph_word, nw, 0);
    update_statistics((long) hyph_list, nl, 0);
  }

  current_prime = hyphen_prime;

  if (trace_flag)
    probe_show();

  return 0; // success
}
#endif

int current_mem_size = 0;   /* current total words in main mem allocated -1 */

/* this gets called from itex.c when it figures out what mem_top is */
/* or gets called from here when in ini_TeX mode */ /* and nowhere else */
/* initial allocation only, may get expanded later */
/* NOTE: we DON't use ALLOCATEHIGH & ALLOCATELOW anymore */
/* returns NULL if it fails */

#ifdef ALLOCATEMAIN   
/* initial main memory alloc - mem_top */
memory_word * allocate_main_memory (int size)
{
  int n;

  if (main_memory != NULL)
  {
    if (trace_flag)
      puts("Reallocating initial memory allocation");
  }

  mem_top = mem_bot + size;
  mem_max = mem_top;
  mem_start = 0;     /* bottom of memory allocated by system */
  mem_min = 0;       /* bottom of area made available to TeX */
  n = (mem_max - mem_start + 1) * sizeof (memory_word);

  if (trace_flag)
    trace_memory("main memory", n);

  main_memory = (memory_word *) REALLOC (main_memory, n);

  if (main_memory == NULL)
  {
    memory_error("initial main memory", n);
    return NULL;
  }

  if (trace_flag)
    printf("Address main memory == %p\n", main_memory);

  mem = main_memory;

  if (mem_start != 0 && !is_initex)
    mem = main_memory - mem_start;

  if (trace_flag)
    printf("Offset address main memory == %p\n", mem);

  update_statistics((long) main_memory, n, (current_mem_size + 1) * sizeof (memory_word));
/*  current_mem_size = (mem_max - mem_start + 1); */
  current_mem_size = mem_max - mem_start;   /* total number of words - 1 */

  if (trace_flag)
    probe_show();

  return mem;
}
#endif

#ifdef ALLOCATEMAIN
/* int firstallocation = 1; */

/* increase main memory allocation at low end and high end */
/* called only from tex0.c *//* called with one of lo_size or hi_size == 0 */
/* returns NULL if it fails */

memory_word * realloc_main (int lo_size, int hi_size)
{  
  int k, min_size;
  int new_size = 0;
  int n = 0;
  memory_word * new_memory = NULL;

  if (trace_flag)
    printf("WARNING: Entering realloc_main lo %d hi %d\n", lo_size, hi_size);

  if (is_initex)
  {
    puts("ERROR: Cannot extent main memory in initex");

    if (!knuth_flag)
      puts("Please use `-m=...' on command line");

    return NULL;
  }

  if (trace_flag)
    printf("Old Address %s == %p\n", "main memory", main_memory);

  /* if we REALLY run up to limit ! */
  if (current_mem_size + 1 == max_mem_size)
  {
    memory_error("main memory", (max_mem_size + 1) * sizeof(memory_word));
    return NULL;
  }

/*  first allocation should expand *both* lo and hi */
  if (hi_size == 0 && mem_end == mem_max)
    hi_size = lo_size;

  if (lo_size == 0 && mem_start == mem_min)
    lo_size = hi_size;

/*  try and prevent excessive frequent reallocations */
/*  while avoiding over allocation by too much */
  min_size = current_mem_size / 100 * percent_grow;

  if (lo_size + hi_size < min_size)
  {
    if (lo_size > 0 && hi_size > 0)
    {
      lo_size = min_size / 2;
      hi_size = min_size / 2;
    }
    else if (lo_size > 0)
      lo_size = min_size;
    else if (hi_size > 0)
      hi_size = min_size;
  }

  if (lo_size > 0 && lo_size < mem_top / 2)
    lo_size = mem_top / 2;

  if (hi_size > 0 && hi_size < mem_top / 2)
    hi_size = mem_top / 2;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_mem_size + lo_size + hi_size;

    if (new_size >= max_mem_size) /* bump against limit - ha ha ha */
    {
      while (new_size >= max_mem_size)
      {
        lo_size = lo_size / 2;
        hi_size = hi_size / 2;
        new_size = current_mem_size + lo_size + hi_size;
      }
    }

    n = (new_size + 1) * sizeof (memory_word);

    if (trace_flag)
      trace_memory("main memory", n);

    new_memory = (memory_word *) REALLOC (main_memory, n);

    if (new_memory != NULL)
      break; /* did we get it ? */

    if (current_mem_size == 0)
      break; /* in case we ever use for initial */

    lo_size = lo_size / 2; hi_size = hi_size / 2;
  }

  if (new_memory == NULL)
  {
    memory_error("main memory", n);
    return mem;
  }

  if (trace_flag)
    printf("New Address %s == %p\n", "main memory", new_memory);

  if (lo_size > 0)
  {
/*  shift everything upward to make space for new low area */
    if (trace_flag)
      printf("memmove %p %p %ld \n", new_memory + lo_size,
          new_memory, (current_mem_size + 1) * sizeof(memory_word));

    memmove (new_memory + lo_size, new_memory,
      (current_mem_size + 1) * sizeof(memory_word));
/*  could reduce words moved by (mem_max - mem_end) */
  }

  main_memory = new_memory;       /* remember for free later */

  if (lo_size > 0)
    mem_start = mem_start - lo_size; /* update lower limit */

  if (hi_size > 0)
    mem_max = mem_max + hi_size;   /* update upper limit */

  update_statistics ((long) main_memory, n,
    (current_mem_size + 1) * sizeof (memory_word));
  current_mem_size = new_size;

  if (current_mem_size != mem_max - mem_start)
    puts("ERROR: Impossible Memory Error");

  if (mem_start != 0)
    mem = main_memory - mem_start;
  else
    mem = main_memory;

  if (trace_flag)
    probe_show();

  return mem;
}
#endif

#ifdef ALLOCATEFONT
int current_font_mem_size = 0;

memory_word * realloc_font_info (int size)
{
  memory_word * new_font_info = NULL;
  int k, min_size;
  int new_size = 0;
  int n = 0;

  if (trace_flag)
    printf("Old Address %s == %p\n", "font_info", font_info);

  /* during initial allocation, font_info == NULL - realloc acts like malloc */
  /* during initial allocation current_font_mem_size == 0 */
  if (current_font_mem_size == font_mem_size)  /* if we REALLY run up to limit */
  {
    /* memory_error("font", (font_mem_size + 1) * sizeof(memory_word)); */
    return font_info;    /* pass it back to TeX 99/Fabe/4 */
  }
  /* try and prevent excessive frequent reallocations */
  /* while avoiding over allocation by too much */
  /* min_size = current_font_mem_size / 2; */
  min_size = current_font_mem_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_font_mem_size)
    size = initial_font_mem_size;

  for (k=0; k < MAXSPLITS; k++)
  {
    new_size = current_font_mem_size + size;

    if (new_size > font_mem_size)
      new_size = font_mem_size; /* bump against limit */

/*    important + 1 since fmemoryword font_info[font_mem_size + 1]  original */
    n = (new_size + 1) * sizeof (memory_word);

    if (trace_flag)
      trace_memory("font_info", n);

    new_font_info = (memory_word *) REALLOC (font_info, n);

    if (new_font_info != NULL)
      break;   /* did we get it ? */

    if (current_font_mem_size == 0)
      break; /* initial allocation must work */

    size = size / 2;
  }

  if (new_font_info == NULL)
  {
    memory_error("font", n);
    return font_info;        /* try and continue !!! */
  }

  font_info = new_font_info;

  if (trace_flag)
    printf("New Address %s == %p\n", "font_info", font_info);

  update_statistics ((long) font_info, n, current_font_mem_size * sizeof(memory_word));
  current_font_mem_size = new_size;

  if (trace_flag)
    probe_show();

  return font_info;
}
#endif

#ifdef ALLOCATESTRING
int current_pool_size = 0;

packed_ASCII_code * realloc_str_pool (int size)
{
  int k, min_size;
  int new_size = 0;
  int n = 0;
  packed_ASCII_code * new_str_pool = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "string pool", str_pool);

  if (current_pool_size == pool_size)
  {
/*    memory_error ("string pool", (pool_size + 1) * sizeof(packed_ASCII_code)); */
/*    exit (1); */
    return str_pool;   /* pass it back to TeX 99/Fabe/4 */
  }

  min_size =  current_pool_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_pool_size)
    size = initial_pool_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_pool_size + size;

    if (new_size > pool_size)
      new_size = pool_size;
/* important + 1 since  packed_ASCII_code str_pool[pool_size + 1]; in original */
    n = (new_size + 1) * sizeof (packed_ASCII_code);

    if (trace_flag)
      trace_memory("str_pool", n);

    new_str_pool = (packed_ASCII_code *) REALLOC (str_pool, n); /* 95/Sep/24 */

    if (new_str_pool != NULL)
      break;    /* did we get it ? */

    if (current_pool_size == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_str_pool == NULL)
  {
    memory_error("string pool", n);
    return str_pool;           /* try and continue !!! */
  }

  str_pool = new_str_pool;
  update_statistics ((long) str_pool, n, current_pool_size);
  current_pool_size = new_size;

  if (trace_flag)
    printf("New Address %s == %p\n", "string pool", str_pool);
  
  if (trace_flag)
    probe_show();

  return str_pool;
}
#endif

#ifdef ALLOCATESTRING
int current_max_strings = 0;

pool_pointer * realloc_str_start (int size)
{
  int k, min_size;
  int n = 0;
  int new_size = 0;
  pool_pointer * new_str_start = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "string start", str_start);

  if (current_max_strings == max_strings)
  {
/*    memory_error ("string pointer", (max_strings + 1) * sizeof(pool_pointer)); */
/*    exit (1); */
    return str_start;    /* pass it back to TeX 99/Fabe/4 */
  }

  min_size = current_max_strings / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_max_strings)
    size = initial_max_strings;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_max_strings + size;

    if (new_size > max_strings)
      new_size = max_strings;
/*    important + 1 since str_start[maxstring + 1] originally */
    n = (new_size + 1) * sizeof (pool_pointer);

    if (trace_flag)
      trace_memory("str_start", n);

    new_str_start = (pool_pointer *) REALLOC (str_start, n);

    if (new_str_start != NULL)
      break;   /* did we get it ? */

    if (current_max_strings == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* otherwise can try smaller */
  }

  if (new_str_start == NULL)
  {
    memory_error("string pointer", n);
    return str_start;          /* try and continue */
  }

  str_start = new_str_start;
  update_statistics((long) str_start, n, current_max_strings * sizeof (pool_pointer));
  current_max_strings = new_size;

  if (trace_flag)
    printf("New Address %s == %p\n", "string start", str_start);

  if (trace_flag)
    probe_show();

  return str_start;
}
#endif

#ifdef ALLOCATEINI
/* returns -1 if it fails */
/* size == trie_size */
int allocate_ini (int size)
{
  int n, nl, no, nc, nr, nh, nt;

  nh = (size + 1) * sizeof(trie_pointer);
  nr = (size + 1) * sizeof(trie_pointer);
  nl = (size + 1) * sizeof(trie_pointer);
  no = (size + 1) * sizeof(trie_op_code);
  nc = (size + 1) * sizeof(packed_ASCII_code);
  nt = (size + 1) * sizeof(char);
  n = nl + no + nc + nr + nh + nt;

  if (trace_flag)
    trace_memory ("initex hyphen trie", n);

  trie_l = (trie_pointer *) malloc (roundup(nl));
  trie_o = (trie_op_code *) malloc (roundup(no));
  trie_c = (packed_ASCII_code *) malloc (roundup(nc));
  trie_r = (trie_pointer *) malloc (roundup(nr));
  trie_hash = (trie_pointer *) malloc (roundup(nh));
  trie_taken = (char *) malloc (roundup(nt));
  
  if (trie_c == NULL || trie_o == NULL || trie_l == NULL || trie_r == NULL ||
      trie_hash == NULL || trie_taken == NULL)
  {
    memory_error("initex hyphen trie", n);
    return -1;
  }
  
  if (trace_flag)
  {
    printf("Addresses: trie_l %p trie_o %p trie_c %p\n", trie_l, trie_o, trie_c);
    printf("Addresses: trie_r %p trie_hash %p trie_taken %p\n", trie_r, trie_hash, trie_taken);
  }

  update_statistics ((long) trie_l, nl, 0);
  update_statistics ((long) trie_o, no, 0);
  update_statistics ((long) trie_c, nc, 0);
  update_statistics ((long) trie_r, nr, 0);
  update_statistics ((long) trie_hash, nh, 0);
  update_statistics ((long) trie_taken, nt, 0);

  if (trace_flag)
    probe_show();

  return 0; // success
}
#endif

#ifdef ALLOCATESAVESTACK
int current_save_size = 0;

memory_word * realloc_save_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  memory_word * new_save_stack = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "save stack", save_stack);

  if (current_save_size == save_size)
  {
    return save_stack; /* let TeX handle the error */
  }

  min_size =  current_save_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_save_size)
    size = initial_save_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_save_size + size;

    if (new_size > save_size)
      new_size = save_size;

    n = (new_size + 1) * sizeof (memory_word);

    if (trace_flag)
      trace_memory("save_stack", n);

    new_save_stack = (memory_word *) REALLOC (save_stack, n);

    if (new_save_stack != NULL)
      break;    /* did we get it ? */

    if (current_save_size == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_save_stack == NULL)
  {
    memory_error("save_stack", n);
    return save_stack;           /* try and continue !!! */
  }

  save_stack = new_save_stack;
  update_statistics ((long) save_stack, n, current_save_size);
  current_save_size = new_size;

  if (trace_flag)
  {
    printf("Current %s %d\n", "save_size", current_save_size);
    printf("New Address %s == %p\n", "save stack", save_stack);
  }

  if (trace_flag)
    probe_show();

  return save_stack;
}
#endif

#ifdef ALLOCATEINPUTSTACK
int current_stack_size = 0;       /* input stack size */

in_state_record * realloc_input_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  in_state_record * new_input_stack = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "input stack", input_stack);

  if (current_stack_size == stack_size)
  {
    return input_stack;
  }

  min_size =  current_stack_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_stack_size)
    size = initial_stack_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_stack_size + size;

    if (new_size > stack_size)
      new_size = stack_size;

    n = (new_size + 1) * sizeof(in_state_record);

    if (trace_flag)
      trace_memory("input_stack", n);

    new_input_stack = (in_state_record *) REALLOC (input_stack, n);

    if (new_input_stack != NULL)
      break;   /* did we get it ? */

    if (current_stack_size == 0)
      break; /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_input_stack == NULL)
  {
    memory_error("input stack", n);
    return input_stack;            /* try and continue !!! */
  }

  input_stack = new_input_stack;
  update_statistics ((long) input_stack, n, current_stack_size);
  current_stack_size = new_size;

  if (trace_flag)
  {
    printf("Current %s %d\n", "stack_size", current_stack_size);
    printf("New Address %s == %p\n", "input stack", input_stack);
  }

  if (trace_flag)
    probe_show();

  return input_stack;
}
#endif

#ifdef ALLOCATENESTSTACK
int current_nest_size = 0;        /* current nest size */

list_state_record * realloc_nest_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  list_state_record * new_nest = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "nest stack", nest);

  if (current_nest_size == nest_size)
  {
    return nest;
  }

  min_size =  current_nest_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_nest_size)
    size = initial_nest_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_nest_size + size;

    if (new_size > nest_size)
      new_size = nest_size;

    n = (new_size + 1) * sizeof (list_state_record);

    if (trace_flag)
      trace_memory("nest stack", n);

    new_nest = (list_state_record *) REALLOC (nest, n);

    if (new_nest != NULL)
      break;   /* did we get it ? */

    if (current_nest_size == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_nest == NULL)
  {
    memory_error("nest stack", n);
    return nest;            /* try and continue !!! */
  }

  nest = new_nest;
  update_statistics ((long) nest, n, current_nest_size);
  current_nest_size = new_size;

  if (trace_flag)
  {
    printf("Current %s %d\n", "nest_size", current_nest_size);
    printf("New Address %s == %p\n", "nest stack", nest);
  }

  if (trace_flag)
    probe_show();

  return nest;
}
#endif

#ifdef ALLOCATEPARAMSTACK
int current_param_size = 0;

halfword * realloc_param_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  halfword * new_param = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "param stack", param_stack);

  if (current_param_size == param_size)
  {
    return param_stack;
  }

  min_size =  current_param_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_param_size)
    size = initial_param_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_param_size + size;

    if (new_size > param_size)
      new_size = param_size;

    n = (new_size + 1) * sizeof(pointer);

    if (trace_flag)
      trace_memory("param stack", n);

    new_param = (pointer *) REALLOC (param_stack, n);

    if (new_param != NULL)
      break;    /* did we get it ? */

    if (current_param_size == 0)
      break; /* initial allocation must work */

    size = size / 2; /* else can retry smaller */
  }

  if (new_param == NULL)
  {
    memory_error("param stack", n);
    return param_stack;            /* try and continue !!! */
  }

  param_stack = new_param;
  update_statistics((long) param_stack, n, current_param_size);
  current_param_size = new_size;

  if (trace_flag)
  {
    printf("Current %s %d\n", "param_size", current_param_size);
    printf("New Address %s == %p\n", "param stack", param_stack);
  }

  if (trace_flag)
    probe_show();

  return param_stack;
}
#endif

#ifdef ALLOCATEBUFFER
int current_buf_size = 0;

ASCII_code * realloc_buffer (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  ASCII_code * new_buffer = NULL;

  if (trace_flag)
    printf("Old Address %s == %p\n", "buffer", buffer);

  if (current_buf_size == buf_size)
  {
    return buffer;
  }

  min_size =  current_buf_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_buf_size)
    size = initial_buf_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_buf_size + size;

    if (new_size > buf_size)
      new_size = buf_size;

    n = (new_size + 1) * sizeof(ASCII_code);

    if (trace_flag)
      trace_memory("buffer", n);

    new_buffer = (ASCII_code *) REALLOC (buffer, n);

    if (new_buffer != NULL)
      break;   /* did we get it ? */

    if (current_buf_size == 0)
      break;   /* initial allocation must work */

    size = size / 2;
  }

  if (new_buffer == NULL)
  {
    memory_error("buffer", n);
    return buffer;            /* try and continue !!! */
  }

  buffer = new_buffer;
  update_statistics ((long) buffer, n, current_buf_size);

#ifdef USEMEMSET
  memset(buffer + current_buf_size, 0, new_size - current_buf_size);
#else
  for (k = current_buf_size; k < new_size; k++)
    buffer[k] = 0;
#endif

  current_buf_size = new_size;

  if (trace_flag)
  {
    printf("Current %s %d\n", "buffer", current_buf_size);
    printf("New Address %s == %p\n", "buffer", buffer);
  }

  if (trace_flag)
    probe_show();

  return buffer;
}
#endif

/* here is the main memory allocation routine -- calls the above */
/* returns -1 if it fails */
/* allocate rather than static 93/Nov/26 */
int allocate_memory (void)
{
#ifdef ALLOCATEINPUTSTACK
  input_stack = NULL;
  current_stack_size = 0;
  input_stack = realloc_input_stack(initial_stack_size);
#endif

#ifdef ALLOCATENESTSTACK
  nest = NULL;
  current_nest_size = 0;
  nest = realloc_nest_stack(initial_nest_size);
#endif

#ifdef ALLOCATEPARAMSTACK
  param_stack = NULL;
  current_param_size = 0;
  param_stack = realloc_param_stack(initial_param_size);
#endif

#ifdef ALLOCATESAVESTACK
  save_stack = NULL;
  current_save_size = 0;
  save_stack = realloc_save_stack (initial_save_size);
#endif

#ifdef IGNORED
  buffer = NULL;        /* need to do earlier */
  current_buf_size = 0;
  buffer = realloc_buffer (initial_buf_size);
#endif

#ifdef ALLOCATESTRING
  str_pool = NULL;
  current_pool_size = 0;
  str_start = NULL;
  current_max_strings = 0;

/*  maybe taylor allocations to actual pool file 1300 strings 27000 bytes ? */
  if (is_initex)
  {
    if (trace_flag)
      puts("ini TeX pool and string allocation");

    str_pool = realloc_str_pool(initial_pool_size);
    str_start = realloc_str_start(initial_max_strings);
  }
#endif

/* the following can save a lot of the usual 800k fixed allocation */
#ifdef ALLOCATEFONT
  font_info = NULL;
  current_font_mem_size = 0;
/* if not iniTeX, then do initial allocation on fmt file read in itex.c */
/* if ini-TeX we need to do it here - no format file read later */
  if (is_initex)
    font_info = realloc_font_info(initial_font_mem_size);
#endif

#ifdef ALLOCATEMAIN
  main_memory = NULL;
  mem = NULL;
  mem_min = mem_bot;        /* just to avoid complaints in texbody */
  mem_top = mem_initex;
  mem_max = mem_top;
/* allocate main memory here if this is iniTeX */
/* otherwise wait for format undumping in itex.c ... */
  if (is_initex)
  {
    /* avoid this if format specified on command line ??? */
    mem = allocate_main_memory(mem_initex); /* made variable ! */

    if (mem == NULL)
      return -1;
  }
#endif

/* now for the hyphenation exception stuff */
#ifdef ALLOCATEHYPHEN
  hyph_word = NULL;
  hyph_list = NULL;
/* this will be overridden later by what is in format file */
  hyphen_prime = default_hyphen_prime;
/* non ini-TeX use assumes format will be read and that specifies size */
  if (is_initex)
  {
    if (new_hyphen_prime)
      hyphen_prime = new_hyphen_prime;

    if (realloc_hyphen(hyphen_prime)) /* allocate just in case no format */
      return -1;
  }
#endif

/* now for memory for the part of the hyphenation stuff that always needed */
/* if iniTeX, need to allocate pre-determined fixed amount - trie_size */
/* if iniTeX not selected, allocate only enough later - undump in itex.c ! */
#ifdef ALLOCATETRIES
  if (is_initex)
  {
    if (allocate_tries (trie_size))
      return -1;
  }
#endif

/* now for memory for hyphenation stuff needed only when running iniTeX */
#ifdef ALLOCATEINI
  if (is_initex)
  {
    if (allocate_ini(trie_size))
      return -1;
  }
  else
  {
    trie_l = NULL;
    trie_r = NULL;
    trie_o = NULL;
    trie_hash = NULL;
    trie_c = NULL;
    trie_taken = NULL;
  }
#endif

  return 0; // success
}

/* returns non-zero if error - done to test integrity of stack mostly */
int free_memory (void)
{
  unsigned int heap_total = 0;

  if (trace_flag)
    puts("free_memory ");

  if (verbose_flag || trace_flag)
    show_maximums(stdout); 

  if (trace_flag)
  {
    printf("Heap total: %u bytes --- max address %u\n", 
        heap_total, max_address);
    printf("Main Memory: variable node %lld (%lld - %d);\n"
      "             one word %d (%d - %d)\n",
      lo_mem_max - mem_min, mem_min, lo_mem_max,
      mem_end - hi_mem_min, hi_mem_min, mem_end);
    puts("Freeing memory again");
  }

/* only free memory if safe ... additional check */
#ifdef ALLOCATEINI
  if (is_initex)
  {
    if (trie_taken != NULL)
      free(trie_taken);

    if (trie_hash != NULL)
      free(trie_hash);

    if (trie_r != NULL)
      free(trie_r);

    if (trie_c != NULL)
      free(trie_c);

    if (trie_o != NULL)
      free(trie_o);

    if (trie_l != NULL)
      free(trie_l);

    trie_taken = NULL;
    trie_hash = NULL;
    trie_l = NULL;
    trie_r = NULL;
    trie_c = NULL;
    trie_o = NULL;
  }
#endif

#ifdef ALLOCATETRIES
  if (trie_trc != NULL)
    free(trie_trc);

  if (trie_tro != NULL)
    free(trie_tro);

  if (trie_trl != NULL)
    free(trie_trl);

  trie_trc = NULL;
  trie_tro = NULL;
  trie_trl = NULL;
#endif

#ifdef ALLOCATEHYPHEN
  if (hyph_list != NULL)
    free(hyph_list);

  if (hyph_word != NULL)
    free(hyph_word);

  hyph_list = NULL;
  hyph_word = NULL;
#endif

#ifdef ALLOCATEMAIN
  if (main_memory != NULL)
    free(main_memory);

  main_memory = NULL;
#endif

#ifdef ALLOCATEFONT
  if (font_info != NULL)
    free(font_info);

  font_info = NULL;
#endif

#ifdef ALLOCATESTRING
  if (str_start != NULL)
    free(str_start);

  if (str_pool != NULL)
    free(str_pool);

  str_start = NULL;
  str_pool = NULL;
#endif

#ifdef ALLOCATEPARAMSTACK
  if (param_stack != NULL)
    free(param_stack);

  param_stack = NULL;
#endif

#ifdef ALLOCATENESTSTACK
  if (nest != NULL)
    free(nest);

  nest = NULL;
#endif

#ifdef ALLOCATEINPUTSTACK
  if (input_stack != NULL)
    free(input_stack);

  input_stack = NULL;
#endif

#ifdef ALLOCATESAVESTACK
  if (save_stack != NULL)
    free(save_stack);

  save_stack = NULL;
#endif

  if (format_file != NULL)
    free(format_file);

  if (source_direct != NULL)
    free(source_direct);

  format_file = NULL;
  source_direct = NULL;

  if (dvi_file_name != NULL)
    free(dvi_file_name);

  if (log_file_name != NULL)
    free(log_file_name);

  if (pdf_file_name != NULL)
    free(pdf_file_name);

  pdf_file_name = NULL;
  log_file_name = NULL;
  dvi_file_name = NULL;

  return 0;
}

boolean prime (int x)
{
  int k;
  int sum = 1;    /* 1 + 3 + 5 + k = (k + 1) * (k + 1) / 4 */

  if (x % 2 == 0)
    return false;

  for (k = 3; k < x; k = k + 2)
  {
    if (x % k == 0)
      return false;

    if (sum * 4 > x)
      return true;

    sum += k;
  }

  return true;
}

boolean show_use = false;

void complainarg (int c, char *s)
{
  printf("ERROR: Do not understand `%c' argument value `%s'\n", c, s);
  show_use = true;
}

/* following is list of allowed command line flags and args */

char *allowedargs = "+bcdfijnpqrstvwyzABCDFGIJKLMNOPQRSTVWXYZ023456789?a=e=g=h=k=l=m=o=u=x=E=H=P=U=";

void reorderargs (int ac, char **av)
{
  int n, m;
  char *s, *t;
  char takeargs[256];   /* large enough for all command line arg chars */

  if (ac < 3)
  {
    return; /* no args ! */
  }

  s = allowedargs;
  t = takeargs;   /* list of those that take args */

  while (*s != '\0' && *(s + 1) != '\0')
  {
    if (*(s + 1) == '=')
      *t++ = *s++;   /* copy over --- without the = */

    s++;
  }

  *t = '\0';

  if (trace_flag)
  {
    show_line(takeargs, 0);
    wterm_cr();
  }
  
  n = 1;

  for (;;)
  {
    if (*av[n] != '-')
      break;

    if (n + 1 < ac && *(av[n] + 2) == '\0' &&
      strchr(takeargs, *(av[n] + 1)) != NULL)
      n += 2; /* step over it */
    else
      n++;

    if (n == ac)
      break;
  }

  for (;;)
  {
    if (n == ac)
      break;

    m = n;

    while (m < ac && *av[m] != '-')
      m++;  /* first command */

    if (m == ac)
      break;
/* does it take an argument ? and is this argument next ? */
/* check first whether the `-x' is isolated, or arg follows directly */
/* then check whether this is one of those that takes an argument */
    if (m+1 < ac && *(av[m] + 2) == '\0' &&
      strchr(takeargs, *(av[m] + 1)) != NULL)
    {
      s = av[m];      /*  move command down before non-command */
      t = av[m + 1];

      for (; m > n; m--)
        av[m + 1] = av[m - 1];

      av[n] = s;
      av[n + 1] = t;
      n += 2;       /* step over moved args */
    }
    else
    {
      s = av[m];      /*  move command down before non-command */

      for (; m > n; m--)
        av[m] = av[m - 1];

      av[n] = s;
      n++;        /* step over moved args */
    }
  }
}

int test_align (long address, int size, const char *str)
{
  int n;

  if (size > sizeof(void *))
    n = address % sizeof(void *);
  else
    n = address % size;

  if (n != 0)
    printf("OFFSET %d (ELEMENT %d) in %s\n", n, size, str);

  return n;
}

/* activate detailed checking of alignment when trace_flag is set */

void check_fixed_align (int flag)
{
  (void) flag;

  if (test_align ((long) &mem_top, 4, "FIXED ALIGNMENT"))
  {
    puts("PLEASE RECOMPILE ME!");
  }

#ifdef CHECKALIGNMENT
  if (!flag)
    return;

  test_align ((long) &mem_top, 4, "mem_top");
  test_align ((long) &mem_max, 4, "mem_max");
  test_align ((long) &mem_min, 4, "mem_min");
  test_align ((long) &bad, 4, "bad");
  test_align ((long) &trie_size, 4, "trie_size");
  test_align ((long) &xord, sizeof(xord[0]), "xord");
  test_align ((long) &xchr, sizeof(xchr[0]), "xchr");
  test_align ((long) &name_length, 4, "name_length");
  test_align ((long) &first, 4, "first");
  test_align ((long) &last, 4, "last");
  test_align ((long) &max_buf_stack, 4, "max_buf_stack");
  test_align ((long) &pool_ptr, 4, "pool_ptr");
  test_align ((long) &str_ptr, 4, "str_ptr");
  test_align ((long) &init_pool_ptr, 4, "init_pool_ptr");
  test_align ((long) &init_str_ptr, 4, "init_str_ptr");
  test_align ((long) &log_file, 4, "log_file");
  test_align ((long) &tally, 4, "tally");
  test_align ((long) &term_offset, 4, "term_offset");
  test_align ((long) &file_offset, 4, "file_offset");
  test_align ((long) &trick_count, 4, "trick_count");
  test_align ((long) &first_count, 4, "first_count");
  test_align ((long) &deletions_allowed, 4, "deletions_allowed");
  test_align ((long) &set_box_allowed, 4, "set_box_allowed");
  test_align ((long) &help_line, sizeof(help_line[0]), "help_line");
  test_align ((long) &use_err_help, 4, "use_err_help");
  test_align ((long) &interrupt, 4, "interrupt");
  test_align ((long) &OK_to_interrupt, 4, "OK_to_interrupt");
  test_align ((long) &arith_error, 4, "arith_error");
  test_align ((long) &tex_remainder, 4, "tex_remainder");
  test_align ((long) &temp_ptr, 4, "temp_ptr");
  test_align ((long) &lo_mem_max, 4, "lo_mem_max");
  test_align ((long) &hi_mem_min, 4, "hi_mem_min");
  test_align ((long) &var_used, 4, "var_used");
  test_align ((long) &dyn_used, 4, "dyn_used");
  test_align ((long) &avail, 4, "avail");
  test_align ((long) &mem_end, 4, "mem_end");
  test_align ((long) &mem_start, 4, "mem_start");
  test_align ((long) &rover, 4, "rover");
  test_align ((long) &font_in_short_display, 4, "font_in_short_display");
  test_align ((long) &depth_threshold, 4, "depth_threshold");
  test_align ((long) &breadth_max, 4, "breadth_max");
  test_align ((long) &nest, sizeof(nest[0]), "nest");
  // test_align ((long) &xeq_level, sizeof(xeq_level[0]), "xeq_level");
  test_align ((long) &zzzad, sizeof(zzzad[0]), "zzzad");
  // test_align ((long) &hash, sizeof(hash[0]), "hash");
  test_align ((long) &zzzae, sizeof(zzzae[0]), "zzzae");
  test_align ((long) &save_stack, sizeof(save_stack[0]), "save_stack");
  test_align ((long) &input_stack, sizeof(input_stack[0]), "input_stack");
  test_align ((long) &input_file, sizeof(input_file[0]), "input_file");
  test_align ((long) &line_stack, sizeof(line_stack[0]), "line_stack");
  test_align ((long) &param_stack, sizeof(param_stack[0]), "param_stack");
  test_align ((long) &cur_mark, sizeof(cur_mark[0]), "cur_mark");
  test_align ((long) &pstack, sizeof(pstack[0]), "pstack");
  test_align ((long) &read_file, sizeof(read_file[0]), "read_file");
  test_align ((long) &font_check, sizeof(font_check[0]), "font_check");
  test_align ((long) &font_size, sizeof(font_size[0]), "font_size");
  test_align ((long) &font_dsize, sizeof(font_dsize[0]), "font_dsize");
  test_align ((long) &font_params, sizeof(font_params[0]), "font_params");
  test_align ((long) &font_name, sizeof(font_name[0]), "font_name");
  test_align ((long) &font_area, sizeof(font_area[0]), "font_area");
  test_align ((long) &font_bc, sizeof(font_bc[0]), "font_bc");
  test_align ((long) &font_ec, sizeof(font_ec[0]), "font_ec");
  test_align ((long) &font_glue, sizeof(font_glue[0]), "font_glue");
  test_align ((long) &font_used, sizeof(font_used[0]), "font_used");
  test_align ((long) &hyphen_char, sizeof(hyphen_char[0]), "hyphen_char");
  test_align ((long) &skew_char, sizeof(skew_char[0]), "skew_char");
  test_align ((long) &bchar_label, sizeof(bchar_label[0]), "bchar_label");
  test_align ((long) &font_bchar, sizeof(font_bchar[0]), "font_bchar");
  test_align ((long) &font_false_bchar, sizeof(font_false_bchar[0]), "font_false_bchar");
  test_align ((long) &char_base, sizeof(char_base[0]), "char_base");
  test_align ((long) &width_base, sizeof(width_base[0]), "width_base");
  test_align ((long) &height_base, sizeof(height_base[0]), "height_base");
  test_align ((long) &depth_base, sizeof(depth_base[0]), "depth_base");
  test_align ((long) &italic_base, sizeof(italic_base[0]), "italic_base");
  test_align ((long) &lig_kern_base, sizeof(lig_kern_base[0]), "lig_kern_base");
  test_align ((long) &kern_base, sizeof(kern_base[0]), "kern_base");
  test_align ((long) &exten_base, sizeof(exten_base[0]), "exten_base");
  test_align ((long) &param_base, sizeof(param_base[0]), "param_base");
  test_align ((long) &total_stretch, sizeof(total_stretch[0]), "total_stretch");
  test_align ((long) &total_shrink, sizeof(total_shrink[0]), "total_shrink");
  test_align ((long) &active_width, sizeof(active_width[0]), "active_width");
  test_align ((long) &cur_active_width, sizeof(cur_active_width[0]), "cur_active_width");
  test_align ((long) &background, sizeof(background[0]), "background");
  test_align ((long) &break_width, sizeof(break_width[0]), "break_width");
  test_align ((long) &minimal_demerits, sizeof(minimal_demerits[0]), "minimal_demerits");
  test_align ((long) &best_place, sizeof(best_place[0]), "best_place");
  test_align ((long) &best_pl_line, sizeof(best_pl_line[0]), "best_pl_line");
  test_align ((long) &hc, sizeof(hc[0]), "hc");
  test_align ((long) &hu, sizeof(hu[0]), "hu");
  test_align ((long) &hyf, sizeof(hyf[0]), "hyf");
  // test_align ((long) &x, sizeof(x[0]), "x");
  test_align ((long) &hyf_distance, sizeof(hyf_distance[0]), "hyf_distance");
  test_align ((long) &hyf_num, sizeof(hyf_num[0]), "hyf_num");
  test_align ((long) &hyf_next, sizeof(hyf_next[0]), "hyf_next");
  test_align ((long) &op_start, sizeof(op_start[0]), "op_start");
  // test_align ((long) &trie_op_hash, sizeof(trie_op_hash[0]), "trie_op_hash");
  test_align ((long) &trie_used, sizeof(trie_used[0]), "trie_used");
/*  test_align ((long) &trie_op_lang, sizeof(trie_op_lang[0]), "trie_op_lang");*/
  test_align ((long) &trie_op_val, sizeof(trie_op_val[0]), "trie_op_val");
  test_align ((long) &trie_min, sizeof(trie_min[0]), "trie_min");
  test_align ((long) &page_so_far, sizeof(page_so_far[0]), "page_so_far");
  test_align ((long) &write_file, sizeof(write_file[0]), "write_file");
  test_align ((long) &write_open, sizeof(write_open[0]), "write_open");
#endif
}

void check_alloc_align (int flag)
{
  (void) flag;

  if (test_align((long) eqtb, sizeof(eqtb[0]), "ALLOCATED ALIGNMENT"))
    puts("PLEASE RECOMPILE ME!");

#ifdef CHECKALIGNMENT
  if (!flag)
    return;

#ifndef ALLOCZEQTB
  test_align ((long) eqtb, sizeof(eqtb[0]), "eqtb"); 
#endif

  test_align ((long) str_pool, sizeof(str_pool[0]), "str_pool"); /* no op */
  test_align ((long) str_start, sizeof(str_start[0]), "str_start");
  test_align ((long) mem, sizeof(mem[0]), "main memory");
  test_align ((long) font_info, sizeof(font_info[0]), "font memory");
  test_align ((long) trie_trl, sizeof(trie_trl[0]), "trie_trl");
  test_align ((long) trie_tro, sizeof(trie_tro[0]), "trie_tro");
  test_align ((long) trie_trc, sizeof(trie_trc[0]), "trie_trc");
  test_align ((long) hyph_word, sizeof(hyph_word[0]), "hyph_word");
  test_align ((long) hyph_list, sizeof(hyph_list[0]), "hyph_list");
/*  test_align ((long) trie_c, sizeof(trie_c[0]), "trie_c"); *//* no op */
  test_align ((long) trie_o, sizeof(trie_o[0]), "trie_o");
  test_align ((long) trie_l, sizeof(trie_l[0]), "trie_l");
  test_align ((long) trie_r, sizeof(trie_r[0]), "trie_r");
  test_align ((long) trie_hash, sizeof(trie_hash[0]), "trie_hash");
  test_align ((long) trie_taken, sizeof(trie_taken[0]), "trie_taken");
#endif
}

boolean shorten_file_name  = false; /* don't shorten file names to 8+3 for DOS */

/* cache to prevent allocating twice in a row */

char * lastname  = NULL;
char * lastvalue = NULL;

/* returns allocated string -- these strings are not freed again */
/* is it safe to do that now ? 98/Jan/31 */
char * grabenv (const char * varname)
{
  char * s;

  if (varname == NULL)
    return NULL;

  if (*varname == '\0')
    return NULL;

  if (lastname != NULL && strcasecmp(lastname, varname) == 0)
  {
    if (trace_flag)
      printf("Cache hit: %s=%s\n", lastname, lastvalue);

    return xstrdup(lastvalue);
  }

  s = getenv(varname);

  if (s != NULL)
  {
    if (lastname != NULL)
      free(lastname);

    lastname = xstrdup(varname);

    if (lastvalue != NULL)
      free(lastvalue);

    lastvalue = xstrdup(s);

    return xstrdup(s);
  }
  else
    return NULL;
}

void flush_trailing_slash (char * directory)
{
  char * s;

  if (strcmp(directory, "") != 0)
  {
    s = directory + strlen(directory) - 1;

    if (*s == '\\' || *s == '/')
      *s = '\0';
  }
}

void knuthify (void)
{
  restrict_to_ascii     = false; /* don't complain non ASCII */
  allow_patterns        = false; /* don't allow pattern redefinition */
  show_in_hex           = true;  /* show character code in hex */
  show_numeric          = false; /* don't show character code decimal */
  show_missing          = false; /* don't show missing characters */
  civilize_flag         = false; /* don't reorder date fields */
  c_style_flag          = false; /* don't add file name to error msg */
  show_fmt_flag         = false; /* don't show format file in log */
  show_tfm_flag         = false; /* don't show metric file in log */
  tab_step              = 0;     /* tab's size of width */
  show_line_break_stats = false; /* do not show line break stats */
  show_fonts_used       = false;
  default_rule          = 26214; /* revert to default rule thickness */
  pseudo_tilde          = false;
  pseudo_space          = false;
  truncate_long_lines   = false;
  allow_quoted_names    = false;
  show_cs_names         = false;
  ignore_frozen         = false;
  suppress_f_ligs       = false;
  full_file_name_flag   = false;
  knuth_flag            = true;  /* so other code can know about this */
}

/* following made global so analyze_flag can be made separate procedure */

char * xchr_file = NULL;
char * repl_file = NULL;

const char * short_options = "m:e:h:0:H:g:P:o:l:a:r:kwvpiKLZMd2t?u";

static struct option long_options[] =
{
  {"main-memory",   required_argument, NULL, 'm'},
  {"hyph-size",     required_argument, NULL, 'e'},
  {"trie-size",     required_argument, NULL, 'h'},
  {"backend",       required_argument, NULL, '0'},
  {"tab-step",      required_argument, NULL, 'H'},
  {"percent-grow",  required_argument, NULL, 'g'},
  {"default-rule",  required_argument, NULL, 'P'},
  {"dvi-dir",       required_argument, NULL, 'o'},
  {"log-dir",       required_argument, NULL, 'l'},
  {"aux-dir",       required_argument, NULL, 'a'},
  {"key-file",      required_argument, NULL, 'k'},
  {"jobname",       required_argument, NULL, 'r'},
  {"showhex",       no_argument,       NULL, 'w'},
  {"verbose",       no_argument,       NULL, 'v'},
  {"patterns",      no_argument,       NULL, 'p'},
  {"initex",        no_argument,       NULL, 'i'},
  {"knuthify",      no_argument,       NULL, 'K'},
  {"cstyle",        no_argument,       NULL, 'L'},
  {"showtfm",       no_argument,       NULL, 'Z'},
  {"showmissing",   no_argument,       NULL, 'M'},
  {"deslash",       no_argument,       NULL, 'd'},
  {"suppressflig",  no_argument,       NULL, '2'},
  {"trace",         no_argument,       NULL, 't'},
  {"help",          no_argument,       NULL, '?'},
  {"usage",         no_argument,       NULL, 'u'},
  {NULL,            0, 0, 0}
};

int analyze_flag (int c, char * optarg)
{
  switch (c)
  {
    case 'r':
      c_job_name = optarg;
      break;
    case 'v':
      verbose_flag = true;
      break;
    case 'i':
      is_initex = true;
      break;
    case 'Q':
      interaction = batch_mode;
      break;
    case 'R':
      interaction = nonstop_mode;
      break;
    case 'S':
      interaction = scroll_mode;
      break;
    case 'T':
      interaction = error_stop_mode;
      break;
    case 'K':
      knuthify();
      break;
    case 'L':
      c_style_flag = true;
      break;
    case 'Z':
      show_tfm_flag = true;
      break;
    case 'M':
      show_missing = false;
      break;
    case 'd':
      deslash = false;
      break;
    case 'p':
      allow_patterns = true;
      break;
    case 'w':
      show_in_hex = true;
      break;
    case 'n':
      restrict_to_ascii = true; /* 0 - 127 1994/Jan/21 */
      break;
    case 'f':
      show_fonts_used = false;
      break;
    case '8':
      shorten_file_name = true;
      break;
    case '9':
      show_cs_names = true;
      break;
    case '4':
      ignore_frozen = true;
      break;
    case 'J':
      show_line_break_stats = false; /* 96/Feb/8 */
      break;
    case 'O':
      show_fmt_flag = false; /* 94/Jun/21 */
      break;
    case '2':
      suppress_f_ligs = true; /* 99/Jan/5 f-lig */
      break;
    case 'z':
      full_file_name_flag = false; // 00 Jun 18
      break;
    case 't':
      trace_flag = true;
      break;
/* The following are really obscure and should not be advertized */
    case 's':
      show_current = false;
      break;
    case 'N':
      show_numeric = false;
      break;
    case 'A':
      civilize_flag = false;
      break; 
    case 'B':
      open_trace_flag = true;
      break;
    case 'Y':
      reorder_arg_flag = false; /* local */
      break;

    case 'm':
      if (optarg == 0)
        mem_initex = mem_top;
      else
        mem_initex = atoi(optarg) * 1024;

      if (mem_initex == 0)
        complainarg(c, optarg);

      mem_spec_flag = true;
      break;

#ifdef VARIABLETRIESIZE
    case 'h':
      if (optarg == 0)
      {
        //trie_size = atoi(kpse_var_value("trie_size"));
        trie_size = default_trie_size;
      }
      else
        trie_size = atoi(optarg);

      if (trie_size == 0)
        complainarg(c, optarg);
      break;
#endif

#ifdef ALLOCATEHYPHEN
    case 'e':
      if (optarg == 0)
        new_hyphen_prime = hyphen_prime * 2;
      else
        new_hyphen_prime = atoi(optarg);

      if (new_hyphen_prime == 0)
        complainarg(c, optarg);

      break;
#endif
    case 'g':
      if (optarg == 0)
        percent_grow = 62;
      else
        percent_grow = atoi(optarg);

      if (percent_grow == 0)
        complainarg(c, optarg);

      break;

    case 'U':
      if (optarg == 0)
        pseudo_tilde = 0;
      else
        pseudo_tilde = atoi(optarg);

      if (pseudo_tilde > 255)
        pseudo_tilde = 255;
      else if (pseudo_tilde < 128)
        pseudo_tilde = 128;

      break;

    case 'H':
      if (optarg == 0)
        tab_step = 8;
      else
        tab_step = atoi(optarg);
      if (tab_step == 0)
        complainarg(c, optarg);
      break;

    case 'x':
      if (optarg == 0)
        xchr_file = xstrdup("xchr.map");
      else
        xchr_file = xstrdup(optarg);

      if (xchr_file == NULL || *xchr_file == '\0')
        complainarg(c, optarg);
      break;

    case 'k':
      if (optarg == 0)
        repl_file = xstrdup("repl.key");
      else
        repl_file = xstrdup(optarg);

      if (repl_file == NULL || *repl_file == '\0')
        complainarg(c, optarg);
      break;

    case 'P':
      if (optarg == 0)
        default_rule = 26214;
      else
        default_rule = atoi(optarg);

      if (default_rule == 0)
        complainarg(c, optarg);
      break;

    case 'E':
      if (optarg != 0)
        putenv(optarg);
      else
        complainarg(c, optarg);
      break;

    case 'o':
      if (optarg == 0)
        dvi_directory = "";
      else
        dvi_directory = xstrdup(optarg);

      if (strcmp(dvi_directory, "") == 0)
        complainarg(c, optarg);

      break;

    case 'l':
      if (optarg == 0)
        log_directory = "";
      else
        log_directory = xstrdup(optarg);

      if (strcmp(log_directory, "") == 0)
        complainarg(c, optarg);

      break;

    case 'a':
      if (optarg == 0)
        aux_directory = "";
      else
        aux_directory = xstrdup(optarg);

      if (strcmp(aux_directory, "") == 0)
        complainarg(c, optarg);

      break;

    case '?':
    default:
      show_use = true;
      return -1;
      break;
  }

  return 0;
}

void strip_name (char *pathname)
{
  char *s;

  if ((s = strrchr(pathname, '\\')) != NULL)
    ;
  else if ((s = strrchr(pathname, '/')) != NULL)
    ;
  else if ((s = strrchr(pathname, ':')) != NULL)
    s++;
  else
    s = pathname;

  *s = '\0';
}

int read_command_line (int ac, char **av)
{ 
  int c;
  char *optargnew;
  int option_idx = 0;

  if (ac < 2)
    return 0;

  while ((c = getopt_long_only(ac, av, short_options, long_options, &option_idx)) != EOF)
  {
    if (optarg != 0 && *optarg == '=')
      optargnew = optarg + 1;
    else
      optargnew = optarg;

    analyze_flag(c, optargnew);
  }

  if (show_use)
  {
    stamp_it(log_line);
    strcat(log_line, "\n");
    show_line(log_line, 0);
    show_usage();
    return -1; // failure
  } 

  if (repl_file != NULL && *repl_file != '\0')
  {
    if (read_xchr_file(repl_file, 1, av))
    {
      if (trace_flag)
        puts("KEY REPLACE ON");

      key_replace = true;
    }
  } 

  if (xchr_file != NULL && *xchr_file != '\0')
  {
    if (read_xchr_file(xchr_file, 0, av))
    {
      if (trace_flag)
        puts("NON ASCII ON");

      non_ascii = true;
    }
  } 

  return 0;
}

int init_commands (int ac, char **av)
{
  is_initex             = false; 
  allow_patterns        = false;
  reset_exceptions      = false;
  non_ascii             = false;
  key_replace           = false;
  open_trace_flag       = false;
  trace_flag            = false;
  verbose_flag          = false;
  restrict_to_ascii     = false;
  show_in_hex           = false; /* default is not to show as hex code ^^ 00/Jun/18 */
  return_flag           = true;  // hard wired now
  trimeof               = true;  // hard wired now
  deslash               = true;
  pseudo_tilde          = 254;   /* default '~' replace 95/Sep/26 filledbox DOS 850 */
  pseudo_space          = 255;   /* default ' ' replace 97/June/5 nbspace DOS 850 */
  default_rule          = 26214;
  show_current          = true;
  civilize_flag         = true;
  show_numeric          = true;
  show_missing          = true;
  c_style_flag          = false;
  show_fmt_flag         = true;
  show_tfm_flag         = false; /* don't show metric file in log */
  shorten_file_name     = false; /* don't shorten file names to 8+3 */
  truncate_long_lines   = true;  /* truncate long lines */
  tab_step              = 0;     /* do not replace tabs with spaces */
  show_line_break_stats = true;  /* show line break statistics 96/Feb/8 */
  show_fonts_used       = true;  /* show fonts used in LOG file 97/Dec/24 */
  allow_quoted_names    = true;  /* allow quoted names with spaces 98/Mar/15 */
  show_cs_names         = false;
  knuth_flag            = false;
  full_file_name_flag   = true;  /* new default 2000 June 18 */
  errout                = stdout; /* as opposed to stderr say --- used ??? */
  new_hyphen_prime      = 0;

#ifdef VARIABLETRIESIZE
  // trie_size = default_trie_size;
  trie_size = 0;
#endif

  mem_extra_high = 0;
  mem_extra_low  = 0;
  mem_initex     = 0;
  format_name    = "plain";

  if (read_command_line(ac, av) < 0)
    return -1;

  if (optind == 0)
    optind = ac;

  return 0;
}

void initial_memory (void)
{
  /* set initial memory allocations */
  if (mem_extra_high < 0)
    mem_extra_high = 0;

  if (mem_extra_low < 0)
    mem_extra_low = 0;

  if (mem_initex < 0)
    mem_initex = 0;

  if (!is_initex)
  {
    if (mem_initex != 0)
    {
      puts("ERROR: Can only set initial main memory size in initex");
      mem_initex = 0;
    }

    if (trie_size != 0)
    {
      puts("ERROR: Need only set hyphenation trie size in initex");
/* trie_size = 0; */
    }
  }

  if (mem_initex == 0)
    mem_initex = default_mem_top;

  if (trie_size == 0)
    trie_size = default_trie_size;

/* Just in case user mistakenly specified words instead of kilo words */
  if (mem_extra_high > 10000L * 1024L)
    mem_extra_high = mem_extra_high / 1024;

  if (mem_extra_low > 10000L * 1024L)
    mem_extra_low = mem_extra_low / 1024;

  if (mem_initex > 10000L * 1024L)
    mem_initex = mem_initex / 1024;

  if (mem_initex > 2048L * 1024L) /* extend main memory by 16 mega byte! */
  {
    puts("WARNING: There may be no benefit to asking for so much memory");
/* mem_initex = 2048 * 1024; */
  }

  if (new_hyphen_prime < 0)
    new_hyphen_prime = 0;

  if (new_hyphen_prime > 0)
  {
    if (! is_initex)
      puts("ERROR: Can only set hyphen prime in initex");
    else
    {
      if (new_hyphen_prime % 2 == 0)
        new_hyphen_prime++;

      while (!prime(new_hyphen_prime))
        new_hyphen_prime = new_hyphen_prime + 2;

      if (trace_flag)
        printf("Using %d as hyphen prime\n", new_hyphen_prime);
    }
  }

  if (percent_grow > 100)
    percent_grow = percent_grow - 100;

  if (percent_grow > 100)
    percent_grow = 100;   /* upper limit - double */

  if (percent_grow < 10)
    percent_grow = 10;   /* lower limit - 10% */
}

void perrormod (const char * s)
{
  printf("`%s': %s\n", s, strerror(errno));
}

/* convert tilde to pseudo_tilde to hide it from TeX --- 95/Sep/26 */
/* convert space to pseudo_space to hide it from TeX --- 97/Jun/5 */
/* called only if pseudo_tilde != 0 or pseudo_space != 0 */
/* this is then undone in tex3.c both for fopen input and output */
/* not ideal, since pseudo name appears in log and in error messages ... */

void hidetwiddle (char *tname)
{
  char *s = tname;

#ifdef DEBUGTWIDDLE
  if (trace_flag)
    printf("Hidetwiddle %s", tname);
#endif

  while (*s != '\0')
  {
    if (*s == '~' && pseudo_tilde != 0)
      *s = (char) pseudo_tilde;  /* typically 254 */
    else if (*s == ' ' && pseudo_space != 0)
      *s = (char) pseudo_space;  /* typically 255 */
    s++;
  }

#ifdef DEBUGTWIDDLE
  if (trace_flag)
    printf("=> %s\n", tname);
#endif
}

void deslash_all (int ac, char **av)
{
  char buffer[file_name_size];  
  char *s;

  if ((s = grabenv("TEXDVI")) != NULL)
    dvi_directory = s;

  if ((s = grabenv("TEXLOG")) != NULL)
    log_directory = s;

  if ((s = grabenv("TEXAUX")) != NULL)
    aux_directory = s;

  if ((s = grabenv("TEXFMT")) != NULL)
    fmt_directory = s;

  if ((s = grabenv("TEXPDF")) != NULL)
    pdf_directory = s;

  strcpy(buffer, av[0]);

  if ((s = strrchr(buffer, '\\')) != NULL)
    *(s + 1) = '\0';
  else if ((s = strrchr(buffer, '/')) != NULL)
    *(s + 1) = '\0';
  else if ((s = strrchr(buffer, ':')) != NULL)
    *(s + 1) = '\0';

  s = buffer + strlen(buffer) - 1;

  if (*s == '\\' || *s == '/')
    *s = '\0';

  if (strcmp(dvi_directory, "") != 0)
    flush_trailing_slash(dvi_directory);

  if (strcmp(log_directory, "") != 0)
    flush_trailing_slash(log_directory);

  if (strcmp(aux_directory, "") != 0)
    flush_trailing_slash(aux_directory);

  if (strcmp(fmt_directory, "") != 0)
    flush_trailing_slash(fmt_directory);

  if (strcmp(pdf_directory, "") != 0)
    flush_trailing_slash(pdf_directory);

  if (deslash)
  {
    if (strcmp(dvi_directory, "") != 0)
      unixify(dvi_directory);
    
    if (strcmp(log_directory, "") != 0)
      unixify(log_directory);

    if (strcmp(aux_directory, "") != 0)
      unixify(aux_directory);

    if (strcmp(fmt_directory, "") != 0)
      unixify(fmt_directory);

    if (strcmp(pdf_directory, "") != 0)
      unixify(pdf_directory);
  }

  format_spec = false;

  if (optind < ac && optind > 0)
  {
    if (deslash)
    {
      if (trace_flag)
        printf("deslash: k %d argv[k] %s (argc %d)\n", optind, av[optind], ac);

      unixify(av[optind]);
    }

    if (pseudo_tilde != 0 || pseudo_space != 0)
      hidetwiddle(av[optind]);

    if (*av[optind] == '&' || *av[optind] == '+')
    {
      format_spec = true;
      format_name = xstrdup(av[optind] + 1);

      if (optind + 1 < ac)
      {
        if (deslash)
        {
          if (trace_flag)
            printf("deslash: k %d argv[k] %s (argc %d)\n", optind + 1, av[optind + 1], ac);

          unixify(av[optind + 1]);
        }

        if (pseudo_tilde != 0 || pseudo_space != 0)
          hidetwiddle(av[optind + 1]);
      }
    }         
  }
}

int main_init (int ac, char ** av)
{
  kpse_set_program_name(av[0], "dvipdfmx");
  init_kanji();
  xputenv("engine", "ptex-ng");

  if (sizeof(memory_word) != sizeof(halfword) * 2)
    printf("ERROR: Bad word size %ld!\n", sizeof(memory_word));

  start_time = clock();
  main_time  = start_time;

/* reset all allocatable memory pointers to NULL - in case we drop out */
  main_memory = NULL;
  font_info   = NULL;
  str_pool    = NULL;
  str_start   = NULL;

#ifdef ALLOCATESAVESTACK
  save_stack = NULL; 
#endif

#ifdef ALLOCATEBUFFER
  buffer           = NULL;
  current_buf_size = 0;
  buffer           = realloc_buffer(initial_buf_size);
#endif

  hyph_list  = NULL;
  hyph_word  = NULL;
  trie_taken = NULL;
  trie_hash  = NULL;
  trie_r     = NULL;
  trie_c     = NULL;
  trie_o     = NULL;
  trie_l     = NULL;
  trie_trc   = NULL;
  trie_tro   = NULL;
  trie_trl   = NULL;

  log_opened          = false;  /* so can tell whether opened */
  interaction         = -1;     /* default state => 3 */
  missing_characters  = 0;      /* none yet! */
  ignore_frozen       = false;  /* default is not to ignore 98/Oct/5 */
  suppress_f_ligs     = false;  /* default is not to ignore f-ligs */

  if (ac > 1 && !strncmp(av[1], "-Y", 2))
    reorder_arg_flag = false;

  if (reorder_arg_flag)
    reorderargs(ac, av);  

  if (init_commands(ac, av))
    return -1;

  check_fixed_align(trace_flag);

  format_file   = NULL;
  source_direct = NULL;
  dvi_file_name = NULL;
  log_file_name = NULL;
  pdf_file_name = NULL;

  first_pass_count  = 0;
  second_pass_count = 0;
  final_pass_count  = 0;
  paragraph_failed  = 0;
  single_line       = 0;
  overfull_hbox     = 0;
  underfull_hbox    = 0;
  overfull_vbox     = 0;
  underfull_vbox    = 0;

  if (trace_flag)
    puts("Entering main_init() (local.c).");

  probe_memory();
  ini_max_address = max_address;

  if (trace_flag)
    show_maximums(stdout);

  initial_memory();
  deslash_all(ac, av);

  if (format_spec && mem_spec_flag)
    puts("WARNING: Cannot change initial main_memory size when format specified");

  if (allocate_memory() != 0)
    return -1;

  check_alloc_align(trace_flag);

  if (trace_flag)
    puts("Leaving main_init() (local.c).");

  return 0;
}

#ifdef __APPLE__
#undef CLK_TCK
#endif
#define CLK_TCK CLOCKS_PER_SEC


void show_inter_val (clock_t inter_val)
{
  int seconds, tenths, hundredth, thousands;

  if (inter_val >= CLK_TCK * 10)
  {
    tenths = (inter_val * 10 + CLK_TCK / 2) / CLK_TCK; 
    seconds = tenths / 10; 
    tenths = tenths % 10;
    printf("%d.%d", seconds, tenths);
  }
  else if (inter_val >= CLK_TCK)
  {
    hundredth = (inter_val * 100 + CLK_TCK / 2) / CLK_TCK;
    seconds = hundredth / 100;
    hundredth = hundredth % 100;
    printf("%d.%02d", seconds, hundredth);
  }
  else if (inter_val > 0)
  {
    thousands = (inter_val * 1000 + CLK_TCK / 2) / CLK_TCK;
    seconds = thousands / 1000;
    thousands = thousands % 1000;
    printf("%d.%03d", seconds, thousands);
  }
  else
    show_line("0", 0);
}

int endit (int flag)
{
  finish_time = clock();

  if (missing_characters != 0)
    flag = 1;

  if (missing_characters)
    printf("! There %s %d missing character%s --- see log file\n",
      (missing_characters == 1) ? "was" : "were", missing_characters,
      (missing_characters == 1) ? "" : "s");

  if (free_memory() != 0)
    flag++;

  if (verbose_flag)
  {
    printf("Total ");
    show_inter_val(finish_time - start_time);
    printf(" sec (");
    show_inter_val(main_time - start_time);
    printf(" format load + ");
    show_inter_val(finish_time - main_time);
    printf(" processing) ");

    if (total_pages > 0)
    {
      show_inter_val((finish_time - main_time) / total_pages);
      printf(" sec per page.\n");
    }
  }

  return flag;
}
// printf control sequences' name
void print_cs_name (FILE * output, int h)
{
  int c, textof, n;

  memset(log_line, 0, sizeof(log_line));

  textof = hash[h].rh;

  if (textof == 0)
    return;

  c = sprintf(log_line, "(%d), ", h);
  n = length(textof);

  memmove(log_line + c, str_pool + str_start[textof], n);
  memmove(log_line + c + n, "\n", 2);

  if (output == stderr)
    show_line(log_line, 1);
  else if (output == stdout)
    show_line(log_line, 0);
  else
    fprintf(output, "%s", log_line);
}
// prototype
int compare_strn (int, int, int, int);
/* compare two csnames in qsort */
int compare_cs (const void *cp1, const void *cp2)
{
  int c1, c2, l1, l2, k1, k2, textof1, textof2;

  c1 = *(int *)cp1;
  c2 = *(int *)cp2;
  textof1 = hash[c1].rh;
  textof2 = hash[c2].rh;
  l1 = length(textof1);
  l2 = length(textof2);
  k1 = str_start[textof1];
  k2 = str_start[textof2];

  return compare_strn(k1, l1, k2, l2);
}

char * csused = NULL;

/* Allocate table of indeces to allow sorting on csname */
/* Allocate flags to remember which ones already listed at start */
/* pass = 0 --> fmt   */
/* pass = 1 --> after */
void print_cs_names (FILE *output, int pass)
{
  int h, k, ccount, repeatflag;
  int *cnumtable;
  int nfcs = frozen_control_sequence;

  if (pass == 0 && csused == NULL)
  {
    csused = (char *) malloc (nfcs);

    if (csused == NULL)
      return;

#ifdef USEMEMSET
    memset(csused, 0, nfcs);
#else
    for (h = 0; h < (hash_size + 780); h++)
      csused[h] = 0;
#endif
  }

  ccount = 0;

  for (h = hash_base + 1; h < nfcs; h++)
  {
    if (pass == 1 && csused[h])
      continue;

    if (text(h) != 0)
    {
      if (pass == 0)
        csused[h] = 1;

      ccount++;
    }
  }

  sprintf(log_line, "\n%d %s multiletter control sequences:\n",
      ccount, (pass == 1) ? "new" : "");

  if (output == stderr)
    show_line(log_line, 1);
  else if (output == stdout)
    show_line(log_line, 0);
  else
    fprintf(output, "%s", log_line);

  if (ccount > 0)
  {
    cnumtable = (int *) malloc (ccount * sizeof(int));

    if (cnumtable == NULL)
      return;

    ccount = 0;

    for (h = hash_base + 1; h < nfcs; h++)
    {
      if (pass == 1 && csused[h])
        continue;

      if (hash[h].rh != 0)
        cnumtable[ccount++] = h;
    }

    //qsort ((void *)cnumtable, ccount, sizeof (int), &compare_cs);

    repeatflag = 0;

    for (k = 0; k < ccount; k++)
    {
      h = cnumtable[k];

      if (pass == 1 && csused[h])
        continue;

      print_cs_name(output, h);
    }

    sprintf(log_line, "\n");

    if (output == stderr)
      show_line(log_line, 1);
    else if (output == stdout)
      show_line(log_line, 0);
    else
      fprintf(output, "%s", log_line);

    free((void *)cnumtable);
  }

  if (pass == 1 && csused != NULL)
  {
    free(csused);
    csused = NULL;
  }
}

/* k1 and k2 are positions in string pool */
/* l1 and l2 are lengths of strings */
int compare_strn (int k1, int l1, int k2, int l2)
{
  int c1, c2;

  while (l1 > 0 && l2 > 0)
  {
    c1 = str_pool[k1];
    c2 = str_pool[k2];

    if (c1 > c2)
      return 1;
    else if (c2 > c1)
      return -1;

    l1--; l2--;
    k1++; k2++;
  }

  if (l1 > 0)
    return 1;   /* first string longer */
  else if (l2 > 0)
    return -1; /* second string longer */

  return 0;         /* strings match */
}
/* compare two font names and their at sizes in qsort */
int compare_fnt (const void * fp1, const void * fp2)
{
  int f1, f2, l1, l2, k1, k2, s;

  f1 = *(short *)fp1;
  f2 = *(short *)fp2;
  l1 = length(font_name[f1]);
  l2 = length(font_name[f2]);
  k1 = str_start[font_name[f1]]; 
  k2 = str_start[font_name[f2]]; 

  s = compare_strn (k1, l1, k2, l2);

  if (s != 0)
    return s;

  if (font_size[f1] > font_size[f2])
    return 1;
  else if (font_size[f1] < font_size[f2])
    return -1;

  return 0;         /* should not ever get here */
}
/* compare two font names */
int compare_fnt_name (int f1, int f2)
{
  int l1, l2, k1, k2, s;

  l1 = length(font_name[f1]);
  l2 = length(font_name[f2]); 
  k1 = str_start[font_name[f1]]; 
  k2 = str_start[font_name[f2]]; 

  s = compare_strn (k1, l1, k2, l2);

  return s;
}
/* decode checksum information */
const unsigned long checkdefault = 0x59265920;
int decode_fourty (unsigned long checksum, char *codingvector)
{
  int c;
  int k;

  if (checksum == 0)
  {
    strcpy(codingvector, "unknwn");
    return 1;
  }
  else if ((checksum >> 8) == (checkdefault >> 8))
  {
    strcpy (codingvector, "fixed ");
    return 1;
  }
  else
  {
    for (k = 0; k < 6; k++)
    {
      c = (int) (checksum % 40);
      checksum = checksum / 40;
      
      if (c <= 'z' - 'a')
        c = c + 'a';
      else if (c < 36)
        c = (c + '0') - ('z' - 'a') - 1;
      else if (c == 36)
        c = '-';
      else if (c == 37)
        c = '&';
      else if (c == 38)
        c = '_';
      else
        c = '.';
      
      codingvector[5-k] = (char) c;
    }

    codingvector[6] = '\0';
  }

  return 0;
}

double sclpnt (long x)
{
  double pt;

  pt = (double) x / 65536.0;
  pt = (double) ((int) (pt * 1000.0 + 0.5)) / 1000.0;

  return (pt);
}

void dvi_font_show (internal_font_number f, int suppressname)
{
  int a, l, k, n;
  unsigned long checksum;
  char checksumvector[8];
  char buffer[32];

  putc(' ', log_file);

  if (suppressname == 0)
  {
    a = length(font_area[f]);
    l = length(font_name[f]);

    k = str_start[font_area[f]];

    memcpy(buffer, str_pool + k, length(font_area[f]));
    fwrite(buffer, sizeof(char), length(font_area[f]), log_file);

    k = str_start[font_name[f]];

    memcpy(buffer, str_pool + k, length(font_name[f]));
    fwrite(buffer, sizeof(char), length(font_name[f]), log_file);
  }
  else a = l = 0;

  for (k = a + l; k < 16; k++)
    putc(' ', log_file);

  sprintf(buffer, "at %lgpt ", sclpnt(font_size[f]));
  fputs(buffer, log_file);

  if (suppressname == 0)
  {
    n = strlen(buffer);

    for (k = n; k < 16; k++)
      putc(' ', log_file);

    checksum = (((font_check[f].b0) << 8 | font_check[f].b1) << 8 | font_check[f].b2) << 8 | font_check[f].b3;
    decode_fourty(checksum, checksumvector);
    log_printf("encoding: %s..", checksumvector);
  }

  putc('\n', log_file);
}
/* Allocate table of indeces to allow sorting on font name */
void show_font_info (void)
{
  int k, m, fcount, repeatflag;
  short *fnumtable;

  fcount = 0;

  for (k = 1; k <= font_ptr; k++)
    if (font_used[k])
      fcount++;

  if (fcount == 0)
    return;

  fnumtable = (short *) malloc(fcount * sizeof(short));

  log_printf("\nUsed %d font%s:\n", fcount, (fcount == 1) ? "" : "s");

  fcount = 0;

  for (k = 1; k <= font_ptr; k++) 
    if (font_used[k])
      fnumtable[fcount++] = (short) k;

  qsort ((void *)fnumtable, fcount, sizeof(short), &compare_fnt);

  repeatflag = 0;

  for (m = 0; m < fcount; m++)
  {
    if (m > 0)
    {
      if (compare_fnt_name(fnumtable[m - 1], fnumtable[m]) == 0)
        repeatflag = 1;
      else
        repeatflag = 0;
    }

    dvi_font_show(fnumtable[m], repeatflag);
  }

  free((void *)fnumtable);
}
