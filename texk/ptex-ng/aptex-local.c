/*
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

#if defined (__ANDROID__)

/*
 for android-ndk-r10c
 
 1. arm64-v8a   --> __arm__ __aarch64__
 2. armeabi-v7a --> __arm__ __ARM_ARCH_7A__
 3. armeabi     --> __arm__
 4. x86         --> __i386__
 5. x86_64      --> __x86_64__
 6. mips        --> __mips__
 7. mips64      --> __mips64
*/

  #if defined (__mips64) || defined (__aarch64__) || defined (__x86_64__)
    #include <malloc.h>
  #else
    #define malloc_usable_size dlmalloc_usable_size
  #endif
#elif defined (__APPLE__)

/*
 for Mac OS X, iOS
 */

  #include <malloc/malloc.h>
  #define malloc_usable_size malloc_size
#elif defined (__gnu_linux__)
  #include <malloc.h>
#endif

#include "aptex.h"

#define USEOUREALLOC

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

#if defined (W32TeX)
const char * banner = "This is Asian pTeX, Version 3.14159265 (W32TeX)";
#else
const char * banner = "This is Asian pTeX, Version 3.14159265";
#endif

char * dvi_directory = "";
char * log_directory = "";
char * aux_directory = "";
char * fmt_directory = "";
char * pdf_directory = "";

static boolean mem_spec_flag = false;
static boolean format_spec   = false;

static void show_usage (void)
{
  printf("\n"
      "Useage: aptex [OPTION]... [+fmt_file_name] [file]\n\n"
      " --help          show this usage summary\n"
      " --version       output version information and exit\n"
      " --jobname=str   set the job name to str\n"
      " --progname=str  set program (and fmt) name to str\n"
      " --synctex=num   generate SyncTeX data for previewers if nonzero\n"
      " --ini           start up as initex (create format file)\n"
      " --verbose       be verbose (show implementation version number)\n"
      " --showhex       do not show `non ASCII' characters in hexadecimal\n"
      "                   (show as is)\n"
      " --patterns      allow use of \\patterns after loading format (initex only)\n"
      " --knuthify      disable all extensions to basic TeX\n"
      " --main-mem      initial main memory size in kilo words (initex only)\n"
      " --hyph-size     hyphenation exception dictionary size (initex only)\n"
      " --trie-size     hyphenation pattern trie size (initex only)\n"
      " --pdf-dir       write PDF file in specified directory (default '.')\n"
      " --log-dir       write LOG file in specified directory (default '.')\n"
      " --aux-dir       write AUX file in specified directory (default '.')\n\n"
      "Email bug reports to clerkma@gmail.com.\n");
  aptex_utils_exit(EXIT_FAILURE);
}

static void show_version (void)
{
  printf("Copyright 2014, 2015 Clerk Ma.\n"
    "%s\n"
    "Compiled with %s\n"
    "Compiled with %s\n"
    "Compiled with zlib version %s\n"
    "Compiled with synctex (build-in edition)\n\n",
    banner, kpathsea_version_string,
    ptexenc_version_string, zlib_version);
  aptex_utils_exit(EXIT_FAILURE);
}

// Sep 27 1990 => 1990 Sep 27
// 0123456789     0123456789
static void scivilize (char * date)
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

static void print_aptex_info (void)
{
  char date[11 + 1];

  strcpy(date, __DATE__);
  scivilize(date);
  printf("Asian pTeX (compiled time: %s %s with %s/%s)\n",
    date, __TIME__, dist, compiler);
}

#define MAXSPLITS 3

/* ad hoc default minimum growth in memory realloc is 62% */
/* golden ratio (1 + \sqrt{5}) / 2 = 1.618033989... */
static int percent_grow    = 62; /* default minimum growth in memory realloc is 62% */
static int total_allocated = 0;  /* total memory allocated so far */
static int ini_max_address = 0;  /* maximum address when starting */
static int max_address     = 0;  /* maximum address seen in allocated memory */

static void show_maximums (FILE * output)
{
  sprintf(aptex_log_line, "Max allocated %d --- max address %d\n", total_allocated, max_address);
  fputs(aptex_log_line, output);
}

static void aptex_trace (const char * fmt_str, ...)
{
  va_list m_ptr;
  va_start(m_ptr, fmt_str);

  if (aptex_env.trace_mem)
    vprintf(fmt_str, m_ptr);

  va_end(m_ptr);
}


/* our own version of realloc --- avoid supposed Microsoft version bug */
/* also tries _expand first, which can avoid address growth ... */

#ifdef USEOUREALLOC
static void * ourrealloc (void * old, size_t new_size)
{
  void * mnew;
  size_t old_size, overlap;

  if ((new_size % 4) != 0)
    new_size = ((new_size / 4) + 1) * 4;

  if (old == NULL)
    return malloc(new_size);

#ifdef _WIN32
  old_size = _msize(old);
#else
  old_size = malloc_usable_size(old);
#endif

  if (old_size >= new_size && old_size < new_size + 4)
    return old;

#ifdef _WIN32
  mnew = _expand(old, new_size);
#else
  mnew = realloc(old, new_size);
#endif

  if (mnew != NULL)
  {
    aptex_trace("EXPANDED! %p (%ld) == %p (%ld)\n", mnew, new_size, old, old_size);
    return mnew;
  }

  /* do this if you want to call the real realloc next - */
  mnew = realloc(old, new_size);

  if (mnew != NULL)
    return mnew;

  /*  we are screwed typically if we ever drop through here - no more space */
  mnew = malloc(new_size); /* otherwise find new space */

  if (mnew == NULL)
    return mnew;        /* if unable to allocate */

  if (old_size < new_size)
    overlap = old_size;
  else
    overlap = new_size;

  memcpy(mnew, old, overlap);
  free(old);

  return mnew;
}
#endif

static void memory_error (const char * s, int n)
{
  int old_setting;

  if (log_opened)
  {
    old_setting = selector;
    selector = term_and_log;
    print_ln();
    prints("! Unable to allocate ");
    print_int(n);
    prints(" bytes for ");
    prints(s);
    print_ln();
    prints("Max allocated ");
    print_int(total_allocated);
    prints(" --- max address ");
    print_int(max_address);
    print_ln();
    selector = old_setting;
  }
}

static void trace_memory (const char * s, int n)
{
  aptex_trace("Allocating %d bytes for %s\n", n, s);
}

static void update_statistics (long address, int size, int old_size)
{
  if (address + size > max_address)
    max_address = address + size;

  total_allocated = total_allocated + size - old_size;
}

static void memory_probe (void)
{
  char * s = (char *) malloc(sizeof(void *));
  free(s);
  update_statistics ((long) s, 0, 0);
}

static void probe_show (void)
{
  memory_probe();
  show_maximums(stdout);
}

static size_t roundup (size_t n)
{
  if ((n % sizeof(void *)) == 0)
    return n;
  else
    return ((n / sizeof(void *)) + 1) * sizeof(void *);
}

static boolean prime (int x)
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


#ifdef APTEX_EXTENSION
// -1 - fails
//  0 - success

int allocate_tries (int trie_max)
{
  int n, nl, no, nc;

  if (trie_max > 1000000)
    trie_max = 1000000;

  nl = (trie_max + 1) * sizeof(halfword);
  no = (trie_max + 1) * sizeof(halfword);
  nc = (trie_max + 1) * sizeof(quarterword);
  n = nl + no + nc;
  trace_memory("hyphen trie", n);
  trie_trl = (halfword *) malloc(roundup(nl));
  trie_tro = (halfword *) malloc(roundup(no));
  trie_trc = (quarterword *) malloc(roundup(nc));

  if (trie_trl == NULL || trie_tro == NULL || trie_trc == NULL)
  {
    memory_error("hyphen trie", n);
    return -1;
  }

  aptex_trace("Addresses trie_trl %p trie_tro %p trie_trc %p\n", trie_trl, trie_tro, trie_trc);
  update_statistics((long) trie_trl, nl, 0);
  update_statistics((long) trie_tro, no, 0);
  update_statistics((long) trie_trc, nc, 0);

  trie_size = trie_max;

  if (aptex_env.trace_mem)
    probe_show();

  return 0;
}

static int current_prime = 0; /* remember in case reallocated later */

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
  trace_memory("hyphen exception", n);
  hyph_word = (str_number *) REALLOC(hyph_word, nw);
  hyph_list = (halfword *) REALLOC(hyph_list, nl);

  if (hyph_word == NULL || hyph_list == NULL)
  {
    memory_error("hyphen exception", n);
    return -1;
  }

  aptex_trace("Addresses hyph_word %p hyph_list %p\n", hyph_word, hyph_list);
  memset(hyph_word, 0, (hyphen_prime + 1) * sizeof (hyph_word[0]));
  memset(hyph_list, 0, (hyphen_prime + 1) * sizeof (hyph_list[0]));
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

  if (aptex_env.trace_mem)
    probe_show();

  return 0; // success
}

memory_word * allocate_main_memory (int size)
{
  int n;

  if (main_memory != NULL)
    aptex_trace("Reallocating initial memory allocation");

  mem_top = mem_bot + size;
  mem_max = mem_top;
  mem_start = 0;     /* bottom of memory allocated by system */
  mem_min = 0;       /* bottom of area made available to TeX */
  n = (mem_max - mem_start + 1) * ((int) sizeof(memory_word));
  trace_memory("main memory", n);
  main_memory = (memory_word *) REALLOC (main_memory, n);

  if (main_memory == NULL)
  {
    memory_error("initial main memory", n);
    return NULL;
  }

  aptex_trace("Address main memory == %p\n", main_memory);
  mem = main_memory;

  if (mem_start != 0 && !aptex_env.flag_initex)
    mem = main_memory - mem_start;

  aptex_trace("Offset address main memory == %p\n", mem);
  update_statistics((long) main_memory, n, (current_mem_size + 1) * sizeof (memory_word));
/*  current_mem_size = (mem_max - mem_start + 1); */
  current_mem_size = mem_max - mem_start;   /* total number of words - 1 */

  if (aptex_env.trace_mem)
    probe_show();

  return mem;
}

/* increase main memory allocation at low end and high end */
/* called with one of lo_size or hi_size == 0 */
/* returns NULL if it fails */

memory_word * realloc_main (int lo_size, int hi_size)
{  
  int k, min_size;
  int new_size = 0;
  int n = 0;
  memory_word * new_memory = NULL;

  aptex_trace("WARNING: Entering realloc_main lo %d hi %d\n", lo_size, hi_size);

  if (aptex_env.flag_initex)
  {
    puts("ERROR: Cannot extent main memory in initex");
    return NULL;
  }

  aptex_trace("Old Address %s == %p\n", "main memory", main_memory);

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

  aptex_trace("New Address %s == %p\n", "main memory", new_memory);

  if (lo_size > 0)
  {
    aptex_trace("memmove %p %p %ld \n", new_memory + lo_size,
          new_memory, (current_mem_size + 1) * sizeof(memory_word));
    /* shift everything upward to make space for new low area */
    memmove (new_memory + lo_size, new_memory, 
      (current_mem_size + 1) * sizeof(memory_word));
    /* could reduce words moved by (mem_max - mem_end) */
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

  if (aptex_env.trace_mem)
    probe_show();

  return mem;
}

memory_word * realloc_font_info (int size)
{
  memory_word * new_font_info = NULL;
  int k, min_size;
  int new_size = 0;
  int n = 0;

  aptex_trace("Old Address %s == %p\n", "font_info", font_info);

  if (current_font_mem_size == font_mem_size)
  {
    /* memory_error("font", (font_mem_size + 1) * sizeof(memory_word)); */
    return font_info;
  }

  min_size = current_font_mem_size / 100 * percent_grow;

  if (size < min_size)
    size = min_size;

  if (size < initial_font_mem_size)
    size = initial_font_mem_size;

  for (k = 0; k < MAXSPLITS; k++)
  {
    new_size = current_font_mem_size + size;

    if (new_size > font_mem_size)
      new_size = font_mem_size; /* bump against limit */

    n = (new_size + 1) * sizeof (memory_word);
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
  aptex_trace("New Address %s == %p\n", "font_info", font_info);
  update_statistics ((long) font_info, n, current_font_mem_size * sizeof(memory_word));
  current_font_mem_size = new_size;

  if (aptex_env.trace_mem)
    probe_show();

  return font_info;
}

packed_ASCII_code * realloc_str_pool (int size)
{
  int k, min_size;
  int new_size = 0;
  int n = 0;
  packed_ASCII_code * new_str_pool = NULL;

  aptex_trace("Old Address %s == %p\n", "string pool", str_pool);

  if (current_pool_size == pool_size)
  {
    /* memory_error ("string pool", (pool_size + 1) * sizeof(packed_ASCII_code)); */
    return str_pool;
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

    n = (new_size + 1) * sizeof (packed_ASCII_code);
    trace_memory("str_pool", n);
    new_str_pool = (packed_ASCII_code *) REALLOC (str_pool, n);

    if (new_str_pool != NULL)
      break;    /* did we get it ? */

    if (current_pool_size == 0)
      break;  /* initial allocation must work */

    size = size / 2; /* else can retry smaller */
  }

  if (new_str_pool == NULL)
  {
    memory_error("string pool", n);
    return str_pool; /* try and continue !!! */
  }

  str_pool = new_str_pool;
  update_statistics ((long) str_pool, n, current_pool_size);
  current_pool_size = new_size;

  if (aptex_env.trace_mem)
    printf("New Address %s == %p\n", "string pool", str_pool);
  
  if (aptex_env.trace_mem)
    probe_show();

  return str_pool;
}

pool_pointer * realloc_str_start (int size)
{
  int k, min_size;
  int n = 0;
  int new_size = 0;
  pool_pointer * new_str_start = NULL;

  if (aptex_env.trace_mem)
    printf("Old Address %s == %p\n", "string start", str_start);

  if (current_max_strings == max_strings)
  {
    /* memory_error ("string pointer", (max_strings + 1) * sizeof(pool_pointer)); */
    return str_start;
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

    n = (new_size + 1) * sizeof (pool_pointer);
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

  if (aptex_env.trace_mem)
    printf("New Address %s == %p\n", "string start", str_start);

  if (aptex_env.trace_mem)
    probe_show();

  return str_start;
}

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
  
  if (aptex_env.trace_mem)
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

  if (aptex_env.trace_mem)
    probe_show();

  return 0; // success
}

memory_word * realloc_save_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  memory_word * new_save_stack = NULL;

  if (aptex_env.trace_mem)
    printf("Old Address %s == %p\n", "save stack", save_stack);

  if (current_save_size == save_size)
  {
    return save_stack;
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

  if (aptex_env.trace_mem)
  {
    printf("Current %s %d\n", "save_size", current_save_size);
    printf("New Address %s == %p\n", "save stack", save_stack);
  }

  if (aptex_env.trace_mem)
    probe_show();

  return save_stack;
}

in_state_record * realloc_input_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  in_state_record * new_input_stack = NULL;

  if (aptex_env.trace_mem)
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

  if (aptex_env.trace_mem)
  {
    printf("Current %s %d\n", "stack_size", current_stack_size);
    printf("New Address %s == %p\n", "input stack", input_stack);
  }

  if (aptex_env.trace_mem)
    probe_show();

  return input_stack;
}

list_state_record * realloc_nest_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  list_state_record * new_nest = NULL;

  if (aptex_env.trace_mem)
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

  if (aptex_env.trace_mem)
  {
    printf("Current %s %d\n", "nest_size", current_nest_size);
    printf("New Address %s == %p\n", "nest stack", nest);
  }

  if (aptex_env.trace_mem)
    probe_show();

  return nest;
}

halfword * realloc_param_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  halfword * new_param = NULL;

  if (aptex_env.trace_mem)
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

  if (aptex_env.trace_mem)
  {
    printf("Current %s %d\n", "param_size", current_param_size);
    printf("New Address %s == %p\n", "param stack", param_stack);
  }

  if (aptex_env.trace_mem)
    probe_show();

  return param_stack;
}

ASCII_code * realloc_buffer (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  ASCII_code * new_buffer = NULL;

  aptex_trace("Old Address %s == %p\n", "buffer", buffer);

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
  update_statistics((long) buffer, n, current_buf_size);
  memset(buffer + current_buf_size, 0, new_size - current_buf_size);
  current_buf_size = new_size;

  aptex_trace("Current buffer %d\n""New Address buffer == %p\n", current_buf_size, buffer);

  if (aptex_env.trace_mem)
    probe_show();

  return buffer;
}
#endif

static int memory_allocate (void)
{
  current_mem_size = 0;

#ifdef APTEX_EXTENSION
  input_stack = NULL;
  current_stack_size = 0;
  input_stack = realloc_input_stack(initial_stack_size);
  nest = NULL;
  current_nest_size = 0;
  nest = realloc_nest_stack(initial_nest_size);
  param_stack = NULL;
  current_param_size = 0;
  param_stack = realloc_param_stack(initial_param_size);
  save_stack = NULL;
  current_save_size = 0;
  save_stack = realloc_save_stack (initial_save_size);
#endif

#ifdef IGNORED
  buffer = NULL;        /* need to do earlier */
  current_buf_size = 0;
  buffer = realloc_buffer (initial_buf_size);
#endif

#ifdef APTEX_EXTENSION
  str_pool = NULL;
  current_pool_size = 0;
  str_start = NULL;
  current_max_strings = 0;

  if (aptex_env.flag_initex)
  {
    aptex_trace("INITEX pool and string allocation");
    str_pool = realloc_str_pool(initial_pool_size);
    str_start = realloc_str_start(initial_max_strings);
  }

  font_info = NULL;
  current_font_mem_size = 0;

  if (aptex_env.flag_initex)
    font_info = realloc_font_info(initial_font_mem_size);

  main_memory = NULL;
  mem = NULL;
  mem_min = mem_bot;
  mem_top = mem_initex;
  mem_max = mem_top;

  if (aptex_env.flag_initex)
  {
    /* avoid this if format specified on command line ??? */
    mem = allocate_main_memory(mem_initex); /* made variable ! */

    if (mem == NULL)
      return -1;
  }

  hyph_word = NULL;
  hyph_list = NULL;
  hyphen_prime = default_hyphen_prime;

  if (aptex_env.flag_initex)
  {
    if (new_hyphen_prime)
      hyphen_prime = new_hyphen_prime;

    if (realloc_hyphen(hyphen_prime)) /* allocate just in case no format */
      return -1;

    if (allocate_tries (trie_size))
      return -1;
  }

  if (aptex_env.flag_initex)
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

#define safe_free(a)  \
do {                  \
  if (a != NULL)      \
    free(a);          \
  a = NULL;           \
} while (0)

static int memory_free (void)
{
  aptex_trace (
    "Statistics of memory_free() \n"
    "  Max allocated %d --- max address %d\n"
    "  Heap total : %u bytes --- max address %u\n"
    "  Main Memory: variable node %d (%d - %d);\n"
    "               one word %d (%d - %d)\n"
    "Freeing memory again.\n",
    total_allocated, max_address,
    0, max_address,
    (int) (lo_mem_max - mem_min), (int) mem_min, (int) lo_mem_max,
    (int) (mem_end - hi_mem_min), (int) hi_mem_min, (int) mem_end
  );

#ifdef APTEX_EXTENSION
  if (aptex_env.flag_initex)
  {
    safe_free(trie_taken);
    safe_free(trie_hash);
    safe_free(trie_l);
    safe_free(trie_r);
    safe_free(trie_c);
    safe_free(trie_o);
  }

  safe_free(trie_trc);
  safe_free(trie_tro);
  safe_free(trie_trl);
  safe_free(hyph_list);
  safe_free(hyph_word);
  safe_free(main_memory);
  safe_free(font_info);
  safe_free(str_start);
  safe_free(str_pool);
  safe_free(param_stack);
  safe_free(nest);
  safe_free(input_stack);
  safe_free(save_stack);
#endif

  safe_free(dvi_file_name);
  safe_free(log_file_name);
  safe_free(pdf_file_name);
  safe_free(fmt_file_name);
  safe_free(TEX_format_default);

  return 0;
}

static void complainarg (int c, char *s)
{
  printf("ERROR: Do not understand `%c' argument value `%s'\n", c, s);
}

static char * lastname  = NULL;
static char * lastvalue = NULL;

/* is it safe to do that now ? 98/Jan/31 */
static char * grabenv (const char * varname)
{
  char * s;

  if (varname == NULL)
    return NULL;

  if (*varname == '\0')
    return NULL;

  if (lastname != NULL && strcasecmp(lastname, varname) == 0)
  {
    aptex_trace("Cache hit: %s=%s\n", lastname, lastvalue);

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

static void knuthify (void)
{
  aptex_env.flag_reset_trie     = false; /* don't allow pattern redefinition */
  aptex_env.flag_caret_hex      = true;  /* show character code in hex */
  aptex_env.flag_caret_dec      = false; /* don't show character code decimal */
  aptex_env.trace_caret         = false; /* don't show missing characters */
  aptex_env.flag_civilize_date  = false; /* don't reorder date fields */
  aptex_env.flag_c_style        = false; /* don't add file name to error msg */
  aptex_env.trace_tfm           = false; /* don't show metric file in log */
  tab_step                      = 0;     /* tab's size of width */
  aptex_env.trace_linebreak     = false; /* do not show line break stats */
  default_rule                  = 26214; /* revert to default rule thickness */
  flag_allow_quoted             = false;
  aptex_env.trace_csname        = false;
  flag_suppress_f_ligs          = false;
  aptex_env.flag_tex82          = true;  /* so other code can know about this */
  aptex_env.flag_compact_fmt    = false;
}

static void read_command_line (int ac, char **av)
{
  int c;
  char * optargnew;
  int option_idx = 0;

#undef name
#define ARGUMENT_IS(a) !strcmp(long_options[option_idx].name, a)

  struct option long_options[] =
  {
    { "main-memory",    required_argument, NULL, 0 },
    { "hyph-size",      required_argument, NULL, 0 },
    { "trie-size",      required_argument, NULL, 0 },
    { "tab-step",       required_argument, NULL, 0 },
    { "percent-grow",   required_argument, NULL, 0 },
    { "default-rule",   required_argument, NULL, 0 },
    { "dvi-dir",        required_argument, NULL, 0 },
    { "log-dir",        required_argument, NULL, 0 },
    { "aux-dir",        required_argument, NULL, 0 },
    { "progname",       required_argument, NULL, 0 },
    { "jobname",        required_argument, NULL, 0 },
    { "synctex",        required_argument, NULL, 0 },
    { "showcsnames",    no_argument, NULL, 0 },
    { "showinhex",      no_argument, NULL, 0 },
    { "verbose",        no_argument, NULL, 0 },
    { "patterns",       no_argument, NULL, 0 },
    { "ini",            no_argument, NULL, 0 },
    { "knuthify",       no_argument, NULL, 0 },
    { "cstyle",         no_argument, NULL, 0 },
    { "showtfm",        no_argument, NULL, 0 },
    { "showlbstats",    no_argument, NULL, 0 },
    { "showmissing",    no_argument, NULL, 0 },
    { "deslash",        no_argument, NULL, 0 },
    { "suppressfligs",  no_argument, NULL, 0 },
    { "trace",          no_argument, NULL, 0 },
    { "help",           no_argument, NULL, 0 },
    { "version",        no_argument, NULL, 0 },
    { "usage",          no_argument, NULL, 0 },
    { NULL, 0, 0, 0 }
  };

  if (ac < 2)
    return;

  while ((c = getopt_long_only(ac, av, "+", long_options, &option_idx)) != EOF)
  {
    if (optarg != 0 && *optarg == '=')
      optargnew = optarg + 1;
    else
      optargnew = optarg;

    if (ARGUMENT_IS("progname"))
      kpse_reset_program_name(optarg);
    else if (ARGUMENT_IS("jobname"))
      aptex_env.aptex_job = xstrdup(optarg);
    else if (ARGUMENT_IS("synctex"))
      synctex_option = (int) strtol(optarg, NULL, 0);
    else if (ARGUMENT_IS("verbose"))
      aptex_env.flag_verbose = true;
    else if (ARGUMENT_IS("ini"))
      aptex_env.flag_initex = true;
    else if (ARGUMENT_IS("knuthify"))
      knuthify();
    else if (ARGUMENT_IS("cstyle"))
      aptex_env.flag_c_style = true;
    else if (ARGUMENT_IS("showtfm"))
      aptex_env.trace_tfm = true;
    else if (ARGUMENT_IS("showcsnames"))
      aptex_env.trace_csname = true;
    else if (ARGUMENT_IS("showinhex"))
      aptex_env.flag_caret_hex = true;
    else if (ARGUMENT_IS("showmissing"))
      aptex_env.trace_caret = false;
    else if (ARGUMENT_IS("aptex_env.flag_deslash"))
      aptex_env.flag_deslash = false;
    else if (ARGUMENT_IS("suppressfligs"))
      flag_suppress_f_ligs = true;
    else if (ARGUMENT_IS("patterns"))
      aptex_env.flag_reset_trie = true;
    else if (ARGUMENT_IS("trace"))
      aptex_env.trace_mem = true;
    else if (ARGUMENT_IS("dvi-dir"))
    {
      if (optarg != NULL)
        dvi_directory = xstrdup(optarg);
    }
    else if (ARGUMENT_IS("log-dir"))
    {
      if (optarg != NULL)
        log_directory = xstrdup(optarg);
    }
    else if (ARGUMENT_IS("aux-dir"))
    {
      if (optarg != NULL)
        aux_directory = xstrdup(optarg);
    }
    else if (ARGUMENT_IS("trie-size"))
    {
#ifdef APTEX_EXTENSION
      if (optarg == 0)
        trie_size = default_trie_size;
      else
        trie_size = atoi(optarg);

      if (trie_size == 0)
        complainarg(c, optarg);
#endif
    }
    else if (ARGUMENT_IS("hyph-size"))
    {
#ifdef APTEX_EXTENSION
      if (optarg == 0)
        new_hyphen_prime = hyphen_prime * 2;
      else
        new_hyphen_prime = atoi(optarg);

      if (new_hyphen_prime == 0)
        complainarg(c, optarg);
#endif
    }
    else if (ARGUMENT_IS("main-memory"))
    {
      if (optarg == 0)
        mem_initex = mem_top;
      else
        mem_initex = atoi(optarg) * 1024;

      if (mem_initex == 0)
        complainarg(c, optarg);

      mem_spec_flag = true;
    }
    else if (ARGUMENT_IS("default-rule"))
    {
      if (optarg == 0)
        default_rule = 26214;
      else
        default_rule = atoi(optarg);

      if (default_rule == 0)
        complainarg(c, optarg);
    }
    else if (ARGUMENT_IS("percent-grow"))
    {
      if (optarg == 0)
        percent_grow = 62;
      else
        percent_grow = atoi(optarg);

      if (percent_grow == 0)
        complainarg(c, optarg);
    }
    else if (ARGUMENT_IS("tab-step"))
    {
      if (optarg == 0)
        tab_step = 8;
      else
        tab_step = atoi(optarg);

      if (tab_step == 0)
        complainarg(c, optarg);
    }
    else if (ARGUMENT_IS("help"))
    {
      print_aptex_info();
      show_usage();
    }
    else if (ARGUMENT_IS("version"))
    {
      print_aptex_info();
      show_version();
    }
  }
}

static void init_commands (int ac, char **av)
{
  aptex_env.flag_initex         = false; 
  aptex_env.flag_reset_trie     = false;
  aptex_env.flag_reset_hyphen   = false;
  aptex_env.trace_mem           = false;
  aptex_env.flag_verbose        = false;
  aptex_env.flag_caret_hex      = false;
  aptex_env.flag_deslash        = true;
  aptex_env.trace_realloc       = true;
  aptex_env.flag_civilize_date  = true;
  aptex_env.flag_caret_dec      = true;
  aptex_env.trace_caret         = true;
  aptex_env.flag_c_style        = false;
  aptex_env.trace_tfm           = false;
  aptex_env.trace_linebreak     = true;
  flag_allow_quoted             = true;
  aptex_env.trace_csname        = false;
  aptex_env.flag_tex82          = false;
  aptex_env.flag_compact_fmt    = true;
  default_rule          = 26214;
  tab_step              = 0;
  new_hyphen_prime      = 0;
  mem_initex            = 0;

#ifdef APTEX_EXTENSION
  trie_size = 0; // default_trie_size
#endif

  read_command_line(ac, av);

  if (optind == 0)
    optind = ac;
}

/* set initial memory allocations */
static void memory_init (void)
{
  if (mem_initex < 0)
    mem_initex = 0;

  if (!aptex_env.flag_initex)
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

  if (mem_initex > 10000L * 1024L)
    mem_initex = mem_initex / 1024;

  /* extend main memory by 16 mega byte! */
  if (mem_initex > 2048L * 1024L)
  {
    puts("WARNING: There may be no benefit to asking for so much memory");
    /* mem_initex = 2048 * 1024; */
  }

  if (new_hyphen_prime < 0)
    new_hyphen_prime = 0;

  if (new_hyphen_prime > 0)
  {
    if (!aptex_env.flag_initex)
      puts("ERROR: Can only set hyphen prime in initex");
    else
    {
      if (new_hyphen_prime % 2 == 0)
        new_hyphen_prime++;

      while (!prime(new_hyphen_prime))
        new_hyphen_prime = new_hyphen_prime + 2;

      aptex_trace("Using %d as hyphen prime\n", new_hyphen_prime);
    }
  }

  if (percent_grow > 100)
    percent_grow = percent_grow - 100;

  if (percent_grow > 100)
    percent_grow = 100;

  if (percent_grow < 10)
    percent_grow = 10;
}

static inline void ng_deslash_path (char * s)
{
  char * r;

  if (strcmp(s, "") != 0)
  {
    r = s + strlen(s) - 1;

    if (*r == '\\' || *r == '/')
      *r = '\0';
  }
}

static inline void unixify_path (char * s)
{
  if (strcmp(s, "") != 0)
    aptex_utils_canonicalize(s);
}

static void aptex_canonicalize_name (int ac, char **av)
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

  ng_deslash_path(dvi_directory);
  ng_deslash_path(log_directory);
  ng_deslash_path(aux_directory);
  ng_deslash_path(fmt_directory);
  ng_deslash_path(pdf_directory);

  if (aptex_env.flag_deslash)
  {
    unixify_path(dvi_directory);
    unixify_path(log_directory);
    unixify_path(aux_directory);
    unixify_path(fmt_directory);
    unixify_path(pdf_directory);
  }

  format_spec = false;

  if (optind < ac && optind > 0)
  {
    if (aptex_env.flag_deslash)
    {
      aptex_trace("aptex: argv[%d] = %s (argc %d)\n", optind, av[optind], ac);
      aptex_utils_canonicalize(av[optind]);
    }

    if (*av[optind] == '&' || *av[optind] == '+')
    {
      format_spec = true;
      format_name = xstrdup(av[optind] + 1);

      if (optind + 1 < ac)
      {
        if (aptex_env.flag_deslash)
        {
          aptex_trace("deslash: argv[%d] %s (argc %d)\n", optind + 1, av[optind + 1], ac);
          aptex_utils_canonicalize(av[optind + 1]);
        }
      }
    }
  }
}

void aptex_init (void)
{
  // check of memory_word's size
  assert(sizeof(memory_word) == sizeof(halfword) * 2);
  assert(sizeof(halfword) == sizeof(quarterword) * 2);
  assert(sizeof(integer) == sizeof(real));

  // for synctex
  synctex_option = INT_MAX;
  // for kpathsea init
  kpse_set_program_name(aptex_env.argv[0], NULL);
  kpse_set_program_enabled(kpse_fontmap_format, true, kpse_src_texmf_cnf);
  // for ptexenc init: encoding
  init_default_kanji("utf8", "uptex");
  // for engine name
  xputenv("engine", "ptex-ng");
  //xputenv("engine", "aptex");

  // try av[0] as preloaded format
  format_name = remove_suffix(xbasename(aptex_env.argv[0]));
  TEX_format_default = (char *) malloc(strlen(format_name) + 6);

  if (TEX_format_default == NULL)
  {
    fprintf(stderr, "%s: something really went bad: cannot allocated mem for default format! Exiting.\n", aptex_env.argv[0]);
    exit(1);
  }

  sprintf(TEX_format_default, " %s.fmt", format_name);
  format_default_length = strlen(TEX_format_default + 1);

  aptex_env.time_start  = clock();
  aptex_env.time_main   = aptex_env.time_start;

#ifdef APTEX_EXTENSION
  main_memory = NULL;
  font_info   = NULL;
  str_pool    = NULL;
  str_start   = NULL;
  save_stack  = NULL;
  hyph_list   = NULL;
  hyph_word   = NULL;
  trie_taken  = NULL;
  trie_hash   = NULL;
  trie_r      = NULL;
  trie_c      = NULL;
  trie_o      = NULL;
  trie_l      = NULL;
  trie_trc    = NULL;
  trie_tro    = NULL;
  trie_trl    = NULL;

  buffer           = NULL;
  current_buf_size = 0;
  buffer           = realloc_buffer(initial_buf_size);
#endif

  log_opened            = false;
  interaction           = -1;
  missing_characters    = 0;
  flag_suppress_f_ligs  = false;

  init_commands(aptex_env.argc, aptex_env.argv);

  switch (aptex_env.flag_compact_fmt)
  {
    case true:
      aptex_env.open_tex_file = &fopen;
      aptex_env.open_tfm_file = &fopen;
      aptex_env.open_fmt_file = &gzopen;
      break;

    case false:
      aptex_env.open_tex_file = &fopen;
      aptex_env.open_tfm_file = &fopen;
      aptex_env.open_fmt_file = &fopen;
      break;
  }

  dvi_file_name = NULL;
  log_file_name = NULL;
  pdf_file_name = NULL;
  fmt_file_name = NULL;

  count_first_pass        = 0;
  count_second_pass       = 0;
  count_final_pass        = 0;
  count_paragraph_failed  = 0;
  count_single_line       = 0;
  count_overfull_hbox     = 0;
  count_underfull_hbox    = 0;
  count_overfull_vbox     = 0;
  count_underfull_vbox    = 0;

  aptex_trace("Entering aptex_init().\n");
  memory_probe();
  ini_max_address = max_address;
  aptex_trace("Max allocated %d --- max address %d\n", total_allocated, max_address);
  memory_init();
  aptex_canonicalize_name(aptex_env.argc, aptex_env.argv);

  if (format_spec && mem_spec_flag)
    puts("WARNING: Cannot change initial main_memory size when format specified");

  if (memory_allocate() != 0)
    exit(1);

  aptex_trace("Leaving aptex_init().\n");
}

static void print_time (clock_t inter_val)
{
  int seconds, tenths, hundredth, thousands;

  if (inter_val >= CLOCKS_PER_SEC * 10)
  {
    tenths = (inter_val * 10 + CLOCKS_PER_SEC / 2) / CLOCKS_PER_SEC; 
    seconds = tenths / 10; 
    tenths = tenths % 10;
    printf("%d.%d", seconds, tenths);
  }
  else if (inter_val >= CLOCKS_PER_SEC)
  {
    hundredth = (inter_val * 100 + CLOCKS_PER_SEC / 2) / CLOCKS_PER_SEC;
    seconds = hundredth / 100;
    hundredth = hundredth % 100;
    printf("%d.%02d", seconds, hundredth);
  }
  else if (inter_val > 0)
  {
    thousands = (inter_val * 1000 + CLOCKS_PER_SEC / 2) / CLOCKS_PER_SEC;
    seconds = thousands / 1000;
    thousands = thousands % 1000;
    printf("%d.%03d", seconds, thousands);
  }
  else
    printf("0");
}

void aptex_fini (void)
{
  aptex_env.time_finish = clock();

  if (!aptex_env.flag_initex)
  {
    printf("Total ");
    print_time(aptex_env.time_finish - aptex_env.time_start);
    printf("s (");
    print_time(aptex_env.time_main - aptex_env.time_start);
    printf(" format load + ");
    print_time(aptex_env.time_finish - aptex_env.time_main);
    printf(" processing)");

    if (total_pages > 0)
    {
      printf(" ");
      print_time((aptex_env.time_finish - aptex_env.time_main) / total_pages);
      printf("s per page");
    }

    printf(".\n");
  }

  if (missing_characters != 0)
  {
    char * be = (missing_characters == 1) ? "was" : "were";
    char * pl = (missing_characters == 1) ? "" : "s";
    printf("! There %s %d missing character%s --- see log file.\n",
      be, missing_characters, pl);
  }

  if (memory_free() != 0)
    exit(1);
}

// print control sequences' name
static void print_cs_name (FILE * output, int h)
{
  if (text(h) == 0)
    return;
  else
  {
    char * cs = get_str_string(text(h));
    fprintf(output, "(%d), \"%s\"\n", h, cs);
    free(cs);
  }
}

/* k1 and k2 are positions in string pool */
/* l1 and l2 are lengths of strings */
static int compare_strn (int k1, int l1, int k2, int l2)
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
    return 1;  /* first string longer */
  else if (l2 > 0)
    return -1; /* second string longer */

  return 0;    /* strings match */
}

/* compare two csnames in qsort */
static int compare_cs (const void *cp1, const void *cp2)
{
  int c1, c2, l1, l2, k1, k2;
  pointer textof1, textof2;

  c1 = *(int *)cp1;
  c2 = *(int *)cp2;
  textof1 = text(c1);
  textof2 = text(c2);
  l1 = length(textof1);
  l2 = length(textof2);
  k1 = str_start[textof1];
  k2 = str_start[textof2];

  return compare_strn(k1, l1, k2, l2);
}

static char * cs_used = NULL;

void print_cs_names (FILE * output, boolean pass)
{
  int h, k, ccount;
  int * cnumtable;
  int nfcs = frozen_control_sequence;

  if ((pass == false) && (cs_used == NULL))
  {
    cs_used = (char *) malloc(nfcs);

    if (cs_used == NULL)
      return;

    memset(cs_used, 0, nfcs);
  }

  ccount = 0;

  for (h = hash_base + 1; h < nfcs; h++)
  {
    if ((pass == true) && cs_used[h])
      continue;

    if (text(h) != 0)
    {
      if (pass == 0)
        cs_used[h] = 1;

      ccount++;
    }
  }

  sprintf(aptex_log_line, "\n%d %s multiletter control sequences:\n",
      ccount, (pass == true) ? "new" : "");
  fprintf(output, "%s", aptex_log_line);

  if (ccount > 0)
  {
    cnumtable = (int *) malloc (ccount * sizeof(int));

    if (cnumtable == NULL)
      return;

    ccount = 0;

    for (h = hash_base + 1; h < nfcs; h++)
    {
      if (pass == true && cs_used[h])
        continue;

      if (text(h) != 0)
        cnumtable[ccount++] = h;
    }

    //qsort ((void *)cnumtable, ccount, sizeof (int), &compare_cs);

    for (k = 0; k < ccount; k++)
    {
      h = cnumtable[k];

      if (pass == true && cs_used[h])
        continue;

      print_cs_name(output, h);
    }

    sprintf(aptex_log_line, "\n");
    fprintf(output, "%s", aptex_log_line);

    free((void *) cnumtable);
  }

  if ((pass == true) && (cs_used != NULL))
  {
    free(cs_used);
    cs_used = NULL;
  }
}
