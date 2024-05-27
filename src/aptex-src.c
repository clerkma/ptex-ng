/*
   Copyright 2007 TeX Users Group
   Copyright 2014-2024 Clerk Ma

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

#include "aptex.h"

#ifndef USE_KPATHSEA
  #define xmalloc malloc
  #define xrealloc realloc
  #define xfopen fopen
  #define xfclose(a, b) fclose(a)
#endif

static int mem_initex;
static int new_hyphen_prime;
static char * format_name;
// line break stats
static int lbs_pass_fst;
static int lbs_pass_snd;
static int lbs_pass_fin;
static int lbs_sing_line;
// hpack stats
static int hps_underfull;
static int hps_overfull;
// vpack stats
static int vps_underfull;
static int vps_overfull;

#if   defined (__clang__)
static const char * compiler = "Clang";
#elif defined (__GNUC__) || defined(__GNUG__)
static const char * compiler = "GCC";
#elif defined (_MSC_VER)
static const char * compiler = "MSVC";
#else
static const char * compiler = "Unknown";
#endif

#if   defined (_WIN64)
static const char * dist = "Win64";
#elif defined (_WIN32)
static const char * dist = "Win32";
#elif defined (__ANDROID__)
static const char * dist = "Android";
#elif defined (__APPLE__)
static const char * dist = "Darwin";
#elif defined (__gnu_linux__)
static const char * dist = "Linux";
#else
static const char * dist = "Unknown";
#endif

static const char * banner = "This is Asiatic pTeX, Version 3.141592653";

static void aptex_utils_exit (int unix_code)
{
  update_terminal();
  exit(unix_code);
}

static void print_aptex_usage (void)
{
  printf("\n"
      "Useage: aptex [OPTION]... [+fmt_file_name] [file]\n\n"
      "  --help\n"
      "    show this usage summary\n"
      "  --version\n"
      "    output version information and exit\n"
      "  --ini\n"
      "    start up as INITEX (create format file)\n"
      "  --shell-escape\n"
      "    enable \\write18\n"
      "  --merge-kanji-baseline\n"
      "    shift baseline of OpenType's ascender/descender to JFM's height/depth\n"
#ifdef USE_MRUBY
      "\n"
      "  --mrb-load-file\n"
      "    execute file of mruby\n"
      "  --mrb-load-string\n"
      "    execute string of mruby\n"
#endif /* USE_MRUBY */
      "\n"
      "  --jobname=str\n"
      "    set the job name to str, e.g.: '--jobname=book2016'\n"
      "  --progname=str\n"
      "    set program (and fmt) name to str\n"
      "  --synctex=num\n"
      "    generate SyncTeX data for previewers if nonzero\n"
      "  --fontmap=map\n"
      "    +mapfile (append mode), !mapfile (replace mode), e.g.: '--fontmap=!replace.map'\n"
      "  --format=fmt\n"
      "    set preloaded format, e.g.: 'aptex --format=plain name.tex'\n"
      "\n"
      "  --patterns\n"
      "    (INITEX only) allow use of \\patterns after loading format\n"
      "  --main-mem\n"
      "    (INITEX only) initial main memory size in kilo words\n"
      "  --hyph-size\n"
      "    (INITEX only) hyphenation exception dictionary size\n"
      "  --trie-size\n"
      "    (INITEX only) hyphenation pattern trie size\n\n"
      "Email bug reports to clerkma@gmail.com.\n"
  );
  aptex_utils_exit(EXIT_SUCCESS);
}

static void print_aptex_version (void)
{
  printf("Copyright 2014-2024 Clerk Ma.\n"
    "banner: \"%s\"\n"
    "base: Y&Y TeX 2.3.0, pTeX%s, upTeX%s\n",
    banner, pTeX_version_string, upTeX_version_string);
#ifdef USE_KPATHSEA
  printf("Compiled with %s\n", kpathsea_version_string);
#endif
  printf("Compiled with %s\n"
    "Compiled with zlib version %s\n",
    ptexenc_version_string, zlib_version);
#ifdef USE_MRUBY
  printf("Compiled with mruby version %s\n", MRUBY_VERSION);
#endif
  printf("Compiled with synctex (build-in edition)\n"
    "Compiled with libdpx (build-in dvipdfmx)\n");
  aptex_utils_exit(EXIT_SUCCESS);
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
  char * executable_path;

  strcpy(date, __DATE__);
  scivilize(date);
  printf("Asiatic pTeX (compiled time: %s %s with %s/%s)\n",
    date, __TIME__, dist, compiler);

  executable_path = (char *) calloc(65536, 1);

#if   defined (_WIN32) || defined (_WIN64)
  GetModuleFileNameA(NULL, executable_path, 65536);
#elif defined (__gnu_linux__) || defined (__ANDROID__)
  ssize_t executable_size;
  executable_size = readlink("/proc/self/exe", executable_path, 65536);
#elif defined (__NetBSD__)
  ssize_t executable_size;
  executable_size = readlink("/proc/curproc/exe", executable_path, 65536);
#elif defined (__DragonFly__)
  ssize_t executable_size;
  executable_size = readlink("/proc/curproc/file", executable_path, 65536);
#elif defined (__APPLE__)
  extern int _NSGetExecutablePath(char* buf, uint32_t* bufsize);
  uint32_t executable_path_length;
  _NSGetExecutablePath(executable_path, &executable_path_length);
#endif

#if defined (__sun)
  printf("Executable PATH: '%s'.\n", getexecname());
#else
  printf("Executable PATH: '%s'.\n", executable_path);
#endif

  free(executable_path);
}

static void print_aptex_time (clock_t inter_val)
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

#define MAXSPLITS 3

/* ad hoc default minimum growth in memory realloc is 62% */
/* golden ratio (1 + \sqrt{5}) / 2 = 1.618033989... */
static int percent_grow    = 62; /* default minimum growth in memory realloc is 62% */
static intptr_t total_allocated = 0;  /* total memory allocated so far */
static intptr_t max_address     = 0;  /* maximum address seen in allocated memory */

static void aptex_trace (const char * fmt_str, ...)
{
  va_list m_ptr;
  va_start(m_ptr, fmt_str);

  if (aptex_env.trace_mem)
  {
    // format => "[Sun Aug 28 14:22:05 2015] (I/V): blabla"
    time_t time_present;
    time(&time_present);
    fprintf(stderr, "[%.24s] (%s): ", asctime(localtime(&time_present)),
      aptex_env.flag_initex == true ? "I" : "V");
    vfprintf(stderr, fmt_str, m_ptr);
  }
  va_end(m_ptr);
}

static void aptex_memory_error (const char * s, int n)
{
  aptex_trace("Unable to allocate %d bytes for '%s'\n", n, s);
  aptex_trace("Max allocated %d, max address %p\n", total_allocated, max_address);
}

static void aptex_memory_trace (const char * s, int n)
{
  aptex_trace("Allocating %d bytes for '%s'\n", n, s);
}

static void aptex_memory_update_statistics (intptr_t address, int size, int old_size)
{
  if (address + size > max_address)
    max_address = address + size;

  total_allocated = total_allocated + size - old_size;
}

static void aptex_memory_probe (void)
{
  if (aptex_env.trace_mem)
  {
    aptex_trace("Max allocated %d, max address %p\n", total_allocated, max_address);
  }
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

static int allocate_tries (int trie_max)
{
  int n, nl, no, nc;

  if (trie_max > 1000000)
    trie_max = 1000000;

  nl = (trie_max + 1) * sizeof(halfword);
  no = (trie_max + 1) * sizeof(halfword);
  nc = (trie_max + 1) * sizeof(quarterword);
  n = nl + no + nc;

  aptex_memory_trace("hyphen trie", n);

  trie_trl = (halfword *) malloc(roundup(nl));
  trie_tro = (halfword *) malloc(roundup(no));
  trie_trc = (quarterword *) malloc(roundup(nc));

  if (trie_trl == NULL || trie_tro == NULL || trie_trc == NULL)
  {
    aptex_memory_error("hyphen trie", n);

    return -1;
  }

  trie_size = trie_max;

  aptex_trace("Addresses trie_trl %p \n", trie_trl);
  aptex_trace("Addresses trie_tro %p \n", trie_tro);
  aptex_trace("Addresses trie_trc %p \n", trie_trc);
  aptex_memory_update_statistics((intptr_t) trie_trl, nl, 0);
  aptex_memory_update_statistics((intptr_t) trie_tro, no, 0);
  aptex_memory_update_statistics((intptr_t) trie_trc, nc, 0);
  aptex_memory_probe();

  return 0;
}

/* remember in case reallocated later */
static int current_prime = 0;

/* we don't return an address here, since TWO memory regions allocated */
/* plus, we don't really reallocate, we FLUSH the old information totally */
/* returns -1 if it fails */

static int realloc_hyphen (int hyphen_prime)
{
  int n, nw, nl;

  if (!prime(hyphen_prime))
  {
    aptex_trace("ERROR: non-prime hyphen exception number (%d)\n", hyphen_prime);
    return -1;
  }

/*  need not/cannot preserve old contents when hyphen prime is changed */
/*  if (hyph_list != NULL) free(hyph_list); */
/*  if (hyph_word != NULL) free(hyph_word); */
  nw = (hyphen_prime + 1) * sizeof(str_number);
  nl = (hyphen_prime + 1) * sizeof(halfword);
  n = nw + nl;

  aptex_memory_trace("hyphen exception", n);

  hyph_word = (str_number *) realloc(hyph_word, nw);
  hyph_list = (halfword *) realloc(hyph_list, nl);

  if (hyph_word == NULL || hyph_list == NULL)
  {
    aptex_memory_error("hyphen exception", n);
    return -1;
  }

  memset(hyph_word, 0, (hyphen_prime + 1) * sizeof (hyph_word[0]));
  memset(hyph_list, 0, (hyphen_prime + 1) * sizeof (hyph_list[0]));

  hyph_count = 0;

  aptex_trace("Addresses hyph_word @ %p hyph_list @ %p\n", hyph_word, hyph_list);

  if (current_prime != 0)
  {
    aptex_memory_update_statistics((intptr_t) hyph_word, nw, (current_prime + 1) * sizeof(str_number));
    aptex_memory_update_statistics((intptr_t) hyph_list, nl, (current_prime + 1) * sizeof(halfword));
  }
  else
  {
    aptex_memory_update_statistics((intptr_t) hyph_word, nw, 0);
    aptex_memory_update_statistics((intptr_t) hyph_list, nl, 0);
  }

  aptex_memory_probe();

  current_prime = hyphen_prime;

  return 0; // success
}

static memory_word * allocate_mem (int size)
{
  int n;

  if (main_memory != NULL)
    aptex_trace("Reallocating initial memory allocation");

  mem_top = mem_bot + size;
  mem_max = mem_top;
  mem_start = 0;     /* bottom of memory allocated by system */
  mem_min = 0;       /* bottom of area made available to TeX */
  n = (mem_max - mem_start + 1) * ((int) sizeof(memory_word));

  aptex_memory_trace("main memory", n);

  main_memory = (memory_word *) realloc(main_memory, n);

  if (main_memory == NULL)
  {
    aptex_memory_error("initial main memory", n);
    return NULL;
  }

  aptex_trace("Address main memory == %p\n", main_memory);

  mem = main_memory;

  if (mem_start != 0 && !aptex_env.flag_initex)
    mem = main_memory - mem_start;

  aptex_trace("Offset address main memory == %p\n", mem);
  aptex_memory_update_statistics((intptr_t) main_memory, n, (current_mem_size + 1) * sizeof (memory_word));
  current_mem_size = mem_max - mem_start;   /* total number of words - 1 *//*  current_mem_size = (mem_max - mem_start + 1); */
  aptex_memory_probe();

  return mem;
}

/* increase main memory allocation at low end and high end */
/* called with one of lo_size or hi_size == 0 */
/* returns NULL if it fails */

static memory_word * realloc_mem (int lo_size, int hi_size)
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

  aptex_trace("Old Address '%s' @ %p\n", "main_memory", main_memory);

  /* if we REALLY run up to limit ! */
  if (current_mem_size + 1 == max_mem_size)
  {
    aptex_memory_error("main memory", (max_mem_size + 1) * sizeof(memory_word));
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

    aptex_memory_trace("main memory", n);

    new_memory = (memory_word *) realloc(main_memory, n);

    if (new_memory != NULL)
      break; /* did we get it ? */

    if (current_mem_size == 0)
      break; /* in case we ever use for initial */

    lo_size = lo_size / 2; hi_size = hi_size / 2;
  }

  if (new_memory == NULL)
  {
    aptex_memory_error("main memory", n);
    return mem;
  }

  aptex_trace("New Address '%s' @ %p\n", "main_memory", new_memory);

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

  aptex_memory_update_statistics ((intptr_t) main_memory, n,
    (current_mem_size + 1) * sizeof (memory_word));
  current_mem_size = new_size;

  if (current_mem_size != mem_max - mem_start)
    puts("ERROR: Impossible Memory Error");

  if (mem_start != 0)
    mem = main_memory - mem_start;
  else
    mem = main_memory;

  aptex_memory_probe();

  return mem;
}

static memory_word * realloc_font_info (int size)
{
  memory_word * new_font_info = NULL;
  int k, min_size;
  int new_size = 0;
  int n = 0;

  aptex_trace("Old Address '%s' @ %p\n", "font_info", font_info);

  if (current_font_mem_size == font_mem_size)
  {
    /* aptex_memory_error("font", (font_mem_size + 1) * sizeof(memory_word)); */
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

    aptex_memory_trace("font_info", n);

    new_font_info = (memory_word *) realloc(font_info, n);

    if (new_font_info != NULL)
      break;   /* did we get it ? */

    if (current_font_mem_size == 0)
      break; /* initial allocation must work */

    size = size / 2;
  }

  if (new_font_info == NULL)
  {
    aptex_memory_error("font", n);
    return font_info;        /* try and continue !!! */
  }

  font_info = new_font_info;

  aptex_trace("New Address '%s' @ %p\n", "font_info", font_info);
  aptex_memory_update_statistics ((intptr_t) font_info, n, current_font_mem_size * sizeof(memory_word));
  aptex_memory_probe();

  current_font_mem_size = new_size;

  return font_info;
}

static packed_ASCII_code * realloc_str_pool (int size)
{
  int k, min_size;
  int new_size = 0;
  int n = 0;
  packed_ASCII_code * new_str_pool = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "str_pool", str_pool);

  if (current_pool_size == pool_size)
  {
    /* aptex_memory_error ("string pool", (pool_size + 1) * sizeof(packed_ASCII_code)); */
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

    aptex_memory_trace("str_pool", n);

    new_str_pool = (packed_ASCII_code *) realloc(str_pool, n);

    if (new_str_pool != NULL)
      break;    /* did we get it ? */

    if (current_pool_size == 0)
      break;  /* initial allocation must work */

    size = size / 2; /* else can retry smaller */
  }

  if (new_str_pool == NULL)
  {
    aptex_memory_error("string pool", n);
    return str_pool; /* try and continue !!! */
  }

  str_pool = new_str_pool;
  current_pool_size = new_size;

  aptex_trace("New Address '%s' @ %p\n", "str_pool", str_pool);
  aptex_memory_update_statistics((intptr_t) str_pool, n, current_pool_size);
  aptex_memory_probe();

  return str_pool;
}

static pool_pointer * realloc_str_start (int size)
{
  int k, min_size;
  int n = 0;
  int new_size = 0;
  pool_pointer * new_str_start = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "str_start", str_start);

  if (current_max_strings == max_strings)
  {
    /* aptex_memory_error ("string pointer", (max_strings + 1) * sizeof(pool_pointer)); */
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

    aptex_memory_trace("str_start", n);

    new_str_start = (pool_pointer *) realloc(str_start, n);

    if (new_str_start != NULL)
      break;   /* did we get it ? */

    if (current_max_strings == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* otherwise can try smaller */
  }

  if (new_str_start == NULL)
  {
    aptex_memory_error("string pointer", n);
    return str_start;          /* try and continue */
  }

  str_start = new_str_start;
  current_max_strings = new_size;

  aptex_trace("New Address '%s' @ %p\n", "str_start", str_start);
  aptex_memory_update_statistics((intptr_t)str_start, n, current_max_strings * sizeof(pool_pointer));
  aptex_memory_probe();

  return str_start;
}

/* returns -1 if it fails */
/* size == trie_size */
static int allocate_ini (int size)
{
  int n, nl, no, nc, nr, nh, nt;

  nh = (size + 1) * sizeof(trie_pointer);
  nr = (size + 1) * sizeof(trie_pointer);
  nl = (size + 1) * sizeof(trie_pointer);
  no = (size + 1) * sizeof(trie_op_code);
  nc = (size + 1) * sizeof(packed_ASCII_code);
  nt = (size + 1) * sizeof(char);
  n = nl + no + nc + nr + nh + nt;

  aptex_memory_trace("initex hyphen trie", n);

  trie_l = (trie_pointer *) malloc (roundup(nl));
  trie_o = (trie_op_code *) malloc (roundup(no));
  trie_c = (packed_ASCII_code *) malloc (roundup(nc));
  trie_r = (trie_pointer *) malloc (roundup(nr));
  trie_hash = (trie_pointer *) malloc (roundup(nh));
  trie_taken = (char *) malloc (roundup(nt));
  
  if (trie_c == NULL || trie_o == NULL || trie_l == NULL || trie_r == NULL ||
      trie_hash == NULL || trie_taken == NULL)
  {
    aptex_memory_error("initex hyphen trie", n);
    return -1;
  }
  
  aptex_trace("Addresses: trie_l %p trie_o %p trie_c %p\n", trie_l, trie_o, trie_c);
  aptex_trace("Addresses: trie_r %p trie_hash %p trie_taken %p\n", trie_r, trie_hash, trie_taken);
  aptex_memory_update_statistics ((intptr_t) trie_l, nl, 0);
  aptex_memory_update_statistics ((intptr_t) trie_o, no, 0);
  aptex_memory_update_statistics ((intptr_t) trie_c, nc, 0);
  aptex_memory_update_statistics ((intptr_t) trie_r, nr, 0);
  aptex_memory_update_statistics ((intptr_t) trie_hash, nh, 0);
  aptex_memory_update_statistics ((intptr_t) trie_taken, nt, 0);
  aptex_memory_probe();

  return 0; // success
}

static memory_word * realloc_save_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  memory_word * new_save_stack = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "save_stack", save_stack);

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

    aptex_memory_trace("save_stack", n);

    new_save_stack = (memory_word *) realloc(save_stack, n);

    if (new_save_stack != NULL)
      break;    /* did we get it ? */

    if (current_save_size == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_save_stack == NULL)
  {
    aptex_memory_error("save_stack", n);
    return save_stack;           /* try and continue !!! */
  }

  save_stack = new_save_stack;
  current_save_size = new_size;

  aptex_trace("Current %s %d\n", "save_size", current_save_size);
  aptex_trace("New Address '%s' @ %p\n", "save_stack", save_stack);
  aptex_memory_update_statistics((intptr_t) save_stack, n, current_save_size);
  aptex_memory_probe();

  return save_stack;
}

static in_state_record * realloc_input_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  in_state_record * new_input_stack = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "input_stack", input_stack);

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

    aptex_memory_trace("input_stack", n);

    new_input_stack = (in_state_record *) realloc(input_stack, n);

    if (new_input_stack != NULL)
      break;   /* did we get it ? */

    if (current_stack_size == 0)
      break; /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_input_stack == NULL)
  {
    aptex_memory_error("input stack", n);
    return input_stack;            /* try and continue !!! */
  }

  input_stack = new_input_stack;
  current_stack_size = new_size;

  aptex_trace("Current %s %d\n", "stack_size", current_stack_size);
  aptex_trace("New Address '%s' @ %p\n", "input_stack", input_stack);
  aptex_memory_update_statistics((intptr_t) input_stack, n, current_stack_size);
  aptex_memory_probe();

  return input_stack;
}

static list_state_record * realloc_nest_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  list_state_record * new_nest = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "nest", nest);

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
    aptex_memory_trace("nest stack", n);
    new_nest = (list_state_record *) realloc(nest, n);

    if (new_nest != NULL)
      break;   /* did we get it ? */

    if (current_nest_size == 0)
      break;  /* initial allocation must work */

    size = size / 2;          /* else can retry smaller */
  }

  if (new_nest == NULL)
  {
    aptex_memory_error("nest stack", n);
    return nest;            /* try and continue !!! */
  }

  nest = new_nest;
  current_nest_size = new_size;

  aptex_trace("Current %s %d\n", "nest_size", current_nest_size);
  aptex_trace("New Address '%s' @ %p\n", "nest", nest);
  aptex_memory_update_statistics((intptr_t) nest, n, current_nest_size);
  aptex_memory_probe();

  return nest;
}

static halfword * realloc_param_stack (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  halfword * new_param = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "param_stack", param_stack);

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
    aptex_memory_trace("param stack", n);
    new_param = (pointer *) realloc(param_stack, n);

    if (new_param != NULL)
      break;    /* did we get it ? */

    if (current_param_size == 0)
      break; /* initial allocation must work */

    size = size / 2; /* else can retry smaller */
  }

  if (new_param == NULL)
  {
    aptex_memory_error("param stack", n);
    return param_stack;            /* try and continue !!! */
  }

  param_stack = new_param;
  current_param_size = new_size;

  aptex_trace("Current %s %d\n", "param_size", current_param_size);
  aptex_trace("New Address '%s' @ %p\n", "param_stack", param_stack);
  aptex_memory_update_statistics((intptr_t) param_stack, n, current_param_size);
  aptex_memory_probe();

  return param_stack;
}

static ASCII_code * realloc_buffer (int size)
{
  int k, min_size;
  int n = 0, new_size = 0;
  ASCII_code * new_buffer = NULL;

  aptex_trace("Old Address '%s' @ %p\n", "buffer", buffer);

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

    aptex_memory_trace("buffer", n);

    new_buffer = (ASCII_code *) realloc(buffer, n);

    if (new_buffer != NULL)
      break;   /* did we get it ? */

    if (current_buf_size == 0)
      break;   /* initial allocation must work */

    size = size / 2;
  }

  if (new_buffer == NULL)
  {
    aptex_memory_error("buffer", n);
    return buffer;  /* try and continue !!! */
  }

  buffer = new_buffer;
  memset(buffer + current_buf_size, 0, new_size - current_buf_size);
  current_buf_size = new_size;

  aptex_trace("Current buffer %d\n", current_buf_size);
  aptex_trace("New Address 'buffer' @ %p\n", buffer);
  aptex_memory_update_statistics((intptr_t) buffer, n, current_buf_size);
  aptex_memory_probe();

  return buffer;
}
#endif

/* set initial memory allocations */
static void aptex_memory_init (void)
{
  if (!aptex_env.flag_initex)
  {
    if (mem_initex != 0)
    {
      fprintf(stderr, "ERROR: Can only set initial main memory size in INITEX");
      mem_initex = 0;
    }

    if (trie_size != 0)
      fprintf(stderr, "ERROR: Need only set hyphenation trie size in INITEX");
  }

  if (mem_initex <= 0)
    mem_initex = default_mem_top;
  else if (mem_initex > 10000L * 1024L)
    mem_initex = mem_initex / 1024;

  if (trie_size <= 0)
    trie_size = default_trie_size;

  if (new_hyphen_prime < 0)
    new_hyphen_prime = 0;

  if (new_hyphen_prime > 0)
  {
    if (!aptex_env.flag_initex)
      puts("ERROR: Can only set hyphen prime in INITEX");
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

  buffer = NULL;
  current_buf_size = 0;
  buffer = realloc_buffer(initial_buf_size);
#endif

  interaction = -1;
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
  hyph_word = NULL;
  hyph_list = NULL;
  hyphen_prime = default_hyphen_prime;

  if (aptex_env.flag_initex)
  {
    /* avoid this if format specified on command line ??? */
    mem = allocate_mem(mem_initex); /* made variable ! */

    if (mem == NULL)
      aptex_utils_exit(EXIT_FAILURE);
  }

  if (aptex_env.flag_initex)
  {
    if (new_hyphen_prime)
      hyphen_prime = new_hyphen_prime;

    if (realloc_hyphen(hyphen_prime)) /* allocate just in case no format */
      aptex_utils_exit(EXIT_FAILURE);

    if (allocate_tries(trie_size))
      aptex_utils_exit(EXIT_FAILURE);

    if (allocate_ini(trie_size))
      aptex_utils_exit(EXIT_FAILURE);
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
}

static void aptex_memory_free (void)
{
#define safe_free(a)  \
do {                  \
  if (a != NULL)      \
    free(a);          \
  a = NULL;           \
} while (0)

  aptex_trace("Max allocated %d, max address %p\n", total_allocated, max_address);
  aptex_trace("Heap total : %u bytes, max address %p\n", 0, max_address);
  aptex_trace("Main Memory: variable node %d (%d - %d)\n", (int)(lo_mem_max - mem_min), (int)mem_min, (int)lo_mem_max);
  aptex_trace("Main Memory: one word node %d (%d - %d)\n", (int)(mem_end - hi_mem_min), (int)hi_mem_min, (int)mem_end);

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

  safe_free(TEX_format_default);
}

static char * make_format_name(char * format_name)
{
#ifdef USE_KPATHSEA
  return remove_suffix(xbasename(format_name));
#else
  return format_name;
#endif
}

static void aptex_commands_init (int ac, char **av)
{
  aptex_env.aptex_fmt             = NULL;
  aptex_env.aptex_job             = NULL;
  aptex_env.aptex_src             = NULL;
  aptex_env.aptex_map             = NULL;

  aptex_env.flag_initex               = false;
  aptex_env.flag_suppress_f_ligs      = false;
  aptex_env.flag_reset_trie           = false;
  aptex_env.flag_reset_hyphen         = false;
  aptex_env.flag_shell_escape         = false;
  aptex_env.flag_tex82                = false;
  aptex_env.flag_compact_fmt          = true;
  aptex_env.flag_merge_kanji_baseline = false;

  aptex_env.trace_realloc         = true;
  aptex_env.trace_mem             = false;
  aptex_env.trace_lbrk            = true;

  new_hyphen_prime                = 0;
#ifdef APTEX_EXTENSION
  trie_size                       = 0;
#endif
  mem_initex                      = 0;

  term_in.file_data = stdin;
  term_in.file_type = 0;

  term_out.file_data = stdout;
  term_out.file_type = 0;

  lbs_pass_fst  = 0;
  lbs_pass_snd  = 0;
  lbs_pass_fin  = 0;
  lbs_sing_line = 0;

  hps_overfull  = 0;
  hps_underfull = 0;
  vps_overfull  = 0;
  vps_underfull = 0;

  if (ac >= 2)
  {
    int c;
    int option_idx = 0;

#pragma push_macro("name")
#undef name
#define ARGUMENT_IS(a) !strcmp(long_options[option_idx].name, a)

    struct option long_options[] =
    {
      { "main-memory",    required_argument, NULL, 0 },
      { "hyph-size",      required_argument, NULL, 0 },
      { "trie-size",      required_argument, NULL, 0 },
      { "percent-grow",   required_argument, NULL, 0 },
      { "progname",       required_argument, NULL, 0 },
      { "jobname",        required_argument, NULL, 0 },
      { "synctex",        required_argument, NULL, 0 },
      { "fontmap",        required_argument, NULL, 0 },
      { "format",         required_argument, NULL, 0 },
      { "mrb-load-file",  required_argument, NULL, 0 },
      { "mrb-load-string",required_argument, NULL, 0 },
      { "shell-escape",   no_argument, NULL, 0 },
      { "merge-kanji-baseline", no_argument, NULL, 0 },
      { "patterns",       no_argument, NULL, 0 },
      { "ini",            no_argument, NULL, 0 },
      { "showlbstats",    no_argument, NULL, 0 },
      { "suppressfligs",  no_argument, NULL, 0 },
      { "trace",          no_argument, NULL, 0 },
      { "help",           no_argument, NULL, 0 },
      { "version",        no_argument, NULL, 0 },
      { "usage",          no_argument, NULL, 0 },
      { NULL, 0, 0, 0 }
    };

    while ((c = getopt_long_only(ac, av, "", long_options, &option_idx)) != EOF)
    {
      if (ARGUMENT_IS("progname"))
#ifdef USE_KPATHSEA
        kpse_reset_program_name(strdup(optarg));
#else
        ;
#endif
      else if (ARGUMENT_IS("jobname"))
        aptex_env.aptex_job = strdup(optarg);
      else if (ARGUMENT_IS("fontmap"))
        aptex_env.aptex_map = strdup(optarg);
      else if (ARGUMENT_IS("synctex"))
        synctex_option = strtoll(optarg, NULL, 0);
      else if (ARGUMENT_IS("format"))
        aptex_env.aptex_fmt = strdup(optarg);
      else if (ARGUMENT_IS("ini"))
        aptex_env.flag_initex = true;
      else if (ARGUMENT_IS("suppressfligs"))
        aptex_env.flag_suppress_f_ligs = true;
      else if (ARGUMENT_IS("shell-escape"))
        aptex_env.flag_shell_escape = true;
      else if (ARGUMENT_IS("merge-kanji-baseline"))
        aptex_env.flag_merge_kanji_baseline = true;
      else if (ARGUMENT_IS("patterns"))
        aptex_env.flag_reset_trie = true;
      else if (ARGUMENT_IS("trace"))
        aptex_env.trace_mem = true;
      else if (ARGUMENT_IS("trie-size"))
        trie_size = (optarg == NULL) ? default_trie_size : atoi(optarg);
      else if (ARGUMENT_IS("hyph-size"))
        new_hyphen_prime = (optarg == NULL) ? hyphen_prime * 2 : atoi(optarg);
      else if (ARGUMENT_IS("main-memory"))
        mem_initex = (optarg == NULL) ? mem_top : atoi(optarg) * 1024;
      else if (ARGUMENT_IS("percent-grow"))
        percent_grow = (optarg == NULL) ? 62 : atoi(optarg);
      else if (ARGUMENT_IS("help"))
        print_aptex_info(), print_aptex_usage();
      else if (ARGUMENT_IS("version"))
        print_aptex_info(), print_aptex_version();
#ifdef USE_MRUBY
      else if (ARGUMENT_IS("mrb-load-file"))
      {
        mrb_state * mrb_cmd = mrb_open();
        print_aptex_info();
        mrb_mruby_aptex_gem_init(mrb_cmd);

        if (mrb_cmd != NULL)
        {
          FILE * mrb_cmd_file = fopen(optarg, "rb");

          if (mrb_cmd_file != NULL)
          {
            mrb_load_file(mrb_cmd, mrb_cmd_file);
            mrb_mruby_aptex_gem_final(mrb_cmd);
            mrb_close(mrb_cmd);
            fclose(mrb_cmd_file);
          }
        }

        aptex_utils_exit(EXIT_SUCCESS);
      }
      else if (ARGUMENT_IS("mrb-load-string"))
      {
        mrb_state * mrb_cmd = mrb_open();
        print_aptex_info();
        mrb_mruby_aptex_gem_init(mrb_cmd);

        if (mrb_cmd != NULL)
        {
          mrb_load_string(mrb_cmd, optarg);
          mrb_mruby_aptex_gem_final(mrb_cmd);
          mrb_close(mrb_cmd);
        }

        aptex_utils_exit(EXIT_SUCCESS);
      }
#endif /* USE_MRUBY */
    }
#pragma pop_macro("name")

    while (optind < ac)
    {
      if (*av[optind] == '&' || *av[optind] == '+')
      {
        if (aptex_env.aptex_fmt != NULL)
          free(aptex_env.aptex_fmt);

        aptex_env.aptex_fmt = strdup(av[optind] + 1);
      }
      else
      {
        if (aptex_env.aptex_src != NULL)
          free(aptex_env.aptex_src);

        aptex_env.aptex_src = (char *) calloc(strlen(av[optind]) + 2, sizeof(char));
        sprintf(aptex_env.aptex_src, "%s ", av[optind]);
      }

      optind++;
    }
  }

  if (aptex_env.aptex_src == NULL)
    aptex_env.aptex_src = strdup(" ");

  if (aptex_env.aptex_fmt == NULL)
    format_name = make_format_name(av[0]);
  else
    format_name = strdup(aptex_env.aptex_fmt);

  TEX_format_default = (char *) malloc(strlen(format_name) + 6);

  if (TEX_format_default == NULL)
  {
    fprintf(stderr, "%s: something really went bad: cannot allocated mem for default format! Exiting.\n", av[0]);
    aptex_utils_exit(EXIT_FAILURE);
  }

  sprintf(TEX_format_default, " %s.fmt", format_name);
  format_default_length = strlen(TEX_format_default + 1);
}

static void catch_interrupt (int err)
{
  (void) err;
  (void) signal(SIGINT, SIG_IGN);

  if (interrupt++ >= 3)
    aptex_utils_exit(EXIT_FAILURE);

  (void) signal(SIGINT, catch_interrupt);
}

static void aptex_set_signal (void)
{
#ifdef _WIN32
  if (signal(SIGINT, catch_interrupt) == SIG_ERR)
  {
    puts("ApTeX: CTRL-C handler not installed");
    aptex_utils_exit(EXIT_FAILURE);
  }
#else
  void(*old_handler)(int);

  old_handler = signal(SIGINT, catch_interrupt);

  if (old_handler != SIG_DFL)
    (void) signal(SIGINT, old_handler);
#endif
}

static int aptex_program (void);

void aptex_run (int argc, char ** argv)
{
#ifdef _WIN32
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

  // check of memory_word's size
  assert(sizeof(memory_word)  == sizeof(halfword) * 2);
  assert(sizeof(halfword)     == sizeof(quarterword) * 2);
  assert(sizeof(integer)      == sizeof(real));

  // for synctex
  synctex_option = INT_MAX;
  stop_at_space = true;
  is_in_csname = false;
#ifdef USE_KPATHSEA
  // for kpathsea init
  kpse_set_program_name(aptex_env.argv[0], NULL);
  kpse_set_program_enabled(kpse_fontmap_format, true, kpse_src_texmf_cnf);
  // for engine name
  xputenv("engine", "ptex-ng");
#endif
  // for ptexenc init: encoding
  init_default_kanji("utf8", "uptex");

  aptex_set_signal();
  aptex_commands_init(aptex_env.argc, aptex_env.argv);
  aptex_memory_init();

  aptex_env.time_start  = clock();
  aptex_env.time_main   = aptex_env.time_start;
  aptex_program();
  aptex_env.time_finish = clock();

  aptex_memory_free();

  if (!aptex_env.flag_initex)
  {
    printf("Total ");
    print_aptex_time(aptex_env.time_finish - aptex_env.time_start);
    printf("s (");
    print_aptex_time(aptex_env.time_main - aptex_env.time_start);
    printf(" format load + ");
    print_aptex_time(aptex_env.time_finish - aptex_env.time_main);
    printf(" processing)");

    if (total_pages > 0)
    {
      printf(" ");
      print_aptex_time((aptex_env.time_finish - aptex_env.time_main) / total_pages);
      printf("s per page");
    }

    printf(".\n");
  }

#ifdef WIN32
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
  safe_free(aptex_env.aptex_map);
}

static integer aptex_utils_round (real r)
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

#define XXHi(x) BYTE1(x)
#define Hi(x)   BYTE3(x)
#define Lo(x)   BYTE4(x)
#define nrestmultichr(x)  ((x) != 0 ? ((x) / 8) + 2 - ((x) % 8) : -1)
#define CJK_TOKEN_FLAG  0xFFFFFF

static boolean is_char_ascii (integer c)
{
  return (0 <= c && c < 0x100);
}

static boolean is_char_kanji (integer c)
{
  if (is_internalUPTEX())
    return (c >= 0);
  else
    return iskanji1(Hi(c)) && iskanji2(Lo(c));
}

static boolean check_kanji (integer c)
{
  if (c >= cs_token_flag)
    return false;
  else if (!(XXHi(c) >= kanji && XXHi(c) <= hangul))
    return false;
  else
    return is_char_kanji(c);
}

static boolean ismultiprn (integer c)
{
  int i, j;

  for (i = 2; i <= 4; i++)
    for (j = 1; j <= i; j++)
    {
      if (ismultichr(i, j, c))
        return true;
    }

  return false;
}

static integer calc_pos (integer c)
{
  unsigned char c1, c2;

  if (c >= 0 && c <= 255)
    return(c);

  c1 = Hi(c);
  c2 = Lo(c);

  c1 = (c1 % 4) * 64;  // c1 = 0, 64, 128, 192
  c2 = c2 % 64;        // c2 = 0..63
  return (c1 + c2);     // ret = 0..255
}

// Ref. http://www.unicode.org/Public/UNIDATA/Blocks.txt
static long ucs_range[] =
{
  0x0000, /* Basic Latin                                         */ /* 0x00 */
  0x0080, /* Latin-1 Supplement                                  */
  0x0100, /* Latin Extended-A                                    */
  0x0180, /* Latin Extended-B                                    */
  0x0250, /* IPA Extensions                                      */
  0x02B0, /* Spacing Modifier Letters                            */
  0x0300, /* Combining Diacritical Marks                         */
  0x0370, /* Greek and Coptic                                    */
  0x0400, /* Cyrillic                                            */
  0x0500, /* Cyrillic Supplement                                 */
  0x0530, /* Armenian                                            */
  0x0590, /* Hebrew                                              */
  0x0600, /* Arabic                                              */
  0x0700, /* Syriac                                              */
  0x0750, /* Arabic Supplement                                   */
  0x0780, /* Thaana                                              */
  0x07C0, /* NKo                                                 */ /* 0x10 */
  0x0800, /* Samaritan                                           */
  0x0840, /* Mandaic                                             */
  0x0860, /* Syriac Supplement                                   */
  0x0870, /* Arabic Extended-B                                   */
  0x08A0, /* Arabic Extended-A                                   */
  0x0900, /* Devanagari                                          */
  0x0980, /* Bengali                                             */
  0x0A00, /* Gurmukhi                                            */
  0x0A80, /* Gujarati                                            */
  0x0B00, /* Oriya                                               */
  0x0B80, /* Tamil                                               */
  0x0C00, /* Telugu                                              */
  0x0C80, /* Kannada                                             */
  0x0D00, /* Malayalam                                           */
  0x0D80, /* Sinhala                                             */
  0x0E00, /* Thai                                                */ /* 0x20 */
  0x0E80, /* Lao                                                 */
  0x0F00, /* Tibetan                                             */
  0x1000, /* Myanmar                                             */
  0x10A0, /* Georgian                                            */
  0x1100, /* Hangul Jamo                                         */
  0x1200, /* Ethiopic                                            */
  0x1380, /* Ethiopic Supplement                                 */
  0x13A0, /* Cherokee                                            */
  0x1400, /* Unified Canadian Aboriginal Syllabics               */
  0x1680, /* Ogham                                               */
  0x16A0, /* Runic                                               */
  0x1700, /* Tagalog                                             */
  0x1720, /* Hanunoo                                             */
  0x1740, /* Buhid                                               */
  0x1760, /* Tagbanwa                                            */
  0x1780, /* Khmer                                               */ /* 0x30 */
  0x1800, /* Mongolian                                           */
  0x18B0, /* Unified Canadian Aboriginal Syllabics Extended      */
  0x1900, /* Limbu                                               */
  0x1950, /* Tai Le                                              */
  0x1980, /* New Tai Lue                                         */
  0x19E0, /* Khmer Symbols                                       */
  0x1A00, /* Buginese                                            */
  0x1A20, /* Tai Tham                                            */
  0x1AB0, /* Combining Diacritical Marks Extended                */
  0x1B00, /* Balinese                                            */
  0x1B80, /* Sundanese                                           */
  0x1BC0, /* Batak                                               */
  0x1C00, /* Lepcha                                              */
  0x1C50, /* Ol Chiki                                            */
  0x1C80, /* Cyrillic Extended-C                                 */
  0x1C90, /* Georgian Extended                                   */ /* 0x40 */
  0x1CC0, /* Sundanese Supplement                                */
  0x1CD0, /* Vedic Extensions                                    */
  0x1D00, /* Phonetic Extensions                                 */
  0x1D80, /* Phonetic Extensions Supplement                      */
  0x1DC0, /* Combining Diacritical Marks Supplement              */
  0x1E00, /* Latin Extended Additional                           */
  0x1F00, /* Greek Extended                                      */
  0x2000, /* General Punctuation                                 */
  0x2070, /* Superscripts and Subscripts                         */
  0x20A0, /* Currency Symbols                                    */
  0x20D0, /* Combining Diacritical Marks for Symbols             */
  0x2100, /* Letterlike Symbols                                  */
  0x2150, /* Number Forms                                        */
  0x2190, /* Arrows                                              */
  0x2200, /* Mathematical Operators                              */
  0x2300, /* Miscellaneous Technical                             */ /* 0x50 */
  0x2400, /* Control Pictures                                    */
  0x2440, /* Optical Character Recognition                       */
  0x2460, /* Enclosed Alphanumerics                              */
  0x2500, /* Box Drawing                                         */
  0x2580, /* Block Elements                                      */
  0x25A0, /* Geometric Shapes                                    */
  0x2600, /* Miscellaneous Symbols                               */
  0x2700, /* Dingbats                                            */
  0x27C0, /* Miscellaneous Mathematical Symbols-A                */
  0x27F0, /* Supplemental Arrows-A                               */
  0x2800, /* Braille Patterns                                    */
  0x2900, /* Supplemental Arrows-B                               */
  0x2980, /* Miscellaneous Mathematical Symbols-B                */
  0x2A00, /* Supplemental Mathematical Operators                 */
  0x2B00, /* Miscellaneous Symbols and Arrows                    */
  0x2C00, /* Glagolitic                                          */ /* 0x60 */
  0x2C60, /* Latin Extended-C                                    */
  0x2C80, /* Coptic                                              */
  0x2D00, /* Georgian Supplement                                 */
  0x2D30, /* Tifinagh                                            */
  0x2D80, /* Ethiopic Extended                                   */
  0x2DE0, /* Cyrillic Extended-A                                 */
  0x2E00, /* Supplemental Punctuation                            */
  0x2E80, /* CJK Radicals Supplement                             */
  0x2F00, /* Kangxi Radicals                                     */
  0x2FF0, /* Ideographic Description Characters                  */
  0x3000, /* CJK Symbols and Punctuation                         */
  0x3040, /* Hiragana                                            */
  0x30A0, /* Katakana                                            */
  0x3100, /* Bopomofo                                            */
  0x3130, /* Hangul Compatibility Jamo                           */
  0x3190, /* Kanbun                                              */ /* 0x70 */
  0x31A0, /* Bopomofo Extended                                   */
  0x31C0, /* CJK Strokes                                         */
  0x31F0, /* Katakana Phonetic Extensions                        */
  0x3200, /* Enclosed CJK Letters and Months                     */
  0x3300, /* CJK Compatibility                                   */
  0x3400, /* CJK Unified Ideographs Extension A                  */
  0x4DC0, /* Yijing Hexagram Symbols                             */
  0x4E00, /* CJK Unified Ideographs                              */
  0xA000, /* Yi Syllables                                        */
  0xA490, /* Yi Radicals                                         */
  0xA4D0, /* Lisu                                                */
  0xA500, /* Vai                                                 */
  0xA640, /* Cyrillic Extended-B                                 */
  0xA6A0, /* Bamum                                               */
  0xA700, /* Modifier Tone Letters                               */
  0xA720, /* Latin Extended-D                                    */ /* 0x80 */
  0xA800, /* Syloti Nagri                                        */
  0xA830, /* Common Indic Number Forms                           */
  0xA840, /* Phags-pa                                            */
  0xA880, /* Saurashtra                                          */
  0xA8E0, /* Devanagari Extended                                 */
  0xA900, /* Kayah Li                                            */
  0xA930, /* Rejang                                              */
  0xA960, /* Hangul Jamo Extended-A                              */
  0xA980, /* Javanese                                            */
  0xA9E0, /* Myanmar Extended-B                                  */
  0xAA00, /* Cham                                                */
  0xAA60, /* Myanmar Extended-A                                  */
  0xAA80, /* Tai Viet                                            */
  0xAAE0, /* Meetei Mayek Extensions                             */
  0xAB00, /* Ethiopic Extended-A                                 */
  0xAB30, /* Latin Extended-E                                    */ /* 0x90 */
  0xAB70, /* Cherokee Supplement                                 */
  0xABC0, /* Meetei Mayek                                        */
  0xAC00, /* Hangul Syllables                                    */
  0xD7B0, /* Hangul Jamo Extended-B                              */
  0xD800, /* High Surrogates                                     */
  0xDB80, /* High Private Use Surrogates                         */
  0xDC00, /* Low Surrogates                                      */
  0xE000, /* Private Use Area                                    */
  0xF900, /* CJK Compatibility Ideographs                        */
  0xFB00, /* Alphabetic Presentation Forms                       */
  0xFB50, /* Arabic Presentation Forms-A                         */
  0xFE00, /* Variation Selectors                                 */
  0xFE10, /* Vertical Forms                                      */
  0xFE20, /* Combining Half Marks                                */
  0xFE30, /* CJK Compatibility Forms                             */
  0xFE50, /* Small Form Variants                                 */ /* 0xA0 */
  0xFE70, /* Arabic Presentation Forms-B                         */
  0xFF00, /* Halfwidth and Fullwidth Forms                       */
  0xFFF0, /* Specials                                            */
  0x10000, /* Linear B Syllabary                                  */
  0x10080, /* Linear B Ideograms                                  */
  0x10100, /* Aegean Numbers                                      */
  0x10140, /* Ancient Greek Numbers                               */
  0x10190, /* Ancient Symbols                                     */
  0x101D0, /* Phaistos Disc                                       */
  0x10280, /* Lycian                                              */
  0x102A0, /* Carian                                              */
  0x102E0, /* Coptic Epact Numbers                                */
  0x10300, /* Old Italic                                          */
  0x10330, /* Gothic                                              */
  0x10350, /* Old Permic                                          */
  0x10380, /* Ugaritic                                            */ /* 0xB0 */
  0x103A0, /* Old Persian                                         */
  0x10400, /* Deseret                                             */
  0x10450, /* Shavian                                             */
  0x10480, /* Osmanya                                             */
  0x104B0, /* Osage                                               */
  0x10500, /* Elbasan                                             */
  0x10530, /* Caucasian Albanian                                  */
  0x10570, /* Vithkuqi                                            */
  0x10600, /* Linear A                                            */
  0x10780, /* Latin Extended-F                                    */
  0x10800, /* Cypriot Syllabary                                   */
  0x10840, /* Imperial Aramaic                                    */
  0x10860, /* Palmyrene                                           */
  0x10880, /* Nabataean                                           */
  0x108E0, /* Hatran                                              */
  0x10900, /* Phoenician                                          */ /* 0xC0 */
  0x10920, /* Lydian                                              */
  0x10980, /* Meroitic Hieroglyphs                                */
  0x109A0, /* Meroitic Cursive                                    */
  0x10A00, /* Kharoshthi                                          */
  0x10A60, /* Old South Arabian                                   */
  0x10A80, /* Old North Arabian                                   */
  0x10AC0, /* Manichaean                                          */
  0x10B00, /* Avestan                                             */
  0x10B40, /* Inscriptional Parthian                              */
  0x10B60, /* Inscriptional Pahlavi                               */
  0x10B80, /* Psalter Pahlavi                                     */
  0x10C00, /* Old Turkic                                          */
  0x10C80, /* Old Hungarian                                       */
  0x10D00, /* Hanifi Rohingya                                     */
  0x10E60, /* Rumi Numeral Symbols                                */
  0x10E80, /* Yezidi                                              */ /* 0xD0 */
  0x10EC0, /* Arabic Extended-C                                   */
  0x10F00, /* Old Sogdian                                         */
  0x10F30, /* Sogdian                                             */
  0x10F70, /* Old Uyghur                                          */
  0x10FB0, /* Chorasmian                                          */
  0x10FE0, /* Elymaic                                             */
  0x11000, /* Brahmi                                              */
  0x11080, /* Kaithi                                              */
  0x110D0, /* Sora Sompeng                                        */
  0x11100, /* Chakma                                              */
  0x11150, /* Mahajani                                            */
  0x11180, /* Sharada                                             */
  0x111E0, /* Sinhala Archaic Numbers                             */
  0x11200, /* Khojki                                              */
  0x11280, /* Multani                                             */
  0x112B0, /* Khudawadi                                           */ /* 0xE0 */
  0x11300, /* Grantha                                             */
  0x11400, /* Newa                                                */
  0x11480, /* Tirhuta                                             */
  0x11580, /* Siddham                                             */
  0x11600, /* Modi                                                */
  0x11660, /* Mongolian Supplement                                */
  0x11680, /* Takri                                               */
  0x11700, /* Ahom                                                */
  0x11800, /* Dogra                                               */
  0x118A0, /* Warang Citi                                         */
  0x11900, /* Dives Akuru                                         */
  0x119A0, /* Nandinagari                                         */
  0x11A00, /* Zanabazar Square                                    */
  0x11A50, /* Soyombo                                             */
  0x11AB0, /* Unified Canadian Aboriginal Syllabics Extended-A    */
  0x11AC0, /* Pau Cin Hau                                         */ /* 0xF0 */
  0x11B00, /* Devanagari Extended-A                               */
  0x11C00, /* Bhaiksuki                                           */
  0x11C70, /* Marchen                                             */
  0x11D00, /* Masaram Gondi                                       */
  0x11D60, /* Gunjala Gondi                                       */
  0x11EE0, /* Makasar                                             */
  0x11F00, /* Kawi                                                */
  0x11FB0, /* Lisu Supplement                                     */
  0x11FC0, /* Tamil Supplement                                    */
  0x12000, /* Cuneiform                                           */
  0x12400, /* Cuneiform Numbers and Punctuation                   */
  0x12480, /* Early Dynastic Cuneiform                            */
  0x12F90, /* Cypro-Minoan                                        */
  0x13000, /* Egyptian Hieroglyphs                                */
  0x13430, /* Egyptian Hieroglyph Format Controls                 */
  0x14400, /* Anatolian Hieroglyphs                               */ /* 0x100 */
  0x16800, /* Bamum Supplement                                    */
  0x16A40, /* Mro                                                 */
  0x16A70, /* Tangsa                                              */
  0x16AD0, /* Bassa Vah                                           */
  0x16B00, /* Pahawh Hmong                                        */
  0x16E40, /* Medefaidrin                                         */
  0x16F00, /* Miao                                                */
  0x16FE0, /* Ideographic Symbols and Punctuation                 */
  0x17000, /* Tangut                                              */
  0x18800, /* Tangut Components                                   */
  0x18B00, /* Khitan Small Script                                 */
  0x18D00, /* Tangut Supplement                                   */
  0x1AFF0, /* Kana Extended-B                                     */
  0x1B000, /* Kana Supplement                                     */
  0x1B100, /* Kana Extended-A                                     */
  0x1B130, /* Small Kana Extension                                */ /* 0x110 */
  0x1B170, /* Nushu                                               */
  0x1BC00, /* Duployan                                            */
  0x1BCA0, /* Shorthand Format Controls                           */
  0x1CF00, /* Znamenny Musical Notation                           */
  0x1D000, /* Byzantine Musical Symbols                           */
  0x1D100, /* Musical Symbols                                     */
  0x1D200, /* Ancient Greek Musical Notation                      */
  0x1D2C0, /* Kaktovik Numerals                                   */
  0x1D2E0, /* Mayan Numerals                                      */
  0x1D300, /* Tai Xuan Jing Symbols                               */
  0x1D360, /* Counting Rod Numerals                               */
  0x1D400, /* Mathematical Alphanumeric Symbols                   */
  0x1D800, /* Sutton SignWriting                                  */
  0x1DF00, /* Latin Extended-G                                    */
  0x1E000, /* Glagolitic Supplement                               */
  0x1E030, /* Cyrillic Extended-D                                 */ /* 0x120 */
  0x1E100, /* Nyiakeng Puachue Hmong                              */
  0x1E290, /* Toto                                                */
  0x1E2C0, /* Wancho                                              */
  0x1E4D0, /* Nag Mundari                                         */
  0x1E7E0, /* Ethiopic Extended-B                                 */
  0x1E800, /* Mende Kikakui                                       */
  0x1E900, /* Adlam                                               */
  0x1EC70, /* Indic Siyaq Numbers                                 */
  0x1ED00, /* Ottoman Siyaq Numbers                               */
  0x1EE00, /* Arabic Mathematical Alphabetic Symbols              */
  0x1F000, /* Mahjong Tiles                                       */
  0x1F030, /* Domino Tiles                                        */
  0x1F0A0, /* Playing Cards                                       */
  0x1F100, /* Enclosed Alphanumeric Supplement                    */
  0x1F200, /* Enclosed Ideographic Supplement                     */
  0x1F300, /* Miscellaneous Symbols and Pictographs               */ /* 0x130 */
  0x1F600, /* Emoticons                                           */
  0x1F650, /* Ornamental Dingbats                                 */
  0x1F680, /* Transport and Map Symbols                           */
  0x1F700, /* Alchemical Symbols                                  */
  0x1F780, /* Geometric Shapes Extended                           */
  0x1F800, /* Supplemental Arrows-C                               */
  0x1F900, /* Supplemental Symbols and Pictographs                */
  0x1FA00, /* Chess Symbols                                       */
  0x1FA70, /* Symbols and Pictographs Extended-A                  */
  0x1FB00, /* Symbols for Legacy Computing                        */
  0x20000, /* CJK Unified Ideographs Extension B                  */
  0x2A700, /* CJK Unified Ideographs Extension C                  */
  0x2B740, /* CJK Unified Ideographs Extension D                  */
  0x2B820, /* CJK Unified Ideographs Extension E                  */
  0x2CEB0, /* CJK Unified Ideographs Extension F                  */
  0x2F800, /* CJK Compatibility Ideographs Supplement             */ /* 0x140 */
  0x30000, /* CJK Unified Ideographs Extension G                  */
  0x31350, /* CJK Unified Ideographs Extension H                  */
  0x323B0, /* Reserved                                            */
  0x40000, /* Reserved                                            */
  0x50000, /* Reserved                                            */
  0x60000, /* Reserved                                            */
  0x70000, /* Reserved                                            */
  0x80000, /* Reserved                                            */
  0x90000, /* Reserved                                            */
  0xA0000, /* Reserved                                            */
  0xB0000, /* Reserved                                            */
  0xC0000, /* Reserved                                            */
  0xD0000, /* Reserved                                            */
  0xE0000, /* Tags                                                */
  0xE0100, /* Variation Selectors Supplement                      */
  0xF0000, /* Supplementary Private Use Area-A                    */ /* 0x150 */
  0x100000, /* Supplementary Private Use Area-B                    */
/* Value over 0x10FFFF is illegal under Unicode, They are for some special use.  *** experimental ***  */
  0x110000, /* Reserved                                          */
  0x120000, /* Reserved                                          */
  0x130000, /* Reserved                                          */
  0x140000, /* Reserved                                          */
  0x150000, /* Reserved                                          */
  0x160000, /* Reserved                                          */
  0x170000, /* Reserved                                          */
  0x180000, /* Reserved                                          */
  0x190000, /* Reserved                                          */
  0x1A0000, /* Reserved                                          */
  0x1B0000, /* Reserved                                          */
  0x1C0000, /* Reserved                                          */
  0x1D0000, /* Reserved                                          */
  0x1E0000, /* Reserved                                          */
  0x1F0000, /* Reserved                                          */ /* 0x160 */
  0x200000, /* Reserved                                          */
  0x210000, /* Reserved                                          */
  0x220000, /* Reserved                                          */
  max_cjk_val
};

#define NUCS_RANGE (sizeof(ucs_range)/sizeof(ucs_range[0]))

static int binary_search (long x, long *a, int left, int right)
{
  right++;

  while (left < right)
  {
    int mid = (left + right) / 2;

    if (a[mid] <= x)
      left = mid + 1;
    else
      right = mid;
  }

  return left - 1;
}

#define FEMININE_ORDINAL_INDICATOR             0x00AA
#define MASCULINE_ORDINAL_INDICATOR            0x00BA
#define LATIN_CAPITAL_LETTER_A_WITH_GRAVE      0x00C0
#define LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS  0x00D6
#define LATIN_CAPITAL_LETTER_O_WITH_STROKE     0x00D8
#define LATIN_SMALL_LETTER_O_WITH_DIAERESIS    0x00F6
#define LATIN_SMALL_LETTER_O_WITH_STROKE       0x00F8
#define LATIN_SMALL_LETTER_Y_WITH_DIAERESIS    0x00FF
#define FULLWIDTH_DIGIT_0             0xFF10
#define FULLWIDTH_DIGIT_9             0xFF19
#define FULLWIDTH_CAPITAL_A           0xFF21
#define FULLWIDTH_CAPITAL_Z           0xFF3A
#define FULLWIDTH_SMALL_A             0xFF41
#define FULLWIDTH_SMALL_Z             0xFF5A
#define HALFWIDTH_KATAKANA_WO         0xFF66
#define HALFWIDTH_KATAKANA_SMALL_TSU  0xFF6F
#define HALFWIDTH_KATAKANA_A          0xFF71
#define HALFWIDTH_KATAKANA_N          0xFF9D

static integer kcatcodekey (integer c)
{
  integer block;

  if (is_internalUPTEX())
  {
    block = binary_search((long)c, ucs_range, 0, NUCS_RANGE-1);

    if (block == 0x01)
    {
      /* Latin-1 Letters */
      if (FEMININE_ORDINAL_INDICATOR == c
        || MASCULINE_ORDINAL_INDICATOR == c
        || (LATIN_CAPITAL_LETTER_A_WITH_GRAVE <= c && c <= LATIN_CAPITAL_LETTER_O_WITH_DIAERESIS)
        || (LATIN_CAPITAL_LETTER_O_WITH_STROKE <= c && c <= LATIN_SMALL_LETTER_O_WITH_DIAERESIS)
        || (LATIN_SMALL_LETTER_O_WITH_STROKE <= c && c <= LATIN_SMALL_LETTER_Y_WITH_DIAERESIS))
        return 0x01FD;
    }

    if (block == 0xa1)
    {
      /* Fullwidth ASCII variants  except for U+FF01..FF0F, U+FF1A..FF20, U+FF3B..FF40, U+FF5B..FF5E */
      if ((FULLWIDTH_DIGIT_0 <= c && c <= FULLWIDTH_DIGIT_9)
        || (FULLWIDTH_CAPITAL_A <= c && c <= FULLWIDTH_CAPITAL_Z)
        || (FULLWIDTH_SMALL_A <= c && c <= FULLWIDTH_SMALL_Z))
        return 0x1FE;

      /* Halfwidth Katakana variants  except for U+FF65, U+FF70, U+FF9E..FF9F */
      if ((HALFWIDTH_KATAKANA_WO <= c && c <= HALFWIDTH_KATAKANA_SMALL_TSU)
        || (HALFWIDTH_KATAKANA_A <= c && c <= HALFWIDTH_KATAKANA_N))
        return 0x1FF;
    }

    return block;
  }
  else
  {
    return Hi(toDVI(c));
  }
}

static integer multilenbuffchar (integer c)
{
  c = toBUFF(c);
  if (BYTE1(c)) return 4;
  if (BYTE2(c)) return 3;
  if (BYTE3(c)) return 2;
  if (BYTE4(c)) return 1;
  return 0;
}

static void init_default_kanji (const_string file_str, const_string internal_str)
{
  enable_UPTEX(true);

  if (!set_enc_string(file_str, internal_str))
  {
    fprintf(stderr, "Bad kanji encoding \"%s\" or \"%s\".\n",
      file_str ? file_str : "NULL",
      internal_str ? internal_str : "NULL");
    aptex_utils_exit(EXIT_FAILURE);
  }
}

static char * mbcs_utf8 (const char * mbcs_str)
{
#ifdef WIN32
  if (mbcs_str == NULL)
    return NULL;
  else
  {
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
      return NULL;

    utf16_str = (LPWSTR) malloc((utf16_len + 1) * sizeof(utf16_str[0]));

    if (utf16_str == NULL)
      return NULL;

    MultiByteToWideChar(codepage, 0, mbcs_str, -1, utf16_str, utf16_len);
    utf8_len = WideCharToMultiByte(CP_UTF8, 0, utf16_str, utf16_len, 0, 0, 0, 0);
    utf8_str = utf8_len ? (char *) malloc(utf8_len + 1) : NULL;

    if (utf8_str)
    {
      WideCharToMultiByte(CP_UTF8, 0, utf16_str, utf16_len, utf8_str, utf8_len, 0, 0);
    }

    free(utf16_str);

    return utf8_str;
  }
#else
  return strdup(mbcs_str);
#endif
}

static char * utf8_mbcs (const char * utf8_str)
{
#ifdef WIN32
  if (utf8_str == NULL)
    return NULL;
  else
  {
    size_t utf8_len;
    int    utf16_len;
    int    mbcs_len;
    LPWSTR utf16_str;
    char * mbcs_str;
    int    codepage;

    utf8_len = strlen(utf8_str);
    utf16_len = MultiByteToWideChar(CP_UTF8, 0, utf8_str, -1, NULL, 0);

    if (utf16_len == 0)
      return NULL;

    utf16_str = (LPWSTR) malloc((utf16_len + 1) * sizeof(utf16_str[0]));

    if (utf16_str == NULL)
      return NULL;

    MultiByteToWideChar(CP_UTF8, 0, utf8_str, -1, utf16_str, utf16_len);
    codepage = AreFileApisANSI() ? CP_ACP : CP_OEMCP;
    mbcs_len = WideCharToMultiByte(codepage, 0, utf16_str, utf16_len, 0, 0, 0, 0);
    mbcs_str = mbcs_len ? (char *) malloc(mbcs_len + 1) : NULL;

    if (mbcs_str)
    {
      WideCharToMultiByte(codepage, 0, utf16_str, utf16_len, mbcs_str, mbcs_len, 0, 0);
    }

    free(utf16_str);

    return mbcs_str;
  }
#else
  return strdup(utf8_str);
#endif
}

static void t_open_in (void)
{
  buffer[first] = 0;
  sprintf((char *) &buffer[first], "%s", aptex_env.aptex_src);

  for (last = first; buffer[last]; ++last)
    do_nothing();

  for (--last; last >= first && isblank(buffer[last]) && buffer[last] != '\r'; --last)
    do_nothing();

  last++;
}

/* sec 0031 */
// inputs the next line or returns |false|
static boolean input_ln (alpha_file f, boolean bypass_eoln)
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
    i = fgetc(f.file_data);

    if ((i == EOF) || (i == '\n') || (i == '\r'))
      break;
    else
    {
      buffer[last++] = i;

#ifdef APTEX_EXTENSION
      if (last >= current_buf_size)
        buffer = realloc_buffer(increment_buf_size);
#endif
    }
  }

  /* LF: Unix. CRLF: Windows */
  if (i == '\r')
  {
    i = fgetc(f.file_data);

    if (i != '\n')
    {
      ungetc(i, f.file_data);
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

static boolean a_open_input (alpha_file * f)
{
  boolean openable = false;
  char * file_name_kpse = NULL;
  char * file_name_mbcs = NULL;

  char * file_name_utf8 = (char *) calloc(1, name_length + 1);
  strncpy(file_name_utf8, (const char *) name_of_file + 1, name_length);

  file_name_mbcs = utf8_mbcs(file_name_utf8);

  if (file_name_mbcs[0] == '|' && aptex_env.flag_shell_escape == true)
  {
    f->file_data = popen(file_name_mbcs + 1, "r");
    f->file_type = 1;
    if (f->file_data) openable = true;
  }
  else
  {
#ifdef USE_KPATHSEA
    file_name_kpse = kpse_find_file((const_string) file_name_mbcs, kpse_tex_format, false);
#else
    file_name_kpse = file_name_mbcs;
#endif /* USE_KPATHSEA */

    if (file_name_kpse != NULL)
    {
      f->file_data = fopen(file_name_kpse, "rb");
      f->file_type = 0;
      if (f->file_data) openable = true;
    }
  }

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  if (file_name_kpse != NULL)
    free(file_name_kpse);

  return openable;
}

static boolean b_open_input (byte_file * f)
{
  boolean openable = false;
  char * file_name_kpse = NULL;
  char * file_name_mbcs = NULL;

  char * file_name_utf8 = (char *) calloc(1, name_length + 1);
  strncpy(file_name_utf8, (const char *) name_of_file + 1, name_length);

  if (name_length > 3 && (strncasecmp(file_name_utf8, "ot:", 3) == 0))
    file_name_mbcs = utf8_mbcs(strrchr(file_name_utf8, ':') + 1);
  else
    file_name_mbcs = utf8_mbcs(file_name_utf8);

#ifdef USE_KPATHSEA
  file_name_kpse = kpse_find_file((const_string) file_name_mbcs, kpse_tfm_format, true);
#else
  file_name_kpse = file_name_mbcs;
#endif

  if (file_name_kpse != NULL)
  {
    *f = fopen(file_name_kpse, "rb");

    if (*f)
    {
      fbyte = fgetc((FILE *) *f);
      openable = true;
    }
  }

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  if (file_name_kpse != NULL)
    free(file_name_kpse);

  return openable;
}

static boolean w_open_input (word_file * f)
{
  boolean openable = false;
  char * file_name_kpse = NULL;
  char * file_name_mbcs = NULL;
  char * file_name_utf8 = (char *) calloc(1, name_length + 1);

  strncpy(file_name_utf8, (const char *) name_of_file + 1, name_length);
  file_name_mbcs = utf8_mbcs(file_name_utf8);
#ifdef USE_KPATHSEA
  file_name_kpse = kpse_find_file((const_string) file_name_mbcs, kpse_fmt_format, false);
#else
  file_name_kpse = file_name_mbcs;
#endif

  if (file_name_kpse != NULL)
  {
    *f = gzopen(file_name_kpse, "rb");
    if (*f) openable = true;
  }

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  if (file_name_kpse != NULL)
    free(file_name_kpse);

  return openable;
}

static void a_close (alpha_file f)
{
  switch (f.file_type)
  {
    case 0:
      fclose(f.file_data);
      break;
    case 1:
      pclose(f.file_data);
      break;
  }
}

static void b_close (byte_file f)
{
  if (f == NULL)
    return;
  else if (ferror(f) || fclose(f))
  {
    perror("\n! I/O Error");
    aptex_utils_exit(EXIT_FAILURE);
  }
}

static void w_close (word_file f)
{
  gzclose(f);
}

static boolean a_open_output (alpha_file * f)
{
  char * file_name_mbcs = NULL;
  char * file_name_utf8 = (char *) calloc(1, name_length + 1);

  strncpy(file_name_utf8, (const char *) name_of_file + 1, name_length);
  file_name_mbcs = utf8_mbcs(file_name_utf8);

  f->file_data = fopen(file_name_mbcs, "wb");
  f->file_type = 0;

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  return (f->file_data != NULL);
}

static boolean b_open_output (byte_file * f)
{
  char * file_name_mbcs = NULL;
  char * file_name_utf8 = (char *) calloc(1, name_length + 1);

  strncpy(file_name_utf8, (const char *) name_of_file + 1, name_length);
  file_name_mbcs = utf8_mbcs(file_name_utf8);

  *f = fopen(file_name_mbcs, "wb");

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  return (*f != NULL);
}

static boolean w_open_output (word_file * f)
{
  char * file_name_mbcs = NULL;
  char * file_name_utf8 = (char *) calloc(1, name_length + 1);

  strncpy(file_name_utf8, (const char *) name_of_file + 1, name_length);
  file_name_mbcs = utf8_mbcs(file_name_utf8);

  *f = gzopen(file_name_mbcs, "wb");

  if (file_name_mbcs != NULL)
    free(file_name_mbcs);

  if (file_name_utf8 != NULL)
    free(file_name_utf8);

  return (*f != NULL);
}

static const char * pool_file_arr[] =
{
/* 0256 */  "", //"buffer size",
/* 0257 */  "", //"pool size",
/* 0258 */  "", //"number of strings",
/* 0259 */  "" "?" "?" "?",
/* 0260 */  "m2d5c2l5x2v5i",
/* 0261 */  "", //"End of file on the terminal!",
/* 0262 */  "", //"! ",
/* 0263 */  "", //"(That makes 100 errors; please try again.)",
/* 0264 */  "", // "? ",
/* 0265 */  "", //"Type <return> to proceed, S to scroll future error messages,",
/* 0266 */  "", //"R to run without stopping, Q to run quietly,",
/* 0267 */  "", //"I to insert something, ",
/* 0268 */  "", //"E to edit your file,",
/* 0269 */  "", //"1 or ... or 9 to ignore the next 1 to 9 tokens of input,",
/* 0270 */  "", //"H for help, X to quit.",
/* 0271 */  "", //"OK, entering ",
/* 0272 */  "", //"batchmode",
/* 0273 */  "", //"nonstopmode",
/* 0274 */  "", //"scrollmode",
/* 0275 */  "", //"...",
/* 0276 */  "", //"insert>",
/* 0277 */  "", //"I have just deleted some text, as you asked.",
/* 0278 */  "", //"You can now delete more, or insert, or whatever.",
/* 0279 */  "", //"Sorry, I don't know how to help in this situation.",
/* 0280 */  "", //"Maybe you should try asking a human" "?",
/* 0281 */  "", //"Sorry, I already gave what help I could...",
/* 0282 */  "", //"An error might have occurred before I noticed any problems.",
/* 0283 */  "", //"``If all else fails, read the instructions.''",
/* 0284 */  "", //" (",
/* 0285 */  "", //"Emergency stop",
/* 0286 */  "", //"TeX capacity exceeded, sorry [",
/* 0287 */  "", //"If you really absolutely need more capacity,",
/* 0288 */  "", //"you can ask a wizard to enlarge me.",
/* 0289 */  "", //"This can't happen (",
/* 0290 */  "", //"I'm broken. Please show this to someone who can fix can fix",
/* 0291 */  "", //"I can't go on meeting you like this",
/* 0292 */  "", //"One of your faux pas seems to have wounded me deeply...",
/* 0293 */  "", //"in fact, I'm barely conscious. Please fix it and try again.",
/* 0294 */  "", //"Interruption",
/* 0295 */  "", //"You rang" "?",
/* 0296 */  "", //"Try to insert some instructions for me (e.g.,`I\\showlists'),",
/* 0297 */  "", //"unless you just want to quit by typing `X'.",
/* 0298 */  "", //"main memory size",
/* 0299 */  "", //"AVAIL list clobbered at ",
/* 0300 */  "", //"Double-AVAIL list clobbered at ",
/* 0301 */  "", //"Doubly free location at ",
/* 0302 */  "", //"Bad flag at ",
/* 0303 */  "", //"New busy locs:",
/* 0304 */  "", //"LINK(",
/* 0305 */  "", //"INFO(",
/* 0306 */  "", //"[]",
/* 0307 */  "", //"CLOBBERED.",
/* 0308 */  "", //"foul",
/* 0309 */  "", //"fil",
/* 0310 */  "", //" plus ",
/* 0311 */  "", //" minus ",
/* 0312 */  "", //" []",
/* 0313 */  "", //"Bad link, display aborted.",
/* 0314 */  "", //"etc.",
/* 0315 */  "", //"Unknown node type!",
/* 0316 */  "", //"unset",
/* 0317 */  "", //"box(",
/* 0318 */  "", //")x",
/* 0319 */  "", //", shifted ",
/* 0320 */  "", //" columns)",
/* 0321 */  "", //", stretch ",
/* 0322 */  "", //", shrink ",
/* 0323 */  "", //", glue set ",
/* 0324 */  "", //"- ",
/* 0325 */  "", //"< -",
/* 0326 */  "", //"rule(",
/* 0327 */  "", //"insert",
/* 0328 */  "", //", natural size ",
/* 0329 */  "", //"; split(",
/* 0330 */  "", //"); float cost ",
/* 0331 */  "", //"glue",
/* 0332 */  "", //"nonscript",
/* 0333 */  "", //"mskip",
/* 0334 */  "", //"mu",
/* 0335 */  "",
/* 0336 */  "", //"leaders ",
/* 0337 */  "", //"kern",
/* 0338 */  "", //" (for accent)",
/* 0339 */  "", //"mkern",
/* 0340 */  "", //"math",
/* 0341 */  "", //"on",
/* 0342 */  "", //"off",
/* 0343 */  "", //", surrounded ",
/* 0344 */  "", //" (ligature ",
/* 0345 */  "", //"penalty ",
/* 0346 */  "", //"discretionary",
/* 0347 */  "", //" replacing ",
/* 0348 */  "", //"mark",
/* 0349 */  "", //"vadjust",
/* 0350 */  "", //"flushing",
/* 0351 */  "", //"copying",
/* 0352 */  "", //"vertical",
/* 0353 */  "", //"horizontal",
/* 0354 */  "", //"display math",
/* 0355 */  "", //"no",
/* 0356 */  "", //"internal vertical",
/* 0357 */  "", //"restricted horizontal",
/* 0358 */  "", //" mode",
/* 0359 */  "", //"semantic nest size",
/* 0360 */  "", //"### ",
/* 0361 */  "", //" entered at line ",
/* 0362 */  "", //" (language",
/* 0363 */  "", //":hyphenmin",
/* 0364 */  "", //" (\\output routine)",
/* 0365 */  "", //"### recent contributions:",
/* 0366 */  "", //"prevdepth ",
/* 0367 */  "", //"ignored",
/* 0368 */  "", //", prevgraf ",
/* 0369 */  "", //" line",
/* 0370 */  "", //"spacefactor ",
/* 0371 */  "", //", current language ",
/* 0372 */  "", //"this will be denominator of:",
/* 0373 */  "", //"lineskip",
/* 0374 */  "", //"baselineskip",
/* 0375 */  "", //"parskip",
/* 0376 */  "", //"abovedisplayskip",
/* 0377 */  "", //"belowdisplayskip",
/* 0378 */  "", //"abovedisplayshortskip",
/* 0379 */  "", //"belowdisplayshortskip",
/* 0380 */  "", //"leftskip",
/* 0381 */  "", //"rightskip",
/* 0382 */  "", //"topskip",
/* 0383 */  "", //"splittopskip",
/* 0384 */  "", //"tabskip",
/* 0385 */  "", //"spaceskip",
/* 0386 */  "", //"xspaceskip",
/* 0387 */  "", //"parfillskip",
/* 0388 */  "", //"thinmuskip",
/* 0389 */  "", //"medmuskip",
/* 0390 */  "", //"thickmuskip",
/* 0391 */  "", //"[unknown glue parameter!]",
/* 0392 */  "", //"skip",
/* 0393 */  "", //"muskip",
/* 0394 */  "", //"pt",
/* 0395 */  "", //"output",
/* 0396 */  "", //"everypar",
/* 0397 */  "", //"everymath",
/* 0398 */  "", //"everydisplay",
/* 0399 */  "", //"everyhbox",
/* 0400 */  "", //"everyvbox",
/* 0401 */  "", //"everyjob",
/* 0402 */  "", //"everycr",
/* 0403 */  "", //"errhelp",
/* 0404 */  "", //"toks",
/* 0405 */  "", //"parshape",
/* 0406 */  "", //"box",
/* 0407 */  "", //"void",
/* 0408 */  "", //"current font",
/* 0409 */  "", //"textfont",
/* 0410 */  "", //"scriptfont",
/* 0411 */  "", //"scriptscriptfont",
/* 0412 */  "", //"catcode",
/* 0413 */  "", //"lccode",
/* 0414 */  "", //"uccode",
/* 0415 */  "", //"sfcode",
/* 0416 */  "", //"mathcode",
/* 0417 */  "", //"pretolerance",
/* 0418 */  "", //"tolerance",
/* 0419 */  "", //"linepenalty",
/* 0420 */  "", //"hyphenpenalty",
/* 0421 */  "", //"exhyphenpenalty",
/* 0422 */  "", //"clubpenalty",
/* 0423 */  "", //"widowpenalty",
/* 0424 */  "", //"displaywidowpenalty",
/* 0425 */  "", //"brokenpenalty",
/* 0426 */  "", //"binoppenalty",
/* 0427 */  "", //"relpenalty",
/* 0428 */  "", //"predisplaypenalty",
/* 0429 */  "", //"postdisplaypenalty",
/* 0430 */  "", //"interlinepenalty",
/* 0431 */  "", //"doublehyphendemerits",
/* 0432 */  "", //"finalhyphendemerits",
/* 0433 */  "", //"adjdemerits",
/* 0434 */  "", //"mag",
/* 0435 */  "", //"delimiterfactor",
/* 0436 */  "", //"looseness",
/* 0437 */  "", //"time",
/* 0438 */  "", //"day",
/* 0439 */  "", //"month",
/* 0440 */  "", //"year",
/* 0441 */  "", //"showboxbreadth",
/* 0442 */  "", //"showboxdepth",
/* 0443 */  "", //"hbadness",
/* 0444 */  "", //"vbadness",
/* 0445 */  "", //"pausing",
/* 0446 */  "", //"tracingonline",
/* 0447 */  "", //"tracingmacros",
/* 0448 */  "", //"tracingstats",
/* 0449 */  "", //"tracingparagraphs",
/* 0450 */  "", //"tracingpages",
/* 0451 */  "", //"tracingoutput",
/* 0452 */  "", //"tracinglostchars",
/* 0453 */  "", //"tracingcommands",
/* 0454 */  "", //"tracingrestores",
/* 0455 */  "", //"uchyph",
/* 0456 */  "", //"outputpenalty",
/* 0457 */  "", //"maxdeadcycles",
/* 0458 */  "", //"hangafter",
/* 0459 */  "", //"floatingpenalty",
/* 0460 */  "", //"globaldefs",
/* 0461 */  "", //"fam",
/* 0462 */  "", //"escapechar",
/* 0463 */  "", //defaulthyphenchar",
/* 0464 */  "", //"defaultskewchar",
/* 0465 */  "", //"endlinechar",
/* 0466 */  "", //"newlinechar",
/* 0467 */  "", //"language",
/* 0468 */  "", //"lefthyphenmin",
/* 0469 */  "", //"righthyphenmin",
/* 0470 */  "", //"holdinginserts",
/* 0471 */  "", //"errorcontextlines",
/* 0472 */  "", //"[unknown integer parameter!]",
/* 0473 */  "", //"count",
/* 0474 */  "", //"delcode",
/* 0475 */  "", //"parindent",
/* 0476 */  "", //"mathsurround",
/* 0477 */  "", //"lineskiplimit",
/* 0478 */  "", //"hsize",
/* 0479 */  "", //"vsize",
/* 0480 */  "", //"maxdepth",
/* 0481 */  "", //"splitmaxdepth",
/* 0482 */  "", //"boxmaxdepth",
/* 0483 */  "", //"hfuzz",
/* 0484 */  "", //"vfuzz",
/* 0485 */  "", //"delimitershortfall",
/* 0486 */  "", //"nulldelimiterspace",
/* 0487 */  "", //"scriptspace",
/* 0488 */  "", //"predisplaysize",
/* 0489 */  "", //"displaywidth",
/* 0490 */  "", //"displayindent",
/* 0491 */  "", //"overfullrule",
/* 0492 */  "", //"hangindent",
/* 0493 */  "", //"hoffset",
/* 0494 */  "", //"voffset",
/* 0495 */  "", //"emergencystretch",
/* 0496 */  "", //"[unknown dimen parameter!]",
/* 0497 */  "", //"dimen",
/* 0498 */  "", //"EQUIV(",
/* 0499 */  "notexpanded:",
/* 0500 */  "", //"hash size",
/* 0501 */  "", //"csname",
/* 0502 */  "", //"endcsname",
/* 0503 */  "", //"IMPOSSIBLE.",
/* 0504 */  "", //"NONEXISTENT.",
/* 0505 */  "", //"accent",
/* 0506 */  "", //"advance",
/* 0507 */  "", //"afterassignment",
/* 0508 */  "", //"aftergroup",
/* 0509 */  "", //"begingroup",
/* 0510 */  "", //"char",
/* 0511 */  "", //"delimiter",
/* 0512 */  "", //"divide",
/* 0513 */  "", //"endgroup",
/* 0514 */  "", //"expandafter",
/* 0515 */  "", //"font",
/* 0516 */  "", //"fontdimen",
/* 0517 */  "", //"halign",
/* 0518 */  "", //"hrule",
/* 0519 */  "", //"ignorespaces",
/* 0520 */  "", //"mathaccent",
/* 0521 */  "", //"mathchar",
/* 0522 */  "", //"mathchoice",
/* 0523 */  "", //"multiply",
/* 0524 */  "", //"noalign",
/* 0525 */  "", //"noboundary",
/* 0526 */  "", //"noexpand",
/* 0527 */  "", //"omit",
/* 0528 */  "", //"penalty",
/* 0529 */  "", //"prevgraf",
/* 0530 */  "", //"radical",
/* 0531 */  "", //"read",
/* 0532 */  "", //"relax",
/* 0533 */  "", //"setbox",
/* 0534 */  "", //"the",
/* 0535 */  "", //"valign",
/* 0536 */  "", //"vcenter",
/* 0537 */  "", //"vrule",
/* 0538 */  "", //"save size",
/* 0539 */  "", //"grouping levels",
/* 0540 */  "", //"curlevel",
/* 0541 */  "", //"retaining",
/* 0542 */  "", //"restoring",
/* 0543 */  "", //"SAVE(",
/* 0544 */  "", //"Incompatible magnification (",
/* 0545 */  "", //");",
/* 0546 */  "", //" the previous value will be retained",
/* 0547 */  "", //"I can handle only one magnification ratio per job. So I've",
/* 0548 */  "", //"reverted to the magnification you used earlier on this run.",
/* 0549 */  "", //"Illegal magnification has been changed to 1000",
/* 0550 */  "", //"The magnification ratio must be between 1 and 32768.",
/* 0551 */  "", //"ETC.",
/* 0552 */  "", //"BAD.",
/* 0553 */  "", //"->",
/* 0554 */  "", //"begin-group character ",
/* 0555 */  "", //"end-group character ",
/* 0556 */  "", //"math shift character ",
/* 0557 */  "", //"macro parameter character ",
/* 0558 */  "", //"superscript character ",
/* 0559 */  "", //"subscript character ",
/* 0560 */  "", //"end of alignment template",
/* 0561 */  "", //"blank space ",
/* 0562 */  "", //"the letter ",
/* 0563 */  "", //"the character ",
/* 0564 */  "", //"[unknown command code!]",
/* 0565 */  "", //": ",
/* 0566 */  "", //"Runaway ",
/* 0567 */  "", //"definition",
/* 0568 */  "", //"argument",
/* 0569 */  "", //"preamble",
/* 0570 */  "", //"text",
/* 0571 */  "", //"<*>",
/* 0572 */  "", //"<insert> ",
/* 0573 */  "", //"<read ",
/* 0574 */  "", //"l.",
/* 0575 */  "", //"<argument> ",
/* 0576 */  "", //"<template> ",
/* 0577 */  "", //"<recently read> ",
/* 0578 */  "", //"<to be read again> ",
/* 0579 */  "", //"<inserted text> ",
/* 0580 */  "", //"<output> ",
/* 0581 */  "", //"<everypar> ",
/* 0582 */  "", //"<everymath> ",
/* 0583 */  "", //"<everydisplay> ",
/* 0584 */  "", //"<everyhbox> ",
/* 0585 */  "", //"<everyvbox> ",
/* 0586 */  "", //"<everyjob> ",
/* 0587 */  "", //"<everycr> ",
/* 0588 */  "", //"<mark> ",
/* 0589 */  "", //"<write> ",
/* 0590 */  "", //"input stack size",
/* 0591 */  "", //"write",
/* 0592 */  "", //"(interwoven alignment preambles are not allowed)",
/* 0593 */  "", //"text input levels",
/* 0594 */  "", //"par",
/* 0595 */  "", //"Incomplete ",
/* 0596 */  "", //"; all text was ignored after line ",
/* 0597 */  "", //"A forbidden control sequence occurred in skipped text.",
/* 0598 */  "", //"This kind of error happens when you say `\\if...' and forget",
/* 0599 */  "", //"the matching `\\fi'. I've inserted a `\\fi'; this might work.",
/* 0600 */  "", //"The file ended while I was skipping conditional text.",
/* 0601 */  "", //"File ended",
/* 0602 */  "", //"Forbidden control sequence found",
/* 0603 */  "", //" while scanning ",
/* 0604 */  "", //" of ",
/* 0605 */  "", //"I suspect you have forgotten a `}', causing me",
/* 0606 */  "", //"to read past where you wanted me to stop.",
/* 0607 */  "", //"I'll try to recover; but if the error is serious,",
/* 0608 */  "", //"you'd better type `E' or `X' now and fix your file.",
/* 0609 */  "", //"use",
/* 0610 */  "", //"Text line contains an invalid character",
/* 0611 */  "", //"A funny symbol that I can't read has just been input.",
/* 0612 */  "", //"Continue, and I'll forget that it ever happened.",
/* 0613 */  "", //"(Please type a command or say `\\end')",
/* 0614 */  "", //"*** (job aborted, no legal \\end found)",
/* 0615 */  "", //"=>",
/* 0616 */  "", //"Undefined control sequence",
/* 0617 */  "", //"The control sequence at the end of the top line",
/* 0618 */  "", //"of your error message was never \\def'ed. If you have",
/* 0619 */  "", //"misspelled it (e.g., `\\hobx'), type `I' and the correct",
/* 0620 */  "", //"spelling (e.g., `I\\hbox'). Otherwise just continue,",
/* 0621 */  "", //"and I'll forget about whatever was undefined.",
/* 0622 */  "", //"Missing ",
/* 0623 */  "", //" inserted",
/* 0624 */  "", //"The control sequence marked <to be read again> should",
/* 0625 */  "", //"not appear between \\csname and \\endcsname.",
/* 0626 */  "", //"input",
/* 0627 */  "", //"endinput",
/* 0628 */  "", //"topmark",
/* 0629 */  "", //"firstmark",
/* 0630 */  "", //"botmark",
/* 0631 */  "", //"splitfirstmark",
/* 0632 */  "", //"splitbotmark",
/* 0633 */  "", //"parameter stack size",
/* 0634 */  "", //"Argument of ",
/* 0635 */  "", //" has an extra }",
/* 0636 */  "", //"I've run across a `}' that doesn't seem to match anything.",
/* 0637 */  "", //"For example, `\\def\\a#1{...}' and `\\a}' would produce",
/* 0638 */  "", //"this error. If you simply proceed now, the `\\par' that",
/* 0639 */  "", //"I've just inserted will cause me to report a runaway",
/* 0640 */  "", //"argument that might be the root of the problem. But if",
/* 0641 */  "", //"your `}' was spurious, just type `2' and it will go away.",
/* 0642 */  "", //"Paragraph ended before ",
/* 0643 */  "", //" was complete",
/* 0644 */  "", //"I suspect you've forgotten a `}', causing me to apply this",
/* 0645 */  "", //"control sequence to too much text. How can we recover" "?",
/* 0646 */  "", //"My plan is to forget the whole thing and hope for the best.",
/* 0647 */  "", //"Use of ",
/* 0648 */  "", //" doesn't match its definition",
/* 0649 */  "", //"If you say, e.g., `\\def\\a1{...}', then you must always",
/* 0650 */  "", //"put `1' after `\\a', since control sequence names are",
/* 0651 */  "", //"made up of letters only. The macro here has not been",
/* 0652 */  "", //"followed by the required stuff, so I'm ignoring it.",
/* 0653 */  "", //"<-",
/* 0654 */  "", //"Missing { inserted",
/* 0655 */  "", //"A left brace was mandatory here, so I've put one in.",
/* 0656 */  "", //"You might want to delete and/or insert some corrections",
/* 0657 */  "", //"so that I will find a matching right brace soon.",
/* 0658 */  "", //"(If you're confused by all this, try typing `I}' now.)",
/* 0659 */  "", //"Incompatible glue units",
/* 0660 */  "", //"I'm going to assume that 1mu=1pt when they're mixed.",
/* 0661 */  "", //"Missing number, treated as zero",
/* 0662 */  "", //"A number should have been here; I inserted `0'.",
/* 0663 */  "", //"(If you can't figure out why I needed to see a number,",
/* 0664 */  "", //"look up `weird error' in the index to The TeXbook.)",
/* 0665 */  "", //"spacefactor",
/* 0666 */  "", //"prevdepth",
/* 0667 */  "", //"deadcycles",
/* 0668 */  "", //"insertpenalties",
/* 0669 */  "", //"wd",
/* 0670 */  "", //"ht",
/* 0671 */  "", //"dp",
/* 0672 */  "", //"lastpenalty",
/* 0673 */  "", //"lastkern",
/* 0674 */  "", //"lastskip",
/* 0675 */  "", //"inputlineno",
/* 0676 */  "", //"badness",
/* 0677 */  "", //"Improper ",
/* 0678 */  "", //"You can refer to \\spacefactor only in horizontal mode;",
/* 0679 */  "", //"you can refer to \\prevdepth only in vertical mode; and",
/* 0680 */  "", //"neither of these is meaningful inside \\write. So",
/* 0681 */  "", //"I'm forgetting what you said and using zero instead.",
/* 0682 */  "", //"You can't use `",
/* 0683 */  "", //"' after ",
/* 0684 */  "", //"Bad register code",
/* 0685 */  "", //"A register number must be between 0 and 255.",
/* 0686 */  "", //"I changed this one to zero.",
/* 0687 */  "", //"Bad character code",
/* 0688 */  "", //"A character number must be between 0 and 255.",
/* 0689 */  "", //"Bad number",
/* 0690 */  "", //"Since I expected to read a number between 0 and 15,",
/* 0691 */  "", //"Bad mathchar",
/* 0692 */  "", //"A mathchar number must be between 0 and 32767.",
/* 0693 */  "", //"Bad delimiter code",
/* 0694 */  "", //"A numeric delimiter code must be between 0 and 2^{27}-1.",
/* 0695 */  "", //"Improper alphabetic constant",
/* 0696 */  "", //"A one-character control sequence belongs after a ` mark.",
/* 0697 */  "", //"So I'm essentially inserting \\0 here.",
/* 0698 */  "", //"Number too big",
/* 0699 */  "", //"I can only go up to 2147483647='17777777777=\"7FFFFFFF,",
/* 0700 */  "", //"so I'm using that number instead of yours.",
/* 0701 */  "", //"true",
/* 0702 */  "", //"Illegal unit of measure (",
/* 0703 */  "", //"replaced by filll)",
/* 0704 */  "", //"I dddon't go any higher than filll.",
/* 0705 */  "", //"em",
/* 0706 */  "", //"ex",
/* 0707 */  "", //"mu inserted)",
/* 0708 */  "", //"The unit of measurement in math glue must be mu.",
/* 0709 */  "", //"To recover gracefully from this error, it's best to",
/* 0710 */  "", //"delete the erroneous units; e.g., type `2' to delete",
/* 0711 */  "", //"two letters. (See Chapter 27 of The TeXbook.)",
/* 0712 */  "", //"in",
/* 0713 */  "", //"pc",
/* 0714 */  "", //"cm",
/* 0715 */  "", //"mm",
/* 0716 */  "", //"bp",
/* 0717 */  "", //"dd",
/* 0718 */  "", //"cc",
/* 0719 */  "", //"sp",
/* 0720 */  "", //"pt inserted)",
/* 0721 */  "", //"Dimensions can be in units of em, ex, in, pt, pc,",
/* 0722 */  "", //"cm, mm, dd, cc, bp, or sp; but yours is a new one!",
/* 0723 */  "", //"I'll assume that you meant to say pt, for printer's points.",
/* 0724 */  "", //"Dimension too large",
/* 0725 */  "", //"I can't work with sizes bigger than about 19 feet.",
/* 0726 */  "", //"Continue and I'll use the largest value I can.",
/* 0727 */  "", //"plus",
/* 0728 */  "", //"minus",
/* 0729 */  "", //"width",
/* 0730 */  "", //"height",
/* 0731 */  "", //"depth",
/* 0732 */  "", //"number",
/* 0733 */  "", //"romannumeral",
/* 0734 */  "", //"string",
/* 0735 */  "", //"meaning",
/* 0736 */  "", //"fontname",
/* 0737 */  "", //"jobname",
/* 0738 */  "", //" at ",
/* 0739 */  "", //"Where was the left brace" "? You said something like `\\def\\a}',",
/* 0740 */  "", //"which I'm going to interpret as `\\def\\a{}'.",
/* 0741 */  "", //"You already have nine parameters",
/* 0742 */  "", //"I'm going to ignore the # sign you just used.",
/* 0743 */  "", //"Parameters must be numbered consecutively",
/* 0744 */  "", //"I've inserted the digit you should have used after the #.",
/* 0745 */  "", //"Type `1' to delete what you did use.",
/* 0746 */  "", //"Illegal parameter number in definition of ",
/* 0747 */  "", //"You meant to type ## instead of #, right" "?",
/* 0748 */  "", //"Or maybe a } was forgotten somewhere earlier, and things",
/* 0749 */  "", //"are all screwed up" "? I'm going to assume that you meant ##.",
/* 0750 */  "", //"*** (cannot \\read from terminal in nonstop modes)",
/* 0751 */  "", //"File ended within ",
/* 0752 */  "", //"This \\read has unbalanced braces.",
/* 0753 */  "", //"if",
/* 0754 */  "", //"ifcat",
/* 0755 */  "", //"ifnum",
/* 0756 */  "", //"ifdim",
/* 0757 */  "", //"ifodd",
/* 0758 */  "", //"ifvmode",
/* 0759 */  "", //"ifhmode",
/* 0760 */  "", //"ifmmode",
/* 0761 */  "", //"ifinner",
/* 0762 */  "", //"ifvoid",
/* 0763 */  "", //"ifhbox",
/* 0764 */  "", //"ifvbox",
/* 0765 */  "", //"ifx",
/* 0766 */  "", //"ifeof",
/* 0767 */  "", //"iftrue",
/* 0768 */  "", //"iffalse",
/* 0769 */  "", //"ifcase",
/* 0770 */  "", //"fi",
/* 0771 */  "", //"or",
/* 0772 */  "", //"else",
/* 0773 */  "", //"Extra ",
/* 0774 */  "", //"I'm ignoring this; it doesn't match any \\if.",
/* 0775 */  "", //"{true}",
/* 0776 */  "", //"{false}",
/* 0777 */  "", //"Missing = inserted for ",
/* 0778 */  "", //"I was expecting to see `<', `=', or `>'. Didn't.",
/* 0779 */  "", //"{case ",
/* 0780 */  "", //".fmt",
/* 0781 */  "", //"input file name",
/* 0782 */  "", //"I can't find file `",
/* 0783 */  "", //"I can't write on file `",
/* 0784 */  "", //"'.",
/* 0785 */  ".tex",
/* 0786 */  "", //"Please type another ",
/* 0787 */  "", //"*** (job aborted, file error in nonstop mode)",
/* 0788 */  "", //".dvi",
/* 0789 */  "", //"file name for output",
/* 0790 */  "texput",
/* 0791 */  "", //".log",
/* 0792 */  "", //"**",
/* 0793 */  "", //"transcript file name",
/* 0794 */  "  ",
/* 0795 */  "nullfont",
/* 0796 */  "", //"Font ",
/* 0797 */  "", //" scaled ",
/* 0798 */  "", //" not loadable: Bad metric (TFM) file",
/* 0799 */  "", //" not loadable: Metric (TFM) file not found",
/* 0800 */  "", //"I wasn't able to read the size data for this font,",
/* 0801 */  "", //"so I will ignore the font specification.",
/* 0802 */  "", //"[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
/* 0803 */  "", //"You might try inserting a different font spec;",
/* 0804 */  "", //"e.g., type `I\\font<same font id>=<substitute font name>'.",
/* 0805 */  ".tfm",
/* 0806 */  "", //" not loaded: Not enough room left",
/* 0807 */  "", //"I'm afraid I won't be able to make use of this font,",
/* 0808 */  "", //"because my memory for character-size data is too small.",
/* 0809 */  "", //"If you're really stuck, ask a wizard to enlarge me.",
/* 0810 */  "", //"Or maybe try `I\\font<same font id>=<name of loaded font>'.",
/* 0811 */  "", //"Missing font identifier",
/* 0812 */  "", //"I was looking for a control sequence whose",
/* 0813 */  "", //"current meaning has been defined by \\font.",
/* 0814 */  "", //" has only ",
/* 0815 */  "", //" fontdimen parameters",
/* 0816 */  "", //"To increase the number of font parameters, you must",
/* 0817 */  "", //"use \\fontdimen immediately after the \\font is loaded.",
/* 0818 */  "", //"font memory",
/* 0819 */  "", //"Missing character: There is no ",
/* 0820 */  "", //" in font ",
/* 0821 */  "", //" TeX output ",
/* 0822 */  "", //"vlistout",
/* 0823 */  "", //"Completed box being shipped out",
/* 0824 */  "", //"Memory usage before: ",
/* 0825 */  "", //" after: ",
/* 0826 */  "", //"; still untouched: ",
/* 0827 */  "", //"Huge page cannot be shipped out",
/* 0828 */  "", //"The page just created is more than 18 feet tall or",
/* 0829 */  "", //"more than 18 feet wide, so I suspect something went wrong.",
/* 0830 */  "", //"The following box has been deleted:",
/* 0831 */  "", //"No pages of output.",
/* 0832 */  "", //"Output written on ",
/* 0833 */  "", //" page",
/* 0834 */  "", //", ",
/* 0835 */  "", //" bytes).",
/* 0836 */  "", //"to",
/* 0837 */  "", //"spread",
/* 0838 */  "", //"Underfull",
/* 0839 */  "", //"Loose",
/* 0840 */  "", //" \\hbox (badness ",
/* 0841 */  "", //") has occurred while \\output is active",
/* 0842 */  "", //") in paragraph at lines ",
/* 0843 */  "", //") in alignment at lines ",
/* 0844 */  "", //"--",
/* 0845 */  "", //") detected at line ",
/* 0846 */  "", //"Overfull \\hbox (",
/* 0847 */  "", //"pt too wide",
/* 0848 */  "", //"Tight \\hbox (badness ",
/* 0849 */  "", //"vpack",
/* 0850 */  "", //" \\vbox (badness ",
/* 0851 */  "", //"Overfull \\vbox (",
/* 0852 */  "", //"pt too high",
/* 0853 */  "", //"Tight \\vbox (badness ",
/* 0854 */  "", //"{}",
/* 0855 */  "", //"displaystyle",
/* 0856 */  "", //"textstyle",
/* 0857 */  "", //"scriptstyle",
/* 0858 */  "", //"scriptscriptstyle",
/* 0859 */  "", //"Unknown style!",
/* 0860 */  "", //"mathord",
/* 0861 */  "", //"mathop",
/* 0862 */  "", //"mathbin",
/* 0863 */  "", //"mathrel",
/* 0864 */  "", //"mathopen",
/* 0865 */  "", //"mathclose",
/* 0866 */  "", //"mathpunct",
/* 0867 */  "", //"mathinner",
/* 0868 */  "", //"overline",
/* 0869 */  "", //"underline",
/* 0870 */  "", //"left",
/* 0871 */  "", //"right",
/* 0872 */  "", //"limits",
/* 0873 */  "", //"nolimits",
/* 0874 */  "", //"fraction, thickness ",
/* 0875 */  "", //"= default",
/* 0876 */  "", //", left-delimiter ",
/* 0877 */  "", //", right-delimiter ",
/* 0878 */  "", //" is undefined (character ",
/* 0879 */  "", //"Somewhere in the math formula just ended, you used the",
/* 0880 */  "", //"stated character from an undefined font family. For example,",
/* 0881 */  "", //"plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,",
/* 0882 */  "", //"and I'll try to forget that I needed that character.",
/* 0883 */  "", //"mlist1",
/* 0884 */  "", //"mlist2",
/* 0885 */  "", //"mlist3",
/* 0886 */  "0234000122*4000133**3**344*0400400*000000234000111*1111112341011",
/* 0887 */  "", //"mlist4",
/* 0888 */  "", //" inside $$'s",
/* 0889 */  "", //"Displays can use special alignments (like \\eqalignno)",
/* 0890 */  "", //"only if nothing but the alignment itself is between $$'s.",
/* 0891 */  "", //"So I've deleted the formulas that preceded this alignment.",
/* 0892 */  "", //"span",
/* 0893 */  "", //"cr",
/* 0894 */  "", //"crcr",
/* 0895 */  "endtemplate",
/* 0896 */  "", //"alignment tab character ",
/* 0897 */  "", //"Missing # inserted in alignment preamble",
/* 0898 */  "", //"There should be exactly one # between &'s, when an",
/* 0899 */  "", //"\\halign or \\valign is being set up. In this case you had",
/* 0900 */  "", //"none, so I've put one in; maybe that will work.",
/* 0901 */  "", //"Only one # is allowed per tab",
/* 0902 */  "", //"more than one, so I'm ignoring all but the first.",
/* 0903 */  "", //"endv",
/* 0904 */  "", //"Extra alignment tab has been changed to ",
/* 0905 */  "", //"You have given more \\span or & marks than there were",
/* 0906 */  "", //"in the preamble to the \\halign or \\valign now in progress.",
/* 0907 */  "", //"So I'll assume that you meant to type \\cr instead.",
/* 0908 */  "", //"256 spans",
/* 0909 */  "", //"align1",
/* 0910 */  "", //"align0",
/* 0911 */  "", //"Infinite glue shrinkage found in a paragraph",
/* 0912 */  "", //"The paragraph just ended includes some glue that has",
/* 0913 */  "", //"infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.",
/* 0914 */  "", //"Such glue doesn't belong there---it allows a paragraph",
/* 0915 */  "", //"of any length to fit on one line. But it's safe to proceed,",
/* 0916 */  "", //"since the offensive shrinkability has been made finite.",
/* 0917 */  "", //"disc1",
/* 0918 */  "", //"disc2",
/* 0919 */  "", //"@@",
/* 0920 */  "", //": line ",
/* 0921 */  "", //" t=",
/* 0922 */  "", //" -> @@",
/* 0923 */  "", //" via @@",
/* 0924 */  "", //" b=",
/* 0925 */  "", //" p=",
/* 0926 */  "", //" d=",
/* 0927 */  "", //"@firstpass",
/* 0928 */  "", //"@secondpass",
/* 0929 */  "", //"@emergencypass",
/* 0930 */  "", //"paragraph",
/* 0931 */  "", //"disc3",
/* 0932 */  "", //"disc4",
/* 0933 */  "", //"line breaking",
/* 0934 */  "", //"HYPH(",
/* 0935 */  "", //"hyphenation",
/* 0936 */  "", //" will be flushed",
/* 0937 */  "", //"Hyphenation exceptions must contain only letters",
/* 0938 */  "", //"and hyphens. But continue; I'll forgive and forget.",
/* 0939 */  "", //"Not a letter",
/* 0940 */  "", //"Letters in \\hyphenation words must have \\lccode>0.",
/* 0941 */  "", //"Proceed; I'll ignore the character I just read.",
/* 0942 */  "", //"exception dictionary",
/* 0943 */  "", //"pattern memory ops",
/* 0944 */  "", //"pattern memory ops per language",
/* 0945 */  "", //"pattern memory",
/* 0946 */  "", //"Too late for ",
/* 0947 */  "", //"patterns",
/* 0948 */  "", //"All patterns must be given before typesetting begins.",
/* 0949 */  "", //"Bad ",
/* 0950 */  "", //"(See Appendix H.)",
/* 0951 */  "", //"Nonletter",
/* 0952 */  "", //"Duplicate pattern",
/* 0953 */  "", //"pruning",
/* 0954 */  "", //"vertbreak",
/* 0955 */  "", //"Infinite glue shrinkage found in box being split",
/* 0956 */  "", //"The box you are \\vsplitting contains some infinitely",
/* 0957 */  "", //"shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
/* 0958 */  "", //"Such glue doesn't belong there; but you can safely proceed,",
/* 0959 */  "", //"vsplit",
/* 0960 */  "", //" needs a ",
/* 0961 */  "", //"vbox",
/* 0962 */  "", //"The box you are trying to split is an \\hbox.",
/* 0963 */  "", //"I can't split such a box, so I'll leave it alone.",
/* 0964 */  "", //"pagegoal",
/* 0965 */  "", //"pagetotal",
/* 0966 */  "", //"pagestretch",
/* 0967 */  "", //"pagefilstretch",
/* 0968 */  "", //"pagefillstretch",
/* 0969 */  "", //"pagefilllstretch",
/* 0970 */  "", //"pageshrink",
/* 0971 */  "", //"pagedepth",
/* 0972 */  "", //"fill",
/* 0973 */  "", //"filll",
/* 0974 */  "", //"### current page:",
/* 0975 */  "", //" (held over for next output)",
/* 0976 */  "", //"total height ",
/* 0977 */  "", //" goal height ",
/* 0978 */  "", //" adds ",
/* 0979 */  "", //", #",
/* 0980 */  "", //" might split",
/* 0981 */  "", //"%% goal height=",
/* 0982 */  "", //", max depth=",
/* 0983 */  "", //"Insertions can only be added to a vbox",
/* 0984 */  "", //"Tut tut: You're trying to \\insert into a",
/* 0985 */  "", //"\\box register that now contains an \\hbox.",
/* 0986 */  "", //"Proceed, and I'll discard its present contents.",
/* 0987 */  "", //"page",
/* 0988 */  "", //"Infinite glue shrinkage found on current page",
/* 0989 */  "", //"The page about to be output contains some infinitely",
/* 0990 */  "", //" g=",
/* 0991 */  "", //" c=",
/* 0992 */  "", //"Infinite glue shrinkage inserted from ",
/* 0993 */  "", //"The correction glue for page breaking with insertions",
/* 0994 */  "", //"must have finite shrinkability. But you may proceed,",
/* 0995 */  "", //"% split",
/* 0996 */  "", //" to ",
/* 0997 */  "", //"255 is not void",
/* 0998 */  "", //"You shouldn't use \\box255 except in \\output routines.",
/* 0999 */  "", //"Output loop---",
/* 1000 */  "", //" consecutive dead cycles",
/* 1001 */  "", //"I've concluded that your \\output is awry; it never does a",
/* 1002 */  "", //"\\shipout, so I'm shipping \\box255 out myself. Next time",
/* 1003 */  "", //"increase \\maxdeadcycles if you want me to be more patient!",
/* 1004 */  "", //"Unbalanced output routine",
/* 1005 */  "", //"Your sneaky output routine has problematic {'s and/or }'s.",
/* 1006 */  "", //"I can't handle that very well; good luck.",
/* 1007 */  "", //"Output routine didn't use all of ",
/* 1008 */  "", //"Your \\output commands should empty \\box255,",
/* 1009 */  "", //"e.g., by saying `\\shipout\\box255'.",
/* 1010 */  "", //"Proceed; I'll discard its present contents.",
/* 1011 */  "", //"Missing $ inserted",
/* 1012 */  "", //"I've inserted a begin-math/end-math symbol since I think",
/* 1013 */  "", //"you left one out. Proceed, with fingers crossed.",
/* 1014 */  "", //"' in ",
/* 1015 */  "", //"Sorry, but I'm not programmed to handle this case;",
/* 1016 */  "", //"I'll just pretend that you didn't ask for it.",
/* 1017 */  "", //"If you're in the wrong mode, you might be able to",
/* 1018 */  "", //"return to the right one by typing `I}' or `I$' or `I\\par'.",
/* 1019 */  "", //"end",
/* 1020 */  "", //"dump",
/* 1021 */  "", //"hskip",
/* 1022 */  "", //"hfil",
/* 1023 */  "", //"hfill",
/* 1024 */  "", //"hss",
/* 1025 */  "", //"hfilneg",
/* 1026 */  "", //"vskip",
/* 1027 */  "", //"vfil",
/* 1028 */  "", //"vfill",
/* 1029 */  "", //"vss",
/* 1030 */  "", //"vfilneg",
/* 1031 */  "", //"I've inserted something that you may have forgotten.",
/* 1032 */  "", //"(See the <inserted text> above.)",
/* 1033 */  "", //"With luck, this will get me unwedged. But if you",
/* 1034 */  "", //"really didn't forget anything, try typing `2' now; then",
/* 1035 */  "", //"my insertion and my current dilemma will both disappear.",
/* 1036 */  "", //"right.",
/* 1037 */  "", //"Things are pretty mixed up, but I think the worst is over.",
/* 1038 */  "", //"Too many }'s",
/* 1039 */  "", //"You've closed more groups than you opened.",
/* 1040 */  "", //"Such booboos are generally harmless, so keep going.",
/* 1041 */  "", //"rightbrace",
/* 1042 */  "", //"Extra }, or forgotten ",
/* 1043 */  "", //"I've deleted a group-closing symbol because it seems to be",
/* 1044 */  "", //"spurious, as in `$x}$'. But perhaps the } is legitimate and",
/* 1045 */  "", //"you forgot something else, as in `\\hbox{$x}'. In such cases",
/* 1046 */  "", //"the way to recover is to insert both the forgotten and the",
/* 1047 */  "", //"deleted material, e.g., by typing `I$}'.",
/* 1048 */  "", //"moveleft",
/* 1049 */  "", //"moveright",
/* 1050 */  "", //"raise",
/* 1051 */  "", //"lower",
/* 1052 */  "", //"copy",
/* 1053 */  "", //"lastbox",
/* 1054 */  "", //"vtop",
/* 1055 */  "", //"hbox",
/* 1056 */  "", //"shipout",
/* 1057 */  "", //"leaders",
/* 1058 */  "", //"cleaders",
/* 1059 */  "", //"xleaders",
/* 1060 */  "", //"Leaders not followed by proper glue",
/* 1061 */  "", //"You should say `\\leaders <box or rule><hskip or vskip>'.",
/* 1062 */  "", //"I found the <box or rule>, but there's no suitable",
/* 1063 */  "", //"<hskip or vskip>, so I'm ignoring these leaders.",
/* 1064 */  "", //"Sorry; this \\lastbox will be void.",
/* 1065 */  "", //"Sorry...I usually can't take things from the current page.",
/* 1066 */  "", //"This \\lastbox will therefore be void.",
/* 1067 */  "", //"Missing `to' inserted",
/* 1068 */  "", //"I'm working on `\\vsplit<box number> to <dimen>';",
/* 1069 */  "", //"will look for the <dimen> next.",
/* 1070 */  "", //"A <box> was supposed to be here",
/* 1071 */  "", //"I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
/* 1072 */  "", //"something like that. So you might find something missing in",
/* 1073 */  "", //"your output. But keep trying; you can fix this later.",
/* 1074 */  "", //"indent",
/* 1075 */  "", //"noindent",
/* 1076 */  "", //"' here except with leaders",
/* 1077 */  "", //"To put a horizontal rule in an hbox or an alignment,",
/* 1078 */  "", //"you should use \\leaders or \\hrulefill (see The TeXbook).",
/* 1079 */  "", //"You can't ",
/* 1080 */  "", //"I'm changing to \\insert0; box 255 is special.",
/* 1081 */  "", //"Try `I\\vskip-\\lastskip' instead.",
/* 1082 */  "", //"Try `I\\kern-\\lastkern' instead.",
/* 1083 */  "", //"Perhaps you can make the output routine do it.",
/* 1084 */  "", //"unpenalty",
/* 1085 */  "", //"unkern",
/* 1086 */  "", //"unskip",
/* 1087 */  "", //"unhbox",
/* 1088 */  "", //"unhcopy",
/* 1089 */  "", //"unvbox",
/* 1090 */  "", //"unvcopy",
/* 1091 */  "", //"Incompatible list can't be unboxed",
/* 1092 */  "", //"Sorry, Pandora. (You sneaky devil.)",
/* 1093 */  "", //"I refuse to unbox an \\hbox in vertical mode or vice versa.",
/* 1094 */  "", //"And I can't open any boxes in math mode.",
/* 1095 */  "", //"Illegal math ",
/* 1096 */  "", //"Sorry: The third part of a discretionary break must be",
/* 1097 */  "", //"empty, in math formulas. I had to delete your third part.",
/* 1098 */  "", //"Discretionary list is too long",
/* 1099 */  "", //"Wow---I never thought anybody would tweak me here.",
/* 1100 */  "", //"You can't seriously need such a huge discretionary list" "?",
/* 1101 */  "", //"Improper discretionary list",
/* 1102 */  "", //"Discretionary lists must contain only boxes and kerns.",
/* 1103 */  "", //"The following discretionary sublist has been deleted:",
/* 1104 */  "", //"Missing } inserted",
/* 1105 */  "", //"I've put in what seems to be necessary to fix",
/* 1106 */  "", //"the current column of the current alignment.",
/* 1107 */  "", //"Try to go on, since this might almost work.",
/* 1108 */  "", //"Misplaced ",
/* 1109 */  "", //"I can't figure out why you would want to use a tab mark",
/* 1110 */  "", //"here. If you just want an ampersand, the remedy is",
/* 1111 */  "", //"simple: Just type `I\\&' now. But if some right brace",
/* 1112 */  "", //"up above has ended a previous alignment prematurely,",
/* 1113 */  "", //"you're probably due for more error messages, and you",
/* 1114 */  "", //"might try typing `S' now just to see what is salvageable.",
/* 1115 */  "", //"or \\cr or \\span just now. If something like a right brace",
/* 1116 */  "", //"I expect to see \\noalign only after the \\cr of",
/* 1117 */  "", //"an alignment. Proceed, and I'll ignore this case.",
/* 1118 */  "", //"I expect to see \\omit only after tab marks or the \\cr of",
/* 1119 */  "", // "I'm guessing that you meant to end an alignment here.",
/* 1120 */  "", //"I'm ignoring this, since I wasn't doing a \\csname.",
/* 1121 */  "", //"eqno",
/* 1122 */  "", //"leqno",
/* 1123 */  "", //"displaylimits",
/* 1124 */  "", //"Limit controls must follow a math operator",
/* 1125 */  "", //"I'm ignoring this misplaced \\limits or \\nolimits command.",
/* 1126 */  "", //"Missing delimiter (. inserted)",
/* 1127 */  "", //"I was expecting to see something like `(' or `\\{' or",
/* 1128 */  "", //"`\\}' here. If you typed, e.g., `{' instead of `\\{', you",
/* 1129 */  "", //"should probably delete the `{' by typing `1' now, so that",
/* 1130 */  "", //"braces don't get unbalanced. Otherwise just proceed.",
/* 1131 */  "", //"Acceptable delimiters are characters whose \\delcode is",
/* 1132 */  "", //"nonnegative, or you can use `\\delimiter <delimiter code>'.",
/* 1133 */  "", //"Please use ",
/* 1134 */  "", //" for accents in math mode",
/* 1135 */  "", //"I'm changing \\accent to \\mathaccent here; wish me luck.",
/* 1136 */  "", //"(Accents are not the same in formulas as they are in text.)",
/* 1137 */  "", //"Double superscript",
/* 1138 */  "", //"I treat `x^1^2' essentially like `x^1{}^2'.",
/* 1139 */  "", //"Double subscript",
/* 1140 */  "", //"I treat `x_1_2' essentially like `x_1{}_2'.",
/* 1141 */  "", //"above",
/* 1142 */  "", //"over",
/* 1143 */  "", //"atop",
/* 1144 */  "", //"abovewithdelims",
/* 1145 */  "", //"overwithdelims",
/* 1146 */  "", //"atopwithdelims",
/* 1147 */  "", //"Ambiguous; you need another { and }",
/* 1148 */  "", //"I'm ignoring this fraction specification, since I don't",
/* 1149 */  "", //"know whether a construction like `x \\over y \\over z'",
/* 1150 */  "", //"means `{x \\over y} \\over z' or `x \\over {y \\over z}'.",
/* 1151 */  "", //"I'm ignoring a \\right that had no matching \\left.",
/* 1152 */  "", //"Math formula deleted: Insufficient symbol fonts",
/* 1153 */  "", //"Sorry, but I can't typeset math unless \\textfont 2",
/* 1154 */  "", //"and \\scriptfont 2 and \\scriptscriptfont 2 have all",
/* 1155 */  "", //"the \\fontdimen values needed in math symbol fonts.",
/* 1156 */  "", //"Math formula deleted: Insufficient extension fonts",
/* 1157 */  "", //"Sorry, but I can't typeset math unless \\textfont 3",
/* 1158 */  "", //"and \\scriptfont 3 and \\scriptscriptfont 3 have all",
/* 1159 */  "", //"the \\fontdimen values needed in math extension fonts.",
/* 1160 */  "", //"Display math should end with $$",
/* 1161 */  "", //"The `$' that I just saw supposedly matches a previous `$$'.",
/* 1162 */  "", //"So I shall assume that you typed `$$' both times.",
/* 1163 */  "", //"display",
/* 1164 */  "", //"Missing $$ inserted",
/* 1165 */  "", //"long",
/* 1166 */  "", //"outer",
/* 1167 */  "", //"global",
/* 1168 */  "", //"def",
/* 1169 */  "", //"gdef",
/* 1170 */  "", //"edef",
/* 1171 */  "", //"xdef",
/* 1172 */  "", //"prefix",
/* 1173 */  "", //"You can't use a prefix with `",
/* 1174 */  "", //"I'll pretend you didn't say \\long or \\outer or \\global.",
/* 1175 */  "", //"' or `",
/* 1176 */  "", //"' with `",
/* 1177 */  "", //"I'll pretend you didn't say \\long or \\outer here.",
/* 1178 */  "", //"Missing control sequence inserted",
/* 1179 */  "", //"Please don't say `\\def cs{...}', say `\\def\\cs{...}'.",
/* 1180 */  "", //"I've inserted an inaccessible control sequence so that your",
/* 1181 */  "", //"definition will be completed without mixing me up too badly.",
/* 1182 */  "", //"You can recover graciously from this error, if you're",
/* 1183 */  "", //"careful; see exercise 27.2 in The TeXbook.",
/* 1184 */  "inaccessible",
/* 1185 */  "", //"let",
/* 1186 */  "", //"futurelet",
/* 1187 */  "", //"chardef",
/* 1188 */  "", //"mathchardef",
/* 1189 */  "", //"countdef",
/* 1190 */  "", //"dimendef",
/* 1191 */  "", //"skipdef",
/* 1192 */  "", //"muskipdef",
/* 1193 */  "", //"toksdef",
/* 1194 */  "", //"You should have said `\\read<number> to \\cs'.",
/* 1195 */  "", //"I'm going to look for the \\cs now.",
/* 1196 */  "", //"Invalid code (",
/* 1197 */  "", //"), should be in the range 0..",
/* 1198 */  "", //"), should be at most ",
/* 1199 */  "", //"I'm going to use 0 instead of that illegal code value.",
/* 1200 */  "", //"by",
/* 1201 */  "", //"Arithmetic overflow",
/* 1202 */  "", //"I can't carry out that multiplication or division,",
/* 1203 */  "", //"since the result is out of range.",
/* 1204 */  "", //"I'm forgetting what you said and not changing anything.",
/* 1205 */  "", //"Sorry, \\setbox is not allowed after \\halign in a display,",
/* 1206 */  "", //"or between \\accent and an accented character.",
/* 1207 */  "", //"Bad space factor",
/* 1208 */  "", //"I allow only values in the range 1..32767 here.",
/* 1209 */  "", //"I allow only nonnegative values here.",
/* 1210 */  "", //"Patterns can be loaded only by INITEX",
/* 1211 */  "", //"hyphenchar",
/* 1212 */  "", //"skewchar",
/* 1213 */  "FONT",
/* 1214 */  "", //"at",
/* 1215 */  "", //"scaled",
/* 1216 */  "", //"Improper `at' size (",
/* 1217 */  "", //"pt), replaced by 10pt",
/* 1218 */  "", //"I can only handle fonts at positive sizes that are",
/* 1219 */  "", //"less than 2048pt, so I've changed what you said to 10pt.",
/* 1220 */  "", //"select font ",
/* 1221 */  "", //"errorstopmode",
/* 1222 */  "", //"openin",
/* 1223 */  "", //"closein",
/* 1224 */  "", //"message",
/* 1225 */  "", //"errmessage",
/* 1226 */  "", //"(That was another \\errmessage.)",
/* 1227 */  "", //"This error message was generated by an \\errmessage",
/* 1228 */  "", //"command, so I can't give any explicit help.",
/* 1229 */  "", //"Pretend that you're Hercule Poirot: Examine all clues,",
/* 1230 */  "", //"and deduce the truth by order and method.",
/* 1231 */  "", //"lowercase",
/* 1232 */  "", //"uppercase",
/* 1233 */  "", //"show",
/* 1234 */  "", //"showbox",
/* 1235 */  "", //"showthe",
/* 1236 */  "", //"showlists",
/* 1237 */  "", //"This isn't an error message; I'm just \\showing something.",
/* 1238 */  "", //"Type `I\\show...' to show more (e.g., \\show\\cs,",
/* 1239 */  "", //"\\showthe\\count10, \\showbox255, \\showlists).",
/* 1240 */  "", //"And type `I\\tracingonline=1\\show...' to show boxes and",
/* 1241 */  "", //"lists on your terminal as well as in the transcript file.",
/* 1242 */  "", //"> ",
/* 1243 */  "", //"undefined",
/* 1244 */  "", //"macro",
/* 1245 */  "", //"long macro",
/* 1246 */  "", //"outer macro",
/* 1247 */  "", //"outer endtemplate",
/* 1248 */  "", //"> \\box",
/* 1249 */  "", //"OK",
/* 1250 */  "", //" (see the transcript file)",
/* 1251 */  " (INITEX)",
/* 1252 */  "", //"You can't dump inside a group",
/* 1253 */  "", //"`{...\\dump}' is a no-no.",
/* 1254 */  "", //" strings of total length ",
/* 1255 */  "", //" memory locations dumped; current usage is ",
/* 1256 */  "", //" multiletter control sequences",
/* 1257 */  "", //" words of font info for ",
/* 1258 */  "", //" preloaded font",
/* 1259 */  "", //"\\font",
/* 1260 */  "", //" hyphenation exception",
/* 1261 */  "", //"Hyphenation trie of length ",
/* 1262 */  "", //" has ",
/* 1263 */  "", //" op",
/* 1264 */  "", //" out of ",
/* 1265 */  "", //" for language ",
/* 1266 */  "", //" (format=",
/* 1267 */  "", //"format file name",
/* 1268 */  "", //"Beginning to dump on file ",
/* 1269 */  "", //"Transcript written on ",
/* 1270 */  "", //" )",
/* 1271 */  "", //"end occurred ",
/* 1272 */  "", //"inside a group at level ",
/* 1273 */  "", //"when ",
/* 1274 */  "", //" on line ",
/* 1275 */  "", //" was incomplete)",
/* 1276 */  "", //"(see the transcript file for additional information)",
/* 1277 */  "", //"(\\dump is performed only by INITEX)",
/* 1278 */  "", //"debug # (-1 to exit):",
/* 1279 */  "", //"openout",
/* 1280 */  "", //"closeout",
/* 1281 */  "", //"special",
/* 1282 */  "", //"immediate",
/* 1283 */  "", //"setlanguage",
/* 1284 */  "", //"[unknown extension!]",
/* 1285 */  "", //"ext1",
/* 1286 */  "", //" (hyphenmin ",
/* 1287 */  "", //"whatsit" "?",
/* 1288 */  "", //"ext2",
/* 1289 */  "", //"ext3",
/* 1290 */  "endwrite",
/* 1291 */  "", //"Unbalanced write command",
/* 1292 */  "", //"On this page there's a \\write with fewer real {'s than }'s.",
/* 1293 */  "", //"ext4",
/* 1294 */  "", //"output file name",
};

static str_number load_pool_strings (integer spare_size)
{
  str_number g;
  size_t k, l, i = 0;

  for (k = 0; k < sizeof(pool_file_arr) / sizeof(char *); k++)
  {
    l = strlen(pool_file_arr[k]);
    i += l;

    if (i >= spare_size)
      return 0;

    memcpy(str_pool + pool_ptr, pool_file_arr[k], l);
    pool_ptr += l;
    g = make_string();
  }

  return g;
}

static str_number make_str_string (const char * s)
{
  size_t slen = strlen(s);

  if (slen == 1)
  {
    return ((str_number) s[0]);
  }
  else
  {
    memcpy(str_pool + pool_ptr, s, slen);
    pool_ptr += slen;
    return (make_string());
  }
}

static str_number get_job_name (str_number job)
{
  if (aptex_env.aptex_job != NULL)
    return make_str_string(aptex_env.aptex_job);
  else
    return job;
}

static char * take_str_string (str_number s)
{
  char * a = (char *) malloc(length(s) + 1);
  strncpy(a, (const char *)(str_pool + str_start[s]), length(s));
  a[length(s)] = '\0';

  return a;
}

static const integer BEGINFMTCHECKSUM = 367403084;
static const integer ENDFMTCHECKSUM   = 69069;

static void reset_trie(void);
static boolean get_strings_started(void);
static void init_prim(void);
static void store_fmt_file(void);
static boolean str_eq_str(str_number s, str_number t);
static pointer prim_lookup(str_number s);
static void primitive_(str_number s, quarterword c, halfword o);
static void fix_date_and_time(void);

static int aptex_dump_put (void * out_file, void * p, int item_size)
{
  boolean fmt_stat; int nitems = 1;

  if (aptex_env.flag_compact_fmt)
    fmt_stat = (gzwrite(out_file, p, (item_size * nitems)) != (item_size * nitems));
  else
    fmt_stat = (fwrite(p, item_size, nitems, out_file) != nitems);

  if (fmt_stat)
  {
    printf("\n! ApTeX: Could not write %d %d-byte item%s.\n", nitems, item_size, (nitems > 1) ? "s" : "");
    aptex_utils_exit(EXIT_FAILURE);
  }

  return 0;
}

static int aptex_dump_get (void * in_file, void * p, int item_size)
{
  boolean fmt_stat; int nitems = 1;
 
  if (aptex_env.flag_compact_fmt)
    fmt_stat = (gzread(in_file, p, (item_size * nitems)) <= 0);
  else
    fmt_stat = (fread(p, item_size, nitems, in_file) != nitems);

  if (fmt_stat)
  {
    printf("\n! ApTeX: Could not read %d %d-byte item%s.\n", nitems, item_size, (nitems > 1) ? "s" : "");
    aptex_utils_exit(EXIT_FAILURE);
  }

  return 0;
}

static inline void dump_int (integer x)
{
  generic_dump(x);
}

static inline void dump_hh (two_halves x)
{
  generic_dump(x);
}

static inline void dump_wd (memory_word x)
{
  generic_dump(x);
}

/* split out to allow optimize for space, not time */
static void do_initex (void)
{
  integer i;
  integer k;

  for (k = mem_bot + 1; k <= lo_mem_stat_max; k++)
    mem[k].cint = 0;

  k = mem_bot;

  while (k <= lo_mem_stat_max)
  {
    glue_ref_count(k) = 1;
    stretch_order(k) = normal;
    shrink_order(k) = normal;
    k = k + glue_spec_size;
  }

  stretch(fil_glue) = unity;
  stretch_order(fil_glue) = fil;
  stretch(fill_glue) = unity;
  stretch_order(fill_glue) = fill;
  stretch(ss_glue) = unity;
  stretch_order(ss_glue) = fil;
  shrink(ss_glue) = unity;
  shrink_order(ss_glue) = fil;
  stretch(fil_neg_glue) = -unity;
  stretch_order(fil_neg_glue) = fil;
  rover = lo_mem_stat_max + 1;
  link(rover) = empty_flag;
  node_size(rover) = block_size;
  llink(rover) = rover;
  rlink(rover) = rover;
  lo_mem_max = rover + block_size;
  link(lo_mem_max) = 0;
  info(lo_mem_max) = 0;

  for (k = hi_mem_stat_min; k <= mem_top; k++)
    mem[k] = mem[lo_mem_max];

  info(omit_template) = end_template_token;
  link(end_span) = max_quarterword + 1;
  info(end_span) = 0;
  type(last_active) = hyphenated;
  line_number(last_active) = max_halfword;
  subtype(last_active) = 0;
  subtype(page_ins_head) = 255;
  type(page_ins_head) = split_up;
  link(mem_top) = page_ins_head;
  type(page_head) = glue_node;
  subtype(page_head) = normal;
  avail = 0;
  mem_end = mem_top;
  hi_mem_min = hi_mem_stat_min;
  var_used = lo_mem_stat_max + 1 - mem_bot;
  dyn_used = hi_mem_stat_usage;
  eq_type(undefined_control_sequence) = undefined_cs;
  equiv(undefined_control_sequence) = 0;
  eq_level(undefined_control_sequence) = level_zero;

  for (k = active_base; k <= undefined_control_sequence - 1; k++)
    eqtb[k] = eqtb[undefined_control_sequence];

  equiv(glue_base) = zero_glue;
  eq_level(glue_base) = level_one;
  eq_type(glue_base) = glue_ref;

  for (k = glue_base + 1; k <= local_base - 1; k++)
    eqtb[k] = eqtb[glue_base];

  glue_ref_count(zero_glue) = glue_ref_count(zero_glue) + local_base - glue_base;

  par_shape_ptr = 0;
  eq_type(par_shape_loc) = shape_ref;
  eq_level(par_shape_loc) = level_one;

  for (k = etex_pen_base; k <= etex_pens - 1; k++)
    eqtb[k] = eqtb[par_shape_loc];

  for (k = output_routine_loc; k <= toks_base + 255; k++)
    eqtb[k] = eqtb[undefined_control_sequence];

  box(0) = 0;
  eq_type(box_base) = box_ref;
  eq_level(box_base) = level_one;

  for (k = box_base + 1; k <= box_base + 255; k++)
    eqtb[k] = eqtb[box_base];

  cur_font = null_font;
  eq_type(cur_font_loc) = data;
  eq_level(cur_font_loc) = level_one;
  cur_jfont = null_font;
  eq_type(cur_jfont_loc) = data;
  eq_level(cur_jfont_loc) = level_one;
  cur_tfont = null_font;
  eq_type(cur_tfont_loc) = data;
  eq_level(cur_tfont_loc) = level_one;

  for (k = math_font_base; k <= math_font_base + 47; k++)
    eqtb[k] = eqtb[cur_font_loc];

  equiv(cat_code_base) = 0;
  eq_type(cat_code_base) = data;
  eq_level(cat_code_base) = level_one;

  for (k = cat_code_base; k <= int_base - 1; k++)
    eqtb[k] = eqtb[cat_code_base];

  eqtb[auto_spacing_code] = eqtb[cat_code_base];
  eqtb[auto_xspacing_code] = eqtb[cat_code_base];
  eqtb[enable_cjk_token_code] = eqtb[cat_code_base];

  for (k = 0; k <= 255; k++)
  {
    cat_code(k) = other_char;
    kcat_code(k) = other_kchar;
    math_code(k) = k;
    sf_code(k) = 1000;
    auto_xsp_code(k) = 0;
  }

  for (k = 0; k <= 1023; k++)
  {
    inhibit_xsp_code(k) = 0;
    inhibit_xsp_type(k) = 0;
    kinsoku_code(k) = 0;
    kinsoku_type(k) = 0;
  }

  for (k = 0; k <= 511; k++)
    kcat_code(k) = other_kchar;

  cat_code(carriage_return) = car_ret;
  cat_code(' ') = spacer;
  cat_code('\\') = escape;
  cat_code('%') = comment;
  cat_code(invalid_code) = invalid_char;
  cat_code(null_code) = ignore;

  for (k = '0'; k <= '9'; k++)
  {
    math_code(k) = k + var_code;
    auto_xsp_code(k) = 3;
  }

  kansuji_char(0) = toDVI(fromJIS(0x213B));
  kansuji_char(1) = toDVI(fromJIS(0x306C));
  kansuji_char(2) = toDVI(fromJIS(0x4673));
  kansuji_char(3) = toDVI(fromJIS(0x3B30));
  kansuji_char(4) = toDVI(fromJIS(0x3B4D));
  kansuji_char(5) = toDVI(fromJIS(0x385E));
  kansuji_char(6) = toDVI(fromJIS(0x4F3B));
  kansuji_char(7) = toDVI(fromJIS(0x3C37));
  kansuji_char(8) = toDVI(fromJIS(0x482C));
  kansuji_char(9) = toDVI(fromJIS(0x3665));

  for (k = 'A'; k <= 'Z'; k++)
  {
    cat_code(k) = letter;
    cat_code(k + 'a' - 'A') = letter;
    math_code(k) = k + var_code + 0x100;
    math_code(k + 'a' - 'A') = k + 'a' - 'A' + var_code + 0x100;
    lc_code(k) = k + 'a' - 'A';
    lc_code(k + 'a' - 'A') = k + 'a' - 'A';
    uc_code(k) = k;
    uc_code(k + 'a' - 'A') = k;
    auto_xsp_code(k) = 3;
    auto_xsp_code(k + 'a' - 'A') = 3;
    sf_code(k) = 999;
  }

  if (is_internalUPTEX())
  {
    kcat_code(0x0) = not_cjk; // { default: other_kchar }
 
    for (k = 0x2; k <= 0x3; k++)
      kcat_code(k) = not_cjk; // { Latin Extended-A, Latin Extended-B }

    kcat_code(0x25) = hangul; // { Hangul Jamo }
    kcat_code(0x46) = not_cjk; // { Latin Extended Additional }

    // { CJK Radicals Supplement .. Kangxi Radicals }
    for (k = 0x68; k <= 0x69; k++)
      kcat_code(k) = kanji;

    // { Hiragana, Katakana }
    for (k = 0x6C; k <= 0x6D; k++)
      kcat_code(k) = kana;

    kcat_code(0x6E) = kanji; // { Bopomofo }
    kcat_code(0x6F) = hangul; // { Hangul Compatibility Jamo }

    // { Kanbun .. CJK Strokes }
    for (k = 0x70; k <= 0x72; k++)
      kcat_code(k) = kanji;

    kcat_code(0x73) = kana; // { Katakana Phonetic Extensions }
    kcat_code(0x76) = kanji; // { CJK Unified Ideographs Extension A }
    kcat_code(0x78) = kanji; // { CJK Unified Ideographs }
    kcat_code(0x88) = hangul; // { Hangul Jamo Extended-A }
    kcat_code(0x93) = hangul; // { Hangul Syllables }
    kcat_code(0x94) = hangul; // { Hangul Jamo Extended-B }
    kcat_code(0x99) = kanji; // { CJK Compatibility Ideographs }
    // { kcat_code(0xA2) = other_kchar; Halfwidth and Fullwidth Forms }

    // { Kana Extended-B .. Small Kana Extension }
    for (k = 0x10D; k <= 0x110; k++)
      kcat_code(k) = kana;
 
    // { CJK Unified Ideographs Extension B .. H }
    for (k = 0x13B; k <= 0x143; k++)
      kcat_code(k) = kanji;

    kcat_code(0x1FD) = not_cjk; // { Latin-1 Letters }
    kcat_code(0x1FE) = kana; // { Fullwidth digit and latin alphabet }
    kcat_code(0x1FF) = kana; // { Halfwidth katakana }
  }
  else
  {
    kcat_code(0x20 + 1) = other_kchar; // {1 ku}
    kcat_code(0x20 + 2) = other_kchar; // {2 ku}

    for (k = 3; k <= 6; k++)
      kcat_code(0x20 + k) = kana; // {3 ku ... 6 ku}

    for (k = 7; k <= 13; k++)
      kcat_code(0x20 + k) = other_kchar; // {7 ku ... 13 ku}

    for (k = 14; k <= 120; k++)
      kcat_code(0x20 + k) = kanji; // {14 ku ... 120 ku}
    //{ $\.{@0x20}+|k| = |kcatcodekey|(|fromKUTEN|(|HILO|(k,1))$ }
    for (k = 16; k <= 94; k++)
      kcat_code(0xA0 + k) = kanji; // {2 men 16 ku ... 94 ku}
  };

  for (k = int_base; k <= del_code_base - 1; k++)
    eqtb[k].cint = 0;

  mag = 1000;
  tolerance = 10000;
  hang_after = 1;
  max_dead_cycles = 25;
  escape_char = '\\';
  end_line_char = carriage_return;
  pdf_compress_level = 9;
  pdf_major_version = 1;
  pdf_minor_version = 5;

  for (k = 0; k <= 255; k++)
    del_code(k) = -1;

  del_code('.') = 0;
  show_stream = -1;

  for (k = dimen_base; k <= eqtb_size; k++)
    eqtb[k].cint = 0;

  hash_used = frozen_control_sequence;
  cs_count = 0;

  if (aptex_env.trace_mem)
    puts("initex cs_count = 0 ");

  eq_type(frozen_dont_expand) = dont_expand;
  prim_used = prim_size; // {nothing is used}
  text(frozen_dont_expand) = 499;  /* "notexpanded:" */
  eq_type(frozen_primitive) = ignore_spaces;
  equiv(frozen_primitive) = 1;
  eq_level(frozen_primitive) = level_one;
  text(frozen_primitive) = make_str_string("pdfprimitive");

  jfm_enc = 0;
  font_ptr                    = null_font;
  fmem_ptr                    = 7;
  font_dir[null_font]         = dir_default;
  font_enc[null_font]         = 0;
  font_num_ext[null_font]     = 0;
  font_name[null_font]        = 795; /* nullfont */
  font_area[null_font]        = 335; /* "" */
  hyphen_char[null_font]      = '-';
  skew_char[null_font]        = -1; 
  bchar_label[null_font]      = non_address;
  font_bchar[null_font]       = non_char;
  font_false_bchar[null_font] = non_char;
  font_bc[null_font]          = 1;
  font_ec[null_font]          = 0;
  font_size[null_font]        = 0;
  font_dsize[null_font]       = 0;
  ctype_base[null_font]       = 0;
  char_base[null_font]        = 0;
  width_base[null_font]       = 0;
  height_base[null_font]      = 0;
  depth_base[null_font]       = 0;
  italic_base[null_font]      = 0;
  lig_kern_base[null_font]    = 0;
  kern_base[null_font]        = 0;
  exten_base[null_font]       = 0;
  font_glue[null_font]        = 0;
  font_params[null_font]      = 7;
  param_base[null_font]       = -1;

  for (k = 0; k <= 6; k++)
    font_info[k].sc = 0;

  text_baseline_shift_factor = 1000;
  script_baseline_shift_factor = 700;
  scriptscript_baseline_shift_factor = 500;

  reset_trie();
  text(frozen_protection) = 1184; /* "inaccessible" */
  format_ident = 1251;            /* " (INITEX)" */
  text(end_write) = 1290;         /* "endwrite" */
  eq_level(end_write) = level_one;
  eq_type(end_write) = outer_call;
  equiv(end_write) = 0;
  eTeX_mode = false;
  max_reg_num = 255;
  max_reg_help_line = "A register number must be between 0 and 255.";

  for (i = int_val; i <= tok_val; ++i)
    sa_root[i] = null;

  hyph_root = 0;
  hyph_start = 0;
}

/* sec 0004 */
static void initialize (void)
{
  integer i;
  integer k;

#ifndef APTEX_EXTENSION
  hyph_pointer z;
#endif

  for (i = 0; i <= 255; i++)
    xchr[i] = (char) i;

#ifdef JOKE
  xchr[32] = ' ';  xchr[33] = '!';  xchr[34] = '"';  xchr[35] = '#';
  xchr[36] = '$';  xchr[37] = '%';  xchr[38] = '&';  xchr[39] = '\'';
  xchr[40] = '(';  xchr[41] = ')';  xchr[42] = '*';  xchr[43] = '+';
  xchr[44] = ',';  xchr[45] = '-';  xchr[46] = '.';  xchr[47] = '/';
  xchr[48] = '0';  xchr[49] = '1';  xchr[50] = '2';  xchr[51] = '3';
  xchr[52] = '4';  xchr[53] = '5';  xchr[54] = '6';  xchr[55] = '7';
  xchr[56] = '8';  xchr[57] = '9';  xchr[58] = ':';  xchr[59] = ';';
  xchr[60] = '<';  xchr[61] = '=';  xchr[62] = '>';  xchr[63] = '?';
  xchr[64] = '@';  xchr[65] = 'A';  xchr[66] = 'B';  xchr[67] = 'C';
  xchr[68] = 'D';  xchr[69] = 'E';  xchr[70] = 'F';  xchr[71] = 'G';
  xchr[72] = 'H';  xchr[73] = 'I';  xchr[74] = 'J';  xchr[75] = 'K';
  xchr[76] = 'L';  xchr[77] = 'M';  xchr[78] = 'N';  xchr[79] = 'O';
  xchr[80] = 'P';  xchr[81] = 'Q';  xchr[82] = 'R';  xchr[83] = 'S';
  xchr[84] = 'T';  xchr[85] = 'U';  xchr[86] = 'V';  xchr[87] = 'W';
  xchr[88] = 'X';  xchr[89] = 'Y';  xchr[90] = 'Z';  xchr[91] = '[';
  xchr[92] = '\\'; xchr[93] = ']';  xchr[94] = '^';  xchr[95] = '_';
  xchr[96] = '`';  xchr[97] = 'a';  xchr[98] = 'b';  xchr[99] = 'c';
  xchr[100] = 'd'; xchr[101] = 'e'; xchr[102] = 'f'; xchr[103] = 'g';
  xchr[104] = 'h'; xchr[105] = 'i'; xchr[106] = 'j'; xchr[107] = 'k';
  xchr[108] = 'l'; xchr[109] = 'm'; xchr[110] = 'n'; xchr[111] = 'o';
  xchr[112] = 'p'; xchr[113] = 'q'; xchr[114] = 'r'; xchr[115] = 's';
  xchr[116] = 't'; xchr[117] = 'u'; xchr[118] = 'v'; xchr[119] = 'w';
  xchr[120] = 'x'; xchr[121] = 'y'; xchr[122] = 'z'; xchr[123] = '{';
  xchr[124] = '|'; xchr[125] = '}'; xchr[126] = '~';

  for (i = 0; i <= 31; i++)
    xchr[i] = chr(i);

  for (i = 127; i <= 255; i++)
    xchr[i]= chr(i);
#endif

  for (i = 0; i <= 255; i++)
    xord[chr(i)] = invalid_code;

#ifdef JOKE
  for (i = 128; i <= 255; i++)
    xord[xchr[i]] = i;

  for (i = 0; i <= 126; i++)
    xord[xchr[i]] = i;
#endif

  for (i = 0; i <= 255; i++)
    xord[xchr[i]] = (char) i;

  xord[127] = 127;

  if (interaction < batch_mode)
    interaction = error_stop_mode;

  deletions_allowed = true;
  set_box_allowed = true;
  error_count = 0;
  help_ptr = 0;
  use_err_help = false;
  interrupt = 0;
  OK_to_interrupt = true;

  /* Random numbers. */
  two_to_the[0] = 1;

  for (k = 1; k <= 30; k++)
    two_to_the[k] = 2 * two_to_the[k - 1];

  spec_log[1] = 93032640;
  spec_log[2] = 38612034;
  spec_log[3] = 17922280;
  spec_log[4] = 8662214;
  spec_log[5] = 4261238;
  spec_log[6] = 2113709;
  spec_log[7] = 1052693;
  spec_log[8] = 525315;
  spec_log[9] = 262400;
  spec_log[10] = 131136;
  spec_log[11] = 65552;
  spec_log[12] = 32772;
  spec_log[13] = 16385;

  for (k = 14; k <= 27; k++)
    spec_log[k] = two_to_the[27 - k];

  spec_log[28] = 1;

#ifdef APTEX_DEBUG
  was_mem_end = mem_min;
  was_lo_max = mem_bot; // mem_min
  was_hi_min = mem_top; // mem_max
  panicking = false;
#endif

  nest_ptr = 0;
  max_nest_stack = 0;
  mode = vmode;
  head = contrib_head;
  tail = contrib_head;
  eTeX_aux = 0;
  prev_node = tail;
  direction = dir_yoko;
  adjust_dir = direction;
  prev_disp = 0;
  last_jchr = null;
  disp_called = false;
  prev_depth = ignore_depth;
  mode_line = 0;
  prev_graf = 0;
  shown_mode = 0;
  page_contents = empty;
  page_tail = page_head;

#ifdef APTEX_EXTENSION
  if (aptex_env.flag_initex)
#endif
    link(page_head) = null;

  last_glue = max_halfword;
  last_penalty = 0;
  last_kern = 0;
  last_node_type = -1;
  last_node_subtype = -1;
  page_depth = 0;
  page_max_depth = 0;

  for (k = int_base; k <= eqtb_size; k++)
    xeq_level[k] = level_one;

  no_new_control_sequence = true;
  prim_next(0) = 0;
  prim_text(0) = 0;

  for (k = 1; k <= prim_size; k++)
    prim[k] = prim[0];

  next(hash_base) = 0;
  text(hash_base) = 0;

  for (k = hash_base + 1; k <= undefined_control_sequence - 1; k++)
    hash[k] = hash[hash_base];

  save_ptr = 0;
  cur_level = level_one;
  cur_group = bottom_level;
  cur_boundary = 0;
  max_save_stack = 0;
  mag_set = 0;
  skip_mode = true;
  top_mark = 0;
  first_mark = 0;
  bot_mark = 0;
  split_first_mark = 0;
  split_bot_mark = 0;
  cur_val = 0;
  cur_val_level = int_val;
  radix = 0;
  cur_order = normal;

  for (k = 0; k <= 16; k++)
    read_open[k] = closed;

  cond_ptr = 0;
  if_limit = normal;
  cur_if = 0;
  if_line = 0;

  for (k = font_base; k <= font_max; k++)
    font_used[k] = false;

  null_character.b0 = min_quarterword;
  null_character.b1 = min_quarterword;
  null_character.b2 = min_quarterword;
  null_character.b3 = min_quarterword;
  total_pages = 0;
  max_v = 0;
  max_h = 0;
  max_push = 0;
  last_bop = -1;
  doing_leaders = false;
  dead_cycles = 0;
  cur_s = -1;
  dir_used = false;
  half_buf = dvi_buf_size / 2;
  dvi_limit = dvi_buf_size;
  dvi_ptr = 0;
  dvi_offset = 0;
  dvi_gone = 0;
  down_ptr = 0;
  right_ptr = 0;
  adjust_tail = 0;
  last_badness = 0;
  cur_kanji_skip = zero_glue;
  cur_xkanji_skip = zero_glue;
  pack_begin_line = 0;
  empty_field.rh = 0;
  empty_field.lh = 0;
  null_delimiter.b0 = 0;
  null_delimiter.b1 = 0;
  null_delimiter.b2 = 0;
  null_delimiter.b3 = 0;
  align_ptr = 0;
  cur_align = 0;
  cur_span = 0;
  cur_loop = 0;
  cur_head = 0;
  cur_tail = 0;

/* *not* OK with APTEX_EXTENSION, since may not be allocated yet */
#ifndef APTEX_EXTENSION
  for (z = 0; z <= hyphen_prime; z++)
  {
    hyph_word[z] = 0;
    hyph_list[z] = 0;
  }
#endif

  hyph_count = 0;
  output_active = false;
  insert_penalties = 0;
  ligature_present = false;
  cancel_boundary = false;
  lft_hit = false;
  rt_hit = false;
  ins_disc = false;
  after_token = 0;
  long_help_seen = false;
  format_ident = 0;

  for (k = 0; k <= 17; k++)
    write_open[k] = false;

  LR_ptr = null;
  LR_problems = 0;
  cur_dir = left_to_right;
  pseudo_files = null;
  sa_mark = null;
  sa_null.hh.lh = null;
  sa_null.hh.rh = null;
  sa_chain = null;
  sa_level = level_zero;
  page_disc = null;
  split_disc = null;
  page_dir = dir_yoko;

  aptex_utils_get_seconds_and_micros(&epochseconds, &microseconds);
  aptex_utils_init_start_time();

  if (aptex_env.flag_initex)
    do_initex();
}

/* sec 1303 */
static boolean load_fmt_file (void)
{
  integer j, k;
  pointer p, q;
  integer x;

  // Undump constants for consistency check
  undump_int(x);

  if (x != BEGINFMTCHECKSUM)
    goto bad_fmt;

  undump(0, 1, eTeX_mode);

  if (eTeX_ex)
  {
    max_reg_num = 32767;
    max_reg_help_line = "A register number must be between 0 and 32767.";
  }
  else
  {
    max_reg_num = 255;
    max_reg_help_line = "A register number must be between 0 and 255.";
  }

  undump_int(x); /* mem_bot */

  if (x != mem_bot)
    goto bad_fmt;

  undump_int(x); /* mem_top */

#ifdef APTEX_EXTENSION
  mem = allocate_mem(x);

  if (mem == NULL)
    exit(EXIT_FAILURE);

  /* do `mem' part of initialize */
  {
#ifdef APTEX_DEBUG
    was_mem_end = mem_min;
    was_lo_max = mem_bot; // mem_min
    was_hi_min = mem_top; // mem_max
    panicking = false;
#endif

    /*  nest_ptr = 0; */
    /*  max_nest_stack = 0; */
    mode = vmode;
    head = contrib_head;
    tail = contrib_head;
    eTeX_aux = 0;
    prev_node = tail;
    direction = dir_yoko;
    adjust_dir = direction;
    prev_disp = 0;
    last_jchr = null;
    disp_called = false;
    prev_depth = ignore_depth;
    mode_line = 0;
    prev_graf = 0;
    /*  shown_mode = 0; */
    /*  page_contents = 0; */
    page_tail = page_head;
    link(page_head) = 0;
  }
#endif

  if (x != mem_top)
    goto bad_fmt;

  undump_int(x); /* eqtb_size */

  if (x != eqtb_size)
    goto bad_fmt;

  undump_int(x); /* hash_prime */

  if (x != hash_prime)
    goto bad_fmt;

  undump_int(x); /* hyphen_prime */

#ifdef APTEX_EXTENSION
/* allow format files dumped with arbitrary (prime) hyphenation exceptions */
  realloc_hyphen(x);
  hyphen_prime = x;
#endif

  if (x != hyphen_prime)
    goto bad_fmt;

  // Undump the string pool
  {
    undump_int(x); /* pool_size */

    if (x < 0)
      goto bad_fmt; 

#ifdef APTEX_EXTENSION
    if (x > current_pool_size)
      str_pool = realloc_str_pool(x - current_pool_size + increment_pool_size);

    if (x > current_pool_size)
#else
    if (x > pool_size)
#endif
    {
      printf("%s%s\n", "---! Must increase the ", "string pool size");
      goto bad_fmt;
    }
    else
      pool_ptr = x;
  }

  {
    undump_int(x);  /* max_strings */

    if (x < 0)
      goto bad_fmt;

#ifdef APTEX_EXTENSION
    if (x > current_max_strings)
      str_start = realloc_str_start(x - current_max_strings + increment_max_strings);

    if (x > current_max_strings)
#else
    if (x > max_strings)
#endif
    {
      printf("%s%s\n", "---! Must increase the ", "max strings");
      goto bad_fmt;
    }
    else
      str_ptr = x;
  }

  undump_things(str_start[0], str_ptr + 1);
  undump_things(str_pool[0], pool_ptr);
  init_str_ptr = str_ptr;
  init_pool_ptr = pool_ptr;
  // Undump the dynamic memory
  undump(lo_mem_stat_max + 1000, hi_mem_stat_min - 1, lo_mem_max);
  undump(lo_mem_stat_max + 1, lo_mem_max, rover);

  if (eTeX_ex)
  {
    for (k = int_val; k <= tok_val; ++k)
      undump(0, lo_mem_max, sa_root[k]);
  }

  p = mem_bot;
  q = rover;

  do {
    if (undump_things(mem[p], q + 2 - p))
      return -1;

    p = q + node_size(q);

    if ((p > lo_mem_max) || ((q >= rlink(q)) && (rlink(q) != rover)))
      goto bad_fmt;

    q = rlink(q);
  } while (!(q == rover));

  if (undump_things(mem[p], lo_mem_max + 1 - p))
    return -1;

  if (mem_min < mem_bot - 2)
  {
/*  or call add_variable_space(mem_bot - (mem_min + 1)) */
    p = llink(rover);
    q = mem_min + 1;
    link(mem_min) = 0;  /* null */
    info(mem_min) = 0;  /* null */
    rlink(p) = q;
    llink(rover) = q;
    rlink(q) = rover;
    llink(q) = p;
    link(q) = empty_flag;
    node_size(q) = mem_bot - q;
  }

  undump(lo_mem_max + 1, hi_mem_stat_min, hi_mem_min);
  undump(mem_bot, mem_top, avail);
  mem_end = mem_top;

  if (undump_things(mem[hi_mem_min], mem_end + 1 - hi_mem_min))
    return -1;

  undump_int(var_used);
  undump_int(dyn_used);

  // Undump the table of equivalents
  // Undump regions 1 to 6 of eqtb
  k = active_base;

  do {
    undump_int(x);

    if ((x < 1) || (k + x > eqtb_size + 1))
      goto bad_fmt;

    if (undump_things(eqtb[k], x))
      return -1;

    k = k + x;
    undump_int(x);

    if ((x < 0) || (k + x > eqtb_size + 1))
      goto bad_fmt;

    for (j = k; j <= k + x - 1; j++)
      eqtb[j] = eqtb[k - 1];

    k = k + x;
  } while (!(k > eqtb_size));

  undump(hash_base, frozen_control_sequence, par_loc);
  par_token = cs_token_flag + par_loc;
  undump(hash_base, frozen_control_sequence, write_loc);
  undump(hash_base, frozen_control_sequence, hash_used);

  // Undump the hash table
  p = hash_base - 1;

  do {
    undump(p + 1, hash_used, p);
    undump_hh(hash[p]);
  } while (!(p == hash_used));

  if (undump_things(hash[hash_used + 1], undefined_control_sequence - 1 - hash_used))
    return -1;

  undump_int(cs_count);

  for (p = 0; p <= prim_size; p++)
    undump_hh(prim[p]);

  // Undump the font information
  {
    undump_int(x); /* font_mem_size */

    if (x < 7)
      goto bad_fmt;

#ifdef APTEX_EXTENSION
    if (x > current_font_mem_size)
      font_info = realloc_font_info (x - current_font_mem_size + increment_font_mem_size);

    if (x > current_font_mem_size)
#else
    if (x > font_mem_size)
#endif
    {
      puts("---! Must increase the font mem size");
      goto bad_fmt;
    }
    else
      fmem_ptr = x;
  }

  undump_things(font_info[0], fmem_ptr);
  undump_size(font_base, font_max, "font max", font_ptr);

  {
    undump_things(font_dir[null_font], font_ptr + 1);
    undump_things(font_enc[null_font], font_ptr + 1);
    undump_things(font_num_ext[null_font], font_ptr + 1);
    undump_things(font_check[null_font], font_ptr + 1);
    undump_things(font_size[null_font], font_ptr + 1);
    undump_things(font_dsize[null_font], font_ptr + 1);
    undump_things(font_params[null_font], font_ptr + 1);
    undump_things(hyphen_char[null_font], font_ptr + 1);
    undump_things(skew_char[null_font], font_ptr + 1);
    undump_things(font_name[null_font], font_ptr + 1);
    undump_things(font_area[null_font], font_ptr + 1);
    undump_things(font_bc[null_font], font_ptr + 1);
    undump_things(font_ec[null_font], font_ptr + 1);
    undump_things(ctype_base[null_font], font_ptr + 1);
    undump_things(char_base[null_font], font_ptr + 1);
    undump_things(width_base[null_font], font_ptr + 1);
    undump_things(height_base[null_font], font_ptr + 1);
    undump_things(depth_base[null_font], font_ptr + 1);
    undump_things(italic_base[null_font], font_ptr + 1);
    undump_things(lig_kern_base[null_font], font_ptr + 1);
    undump_things(kern_base[null_font], font_ptr + 1);
    undump_things(exten_base[null_font], font_ptr + 1);
    undump_things(param_base[null_font], font_ptr + 1);
    undump_things(font_glue[null_font], font_ptr + 1);
    undump_things(bchar_label[null_font], font_ptr + 1);
    undump_things(font_bchar[null_font], font_ptr + 1);
    undump_things(font_false_bchar[null_font], font_ptr + 1);
  }

#ifdef APTEX_EXTENSION
  {
    integer count = 0, oldfont_mem_size = 0;

    for (x = 0; x <= font_ptr; x++)
    {
      if (bchar_label[x] > oldfont_mem_size)
        oldfont_mem_size = bchar_label[x];
    }

    if ((oldfont_mem_size != non_address) && (oldfont_mem_size > font_max))
    {
      for (x = 0; x <= font_ptr; x++)
      {
        if (bchar_label[x] == oldfont_mem_size)
        {
          bchar_label[x] = non_address;
          count++;
        }
      }

      if (aptex_env.trace_mem)
        printf("oldfont_mem_size is %"PRId64" --- hit %"PRId64" times. Using non_address %d\n",
          oldfont_mem_size, count, non_address);
    }
  }
#endif

  // Undump the hyphenation tables
  undump(0, hyphen_prime, hyph_count);

  for (k = 1; k <= hyph_count; k++)
  {
    undump(0, hyphen_prime, j);
    undump(0, str_ptr, hyph_word[j]);
    undump(0, max_halfword, hyph_list[j]);
  }

#ifdef APTEX_EXTENSION
/* if user specified new hyphen prime - flush existing exception patterns ! */
/* but, we can reclaim the string storage wasted ... */
  if (aptex_env.flag_initex)
  {
    if (new_hyphen_prime != 0)
    {
      realloc_hyphen(new_hyphen_prime); /* reset_hyphen(); */
      hyphen_prime = new_hyphen_prime;
    }
  }
#endif

  {
    undump_int(x);

    if (x < 0)
      goto bad_fmt;

#ifdef APTEX_EXTENSION
    if (!aptex_env.flag_initex)
    {
      allocate_tries(x); /* allocate only as much as is needed */
    }
#endif

    if (x > trie_size)
    {
      puts("---! Must increase the trie size");
      goto bad_fmt;
    }
    else
      j = x;
  }

  if (aptex_env.flag_initex)
    trie_max = j;

  undump(0, j, hyph_start);
  undump_things(trie_trl[0], j + 1);
  undump_things(trie_tro[0], j + 1);
  undump_things(trie_trc[0], j + 1);
  undump_size(0, trie_op_size, "trie op size", j);

  if (aptex_env.flag_initex)
    trie_op_ptr = j;

  undump_things(hyf_distance[1], j);
  undump_things(hyf_num[1], j);
  undump_things(hyf_next[1], j);

  if (aptex_env.flag_initex)
  {
    for (k = 0; k <= 255; k++)
      trie_used[k] = min_quarterword;
  }

  k = 256;

  while (j > 0)
  {
    undump(0, k - 1, k);
    undump(1, j, x);

    if (aptex_env.flag_initex)
      trie_used[k] = x;

    j = j - x;
    op_start[k] = j;
  }

  if (aptex_env.flag_initex)
    trie_not_ready = false;

  undump(batch_mode, error_stop_mode, interaction);
  undump(0, str_ptr, format_ident);
  undump_int(x);
  
  if ((x != ENDFMTCHECKSUM) || w_eof(fmt_file))
    goto bad_fmt;

  return true;

bad_fmt:
  wake_up_terminal();
  puts("(Fatal format file error; I'm stymied)");

  return false;
}

/* sec 1335 */
static void final_cleanup (void)
{
  small_number c;

  c = cur_chr;

  if (c != 1)
    new_line_char = -1;

  if (job_name == 0)
    open_log_file();

  while (input_ptr > 0)
  {
    if (state == token_list)
      end_token_list();
    else
      end_file_reading();
  }

  while (open_parens > 0)
  {
    prints(" )");
    decr(open_parens);
  }

  if (cur_level > level_one)
  {
    print_nl("(");
    print_esc("end occurred ");
    prints("inside a group at level ");
    print_int(cur_level - level_one);
    print_char(')');

    if (eTeX_ex)
      show_save_groups();
  }

  while (cond_ptr != null)
  {
    print_nl("(");
    print_esc("end occurred ");
    prints("when ");
    print_cmd_chr(if_test, cur_if);

    if (if_line != 0)
    {
      prints(" on line ");
      print_int(if_line);
    }

    prints(" was incomplete)");
    if_line = if_line_field(cond_ptr);
    cur_if = subtype(cond_ptr);
    temp_ptr = cond_ptr;
    cond_ptr = link(cond_ptr);
    free_node(temp_ptr, if_node_size);
  }

  if (history != spotless)
  {
    if ((history == warning_issued) || (interaction < error_stop_mode))
    {
      if (selector == term_and_log)
      {
        selector = term_only;
        print_nl("(see the transcript file for additional information)");
        selector = term_and_log;
      }
    }
  }

  if (c == 1)
  {
    if (aptex_env.flag_initex)
    {
      for (c = top_mark_code; c <= split_bot_mark_code; c++)
      {
        if (cur_mark[c] != null)
          delete_token_ref(cur_mark[c]);
      }

      if (sa_mark != null)
      {
        if (do_marks(destroy_marks, 0, sa_mark))
          sa_mark = null;
      }

      for (c = last_box_code; c <= vsplit_code; ++c)
        flush_node_list(disc_ptr[c]);

      if (last_glue != max_halfword)
        delete_glue_ref(last_glue);

      store_fmt_file();
    }
    else
      print_nl("(\\dump is performed only by INITEX)");
  }
}

static void init_randoms (integer seed);

static int aptex_program (void)
{
  history = fatal_error_stop;

  if (ready_already == 314159)
    goto start_of_TEX;

  bad = 0;

  if ((half_error_line < 30) || (half_error_line > error_line - 15))
    bad = 1;

  if (max_print_line < 60)
    bad = 2;

  if (dvi_buf_size % 8 != 0)
    bad = 3;

  if (mem_bot + 1100 > mem_top)
    bad = 4;

  if (hash_prime > hash_size)
    bad = 5;

  if (max_in_open >= 128)
    bad = 6;

  if (mem_top < 256 + 11)
    bad = 7;

  if (aptex_env.flag_initex)
  {
    if ((mem_min != 0) || (mem_max != mem_top))
      bad = 10;
  }

  if ((mem_min > mem_bot) || (mem_max < mem_top))
    bad = 10;

  if ((min_quarterword > 0) || (max_quarterword < 255))
    bad = 11;

  if ((min_halfword > 0) || (max_halfword < 32767))
    bad = 12;

  if ((min_quarterword < min_halfword) || (max_quarterword > max_halfword))
    bad = 13;

  if ((mem_min < min_halfword) || (mem_max >= max_halfword) || (mem_bot - mem_min >= max_halfword))
    bad = 14;

  if (mem_max > mem_top)
    bad = 14;

  if ((0 < min_quarterword) || (font_max > max_quarterword))
    bad = 15;

#ifdef APTEX_EXTENSION
  if (font_max > 65535)
#else
  if (font_max > 256)
#endif
    bad = 16;

  if ((save_size > max_halfword) || (max_strings > max_halfword))
    bad = 17;

  if (buf_size > max_halfword)
    bad = 18;

  if (max_quarterword - min_quarterword < 255)
    bad = 19;

  if (cs_token_flag + undefined_control_sequence > max_halfword)
    bad = 21;

  if (format_default_length > file_name_size)
    bad = 31;

  if (max_halfword < (mem_top - mem_min) / 2)
    bad = 41;

  if (bad > 0)
  {
    printf("%s%s%" PRId64 "\n", "Ouch---my internal constants have been clobbered!",
        "---case ", bad);

    goto final_end;
  }

  initialize();

  if (aptex_env.flag_initex)
  {
    if (!get_strings_started())
      goto final_end;

    init_prim();
    init_str_ptr = str_ptr;
    init_pool_ptr = pool_ptr;
    fix_date_and_time();
  }

  ready_already = 314159;

start_of_TEX:
  selector = term_only;
  tally = 0;
  term_offset = 0;
  file_offset = 0;
  kcode_pos = 0;
  prints(banner);

  if (format_ident == 0)
  {
    prints(" (preloaded format=");
    prints(format_name);
    prints(")");
    print_ln();
  }
  else
  {
    slow_print(format_ident);
    print_ln();
  }

  update_terminal();
  job_name = 0;
  name_in_progress = false;
  log_opened = false;
  output_file_name = 0;

  {
    {
      input_ptr = 0;
      max_in_stack = 0;
      in_open = 0;
      high_in_open = 0;
      open_parens = 0;
      max_buf_stack = 0;
      grp_stack[0] = 0;
      if_stack[0] = null;
      param_ptr = 0;
      max_param_stack = 0;

#ifdef APTEX_EXTENSION
      memset(buffer, 0, current_buf_size);
#else
      memset(buffer, 0, buf_size);
#endif

      first = 0;
      scanner_status = normal;
      warning_index = 0;
      first = 1;
      state = new_line;
      start = 1;
      index = 0;
      line = 0;
      name = 0;
      force_eof = false;
      align_state = 1000000;

      if (!init_terminal())
        goto final_end;

      limit = last;
      first = last + 1;
    }

    if (aptex_env.flag_initex)
    {
      if (true || ((buffer[loc] == '*') && (format_ident == 1251)))
      {
        no_new_control_sequence = false;
        primitive("lastnodetype", last_item, last_node_type_code);
        primitive("lastnodechar", last_item, last_node_char_code);
        primitive("lastnodesubtype", last_item, last_node_subtype_code);
        primitive("eTeXversion", last_item, eTeX_version_code);
        primitive("eTeXrevision", convert, eTeX_revision_code);
        primitive("everyeof", assign_toks, every_eof_loc);
        primitive("tracingassigns", assign_int, int_base + tracing_assigns_code);
        primitive("tracinggroups", assign_int, int_base + tracing_groups_code);
        primitive("tracingifs", assign_int, int_base + tracing_ifs_code);
        primitive("tracingscantokens", assign_int, int_base + tracing_scan_tokens_code);
        primitive("tracingnesting", assign_int, int_base + tracing_nesting_code);
        primitive("predisplaydirection", assign_int, int_base + pre_display_direction_code);
        primitive("lastlinefit", assign_int, int_base + last_line_fit_code);
        primitive("savingvdiscards", assign_int, int_base + saving_vdiscards_code);
        primitive("savinghyphcodes", assign_int, int_base + saving_hyph_codes_code);
        primitive("currentgrouplevel", last_item, current_group_level_code);
        primitive("currentgrouptype", last_item, current_group_type_code);
        primitive("currentiflevel", last_item, current_if_level_code);
        primitive("currentiftype", last_item, current_if_type_code);
        primitive("currentifbranch", last_item, current_if_branch_code);
        primitive("fontcharwd", last_item, font_char_wd_code);
        primitive("fontcharht", last_item, font_char_ht_code);
        primitive("fontchardp", last_item, font_char_dp_code);
        primitive("fontcharic", last_item, font_char_ic_code);
        primitive("parshapelength", last_item, par_shape_length_code);
        primitive("parshapeindent", last_item, par_shape_indent_code);
        primitive("parshapedimen", last_item, par_shape_dimen_code);
        primitive("showgroups", xray, show_groups);
        primitive("showtokens", xray, show_tokens);
        primitive("unexpanded", the, 1);
        primitive("detokenize", the, show_tokens);
        primitive("showifs", xray, show_ifs);
        primitive("interactionmode", set_page_int, 2);
        primitive("middle", left_right, middle_noad);
        primitive("TeXXeTstate", assign_int, eTeX_state_base + TeXXeT_code);
        primitive("beginL", valign, begin_L_code);
        primitive("endL", valign, end_L_code);
        primitive("beginR", valign, begin_R_code);
        primitive("endR", valign, end_R_code);
        primitive("scantokens", input, 2);
        primitive("readline", read_to_cs, 1);
        primitive("unless", expand_after, 1);
        primitive("ifdefined", if_test, if_def_code);
        primitive("ifcsname", if_test, if_cs_code);
        primitive("iffontchar", if_test, if_font_char_code);
        primitive("ifincsname", if_test, if_in_csname_code);
        primitive("protected", prefix, 8);
        primitive("numexpr", last_item, eTeX_expr - int_val + int_val);
        primitive("dimexpr", last_item, eTeX_expr - int_val + dimen_val);
        primitive("glueexpr", last_item, eTeX_expr - int_val + glue_val);
        primitive("muexpr", last_item, eTeX_expr - int_val + mu_val);
        primitive("gluestretchorder", last_item, glue_stretch_order_code);
        primitive("glueshrinkorder", last_item, glue_shrink_order_code);
        primitive("currentspacingmode", last_item, current_spacing_mode_code);
        primitive("currentxspacingmode", last_item, current_xspacing_mode_code);
        primitive("currentcjktoken", last_item, current_cjk_token_code);
        primitive("gluestretch", last_item, glue_stretch_code);
        primitive("glueshrink", last_item, glue_shrink_code);
        primitive("mutoglue", last_item, mu_to_glue_code);
        primitive("gluetomu", last_item, glue_to_mu_code);
        primitive("marks", mark, marks_code);
        primitive("topmarks", top_bot_mark, top_mark_code + marks_code);
        primitive("firstmarks", top_bot_mark, first_mark_code + marks_code);
        primitive("botmarks", top_bot_mark, bot_mark_code + marks_code);
        primitive("splitfirstmarks", top_bot_mark, split_first_mark_code + marks_code);
        primitive("splitbotmarks", top_bot_mark, split_bot_mark_code + marks_code);
        primitive("pagediscards", un_vbox, last_box_code);
        primitive("splitdiscards", un_vbox, vsplit_code);
        primitive("interlinepenalties", set_shape, inter_line_penalties_loc);
        primitive("clubpenalties", set_shape, club_penalties_loc);
        primitive("widowpenalties", set_shape, widow_penalties_loc);
        primitive("displaywidowpenalties", set_shape, display_widow_penalties_loc);

        if (buffer[loc] == '*')
          incr(loc);

        eTeX_mode = true;
        max_reg_num = 32767;
        max_reg_help_line = "A register number must be between 0 and 32767.";
      }
    }

    if (!no_new_control_sequence)
      no_new_control_sequence = true;
    else if ((format_ident == 0) || (buffer[loc] == '&') || (buffer[loc] == '+'))
    {
      if (format_ident != 0)
        initialize();

      if (!open_fmt_file())
        goto final_end;

      if (!load_fmt_file())
      {
        w_close(fmt_file);
        goto final_end;
      }

      w_close(fmt_file);

      while ((loc < limit) && (buffer[loc] == ' '))
        incr(loc);
    }

    if (eTeX_ex)
      printf("entering extended mode\n");

    if (end_line_char_inactive())
      decr(limit);
    else
      buffer[limit] = end_line_char;

    fix_date_and_time();
    random_seed = (microseconds * 1000) + (epochseconds % 1000000);
    init_randoms(random_seed);
    magic_offset = str_start[886] - 9 * ord_noad; /* math_spacing = 886 */

    // @<Initialize the print |selector| based on |interaction|@>;
    if (interaction == batch_mode)
      selector = no_print;
    else
      selector = term_only;

    if ((loc < limit) && (cat_code(buffer[loc]) != escape))
      start_input();
  }

  aptex_env.time_main = clock();
  history = spotless;
  synctex_init();
  main_control();
  final_cleanup();
  close_files_and_terminate();

final_end:
  return do_final_end();
}

#ifdef APTEX_EXTENSION
/* add a block of variable size node space below mem_bot(0) */
static void add_variable_space (int size)
{
  halfword p;
  halfword q;
  integer t;

  if (mem_min == 0)
    t = mem_min;
  else
    t = mem_min + 1;

  mem_min = t - (size + 1); /* first word in new block - 1 */

  if (mem_min < mem_start)
  {
    if (aptex_env.trace_mem)
      puts("WARNING: mem_min < mem_start!");

    mem_min = mem_start;
  }

  p = llink(rover);
  q = mem_min + 1;
  link(mem_min) = 0; /* insert blank word below ??? */
  info(mem_min) = 0; /* insert blank word below ??? */
  rlink(p) = q;
  llink(rover) = q;
  rlink(q) = rover;
  llink(q) = p;
  link(q) = empty_flag;
  info(q) = t - q; /* block size */
  rover = q;
}
#endif

#ifdef INITEX
static void reset_trie (void)
{
  integer k;

  for (k = -trie_op_size; k <= trie_op_size; k++)
    trie_op_hash[k] = 0;

  for (k = 0; k <= 255; k++)
    trie_used[k] = min_trie_op;

  max_op_used = min_trie_op;
  trie_op_ptr = 0;
  trie_not_ready = true;
  trie_root = 0;
  trie_c[0] = 0;
  trie_ptr = 0;
  trie_not_ready = true;
}

/* borrowed code from initialize() */
static void reset_hyphen(void)
{
  hyph_pointer z;

  for (z = 0; z <= hyphen_prime; z++)
  {
    hyph_word[z] = 0;
    hyph_list[z] = 0;
  }

  hyph_count = 0;
}

/* sec 0047 */
static boolean get_strings_started (void)
{
  integer k;
  str_number g;

  pool_ptr = 0;
  str_ptr = 0;
  str_start[0] = 0;

  for (k = 0; k <= 255; k++)
  {
    if (((k < ' ') || (k > '~')) && !(ismultiprn(k)))
    {
      append_char('^');
      append_char('^');

      if (k < 64)
        append_char(k + 64);
      else if (k < 128)
        append_char(k - 64);
      else
      {
        append_lc_hex(k / 16);
        append_lc_hex(k % 16);
      }
    }
    else
      append_char(k);

    g = make_string();
  }

  g = load_pool_strings(pool_size - string_vacancies);

  if (g == 0)
  {
    printf("%s\n", "! You have to increase POOLSIZE.");
    return false;
  }

  return true;
}

/* sec 0131 */
static void sort_avail (void)
{
  pointer p, q, r;
  pointer old_rover;

  p = get_node(010000000000);
  p = rlink(rover);
  rlink(rover) = max_halfword;
  old_rover = rover;

  while (p != old_rover)
  {
    if (p < rover)
    {
      q = p;
      p = rlink(q);
      rlink(q) = rover;
      rover = q;
    }
    else
    {
      q = rover;

      while (rlink(q) < p)
        q = rlink(q);

      r = rlink(p);
      rlink(p) = rlink(q);
      rlink(q) = p;
      p = r;
    }
  }

  p = rover;

  while (rlink(p) != max_halfword)
  {
    llink(rlink(p)) = p;
    p = rlink(p);
  }

  rlink(p) = rover;
  llink(rover) = p;
}
/* sec 0241 */
static void fix_date_and_time (void)
{
  time_t clock;
  struct tm * tm_ptr;

  if ((clock = time(NULL)) < 0)
    puts("Time is not available!");

  tm_ptr = localtime(&clock);

  if (tm_ptr == NULL)
  {
    sys_year  = 2038;
    sys_month = 1;
    sys_day   = 18;
    sys_time  = 22 * 60 + 14;
  }
  else
  {
    sys_time  = tm_ptr->tm_hour * 60 + tm_ptr->tm_min;
    sys_day   = tm_ptr->tm_mday;
    sys_month = tm_ptr->tm_mon + 1;
    sys_year  = tm_ptr->tm_year + 1900;
  }

  tex_time = sys_time;
  day = sys_day;
  month = sys_month;
  year = sys_year;
}
/* sec 0264 */
static void primitive_ (str_number s, quarterword c, halfword o)
{ 
  pool_pointer k; // {index into |str_pool|}
  integer prim_val; // {needed to fill |prim_eqtb|}
  int j;
  /* small_number l; */
  int l;

  if (s < 256)
  {
    cur_val = s + single_base;
    prim_val = prim_lookup(s);
  }
  else
  {
    k = str_start[s];
    l = str_start[s + 1] - k;

#ifdef APTEX_EXTENSION
    if (first + l > current_buf_size + 1)
      buffer = realloc_buffer(increment_buf_size);

    if (first + l > current_buf_size + 1)
      overflow("buffer size", current_buf_size);
#else
    if (first + l > buf_size + 1)
      overflow("buffer size", buf_size);
#endif

    for (j = 0; j <= l - 1; j++)
      buffer[first + j] = str_pool[k + j];

    cur_val = id_lookup(first, l);
    flush_string();
    text(cur_val) = s;
    prim_val = prim_lookup(s);
  }

  eq_level(cur_val) = level_one;
  eq_type(cur_val) = c;
  equiv(cur_val) = o;
  prim_eq_level(prim_val) = level_one;
  prim_eq_type(prim_val) = c;
  prim_equiv(prim_val) = o;
}
/* sec 0944 */
static trie_op_code new_trie_op (small_number d, small_number n, trie_op_code v)
{
  integer h;
  trie_op_code u;
  integer l;

  h = abs(n + 313 * d + 361 * v + 1009 * cur_lang) % (trie_op_size + trie_op_size) + neg_trie_op_size;

  while (true)
  {
    l = trie_op_hash[h];

    if (l == 0)
    {
      if (trie_op_ptr == trie_op_size)
        overflow("pattern memory ops", trie_op_size);

      u = trie_used[cur_lang];

      if (u == max_trie_op)
        overflow("pattern memory ops per language", max_trie_op - min_trie_op);

      incr(trie_op_ptr);
      incr(u);
      trie_used[cur_lang] = u;

      if (u > max_op_used)
        max_op_used = u;

      hyf_distance[trie_op_ptr] = d;
      hyf_num[trie_op_ptr] = n;
      hyf_next[trie_op_ptr] = v;
      trie_op_lang[trie_op_ptr] = cur_lang;
      trie_op_hash[h] = trie_op_ptr;
      trie_op_val[trie_op_ptr] = u;
      return u;
    }

    if ((hyf_distance[l] == d) && (hyf_num[l] == n) &&
      (hyf_next[l] == v) && (trie_op_lang[l] == cur_lang))
    {
      return trie_op_val[l];
    }

    if (h > -trie_op_size)
      decr(h);
    else
      h = trie_op_size;
  }
}
/* sec 0948 */
static trie_pointer trie_node (trie_pointer p)
{
  trie_pointer h;
  trie_pointer q;

  /* compute hash value */
  h = abs(trie_c[p] + 1009 * trie_o[p] + 2718 * trie_l[p] + 3142 * trie_r[p]) % trie_size;

  while (true)
  {
    q = trie_hash[h];

    if (q == 0)
    {
      trie_hash[h] = p;
      return p;
    }

    if ((trie_c[q] == trie_c[p]) && (trie_o[q] == trie_o[p]) &&
      (trie_l[q] == trie_l[p]) && (trie_r[q] == trie_r[p]))
    {
      return q;
    }

    if (h > 0)
      decr(h);
    else
      h = trie_size;
  }
}
/* sec 0949 */
static trie_pointer compress_trie (trie_pointer p)
{
  if (p == 0)
    return 0;
  else
  {
    trie_l[p] = compress_trie(trie_l[p]);
    trie_r[p] = compress_trie(trie_r[p]);

    return trie_node(p);
  }
}
/* sec 0953 */
static void first_fit (trie_pointer p)
{
  trie_pointer h;
  trie_pointer z;
  trie_pointer q;
  ASCII_code c;
  trie_pointer l, r;
  short ll;

  c = trie_c[p];
  z = trie_min[c];

  while (true)
  {
    h = z - c;

    if (trie_max < h + 256)
    {
      if (trie_size <= h + 256)
        overflow("pattern memory", trie_size);

      do {
        incr(trie_max);
        trie_taken[trie_max] = false;
        trie_link(trie_max) = trie_max + 1;
        trie_tro[trie_max] = trie_max - 1;
      } while (!(trie_max == h + 256));
    }

    if (trie_taken[h])
      goto not_found;

    q = trie_r[p];

    while (q > 0)
    {
      if (trie_link(h + trie_c[q]) == 0)
        goto not_found;

      q = trie_r[q];
    }

    goto found;

not_found:
    z = trie_link(z);
  }

found:
  trie_taken[h] = true;
  trie_hash[p] = h;
  q = p;

  do {
    z = h + trie_c[q];
    l = trie_tro[z];
    r = trie_link(z);
    trie_tro[r] = l;
    trie_link(l) = r;
    trie_link(z) = 0;

    if (l < 256)
    {
      if (z < 256)
        ll = z;
      else
        ll = 256;
      
      do {
        trie_min[l] = r;
        incr(l);
      } while (!(l == ll));
    }

    q = trie_r[q];
  } while (!(q == 0));
}
/* sec 0957 */
static void trie_pack (trie_pointer p)
{
  trie_pointer q;

  do {
    q = trie_l[p];

    if ((q > 0) && (trie_hash[q] == 0))
    {
      first_fit(q);
      trie_pack(q);
    }

    p = trie_r[p];
  } while (!(p == 0));
}
/* sec 0959 */
static void trie_fix (trie_pointer p)
{
  trie_pointer q;
  ASCII_code c;
  trie_pointer z;

  z = trie_hash[p];

  do {
    q = trie_l[p];
    c = trie_c[p];
    trie_link(z + c) = trie_hash[q];
    trie_char(z + c) = c;
    trie_op(z + c) = trie_o[p];

    if (q > 0)
      trie_fix(q);

    p = trie_r[p];
  } while (!(p == 0));
}
/* sec 0960 */
void new_patterns (void)
{
  uint32_t k, l;
  boolean digit_sensed;
  trie_op_code v;
  trie_pointer p, q;
  boolean first_child;
  /* ASCII_code c; */
  int c;

  if (!trie_not_ready)
  {
    if (aptex_env.flag_reset_trie)
    {
      if (aptex_env.trace_mem)
        puts("Resetting patterns");

      reset_trie();

      if (aptex_env.flag_reset_hyphen)
      {
        if (aptex_env.trace_mem)
          puts("Resetting exceptions");

        reset_hyphen();
      }
    }
  }

  if (trie_not_ready)
  {
    set_cur_lang();
    scan_left_brace();
    k = 0;
    hyf[0] = 0;
    digit_sensed = false;

    while (true)
    {
      get_x_token();

      switch (cur_cmd)
      {
        case letter:
        case other_char:
          if (digit_sensed || (cur_chr < '0') || (cur_chr > '9'))
          {
            if (cur_chr == '.')
              cur_chr = 0;
            else
            {
              cur_chr = lc_code(cur_chr);

              if (cur_chr == 0)
              {
                print_err("Nonletter");
                help1("(See Appendix H.)");
                error();
              }
            }

            if (k < 63)
            {
              incr(k);
              hc[k] = cur_chr;
              hyf[k] = 0;
              digit_sensed = false;
            }
          }
          else if (k < 63)
          {
            hyf[k] = cur_chr - '0';
            digit_sensed = true;
          }
          break;

        case spacer:
        case right_brace:
          {
            if (k > 0)
            {
              if (hc[1] == 0)
                hyf[0] = 0;

              if (hc[k] == 0)
                hyf[k] = 0;

              l = k;
              v = min_trie_op;

              while (true)
              {
                if (hyf[l] != 0)
                  v = new_trie_op(k - l, hyf[l], v);

                if (l > 0)
                  decr(l);
                else
                  goto done1;
              }
done1:
              q = 0;
              hc[0] = cur_lang;

              while (l <= k)
              {
                c = hc[l];
                incr(l);
                p = trie_l[q];
                first_child = true;

                while ((p > 0) && (c > trie_c[p]))
                {
                  q = p;
                  p = trie_r[q];
                  first_child = false;
                }

                if ((p == 0) || (c < trie_c[p]))
                {
                  if (trie_ptr == trie_size)
                    overflow("pattern memory", trie_size);

                  incr(trie_ptr);
                  trie_r[trie_ptr] = p;
                  p = trie_ptr;
                  trie_l[p] = 0;

                  if (first_child)
                    trie_l[q] = p;
                  else
                    trie_r[q] = p;

                  trie_c[p] = c;
                  trie_o[p] = min_quarterword;
                }

                q = p;
              }

              if (trie_o[q] != min_trie_op)
              {
                print_err("Duplicate pattern");
                help1("(See Appendix H.)");
                error();
              }

              trie_o[q] = v;
            }

            if (cur_cmd == right_brace)
              goto done;

            k = 0;
            hyf[0] = 0;
            digit_sensed = false;
          }
          break;

        default:
          {
            print_err("Bad ");
            print_esc("patterns");
            help1("(See Appendix H.)");
            error();
          }
          break;
      }
    }

done:
    if (saving_hyph_codes > 0)
    {
      c = cur_lang;
      first_child = false;
      p = 0;

      do {
        q = p;
        p = trie_r[q];
      } while (!((p == 0) || (c <= trie_c[p])));

      if ((p == 0) || (c < trie_c[p]))
      {
        if (trie_ptr == trie_size)
          overflow("pattern memory", trie_size);

        incr(trie_ptr);
        trie_r[trie_ptr] = p;
        p = trie_ptr;
        trie_l[p] = 0;

        if (first_child)
          trie_l[q] = p;
        else
          trie_r[q] = p;

        trie_c[p] = c;
        trie_o[p] = min_quarterword;
      }

      q = p;
      p = trie_l[q];
      first_child = true;

      for (c = 0; c <= 255; ++c)
      {
        if ((lc_code(c) > 0) || ((c == 255) && first_child))
        {
          if (p == 0)
          {
            if (trie_ptr == trie_size)
              overflow("pattern memory", trie_size);

            incr(trie_ptr);
            trie_r[trie_ptr] = p;
            p = trie_ptr;
            trie_l[p] = 0;

            if (first_child)
              trie_l[q] = p;
            else
              trie_r[q] = p;

            trie_c[p] = c;
            trie_o[p] = min_quarterword;
          }
          else
            trie_c[p] = c;

          trie_o[p] = lc_code(c);
          q = p;
          p = trie_r[q];
          first_child = false;
        }
      }

      if (first_child)
        trie_l[q] = 0;
      else
        trie_r[q] = 0;
    }
  }
  else
  {
    print_err("Too late for ");
    print_esc("patterns");
    help1("All patterns must be given before typesetting begins.");
    error();
    link(garbage) = scan_toks(false, false);
    flush_list(def_ref);
  }
}
/* sec 0966 */
void init_trie (void)
{
  trie_pointer p;
  integer j, k, t;
  trie_pointer r, s;

  op_start[0] = -min_trie_op;

  for (j = 1; j <= 255; j++)
    op_start[j] = op_start[j - 1] + trie_used[j - 1];

  for (j = 1; j <= trie_op_ptr; j++)
    trie_op_hash[j] = op_start[trie_op_lang[j]] + trie_op_val[j];

  for (j = 1; j <= trie_op_ptr; j++)
  {
    while (trie_op_hash[j] > j)
    {
      k = trie_op_hash[j];
      t = hyf_distance[k];
      hyf_distance[k] = hyf_distance[j];
      hyf_distance[j] = t;
      t = hyf_num[k];
      hyf_num[k] = hyf_num[j];
      hyf_num[j] = t;
      t = hyf_next[k];
      hyf_next[k] = hyf_next[j];
      hyf_next[j]= t;
      trie_op_hash[j] = trie_op_hash[k];
      trie_op_hash[k] = k;
    }
  }

  for (p = 0; p <= trie_size; p++)
    trie_hash[p] = 0;

  hyph_root = compress_trie(hyph_root);
  trie_root = compress_trie(trie_root);

  for (p = 0; p <= trie_ptr; p++)
    trie_hash[p] = 0;

  for (p = 0; p <= 255; p++)
    trie_min[p] = p + 1;

  trie_link(0) = 1;
  trie_max = 0;

  if (trie_root != 0)
  {
    first_fit(trie_root);
    trie_pack(trie_root);
  }

  if (hyph_root > 0)
  {
    if (trie_root == 0)
    {
      for (p = 0; p <= 255; ++p)
        trie_min[p] = p + 2;
    }

    first_fit(hyph_root);
    trie_pack(hyph_root);
    hyph_start = trie_hash[hyph_root];
  }

  if (trie_max == 0)
  {
    for (r = 0; r <= 256; r++)
    {
      trie_link(r) = 0;
      trie_op(r) = min_trie_op;
      trie_char(r) = min_quarterword;
    }

    trie_max = 256;
  }
  else
  {
    if (hyph_root > 0)
      trie_fix(hyph_root);

    if (trie_root > 0)
      trie_fix(trie_root);

    r = 0;

    do {
      s = trie_link(r);

      {
        trie_link(r) = 0;
        trie_op(r) = min_trie_op;
        trie_char(r) = min_quarterword;
      }

      r = s;
    } while (!(r > trie_max));
  }

  trie_char(0) = '?';
  trie_not_ready = false;
}
/* sec 1302 */
static void store_fmt_file (void)
{
  integer j, k, l;
  pointer p, q;
  integer x;

  if (!aptex_env.flag_initex)
  {
    puts("! \\dump is performed only by INITEX");

    return;
  }

  if (save_ptr != 0)
  {
    print_err("You can't dump inside a group");
    help1("`{...\\dump}' is a no-no.");
    succumb();
  }

  selector = new_string;
  prints(" (preloaded format=");
  print(job_name);
  print_char(' ');
  print_int(year);
  print_char('.');
  print_int(month);
  print_char('.');
  print_int(day);
  print_char(')');

  if (interaction == batch_mode)
    selector = log_only;
  else
    selector = term_and_log;

  str_room(1);
  format_ident = make_string();
  pack_job_name(".fmt");

  while (!w_open_out(fmt_file))
    prompt_file_name("format file name", ".fmt");

  print_nl("Beginning to dump on file ");
  slow_print(w_make_name_string(fmt_file));
  flush_string();
  print_nl("");
  slow_print(format_ident);

  // Dump constants for consistency check
  dump_int(BEGINFMTCHECKSUM);

  while (pseudo_files != null)
    pseudo_close();

  dump_int(eTeX_mode);
  eTeX_state(0) = 0;
  dump_int(mem_bot);
  dump_int(mem_top);
  dump_int(eqtb_size);
  dump_int(hash_prime);
  dump_int(hyphen_prime);

  // Dump the string pool
  dump_int(pool_ptr);
  dump_int(str_ptr);

  for (k = 0; k <= str_ptr; k++)
    dump_int(str_start[k]);

  dump_things(str_pool[0], pool_ptr);
  print_ln();
  print_int(str_ptr);
  prints(" strings of total length ");
  print_int(pool_ptr);

  // Dump the dynamic memory
  sort_avail();
  var_used = 0;
  dump_int(lo_mem_max);
  dump_int(rover);

  if (eTeX_ex)
  {
    for (k = int_val; k <= tok_val; ++k)
      dump_int(sa_root[k]);
  }

  p = mem_bot;
  q = rover;
  x = 0;

  do {
    for (k = p; k <= q + 1; k++)
      dump_wd(mem[k]);

    x = x + q + 2 - p;
    var_used = var_used + q - p;
    p = q + node_size(q);
    q = rlink(q);
  } while (!(q == rover));

  var_used = var_used + lo_mem_max - p;
  dyn_used = mem_end + 1 - hi_mem_min;

  for (k = p; k <= lo_mem_max; k++)
    dump_wd(mem[k]);

  x = x + lo_mem_max + 1 - p;
  dump_int(hi_mem_min);
  dump_int(avail); 

  for (k = hi_mem_min; k <= mem_end; k++)
    dump_wd(mem[k]);

  x = x + mem_end + 1 - hi_mem_min;
  p = avail;

  while (p != 0)
  {
    decr(dyn_used);
    p = link(p);
  }

  dump_int(var_used);
  dump_int(dyn_used);
  print_ln();
  print_int(x);
  prints(" memory locations dumped; current usage is ");
  print_int(var_used);
  print_char('&');
  print_int(dyn_used);

  // Dump the table of equivalents
  // Dump regions 1 to 4 of eqtb
  k = active_base;

  do {
    j = k;

    while (j < int_base - 1)
    {
      if ((equiv(j) == equiv(j + 1)) &&
          (eq_type(j) == eq_type(j + 1)) &&
          (eq_level(j) == eq_level(j + 1)))
          goto found1;

      incr(j);
    }

    l = int_base;
    goto done1;

found1:
    incr(j);
    l = j;

    while (j < int_base - 1)
    {
      if ((equiv(j) != equiv(j + 1)) ||
          (eq_type(j) != eq_type(j + 1)) ||
          (eq_level(j) != eq_level(j + 1)))
          goto done1;

      incr(j);
    }

done1:
    dump_int(l - k);

    while (k < l)
    {
      dump_wd(eqtb[k]);
      incr(k);
    }

    k = j + 1;
    dump_int(k - l);
  } while (!(k == int_base));

  // Dump regions 5 and 6 of eqtb
  do {
    j = k;

    while (j < eqtb_size)
    {
      if (eqtb[j].cint == eqtb[j + 1].cint)
        goto found2;

      incr(j);
    }

    l = eqtb_size + 1;
    goto done2;

found2:
    incr(j);
    l = j;

    while (j < eqtb_size)
    {
      if (eqtb[j].cint != eqtb[j + 1].cint)
        goto done2;

      incr(j);
    }

done2:
    dump_int(l - k);

    while (k < l)
    {
      dump_wd(eqtb[k]);
      incr(k);
    }

    k = j + 1;
    dump_int(k - l);
  } while (!(k > eqtb_size));

  dump_int(par_loc);
  dump_int(write_loc);

  // Dump the hash table
  dump_int(hash_used);
  cs_count = frozen_control_sequence - 1 - hash_used;

  for (p = hash_base; p <= hash_used; p++)
  {
    if (text(p) != 0)
    {
      dump_int(p);
      dump_hh(hash[p]);
      incr(cs_count);
    }
  }

  for (p = hash_used + 1; p <= undefined_control_sequence - 1; p++)
    dump_hh(hash[p]);

  dump_int(cs_count);
  print_ln();
  print_int(cs_count);
  prints(" multiletter control sequences");

  for (p = 0; p <= prim_size; p++)
    dump_hh(prim[p]);

  // Dump the font information
  dump_int(fmem_ptr);

  for (k = 0; k <= fmem_ptr - 1; k++)
    dump_wd(font_info[k]);

  dump_int(font_ptr);

  {
    dump_things(font_dir[null_font], font_ptr + 1);
    dump_things(font_enc[null_font], font_ptr + 1);
    dump_things(font_num_ext[null_font], font_ptr + 1);
    dump_things(font_check[null_font], font_ptr + 1);
    dump_things(font_size[null_font], font_ptr + 1);
    dump_things(font_dsize[null_font], font_ptr + 1);
    dump_things(font_params[null_font], font_ptr + 1);
    dump_things(hyphen_char[null_font], font_ptr + 1);
    dump_things(skew_char[null_font], font_ptr + 1);
    dump_things(font_name[null_font], font_ptr + 1);
    dump_things(font_area[null_font], font_ptr + 1);
    dump_things(font_bc[null_font], font_ptr + 1);
    dump_things(font_ec[null_font], font_ptr + 1);
    dump_things(ctype_base[null_font], font_ptr + 1);
    dump_things(char_base[null_font], font_ptr + 1);
    dump_things(width_base[null_font], font_ptr + 1);
    dump_things(height_base[null_font], font_ptr + 1);
    dump_things(depth_base[null_font], font_ptr + 1);
    dump_things(italic_base[null_font], font_ptr + 1);
    dump_things(lig_kern_base[null_font], font_ptr + 1);
    dump_things(kern_base[null_font], font_ptr + 1);
    dump_things(exten_base[null_font], font_ptr + 1);
    dump_things(param_base[null_font], font_ptr + 1);
    dump_things(font_glue[null_font], font_ptr + 1);
    dump_things(bchar_label[null_font], font_ptr + 1);
    dump_things(font_bchar[null_font], font_ptr + 1);
    dump_things(font_false_bchar[null_font], font_ptr + 1);

    for (k = 0; k <= font_ptr; k++)
    {
      print_nl("\\font");
      sprint_esc(font_id_text(k));
      print_char('=');
      print_file_name(font_name[k], font_area[k], 335);

      if (font_size[k] != font_dsize[k])
      {
        prints(" at ");
        print_scaled(font_size[k]);
        prints("pt");
      }
    }
  }

  print_ln();
  print_int(fmem_ptr - 7);
  prints(" words of font info for ");
  print_int(font_ptr - font_base);
  prints(" preloaded font");

  if (font_ptr != font_base + 1)
    print_char('s');

  // Dump the hyphenation tables
  dump_int(hyph_count);

  for (k = 0; k <= hyphen_prime; k++)
  {
    if (hyph_word[k] != 0)
    {
      dump_int(k);
      dump_int(hyph_word[k]);
      dump_int(hyph_list[k]);
    }
  }

  print_ln();
  print_int(hyph_count);
  prints(" hyphenation exception");

  if (hyph_count != 1)
    print_char('s');

  if (trie_not_ready)
    init_trie();

  dump_int(trie_max);
  dump_int(hyph_start);
  dump_things(trie_trl[0], trie_max + 1);
  dump_things(trie_tro[0], trie_max + 1);
  dump_things(trie_trc[0], trie_max + 1);
  dump_int(trie_op_ptr);
  dump_things(hyf_distance[1], trie_op_ptr);
  dump_things(hyf_num[1], trie_op_ptr);
  dump_things(hyf_next[1], trie_op_ptr);
  print_nl("Hyphenation trie of length ");
  print_int(trie_max);
  prints(" has ");
  print_int(trie_op_ptr);
  prints(" op");

  if (trie_op_ptr != 1)
    print_char('s');

  prints(" out of ");
  print_int(trie_op_size);

  for (k = 255; k >= 0; k--)
  {
    if (trie_used[k] > 0)
    {
      print_nl("  ");
      print_int(trie_used[k]);
      prints(" for language ");
      print_int(k);
      dump_int(k);
      dump_int(trie_used[k]);
    }
  }

  // Dump a couple more things and the closing check word
  dump_int(interaction);
  dump_int(format_ident);
  dump_int(ENDFMTCHECKSUM);
  tracing_stats = 0;
  w_close(fmt_file);
}

/* sec 01336 */
static void init_prim (void)
{
  no_new_control_sequence = false;
  first = 0;
  /* sec 0266 */
  primitive("lineskip", assign_glue, glue_base + line_skip_code);
  primitive("baselineskip", assign_glue, glue_base + baseline_skip_code);
  primitive("parskip", assign_glue, glue_base + par_skip_code);
  primitive("abovedisplayskip", assign_glue, glue_base + above_display_skip_code);
  primitive("belowdisplayskip", assign_glue, glue_base + below_display_skip_code);
  primitive("abovedisplayshortskip", assign_glue, glue_base + above_display_short_skip_code);
  primitive("belowdisplayshortskip", assign_glue, glue_base + below_display_short_skip_code);
  primitive("leftskip", assign_glue, glue_base + left_skip_code);
  primitive("rightskip", assign_glue, glue_base + right_skip_code);
  primitive("topskip", assign_glue, glue_base + top_skip_code);
  primitive("splittopskip", assign_glue, glue_base + split_top_skip_code);
  primitive("tabskip", assign_glue, glue_base + tab_skip_code);
  primitive("spaceskip", assign_glue, glue_base + space_skip_code);
  primitive("xspaceskip", assign_glue, glue_base + xspace_skip_code);
  primitive("parfillskip", assign_glue, glue_base + par_fill_skip_code);
  primitive("kanjiskip", assign_glue, glue_base + kanji_skip_code);
  primitive("xkanjiskip", assign_glue, glue_base + xkanji_skip_code);
  primitive("thinmuskip", assign_mu_glue, glue_base + thin_mu_skip_code);
  primitive("medmuskip", assign_mu_glue, glue_base + med_mu_skip_code);
  primitive("thickmuskip", assign_mu_glue, glue_base + thick_mu_skip_code);
  /* sec 0230 */
  primitive("output", assign_toks, output_routine_loc);
  primitive("everypar", assign_toks, every_par_loc);
  primitive("everymath", assign_toks, every_math_loc);
  primitive("everydisplay", assign_toks, every_display_loc);
  primitive("everyhbox", assign_toks, every_hbox_loc);
  primitive("everyvbox", assign_toks, every_vbox_loc);
  primitive("everyjob", assign_toks, every_job_loc);
  primitive("everycr", assign_toks, every_cr_loc);
  primitive("errhelp", assign_toks, err_help_loc);
  /* sec 0238 */
  primitive("pretolerance", assign_int, int_base + pretolerance_code);
  primitive("tolerance", assign_int, int_base + tolerance_code);
  primitive("linepenalty", assign_int, int_base + line_penalty_code);
  primitive("hyphenpenalty", assign_int, int_base + hyphen_penalty_code);
  primitive("exhyphenpenalty", assign_int, int_base + ex_hyphen_penalty_code);
  primitive("clubpenalty", assign_int, int_base + club_penalty_code);
  primitive("widowpenalty", assign_int, int_base + widow_penalty_code);
  primitive("displaywidowpenalty", assign_int, int_base + display_widow_penalty_code);
  primitive("brokenpenalty", assign_int, int_base + broken_penalty_code);
  primitive("binoppenalty", assign_int, int_base + bin_op_penalty_code);
  primitive("relpenalty", assign_int, int_base + rel_penalty_code);
  primitive("predisplaypenalty", assign_int, int_base + pre_display_penalty_code);
  primitive("postdisplaypenalty", assign_int, int_base + post_display_penalty_code);
  primitive("interlinepenalty", assign_int, int_base + inter_line_penalty_code);
  primitive("doublehyphendemerits", assign_int, int_base + double_hyphen_demerits_code);
  primitive("finalhyphendemerits", assign_int, int_base + final_hyphen_demerits_code);
  primitive("adjdemerits", assign_int, int_base + adj_demerits_code);
  primitive("mag", assign_int, int_base + mag_code);
  primitive("delimiterfactor", assign_int, int_base + delimiter_factor_code);
  primitive("looseness", assign_int, int_base + looseness_code);
  primitive("time", assign_int, int_base + time_code);
  primitive("day", assign_int, int_base + day_code);
  primitive("month", assign_int, int_base + month_code);
  primitive("year", assign_int, int_base + year_code);
  primitive("showboxbreadth", assign_int, int_base + show_box_breadth_code);
  primitive("showboxdepth", assign_int, int_base + show_box_depth_code);
  primitive("hbadness", assign_int, int_base + hbadness_code);
  primitive("vbadness", assign_int, int_base + vbadness_code);
  primitive("pausing", assign_int, int_base + pausing_code);
  primitive("tracingonline", assign_int, int_base + tracing_online_code);
  primitive("tracingmacros", assign_int, int_base + tracing_macros_code);
  primitive("tracingstats", assign_int, int_base + tracing_stats_code);
  primitive("tracingparagraphs", assign_int, int_base + tracing_paragraphs_code);
  primitive("tracingpages", assign_int, int_base + tracing_pages_code);
  primitive("tracingoutput", assign_int, int_base + tracing_output_code);
  primitive("tracinglostchars", assign_int, int_base + tracing_lost_chars_code);
  primitive("tracingcommands", assign_int, int_base + tracing_commands_code);
  primitive("tracingrestores", assign_int, int_base + tracing_restores_code);
  primitive("tracingfontloaders", assign_int, int_base + tracing_fontloaders_code);
  primitive("uchyph", assign_int, int_base + uc_hyph_code);
  primitive("outputpenalty", assign_int, int_base + output_penalty_code);
  primitive("maxdeadcycles", assign_int, int_base + max_dead_cycles_code);
  primitive("hangafter", assign_int, int_base + hang_after_code);
  primitive("floatingpenalty", assign_int, int_base + floating_penalty_code);
  primitive("globaldefs", assign_int, int_base + global_defs_code);
  primitive("fam", assign_int, int_base + cur_fam_code);
  primitive("jfam", assign_int, int_base + cur_jfam_code);
  primitive("escapechar", assign_int, int_base + escape_char_code);
  primitive("defaulthyphenchar", assign_int, int_base + default_hyphen_char_code);
  primitive("defaultskewchar", assign_int, int_base + default_skew_char_code);
  primitive("endlinechar", assign_int, int_base + end_line_char_code);
  primitive("newlinechar", assign_int, int_base + new_line_char_code);
  primitive("language", assign_int, int_base + language_code);
  primitive("lefthyphenmin", assign_int, int_base + left_hyphen_min_code);
  primitive("righthyphenmin", assign_int, int_base + right_hyphen_min_code);
  primitive("holdinginserts", assign_int, int_base + holding_inserts_code);
  primitive("errorcontextlines", assign_int, int_base + error_context_lines_code);
  primitive("jcharwidowpenalty", assign_int, int_base + jchr_widow_penalty_code);
  primitive("textbaselineshiftfactor", assign_int, int_base + text_baseline_shift_factor_code);
  primitive("scriptbaselineshiftfactor", assign_int, int_base + script_baseline_shift_factor_code);
  primitive("scriptscriptbaselineshiftfactor", assign_int, int_base + scriptscript_baseline_shift_factor_code);
  primitive("ptexlineendmode", assign_int, int_base + ptex_lineend_code);
  primitive("ptextracingfonts", assign_int, int_base + ptex_tracing_fonts_code);
  primitive("pdfcompresslevel", assign_int, int_base + pdf_compress_level_code);
  primitive("pdfmajorversion", assign_int, int_base + pdf_major_version_code);
  primitive("pdfminorversion", assign_int, int_base + pdf_minor_version_code);
  primitive("synctex", assign_int, int_base + synctex_code);
  primitive("tracingstacklevels", assign_int, int_base + tracing_stack_levels_code);
  primitive("partokenname", partoken_name, 0);
  primitive("partokencontext", assign_int, int_base + partoken_context_code);
  primitive("showstream", assign_int, int_base + show_stream_code);
  /* sec 0248 */
  primitive("parindent", assign_dimen, dimen_base + par_indent_code);
  primitive("mathsurround", assign_dimen, dimen_base + math_surround_code);
  primitive("lineskiplimit", assign_dimen, dimen_base + line_skip_limit_code);
  primitive("hsize", assign_dimen, dimen_base + hsize_code);
  primitive("vsize", assign_dimen, dimen_base + vsize_code);
  primitive("maxdepth", assign_dimen, dimen_base + max_depth_code);
  primitive("splitmaxdepth", assign_dimen, dimen_base + split_max_depth_code);
  primitive("boxmaxdepth", assign_dimen, dimen_base + box_max_depth_code);
  primitive("hfuzz", assign_dimen, dimen_base + hfuzz_code);
  primitive("vfuzz", assign_dimen, dimen_base + vfuzz_code);
  primitive("delimitershortfall", assign_dimen, dimen_base + delimiter_shortfall_code);
  primitive("nulldelimiterspace", assign_dimen, dimen_base + null_delimiter_space_code);
  primitive("scriptspace", assign_dimen, dimen_base + script_space_code);
  primitive("predisplaysize", assign_dimen, dimen_base + pre_display_size_code);
  primitive("displaywidth", assign_dimen, dimen_base + display_width_code);
  primitive("displayindent", assign_dimen, dimen_base + display_indent_code);
  primitive("overfullrule", assign_dimen, dimen_base + overfull_rule_code);
  primitive("hangindent", assign_dimen, dimen_base + hang_indent_code);
  primitive("hoffset", assign_dimen, dimen_base + h_offset_code);
  primitive("voffset", assign_dimen, dimen_base + v_offset_code);
  primitive("tbaselineshift", assign_dimen, dimen_base + t_baseline_shift_code);
  primitive("ybaselineshift", assign_dimen, dimen_base + y_baseline_shift_code);
  primitive("pdfhorigin", assign_dimen, dimen_base + pdf_h_origin_code);
  primitive("pdfvorigin", assign_dimen, dimen_base + pdf_v_origin_code);
  primitive("pdfpagewidth", assign_dimen, dimen_base + pdf_page_width_code);
  primitive("pdfpageheight", assign_dimen, dimen_base + pdf_page_height_code);
  primitive("emergencystretch", assign_dimen, dimen_base + emergency_stretch_code);
  primitive(" ", ex_space, 0);
  primitive("/", ital_corr, 0);
  primitive("accent", accent, 0);
  primitive("advance", advance, 0);
  primitive("afterassignment", after_assignment, 0);
  primitive("aftergroup", after_group, 0);
  primitive("begingroup", begin_group, 0);
  primitive("char", char_num, 0);
  primitive("kchar", kchar_num, 0);
  primitive("csname", cs_name, 0);
  primitive("delimiter", delim_num, 0);
  primitive("divide", divide, 0);
  primitive("endcsname", end_cs_name, 0);
  primitive("endgroup", end_group, 0);
  text(frozen_end_group) = make_str_string("endgroup");
  eqtb[frozen_end_group] = eqtb[cur_val]; 
  primitive("expandafter", expand_after, 0);
  primitive("font", def_font, 0);
  primitive("jfont", def_jfont, 0);
  primitive("tfont", def_tfont, 0);
  primitive("fontdimen", assign_font_dimen, 0);
  primitive("halign", halign, 0);
  primitive("hrule", hrule, 0);
  primitive("ignorespaces", ignore_spaces, 0);
  primitive("insert", insert, 0);
  primitive("mark", mark, 0);
  primitive("mathaccent", math_accent, 0);
  primitive("mathchar", math_char_num, 0);
  primitive("mathchoice", math_choice, 0);
  primitive("multiply", multiply, 0);
  primitive("noalign", no_align, 0);
  primitive("noboundary", no_boundary, 0);
  primitive("noexpand", no_expand, 0);
  primitive("pdfprimitive", no_expand, 1);
  primitive("nonscript", non_script, 0);
  primitive("omit", omit, 0);
  primitive("parshape", set_shape, par_shape_loc);
  primitive("penalty", break_penalty, 0);
  primitive("prevgraf", set_prev_graf, 0);
  primitive("radical", radical, 0);
  primitive("read", read_to_cs, 0);
  primitive("relax", relax, 256);
  text(frozen_relax) = make_str_string("relax");
  eqtb[frozen_relax] = eqtb[cur_val];
  primitive("setbox", set_box, 0);
  primitive("the", the, 0);
  primitive("toks", toks_register, mem_bot);
  primitive("vadjust", vadjust, 0);
  primitive("valign", valign, 0);
  primitive("vcenter", vcenter, 0);
  primitive("vrule", vrule, 0);
  primitive("par", par_end, 256);
  par_loc = cur_val; 
  par_token = cs_token_flag + par_loc;
  primitive("input", input, 0);
  primitive("endinput", input, 1);
  primitive("topmark", top_bot_mark, top_mark_code);
  primitive("firstmark", top_bot_mark, first_mark_code);
  primitive("botmark", top_bot_mark, bot_mark_code);
  primitive("splitfirstmark", top_bot_mark, split_first_mark_code);
  primitive("splitbotmark", top_bot_mark, split_bot_mark_code);
  primitive("count", tex_register, mem_bot + int_val);
  primitive("dimen", tex_register, mem_bot + dimen_val);
  primitive("skip", tex_register, mem_bot + glue_val);
  primitive("muskip", tex_register, mem_bot + mu_val);
  primitive("spacefactor", set_aux, hmode);
  primitive("prevdepth", set_aux, vmode);
  primitive("deadcycles", set_page_int, 0);
  primitive("insertpenalties", set_page_int, 1);
  primitive("wd", set_box_dimen, width_offset);
  primitive("ht", set_box_dimen, height_offset);
  primitive("dp", set_box_dimen, depth_offset);
  primitive("lastpenalty", last_item, int_val);
  primitive("lastkern", last_item, dimen_val);
  primitive("lastskip", last_item, glue_val);
  primitive("inputlineno", last_item, input_line_no_code);
  primitive("badness", last_item, badness_code);
  primitive("shellescape", last_item, shell_escape_code);
  primitive("pdfshellescape", last_item, shell_escape_code);
  primitive("ifpdfprimitive", if_test, if_pdfprimitive_code);
  primitive("ptexversion", last_item, ptex_version_code);
  primitive("uptexversion", last_item, uptex_version_code);
  primitive("epTeXversion", last_item, eptex_version_code);
  primitive("ptexminorversion", last_item, ptex_minor_version_code);
  primitive("pdflastxpos", last_item, pdf_last_x_pos_code);
  primitive("pdflastypos", last_item, pdf_last_y_pos_code);
  primitive("pdfelapsedtime", last_item, elapsed_time_code);
  primitive("pdfrandomseed", last_item, random_seed_code);
  primitive("number", convert, number_code);
  primitive("romannumeral", convert, roman_numeral_code);
  primitive("kansuji", convert, kansuji_code);
  primitive("string", convert, string_code);
  primitive("meaning", convert, meaning_code);
  primitive("fontname", convert, font_name_code);
  primitive("euc", convert, euc_code);
  primitive("sjis", convert, sjis_code);
  primitive("jis", convert, jis_code);
  primitive("kuten", convert, kuten_code);
  primitive("ptexrevision", convert, ptex_revision_code);
  primitive("uptexrevision", convert, uptex_revision_code);
  primitive("ucs", convert, ucs_code);
  primitive("toucs", convert, toucs_code);
  primitive("tojis", convert, tojis_code);
  primitive("ptexfontname", convert, ptex_font_name_code);
  primitive("pdfstrcmp", convert, ng_strcmp_code);
  primitive("ngbanner", convert, ng_banner_code);
  primitive("ngostype", convert, ng_os_type_code);
  primitive("pdfcreationdate", convert, pdf_creation_date_code);
  primitive("pdffilemoddate", convert, pdf_file_mod_date_code);
  primitive("pdffilesize", convert, pdf_file_size_code);
  primitive("pdfmdfivesum", convert, pdf_mdfive_sum_code);
  primitive("pdffiledump", convert, pdf_file_dump_code);
  primitive("pdfuniformdeviate", convert, pdf_uniform_deviate_code);
  primitive("pdfnormaldeviate", convert, pdf_normal_deviate_code);
  primitive("expanded", convert, expanded_code);
  primitive("Uchar", convert, Uchar_convert_code);
  primitive("Ucharcat", convert, Ucharcat_convert_code);
  primitive("jobname", convert, job_name_code);
  primitive("if", if_test, if_char_code);
  primitive("ifcat", if_test, if_cat_code);
  primitive("ifnum", if_test, if_int_code);
  primitive("ifdim", if_test, if_dim_code);
  primitive("ifodd", if_test, if_odd_code);
  primitive("ifvmode", if_test, if_vmode_code);
  primitive("ifhmode", if_test, if_hmode_code);
  primitive("ifmmode", if_test, if_mmode_code);
  primitive("ifinner", if_test, if_inner_code);
  primitive("ifvoid", if_test, if_void_code);
  primitive("ifhbox", if_test, if_hbox_code);
  primitive("ifvbox", if_test, if_vbox_code);
  primitive("ifx", if_test, ifx_code);
  primitive("ifeof", if_test, if_eof_code);
  primitive("iftrue", if_test, if_true_code);
  primitive("iffalse", if_test, if_false_code);
  primitive("ifcase", if_test, if_case_code);
  primitive("iftdir", if_test, if_tdir_code);
  primitive("ifydir", if_test, if_ydir_code);
  primitive("ifddir", if_test, if_ddir_code);
  primitive("ifmdir", if_test, if_mdir_code);
  primitive("iftbox", if_test, if_tbox_code);
  primitive("ifybox", if_test, if_ybox_code);
  primitive("ifdbox", if_test, if_dbox_code);
  primitive("ifmbox", if_test, if_mbox_code);
  primitive("ifjfont", if_test, if_jfont_code);
  primitive("iftfont", if_test, if_tfont_code);
  primitive("fi", fi_or_else, fi_code);
  text(frozen_fi) = make_str_string("fi");
  eqtb[frozen_fi] = eqtb[cur_val];
  primitive("or", fi_or_else, or_code);
  primitive("else", fi_or_else, else_code);
  primitive("nullfont", set_font, null_font);
  text(frozen_null_font) = 795;
  eqtb[frozen_null_font] = eqtb[cur_val];
  primitive("span", tab_mark, span_code);
  primitive("cr", car_ret, cr_code);
  text(frozen_cr) = make_str_string("cr");
  eqtb[frozen_cr] = eqtb[cur_val];
  primitive("crcr", car_ret, cr_cr_code);
  text(frozen_end_template) = make_str_string("endtemplate");
  text(frozen_endv) = make_str_string("endtemplate");
  eq_type(frozen_endv) = endv;
  equiv(frozen_endv) = null_list; 
  eq_level(frozen_endv) = level_one; 
  eqtb[frozen_end_template] = eqtb[frozen_endv]; 
  eq_type(frozen_end_template) = end_template;
  primitive("pagegoal", set_page_dimen, 0);
  primitive("pagetotal", set_page_dimen, 1);
  primitive("pagestretch", set_page_dimen, 2);
  primitive("pagefilstretch", set_page_dimen, 3);
  primitive("pagefillstretch", set_page_dimen, 4);
  primitive("pagefilllstretch", set_page_dimen, 5);
  primitive("pageshrink", set_page_dimen, 6);
  primitive("pagedepth", set_page_dimen, 7);
  primitive("end", stop, 0);
  primitive("dump", stop, 1);
  primitive("hskip", hskip, skip_code);
  primitive("hfil", hskip, fil_code);
  primitive("hfill", hskip, fill_code);
  primitive("hss", hskip, ss_code);
  primitive("hfilneg", hskip, fil_neg_code);
  primitive("vskip", vskip, skip_code);
  primitive("vfil", vskip, fil_code);
  primitive("vfill", vskip, fill_code);
  primitive("vss", vskip, ss_code);
  primitive("vfilneg", vskip, fil_neg_code);
  primitive("mskip", mskip, mskip_code);
  primitive("kern", kern, explicit);
  primitive("mkern", mkern, mu_glue);
  primitive("moveleft", hmove, 1);
  primitive("moveright", hmove, 0);
  primitive("raise", vmove, 1);
  primitive("lower", vmove, 0);
  primitive("box", make_box, box_code);
  primitive("copy", make_box, copy_code);
  primitive("lastbox", make_box, last_box_code);
  primitive("vsplit", make_box, vsplit_code);
  primitive("vtop", make_box, vtop_code);
  primitive("vbox", make_box, vtop_code + vmode);
  primitive("hbox", make_box, vtop_code + hmode);
  primitive("tate", chg_dir, dir_tate);
  primitive("yoko", chg_dir, dir_yoko);
  primitive("dtou", chg_dir, dir_dtou);
  primitive("shipout", leader_ship, a_leaders - 1);
  primitive("leaders", leader_ship, a_leaders);
  primitive("cleaders", leader_ship, c_leaders);
  primitive("xleaders", leader_ship, x_leaders);
  primitive("indent", start_par, 1);
  primitive("noindent", start_par, 0);
  primitive("quitvmode", start_par, 2);
  primitive("unpenalty", remove_item, penalty_node);
  primitive("unkern", remove_item, kern_node);
  primitive("unskip", remove_item, glue_node);
  primitive("unhbox", un_hbox, box_code);
  primitive("unhcopy", un_hbox, copy_code);
  primitive("unvbox", un_vbox, box_code);
  primitive("unvcopy", un_vbox, copy_code);
  primitive("-", discretionary, 1);
  primitive("discretionary", discretionary, 0);
  primitive("eqno", eq_no, 0);
  primitive("leqno", eq_no, 1);
  primitive("mathord", math_comp, ord_noad);
  primitive("mathop", math_comp, op_noad);
  primitive("mathbin", math_comp, bin_noad);
  primitive("mathrel", math_comp, rel_noad);
  primitive("mathopen", math_comp, open_noad);
  primitive("mathclose", math_comp, close_noad);
  primitive("mathpunct", math_comp, punct_noad);
  primitive("mathinner", math_comp, inner_noad);
  primitive("underline", math_comp, under_noad);
  primitive("overline", math_comp, over_noad);
  primitive("displaylimits", limit_switch, normal);
  primitive("limits", limit_switch, limits);
  primitive("nolimits", limit_switch, no_limits);
  primitive("displaystyle", math_style, display_style);
  primitive("textstyle", math_style, text_style);
  primitive("scriptstyle", math_style, script_style);
  primitive("scriptscriptstyle", math_style, script_script_style);
  primitive("above", above, above_code);
  primitive("over", above, over_code);
  primitive("atop", above, atop_code);
  primitive("abovewithdelims", above, delimited_code + above_code);
  primitive("overwithdelims", above, delimited_code + over_code);
  primitive("atopwithdelims", above, delimited_code + atop_code);
  primitive("left", left_right, left_noad);
  primitive("right", left_right, right_noad);
  text(frozen_right) = make_str_string("right");
  eqtb[frozen_right] = eqtb[cur_val]; 
  primitive("long", prefix, 1);
  primitive("outer", prefix, 2);
  primitive("global", prefix, 4);
  primitive("def", def, 0);
  primitive("gdef", def, 1);
  primitive("edef", def, 2);
  primitive("xdef", def, 3);
  primitive("let", let, normal);
  primitive("futurelet", let, normal + 1);
  primitive("chardef", shorthand_def, char_def_code);
  primitive("kchardef", shorthand_def, kchar_def_code);
  primitive("mathchardef", shorthand_def, math_char_def_code);
  primitive("countdef", shorthand_def, count_def_code);
  primitive("dimendef", shorthand_def, dimen_def_code);
  primitive("skipdef", shorthand_def, skip_def_code);
  primitive("muskipdef", shorthand_def, mu_skip_def_code);
  primitive("toksdef", shorthand_def, toks_def_code);
  primitive("catcode", def_code, cat_code_base);
  primitive("kcatcode", def_code, kcat_code_base);
  primitive("xspcode", def_code, auto_xsp_code_base);
  primitive("mathcode", def_code, math_code_base);
  primitive("lccode", def_code, lc_code_base);
  primitive("uccode", def_code, uc_code_base);
  primitive("sfcode", def_code, sf_code_base);
  primitive("delcode", def_code, del_code_base);
  primitive("textfont", def_family, math_font_base);
  primitive("scriptfont", def_family, math_font_base + script_size);
  primitive("scriptscriptfont", def_family, math_font_base + script_script_size);
  primitive("hyphenation", hyph_data, 0);
  primitive("patterns", hyph_data, 1);
  primitive("hyphenchar", assign_font_int, 0);
  primitive("skewchar", assign_font_int, 1);
  primitive("batchmode", set_interaction, batch_mode);
  primitive("nonstopmode", set_interaction, nonstop_mode);
  primitive("scrollmode", set_interaction, scroll_mode);
  primitive("errorstopmode", set_interaction, error_stop_mode);
  primitive("openin", in_stream, 1);
  primitive("closein", in_stream, 0);
  primitive("message", message, 0);
  primitive("errmessage", message, 1);
  primitive("lowercase", case_shift, lc_code_base);
  primitive("uppercase", case_shift, uc_code_base);
  primitive("show", xray, show_code);
  primitive("showbox", xray, show_box_code);
  primitive("showthe", xray, show_the_code);
  primitive("showlists", xray, show_lists_code);
  primitive("showmode", xray, show_mode);
  primitive("openout", extension, open_node);
  primitive("write", extension, write_node);
  write_loc = cur_val;
  primitive("closeout", extension, close_node);
  primitive("special", extension, special_node);
  primitive("immediate", extension, immediate_code);
  primitive("setlanguage", extension, set_language_code);
  primitive("pdfsavepos", extension, pdf_save_pos_node);
  primitive("pdfresettimer", extension, reset_timer_code);
  primitive("pdfsetrandomseed", extension, set_random_seed_code);
  primitive("kansujichar", set_kansuji_char, 0);
  primitive("autospacing", set_auto_spacing, set_auto_spacing_code);
  primitive("noautospacing", set_auto_spacing, reset_auto_spacing_code);
  primitive("autoxspacing", set_auto_spacing, set_auto_xspacing_code);
  primitive("noautoxspacing", set_auto_spacing, reset_auto_xspacing_code);
  primitive("enablecjktoken", set_enable_cjk_token, reset_enable_cjk_token_code);
  primitive("disablecjktoken", set_enable_cjk_token, set_enable_cjk_token_code);
  primitive("forcecjktoken", set_enable_cjk_token, set_force_cjk_token_code);
  primitive("inhibitglue", inhibit_glue, 0);
  primitive("disinhibitglue", inhibit_glue, 1);
  primitive("inhibitxspcode", assign_inhibit_xsp_code, inhibit_xsp_code_base);
  primitive("prebreakpenalty", assign_kinsoku, pre_break_penalty_code);
  primitive("postbreakpenalty", assign_kinsoku, post_break_penalty_code);
  no_new_control_sequence = true; 
}
#endif

// prints an end-of-line
void print_ln (void)
{
  integer ii;

  switch (selector)
  {
    case term_and_log:
      {
        if (nrestmultichr(kcode_pos) > 0)
          for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
          {
            wterm(' ');
            wlog(' ');
          }

        wterm_cr();
        wlog_cr();
        term_offset = 0;
        file_offset = 0;
      }
      break;

    case log_only:
      {
        if (nrestmultichr(kcode_pos) > 0)
          for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
            wlog(' ');

        wlog_cr();
        file_offset = 0;
      }
      break;

    case term_only:
      {
        if (nrestmultichr(kcode_pos) > 0)
          for (ii = 0; ii <= nrestmultichr(kcode_pos) - 1; ii++)
            wterm(' ');

        wterm_cr();
        term_offset = 0;
      }
      break;

    case no_print:
    case pseudo:
    case new_string:
      do_nothing();
      break;

    default:
      write_ln(write_file[selector]);
      break;
  }

  kcode_pos = 0;
  // {|tally| is not affected}
}

// prints a single character
void print_char (ASCII_code s)
{
  if (s == new_line_char)
  {
    if (selector < pseudo)
    {
      print_ln();
      return;
    }
  }

  if ((kcode_pos == 1) || ((kcode_pos >= 011) && (kcode_pos <= 012))
    || ((kcode_pos >= 021) && (kcode_pos <= 023)))
    incr(kcode_pos);
  else if (iskanji1(xchr[s]))
  {
    if (ismultichr(4, 1, xchr[s]))
      kcode_pos = 021;
    else if (ismultichr(3, 1, xchr[s]))
      kcode_pos = 011;
    else
      kcode_pos = 1;

    if ((selector == term_and_log) || (selector == log_only))
    {
      if (file_offset >= max_print_line - nrestmultichr(kcode_pos))
      {
        wlog_cr();
        file_offset = 0;
      }
    }

    if ((selector == term_and_log) || (selector == term_only))
    {
      if (term_offset >= max_print_line - nrestmultichr(kcode_pos))
      {
        wterm_cr();
        term_offset = 0;
      }
    }
  }
  else
    kcode_pos = 0;

  switch (selector)
  {
    case term_and_log:
      {
        wterm(xchr[s]);
        incr(term_offset);

        if (term_offset == max_print_line)
        {
          wterm_cr();
          term_offset = 0;
        }

        wlog(xchr[s]);
        incr(file_offset);

        if (file_offset == max_print_line)
        {
          wlog_cr();
          file_offset = 0;
        }
      }
      break;

    case log_only:
      {
        wlog(xchr[s]);
        incr(file_offset);

        if (file_offset == max_print_line)
          print_ln();
      }
      break;

    case term_only:
      {
        wterm(xchr[s]);
        incr(term_offset);

        if (term_offset == max_print_line)
          print_ln();
      }
      break;

    case no_print:
      do_nothing();
      break;

    case pseudo:
      if (tally < trick_count)
      {
        trick_buf[tally % error_line] = s;
        trick_buf2[tally % error_line] = kcode_pos;
      }
      break;

    case new_string:
#ifdef APTEX_EXTENSION
      if (pool_ptr + 1 > current_pool_size)
        str_pool = realloc_str_pool(increment_pool_size);

      if (pool_ptr < current_pool_size)
        append_char(s);
#else
      if (pool_ptr < pool_size)
        append_char(s);
      // {we drop characters if the string space is full}
#endif
      break;

    default:
      fputc(xchr[s], write_file[selector].file_data);
      break;
  }

  incr(tally);
}

// prints string |s|
void print_ (integer s)
{
  pool_pointer j; // {current character code position}
  integer nl;     // {new-line character to restore}

  if (s >= str_ptr)
    s = 259; // '???' {this can't happen}
  else
  {
    if (s < 256)
    {
      if (s < 0)
        s = 259;
      else
      {
        if (selector > pseudo)
        {
          print_char(s); // {internal strings are not expanded}
          return;
        }

        if (s == new_line_char)
        {
          if (selector < pseudo)
          {
            print_ln();
            return;
          }
        }

        nl = new_line_char;
        new_line_char = -1; // {temporarily disable new-line character}
        j = str_start[s];

        while (j < str_start[s + 1])
        {
          print_char(str_pool[j]);
          incr(j);
        }

        new_line_char = nl;
        return;
      }
    }
  }

  j = str_start[s];

  while (j < str_start[s + 1])
  {
    print_char(str_pool[j]);
    incr(j);
  }
}

// string version print.
void prints_ (const char * s)
{
  while (*s)
    print_char(*s++);
}

// prints string |s|
void slow_print (integer s)
{
  pool_pointer j; // {current character code position}

  if ((s >= str_ptr) || (s < 256))
    print(s);
  else
  {
    j = str_start[s];

    while (j < str_start[s + 1])
    {
      print(str_pool[j]);
      incr(j);
    }
  }
}

// prints string |s| at beginning of line
void print_nl (const char * s)
{
  if ((selector < no_print) || ((term_offset > 0) && (odd(selector))) ||
      ((file_offset > 0) && (selector >= log_only)))
    print_ln();

  prints(s);
}

// prints escape character, then |s|
void print_esc (const char * s)
{
  integer c; // {the escape character code}

  c = escape_char;

  if (c >= 0)
  {
    if (c < 256)
      print(c);
  }

  prints(s);
}

void sprint_esc (str_number s)
{
  integer c; // {the escape character code}

  c = escape_char;

  if (c >= 0)
  {
    if (c < 256)
      print(c);
  }

  print(s);
}

// prints |dig[k-1]|$\,\ldots\,$|dig[0]|
static void print_the_digs (eight_bits k)
{
  while (k > 0)
  {
    decr(k);

    if (dig[k] < 10)
      print_char('0' + dig[k]);
    else
      print_char('A' - 10 + dig[k]);
  }
}

// prints an integer in decimal form
void print_int (integer n)
{
  uint32_t k;     // var k:0..23; {index to current digit; we assume that $|n|<10^{23}$}
  integer m;  // {used to negate |n| in possibly dangerous cases}

  k = 0;

  if (n < 0)
  {
    print_char('-');

    if (n > -100000000)
      negate(n);
    else
    {
      m = -1 - n;
      n = m / 10;
      m = (m % 10) + 1;
      k = 1;

      if (m < 10)
        dig[0] = m;
      else
      {
        dig[0] = 0;
        incr(n);
      }
    }
  }

  do {
    dig[k] = (n % 10);
    n = n / 10;
    incr(k);
  } while (!(n == 0));

  print_the_digs(k);
}

static pointer prim_lookup(str_number s) //{search the primitives table}
{
 integer h; // {hash code}
 pointer p; // {index in |hash| array}
 pointer k; // {index in string pool}
 integer j, l;

  if (s <= biggest_char)
  {
    if (s < 0)
    {
      p = undefined_primitive;
      goto found;
    }
    else
      p = (s % prim_prime) + prim_base; // {we start searching here}
  }
  else
  {
    j = str_start[s];

    if (s == str_ptr)
      l = cur_length;
    else
      l = length(s);

    // @<Compute the primitive code |h|@>;
    h = str_pool[j];

    for (k = j + 1; k <= j + l - 1; k++)
    {
      h = h + h + str_pool[k];

      while (h >= prim_prime)
        h = h - prim_prime;
    }

    p = h + prim_base; //{we start searching here; note that |0<=h<prim_prime|}
  }

  while (true)
  {
    if (prim_text(p) > 1 + biggest_char) // { |p| points a multi-letter primitive }
    {
      if (length(prim_text(p) - 1) == l)
        if (str_eq_str(prim_text(p) - 1, s))
          goto found;
    }
    else if (prim_text(p) == 1 + s)
      goto found; // { |p| points a single-letter primitive }

    if (prim_next(p) == 0)
    {
      if (no_new_control_sequence)
        p = undefined_primitive;
      else //@<Insert a new primitive after |p|, then make
        //|p| point to it@>;
      {
        if (prim_text(p) > 0)
        {
          do {
            if (prim_is_full)
              overflow("primitive size", prim_size);

            decr(prim_used);
          } while (!(prim_text(prim_used) == 0)); // {search for an empty location in |prim|}

          prim_next(p) = prim_used;
          p = prim_used;
        }

        prim_text(p) = s + 1;
      }

      goto found;
    }

    p = prim_next(p);
  }

found:
  return p;
}

// prints a purported control sequence
static void print_cs (integer p)
{
  pool_pointer j, l;
  uint32_t cat;

  if (p < hash_base)
  {
    // {single character}
    if (p >= single_base)
    {
      if (p == null_cs)
      {
        print_esc("csname");
        print_esc("endcsname");
        print_char(' ');
      }
      else
      {
        sprint_esc(p - single_base);

        if (cat_code(p - single_base) == letter)
          print_char(' ');
      }
    }
    else if (p < active_base)
      print_esc("IMPOSSIBLE.");
    else
      print(p - active_base);
  }
  else if (p >= undefined_control_sequence)
    print_esc("IMPOSSIBLE.");
  else if ((text(p) < 0) || (text(p) >= str_ptr))
    print_esc("NONEXISTENT.");
  else
  {
    if ((p >= prim_eqtb_base) && (p < frozen_null_font))
      l = prim_text(p - prim_eqtb_base) - 1;
    else
      l = text(p);

    sprint_esc(l);
    j = str_start[l];
    l = str_start[l + 1];
    if (l > j + 1)
    {
      if (l - j == multistrlen(str_pool, l, j))
      {
        cat = kcat_code(kcatcodekey(fromBUFF(str_pool, l, j)));
        if (cat != other_kchar)
          print_char(' ');
      }
      else
        print_char(' ');
    }
    else
      print_char(' ');
  }
}

// prints a control sequence
static void sprint_cs (pointer p)
{
  if (p < hash_base)
  {
    if (p < single_base)
      print(p - active_base);
    else if (p < null_cs)
      sprint_esc(p - single_base);
    else
    {
      print_esc("csname");
      print_esc("endcsname");
    }
  }
  else if ((p >= prim_eqtb_base) && (p < frozen_null_font))
    sprint_esc(prim_text(p - prim_eqtb_base) - 1);
  else
    sprint_esc(text(p));
}

void print_file_name (integer n, integer a, integer e)
{
  slow_print(a);
  slow_print(n);
  slow_print(e);
}

static void print_size (integer s)
{
  if (s == text_size)
    print_esc("textfont");
  else if (s == script_size)
    print_esc("scriptfont");
  else
    print_esc("scriptscriptfont");
}

static void print_write_whatsit (const char * s, pointer p)
{
  print_esc(s);

  if (write_stream(p) < 16)
    print_int(write_stream(p));
  else if (write_stream(p) == 16)
    print_char('*');
  else
    print_char('-');
}

// {print register number}
static void print_sa_num (pointer q)
{
  halfword n; // {the register number}

  if (sa_index(q) < dimen_val_limit)
    n = sa_num(q); // {the easy case}
  else
  {
    n = hex_dig4(sa_index(q));
    q = link(q);
    n = n + 16 * sa_index(q);
    q = link(q);
    n = n + 256 * (sa_index(q) + 16 * sa_index(link(q)));
  }

  print_int(n);
}

// prints |dir| data
static void print_dir (eight_bits dir)
{
  if (dir == dir_yoko)
    print_char('Y');
  else if (dir == dir_tate)
    print_char('T');
  else if (dir == dir_dtou)
    print_char('D');
}

//
static void print_direction_alt (integer d)
{
  boolean x;

  x = false;

  switch (abs(d))
  {
    case dir_yoko:
      {
        prints(", yoko");
        x = true;
      }
      break;

    case dir_tate:
      {
        prints(", tate");
        x = true;
      }
      break;

    case dir_dtou:
      {
        prints(", dtou");
        x = true;
      }
      break;
  }

  if (x)
  {
    if (d < 0)
      prints("(math)");

    prints(" direction");
  }
}

// print the direction represented by d
static void print_direction (integer d)
{
  switch (abs(d))
  {
    case dir_yoko:
      prints("yoko");
      break;

    case dir_tate:
      prints("tate");
      break;

    case dir_dtou:
      prints("dtou");
      break;
  }

  if (d < 0)
    prints("(math)");

  prints(" direction");
}

//
static void print_kansuji (integer n)
{
  uint32_t k;         // {index to current digit; we assume that $|n|<10^{23}$}
  KANJI_code cx;  // {temporary register for KANJI}

  k = 0;

  if (n < 0)
    return; // {nonpositive input produces no output}

  do {
    dig[k] = n % 10;
    n = n / 10;
    incr(k);
  } while (!(n == 0));

  while (k > 0)
  {
    decr(k);
    cx = kansuji_char(dig[k]);
    print_kanji(fromDVI(cx));
  }
}

// prints a single character
void print_kanji (KANJI_code s)
{
  if (is_print_utf8)
    s = UCStoUTF8(toUCS(s % max_cjk_val));
  else
    s = toBUFF(s % max_cjk_val);

  if (BYTE1(s) != 0)
    print_char(BYTE1(s));

  if (BYTE2(s) != 0)
    print_char(BYTE2(s));

  if (BYTE3(s) != 0)
    print_char(BYTE3(s));

  print_char(BYTE4(s));
}

// todo: noreturn
void jump_out (void)
{
  close_files_and_terminate();
  aptex_utils_exit(do_final_end());
}

// completes the job of error reporting
void error (void)
{
  ASCII_code c;           // {what the user types}
  integer s1, s2, s3, s4; // {used to save global variables when deleting tokens}

  if (history < error_message_issued)
    history = error_message_issued;

  print_char('.');
  show_context();

  if (interaction == error_stop_mode)
  {
    while (true)
    {
continu:
      if (interaction != error_stop_mode)
        return;
      clear_for_error_prompt();
      prompt_input("? ");

      if (last == first)
        return;

      c = buffer[first];

      if (c >= 'a')
        c = c + 'A' - 'a'; // {convert to uppercase}

      switch (c)
      {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          if (deletions_allowed)
          {
            s1 = cur_tok;
            s2 = cur_cmd;
            s3 = cur_chr;
            s4 = align_state;
            align_state = 1000000;
            OK_to_interrupt = false;

            if ((last > first + 1) && (buffer[first + 1] >= '0') &&
              (buffer[first + 1] <= '9'))
              c = c * 10 + buffer[first + 1] - '0' * 11;
            else
              c = c - '0';

            while (c > 0)
            {
              get_token(); // {one-level recursive call of |error| is possible}
              decr(c);
            }

            cur_tok = s1;
            cur_cmd = s2;
            cur_chr = s3;
            align_state = s4;
            OK_to_interrupt = true;
            help2("I have just deleted some text, as you asked.",
              "You can now delete more, or insert, or whatever.");
            show_context();
            goto continu;
          }
          break;

  #ifdef APTEX_DEBUG
        case 'D':
          {
            debug_help();
            goto continu;
          }
          break;
  #endif

        case 'E':
          if (base_ptr > 0)
          {
            if (input_stack[base_ptr].name_field >= 256)
            {
              print_nl("You want to edit file ");
              slow_print(input_stack[base_ptr].name_field);
              prints(" at line ");
              print_int(line);
              interaction = scroll_mode;
              jump_out();
            }
          }
          break;

        case 'H':
          {
            if (use_err_help)
            {
              give_err_help();
              use_err_help = false;
            }
            else
            {
              if (help_ptr == 0)
                help2("Sorry, I don't know how to help in this situation.",
                  "Maybe you should try asking a human?");

              do {
                decr(help_ptr);
                prints(help_line[help_ptr]);
                print_ln();
              } while (!(help_ptr == 0));
            }

            help4("Sorry, I already gave what help I could...",
              "Maybe you should try asking a human?",
              "An error might have occurred before I noticed any problems.",
              "``If all else fails, read the instructions.''");
            goto continu;
          }
          break;

        case 'I':
          {
            begin_file_reading(); // {enter a new syntactic level for terminal input}

            if (last > first + 1)
            {
              loc = first + 1;
              buffer[first] = ' ';
            }
            else
            {
              prompt_input("insert>");
              loc = first;
            }

            first = last;
            cur_input.limit_field = last - 1; // {no |end_line_char| ends this line}

            return;
          }
          break;

        case 'Q':
        case 'R':
        case 'S':
          {
            error_count = 0;
            interaction = batch_mode + c - 'Q';
            prints("OK, entering ");

            switch (c)
            {
              case 'Q':
                {
                  print_esc("batchmode");
                  decr(selector);
                }
                break;

              case 'R':
                print_esc("nonstopmode");
                break;

              case 'S':
                print_esc("scrollmode");
                break;
            }

            prints("...");
            print_ln();
            update_terminal();

            return;
          }
          break;

        case 'X':
          {
            interaction = scroll_mode;
            jump_out();
          }
          break;

        default:
          do_nothing();
          break;
      }
      // Print the menu of available options
      {
        prints("Type <return> to proceed, S to scroll future error messages,");
        print_nl("R to run without stopping, Q to run quietly,");
        print_nl("I to insert something, ");

        if (base_ptr > 0)
          if (input_stack[base_ptr].name_field >= 256)
            prints("E to edit your file,");

        if (deletions_allowed)
          print_nl("1 or ... or 9 to ignore the next 1 to 9 tokens of input,");

        print_nl("H for help, X to quit.");
      }
      }
  }


  incr(error_count);

  if (error_count == 100)
  {
    print_nl("(That makes 100 errors; please try again.)");
    history = fatal_error_stop;
    jump_out();
  }

  if (interaction > batch_mode)
    decr(selector); // {avoid terminal output}

  if (use_err_help)
  {
    print_ln();
    give_err_help();
  }
  else while (help_ptr > 0)
  {
    decr(help_ptr);
    print_nl(help_line[help_ptr]);
  }

  print_ln();

  if (interaction > batch_mode)
    incr(selector); // {re-enable terminal output}
  
  print_ln();
}

// prints |s|, and that's it
static void fatal_error (const char * s)
{
  normalize_selector();
  print_err("Emergency stop");
  help1(s);
  succumb();
}

// stop due to finiteness
void overflow (const char * s, integer n)
{
  normalize_selector();
  print_err("TeX capacity exceeded, sorry [");
  prints(s);
  print_char('=');
  print_int(n);
  print_char(']');
  help2("If you really absolutely need more capacity,",
      "you can ask a wizard to enlarge me.");

  if (!aptex_env.flag_tex82)
  {
    if (!strcmp(s, "pattern memory") && (n == trie_size))
      printf("\n  (Maybe use -h=... on command line in initex)\n");
    else if (!strcmp(s, "exception dictionary") && (n == hyphen_prime))
      printf("\n  (Maybe use -e=... on command line in initex)\n");
  }

  succumb();
}

// consistency check violated; |s| tells where
static void confusion (const char * s)
{
  normalize_selector();

  if (history < error_message_issued)
  {
    print_err("This can't happen (");
    prints(s);
    print_char(')');
    help1("I'm broken. Please show this to someone who can fix can fix");
  }
  else
  {
    print_err("I can't go on meeting you like this");
    help2("One of your faux pas seems to have wounded me deeply...",
        "in fact, I'm barely conscious. Please fix it and try again.");
  }

  succumb();
}

// gets the terminal input started
boolean init_terminal (void)
{
  t_open_in();

  if (last > first)
  {
    loc = first;

    while ((loc < last) && (buffer[loc] == ' '))
      incr(loc);

    if (loc < last)
      return true;
  }

  while (true)
  {
    wake_up_terminal();
    fputs("**", stdout);
    update_terminal();

    if (!input_ln(term_in, true)) // {this shouldn't happen}
    {
      wterm_cr();
      fputs("! End of file on the terminal... why?", stdout);
      return false;
    }

    loc = first;

    while ((loc < last) && (buffer[loc] == ' '))
      incr(loc);

    if (loc < last)
      return true;

    fputs("Please type the name of your input file.", stdout);
  }
}

// current string enters the pool
str_number make_string (void)
{
#ifdef APTEX_EXTENSION
  if (str_ptr == current_max_strings)
    str_start = realloc_str_start(increment_max_strings);

  if (str_ptr == current_max_strings)
    overflow("number of strings", current_max_strings - init_str_ptr);
#else
  if (str_ptr == max_strings)
    overflow("number of strings", max_strings - init_str_ptr);
#endif

  incr(str_ptr);
  str_start[str_ptr] = pool_ptr;

  return (str_ptr - 1);
}

// test equality of strings
static boolean str_eq_buf (str_number s, integer k)
{
  pool_pointer j; // {running index}
  boolean result; // {result of comparison}

  j = str_start[s];

  while (j < str_start[s + 1])
  {
    if (str_pool[j] != buffer[k])
    {
      result = false;
      goto not_found;
    }

    incr(j);
    incr(k);
  }

  result = true;

not_found:
  return result;
}

// test equality of strings
static boolean str_eq_str (str_number s, str_number t)
{
  pool_pointer j, k;  // {running indices}
  boolean result;     // {result of comparison}

  result = false;

  if (length(s) != length(t))
    goto not_found;

  j = str_start[s];
  k = str_start[t];

  while (j < str_start[s + 1])
  {
    if (str_pool[j] != str_pool[k])
      goto not_found;

    incr(j);
    incr(k);
  }

  result = true;

not_found:
  return result;
}

// prints two least significant digits
static void print_two (integer n)
{
  n = abs(n) % 100;
  print_char('0' + (n / 10));
  print_char('0' + (n % 10));
}

// Random numbers.

static inline integer halfp (integer x)
{
  return x / 2;
}

#define _double(f) f = f + f

static integer make_frac (integer p, integer q)
{
  integer f; // {the fraction bits, with a leading 1 bit}
  integer n; // {the integer part of $\vert p/q\vert$}
  boolean negative; // {should the result be negated?}
  integer be_careful; // {disables certain compiler optimizations}

  if (p >= 0)
    negative = false;
  else
  {
    negate(p);
    negative = true;
  }

  if (q <= 0)
  {
    negate(q);
    negative = !negative;
  }

  n = p / q;
  p = p % q;

  if (n >= 8)
  {
    arith_error = true;

    if (negative)
      return -el_gordo;
    else
      return el_gordo;
  }
  else
  {
    n = (n - 1) * fraction_one;

    // @<Compute $f=\lfloor 2^{28}(1+p/q)+{1\over2}\rfloor$@>;
    f = 1;

    do {
      be_careful = p - q;
      p = be_careful + p;

      if (p >= 0)
        f = f + f + 1;
      else
      {
        _double(f);
        p = p + q;
      }
    } while (!(f >= fraction_one));

    be_careful = p - q;

    if (be_careful + p >= 0)
      incr(f);

    if (negative)
      return -(f + n);
    else
      return (f + n);
  }
}

static integer take_frac (integer q, integer f)
{
  integer p; // {the fraction so far}
  boolean negative; // {should the result be negated?}
  integer n; // {additional multiple of $q$}
  integer be_careful; // {disables certain compiler optimizations}

  // @<Reduce to the case that |f>=0| and |q>0|@>;
  if (f >= 0)
    negative = false;
  else
  {
    negate(f);
    negative = true;
  }

  if (q < 0)
  {
    negate(q);
    negative = !negative;
  }

  if (f < fraction_one)
    n = 0;
  else
  {
    n = f / fraction_one;
    f = f % fraction_one;

    if (q <= el_gordo / n)
      n = n * q;
    else
    {
      arith_error = true;
      n = el_gordo;
    }
  }

  f = f + fraction_one;
  // @<Compute $p=\lfloor qf/2^{28}+{1\over2}\rfloor-q$@>;
  p = fraction_half; // {that's $2^{27}$; the invariants hold now with $k=28$}
  if (q < fraction_four) 
    do {
      if (odd(f))
        p = halfp(p + q);
      else
        p = halfp(p);

      f = halfp(f);
    } while (!(f == 1));
  else
    do {
      if (odd(f))
        p = p + halfp(q - p);
      else
        p = halfp(p);

      f = halfp(f);
    } while (!(f == 1));
  
  be_careful = n - el_gordo;

  if (be_careful + p > 0) 
  {
    arith_error = true;
    n = el_gordo - p;
  }

  if (negative)
    return -(n + p);
  else
    return n + p;
}

static integer m_log (integer x)
{
  integer y, z; // {auxiliary registers}
  integer k; // {iteration counter}

  if (x <= 0)
  {
    print_err("Logarithm of ");
    print_scaled(x);
    prints(" has been replaced by 0");
    help2("Since I don't take logs of non-positive numbers,",
          "I'm zeroing this one. Proceed, with fingers crossed.");
    error();
    return 0;
  }
  else
  {
    y = 1302456956 + 4 - 100; // {$14\times2^{27}\ln2\approx1302456956.421063$}
    z = 27595 + 6553600; // {and $2^{16}\times .421063\approx 27595$}

    while (x < fraction_four)
    {
      _double(x);
      y = y - 93032639;
      z = z - 48782;
    }
    // {$2^{27}\ln2\approx 93032639.74436163$
    // and $2^{16}\times.74436163\approx 48782$}
    y = y + (z / unity);
    k = 2;

    while (x > fraction_four + 4)
    {
      z = ((x - 1) / two_to_the[k]) + 1; // {$z=\lceil x/2^k\rceil$}

      while (x < fraction_four + z)
      {
        z = halfp(z + 1);
        k = k + 1;
      }

      y = y + spec_log[k];
      x = x - z;
    }

    return y / 8;
  }
}

static integer ab_vs_cd (integer a, integer b, integer c, integer d)
{
  integer q, r; // {temporary registers}

  //@<Reduce to the case that |a,c>=0|, |b,d>0|@>;
  if (a < 0) 
  {
    negate(a);
    negate(b);
  }

  if (c < 0)
  {
    negate(c);
    negate(d);
  }

  if (d <= 0)
  {
    if (b >= 0) 
      if (((a == 0) || (b == 0)) && ((c == 0) || (d == 0)))
        return 0;
      else
        return 1;

    if (d == 0)
      if (a == 0)
        return 0;
      else
        return -1;
    
    get_microinterval();
    q = a;
    a = c; c = q; q = -b; b = -d; d = q;
  }
  else if (b <= 0)
  {
    if (b < 0)
      if (a > 0)
        return -1;

    if (c == 0)
      return 0;
    else
      return -1;
  }

  for (;;)
  {
    q = a / d;
    r = c / b;

    if (q != r)
      if (q > r)
        return 1;
      else
        return -1;

    q = a % d;
    r = c % b;

    if (r == 0)
      if (q == 0)
        return 0;
      else
        return 1;

    if (q == 0)
      return -1;

    a = b;
    b = q;
    c = d;
    d = r;
  } //{now |a>d>0| and |c>b>0|}
}

#define next_random() \
do {                  \
  if (j_random == 0)  \
    new_randoms();    \
  else                \
    decr(j_random);   \
} while (0)

static void new_randoms()
{
  uint32_t k; // {index into |randoms|}
  integer x; // {accumulator}

  for (k = 0; k <= 23; k++)
  {
    x = randoms[k] - randoms[k + 31];

    if (x < 0)
      x = x + fraction_one;

    randoms[k] = x;
  }

  for (k = 24; k <= 54; k++)
  {
    x = randoms[k] - randoms[k - 24];

    if (x < 0)
      x = x + fraction_one;

    randoms[k] = x;
  }

  j_random = 54;
}

static void init_randoms (integer seed)
{
  integer j, jj, k; // {more or less random integers}
  uint32_t i; // {index into |randoms|}

  j = abs(seed);

  while (j >= fraction_one)
    j = halfp(j);

  k = 1;

  for (i = 0; i <= 54; i++)
  {
    jj = k;
    k = j-k;
    j = jj;

    if (k < 0)
      k = k + fraction_one;

    randoms[(i * 21) % 55] = j;
  }

  new_randoms();
  new_randoms();
  new_randoms(); // {``warm up'' the array}
}

static integer unif_rand (integer x)
{
  integer y; // {trial value}

  next_random();
  y = take_frac(abs(x), randoms[j_random]);

  if (y == abs(x))
    return 0;
  else if (x > 0)
    return y;
  else
    return -y;
}

static integer norm_rand()
{
  integer x, u, l; // {what the book would call $2^{16}X$, $2^{28}U$,
                   //  and $-2^{24}\ln U$}

  do {
    do {
      next_random();
      x = take_frac(112429, randoms[j_random] - fraction_half);
      // {$2^{16}\sqrt{8/e}\approx 112428.82793$}
      next_random();
      u = randoms[j_random];
    } while (!(abs(x) < u));

    x = make_frac(x,u);
    l = 139548960 - m_log(u); // {$2^{24}\cdot12\ln2\approx139548959.6165$}
  } while (!(ab_vs_cd(1024,l,x,x) >= 0));

  return x;  
}

// prints a positive integer in hexadecimal form
static void print_hex (integer n)
{
  uint32_t k; // {index to current digit; we assume that $0\L n<16^{22}$}

  k = 0;
  print_char('"');

  do {
    dig[k] = n % 16;
    n = n / 16;
    incr(k);
  } while (!(n == 0));

  print_the_digs(k);
}

static void print_hex_safe (integer n)
{
  if (n < 0)
    print_int(n);
  else
    print_hex(n);
}

static void print_roman_int (integer n)
{
  pool_pointer j, k; // {mysterious indices into |str_pool|}
  nonnegative_integer u, v; // {mysterious numbers}

  j = str_start[260]; /* m2d5c2l5x2v5i */
  v = 1000;

  while (true)
  {
    while (n >= v)
    {
      print_char(str_pool[j]);
      n = n - v;
    }

    if (n <= 0)
      return;

    k = j + 2;
    u = v / (str_pool[k - 1] - '0');

    if (str_pool[k - 1] == '2')
    {
      k = k + 2;
      u = u / (str_pool[k - 1] - '0');
    }

    if (n + u >= v)
    {
      print_char(str_pool[k]);
      n = n + u;
    }
    else
    {
      j = j + 2;
      v = v / (str_pool[j - 1] - '0');
    }
  }
}

// prints a yet-unmade string
static void print_current_string (void)
{
  pool_pointer j; // {points to current character code}

  j = str_start[str_ptr];

  while (j < pool_ptr)
  {
    print_char(str_pool[j]);
    incr(j);
  }
}

// gets a line from the terminal
void term_input (void)
{ 
  uint32_t k; // {index into |buffer|}

  update_terminal();

  if (!input_ln(term_in, true))
    fatal_error("End of file on the terminal!");

  term_offset = 0;  // {the user's line ended with \<\rm return>}
  decr(selector);   // {prepare to echo the input}

  if (last != first)
    for (k = first; k <= last - 1; k++)
      print(buffer[k]);

  print_ln();
  incr(selector); // {restore previous status}
}

static void int_error (integer n)
{
  prints(" (");
  print_int(n);
  print_char(')');
  error();
}

void normalize_selector (void)
{
  if (log_opened)
    selector = term_and_log;
  else
    selector = term_only;

  if (job_name == 0)
    open_log_file();

  if (interaction == batch_mode)
    decr(selector);
}

void pause_for_instructions (void)
{
  if (OK_to_interrupt)
  {
    interaction = error_stop_mode;

    if ((selector == log_only) || (selector == no_print))
      incr(selector);

    print_err("Interruption");
    help3("You rang?",
        "Try to insert an instructions for me (e.g.,`I\\showlists'),",
        "unless you just want to quit by typing `X'.");
    deletions_allowed = false;
    error();
    deletions_allowed = true;
    interrupt = 0;
  }
}

static integer half (integer x)
{
  if (odd(x))
    return ((x + 1) / 2);
  else
    return (x / 2);
}

static scaled round_decimals (small_number k)
{
  integer a; // {the accumulator}

  a = 0;

  while (k > 0)
  {
    decr(k);
    a = (a + dig[k] * two) / 10;
  }
  
  return ((a + 1) / 2);
}

// prints scaled real, rounded to five digits
void print_scaled (scaled s)
{
  scaled delta; // {amount of allowable inaccuracy}

  if (s < 0)
  {
    print_char('-');
    negate(s); // {print the sign, if negative}
  }

  print_int(s / unity); // {print the integer part}
  print_char('.');
  s = 10 * (s % unity) + 5;
  delta = 10;

  do {
    if (delta > unity)
      s = s + 0100000 - 50000; // {round the last digit}

    print_char('0' + (s / unity));
    s = 10 * (s % unity);
    delta = delta * 10;
  } while (!(s <= delta));
}

static scaled mult_and_add (integer n, scaled x, scaled y, scaled max_answer)
{
  if (n < 0)
  {
    negate(x);
    negate(n);
  }

  if (n == 0)
    return y;
  else if (((x <= (max_answer - y) / n) && (-x <= (max_answer + y) / n)))
    return (n * x + y); 
  else
  {
    arith_error = true;
    return 0;
  }
}

static scaled x_over_n (scaled x, integer n)
{
  scaled Result;
  boolean negative; // {should |remainder| be negated?}

  negative = false;

  if (n == 0)
  {
    arith_error = true;
    Result = 0;
    ng_remainder = x;
  }
  else
  {
    if (n < 0)
    {
      negate(x);
      negate(n);
      negative = true;
    }

    if (x >= 0)
    {
      Result = x / n;
      ng_remainder = x % n;
    }
    else
    {
      Result = -((-x) / n);
      ng_remainder = -((-x) % n);
    }
  }

  if (negative)
    negate(ng_remainder);

  return Result;
}

static scaled xn_over_d (scaled x, integer n, integer d)
{
  scaled Result;
  boolean positive; // {was |x>=0|?}
  nonnegative_integer t, u, v; // {intermediate quantities}

  if (x >= 0)
    positive = true;
  else
  {
    negate(x);
    positive = false;
  }

  t = (x % 0100000) * n;
  u = (x / 0100000) * n + (t / 0100000);
  v = (u % d) * 0100000 + (t % 0100000); 

  if (u / d >= 0100000)
    arith_error = true; 
  else
    u = 0100000 * (u / d) + (v / d);

  if (positive)
  {
    Result = u;
    ng_remainder = v % d;
  }
  else
  {
    Result = -u;
    ng_remainder = -(v % d);
  }

  return Result;
}

// compute badness, given |t>=0|
static halfword badness (scaled t, scaled s)
{
  integer r; // {approximation to $\alpha t/s$, where $\alpha^3\approx 100\cdot2^{18}$}

  if (t == 0)
    return 0;
  else if (s <= 0)
    return inf_bad;
  else
  {
    if (t <= 7230584)
      r = (t * 297) / s; // {$297^3=99.94\times2^{18}$}
    else if (s >= 1663497)
      r = t / (s / 297);
    else
      r = t;

    if (r > 1290)
      return inf_bad; // {$1290^3<2^{31}<1291^3$}
    else
      return (r * r * r + 0400000) / 01000000; // {that was $r^3/2^{18}$, rounded to the nearest integer}
  }
}

static void print_word (memory_word w)
{ 
  print_int(w.cint); 
  print_char(' ');
  print_scaled(w.cint); 
  print_char(' ');
  print_scaled(round(unity * w.gr));
  print_ln();
  print_int(w.hh.lh);
  print_char('=');
  print_int(w.hh.b0);
  print_char(':');
  print_int(w.hh.b1);
  print_char(';');
  print_int(w.hh.rh);
  print_char(' ');
  print_int(w.qqqq.b0); 
  print_char(':');
  print_int(w.qqqq.b1); 
  print_char(':');
  print_int(w.qqqq.b2); 
  print_char(':');
  print_int(w.qqqq.b3);
}

void show_token_list (integer p, integer q, integer l)
{
  integer m, c; // {pieces of a token}
  ASCII_code match_chr; // {character used in a `|match|'}
  ASCII_code n; // {the highest parameter number, as an ASCII digit}

  match_chr = '#';
  n = '0';
  tally = 0;

  while ((p != null) && (tally < l))
  {
    if (p == q)
      set_trick_count();

    // Display token |p|, and |return| if there are problems
    if ((p < hi_mem_min) || (p > mem_end))
    {
      print_esc("CLOBBERED.");
      return;
    }

    if (info(p) >= cs_token_flag)
      print_cs(info(p) - cs_token_flag); // {|wchar_token|}
    else
    {
      if (check_kanji(info(p))) // {|wchar_token|}
      {
        m = info(p) / max_cjk_val;
        c = info(p) % max_cjk_val;
      }
      else
      {
        m = info(p) / max_char_val;
        c = info(p) % max_char_val;
      }

      if ((m < kanji) && (c > 256))
        print_esc("BAD.");
      else switch (m)
      {
        case kanji:
        case kana:
        case other_kchar:
        case hangul:
          print_kanji(KANJI(c));
          break;

        case left_brace:
        case right_brace:
        case math_shift:
        case tab_mark:
        case sup_mark:
        case sub_mark:
        case spacer:
        case letter:
        case other_char:
          print(c);
          break;

        case mac_param:
          {
            print(c);
            print(c);
          }
          break;

        case out_param:
          {
            print(match_chr);

            if (c <= 9)
              print_char(c + '0');
            else
            {
              print_char('!');
              return;
            }
          }
          break;

        case match:
          {
            match_chr = (ASCII_code) c;
            print(c);
            incr(n);
            print_char(n);

            if (n > '9')
              return;
          }
          break;
        
        case end_match:
          if (c == 0)
            prints("->");
          break;

        default:
          print_esc("BAD.");
          break;
      }
    }

    p = link(p);
  }

  if (p != null)
    print_esc("ETC.");
}

static void runaway (void)
{
  pointer p; // {head of runaway list}

  if (scanner_status > skipping)
  {
    print_nl("Runaway ");

    switch (scanner_status)
    {
      case defining:
        {
          prints("definition");
          p = def_ref;
        }
        break;

      case matching:
        {
          prints("argument");
          p = temp_head;
        }
        break;

      case aligning:
        {
          prints("preamble");
          p = hold_head;
        }
        break;

      case absorbing:
        {
          prints("text");
          p = def_ref;
        }
        break;
    }

    print_char('?');
    print_ln();
    show_token_list(link(p), null, error_line - 10);
  }
}

pointer get_avail (void)
{
  pointer p; // {the new node being got}

  p = avail; // {get top location in the |avail| stack}

  if (p != null)
    avail = link(avail); // {and pop it off}
  else if (mem_end < mem_max) // {or go into virgin territory}
  {
    incr(mem_end);
    p = mem_end;
  }
  else
  {
    decr(hi_mem_min);
    p = hi_mem_min;

    if (hi_mem_min <= lo_mem_max)
    {
      incr(hi_mem_min);
      mem = realloc_mem(0, mem_top / 2);

      if (mem == NULL)
        return 0;

      if (mem_end >= mem_max)
      {
        runaway();  // {if memory is exhausted, display possible runaway text}
        overflow("main memory size", mem_max + 1 - mem_min);  // {quit; all one-word nodes are busy}
      }

      incr(mem_end);
      p = mem_end;
    }
  }

  link(p) = null;  // {provide an oft-desired initialization of the new node}

#ifdef STAT
  incr(dyn_used);
#endif

  return p;
} 

// makes list of single-word nodes
void flush_list (pointer p)
{ 
  pointer q, r; // {list traversers}

  if (p != null)
  {
    r = p;

    do {
      q = r;
      r = link(r);
#ifdef STAT
      decr(dyn_used);
#endif
    } while (!(r == null)); // {now |q| is the last node on the list}

    link(q) = avail;
    avail = p;
  }
}

// variable-size node allocation
pointer get_node (integer s)
{
  pointer p; // {the node currently under inspection}
  pointer q; // {the node physically after node |p|}
  integer r; // {the newly allocated node, or a candidate for this honor}
  integer t; // {temporary register}

restart:
  p = rover; // {start at some free node in the ring}

  do {
    q = p + node_size(p);

    while (is_empty(q))
    {
      t = rlink(q);

      if (q == rover)
        rover = t;

      llink(t) = llink(q);
      rlink(llink(q)) = t;
      q = q + node_size(q);
    }

    r = q - s;

    if (r > p + 1) 
    {
      node_size(p) = r - p;
      rover = p;
      goto found;
    }

    if (r == p)
    {
      if (rlink(p) != p)
      {
        rover = rlink(p);
        t = llink(p);
        llink(rover) = t;
        rlink(t) = rover;
        goto found;
      }
    }

    node_size(p) = q - p;
    p = rlink(p);
  } while (!(p == rover));

  if (s == 010000000000)
  {
    return max_halfword;
  }

  if (lo_mem_max + 2 < hi_mem_min)
  {
    if (lo_mem_max + 2 <= mem_bot + max_halfword)
    {
      if (hi_mem_min - lo_mem_max >= (block_size + block_size - 2))
        t = lo_mem_max + block_size;
      else
        t = lo_mem_max + 1 + (hi_mem_min - lo_mem_max) / 2;

      p = llink(rover);
      q = lo_mem_max;
      rlink(p) = q;
      llink(rover) = q;

      if (t > mem_bot + max_halfword)
        t = mem_bot + max_halfword;

      rlink(q) = rover;
      llink(q) = p;
      link(q) = empty_flag;
      node_size(q) = t - lo_mem_max;
      lo_mem_max = t;
      link(lo_mem_max) = null;
      info(lo_mem_max) = null;
      rover = q;
      goto restart;
    }
  }

  /* extend lower memory downwards */
  if (mem_min - (block_size + 1) <= mem_start)
  {
    mem = realloc_mem(mem_top / 2 + block_size, 0);

    if (mem == NULL)
      return 0;
  }

  if (mem_min - (block_size + 1) <= mem_start) /* check again */
  {
    overflow("main memory size", mem_max + 1 - mem_min);
  }

  add_variable_space(block_size);
  goto restart;

found:
  if (s >= medium_node_size)
  {
    sync_tag(r + s) = synctex_tag;
    sync_line(r + s) = line;
  }

  link(r) = null;

#ifdef STAT
  var_used = var_used + s;
#endif

  return r;
}

// variable-size node liberation
void free_node (pointer p, halfword s)
{
  pointer q; // {|llink(rover)|}

  node_size(p) = s;
  link(p) = empty_flag;
  q = llink(rover);
  llink(p) = q;
  rlink(p) = rover;
  llink(rover) = p;
  rlink(q) = p;

#ifdef STAT
  var_used = var_used - s;
#endif
}

// creates a new box node
static pointer new_null_box (void)
{
  pointer p; // {the new node}

  p = get_node(box_node_size);
  type(p) = hlist_node;
  subtype(p) = min_quarterword;
  width(p) = 0;
  depth(p) = 0;
  height(p) = 0;
  shift_amount(p) = 0;
  list_ptr(p) = null;
  glue_sign(p) = normal;
  glue_order(p) = normal;
  set_glue_ratio_zero(glue_set(p));
  space_ptr(p) = zero_glue;
  xspace_ptr(p) = zero_glue;
  set_box_dir(p, dir_default);
  add_glue_ref(zero_glue);
  add_glue_ref(zero_glue);

  return p;
}

static pointer new_rule (void)
{
  pointer p; // {the new node}

  p = get_node(rule_node_size);
  type(p) = rule_node;
  subtype(p) = 0;
  width(p) = null_flag;
  depth(p) = null_flag;
  height(p) = null_flag;

  return p;
}

static pointer new_ligature (quarterword f, quarterword c, pointer q)
{
  pointer p; // {the new node}

  p = get_node(small_node_size);
  type(p) = ligature_node;
  font(lig_char(p)) = f;
  character(lig_char(p)) = c;
  lig_ptr(p) = q;
  subtype(p) = 0;

  return p;
}

static pointer new_lig_item (quarterword c)
{
  pointer p; // {the new node}

  p = get_node(small_node_size);
  character(p) = c;
  lig_ptr(p) = null;

  return p;
}

// creates an empty |disc_node|
static pointer new_disc (void)
{
  pointer p; // {the new node}

  p = get_node(small_node_size);
  type(p) = disc_node;
  replace_count(p) = 0;
  pre_break(p) = null;
  post_break(p) = null;

  return p;
}

static pointer new_math (scaled w, small_number s)
{
  pointer p; // {the new node}

  p = get_node(medium_node_size);
  type(p) = math_node;
  subtype(p) = s;
  width(p) = w;

  return p;
}

// duplicates a glue specification
static pointer new_spec (pointer p)
{
  pointer q; // {the new spec}

  q = get_node(glue_spec_size);
  mem[q] = mem[p];
  glue_ref_count(q) = null;
  width(q) = width(p);
  stretch(q) = stretch(p);
  shrink(q) = shrink(p);

  return q;
}

static pointer new_param_glue (small_number n)
{
  pointer p; // {the new node}
  pointer q; // {the glue specification}

  p = get_node(medium_node_size);
  type(p) = glue_node;
  subtype(p) = n + 1;
  leader_ptr(p) = null;
  q = glue_par(n);
  glue_ptr(p) = q;
  incr(glue_ref_count(q));

  return p;
}

static pointer new_glue (pointer q)
{
  pointer p; // {the new node}

  p = get_node(medium_node_size);
  type(p) = glue_node;
  subtype(p) = normal;
  leader_ptr(p) = null;
  glue_ptr(p) = q;
  incr(glue_ref_count(q));

  return p;
}

static pointer new_skip_param (small_number n)
{
  pointer p; // {the new node}

  temp_ptr = new_spec(glue_par(n));
  p = new_glue(temp_ptr);
  glue_ref_count(temp_ptr) = null;
  subtype(p) = n + 1;

  return p;
}

static pointer new_kern (scaled w)
{
  pointer p; // {the new node}

  p = get_node(medium_node_size);
  type(p) = kern_node;
  subtype(p) = normal;
  width(p) = w;

  return p;
}

static pointer new_penalty (integer m)
{
  pointer p; // {the new node}

  p = get_node(medium_node_size);
  type(p) = penalty_node;
  subtype(p) = 0;
  penalty(p) = m;

  return p;
}

#ifdef APTEX_DEBUG
/* sec 0167 */
void check_mem (boolean print_locs)
{
  pointer p, q;
  boolean clobbered;

  for (p = mem_min; p <= lo_mem_max; p++)
    freearr[p] = false;

  for (p = hi_mem_min; p <= mem_end; p++)
    freearr[p] = false;

  p = avail;
  q = 0;
  clobbered = false;

  while (p != null)
  {
    if ((p > mem_end) || (p < hi_mem_min))
      clobbered = true;
    else if (freearr[p])
      clobbered = true;

    if (clobbered)
    {
      print_nl("AVAIL list clobbered at ");
      print_int(q);
      goto done1;
    }

    freearr[p] = true;
    q = p;
    p = link(q);
  }

done1:
  p = rover;
  q = null;
  clobbered = false;

  do {
    if ((p >= lo_mem_max) || (p < mem_min))
      clobbered = true;
    else if ((rlink(p) >= lo_mem_max) || (rlink(p) < mem_min))
      clobbered = true;
    else if (!(is_empty(p)) || (node_size(p) < 2) ||
        (p + node_size(p) > lo_mem_max) || (llink(rlink(p)) != p))
      clobbered = true;

    if (clobbered)
    {
      print_nl("Double-AVAIL list clobbered at ");
      print_int(q);
      goto done2;
    }

    for (q = p; q <= p + node_size(p) - 1; q++)
    {
      if (freearr[q])
      {
        print_nl("Doubly free location at ");
        print_int(q);
        goto done2;
      }

      freearr[q] = true;
    }

    q = p;
    p = rlink(p);
  } while (!(p == rover));

done2:
  p = mem_min;

  while (p <= lo_mem_max)
  {
    if (is_empty(p))
    {
      print_nl("Bad flag at ");
      print_int(p);
    }

    while ((p <= lo_mem_max) && !freearr[p])
      incr(p);

    while ((p <= lo_mem_max) && freearr[p])
      incr(p);
  }

  if (print_locs)
  {
    print_nl("New busy locs:");

    for (p = mem_min; p <= lo_mem_max; p++)
    {
      if (!freearr[p] && ((p > was_lo_max) || wasfree[p]))
      {
        print_char(' ');
        print_int(p);
      }
    }

    for (p = hi_mem_min; p <= mem_end; p++)
    {
      if (!freearr[p] && ((p < was_hi_min) || (p > was_mem_end) || wasfree[p]))
      {
        print_char(' ');
        print_int(p);
      }
    }
  }

  for (p = mem_min; p <= lo_mem_max; p++)
    wasfree[p] = freearr[p];

  for (p = hi_mem_min; p <= mem_end; p++)
    wasfree[p] = freearr[p];

  was_mem_end = mem_end;
  was_lo_max = lo_mem_max;
  was_hi_min = hi_mem_min;
}

void search_mem (pointer p)
{
  integer q;

  for (q = mem_min; q <= lo_mem_max; q++)
  {
    if (link(q) == p)
    {
      print_nl("LINK(");
      print_int(q);
      print_char(')');
    }

    if (info(q) == p)
    {
      print_nl("INFO(");
      print_int(q);
      print_char(')');
    }
  }

  for (q = hi_mem_min; q <= mem_end; q++)
  {
    if (link(q) == p)
    {
      print_nl("LINK(");
      print_int(q);
      print_char(')');
    }

    if (info(q) == p)
    {
      print_nl("INFO(");
      print_int(q);
      print_char(')');
    }
  }

  for (q = active_base; q <= box_base + 255; q++)
  {
    if (equiv(q) == p)
    {
      print_nl("EQUIV(");
      print_int(q);
      print_char(')');
    }
  }

  if (save_ptr > 0)
  {
    for (q = 0; q <= save_ptr - 1; q++)
    {
      if (equiv_field(save_stack[q]) == p)
      {
        print_nl("SAVE(");
        print_int(q);
        print_char(')');
      }
    }
  }

  for (q = 0; q <= hyphen_prime; q++)
  {
    if (hyph_list[q] == p)
    {
      print_nl("HYPH(");
      print_int(q);
      print_char(')');
    }
  }
}
#endif

static void print_font_name_and_size (internal_font_number f)
{
  print(font_name[f]);
  if (font_size[f] != font_dsize[f])
  {
    prints("@@");
    print_scaled(font_size[f]);
    prints("pt");
  }
}

static void print_font_dir_and_enc (internal_font_number f)
{
  if (font_dir[f] == dir_tate)
    prints("/TATE");
  else if (font_dir[f] == dir_yoko)
    prints("/YOKO");
  if (font_enc[f] == 2)
    prints("+Unicode");
  else if (font_enc[f] == 1)
    prints("+JIS");
}

// prints highlights of list |p|
static void short_display (integer p)
{
  integer n; // {for replacement counts}

  while (p != null) /* want "p != null" here ! */
  {
    if (is_char_node(p))
    {
      if (p <= mem_end)
      {
        if (font(p) != font_in_short_display)
        {
          if (font(p) > font_max)
            print_char('*');
          else
            print_the_font_identifier_for_font_p();

          print_char(' ');
          font_in_short_display = font(p);
        }

        if (font_dir[font(p)] != dir_default)
        {
          p = link(p);
          print_kanji(info(p));
        }
        else
          print(character(p));
      }
    }
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case ins_node:
      case whatsit_node:
      case mark_node:
      case adjust_node:
      case unset_node:
        prints("[]");
        break;

      case rule_node:
        print_char('|');
        break;

      case glue_node:
        if (glue_ptr(p) != zero_glue)
          print_char(' ');
        break;

      case math_node:
        if (subtype(p) >= L_code)
          prints("[]");
        else
          print_char('$');
        break;

      case ligature_node:
        short_display(lig_ptr(p));
        break;

      case disc_node:
        {
          short_display(pre_break(p));
          short_display(post_break(p));
          n = replace_count(p);

          while (n > 0)
          {
            if (link(p) != null)
              p = link(p);

            decr(n);
          }
        }
        break;

      default:
        do_nothing();
        break;
    }

    p = link(p);
  }
}

// prints |char_node| data
static void print_font_and_char (integer p)
{
  if (p > mem_end)
    print_esc("CLOBBERED.");
  else
  {
    if (font(p) > font_max)
      print_char('*');
    else
      print_the_font_identifier_for_font_p();

    print_char(' ');

    if (font_dir[font(p)] != dir_default)
    {
      p = link(p);
      print_kanji(info(p));
    }
    else
      print(character(p));
  }
}

// prints token list data in braces
static void print_mark (integer p)
{
  print_char('{');

  if ((p < hi_mem_min) || (p > mem_end))
    print_esc("CLOBBERED.");
  else
    show_token_list(link(p), null, max_print_line - 10);

  print_char('}');
}

// prints dimension in rule node
static void print_rule_dimen (scaled d)
{
  if (is_running(d))
    print_char('*');
  else
    print_scaled(d);
}

static void print_glue (scaled d, integer order, const char * s)
{
  print_scaled(d);

  if ((order < normal) || (order > filll))
    prints("foul");
  else if (order > normal)
  {
    prints("fil");

    while (order > fil)
    {
      print_char('l');
      decr(order);
    }
  }
  else if (*s != '\0')
    prints(s);
}

static void print_spec (integer p, const char * s)
{
  if ((p < mem_min) || (p >= lo_mem_max))
    print_char('*');
  else
  {
    print_scaled(width(p));

    if (*s != '\0')
      prints(s);

    if (stretch(p) != 0)
    {
      prints(" plus ");
      print_glue(stretch(p), stretch_order(p), s);
    }

    if (shrink(p) != 0)
    {
      prints(" minus ");
      print_glue(shrink(p), shrink_order(p), s);
    }
  }
}

// prints family and character
static void print_fam_and_char (pointer p, small_number t)
{
  KANJI_code cx; // {temporary register for KANJI}

  print_esc("fam");
  print_int(fam(p));
  print_char(' ');

  if (t == math_char)
    print(character(p));
  else
  {
    KANJI(cx) = math_kcode_nucleus(p);
    print_kanji(cx);
  }
}

// prints a delimiter as 24-bit hex value
static void print_delimiter (pointer p)
{
  integer a; // {accumulator}

  a = small_fam(p) * 256 + small_char(p);
  a = a * 0x1000 + large_fam(p) * 256 + large_char(p);

  if (a < 0)
    print_int(a);
  else
    print_hex(a);
}

// display a noad field
static void print_subsidiary_data (pointer p, ASCII_code c)
{
  if (cur_length >= depth_threshold)
  {
    if (math_type(p) != empty)
      prints(" []");
  }
  else
  {
    append_char(c); // {include |c| in the recursion history}
    temp_ptr = p; // {prepare for |show_info| if recursion is needed}

    switch (math_type(p))
    {
      case math_char:
      case math_jchar:
        {
          print_ln();
          print_current_string();
          print_fam_and_char(p, math_type(p));
        }
        break;

      case sub_box:
      case sub_exp_box:
        show_info();
        break;

      case sub_mlist:
        if (info(p) == null)
        {
          print_ln();
          print_current_string();
          prints("{}");
        }
        else
          show_info();
        break;

      default:
        do_nothing();
        break;
    }

    flush_char();
  }
}

static void print_style (integer c)
{
  switch (c / 2)
  {
    case 0:
      print_esc("displaystyle"); // {|display_style=0|}
      break;

    case 1:
      print_esc("textstyle"); // {|text_style=2|}
      break;

    case 2:
      print_esc("scriptstyle"); // {|script_style=4|}
      break;

    case 3:
      print_esc("scriptscriptstyle"); // {|script_script_style=6|}
      break;

    default:
      prints("Unknown style!");
      break;
  }
}

static void print_skip_param (integer n)
{
  switch (n)
  {
    case line_skip_code:
      print_esc("lineskip");
      break;

    case baseline_skip_code:
      print_esc("baselineskip");
      break; 

    case par_skip_code:
      print_esc("parskip");
      break;

    case above_display_skip_code:
      print_esc("abovedisplayskip");
      break;

    case below_display_skip_code:
      print_esc("belowdisplayskip");
      break;

    case above_display_short_skip_code:
      print_esc("abovedisplayshortskip");
      break;

    case below_display_short_skip_code:
      print_esc("belowdisplayshortskip");
      break;

    case left_skip_code:
      print_esc("leftskip");
      break;

    case right_skip_code:
      print_esc("rightskip");
      break;

    case top_skip_code:
      print_esc("topskip");
      break;

    case split_top_skip_code:
      print_esc("splittopskip");
      break;

    case tab_skip_code:
      print_esc("tabskip");
      break;

    case space_skip_code:
      print_esc("spaceskip");
      break;

    case xspace_skip_code:
      print_esc("xspaceskip");
      break;

    case par_fill_skip_code:
      print_esc("parfillskip");
      break;

    case kanji_skip_code:
      print_esc("kanjiskip");
      break;

    case xkanji_skip_code:
      print_esc("xkanjiskip");
      break;

    case thin_mu_skip_code:
      print_esc("thinmuskip");
      break;

    case med_mu_skip_code:
      print_esc("medmuskip");
      break;

    case thick_mu_skip_code:
      print_esc("thickmuskip");
      break;

    case jfm_skip:
      prints("refer from jfm");
      break;

    default:
      prints("[unknown glue parameter!]");
      break;
  }
}

// prints a node list symbolically
void show_node_list (integer p)
{
  integer n; // {the number of items already printed at this level}
  real g; // {a glue ratio, as a floating point number}

  if (cur_length > depth_threshold)
  {
    if (p > null)
      prints(" []");
    //{indicate that there's been some truncation}
    return;
  }

  n = 0;

  while (p != null) /* ori. "p > mem_min" */
  {
    print_ln();
    print_current_string(); // {display the nesting history}

    if (p > mem_end)
    {
      prints("Bad link, display aborted.");

      return;
    }

    incr(n);

    if (n > breadth_max)
    {
      prints("etc.");

      return;
    }

    if (is_char_node(p))
    {
      print_font_and_char(p);

      if (font_dir[font(p)] != dir_default)
        p = link(p);
    }
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case unset_node:
        {
          switch (type(p))
          {
            case hlist_node:
              print_esc("h");
              break;

            case vlist_node:
              print_esc("v");
              break;

            case dir_node:
              print_esc("dir");
              break;

            default:
              print_esc("unset");
              break;
          }

          prints("box(");
          print_scaled(height(p));
          print_char('+');
          print_scaled(depth(p));
          prints(")x");
          print_scaled(width(p));

          if (type(p) == unset_node)
          {
            if (span_count(p) != min_quarterword)
            {
              prints(" (");
              print_int(span_count(p) + 1);
              prints(" columns)");
            }

            if (glue_stretch(p) != 0)
            {
              prints(", stretch ");
              print_glue(glue_stretch(p), glue_order(p), "");
            }

            if (glue_shrink(p) != 0)
            {
              prints(", shrink ");
              print_glue(glue_shrink(p), glue_sign(p), "");
            }
          }
          else
          {
            g = glue_set(p);

            if ((g != 0.0) && (glue_sign(p) != normal))
            {
              prints(", glue set ");

              if (glue_sign(p) == shrinking)
                prints("- ");

              if (fabs(g) > 20000.0)
              {
                if (g > 0.0)
                  print_char('>');
                else
                  prints("< -");

                print_glue(20000 * unity, glue_order(p), "");
              }
              else
                print_glue(round(unity * g), glue_order(p), "");
            }

            if (shift_amount(p) != 0)
            {
              prints(", shifted ");
              print_scaled(shift_amount(p));
            }

            if (eTeX_ex)
            {
              if ((type(p) == hlist_node) && (box_lr(p) == dlist))
                prints(", display");
            }

            if (box_dir(p) != dir_default)
            {
              print_direction_alt(box_dir(p));
            }
          }

          node_list_display(list_ptr(p));
        }
        break;

      case rule_node:
        {
          print_esc("rule(");
          print_rule_dimen(height(p));
          print_char('+');
          print_rule_dimen(depth(p));
          prints(")x");
          print_rule_dimen(width(p));
        }
        break;

      case ins_node:
        {
          print_esc("insert");
          print_int(subtype(p));
          print_dir(abs(ins_dir(p)));
          prints(", natural size ");
          print_scaled(height(p));
          prints("; split(");
          print_spec(split_top_ptr(p), "");
          print_char(',');
          print_scaled(depth(p));
          prints("); float cost ");
          print_int(float_cost(p));
          node_list_display(ins_ptr(p));
        }
        break;

      case whatsit_node:
        switch (subtype(p))
        {
          case open_node:
            {
              print_write_whatsit("openout", p);
              print_char('=');
              print_file_name(open_name(p), open_area(p), open_ext(p));
            }
            break;

          case write_node:
            {
              print_write_whatsit("write", p);
              print_mark(write_tokens(p));
            }
            break;

          case close_node:
            print_write_whatsit("closeout", p);
            break;

          case special_node:
            {
              print_esc("special");
              print_mark(write_tokens(p));
            }
            break;

          case language_node:
            {
              print_esc("setlanguage");
              print_int(what_lang(p));
              prints(" (hyphenmin ");
              print_int(what_lhm(p));
              print_char(',');
              print_int(what_rhm(p));
              print_char(')');
            }
            break;

          case pdf_save_pos_node:
            print_esc("pdfsavepos");
            break;

          case set_random_seed_code:
            print_esc("pdfsetrandomseed");
            break;

          case reset_timer_code:
            print_esc("pdfresettimer");
            break;

          default:
            prints("whatsit?");
            break;
        }
        break;

      case disp_node:
        {
          print_esc("displace ");
          print_scaled(disp_dimen(p));
        }
        break;

      case glue_node:
        if (subtype(p) >= a_leaders)
        {
          print_esc("");

          if (subtype(p) == c_leaders)
            print_char('c');
          else if (subtype(p) == x_leaders)
            print_char('x');

          prints("leaders ");
          print_spec(glue_ptr(p), "");
          node_list_display(leader_ptr(p));
        }
        else
        {
          print_esc("glue");

          if (subtype(p) != normal)
          {
            print_char('(');

            if (subtype(p) < cond_math_glue)
              print_skip_param(subtype(p) - 1);
            else if (subtype(p) == cond_math_glue)
              print_esc("nonscript");
            else
              print_esc("mskip");

            print_char(')');
          }

          if (subtype(p) != cond_math_glue)
          {
            print_char(' ');

            if (subtype(p) < cond_math_glue)
              print_spec(glue_ptr(p), "");
            else
              print_spec(glue_ptr(p), "mu");
          }
        }
        break;

      case kern_node:
        if (subtype(p) != mu_glue)
        {
          print_esc("kern");

          if (subtype(p) != normal)
            print_char(' ');

          print_scaled(width(p));

          if (subtype(p) == acc_kern)
            prints(" (for accent)");
        }
        else
        {
          print_esc("mkern");
          print_scaled(width(p));
          prints("mu");
        }
        break;

      case math_node:
        if (subtype(p) > after)
        {
          if (end_LR(p))
            print_esc("end");
          else
            print_esc("begin");
          
          if (subtype(p) > R_code)
            print_char('R');
          else if (subtype(p) > L_code)
            print_char('L');
          else
            print_char('M');
        }
        else
        {
          print_esc("math");

          if (subtype(p) == before)
            prints("on");
          else
            prints("off");

          if (width(p) != 0)
          {
            prints(", surrounded ");
            print_scaled(width(p));
          }
        }
        break;

      case ligature_node:
        {
          print_font_and_char(lig_char(p));
          prints(" (ligature ");

          if (subtype(p) > 1)
            print_char('|');

          font_in_short_display = font(lig_char(p));
          short_display(lig_ptr(p));

          if (odd(subtype(p)))
            print_char('|');

          print_char(')');
        }
        break;

      case penalty_node:
        {
          print_esc("penalty ");
          print_int(penalty(p));

          if (subtype(p) == widow_pena)
            prints("(for \\jcharwidowpenalty)");
          else if (subtype(p) == kinsoku_pena)
            prints("(for kinsoku)");
        }
        break;

      case disc_node:
        {
          print_esc("discretionary");

          if (replace_count(p) > 0)
          {
            prints(" replacing ");
            print_int(replace_count(p));
          }

          node_list_display(pre_break(p));
          append_char('|');
          show_node_list(post_break(p));
          flush_char();
        }
        break;

      case mark_node:
        {
          print_esc("mark");

          if (mark_class(p) != 0)
          {
            print_char('s');
            print_int(mark_class(p));
          }

          print_mark(mark_ptr(p));
        }
        break;

      case adjust_node:
        {
          print_esc("vadjust");
          node_list_display(adjust_ptr(p));
        }
        break;

      case style_node:
        print_style(subtype(p));
        break;

      case choice_node:
        {
          print_esc("mathchoice");
          append_char('D');
          show_node_list(display_mlist(p));
          flush_char();
          append_char('T');
          show_node_list(text_mlist(p));
          flush_char();
          append_char('S');
          show_node_list(script_mlist(p));
          flush_char();
          append_char('s');
          show_node_list(script_script_mlist(p));
          flush_char();
        }
        break;

      case ord_noad:
      case op_noad:
      case bin_noad:
      case rel_noad:
      case open_noad:
      case close_noad:
      case punct_noad:
      case inner_noad:
      case radical_noad:
      case over_noad:
      case under_noad:
      case vcenter_noad:
      case accent_noad:
      case left_noad:
      case right_noad:
        {
          switch (type(p))
          {
            case ord_noad:
              print_esc("mathord");
              break;

            case op_noad:
              print_esc("mathop");
              break;

            case bin_noad:
              print_esc("mathbin");
              break;

            case rel_noad:
              print_esc("mathrel");
              break;

            case open_noad:
              print_esc("mathopen");
              break;

            case close_noad:
              print_esc("mathclose");
              break;

            case punct_noad:
              print_esc("mathpunct");
              break;

            case inner_noad:
              print_esc("mathinner");
              break;

            case over_noad:
              print_esc("overline");
              break;

            case under_noad:
              print_esc("underline");
              break;

            case vcenter_noad:
              print_esc("vcenter");
              break;

            case radical_noad:
              {
                print_esc("radical");
                print_delimiter(left_delimiter(p));
              }
              break;

            case accent_noad:
              {
                print_esc("accent");
                print_fam_and_char(accent_chr(p), math_char);
              }
              break;

            case left_noad:
              {
                print_esc("left");
                print_delimiter(delimiter(p));
              }
              break;

            case right_noad:
              {
                if (subtype(p) == normal)
                  print_esc("right");
                else
                  print_esc("middle");

                print_delimiter(delimiter(p));
              }
              break;
          }

          if (type(p) < left_noad)
          {
            if (subtype(p) != normal)
            {
              if (subtype(p) == limits)
                print_esc("limits");
              else
                print_esc("nolimits");
            }

            print_subsidiary_data(nucleus(p), '.');
          }

          print_subsidiary_data(supscr(p), '^');
          print_subsidiary_data(subscr(p), '_');
        }
        break;

      case fraction_noad:
        {
          print_esc("fraction, thickness ");

          if (thickness(p) == default_code)
            prints("= default");
          else
            print_scaled(thickness(p));

          if ((small_fam(left_delimiter(p)) != 0) ||
              (small_char(left_delimiter(p)) != min_quarterword) ||
              (large_fam(left_delimiter(p)) != 0) ||
              (large_char(left_delimiter(p)) != min_quarterword))
          {
            prints(", left-delimiter ");
            print_delimiter(left_delimiter(p));
          }

          if ((small_fam(right_delimiter(p)) != 0) ||
              (small_char(right_delimiter(p)) != min_quarterword) ||
              (large_fam(right_delimiter(p)) != 0) ||
              (large_char(right_delimiter(p)) != min_quarterword))
          {
            prints(", right-delimiter ");
            print_delimiter(right_delimiter(p));
          }

          print_subsidiary_data(numerator(p), '\\');
          print_subsidiary_data(denominator(p), '/');
        }
        break;

      default:
        prints("Unknown node type!");
        break;
    }

    p = link(p);
  }
}

void show_box (pointer p)
{
  /*
    @<Assign the values |depth_threshold:=show_box_depth| and
    |breadth_max:=show_box_breadth|@>
  */
  depth_threshold = show_box_depth;
  breadth_max = show_box_breadth;

  if (breadth_max <= 0)
    breadth_max = 5;

#ifdef APTEX_EXTENSION
  if (pool_ptr + depth_threshold >= current_pool_size)
    str_pool = realloc_str_pool(increment_pool_size);

  if (pool_ptr + depth_threshold >= current_pool_size)
    depth_threshold = current_pool_size - pool_ptr - 1;
#else
  if (pool_ptr + depth_threshold >= pool_size)
    depth_threshold = pool_size - pool_ptr - 1;
#endif

  // {now there's enough room for prefix string}
  show_node_list(p);  // {the show starts at |p|}
  print_ln();
}

// |p| points to the reference count
// of a token list that is losing one reference
void delete_token_ref (pointer p)
{
  if (token_ref_count(p) == null)
    flush_list(p);
  else
    decr(token_ref_count(p));
}

// |p| points to a glue specification
void delete_glue_ref (pointer p)
{
  if (glue_ref_count(p) == null)
    free_node(p, glue_spec_size);
  else
    decr(glue_ref_count(p));
}

// erase list of nodes starting at |p|
void flush_node_list (pointer p)
{
  pointer q;  // {successor to node |p|}

  while (p != null)
  {
    q = link(p);

    if (is_char_node(p))
      free_avail(p);
    else
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case unset_node:
          {
            flush_node_list(list_ptr(p));
            fast_delete_glue_ref(space_ptr(p));
            fast_delete_glue_ref(xspace_ptr(p));
            free_node(p, box_node_size);
            goto done;
          }
          break;

        case rule_node:
          {
            free_node(p, rule_node_size);
            goto done;
          }
          break;

        case ins_node:
          {
            flush_node_list(ins_ptr(p));
            delete_glue_ref(split_top_ptr(p));
            free_node(p, ins_node_size);
            goto done;
          }
          break;

        case whatsit_node:
          // @<Wipe out the whatsit node |p| and |goto done|@>
          {
            switch (subtype(p))
            {
              case open_node:
                free_node(p, open_node_size);
                break;

              case write_node:
              case special_node:
                {
                  delete_token_ref(write_tokens(p));
                  free_node(p, write_node_size);
                  goto done;
                }
                break;

              case close_node:
              case language_node:
                free_node(p, small_node_size);
                break;

              case pdf_save_pos_node:
                free_node(p, small_node_size);
                break;

              default:
                confusion("ext3");
                break;
            }

            goto done;
          }
          break;

        case glue_node:
          {
            fast_delete_glue_ref(glue_ptr(p));

            if (leader_ptr(p) != null)
              flush_node_list(leader_ptr(p));

            free_node(p, medium_node_size);
            goto done;
          }
          break;

        case disp_node:
          do_nothing();
          break;

        case kern_node:
        case math_node:
        case penalty_node:
          {
            free_node(p, medium_node_size);
            goto done;
          }
          break;

        case ligature_node:
          flush_node_list(lig_ptr(p));
          break;

        case mark_node:
          delete_token_ref(mark_ptr(p));
          break;

        case disc_node:
          {
            flush_node_list(pre_break(p));
            flush_node_list(post_break(p));
          }
          break;

        case adjust_node:
          flush_node_list(adjust_ptr(p));
          break;

        case style_node:
          {
            free_node(p, style_node_size);
            goto done;
          }
          break;

        case choice_node:
          {
            flush_node_list(display_mlist(p));
            flush_node_list(text_mlist(p));
            flush_node_list(script_mlist(p));
            flush_node_list(script_script_mlist(p));
            free_node(p, style_node_size);
            goto done;
          }
          break;

        case ord_noad:
        case op_noad:
        case bin_noad:
        case rel_noad:
        case open_noad:
        case close_noad:
        case punct_noad:
        case inner_noad:
        case radical_noad:
        case over_noad:
        case under_noad:
        case vcenter_noad:
        case accent_noad:
          {
            if ((math_type(nucleus(p)) >= sub_box)
              && (math_type(nucleus(p)) != math_jchar)
              && (math_type(nucleus(p) != math_text_jchar)))
              flush_node_list(info(nucleus(p)));

            if ((math_type(supscr(p)) >= sub_box)
              && (math_type(supscr(p)) != math_jchar)
              && (math_type(supscr(p) != math_text_jchar)))
              flush_node_list(info(supscr(p)));

            if ((math_type(subscr(p)) >= sub_box)
              && (math_type(subscr(p)) != math_jchar)
              && (math_type(subscr(p) != math_text_jchar)))
              flush_node_list(info(subscr(p)));

            if (type(p) == radical_noad)
              free_node(p, radical_noad_size);
            else if (type(p) == accent_noad)
              free_node(p, accent_noad_size);
            else
              free_node(p, noad_size);

            goto done;
          }
          break;

        case left_noad:
        case right_noad:
          {
            free_node(p, noad_size);
            goto done;
          }
          break;

        case fraction_noad:
          {
            flush_node_list(info(numerator(p)));
            flush_node_list(info(denominator(p)));
            free_node(p, fraction_noad_size);
            goto done;
          }
          break;

        default:
          confusion("flushing");
          break;
      }

      free_node(p, small_node_size);
done:;
    }

    p = q;
  }
}

// makes a duplicate of the node list that starts
// at |p| and returns a pointer to the new lis
static pointer copy_node_list (pointer p)
{
  pointer h;  // {temporary head of copied list}
  pointer q;  // {previous position in new list}
  pointer r;  // {current node being fabricated for new list}
  char words; // {number of words remaining to be copied}

  h = get_avail();
  q = h;

  while (p != null)
  {
    // @<Make a copy of node |p| in node |r|@>
    words = 1;  // {this setting occurs in more branches than any other}

    if (is_char_node(p))
      r = get_avail();
    else switch (type(p))
    {
      /*
        @<Case statement to copy different types and set |words| to the number
        of initial words not yet copied@>
      */
      case dir_node:
      case hlist_node:
      case vlist_node:
      case unset_node:
        {
          r = get_node(box_node_size);
          sync_tag(r + box_node_size) = sync_tag(p + box_node_size);
          sync_line(r + box_node_size) = sync_line(p + box_node_size);
          mem[r + 7] = mem[p + 7];
          mem[r + 6] = mem[p + 6];
          mem[r + 5] = mem[p + 5];  // {copy the last three words}
          add_glue_ref(space_ptr(r));
          add_glue_ref(xspace_ptr(r));
          list_ptr(r) = copy_node_list(list_ptr(p));  // {this affects |mem[r+5]|}
          words = 5;
        }
        break;

      case rule_node:
        {
          r = get_node(rule_node_size);
          words = rule_node_size - synctex_field_size;
        }
        break;

      case ins_node:
        {
          r = get_node(ins_node_size);
          mem[r + 5] = mem[p + 5];
          mem[r + 4] = mem[p + 4];
          add_glue_ref(split_top_ptr(p));
          ins_ptr(r) = copy_node_list(ins_ptr(p));  // {this affects |mem[r+4]|}
          words = ins_node_size - 2;
        }
        break;

      case whatsit_node:
        /*
          @<Make a partial copy of the whatsit node |p| and make |r|
          point to it; set |words| to the number of initial words not yet copied@>
        */
        switch (subtype(p))
        {
          case open_node:
            {
              r = get_node(open_node_size);
              words = open_node_size;
            }
            break;

          case write_node:
          case special_node:
            {
              r = get_node(write_node_size);
              add_token_ref(write_tokens(p));
              words = write_node_size;
            }
            break;

          case close_node:
          case language_node:
            {
              r = get_node(small_node_size);
              words = small_node_size;
            }
            break;

          case pdf_save_pos_node:
            r = get_node(small_node_size);
            break;

          default:
            confusion("ext2");
            break;
        }
        break;

      case glue_node:
        {
          r = get_node(medium_node_size);
          sync_tag(r + medium_node_size) = sync_tag(p + medium_node_size);
          sync_line(r + medium_node_size) = sync_line(p + medium_node_size);
          add_glue_ref(glue_ptr(p));
          glue_ptr(r) = glue_ptr(p);
          leader_ptr(r) = copy_node_list(leader_ptr(p));
        }
        break;

      case disp_node:
        {
          r = get_node(small_node_size);
          words = small_node_size;
        }
        break;

      case kern_node:
      case math_node:
      case penalty_node:
        {
          r = get_node(medium_node_size);
          words = medium_node_size;
        }
        break;

      case ligature_node:
        {
          r = get_node(small_node_size);
          mem[lig_char(r)] = mem[lig_char(p)];  // {copy |font| and |character|}
          lig_ptr(r) = copy_node_list(lig_ptr(p));
        }
        break;

      case disc_node:
        {
          r = get_node(small_node_size);
          pre_break(r) = copy_node_list(pre_break(p));
          post_break(r) = copy_node_list(post_break(p));
        }
        break;

      case mark_node:
        {
          r = get_node(small_node_size);
          add_token_ref(mark_ptr(p));
          words = small_node_size;
        }
        break;

      case adjust_node:
        {
          r = get_node(small_node_size);
          adjust_ptr(r) = copy_node_list(adjust_ptr(p));
        }
        // {|words=1=small_node_size-1|}
        break;

      default:
        confusion("copying");
        break;
    }

    while (words > 0)
    {
      decr(words);
      mem[r + words] = mem[p + words];
    }

    link(q) = r;
    q = r;
    p = link(p);
  }

  link(q) = null;
  q = link(h);
  free_avail(h);

  return q;
}

// prints the mode represented by |m|
static void print_mode (integer m)
{ 
  if (m > 0)
  {
    switch (m / (max_command + 1))
    {
      case 0:
        prints("vertical");
        break;

      case 1:
        prints("horizontal");
        break;

      case 2:
        prints("display math");
        break;
    }
  }
  else
  {
    if (m == 0)
      prints("no");
    else
    {
      switch ((-m) / (max_command + 1))
      {
        case 0:
          prints("internal vertical");
          break;

        case 1:
          prints("restricted horizontal");
          break;

        case 2:
          prints("math");
          break;
      }
    }
  }

  prints(" mode");
}

// enter a new semantic level, save the old
static void push_nest (void) 
{
  if (nest_ptr > max_nest_stack)
  {
    max_nest_stack = nest_ptr;

#ifdef APTEX_EXTENSION
    if (nest_ptr == current_nest_size)
      nest = realloc_nest_stack(increment_nest_size);

    if (nest_ptr == current_nest_size)
      overflow("semantic nest size", current_nest_size);
#else
    if (nest_ptr == nest_size)
      overflow("semantic nest size", nest_size);
#endif
  }

  nest[nest_ptr] = cur_list;
  incr(nest_ptr);
  head = new_null_box();
  tail = head;
  prev_node = tail;
  prev_graf = 0;
  prev_disp = 0;
  disp_called = false;
  last_jchr = null;
  mode_line = line;
  eTeX_aux = 0;
}

// leave a semantic level, re-enter the old
void pop_nest (void) 
{
  fast_delete_glue_ref(space_ptr(head));
  fast_delete_glue_ref(xspace_ptr(head));
  free_node(head, box_node_size);
  decr(nest_ptr);
  cur_list = nest[nest_ptr];
}

static void show_activities (void)
{
  integer p;  // {index into |nest|}
  short m;  // {mode}
  memory_word a;  // {auxiliary}
  pointer q, r; // {for showing the current page}
  integer t;  // {ditto}

  nest[nest_ptr] = cur_list;  // {put the top level into the array}
  print_nl("");
  print_ln();

  for (p = nest_ptr; p >= 0; p--)
  {
    m = nest[p].mode_field;
    a = nest[p].aux_field;
    print_nl("### ");
    print_direction(nest[p].dir_field);
    prints(", ");
    print_mode(m);
    prints(" entered at line ");
    print_int(abs(nest[p].ml_field));

    if (m == hmode)
    {
      if (nest[p].pg_field != 040600000)
      {
        prints(" (language");
        print_int(nest[p].pg_field % 0200000);
        prints(":hyphenmin");
        print_int(nest[p].pg_field / 020000000);
        print_char(',');
        print_int((nest[p].pg_field / 0200000) % 0100);
        print_char(')');
      }
    }

    if (nest[p].ml_field < 0)
      prints(" (\\output routine)");

    if (p == 0)
    {
      // @<Show the status of the current page@>
      if (page_head != page_tail)
      {
        print_nl("### current page:");
        
        if (output_active)
          prints(" (held over for next output)");

        show_box(link(page_head));

        if (page_contents > empty)
        {
          print_nl("total height ");
          print_totals();
          print_nl(" goal height ");
          print_scaled(page_goal);
          r = link(page_ins_head);

          while (r != page_ins_head)
          {
            print_ln();
            print_esc("insert");
            t = subtype(r);
            print_int(t);
            prints(" adds ");

            if (count(t) == 1000)
              t = height(r);
            else
              t = x_over_n(height(r), 1000) * count(t);

            print_scaled(t);

            if (type(r) == split_up)
            {
              q = page_head;
              t = 0;

              do {
                q = link(q);

                if ((type(q) == ins_node) && (subtype(q) == subtype(r)))
                  incr(t);
              } while (!(q == broken_ins(r)));

              prints(", #");
              print_int(t);
              prints(" might split");
            }

            r = link(r);
          }
        }
      }

      if (link(contrib_head) != null)
        print_nl("### recent contributions:");
    }

    show_box(link(nest[p].head_field));

    // @<Show the auxiliary field, |a|@>
    switch (abs(m) / (max_command + 1))
    {
      case 0:
        {
          print_nl("prevdepth ");

          if (a.sc <= ignore_depth)
            prints("ignored");
          else
            print_scaled(a.sc);

          if (nest[p].pg_field != 0)
          {
            prints(", prevgraf ");
            print_int(nest[p].pg_field);
            prints(" line");

            if (nest[p].pg_field != 1)
              print_char('s');
          }
        }
        break;

      case 1:
        {
          print_nl("spacefactor ");
          print_int(a.hh.lh);

          if (m > 0)
            if (a.hh.rh > 0)
            {
              prints(", current language ");
              print_int(a.hh.rh);
            }
        }
        break;

      case 2:
        if (a.cint != null)
        {
          prints("this will begin denominator of:");
          show_box(a.cint);
        }
        break;
    }
  }
}

static void print_param (integer n)
{
  switch (n)
  {
    case pretolerance_code:
      print_esc("pretolerance");
      break;

    case tolerance_code:
      print_esc("tolerance");
      break;

    case line_penalty_code:
      print_esc("linepenalty");
      break;

    case hyphen_penalty_code:
      print_esc("hyphenpenalty");
      break;

    case ex_hyphen_penalty_code:
      print_esc("exhyphenpenalty");
      break;

    case club_penalty_code:
      print_esc("clubpenalty");
      break;

    case widow_penalty_code:
      print_esc("widowpenalty");
      break;

    case display_widow_penalty_code:
      print_esc("displaywidowpenalty");
      break;

    case broken_penalty_code:
      print_esc("brokenpenalty");
      break;

    case bin_op_penalty_code:
      print_esc("binoppenalty");
      break;

    case rel_penalty_code:
      print_esc("relpenalty");
      break;

    case pre_display_penalty_code:
      print_esc("predisplaypenalty");
      break;

    case post_display_penalty_code:
      print_esc("postdisplaypenalty");
      break;

    case inter_line_penalty_code:
      print_esc("interlinepenalty");
      break;

    case double_hyphen_demerits_code:
      print_esc("doublehyphendemerits");
      break;

    case final_hyphen_demerits_code:
      print_esc("finalhyphendemerits");
      break;

    case adj_demerits_code:
      print_esc("adjdemerits");
      break;

    case mag_code:
      print_esc("mag");
      break;

    case delimiter_factor_code:
      print_esc("delimiterfactor");
      break;

    case looseness_code:
      print_esc("looseness");
      break;

    case time_code:
      print_esc("time");
      break;

    case day_code:
      print_esc("day");
      break;

    case month_code:
      print_esc("month");
      break;

    case year_code:
      print_esc("year");
      break;

    case show_box_breadth_code:
      print_esc("showboxbreadth");
      break;

    case show_box_depth_code:
      print_esc("showboxdepth");
      break;

    case hbadness_code:
      print_esc("hbadness");
      break;

    case vbadness_code:
      print_esc("vbadness");
      break;

    case pausing_code:
      print_esc("pausing");
      break;

    case tracing_online_code:
      print_esc("tracingonline");
      break;

    case tracing_macros_code:
      print_esc("tracingmacros");
      break;

    case tracing_stats_code:
      print_esc("tracingstats");
      break;

    case tracing_paragraphs_code:
      print_esc("tracingparagraphs");
      break;

    case tracing_pages_code:
      print_esc("tracingpages");
      break;

    case tracing_output_code:
      print_esc("tracingoutput");
      break;

    case tracing_lost_chars_code:
      print_esc("tracinglostchars");
      break;

    case tracing_commands_code:
      print_esc("tracingcommands");
      break;

    case tracing_restores_code:
      print_esc("tracingrestores");
      break;

    case tracing_fontloaders_code:
      print_esc("tracingfontloaders");
      break;

    case uc_hyph_code:
      print_esc("uchyph");
      break;

    case output_penalty_code:
      print_esc("outputpenalty");
      break;

    case max_dead_cycles_code:
      print_esc("maxdeadcycles");
      break;

    case hang_after_code:
      print_esc("hangafter");
      break;

    case floating_penalty_code:
      print_esc("floatingpenalty");
      break;

    case global_defs_code:
      print_esc("globaldefs");
      break;

    case cur_fam_code:
      print_esc("fam");
      break;

    case cur_jfam_code:
      print_esc("jfam");
      break;

    case escape_char_code:
      print_esc("escapechar");
      break;

    case default_hyphen_char_code:
      print_esc("defaulthyphenchar");
      break;

    case default_skew_char_code:
      print_esc("defaultskewchar");
      break;

    case end_line_char_code:
      print_esc("endlinechar");
      break;

    case new_line_char_code:
      print_esc("newlinechar");
      break;

    case language_code:
      print_esc("language");
      break;

    case left_hyphen_min_code:
      print_esc("lefthyphenmin");
      break;

    case right_hyphen_min_code:
      print_esc("righthyphenmin");
      break;

    case holding_inserts_code:
      print_esc("holdinginserts");
      break;

    case error_context_lines_code:
      print_esc("errorcontextlines");
      break;

    case jchr_widow_penalty_code:
      print_esc("jcharwidowpenalty");
      break;
    
    case text_baseline_shift_factor_code:
      print_esc("textbaselineshiftfactor");
      break;

    case script_baseline_shift_factor_code:
      print_esc("scriptbaselineshiftfactor");
      break;

    case scriptscript_baseline_shift_factor_code:
      print_esc("scriptscriptbaselineshiftfactor");
      break;

    case ptex_lineend_code:
      print_esc("ptexlineendmode");
      break;

    case ptex_tracing_fonts_code:
      print_esc("ptextracingfonts");
      break;

    case tracing_assigns_code:
      print_esc("tracingassigns");
      break;

    case tracing_groups_code:
      print_esc("tracinggroups");
      break;

    case tracing_ifs_code:
      print_esc("tracingifs");
      break;

    case tracing_scan_tokens_code:
      print_esc("tracingscantokens");
      break;

    case tracing_nesting_code:
      print_esc("tracingnesting");
      break;

    case pre_display_direction_code:
      print_esc("predisplaydirection");
      break;

    case last_line_fit_code:
      print_esc("lastlinefit");
      break;

    case saving_vdiscards_code:
      print_esc("savingvdiscards");
      break;

    case saving_hyph_codes_code:
      print_esc("savinghyphcodes");
      break;

    case eTeX_state_code + TeXXeT_code:
      print_esc("TeXXeTstate");
      break;

    case pdf_compress_level_code:
      print_esc("pdfcompresslevel");
      break;

    case pdf_major_version_code:
      print_esc("pdfmajorversion");
      break;

    case pdf_minor_version_code:
      print_esc("pdfminorversion");
      break;

    case synctex_code:
      print_esc("synctex");
      break;

    case tracing_stack_levels_code:
      print_esc("tracingstacklevels");
      break;

    case show_stream_code:
      print_esc("showstream");
      break;

    default:
      prints("[unknown integer parameter!]");
      break;
  }
}

// prepare to do some tracing
void begin_diagnostic (void)
{
  old_setting = selector;

  if ((tracing_online <= 0) && (selector == term_and_log))
  {
    decr(selector);

    if (history == spotless)
      history = warning_issued;
  }
}

// restore proper conditions after tracing
void end_diagnostic (boolean blank_line)
{
  print_nl("");

  if (blank_line)
    print_ln();

  selector = old_setting;
}

static void print_length_param (integer n)
{
  switch (n)
  {
    case par_indent_code:
      print_esc("parindent");
      break;

    case math_surround_code:
      print_esc("mathsurround");
      break;

    case line_skip_limit_code:
      print_esc("lineskiplimit");
      break;

    case hsize_code:
      print_esc("hsize");
      break;

    case vsize_code:
      print_esc("vsize");
      break;

    case max_depth_code:
      print_esc("maxdepth");
      break;

    case split_max_depth_code:
      print_esc("splitmaxdepth");
      break;

    case box_max_depth_code:
      print_esc("boxmaxdepth");
      break;

    case hfuzz_code:
      print_esc("hfuzz");
      break;

    case vfuzz_code:
      print_esc("vfuzz");
      break;

    case delimiter_shortfall_code:
      print_esc("delimitershortfall");
      break;

    case null_delimiter_space_code:
      print_esc("nulldelimiterspace");
      break;

    case script_space_code:
      print_esc("scriptspace");
      break;

    case pre_display_size_code:
      print_esc("predisplaysize");
      break;

    case display_width_code:
      print_esc("displaywidth");
      break;

    case display_indent_code:
      print_esc("displayindent");
      break;

    case overfull_rule_code:
      print_esc("overfullrule");
      break;

    case hang_indent_code:
      print_esc("hangindent");
      break;

    case h_offset_code:
      print_esc("hoffset");
      break;

    case v_offset_code:
      print_esc("voffset");
      break;

    case t_baseline_shift_code:
      print_esc("tbaselineshift");
      break;

    case y_baseline_shift_code:
      print_esc("ybaselineshift");
      break;

    case emergency_stretch_code:
      print_esc("emergencystretch");
      break;

    case pdf_h_origin_code:
      print_esc("pdfhorigin");
      break;

    case pdf_v_origin_code:
      print_esc("pdfvorigin");
      break;

    case pdf_page_width_code:
      print_esc("pdfpagewidth");
      break;

    case pdf_page_height_code:
      print_esc("pdfpageheight");
      break;

    default:
      prints("[unknown dimen parameter!]");
      break;
  }
}

void print_cmd_chr (quarterword cmd, halfword chr_code)
{
  integer n;

  switch (cmd)
  {
    case left_brace:
      chr_cmd("begin-group character ");
      break;

    case right_brace:
      chr_cmd("end-group character ");
      break;

    case math_shift:
      chr_cmd("math shift character ");
      break;

    case mac_param:
      chr_cmd("macro parameter character ");
      break;

    case sup_mark:
      chr_cmd("superscript character ");
      break;

    case sub_mark:
      chr_cmd("subscript character ");
      break;

    case endv:
      prints("end of alignment template");
      break;

    case spacer:
      chr_cmd("blank space ");
      break;

    case letter:
      chr_cmd("the letter ");
      break;

    case other_char:
      chr_cmd("the character ");
      break;

    case kanji:
    case kana:
    case other_kchar:
    case hangul:
      {
        prints("kanji character ");
        print_kanji(KANJI(chr_code));
      }
      break;

    case assign_glue:
    case assign_mu_glue:
      if (chr_code < skip_base)
        print_skip_param(chr_code - glue_base);
      else if (chr_code < mu_skip_base)
      {
        print_esc("skip");
        print_int(chr_code - skip_base);
      }
      else
      {
        print_esc("muskip");
        print_int(chr_code - mu_skip_base);
      }
      break;

    case assign_toks:
      if (chr_code >= toks_base)
      {
        print_esc("toks");
        print_int(chr_code - toks_base);
      }
      else switch (chr_code)
      {
        case output_routine_loc:
          print_esc("output");
          break;

        case every_par_loc:
          print_esc("everypar");
          break;

        case every_math_loc:
          print_esc("everymath");
          break;

        case every_display_loc:
          print_esc("everydisplay");
          break;

        case every_hbox_loc:
          print_esc("everyhbox");
          break;

        case every_vbox_loc:
          print_esc("everyvbox");
          break;

        case every_job_loc:
          print_esc("everyjob");
          break;

        case every_cr_loc:
          print_esc("everycr");
          break;

        case every_eof_loc:
          print_esc("everyeof");
          break;

        default:
          print_esc("errhelp");
          break;
      }
      break;

    case assign_int:
      if (chr_code < count_base)
        print_param(chr_code - int_base);
      else
      {
        print_esc("count");
        print_int(chr_code - count_base);
      }
      break;

    case assign_dimen:
      if (chr_code < scaled_base)
        print_length_param(chr_code - dimen_base);
      else
      {
        print_esc("dimen");
        print_int(chr_code - scaled_base);
      }
      break;

    case accent:
      print_esc("accent");
      break;

    case advance:
      print_esc("advance");
      break;

    case after_assignment:
      print_esc("afterassignment");
      break;

    case after_group:
      print_esc("aftergroup");
      break;

    case assign_font_dimen:
      print_esc("fontdimen");
      break;

    case begin_group:
      print_esc("begingroup");
      break;

    case break_penalty:
      print_esc("penalty");
      break;

    case char_num:
      print_esc("char");
      break;

    case kchar_num:
      print_esc("kchar");
      break;

    case cs_name:
      print_esc("csname");
      break;

    case def_font:
      print_esc("font");
      break;

    case def_jfont:
      print_esc("jfont");
      break;

    case def_tfont:
      print_esc("tfont");
      break;

    case delim_num:
      print_esc("delimiter");
      break;

    case divide:
      print_esc("divide");
      break;

    case end_cs_name:
      print_esc("endcsname");
      break;

    case end_group:
      print_esc("endgroup");
      break;

    case ex_space:
      print_esc(" ");
      break;

    case expand_after:
      if (chr_code == 0)
        print_esc("expandafter");
      else
        print_esc("unless");
      break;

    case halign:
      print_esc("halign");
      break;

    case hrule:
      print_esc("hrule");
      break;

    case ignore_spaces:
      if (chr_code == 0)
        print_esc("ignorespaces");
      else
        print_esc("pdfprimitive");
      break;

    case insert:
      print_esc("insert");
      break;

    case ital_corr:
      print_esc("/");
      break;

    case mark:
      {
        print_esc("mark");

        if (chr_code > 0)
          print_char('s');
      }
      break;

    case math_accent:
      print_esc("mathaccent");
      break;

    case math_char_num:
      print_esc("mathchar");
      break;

    case math_choice:
      print_esc("mathchoice");
      break;

    case multiply:
      print_esc("multiply");
      break;

    case no_align:
      print_esc("noalign");
      break;

    case no_boundary:
      print_esc("noboundary");
      break;

    case no_expand:
      if (chr_code == 0)
        print_esc("noexpand");
      else
        print_esc("pdfprimitive");
      break;

    case non_script:
      print_esc("nonscript");
      break;

    case omit:
      print_esc("omit");
      break;

    case radical:
      print_esc("radical");
      break;

    case read_to_cs:
      if (chr_code == 0)
        print_esc("read");
      else
        print_esc("readline");
      break;

    case relax:
      print_esc("relax");
      break;

    case set_box:
      print_esc("setbox");
      break;

    case set_prev_graf:
      print_esc("prevgraf");
      break;

    case set_shape:
      switch (chr_code)
      {
        case par_shape_loc:
          print_esc("parshape");
          break;

        case inter_line_penalties_loc:
          print_esc("interlinepenalties");
          break;

        case club_penalties_loc:
          print_esc("clubpenalties");
          break;

        case widow_penalties_loc:
          print_esc("widowpenalties");
          break;

        case display_widow_penalties_loc:
          print_esc("displaywidowpenalties");
          break;
      }
      break;

    case the:
      if (chr_code == 0)
        print_esc("the");
      else if (chr_code == 1)
        print_esc("unexpanded");
      else
        print_esc("detokenize");
      break;

    case toks_register:
      {
        print_esc("toks");
      
        if (chr_code != mem_bot)
          print_sa_num(chr_code);
      }
      break;

    case vadjust:
      print_esc("vadjust");
      break;

    case valign:
      if (chr_code == 0)
        print_esc("valign");
      else switch (chr_code)
      {
        case begin_L_code:
          print_esc("beginL");
          break;

        case end_L_code:
          print_esc("endL");
          break;

        case begin_R_code:
          print_esc("beginR");
          break;

        default:
          print_esc("endR");
          break;
      }
      break;

    case vcenter:
      print_esc("vcenter");
      break;

    case vrule:
      print_esc("vrule");
      break;

    case partoken_name:
      print_esc("partokenname");
      break;

    case par_end:
      print_esc("par");
      break;

    case input:
      if (chr_code == 0)
        print_esc("input");
      else if (chr_code == 2)
        print_esc("scantokens");
      else
        print_esc("endinput");
      break;

    case top_bot_mark:
      switch (chr_code % marks_code)
      {
        case first_mark_code:
          print_esc("firstmark");
          break;

        case bot_mark_code:
          print_esc("botmark");
          break;

        case split_first_mark_code:
          print_esc("splitfirstmark");
          break;

        case split_bot_mark_code:
          print_esc("splitbotmark");
          break;

        default:
          print_esc("topmark");
          break;
      }

      if (chr_code >= marks_code)
        print_char('s');
      break;

    case tex_register:
      {
        if ((chr_code < mem_bot) || (chr_code > lo_mem_stat_max))
          cmd = sa_type(chr_code);
        else
        {
          cmd = chr_code - mem_bot;
          chr_code = null;
        }

        if (cmd == int_val)
          print_esc("count");
        else if (cmd == dimen_val)
          print_esc("dimen");
        else if (cmd == glue_val)
          print_esc("skip");
        else
          print_esc("muskip");

        if (chr_code != 0)
          print_sa_num(chr_code);
      }
      break;

    case set_aux:
      if (chr_code == vmode)
        print_esc("prevdepth");
      else
        print_esc("spacefactor");
      break;

    case set_page_int:
      if (chr_code == 0)
        print_esc("deadcycles");
      else if (chr_code == 2)
        print_esc("interactionmode");
      else
        print_esc("insertpenalties");
      break;

    case set_box_dimen:
      if (chr_code == width_offset)
        print_esc("wd");
      else if (chr_code == height_offset)
        print_esc("ht");
      else
        print_esc("dp");
      break;

    case last_item:
      switch (chr_code)
      {
        case int_val:
          print_esc("lastpenalty");
          break;

        case dimen_val:
          print_esc("lastkern");
          break;

        case glue_val:
          print_esc("lastskip");
          break;

        case input_line_no_code:
          print_esc("inputlineno");
          break;

        case shell_escape_code:
          print_esc("shellescape");
          break;

        case ptex_version_code:
          print_esc("ptexversion");
          break;

        case uptex_version_code:
          print_esc("uptexversion");
          break;

        case eptex_version_code:
          print_esc("epTeXversion");
          break;

        case ptex_minor_version_code:
          print_esc("ptexminorversion");
          break;

        case last_node_type_code:
          print_esc("lastnodetype");
          break;

        case last_node_char_code:
          print_esc("lastnodechar");
          break;

        case last_node_subtype_code:
          print_esc("lastnodesubtype");
          break;

        case eTeX_version_code:
          print_esc("eTeXversion");
          break;

        case pdf_last_x_pos_code:
          print_esc("pdflastxpos");
          break;

        case pdf_last_y_pos_code:
          print_esc("pdflastypos");
          break;

        case elapsed_time_code:
          print_esc("pdfelapsedtime");
          break;

        case random_seed_code:
          print_esc("pdfrandomseed");
          break;

        case current_group_level_code:
          print_esc("currentgrouplevel");
          break;

        case current_group_type_code:
          print_esc("currentgrouptype");
          break;

        case current_if_level_code:
          print_esc("currentiflevel");
          break;

        case current_if_type_code:
          print_esc("currentiftype");
          break;

        case current_if_branch_code:
          print_esc("currentifbranch");
          break;

        case font_char_wd_code:
          print_esc("fontcharwd");
          break;

        case font_char_ht_code:
          print_esc("fontcharht");
          break;

        case font_char_dp_code:
          print_esc("fontchardp");
          break;

        case font_char_ic_code:
          print_esc("fontcharic");
          break;

        case par_shape_length_code:
          print_esc("parshapelength");
          break;

        case par_shape_indent_code:
          print_esc("parshapeindent");
          break;

        case par_shape_dimen_code:
          print_esc("parshapedimen");
          break;

        case eTeX_expr - int_val + int_val:
          print_esc("numexpr");
          break;

        case eTeX_expr - int_val + dimen_val:
          print_esc("dimexpr");
          break;

        case eTeX_expr - int_val + glue_val:
          print_esc("glueexpr");
          break;

        case eTeX_expr - int_val + mu_val:
          print_esc("muexpr");
          break;

        case glue_stretch_order_code:
          print_esc("gluestretchorder");
          break;

        case glue_shrink_order_code:
          print_esc("glueshrinkorder");
          break;

        case current_spacing_mode_code:
          print_esc("currentspacingmode");
          break;

        case current_xspacing_mode_code:
          print_esc("currentxspacingmode");
          break;

        case current_cjk_token_code:
          print_esc("currentcjktoken");
          break;

        case glue_stretch_code:
          print_esc("gluestretch");
          break;

        case glue_shrink_code:
          print_esc("glueshrink");
          break;

        case mu_to_glue_code:
          print_esc("mutoglue");
          break;

        case glue_to_mu_code:
          print_esc("gluetomu");
          break;

        default:
          print_esc("badness");
          break;
      }
      break;

    case convert:
      switch (chr_code)
      {
        case number_code:
          print_esc("number");
          break;

        case roman_numeral_code:
          print_esc("romannumeral");
          break;

        case string_code:
          print_esc("string");
          break;

        case meaning_code:
          print_esc("meaning");
          break;

        case font_name_code:
          print_esc("fontname");
          break;

        case kansuji_code:
          print_esc("kansuji");
          break;

        case euc_code:
          print_esc("euc");
          break;

        case sjis_code:
          print_esc("sjis");
          break;

        case jis_code:
          print_esc("jis");
          break;

        case kuten_code:
          print_esc("kuten");
          break;

        case ptex_revision_code:
          print_esc("ptexrevision");
          break;

        case uptex_revision_code:
          print_esc("uptexrevision");
          break;

        case ucs_code:
          print_esc("ucs");
          break;

        case toucs_code:
          print_esc("toucs");
          break;

        case tojis_code:
          print_esc("tojis");
          break;

        case ptex_font_name_code:
          print_esc("ptexfontname");
          break;

        case eTeX_revision_code:
          print_esc("eTeXrevision");
          break;

        case ng_strcmp_code:
          print_esc("pdfstrcmp");
          break;

        case ng_banner_code:
          print_esc("ngbanner");
          break;

        case ng_os_type_code:
          print_esc("ngostype");
          break;

        case pdf_creation_date_code:
          print_esc("pdfcreationdate");
          break;

        case pdf_file_mod_date_code:
          print_esc("pdffilemoddate");
          break;

        case pdf_file_size_code:
          print_esc("pdffilesize");
          break;

        case pdf_mdfive_sum_code:
          print_esc("pdfmdfivesum");
          break;

        case pdf_file_dump_code:
          print_esc("pdffiledump");
          break;

        case pdf_uniform_deviate_code:
          print_esc("pdfuniformdeviate");
          break;

        case pdf_normal_deviate_code:
          print_esc("pdfnormaldeviate");
          break;

        case expanded_code:
          print_esc("expanded");
          break;

        case Uchar_convert_code:
          print_esc("Uchar");
          break;

        case Ucharcat_convert_code:
          print_esc("Ucharcat");
          break;

        default:
          print_esc("jobname");
          break;
      }
      break;

    case if_test:
      {
        if (chr_code >= unless_code)
          print_esc("unless");

        switch (chr_code % unless_code)
        {
          case if_cat_code:
            print_esc("ifcat");
            break;

          case if_int_code:
            print_esc("ifnum");
            break;

          case if_dim_code:
            print_esc("ifdim");
            break;

          case if_odd_code:
            print_esc("ifodd");
            break;

          case if_vmode_code:
            print_esc("ifvmode");
            break;

          case if_hmode_code:
            print_esc("ifhmode");
            break;

          case if_mmode_code:
            print_esc("ifmmode");
            break;

          case if_inner_code:
            print_esc("ifinner");
            break;

          case if_void_code:
            print_esc("ifvoid");
            break;

          case if_hbox_code:
            print_esc("ifhbox");
            break;

          case if_vbox_code:
            print_esc("ifvbox");
            break;

          case ifx_code:
            print_esc("ifx");
            break;

          case if_eof_code:
            print_esc("ifeof");
            break;

          case if_true_code:
            print_esc("iftrue");
            break;

          case if_false_code:
            print_esc("iffalse");
            break;

          case if_case_code:
            print_esc("ifcase");
            break;

          case if_tdir_code:
            print_esc("iftdir");
            break;

          case if_ydir_code:
            print_esc("ifydir");
            break;

          case if_ddir_code:
            print_esc("ifddir");
            break;

          case if_mdir_code:
            print_esc("ifmdir");
            break;

          case if_tbox_code:
            print_esc("iftbox");
            break;

          case if_ybox_code:
            print_esc("ifybox");
            break;

          case if_dbox_code:
            print_esc("ifdbox");
            break;

          case if_mbox_code:
            print_esc("ifmbox");
            break;

          case if_pdfprimitive_code:
            print_esc("ifpdfprimitive");
            break;

          case if_jfont_code:
            print_esc("ifjfont");
            break;

          case if_tfont_code:
            print_esc("iftfont");
            break;

          case if_def_code:
            print_esc("ifdefined");
            break;

          case if_cs_code:
            print_esc("ifcsname");
            break;

          case if_font_char_code:
            print_esc("iffontchar");
            break;

          case if_in_csname_code:
            print_esc("ifincsname");
            break;

          default:
            print_esc("if");
            break;
        }
      }
      break;

    case fi_or_else:
      if (chr_code == fi_code)
        print_esc("fi");
      else if (chr_code == or_code)
        print_esc("or");
      else
        print_esc("else");
      break;

    case tab_mark:
      if (chr_code == span_code)
        print_esc("span");
      else
        chr_cmd("alignment tab character ");
      break;

    case car_ret:
      if (chr_code == cr_code)
        print_esc("cr");
      else
        print_esc("crcr");
      break;

    case set_page_dimen:
      switch (chr_code)
      {
        case 0:
          print_esc("pagegoal");
          break;

        case 1:
          print_esc("pagetotal");
          break;

        case 2:
          print_esc("pagestretch");
          break;

        case 3:
          print_esc("pagefilstretch");
          break;

        case 4:
          print_esc("pagefillstretch");
          break;

        case 5:
          print_esc("pagefilllstretch");
          break;

        case 6:
          print_esc("pageshrink");
          break;

        default:
          print_esc("pagedepth");
          break;
      }
      break;

    case stop:
      if (chr_code == 1)
        print_esc("dump");
      else
        print_esc("end");
      break;

    case hskip:
      switch (chr_code)
      {
        case skip_code:
          print_esc("hskip");
          break;

        case fil_code:
          print_esc("hfil");
          break;

        case fill_code:
          print_esc("hfill");
          break;

        case ss_code:
          print_esc("hss");
          break;

        default:
          print_esc("hfilneg");
          break;
      }
      break;

    case vskip:
      switch (chr_code)
      {
        case skip_code:
          print_esc("vskip");
          break;

        case fil_code:
          print_esc("vfil");
          break;

        case fill_code:
          print_esc("vfill");
          break;

        case ss_code:
          print_esc("vss");
          break;

        default:
          print_esc("vfilneg");
          break;
      }
      break;

    case mskip:
      print_esc("mskip");
      break;

    case kern:
      print_esc("kern");
      break;

    case mkern:
      print_esc("mkern");
      break;

    case hmove:
      if (chr_code == 1)
        print_esc("moveleft");
      else
        print_esc("moveright");
      break;

    case vmove:
      if (chr_code == 1)
        print_esc("raise");
      else
        print_esc("lower");
      break;

    case make_box:
      switch (chr_code)
      {
        case box_code:
          print_esc("box");
          break;

        case copy_code:
          print_esc("copy");
          break;

        case last_box_code:
          print_esc("lastbox");
          break;

        case vsplit_code:
          print_esc("vsplit");
          break;

        case vtop_code:
          print_esc("vtop");
          break;

        case vtop_code + vmode:
          print_esc("vbox");
          break;

        default:
          print_esc("hbox");
          break;
      }
      break;

    case chg_dir:
      switch (chr_code)
      {
        case dir_yoko:
          print_esc("yoko");
          break;

        case dir_tate:
          print_esc("tate");
          break;

        case dir_dtou:
          print_esc("dtou");
          break;
      }
      break;

    case leader_ship:
      if (chr_code == a_leaders)
        print_esc("leaders");
      else if (chr_code == c_leaders)
        print_esc("cleaders");
      else if (chr_code == x_leaders)
        print_esc("xleaders");
      else
        print_esc("shipout");
      break;

    case start_par:
      if (chr_code == 0)
        print_esc("noindent");
      else if (chr_code == 1)
        print_esc("indent");
      else
        print_esc("quitvmode");
      break;

    case remove_item:
      if (chr_code == glue_node)
        print_esc("unskip");
      else if (chr_code == kern_node)
        print_esc("unkern");
      else
        print_esc("unpenalty");
      break;

    case un_hbox:
      if (chr_code == copy_code)
        print_esc("unhcopy");
      else
        print_esc("unhbox");
      break;

    case un_vbox:
      if (chr_code == copy_code)
        print_esc("unvcopy");
      else if (chr_code == last_box_code)
        print_esc("pagediscards");
      else if (chr_code == vsplit_code)
        print_esc("splitdiscards");
      else
        print_esc("unvbox");
      break;

    case discretionary:
      if (chr_code == 1)
        print_esc("-");
      else
        print_esc("discretionary");
      break;

    case eq_no:
      if (chr_code == 1)
        print_esc("leqno");
      else
        print_esc("eqno");
      break;

    case math_comp:
      switch (chr_code)
      {
        case ord_noad:
          print_esc("mathord");
          break;

        case op_noad:
          print_esc("mathop");
          break;

        case bin_noad:
          print_esc("mathbin");
          break;

        case rel_noad:
          print_esc("mathrel");
          break;

        case open_noad:
          print_esc("mathopen");
          break;

        case close_noad:
          print_esc("mathclose");
          break;

        case punct_noad:
          print_esc("mathpunct");
          break;

        case inner_noad:
          print_esc("mathinner");
          break;

        case under_noad:
          print_esc("underline");
          break;

        default:
          print_esc("overline");
          break;
      }
      break;

    case limit_switch:
      if (chr_code == limits)
        print_esc("limits");
      else if (chr_code == no_limits)
        print_esc("nolimits");
      else
        print_esc("displaylimits");
      break;

    case math_style:
      print_style(chr_code);
      break;

    case above:
      switch (chr_code)
      {
        case over_code:
          print_esc("over");
          break;

        case atop_code:
          print_esc("atop");
          break;

        case delimited_code + above_code:
          print_esc("abovewithdelims");
          break;

        case delimited_code + over_code:
          print_esc("overwithdelims");
          break;

        case delimited_code + atop_code:
          print_esc("atopwithdelims");
          break;

        default:
          print_esc("above");
          break;
      }
      break;

    case left_right:
      if (chr_code == left_noad)
        print_esc("left");
      else if (chr_code == middle_noad)
        print_esc("middle");
      else
        print_esc("right");
      break;

    case prefix:
      if (chr_code == 1)
        print_esc("long");
      else if (chr_code == 2)
        print_esc("outer");
      else if (chr_code == 8)
        print_esc("protected");
      else
        print_esc("global");
      break;

    case def:
      if (chr_code == 0)
        print_esc("def");
      else if (chr_code == 1)
        print_esc("gdef");
      else if (chr_code == 2)
        print_esc("edef");
      else
        print_esc("xdef");
      break;

    case let:
      if (chr_code != normal)
        print_esc("futurelet");
      else
        print_esc("let");
      break;

    case shorthand_def:
      switch (chr_code)
      {
        case char_def_code:
          print_esc("chardef");
          break;

        case kchar_def_code:
          print_esc("kchardef");
          break;

        case math_char_def_code:
          print_esc("mathchardef");
          break;

        case count_def_code:
          print_esc("countdef");
          break;

        case dimen_def_code:
          print_esc("dimendef");
          break;

        case skip_def_code:
          print_esc("skipdef");
          break;

        case mu_skip_def_code:
          print_esc("muskipdef");
          break;

        default:
          print_esc("toksdef");
          break;
      }
      break;

    case char_given:
      {
        print_esc("char");
        print_hex(chr_code);
      }
      break;

    case kchar_given:
      {
        print_esc("kchar");
        print_hex(chr_code);
      }
      break;

    case math_given:
      {
        print_esc("mathchar");
        print_hex(chr_code);
      }
      break;

    case def_code:
      if (chr_code == cat_code_base)
        print_esc("catcode");
      else if (chr_code == kcat_code_base)
        print_esc("kcatcode");
      else if (chr_code == auto_xsp_code_base)
        print_esc("xspcode");
      else if (chr_code == math_code_base)
        print_esc("mathcode");
      else if (chr_code == lc_code_base)
        print_esc("lccode");
      else if (chr_code == uc_code_base)
        print_esc("uccode");
      else if (chr_code == sf_code_base)
        print_esc("sfcode");
      else
        print_esc("delcode");
      break;

    case def_family:
      print_size(chr_code - math_font_base);
      break; 

    case hyph_data:
      if (chr_code == 1)
        print_esc("patterns");
      else
        print_esc("hyphenation");
      break;

    case assign_font_int:
      if (chr_code == 0)
        print_esc("hyphenchar");
      else
        print_esc("skewchar");
      break;

    case set_font:
      {
        prints("select font ");
        slow_print(font_name[chr_code]);

        if (font_size[chr_code] != font_dsize[chr_code])
        {
          prints(" at ");
          print_scaled(font_size[chr_code]);
          prints("pt");
        }
      }
      break;

    case set_interaction:
      switch (chr_code)
      {
        case batch_mode:
          print_esc("batchmode");
          break;

        case nonstop_mode:
          print_esc("nonstopmode");
          break;

        case scroll_mode:
          print_esc("scrollmode");
          break;

        default:
          print_esc("errorstopmode");
          break;
      }
      break;

    case in_stream:
      if (chr_code == 0)
        print_esc("closein");
      else
        print_esc("openin");
      break;

    case message:
      if (chr_code == 0)
        print_esc("message");
      else
        print_esc("errmessage");
      break;

    case case_shift:
      if (chr_code == lc_code_base)
        print_esc("lowercase");
      else
        print_esc("uppercase");
      break;

    case xray:
      switch (chr_code)
      {
        case show_box_code:
          print_esc("showbox");
          break;

        case show_the_code:
          print_esc("showthe");
          break;

        case show_lists_code:
          print_esc("showlists");
          break;

        case show_groups:
          print_esc("showgroups");
          break;

        case show_tokens:
          print_esc("showtokens");
          break;

        case show_ifs:
          print_esc("showifs");
          break;

        case show_mode:
          print_esc("showmode");
          break;

        default:
          print_esc("show");
          break;
      }
      break;

    case undefined_cs:
      prints("undefined");
      break;

    case call:
    case long_call:
    case outer_call:
    case long_outer_call:
      {
        n = cmd - call;

        if (info(link(chr_code)) == protected_token)
          n = n + 4;

        if (odd(n / 4))
          print_esc("protected");

        if (odd(n))
          print_esc("long");

        if (odd(n / 2))
          print_esc("outer");

        if (n > 0)
          print_char(' ');

        prints("macro");
      }
      break;

    case end_template:
      print_esc("outer endtemplate");
      break;

    case extension:
      switch (chr_code)
      {
        case open_node:
          print_esc("openout");
          break;

        case write_node:
          print_esc("write");
          break;

        case close_node:
          print_esc("closeout");
          break;

        case special_node:
          print_esc("special");
          break;

        case immediate_code:
          print_esc("immediate");
          break;

        case set_language_code:
          print_esc("setlanguage");
          break;

        case pdf_save_pos_node:
          print_esc("pdfsavepos");
          break;

        case reset_timer_code:
          print_esc("pdfresettimer");
          break;

        case set_random_seed_code:
          print_esc("pdfsetrandomseed");
          break;

        default:
          prints("[unknown extension!]");
          break;
      }
      break;

    case set_kansuji_char:
      print_esc("kansujichar");
      break;

    case set_auto_spacing:
      {
        if ((chr_code % 2) == 0)
          print_esc("noauto");
        else
          print_esc("auto");

        if (chr_code < 2)
          prints("spacing");
        else
          prints("xspacing");
      }
      break;

    case set_enable_cjk_token:
      {
        if (chr_code == 0)
          print_esc("enable");
        else if (chr_code == 1)
          print_esc("disable");
        else
          print_esc("force");

        prints("cjktoken");
      }
      break;

    case inhibit_glue:
      if (cur_chr > 0)
        print_esc("disinhibitglue");
      else
        print_esc("inhibitglue");
      break;

    case assign_inhibit_xsp_code:
      print_esc("inhibitxspcode");
      break;

    case assign_kinsoku:
      switch (chr_code)
      {
        case pre_break_penalty_code:
          print_esc("prebreakpenalty");
          break;

        case post_break_penalty_code:
          print_esc("postbreakpenalty");
          break;
      }
      break;

    default:
      prints("[unknown command code!]");
      break;
  }
}

#ifdef STAT
static void show_eqtb (pointer n)
{ 
  if (n < active_base)
    print_char('?');
  else if (n < glue_base)
  {
    sprint_cs(n);
    print_char('=');
    print_cmd_chr(eq_type(n), equiv(n));

    if (eq_type(n) >= call)
    {
      print_char(':');
      show_token_list(link(equiv(n)), null, 32);
    }
  }
  else if (n < local_base)
  {
    if (n < skip_base)
    {
      print_skip_param(n - glue_base);
      print_char('=');

      if (n < glue_base + thin_mu_skip_code)
        print_spec(equiv(n), "pt");
      else
        print_spec(equiv(n), "mu");
    }
    else if (n < mu_skip_base)
    {
      print_esc("skip");
      print_int(n - skip_base);
      print_char('=');
      print_spec(equiv(n), "pt");
    }
    else
    {
      print_esc("muskip");
      print_int(n - mu_skip_base);
      print_char('=');
      print_spec(equiv(n), "mu");
    }
  }
  else if (n < int_base)
    if ((n == par_shape_loc) || ((n >= etex_pen_base) && (n < etex_pens)))
    {
      print_cmd_chr(set_shape, n);
      print_char('=');

      if (equiv(n) == 0)
        print_char('0');
      else if (n > par_shape_loc)
      {
        print_int(penalty(equiv(n)));
        print_char(' ');
        print_int(penalty(equiv(n) + 1));
        
        if (penalty(equiv(n)) > 1)
          print_esc("ETC.");
      }
      else
        print_int(info(par_shape_ptr));
    }
    else if (n < toks_base)
    {
      print_cmd_chr(assign_toks, n);
      print_char('=');

      if (equiv(n) != 0)
        show_token_list(link(equiv(n)), 0, 32);
    }
    else if (n < box_base)
    {
      print_esc("toks");
      print_int(n - toks_base);
      print_char('=');

      if (equiv(n) != 0)
        show_token_list(link(equiv(n)), 0, 32);
    }
    else if (n < cur_font_loc)
    {
      print_esc("box");
      print_int(n - box_base);
      print_char('=');

      if (equiv(n) == 0)
        prints("void");
      else
      {
        depth_threshold = 0;
        breadth_max = 1;
        show_node_list(equiv(n));
      }
    }
    else if (n < cat_code_base)
    {
      if (n == cur_font_loc)
        prints("current font");
      else if (n < math_font_base + 16)
      {
        print_esc("textfont");
        print_int(n - math_font_base);
      }
      else if (n < math_font_base + 32)
      {
        print_esc("scriptfont");
        print_int(n - math_font_base - 16);
      }
      else
      {
        print_esc("scriptscriptfont");
        print_int(n - math_font_base - 32);
      }

      print_char('=');
      sprint_esc(hash[font_id_base + equiv(n)].rh);
    }
    else if (n < math_code_base)
    {
      if (n < kcat_code_base)
      {
        print_esc("catcode");
        print_int(n - cat_code_base);
      }
      else if (n < auto_xsp_code_base)
      {
        print_esc("kcatcode");
        print_int(n - kcat_code_base);
      }
      else if (n < inhibit_xsp_code_base)
      {
        print_esc("xspcode");
        print_int(n - auto_xsp_code_base);
      }
      else if (n < kinsoku_base)
      {
        print_esc("inhibitxspcode table ");
        print_int(n - inhibit_xsp_code_base);
        prints(", type=");
        switch (eq_type(n))
        {
          case 0:
            prints("both");   // { |inhibit_both| }
            break;
          case 1:
            prints("before"); // { |inhibit_previous| }
            break;
          case 2:
            prints("after"); // { |inhibit_after| }
            break;
          case 3:
            prints("none");  // { |inhibit_none| }
            break;
          case 4:
            prints("unused"); // { |inhibit_unused| }
            break;
        }
        prints(", code");
      }
      else if (n < kansuji_base)
      {
        print_esc("kinsoku table ");
        print_int(n - kinsoku_base);
        prints(", type=");
        switch (eq_type(n))
        {
          case 0:
            prints("no");
            break;
          case 1:
            prints("pre");    // { |pre_break_penalty_code| }
            break;
          case 2:
            prints("post");   // { |post_break_penalty_code| }
            break;
          case 3:
            prints("unused"); // { |kinsoku_unused_code| }
            break;
        }
        prints(", code");
      }
      else if (n < lc_code_base)
      {
        print_esc("kansujichar");
        print_int(n - kansuji_base);
      }
      else if (n < uc_code_base)
      {
        print_esc("lccode");
        print_int(n - lc_code_base);
      }
      else if (n < sf_code_base)
      {
        print_esc("uccode");
        print_int(n - uc_code_base);
      }
      else
      {
        print_esc("sfcode");
        print_int(n - sf_code_base);
      }

      print_char('=');
      print_int(equiv(n));
    }
    else
    {
      print_esc("mathcode");
      print_int(n - math_code_base);
      print_char('=');
      print_int(equiv(n));
    }
  else if (n < dimen_base)
  {
    if (n < count_base)
      print_param(n - int_base);
    else if (n < del_code_base)
    {
      print_esc("count");
      print_int(n - count_base);
    }
    else
    {
      print_esc("delcode");
      print_int(n - del_code_base);
    }
    
    print_char('=');
    print_int(eqtb[n].cint);
  }
  else if (n <= kinsoku_penalty_base)
  {
    if (n < scaled_base)
      print_length_param(n - dimen_base);
    else
    {
      print_esc("dimen");
      print_int(n - scaled_base);
    }

    print_char('=');
    print_scaled(eqtb[n].cint);
    prints("pt");
  }
  else if (n <= eqtb_size)
  {
    prints("kinsoku table ");
    print_int(n - kinsoku_penalty_base);
    prints(", penalty=");
    print_int(eqtb[n].cint);
  }
  else
    print_char('?');
}
#endif

// search the hash table
pointer id_lookup (integer j, integer l)
{
  integer h;  // {hash code}
  integer d;  // {number of characters in incomplete current string}
  pointer p;  // {index in |hash| array}
  pointer k;  // {index in |buffer| array}

  h = buffer[j];

  for (k = j + 1; k <= j + l - 1; k++)
  {
    h = h + h + buffer[k];

    while (h >= hash_prime)
      h = h - hash_prime;
  }

  p = h + hash_base;

  while (true)
  {
    if (text(p) > 0)
    {
      if (length(text(p)) == l)
      {
        if (str_eq_buf(text(p), j))
          goto found;
      }
    }

    if (next(p) == 0)
    {
      if (no_new_control_sequence)
        p = undefined_control_sequence;
      else
      {
        if (text(p) > 0)
        {
          do {
            if (hash_is_full)
              overflow("hash size", hash_size);

            decr(hash_used);
          } while (!(text(hash_used) == 0));

          next(p) = hash_used;
          p = hash_used;
        }

        str_room(l);
        d = cur_length;

        while (pool_ptr > str_start[str_ptr])
        {
          decr(pool_ptr);
          str_pool[pool_ptr + l] = str_pool[pool_ptr];
        }

        for (k = j; k <= j + l - 1; k++)
          append_char(buffer[k]);

        text(p) = make_string();
        pool_ptr = pool_ptr + d;

#ifdef STAT
        incr(cs_count);
#endif
      }

      goto found;
    }

    p = next(p);
  }

found:
  return p;
}

// begin a new level of grouping
static void new_save_level (group_code c)
{
  check_full_save_stack();

  if (eTeX_ex)
  {
    saved(0) = line;
    incr(save_ptr);
  }

  save_type(save_ptr) = level_boundary;
  save_level(save_ptr) = (quarterword) cur_group;
  save_index(save_ptr) = cur_boundary;

  if (cur_level == max_quarterword)
    overflow("grouping levels", max_quarterword - min_quarterword);

  cur_boundary = save_ptr;
  cur_group = c;

#ifdef STAT
  if (tracing_groups > 0)
    group_trace(false);
#endif

  incr(cur_level);
  incr(save_ptr);
}

// gets ready to forget |w|
static void eq_destroy (memory_word w)
{
  pointer q;

  switch (eq_type_field(w))
  {
    case call:
    case long_call:
    case outer_call:
    case long_outer_call:
      delete_token_ref(equiv_field(w));
      break;

    case glue_ref:
      delete_glue_ref(equiv_field(w));
      break;

    case shape_ref:
      {
        q = equiv_field(w);

        if (q != null)
          free_node(q, info(q) + info(q) + 1);
      }
      break;

    case box_ref:
      flush_node_list(equiv_field(w));
      break;

    case toks_register:
    case tex_register:
      if ((equiv_field(w) < mem_bot) || (equiv_field(w) > lo_mem_stat_max))
        delete_sa_ref(equiv_field(w));

    default:
      do_nothing();
      break;
  }
}

// saves |eqtb[p]|
static void eq_save (pointer p, quarterword l)
{
  check_full_save_stack();

  if (l == level_zero)
    save_type(save_ptr) = restore_zero;
  else
  {
    save_stack[save_ptr] = eqtb[p];
    incr(save_ptr);
    save_type(save_ptr) = restore_old_value;
  }

  save_level(save_ptr) = l;
  save_index(save_ptr) = p;
  incr(save_ptr);
}

// new data for |eqtb|
static void eq_define (pointer p, quarterword t, halfword e)
{
  if (eTeX_ex && (eq_type(p) == t) && (equiv(p) == e))
  {
    assign_trace(p, "reassigning");
    eq_destroy(eqtb[p]);
    return;
  }

  assign_trace(p, "changing");

  if (eq_level(p) == cur_level)
    eq_destroy(eqtb[p]);
  else if (cur_level > level_one)
    eq_save(p, eq_level(p));

  eq_level(p) = cur_level;
  eq_type(p) = t;
  equiv(p) = e;
  assign_trace(p, "into");
}

static void eq_word_define (pointer p, integer w)
{
  if (eTeX_ex && (eqtb[p].cint == w))
  {
    assign_trace(p, "reassigning");
    return;
  }

  assign_trace(p, "changing");

  if (xeq_level[p] != cur_level)
  {
    eq_save(p, xeq_level[p]);
    xeq_level[p] = cur_level;
  }

  eqtb[p].cint = w;
  assign_trace(p, "into");
}

// global |eq_define|
static void geq_define (pointer p, quarterword t, halfword e)
{
  assign_trace(p, "globally changing");

  {
    eq_destroy(eqtb[p]);
    eq_level(p) = level_one;
    eq_type(p) = t;
    equiv(p) = e;
  }

  assign_trace(p, "into");
}

// global |eq_word_define|
static void geq_word_define (pointer p, integer w)
{
  assign_trace(p, "globally changing");

  {
    eqtb[p].cint = w;
    xeq_level[p]= level_one;
  }

  assign_trace(p, "into");
}

static void save_for_after (halfword t)
{ 
  if (cur_level > level_one)
  {
    check_full_save_stack();
    save_type(save_ptr) = insert_token;
    save_level(save_ptr) = level_zero;
    save_index(save_ptr) = t;
    incr(save_ptr);
  }
}

#ifdef STAT
// |eqtb[p]| has just been restored or retained
static void restore_trace (pointer p, const char * s)
{
  begin_diagnostic();
  print_char('{');
  prints(s);
  print_char(' ');
  show_eqtb(p);
  print_char('}');
  end_diagnostic(false);
}

// macros -> function
void assign_trace (pointer p, const char * s)
{
  if (tracing_assigns > 0)
    restore_trace(p, s);
}
#endif

// pops the top level off the save stack
static void unsave (void)
{
  pointer p;      // {position to be restored}
  quarterword l;  // {saved level, if in fullword regions of |eqtb|}
  halfword t;     // {saved value of |cur_tok|}
  boolean a;      // {have we already processed an \.{\\aftergroup} ?}

  a = false;

  if (cur_level > level_one)
  {
    decr(cur_level);

    while (true)
    {
      decr(save_ptr);

      if (save_type(save_ptr) == level_boundary)
        goto done;

      p = save_index(save_ptr);

      if (save_type(save_ptr) == insert_token)
      {
        t = cur_tok;
        cur_tok = p;

        if (a)
        {
          p = get_avail();
          info(p) = cur_tok;
          link(p) = loc;
          loc = p;
          start = p;

          if (cur_tok < right_brace_limit)
          {
            if (cur_tok < left_brace_limit)
              decr(align_state);
            else
              incr(align_state);
          }
        }
        else
        {
          back_input();
          a = eTeX_ex;
        };

        cur_tok = t;
      }
      else if (save_type(save_ptr) == restore_sa)
      {
        sa_restore();
        sa_chain = p;
        sa_level = save_level(save_ptr);
      }
      else
      {
        if (save_type(save_ptr) == restore_old_value)
        {
          l = save_level(save_ptr);
          decr(save_ptr);
        }
        else
          save_stack[save_ptr] = eqtb[undefined_control_sequence];

        if (p < int_base)
        {
          if (eq_level(p) == level_one)
          {
            eq_destroy(save_stack[save_ptr]);

#ifdef STAT
            if (tracing_restores > 0)
              restore_trace(p, "retaining");
#endif
          }
          else
          {
            eq_destroy(eqtb[p]);
            eqtb[p] = save_stack[save_ptr];

#ifdef STAT
            if (tracing_restores > 0)
              restore_trace(p, "restoring");
#endif
          }
        }
        else if (xeq_level[p] != level_one)
        {
          eqtb[p] = save_stack[save_ptr];
          xeq_level[p] = l;

#ifdef STAT
          if (tracing_restores > 0)
            restore_trace(p, "restoring");
#endif
        }
        else
        {
#ifdef STAT
          if (tracing_restores > 0)
            restore_trace(p, "retaining");
#endif
        }
      }
    }

done:
#ifdef STAT
    if (tracing_groups > 0)
      group_trace(true);
#endif

    if (grp_stack[in_open] == cur_boundary)
      group_warning();

    cur_group = save_level(save_ptr);
    cur_boundary = save_index(save_ptr);

    if (eTeX_ex)
      decr(save_ptr);
  }
  else
    confusion("curlevel");
}

static void prepare_mag (void) 
{
  if ((mag_set > 0) && (mag != mag_set))
  {
    print_err("Incompatible magnification (");
    print_int(mag);
    prints(");");
    print_nl(" the previous value will be retained");
    help2("I can handle only one magnification ratio per job. So I've",
        "reverted to the magnification you used earlier on this run.");
    int_error(mag_set);
    geq_word_define(int_base + mag_code, mag_set);  // {|mag:=mag_set|}
  }

  if ((mag <= 0) || (mag > 32768))
  {
    print_err("Illegal magnification has been changed to 1000");
    help1("The magnification ratio must be between 1 and 32768.");
    int_error(mag);
    geq_word_define(int_base + mag_code, 1000);
  }

  mag_set = mag;
}

static void token_show (pointer p)
{
  if (p != null)
    show_token_list(link(p), null, 10000000);
}

static void print_meaning (void)
{
  print_cmd_chr(cur_cmd, cur_chr);

  if (cur_cmd >= call)
  {
    print_char(':');
    print_ln();
    token_show(cur_chr);
  }
  else if ((cur_cmd == top_bot_mark) && (cur_chr < marks_code))
  {
    print_char(':');
    print_ln();
    token_show(cur_mark[cur_chr]);
  }
}

static void show_cur_cmd_chr (void)
{
  integer n;  // {level of \.{\\if...\\fi} nesting}
  integer l;  // {line where \.{\\if} started}
  pointer p;

  begin_diagnostic();
  print_nl("{");

  if (mode != shown_mode)
  {
    print_mode(mode);
    prints(": ");
    shown_mode = mode;
  }

  print_cmd_chr(cur_cmd, cur_chr);

  if (tracing_ifs > 0)
  {
    if (cur_cmd >= if_test)
    {
      if (cur_cmd <= fi_or_else)
      {
        prints(": ");

        if (cur_cmd == fi_or_else)
        {
          print_cmd_chr(if_test, cur_if);
          print_char(' ');
          n = 0;
          l = if_line;
        }
        else
        {
          n = 1;
          l = line;
        }

        p = cond_ptr;

        while (p != null)
        {
          incr(n);
          p = link(p);
        }

        prints("(level ");
        print_int(n);
        print_char(')');
        print_if_line(l);
      }
    }
  }

  print_char('}');
  end_diagnostic(false);
}

// prints where the scanner is
void show_context (void)
{
  char old_setting; // {saved |selector| setting}
  pointer s;  // {temporary pointer}
  integer nn; // {number of contexts shown so far, less one}
  boolean bottom_line;  // {have we reached the final context to be shown?}
  uint32_t i;  // {index into |buffer|}
  uint32_t j;  // {end of current line in |buffer|}
  uint32_t l;  // {length of descriptive information on line 1}
  integer m;  // {context information gathered for line 2}
  uint32_t n;  // {length of line 1}
  integer p;  // {starting or ending place in |trick_buf|}
  integer q;  // {temporary index}

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  // {store current state}
  nn = -1;
  bottom_line = false;

  while (true)
  {
    cur_input = input_stack[base_ptr];  // {enter into the context}

    if (state != token_list)
    {
      if ((name > 19) || (base_ptr == 0))
        bottom_line = true;
    }

    if ((base_ptr == input_ptr) || bottom_line || (nn < error_context_lines))
    {
      // @<Display the current context@>
      if ((base_ptr == input_ptr) || (state != token_list) ||
          (token_type != backed_up) || (loc != 0))
      {
        // {we omit backed-up token lists that have already been read}
        tally = 0;  // {get ready to count characters}
        old_setting = selector;

        if (state != token_list)
        {
          // @<Print location of current line@>
          if (name <= 17)
          {
            if (terminal_input)
            {
              if (base_ptr == 0)
                print_nl("<*>");
              else
                print_nl("<insert> ");
            }
            else
            {
              print_nl("<read ");

              if (name == 17)
                print_char('*');
              else
                print_int(name - 1);

              print_char('>');
            }
          }
          else
          {
            print_nl("l.");

            if (index == in_open)
              print_int(line);
            else
              print_int(line_stack[index + 1]); // {input from a pseudo file}
          }

          print_char(' ');
          // @<Pseudoprint the line@>
          begin_pseudoprint();

          if (buffer[limit] == end_line_char)
            j = limit;
          else
            j = limit + 1;  // {determine the effective end of the line}

          if (j > 0)
          {
            for (i = start; i <= j - 1; i++)
            {
              if (i == loc)
                set_trick_count();

              print(buffer[i]);
            }
          }
        }
        else
        {
          // @<Print type of token list@>
          switch (token_type)
          {
            case parameter:
              print_nl("<argument> ");
              break;

            case u_template:
            case v_template:
              print_nl("<template> ");
              break;

            case backed_up:
              if (loc == null)
                print_nl("<recently read> ");
              else
                print_nl("<to be read again> ");
              break;

            case inserted:
              print_nl("<inserted text> ");
              break;

            case macro:
              {
                print_ln();
                print_cs(name);
              }
              break;

            case output_text:
              print_nl("<output> ");
              break;

            case every_par_text:
              print_nl("<everypar> ");
              break;

            case every_math_text:
              print_nl("<everymath> ");
              break;

            case every_display_text:
              print_nl("<everydisplay> ");
              break;

            case every_hbox_text:
              print_nl("<everyhbox> ");
              break;

            case every_vbox_text:
              print_nl("<everyvbox> ");
              break;

            case every_job_text:
              print_nl("<everyjob> ");
              break;

            case every_cr_text:
              print_nl("<everycr> ");
              break;

            case mark_text:
              print_nl("<mark> ");
              break;

            case every_eof_text:
              print_nl("<everyeof> ");
              break;

            case write_text:
              print_nl("<write> ");
              break;

            default:
              print_nl("?");  // {this should never happen}
              break;
          }

          // @<Pseudoprint the token list@>
          begin_pseudoprint();

          if (token_type < macro)
          {
            if ((token_type == backed_up) && (loc != null))
            {
              if ((link(start) == null) && (check_kanji(info(start))))
              {
                cur_input = input_stack[base_ptr - 1];
                s = get_avail();
                info(s) = (info(loc) % max_char_val);
                cur_input = input_stack[base_ptr];
                link(start) = s;
                show_token_list(start, loc, 100000);
                free_avail(s);
                link(start) = null;
                goto done1;
              }
            }

            show_token_list(start, loc, 100000);
          }
          else
            show_token_list(link(start), loc, 100000);  // {avoid reference count}

done1:;
        }

        selector = old_setting; // {stop pseudoprinting}

        // @<Print two lines using the tricky pseudoprinted information@>
        if (trick_count == 1000000)
          set_trick_count();
        // {|set_trick_count| must be performed}

        if (tally < trick_count)
          m = tally - first_count;
        else
          m = trick_count - first_count;  // {context on line 2}

        if (l + first_count <= half_error_line)
        {
          p = 0;
          n = l + first_count;
        }
        else
        {
          prints("...");
          p = l + first_count - half_error_line + 3;
          n = half_error_line;
        }

        kcp = trick_buf2[p % error_line];

        if ((kcp % 010) > 1)
        {
          p = p + nrestmultichr(kcp) + 1;
          n = n - nrestmultichr(kcp) - 1;
        }

        for (q = p; q <= first_count - 1; q++)
          print_char(trick_buf[q % error_line]);

        print_ln();

        for (q = 1; q <= n; q++)
          print_char(' ');  // {print |n| spaces to begin line~2}

        if (m + n <= error_line)
          p = first_count + m;
        else
          p = first_count +(error_line - n - 3);

        kcp = trick_buf2[(p - 1) % error_line];

        if (((kcp % 010) > 0) && (nrestmultichr(kcp) > 0))
          p = p - (kcp % 010);

        for (q = first_count; q <= p - 1; q++)
          print_char(trick_buf[q % error_line]);

        if (m + n > error_line)
          prints("...");

        incr(nn);
      }
    }
    else if (nn == error_context_lines)
    {
      print_nl("...");
      incr(nn); // {omitted if |error_context_lines<0|}
    }

    if (bottom_line)
      goto done;

    decr(base_ptr);
  }

done:
  cur_input = input_stack[input_ptr]; // {restore original state}
}

void begin_token_list (pointer p, quarterword t)
{
  push_input();
  state = token_list;
  start = p;
  token_type = t;

  if (t >= macro)
  {
    add_token_ref(p);

    if (t == macro)
      param_start = param_ptr;
    else
    {
      loc = link(p);

      if (tracing_macros > 1)
      {
        begin_diagnostic();
        print_nl("");

        switch (t)
        {
          case mark_text:
            print_esc("mark");
            break;

          case write_text:
            print_esc("write");
            break;

          default:
            print_cmd_chr(assign_toks, t - output_text + output_routine_loc);
            break;
        }

        prints("->");
        token_show(p);
        end_diagnostic(false);
      }
    }
  }
  else
    loc = p;
}

// leave a token-list input level
void end_token_list (void)
{
  if (token_type >= backed_up)
  {
    if (token_type <= inserted)
      flush_list(start);
    else
    {
      delete_token_ref(start);

      if (token_type == macro)
        while (param_ptr > param_start)
        {
          decr(param_ptr);
          flush_list(param_stack[param_ptr]);
        }
    }
  }
  else if (token_type == u_template)
  {
    if (align_state > 500000)
      align_state = 0;
    else
      fatal_error("(interwoven alignment preambles are not allowed)");
  }

  pop_input();
  check_interrupt();
}

// undoes one token of input
void back_input (void)
{
  pointer p;  // {a token list of length one}

  while ((loc == null) && (token_type != v_template))
    end_token_list(); // {conserve stack space}

  p = get_avail();
  info(p) = cur_tok;

  if (cur_tok < right_brace_limit)
  {
    if (cur_tok < left_brace_limit)
      decr(align_state);
    else
      incr(align_state);
  }

  push_input();
  state = token_list;
  start = p;
  token_type = backed_up;
  loc = p;  // {that was |back_list(p)|, without procedure overhead}
}

// back up one token and call |error|
void back_error (void)
{
  OK_to_interrupt = false;
  back_input();
  OK_to_interrupt = true;
  error();
}

// back up one inserted token and call |error|
static void ins_error (void) 
{
  OK_to_interrupt = false;
  back_input();
  token_type = inserted;
  OK_to_interrupt = true;
  error();
}

void begin_file_reading (void)
{
  if (in_open == max_in_open)
    overflow("text input levels", max_in_open);

#ifdef APTEX_EXTENSION
  if (first == current_buf_size)
    buffer = realloc_buffer(increment_buf_size);

  if (first == current_buf_size)
    overflow("buffer size", current_buf_size);
#else
  if (first == buf_size)
    overflow("buffer size", buf_size);
#endif

  incr(in_open);

  if (in_open > high_in_open)
    high_in_open = in_open;

  push_input();
  index = in_open;
  eof_seen[index] = false;
  grp_stack[index] = cur_boundary;
  if_stack[index] = cond_ptr;
  line_stack[index] = line;
  start = first;
  state = mid_line;
  name = 0; // {|terminal_input| is now |true|}
  synctex_tag = 0;
}

void end_file_reading (void)
{
  first = start;
  line = line_stack[index];

  if ((name == 18) || (name == 19))
    pseudo_close();
  else if (name > 17)
    a_close(cur_file);  // {forget it}

  pop_input();
  decr(in_open);
}

void clear_for_error_prompt (void)
{
  while ((state != token_list) && terminal_input &&
      (input_ptr > 0) && (loc > limit))
    end_file_reading();

  print_ln();
  clear_terminal();
}

static void check_outer_validity (void)
{
  pointer p;  // {points to inserted token list}
  pointer q;  // {auxiliary pointer}

  if (scanner_status != normal)
  {
    deletions_allowed = false;

    // @<Back up an outer control sequence so that it can be reread@>
    if (cur_cs != 0)
    {
      if ((state == token_list) || (name < 1) || (name > 17))
      {
        p = get_avail();
        info(p) = cs_token_flag + cur_cs;
        back_list(p);
      }

      cur_cmd = spacer;
      cur_chr = ' ';  // {replace it by a space}
    }

    if (scanner_status > skipping)
    {
      // @<Tell the user what has run away and try to recover@>
      runaway();  // {print a definition, argument, or preamble}

      if (cur_cs == 0)
        print_err("File ended");
      else
      {
        cur_cs = 0;
        print_err("Forbidden control sequence found");
      }

      /*
        @<Print either `\.{definition}' or `\.{use}' or `\.{preamble}' or `\.{text}',
        and insert tokens that should lead to recovery@>
      */
      prints(" while scanning ");
      p = get_avail();

      switch (scanner_status)
      {
        case defining:
          {
            prints("definition");
            info(p) = right_brace_token + '}';
          }
          break;

        case matching:
          {
            prints("use");
            info(p) = par_token;
            long_state = outer_call;
          }
          break;

        case aligning:
          {
            prints("preamble");
            info(p) = right_brace_token + '}';
            q = p;
            p = get_avail();
            link(p) = q;
            info(p) = cs_token_flag + frozen_cr;
            align_state = -1000000;
          }
          break;

        case absorbing:
          {
            prints("text");
            info(p) = right_brace_token + '}';
          }
          break;
      }

      ins_list(p);
      prints(" of ");
      sprint_cs(warning_index);
      help4("I suspect you have forgotten a `}', causing me",
          "to read past where you wanted me to stop.",
          "I'll try to recover; but if the error is serious,",
          "you'd better type `E' or `X' now and fix your file.");
      error();
    }
    else
    {
      print_err("Incomplete ");
      print_cmd_chr(if_test, cur_if);
      prints("; all text was ignored after line ");
      print_int(skip_line);
      help3("A forbidden control sequence occurred in skipped text.",
          "This kind of error happens when you say `\\if...' and forget",
          "the matching `\\fi'. I've inserted a `\\fi'; this might work.");

      if (cur_cs != 0)
        cur_cs = 0;
      else
        help_line[2] = "The file ended while I was skipping conditional text.";

      cur_tok = cs_token_flag + frozen_fi;
      ins_error();
    }

    deletions_allowed = true;
  }
}

static void firm_up_the_line (void)
{
  uint32_t k;  // {an index into |buffer|}

  limit = last;

  if (pausing > 0)
  {
    if (interaction > nonstop_mode)
    {
      wake_up_terminal();
      print_ln();

      if (start < limit)
        for (k = start; k <= limit - 1; k++)
          print(buffer[k]);

      first = limit;
      prompt_input("=>"); // {wait for user response}

      if (last > first)
      {
        for (k = first; k <= last - 1; k++) // {move line down in buffer}
          buffer[k + start - first] = buffer[k];

        limit = start + last - first;
      }
    }
  }
}

// sets |cur_cmd|, |cur_chr|, |cur_tok|
void get_token (void)
{
  no_new_control_sequence = false;
  get_next();
  no_new_control_sequence = true;

  if (cur_cs == 0)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  }
  else
    cur_tok = cs_token_flag + cur_cs;
}

// invokes a user-defined control sequence
static void macro_call (void)
{
  pointer r;  // {current node in the macro's token list}
  pointer p;  // {current node in parameter token list being built}
  pointer q;  // {new node being put into the token list}
  pointer s;  // {backup pointer for parameter matching}
  pointer t;  // {cycle pointer for backup recovery}
  pointer u, v; // {auxiliary pointers for backup recovery}
  pointer rbrace_ptr; // {one step before the last |right_brace| token}
  small_number n; // {the number of parameters scanned}
  halfword unbalance; // {unmatched left braces in current parameter}
  halfword m; // {the number of tokens or groups (usually)}
  pointer ref_count;  // {start of the token list}
  small_number save_scanner_status; // {|scanner_status| upon entry}
  pointer save_warning_index; // {|warning_index| upon entry}
  ASCII_code match_chr; // {character used in parameter}

  save_scanner_status = scanner_status;
  save_warning_index = warning_index;
  warning_index = cur_cs;
  ref_count = cur_chr;
  r = link(ref_count);
  n = 0;

  if (tracing_macros > 0)
  {
    // @<Show the text of the macro being expanded@>
    begin_diagnostic();

    if (tracing_stack_levels > 0)
      if (input_ptr < tracing_stack_levels)
      {
        v = input_ptr;
        print_ln();
        print_char('~');

        while (v > 0)
        {
          print_char('.');
          decr(v);
        };

        print_cs(warning_index);
        token_show(ref_count);
      }
      else
      {
        print_char('~');
        print_char('~');
        print_cs(warning_index);
      }
    else
    {
      print_ln();
      print_cs(warning_index);
      token_show(ref_count);
    }

    end_diagnostic(false);
  }

  if (info(r) == protected_token)
    r = link(r);

  if (info(r) != end_match_token)
  {
    scanner_status = matching;
    unbalance = 0;
    long_state = eq_type(cur_cs);

    if (long_state >= outer_call)
      long_state = long_state - 2;

    do {
      link(temp_head) = null;

      if ((info(r) > match_token + 255) || (info(r) < match_token))
        s = null;
      else
      {
        match_chr = info(r) - match_token;
        s = link(r);
        r = s;
        p = temp_head;
        m = 0;
      }

continu:
      get_token();

      if (cur_tok == info(r))
      {
        r = link(r);

        if ((info(r) >= match_token) && (info(r) <= end_match_token))
        {
          if (cur_tok < left_brace_limit)
            decr(align_state);

          goto found;
        }
        else
          goto continu;
      }

      if (s != r)
      {
        if (s == null)
        {
          print_err("Use of ");
          sprint_cs(warning_index);
          prints(" doesn't match its definition");
          help4("If you say, e.g., `\\def\\a1{...}', then you must always",
            "put `1' after `\\a', since control sequence names are",
            "made up of letters only. The macro here has not been",
            "followed by the required stuff, so I'm ignoring it.");
          error();
          goto exit;
        }
        else
        {
          t = s;

          do {
            store_new_token(info(t));
            incr(m);
            u = link(t);
            v = s;

            while (true)
            {
              if (u == r)
              {
                if (cur_tok != info(v))
                  goto done;
                else
                {
                  r = link(v);
                  goto continu;
                }
              }

              if (info(u) != info(v))
                goto done;

              u = link(u);
              v = link(v);
            }
done:
            t = link(t);
          } while (!(t == r));

          r = s;
        }
      }

      if (cur_tok == par_token)
      {
        if (long_state != long_call)
        {
          if (long_state == call)
          {
            runaway();
            print_err("Paragraph ended before ");
            sprint_cs(warning_index);
            prints("was complete");
            help3("I suspect you've forgotten a `}', causing me to apply this",
                "control sequence to too much text. How can we recover?",
                "My plan is to forget the whole thing and hope for the best.");
            back_error();
          }

          pstack[n] = link(temp_head);
          align_state = align_state - unbalance;

          for (m = 0; m <= n; m++)
            flush_list(pstack[m]);

          goto exit;
        }
      }

      if (cur_tok < right_brace_limit)
        if (cur_tok < left_brace_limit)
        {
          unbalance = 1;

          while (true)
          {
            fast_store_new_token(cur_tok);
            get_token();

            if (cur_tok == par_token)
              if (long_state != long_call)
              {
                if (long_state == call)
                {
                  runaway();
                  print_err("Paragraph ended before ");
                  sprint_cs(warning_index);
                  prints(" was complete");
                  help3("I suspect you've forgotten a `}', causing me to apply this",
                      "control sequence to too much text. How can we recover?",
                      "My plan is to forget the whole thing and hope for the best.");
                  back_error();
                }

                pstack[n] = link(temp_head);
                align_state = align_state - unbalance;

                for (m = 0; m <= n; m++)
                  flush_list(pstack[m]);

                goto exit;
              }

            if (cur_tok < right_brace_limit)
            {
              if (cur_tok < left_brace_limit)
                incr(unbalance);
              else
              {
                decr(unbalance);

                if (unbalance == 0)
                  goto done1;
              }
            }
          }
done1:
          rbrace_ptr = p;
          store_new_token(cur_tok);
        }
        else
        {
          back_input();
          print_err("Argument of ");
          sprint_cs(warning_index);
          prints(" has an extra }");
          help6("I've run across a `}' that doesn't seem to match anything.",
              "For example, `\\def\\a#1{...}' and `\\a}' would produce",
              "this error. If you simply proceed now, the `\\par' that",
              "I've just inserted will cause me to report a runaway",
              "argument that might be the root of the problem. But if",
              "your `}' was spurious, just type `2' and it will go away.");
          incr(align_state);
          long_state = call;
          cur_tok = par_token;
          ins_error();
          goto continu;
        }
      else
      {
        if (cur_tok == space_token)
        {
          if (info(r) <= end_match_token)
          {
            if (info(r) >= match_token)
              goto continu;
          }
        }

        store_new_token(cur_tok);
      }

      incr(m);

      if (info(r) > end_match_token)
        goto continu;

      if (info(r) < match_token)
        goto continu;

found:
      if (s != null)
      {
        if ((m == 1) && (info(p) < right_brace_limit) && (p != temp_head))
        {
          link(rbrace_ptr) = null;
          free_avail(p);
          p = link(temp_head);
          pstack[n] = link(p);
          free_avail(p);
        }
        else
          pstack[n] = link(temp_head);

        incr(n);

        if (tracing_macros > 0)
          if ((tracing_stack_levels == 0) || (input_ptr < tracing_stack_levels))
          {
            begin_diagnostic();
            print_nl("");
            print(match_chr);
            print_int(n);
            prints("<-");
            show_token_list(pstack[n - 1], null, 1000);
            end_diagnostic(false);
          }
      }
    } while (!(info(r) == end_match_token));
  }

  while ((loc == null) && (token_type != v_template))
    end_token_list();

  begin_token_list(ref_count, macro);
  name = warning_index;
  loc = link(r);

  if (n > 0)
  {
    if (param_ptr + n > max_param_stack)
    {
      max_param_stack = param_ptr + n;

#ifdef APTEX_EXTENSION
      if (max_param_stack > current_param_size)
        param_stack = realloc_param_stack(increment_param_size);

      if (max_param_stack > current_param_size)
        overflow("parameter stack size", current_param_size);
#else
      if (max_param_stack > param_size)
        overflow("parameter stack size", param_size);
#endif
    }

    for (m = 0; m <= n - 1; m++)
      param_stack[param_ptr + m] = pstack[m];

    param_ptr = param_ptr + n;
  }

exit:
  scanner_status = save_scanner_status;
  warning_index = save_warning_index;
}

static void insert_relax (void)
{
  cur_tok = cs_token_flag + cur_cs;
  back_input();
  cur_tok = cs_token_flag + frozen_relax;
  back_input();
  token_type = inserted;
}

static void expand (void)
{
  halfword t; // {token that is being ``expanded after''}
  boolean b; // {keep track of nested csnames}
  pointer p, q, r;  // {for list manipulation}
  integer j;  // {index into |buffer|}
  integer cv_backup;  // {to save the global quantity |cur_val|}
  small_number cvl_backup, radix_backup, co_backup; // {to save |cur_val_level|, etc.}
  pointer backup_backup;  // {to save |link(backup_head)|}
  small_number save_scanner_status; // {temporary storage of |scanner_status|}

  cv_backup = cur_val;
  cvl_backup = cur_val_level;
  radix_backup = radix;
  co_backup = cur_order;
  backup_backup = link(backup_head);

reswitch:
  if (cur_cmd < call)
  {
    // @<Expand a nonmacro@>

    if (tracing_commands > 1)
      show_cur_cmd_chr();

    switch (cur_cmd)
    {
      case top_bot_mark:
        // @<Insert the \(a)appropriate mark text into the scanner@>
        {
          t = cur_chr % marks_code;

          if (cur_chr >= marks_code)
            scan_register_num();
          else
            cur_val = 0;

          if (cur_val == 0)
            cur_ptr = cur_mark[t];
          else
          {
            find_sa_element(mark_val, cur_val, false);

            if (cur_ptr != 0)
            {
              if (odd(t))
                cur_ptr = link(cur_ptr + (t / 2) + 1);
              else
                cur_ptr = info(cur_ptr + (t / 2) + 1);
            }
          }

          if (cur_ptr != 0)
            begin_token_list(cur_ptr, mark_text);
        }
        break;

      case expand_after:
        if (cur_chr == 0)
        {
          get_token();
          t = cur_tok;
          get_token();

          if (cur_cmd > max_command)
            expand();
          else
            back_input();

          cur_tok = t;
          back_input();
        }
        else
        {
          get_token();

          if ((cur_cmd == if_test) && (cur_chr != if_case_code))
          {
            cur_chr = cur_chr + unless_code;
            goto reswitch;
          }

          print_err("You can't use `");
          print_esc("unless");
          prints("' before `");
          print_cmd_chr(cur_cmd, cur_chr);
          print_char('\'');
          help1("Continue, and I'll forget that it ever happened.");
          back_error();
        }
        break;

      case no_expand:
        if (cur_chr == 0)
        {
          save_scanner_status = scanner_status;
          scanner_status = normal;
          get_token();
          scanner_status = save_scanner_status;
          t = cur_tok;
          back_input();

          if ((t >= cs_token_flag) && (t != end_write_token))
          {
            p = get_avail();
            info(p) = cs_token_flag + frozen_dont_expand;
            link(p) = loc;
            start = p;
            loc = p;
          }
        }
        else
        {
          save_scanner_status = scanner_status;
          scanner_status = normal;
          get_token();
          scanner_status = save_scanner_status;

          if (cur_cs < hash_base)
            cur_cs = prim_lookup(cur_cs - single_base);
          else
            cur_cs = prim_lookup(text(cur_cs));

          if (cur_cs != undefined_primitive)
          {
            t = prim_eq_type(cur_cs);

            if (t > max_command)
            {
              cur_cmd = t;
              cur_chr = prim_equiv(cur_cs);
              cur_tok = (cur_cmd * 0400) + cur_chr;
              cur_cs  = 0;
              goto reswitch;
            }
            else
            {
              back_input(); // { now |loc| and |start| point to a one-item list }
              p = get_avail();
              info(p) = cs_token_flag + frozen_primitive;
              link(p) = loc;
              loc = p;
              start = p;
            }
          }
        }
        break;

      case cs_name:
        {
          r = get_avail();
          p = r;
          b = is_in_csname;
          is_in_csname = true;

          do {
            get_x_token();
  
            if (cur_cs == 0)
              store_new_token(cur_tok);
          } while (!(cur_cs != 0));
          
          if (cur_cmd != end_cs_name)
          {
            print_err("Missing ");
            print_esc("endcsname");
            prints(" inserted");
            help2("The control sequence marked <to be read again> should",
                "not appear between \\csname and \\endcsname.");
            back_error();
          }

          is_in_csname = b;
          j = first;
          p = link(r);

          while (p != null)
          {
            if (j >= max_buf_stack)
            {
              max_buf_stack = j + 1;

#ifdef APTEX_EXTENSION
              if (max_buf_stack == current_buf_size)
                buffer = realloc_buffer (increment_buf_size);

              if (max_buf_stack == current_buf_size)
                overflow("buffer size", current_buf_size);
#else
              if (max_buf_stack == buf_size)
                overflow("buffer size", buf_size);
#endif
            }

            if (check_kanji(info(p)))
            {
              t = toBUFF(info(p) % max_cjk_val);

              if (BYTE1(t) != 0)
              {
                buffer[j] = BYTE1(t);
                incr(j);
              }
              
              if (BYTE2(t) != 0)
              {
                buffer[j] = BYTE2(t);
                incr(j);
              }

              if (BYTE3(t) != 0)
              {
                buffer[j] = BYTE3(t);
                incr(j);
              }

              buffer[j] = BYTE4(t);
              incr(j);
              p = link(p);
            }
            else
            {
              buffer[j] = info(p) % max_char_val;
              incr(j);
              p = link(p);
            }
          }

          if (j > first + 1)
          {
            no_new_control_sequence = false;
            cur_cs = id_lookup(first, j - first);
            no_new_control_sequence = true;
          }
          else if (j == first)
            cur_cs = null_cs;
          else
            cur_cs = single_base + buffer[first];

          flush_list(r);

          if (eq_type(cur_cs) == undefined_cs)
          {
            eq_define(cur_cs, relax, 256);
          }

          cur_tok = cur_cs + cs_token_flag;
          back_input();
        }
        break;

      case convert:
        conv_toks();
        break;

      case the:
        ins_the_toks();
        break;

      case if_test:
        conditional();
        break;

      case fi_or_else:
        {
          if (tracing_ifs > 0)
          {
            if (tracing_commands <= 1)
              show_cur_cmd_chr();
          }

          if (cur_chr > if_limit)
          {
            if (if_limit == if_code)
              insert_relax();
            else
            {
              print_err("Extra ");
              print_cmd_chr(fi_or_else, cur_chr);
              help1("I'm ignoring this; it doesn't match any \\if.");
              error();
            }
          }
          else
          {
            while (cur_chr != fi_code)
              pass_text();

            {
              if (if_stack[in_open] == cond_ptr)
                if_warning();

              p = cond_ptr;
              if_line = if_line_field(p);
              cur_if = subtype(p);
              if_limit = type(p);
              cond_ptr = link(p);
              free_node(p, if_node_size);
            }
          }
        }
        break;

      case input:
        if (cur_chr == 1)
          force_eof = true;
        else if (cur_chr == 2)
          pseudo_start();
        else if (name_in_progress)
          insert_relax();
        else
          start_input();
        break;

      default:
        {
          print_err("Undefined control sequence");
          help5("The control sequence at the end of the top line",
              "of your error message was never \\def'ed. If you have",
              "misspelled it (e.g., `\\hobx'), type `I' and the correct",
              "spelling (e.g., `I\\hbox'). Otherwise just continue,",
              "and I'll forget about whatever was undefined.");
          error();
        }
        break;
    }
  }
  else if (cur_cmd < end_template)
    macro_call();
  else
  {
    cur_tok = cs_token_flag + frozen_endv;
    back_input();
  }

  cur_val = cv_backup;
  cur_val_level = cvl_backup;
  radix = radix_backup;
  cur_order = co_backup;
  link(backup_head) = backup_backup;
}

// sets |cur_cmd|, |cur_chr|, |cur_tok|, and expands macros
void get_x_token (void)
{
restart:
  get_next();

  if (cur_cmd <= max_command)
    goto done;

  if (cur_cmd >= call)
  {
    if (cur_cmd < end_template)
      macro_call();
    else
    {
      cur_cs = frozen_endv;
      cur_cmd = endv;
      goto done;
    }
  }
  else
    expand();

  goto restart;

done:
  if (cur_cs == 0)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  }
  else
    cur_tok = cs_token_flag + cur_cs;
}

// |get_x_token| without the initial |get_next|
void x_token (void)
{
  while (cur_cmd > max_command)
  {
    expand();
    get_next();
  }

  if (cur_cs == 0)
  {
    if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      cur_tok = (cur_cmd * max_cjk_val) + cur_chr;
    else
      cur_tok = (cur_cmd * max_char_val) + cur_chr;
  }
  else
    cur_tok = cs_token_flag + cur_cs;
}

// reads a mandatory |left_brace|
void scan_left_brace (void)
{
  do {
    get_x_token();
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

  if (cur_cmd != left_brace)
  {
    print_err("Missing { inserted");
    help4("A left brace was mandatory here, so I've put one in.",
        "You might want to delete and/or insert some corrections",
        "so that I will find a matching right brace soon.",
        "(If you're confused by all this, try typing `I}' now.)");
    back_error();
    cur_tok = left_brace_token + '{';
    cur_cmd = left_brace;
    cur_chr = '{';
    incr(align_state);
  }
}

static void scan_optional_equals (void)
{
  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok != other_token + '=')
    back_input();
}

// look for a given string
static boolean scan_keyword (const char * s)
{
  pointer p;  // {tail of the backup list}
  pointer q;  // {new node being added to the token list via |store_new_token|}
  const char * k; // {index into |str_pool|}
  pointer save_cur_cs; // {to save |cur_cs|}

  p = backup_head;
  link(p) = null;
  k = s;
  save_cur_cs = cur_cs;

  while (*k)
  {
    get_x_token();  // {recursion is possible here}

    if ((cur_cs == 0) && ((cur_chr == (*k)) || (cur_chr == (*k) - 'a' + 'A')))
    {
      store_new_token(cur_tok);
      incr(k);
    }
    else if ((cur_cmd != spacer) || (p != backup_head))
    {
      back_input();

      if (p != backup_head)
        back_list(link(backup_head));

      cur_cs = save_cur_cs;
      return false;
    }
  }

  flush_list(link(backup_head));

  return true;
}

static void mu_error (void)
{
  print_err("Incompatible glue units");
  help1("I'm going to assume that 1mu=1pt when they're mixed.");
  error();
}

static void scan_eight_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 255))
  {
    print_err("Bad register code");
    help2("A register number must be between 0 and 255.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void scan_ascii_num (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 255))
  {
    print_err("Bad character code");
    help2("A character number must be between 0 and 255.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void scan_char_num (void)
{
  scan_int();

  if (!(is_char_ascii(cur_val)) && !(is_char_kanji(cur_val)))
  {
    print_err("Bad character code");
    help2("A character number must be between 0 and 255, or KANJI code.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void scan_four_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 15))
  {
    print_err("Bad number");
    help2("Since I expected to read a number between 0 and 15,",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void scan_four_bit_int_or_18 (void)
{
  scan_int();

  if ((cur_val < 0) || ((cur_val > 15) && (cur_val != 18)))
  {
    print_err("Bad number");
    help2("Since I expected to read a number between 0 and 15,",
      "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void scan_fifteen_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 32767))
  {
    print_err("Bad mathchar");
    help2("A mathchar number must be between 0 and 32767.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

static void scan_twenty_seven_bit_int (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > 0777777777))
  {
    print_err("Bad delimiter code");
    help2("A numeric delimiter code must be between 0 and 2^{27}-1.",
        "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void scan_font_ident (void)
{
  internal_font_number f;
  halfword m;

  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_cmd == def_font)
    f = cur_font;
  else if (cur_cmd == def_tfont)
    f = cur_tfont;
  else if (cur_cmd == def_jfont)
    f = cur_jfont;
  else if (cur_cmd == set_font)
    f = cur_chr; 
  else if (cur_cmd == def_family)
  {
    m = cur_chr;
    scan_four_bit_int();
    f = equiv(m + cur_val);
  }
  else
  {
    print_err("Missing font identifier");
    help2("I was looking for a control sequence whose",
        "current meaning has been defined by \\font.");
    back_error();
    f = null_font;
  }

  cur_val = f;
}

// sets |cur_val| to |font_info| location
void find_font_dimen (boolean writing)
{
  internal_font_number f;
  integer n;  // {the parameter number}

  scan_int();
  n = cur_val;
  scan_font_ident();
  f = cur_val;

  if (n < 0)
    cur_val = fmem_ptr;
  else
  {
    if (writing && (n <= space_shrink_code) && (n >= space_code) && (font_glue[f] != 0))
    {
      delete_glue_ref(font_glue[f]);
      font_glue[f] = null;
    }

    if (n > font_params[f])
    {
      if (f < font_ptr)
        cur_val = fmem_ptr;
      else
      {
        do {
 #ifdef APTEX_EXTENSION
          if (fmem_ptr == current_font_mem_size)
            font_info = realloc_font_info(increment_font_mem_size);

          if (fmem_ptr == current_font_mem_size)
            overflow("font memory", current_font_mem_size);
#else
          if (fmem_ptr == font_mem_size)
            overflow("font memory", font_mem_size);
#endif
          font_info[fmem_ptr].sc = 0;
          incr(fmem_ptr);
          incr(font_params[f]);
        } while (!(n == font_params[f]));

        cur_val = fmem_ptr - 1; // {this equals |param_base[f]+font_params[f]|}
      }
    }
    else
      cur_val = n + param_base[f];
  }
  // @<Issue an error message if |cur_val=fmem_ptr|@>
  if (cur_val == fmem_ptr)
  {
    print_err("Font ");
    sprint_esc(font_id_text(f));
    prints(" has only ");
    print_int(font_params[f]);
    prints(" fontdimen parameters");
    help2("To increase the number of font parameters, you must",
      "use \\fontdimen immediately after the \\font is loaded.");
    error();
  }
}

// fetch an internal parameter
static void scan_something_internal (small_number level, boolean negative)
{
  halfword m;
  pointer q, r;
  pointer tx;
  halfword qx;
  four_quarters i;
  integer p;

restart:
  m = cur_chr;

  switch (cur_cmd)
  {
    case assign_kinsoku:
      {
        scan_int();
        q = get_kinsoku_pos(tokanji(cur_val), cur_pos);
        cur_val_level = int_val;
        cur_val = 0;

        if ((q != no_entry) && (m == kinsoku_type(q)))
          scanned_result(kinsoku_penalty(q), int_val);
      }
      break;

    case assign_inhibit_xsp_code:
      {
        scan_int();
        q = get_inhibit_pos(tokanji(cur_val), cur_pos);
        cur_val_level = int_val;
        cur_val = inhibit_none;

        if (q != no_entry)
          cur_val = inhibit_xsp_type(q);

        if (cur_val > inhibit_none)
          cur_val = inhibit_none;
      }
      break;

    case set_kansuji_char:
      {
        scan_int();
        cur_val_level = int_val;

        if ((cur_val < 0) || (cur_val > 9))
        {
          print_err("Invalid KANSUJI number (");
          print_int(cur_val);
          print_char(')');
          help1("I'm skipping this control sequences.");
          error();
          return;
        }
        else
          cur_val = fromDVI(kansuji_char(cur_val));
      }
      break;

    case def_code:
      {
        if (m == math_code_base)
        {
          scan_ascii_num();
          scanned_result(math_code(cur_val), int_val);
        }
        else if (m == kcat_code_base)
        {
          scan_char_num();
          scanned_result(equiv(m + kcatcodekey(cur_val)), int_val);
        }
        else if (m < math_code_base)
        {
          scan_ascii_num();
          scanned_result(equiv(m + cur_val), int_val);
        }
        else
        {
          scan_ascii_num();
          scanned_result(eqtb[m + cur_val].cint, int_val);
        }
      }
      break;

    case toks_register:
    case assign_toks:
    case def_family:
    case set_font:
    case def_font:
      if (level != tok_val)
      {
        print_err("Missing number, treated as zero");
        help3("A number should have been here; I inserted `0'.",
            "(If you can't figure out why I needed to see a number,",
            "look up `weird error' in the index to The TeXbook.)");
        back_error();
        scanned_result(0, dimen_val);
      }
      else if (cur_cmd <= assign_toks)
      {
        if (cur_cmd < assign_toks)
        {
          if (m == mem_bot)
          {
            scan_register_num();

            if (cur_val < 256)
              cur_val = equiv(toks_base + cur_val);
            else
            {
              find_sa_element(tok_val, cur_val, false);

              if (cur_ptr == null)
                cur_val = null;
              else
                cur_val = sa_ptr(cur_ptr);
            }
          }
          else
            cur_val = sa_ptr(m);
        }
        else
          cur_val = equiv(m);

        cur_val_level = tok_val;
      }
      else
      {
        back_input();
        scan_font_ident();
        scanned_result(font_id_base + cur_val, ident_val);
      }
      break;

    case assign_int:
      scanned_result(eqtb[m].cint, int_val);
      break;

    case assign_dimen:
      scanned_result(eqtb[m].sc, dimen_val);
      break;

    case assign_glue:
      scanned_result(equiv(m), glue_val);
      break;

    case assign_mu_glue:
      scanned_result(equiv(m), mu_val);
      break;

    case set_aux:
      if (abs(mode) != m)
      {
        print_err("Improper ");
        print_cmd_chr(set_aux, m);
        help4("You can refer to \\spacefactor only in horizontal mode;",
            "you can refer to \\prevdepth only in vertical mode; and",
            "neither of these is meaningful inside \\write. So",
            "I'm forgetting what you said and using zero instead.");
        error();

        if (level != tok_val)
          scanned_result(0, dimen_val);
        else
          scanned_result(0, int_val);
      }
      else if (m == vmode)
        scanned_result(prev_depth, dimen_val);
      else
        scanned_result(space_factor, int_val);
      break;

    case set_prev_graf:
      if (mode == 0)
        scanned_result(0, int_val);
      else
      {
        nest[nest_ptr] = cur_list;
        p = nest_ptr;

        while (abs(nest[p].mode_field) != vmode)
          decr(p);

        scanned_result(nest[p].pg_field, int_val);
      }
      break;

    case set_page_int:
      {
        if (m == 0)
          cur_val = dead_cycles;
        else if (m == 2)
          cur_val = interaction;
        else
          cur_val = insert_penalties;

        cur_val_level = int_val;
      }
      break;

    case set_page_dimen:
      {
        if ((page_contents == empty) && (!output_active))
        {
          if (m == 0)
            cur_val = max_dimen;
          else
            cur_val = 0;
        }
        else
          cur_val = page_so_far[m];

        cur_val_level = dimen_val;
      }
      break;

    case set_shape:
      {
        if (m > par_shape_loc)
        {
          scan_int();

          if ((equiv(m) == null) || (cur_val < 0))
            cur_val = 0;
          else
          {
            if (cur_val > penalty(equiv(m)))
              cur_val = penalty(equiv(m));

            cur_val = penalty(equiv(m) + cur_val);
          }
        }
        else if (par_shape_ptr == 0)
          cur_val = 0;
        else
          cur_val = info(par_shape_ptr);

        cur_val_level = int_val;
      }
      break;

    case set_box_dimen:
      {
        scan_register_num();
        fetch_box(q);

        if (q == 0)
          cur_val = 0;
        else
        {
          qx = q;

          while ((q != null) && (abs(box_dir(q)) != abs(direction)))
            q = link(q);

          if (q == 0)
          {
            r = link(qx);
            link(qx) = null;
            q = new_dir_node(qx, abs(direction));
            link(qx) = r;
            cur_val = mem[q + m].sc;
            delete_glue_ref(space_ptr(q));
            delete_glue_ref(xspace_ptr(q));
            free_node(q, box_node_size);
          }
          else
            cur_val = mem[q + m].sc;
        }

        cur_val_level = dimen_val;
      }
      break;

    case kchar_given:
    case char_given:
    case math_given:
      scanned_result(cur_chr, int_val);
      break;

    case assign_font_dimen:
      {
        find_font_dimen(false);
        font_info[fmem_ptr].sc = 0;
        scanned_result(font_info[cur_val].sc, dimen_val);
      }
      break;

    case assign_font_int:
      {
        scan_font_ident();

        if (m == 0)
          scanned_result(hyphen_char[cur_val], int_val);
        else
          scanned_result(skew_char[cur_val], int_val);
      }
      break;

    case tex_register:
      {
        if ((m < mem_bot) || (m > lo_mem_stat_max))
        {
          cur_val_level = sa_type(m);

          if (cur_val_level < glue_val)
            cur_val = sa_int(m);
          else
            cur_val = sa_ptr(m);
        }
        else
        {
          scan_register_num();
          cur_val_level = m - mem_bot;

          if (cur_val > 255)
          {
            find_sa_element(cur_val_level, cur_val, false);

            if (cur_ptr == null)
            {
              if (cur_val_level < glue_val)
                cur_val = 0;
              else
                cur_val = zero_glue;
            }
            else if (cur_val_level < glue_val)
              cur_val = sa_int(cur_ptr);
            else
              cur_val = sa_ptr(cur_ptr);
          }
          else switch (cur_val_level)
          {
            case int_val:
              cur_val = count(cur_val);
              break;

            case dimen_val:
              cur_val = dimen(cur_val);
              break;

            case glue_val:
              cur_val = skip(cur_val);
              break;

            case mu_val:
              cur_val = mu_skip(cur_val);
              break;
          }
        }
      }
      break;

    case last_item:
      if (cur_chr >= input_line_no_code)
      {
        if (m >= eTeX_glue)
        {
          if (m < eTeX_mu)
          {
            switch (m)
            {
              case mu_to_glue_code:
                scan_mu_glue();
                break;
            }

            cur_val_level = glue_val;
          }
          else if (m < eTeX_expr)
          {
            switch (m)
            {
              case glue_to_mu_code:
                scan_normal_glue();
                break;
            }

            cur_val_level = mu_val;
          }
          else
          {
            cur_val_level = m - eTeX_expr + int_val;
            scan_expr();
          }

          while (cur_val_level > level)
          {
            if (cur_val_level == glue_val)
            {
              m = cur_val;
              cur_val = width(m);
              delete_glue_ref(m);
            }
            else if (cur_val_level == mu_val)
              mu_error();

            decr(cur_val_level);
          }

          if (negative)
          {
            if (cur_val_level >= glue_val)
            {
              m = cur_val;
              cur_val = new_spec(m);
              delete_glue_ref(m);

              {
                negate(width(cur_val));
                negate(stretch(cur_val));
                negate(shrink(cur_val));
              }
            }
            else
              negate(cur_val);
          }

          return;
        }
        else if (m >= eTeX_dim)
        {
          switch (m)
          {
            case font_char_wd_code:
            case font_char_ht_code:
            case font_char_dp_code:
            case font_char_ic_code:
              {
                scan_font_ident();
                q = cur_val;

                if (font_dir[q] != dir_default)
                {
                  scan_int();
                  
                  if (cur_val >= 0)
                  {
                    if (is_char_kanji(cur_val))
                      cur_val = get_jfm_pos(KANJI(cur_val), q); 
                    else
                      cur_val = -1;
                  }
                  else
                  {
                    cur_val = -(cur_val + 1);

                    if ((font_bc[q] > cur_val) || (font_ec[q] < cur_val))
                      cur_val = -1;
                  }

                  if (cur_val != -1)
                  {
                    i = char_info(q, cur_val);

                    switch (m)
                    {
                      case font_char_wd_code:
                        cur_val = char_width(q, i);
                        break;

                      case font_char_ht_code:
                        cur_val = char_height(q, height_depth(i));
                        break;

                      case font_char_dp_code:
                        cur_val = char_depth(q, height_depth(i));
                        break;

                      case font_char_ic_code:
                        cur_val = char_italic(q, i);
                        break;
                    }
                  }
                  else
                    cur_val = 0;
                }
                else
                {
                  scan_char_num();

                  if ((font_bc[q] <= cur_val) && (font_ec[q] >= cur_val))
                  {
                    i = char_info(q, cur_val);

                    switch (m)
                    {
                      case font_char_wd_code:
                        cur_val = char_width(q, i);
                        break;

                      case font_char_ht_code:
                        cur_val = char_height(q, height_depth(i));
                        break;

                      case font_char_dp_code:
                        cur_val = char_depth(q, height_depth(i));
                        break;

                      case font_char_ic_code:
                        cur_val = char_italic(q, i);
                        break;
                    }
                  }
                  else
                    cur_val = 0;
                }
              }
              break;

            case par_shape_length_code:
            case par_shape_indent_code:
            case par_shape_dimen_code:
              {
                q = cur_chr - par_shape_length_code;
                scan_int();

                if ((par_shape_ptr == null) || (cur_val <= 0))
                  cur_val = 0;
                else
                {
                  if (q == 2)
                  {
                    q = cur_val % 2;
                    cur_val = (cur_val + q) / 2;
                  }

                  if (cur_val > info(par_shape_ptr))
                    cur_val = info(par_shape_ptr);

                  cur_val = mem[par_shape_ptr + 2 * cur_val - q].sc;
                }

                cur_val_level = dimen_val;
              }
              break;

            case glue_stretch_code:
            case glue_shrink_code:
              {
                scan_normal_glue();
                q = cur_val;

                if (m == glue_stretch_code)
                  cur_val = stretch(q);
                else
                  cur_val = shrink(q);

                delete_glue_ref(q);
              }
              break;
          }

          cur_val_level = dimen_val;
        }
        else
        {
          switch (m)
          {
            case input_line_no_code:
              cur_val = line;
              break;

            case badness_code:
              cur_val = last_badness;
              break;

            case pdf_last_x_pos_code:
              cur_val = pdf_last_x_pos;
              break;

            case pdf_last_y_pos_code:
              cur_val = pdf_last_y_pos;
              break;

            case elapsed_time_code:
              cur_val = get_microinterval();
              break;

            case random_seed_code:
              cur_val = random_seed;
              break;

            case shell_escape_code:
              cur_val = aptex_env.flag_shell_escape;
              break;

            case ptex_version_code:
              cur_val = pTeX_version;
              break;

            case uptex_version_code:
              cur_val = upTeX_version;
              break;

            case eptex_version_code:
              cur_val = epTeX_version_number;
              break;

            case ptex_minor_version_code:
              cur_val = pTeX_minor_version;
              break;

            case eTeX_version_code:
              cur_val = eTeX_version;
              break;

            case current_group_level_code:
              cur_val = cur_level - level_one;
              break;

            case current_group_type_code:
              cur_val = cur_group;
              break;

            case current_if_level_code:
              {
                q = cond_ptr;
                cur_val = 0;

                while (q != null)
                {
                  incr(cur_val);
                  q = link(q);
                }
              }
              break;

            case current_if_type_code:
              if (cond_ptr == null)
                cur_val = 0;
              else if (cur_if < unless_code)
                cur_val = cur_if + 1;
              else
                cur_val = -(cur_if - unless_code + 1);
              break;

            case current_if_branch_code:
              if ((if_limit == or_code) || (if_limit == else_code))
                cur_val = 1;
              else if (if_limit == fi_code)
                cur_val = -1;
              else
                cur_val = 0;
              break;

            case glue_stretch_order_code:
            case glue_shrink_order_code:
              {
                scan_normal_glue();
                q = cur_val;

                if (m == glue_stretch_order_code)
                  cur_val = stretch_order(q);
                else
                  cur_val = shrink_order(q);

                delete_glue_ref(q);
              }
              break;

            case current_spacing_mode_code:
              cur_val = auto_spacing;
              break;

            case current_xspacing_mode_code:
              cur_val = auto_xspacing;
              break;

            case current_cjk_token_code:
              cur_val = enable_cjk_token;
              break;
          }

          cur_val_level = int_val;
        }
      }
      else
      {
        if (cur_chr == glue_val)
          cur_val = zero_glue;
        else
          cur_val = 0;

        find_effective_tail();

        if ((cur_chr == last_node_type_code) || (cur_chr == last_node_subtype_code))
        {
          cur_val_level = int_val;

          if ((tx == head) || (mode == 0))
            cur_val = -1;
        }
        else if (cur_chr == last_node_char_code)
        {
          cur_val_level = int_val;
          cur_val = -1;
        }
        else
          cur_val_level = cur_chr;

        if ((cur_chr == last_node_char_code) && (is_char_node(tx)) && (tx != head))
        //{ |tx| might be ``second node'' of a KANJI character; so we need to look the node before |tx| }
        {
          r = head;
          q = head;
          while (q != tx)
          {
            r = q;
            q = link(q);
          } // { |r| is the node just before |tx| }
          if ((r != head) && is_char_node(r))
            if (font_dir[font(r)] != dir_default)
              tx = r;
          find_last_char();
        }

        if (!is_char_node(tx) && (tx != head) && (mode != 0))
          switch (cur_chr)
          {
            case int_val:
              if (type(tx) == penalty_node)
                cur_val = penalty(tx);
              break;

            case dimen_val:
              if (type(tx) == kern_node)
                cur_val = width(tx);
              break;

            case glue_val:
              if (type(tx) == glue_node)
              {
                cur_val = glue_ptr(tx);

                if (subtype(tx) == mu_glue)
                  cur_val_level = mu_val;
              }
              break;

            case last_node_type_code:
              if (type(tx) <= unset_node)
              {
                if (type(tx) == dir_node)
                  tx = list_ptr(tx);

                cur_val = type(tx);

                if (cur_val < dir_node)
                  cur_val = cur_val + 1;
                else if (cur_val > disp_node)
                  cur_val = cur_val - 1;
              }
              else
                cur_val = unset_node;
              break;

            case last_node_subtype_code:
              if (type(tx) <= unset_node)
                cur_val = subtype(tx);
              else
              {
                cur_val = type(tx);
                if (cur_val < unset_node + 4)
                  cur_val = cur_val - unset_node - 1;
                else if (cur_val == unset_node + 4)
                  cur_val = cur_val - unset_node - 1 + subtype(tx);
                else
                  cur_val = cur_val - unset_node + 1;
              }
              break;

            case last_node_char_code:
              ignore_font_kerning();
              break;
          }
        else if ((mode == vmode) && (tx == head))
          switch (cur_chr)
          {
            case int_val:
              cur_val = last_penalty;
              break;

            case dimen_val:
              cur_val = last_kern;
              break;

            case glue_val:
              if (last_glue != max_halfword)
                cur_val = last_glue;
              break;

            case last_node_type_code:
              cur_val = last_node_type;
              break;

            case last_node_subtype_code:
              cur_val = last_node_subtype;
              break;
          }
      }
      break;

    case ignore_spaces: // {trap unexpandable primitives}
      if (cur_chr == 1)
      {
        get_token();

        if (cur_cs < hash_base)
          cur_cs = prim_lookup(cur_cs - single_base);
        else
          cur_cs = prim_lookup(text(cur_cs));

        if (cur_cs != undefined_primitive)
        {
          cur_cmd = prim_eq_type(cur_cs);
          cur_chr = prim_equiv(cur_cs);
          cur_cs = prim_eqtb_base + cur_cs;
          cur_tok = cs_token_flag + cur_cs;
        }
        else
        {
          cur_cmd = relax;
          cur_chr = 0;
          cur_tok = cs_token_flag + frozen_relax;
          cur_cs = frozen_relax;
        }

        goto restart;
      }
      break;

    default:
      {
        print_err("You can't use `");
        print_cmd_chr(cur_cmd, cur_chr);
        prints("' after ");
        print_esc("the");
        help1("I'm forgetting what you said and using zero instead.");
        error();

        if (level != tok_val)
          scanned_result(0, dimen_val);
        else
          scanned_result(0, int_val);
      }
      break;
  }

  while (cur_val_level > level)
  {
    if (cur_val_level == glue_val)
      cur_val = width(cur_val);
    else if (cur_val_level == mu_val)
      mu_error();

    decr(cur_val_level);
  }
 
  if (negative)
  {
    if (cur_val_level >= glue_val)
    {
      cur_val = new_spec(cur_val);

      {
        negate(width(cur_val));
        negate(stretch(cur_val));
        negate(shrink(cur_val));
      }
    }
    else
      negate(cur_val);
  }
  else if ((cur_val_level >= glue_val) && (cur_val_level <= mu_val))
    add_glue_ref(cur_val);
}

// sets |cur_cmd|, |cur_chr|, |cur_cs| to next token
void get_next (void)
{
  uint32_t k;
  halfword t;
  uint32_t cat;
  integer l;
  ASCII_code c, cc;
  uint32_t d;

restart:
  cur_cs = 0;

  if (state != token_list)
  {
lab_switch:
    if (loc <= limit)
    {
      cur_chr = fromBUFF(buffer, limit + 1, loc);
      cur_cmd = kcat_code(kcatcodekey(cur_chr));

      if ((multistrlen(buffer, limit + 1, loc) > 1) && check_kcat_code(cur_cmd))
      {
        if (cur_cmd == not_cjk)
          cur_cmd = other_kchar;

        loc = loc + multistrlen(buffer, limit + 1, loc);
      }
      else
      {
        cur_chr = buffer[loc];
        incr(loc);

reswitch:
        cur_cmd = cat_code(cur_chr);
      };

      switch (state + cur_cmd)
      {
        case any_state_plus(ignore):
        case skip_blanks + spacer:
        case skip_blanks_kanji + spacer:
        case new_line + spacer:
          goto lab_switch;
          break;

        case any_state_plus(escape):
          {
            if (loc > limit)
              cur_cs = null_cs;
            else
            {
              k = loc;
              cur_chr = fromBUFF(buffer, limit + 1, k);
              cat = kcat_code(kcatcodekey(cur_chr));

              if ((multistrlen(buffer, limit + 1, k) > 1) && check_kcat_code(cat))
              {
                if (cat == not_cjk)
                  cat = other_kchar;

                k = k + multistrlen(buffer, limit + 1, k);
              }
              else
              {
                cur_chr = buffer[k];
                cat = cat_code(cur_chr);
                incr(k);
              };

start_cs:
              if ((cat == letter) || (cat == hangul))
                state = skip_blanks;
              else if ((cat == kanji) || (cat == kana))
              {
                if (ptex_lineend % 2 == 0)
                  state = skip_blanks_kanji;
                else
                  state = skip_blanks;
              }
              else if (cat == spacer)
                state = skip_blanks;
              else if (cat == other_kchar)
              {
                if ((ptex_lineend / 2) % 2 == 0)
                  state = mid_kanji;
                else
                  state = mid_line;
              }
              else
                state = mid_line;

              if (cat == other_kchar)
              {
                cur_cs = id_lookup(loc, k - loc);
                loc = k;
                goto found;
              }
              else if (((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)) && (k <= limit))
              {
                do {
                  cur_chr = fromBUFF(buffer, limit + 1, k);
                  cat = kcat_code(kcatcodekey(cur_chr));

                  if ((multistrlen(buffer, limit + 1, k) > 1) && check_kcat_code(cat))
                  {
                    if (cat == not_cjk)
                      cat = other_kchar;

                    k = k + multistrlen(buffer, limit + 1, k);

                    if ((cat == kanji) || (cat == kana))
                    {
                      if (ptex_lineend % 2 == 0)
                        state = skip_blanks_kanji;
                      else
                        state = skip_blanks;
                    }
                    else if (cat == hangul)
                      state = skip_blanks;
                  }
                  else
                  {
                    cur_chr = buffer[k];
                    cat = cat_code(cur_chr);
                    incr(k);
                  }
                  
                  while ((buffer[k] == cur_chr) && (cat == sup_mark) && (k < limit))
                  {
                    c = buffer[k + 1];

                    if (c < 0200)
                    {
                      d = 2;

                      if (is_hex(c))
                      {
                        if (k + 2 <= limit)
                        {
                          cc = buffer[k + 2];

                          if (is_hex(cc))
                            incr(d);
                        }
                      }

                      if (d > 2)
                      {
                        hex_to_cur_chr();
                      }
                      else if (c < 0100)
                        cur_chr = c + 0100;
                      else
                        cur_chr = c - 0100;

                      cat = cat_code(cur_chr);

                      if ((cat == letter) || (cat == sup_mark))
                      {
                        buffer[k - 1] = cur_chr;
                        limit = limit - d;
                        first = first - d;
                        l = k;

                        while (l <= limit)
                        {
                          buffer[l] = buffer[l + d];
                          incr(l);
                        }
                      }
                    }
                  }
                  if (cat == letter)
                    state = skip_blanks;
                } while (!(!((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)) || (k > limit)));

                if (!((cat == letter) || (cat == kanji) || (cat == kana) || (cat == hangul)))
                  decr(k);

                if (cat == other_kchar)
                  k = k - multilenbuffchar(cur_chr) + 1;

                if (k > loc + 1)
                {
                  cur_cs = id_lookup(loc, k - loc);
                  loc = k;
                  goto found;
                }
              }
              else
              {
                if (buffer[k] == cur_chr)
                {
                  if (cat == sup_mark)
                  {
                    if (k < limit)
                    {
                      c = buffer[k + 1];

                      if (c < 128)
                      {
                        d = 2;

                        if (is_hex(c))
                        {
                          if (k + 2 <= limit)
                          {
                            cc = buffer[k + 2];

                            if (is_hex(cc))
                              incr(d);
                          }
                        }

                        if (d > 2)
                        {
                          hex_to_cur_chr();
                          buffer[k - 1] = cur_chr;
                        }
                        else if (c < 64)
                          buffer[k - 1] = c + 64;
                        else
                          buffer[k - 1] = c - 64;

                        limit = limit - d;
                        first = first - d;
                        l = k;
                        cur_chr = buffer[k - 1];
                        cat = cat_code(cur_chr);

                        while (l <= limit)
                        {
                          buffer[l] = buffer[l + d];
                          incr(l);
                        }

                        goto start_cs;
                      }
                    }
                  }
                }
              }

              if ((cat == kanji) || (cat == kana) || (cat == hangul))
              {
                cur_cs = id_lookup(loc, k - loc);
                loc = k;
                goto found;
              }
              else
              {
                cur_cs = single_base + buffer[loc];
                incr(loc);
              }
            }

found:
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);

            if (cur_cmd >= outer_call)
              check_outer_validity();
          }
          break;

        case any_state_plus(active_char):
          {
            cur_cs = cur_chr + active_base;
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);
            state = mid_line;

            if (cur_cmd >= outer_call)
              check_outer_validity();
          }
          break;

        case any_state_plus(sup_mark):
          {
            if (cur_chr == buffer[loc])
            {
              if (loc < limit)
              {
                c = buffer[loc + 1];

                if (c < 128)
                {
                  loc = loc + 2;

                  if (is_hex(c))
                  {
                    if (loc <= limit)
                    {
                      cc = buffer[loc];

                      if (is_hex(cc))
                      {
                        incr(loc);
                        hex_to_cur_chr();
                        goto reswitch;
                      }
                    }
                  }

                  if (c < 64)
                    cur_chr = c + 64;
                  else
                    cur_chr = c - 64;

                  goto reswitch;
                }
              }
            }

            state = mid_line;
          }
          break;

        case any_state_plus(invalid_char):
          {
            print_err("Text line contains an invalid character");
            help2("A funny symbol that I can't read has just been input.",
                "Continue, and I'll forget that it ever happened.");
            deletions_allowed = false;
            error();
            deletions_allowed = true;
            goto restart;
          }
          break;

        case mid_kanji + spacer:
        case mid_line + spacer:
          {
            state = skip_blanks;
            cur_chr = ' ';
          }
          break;

        case mid_line + car_ret:
          {
            loc = limit + 1;
            cur_cmd = spacer;
            cur_chr = ' ';
          }
          break;

        case mid_kanji + car_ret:
          if (skip_mode)
          {
            loc = limit + 1;
            goto lab_switch;
          }
          else
          {
            loc = limit + 1;
            cur_cmd = spacer;
            cur_chr = ' ';
          }
          break;

        case skip_blanks + car_ret:
        case skip_blanks_kanji + car_ret:
        case any_state_plus(comment):
          {
            loc = limit + 1;
            goto lab_switch;
          }
          break;

        case new_line + car_ret:
          {
            loc = limit + 1;
            cur_cs = par_loc;
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);

            if (cur_cmd >= outer_call)
              check_outer_validity();
          }
          break;

        case mid_line + left_brace:
          incr(align_state);
          break;

        case mid_kanji + left_brace:
          {
            incr(align_state);
            if ((ptex_lineend / 4) % 2 == 1)
              state = mid_line;
          }
          break;

        case skip_blanks + left_brace:
        case new_line + left_brace:
          {
            state = mid_line;
            incr(align_state);
          }
          break;

        case skip_blanks_kanji + left_brace:
          {
            state = mid_kanji;
            incr(align_state);
          }
          break;

        case mid_line + right_brace:
          decr(align_state);
          break;

        case mid_kanji + right_brace:
          {
            decr(align_state);
            if ((ptex_lineend / 4) % 2 == 1)
              state = mid_line;
          }
          break;

        case skip_blanks + right_brace:
        case new_line + right_brace:
          {
            state = mid_line;
            decr(align_state);
          }
          break;

        case skip_blanks_kanji + right_brace:
          {
            state = mid_kanji;
            decr(align_state);
          }
          break;

        case add_delims_to(skip_blanks):
        case add_delims_to(skip_blanks_kanji):
        case add_delims_to(new_line):
        case add_delims_to(mid_kanji):
          state = mid_line;
          break;

        case all_jcode(skip_blanks):
        case all_jcode(skip_blanks_kanji):
        case all_jcode(new_line):
        case all_jcode(mid_line):
          state = mid_kanji;
          break;

        case hangul_code(skip_blanks):
        case hangul_code(skip_blanks_kanji):
        case hangul_code(new_line):
        case hangul_code(mid_kanji):
          state = mid_line;
          break;

        default:
          do_nothing();
          break;
      }
    }
    else
    {
      state = new_line;

      if (name > 17)
      {
        incr(line);
        first = start;

        if (!force_eof)
        {
          if (name <= 19)
          {
            if (pseudo_input())
              firm_up_the_line();
            else if ((every_eof != null) && !eof_seen[index])
            {
              limit = first - 1;
              eof_seen[index] = true;
              begin_token_list(every_eof, every_eof_text);
              goto restart;
            }
            else
              force_eof = true;
          }
          else
          {
            if (input_ln(cur_file, true))
              firm_up_the_line();
            else if ((every_eof != null) && !eof_seen[index])
            {
              limit = first - 1;
              eof_seen[index] = true;
              begin_token_list(every_eof, every_eof_text);
              goto restart;
            }
            else
              force_eof = true;
          }
        }

        if (force_eof)
        {
          if (tracing_nesting > 0)
          {
            if ((grp_stack[in_open] != cur_boundary) ||
              (if_stack[in_open] != cond_ptr))
              file_warning();
          }

          if (name >= 19)
          {
            print_char(')');
            decr(open_parens);
            update_terminal();
          }

          force_eof = false;
          end_file_reading();
          check_outer_validity();
          goto restart;
        }

        if (end_line_char_inactive())
          decr(limit);
        else
          buffer[limit] = end_line_char;

        first = limit + 1;
        loc = start;
      }
      else
      {
        if (!terminal_input)
        {
          cur_cmd = 0;
          cur_chr = 0;
          return;
        }

        if (input_ptr > 0)
        {
          end_file_reading();
          goto restart;
        }

        if (selector < log_only)
          open_log_file();

        if (interaction > nonstop_mode)
        {
          if (end_line_char_inactive())
            incr(limit);

          if (limit == start)
            print_nl("(Please type a command or say `\\end')");

          print_ln();
          first = start;
          prompt_input("*");
          limit = last;

          if (end_line_char_inactive())
            decr(limit);
          else
            buffer[limit]= end_line_char;

          first = limit + 1;
          loc = start;
        }
        else
          fatal_error("*** (job aborted, no legal \\end found)");
      }

      check_interrupt();
      goto lab_switch;
    }
  }
  else if (loc != 0)
  {
    t = info(loc);
    loc = link(loc);

    if (t >= cs_token_flag)
    {
      cur_cs = t - cs_token_flag;
      cur_cmd = eq_type(cur_cs);
      cur_chr = equiv(cur_cs);

      if (cur_cmd >= outer_call)
      {
        if (cur_cmd == dont_expand)
        {
          cur_cs = info(loc) - cs_token_flag;
          loc = 0;
          cur_cmd = eq_type(cur_cs);
          cur_chr = equiv(cur_cs);

          if (cur_cmd > max_command)
          {
            cur_cmd = relax;
            cur_chr = no_expand_flag;
          }
        }
        else
          check_outer_validity();
      }
    }
    else if (check_kanji(t))
    {
      cur_cmd = t / max_cjk_val;
      cur_chr = t % max_cjk_val;
    }
    else
    {
      cur_cmd = t / max_char_val;
      cur_chr = t % max_char_val;

      switch (cur_cmd)
      {
        case left_brace:
          incr(align_state);
          break;

        case right_brace:
          decr(align_state);
          break;

        case out_param:
          {
            begin_token_list(param_stack[param_start + cur_chr - 1], parameter);
            goto restart;
          }
          break;

        default:
          do_nothing();
          break;
      }
    }
  }
  else
  {
    end_token_list();
    goto restart;
  }

  if (cur_cmd <= car_ret)
  {
    if (cur_cmd >= tab_mark)
    {
      if (align_state == 0)
      {
        if ((scanner_status == aligning) || (cur_align == null))
          fatal_error("(interwoven alignment preambles are not allowed)");

        cur_cmd = extra_info(cur_align);
        extra_info(cur_align) = cur_chr;

        if (cur_cmd == omit)
          begin_token_list(omit_template, v_template);
        else
          begin_token_list(v_part(cur_align), v_template);

        align_state = 1000000;
        goto restart;
      }
    }
  }
}

// sets |cur_val| to an integer
void scan_int (void)
{
  boolean negative;
  integer m;
  small_number d;
  boolean vacuous;
  boolean OK_so_far;

  radix = 0;
  OK_so_far = true;
  negative = false;

  do {
    do {
      get_x_token();
    } while (!(cur_cmd != spacer));

    if (cur_tok == other_token + '-')
    {
      negative = !negative;
      cur_tok = other_token + '+';
    }
  } while (!(cur_tok != other_token + '+'));

restart:
  if (cur_tok == alpha_token)
  {
    get_token();

    if (cur_tok < cs_token_flag)
    {
      if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      {
        skip_mode = false;
        cur_val = tonum(cur_chr);
      }
      else
      {
        cur_val = cur_chr;

        if (cur_cmd <= right_brace)
        {
          if (cur_cmd == right_brace)
            incr(align_state);
          else
            decr(align_state);
        }
      }
    }
    else if (cur_tok < cs_token_flag + single_base)
      cur_val = cur_tok - cs_token_flag - active_base;
    else if (cur_tok < cs_token_flag + null_cs)
      cur_val = cur_tok - cs_token_flag - single_base;
    else
    {
      m = text(cur_tok - cs_token_flag);
      if (str_start[m + 1] - str_start[m] == multistrlen(str_pool, str_start[m + 1], str_start[m]))
        cur_val = fromBUFF(str_pool, str_start[m + 1], str_start[m]);
      else
      {
        cur_cmd = invalid_char;
        cur_val = 256;
      }
    }

    if ((cur_val > 255) && (cur_cmd < kanji))
    {
      print_err("Improper alphabetic or KANJI constant");
      help2("A one-character control sequence belongs after a ` mark.",
        "So I'm essentially inserting \\0 here.");
      cur_val = '0';
      back_error();
    }
    else
    {
      get_x_token();

      if (cur_cmd != spacer)
        back_input();
    }

    skip_mode = true;
  }
  else if (cur_tok == cs_token_flag + frozen_primitive)
  {
    get_token();

    if (cur_cs < hash_base)
      cur_cs = prim_lookup(cur_cs - single_base);
    else
      cur_cs = prim_lookup(text(cur_cs));

    if (cur_cs != undefined_primitive)
    {
      cur_cmd = prim_eq_type(cur_cs);
      cur_chr = prim_equiv(cur_cs);
      cur_cs = prim_eqtb_base + cur_cs;
      cur_tok = cs_token_flag + cur_cs;
    }
    else
    {
      cur_cmd = relax;
      cur_chr = 0;
      cur_tok = cs_token_flag + frozen_relax;
      cur_cs = frozen_relax;
    }

    goto restart;
  }
  else if ((cur_cmd >= min_internal) && (cur_cmd <= max_internal))
    scan_something_internal(int_val, false);
  else
  {
    radix = 10;
    m = 214748364;

    if (cur_tok == octal_token)
    {
      radix = 8;
      m = 02000000000;
      get_x_token();
    }
    else if (cur_tok == hex_token)
    {
      radix = 16;
      m = 01000000000;
      get_x_token();
    }

    vacuous = true;
    cur_val = 0;

    while (true)
    {
      if ((cur_tok < zero_token + radix) && (cur_tok >= zero_token) && (cur_tok <= zero_token + 9))
        d = cur_tok - zero_token;
      else if (radix == 16)
      {
        if ((cur_tok <= A_token + 5) && (cur_tok >= A_token))
          d = cur_tok - A_token + 10;
        else if ((cur_tok <= other_A_token + 5) && (cur_tok >= other_A_token))
          d = cur_tok - other_A_token + 10;
        else
          goto done;
      }
      else
        goto done;

      vacuous = false;

      if ((cur_val >= m) && ((cur_val > m) || (d > 7) || (radix != 10)))
      {
        if (OK_so_far)
        {
          print_err("Number too big");
          help2("I can only go up to 2147483647='17777777777=\"7FFFFFFF,",
            "so I'm using that number instead of yours.");
          error();
          cur_val = infinity;
          OK_so_far = false;
        }
      }
      else
        cur_val = cur_val * radix + d;

      get_x_token();
    }

done:
    if (vacuous)
    {
      print_err("Missing number, treated as zero");
      help3("A number should have been here; I inserted `0'.",
        "(If you can't figure out why I needed to see a number,",
        "look up `weird error' in the index to The TeXbook.)");
      back_error();
    } 
    else if (cur_cmd != spacer)
      back_input();
  }

  if (negative)
    negate(cur_val);
}

// sets |cur_val| to a dimension
void scan_dimen (boolean mu, boolean inf, boolean shortcut)
{
  boolean negative;
  integer f;
  integer num, denom;
  small_number k, kk;
  halfword p, q;
  scaled v;
  integer save_cur_val;
  eight_bits t;

  f = 0;
  arith_error = false;
  cur_order = normal;
  negative = false;

  if (!shortcut)
  {
    negative = false;

    do {
      do {
        get_x_token();
      } while (!(cur_cmd != spacer));

      if (cur_tok == other_token + '-')
      {
        negative = !negative;
        cur_tok = other_token + '+';
      }
    } while (!(cur_tok != other_token + '+'));

    if ((cur_cmd >= min_internal) && (cur_cmd <= max_internal))
    {
      if (mu)
      {
        scan_something_internal(mu_val, false);

        if (cur_val_level >= glue_val)
        {
          v = width(cur_val);
          delete_glue_ref(cur_val);
          cur_val = v;
        }

        if (cur_val_level == mu_val)
          goto attach_sign;

        if (cur_val_level != int_val)
          mu_error();
      }
      else
      {
        scan_something_internal(dimen_val, false);

        if (cur_val_level == dimen_val)
          goto attach_sign;
      }
    }
    else
    {
      back_input();

      if (cur_tok == continental_point_token)
        cur_tok = point_token;

      if (cur_tok != point_token)
        scan_int();
      else
      {
        radix = 10;
        cur_val = 0;
      }

      if (cur_tok == continental_point_token)
        cur_tok = point_token;

      if ((radix == 10) && (cur_tok == point_token))
      {
        k = 0;
        p = 0;
        get_token();

        while (true)
        {
          get_x_token();

          if ((cur_tok > zero_token + 9) || (cur_tok < zero_token))
            goto done1;

          if (k < 17)
          {
            q = get_avail();
            link(q) = p;
            info(q) = cur_tok - zero_token;
            p = q;
            incr(k);
          }
        }

done1:
        for (kk = k; kk >= 1; kk--)
        {
          dig[kk - 1] = info(p);
          q = p;
          p = link(p);
          free_avail(q);
        }

        f = round_decimals(k);

        if (cur_cmd != spacer)
          back_input();
      }
    }
  }

  if (cur_val < 0)
  {
    negative = !negative;
    negate(cur_val);
  }

  if (inf)
  {
    if (scan_keyword("fil"))
    {
      cur_order = fil;

      while (scan_keyword("l"))
      {
        if (cur_order == filll)
        {
          print_err("Illegal unit of measure (");
          prints("replaced by filll)");
          help1("I dddon't go any higher than filll.");
          error();
        }
        else
          incr(cur_order);
      }

      goto attach_fraction;
    }
  }

  save_cur_val = cur_val;

  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if ((cur_cmd < min_internal) || (cur_cmd > max_internal))
    back_input();
  else
  {
    if (mu)
    {
      scan_something_internal(mu_val, false);

      if (cur_val_level >= glue_val)
      {
        v = width(cur_val);
        delete_glue_ref(cur_val);
        cur_val = v;
      }

      if (cur_val_level != mu_val)
        mu_error();
    }
    else
      scan_something_internal(dimen_val, false);

    v = cur_val;
    goto found;
  }

  if (mu)
    goto not_found;

  if (scan_keyword("em"))
    v = quad(cur_font);
  else if (scan_keyword("ex"))
    v = x_height(cur_font);
  else if (scan_keyword("zw"))
  {
    if (direction == dir_tate)
      v = char_width(cur_tfont, char_info(cur_tfont, 0));
    else
      v = char_width(cur_jfont, char_info(cur_jfont, 0));
  }
  else if (scan_keyword("zh"))
  {
    if (direction == dir_tate)
    {
      t = height_depth(char_info(cur_tfont, 0));
      v = char_height(cur_tfont, t) + char_depth(cur_tfont, t);
    }
    else
    {
      t = height_depth(char_info(cur_jfont, 0));
      v = char_height(cur_jfont, t) + char_depth(cur_jfont, t);
    }
  }
  else
    goto not_found;

  {
    get_x_token();

    if (cur_cmd != spacer)
      back_input();
  }

found:
  cur_val = nx_plus_y(save_cur_val, v, xn_over_d(v, f, 65536));
  goto attach_sign;

not_found:
  if (mu)
  {
    if (scan_keyword("mu"))
      goto attach_fraction;
    else
    {
      print_err("Illegal unit of measure (");
      prints("mu inserted)");
      help4("The unit of measurement in math glue must be mu.",
          "To recover gracefully from this error, it's best to",
          "delete the erroneous units; e.g., type `2' to delete",
          "two letters. (See Chapter 27 of The TeXbook.)");
      error();
      goto attach_fraction;
    }
  }

  if (scan_keyword("true"))
  {
    prepare_mag();

    if (mag != 1000)
    {
      cur_val = xn_over_d(cur_val, 1000, mag);
      f = (1000 * f + 0200000 * ng_remainder) / mag;
      cur_val = cur_val + (f / 0200000);
      f = f % 0200000;
    }
  }

  if (scan_keyword("pt"))
    goto attach_fraction;

  if (scan_keyword("in"))
    set_conversion(7227, 100);
  else if (scan_keyword("pc"))
    set_conversion(12, 1);
  else if (scan_keyword("cm"))
    set_conversion(7227, 254);
  else if (scan_keyword("mm"))
    set_conversion(7227, 2540);
  else if (scan_keyword("bp"))
    set_conversion(7227, 7200);
  else if (scan_keyword("dd"))
    set_conversion(1238, 1157);
  else if (scan_keyword("cc"))
    set_conversion(14856, 1157);
  else if (scan_keyword("Q"))
    set_conversion(7227, 10160);
  else if (scan_keyword("H"))
    set_conversion(7227, 10160);
  else if (scan_keyword("twip"))
    set_conversion(1, 20);
  else if (scan_keyword("sp"))
    goto done;
  else
  {
    print_err("Illegal unit of measure (");
    prints("pt inserted)");
    help6("Dimensions can be in units of em, ex, zw, zh, in, pt, pc,",
      "cm, mm, dd, cc, bp, H, Q, twip, or sp; but yours is a new one!",
      "I'll assume that you meant to say pt, for printer's points.",
      "To recover gracefully from this error, it's best to",
      "delete the erroneous units; e.g., type `2' to delete",
      "two letters. (See Chapter 27 of The TeXbook.)");
    error();
    goto done2;
  }

  cur_val = xn_over_d(cur_val, num, denom);
  f = (num * f + 0200000 * ng_remainder) / denom;
  cur_val = cur_val + (f / 0200000);
  f = f % 0200000;

done2:
attach_fraction:
  if (cur_val >= 040000)
    arith_error = true;
  else
    cur_val = cur_val * unity + f;

done:
  {
    get_x_token();

    if (cur_cmd != spacer)
      back_input();
  }

attach_sign:
  if (arith_error || (abs(cur_val) >= 010000000000))
  {
    print_err("Dimension too large");
    help2("I can't work with sizes bigger than about 19 feet.",
        "Continue and I'll use the largest value I can.");
    error();
    cur_val = max_dimen;
    arith_error = false;
  }

  if (negative)
    negate(cur_val);
}

// sets |cur_val| to a glue spec pointer
void scan_glue (small_number level)
{
  boolean negative;
  pointer q;
  boolean mu;

  mu = (level == mu_val);
  negative = false;

  do {
    do {
      get_x_token();
    } while (!(cur_cmd != spacer));

    if (cur_tok == other_token + '-')
    {
      negative = !negative;
      cur_tok = other_token + '+';
    }
  } while (!(cur_tok != other_token + '+'));

  if ((cur_cmd >= min_internal) && (cur_cmd <= max_internal))
  {
    scan_something_internal(level, negative);

    if (cur_val_level >= glue_val)
    {
      if (cur_val_level != level)
        mu_error();

      return;
    }

    if (cur_val_level == int_val)
      scan_dimen(mu, false, true);
    else if (level == mu_val)
      mu_error();
  }
  else
  {
    back_input();
    scan_dimen(mu, false, false);

    if (negative)
      negate(cur_val);
  }

  q = new_spec(zero_glue);
  width(q) = cur_val;

  if (scan_keyword("plus"))
  {
    scan_dimen(mu, true, false);
    stretch(q) = cur_val;
    stretch_order(q) = cur_order;
  }

  if (scan_keyword("minus"))
  {
    scan_dimen(mu, true, false);
    shrink(q) = cur_val;
    shrink_order(q) = cur_order;
  }

  cur_val = q;
}

#define default_rule 26214 // 0.4pt

static pointer scan_rule_spec (void)
{
  pointer q;

  q = new_rule();

  if (cur_cmd == vrule)
    width(q) = default_rule;
  else
  {
    height(q) = default_rule;
    depth(q) = 0;
  }

reswitch:
  if (scan_keyword("width"))
  {
    scan_normal_dimen();
    width(q) = cur_val;
    goto reswitch;
  }

  if (scan_keyword("height"))
  {
    scan_normal_dimen();
    height(q) = cur_val;
    goto reswitch;
  }

  if (scan_keyword("depth"))
  {
    scan_normal_dimen();
    depth(q) = cur_val;
    goto reswitch;
  }

  return q;
}

// changes the string |str_pool[b..pool_ptr]| to a token list
static pointer str_toks_cat (pool_pointer b, uint32_t cat)
{
  pointer p;
  pointer q;
  halfword t;
  pool_pointer k;
  int cc;

  str_room(1);
  p = temp_head;
  link(p) = null;
  k = b;

  while (k < pool_ptr)
  {
    t = fromBUFF(str_pool, pool_ptr, k);
    cc = kcat_code(kcatcodekey(t));

    if ((multistrlen(str_pool, pool_ptr, k) > 1) && ((cat >= kanji) || check_kcat_code(cc)))
    {
      if (cat >= kanji)
        cc = cat;
      else if (cc == not_cjk)
        cc = other_kchar;

      t = t + cc * max_cjk_val;
      k = k + multistrlen(str_pool, pool_ptr, k) - 1;
    }
    else
    {
      t = str_pool[k];

      if ((t == ' ') && (cat == 0))
        t = space_token;
      else if ((cat == 0) || (cat >= kanji))
        t = other_token + t;
      else if (cat == active_char)
        t = cs_token_flag + active_base + t;
      else
        t = left_brace_token * cat + t;
    }

    fast_store_new_token(t);
    incr(k);
  }

  pool_ptr = b;

  return p;
}

static pointer str_toks (pool_pointer b)
{
  return str_toks_cat(b, 0);
}

static pointer the_toks (void)
{
  char old_setting; // {holds |selector| setting}
  pointer p, q, r;  // {used for copying a token list}
  pool_pointer b; // {base of temporary string}
  small_number c; // {value of |cur_chr|}

  // @<Handle \.{\\unexpanded} or \.{\\detokenize} and |return|@>
  if (odd(cur_chr))
  {
    c = cur_chr;
    scan_general_text();

    if (c == 1)
      return cur_val;
    else
    {
      old_setting = selector;
      selector = new_string;
      b = pool_ptr;
      p = get_avail();
      link(p) = link(temp_head);
      token_show(p);
      flush_list(p);
      selector = old_setting;
     
      return str_toks(b);
    }
  }

  get_x_token();
  scan_something_internal(tok_val, false);

  if (cur_val_level >= ident_val)
  {
    // @<Copy the token list@>
    p = temp_head;
    link(p) = null;

    if (cur_val_level == ident_val)
      store_new_token(cs_token_flag + cur_val);
    else if (cur_val != null)
    {
      r = link(cur_val);  // {do not copy the reference count}

      while (r != null)
      {
        fast_store_new_token(info(r));
        r = link(r);
      }
    }

    return p;
  }
  else
  {
    old_setting = selector;
    selector = new_string;
    b = pool_ptr;

    switch (cur_val_level)
    {
      case int_val:
        print_int(cur_val);
        break;

      case dimen_val:
        {
          print_scaled(cur_val);
          prints("pt");
        }
        break;

      case glue_val:
        {
          print_spec(cur_val, "pt");
          delete_glue_ref(cur_val);
        }
        break;

      case mu_val:
        {
          print_spec(cur_val, "mu");
          delete_glue_ref(cur_val);
        }
        break;
    }

    selector = old_setting;
    return str_toks(b);
  }
}

void ins_the_toks (void) 
{ 
  link(garbage) = the_toks();
  ins_list(link(temp_head));
}

#define save_cur_string()           \
do {                                \
  if (str_start[str_ptr] < pool_ptr)\
    u = make_string();              \
  else                              \
    u = 0;                          \
} while (0)

#define restore_cur_string()  \
do {                          \
  if (u != 0)                 \
    decr(str_ptr);            \
} while (0)

#define scan_pdf_ext_toks() \
do {                        \
  scan_toks(false, true);   \
} while (0)


static char * aptex_find_file (str_number s)
{
  char * file_name_kpse = NULL;
  char * file_name_mbcs = NULL;
  char * file_name_utf8 = NULL;

  file_name_utf8 = take_str_string(s);

  if (file_name_utf8 != NULL)
  {
    file_name_mbcs = utf8_mbcs(file_name_utf8);

    if (file_name_mbcs != NULL)
    {
#ifdef USE_KPATHSEA
      file_name_kpse = kpse_find_tex((const_string) file_name_mbcs);
#else
      file_name_kpse = file_name_mbcs;
#endif
      free(file_name_mbcs);
    }

    free(file_name_utf8);
  }

  return file_name_kpse;
}

static void get_creation_date (void)
{
  size_t date_len;
  char * date_str;

  date_str = aptex_utils_get_creation_date();
  date_len = strlen(date_str);
  str_room(date_len);
  memcpy(&str_pool[pool_ptr], date_str, date_len);
  pool_ptr += date_len;
}

static void get_file_mod_date (str_number s)
{
  char * file_name;
  char * file_mod_date;
  size_t file_mod_date_len;

  file_name = aptex_find_file(s);

  if (file_name != NULL)
  {
    file_mod_date = aptex_utils_get_file_mod_date(file_name);

    if (file_mod_date != NULL)
    {
      file_mod_date_len = strlen(file_mod_date);
      str_room(file_mod_date_len);
      memcpy(&str_pool[pool_ptr], file_mod_date, file_mod_date_len);
      pool_ptr += file_mod_date_len;
    }

    free(file_name);
  }
}

static void get_file_size (str_number s)
{
  char * file_name;
  char * file_size_str;
  size_t file_size_str_len;

  file_name = aptex_find_file(s);

  if (file_name != NULL)
  {
    file_size_str = aptex_utils_get_file_size(file_name);

    if (file_size_str != NULL)
    {
      file_size_str_len = strlen(file_size_str);
      str_room(file_size_str_len);
      memcpy(&str_pool[pool_ptr], file_size_str, file_size_str_len);
      pool_ptr += file_size_str_len;
      free(file_size_str);
    }

    free(file_name);
  }
}

static void get_md5_sum (str_number s, boolean f)
{
  char * file_name;
  char * file_md5_sum;
  size_t file_md5_sum_len;

  if (f == 1)
    file_name = aptex_find_file(s);
  else
    file_name = take_str_string(s);

  if (file_name != NULL)
  {
    file_md5_sum = aptex_utils_get_md5_sum(file_name, f);

    if (file_md5_sum != NULL)
    {
      file_md5_sum_len = strlen(file_md5_sum);
      str_room(file_md5_sum_len);
      memcpy(&str_pool[pool_ptr], file_md5_sum, file_md5_sum_len);
      pool_ptr += file_md5_sum_len;
      free(file_md5_sum);
    }

    free(file_name);
  }
}

static void get_file_dump (str_number s, integer i, integer j)
{
  char * file_name;
  char * file_dump;
  size_t file_dump_len;

  file_name = aptex_find_file(s);

  if (file_name != NULL)
  {
    file_dump = aptex_utils_get_file_dump(file_name, i, j);

    if (file_dump != NULL)
    {
      file_dump_len = strlen(file_dump);
      str_room(file_dump_len);
      memcpy(&str_pool[pool_ptr], file_dump, file_dump_len);
      pool_ptr += file_dump_len;
      free(file_dump);
    }

    free(file_name);
  }
}

void conv_toks (void)
{
  char old_setting;
  KANJI_code cx;
  char c;
  small_number save_scanner_status;
  pointer save_def_ref;
  pointer save_warning_index;
  boolean boolvar;
  str_number u;
  str_number s;
  integer i;
  integer j;
  uint32_t cat;
  pool_pointer b;

  cat = 0;
  c = cur_chr;
  KANJI(cx) = 0;

  switch (c)
  {
    case number_code:
    case roman_numeral_code:
    case kansuji_code:
    case euc_code:
    case sjis_code:
    case jis_code:
    case kuten_code:
    case ucs_code:
    case toucs_code:
    case tojis_code:
      scan_int();
      break;

    case ptex_font_name_code:
      scan_font_ident();
      break;

    case ptex_revision_code:
    case uptex_revision_code:
      do_nothing();
      break;

    case string_code:
    case meaning_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_token();

        if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
          KANJI(cx) = cur_tok;

        scanner_status = save_scanner_status;
      }
      break;

    case font_name_code:
      scan_font_ident();
      break;

    case eTeX_revision_code:
      do_nothing();
      break;

    case ng_strcmp_code:
      {
        save_scanner_status = scanner_status;
        save_def_ref = def_ref;
        save_cur_string();
        compare_strings();
        def_ref = save_def_ref;
        scanner_status = save_scanner_status;
        restore_cur_string();
      }
      break;

    case ng_banner_code:
      do_nothing();
      break;

    case ng_os_type_code:
      do_nothing();
      break;

    case pdf_creation_date_code:
      {
        b = pool_ptr;
        get_creation_date();
        link(garbage) = str_toks(b);
        ins_list(link(temp_head));
        return;
      }
      break;

    case pdf_file_mod_date_code:
      {
        save_scanner_status = scanner_status;
        save_warning_index = warning_index;
        save_def_ref = def_ref;
        save_cur_string();
        scan_pdf_ext_toks();
        s = tokens_to_string(def_ref);
        delete_token_ref(def_ref);
        def_ref = save_def_ref;
        warning_index = save_warning_index;
        scanner_status = save_scanner_status;
        b = pool_ptr;
        get_file_mod_date(s);
        link(garbage) = str_toks(b);
        flush_str(s);
        ins_list(link(temp_head));
        restore_cur_string();
        return;
      }
      break;

    case pdf_file_size_code:
      {
        save_scanner_status = scanner_status;
        save_warning_index = warning_index;
        save_def_ref = def_ref;
        save_cur_string();
        scan_pdf_ext_toks();
        s = tokens_to_string(def_ref);
        delete_token_ref(def_ref);
        def_ref = save_def_ref;
        warning_index = save_warning_index;
        scanner_status = save_scanner_status;
        b = pool_ptr;
        get_file_size(s);
        link(garbage) = str_toks(b);
        flush_str(s);
        ins_list(link(temp_head));
        restore_cur_string();
        return;
      }
      break;

    case pdf_mdfive_sum_code:
      {
        save_scanner_status = scanner_status;
        save_warning_index = warning_index;
        save_def_ref = def_ref;
        save_cur_string();
        boolvar = scan_keyword("file");
        scan_pdf_ext_toks();

        if (boolvar)
          s = tokens_to_string(def_ref);
        else
        {
          is_print_utf8 = true;
          s = tokens_to_string(def_ref);
          is_print_utf8 = false;
        }

        delete_token_ref(def_ref);
        def_ref = save_def_ref;
        warning_index = save_warning_index;
        scanner_status = save_scanner_status;
        b = pool_ptr;
        get_md5_sum(s, boolvar);
        link(garbage) = str_toks(b);
        flush_str(s);
        ins_list(link(temp_head));
        restore_cur_string();
        return;
      }
      break;

    case pdf_file_dump_code:
      {
        save_scanner_status = scanner_status;
        save_warning_index = warning_index;
        save_def_ref = def_ref;
        save_cur_string();

        // {scan offset}
        cur_val = 0;

        if (scan_keyword("offset"))
        {
          scan_int();

          if (cur_val < 0)
          {
            print_err("Bad file offset");
            help2("A file offset must be between 0 and 2^{31}-1,", "I changed this one to zero.");
            int_error(cur_val);
            cur_val = 0;
          }
        }

        i = cur_val;

        // {scan length}
        cur_val = 0;

        if (scan_keyword("length"))
        {
          scan_int();

          if (cur_val < 0)
          {
            print_err("Bad dump length");
            help2("A dump length must be between 0 and 2^{31}-1,", "I changed this one to zero.");
            int_error(cur_val);
            cur_val = 0;
          }
        }

        j = cur_val;

        //{scan file name}
        scan_pdf_ext_toks();
        s = tokens_to_string(def_ref);
        delete_token_ref(def_ref);
        def_ref = save_def_ref;
        warning_index = save_warning_index;
        scanner_status = save_scanner_status;
        b = pool_ptr;
        get_file_dump(s, i, j);
        link(garbage) = str_toks(b);
        flush_str(s);
        ins_list(link(temp_head));
        restore_cur_string();
        return;
      }
      break;

    case pdf_uniform_deviate_code:
      scan_int();
      break;

    case pdf_normal_deviate_code:
      do_nothing();
      break;

    case expanded_code:
      {
        save_scanner_status = scanner_status;
        save_warning_index = warning_index;
        save_def_ref = def_ref;
        save_cur_string();
        scan_pdf_ext_toks();
        warning_index = save_warning_index;
        scanner_status = save_scanner_status;
        ins_list(link(def_ref));
        def_ref = save_def_ref;
        restore_cur_string();
        return;
      }
      break;

    case Uchar_convert_code:
      {
        scan_char_num();

        if (!is_char_ascii(cur_val))
          if (kcat_code(kcatcodekey(cur_val)) == not_cjk)
            cat = other_kchar;
      }
      break;

    case Ucharcat_convert_code:
      {
        scan_char_num();
        i = cur_val;
        scan_int();

        if (i <= 0x7F) //{ no |wchar_token| }
        {
          if (illegal_Ucharcat_ascii_catcode(cur_val))
          {
            print_err("Invalid code (");
            print_int(cur_val);
            prints("), should be in the ranges 1..4, 6..8, 10..13");
            help1("I'm going to use 12 instead of that illegal code value.");
            error();
            cat = 12;
          }
          else
            cat = cur_val;
        }
        else if (i <= 0xFF)
        {
          if ((illegal_Ucharcat_ascii_catcode(cur_val))
            && (illegal_Ucharcat_wchar_catcode(cur_val)))
          {
            print_err("Invalid code (");
            print_int(cur_val);
            prints("), should be in the ranges 1..4, 6..8, 10..13, 16..19");
            help1("I'm going to use 12 instead of that illegal code value.");
            error();
            cat = 12;
          }
          else
            cat = cur_val;
        }
        else //{ |wchar_token| only }
        {
          if (illegal_Ucharcat_wchar_catcode(cur_val))
          {
            print_err("Invalid code (");
            print_int(cur_val);
            prints("), should be in the ranges 16..19");
            help1("I'm going to use 18 instead of that illegal code value.");
            error();
            cat = other_kchar;
          }
          else
            cat = cur_val;
        }

        cur_val = i;
      }
      break;

    case job_name_code:
      if (job_name == 0)
        open_log_file();
      break;
  }

  old_setting = selector;
  selector = new_string;
  b = pool_ptr;

  switch (c)
  {
    case number_code:
      print_int(cur_val);
      break;

    case roman_numeral_code:
      print_roman_int(cur_val);
      break;

    case jis_code:
      {
        cur_val = fromJIS(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case euc_code:
      {
        cur_val = fromEUC(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case sjis_code:
      {
        cur_val = fromSJIS(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case kuten_code:
      {
        cur_val = fromKUTEN(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case ptex_revision_code:
      prints(pTeX_revision);
      break;

    case uptex_revision_code:
      prints(upTeX_revision);
      break;

    case ucs_code:
      if (is_internalUPTEX())
        print_int(fromUCS(cur_val));
      else
      {
        cur_val = fromUCS(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case toucs_code:
      if (is_internalUPTEX())
        print_int(toUCS(cur_val));
      else
      {
        cur_val = toUCS(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case kansuji_code:
      print_kansuji(cur_val);
      break;

    case tojis_code:
      {
        cur_val = toJIS(cur_val);
        if (cur_val == 0)
          print_int(-1);
        else
          print_int(cur_val);
      }
      break;

    case ptex_font_name_code:
      {
        print_font_name_and_size(cur_val);
        print_font_dir_and_enc(cur_val);
      }
      break;

    case string_code:
      if (cur_cs != 0)
        sprint_cs(cur_cs);
      else if (KANJI(cx) == 0)
        print_char(cur_chr);
      else
        print_kanji(cx);
      break;

    case meaning_code:
      print_meaning();
      break;

    case font_name_code:
      {
        print(font_name[cur_val]);

        if (font_size[cur_val] != font_dsize[cur_val])
        {
          prints(" at ");
          print_scaled(font_size[cur_val]);
          prints("pt");
        }
      }
      break;

    case eTeX_revision_code:
      prints(eTeX_revision);
      break;

    case ng_strcmp_code:
      print_int(cur_val);
      break;

    case ng_banner_code:
      prints(banner);
      break;

    case ng_os_type_code:
      prints(dist);
      break;

    case pdf_uniform_deviate_code:
      print_int(unif_rand(cur_val));
      break;

    case pdf_normal_deviate_code:
      print_int(norm_rand());
      break;

    case Uchar_convert_code:
      if (is_char_ascii(cur_val))
        print_char(cur_val);
      else
        print_kanji(cur_val);
      break;

    case Ucharcat_convert_code:
      if (cat < kanji)
        print_char(cur_val);
      else
        print_kanji(cur_val);
      break;

    case job_name_code:
      print(job_name);
      break;
  }

  selector = old_setting;
  link(garbage) = str_toks_cat(b, cat);
  ins_list(link(temp_head));
}

pointer scan_toks (boolean macro_def, boolean xpand)
{
  halfword t; // {token representing the highest parameter number}
  halfword s; // {saved token}
  pointer p;  // {tail of the token list being built}
  pointer q;  // {new node being added to the token list via |store_new_token|}
  halfword unbalance;   // {number of unmatched left braces}
  halfword hash_brace;  // {possible `\.{\#\{}' token}

  if (macro_def)
    scanner_status = defining;
  else
    scanner_status = absorbing;

  warning_index = cur_cs;
  def_ref = get_avail();
  token_ref_count(def_ref) = null;
  p = def_ref;
  hash_brace = 0;
  t = zero_token;

  if (macro_def)
  {
    // @<Scan and build the parameter part of the macro definition@>
    while (true)
    {
continu:
      get_token();  // {set |cur_cmd|, |cur_chr|, |cur_tok|}

      if (cur_tok < right_brace_limit)
        goto done1;

      if (cur_cmd == mac_param)
      {
        /*
          @<If the next character is a parameter number, make |cur_tok|
          a |match| token; but if it is a left brace, store
          `|left_brace|, |end_match|', set |hash_brace|, and |goto done|@>
        */
        s = match_token + cur_chr;
        get_token();

        if (cur_tok < left_brace_limit)
        {
          hash_brace = cur_tok;
          store_new_token(cur_tok);
          store_new_token(end_match_token);
          goto done;
        }

        if (t == zero_token + 9)
        {
          print_err("You already have nine parameters");
          help2("I'm going to ignore the # sign you just used.",
            "as well as the token that followed it.");
          error();
          goto continu;
        }
        else
        {
          incr(t);

          if (cur_tok != t)
          {
            print_err("Parameters must be numbered consecutively");
            help2("I've inserted the digit you should have used after the #.",
                "Type `1' to delete what you did use.");
            back_error();
          }

          cur_tok = s;
        }
      }

      store_new_token(cur_tok);
    }

done1:
    store_new_token(end_match_token);

    if (cur_cmd == right_brace)
    {
      // @<Express shock at the missing left brace; |goto found|@>
      print_err("Missing { inserted");
      incr(align_state);
      help2("Where was the left brace? You said something like `\\def\\a}',",
          "which I'm going to interpret as `\\def\\a{}'.");
      error();
      goto found;
    }
done:;
  }
  else
    scan_left_brace();  // {remove the compulsory left brace}

  // @<Scan and build the body of the token list; |goto found| when finished@>
  unbalance = 1;

  while (true)
  {
    if (xpand)
    {
      // @<Expand the next part of the input@>
      while (true)
      {
        get_next();

        if (cur_cmd >= call)
        {
          if (info(link(cur_chr)) == protected_token)
          {
            cur_cmd = relax;
            cur_chr = no_expand_flag;
          }
        }

        if (cur_cmd <= max_command)
          goto done2;

        if (cur_cmd != the)
          expand();
        else
        {
          q = the_toks();

          if (link(temp_head) != null)
          {
            link(p) = link(temp_head);
            p = q;
          }
        }
      }

done2:
      x_token();
    }
    else
      get_token();

    if (cur_tok < right_brace_limit)
    {
      if (cur_cmd < right_brace)
        incr(unbalance);
      else
      {
        decr(unbalance);

        if (unbalance == 0)
          goto found;
      }
    }
    else if (cur_cmd == mac_param)
    {
      if (macro_def)
      {
        // @<Look for parameter number or \.{\#\#}@>
        s = cur_tok;

        if (xpand)
          get_x_token();
        else
          get_token();

        if (cur_cmd != mac_param)
        {
          if ((cur_tok <= zero_token) || (cur_tok > t))
          {
            print_err("Illegal parameter number in definition of ");
            sprint_cs(warning_index);
            help3("You meant to type ## instead of #, right?",
                "Or maybe a } was forgotten somewhere earlier, and things",
                "are all screwed up? I'm going to assume that you meant ##.");
            back_error();
            cur_tok = s;
          }
          else
            cur_tok = out_param_token - '0' + cur_chr;
        }
      }
    }

    store_new_token(cur_tok);
  }

found:
  scanner_status = normal;

  if (hash_brace != 0)
    store_new_token(hash_brace);

  return p;
}

void read_toks (integer n, pointer r, halfword j)
{
  pointer p;  // {tail of the token list}
  pointer q;  // {new node being added to the token list via |store_new_token|}
  integer s;  // {saved value of |align_state|}
  /* small_number m; */
  int m;  // {stream number}

  scanner_status = defining;
  warning_index = r;
  def_ref = get_avail();
  token_ref_count(def_ref) = null;
  p = def_ref;  // {the reference count}
  store_new_token(end_match_token);

  if ((n < 0) || (n > 15))
    m = 16;
  else
    m = n;

  s = align_state;
  align_state = 1000000;  // {disable tab marks, etc.}

  do {
    // @<Input and store tokens from the next line of the file@>
    begin_file_reading();
    name = m + 1;

    if (read_open[m] == closed)
    {
      // @<Input for \.{\\read} from the terminal@>
      if (interaction > nonstop_mode)
      {
        if (n < 0)
          prompt_input("");
        else
        {
          wake_up_terminal();
          print_ln();
          sprint_cs(r);
          prompt_input("=");
          n = -1;
        }
      }
      else
        fatal_error("*** (cannot \\read from terminal in nonstop modes)");
    }
    else if (read_open[m] == just_open)
    {
      // @<Input the first line of |read_file[m]|@>
      if (input_ln(read_file[m], false))
        read_open[m] = normal;
      else
      {
        a_close(read_file[m]);
        read_open[m] = closed;
      }
    }
    else
    {
      // @<Input the next line of |read_file[m]|@>
      if (!input_ln(read_file[m], true))
      {
        a_close(read_file[m]);
        read_open[m] = closed;

        if (align_state != 1000000)
        {
          runaway();
          print_err("File ended within ");
          print_esc("read");
          help1("This \\read has unbalanced braces.");
          align_state = 1000000;
          limit = 0;
          error();
        }
      }
    }

    limit = last;

    if (end_line_char_inactive())
      decr(limit);
    else
      buffer[limit] = end_line_char;

    first = limit + 1;
    loc = start;
    state = new_line;

    // @<Handle \.{\\readline} and |goto done|@>
    if (j == 1)
    {
      while (loc <= limit)  // {current line not yet finished}
      {
        cur_chr = fromBUFF(buffer, limit + 1, loc);
        cur_tok = kcat_code(kcatcodekey(cur_chr));

        if ((multistrlen(buffer, limit + 1, loc) > 1) && check_kcat_code(cur_tok))
        {
          if (cur_tok == not_cjk)
            cur_tok = other_kchar;

          cur_tok = cur_chr + cur_tok * max_cjk_val;
          loc = loc + multistrlen(buffer, limit + 1, loc);
        }
        else
        {
          cur_chr = buffer[loc];
          incr(loc);

          if (cur_chr == ' ')
            cur_tok = space_token;
          else
            cur_tok = cur_chr + other_token;
        }

        store_new_token(cur_tok);
      }

      goto done;
    }

    while (true)
    {
      get_token();

      if (cur_tok == 0)
        goto done;
      // {|cur_cmd=cur_chr=0| will occur at the end of the line}

      if (align_state < 1000000)  // {unmatched `\.\}' aborts the line}
      {
        do {
          get_token();
        } while (!(cur_tok == 0));

        align_state = 1000000;
        goto done;
      }

      store_new_token(cur_tok);
    }

done:
    end_file_reading();
  } while (!(align_state == 1000000));

  cur_val = def_ref;
  scanner_status = normal;
  align_state = s;
}

void pass_text (void)
{
  integer l;  // {level of $\.{\\if}\ldots\.{\\fi}$ nesting}
  small_number save_scanner_status; // {|scanner_status| upon entry}

  save_scanner_status = scanner_status;
  scanner_status = skipping;
  l = 0;
  skip_line = line;

  while (true)
  {
    get_next();

    if (cur_cmd == fi_or_else)
    {
      if (l == 0)
        goto done;

      if (cur_chr == fi_code)
        decr(l);
    }
    else if (cur_cmd == if_test)
      incr(l);
  }

done:
  scanner_status = save_scanner_status;

  if (tracing_ifs > 0)
    show_cur_cmd_chr();
}

static void change_if_limit (small_number l, pointer p)
{
  pointer q;

  if (p == cond_ptr)
    if_limit = l; // {that's the easy case}
  else
  {
    q = cond_ptr;

    while (true)
    {
      if (q == null)
        confusion("if");

      if (link(q) == p)
      {
        type(q) = l;
        return;
      }

      q = link(q);
    }
  }
}

void conditional (void)
{
  boolean b; // {is the condition true?}
  boolean e; // {keep track of nested csnames}
  char r; // {relation to be evaluated}
  integer m, n; // {to be tested against the second operand}
  pointer p, q; // {for traversing token lists in \.{\\ifx} tests}
  small_number save_scanner_status; // {|scanner_status| upon entry}
  pointer save_cond_ptr;  // {|cond_ptr| corresponding to this conditional}
  small_number this_if; // {type of this conditional}
  boolean is_unless;  // {was this if preceded by `\.{\\unless}' ?}

  if (tracing_ifs > 0)
  {
    if (tracing_commands <= 1)
      show_cur_cmd_chr();
  }

  {
    p = get_node(if_node_size);
    link(p) = cond_ptr;
    type(p) = if_limit;
    subtype(p) = cur_if;
    if_line_field(p) = if_line;
    cond_ptr = p;
    cur_if = cur_chr;
    if_limit = if_code;
    if_line = line;
  }

  save_cond_ptr = cond_ptr;
  is_unless = (cur_chr >= unless_code);
  this_if = cur_chr % unless_code;

  switch (this_if)
  {
    case if_char_code:
    case if_cat_code:
      {
        get_x_token_or_active_char();

        if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
        {
          m = cur_cmd;
          n = cur_chr;
        }
        else if ((cur_cmd > active_char) || (cur_chr > 255))
        {
          m = relax;
          n = max_cjk_val;
        }
        else
        {
          m = cur_cmd;
          n = cur_chr;
        }

        get_x_token_or_active_char();

        if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
        {
          cur_cmd = cur_cmd;
        }
        else if ((cur_cmd > active_char) || (cur_chr > 255))
        {
          cur_cmd = relax;
          cur_chr = max_cjk_val;
        }

        if (this_if == if_char_code)
          b = (n == cur_chr); 
        else
          b = (m == cur_cmd);
      }
      break;

    case if_int_code:
    case if_dim_code:
      {
        if (this_if == if_int_code)
          scan_int();
        else
          scan_normal_dimen();

        n = cur_val;

        do {
          get_x_token();
        } while (!(cur_cmd != spacer));

        if ((cur_tok >= other_token + '<') && (cur_tok <= other_token + '>'))
          r = cur_tok - other_token;
        else
        {
          print_err("Missing = inserted for ");
          print_cmd_chr(if_test, this_if);
          help1("I was expecting to see `<', `=', or `>'. Didn't.");
          back_error();
          r = '=';
        }

        if (this_if == if_int_code)
          scan_int();
        else 
          scan_normal_dimen();

        switch (r)
        {
          case '<':
            b = (n < cur_val);
            break;

          case '=':
            b = (n == cur_val);
            break;

          case '>':
            b = (n > cur_val);
            break;
        }
      }
      break;

    case if_odd_code:
      {
        scan_int();
        b = odd(cur_val);
      }
      break;

    case if_vmode_code:
      b = (abs(mode) == vmode);
      break;

    case if_hmode_code:
      b = (abs(mode) == hmode);
      break;

    case if_mmode_code:
      b = (abs(mode) == mmode);
      break;

    case if_inner_code:
      b = (mode < 0);
      break;

    case if_tdir_code:
      b = (abs(direction) == dir_tate);
      break;

    case if_ydir_code:
      b = (abs(direction) == dir_yoko);
      break;

    case if_ddir_code:
      b = (abs(direction) == dir_dtou);
      break;

    case if_mdir_code:
      b = (direction < 0);
      break;

    case if_tbox_code:
    case if_ybox_code:
    case if_dbox_code:
    case if_mbox_code:
    case if_void_code:
    case if_hbox_code:
    case if_vbox_code:
      {
        scan_register_num();
        fetch_box(p);

        if (this_if == if_void_code)
          b = (p == 0);
        else if (p == 0)
          b = false;
        else
        {
          if (type(p) == dir_node)
            p = list_ptr(p);

          if (this_if == if_hbox_code)
            b = (type(p) == hlist_node);
          else if (this_if == if_vbox_code)
            b = (type(p) == vlist_node);
          else if (this_if == if_tbox_code)
            b = (abs(box_dir(p)) == dir_tate);
          else if (this_if == if_ybox_code)
            b = (abs(box_dir(p)) == dir_yoko);
          else if (this_if == if_dbox_code)
            b = (abs(box_dir(p)) == dir_dtou);
          else
            b = (box_dir(p) < 0);
        }
      }
      break;

    case if_pdfprimitive_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_next();
        scanner_status = save_scanner_status;

        if (cur_cs < hash_base)
          m = prim_lookup(cur_cs - single_base);
        else
          m = prim_lookup(text(cur_cs));

        b = ((cur_cmd != undefined_cs) &&
             (m != undefined_primitive) &&
             (cur_cmd == prim_eq_type(m)) &&
             (cur_chr == prim_equiv(m)));
      }
      break;

    case if_jfont_code:
    case if_tfont_code:
      {
        scan_font_ident();

        if (this_if == if_jfont_code)
          b = (font_dir[cur_val] == dir_yoko);
        else if (this_if == if_tfont_code)
          b = (font_dir[cur_val] == dir_tate);
      }
      break;

    case ifx_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_next();
        n = cur_cs;
        p = cur_cmd;
        q = cur_chr;
        get_next();

        if (cur_cmd != p)
          b = false;
        else if (cur_cmd < call)
          b = (cur_chr == q);
        else
        {
          p = link(cur_chr);
          q = link(equiv(n));

          if (p == q)
            b = true;
          else
          {
            while ((p != 0) && (q != 0))
            {
              if (info(p) != info(q))
                p = 0;
              else
              {
                p = link(p);
                q = link(q);
              }
            }

            b = ((p == 0) && (q == 0));
          }
        }

        scanner_status = save_scanner_status;
      }
      break;

    case if_eof_code:
      {
        scan_four_bit_int_or_18();
        if (cur_val == 18)
          b = false;
        else
          b = (read_open[cur_val] == closed);
      }
      break;

    case if_true_code:
      b = true;
      break;

    case if_false_code:
      b = false;
      break;

    case if_def_code:
      {
        save_scanner_status = scanner_status;
        scanner_status = normal;
        get_next();
        b = (cur_cmd != undefined_cs);
        scanner_status = save_scanner_status;
      }
      break;

    case if_cs_code:
      {
        n = get_avail();
        p = n;
        e = is_in_csname;
        is_in_csname = true;

        do {
          get_x_token();

          if (cur_cs == 0)
            store_new_token(cur_tok);
        } while (!(cur_cs != 0));

        if (cur_cmd != end_cs_name)
        {
          print_err("Missing ");
          print_esc("endcsname");
          prints(" inserted");
          help2("The control sequence marked <to be read again> should",
            "not appear between \\csname and \\endcsname.");
          back_error();
        }

        m = first;
        p = link(n);

        while (p != null)
        {
          if (m >= max_buf_stack)
          {
            max_buf_stack = m + 1;

#ifdef APTEX_EXTENSION
            if (max_buf_stack == current_buf_size)
              buffer = realloc_buffer(increment_buf_size);

            if (max_buf_stack == current_buf_size)
              overflow("buffer size", current_buf_size);
#else
            if (max_buf_stack == buf_size)
              overflow("buffer size", buf_size);
#endif
          }

          if (check_kanji(info(p)))
          {
            if (BYTE1(toBUFF(info(p) % max_cjk_val)) != 0)
            {
              buffer[m] = BYTE1(toBUFF(info(p) % max_cjk_val));
              incr(m);
            }

            if (BYTE2(toBUFF(info(p) % max_cjk_val)) != 0)
            {
              buffer[m] = BYTE2(toBUFF(info(p) % max_cjk_val));
              incr(m);
            }

            if (BYTE3(toBUFF(info(p) % max_cjk_val)) != 0)
            {
              buffer[m] = BYTE3(toBUFF(info(p) % max_cjk_val));
              incr(m);
            }

            buffer[m] = BYTE4(toBUFF(info(p) % max_cjk_val));
            incr(m);
            p = link(p);
          }
          else
          {
            buffer[m] = info(p) % max_char_val;
            incr(m);
            p = link(p);
          }
        }

        if (m > first + 1)
          cur_cs = id_lookup(first, m - first);
        else if (m == first)
          cur_cs = null_cs;
        else
          cur_cs = single_base + buffer[first];

        flush_list(n);
        b = (eq_type(cur_cs) != undefined_cs);
        is_in_csname = e;
      }
      break;

    case if_in_csname_code:
      b = is_in_csname;
      break;

    case if_font_char_code:
      {
        scan_font_ident();
        n = cur_val;

        if (font_dir[n] != dir_default)
        {
          scan_int();

          if (cur_val >= 0)
            b = is_char_kanji(cur_val);
          else
          {
            cur_val = -(cur_val + 1);
            b = (font_bc[n] <= cur_val) && (font_ec[n] >= cur_val);
          }
        }
        else
        {
          scan_char_num();

          if ((font_bc[n] <= cur_val) && (font_ec[n] >= cur_val))
            b = char_exists(char_info(n, cur_val));
          else
            b = false;
        }
      }
      break;

    case if_case_code:
      {
        scan_int();
        n = cur_val;

        if (tracing_commands > 1)
        {
          begin_diagnostic();
          prints("{case ");
          print_int(n); 
          print_char('}');
          end_diagnostic(false);
        }

        while (n != 0)
        {
          pass_text();

          if (cond_ptr == save_cond_ptr)
          {
            if (cur_chr == or_code)
              decr(n);
            else 
              goto common_ending;
          }
          else if (cur_chr == fi_code)
          {
            if (if_stack[in_open] == cond_ptr)
              if_warning();

            p = cond_ptr;
            if_line = if_line_field(p);
            cur_if = subtype(p);
            if_limit = type(p);
            cond_ptr = link(p);
            free_node(p, if_node_size);
          }
        }

        change_if_limit(or_code, save_cond_ptr);
        return;
      }
      break;
  }

  if (is_unless)
    b = !b;

  if (tracing_commands > 1)
  {
    begin_diagnostic();

    if (b)
      prints("{true}");
    else
      prints("{false}");

    end_diagnostic(false);
  }

  if (b)
  {
    change_if_limit(else_code, save_cond_ptr);
    return;
  }

  while (true)
  {
    pass_text();

    if (cond_ptr == save_cond_ptr)
    {
      if (cur_chr != or_code)
        goto common_ending;

      print_err("Extra ");
      print_esc("or");
      help1("I'm ignoring this; it doesn't match any \\if.");
      error();
    }
    else if (cur_chr == fi_code)
    {
      if (if_stack[in_open] == cond_ptr)
        if_warning();

      p = cond_ptr;
      if_line = if_line_field(p);
      cur_if = subtype(p);
      if_limit = type(p);
      cond_ptr = link(p);
      free_node(p, if_node_size);
    }
  }

common_ending:
  if (cur_chr == fi_code)
  {
    if (if_stack[in_open] == cond_ptr)
      if_warning();

    p = cond_ptr;
    if_line = if_line_field(p);
    cur_if = subtype(p);
    if_limit = type(p);
    cond_ptr = link(p);
    free_node(p, if_node_size);
  }
  else
    if_limit = fi_code;
}

static void begin_name (void)
{
  area_delimiter = 0;
  ext_delimiter = 0;
  prev_char = 0;
  quoted_file_name = false;
}

static boolean more_name (ASCII_code c)
{
  if (c == ' ' && stop_at_space && !quoted_file_name)
    return false;
  else if (c == '"')
  {
    quoted_file_name = !quoted_file_name; // catch next space character 
    return true;     // accept ending quote, but throw away
  }
  else
  {   
    str_room(1);
    append_char(c);

    //  for DOS/Windows
    if ((c == '/' || c == '\\' || c == ':')) 
    {
      area_delimiter = cur_length;
      ext_delimiter = 0;
    } 
    else if (c == '.')
      ext_delimiter = cur_length;

    return true;
  }
}

static void end_name (void) 
{
#ifdef APTEX_EXTENSION
  if (str_ptr + 3 > current_max_strings)
    str_start = realloc_str_start(increment_max_strings + 3);

  if (str_ptr + 3 > current_max_strings)
    overflow("number of strings", current_max_strings - init_str_ptr);
#else
  if (str_ptr + 3 > max_strings)
    overflow("number of strings", max_strings - init_str_ptr);
#endif

  if (area_delimiter == 0)
    cur_area = 335;
  else
  {
    cur_area = str_ptr;
    str_start[str_ptr + 1] = str_start[str_ptr] + area_delimiter;
    incr(str_ptr);
  }

  if (ext_delimiter == 0)
  {
    cur_ext = 335;
    cur_name = make_string();
  } 
  else
  {
    cur_name = str_ptr;
    str_start[str_ptr + 1] = str_start[str_ptr] + ext_delimiter - area_delimiter - 1;
    incr(str_ptr);
    cur_ext = make_string();
  }
}

void pack_file_name (str_number n, str_number a, str_number e)
{
  integer k;  // {number of positions filled in |name_of_file|}
  ASCII_code c; // {character being packed}
  pool_pointer j; // {index into |str_pool|}

  k = 0;

  for (j = str_start[a]; j <= str_start[a + 1] - 1; j++)
    append_to_name(str_pool[j]);

  for (j = str_start[n]; j <= str_start[n + 1] - 1; j++)
    append_to_name(str_pool[j]);

  for (j = str_start[e]; j <= str_start[e + 1] - 1; j++)
    append_to_name(str_pool[j]);

  if (k < file_name_size)
    name_length = k;
  else
    name_length = file_name_size - 1;

  for (k = name_length + 1; k <= file_name_size; k++)
    name_of_file[k] = ' ';

  name_of_file[file_name_size] = '\0';
}

static void pack_buffered_name (small_number n, integer a, integer b)
{
  integer k;  // {number of positions filled in |name_of_file|}
  ASCII_code c; // {character being packed}
  integer j;  // {index into |buffer| or |TEX_format_default|}

  if (n + b - a + 5 > file_name_size)
    b = a + file_name_size - n - 5;

  k = 0;

  for (j = 1; j <= n; j++)
    append_to_name(xord[TEX_format_default[j]]);

  for (j = a; j <= b; j++)
    append_to_name(buffer[j]);

  for (j = format_default_length - 3; j <= format_default_length; j++)
    append_to_name(xord[TEX_format_default[j]]);

  if (k < file_name_size)
    name_length = k;
  else
    name_length = file_name_size - 1;

  for (k = name_length + 1; k <= file_name_size; k++)
    name_of_file[k]= ' ';

  name_of_file[file_name_size] = '\0';
}

static str_number make_name_string (void)
{
  integer k;

#ifdef APTEX_EXTENSION
  if (pool_ptr + name_length > current_pool_size)
    str_pool = realloc_str_pool(increment_pool_size + name_length);

  if (str_ptr == current_max_strings)
    str_start = realloc_str_start(increment_max_strings);

  if ((pool_ptr + name_length > current_pool_size) || (str_ptr == current_max_strings) || (cur_length > 0))
#else
  if ((pool_ptr + name_length > pool_size) || (str_ptr == max_strings) || (cur_length > 0))
#endif
  {
    return '?';
  }
  else
  {
    for (k = 1; k <= name_length; k++)
      append_char(xord[name_of_file[k]]);

    return make_string();
  }
}

static str_number a_make_name_string (alpha_file f)
{
  (void) f;
  return make_name_string();
}

str_number b_make_name_string (byte_file f)
{
  (void) f;
  return make_name_string(); 
}

str_number w_make_name_string (word_file f)
{
  (void) f;
  return make_name_string();
}

static void scan_file_name_braced (void);

static void scan_file_name (void)
{
  pointer save_warning_index;

  save_warning_index = warning_index;
  warning_index = cur_cs; // {store |cur_cs| here to remember until later}

  do {
    get_x_token();
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

  back_input(); // {return the last token to be read by either code path}

  if (cur_cmd == left_brace)
    scan_file_name_braced();
  else
  {
    name_in_progress = true;
    begin_name();

    do {
      get_x_token(); 
    } while (!(cur_cmd != spacer));

    skip_mode = false;

    while (true)
    {
      if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
      {
        str_room(4);
        cur_chr = toBUFF(cur_chr);

        if (BYTE1(cur_chr) != 0)
          append_char(BYTE1(cur_chr));

        if (BYTE2(cur_chr) != 0)
          append_char(BYTE2(cur_chr));

        if (BYTE3(cur_chr) != 0)
          append_char(BYTE3(cur_chr));

        append_char(BYTE4(cur_chr));
      }
      else if ((cur_cmd > other_char) || (cur_chr > 255)) 
      {
        back_input();
        goto done; 
      }
      else if (((cur_chr == ' ') && (state != token_list) && (loc > limit)) || !more_name(cur_chr))
        goto done;

      get_x_token();
    }
  }

done:
  end_name();
  name_in_progress = false;
  skip_mode = true;
  warning_index = save_warning_index; // {restore |warning_index|}
}

static void scan_file_name_braced (void)
{
  small_number save_scanner_status; // {|scanner_status| upon entry}
  pointer save_def_ref; // {|def_ref| upon entry, important if inside `\.{\\message}}
  pointer save_cur_cs;
  str_number s; // {temp string}
  pointer p; // {temp pointer}
  integer i; // {loop tally}
  boolean save_stop_at_space; // {this should be in tex.ch}
  boolean dummy; //{Initialising}

  save_scanner_status = scanner_status; // {|scan_toks| sets |scanner_status| to |absorbing|}
  save_def_ref = def_ref; // {|scan_toks| uses |def_ref| to point to the token list just read}
  save_cur_cs = cur_cs; // {we set |cur_cs| back a few tokens to use in runaway errors}
  //  {Scanning a token list}
  cur_cs = warning_index; // {for possible runaway error}
  // {mimick |call_func| from pdfTeX}

  if (scan_toks(false, true) != 0)
    do_nothing(); // {actually do the scanning}
 
  // {s := tokens_to_string(def_ref);}
  old_setting = selector;
  selector = new_string;
  show_token_list(link(def_ref), null, pool_size - pool_ptr);
  selector = old_setting;
  s = make_string();
  // {turns the token list read in a string to input}
  //  {Restoring some variables}
  delete_token_ref(def_ref); // {remove the token list from memory}
  def_ref = save_def_ref; // {and restore |def_ref|}
  cur_cs = save_cur_cs; // {restore |cur_cs|}
  scanner_status = save_scanner_status; // {restore |scanner_status|}
  //  {Passing the read string to the input machinery}
  save_stop_at_space = stop_at_space; // {save |stop_at_space|}
  stop_at_space = false; // {set |stop_at_space| to false to allow spaces in file names}
  begin_name();

  for (i = str_start[s]; i <= str_start[s + 1] - 1; i++)
    dummy = more_name(str_pool[i]); // {add each read character to the current file name}

  stop_at_space = save_stop_at_space; // {restore |stop_at_space|}
}

void pack_job_name_ (str_number s)
{
  cur_area = 335; /* "" */
  cur_ext  = s;
  cur_name = job_name;
  pack_cur_name();
}

void prompt_file_name_ (const char * s, str_number e)
{
  uint32_t k;

  if (interaction == scroll_mode)
    wake_up_terminal();

  if (!strcmp("input file name", s))
    print_err("I can't find file `");
  else
    print_err("I can't write on file `");

  print_file_name(cur_name, cur_area, cur_ext);
  prints("'.");

  if (e == 785)    /* .tex */
    show_context();

  print_nl("Please type another ");
  prints(s);

  if (interaction < scroll_mode)
    fatal_error("*** (job aborted, file error in nonstop mode)");

  clear_terminal();
  prompt_input(": ");

  {
    begin_name();
    k = first;

    while ((buffer[k] == ' ') && (k < last))
      incr(k);

    while (true)
    {
      if (k == last)
        goto done;

      if (!more_name(buffer[k]))
        goto done;

      incr(k);
    }

done:
    end_name();
  }

  if (cur_ext == 335) /* "" */
    cur_ext = e;

  pack_cur_name();
}

void open_log_file (void)
{
  char old_setting;
  uint32_t k;
  uint32_t l;
  const char * months;

  old_setting = selector;

  if (job_name == 0)
    job_name = get_job_name(790); // "texput"

  pack_job_name(".log");

  while (!a_open_out(log_file))
  {
    selector = term_only;
    prompt_file_name("transcript file name", ".log");
  }

  log_name = a_make_name_string(log_file);
  selector = log_only;
  log_opened = true;

  {
    write_log("%s", banner);

    if (format_ident > 0)
      slow_print(format_ident);

    prints("  ");
    print_int(sys_day);
    print_char(' ');
    months = " JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC";

    for (k = 3 * sys_month - 2; k <= 3 * sys_month; k++)
      wlog(months[k]);

    print_char(' ');
    print_int(sys_year);
    print_char(' ');
    print_two(sys_time / 60);
    print_char(':');
    print_two(sys_time % 60);

    if (eTeX_ex)
    {
      wlog_cr();
      fputs("entering extended mode", log_file.file_data);
    }
  }

  input_stack[input_ptr] = cur_input;
  print_nl("**");
  l = input_stack[0].limit_field;

  if (buffer[l] == end_line_char)
    decr(l);

  for (k = 1; k <= l; k++)
    print(buffer[k]);

  print_ln();
  selector = old_setting + 2;
}

// \TeX\ will \.{\\input} something
void start_input (void)
{
  pointer v;

  scan_file_name();
  pack_cur_name();

  while (true)
  {
    begin_file_reading();

    if (a_open_in(cur_file))
      goto done;

    end_file_reading();
    prompt_file_name("input file name", ".tex");
  }

done:
  name = a_make_name_string(cur_file);

  if (job_name == 0)
  {
    job_name = get_job_name(cur_name);
    open_log_file();
  }

  if (term_offset + length(name) > max_print_line - 2)
    print_ln();
  else if ((term_offset > 0) || (file_offset > 0))
    print_char(' ');

  print_char('(');
  incr(open_parens);
  slow_print(name);
  update_terminal();

  if (tracing_stack_levels > 0)
  {
    begin_diagnostic();
    print_ln();
    print_char('~');
    v = input_ptr - 1;

    if (v < tracing_stack_levels)
      while (v > 0)
      {
        print_char('.');
        decr(v);
      }
    else
      print_char('~');

    prints_("INPUT "); // slow_print
    slow_print(cur_name);
    slow_print(cur_ext);
    print_ln();
    end_diagnostic(false);
  }

  state = new_line;

  synctex_start_input();

  {
    line = 1;

    if (input_ln(cur_file, false))
      do_nothing();

    firm_up_the_line();

    if (end_line_char_inactive())
      decr(limit);
    else
      buffer[limit] = end_line_char;

    first = limit + 1;
    loc = start;
  }
}

// input a \.{TFM} file
static internal_font_number read_font_info (pointer u, str_number nom, str_number aire, scaled s)
{
  font_index k;
  int jfm_flag;
  halfword nt;
  KANJI_code cx;
  boolean file_opened;
  halfword lf, lh, nw, nh, nd, ni, nl, nk, ne, np;
  int bc, ec;
  internal_font_number f;
  internal_font_number g;
  eight_bits a, b, c, d;
  four_quarters qw;
  scaled sw;
  integer bch_label;
  short bchar;
  scaled z;
  integer alpha;
  char beta;

  g = null_font;
  file_opened = false;
  pack_file_name(nom, aire, 805); /* .tfm */

  if (tracing_fontloaders)
  {
    begin_diagnostic();
    print_nl("Requested font \"");
    print(nom);
    print_char('\"');

    if (s < 0)
    {
      prints(" scaled ");
      print_int(-s);
    }
    else
    {
      prints(" at ");
      print_scaled(s);
      prints("pt");
    }

    end_diagnostic(false);
  }

  if (!b_open_in(tfm_file))
    goto bad_tfm;

  file_opened = true;

  {
    read_sixteen(lf);
    fget();
    read_sixteen(lh);

    if (lf == yoko_jfm_id)
    {
      jfm_flag = dir_yoko;
      nt = lh;
      fget();
      read_sixteen(lf);
      fget();
      read_sixteen(lh);
    }
    else if (lf == tate_jfm_id)
    {
      jfm_flag = dir_tate;
      nt = lh;
      fget();
      read_sixteen(lf);
      fget();
      read_sixteen(lh);
    }
    else
    {
      jfm_flag = dir_default;
      nt = 0;
    }

    fget();
    read_sixteen(bc);
    fget();
    read_sixteen(ec);

    if ((bc > ec + 1) || (ec > 255))
      goto bad_tfm;

    if (bc > 255)
    {
      bc = 1;
      ec = 0;
    }

    fget();
    read_sixteen(nw);
    fget();
    read_sixteen(nh);
    fget();
    read_sixteen(nd);
    fget();
    read_sixteen(ni);
    fget();
    read_sixteen(nl);
    fget();
    read_sixteen(nk);
    fget();
    read_sixteen(ne);
    fget();
    read_sixteen(np);

    if (jfm_flag != dir_default)
    {
      if (lf != 7 + lh + nt + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np)
        goto bad_tfm;
    }
    else
    {
      if (lf != 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np)
        goto bad_tfm;
    }

    if ((nw == 0) || (nh == 0) || (nd == 0) || (ni == 0))
      goto bad_tfm;
  }

  if (jfm_flag != dir_default)
    lf = lf - 7 - lh;
  else
    lf = lf - 6 - lh;

  if (np < 7)
    lf = lf + 7 - np;

#ifdef APTEX_EXTENSION
  if ((fmem_ptr + lf > current_font_mem_size))
    font_info = realloc_font_info(increment_font_mem_size + lf);

  if ((font_ptr == font_max) || (fmem_ptr + lf > current_font_mem_size))
#else
  if ((font_ptr == font_max) || (fmem_ptr + lf > font_mem_size))
#endif
  {
    start_font_error_message();
    prints(" not loaded: Not enough room left");
    help4("I'm afraid I won't be able to make use of this font,",
        "because my memory for character-size data is too small.",
        "If you're really stuck, ask a wizard to enlarge me.",
        "Or maybe try `I\\font<same font id>=<name of loaded font>'.");
    error();
    goto done;
  }

  f = font_ptr + 1;
  font_dir[f] = jfm_flag;
  font_enc[f] = jfm_enc;
  if (jfm_flag == dir_default)
    font_enc[f] = 0;
  font_num_ext[f] = nt;
  ctype_base[f] = fmem_ptr;
  char_base[f] = ctype_base[f] + nt - bc;
  width_base[f] = char_base[f] + ec + 1;
  height_base[f] = width_base[f] + nw;
  depth_base[f] = height_base[f] + nh;
  italic_base[f] = depth_base[f] + nd;
  lig_kern_base[f] = italic_base[f] + ni;
  kern_base[f] = lig_kern_base[f] + nl - kern_base_offset;
  exten_base[f] = kern_base[f] + kern_base_offset + nk;
  param_base[f] = exten_base[f] + ne;

  {
    if (lh < 2)
      goto bad_tfm;

    store_four_quarters(font_check[f]);
    fget();
    read_sixteen(z);
    fget();
    z = z * 256 + fbyte;
    fget();
    z = (z * 16) + (fbyte / 16);

    if (z < unity)
      goto bad_tfm;

    while (lh > 2)
    {
      fget();
      fget();
      fget();
      fget();
      decr(lh);
    }

    font_dsize[f] = z;

    if (s != -1000)
    {
      if (s >= 0)
        z = s;
      else
        z = xn_over_d(z, -s, 1000);
    }

    font_size[f] = z;
  }

  if (jfm_flag != dir_default)
  {
    for (k = ctype_base[f]; k <= ctype_base[f] + nt - 1; k++)
    {
      fget();
      read_twentyfourx(cx);
      if (jfm_enc == 2) // {Unicode TFM}
        font_info[k].hh.rh = toDVI(fromUCS(cx));
      else if (jfm_enc == 1) // {JIS-encoded TFM}
        font_info[k].hh.rh = toDVI(fromJIS(cx));
      else
        font_info[k].hh.rh = tokanji(cx); // {|kchar_code|}
      fget();
      cx = fbyte;
      font_info[k].hh.lh = tonum(cx); // {|kchar_type|}
    }
  }

  for (k = char_base[f] + bc; k <= width_base[f] - 1; k++)
  {
    store_four_quarters(font_info[k].qqqq);

    if ((a >= nw) || (b / 16 >= nh) || (b % 16 >= nd) || (c / 4 >= ni))
      goto bad_tfm;

    switch (c % 4)
    {
      case lig_tag:
        if (d >= nl)
          goto bad_tfm;
        break;

      case ext_tag:
        if (d >= ne)
          goto bad_tfm;
        break;

      case list_tag:
        {
          check_byte_range(d);

          while (d < current_character_being_worked_on)
          {
            qw = char_info(f, d);
 
            if (char_tag(qw) != list_tag)
              goto not_found;

            d = rem_byte(qw);
          }

          if (d == current_character_being_worked_on)
            goto bad_tfm;
not_found:;
        }
        break;

      default:
        do_nothing();
        break;
    }
  }

  {
    {
      alpha = 16;

      while (z >= 040000000)
      {
        z = z / 2;
        alpha = alpha + alpha;
      }

      beta = (char) (256 / alpha);
      alpha = alpha * z;
    }

    for (k = width_base[f]; k <= lig_kern_base[f] - 1; k++)
      store_scaled(font_info[k].sc);

    if (font_info[width_base[f]].sc != 0)
      goto bad_tfm;

    if (font_info[height_base[f]].sc != 0)
      goto bad_tfm;

    if (font_info[depth_base[f]].sc != 0)
      goto bad_tfm;

    if (font_info[italic_base[f]].sc != 0)
      goto bad_tfm;
  }

  bch_label = 077777;
  bchar = 256;

  if (nl > 0)
  {
    for (k = lig_kern_base[f]; k <= kern_base[f] + kern_base_offset - 1; k++)
    {
      store_four_quarters(font_info[k].qqqq);

      if (a > 128)
      {
        if (256 * c + d >= nl)
          goto bad_tfm;

        if (a == 255)
          if (k == lig_kern_base[f])
            bchar = b;
      }
      else
      {
        if (b != bchar)
          check_existence(b);

        if (c < 128)
        {
          if (jfm_flag != dir_default)
          {
            if (256 * c + d >= ne)
              goto bad_tfm;
          }
          else
           check_existence(d);
        }
        else if (256 * (c - 128) + d >= nk)
          goto bad_tfm;

        if (a < 128)
          if (k - lig_kern_base[f] + a + 1 >= nl)
            goto bad_tfm;
      }
    }

    if (a == 255)
      bch_label = 256 * c + d;
  }

  for (k = kern_base[f] + kern_base_offset; k <= exten_base[f] - 1; k++)
    store_scaled(font_info[k].sc);

  if (jfm_flag != dir_default)
  {
    for (k = exten_base[f]; k <= param_base[f] - 1; k++)
      store_scaled(font_info[k].cint);
  }
  else for (k = exten_base[f]; k <= param_base[f] - 1; k++)
  {
    store_four_quarters(font_info[k].qqqq);

    if (a != 0)
      check_existence(a);

    if (b != 0)
      check_existence(b);

    if (c != 0)
      check_existence(c);

    check_existence(d);
  }

  {
    for (k = 1; k <= np; k++)
    {
      if (k == 1)
      {
        fget();
        sw = fbyte;

        if (sw > 127)
          sw = sw - 256;

        fget();
        sw = sw * 256 + fbyte;
        fget();
        sw = sw * 256 + fbyte;
        fget();
        font_info[param_base[f]].sc = (sw * 16) + (fbyte / 16);
      }
      else
        store_scaled(font_info[param_base[f] + k - 1].sc);
    }

    if (feof(tfm_file))
      goto bad_tfm;

    for (k = np + 1; k <= 7; k++)
      font_info[param_base[f] + k - 1].sc = 0;
  }

  if (np >= 7)
    font_params[f] = np;
  else
    font_params[f] = 7;

  hyphen_char[f] = default_hyphen_char;
  skew_char[f] = default_skew_char;

  if (bch_label < nl)
    bchar_label[f] = bch_label + lig_kern_base[f];
  else
    bchar_label[f] = non_address;

  font_bchar[f] = bchar;
  font_false_bchar[f] = bchar;

  if (bchar <= ec)
    if (bchar >= bc)
    {
      qw = char_info(f, bchar);

      if (char_exists(qw))
        font_false_bchar[f] = non_char;
    }

  font_name[f] = nom;
  font_area[f] = aire;
  font_bc[f] = bc;
  font_ec[f] = ec;
  font_glue[f] = null;
  adjust(ctype_base);
  adjust(char_base);
  adjust(width_base);
  adjust(lig_kern_base);
  adjust(kern_base);
  adjust(exten_base);
  decr(param_base[f]);
  fmem_ptr = fmem_ptr + lf;
  font_ptr = f;
  g = f;
  goto done;

bad_tfm:
  start_font_error_message();

  if (file_opened)
    prints(" not loadable: Bad metric (TFM) file");
  else
    prints(" not loadable: Metric (TFM) file not found");

  help5("I wasn't able to read the size data for this font,",
      "so I will ignore the font specification.",
      "[Wizards can fix TFM files using TFtoPL/PLtoTF.]",
      "You might try inserting a different font spec;",
      "e.g., type `I\\font<same font id>=<substitute font name>'.");
  error();

done:
  if (file_opened)
    b_close(tfm_file);

  if (tracing_fontloaders > 0)
  {
    if (g == null_font)
    {
      begin_diagnostic();
      print_nl(" -> font not found, using \"nullfont\"");
      end_diagnostic(false);
    }
    else
    {
      begin_diagnostic();
      print_nl(" -> ");

      switch (font_dir[g])
      {
        case dir_default:
          prints("[TFM:D] ");
          break;

        case dir_yoko:
          prints("[JFM:Y] ");
          break;

        case dir_tate:
          prints("[JFM:T] ");
          break;
      }

      for (k = 0; k <= name_length; k++)
        print_char(name_of_file[k + 1]);

      end_diagnostic(false);
    }
  }

  return g;
}

static void char_warning (internal_font_number f, eight_bits c)
{
  ASCII_code l; // {small indices or counters}
  integer old_setting;  // {saved value of |tracing_online|}

  if (tracing_lost_chars > 0)
  {
    old_setting = tracing_online;

    if (eTeX_ex && (tracing_lost_chars > 1))
      tracing_online = 1;

    if (tracing_lost_chars > 2)
    {
      print_err("Missing character: There is no ");
    }
    else
    {
      begin_diagnostic();
      print_nl("Missing character: there is no ");
    }

    if ((c < ' ') || (c > '~'))
    {
      print_char('^');
      print_char('^');

      if (c < 64)
        print_char(c + 64);
      else if (c < 128)
        print_char(c - 64);
      else
      {
        print_lc_hex(c / 16);
        print_lc_hex(c % 16);
      }
    }
    else
      print(c);

    if (tracing_lost_chars > 2)
    {
      prints(" (");
      print_hex(c);
      prints(")");
    }

    prints(" in font ");
    slow_print(font_name[f]);

    if (tracing_lost_chars < 3)
      print_char('!');

    tracing_online = old_setting;

    if (tracing_lost_chars > 2)
    {
      help0();
      error();
    }
    else
      end_diagnostic(false);
  }
}

static void char_warning_jis (internal_font_number f, KANJI_code jc)
{
  if (tracing_lost_chars > 0)
  {
    begin_diagnostic();
    print_nl("Character ");
    print_kanji(jc);
    prints(" (");
    print_hex(jc);
    prints(") cannot be typeset in JIS-encoded JFM ");
    slow_print(font_name[f]);
    print_char(',');
    print_nl("so I use .notdef glyph instead.");
    end_diagnostic(false);
  }
}

static pointer new_character (internal_font_number f, eight_bits c)
{
  pointer p;  // {newly allocated node}

  if (font_bc[f] <= c)
  {
    if (font_ec[f] >= c)
    {
      if (char_exists(char_info(f, c)))
      {
        p = get_avail();
        font(p) = f;
        character(p) = c;
        return p;
      }
    }
  }

  char_warning(f, c);
  return null;
}

// {outputs half of the buffer}
void dvi_swap (void)
{
  if (dvi_limit == dvi_buf_size)
  {
    write_dvi(0, half_buf - 1);
    dvi_limit = half_buf;
    dvi_offset = dvi_offset + dvi_buf_size;
    dvi_ptr = 0;
  }
  else
  {
    write_dvi(half_buf, dvi_buf_size - 1);
    dvi_limit = dvi_buf_size;
  }

  dvi_gone = dvi_gone + half_buf;
}

static void dvi_four (integer x)
{ 
  if (x >= 0)
    dvi_out(x / 0100000000);
  else
  {
    x = x + 010000000000;
    x = x + 010000000000;
    dvi_out((x / 0100000000) + 128);
  }

  x = x % 0100000000;
  dvi_out(x / 0200000);
  x = x % 0200000;
  dvi_out(x / 0400);
  dvi_out(x % 0400);
}

static void dvi_pop (integer l)
{
  if ((l == dvi_offset + dvi_ptr) && (dvi_ptr > 0))
    decr(dvi_ptr);
  else
    dvi_out(pop);
}

void movement (scaled w, eight_bits o)
{
  small_number mstate;
  pointer p, q;
  integer k;

  q = get_node(movement_node_size);
  width(q) = w;
  location(q) = dvi_offset + dvi_ptr;

  if (o == down1)
  {
    link(q) = down_ptr;
    down_ptr = q;
  }
  else
  {
    link(q) = right_ptr;
    right_ptr = q;
  }

  p = link(q);
  mstate = none_seen;

  while (p != null)
  {
    if (width(p) == w)
    {
      switch (mstate + info(p))
      {
        case none_seen + yz_OK:
        case none_seen + y_OK:
        case z_seen + yz_OK:
        case z_seen + y_OK:
          if (location(p) < dvi_gone)
            goto not_found;
          else
          {
            k = location(p) - dvi_offset;

            if (k < 0)
              k = k + dvi_buf_size;

            dvi_buf[k] = dvi_buf[k] + y1 - down1;
            info(p) = y_here;
            goto found;
          }
          break;

        case none_seen + z_OK:
        case y_seen + yz_OK:
        case y_seen + z_OK:
          if (location(p) < dvi_gone)
            goto not_found;
          else
          {
            k = location(p) - dvi_offset;

            if (k < 0)
              k = k + dvi_buf_size;

            dvi_buf[k] = dvi_buf[k] + z1 - down1;
            info(p) = z_here;
            goto found;
          }
          break;

        case none_seen + y_here:
        case none_seen + z_here:
        case y_seen + z_here:
        case z_seen + y_here:
          goto found;
          break;

        default:
          do_nothing();
          break;
      }
    }
    else
    {
      switch (mstate + info(p))
      {
        case none_seen + y_here:
          mstate = y_seen;
          break;

        case none_seen + z_here:
          mstate = z_seen;
          break;

        case y_seen + z_here:
        case z_seen + y_here:
          goto not_found;
          break;

        default:
          do_nothing();
          break;
      }
    }

    p = link(p);
  }

not_found:

  info(q) = yz_OK;

  if (abs(w) >= 040000000)
  {
    dvi_out(o + 3);
    dvi_four(w);

    return;
  }

  if (abs(w) >= 0100000)
  {
    dvi_out(o + 2);

    if (w < 0)
      w = w + 0100000000;
    //dvi_out(w / 65536L);
    dvi_out((w >> 16));
    //w = w % 65536L;
    w = w & 65535L;
    goto lab2;
  }

  if (abs(w) >= 128)
  {
    dvi_out(o + 1);

    if (w < 0)
      w = w + 65536L;

    goto lab2;
  }

  dvi_out(o);

  if (w < 0)
    w = w + 256;

  goto lab1;

lab2:
  dvi_out(w / 256);

lab1:
  dvi_out(w % 256);
  return;

found:
  info(q) = info(p);

  if (info(q) == y_here)
  {
    dvi_out(o + y0 - down1);

    while (link(q) != p)
    {
      q = link(q);

      switch (info(q))
      {
        case yz_OK:
          info(q) = z_OK;
          break;

        case y_OK:
          info(q) = d_fixed;
          break;

        default:
          do_nothing();
          break;
      }
    }
  }
  else
  {
    dvi_out(o + z0 - down1);

    while (link(q) != p)
    {
      q = link(q);

      switch (info(q))
      {
        case yz_OK:
          info(q) = y_OK;
          break;

        case z_OK:
          info(q) = d_fixed;
          break;

        default:
          do_nothing();
          break;
      }
    }
  }
}

void prune_movements (integer l)
{
  pointer p;

  while (down_ptr != null)
  {
    if (location(down_ptr) < l)
      goto done;

    p = down_ptr;
    down_ptr = link(p);
    free_node(p, movement_node_size);
  }

done:
  while (right_ptr != null)
  {
    if (location(right_ptr) < l)
      return;

    p = right_ptr;
    right_ptr = link(p);
    free_node(p, movement_node_size);
  }
}

#ifndef APTEX_DVI_ONLY

// PDF output functions from texlive/texk/dvipdfm-x

/* from "dvipdfm-x/fontmap.h" */
extern void pdf_init_fontmaps (void);
extern void pdf_close_fontmaps (void);
extern int pdf_load_fontmap_file (const char *filename, int map_mode);

/* from "dvipdfm-x/pdfdoc.h" */
struct pdf_dev_setting {
    double      dvi2pts;   /* conversion unit */
    int         precision; /* number of decimal digits kept */
    int         ignore_colors; /* 1 for black or white */
};

struct pdf_enc_setting {
    int         key_size;
    uint32_t    permission;
    const char *uplain, *oplain; /* password */
    int         use_aes;
    int         encrypt_metadata;
};

struct pdf_obj_setting {
    int         enable_objstm;
    int         enable_predictor;
    int         compression_level;
};

struct pdf_setting
{
    int    ver_major, ver_minor;
    double media_width, media_height;
    struct {
      double x, y;
    } annot_grow_amount;
    int    outline_open_depth;
    int    check_gotos;
    int    enable_manual_thumb;
    int    enable_encrypt;
    struct pdf_enc_setting encrypt;
    struct pdf_dev_setting device;
    struct pdf_obj_setting object;
};

extern void pdf_open_document (const char *filename, const char * creator, 
  const unsigned char * id1, const unsigned char * id2, struct pdf_setting settings);
extern void pdf_close_document (void);
extern void pdf_doc_begin_page (double scale, double x_origin, double y_origin);
extern void pdf_doc_end_page (void);
typedef struct pdf_rect {
  double llx, lly, urx, ury;
} pdf_rect;
extern void pdf_doc_set_mediabox (unsigned page_no, const pdf_rect *mediabox);

/* from "dvipdfm-x/pdfobj.h" */
extern long pdf_output_stats (void);

/* from "dvipdfm-x/pdfdev.h" */
extern void graphics_mode (void);
typedef signed long spt_t;
extern void pdf_dev_set_rule (spt_t xpos, spt_t ypos, spt_t width, spt_t height);
extern void pdf_dev_set_dirmode (int dir_mode);
extern void pdf_dev_begin_actualtext (uint16_t * unicodes, int len);
extern void pdf_dev_end_actualtext (void);

/* from "dvipdfm-x/pdflimits.h" */
#define PDF_VERSION_MIN 13
#define PDF_VERSION_MAX 20

/* from "dvipdfm-x/specials.h" */
extern int spc_exec_at_begin_document (void);
extern void spc_exec_at_end_document (void);
extern int spc_exec_at_begin_page (void);
extern int spc_exec_at_end_page (void);
extern int spc_exec_special (const char *buffer, long size,
 double x_user, double y_user, double dpx_mag, int * is_drawable, pdf_rect *rect);

/* from "dvipdfm-x/dvi.c" */
extern int dvi_locate_font (const char *tfm_name, spt_t ptsize);
extern int dvi_locate_native_font (const char *filename, uint32_t fidx,
  spt_t ptsize, int layout_dir, int extend, int slant, int embolden);

/* from "src/libdpx/ng/dvi_ng.c" */
extern void ng_set (int32_t ch, int ng_font_id, int32_t h, int32_t v);
extern void ng_gid (uint16_t gid, int ng_font_id, int32_t h, int32_t v);
extern void ng_layer (uint16_t gid, int ng_font_id, int32_t h, int32_t v, uint8_t r, uint8_t g, uint8_t b);
extern void spc_moveto (int32_t, int32_t);

/* dvipdfm-x/dvipdfmx.c */
extern int dpx_util_format_asn_date (char *, int);
typedef struct {
  uint32_t A, B, C, D;
  size_t nblocks;
  unsigned char buf[64];
  int count;
} MD5_CONTEXT;
extern void MD5_init (MD5_CONTEXT *);
extern void MD5_write (MD5_CONTEXT *, const unsigned char *, unsigned int);
extern void MD5_final (unsigned char *, MD5_CONTEXT *);

static void dpx_compute_id_string (unsigned char * id, const char * producer, const char * dvi_file_name, const char * pdf_file_name)
{
  char datestr[32];
  MD5_CONTEXT md5;

  MD5_init(&md5);
  dpx_util_format_asn_date(datestr, 0);
  MD5_write(&md5, (const unsigned char *) datestr, strlen(datestr));
  MD5_write(&md5, (const unsigned char *) producer, strlen(producer));
  MD5_write(&md5, (const unsigned char *) dvi_file_name, strlen(dvi_file_name));
  MD5_write(&md5, (const unsigned char *) pdf_file_name, strlen(pdf_file_name));
  MD5_final(id, &md5);
}

static const double sp2bp = 0.000015202;
static int     font_id[65536];
static char * output_pdf_name;
static char * output_dvi_name;

static void pdf_locate_font (internal_font_number f)
{
  char * lfont_name = take_str_string(font_name[f]);
  font_id[f] = dvi_locate_font(lfont_name, font_size[f]);
  free(lfont_name);
}

static void pdf_char_out (internal_font_number f, ASCII_code c)
{
  switch (cur_dir_hv)
  {
    case dir_yoko:
      pdf_dev_set_dirmode(dvi_yoko);
      ng_set(c, font_id[f], cur_h, -cur_v);
      break;

    case dir_tate:
      pdf_dev_set_dirmode(dvi_tate);
      ng_set(c, font_id[f], -cur_v, -cur_h);
      break;

    case dir_dtou:
      pdf_dev_set_dirmode(dvi_dtou);
      ng_set(c, font_id[f], cur_v, cur_h);
      break;
  }
}

static void pdf_kanji_out (internal_font_number f, KANJI_code c)
{
  switch (cur_dir_hv)
  {
    case dir_yoko:
      pdf_dev_set_dirmode(dvi_yoko);
      ng_set(c, font_id[f], cur_h, -cur_v);
      break;

    case dir_tate:
      pdf_dev_set_dirmode(dvi_tate);
      ng_set(c, font_id[f], -cur_v, -cur_h);
      break;

    case dir_dtou:
      pdf_dev_set_dirmode(dvi_dtou);
      ng_set(c, font_id[f], cur_v, cur_h);
      break;
  }
}

void pdf_rule_out (scaled rule_wd, scaled rule_ht)
{
  switch (cur_dir_hv)
  {
    case dir_yoko:
      pdf_dev_set_rule(cur_h, -cur_v, rule_wd, rule_ht);
      break;

    case dir_tate:
      pdf_dev_set_rule(-cur_v, -cur_h - rule_wd, rule_ht, rule_wd);
      break;

    case dir_dtou:
      pdf_dev_set_rule(cur_v, cur_h, rule_ht, rule_wd);
      break;
  }
}

static void aptex_dpx_init_page (scaled page_wd, scaled page_ht)
{
  pdf_rect mediabox;

  mediabox.llx = 0.0;
  mediabox.lly = 0.0;
  mediabox.urx = page_wd * sp2bp;
  mediabox.ury = page_ht * sp2bp;

  pdf_doc_set_mediabox(0, &mediabox);
}

static void aptex_dpx_bop (integer page_no, scaled page_wd, scaled page_ht, scaled page_ho, scaled page_vo)
{
  pdf_rect mediabox;

  mediabox.llx = 0.0;
  mediabox.lly = 0.0;
  mediabox.urx = page_wd * sp2bp * (mag / 1000.0);
  mediabox.ury = page_ht * sp2bp * (mag / 1000.0);
  pdf_doc_set_mediabox(page_no, &mediabox);
  pdf_doc_begin_page(mag / 1000.0, page_ho * sp2bp * (mag / 1000.0), (page_ht - page_vo) * sp2bp * (mag / 1000.0));
  spc_exec_at_begin_page();
}

static void aptex_dpx_eop (void)
{
  spc_exec_at_end_page();
  pdf_doc_end_page();
}

#endif

// ship out part
static void dvi_font_def (internal_font_number f)
{
  pool_pointer k;

#ifdef APTEX_EXTENSION
  if (f <= 256)
  {
    dvi_out(fnt_def1);
    dvi_out(f - 1);
  }
  else
  {
    dvi_out(fnt_def2);
    dvi_out(((f - 1) >> 8));
    dvi_out(((f - 1) & 255));
  }
#else
  dvi_out(fnt_def1);
  dvi_out(f - 1);
#endif

  dvi_out(font_check[f].b0);
  dvi_out(font_check[f].b1);
  dvi_out(font_check[f].b2);
  dvi_out(font_check[f].b3);
  dvi_four(font_size[f]);
  dvi_four(font_dsize[f]);
  dvi_out(length(font_area[f]));
  dvi_out(length(font_name[f]));

  for (k = str_start[font_area[f]]; k <= str_start[font_area[f] + 1] - 1; k++)
    dvi_out(str_pool[k]);

  for (k = str_start[font_name[f]]; k <= str_start[font_name[f] + 1] - 1; k++)
    dvi_out(str_pool[k]);
}

// {output the box |p|}
static void ship_out (pointer p)
{
  integer page_loc; // {location of the current |bop|}
  pointer del_node; // {used when delete the |dir_node| continued box}
  char j, k;  // {indices to first ten count registers}
  pool_pointer s; // {index into |str_pool|}
  char old_setting; // {saved |selector| setting}

  // @<Start sheet {\sl Sync\TeX} information record@>
  synctex_sheet(mag);

  if (tracing_output > 0)
  {
    print_nl("");
    print_ln();
    prints("Completed box being shipped out");
  }

  if (term_offset > max_print_line - 9)
    print_ln();
  else if ((term_offset > 0) || (file_offset > 0))
    print_char(' ');

  print_char('[');
  j = 9;

  while ((count(j) == 0) && (j > 0))
    decr(j);

  for (k = 0; k <= j; k++)
  {
    print_int(count(k));

    if (k < j)
      print_char('.');
  }

  update_terminal();

  if (tracing_output > 0)
  {
    print_char(']');
    begin_diagnostic();
    show_box(p);
    end_diagnostic(true);
  }

  if (type(p) == dir_node)
  {
    del_node = p;
    p = list_ptr(p);
    delete_glue_ref(space_ptr(del_node));
    delete_glue_ref(xspace_ptr(del_node));
    free_node(del_node, box_node_size);
  }

  flush_node_list(link(p));
  link(p) = null;

  if (abs(box_dir(p)) != dir_yoko)
    p = new_dir_node(p, dir_yoko);

  if ((height(p) > max_dimen) || (depth(p) > max_dimen) ||
    (height(p) + depth(p) + v_offset > max_dimen) ||
    (width(p) + h_offset > max_dimen))
  {
    print_err("Huge page cannot be shipped out");
    help2("The page just created is more than 18 feet tall or",
      "more than 18 feet wide, so I suspect something went wrong.");
    error();

    if (tracing_output <= 0)
    {
      begin_diagnostic();
      print_nl("The following box has been deleted:");
      show_box(p);
      end_diagnostic(true);
    }

    goto done;
  }

  if (height(p) + depth(p) + v_offset > max_v)
    max_v = height(p) + depth(p) + v_offset;

  if (width(p) + h_offset > max_h)
    max_h = width(p) + h_offset;

  dvi_h = 0;
  dvi_v = 0;
  cur_h = h_offset;
  dvi_f = null_font;
  dvi_dir = dir_yoko;
  cur_dir_hv = dvi_dir;

  if (pdf_page_height != 0)
    cur_page_height = pdf_page_height;
  else if ((box_dir(p) == dir_tate) || (box_dir(p) == dir_dtou))
    cur_page_height = width(p) + 2 * v_offset + 2 * 4736286;
  else
    cur_page_height = height(p) + depth(p) + 2 * v_offset + 2 * 4736286;

  if (pdf_page_width != 0)
    cur_page_width = pdf_page_width;
  else if ((box_dir(p) == dir_tate) || (box_dir(p) == dir_dtou))
    cur_page_width = height(p) + depth(p) + 2 * h_offset + 2 * 4736286;
  else
    cur_page_width = width(p) + 2 * h_offset + 2 * 4736286;

  ensure_dvi_open();

  if (total_pages == 0)
  {
    dvi_out(pre);
    dvi_out(id_byte);
    dvi_four(25400000);
    dvi_four(473628672);
    prepare_mag();
    dvi_four(mag);
    old_setting = selector;
    selector = new_string;
    prints(" TeX output ");
    print_int(year);
    print_char('.');
    print_two(month);
    print_char('.');
    print_two(day);
    print_char(':');
    print_two(tex_time / 60);
    print_two(tex_time % 60);
    selector = old_setting;
    dvi_out(cur_length);

    for (s = str_start[str_ptr]; s <= pool_ptr - 1; s++)
      dvi_out(str_pool[s]);

    pool_ptr = str_start[str_ptr];

#ifndef APTEX_DVI_ONLY
    {
      struct pdf_setting aptex_pdf_setting;
      char * aptex_producer = "Asiatic pTeX 2024";
      int aptex_pdf_version;
      unsigned char aptex_id1[16], aptex_id2[16];

      output_dvi_name = take_str_string(output_file_name);
      output_pdf_name = take_str_string(output_file_name);
      memcpy(output_pdf_name + length(output_file_name) - 4, ".pdf", 4);
      dpx_compute_id_string(aptex_id1, aptex_producer, output_dvi_name, output_pdf_name);
      memcpy(aptex_id2, aptex_id1, 16);

      aptex_pdf_version = 10 * pdf_major_version + pdf_minor_version;
      if ((aptex_pdf_version < PDF_VERSION_MIN) || (aptex_pdf_version > PDF_VERSION_MAX))
      {
        aptex_pdf_setting.ver_major = 1;
        aptex_pdf_setting.ver_minor = 5;
      }
      else
      {
        aptex_pdf_setting.ver_major = pdf_major_version;
        aptex_pdf_setting.ver_minor = pdf_minor_version;
      }

      if ((pdf_compress_level < 0) || (pdf_compress_level > 9))
        aptex_pdf_setting.object.compression_level = 9;
      else
        aptex_pdf_setting.object.compression_level = pdf_compress_level;

      pdf_init_fontmaps();

      /* TeX Live */
#ifdef USE_KPATHSEA
      if (kpse_find_file("pdftex.map", kpse_fontmap_format, false) != NULL)
        pdf_load_fontmap_file("pdftex.map", '+');
      if (kpse_find_file("kanjix.map", kpse_fontmap_format, false) != NULL)
        pdf_load_fontmap_file("kanjix.map", '+');
      if (kpse_find_file("ckx.map", kpse_fontmap_format, false) != NULL)
        pdf_load_fontmap_file("ckx.map", '+');
      /* W32TeX */
      if (kpse_find_file("dlbase14.map", kpse_fontmap_format, false) != NULL)
        pdf_load_fontmap_file("dlbase14.map", '+');
      if (kpse_find_file("dvipdfm.map", kpse_fontmap_format, false) != NULL)
        pdf_load_fontmap_file("dvipdfm.map", '+');

      if (aptex_env.aptex_map != NULL)
      {
        switch (aptex_env.aptex_map[0])
        {
          case '+':
            if (kpse_find_file(aptex_env.aptex_map + 1, kpse_fontmap_format, false) != NULL)
              pdf_load_fontmap_file(aptex_env.aptex_map + 1, '+');
            break;
          case '!':
            if(kpse_find_file(aptex_env.aptex_map + 1, kpse_fontmap_format, false) != NULL)
              pdf_load_fontmap_file(aptex_env.aptex_map + 1, 0);
            break;
        }
      }
#endif /* USE_KPATHSEA */

      aptex_pdf_setting.media_width = 595.0;
      aptex_pdf_setting.media_height = 842.0;
      aptex_pdf_setting.annot_grow_amount.x = 0.0;
      aptex_pdf_setting.annot_grow_amount.y = 0.0;
      aptex_pdf_setting.outline_open_depth = 0;
      aptex_pdf_setting.check_gotos = 0; // !(1 << 4);
      aptex_pdf_setting.enable_manual_thumb = 0;
      aptex_pdf_setting.enable_encrypt = 0;
      aptex_pdf_setting.object.enable_objstm = 1;
      aptex_pdf_setting.object.enable_predictor = 1;
      aptex_pdf_setting.device.dvi2pts = sp2bp;
      aptex_pdf_setting.device.precision = 3;
      aptex_pdf_setting.device.ignore_colors = 0;
      pdf_open_document(utf8_mbcs(output_pdf_name), aptex_producer, aptex_id1, aptex_id2, aptex_pdf_setting);
      aptex_dpx_init_page(pdf_page_width, pdf_page_height);
      spc_exec_at_begin_document();
    }
#endif
  }

#ifndef APTEX_DVI_ONLY
  aptex_dpx_bop(total_pages + 1, pdf_page_width, pdf_page_height, pdf_h_origin, pdf_v_origin);
#endif
  page_loc = dvi_offset + dvi_ptr;
  dvi_out(bop);

  for (k = 0; k <= 9; k++)
    dvi_four(count(k));

  dvi_four(last_bop);
  last_bop = page_loc;
  cur_v = height(p) + v_offset;
  temp_ptr = p;

  switch (type(p))
  {
    case hlist_node:
      hlist_out();
      break;

    case vlist_node:
      vlist_out();
      break;

    case dir_node:
      dir_out();
      break;
  }

#ifndef APTEX_DVI_ONLY
  aptex_dpx_eop();
#endif
  dvi_out(eop);

  incr(total_pages);
  cur_s = -1;

  if (eTeX_ex)
  {
    if (LR_problems > 0)
    {
      report_LR_problems();
      print_char(')');
      print_ln();
    }

    if ((LR_ptr != null) || (cur_dir != left_to_right))
      confusion("LR3");
  }

done:
  if (tracing_output <= 0)
    print_char(']');

  dead_cycles = 0;
  update_terminal();
  synctex_teehs();

#ifdef STAT
  if (tracing_stats > 1)
  {
    print_nl("Memory usage before: ");
    print_int(var_used);
    print_char('&');
    print_int(dyn_used);
    print_char(';');
  }
#endif

  flush_node_list(p);

#ifdef STAT
  if (tracing_stats > 1)
  {
    prints(" after: ");
    print_int(var_used);
    print_char('&');
    print_int(dyn_used);
    prints("; still utouched: ");
    print_int(hi_mem_min - lo_mem_max - 1);
    print_ln();
  }
#endif
}

static void synch_dir (void)
{
  scaled tmp; // {temporary resister}

  switch (cur_dir_hv)
  {
    case dir_yoko:
      if (dvi_dir != cur_dir_hv)
      {
        synch_h();
        synch_v();
        dvi_out(dirchg);
        dvi_out(dvi_yoko);
        dir_used = true;

        switch (dvi_dir)
        {
          case dir_tate:
            {
              tmp = cur_h;
              cur_h = -cur_v;
              cur_v = tmp;
            }
            break;

          case dir_dtou:
            {
              tmp = cur_h;
              cur_h = cur_v;
              cur_v = -tmp;
            }
            break;
        }

        dvi_h = cur_h;
        dvi_v = cur_v;
        dvi_dir = cur_dir_hv;
      }
      break;

    case dir_tate:
      if (dvi_dir != cur_dir_hv)
      {
        synch_h();
        synch_v();
        dvi_out(dirchg);
        dvi_out(dvi_tate);
        dir_used = true;

        switch (dvi_dir)
        {
          case dir_yoko:
            {
              tmp = cur_h;
              cur_h = cur_v;
              cur_v = -tmp;
            }
            break;

          case dir_dtou:
            {
              cur_v = -cur_v;
              cur_h = -cur_h;
            }
            break;
        }

        dvi_h = cur_h;
        dvi_v = cur_v;
        dvi_dir = cur_dir_hv;
      }
      break;

    case dir_dtou:
      if (dvi_dir != cur_dir_hv)
      {
        synch_h();
        synch_v();
        dvi_out(dirchg);
        dvi_out(dvi_dtou);
        dir_used = true;

        switch (dvi_dir)
        {
          case dir_yoko:
            {
              tmp = cur_h;
              cur_h = -cur_v;
              cur_v = tmp;
            }
            break;

          case dir_tate:
            {
              cur_v = -cur_v;
              cur_h = -cur_h;
            }
            break;
        }

        dvi_h = cur_h;
        dvi_v = cur_v;
        dvi_dir = cur_dir_hv;
      }
      break;

    default:
      confusion("synch_dir");
      break;
  }
}

// output an |hlist_node| box
void hlist_out (void)
{
  scaled base_line; // {the baseline coordinate for this box}
  scaled disp;  // {displacement}
  eight_bits save_dir;  // {what |dvi_dir| should pop to}
  KANJI_code jc;  // {temporary register for KANJI codes}
  pointer ksp_ptr;  // {position of |auto_spacing_glue| in the hlist}
  scaled left_edge; // {the left coordinate for this box}
  scaled save_h, save_v;  // {what |dvi_h| and |dvi_v| should pop to}
  pointer this_box; // {pointer to containing box}
  // glue_ord g_order;
  int g_order;  // {applicable order of infinity for glue}
  // char g_sign;
  int g_sign; // {selects type of glue}
  pointer p;  // {current position in the hlist}
  integer save_loc; // {\.{DVI} byte location upon entry}
  pointer leader_box; // {the leader box being replicated}
  scaled leader_wd; // {width of leader box being replicated}
  scaled lx;  // {extra space between leader boxes}
  boolean outer_doing_leaders;  // {were we doing leaders?}
  scaled edge;  // {right edge of sub-box or leader space}
  pointer prev_p; // {one step behind |p|}
  real glue_temp; // {glue value before rounding}
  real cur_glue;  // {glue seen so far}
  scaled cur_g; // {rounded equivalent of |cur_glue| times the glue ratio}

  cur_g = 0;
  cur_glue = 0.0;
  this_box = temp_ptr;
  g_order = glue_order(this_box);
  g_sign = glue_sign(this_box);
  p = list_ptr(this_box);
  ksp_ptr = space_ptr(this_box);
  incr(cur_s);

  if (cur_s > 0)
    dvi_out(push);

  if (cur_s > max_push)
    max_push = cur_s;

  save_loc = dvi_offset + dvi_ptr;
  synch_dir();
  base_line = cur_v;
  disp = 0;
  revdisp = 0;
  prev_p = this_box + list_offset;

  // @<Initialize |hlist_out| for mixed direction typesetting@>
  if (eTeX_ex)
  {
    put_LR(before);
  
    if (box_lr(this_box) == dlist)
    {
      if (cur_dir == right_to_left)
      {
        cur_dir = left_to_right;
        cur_h = cur_h - width(this_box);
      }
      else
        set_box_lr(this_box, 0);
    }

    if ((cur_dir == right_to_left) && (box_lr(this_box) != reversed))
    {
      save_h = cur_h;
      temp_ptr = p;
      p = new_kern(0);
      link(prev_p) = p;
      cur_h = 0;
      link(p) = reverse(this_box, null, cur_g, cur_glue);
      width(p) = -cur_h;
      cur_h = save_h;
      set_box_lr(this_box, reversed);
    }
  }

  left_edge = cur_h;
  // @<Start hlist {\sl Sync\TeX} information record@>
  synctex_hlist(this_box);
 
  /*
    @<Output node |p| for |hlist_out| and move to the next node,
    maintaining the condition |cur_v=base_line|@>
  */
  while (p != null)
reswitch:
  if (is_char_node(p))
  {
    synch_h();
    synch_v();
    chain = false;

    do {
      f = font(p);
      c = character(p);

      // @<Change font |dvi_f| to |f|@>
      if (f != dvi_f)
      {
        if (!font_used[f])
        {
          dvi_font_def(f);
#ifndef APTEX_DVI_ONLY
          pdf_locate_font(f);
#endif
          font_used[f] = true;
        }

        if (f <= 64 + font_base)
          dvi_out(f - font_base - 1 + fnt_num_0);
#ifdef INCREASEFONTS
        else if (f <= 256)
        {
          dvi_out(fnt1);
          dvi_out(f - 1);
        }
        else
        {
          dvi_out(fnt2);
          dvi_out(((f - 1) >> 8));  /* top byte */
          dvi_out(((f - 1) & 255)); /* bottom byte */
        }
#else
        else
        {
          dvi_out(fnt1);
          dvi_out(f - 1);
        }
#endif

        dvi_f = f;
      }

      if (font_dir[f] == dir_default)
      {
        chain = false;

        if (c >= 128)
          dvi_out(set1);

        dvi_out(c);

#ifndef APTEX_DVI_ONLY
        pdf_char_out(dvi_f, c);
#endif
        cur_h = cur_h + char_width(f, char_info(f, c));
      }
      else
      {
        if (chain == false)
          chain = true;
        else
        {
          cur_h = cur_h + width(ksp_ptr);

          if (g_sign != normal)
          {
            if (g_sign == stretching)
            {
              if (stretch_order(ksp_ptr) == g_order)
                cur_h = cur_h + round(tex_float(glue_set(this_box)) * stretch(ksp_ptr));
            }
            else
            {
              if (shrink_order(ksp_ptr) == g_order)
                cur_h = cur_h - round(tex_float(glue_set(this_box)) * shrink(ksp_ptr));
            }
          }

          synch_h();
        }

        prev_p = link(prev_p);  // {N.B.: not |prev_p:=p|, |p| might be |lig_trick|}
        p = link(p);
        jc = KANJI(info(p)) % max_cjk_val;

        if (font_enc[f] == 2)
          jc = toUCS(jc);
        else if (font_enc[f] == 1)
        {
          if (toJIS(jc) == 0)
            char_warning_jis(f, jc);
          jc = toJIS(jc);
        }
        else
          jc = toDVI(jc);

        if (jc < 0x10000)
        {
          dvi_out(set2);
        }
        else
        {
          dvi_out(set3);
          dvi_out(BYTE2(jc));
        }

        dvi_out(BYTE3(jc));
        dvi_out(BYTE4(jc));

#ifndef APTEX_DVI_ONLY
        pdf_kanji_out(dvi_f, jc);
#endif
        cur_h = cur_h + char_width(f, char_info(f, c));
      }

      dvi_h = cur_h;
      prev_p = link(prev_p);
      p = link(p);
    } while (!(!is_char_node(p)));

    // @<Record current point {\sl Sync\TeX} information@>
    synctex_current();
    chain = false;
  }
  else
  {
    switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
        // @<Output a box in an hlist@>
        if (list_ptr(p) == null)
        {
          if (type(p) != dir_node)
          {
            // @<Record void list {\sl Sync\TeX} information@>
            if (type(p) == vlist_node)
              synctex_void_vlist(p, this_box);
            else
              synctex_void_hlist(p, this_box);
          }

          cur_h = cur_h + width(p);
        }
        else
        {
          save_h = dvi_h;
          save_v = dvi_v;
          save_dir = dvi_dir;
          cur_v = base_line + disp + shift_amount(p); // {shift the box down}
          temp_ptr = p;
          edge = cur_h + width(p);

          if (cur_dir == right_to_left)
            cur_h = edge;

          switch (type(p))
          {
            case hlist_node:
              hlist_out();
              break;

            case vlist_node:
              vlist_out();
              break;

            case dir_node:
              dir_out();
              break;
          }

          dvi_h = save_h;
          dvi_v = save_v;
          dvi_dir = save_dir;
          cur_h = edge;
          cur_v = base_line + disp;
          cur_dir_hv = save_dir;
        }
        break;

      case rule_node:
        {
          rule_ht = height(p);
          rule_dp = depth(p);
          rule_wd = width(p);
          goto fin_rule;
        }
        break;

      case whatsit_node:
        // @<Output the whatsit node |p| in an hlist@>
        out_what(p);
        break;

      case disp_node:
        {
          disp = disp_dimen(p);
          revdisp = disp;
          cur_v = base_line + disp;
        }
        break;

      case glue_node:
        // @<Move right or output leaders@>
        {
          round_glue();

          if (eTeX_ex)
            handle_a_glue_node();

          /*
            @<Output leaders in an hlist, |goto fin_rule| if a rule
            or to |next_p| if done@>
          */
          if (subtype(p) >= a_leaders)
          {
            leader_box = leader_ptr(p); 

            if (type(leader_box) == rule_node)
            {
              rule_ht = height(leader_box);
              rule_dp = depth(leader_box);
              goto fin_rule;
            }

            leader_wd = width(leader_box);

            if ((leader_wd > 0) && (rule_wd > 0))
            {
              rule_wd = rule_wd + 10; // {compensate for floating-point rounding}

              if (cur_dir == right_to_left)
                cur_h = cur_h - 10;

              edge = cur_h + rule_wd;
              lx = 0;

              /*
                @<Let |cur_h| be the position of the first box, and set |leader_wd+lx|
                to the spacing between corresponding parts of boxes@>
              */
              if (subtype(p) == a_leaders)
              {
                save_h = cur_h;
                cur_h = left_edge + leader_wd * ((cur_h - left_edge) / leader_wd);

                if (cur_h < save_h)
                  cur_h = cur_h + leader_wd;
              }
              else
              {
                lq = rule_wd / leader_wd; // {the number of box copies}
                lr = rule_wd % leader_wd; // {the remaining space}

                if (subtype(p) == c_leaders)
                  cur_h = cur_h + (lr / 2);
                else
                {
                  lx = lr / (lq + 1);
                  cur_h = cur_h + ((lr - (lq - 1) * lx) / 2);
                }
              }

              while (cur_h + leader_wd <= edge)
              {
                /*
                  @<Output a leader box at |cur_h|,
                  then advance |cur_h| by |leader_wd+lx|@>
                */
                cur_v = base_line + disp + shift_amount(leader_box);
                synch_v();
                save_v = dvi_v;
                synch_h();
                save_h = dvi_h;
                save_dir = dvi_dir;
                temp_ptr = leader_box;

                if (cur_dir == right_to_left)
                  cur_h = cur_h + leader_wd;

                outer_doing_leaders = doing_leaders;
                doing_leaders = true;

                switch (type(leader_box))
                {
                  case hlist_node:
                    hlist_out();
                    break;

                  case vlist_node:
                    vlist_out();
                    break;

                  case dir_node:
                    dir_out();
                    break;
                }

                doing_leaders = outer_doing_leaders;
                dvi_v = save_v;
                dvi_h = save_h;
                dvi_dir = save_dir;
                cur_v = base_line;
                cur_h = save_h + leader_wd + lx;
                cur_dir_hv = save_dir;
              }

              if (cur_dir == right_to_left)
                cur_h = edge;
              else
                cur_h = edge - 10;

              goto next_p;
            }
          }

          goto move_past;
        }
        break;

      case kern_node:
        // @<Record |kern_node| {\sl Sync\TeX} information@>
        synctex_kern(p, this_box);
        cur_h = cur_h + width(p);
        break;

      case math_node:
        {
          // @<Record |math_node| {\sl Sync\TeX} information@>
          synctex_math(p, this_box);

          // @<Handle a math node in |hlist_out|@>
          if (eTeX_ex)
          {
            if (end_LR(p))
            {
              if (info(LR_ptr) == end_LR_type(p))
                pop_LR();
              else
              {
                if (subtype(p) > L_code)
                  incr(LR_problems);
              }
            }
            else
            {
              push_LR(p);

              if (LR_dir(p) != cur_dir)
              {
                save_h = cur_h;
                temp_ptr = link(p);
                rule_wd = width(p);
                free_node(p, small_node_size);
                cur_dir = reflected;
                p = new_edge(cur_dir, rule_wd);
                link(prev_p) = p;
                cur_h = cur_h - left_edge + rule_wd;
                link(p) = reverse(this_box, new_edge(reflected, 0), cur_g, cur_glue);
                edge_dist(p) = cur_h;
                cur_dir = reflected;
                cur_h = save_h;
                goto reswitch;
              }
            }

            type(p) = kern_node;
          }

          cur_h = cur_h + width(p);
        }
        break;

      case ligature_node:
        // @<Make node |p| look like a |char_node| and |goto reswitch|@>
        {
          mem[lig_trick] = mem[lig_char(p)];
          link(lig_trick) = link(p);
          p = lig_trick;
          goto reswitch;
        }
        break;

      case edge_node:
        {
          cur_h = cur_h + width(p);
          left_edge = cur_h + edge_dist(p);
          cur_dir = subtype(p);
        }

      default:
        do_nothing();
        break;
    }

    goto next_p;

  fin_rule:
    // @<Output a rule in an hlist@>
    if (is_running(rule_ht))
      rule_ht = height(this_box) + disp;

    if (is_running(rule_dp))
      rule_dp = depth(this_box) - disp;

    rule_ht = rule_ht + rule_dp;

    if ((rule_ht > 0) && (rule_wd > 0))
    {
      synch_h();
      cur_v = base_line + rule_dp;
      synch_v();
      dvi_out(set_rule);
      dvi_four(rule_ht);
      dvi_four(rule_wd);
#ifndef APTEX_DVI_ONLY
      pdf_rule_out(rule_wd, rule_ht);
#endif
      cur_v = base_line;
      dvi_h = dvi_h + rule_wd;
    }

move_past:
    cur_h = cur_h + rule_wd;
    // @<Record horizontal |rule_node| or |glue_node| {\sl Sync\TeX} information@>
    synctex_horizontal_rule_or_glue(p, this_box);

next_p:
    prev_p = p;
    p = link(p);
  }

  if (eTeX_ex)
  {
    {
      while (info(LR_ptr) != before)
      {
        if (info(LR_ptr) > L_code)
          LR_problems = LR_problems + 10000;

        pop_LR();
      }

      pop_LR();
    }

    if (box_lr(this_box) == dlist)
      cur_dir = right_to_left;
  }

  synctex_tsilh(this_box);
  prune_movements(save_loc);

  if (cur_s > 0)
    dvi_pop(save_loc);

  decr(cur_s);
}

// output an |vlist_node| box
void vlist_out (void)
{
  scaled left_edge; // {the left coordinate for this box}
  scaled top_edge;  // {the top coordinate for this box}
  scaled save_h, save_v;  // {what |dvi_h| and |dvi_v| should pop to}
  pointer this_box; // {pointer to containing box}
  // glue_ord g_order;
  int g_order;  // {applicable order of infinity for glue}
  // char g_sign;
  int g_sign; // {selects type of glue}
  pointer p;  // {current position in the vlist}
  integer save_loc; // {\.{DVI} byte location upon entry}
  pointer leader_box; // {the leader box being replicated}
  scaled leader_ht; // {height of leader box being replicated}
  scaled lx;  // {extra space between leader boxes}
  boolean outer_doing_leaders;  // {were we doing leaders?}
  scaled edge;  // {bottom boundary of leader space}
  real glue_temp; // {glue value before rounding}
  real cur_glue;  // {glue seen so far}
  scaled cur_g; // {rounded equivalent of |cur_glue| times the glue ratio}
  integer save_dir; // {what |dvi_dir| should pop to}

  cur_g = 0;
  cur_glue = 0.0;
  this_box = temp_ptr;
  g_order = glue_order(this_box);
  g_sign = glue_sign(this_box);
  p = list_ptr(this_box);
  incr(cur_s);

  if (cur_s > 0)
    dvi_out(push);

  if (cur_s > max_push)
    max_push = cur_s;

  save_loc = dvi_offset + dvi_ptr;
  synch_dir();
  left_edge = cur_h;
  left_edge = cur_h;
  // @<Start vlist {\sl Sync\TeX} information record@>
  synctex_vlist(this_box);
  cur_v = cur_v - height(this_box);
  top_edge = cur_v;

  while (p != null)
  {
    /*
      @<Output node |p| for |vlist_out| and move to the next node,
      maintaining the condition |cur_h=left_edge|@>
    */
    if (is_char_node(p))
      confusion("vlistout");
    else
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
          // @<Output a box in a vlist@>
          if (list_ptr(p) == null)
          {
            cur_v = cur_v + height(p);

            if (type(p) != dir_node)
            {
              // @<Record void list {\sl Sync\TeX} information@>
              if (type(p) == vlist_node)
                synctex_void_vlist(p, this_box);
              else
                synctex_void_hlist(p, this_box);
            }

            cur_v = cur_v + depth(p);
          }
          else
          {
            cur_v = cur_v + height(p);
            synch_v();
            save_h = dvi_h;
            save_v = dvi_v;
            save_dir = dvi_dir;

            if (cur_dir == right_to_left)
              cur_h = left_edge - shift_amount(p);
            else
              cur_h = left_edge + shift_amount(p);  // {shift the box right}

            temp_ptr = p;

            switch (type(p))
            {
              case hlist_node:
                hlist_out();
                break;

              case vlist_node:
                vlist_out();
                break;

              case dir_node:
                dir_out();
                break;
            }

            dvi_h = save_h;
            dvi_v = save_v;
            dvi_dir = save_dir;
            cur_v = save_v + depth(p);
            cur_h = left_edge;
            cur_dir_hv = save_dir;
          }
          break;

        case rule_node:
          {
            rule_ht = height(p);
            rule_dp = depth(p);
            rule_wd = width(p);
            goto fin_rule;
          }
          break;

        case whatsit_node:
          // @<Output the whatsit node |p| in a vlist@>
          out_what(p);
          break;

        case glue_node:
          // @<Move down or output leaders@>
          {
            g = glue_ptr(p);
            rule_ht = width(g) - cur_g;

            if (g_sign != normal)
            {
              if (g_sign == stretching)
              {
                if (stretch_order(g) == g_order)
                {
                  cur_glue = cur_glue + stretch(g);
                  vet_glue(glue_set(this_box) * cur_glue);
                  cur_g = round(glue_temp);
                }
              }
              else if (shrink_order(g) == g_order)
              {
                cur_glue = cur_glue - shrink(g);
                vet_glue(glue_set(this_box) * cur_glue);
                cur_g = round(glue_temp);
              }
            }

            rule_ht = rule_ht + cur_g;

            /*
              @<Output leaders in a vlist, |goto fin_rule| if a rule
              or to |next_p| if done@>
            */
            if (subtype(p) >= a_leaders)
            {
              leader_box = leader_ptr(p);

              if (type(leader_box) == rule_node)
              {
                rule_wd = width(leader_box);
                rule_dp = 0;
                goto fin_rule;
              }

              leader_ht = height(leader_box) + depth(leader_box);

              if ((leader_ht > 0) && (rule_ht > 0))
              {
                rule_ht = rule_ht + 10; // {compensate for floating-point rounding}
                edge = cur_v + rule_ht;
                lx = 0;

                /*
                  @<Let |cur_v| be the position of the first box, and set |leader_ht+lx|
                  to the spacing between corresponding parts of boxes@>
                */
                if (subtype(p) == a_leaders)
                {
                  save_v = cur_v;
                  cur_v = top_edge + leader_ht * ((cur_v - top_edge) / leader_ht);

                  if (cur_v < save_v)
                    cur_v = cur_v + leader_ht;
                }
                else
                {
                  lq = rule_ht / leader_ht; // {the number of box copies}
                  lr = rule_ht % leader_ht; // {the remaining space}

                  if (subtype(p) == c_leaders)
                    cur_v = cur_v + (lr / 2);
                  else
                  {
                    lx = lr / (lq + 1);
                    cur_v = cur_v + ((lr - (lq - 1) * lx) / 2);
                  }
                }

                /*
                  @<Output a leader box at |cur_v|,
                  then advance |cur_v| by |leader_ht+lx|@>
                */
                while (cur_v + leader_ht <= edge)
                {
                  if (cur_dir == right_to_left)
                    cur_h = left_edge - shift_amount(leader_box);
                  else
                    cur_h = left_edge + shift_amount(leader_box);

                  synch_h();
                  save_h = dvi_h;
                  cur_v = cur_v + height(leader_box);
                  synch_v();
                  save_v = dvi_v;
                  save_dir = dvi_dir;
                  temp_ptr = leader_box;
                  outer_doing_leaders = doing_leaders;
                  doing_leaders = true;

                  switch (type(leader_box))
                  {
                    case hlist_node:
                      hlist_out();
                      break;

                    case vlist_node:
                      vlist_out();
                      break;

                    case dir_node:
                      dir_out();
                      break;
                  }

                  doing_leaders = outer_doing_leaders;
                  dvi_v = save_v;
                  dvi_h = save_h;
                  dvi_dir = save_dir;
                  cur_h = left_edge;
                  cur_v = save_v - height(leader_box) + leader_ht + lx;
                  cur_dir_hv = save_dir;
                }

                cur_v = edge - 10;
                goto next_p;
              }
            }

            goto move_past;
          }
          break;

        case kern_node:
          cur_v = cur_v + width(p);
          break;

        default:
          do_nothing();
          break;
      }

      goto next_p;

fin_rule:
      // @<Output a rule in a vlist, |goto next_p|@>
      if (is_running(rule_wd))
        rule_wd = width(this_box);

      rule_ht = rule_ht + rule_dp;  // {this is the rule thickness}
      cur_v = cur_v + rule_ht;

      if ((rule_ht > 0) && (rule_wd > 0)) // {we don't output empty rules}
      {
        if (cur_dir == right_to_left)
          cur_h = cur_h - rule_wd;

        synch_h();
        synch_v();
        dvi_out(put_rule);
        dvi_four(rule_ht);
        dvi_four(rule_wd);
#ifndef APTEX_DVI_ONLY
        pdf_rule_out(rule_wd, rule_ht);
#endif
        cur_h = left_edge;
      }

      goto next_p;

move_past:
      cur_v = cur_v + rule_ht;
    }

next_p:
    p = link(p);
  }

  // @<Finish vlist {\sl Sync\TeX} information record@>
  synctex_tsilv(this_box);
  prune_movements(save_loc);

  if (cur_s > 0)
    dvi_pop(save_loc);

  decr(cur_s);
}

void dir_out (void)
{
  pointer this_box; // {pointer to containing box}

  this_box = temp_ptr;
  temp_ptr = list_ptr(this_box);

  if ((type(temp_ptr) != hlist_node) && (type(temp_ptr) != vlist_node))
    confusion("dir_out");

  switch (box_dir(this_box))
  {
    case dir_yoko:
      switch (abs(box_dir(temp_ptr)))
      {
        case dir_tate:
          // {Tate in Yoko}
          {
            cur_v = cur_v - height(this_box);
            cur_h = cur_h + depth(temp_ptr);
          }
          break;

        case dir_dtou:
          // {DtoU in Yoko}
          {
            cur_v = cur_v + depth(this_box);
            cur_h = cur_h + height(temp_ptr);
          }
          break;
      }
      break;

    case dir_tate:
      switch (abs(box_dir(temp_ptr)))
      {
        case dir_yoko:
          // {Yoko in Tate}
          {
            cur_v = cur_v + depth(this_box);
            cur_h = cur_h + height(temp_ptr);
          }
          break;

        case dir_dtou:
          // {DtoU in Tate}
          {
            cur_v = cur_v + depth(this_box) - height(temp_ptr);
            cur_h = cur_h + width(temp_ptr);
          }
          break;
      }
      break;

    case dir_dtou:
      switch (abs(box_dir(temp_ptr)))
      {
        case dir_yoko:
          // {Yoko in DtoU}
          {
            cur_v = cur_v - height(this_box);
            cur_h = cur_h + depth(temp_ptr);
          }
          break;

        case dir_tate:
          // {Tate in DtoU}
          {
            cur_v = cur_v + depth(this_box) - height(temp_ptr);
            cur_h = cur_h + width(temp_ptr);
          }
          break;
      }
      break;
  }

  cur_dir_hv = abs(box_dir(temp_ptr));

  if (type(temp_ptr) == vlist_node)
    vlist_out();
  else
    hlist_out();
}

static void special_out (pointer p)
{
  char old_setting;
  pool_pointer k;

  synch_h();
  synch_v();
  old_setting = selector;
  selector = new_string;

#ifdef APTEX_EXTENSION
  if (pool_ptr + 32000 > current_pool_size)
    str_pool = realloc_str_pool (increment_pool_size);

  show_token_list(link(write_tokens(p)), 0, 10000000L);
#else
  show_token_list(link(write_tokens(p)), 0, pool_size - pool_ptr);
#endif

  selector = old_setting;
  str_room(1);

  if (cur_length < 256)
  {
    dvi_out(xxx1);
    dvi_out(cur_length);
  }
  else
  {
    dvi_out(xxx4);
    dvi_four(cur_length);
  }

  for (k = str_start[str_ptr]; k <= pool_ptr - 1; k++)
    dvi_out(str_pool[k]);
#ifndef APTEX_DVI_ONLY
  {
    const char * spc_str = (const char *) str_pool + str_start[str_ptr];
    scaled spc_h, spc_v;

    switch (cur_dir_hv)
    {
      case dir_yoko:
        spc_h = cur_h;
        spc_v = -cur_v;
        break;

      case dir_tate:
        spc_h = -cur_v;
        spc_v = -cur_h;
        break;

      case dir_dtou:
        spc_h = cur_v;
        spc_v = cur_h;
        break;
    }

    {
      int is_drawable = 0;
      pdf_rect rect = {0.0, 0.0, 0.0, 0.0};

      graphics_mode();
      spc_moveto(cur_h * sp2bp / 1.5202, cur_v * sp2bp / 1.5202);
      spc_exec_special(spc_str, cur_length,
        spc_h * sp2bp, spc_v * sp2bp, mag / 1000.0, &is_drawable, &rect);
    }
  }
#endif
  pool_ptr = str_start[str_ptr];
}

static void write_out (pointer p)
{
  char old_setting;
  integer old_mode;
  /* small_number j; */
  int j;
  pointer q, r;
  integer d;
  boolean clobbered;
  integer runsystem_ret;

  q = get_avail();
  info(q) = right_brace_token + '}';
  r = get_avail();
  link(q) = r;
  info(r) = end_write_token;
  ins_list(q);
  begin_token_list(write_tokens(p), write_text);
  q = get_avail();
  info(q) = left_brace_token + '{';
  ins_list(q);
  old_mode = mode;
  mode = 0;
  cur_cs = write_loc;
  q = scan_toks(false, true);
  get_token();

  if (cur_tok != end_write_token)
  {
    print_err("Unbalanced write command");
    help2("On this page there's a \\write with fewer real {'s than }'s.",
        "I can't handle that very well; good luck.");
    error();

    do {
      get_token();
    } while (!(cur_tok == end_write_token));
  }

  mode = old_mode;
  end_token_list();
  old_setting = selector;
  j = write_stream(p);

  if (j == 18)
    selector = new_string;
  else if (write_open[j])
    selector = j;
  else
  {
    if ((j == 17) && (selector == term_and_log))
      selector = log_only;

    print_nl("");
  }

  token_show(def_ref);
  print_ln();
  flush_list(def_ref);
  if (j == 18)
  {
    if (tracing_online <= 0)
      selector = log_only;  //{Show what we're doing in the log file.}
    else
      selector = term_and_log; //{Show what we're doing.}
    //{If the log file isn't open yet, we can only send output to the terminal.
    // Calling |open_log_file| from here seems to result in bad data in the log.}
    if (!log_opened)
      selector = term_only;
    print_nl("runsystem(");
    for (d = 0; d <= cur_length - 1; d++)
    {
      //{|print| gives up if passed |str_ptr|, so do it by hand.}
      print(str_pool[str_start[str_ptr] + d]); //{N.B.: not |print_char|}
    }
    prints(")...");
    if (aptex_env.flag_shell_escape)
    {
      str_room(1);
      append_char(0); //{Append a null byte to the expansion.}
      clobbered = false;
      for (d = 0; d <= cur_length - 1; d++) //{Convert to external character set.}
      {
        str_pool[str_start[str_ptr] + d] = xchr[str_pool[str_start[str_ptr] + d]];
        if ((str_pool[str_start[str_ptr] + d] == null_code) && (d < cur_length - 1))
          clobbered = true;
        //{minimal checking : NUL not allowed in argument string of |system|()}
      }
      if (clobbered)
        prints("clobbered");
      else  /*{We have the command. See if we're allowed to execute it,
             and report in the log. We don't check the actual exit status of
             the command, or do anything with the output.}*/
      {
        char * shell_cmd = calloc(cur_length, 1);
        strncpy(shell_cmd, (char *) str_pool + str_start[str_ptr], cur_length);
        runsystem_ret = system(shell_cmd);
        switch (runsystem_ret)
        {
          case -1:
            prints("quotation error in system command");
            break;
          case 0:
            prints("disabled (restricted)");
            break;
          case 1:
            prints("executed");
            break;
          case 2:
            prints("executed safely (allowed)");
            break;
        }
        free(shell_cmd);
      }
    }
    else
    {
      prints("disabled"); //{|shellenabledp| false}
    }
    print_char('.');
    print_nl("");
    print_ln();
    pool_ptr = str_start[str_ptr]; //{erase the string}
  }
  selector = old_setting;
}

void out_what (pointer p)
{
  /* small_number j; */
  int j;

  switch (subtype(p))
  {
    case open_node:
    case write_node:
    case close_node:
      if (!doing_leaders)
      {
        j = write_stream(p);

        if (subtype(p) == write_node)
          write_out(p);
        else
        {
          if (write_open[j])
            a_close(write_file[j]);

          if (subtype(p) == close_node)
            write_open[j] = false;
          else if (j < 16)
          {
            cur_name = open_name(p);
            cur_area = open_area(p);
            cur_ext = open_ext(p); 

            if (cur_ext == 335) /* "" */
              cur_ext = 785;    /* ".tex" */

            pack_cur_name();

            while (!a_open_out(write_file[j]))
              prompt_file_name("output file name", ".tex");

            write_open[j] = true;
          }
        }
      }
      break;

    case special_node:
      special_out(p);
      break;

    case language_node:
      do_nothing();
      break;

    case pdf_save_pos_node:
      {
        switch (dvi_dir)
        {
          case dir_yoko:
            {
              pdf_last_x_pos = cur_h;
              pdf_last_y_pos = cur_v;
            }
            break;

          case dir_tate:
            {
              pdf_last_x_pos = -cur_v;
              pdf_last_y_pos = cur_h;
            }
            break;

          case dir_dtou:
            {
              pdf_last_x_pos = cur_v;
              pdf_last_y_pos = -cur_h;
            }
            break;
        }

        pdf_last_x_pos = pdf_last_x_pos + 4736286;
        pdf_last_y_pos = cur_page_height - pdf_last_y_pos - 4736286;
      }
      break;

    default:
      confusion("ext4");
      break;
  }
}

// scans a box specification and left brace
static void scan_spec (group_code c, boolean three_codes)
{
  integer s;
  char spec_code;

  if (three_codes)
    s = saved(0);

  if (scan_keyword("to"))
    spec_code = exactly;
  else if (scan_keyword("spread"))
    spec_code = additional;
  else
  {
    spec_code = additional;
    cur_val = 0;
    goto found;
  }

  scan_normal_dimen();

found:
  if (three_codes)
  {
    saved(0) = s;
    incr(save_ptr);
  }

  saved(0) = spec_code;
  saved(1) = cur_val;
  save_ptr = save_ptr + 2;
  new_save_level(c);
  scan_left_brace();
}

static pointer hpack (pointer p, scaled w, small_number m)
{
  pointer r;              // {the box node that will be returned}
  pointer k;              // {points to a |kanji_space| specification}
  scaled disp;            // {displacement}
  pointer q;              // {trails behind |p|}
  scaled h, d, x;         // {height, depth, and natural width}
  scaled s;               // {shift amount}
  pointer g;              // {points to a glue specification}
  glue_ord o;             // {order of infinity}
  internal_font_number f; //  {the font in a |char_node|}
  four_quarters i;        // {font information about a |char_node|}
  eight_bits hd;          // {height and depth indices for a character}

  last_badness = 0;
  r = get_node(box_node_size);
  type(r) = hlist_node;
  subtype(r) = min_quarterword;
  shift_amount(r) = 0;
  set_box_dir(r, dir_default);
  space_ptr(r) = cur_kanji_skip;
  xspace_ptr(r) = cur_xkanji_skip;
  add_glue_ref(cur_kanji_skip);
  add_glue_ref(cur_xkanji_skip);
  k = cur_kanji_skip;
  q = r + list_offset;
  link(q) = p;
  h = 0;
  d = 0;
  x = 0;
  total_stretch[normal] = 0;
  total_shrink[normal] = 0;
  total_stretch[fil] = 0;
  total_shrink[fil] = 0;
  total_stretch[fill] = 0;
  total_shrink[fill] = 0;
  total_stretch[filll] = 0;
  total_shrink[filll] = 0;
  disp = 0;

  if (TeXXeT_en)
    put_LR(before);

  while (p != null)
  {
reswitch:
    chain = false;

    while (is_char_node(p))
    {
      f = font(p);
      i = char_info(f, character(p));
      hd = height_depth(i);
      x = x + char_width(f, i);
      s = char_height(f, hd) - disp;

      if (s > h)
        h = s;

      s = char_depth(f, hd) + disp;

      if (s > d)
        d = s;

      if (font_dir[f] != dir_default)
      {
        p = link(p);

        if (chain)
        {
          x = x + width(k);
          o = stretch_order(k);
          total_stretch[o] = total_stretch[o] + stretch(k);
          o = shrink_order(k);
          total_shrink[o] = total_shrink[o] + shrink(k);
        }
        else
          chain = true;
      }
      else
        chain = false;

      p = link(p);
    }

    if (p != null)
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case rule_node:
        case unset_node:
          {
            x = x + width(p);

            if (type(p) >= rule_node)
              s = disp;
            else
              s = shift_amount(p) + disp;

            if (height(p) - s > h)
              h = height(p) - s;

            if (depth(p) + s > d)
              d = depth(p) + s;
          }
          break;

        case ins_node:
        case mark_node:
        case adjust_node:
          if (adjust_tail != null)
          {
            while (link(q) != p)
              q = link(q);

            if (type(p) == adjust_node)
            {
              link(adjust_tail) = adjust_ptr(p);

              while (link(adjust_tail) != null)
                adjust_tail = link(adjust_tail);

              p = link(p);
              free_node(link(q), small_node_size);
            }
            else
            {
              link(adjust_tail) = p;
              adjust_tail = p;
              p = link(p);
            }

            link(q) = p;
            p = q;
          }
          break;

        case whatsit_node:
          do_nothing();
          break;

        case disp_node:
          {
            disp = disp_dimen(p);
            revdisp = disp;
          }
          break;

        case glue_node:
          {
            g = glue_ptr(p);
            x = x + width(g);
            o = stretch_order(g);
            total_stretch[o] = total_stretch[o] + stretch(g);
            o = shrink_order(g);
            total_shrink[o] = total_shrink[o] + shrink(g);

            if (subtype(p) >= a_leaders)
            {
              g = leader_ptr(p);

              if (height(g) > h)
                h = height(g);

              if (depth(g) > d)
                d = depth(g);
            }
          }
          break;

        case kern_node:
          x = x + width(p);
          break;

        case math_node:
          {
            x = x + width(p);

            if (TeXXeT_en)
            {
              if (end_LR(p))
              {
                if (info(LR_ptr) == end_LR_type(p))
                  pop_LR();
                else
                {
                  incr(LR_problems);
                  type(p) = kern_node;
                  subtype(p) = explicit;
                }
              }
              else
                push_LR(p);
            }
          }
          break;

        case ligature_node:
          {
            mem[lig_trick] = mem[lig_char(p)];
            link(lig_trick) = link(p);
            p = lig_trick;
            goto reswitch;
          }
          break;

        default:
          do_nothing();
          break;
      }

      p = link(p);
    }
  }

  if (adjust_tail != null)
    link(adjust_tail) = null;

  height(r) = h;
  depth(r) = d;

  if (m == additional)
    w = x + w;

  width(r) = w;
  x = w - x;

  if (x == 0)
  {
    glue_sign(r) = normal;
    glue_order(r) = normal;
    set_glue_ratio_zero(glue_set(r));
    goto exit;
  }
  else if (x > 0)
  {
    if (total_stretch[filll] != 0)
      o = filll;
    else if (total_stretch[fill] != 0)
      o = fill;
    else if (total_stretch[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = stretching;

    if (total_stretch[o] != 0)
      glue_set(r) = x / ((real) total_stretch[o]);
    else
    {
      glue_sign(r) = normal;
      set_glue_ratio_zero(glue_set(r));
    }

    if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(x, total_stretch[normal]);

        if (last_badness > hbadness)
        {
          print_ln();

          if (last_badness > 100)
            print_nl("Underfull");
          else
            print_nl("Loose");

          prints(" \\hbox (badness ");
          print_int(last_badness);

          if (last_badness > 100)
            hps_underfull++;

          goto common_ending;
        }
      }

    goto exit;
  }
  else
  {
    if (total_shrink[filll] != 0)
      o = filll;
    else if (total_shrink[fill] != 0)
      o = fill;
    else if (total_shrink[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = shrinking;

    if (total_shrink[o] != 0)
      glue_set(r) = ((-x) / ((real) total_shrink[o]));
    else
    {
      glue_sign(r) = normal;
      set_glue_ratio_zero(glue_set(r));
    }

    if ((total_shrink[o] < -x) && (o == normal) && (list_ptr(r) != 0))
    {
      last_badness = 1000000;
      glue_set(r) = 1.0;

      if ((-x - total_shrink[normal] > hfuzz) || (hbadness < 100))
      {
        if ((overfull_rule > 0) && (-x - total_shrink[normal] > hfuzz))
        {
          while (link(q) != null)
            q = link(q);
          
          link(q) = new_rule();
          width(link(q)) = overfull_rule;
        }
        
        print_ln();
        print_nl("Overfull \\hbox (");
        print_scaled(-x - total_shrink[normal]);
        prints("pt too wide");
        
        hps_overfull++;
        goto common_ending;
      }
    }
    else if (o == normal)
      if (list_ptr(r) != null)
      {
        last_badness = badness(-x, total_shrink[normal]);

        if (last_badness > hbadness)
        {
          print_ln();
          print_nl("Tight \\hbox (badness ");
          print_int(last_badness);
          goto common_ending;
        }
      }

    goto exit;
  }

common_ending:
  if (output_active)
    prints(") has occurred while \\output is active");
  else
  {
    if (pack_begin_line != 0)
    {
      if (pack_begin_line > 0)
        prints(") in paragraph at lines ");
      else
        prints(") in alignment at lines ");

      print_int(abs(pack_begin_line));
      prints("--");
    }
    else
      prints(") detected at line ");

    print_int(line);
  }

  print_ln();
  font_in_short_display = null_font;
  short_display(list_ptr(r));
  print_ln();
  begin_diagnostic();
  show_box(r);
  end_diagnostic(true);

exit:
  last_disp = disp;

  if (TeXXeT_en)
  {
    if (info(LR_ptr) != before)
    {
      while (link(q) != null)
        q = link(q);

      do {
        temp_ptr = q;
        q = new_math(0, info(LR_ptr));
        link(temp_ptr) = q;
        LR_problems = LR_problems + 10000;
        pop_LR();
      } while (!(info(LR_ptr) == before));
    }

    if (LR_problems > 0)
    {
      report_LR_problems();
      goto common_ending;
    }

    pop_LR();

    if (LR_ptr != null)
      confusion("LR1");
  }

  return r;
}

static pointer vpackage (pointer p, scaled h, small_number m, scaled l)
{
  pointer r;  // {the box node that will be returned}
  scaled w, d, x; // {width, depth, and natural height}
  scaled s; // {shift amount}
  pointer g;  // {points to a glue specification}
  glue_ord o; // {order of infinity}

  last_badness = 0;
  r = get_node(box_node_size);
  type(r) = vlist_node;
  subtype(r) = min_quarterword;
  shift_amount(r) = 0;
  set_box_dir(r, dir_default);
  space_ptr(r) = zero_glue;
  xspace_ptr(r) = zero_glue;
  add_glue_ref(zero_glue);
  add_glue_ref(zero_glue);
  list_ptr(r) = p;
  w = 0;
  d = 0;
  x = 0;
  total_stretch[normal] = 0;
  total_shrink[normal] = 0;
  total_stretch[fil] = 0;
  total_shrink[fil] = 0;
  total_stretch[fill] = 0;
  total_shrink[fill] = 0;
  total_stretch[filll] = 0;
  total_shrink[filll] = 0;

  while (p != null)
  {
    if (is_char_node(p))
      confusion("vpack");
    else switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case rule_node:
      case unset_node:
        {
          x = x + d + height(p);
          d = depth(p);

          if (type(p) >= rule_node)
            s = 0;
          else
            s = shift_amount(p);

          if (width(p) + s > w)
            w = width(p) + s;
        }
        break;

      case whatsit_node:
        do_nothing();
        break;

      case glue_node:
        {
          x = x + d;
          d = 0;
          g = glue_ptr(p);
          x = x + width(g);
          o = stretch_order(g);
          total_stretch[o] = total_stretch[o] + stretch(g);
          o = shrink_order(g);
          total_shrink[o] = total_shrink[o] + shrink(g);

          if (subtype(p) >= a_leaders)
          {
            g = leader_ptr(p);

            if (width(g) > w)
              w = width(g);
          }
        }
        break;

      case kern_node:
        {
          x = x + d + width(p);
          d = 0;
        }
        break;

      default:
        do_nothing();
        break;
    }

    p = link(p);
  }

  width(r) = w;

  if (d > l)
  {
    x = x + d - l;
    depth(r) = l;
  }
  else
    depth(r) = d;

  if (m == additional)
    h = x + h;

  height(r) = h;
  x = h - x;

  if (x == 0)
  {
    glue_sign(r) = normal;
    glue_order(r) = normal;
    set_glue_ratio_zero(glue_set(r));
    goto exit;
  }
  else if (x > 0)
  {
    if (total_stretch[filll] != 0)
      o = filll;
    else if (total_stretch[fill] != 0)
      o = fill;
    else if (total_stretch[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = stretching;

    if (total_stretch[o] != 0)
      glue_set(r) = x / ((real) total_stretch[o]);
    else
    {
      glue_sign(r) = normal;
      set_glue_ratio_zero(glue_set(r));
    }

    if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(x, total_stretch[normal]);

        if (last_badness > vbadness)
        {
          print_ln();

          if (last_badness > 100)
            print_nl("Underfull");
          else
            print_nl("Loose");

          prints(" \\vbox (badness ");
          print_int(last_badness);

          if (last_badness > 100)
            vps_underfull++;

          goto common_ending;
        }
      }

    goto exit;
  }
  else
  {
    if (total_shrink[filll] != 0)
      o = filll;
    else if (total_shrink[fill] != 0)
      o = fill;
    else if (total_shrink[fil] != 0)
      o = fil;
    else
      o = normal;

    glue_order(r) = o;
    glue_sign(r) = shrinking;

    if (total_shrink[o] != 0)
      glue_set(r) = (-x) / ((real) total_shrink[o]);
    else
    {
      glue_sign(r) = normal;
      glue_set(r) = 0.0;
    }

    if ((total_shrink[o] < -x) && (o == 0) && (list_ptr(r) != 0))
    {
      last_badness = 1000000;
      set_glue_ratio_one(glue_set(r));

      if ((-x - total_shrink[0] > vfuzz) || (vbadness < 100))
      {
        print_ln();
        print_nl("Overfull \\vbox (");
        print_scaled(-x - total_shrink[0]);
        prints("pt too high");

        vps_overfull++;

        goto common_ending;
      }
    }
    else if (o == normal)
      if (list_ptr(r) != 0)
      {
        last_badness = badness(-x, total_shrink[normal]);

        if (last_badness > vbadness)
        {
          print_ln();
          print_nl("Tight \\vbox (badness ");
          print_int(last_badness);
          goto common_ending;
        }
      }

    goto exit;
  }

common_ending:
  if (output_active)
    prints(") has occurred while \\output is active");
  else
  {
    if (pack_begin_line != 0)
    {
      prints(") in alignment at lines ");
      print_int(abs(pack_begin_line));
      prints("--");
    }
    else
      prints(") detected at line ");

    print_int(line);
    print_ln();
  }

  begin_diagnostic();
  show_box(r);
  end_diagnostic(true);

exit:
  return r;
}

static void append_to_vlist (pointer b)
{
  scaled d;
  pointer p;

  if (prev_depth > ignore_depth)
  {
    d = width(baseline_skip) - prev_depth - height(b);

    if (d < line_skip_limit)
      p = new_param_glue(line_skip_code);
    else
    {
      p = new_skip_param(baseline_skip_code);
      width(temp_ptr) = d;
    }

    link(tail) = p;
    tail = p;
  }

  link(tail) = b;
  tail = b;
  prev_depth = depth(b);
}

static pointer new_noad (void)
{
  pointer p;

  p = get_node(noad_size);
  type(p) = ord_noad;
  subtype(p) = normal;
  mem[nucleus(p)].hh = empty_field;
  mem[subscr(p)].hh = empty_field;
  mem[supscr(p)].hh = empty_field;
  mem[kcode_noad(p)].hh = empty_field;

  return p;
}

// create a style node
static pointer new_style (small_number s)
{
  pointer p;  // {the new node}

  p = get_node(style_node_size);
  type(p) = style_node;
  subtype(p) = s;
  width(p) = 0;
  depth(p) = 0; // {the |width| and |depth| are not used}

  return p;
}

// create a choice node
static pointer new_choice (void)
{
  pointer p;  // {the new node}

  p = get_node(style_node_size);
  type(p) = choice_node;
  subtype(p) = 0; // {the |subtype| is not used}
  display_mlist(p) = 0;
  text_mlist(p) = 0;
  script_mlist(p) = 0;
  script_script_mlist(p) = 0;

  return p;
}

// the reader will kindly forgive this
void show_info (void)
{
  show_node_list(info(temp_ptr));
}

// construct the bar for a fraction
static pointer fraction_rule (scaled t)
{
  pointer p;  // {the new node}

  p = new_rule();
  height(p) = t;
  depth(p) = 0;

  return p;
}

static pointer overbar (pointer b, scaled k, scaled t)
{
  pointer p, q; // {nodes being constructed}

  p = new_kern(k);
  link(p) = b;
  q = fraction_rule(t);
  link(q) = p;
  p = new_kern(t);
  link(p) = q;

  return vpackage(p, 0, 1, max_dimen);
}

static pointer char_box (internal_font_number f, quarterword c)
{
  four_quarters q;
  eight_bits hd;  // {|height_depth| byte}
  pointer b, p;   // {the new box and its character node}

  q = char_info(f, c);
  hd = height_depth(q);
  b = new_null_box();
  width(b) = char_width(f, q) + char_italic(f, q);
  height(b) = char_height(f, hd);
  depth(b) = char_depth(f, hd);
  p = get_avail();
  character(p) = c;
  font(p) = f;
  list_ptr(b) = p;

  return b;
}

static void stack_into_box (pointer b, internal_font_number f, quarterword c)
{
  pointer p;  // {new node placed into |b|}

  p = char_box(f, c);
  link(p) = list_ptr(b);
  list_ptr(b) = p;
  height(b) = height(p);
}

static scaled height_plus_depth (internal_font_number f, quarterword c)
{
  four_quarters q;
  eight_bits hd;  // {|height_depth| byte}

  q = char_info(f, c);
  hd = height_depth(q);

  return char_height(f, hd) + char_depth(f, hd);
}

static pointer var_delimiter (pointer d, small_number s, scaled v)
{
  pointer b;                // {the box that will be constructed}
  internal_font_number f, g;// {best-so-far and tentative font codes}
  quarterword c, x, y;      // {best-so-far and tentative character codes}
  integer m, n;             // {the number of extensible pieces}
  scaled u;                 // {height-plus-depth of a tentative character}
  scaled w;                 // {largest height-plus-depth so far}
  four_quarters q;          // {character info}
  eight_bits hd;            // {height-depth byte}
  four_quarters r;          // {extensible pieces}
  small_number z;           // {runs through font family members}
  boolean large_attempt;    // {are we trying the ``large'' variant?}

  f = null_font;
  w = 0;
  large_attempt = false;
  z = small_fam(d);
  x = small_char(d);

  /*
    @<Look at the variants of |(z,x)|; set |f| and |c| whenever
    a better character is found; |goto found| as soon as a
    large enough variant is encountered@>;
  */
  while (true)
  {
    if ((z != 0) || (x != min_quarterword))
    {
      z = z + s + 16;

      do {
        z = z - 16;
        g = fam_fnt(z);

        /*
          @<Look at the list of characters starting with |x| in
          font |g|; set |f| and |c| whenever
          a better character is found; |goto found| as soon as a
          large enough variant is encountered@>;
        */

        if (g != null_font)
        {
          y = x;

          if ((y >= font_bc[g]) && (y <= font_ec[g]))
          {
continu:
            q = char_info(g, y);
            
            if (char_exists(q))
            {
              if (char_tag(q) == ext_tag)
              {
                f = g;
                c = y;
                goto found;
              }

              hd = height_depth(q);
              u = char_height(g, hd) + char_depth(g, hd);

              if (u > w)
              {
                f = g;
                c = y;
                w = u;

                if (u >= v)
                  goto found;
              }

              if (char_tag(q) == list_tag)
              {
                y = rem_byte(q);
                goto continu;
              }
            }
          }
        }
      } while (!(z < 16));
    }

    if (large_attempt)
      goto found; // {there were none large enough}

    large_attempt = true;
    z = large_fam(d);
    x = large_char(d);
  }

found:
  // @<Make variable |b| point to a box for |(f,c)|@>
  if (f != null_font)
    if (char_tag(q) == ext_tag)
    {
      /*
        @<Construct an extensible character in a new box |b|,
        using recipe |rem_byte(q)| and font |f|@>
      */
      b = new_null_box();
      type(b) = vlist_node;
      r = font_info[exten_base[f] + rem_byte(q)].qqqq;
      /*
        @<Compute the minimum suitable height, |w|, and the corresponding
        number of extension steps, |n|; also set |width(b)|@>;
      */
      c = ext_rep(r);
      u = height_plus_depth(f, c);
      w = 0;
      q = char_info(f, c);
      width(b) = char_width(f, q) + char_italic(f, q);
      c = ext_bot(r);

      if (c != min_quarterword)
        w = w + height_plus_depth(f, c);

      c = ext_mid(r);

      if (c != min_quarterword)
        w = w + height_plus_depth(f, c);

      c = ext_top(r);

      if (c != min_quarterword)
        w = w + height_plus_depth(f, c);

      n = 0;

      if (u > 0)
        while (w < v)
        {
          w = w + u;
          incr(n);

          if (ext_mid(r) != min_quarterword)
            w = w + u;
        }

      c = ext_bot(r);

      if (c != min_quarterword)
        stack_into_box(b, f, c);

      c = ext_rep(r);

      for (m = 1; m <= n; m++)
        stack_into_box(b, f, c);

      c = ext_mid(r);

      if (c != min_quarterword)
      {
        stack_into_box(b, f, c);
        c = ext_rep(r);

        for (m = 1; m <= n; m++)
          stack_into_box(b, f, c);
      }

      c = ext_top(r);

      if (c != min_quarterword)
        stack_into_box(b, f, c);
      
      depth(b) = w - height(b);
    }
    else
      b = char_box(f, c);
  else
  {
    b = new_null_box();
    width(b) = null_delimiter_space;  // {use this width if no delimiter was found}
  }

  shift_amount(b) = half(height(b) - depth(b)) - axis_height(s);

  return b;
}

static pointer rebox (pointer b, scaled w)
{
  pointer p;              // {temporary register for list manipulation}
  internal_font_number f; // {font in a one-character box}
  scaled v;               // {width of a character without italic correction}

  if ((width(b) != w) && (list_ptr(b) != null))
  {
    if (type(b) != hlist_node)
      b = hpack(b, 0, 1);

    p = list_ptr(b);

    if (is_char_node(p))
    {
      if (font_dir[font(p)] != dir_default)
      {
        if (link(link(p)) == null)
        {
          f = font(p);
          v = char_width(f, char_info(f, character(p)));

          if (v != width(b))
            link(link(p)) = new_kern(width(b) - v);
        }
      }
      else if (link(p) == null)
      {
        f = font(p);
        v = char_width(f, char_info(f, character(p)));
        
        if (v != width(b))
          link(p) = new_kern(width(b) - v);
      }
    }

    delete_glue_ref(space_ptr(b));
    delete_glue_ref(xspace_ptr(b));
    free_node(b, box_node_size);
    b = new_glue(ss_glue);
    link(b) = p;

    while (link(p) != null)
      p = link(p);

    link(p) = new_glue(ss_glue);

    return hpack(b, w, exactly);
  }
  else
  {
    width(b) = w;

    return b;
  }
}

static pointer math_glue (pointer g, scaled m)
{
  pointer p;  // {the new glue specification}
  integer n;  // {integer part of |m|}
  scaled f;   // {fraction part of |m|}

  n = x_over_n(m, 0200000);
  f = ng_remainder;

  if (f < 0)
  {
    decr(n);
    f = f + 0200000;
  }

  p = get_node(glue_spec_size);
  width(p) = mu_mult(width(g)); // {convert \.{mu} to \.{pt}}
  stretch_order(p) = stretch_order(g);

  if (stretch_order(p) == normal)
    stretch(p) = mu_mult(stretch(g));
  else
    stretch(p) = stretch(g);

  shrink_order(p) = shrink_order(g);

  if (shrink_order(p) == normal)
    shrink(p) = mu_mult(shrink(g));
  else
    shrink(p) = shrink(g);

  return p;
}

static void math_kern (pointer p, scaled m)
{
  integer n;  // {integer part of |m|}
  scaled f;   // {fraction part of |m|}

  if (subtype(p) == mu_glue)
  {
    n = x_over_n(m, 0200000);
    f = ng_remainder;

    if (f < 0)
    {
      decr(n);
      f = f + 0200000;
    }

    width(p) = mu_mult(width(p));
    subtype(p) = explicit;
  }
}

static void flush_math (void)
{
  flush_node_list(link(head));
  flush_node_list(incompleat_noad);
  link(head) = null;
  tail = head;
  incompleat_noad = null;
}

// { We assume that |math_type(q)=sub_exp_box| }
static pointer shift_sub_exp_box (pointer q)
{
  halfword d; // {displacement}

  if (abs(direction) == abs(box_dir(info(q))))
  {
    if (abs(direction) == dir_tate)
    {
      if (box_dir(info(q)) == dir_tate)
        d = t_baseline_shift;
      else
        d = y_baseline_shift;
    }
    else
      d = y_baseline_shift;

    if (cur_style < script_style)
      d = xn_over_d(d, text_baseline_shift_factor, 1000);
    else if (cur_style < script_script_style)
      d = xn_over_d(d, script_baseline_shift_factor, 1000);
    else
      d = xn_over_d(d, scriptscript_baseline_shift_factor, 1000);

    shift_amount(info(q)) = shift_amount(info(q)) - d;
  }
  math_type(q) = sub_box;
  
  return info(q);
}

static pointer clean_box (pointer p, small_number s, halfword jc)
{
  pointer q;              // {beginning of a list to be boxed}
  small_number save_style;// {|cur_style| to be restored}
  pointer x;              // {box to be returned}
  pointer r;              // {temporary pointer}

  switch (math_type(p))
  {
    case math_char:
      {
        cur_mlist = new_noad();
        mem[nucleus(cur_mlist)] = mem[p];
      }
      break;

    case math_jchar:
      {
        cur_mlist = new_noad();
        mem[nucleus(cur_mlist)] = mem[p];
        math_kcode(cur_mlist) = jc;
      }
      break;

    case sub_box:
      {
        q = info(p);
        goto found;
      }
      break;

    case sub_exp_box:
      {
        q = shift_sub_exp_box(p);
        goto found;
      }
      break;

    case sub_mlist:
      cur_mlist = info(p);
      break;

    default:
      {
        q = new_null_box();
        goto found;
      }
      break;
  }

  save_style = cur_style;
  cur_style = s;
  mlist_penalties = false;
  mlist_to_hlist();
  q = link(temp_head);  // {recursive call}
  cur_style = save_style; // {restore the style}

  // @<Set up the values of |cur_size| and |cur_mu|, based on |cur_style|@>
  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

found:
  if (is_char_node(q) || (q == null))
    x = hpack(q, 0, 1);
  else if ((link(q) == null) && (type(q) <= dir_node) && (shift_amount(q) == 0))
    x = q;  // {it's already clean}
  else
    x = hpack(q, 0, 1);

  // @<Simplify a trivial box@>;
  q = list_ptr(x);

  if (is_char_node(q))
  {
    if (font_dir[font(q)] != dir_default)
      q = link(q);

    r = link(q);

    if (r != null)
      if (link(r) == null)
        if (!is_char_node(r))
          if (type(r) == kern_node) // {unneeded italic correction}
          {
            free_node(r, medium_node_size);
            link(q) = null;
          }
  }

  return x;
}

// unpack the |math_char| field |a|
static void fetch (pointer a)
{
  cur_c = character(a);
  cur_f = fam_fnt(fam(a) + cur_size);
  //  @<Complain about an undefined family and set |cur_i| null@>
  if (cur_f == null_font)
  {
    print_err("");
    print_size(cur_size);
    print_char(' ');
    print_int(fam(a));
    prints(" is undefined (character ");
    print(cur_c);
    print_char(')');
    help4("Somewhere in the math formula just ended, you used the",
        "stated character from an undefined font family. For example,",
        "plain TeX doesn't allow \\it or \\sl in subscripts. Proceed,",
        "and I'll try to forget that I needed that character.");
    error();
    cur_i = null_character;
    math_type(a) = empty;
  }
  else
  {
    if (font_dir[cur_f] != dir_default)
      cur_c = get_jfm_pos(KANJI(math_kcode_nucleus(a)), cur_f);

    if ((cur_c >= font_bc[cur_f]) && (cur_c <= font_ec[cur_f]))
      cur_i = char_info(cur_f, cur_c);
    else
      cur_i = null_character;

    if (!char_exists(cur_i))
    {
      char_warning(cur_f, cur_c);
      math_type(a) = empty;
      cur_i = null_character;
    }
  }
}

static void make_over (pointer q)
{
  info(nucleus(q)) = overbar(clean_box(nucleus(q), cramped_style(cur_style), math_kcode(q)),
      3 * default_rule_thickness, default_rule_thickness);
  math_type(nucleus(q)) = sub_box;
}

static void make_under (pointer q)
{
  pointer p, x, y;  // {temporary registers for box construction}
  scaled delta;     // {overall height plus depth}

  x = clean_box(nucleus(q), cur_style, math_kcode(q));
  p = new_kern(3 * default_rule_thickness);
  link(x) = p;
  link(p) = fraction_rule(default_rule_thickness);
  y = vpackage(x, 0, 1, max_dimen);
  delta = height(y) + depth(y) + default_rule_thickness;
  height(y) = height(x);
  depth(y) = delta - height(y);
  info(nucleus(q)) = y;
  math_type(nucleus(q)) = sub_box;
}

static void make_vcenter (pointer q)
{ 
  pointer v;    // {the box that should be centered vertically}
  scaled delta; // {its height plus depth}

  v = info(nucleus(q));

  if (type(v) == dir_node)
  {
    if (type(list_ptr(v)) != vlist_node)
      confusion("dircenter");
  }
  else
  {
    if (type(v) != vlist_node)
      confusion("vcenter");
  }

  delta = height(v) + depth(v);
  height(v) = axis_height(cur_size) + half(delta);
  depth(v) = delta - height(v);
}

static void make_radical (pointer q)
{
  pointer x, y;       // {temporary registers for box construction}
  scaled delta, clr;  // {dimensions involved in the calculation}

  x = clean_box(nucleus(q), cramped_style(cur_style), math_kcode(q));

  if (cur_style < text_style)
    clr = default_rule_thickness + (abs(math_x_height(cur_size)) / 4);
  else
  {
    clr = default_rule_thickness;
    clr = clr + (abs(clr) / 4);
  }

  y = var_delimiter(left_delimiter(q), cur_size, height(x) + depth(x) + clr + default_rule_thickness);
  delta = depth(y) - (height(x) + depth(x) + clr);

  if (delta > 0)
    clr = clr + half(delta);  // {increase the actual clearance}

  shift_amount(y) = -(height(x) + clr);
  link(y) = overbar(x, clr, height(y));
  info(nucleus(q)) = hpack(y, 0, 1);
  math_type(nucleus(q)) = sub_box;
}

static void make_math_accent (pointer q)
{
  pointer p, x, y;        // {temporary registers for box construction}
  integer a;              // {address of lig/kern instruction}
  quarterword c;          // {accent character}
  internal_font_number f; // {its font}
  four_quarters i;        // {its |char_info|}
  scaled s;               // {amount to skew the accent to the right}
  scaled h;               // {height of character being accented}
  scaled delta;           // {space to remove between accent and accentee}
  scaled w;               // {width of the accentee, not including sub/superscripts}

  fetch(accent_chr(q));

  if (char_exists(cur_i))
  {
    i = cur_i;
    c = cur_c;
    f = cur_f;

    // @<Compute the amount of skew@>;
    s = 0;

    if (math_type(nucleus(q)) == math_char)
    {
      fetch(nucleus(q));

      if (char_tag(cur_i) == lig_tag)
      {
        a = lig_kern_start(cur_f, cur_i);
        cur_i = font_info[a].qqqq;

        if (skip_byte(cur_i) > stop_flag)
        {
          a = lig_kern_restart(cur_f, cur_i);
          cur_i = font_info[a].qqqq;
        }

        while (true)
        {
          if (next_char(cur_i) == skew_char[cur_f])
          {
            if (op_byte(cur_i) >= kern_flag)
              if (skip_byte(cur_i) <= stop_flag)
                s = char_kern(cur_f, cur_i);

            goto done1;
          }

          if (skip_byte(cur_i) >= stop_flag)
            goto done1;

          a = a + skip_byte(cur_i) + 1;
          cur_i = font_info[a].qqqq;
        }
      }
    }

done1:
    x = clean_box(nucleus(q), cramped_style(cur_style), math_kcode(q));
    w = width(x);
    h = height(x);

    // @<Switch to a larger accent if available and appropriate@>;
    while (true)
    {
      if (char_tag(i) != list_tag)
        goto done;

      y = rem_byte(i);
      i = char_info(f, y);

      if (!char_exists(i))
        goto done;

      if (char_width(f, i) > w)
        goto done;

      c = y;
    }

done:
    if (h < x_height(f))
      delta = h;
    else
      delta = x_height(f);

    if ((math_type(supscr(q)) != empty) || (math_type(subscr(q)) != empty))
      if (math_type(nucleus(q)) == math_char)
      {
        flush_node_list(x);
        x = new_noad();
        mem[nucleus(x)] = mem[nucleus(q)];
        mem[supscr(x)] = mem[supscr(q)];
        mem[subscr(x)] = mem[subscr(q)];
        mem[supscr(q)].hh = empty_field;
        mem[subscr(q)].hh = empty_field;
        math_type(nucleus(q)) = sub_mlist;
        info(nucleus(q)) = x;
        x = clean_box(nucleus(q), cur_style, math_kcode(q));
        delta = delta + height(x) - h;
        h = height(x);
      }

    y = char_box(f, c);
    shift_amount(y) = s + half(w - width(y));
    width(y) = 0;
    p = new_kern(-delta);
    link(p) = x;
    link(y) = p;
    y = vpackage(y, 0, 1, max_dimen);
    width(y) = width(x);

    // @<Make the height of box |y| equal to |h|@>;
    if (height(y) < h)
    {
      p = new_kern(h - height(y));
      link(p) = list_ptr(y);
      list_ptr(y) = p;
      height(y) = h;
    }

    info(nucleus(q)) = y;
    math_type(nucleus(q)) = sub_box;
  }
}

static void make_fraction (pointer q)
{
  pointer p, v, x, y, z; // {temporary registers for box construction}
  scaled delta, delta1, delta2, shift_up, shift_down, clr; // {dimensions for box calculations}
  
  if (thickness(q) == default_code)
    thickness(q) = default_rule_thickness;

  /*
    @<Create equal-width boxes |x| and |z| for the numerator and denominator,
    and compute the default amounts |shift_up| and |shift_down| by which they
    are displaced from the baseline@>;
  */

  x = clean_box(numerator(q), num_style(cur_style), math_kcode(q));
  z = clean_box(denominator(q), denom_style(cur_style), math_kcode(q));

  if (width(x) < width(z))
    x = rebox(x, width(z));
  else
    z = rebox(z, width(x));

  if (cur_style < text_style)
  {
    shift_up = num1(cur_size);
    shift_down = denom1(cur_size);
  }
  else
  {
    shift_down = denom2(cur_size);

    if (thickness(q) != 0)
      shift_up = num2(cur_size);
    else
      shift_up = num3(cur_size);
  }

  /*
    @<Adjust \(s)|shift_up| and |shift_down| for the case
    of no fraction line@>
  */

  if (thickness(q) == 0)
  {
    if (cur_style < text_style)
      clr = 7 * default_rule_thickness;
    else
      clr = 3 * default_rule_thickness;

    delta = half(clr - ((shift_up - depth(x)) - (height(z) - shift_down)));

    if (delta > 0)
    {
      shift_up = shift_up + delta;
      shift_down = shift_down + delta;
    }
  }
  else
  {
    if (cur_style < text_style)
      clr = 3 * thickness(q);
    else
      clr = thickness(q);

    delta = half(thickness(q));
    delta1 = clr - ((shift_up - depth(x)) - (axis_height(cur_size) + delta));
    delta2 = clr - ((axis_height(cur_size) - delta) - (height(z) - shift_down));

    if (delta1 > 0)
      shift_up = shift_up + delta1;

    if (delta2 > 0)
      shift_down = shift_down + delta2;
  }

  /*
    @<Construct a vlist box for the fraction, according to |shift_up| and
    |shift_down|@>;
  */
  v = new_null_box();
  type(v) = vlist_node;
  height(v) = shift_up + height(x);
  depth(v) = depth(z) + shift_down;
  width(v) = width(x);

  if (thickness(q) == 0)
  {
    p = new_kern((shift_up - depth(x)) - (height(z) - shift_down));
    link(p) = z;
  }
  else
  {
    y = fraction_rule(thickness(q));
    p = new_kern((axis_height(cur_size) - delta) - (height(z) - shift_down));
    link(y) = p;
    link(p) = z;
    p = new_kern((shift_up - depth(x)) - (axis_height(cur_size) + delta));
    link(p) = y;
  }

  link(x) = p;
  list_ptr(v) = x;

  /*
    @<Put the \(f)fraction into a box with its delimiters, and make |new_hlist(q)|
    point to it@>;
  */

  if (cur_style < text_style)
    delta = delim1(cur_size);
  else
    delta = delim2(cur_size);

  x = var_delimiter(left_delimiter(q), cur_size, delta);
  link(x) = v;
  z = var_delimiter(right_delimiter(q), cur_size, delta);
  link(v) = z;
  new_hlist(q) = hpack(x, 0, 1);
}

static scaled make_op (pointer q)
{
  scaled delta; // {offset between subscript and superscript}
  pointer p, v, x, y, z;  // {temporary registers for box construction}
  quarterword c;  // {registers for character examination}
  four_quarters i;
  scaled shift_up, shift_down;  // {dimensions for box calculation}

  if ((subtype(q) == normal) && (cur_style < text_style))
    subtype(q) = limits;

  if (math_type(nucleus(q)) == math_char)
  {
    fetch(nucleus(q));

    if ((cur_style < text_style) && (char_tag(cur_i) == list_tag))
    {
      c = rem_byte(cur_i);
      i = char_info(cur_f, c);

      if (char_exists(i))
      {
        cur_c = c;
        cur_i = i;
        character(nucleus(q)) = c;
      }
    }

    delta = char_italic(cur_f, cur_i);
    x = clean_box(nucleus(q), cur_style, math_kcode(q));

    if ((math_type(subscr(q)) != empty) && (subtype(q) != limits))
      width(x) = width(x) - delta;

    shift_amount(x) = half(height(x) - depth(x)) - axis_height(cur_size);
    math_type(nucleus(q)) = sub_box;
    info(nucleus(q)) = x;
  }
  else
    delta = 0;

  if (subtype(q) == limits)
  {
    x = clean_box(supscr(q), sup_style(cur_style), math_kcode(q));
    y = clean_box(nucleus(q), cur_style, math_kcode(q));
    z = clean_box(subscr(q), sub_style(cur_style), math_kcode(q));
    v = new_null_box();
    type(v) = vlist_node;
    width(v) = width(y);

    if (width(x) > width(v))
      width(v) = width(x);

    if (width(z) > width(v))
      width(v) = width(z);

    x = rebox(x, width(v));
    y = rebox(y, width(v));
    z = rebox(z, width(v));
    shift_amount(x) = half(delta);
    shift_amount(z) = -shift_amount(x);
    height(v) = height(y);
    depth(v) = depth(y);

    if (math_type(supscr(q)) == empty)
    {
      delete_glue_ref(space_ptr(x));
      delete_glue_ref(xspace_ptr(x));
      free_node(x, box_node_size);
      list_ptr(v) = y;
    }
    else
    {
      shift_up = big_op_spacing3 - depth(x);

      if (shift_up < big_op_spacing1)
        shift_up = big_op_spacing1;

      p = new_kern(shift_up);
      link(p) = y;
      link(x) = p;
      p = new_kern(big_op_spacing5);
      link(p) = x;
      list_ptr(v) = p;
      height(v) = height(v) + big_op_spacing5 + height(x) + depth(x) + shift_up;
    }

    if (math_type(subscr(q)) == empty)
    {
      delete_glue_ref(space_ptr(z));
      delete_glue_ref(xspace_ptr(z));
      free_node(z, box_node_size);
    }
    else
    {
      shift_down = big_op_spacing4 - height(z);

      if (shift_down < big_op_spacing2)
        shift_down = big_op_spacing2;

      p = new_kern(shift_down);
      link(y) = p;
      link(p) = z;
      p = new_kern(big_op_spacing5);
      link(z) = p;
      depth(v) = depth(v) + big_op_spacing5 + height(z) + depth(z) + shift_down;
    }

    new_hlist(q) = v;
  }

  return delta;
}

static void make_ord (pointer q)
{
  integer a; // {address of lig/kern instruction}
  pointer gp, gq, p, r; // {temporary registers for list manipulation}
  halfword rr;

restart:
  if ((math_type(subscr(q)) == empty) && (math_type(supscr(q)) == empty) &&
    ((math_type(nucleus(q)) == math_char) || (math_type(nucleus(q)) == math_jchar)))
  {
    p = link(q);

    if (p != null)
      if ((type(p) >= ord_noad) && (type(p) <= punct_noad))
        if (fam(nucleus(p)) == fam(nucleus(q)))
          if (math_type(nucleus(p)) == math_char)
          {
            math_type(nucleus(q)) = math_text_char;
            fetch(nucleus(q));
            
            if (char_tag(cur_i) == lig_tag)
            {
              a = lig_kern_start(cur_f, cur_i);
              cur_c = character(nucleus(p));
              cur_i = font_info[a].qqqq;
              
              if (skip_byte(cur_i) > stop_flag)
              {
                a = lig_kern_restart(cur_f, cur_i);
                cur_i = font_info[a].qqqq;
              }
              
              /*
                @<If instruction |cur_i| is a kern with |cur_c|, attach
                the kern after~|q|; or if it is a ligature with |cur_c|, combine
                noads |q| and~|p| appropriately; then |return| if the cursor has
                moved past a noad, or |goto restart|@>;
              */

              while (true)
              {
                if (next_char(cur_i) == cur_c)
                  if (skip_byte(cur_i) <= stop_flag)
                  {
                    if (op_byte(cur_i) >= kern_flag)
                    {
                      p = new_kern(char_kern(cur_f, cur_i));
                      link(p) = link(q);
                      link(q) = p;

                      return;
                    }
                    else
                    {
                      check_interrupt(); // {allow a way out of infinite ligature loop}

                      switch (op_byte(cur_i))
                      {
                        // {\.{=:\?}, \.{=:\?>}}
                        case 1:
                        case 5:
                          character(nucleus(q)) = rem_byte(cur_i);
                          break;
                        // {\.{\?=:}, \.{\?=:>}}
                        case 2:
                        case 6:
                          character(nucleus(p)) = rem_byte(cur_i);
                          break;
                        // {\.{\?=:\?}, \.{\?=:\?>}, \.{\?=:\?>>}}
                        case 3:
                        case 7:
                        case 11:
                          {
                            r = new_noad();
                            character(nucleus(r)) = rem_byte(cur_i);
                            fam(nucleus(r)) = fam(nucleus(q));
                            link(q) = r;
                            link(r) = p;

                            if (op_byte(cur_i) < 11)
                              math_type(nucleus(r)) = math_char;
                            else
                              math_type(nucleus(r)) = math_text_char; // {prevent combination}
                          }
                          break;

                        default:
                          {
                            link(q) = link(p);
                            character(nucleus(q)) = rem_byte(cur_i); // {\.{=:}}
                            mem[subscr(q)] = mem[subscr(p)];
                            mem[supscr(q)] = mem[supscr(p)];
                            free_node(p, noad_size);
                          }
                          break;
                      }

                      if (op_byte(cur_i) > 3)
                        return;

                      math_type(nucleus(q)) = math_char;
                      goto restart;
                    }
                  }

                if (skip_byte(cur_i) >= stop_flag)
                  return;

                a = a + skip_byte(cur_i) + 1;
                cur_i = font_info[a].qqqq;
              }
            }
          }
          else if (math_type(nucleus(p)) == math_jchar)
          {
            math_type(nucleus(q)) = math_text_jchar;
            fetch(nucleus(p));
            a = cur_c;
            fetch(nucleus(q));

            if (char_tag(cur_i) == gk_tag)
            {
              cur_c = a;
              a = glue_kern_start(cur_f, cur_i);
              cur_i = font_info[a].qqqq;

              if (skip_byte(cur_i) > stop_flag)
              {
                a = glue_kern_restart(cur_f, cur_i);
                cur_i = font_info[a].qqqq;
              }
              
              while (true)
              {
                if (next_char(cur_i) == cur_c)
                  if (skip_byte(cur_i) <= stop_flag)
                    if (op_byte(cur_i) < kern_flag)
                    {
                      gp = font_glue[cur_f];
                      rr = op_byte(cur_i) * 256 + rem_byte(cur_i);
                        
                      if (gp != null)
                      {
                        while ((type(gp) != rr) && (link(gp) != null))
                        {
                          gp = link(gp);
                        }

                        gq = glue_ptr(gp);
                      }
                      else
                      {
                        gp = get_node(small_node_size);
                        font_glue[cur_f] = gp;
                        gq = null;
                      }

                      if (gq == null)
                      {
                        type(gp) = rr;
                        gq = new_spec(zero_glue);
                        glue_ptr(gp) = gq;
                        a = exten_base[cur_f] + (((rr)) * 3);
                        width(gq) = font_info[a].sc;
                        stretch(gq) = font_info[a + 1].sc;
                        shrink(gq) = font_info[a + 2].sc;
                        add_glue_ref(gq);
                        link(gp) = get_node(small_node_size);
                        gp = link(gp);
                        glue_ptr(gp) = null;
                        link(gp) = null;
                      }

                      p = new_glue(gq);
                      subtype(p) = jfm_skip + 1;
                      link(p) = link(q);
                      link(q) = p;

                      return;
                    }
                    else
                    {
                      p = new_kern(char_kern(cur_f, cur_i));
                      link(p) = link(q);
                      link(q) = p;

                      return;
                    }

                if (skip_byte(cur_i) >= stop_flag)
                  return;

                a = a + skip_byte(cur_i) + 1; //{SKIP property}
                cur_i = font_info[a].qqqq;
              }
            }
          }
      }
}

static void make_scripts (pointer q, scaled delta)
{
  pointer p, x, y, z; // {temporary registers for box construction}
  scaled shift_up, shift_down, clr; // {dimensions in the calculation}
  small_number t; // {subsidiary size code}

  p = new_hlist(q);

  if (is_char_node(p))
  {
    shift_up = 0;
    shift_down = 0;
  }
  else
  {
    z = hpack(p, 0, 1);

    if (cur_style < script_style)
      t = script_size;
    else
      t = script_script_size;

    shift_up = height(z) - sup_drop(t);
    shift_down = depth(z) + sub_drop(t);
    delete_glue_ref(space_ptr(z));
    delete_glue_ref(xspace_ptr(z));
    free_node(z, box_node_size);
  }

  if (math_type(supscr(q)) == empty)
  {
    x = clean_box(subscr(q), sub_style(cur_style), math_kcode(q));
    width(x) = width(x) + script_space;

    if (shift_down < sub1(cur_size))
      shift_down = sub1(cur_size);

    clr = height(x) - (abs(math_x_height(cur_size) * 4) / 5);

    if (shift_down < clr)
      shift_down = clr;

    shift_amount(x) = shift_down;
  }
  else
  {
    {
      x = clean_box(supscr(q), sup_style(cur_style), math_kcode(q));
      width(x) = width(x) + script_space;

      if (odd(cur_style))
        clr = sup3(cur_size);
      else if (cur_style < text_style)
        clr = sup1(cur_size);
      else
        clr = sup2(cur_size);

      if (shift_up < clr)
        shift_up = clr;

      clr = depth(x) + (abs(math_x_height(cur_size)) / 4);

      if (shift_up < clr)
        shift_up = clr;
    }

    if (math_type(subscr(q)) == empty)
      shift_amount(x) = -shift_up;
    else
    {
      y = clean_box(subscr(q), sub_style(cur_style), math_kcode(q));
      width(y) = width(y) + script_space;

      if (shift_down < sub2(cur_size))
        shift_down = sub2(cur_size);

      clr = 4 * default_rule_thickness - ((shift_up - depth(x)) - (height(y) - shift_down));

      if (clr > 0)
      {
        shift_down = shift_down + clr;

        clr = (abs(math_x_height(cur_size) * 4) / 5) - (shift_up - depth(x));

        if (clr > 0)
        {
          shift_up = shift_up + clr;
          shift_down = shift_down - clr;
        }
      }

      shift_amount(x) = delta;
      p = new_kern((shift_up - depth(x)) - (height(y) - shift_down));
      link(x) = p;
      link(p) = y;
      x = vpackage(x, 0, 1, max_dimen);
      shift_amount(x) = shift_down;
    }
  }

  if (new_hlist(q) == null)
    new_hlist(q) = x;
  else
  {
    p = new_hlist(q);

    while (link(p) != null)
      p = link(p);

    link(p) = x;
  }
}

static small_number make_left_right (pointer q, small_number style, scaled max_d, scaled max_h)
{
  scaled delta, delta1, delta2; // {dimensions used in the calculation}

  cur_style = style;

  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

  delta2 = max_d + axis_height(cur_size);
  delta1 = max_h + max_d - delta2;

  if (delta2 > delta1)
    delta1 = delta2; // {|delta1| is max distance from axis}

  delta = (delta1 / 500) * delimiter_factor;
  delta2 = delta1 + delta1 - delimiter_shortfall;

  if (delta < delta2)
    delta = delta2;

  new_hlist(q) = var_delimiter(delimiter(q), cur_size, delta);

  return type(q) - (left_noad - open_noad); // {|open_noad| or |close_noad|}
}

#if defined (_MSC_VER)
  #pragma optimize("", off)
#endif

void mlist_to_hlist (void)
{
  pointer mlist;            // {beginning of the given list}
  boolean penalties;        // {should penalty nodes be inserted?}
  small_number style;       // {the given style}
  pointer u;                // {temporary register}
  small_number save_style;  // {holds |cur_style| during recursion}
  pointer q;                // {runs through the mlist}
  pointer r;                // {the most recent noad preceding |q|}
  small_number r_type;      // {the |type| of noad |r|, or |op_noad| if |r=null|}
  small_number t;           // {the effective |type| of noad |q| during the second pass}
  pointer p, x, y, z;       // {temporary registers for list construction}
  integer pen;              // {a penalty to be inserted}
  small_number s;           // {the size of a noad to be deleted}
  scaled max_h, max_d;      // {maximum height and depth of the list translated so far}
  scaled delta;             // {offset between subscript and superscript}

  mlist = cur_mlist;
  penalties = mlist_penalties;
  style = cur_style; // {tuck global parameters away as local variables}
  q = mlist;
  r = null;
  r_type = op_noad;
  max_h = 0;
  max_d = 0;

  // @<Set up the values of |cur_size| and |cur_mu|, based on |cur_style|@>;
  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

  /*
    @<Process node-or-noad |q| as much as possible in preparation
    for the second pass of |mlist_to_hlist|, then move to the next
    item in the mlist@>;
  */

  while (q != null)
  {
    /*
      @<Do first-pass processing based on |type(q)|; |goto done_with_noad|
      if a noad has been fully processed, |goto check_dimensions| if it
      has been translated into |new_hlist(q)|, or |goto done_with_node|
      if a node has been fully processed@>
    */
reswitch:
    delta = 0;

    switch (type(q))
    {
      case bin_noad:
        switch (r_type)
        {
          case bin_noad:
          case op_noad:
          case rel_noad:
          case open_noad:
          case punct_noad:
          case left_noad:
            {
              type(q) = ord_noad;
              goto reswitch;
            }
            break;

          default:
            do_nothing();
            break;
        }
        break;

      case rel_noad:
      case close_noad:
      case punct_noad:
      case right_noad:
        {
          // @<Convert \(a)a final |bin_noad| to an |ord_noad|@>
          if (r_type == bin_noad)
            type(r) = ord_noad;

          if (type(q) == right_noad)
            goto done_with_noad;
        }
        break;

      // @<Cases for noads that can follow a |bin_noad|@>
      case left_noad:
        goto done_with_noad;
        break;

      case fraction_noad:
        {
          make_fraction(q);
          goto check_dimensions;
        }
        break;

      case op_noad:
        {
          delta = make_op(q);

          if (subtype(q) == limits)
            goto check_dimensions;
        }
        break;

      case ord_noad:
        make_ord(q);
        break;

      case open_noad:
      case inner_noad:
        do_nothing();
        break;

      case radical_noad:
        make_radical(q);
        break;

      case over_noad:
        make_over(q);
        break;

      case under_noad:
        make_under(q);
        break;

      case accent_noad:
        make_math_accent(q);
        break;

      case vcenter_noad:
        make_vcenter(q);
        break;

      /*
        @<Cases for nodes that can appear in an mlist, after which we
        |goto done_with_node|@>
      */
      case style_node:
        {
          cur_style = subtype(q);

          // @<Set up the values of |cur_size| and |cur_mu|, based on |cur_style|@>
          {
            if (cur_style < script_style)
              cur_size = text_size;
            else
              cur_size = 16 * ((cur_style - text_style) / 2);

            cur_mu = x_over_n(math_quad(cur_size), 18);
          }

          goto done_with_node;
        }
        break;

      /*
        @<Change this node to a style node followed by the correct choice,
        then |goto done_with_node|@>
      */
      case choice_node:
        {
          switch (cur_style / 2)
          {
            case 0:
              choose_mlist(display_mlist); // {|display_style=0|}
              break;

            case 1:
              choose_mlist(text_mlist); // {|text_style=2|}
              break;

            case 2:
              choose_mlist(script_mlist); // {|script_style=4|}
              break;

            case 3:
              choose_mlist(script_script_mlist); // {|script_script_style=6|}
              break;
          }

          flush_node_list(display_mlist(q));
          flush_node_list(text_mlist(q));
          flush_node_list(script_mlist(q));
          flush_node_list(script_script_mlist(q));
          type(q) = style_node;
          subtype(q) = cur_style;
          width(q) = 0;
          depth(q) = 0;

          if (p != null)
          {
            z = link(q);
            link(q) = p;

            while (link(p) != null)
              p = link(p);

            link(p) = z;
          }

          goto done_with_node;
        }
        break;

      case ins_node:
      case mark_node:
      case adjust_node:
      case whatsit_node:
      case penalty_node:
      case disc_node:
        goto done_with_node;
        break;

      case rule_node:
        {
          if (height(q) > max_h)
            max_h = height(q);

          if (depth(q) > max_d)
            max_d = depth(q);

          goto done_with_node;
        }
        break;

      case glue_node:
        {
          // @<Convert \(m)math glue to ordinary glue@>
          if (subtype(q) == mu_glue)
          {
            x = glue_ptr(q);
            y = math_glue(x, cur_mu);
            delete_glue_ref(x);
            glue_ptr(q) = y;
            subtype(q) = normal;
          }
          else if ((cur_size != text_size) && (subtype(q) == cond_math_glue))
          {
            p = link(q);

            if (p != null)
              if ((type(q) == glue_node) || (type(p) == kern_node))
              {
                link(q) = link(p);
                link(p) = null;
                flush_node_list(p);
              }
          }

          goto done_with_node;
        }
        break;

      case kern_node:
        {
          math_kern(q, cur_mu);
          goto done_with_node;
        }
        break;

      case disp_node:
        goto done_with_node;
        break;

      default:
        confusion("mlist1");
        break;
    }

    // @<Convert \(n)|nucleus(q)| to an hlist and attach the sub/superscripts@>
    switch (math_type(nucleus(q)))
    {
      case math_char:
      case math_text_char:
      case math_jchar:
      case math_text_jchar:
        {
          /*
            @<Create a character node |p| for |nucleus(q)|, possibly followed
            by a kern node for the italic correction, and set |delta| to the
            italic correction if a subscript is present@>
          */
          fetch(nucleus(q));

          if (char_exists(cur_i))
          {
            delta = char_italic(cur_f, cur_i);
            p = new_character(cur_f, cur_c);
            u = p;

            if (font_dir[cur_f] != dir_default)
            {
              link(u) = get_avail();
              u = link(u);
              info(u) = math_kcode(q);
            }

            if (((math_type(nucleus(q)) == math_text_char) ||
                (math_type(nucleus(q)) == math_text_jchar)) && (space(cur_f) != 0))
              delta = 0;  // {no italic correction in mid-word of text font}

            if ((math_type(subscr(q)) == empty) && (delta != 0))
            {
              link(u) = new_kern(delta);
              delta = 0;
            }
          }
          else
            p = null;
        }
        break;

      case empty:
        p = null;
        break;

      case sub_box:
        p = info(nucleus(q));
        break;

      case sub_exp_box:
        p = shift_sub_exp_box(nucleus(q));
        break;

      case sub_mlist:
        {
          cur_mlist = info(nucleus(q));
          save_style = cur_style;
          mlist_penalties = false;
          mlist_to_hlist(); // {recursive call}
          cur_style = save_style;

          {
            if (cur_style < script_style)
              cur_size = text_size;
            else
              cur_size = 16 * ((cur_style - text_style) / 2);

            cur_mu = x_over_n(math_quad(cur_size), 18);
          }

          p = hpack(link(temp_head), 0, 1);
        }
        break;

      default:
        confusion("mlist2");
        break;
    }
  
    new_hlist(q) = p;

    if ((math_type(subscr(q)) == empty) && (math_type(supscr(q)) == empty))
      goto check_dimensions;

    make_scripts(q, delta);

    // {go here to update |max_h| and |max_d|}
check_dimensions:
    z = hpack(new_hlist(q), 0, 1);

    if (height(z) > max_h)
      max_h = height(z);

    if (depth(z) > max_d)
      max_d = depth(z);

    delete_glue_ref(space_ptr(z));
    delete_glue_ref(xspace_ptr(z));
    free_node(z, box_node_size);

    // {go here when a noad has been fully translated}
done_with_noad:
    r = q;
    r_type = type(r);

    if (r_type == right_noad)
    {
      r_type = left_noad;
      cur_style = style;

      {
        if (cur_style < script_style)
          cur_size = text_size;
        else
          cur_size = 16 * ((cur_style - text_style) / 2);

        cur_mu = x_over_n(math_quad(cur_size), 18);
      }
    }

    // {go here when a node has been fully converted}
done_with_node:
    q = link(q);
  }

  // @<Convert \(a)a final |bin_noad| to an |ord_noad|@>;
  if (r_type == bin_noad)
    type(r) = ord_noad;
  
  /*
    @<Make a second pass over the mlist, removing all noads and inserting the
    proper spacing and penalties@>;
  */

  p = temp_head;
  link(p) = null;
  q = mlist;
  r_type = 0;
  cur_style = style;

  {
    if (cur_style < script_style)
      cur_size = text_size;
    else
      cur_size = 16 * ((cur_style - text_style) / 2);

    cur_mu = x_over_n(math_quad(cur_size), 18);
  }

  while (q != null)
  {
    /*
      @<If node |q| is a style node, change the style and |goto delete_q|;
      otherwise if it is not a noad, put it into the hlist,
      advance |q|, and |goto done|; otherwise set |s| to the size
      of noad |q|, set |t| to the associated type (|ord_noad..
      inner_noad|), and set |pen| to the associated penalty@>;
    */
    t = ord_noad;
    s = noad_size;
    pen = inf_penalty;

    switch (type(q))
    {
      case op_noad:
      case open_noad:
      case close_noad:
      case punct_noad:
      case inner_noad:
        t = type(q);
        break;

      case bin_noad:
        {
          t = bin_noad;
          pen = bin_op_penalty;
        }
        break;

      case rel_noad:
        {
          t = rel_noad;
          pen = rel_penalty;
        }
        break;

      case ord_noad:
      case vcenter_noad:
      case over_noad:
      case under_noad:
        do_nothing();
        break;

      case radical_noad:
        s = radical_noad_size;
        break;

      case accent_noad:
        s = accent_noad_size;
        break;

      case fraction_noad:
        s = fraction_noad_size;
        break;

      case left_noad:
      case right_noad:
        t = make_left_right(q, style, max_d, max_h);
        break;

      case style_node:
        {
          // @<Change the current style and |goto delete_q|@>
          cur_style = subtype(q);
          s = style_node_size;

          {
            if (cur_style < script_style)
              cur_size = text_size;
            else
              cur_size = 16 * ((cur_style - text_style) / 2);

            cur_mu = x_over_n(math_quad(cur_size), 18);
          }

          goto delete_q;
        }
        break;

      case whatsit_node:
      case penalty_node:
      case rule_node:
      case disc_node:
      case adjust_node:
      case ins_node:
      case mark_node:
      case glue_node:
      case kern_node:
        {
          link(p) = q;
          p = q;
          q = link(q);
          link(p) = null;
          goto done;
        }
        break;

      case disp_node:
        {
          link(p) = q;
          p = q;
          q = link(q);
          link(p) = null;
          goto done;
        }
        break;

      default:
        confusion("mlist3");
        break;
    }

    // @<Append inter-element spacing based on |r_type| and |t|@>;
    if (r_type > 0)
    {
      switch (str_pool[r_type * 8 + t + magic_offset])
      {
        case '0':
          x = 0;
          break;

        case '1':
          if (cur_style < script_style)
            x = thin_mu_skip_code;
          else
            x = 0;
          break;

        case '2':
          x = thin_mu_skip_code;
          break;

        case '3':
          if (cur_style < script_style)
            x = med_mu_skip_code;
          else
            x = 0;
          break;

        case '4':
          if (cur_style < script_style)
            x = thick_mu_skip_code;
          else
            x = 0;
          break;

        default:
          confusion("mlist4");
          break;
      }

      if (x != 0)
      {
        y = math_glue(glue_par(x), cur_mu);
        z = new_glue(y);
        glue_ref_count(y) = null;
        link(p) = z;
        p = z;
        subtype(z) = x + 1; // {store a symbolic subtype}
      }
    }

    // @<Append any |new_hlist| entries for |q|, and any appropriate penalties@>;
    if (new_hlist(q) != null)
    {
      link(p) = new_hlist(q);

      do {
        p = link(p);
      } while (!(link(p) == null));
    }

    if (penalties)
      if (link(q) != null)
        if (pen < inf_penalty)
        {
          r_type = type(link(q));

          if (r_type != penalty_node)
            if (r_type != rel_noad)
            {
              z = new_penalty(pen);
              link(p) = z;
              p = z;
            }
        }

    if (type(q) == right_noad)
      t = open_noad;

    r_type = t;

    // {go here to delete |q| and move to the next node}
delete_q:
    r = q;
    q = link(q);
    free_node(r, s);
done:;
  }
  
  p = new_null_box();
  link(p) = link(temp_head);
  adjust_hlist(p, false);
  link(temp_head) = link(p);
  delete_glue_ref(space_ptr(p));
  delete_glue_ref(xspace_ptr(p));
  free_node(p, box_node_size);
}

#if defined (_MSC_VER)
  #pragma optimize("", on)
#endif

static void push_alignment (void)
{
  pointer p;  // {the new alignment stack node}

  p = get_node(align_stack_node_size);
  link(p) = align_ptr;
  info(p) = cur_align;
  llink(p) = preamble;
  rlink(p) = cur_span;
  mem[p + 2].cint = cur_loop;
  mem[p + 3].cint = align_state;
  info(p + 4) = cur_head;
  link(p + 4) = cur_tail;
  align_ptr = p;
  cur_head = get_avail();
}

static void pop_alignment (void)
{
  pointer p;  // {the top alignment stack node}

  free_avail(cur_head);
  p = align_ptr;
  cur_tail = link(p + 4);
  cur_head = info(p + 4);
  align_state = mem[p + 3].cint;
  cur_loop = mem[p + 2].cint;
  cur_span = rlink(p);
  preamble = llink(p);
  cur_align = info(p);
  align_ptr = link(p);
  free_node(p, align_stack_node_size);
}

static void get_preamble_token (void)
{
restart:
  get_token();

  while ((cur_chr == span_code) && (cur_cmd == tab_mark))
  {
    get_token();  // {this token will be expanded once}

    if (cur_cmd > max_command)
    {
      expand();
      get_token();
    }
  }

  if (cur_cmd == endv)
    fatal_error("(interwoven alignment preambles are not allowed)");

  if ((cur_cmd == assign_glue) && (cur_chr == glue_base + tab_skip_code))
  {
    scan_optional_equals();
    scan_glue(glue_val);

    if (global_defs > 0)
      geq_define(glue_base + tab_skip_code, glue_ref, cur_val);
    else
      eq_define(glue_base + tab_skip_code, glue_ref, cur_val);

    goto restart;
  }
}

static void init_align (void)
{
  pointer save_cs_ptr;  // {|warning_index| value for error messages}
  pointer p;  // {for short-term temporary use}

  save_cs_ptr = cur_cs; // {\.{\\halign} or \.{\\valign}, usually}
  push_alignment();
  align_state = -1000000; // {enter a new alignment level}

  // @<Check for improper alignment in displayed math@>;
  if ((mode == mmode) && ((tail != head) || (incompleat_noad != null)))
  {
    print_err("Improper ");
    print_esc("halign");
    prints(" inside $$'s");
    help3("Displays can use special alignments (like \\eqalignno)",
        "only if nothing but the alignment itself is between $$'s.",
        "So I've deleted the formulas that preceded this alignment.");
    error();
    flush_math();
  }

  push_nest();  // {enter a new semantic level}

  // @<Change current mode to |-vmode| for \.{\\halign}, |-hmode| for \.{\\valign}@>
  if (mode == mmode)
  {
    mode = -vmode;
    prev_depth = nest[nest_ptr - 2].aux_field.sc;
  }
  else if (mode > 0)
    negate(mode);

  scan_spec(align_group, false);

  // @<Scan the preamble and record it in the |preamble| list@>
  preamble = null;
  cur_align = align_head;
  cur_loop = null;
  scanner_status = aligning;
  warning_index = save_cs_ptr;
  align_state = -1000000;

  while (true)
  {
    // @<Append the current tabskip glue to the preamble list@>
    link(cur_align) = new_param_glue(tab_skip_code);
    cur_align = link(cur_align);

    // {\.{\\cr} ends the preamble}
    if (cur_cmd == car_ret)
      goto done;

    /*
      @<Scan preamble text until |cur_cmd| is |tab_mark| or |car_ret|,
      looking for changes in the tabskip glue; append an
      alignrecord to the preamble list@>
    */
    // @<Scan the template \<u_j>, putting the resulting token list in |hold_head|@>
    p = hold_head;
    link(p) = null;

    while (true)
    {
      get_preamble_token();

      if (cur_cmd == mac_param)
        goto done1;

      if ((cur_cmd <= car_ret) && (cur_cmd >= tab_mark) && (align_state == -1000000))
        if ((p == hold_head) && (cur_loop == null) && (cur_cmd == tab_mark))
          cur_loop = cur_align;
        else
        {
          print_err("Missing # inserted in alignment preamble");
          help3("There should be exactly one # between &'s, when an",
              "\\halign or \\valign is being set up. In this case you had",
              "none, so I've put one in; maybe that will work.");
          back_error();
          goto done1;
        }
      else if ((cur_cmd != spacer) || (p != hold_head))
      {
        link(p) = get_avail();
        p = link(p);
        info(p) = cur_tok;
      }
    }

done1:
    link(cur_align) = new_null_box();
    cur_align = link(cur_align);  // {a new alignrecord}
    info(cur_align) = end_span;
    width(cur_align) = null_flag;
    u_part(cur_align) = link(hold_head);
    // @<Scan the template \<v_j>, putting the resulting token list in |hold_head|@>
    p = hold_head;
    link(p) = null;

    while (true)
    {
continu:
      get_preamble_token();

      if ((cur_cmd <= car_ret) && (cur_cmd >= tab_mark) && (align_state == -1000000))
        goto done2;

      if (cur_cmd == mac_param)
      {
        print_err("Only one # is allowed per tab");
        help3("There should be exactly one # between &'s, when an",
            "\\halign or \\valign is being set up. In this case you had",
            "more than one, so I'm ignoring all but the first.");
        error();
        goto continu;
      }

      link(p) = get_avail();
      p = link(p);
      info(p) = cur_tok;
    }

done2:
    link(p) = get_avail();
    p = link(p);
    info(p) = end_template_token; // {put \.{\\endtemplate} at the end}
    v_part(cur_align) = link(hold_head);
  }

done:
  scanner_status = normal;
  new_save_level(align_group);

  if (every_cr != null)
    begin_token_list(every_cr, every_cr_text);

  align_peek(); // {look for \.{\\noalign} or \.{\\omit}}
}

static void init_span (pointer p)
{
  push_nest();

  if (mode == -hmode)
    space_factor = 1000;
  else
  {
    prev_depth = ignore_depth;
    normal_paragraph();
  }

  inhibit_glue_flag = false;
  cur_span = p;
}

static void init_row (void)
{
  push_nest();
  mode = (-hmode - vmode) - mode;

  if (mode == -hmode)
    space_factor = 0;
  else
    prev_depth = 0;

  tail_append(new_glue(glue_ptr(preamble)));
  subtype(tail) = tab_skip_code + 1;
  cur_align = link(preamble);
  cur_tail = cur_head;
  init_span(cur_align);
}

static void init_col (void)
{
  extra_info(cur_align) = cur_cmd;

  if (cur_cmd == omit)
    align_state = 0;
  else
  {
    back_input();
    begin_token_list(u_part(cur_align), u_template);
  }
  // {now |align_state=1000000|}
}

static void fin_row (void)
{
  pointer p;  // {the new unset box}

  if (mode == -hmode)
  {
    adjust_hlist(head, false);
    delete_glue_ref(cur_kanji_skip);
    delete_glue_ref(cur_xkanji_skip);
    cur_kanji_skip = space_ptr(head);
    cur_xkanji_skip = xspace_ptr(head);
    add_glue_ref(cur_kanji_skip);
    add_glue_ref(cur_xkanji_skip);
    p = hpack(link(head), 0, 1);
    pop_nest();
    append_to_vlist(p);

    if (cur_head != cur_tail)
    {
      link(tail) = link(cur_head);
      tail = cur_tail;
    }
  }
  else
  {
    p = vpackage(link(head), 0, 1, max_dimen);
    pop_nest();
    link(tail) = p;
    tail = p;
    space_factor = 1000;
    inhibit_glue_flag = false;
  }

  type(p) = unset_node;
  glue_stretch(p) = 0;

  if (every_cr != null)
    begin_token_list(every_cr, every_cr_text);

  align_peek();
  // {note that |glue_shrink(p)=0| since |glue_shrink==shift_amount|}
}

static void fin_align (void)
{
  pointer p, q, r, s, u, v, z;  // {registers for the list operations}
  scaled t, w;  // {width of column}
  scaled o; // {shift offset for unset boxes}
  halfword n; // {matching span amount}
  scaled rule_save; // {temporary storage for |overfull_rule|}
  memory_word aux_save; // {temporary storage for |aux|}

  if (cur_group != align_group)
    confusion("align1");

  unsave(); // {that |align_group| was for individual entries}

  if (cur_group != align_group)
    confusion("align0");

  unsave(); // {that |align_group| was for the whole alignment}

  if (nest[nest_ptr - 1].mode_field == mmode)
    o = display_indent;
  else
    o = 0;

  /*
    @<Go through the preamble list, determining the column widths and
    changing the alignrecords to dummy unset boxes@>
  */

  q = link(preamble);

  do {
    flush_list(u_part(q));
    flush_list(v_part(q));
    p = link(link(q));

    if (width(q) == null_flag)
    {
      // @<Nullify |width(q)| and the tabskip glue following this column@>
      width(q) = 0;
      r = link(q);
      s = glue_ptr(r);

      if (s != zero_glue)
      {
        add_glue_ref(zero_glue);
        delete_glue_ref(s);
        glue_ptr(r) = zero_glue;
      }
    }

    if (info(q) != end_span)
    {
      /*
        @<Merge the widths in the span nodes of |q| with those of |p|,
        destroying the span nodes of |q|@>
      */
      t = width(q) + width(glue_ptr(link(q)));
      r = info(q);
      s = end_span;
      info(s) = p;
      n = min_quarterword + 1;

      do {
        width(r) = width(r) - t;
        u = info(r);

        while (link(r) > n)
        {
          s = info(s);
          n = link(info(s)) + 1;
        }

        if (link(r) < n)
        {
          info(r) = info(s);
          info(s) = r;
          decr(link(r));
          s = r;
        }
        else
        {
          if (width(r) > width(info(s)))
            width(info(s)) = width(r);

          free_node(r, span_node_size);
        }

        r = u;
      } while (!(r == end_span));
    }

    type(q) = unset_node;
    span_count(q) = min_quarterword;
    height(q) = 0;
    depth(q) = 0;
    glue_order(q) = normal;
    glue_sign(q) = normal;
    glue_stretch(q) = 0;
    glue_shrink(q) = 0;
    q = p;
  } while (!(q == null));

  /*
    @<Package the preamble list, to determine the actual tabskip glue amounts,
    and let |p| point to this prototype box@>
  */

  save_ptr = save_ptr - 2;
  pack_begin_line = -mode_line;

  if (mode == -vmode)
  {
    rule_save = overfull_rule;
    overfull_rule = 0;  // {prevent rule from being packaged}
    z = new_null_box();
    link(z) = preamble;
    adjust_hlist(z, false);
    delete_glue_ref(cur_kanji_skip);
    delete_glue_ref(cur_xkanji_skip);
    cur_kanji_skip = space_ptr(z);
    cur_xkanji_skip = xspace_ptr(z);
    add_glue_ref(cur_kanji_skip);
    add_glue_ref(cur_xkanji_skip);
    p = hpack(preamble, saved(1), saved(0));
    overfull_rule = rule_save;
    delete_glue_ref(space_ptr(z));
    delete_glue_ref(xspace_ptr(z));
    free_node(z, box_node_size);
  }
  else
  {
    q = link(preamble);

    do {
      height(q) = width(q);
      width(q) = 0;
      q = link(link(q));
    } while (!(q == null));

    p = vpackage(preamble, saved(1), saved(0), max_dimen);
    q = link(preamble);

    do {
      width(q) = height(q);
      height(q) = 0;
      q = link(link(q));
    } while (!(q == null));
  }

  pack_begin_line = 0;
  // @<Set the glue in all the unset boxes of the current list@>
  q = link(head);
  s = head;

  while (q != null)
  {
    if (!is_char_node(q))
      if (type(q) == unset_node)
      {
        if (mode == -vmode)
        {
          type(q) = hlist_node;
          width(q) = width(p);

          if (nest[nest_ptr - 1].mode_field == mmode)
            set_box_lr(q, dlist);
        }
        else
        {
          type(q) = vlist_node;
          height(q) = height(p);
        }

        set_box_dir(q, direction);
        glue_order(q) = glue_order(p);
        glue_sign(q) = glue_sign(p);
        glue_set(q) = glue_set(p);
        shift_amount(q) = o;
        r = link(list_ptr(q));
        s = link(list_ptr(p));

        do {
          n = span_count(r);
          t = width(s);
          w = t;
          u = hold_head;
          set_box_lr(r, 0);

          while (n > min_quarterword)
          {
            decr(n);
            s = link(s);
            v = glue_ptr(s);
            link(u) = new_glue(v);
            u = link(u);
            subtype(u) = tab_skip_code + 1;
            t = t + width(v);

            if (glue_sign(p) == stretching)
            {
              if (stretch_order(v) == glue_order(p))
                t = t + round(glue_set(p) * stretch(v));
            }
            else if (glue_sign(p) == shrinking)
            {
              if (shrink_order(v) == glue_order(p))
                t = t - round(glue_set(p) * shrink(v));
            }

            s = link(s);
            link(u) = new_null_box();
            u = link(u);
            t = t + width(s);

            if (mode == -vmode)
              width(u) = width(s);
            else
            {
              type(u) = vlist_node;
              height(u) = width(s);
            }

            set_box_dir(u, direction);
          }
            

          if (mode == -vmode)
          {
            height(r) = height(q);
            depth(r) = depth(q);

            if (t == width(r))
            {
              glue_sign(r) = normal;
              glue_order(r) = normal;
              set_glue_ratio_zero(glue_set(r));
            }
            else if (t > width(r))
            {
              glue_sign(r) = stretching;

              if (glue_stretch(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else
                glue_set(r) = (t - width(r)) / ((real) glue_stretch(r));
            }
            else
            {
              glue_order(r) = glue_sign(r);
              glue_sign(r) = shrinking;

              if (glue_shrink(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else if ((glue_order(r) == normal) && (width(r) - t > glue_shrink(r)))
                set_glue_ratio_one(glue_set(r));
              else
                glue_set(r) = (width(r) - t) / ((real) glue_shrink(r));
            }

            width(r) = w;
            type(r) = hlist_node;
            set_box_dir(r, direction);
          }
          else
          {
            width(r) = width(q);

            if (t == height(r))
            {
              glue_sign(r) = normal;
              glue_order(r) = normal;
              set_glue_ratio_zero(glue_set(r));
            }
            else if (t > height(r))
            {
              glue_sign(r) = stretching;

              if (glue_stretch(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else
                glue_set(r) = (t - height(r)) / ((real) glue_stretch(r));
            }
            else
            {
              glue_order(r) = glue_sign(r);
              glue_sign(r) = shrinking;

              if (glue_shrink(r) == 0)
                set_glue_ratio_zero(glue_set(r));
              else if ((glue_order(r) == normal) && (height(r) - t > glue_shrink(r)))
                set_glue_ratio_one(glue_set(r));
              else
                glue_set(r) = (height(r) - t) / ((real) glue_shrink(r));
            }

            height(r) = w;
            type(r) = vlist_node;
            set_box_dir(r, abs(direction));
          }

          shift_amount(r) = 0;

          if (u != hold_head)
          {
            link(u) = link(r);
            link(r) = link(hold_head);
            r = u;
          }

          r = link(link(r));
          s = link(link(s));
        } while (!(r == 0));
      }
      else if (type(q) == rule_node)
      {
        if (is_running(width(q)))
          width(q) = width(p);

        if (is_running(height(q)))
          height(q) = height(p);

        if (is_running(depth(q)))
          depth(q) = depth(p);

        if (o != 0)
        {
          r = link(q);
          link(q) = 0;
          q = hpack(q, 0, 1);
          shift_amount(q) = o;
          link(q) = r;
          link(s) = q;
        }
      }

    s = q;
    q = link(q);
  }

  flush_node_list(p);
  pop_alignment();
  // @<Insert the \(c)current list into its environment@>
  aux_save = aux;
  p = link(head);
  q = tail;
  pop_nest();

  if (mode == mmode)
  {
    do_assignments();

    if (cur_cmd != math_shift)
    {
      print_err("Missing $$ inserted");
      help2("Displays can use special alignments (like \\eqalignno)",
          "only if nothing but the alignment itself is between $$'s.");
      back_error();
    }
    else
    {
      get_x_token();

      if (cur_cmd != math_shift)
      {
        print_err("Display math should end with $$");
        help2("The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.");
        back_error();
      }
    }

    flush_node_list(LR_box);
    pop_nest();
    tail_append(new_penalty(pre_display_penalty));
    tail_append(new_param_glue(above_display_skip_code));
    link(tail) = p;

    if (p != 0)
      tail = q;

    tail_append(new_penalty(post_display_penalty));
    tail_append(new_param_glue(below_display_skip_code));
    prev_depth = aux_save.sc;
    resume_after_display();
  }
  else
  {
    aux = aux_save;
    link(tail) = p;

    if (p != 0)
      tail = q;

    if (mode == vmode)
      build_page();
  }
}

static boolean fin_col (void)
{
  pointer p;  // {the alignrecord after the current one}
  pointer q, r; // {temporary pointers for list manipulation}
  pointer s;  // {a new span node}
  pointer u;  // {a new unset box}
  scaled w; // {natural width}
  glue_ord o; // {order of infinity}
  halfword n; // {span counter}

  if (cur_align == null)
    confusion("endv");

  q = link(cur_align);

  if (q == null)
    confusion("endv");

  if (align_state < 500000)
    fatal_error("(interwoven alignment preambles are not allowed)");

  p = link(q);

  // @<If the preamble list has been traversed, check that the row has ended@>
  if ((p == null) && (extra_info(cur_align) < cr_code))
    if (cur_loop != null)
    {
      link(q) = new_null_box();
      p = link(q);  // {a new alignrecord}
      info(p) = end_span;
      width(p) = null_flag;
      cur_loop = link(cur_loop);
      q = hold_head;
      r = u_part(cur_loop);

      while (r != null)
      {
        link(q) = get_avail();
        q = link(q);
        info(q) = info(r);
        r = link(r);
      }

      link(q) = null;
      u_part(p) = link(hold_head);
      q = hold_head;
      r = v_part(cur_loop);

      while (r != null)
      {
        link(q) = get_avail();
        q = link(q);
        info(q) = info(r);
        r = link(r);
      }

      link(q) = null;
      v_part(p) = link(hold_head);
      cur_loop = link(cur_loop);
      link(p) = new_glue(glue_ptr(cur_loop));
      subtype(link(p)) = tab_skip_code + 1;
    }
    else
    {
      print_err("Extra alignment tab has been changed to ");
      print_esc("cr");
      help3("You have given more \\span or & marks than there were",
          "in the preamble to the \\halign or \\valign now in progress.",
          "So I'll assume that you meant to type \\cr instead.");
      extra_info(cur_align) = cr_code;
      error();
    }

  if (extra_info(cur_align) != span_code)
  {
    unsave();
    new_save_level(align_group);

    // @<Package an unset box for the current column and record its width@>
    {
      if (mode == -hmode)
      {
        adjust_tail = cur_tail;
        adjust_hlist(head, false);
        delete_glue_ref(cur_kanji_skip);
        delete_glue_ref(cur_xkanji_skip);
        cur_kanji_skip = space_ptr(head);
        cur_xkanji_skip = xspace_ptr(head);
        add_glue_ref(cur_kanji_skip);
        add_glue_ref(cur_xkanji_skip);
        u = hpack(link(head), 0, 1);
        w = width(u);
        cur_tail = adjust_tail;
        adjust_tail = null;
      }
      else
      {
        u = vpackage(link(head), 0, 1, 0);
        w = height(u);
      }

      n = min_quarterword;

      if (cur_span != cur_align)
      {
        q = cur_span;

        do {
          incr(n);
          q = link(link(q));
        } while (!(q == cur_align));

        if (n > max_quarterword)
          confusion("256 spans");

        q = cur_span;

        while (link(info(q)) < n)
          q = info(q);

        if (link(info(q)) > n)
        {
          s = get_node(span_node_size);
          info(s) = info(q);
          link(s) = n;
          info(q) = s;
          width(s) = w;
        }
        else if (width(info(q)) < w)
          width(info(q)) = w;
      }
      else if (w > width(cur_align))
        width(cur_align) = w;

      type(u) = unset_node;
      span_count(u) = n;

      if (total_stretch[filll] != 0)
        o = filll;
      else if (total_stretch[fill] != 0)
        o = fill;
      else if (total_stretch[fil] != 0)
        o = fil;
      else
        o = normal;

      glue_order(u) = o;
      glue_stretch(u) = total_stretch[o];

      if (total_shrink[filll] != 0)
        o = filll;
      else if (total_shrink[fill] != 0)
        o = fill;
      else if (total_shrink[fil] != 0)
        o = fil;
      else
        o = normal;

      glue_sign(u) = o;
      glue_shrink(u) = total_shrink[o];
      pop_nest();
      link(tail) = u;
      tail = u;
    }

    tail_append(new_glue(glue_ptr(link(cur_align))));
    subtype(tail) = tab_skip_code + 1;

    if (extra_info(cur_align) >= cr_code)
    {
      return true;
    }

    init_span(p);
  }

  align_state = 1000000;

  do {
    get_x_or_protected();
  } while (!(cur_cmd != spacer));

  cur_align = p;
  init_col();

  return false;
}

void align_peek (void)
{
restart:
  align_state = 1000000;

  do {
    get_x_or_protected();
  } while (!(cur_cmd != spacer));

  if (cur_cmd == no_align)
  {
    scan_left_brace();
    new_save_level(no_align_group);

    if (mode == -vmode)
      normal_paragraph();
  }
  else if (cur_cmd == right_brace)
    fin_align();
  else if ((cur_cmd == car_ret) && (cur_chr == cr_cr_code))
    goto restart; // {ignore \.{\\crcr}}
  else
  {
    init_row(); // {start a new row}
    init_col(); // {start a new column and replace what we peeked at}
  }
}

/* sec 0815 */
static void line_break (boolean d)
{
  boolean auto_breaking;  // {is node |cur_p| outside a formula?}
  pointer prev_p; // {helps to determine when glue nodes are breakpoints}
  pointer q, r, s, prev_s;  // {miscellaneous nodes of temporary interest}
  internal_font_number f, post_f; // {used when calculating character widths}
  pointer post_p;
  ASCII_code cc;
  boolean first_use;
  /* small_number j; */
  int j;  // {an index into |hc| or |hu|}
  /* unsigned char c; */
  unsigned int c; // {character being considered for hyphenation}

  pack_begin_line = mode_line;  // {this is for over/underfull box messages}

  // @<Get ready to start line breaking@>;
  first_use = true;
  chain = false;
  delete_glue_ref(cur_kanji_skip);
  delete_glue_ref(cur_xkanji_skip);
  cur_kanji_skip = space_ptr(head);
  cur_xkanji_skip = xspace_ptr(head);
  add_glue_ref(cur_kanji_skip);
  add_glue_ref(cur_xkanji_skip);

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    free_node(tail, small_node_size);
    tail = prev_node;
    link(tail) = null;
  }

  link(temp_head) = link(head);

  if (is_char_node(tail))
    tail_append(new_penalty(inf_penalty));
  else if (type(tail) != glue_node)
    tail_append(new_penalty(inf_penalty));
  else
  {
    type(tail) = penalty_node;
    delete_glue_ref(glue_ptr(tail));
    flush_node_list(leader_ptr(tail));
    penalty(tail) = inf_penalty;
  }

  link(tail) = new_param_glue(par_fill_skip_code);
  last_line_fill = link(tail);
  init_cur_lang = prev_graf % 0200000;
  init_l_hyf = prev_graf / 020000000;
  init_r_hyf = (prev_graf / 0200000) % 0100;
  pop_nest();
  no_shrink_error_yet = true;
  check_shrinkage(left_skip);
  check_shrinkage(right_skip);
  q = left_skip;
  r = right_skip;
  background[1] = width(q) + width(r);
  background[2] = 0;
  background[3] = 0;
  background[4] = 0;
  background[5] = 0;
  background[2 + stretch_order(q)] = stretch(q);
  background[2 + stretch_order(r)] = background[2 + stretch_order(r)] + stretch(r);
  background[6] = shrink(q) + shrink(r);
  do_last_line_fit = false;
  active_node_size = active_node_size_normal;

  if (last_line_fit > 0)
  {
    q = glue_ptr(last_line_fill);

    if ((stretch(q) > 0) && (stretch_order(q) > normal))
    {
      if ((background[3] == 0) && (background[4] == 0) && (background[5] == 0))
      {
        do_last_line_fit = true;
        active_node_size = active_node_size_extended;
        fill_width[0] = 0;
        fill_width[1] = 0;
        fill_width[2] = 0;
        fill_width[stretch_order(q) - 1] = stretch(q);
      }
    }
  }

  minimum_demerits = awful_bad;
  minimal_demerits[tight_fit] = awful_bad;
  minimal_demerits[decent_fit] = awful_bad;
  minimal_demerits[loose_fit] = awful_bad;
  minimal_demerits[very_loose_fit] = awful_bad;

  if (par_shape_ptr == null)
  {
    if (hang_indent == 0)
    {
      last_special_line = 0;
      second_width = hsize;
      second_indent = 0;
    }
    else
    {
      // @<Set line length parameters in preparation for hanging indentation@>
      last_special_line = abs(hang_after);

      if (hang_after < 0)
      {
        first_width = hsize - abs(hang_indent);

        if (hang_indent >= 0)
          first_indent = hang_indent;
        else
          first_indent = 0;

        second_width = hsize;
        second_indent = 0;
      }
      else
      {
        first_width = hsize;
        first_indent = 0;
        second_width = hsize - abs(hang_indent);

        if (hang_indent >= 0)
          second_indent = hang_indent;
        else
          second_indent = 0;
      }
    }
  }
  else
  {
    last_special_line = info(par_shape_ptr) - 1;
    second_width = mem[par_shape_ptr + 2 * (last_special_line + 1)].sc;
    second_indent = mem[par_shape_ptr + 2 * last_special_line + 1].sc;
  }

  if (looseness == 0)
    easy_line = last_special_line;
  else
    easy_line = max_halfword;

  // @<Find optimal breakpoints@>;
  threshold = pretolerance;

  if (threshold >= 0)
  {
#ifdef STAT
    if (tracing_paragraphs > 0)
    {
      begin_diagnostic();
      print_nl("@firstpass");
    }
#endif

    second_pass = false;
    final_pass = false;
    lbs_pass_fst++;
  }
  else
  {
    threshold = tolerance;
    second_pass = true;
    final_pass = (emergency_stretch <= 0);

#ifdef STAT
    if (tracing_paragraphs > 0)
      begin_diagnostic();
#endif
  }

  while (true)
  {
    if (threshold > inf_bad)
      threshold = inf_bad;

    if (second_pass)
    {
      // @<Initialize for hyphenating a paragraph@>
      if (aptex_env.flag_initex)
      {
        if (trie_not_ready)
          init_trie();
      }

      cur_lang = init_cur_lang;
      l_hyf = init_l_hyf;
      r_hyf = init_r_hyf;
      set_hyph_index();
    }

    // @<Create an active breakpoint representing the beginning of the paragraph@>;
    q = get_node(active_node_size);
    type(q) = unhyphenated;
    fitness(q) = decent_fit;
    link(q) = active;
    break_node(q) = null;
    line_number(q) = prev_graf + 1;
    total_demerits(q) = 0;
    link(active) = q;

    if (do_last_line_fit)
    {
      active_short(q) = 0;
      active_glue(q) = 0;
    }

    act_width = background[1];
    do_all_six(store_background);
    passive = null;
    printed_node = temp_head;
    pass_number = 0;
    font_in_short_display = null_font;
    cur_p = link(temp_head);
    auto_breaking = true;
    prev_p = cur_p; // {glue at beginning is not a legal breakpoint}

    while ((cur_p != null) && (link(active) != last_active))
    {
      /*
        @<Call |try_break| if |cur_p| is a legal breakpoint;
        on the second pass, also try to hyphenate the next
        word, if |cur_p| is a glue node;
        then advance |cur_p| to the next node of the paragraph
        that could possibly be a legal breakpoint@>;
      */
      if (is_char_node(cur_p))
      {
        chain = false;

        if (is_char_node(cur_p))
        {
          if (font_dir[font(cur_p)] != dir_default)
          {
            switch (type(prev_p))
            {
              case hlist_node:
              case vlist_node:
              case dir_node:
              case rule_node:
              case ligature_node:
              case disc_node:
              case math_node:
                {
                  cur_p = prev_p;
                  try_break(0, unhyphenated);
                  cur_p = link(cur_p);
                }
                break;

              default:
                do_nothing();
                break;
            }
          }
        }

        prev_p = cur_p;
        post_p = cur_p;
        post_f = font(post_p);

        do {
          f = post_f;
          cc = character(post_p);
          act_width = act_width + char_width(f, char_info(f, cc));
          post_p = link(cur_p);

          if (font_dir[f] != dir_default)
          {
            prev_p = cur_p;
            cur_p = post_p;
            post_p = link(post_p);

            if (is_char_node(post_p))
            {
              post_f = font(post_p);

              if (font_dir[post_f] != dir_default)
                chain = true;
              else
                chain = false;

              try_break(0, unhyphenated);
            }
            else
            {
              chain = false;

              switch (type(post_p))
              {
                case hlist_node:
                case vlist_node:
                case dir_node:
                case rule_node:
                case ligature_node:
                case disc_node:
                case math_node:
                  try_break(0, unhyphenated);
                  break;

                default:
                  do_nothing();
                  break;
              }
            }

            if (chain)
            {
              if (first_use)
              {
                check_shrinkage(cur_kanji_skip);
                first_use = false;
              }

              act_width = act_width + width(cur_kanji_skip);
              active_width[2 + stretch_order(cur_kanji_skip)] =
                active_width[2 + stretch_order(cur_kanji_skip)] + stretch(cur_kanji_skip);
              active_width[6] = active_width[6] + shrink(cur_kanji_skip);
            }

            prev_p = cur_p;
          }
          else if (is_char_node(post_p))
          {
            post_f = font(post_p);
            chain = false;

            if (font_dir[post_f] != dir_default)
              try_break(0, unhyphenated);
          }

          cur_p = post_p;
        } while (!(!is_char_node(cur_p)));

        chain = false;
      }

      switch (type(cur_p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case rule_node:
          act_width = act_width + width(cur_p);
          break;

        case whatsit_node:
          if (subtype(cur_p) == language_node)
          {
            cur_lang = what_lang(cur_p);
            l_hyf = what_lhm(cur_p);
            r_hyf = what_rhm(cur_p);
            set_hyph_index();
          }
          break;

        case glue_node:
          {
            if (auto_breaking)
            {
              if (is_char_node(prev_p))
                try_break(0, unhyphenated);
              else if (precedes_break(prev_p))
                try_break(0, unhyphenated);
              else if (type(prev_p) == kern_node)
              {
                if ((subtype(prev_p) != explicit) && (subtype(prev_p) != ita_kern))
                  try_break(0, unhyphenated);
              }
            }

            check_shrinkage(glue_ptr(cur_p));
            q = glue_ptr(cur_p);
            act_width = act_width + width(q);
            active_width[2 + stretch_order(q)] = active_width[2 + stretch_order(q)] + stretch(q);
            active_width[6] = active_width[6] + shrink(q);

            if (second_pass && auto_breaking)
            {
              prev_s = cur_p;
              s = link(prev_s);

              if (s != null)
              {
                while (true)
                {
                  if (is_char_node(s))
                  {
                    hf = font(s);

                    if (font_dir[hf] != dir_default)
                    {
                      prev_s = s;
                      s = link(prev_s);
                      c = info(s);
                      goto continu;
                    }
                    else
                      c = character(s);
                  }
                  else if (type(s) == disp_node)
                  {
                    goto continu;
                  }
                  else if ((type(s) == penalty_node) && (subtype(s) != normal))
                  {
                    goto continu;
                  }
                  else if (type(s) == ligature_node)
                  {
                    if (lig_ptr(s) == null)
                      goto continu;
                    else
                    {
                      q = lig_ptr(s);
                      c = character(q);
                      hf = font(q);
                    }
                  }
                  else if ((type(s) == kern_node) && (subtype(s) == normal))
                    goto continu;
                  else if ((type(s) == math_node) && (subtype(s) >= L_code))
                    goto continu;
                  else if (type(s) == whatsit_node)
                  {
                    if (subtype(s) == language_node)
                    {
                      cur_lang = what_lang(s);
                      l_hyf = what_lhm(s);
                      r_hyf = what_rhm(s);
                      set_hyph_index();
                    }
                    goto continu;
                  }
                  else
                    goto done1;

                  set_lc_code(c);

                  if (hc[0] != 0)
                  {
                    if ((hc[0] == (halfword) c) || (uc_hyph > 0))
                      goto done2;
                    else
                      goto done1;
                  }
continu:
                  prev_s = s;
                  s = link(prev_s);
                }
done2:
                hyf_char = hyphen_char[hf];

                if (hyf_char < 0)
                  goto done1; 

                if (hyf_char > 255)
                  goto done1;

                ha = prev_s;

                if (l_hyf + r_hyf > 63)
                  goto done1;

                hn = 0;

                while (true)
                {
                  if (is_char_node(s))
                  {
                    if (font(s) != hf)
                      goto done3;

                    hyf_bchar = character(s);

                    c = hyf_bchar;
                    set_lc_code(c);

                    if (hc[0] == 0)
                      goto done3;

                    if (hn == 63)
                      goto done3;

                    hb = s;
                    incr(hn);
                    hu[hn] = c;
                    hc[hn]= hc[0];
                    hyf_bchar = non_char;
                  }
                  else if (type(s) == ligature_node)
                  {
                    if (font(lig_char(s)) != hf)
                      goto done3;

                    j = hn;
                    q = lig_ptr(s);

                    if (q != null)
                      hyf_bchar = character(q);

                    while (q != null)
                    {
                      c = character(q);
                      set_lc_code(c);

                      if (hc[0] == 0)
                        goto done3;

                      if (j == 63)
                        goto done3;

                      incr(j);
                      hu[j] = c;
                      hc[j] = hc[0];
                      q = link(q);
                    }

                    hb = s;
                    hn = j;

                    if (odd(subtype(s)))
                      hyf_bchar = font_bchar[hf];
                    else
                      hyf_bchar = non_char;
                  }
                  else if ((type(s) == kern_node) && (subtype(s) == normal))
                  {
                    hb = s;
                    hyf_bchar = font_bchar[hf];
                  }
                  else
                    goto done3;

                  s = link(s);
                }
done3:
                if (hn < l_hyf + r_hyf)
                  goto done1;

                while (true)
                {
                  if (!(is_char_node(s)))
                  {
                    switch (type(s))
                    {
                      case ligature_node:
                        do_nothing();
                        break;
    
                      case kern_node:
                        if (subtype(s) != normal)
                          goto done4;
                        break;

                      case disp_node:
                        do_nothing();
                        break;

                      case whatsit_node:
                      case glue_node:
                      case penalty_node:
                      case ins_node:
                      case adjust_node:
                      case mark_node:
                        goto done4;
                        break;

                      case math_node:
                        if (subtype(s) >= L_code)
                          goto done4;
                        else
                          goto done1;
                        break;

                      default:
                        goto done1;
                        break;
                    }
                  }

                  s = link(s);
                }
done4:
                hyphenate();
              }
done1:;
            }
          }
          break;

        case kern_node:
          if ((subtype(cur_p) == explicit) || (subtype(cur_p) == ita_kern))
            kern_break();
          else
            act_width = act_width + width(cur_p);
          break;

        case ligature_node:
          {
            f = font(lig_char(cur_p));
            act_width = act_width + char_width(f, char_info(f, character(lig_char(cur_p))));
          }
          break;

        case disc_node:
          {
            s = pre_break(cur_p);
            disc_width = 0;

            if (s == null)
              try_break(ex_hyphen_penalty, hyphenated);
            else
            {
              do {
                if (is_char_node(s))
                {
                  f = font(s);
                  disc_width = disc_width + char_width(f, char_info(f, character(s)));

                  if (font_dir[f] != dir_default)
                    s = link(s);
                }
                else switch (type(s))
                {
                  case ligature_node:
                    {
                      f = font(lig_char(s));
                      disc_width = disc_width + char_width(f, char_info(f, character(lig_char(s))));
                    }
                    break;

                  case hlist_node:
                  case vlist_node:
                  case dir_node:
                  case rule_node:
                  case kern_node:
                    disc_width = disc_width + width(s);
                    break;

                  case disp_node:
                    do_nothing();
                    break;

                  default:
                    confusion("disc3");
                    break;
                }

                s = link(s);
              } while (!(s == null));

              act_width = act_width + disc_width;
              try_break(hyphen_penalty, hyphenated);
              act_width = act_width - disc_width;
            }

            r = replace_count(cur_p);
            s = link(cur_p);

            while (r > 0)
            {
              if (is_char_node(s))
              {
                f = font(s);
                act_width = act_width + char_width(f, char_info(f, character(s)));

                if (font_dir[f] != dir_default)
                  s = link(s);
              }
              else switch (type(s))
              {
                case ligature_node:
                  {
                    f = font(lig_char(s));
                    act_width = act_width + char_width(f, char_info(f, character(lig_char(s))));
                  }
                  break;

                case hlist_node:
                case vlist_node:
                case dir_node:
                case rule_node:
                case kern_node:
                  act_width = act_width + width(s);
                  break;

                case disp_node:
                  do_nothing();
                  break;

                default:
                  confusion("disc4");
                  break;
              }

              decr(r);
              s = link(s);
            }

            prev_p = cur_p;
            cur_p = s;
            goto done5;
          }
          break;

        case math_node:
          {
            if (subtype(cur_p) < L_code)
              auto_breaking = odd(subtype(cur_p));
            kern_break();
          }
          break;

        case penalty_node:
          try_break(penalty(cur_p), unhyphenated);
          break;

        case disp_node:
        case mark_node:
        case ins_node:
        case adjust_node:
          do_nothing();
          break;

        default:
          confusion("paragraph");
          break;
      }

      prev_p = cur_p;
      cur_p = link(cur_p);
done5:;
    }

    if (cur_p == null)
    {
      try_break(eject_penalty, hyphenated);

      if (link(active) != active)
      {
        r = link(active);
        fewest_demerits = awful_bad;

        do {
          if (type(r) != delta_node)
          {
            if (total_demerits(r) < fewest_demerits)
            {
              fewest_demerits = total_demerits(r);
              best_bet = r;
            }
          }

          r = link(r);
        } while (!(r == active));

        best_line = line_number(best_bet);

        if (looseness == 0)
        {
          goto done;
        }

        {
          r = link(active);
          actual_looseness = 0;

          do {
            if (type(r) != delta_node)
            {
              line_diff = line_number(r) - best_line;

              if (((line_diff < actual_looseness) && (looseness <= line_diff)) ||
                  ((line_diff > actual_looseness) && (looseness >= line_diff)))
              {
                best_bet = r;
                actual_looseness = line_diff;
                fewest_demerits = total_demerits(r);
              }
              else if ((line_diff == actual_looseness) && (total_demerits(r) < fewest_demerits))
              {
                best_bet = r;
                fewest_demerits = total_demerits(r);
              }
            }

            r = link(r);
          } while (!(r == active));

          best_line = line_number(best_bet);
        }

        if ((actual_looseness == looseness) || final_pass)
        {
          goto done;
        }
      }
    }

    q = link(active);

    while (q != active)
    {
      cur_p = link(q);

      if (type(q) == delta_node)
        free_node(q, delta_node_size);
      else
        free_node(q, active_node_size);

      q = cur_p;
    }

    q = passive;

    while (q != null)
    {
      cur_p = link(q);
      free_node(q, passive_node_size);
      q = cur_p;
    }

    if (!second_pass)
    {
#ifdef STAT
      if (tracing_paragraphs > 0)
        print_nl("@secondpass");
#endif

      threshold = tolerance;
      second_pass = true;
      lbs_pass_snd++;
      final_pass = (emergency_stretch <= 0);
    }
    else
    {
#ifdef STAT
      if (tracing_paragraphs > 0)
        print_nl("@emergencypass");
#endif

      background[2] = background[2] + emergency_stretch;
      final_pass = true;
      ++lbs_pass_fin;
    }
  }

done:
  if (best_line == decent_fit)
    lbs_sing_line++;

#ifdef STAT
  if (tracing_paragraphs > 0)
  {
    end_diagnostic(true);
    normalize_selector();
  }
#endif

  if (do_last_line_fit)
  {
    if (active_short(best_bet) == 0)
      do_last_line_fit = false;
    else
    {
      q = new_spec(glue_ptr(last_line_fill));
      delete_glue_ref(glue_ptr(last_line_fill));
      width(q) = width(q) + active_short(best_bet) - active_glue(best_bet);
      stretch(q) = 0; glue_ptr(last_line_fill) = q;
    }
  }

  post_line_break(d);
  q = link(active);

  while (q != active)
  {
    cur_p = link(q);

    if (type(q) == delta_node)
      free_node(q, delta_node_size);
    else
      free_node(q, active_node_size);

    q = cur_p;
  }

  q = passive;

  while (q != null)
  {
    cur_p = link(q);
    free_node(q, passive_node_size);
    q = cur_p;
  }

  pack_begin_line = 0;
}

// recovers from infinite shrinkage
pointer finite_shrink (pointer p)
{
  pointer q;  // {new glue specification}

  if (no_shrink_error_yet)
  {
    no_shrink_error_yet = false;
#ifdef STAT
    if (tracing_paragraphs > 0)
      end_diagnostic(true);
#endif
    print_err("Infinite glue shrinkage found in a paragraph");
    help5("The paragraph just ended includes some glue that has",
        "infinite shrinkability, e.g., `\\hskip 0pt minus 1fil'.",
        "Such glue doesn't belong there---it allows a paragraph",
        "of any length to fit on one line. But it's safe to proceed,",
        "since the offensive shrinkability has been made finite.");
    error();
#ifdef STAT
    if (tracing_paragraphs > 0)
      begin_diagnostic();
#endif
  }

  q = new_spec(p);
  shrink_order(q) = normal;
  delete_glue_ref(p);

  return q;
}

void try_break (integer pi, small_number break_type)
{
  pointer r;                    // {runs through the active list}
  pointer prev_r;               // {stays a step behind |r|}
  halfword old_l;               // {maximum line number in current equivalence class of lines}
  boolean no_break_yet;         // {have we found a feasible break at |cur_p|?}
  pointer prev_prev_r;          // {a step behind |prev_r|, if |type(prev_r)=delta_node|}
  pointer s;                    // {runs through nodes ahead of |cur_p|}
  pointer q;                    // {points to a new node being created}
  pointer v;                    // {points to a glue specification or a node ahead of |cur_p|}
  integer t;                    // {node count, if |cur_p| is a discretionary node}
  internal_font_number f;       // {used in character width calculation}
  halfword l;                   // {line number of current active node}
  boolean node_r_stays_active;  // {should node |r| remain in the active list?}
  scaled line_width;            // {the current line will be justified to this width}
  uint32_t fit_class;           // {possible fitness class of test line}
  halfword b;                   // {badness of test line}
  integer d;                    // {demerits of test line}
  boolean artificial_demerits;  // {has |d| been forced to zero?}
  pointer save_link;            // {temporarily holds value of |link(cur_p)|}
  scaled shortfall;             // {used in badness calculations}
  scaled g;                     // {glue stretch or shrink of test line, adjustment for last line}

  // @<Make sure that |pi| is in the proper range@>
  if (abs(pi) >= inf_penalty)
    if (pi > 0)
      goto exit;  // {this breakpoint is inhibited by infinite penalty}
    else
      pi = eject_penalty; // {this breakpoint will be forced}

  no_break_yet = true;
  prev_r = active;
  old_l = 0;
  do_all_six(copy_to_cur_active);

  while (true)
  {
continu:
    r = link(prev_r);

    /*
      @<If node |r| is of type |delta_node|, update |cur_active_width|,
      set |prev_r| and |prev_prev_r|, then |goto continue|@>
    */

    if (type(r) == delta_node)
    {
      do_all_six(update_width);
      prev_prev_r = prev_r;
      prev_r = r;
      goto continu;
    }

    /*
      @<If a line number class has ended, create new active nodes for
      the best feasible breaks in that class; then |return|
      if |r=last_active|, otherwise compute the new |line_width|@>
    */
    {
      l = line_number(r);

      if (l > old_l)
      {
        if ((minimum_demerits < awful_bad) && ((old_l != easy_line) || (r == active)))
        {
          if (no_break_yet)
          {
            no_break_yet = false;
            do_all_six(set_break_width_to_background);
            s = cur_p;

            if (break_type > unhyphenated)
              if (cur_p != null)
              {
                t = replace_count(cur_p);
                v = cur_p;
                s = post_break(cur_p);

                while (t > 0)
                {
                  decr(t);
                  v = link(v);

                  if (is_char_node(v))
                  {
                    f = font(v);
                    break_width[1] = break_width[1] - char_width(f, char_info(f, character(v)));

                    if (font_dir[f] != dir_default)
                      v = link(v);
                  }
                  else switch (type(v))
                  {
                    case ligature_node:
                      {
                        f = font(lig_char(v));
                        break_width[1] = break_width[1] - char_width(f, char_info(f, character(lig_char(v))));
                      }
                      break;

                    case hlist_node:
                    case vlist_node:
                    case dir_node:
                    case rule_node:
                    case kern_node:
                      break_width[1] = break_width[1] - width(v);
                      break;

                    case disp_node:
                      do_nothing();
                      break;

                    default:
                      confusion("disc1");
                      break;
                  }
                }

                while (s != null)
                {
                  if (is_char_node(s))
                  {
                    f = font(s);
                    break_width[1] = break_width[1] + char_width(f, char_info(f, character(s)));

                    if (font_dir[f] != dir_default)
                      s = link(s);
                  }
                  else switch (type(s))
                  {
                    case ligature_node:
                      {
                        f = font(lig_char(s));
                        break_width[1] = break_width[1] + char_width(f, char_info(f, character(lig_char(s))));
                      }
                      break;

                    case hlist_node:
                    case vlist_node:
                    case dir_node:
                    case rule_node:
                    case kern_node:
                      break_width[1] = break_width[1] + width(s);
                      break;

                    case disp_node:
                      do_nothing();
                      break;

                    default:
                      confusion("disc2");
                      break;
                  }

                  s = link(s);
                }

                break_width[1] = break_width[1] + disc_width;

                if (post_break(cur_p) == null)
                  s = link(v);
              }

            while (s != null)
            {
              if (is_char_node(s))
              {
                if (chain)
                {
                  break_width[1] = break_width[1] - width(cur_kanji_skip);
                  break_width[2 + stretch_order(cur_kanji_skip)] =
                    break_width[2 + stretch_order(cur_kanji_skip)] - stretch(cur_kanji_skip);
                  break_width[6] = break_width[6] - shrink(cur_kanji_skip);
                }

                goto done;
              }

              switch (type(s))
              {
                case glue_node:
                  {
                    v = glue_ptr(s);
                    break_width[1] = break_width[1] - width(v);
                    break_width[2 + stretch_order(v)] = break_width[2 + stretch_order(v)] - stretch(v);
                    break_width[6] = break_width[6] - shrink(v);
                  }
                  break;

                case penalty_node:
                  do_nothing();
                  break;

                case math_node:
                  break_width[1] = break_width[1] - width(s);
                  break;

                case kern_node:
                  if ((subtype(s) != explicit) && (subtype(s) != ita_kern))
                    goto done;
                  else
                    break_width[1] = break_width[1] - width(s);
                  break;

                default:
                  goto done;
                  break;
              }

              s = link(s);
            }
done:;
          }

          if (type(prev_r) == delta_node)
          {
            do_all_six(convert_to_break_width);
          }
          else if (prev_r == active)
          {
            do_all_six(store_break_width);
          }
          else
          {
            q = get_node(delta_node_size);
            link(q) = r;
            type(q) = delta_node;
            subtype(q) = 0;
            do_all_six(new_delta_to_break_width);
            link(prev_r) = q;
            prev_prev_r = prev_r;
            prev_r = q;
          }

          if (abs(adj_demerits) >= awful_bad - minimum_demerits)
            minimum_demerits = awful_bad - 1;
          else
            minimum_demerits = minimum_demerits + abs(adj_demerits);

          for (fit_class = very_loose_fit; fit_class <= tight_fit; fit_class++)
          {
            if (minimal_demerits[fit_class] <= minimum_demerits)
            {
              q = get_node(passive_node_size);
              link(q) = passive;
              passive = q;
              cur_break(q) = cur_p;

#ifdef STAT
              incr(pass_number);
              serial(q) = pass_number;
#endif

              prev_break(q) = best_place[fit_class];
              q = get_node(active_node_size);
              break_node(q) = passive;
              line_number(q) = best_pl_line[fit_class] + 1;
              fitness(q) = fit_class;
              type(q) = break_type;
              total_demerits(q) = minimal_demerits[fit_class];

              if (do_last_line_fit)
              {
                active_short(q) = best_pl_short[fit_class];
                active_glue(q) = best_pl_glue[fit_class];
              }

              link(q) = r;
              link(prev_r) = q;
              prev_r = q;

#ifdef STAT
              if (tracing_paragraphs > 0)
              {
                print_nl("@@");
                print_int(serial(passive));
                prints(": line ");
                print_int(line_number(q) - 1);
                print_char('.');
                print_int(fit_class);

                if (break_type == hyphenated)
                  print_char('-');

                prints(" t=");
                print_int(total_demerits(q));

                if (do_last_line_fit)
                {
                  prints(" s=");
                  print_scaled(active_short(q));

                  if (cur_p == null)
                    prints(" a=");
                  else
                    prints(" g=");

                  print_scaled(active_glue(q));
                }

                prints(" -> @@");

                if (prev_break(passive) == null)
                  print_char('0');
                else
                  print_int(serial(prev_break(passive)));
              }
#endif
            }

            minimal_demerits[fit_class] = awful_bad;
          }

          minimum_demerits = awful_bad;

          if (r != active)
          {
            q = get_node(delta_node_size);
            link(q) = r;
            type(q) = delta_node;
            subtype(q) = 0;
            do_all_six(new_delta_from_break_width);
            link(prev_r) = q;
            prev_prev_r = prev_r;
            prev_r = q;
          }
        }

        if (r == active)
          goto exit;

        if (l > easy_line)
        {
          line_width = second_width;
          old_l = max_halfword - 1;
        }
        else
        {
          old_l = l;

          if (l > last_special_line)
            line_width = second_width;
          else if (par_shape_ptr == 0)
            line_width = first_width;
          else
            line_width = mem[par_shape_ptr + 2 * l].sc;
        }
      }
    }

    {
      artificial_demerits = false;
      shortfall = line_width - cur_active_width[1];

      if (shortfall > 0)
        if ((cur_active_width[3] != 0) || (cur_active_width[4] != 0) ||
          (cur_active_width[5] != 0))
        {
          if (do_last_line_fit)
          {
            if (cur_p == null)
            {
              if ((active_short(r) == 0) || (active_glue(r) <= 0))
                goto not_found;

              if ((cur_active_width[3] != fill_width[0]) ||
                (cur_active_width[4] != fill_width[1]) ||
                (cur_active_width[5] != fill_width[2]))
                goto not_found;

              if (active_short(r) > 0)
                g = cur_active_width[2];
              else
                g = cur_active_width[6];

              if (g <= 0)
                goto not_found;

              arith_error = false;
              g = fract(g, active_short(r), active_glue(r), max_dimen);

              if (last_line_fit < 1000)
                g = fract(g, last_line_fit, 1000, max_dimen);

              if (arith_error)
                if (active_short(r)>0)
                  g = max_dimen;
                else
                  g = -max_dimen;

              if (g > 0)
              {
                if (g > shortfall)
                  g = shortfall;

                if (g > 7230584)
                  if (cur_active_width[2] < 1663497)
                  {
                    b = inf_bad;
                    fit_class = very_loose_fit;
                    goto found;
                  }

                b = badness(g, cur_active_width[2]);
                  
                if (b > 12)
                  if (b > 99)
                    fit_class = very_loose_fit;
                  else
                    fit_class = loose_fit;
                else
                  fit_class = decent_fit;

                goto found;
              }
              else if (g < 0)
              {
                if (-g > cur_active_width[6])
                  g = -cur_active_width[6];

                b = badness(-g, cur_active_width[6]);

                if (b > 12)
                  fit_class = tight_fit;
                else
                  fit_class = decent_fit;

                goto found;
              }

not_found:;
            }

            shortfall = 0;
          }

          b = 0;
          fit_class = decent_fit;
        }
        else
        {
          if (shortfall > 7230584)
            if (cur_active_width[2] < 1663497)
            {
              b = inf_bad;
              fit_class = very_loose_fit;
              goto done1;
            }

          b = badness(shortfall, cur_active_width[2]);

          if (b > 12)
            if (b > 99)
              fit_class = very_loose_fit;
            else
              fit_class = loose_fit;
          else
            fit_class = decent_fit;
done1:;
        }
      else
      {
        if (-shortfall > cur_active_width[6])
          b = inf_bad + 1;
        else
          b = badness(-shortfall, cur_active_width[6]);

        if (b > 12)
          fit_class = tight_fit;
        else
          fit_class = decent_fit;
      }

      if (do_last_line_fit)
      {
        if (cur_p == null)
          shortfall = 0;

        if (shortfall > 0)
          g = cur_active_width[2];
        else if (shortfall < 0)
          g = cur_active_width[6];
        else
          g = 0;
      }

found:
      if ((b > inf_bad) || (pi == eject_penalty))
      {
        if (final_pass && (minimum_demerits == awful_bad) &&
          (link(r) == active) && (prev_r == active))
          artificial_demerits = true;
        else if (b > threshold)
          goto deactivate;

        node_r_stays_active = false;
      }
      else
      {
        prev_r = r;

        if (b > threshold)
          goto continu;

        node_r_stays_active = true;
      }

      if (artificial_demerits)
        d = 0;
      else
      {
        d = line_penalty + b;

        if (abs(d) >= 10000)
          d = 100000000;
        else
          d = d * d;

        if (pi != 0)
          if (pi > 0)
            d = d + pi * pi;
          else if (pi > eject_penalty)
            d = d - pi * pi;

        if ((break_type == hyphenated) && (type(r) == hyphenated))
          if (cur_p != null)
            d = d + double_hyphen_demerits;
          else
            d = d + final_hyphen_demerits;

        if (abs(fit_class - fitness(r)) > 1)
          d = d + adj_demerits;
      }

#ifdef STAT
      if (tracing_paragraphs > 0)
      {
        if (printed_node != cur_p)
        {
          print_nl("");

          if (cur_p == null)
            short_display(link(printed_node));
          else
          {
            save_link = link(cur_p);
            link(cur_p) = null;
            print_nl("");
            short_display(link(printed_node));
            link(cur_p) = save_link;
          }

          printed_node = cur_p;
        }

        print_nl("@");

        if (cur_p == null)
          print_esc("par");
        else if ((type(cur_p) != glue_node) && (!is_char_node(cur_p)))
        {
          if (type(cur_p) == penalty_node)
            print_esc("penalty");
          else if (type(cur_p) == disc_node)
            print_esc("discretionary");
          else if (type(cur_p) == kern_node)
            print_esc("kern");
          else
            print_esc("math");
        }

        prints(" via @@");

        if (break_node(r) == null)
          print_char('0');
        else
          print_int(serial(break_node(r)));

        prints(" b=");

        if (b > inf_bad)
          print_char('*');
        else
          print_int(b);

        prints(" p=");
        print_int(pi);
        prints(" d=");

        if (artificial_demerits)
          print_char('*');
        else
          print_int(d);
      }
#endif

      d = d + total_demerits(r);

      if (d <= minimal_demerits[fit_class])
      {
        minimal_demerits[fit_class] = d;
        best_place[fit_class] = break_node(r);
        best_pl_line[fit_class] = l;

        if (do_last_line_fit)
        {
          best_pl_short[fit_class] = shortfall;
          best_pl_glue[fit_class] = g;
        }

        if (d < minimum_demerits)
          minimum_demerits = d;
      }

      if (node_r_stays_active)
        goto continu;

deactivate:
      link(prev_r) = link(r);
      free_node(r, active_node_size);

      if (prev_r == active)
      {
        r = link(active);

        if (type(r) == delta_node)
        {
          do_all_six(update_active);
          do_all_six(copy_to_cur_active);
          link(active) = link(r);
          free_node(r, delta_node_size);
        }
      }
      else if (type(prev_r) == delta_node)
      {
        r = link(prev_r);

        if (r == last_active)
        {
          do_all_six(downdate_width);
          link(prev_prev_r) = last_active;
          free_node(prev_r, delta_node_size);
          prev_r = prev_prev_r;
        }
        else if (type(r) == delta_node)
        {
          do_all_six(update_width);
          do_all_six(combine_two_deltas);
          link(prev_r) = link(r);
          free_node(r, delta_node_size);
        }
      }
    }
  }

exit:
#ifdef STAT
  if (cur_p == printed_node)
    if (cur_p != null)
      if (type(cur_p) == disc_node)
      {
        t = replace_count(cur_p);

        while (t > 0)
        {
          decr(t);
          printed_node = link(printed_node);
        }
      }
#endif
}

void post_line_break (boolean d)
{
  pointer q, r, s;          // {temporary registers for list manipulation}
  boolean disc_break;       // {was the current break at a discretionary node?}
  boolean post_disc_break;  // {and did it have a nonempty post-break part?}
  scaled cur_width;         // {width of line number |cur_line|}
  scaled cur_indent;        // {left margin of line number |cur_line|}
  quarterword t;            // {used for replacement counts in discretionary nodes}
  integer pen;              // {use when calculating penalties between lines}
  halfword cur_line;        // {the current line number being justified}
  pointer LR_ptr;           // {stack of LR codes}

  LR_ptr = LR_save;
  q = break_node(best_bet);
  cur_p = null;

  do {
    r = q;
    q = prev_break(q);
    next_break(r) = cur_p;
    cur_p = r;
  } while (!(q == null));

  cur_line = prev_graf + 1;
  last_disp = 0;

  do {
    if (TeXXeT_en)
    {
      q = link(temp_head);

      if (LR_ptr != null)
      {
        temp_ptr = LR_ptr;
        r = q;

        do {
          s = new_math(0, begin_LR_type(info(temp_ptr)));
          link(s) = r;
          r = s;
          temp_ptr = link(temp_ptr);
        } while (!(temp_ptr == null));

        link(temp_head) = r;
      }

      while (q != cur_break(cur_p))
      {
        if (!is_char_node(q))
          if (type(q) == math_node)
            adjust_the_LR_stack_p();

        q = link(q);
      }
    }

    q = cur_break(cur_p);
    disc_break = false;
    post_disc_break = false;

    if (q != null)
    {
      if (!is_char_node(q))
        if (type(q) == glue_node)
        {
          delete_glue_ref(glue_ptr(q));
          glue_ptr(q) = right_skip;
          subtype(q) = right_skip_code + 1;
          add_glue_ref(right_skip);
          goto done;
        }
        else
        {
          if (type(q) == disc_node)
          {
            t = replace_count(q);

            if (t == 0)
              r = link(q);
            else
            {
              r = q;

              while (t > 1)
              {
                r = link(r);
                decr(t);
              }

              s = link(r);
              r = link(s);
              link(s) = null;
              flush_node_list(link(q));
              replace_count(q) = 0;
            }

            if (post_break(q) != null)
            {
              s = post_break(q);

              while (link(s) != null)
                s = link(s);

              link(s) = r;
              r = post_break(q);
              post_break(q) = null;
              post_disc_break = true;
            }

            if (pre_break(q) != null)
            {
              s = prev_break(q);
              link(q) = s;

              while (link(s) != null)
                s = link(s);

              prev_break(q) = null;
              q = s;
            }

            link(q) = r;
            disc_break = true;
          }
          else if (type(q) == kern_node)
            width(q) = 0;
          else if (type(q) == math_node)
          {
            width(q) = 0;

            if (TeXXeT_en)
              adjust_the_LR_stack_p();
          }
        }
    }
    else
    {
      q = temp_head;

      while (link(q) != null)
        q = link(q);
    }

    r = new_param_glue(right_skip_code);
    link(r) = link(q);
    link(q) = r;
    q = r;

done:
    if (TeXXeT_en)
      if (LR_ptr != null)
      {
        s = temp_head;
        r = link(s);

        while (r != q)
        {
          s = r;
          r = link(s);
        }

        r = LR_ptr;

        while (r != null)
        {
          temp_ptr = new_math(0, info(r));
          link(s) = temp_ptr;
          s = temp_ptr;
          r = link(r);
        }

        link(s) = q;
      }

    r = link(q);
    link(q) = null;
    q = link(temp_head);
    link(temp_head) = r;

    if (last_disp != 0)
    {
      r = get_node(small_node_size);
      type(r) = disp_node;
      disp_dimen(r) = last_disp;
      link(r) = q;
      q = r;
      disp_called = true;
    }

    if (left_skip != zero_glue)
    {
      r = new_param_glue(left_skip_code);
      link(r) = q;
      q = r;
    }

    if (cur_line > last_special_line)
    {
      cur_width = second_width;
      cur_indent = second_indent;
    }
    else if (par_shape_ptr == null)
    {
      cur_width = first_width;
      cur_indent = first_indent;
    }
    else
    {
      cur_width = mem[par_shape_ptr + 2 * cur_line].sc;
      cur_indent = mem[par_shape_ptr + 2 * cur_line - 1].sc;
    }

    adjust_tail = adjust_head;
    just_box = hpack(q, cur_width, exactly);
    shift_amount(just_box) = cur_indent;
    append_to_vlist(just_box);

    if (adjust_head != adjust_tail)
    {
      link(tail) = link(adjust_head);
      tail = adjust_tail;
    }

    adjust_tail = null;

    if (cur_line + 1 != best_line)
    {
      q = inter_line_penalties_ptr;

      if (q != null)
      {
        r = cur_line;

        if (r > penalty(q))
          r = penalty(q);

        pen = penalty(q + r);
      }
      else
        pen = inter_line_penalty;

      q = club_penalties_ptr;

      if (q != null)
      {
        r = cur_line - prev_graf;

        if (r > penalty(q))
          r = penalty(q);

        pen = pen + penalty(q + r);
      }
      else if (cur_line == prev_graf + 1)
        pen = pen + club_penalty;

      if (d)
        q = display_widow_penalties_ptr;
      else
        q = widow_penalties_ptr;

      if (q != null)
      {
        r = best_line - cur_line - 1;

        if (r > penalty(q))
          r = penalty(q);

        pen = pen + penalty(q + r);
      }
      else if (cur_line + 2 == best_line)
        if (d)
          pen = pen + display_widow_penalty;
        else
          pen = pen + widow_penalty;

      if (disc_break)
        pen = pen + broken_penalty;

      if (pen != 0)
      {
        r = new_penalty(pen);
        link(tail) = r;
        tail = r;
      }
    }

    incr(cur_line);
    cur_p = next_break(cur_p);

    if (cur_p != null)
      if (!post_disc_break)
      {
        r = temp_head;

        while (true)
        {
          q = link(r);

          if (q == cur_break(cur_p))
            goto done1;

          if (is_char_node(q))
            goto done1;

          if (non_discardable(q))
            goto done1;

          if (type(q) == kern_node)
            if ((subtype(q) != explicit) && (subtype(q) != ita_kern))
              goto done1;

          r = q;

          if (type(q) == math_node)
            if (TeXXeT_en)
              adjust_the_LR_stack_p();
        }

done1:
        if (r != temp_head)
        {
          link(r) = null;
          flush_node_list(link(temp_head));
          link(temp_head) = q;
        }
      }
  } while (!(cur_p == null));

  if ((cur_line != best_line) || (link(temp_head) != null))
    confusion("line breaking");

  prev_graf = best_line - 1;
  LR_save = LR_ptr;
}

static small_number reconstitute (small_number j, small_number n, halfword bchar, halfword hchar)
{
  pointer p;          // {temporary register for list manipulation}
  pointer t;          // {a node being appended to}
  four_quarters q;    // {character information or a lig/kern instruction}
  halfword cur_rh;    // {hyphen character for ligature testing}
  halfword test_char; // {hyphen or other character for ligature testing}
  scaled w;           // {amount of kerning}
  font_index k;       // {position of current lig/kern instruction}

  hyphen_passed = 0;
  t = hold_head;
  w = 0;
  link(hold_head) = null;
  cur_l = hu[j];
  cur_q = t;

  if (j == 0)
  {
    ligature_present = init_lig;
    p = init_list; 

    if (ligature_present)
      lft_hit = init_lft; 

    while (p != null)
    {
      append_charnode_to_t(character(p));
      p = link(p);
    }
  }
  else if (cur_l < non_char)
    append_charnode_to_t(cur_l);

  lig_stack = null;
  set_cur_r();

continu:
  if (cur_l == non_char)
  {
    k = bchar_label[hf];

    if (k == non_address)
      goto done;
    else
      q = font_info[k].qqqq;
  }
  else
  {
    q = char_info(hf, cur_l);

    if (char_tag(q) != lig_tag)
      goto done;

    k = lig_kern_start(hf, q);
    q = font_info[k].qqqq;

    if (skip_byte(q) > stop_flag)
    {
      k = lig_kern_restart(hf, q);
      q = font_info[k].qqqq;
    }
  }

  if (cur_rh < non_char)
    test_char = cur_rh;
  else
    test_char = cur_r;

  while (true)
  {
    if (next_char(q) == test_char)
      if (skip_byte(q) <= stop_flag)
        if (cur_rh < non_char)
        {
          hyphen_passed = j;
          hchar = non_char;
          cur_rh = non_char;
          goto continu;
        }
        else
        {
          if (hchar < non_char)
            if (odd(hyf[j]))
            {
              hyphen_passed = j;
              hchar = non_char;
            }

          if (op_byte(q) < kern_flag)
          {
            if (cur_l == non_char)
              lft_hit = true;

            if (j == n)
              if (lig_stack == null)
                rt_hit = true;

            check_interrupt();

            switch (op_byte(q))
            {
              case 1:
              case 5:
                {
                  cur_l = rem_byte(q);
                  ligature_present = true;
                }
                break;

              case 2:
              case 6:
                {
                  cur_r = rem_byte(q);

                  if (lig_stack != null)
                    character(lig_stack) = cur_r;
                  else
                  {
                    lig_stack = new_lig_item(cur_r);

                    if (j == n)
                      bchar = non_char;
                    else
                    {
                      p = get_avail();
                      list_ptr(lig_stack) = p;
                      character(p) = hu[j + 1];
                      font(p) = hf;
                    }
                  }
                }
                break;

              case 3:
                {
                  cur_r = rem_byte(q);
                  p = lig_stack;
                  lig_stack = new_lig_item(cur_r);
                  link(lig_stack) = p;
                }
                break;

              case 7:
              case 11:
                {
                  wrap_lig(false);
                  cur_q = t;
                  cur_l = rem_byte(q);
                  ligature_present = true;
                }
                break;

              default:
                {
                  cur_l = rem_byte(q);
                  ligature_present = true;

                  if (lig_stack != null)
                    pop_lig_stack();
                  else if (j == n)
                    goto done;
                  else
                  {
                    append_charnode_to_t(cur_r);
                    incr(j);
                    set_cur_r();
                  }
                }
                break;
            }

            if (op_byte(q) > 4)
              if (op_byte(q) != 7)
                goto done;

            goto continu;
          }

          w = char_kern(hf, q);
          goto done;
        }

    if (skip_byte(q) >= stop_flag)
      if (cur_rh == non_char)
        goto done;
      else
      {
        cur_rh = non_char;
        goto continu;
      }
      
    k = k + skip_byte(q) + 1;
    q = font_info[k].qqqq;
  }

done:
  wrap_lig(rt_hit);

  if (w != 0)
  {
    link(t) = new_kern(w);
    t = link(t);
    w = 0;
    sync_tag(t + medium_node_size) = 0;
  }

  if (lig_stack != null)
  {
    cur_q = t;
    cur_l = character(lig_stack);
    ligature_present = true;
    pop_lig_stack();
    goto continu;
  }

  return j;
}

void hyphenate (void)
{
  uint32_t i, j, l;
  pointer q, r, s;
  halfword bchar;
  pointer major_tail, minor_tail;
  /* ASCII_code c; */
  int c;
  uint32_t c_loc;
  /* integer r_count; */
  int r_count;
  pointer hyf_node;
  trie_pointer z;
  integer v;
  hyph_pointer h;
  str_number k;
  pool_pointer u;

  for (j = 0; j <= hn; j++)
    hyf[j] = 0;

  h = hc[1];
  incr(hn);
  hc[hn] = cur_lang;

  for (j = 2; j <= hn; j++)
    h = (h + h + hc[j]) % hyphen_prime;

  while (true)
  {
    k = hyph_word[h];

    if (k == 0)
      goto not_found;

    if (length(k) < hn)
      goto not_found;

    if (length(k) == hn)
    {
      j = 1;
      u = str_start[k];

      do {
        if (str_pool[u] < hc[j])
          goto not_found;

        if (str_pool[u] > hc[j])
          goto done;

        incr(j);
        incr(u);
      } while (!(j > hn));

      s = hyph_list[h];

      while (s != null)
      {
        hyf[info(s)] = 1;
        s = link(s);
      }

      decr(hn);
      goto found;
    }

done:
    if (h > 0)
      decr(h);
    else
      h = hyphen_prime;
  }

not_found:
  decr(hn);

  if (trie_char(cur_lang + 1) != cur_lang)
    return;

  hc[0] = 0;
  hc[hn + 1] = 0;
  hc[hn + 2] = 256;

  for (j = 0; j <= hn - r_hyf + 1; j++)
  {
    z = trie_link(cur_lang + 1) + hc[j];
    l = j;

    while (hc[l] == trie_char(z))
    {
      if (trie_op(z) != min_trie_op)
      {
        v = trie_op(z);

        do {
          v = v + op_start[cur_lang];
          i = l - hyf_distance[v];

          if (hyf_num[v] > hyf[i])
            hyf[i]= hyf_num[v];

          v = hyf_next[v];
        } while (!(v == min_trie_op));
      }

      incr(l);
      z = trie_link(z) + hc[l];
    }
  }

found:
  for (j = 0; j <= l_hyf - 1; j++)
    hyf[j] = 0;

  for (j = 0; j <= r_hyf - 1; j++)
    hyf[hn - j]= 0;

  for (j = l_hyf; j <= hn - r_hyf; j++)
    if (odd(hyf[j]))
      goto found1;

  return;

found1:
  q = link(hb);
  link(hb) = null;
  r = link(ha);
  link(ha) = null;
  bchar = hyf_bchar;

  if (is_char_node(ha))
    if (font(ha) != hf)
      goto found2;
    else
    {
      init_list = ha;
      init_lig = false;
      hu[0] = character(ha);
    }
  else if (type(ha) == ligature_node)
    if (font(lig_char(ha)) != hf)
      goto found2;
    else
    {
      init_list = lig_ptr(ha);
      init_lig = true;
      init_lft = (subtype(ha) > 1);
      hu[0] = character(lig_char(ha));

      if (init_list == 0)
        if (init_lft)
        {
          hu[0] = 256;
          init_lig = false;
        }

      free_node(ha, small_node_size);
    }
  else
  {
    if (!is_char_node(r))
      if (type(r) == ligature_node)
        if (subtype(r) > 1)
          goto found2;

    j = 1;
    s = ha;
    init_list = null;
    goto common_ending;
  }

  s = cur_p;

  while (link(s) != ha)
    s = link(s);

  j = 0;
  goto common_ending;

found2:
  s = ha;
  j = 0;
  hu[0] = 256;
  init_lig = false;
  init_list = null;

common_ending:
  flush_node_list(r);

  do {
    l = j;
    j = reconstitute(j, hn, bchar, hyf_char) + 1;

    if (hyphen_passed == 0)
    {
      link(s) = link(hold_head);

      while (link(s) != null)
        s = link(s);

      if (odd(hyf[j - 1]))
      {
        l = j;
        hyphen_passed = j - 1;
        link(hold_head) = null;
      }
    }

    if (hyphen_passed > 0)
      do {
        r = get_node(small_node_size);
        link(r) = link(hold_head);
        type(r) = disc_node;
        major_tail = r;
        r_count = 0;

        while (link(major_tail) != null)
          advance_major_tail();

        i = hyphen_passed;
        hyf[i] = 0;
        minor_tail = null;
        pre_break(r) = null;
        hyf_node = new_character(hf, hyf_char);

        if (hyf_node != null)
        {
          incr(i);
          c = hu[i];
          hu[i] = hyf_char;
          free_avail(hyf_node);
        }

        while (l <= i)
        {
          l = reconstitute(l, i, font_bchar[hf], non_char) + 1;

          if (link(hold_head) != null)
          {
            if (minor_tail == null)
              pre_break(r) = link(hold_head);
            else
              link(minor_tail) = link(hold_head);

            minor_tail = link(hold_head);

            while (link(minor_tail) != null)
              minor_tail = link(minor_tail);
          }
        }

        if (hyf_node != null)
        {
          hu[i] = c;
          l = i;
          decr(i);
        }

        minor_tail = null;
        post_break(r) = null;
        c_loc = 0;

        if (bchar_label[hf] != non_address)
        {
          decr(l);
          c = hu[l];
          c_loc = l;
          hu[l] = 256;
        }

        while (l < j)
        {
          do {
            l = reconstitute(l, hn, bchar, non_char) + 1;

            if (c_loc > 0)
            {
              hu[c_loc] = c;
              c_loc = 0;
            }

            if (link(hold_head) != null)
            {
              if (minor_tail == null)
                post_break(r) = link(hold_head);
              else
                link(minor_tail) = link(hold_head);

              minor_tail = link(hold_head);

              while (link(minor_tail) != null)
                minor_tail = link(minor_tail);
            }
          } while (!(l >= j));

          while (l > j)
          {
            j = reconstitute(j, hn, bchar, non_char) + 1;
            link(major_tail) = link(hold_head);

            while (link(major_tail) != null)
              advance_major_tail();
          }
        }

        if (r_count > 127)
        {
          link(s) = link(r);
          link(r) = null;
          flush_node_list(r);
        }
        else
        {
          link(s) = r;
          replace_count(r) = r_count;
        }

        s = major_tail;
        hyphen_passed = j - 1;
        link(hold_head) = null;
      } while (!(!odd(hyf[j - 1])));
  } while (!(j > hn));

  link(s) = q;
  flush_list(init_list);
}

// enters new exceptions
void new_hyph_exceptions (void)
{
  uint32_t n;
  uint32_t j;
  hyph_pointer h;
  str_number k;
  pointer p;
  pointer q;
  str_number s, t;
  pool_pointer u, v;

  scan_left_brace();
  set_cur_lang();

  if (aptex_env.flag_initex)
  {
    hyph_index = 0;
    goto not_found1;
  }

  set_hyph_index();

not_found1:
  n = 0;
  p = null;

  while (true)
  {
    get_x_token();

reswitch:
    switch (cur_cmd)
    {
      case letter:
      case other_char:
      case char_given:
      case kchar_given:
        if (cur_chr == '-')
        {
          if (n < 63)
          {
            q = get_avail();
            link(q) = p;
            info(q) = n;
            p = q;
          }
        }
        else
        {
          set_lc_code(cur_chr);

          if (hc[0] == 0)
          {
            print_err("Not a letter");
            help2("Letters in \\hyphenation words must have \\lccode>0.",
                "Proceed; I'll ignore the character I just read.");
            error();
          }
          else if (n < 63)
          {
            incr(n);
            hc[n] = hc[0];
          }
        }
        break;

      case char_num:
      case kchar_num:
        {
          scan_char_num();
          cur_chr = cur_val;
          cur_cmd = char_given;
          goto reswitch;
        }
        break;

      case spacer:
      case right_brace:
        {
          if (n > 1)
          {
            incr(n);
            hc[n] = cur_lang;
            str_room(n);
            h = 0;

            for (j = 1; j <= n; j++)
            {
              h = (h + h + hc[j]) % hyphen_prime;
              append_char(hc[j]);
            }

            s = make_string();

            if (hyph_count == hyphen_prime)
              overflow("exception dictionary", hyphen_prime);

            incr(hyph_count);

            while (hyph_word[h] != 0)
            {
              k = hyph_word[h];

              if (length(k) < length(s))
                goto found;

              if (length(k) > length(s))
                goto not_found;

              u = str_start[k];
              v = str_start[s];

              do {
                if (str_pool[u] < str_pool[v])
                  goto found;

                if (str_pool[u] > str_pool[v])
                  goto not_found;

                incr(u);
                incr(v);
              } while (!(u == str_start[k + 1]));

found:
              q = hyph_list[h];
              hyph_list[h] = p;
              p = q;
              t = hyph_word[h];
              hyph_word[h] = s;
              s = t;

not_found:
              if (h > 0)
                decr(h);
              else
                h = hyphen_prime;
            }

            hyph_word[h] = s;
            hyph_list[h] = p;
          }

          if (cur_cmd == right_brace)
            return;

          n = 0;
          p = null;
        }
        break;

      default:
        {
          print_err("Improper ");
          print_esc("hyphenation");
          prints(" will be flushed");
          help2("Hyphenation exceptions must contain only letters",
              "and hyphens. But continue; I'll forgive and forget.");
          error();
        }
        break;
    }
  }
}

static pointer prune_page_top (pointer p, boolean s)
{
  pointer prev_p;
  pointer q, r;

  prev_p = temp_head;
  link(temp_head) = p;

  while (p != null)
    switch (type(p))
    {
      case dir_node:
      case hlist_node:
      case vlist_node:
      case rule_node:
        {
          q = new_skip_param(split_top_skip_code);
          link(prev_p) = q;
          link(q) = p;

          if (width(temp_ptr) > height(p))
            width(temp_ptr) = width(temp_ptr) - height(p);
          else
            width(temp_ptr) = 0;

          p = null;
        }
        break;

      case whatsit_node:
      case mark_node:
      case ins_node:
        {
          prev_p = p;
          p = link(prev_p);
        }
        break;

      case glue_node:
      case kern_node:
      case penalty_node:
        {
          q = p;
          p = link(q);
          link(q) = null;
          link(prev_p) = p;

          if (s)
          {
            if (split_disc == null)
              split_disc = q;
            else
              link(r) = q;

            r = q;
          }
          else
            flush_node_list(q);
        }
        break;

      default:
        confusion("pruning");
        break;
    }

  return link(temp_head);
}

// finds optimum page break
static pointer vert_break (pointer p, scaled h, scaled d)
{
  pointer prev_p; // {if |p| is a glue node, |type(prev_p)| determines
                  // whether |p| is a legal breakpoint}
  pointer q, r;   // {glue specifications}
  integer pi;     // {penalty value}
  integer b;      // {badness at a trial breakpoint}
  integer least_cost; // {the smallest badness plus penalties found so far}
  pointer best_place; // {the most recent break that leads to |least_cost|}
  scaled prev_dp;   // {depth of previous box in the list}
  /* small_number t; */
  int t;  // {|type| of the node following a kern}

  prev_p = p;
  least_cost = awful_bad;
  do_all_six(set_height_zero);
  prev_dp = 0;

  while (true)
  {
    if (p == null)
      pi = eject_penalty;
    else switch (type(p))
    {
      case dir_node:
      case hlist_node:
      case vlist_node:
      case rule_node:
        {
          cur_height = cur_height + prev_dp + height(p);
          prev_dp = depth(p);
          goto not_found;
        }
        break;

      case whatsit_node:
        goto not_found;
        break;

      case glue_node:
        if (precedes_break(prev_p))
          pi = 0;
        else
          goto update_heights;
        break;

      case kern_node:
        {
          if (link(p) == null)
            t = penalty_node;
          else
            t = type(link(p));

          if (t == glue_node)
            pi = 0;
          else
            goto update_heights;
        }
        break;

      case penalty_node:
        pi = penalty(p);
        break;

      case mark_node:
      case ins_node:
        goto not_found;
        break;

      default:
        confusion("vertbreak");
        break;
    }

    if (pi < inf_penalty)
    {
      if (cur_height < h)
        if ((active_width[3] != 0) || (active_width[4] != 0) ||
          (active_width[5] != 0))
          b = 0;
        else
          b = badness(h - cur_height, active_width[2]);
      else if (act_width - h > active_width[6])
        b = awful_bad;
      else
        b = badness(cur_height - h, active_width[6]);

      if (b < awful_bad)
        if (pi <= eject_penalty)
          b = pi;
        else if (b < inf_bad)
          b = b + pi;
        else
          b = deplorable;

      if (b <= least_cost)
      {
        best_place = p;
        least_cost = b;
        best_height_plus_depth = cur_height + prev_dp;
      }

      if ((b == awful_bad) || (pi <= eject_penalty))
        goto done;
    }

    if ((type(p) < glue_node) || (type(p) > kern_node))
      goto not_found;

update_heights:
    if (type(p) == kern_node)
      q = p;
    else
    {
      q = glue_ptr(p);
      active_width[2 + stretch_order(q)] = active_width[2 + stretch_order(q)] + stretch(q);
      active_width[6] = active_width[6] + shrink(q);

      if ((shrink_order(q) != normal) && (shrink(q) != 0))
      {
        print_err("Infinite glue shrinkage found in box being split");
        help4("The box you are \\vsplitting contains some infinitely",
            "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
            "Such glue doesn't belong there; but you can safely proceed,",
            "since the offensive shrinkability has been made finite.");
        error();
        r = new_spec(q);
        shrink_order(r) = normal;
        delete_glue_ref(q);
        glue_ptr(p) = r;
        q = r;
      }
    }

    cur_height = cur_height + prev_dp + width(q);
    prev_dp = 0;

not_found:
    if (prev_dp > d)
    {
      cur_height = cur_height + prev_dp - d;
      prev_dp = d;
    }

    prev_p = p;
    p = link(prev_p);
  }

done:
  return best_place;
}

// extracts a page of height |h| from box |n|
static pointer vsplit (halfword n, scaled h)
{
  pointer v;  // {the box to be split}
  pointer w;  // {|dir_node|}
  pointer p;  // {runs through the vlist}
  pointer q;  // {points to where the break occurs}

  cur_val = n;
  fetch_box(v);
  flush_node_list(split_disc);
  split_disc = null;

  if (sa_mark != null)
    if (do_marks(vsplit_init, 0, sa_mark))
      sa_mark = null;

  if (split_first_mark != null)
  {
    delete_token_ref(split_first_mark);
    split_first_mark = null;
    delete_token_ref(split_bot_mark);
    split_bot_mark = null;
  }

  if (v == null)
  {
    return null;
  }

  if (type(v) == dir_node)
  {
    w = v;
    v =list_ptr(v);
    delete_glue_ref(space_ptr(w));
    delete_glue_ref(xspace_ptr(w));
    free_node(w, box_node_size);
  }

  if (type(v) != vlist_node)
  {
    print_err("");
    print_esc("vsplit");
    prints(" needs a ");
    print_esc("vbox");
    help2("The box you are trying to split is an \\hbox.",
        "I can't split such a box, so I'll leave it alone.");
    error();
    return null;
  }

  flush_node_list(link(v));
  link(v) = null;
  q = vert_break(list_ptr(v), h, split_max_depth);
  p = list_ptr(v);

  if (p == q)
    list_ptr(v) = null;
  else while (true)
  {
    if (type(p) == mark_node)
      if (mark_class(p) != 0)
      {
        find_sa_element(mark_val, mark_class(p), true);

        if (sa_split_first_mark(cur_ptr) == null)
        {
          sa_split_first_mark(cur_ptr) = mark_ptr(p);
          add_token_ref(mark_ptr(p));
        }
        else
          delete_token_ref(sa_split_bot_mark(cur_ptr));

        sa_split_bot_mark(cur_ptr) = mark_ptr(p);
        add_token_ref(mark_ptr(p));
      }
      else if (split_first_mark == null)
      {
        split_first_mark = mark_ptr(p);
        split_bot_mark = split_first_mark;
        token_ref_count(split_first_mark) = token_ref_count(split_first_mark) + 2;
      }
      else
      {
        delete_token_ref(split_bot_mark);
        split_bot_mark = mark_ptr(p);
        add_token_ref(split_bot_mark);
      }

    if (link(p) == q)
    {
      link(p) = null;
      goto done;
    }

    p = link(p);
  }

done:
  q = prune_page_top(q, saving_vdiscards > 0);
  p = list_ptr(v);
 
  if (q != null)
  {
    q = vpackage(q, 0, 1, max_dimen);
    set_box_dir(q, box_dir(v));
  }

  change_box(q);
  q = vpackage(p, h, exactly, split_max_depth);
  set_box_dir(q, box_dir(v));
  delete_glue_ref(space_ptr(v));
  delete_glue_ref(xspace_ptr(v));
  free_node(v, box_node_size);

  return q;
}

void print_totals (void)
{
  print_scaled(page_so_far[1]);
  print_plus(2, "");
  print_plus(3, "fil");
  print_plus(4, "fill");
  print_plus(5, "filll");

  if (page_shrink != 0)
  {
    prints(" minus ");
    print_scaled(page_shrink);
  }
}

static void freeze_page_specs (small_number s)
{
  page_contents = s;
  page_goal = vsize;
  page_max_depth = max_depth;
  page_depth = 0;
  do_all_six(set_page_so_far_zero);
  least_page_cost = awful_bad;

#ifdef STAT
  if (tracing_pages > 0)
  {
    begin_diagnostic();
    print_nl("%% goal height=");
    print_scaled(page_goal);
    prints(", max depth=");
    print_scaled(page_max_depth);
    end_diagnostic(false);
  }
#endif
}

static void box_error (eight_bits n)
{
  error();
  begin_diagnostic();
  print_nl("The following box has been deleted:");
  show_box(box(n));
  end_diagnostic(true);
  flush_node_list(box(n));
  box(n) = null;
}

static void ensure_vbox (eight_bits n)
{
  pointer p;  // {the box register contents}

  p = box(n);

  if (p != null)
    if (type(p) == dir_node)
    {
      p = list_ptr(p);
      delete_glue_ref(space_ptr(box(n)));
      delete_glue_ref(xspace_ptr(box(n)));
      free_node(box(n), box_node_size);
      box(n) = p;
    }

  if (p != null)
    if (type(p) != vlist_node)
    {
      print_err("Insertions can only be added to a vbox");
      help3("Tut tut: You're trying to \\insert into a",
          "\\box register that now contains an \\hbox.",
          "Proceed, and I'll discard its present contents.");
      box_error(n);
    }
}

static void fire_up (pointer c)
{
  pointer p, q, r, s;
  pointer prev_p;
  /* unsigned char n; */
  unsigned int n;
  boolean wait;
  integer save_vbadness;
  scaled save_vfuzz;
  pointer save_split_top_skip;

  if (type(best_page_break) == penalty_node)
  {
    geq_word_define(int_base + output_penalty_code, penalty(best_page_break));
    penalty(best_page_break) = inf_penalty;
  }
  else
    geq_word_define(int_base + output_penalty_code, inf_penalty);

  if (sa_mark != null)
    if (do_marks(fire_up_init, 0, sa_mark))
      sa_mark = null;

  if (bot_mark != null)
  {
    if (top_mark != null)
      delete_token_ref(top_mark);

    top_mark = bot_mark;
    add_token_ref(top_mark);
    delete_token_ref(first_mark);
    first_mark = null;
  }

  if (c == best_page_break)
    best_page_break = null;

  if (box(255) != null)
  {
    print_err("");
    print_esc("box");
    prints("255 is not void");
    help2("You shouldn't use \\box255 except in \\output routines.",
        "Proceed, and I'll discard its present contents.");
    box_error(255);
  }

  insert_penalties = 0;
  save_split_top_skip = split_top_skip;

  if (holding_inserts <= 0)
  {
    r = link(page_ins_head);

    while (r != page_ins_head)
    {
      if (best_ins_ptr(r) != null)
      {
        n = subtype(r);
        ensure_vbox(n);

        if (box(n) == null)
          box(n) = new_null_box();

        p = box(n) + list_offset;

        while (link(p) != null)
          p = link(p);

        last_ins_ptr(r) = p;
      }

      r = link(r);
    }
  }

  q = hold_head;
  link(q) = null;
  prev_p = page_head;
  p = link(prev_p);

  while (p != best_page_break)
  {
    if (type(p) == ins_node)
    {
      if (holding_inserts <= 0)
      {
        r = link(page_ins_head);

        while (subtype(r) != subtype(p))
          r = link(r);

        if (best_ins_ptr(r) == null)
          wait = true;
        else
        {
          wait = false;
          n = subtype(p);

          switch (abs(box_dir(box(n))))
          {
            case any_dir:
              if (abs(ins_dir(p)) != abs(box_dir(box(n))))
              {
                print_err("Insertions can only be added to a same direction vbox");
                help3("Tut tut: You're trying to \\insert into a",
                  "\\box register that now have a different direction.",
                  "Proceed, and I'll discard its present contents.");
                box_error(n);
                box(n) = new_null_box();
                last_ins_ptr(r) = box(n) + list_offset;
              }
              break;

            default:
              set_box_dir(box(n), abs(ins_dir(p)));
              break;
          }

          s = last_ins_ptr(r);
          link(s) = ins_ptr(p);

          if (best_ins_ptr(r) == p)
          {
            if (type(r) == split_up)
              if ((broken_ins(r) == p) && (broken_ins(r) != null))
              {
                while (link(s) != broken_ptr(r))
                  s = link(s);

                link(s) = null;
                split_top_skip = split_top_ptr(p);
                ins_ptr(p) = prune_page_top(broken_ptr(r), false);

                if (ins_ptr(p) != null)
                {
                  temp_ptr = vpackage(ins_ptr(p), 0, 1, max_dimen);
                  height(p) = height(temp_ptr) + depth(temp_ptr);
                  delete_glue_ref(space_ptr(temp_ptr));
                  delete_glue_ref(xspace_ptr(temp_ptr));
                  free_node(temp_ptr, box_node_size);
                  wait = true;
                }
              }

            best_ins_ptr(r) = null;
            n = subtype(r);
            temp_ptr = list_ptr(box(n));
            delete_glue_ref(space_ptr(box(n)));
            delete_glue_ref(xspace_ptr(box(n)));
            flush_node_list(link(box(n)));
            free_node(box(n), box_node_size);
            box(n) = vpackage(temp_ptr, 0, 1, max_dimen);
            set_box_dir(box(n), abs(ins_dir(p)));
          }
          else
          {
            while (link(s) != null)
              s = link(s);

            last_ins_ptr(r) = s;
          }
        }

        link(prev_p) = link(p);
        link(p) = null;

        if (wait)
        {
          link(q) = p;
          q = p;
          incr(insert_penalties);
        }
        else
        {
          delete_glue_ref(split_top_ptr(p));
          free_node(p, ins_node_size);
        }

        p = prev_p;
      }
    }
    else if (type(p) == mark_node)
      if (mark_class(p) != null)
      {
        find_sa_element(mark_val, mark_class(p), true);

        if (sa_first_mark(cur_ptr) == null)
        {
          sa_first_mark(cur_ptr) = mark_ptr(p);
          add_token_ref(mark_ptr(p));
        }

        if (sa_bot_mark(cur_ptr) != null)
          delete_token_ref(sa_bot_mark(cur_ptr));

        sa_bot_mark(cur_ptr) = mark_ptr(p);
        add_token_ref(mark_ptr(p));
      }
      else
      {
        if (first_mark == null)
        {
          first_mark = mark_ptr(p);
          add_token_ref(first_mark);
        }

        if (bot_mark != null)
          delete_token_ref(bot_mark);

        bot_mark = mark_ptr(p);
        add_token_ref(bot_mark);
      }

    prev_p = p;
    p = link(prev_p);
  }

  split_top_skip = save_split_top_skip;

  if (p != null)
  {
    if (link(contrib_head) == null)
      if (nest_ptr == 0)
        tail = page_tail;
      else
        contrib_tail = page_tail;

    link(page_tail) = link(contrib_head);
    link(contrib_head) = p;
    link(prev_p) = null;
  }

  save_vbadness = vbadness;
  vbadness = inf_bad;
  save_vfuzz = vfuzz;
  vfuzz = max_dimen;
  box(255) = vpackage(link(page_head), best_size, exactly, page_max_depth);
  set_box_dir(box(255), page_dir);
  vbadness = save_vbadness;
  vfuzz = save_vfuzz;

  if (last_glue != max_halfword)
    delete_glue_ref(last_glue);

  page_contents = empty;
  page_tail = page_head;
  link(page_head) = null;
  last_glue = max_halfword;
  last_penalty = 0;
  last_kern = 0;
  last_node_type = -1;
  last_node_subtype = -1;
  page_depth = 0;
  page_max_depth = 0;

  if (q != hold_head)
  {
    link(page_head) = link(hold_head);
    page_tail = q;
  }

  r = link(page_ins_head);

  while (r != page_ins_head)
  {
    q = link(r);
    free_node(r, page_ins_node_size);
    r = q;
  }
 
  link(page_ins_head) = page_ins_head;

  if (sa_mark != null)
    if (do_marks(fire_up_done, 0, sa_mark))
      sa_mark = null;

  if ((top_mark != null) && (first_mark == null))
  {
    first_mark = top_mark;
    add_token_ref(top_mark);
  }

  if (output_routine != null)
    if (dead_cycles >= max_dead_cycles)
    {
      print_err("Output loop---");
      print_int(dead_cycles);
      prints(" consecutive dead cycles");
      help3("I've concluded that your \\output is awry; it never does",
          "\\ship_out, so I'm shipping \box255 out myself. Next ",
          "increase \\maxdeadcycles if you want me to be more patient!");
      error();
    }
    else
    {
      output_active = true;
      incr(dead_cycles);
      push_nest();
      mode = -vmode;
      prev_depth = ignore_depth;
      mode_line = -line;
      begin_token_list(output_routine, output_text);
      new_save_level(output_group);
      normal_paragraph();
      scan_left_brace();
      return;
    }

  {
    if (link(page_head) != null)
    {
      if (link(contrib_head) == null)
        if (nest_ptr == 0)
          tail = page_tail;
        else
          contrib_tail = page_tail;
      else
        link(page_tail) = link(contrib_head);

      link(contrib_head) = link(page_head);
      link(page_head) = null;
      page_tail = page_head;
    }

    flush_node_list(page_disc);
    page_disc = null;
    ship_out(box(255));
    box(255) = null;
  }
}

// append contributions to the current page
void build_page (void)
{
  pointer p;  // {the node being appended}
  pointer q, r; // {nodes being examined}
  integer b, c; // {badness and cost of current page}
  integer pi; // {penalty to be added to the badness}
  /* unsigned char n; */
  unsigned int n; // {insertion box number}
  scaled delta, h, w; // {sizes used for insertion calculations}

  if ((link(contrib_head) == null) || output_active)
    return;

  do {
continu:
    p = link(contrib_head);

    // @<Update the values of |last_glue|, |last_penalty|, and |last_kern|@>
    if (last_glue != max_halfword)
      delete_glue_ref(last_glue);

    last_penalty = 0;
    last_kern = 0;

    if (type(p) < dir_node)
      last_node_type = type(p) + 1;
    else if (type(p) == dir_node)
      last_node_type = type(list_ptr(p)) + 1;
    else if (type(p) < disp_node)
      last_node_type = type(p);
    else
      last_node_type = type(p) - 1; // {no |disp_node| in a vertical list}
    last_node_subtype = subtype(p);

    if (type(p) == glue_node)
    {
      last_glue = glue_ptr(p);
      add_glue_ref(last_glue);
    }
    else
    {
      last_glue = max_halfword;

      if (type(p) == penalty_node)
        last_penalty = penalty(p);
      else if (type(p) == kern_node)
        last_kern = width(p);
    }

    /*
      @<Move node |p| to the current page; if it is time for a page break,
      put the nodes following the break back onto the contribution list,
      and |return| to the user's output routine if there is one@>
    */

    /*
      @<If the current page is empty and node |p| is to be deleted, |goto done1|;
      otherwise use node |p| to update the state of the current page;
      if this node is an insertion, |goto contribute|; otherwise if this node
      is not a legal breakpoint, |goto contribute| or |update_heights|;
      otherwise set |pi| to the penalty associated with this breakpoint@>
    */
    switch (type(p))
    {
      case hlist_node:
      case vlist_node:
      case dir_node:
      case rule_node:
        if (page_contents < box_there)
        {
          /*
            @<Initialize the current page, insert the \.{\\topskip} glue
            ahead of |p|, and |goto continue|@>
          */
          if (page_contents == empty)
            freeze_page_specs(box_there);
          else
            page_contents = box_there;

          q = new_skip_param(top_skip_code);  // {now |temp_ptr=glue_ptr(q)|}

          if (width(temp_ptr) > height(p))
            width(temp_ptr) = width(temp_ptr) - height(p);
          else
            width(temp_ptr) = 0;

          link(q) = p;
          link(contrib_head) = q;
          goto continu;
        }
        else
        {
          /*
            @<Prepare to move a box or rule node to the current page,
            then |goto contribute|@>
          */
          page_total = page_total + page_depth + height(p);
          page_depth = depth(p);
          goto contribute;
        }
        break;

      case whatsit_node:
        /*
          @<Prepare to move whatsit |p| to the current page,
          then |goto contribute|@>
        */
        goto contribute;
        break;

      case glue_node:
        if (page_contents < box_there)
          goto done1;
        else if (precedes_break(page_tail))
          pi = 0;
        else
          goto update_heights;
        break;

      case kern_node:
        if (page_contents < box_there)
          goto done1;
        else if (link(p) == null)
          return;
        else if (type(link(p)) == glue_node)
          pi = 0;
        else
          goto update_heights;
        break;

      case penalty_node:
        if (page_contents < box_there)
          goto done1;
        else
          pi = penalty(p);
        break;

      case mark_node:
        goto contribute;
        break;

      case ins_node:
        // @<Append an insertion to the current page and |goto contribute|@>
        {
          if (page_contents == empty)
            freeze_page_specs(inserts_only);

          n = subtype(p);
          r = page_ins_head;

          while (n >= subtype(link(r)))
            r = link(r);

          n = n;

          /*
            @<Create a page insertion node with |subtype(r)=qi(n)|, and
            include the glue correction for box |n| in the
            current page state@>
          */
          if (subtype(r) != n)
          {
            q = get_node(page_ins_node_size);
            link(q) = link(r);
            link(r) = q;
            r = q;
            subtype(r) = n;
            type(r) = inserting;
            ensure_vbox(n);

            if (box(n) == null)
              height(r) = 0;
            else
            {
              if (abs(ins_dir(p)) != abs(box_dir(box(n))))
              {
                print_err("Insertions can only be added to a same direction vbox");
                help3("Tut tut: You're trying to \\insert into a",
                  "\\box register that now have a different direction.",
                  "Proceed, and I'll discard its present contents.");
                box_error(n);
              }
              else
                height(r) = height(box(n)) + depth(box(n));
            }

            best_ins_ptr(r) = null;
            q = skip(n);

            if (count(n) == 1000)
              h = height(r);
            else
              h = x_over_n(height(r), 1000) * count(n);

            page_goal = page_goal - h - width(q);
            page_so_far[2 + stretch_order(q)] = page_so_far[2 + stretch_order(q)] + stretch(q);
            page_shrink = page_shrink + shrink(q);

            if ((shrink_order(q) != normal) && (shrink(q) != 0))
            {
              print_err("Infinite glue shrinkage inserted from ");
              print_esc("skip");
              print_int(n);
              help3("The correction glue for page breaking with insertions",
                  "must have finite shrinkability. But you may proceed,",
                  "since the offensive shrinkability has been made finite.");
              error();
            }
          }

          if (type(r) == split_up)
            insert_penalties = insert_penalties + float_cost(p);
          else
          {
            last_ins_ptr(r) = p;
            delta = page_goal - page_total - page_depth + page_shrink;
            // {this much room is left if we shrink the maximum}

            if (count(n) == 1000)
              h = height(p);
            else
              h = x_over_n(height(p), 1000) * count(n);

            if (((h <= 0) || (h <= delta)) && (height(p) + height(r) <= dimen(n)))
            {
              page_goal = page_goal - h;
              height(r) = height(r) + height(p);
            }
            else
            {
              /*
                @<Find the best way to split the insertion, and change
                |type(r)| to |split_up|@>
              */
              if (count(n) <= 0)
                w = max_dimen;
              else
              {
                w = page_goal - page_total - page_depth;

                if (count(n) != 1000)
                  w = x_over_n(w, count(n)) * 1000;
              }

              if (w > dimen(n) - height(r))
                w = dimen(n) - height(r);

              q = vert_break(ins_ptr(p), w, depth(p));
              height(r) = height(r) + best_height_plus_depth;
#ifdef STAT
              if (tracing_pages > 0)
              {
                begin_diagnostic();
                print_nl("% split");
                print_int(n);
                prints(" to ");
                print_scaled(w);
                print_char(',');
                print_scaled(best_height_plus_depth);
                prints(" p=");

                if (q == null)
                  print_int(eject_penalty);
                else if (type(q) == penalty_node)
                  print_int(penalty(q));
                else
                  print_char('0');

                end_diagnostic(false);
              }
#endif
              if (count(n) != 1000)
                best_height_plus_depth = x_over_n(best_height_plus_depth, 1000) * count(n);

              page_goal = page_goal - best_height_plus_depth;
              type(r) = split_up;
              broken_ptr(r) = q;
              broken_ins(r) = p;

              if (q == null)
                insert_penalties = insert_penalties + eject_penalty;
              else if (type(q) == penalty_node)
                insert_penalties = insert_penalties + penalty(q);
            }
          }

          goto contribute;
        }
        break;

      default:
        confusion("page");
        break;
    }

    /*
      @<Check if node |p| is a new champion breakpoint; then \(if)if it is time for
      a page break, prepare for output, and either fire up the user's
      output routine and |return| or ship out the page and |goto done|@>
    */

    if (pi < inf_penalty)
    {
      if (page_total < page_goal)
        if ((page_so_far[3] != 0) || (page_so_far[4] != 0) || (page_so_far[5] != 0))
          b = 0;
        else
          b = badness(page_goal - page_total, page_so_far[2]);
        else if (page_total - page_goal > page_shrink)
          b = awful_bad;
        else
          b = badness(page_total - page_goal, page_shrink);
  
      if (b < awful_bad)
        if (pi <= eject_penalty)
          c = pi; 
        else if (b < inf_bad)
          c = b + pi + insert_penalties;
        else
          c = deplorable;
      else
        c = b;

      if (insert_penalties >= 10000)
        c = awful_bad;

#ifdef STAT
      if (tracing_pages > 0)
      {
        begin_diagnostic();
        print_nl("%");
        prints(" t=");
        print_totals();
        prints(" g=");
        print_scaled(page_goal);
        prints(" b=");

        if (b == awful_bad)
          print_char('*');
        else
          print_int(b);

        prints(" p=");
        print_int(pi);
        prints(" c=");

        if (c == awful_bad)
          print_char('*');
        else
          print_int(c);

        if (c <= least_page_cost)
          print_char('#');

        end_diagnostic(false);
      }
#endif

      if (c <= least_page_cost)
      {
        best_page_break = p;
        best_size = page_goal;
        least_page_cost = c;
        r = link(page_ins_head);

        while (r != page_ins_head)
        {
          best_ins_ptr(r) = last_ins_ptr(r);
          r = link(r);
        }
      }

      if ((c == awful_bad) || (pi <= eject_penalty))
      {
        fire_up(p); // {output the current page at the best place}

        if (output_active)
          return; // {user's output routine will act}

        goto done;  // {the page has been shipped out by default output routine}
      }
    }

    if ((type(p) < glue_node) || (type(p) > kern_node))
      goto contribute;

  update_heights:
    /*
      @<Update the current page measurements with respect to the
      glue or kern specified by node~|p|@>
    */
    if (type(p) == kern_node)
      q = p;
    else
    {
      q = glue_ptr(p);
      page_so_far[2 + stretch_order(q)] = page_so_far[2 + stretch_order(q)] + stretch(q);
      page_shrink = page_shrink + shrink(q);

      if ((shrink_order(q) != normal) && (shrink(q) != 0))
      {
        print_err("Infinite glue shrinkage found on current page");
        help4("The page about to be output contains some infinitely",
          "shrinkable glue, e.g., `\\vss' or `\\vskip 0pt minus 1fil'.",
          "Such glue doesn't belong there; but you can safely proceed,",
          "since the offensive shrinkability has been made finite.");
        error();
        r = new_spec(q);
        shrink_order(r) = normal;
        delete_glue_ref(q);
        glue_ptr(p) = r;
        q = r;
      }
    }

    page_total = page_total + page_depth + width(q);
    page_depth = 0;

  contribute:
    // @<Make sure that |page_max_depth| is not exceeded@>;
    if (page_depth > page_max_depth)
    {
      page_total = page_total + page_depth - page_max_depth;
      page_depth = page_max_depth;
    }
    // @<Link node |p| into the current page and |goto done|@>
    link(page_tail) = p;
    page_tail = p;
    link(contrib_head) = link(p);
    link(p) = null;
    goto done;

  done1:
    // @<Recycle node |p|@>
    link(contrib_head) = link(p);
    link(p) = null;

    if (saving_vdiscards > 0)
    {
      if (page_disc == null)
        page_disc = p;
      else
        link(tail_page_disc) = p;

      tail_page_disc = p;
    }
    else
      flush_node_list(p);
done:;
  } while (!(link(contrib_head) == null));

  // @<Make the contribution list empty by setting its tail to |contrib_head|@>
  if (nest_ptr == 0)
    tail = contrib_head;
  else
    contrib_tail = contrib_head;
} 

// handle spaces when |space_factor<>1000
static void app_space (void)
{
  pointer q;

  if ((space_factor >= 2000) && (xspace_skip != zero_glue))
    q = new_param_glue(xspace_skip_code);
  else
  {
    if (space_skip != zero_glue)
      main_p = space_skip;
    else
    {
      main_p = font_glue[cur_font];

      if (main_p == 0)
      {
        main_p = new_spec(zero_glue);
        main_k = param_base[cur_font] + space_code;
        width(main_p) = font_info[main_k].cint;
        stretch(main_p) = font_info[main_k + 1].cint;
        shrink(main_p) = font_info[main_k + 2].cint;
        font_glue[cur_font] = main_p;
      }
    }

    main_p = new_spec(main_p);

    if (space_factor >= 2000)
      width(main_p) = width(main_p) + extra_space(cur_font);

    stretch(main_p) = xn_over_d(stretch(main_p), space_factor, 1000);
    shrink(main_p) = xn_over_d(shrink(main_p), 1000, space_factor);
    q = new_glue(main_p);
    glue_ref_count(main_p) = 0;
  }

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    link(prev_node) = q;
    link(q) = tail;
    prev_node = q;
  }
  else
  {
    link(tail) = q;
    tail = q;
  }
}

static void insert_dollar_sign (void)
{
  back_input();
  cur_tok = math_shift_token + '$';
  print_err("Missing $ inserted");
  help2("I've inserted a begin-math/end-math symbol since I think",
      "you left one out. Proceed, with fingers crossed.");
  ins_error();
}

static void you_cant (void)
{
  print_err("You can't use `");
  print_cmd_chr(cur_cmd, cur_chr);
  prints("' in ");
  print_mode(mode);
}

static void report_illegal_case (void)
{
  you_cant();
  help4("Sorry, but I'm not programmed to handle this case;",
      "I'll just pretend that you didn't ask for it.",
      "If you're in the wrong mode, you might be able to",
      "return to the right one by typing `I}' or `I$' or `I\\par'.");
  error();
}

static boolean privileged (void)
{
  if (mode > 0)
    return true;
  else
  {
    report_illegal_case();
    return false;
  }
}

// do this when \.{\\end} or \.{\\dump} occurs
static boolean its_all_over (void)
{
  if (privileged())
  {
    if ((page_head == page_tail) && (head == tail) && (dead_cycles == 0))
    {
      return true;
    }

    back_input(); // {we will try to end again after ejecting residual material}
    tail_append(new_null_box());
    width(tail) = hsize;
    tail_append(new_glue(fill_glue));
    tail_append(new_penalty(-010000000000));
    build_page(); // {append \.{\\hbox to \\hsize\{\}\\vfill\\penalty-'10000000000}}
  }

  return false;
}

static void append_glue (void)
{
  small_number s; // {modifier of skip command}

  s = cur_chr;

  switch (s)
  {
    case fil_code:
      cur_val = fil_glue;
      break;

    case fill_code:
      cur_val = fill_glue;
      break;

    case ss_code:
      cur_val = ss_glue;
      break;

    case fil_neg_code:
      cur_val = fil_neg_glue;
      break;

    case skip_code:
      scan_glue(glue_val);
      break;

    case mskip_code:
      scan_glue(mu_val);
      break;
  }
  // {now |cur_val| points to the glue specification}

  tail_append(new_glue(cur_val));
  inhibit_glue_flag = false;

  if (s >= skip_code)
  {
    decr(glue_ref_count(cur_val));

    if (s > skip_code)
      subtype(tail) = mu_glue;
  }
}

static void append_kern (void)
{ 
  quarterword s;  // {|subtype| of the kern node}

  s = cur_chr;
  scan_dimen((s == mu_glue), false, false);
  inhibit_glue_flag = false;

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    prev_append(new_kern(cur_val));
    subtype(prev_node) = s;
  }
  else
  {
    tail_append(new_kern(cur_val));
    subtype(tail) = s;
  }
}

static void off_save (void)
{
  pointer p;  // {inserted token}

  if (cur_group == bottom_level)
  {
    // @<Drop current token and complain that it was unmatched@>
    print_err("Extra ");
    print_cmd_chr(cur_cmd, cur_chr);
    help1("Things are pretty mixed up, but I think the worst is over.");
    error();
  }
  else
  {
    back_input();
    p = get_avail();
    link(temp_head) = p;
    print_err("Missing ");

    /*
      @<Prepare to insert a token that matches |cur_group|,
      and print what it is@>
    */
    switch (cur_group)
    {
      case semi_simple_group:
        {
          info(p) = cs_token_flag + frozen_end_group;
          print_esc("endgroup");
        }
        break;

      case math_shift_group:
        {
          info(p) = math_shift_token + '$';
          print_char('$');
        }
        break;

      case math_left_group:
        {
          info(p) = cs_token_flag + frozen_right;
          link(p) = get_avail();
          p = link(p);
          info(p) = other_token + '.';
          print_esc("right.");
        }
        break;

      default:
        {
          info(p) = right_brace_token + '}';
          print_char('}');
        }
        break;
    }

    prints(" inserted");
    ins_list(link(temp_head));
    help5("I've inserted something that you may have forgotten.",
        "(See the <inserted text> above.)",
        "With luck, this will get me unwedged. But if you",
        "really didn't forget anything, try typing `2' now; then",
        "my insertion and my current dilemma will both disappear.");
    error();
  }
}

static void extra_right_brace (void)
{
  print_err("Extra }, or forgotten ");

  switch (cur_group)
  {
    case semi_simple_group:
      print_esc("endgroup");
      break;

    case math_shift_group:
      print_char('$');
      break;

    case math_left_group:
      print_esc("right");
      break;
  }

  help5("I've deleted a group-closing symbol because it seems to be",
      "spurious, as in `$x}$'. But perhaps the } is legitimate and",
      "you forgot something else, as in `\\hbox{$x}'. In such cases",
      "the way to recover is to insert both the forgotten and the",
      "deleted material, e.g., by typing `I$}'.");
  error();
  incr(align_state);
}

void normal_paragraph (void)
{
  if (looseness != 0)
    eq_word_define(int_base + looseness_code, 0);

  if (hang_indent != 0)
    eq_word_define(dimen_base + hang_indent_code, 0);

  if (hang_after != 1)
    eq_word_define(int_base + hang_after_code, 1);

  if (par_shape_ptr != 0)
    eq_define(par_shape_loc, shape_ref, 0);

  if (inter_line_penalties_ptr != 0)
    eq_define(inter_line_penalties_loc, shape_ref, null);
}

static void box_end (integer box_context)
{
  pointer p;
  small_number a;
  pointer q;

  if (box_context < box_flag)
  {
    if (cur_box != 0)
    {
      p = link(cur_box);
      link(cur_box) = null;

      while (p != null)
      {
        q = p;
        p = link(p);

        if (abs(box_dir(q)) == abs(direction))
        {
          list_ptr(q) = cur_box;
          cur_box = q;
          link(cur_box) = null;
        }
        else
        {
          delete_glue_ref(space_ptr(q));
          delete_glue_ref(xspace_ptr(q));
          free_node(q, box_node_size);
        }
      }

      if (abs(box_dir(cur_box)) != abs(direction))
        cur_box = new_dir_node(cur_box, abs(direction));

      shift_amount(cur_box) = box_context;

      if (abs(mode) == vmode)
      {
        append_to_vlist(cur_box);

        if (adjust_tail != 0)
        {
          if (adjust_head != adjust_tail)
          {
            link(tail) = link(adjust_head);
            tail = adjust_tail;
          }

          adjust_tail = 0;
        }

        if (mode > 0)
          build_page();
      }
      else
      {
        if (abs(mode) == hmode)
        {
          space_factor = 1000;
          inhibit_glue_flag = false;
        }
        else
        {
          p = new_noad();
          math_type(nucleus(p)) = sub_exp_box;
          info(nucleus(p)) = cur_box;
          cur_box = p;
        }

        link(tail) = cur_box;
        tail = cur_box;
      }
    }
  }
  else if (box_context < ship_out_flag)
  {
    if (box_context < global_box_flag)
    {
      cur_val = box_context - box_flag;
      a = 0;
    }
    else
    {
      cur_val = box_context - global_box_flag;
      a = 4;
    }

    if (cur_val < 256)
      define(box_base + cur_val, box_ref, cur_box);
    else
      sa_def_box();
  }
  else if (cur_box != 0)
    if (box_context > ship_out_flag)
    {
      do {
        get_x_token();
      } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

      if (((cur_cmd == hskip) && (abs(mode) != vmode)) ||
        ((cur_cmd == vskip) && (abs(mode) == vmode)))
      {
        append_glue();
        subtype(tail) = box_context - (leader_flag - a_leaders);

        if (type(cur_box) <= dir_node)
        {
          p = link(cur_box);
          link(cur_box) = null;

          while (p != null)
          {
            q = p;
            p = link(p);

            if (abs(box_dir(q)) == abs(direction))
            {
              list_ptr(q) = cur_box;
              cur_box = q;
              link(cur_box) = null;
            }
            else
            {
              delete_glue_ref(space_ptr(q));
              delete_glue_ref(xspace_ptr(q));
              free_node(q, box_node_size);
            }
          }

          if (abs(box_dir(cur_box)) != abs(direction))
            cur_box = new_dir_node(cur_box, abs(direction));
        }

        leader_ptr(tail) = cur_box;
      }
      else
      {
        print_err("Leaders not followed by proper glue");
        help3("You should say `\\leaders <box or rule><hskip or vskip>'.",
            "I found the <box or rule>, but there's no suitable",
            "<hskip or vskip>, so I'm ignoring these leaders.");
        back_error();
        flush_node_list(cur_box);
      }
    }
    else
      ship_out(cur_box);
}

static void begin_box (integer box_context)
{
  pointer p, q;
  pointer r;
  pointer s;
  pointer t;
  integer fm;
  integer gm;
  boolean fd, gd;
  scaled disp, pdisp;
  eight_bits a_dir;
  pointer tx;
  quarterword m;
  halfword k;
  halfword n;

  switch (cur_chr)
  {
    case box_code:
      {
        scan_register_num();
        fetch_box(cur_box);
        change_box(null);
      }
      break;

    case copy_code:
      {
        scan_register_num();
        fetch_box(q);
        cur_box = copy_node_list(q);
      }
      break;

    case last_box_code:
      {
        cur_box = 0;

        if (abs(mode) == mmode)
        {
          you_cant();
          help1("Sorry; this \\lastbox will be void.");
          error();
        }
        else if ((mode == vmode) && (head == tail))
        {
          you_cant();
          help2("Sorry...I usually can't take things from the current page.",
                "This \\lastbox will therefore be void.");
          error();
        }
        else
        {
          check_effective_tail(goto done);

          if (!is_char_node(tx) && (head != tx))
            if ((type(tx) == hlist_node) || (type(tx) == vlist_node) ||
              (type(tx) == dir_node))
            {
              fetch_effective_tail(goto done);
              cur_box = tx;
              shift_amount(cur_box) = 0;

              if (type(cur_box) == dir_node)
              {
                link(list_ptr(cur_box)) = cur_box;
                cur_box = list_ptr(cur_box);
                list_ptr(link(cur_box)) = null;
              }
              else if (box_dir(cur_box) == dir_default)
                set_box_dir(cur_box, direction);
done:;
            }
        }
      }
      break;

    case vsplit_code:
      {
        scan_register_num();
        n = cur_val;

        if (!scan_keyword("to"))
        {
          print_err("Missing `to' inserted");
          help2("I'm working on `\\vsplit<box number> to <dimen>';",
              "will look for the <dimen> next.");
          error();
        }

        scan_normal_dimen();
        cur_box = vsplit(n, cur_val);
      }
      break;

    default:
      {
        k = cur_chr - vtop_code;
        saved(0) = box_context;
        a_dir = adjust_dir;

        if (k == hmode)
          if ((box_context < box_flag) && (abs(mode) == vmode))
          {
            a_dir = abs(direction);
            scan_spec(adjusted_hbox_group, true);
          }
          else
            scan_spec(hbox_group, true);
        else
        {
          if (k == vmode)
            scan_spec(vbox_group, true);
          else
          {
            scan_spec(vtop_group, true);
            k = vmode;
          }

          normal_paragraph();
        }

        push_nest();
        mode = -k;
        adjust_dir = a_dir;

        if (k == vmode)
        {
          prev_depth = ignore_depth;

          if (every_vbox != null)
            begin_token_list(every_vbox, every_vbox_text);
        }
        else
        {
          space_factor = 1000;
          inhibit_glue_flag = false;

          if (every_hbox != null)
            begin_token_list(every_hbox, every_vbox_text);
        }

        return;
      }
      break;
  }

  box_end(box_context);
}

// the next input should specify a box or perhaps a rule
void scan_box (integer box_context)
{
  do {
    get_x_token(); 
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

  if (cur_cmd == make_box)
    begin_box(box_context);
  else if ((box_context >= leader_flag) && ((cur_cmd == hrule) || (cur_cmd == vrule)))
  {
    cur_box = scan_rule_spec();
    box_end(box_context);
  }
  else
  {
    print_err("A <box> was supposed to be here");
    help3("I was expecting to see \\hbox or \\vbox or \\copy or \\box or",
          "something like that. So you might find something missing in",
          "your output. But keep trying; you can fix this later.");
    back_error();
  }
}

static small_number norm_min (integer h)
{
  if (h <= 0)
    return 1;
  else if (h >= 63)
    return 63;
  else
    return h;
}

static void new_graf (boolean indented)
{
  prev_graf = 0;

  if ((mode == vmode) || (head != tail))
    tail_append(new_param_glue(par_skip_code));

  inhibit_glue_flag = false;
  push_nest();
  adjust_dir = direction;
  mode = hmode;
  space_factor = 1000;
  set_cur_lang();
  clang = cur_lang;
  prev_graf = (norm_min(left_hyphen_min) * 0100 + norm_min(right_hyphen_min)) * 0200000 + cur_lang;

  if (indented)
  {
    tail = new_null_box();
    link(head) = tail;
    width(tail) = par_indent;
  }

  if (every_par != null)
    begin_token_list(every_par, every_par_text);

  if (nest_ptr == 1)
    build_page();
}

static void indent_in_hmode (void)
{
  pointer p, q;

  if (cur_chr > 0)
  {
    p = new_null_box();
    width(p) = par_indent;

    if (abs(mode) == hmode)
    {
      space_factor = 1000;
      inhibit_glue_flag = false;
    }
    else
    {
      q = new_noad();
      math_type(nucleus(q)) = sub_box;
      info(nucleus(q)) = p;
      p = q;
    }

    tail_append(p);
  }
}

static void head_for_vmode (void)
{
  if (mode < 0)
  {
    if (cur_cmd != hrule)
      off_save();
    else
    {
      print_err("You can't use `");
      print_esc("hrule");
      prints("' here except with leaders");
      help2("To put a horizontal rule in an hbox or an alignment,",
            "you should use \\leaders or \\hrulefill (see The TeXbook).");
      error();
    }
  }
  else
  {
    back_input();
    cur_tok = par_token;
    back_input();
    token_type = inserted;
  }
}

static void end_graf (void)
{
  if (mode == hmode)
  {
    if ((link(head) == tail) && (!is_char_node(tail) && (type(tail) == disp_node)))
    {
      free_node(tail, small_node_size);
      tail = head;
      link(head) = null;
    }

    if (head == tail)
      pop_nest();
    else
    {
      adjust_hlist(head, true);
      line_break(false);
    }

    if (LR_save != null)
    {
      flush_list(LR_save);
      LR_save = null;
    }

    normal_paragraph();
    error_count = 0;
  }
}

static void begin_insert_or_adjust (void)
{
  if (cur_cmd == vadjust)
    cur_val = 255;
  else
  {
    scan_eight_bit_int();

    if (cur_val == 255)
    {
      print_err("You can't ");
      print_esc("insert");
      print_int(255);
      help1("I'm changing to \\insert0; box 255 is special.");
      error();
      cur_val = 0;
    }
  }

  saved(0) = cur_val;
  incr(save_ptr);
  inhibit_glue_flag = false;
  new_save_level(insert_group);
  scan_left_brace();
  normal_paragraph();
  push_nest();
  mode = -vmode;
  direction = adjust_dir;
  prev_depth = ignore_depth;
}

static void make_mark (void)
{
  pointer p;
  halfword c;

  if (cur_chr == 0)
    c = 0;
  else
  {
    scan_register_num();
    c = cur_val;
  }

  p = scan_toks(false, true);
  p = get_node(small_node_size);
  mark_class(p) = c;
  type(p) = mark_node;
  subtype(p) = 0;
  inhibit_glue_flag = false;
  mark_ptr(p) = def_ref;

  if (!is_char_node(tail) && (type(tail) == disp_node))
    prev_append(p);
  else
    tail_append(p);
}

static void append_penalty (void)
{
  scan_int();
  inhibit_glue_flag = false;

  if (!is_char_node(tail) && (type(tail) == disp_node))
    prev_append(new_penalty(cur_val));
  else
    tail_append(new_penalty(cur_val));

  if (mode == vmode)
    build_page();
}

static void delete_last (void)
{
  pointer p, q;
  pointer r;
  pointer s;
  pointer t;
  integer fm;
  integer gm;
  boolean fd, gd;
  scaled disp, pdisp;
  pointer tx;
  quarterword m;

  if ((mode == vmode) && (tail == head))
  {
    if ((cur_chr != glue_node) || (last_glue != max_halfword))
    {
      you_cant();
      help2("Sorry...I usually can't take things from the current page.",
          "Try `I\\vskip-\\lastskip' instead.");

      if (cur_chr == kern_node)
        help_line[0] = "Try `I\\kern-\\last_kern' instead.";
      else if (cur_chr != glue_node)
        help_line[0] = "Perhaps you can make the output routine do it.";

      error();
    }
  }
  else
  {
    check_effective_tail(return);

    if (!is_char_node(tx))
      if (type(tx) == cur_chr)
      {
        fetch_effective_tail(return);
        flush_node_list(tx);
      }
  }
}

static void unpackage (void)
{
  pointer p;  // {the box}
  char c;     // {should we copy?}
  scaled disp;

  if (cur_chr > copy_code)
  {
    link(tail) = disc_ptr[cur_chr];
    disc_ptr[cur_chr] = null;
    goto done;
  }

  c = cur_chr;
  scan_register_num();
  fetch_box(p);

  if (p == 0)
    return;

  if (type(p) == dir_node)
    p = list_ptr(p);

  if ((abs(mode) == mmode) || ((abs(mode) == vmode) && (type(p) != vlist_node)) ||
    ((abs(mode) == hmode) && (type(p) != hlist_node)))
  {
    print_err("Incompatible list can't be unboxed");
    help3("Sorry, Pandora. (You sneaky devil.)",
        "I refuse to unbox an \\hbox in vertical mode or vice versa.",
        "And I can't open any boxes in math mode.");
    error();
    return;
  }

  switch (abs(box_dir(p)))
  {
    case any_dir:
      if (abs(direction) != abs(box_dir(p)))
      {
        print_err("Incompatible direction list can't be unboxed");
        help2("Sorry, Pandora. (You sneaky devil.)",
          "I refuse to unbox a box in different direction.");
        error();
        return;
      }
      break;
  }

  disp = 0;

  if (c == copy_code)
    link(tail) = copy_node_list(list_ptr(p));
  else
  {
    if (type(p) == dir_node)
    {
      delete_glue_ref(space_ptr(p));
      delete_glue_ref(xspace_ptr(p));
      free_node(p, box_node_size);
    }

    flush_node_list(link(p));
    link(tail) = list_ptr(p);
    change_box(null);
    delete_glue_ref(space_ptr(p));
    delete_glue_ref(xspace_ptr(p));
    free_node(p, box_node_size);
  }

done:
  while (link(tail) != null)
  {
    p = tail;
    tail = link(tail);

    if (is_char_node(tail))
    {
      inhibit_glue_flag = false;
      if (font_dir[font(tail)] != dir_default)
        last_jchr = link(tail);
    }
    else
    {
      switch (type(tail))
      {
        case glue_node:
          {
            inhibit_glue_flag = false;
            if ((subtype(tail) == kanji_skip_code + 1) ||
              (subtype(tail) == xkanji_skip_code + 1))
            {
              link(p) = link(tail);
              delete_glue_ref(glue_ptr(tail));
              free_node(tail, small_node_size);
              tail = p;
            }
          }
          break;

        case penalty_node:
          {
            inhibit_glue_flag = false;
            if (subtype(tail) == widow_pena)
            {
              link(p) = link(tail);
              free_node(tail, small_node_size);
              tail = p;
            }
          }
          break;

        case disp_node:
          {
            prev_disp = disp;
            disp = disp_dimen(tail);
            prev_node = p;
          }
          break;

        default:
          inhibit_glue_flag = false;
          break;
      }
    }
  }
}

static void append_italic_correction (void)
{
  pointer p;
  internal_font_number f;
  pointer d;

  if (tail != head)
  {
    if (!is_char_node(tail) && (type(tail) == disp_node))
    {
      d = tail;
      tail = prev_node;
    }
    else
      d = null;

    if ((last_jchr != null) && (link(last_jchr) == tail) && is_char_node(tail))
      p = last_jchr;
    else if (is_char_node(tail))
      p = tail;
    else if (type(tail) == ligature_node)
      p = lig_char(tail);
    else
      return;

    f = font(p);
    tail_append(new_kern(char_italic(f, char_info(f, character(p)))));
    subtype(tail) = ita_kern;

    if (d != null)
    {
      prev_node = tail;
      tail_append(d);
    }
  }
}

static void append_discretionary (void)
{
  integer c;

  tail_append(new_disc());
  inhibit_glue_flag = false;

  if (cur_chr == 1)
  {
    c = hyphen_char[cur_font];

    if (c >= 0)
      if (c < 256)
        pre_break(tail) = new_character(cur_font, c);
  }
  else
  {
    incr(save_ptr);
    saved(-1) = 0;
    new_save_level(disc_group);
    scan_left_brace();
    push_nest();
    mode = -hmode;
    space_factor = 1000;
  }
}

static void build_discretionary (void)
{
  pointer p, q;
  integer n;
  integer d;

  unsave();
  q = head;
  p = link(q);
  n = 0;

  while (p != 0)
  {
    if (!is_char_node(p))
      if ((type(p) > rule_node) && (type(p) != kern_node) &&
        (type(p) != ligature_node) && (type(p) != disp_node))
        if ((type(p) == penalty_node) && (subtype(p) != normal))
        {
          link(q) = link(p);
          free_node(p, small_node_size);
          p = q;
        }
        else
        {
          print_err("Improper discretionary list");
          help1("Discretionary lists must contain only boxes and kerns.");
          error();
          begin_diagnostic();
          print_nl("The following discretionary sublist has been deleted:");
          show_box(p);
          end_diagnostic(true);
          flush_node_list(p);
          link(q) = 0;
          goto done;
        }

    q = p;
    p = link(q);
    incr(n);
  }

done:
  p = link(head);
  d = abs(direction);
  pop_nest();

  switch (saved(-1))
  {
    case 0:
      if (abs(direction) == d)
        pre_break(tail) = p;
      else
      {
        print_err("Direction Incompatible");
        help2("\\discretionary's argument and outer hlist must have same direction.",
              "I delete your first part.");
        error();
        pre_break(tail) = null;
        flush_node_list(p);
      }
      break;

    case 1:
      if (abs(direction) == d)
        post_break(tail) = p;
      else
      {
        print_err("Direction Incompatible");
        help2("\\discretionary's argument and outer hlist must have same direction.",
          "I delete your second part.");
        error();
        post_break(tail) = null;
        flush_node_list(p);
      }
      break;

    case 2:
      {
        if ((n > 0) && (abs(mode) == mmode))
        {
          print_err("Illegal math ");
          print_esc("discretionary");
          help2("Sorry: The third part of a discretionary break must be",
              "empty, in math formulas. I had to delete your third part.");
          flush_node_list(p);
          n = 0;
          error();
        }
        else if ((n > 0) && (abs(direction) != d))
        {
          print_err("Direction Incompatible");
          help2("\\discretionary's argument and outer hlist must have same direction.",
            "I delete your third part.");
          flush_node_list(p);
          n = 0;
          error();
        }
        else
          link(tail) = p;

        if (n <= max_quarterword)
          replace_count(tail) = n;
        else
        {
          print_err("Discretionary list is too long");
          help2("Wow---I never thought anybody would tweak me here.",
              "You can't seriously need such a huge discretionary list?");
          error();
        }

        if (n > 0)
          tail = q;

        decr(save_ptr);
        prev_node = tail;
        tail_append(get_node(small_node_size));
        type(tail) = disp_node;
        disp_dimen(tail) = 0;
        prev_disp = 0;
        return;
      }
      break;
  }

  incr(saved(-1));
  new_save_level(disc_group);
  scan_left_brace();
  push_nest();
  mode = -hmode;
  space_factor = 1000;
  inhibit_glue_flag = false;
}

/* sec 1123 */
static void make_accent (void)
{
  real s, t;
  scaled disp;
  KANJI_code cx;
  pointer p, q, r;
  internal_font_number f;
  scaled a, h, x, w, delta;
  four_quarters i;

  scan_char_num();

  if (check_echar_range(cur_val) == false)
  {
    KANJI(cx) = cur_val;

    if (direction == dir_tate)
      f = cur_tfont;
    else
      f = cur_jfont;

    p = new_character(f, get_jfm_pos(KANJI(cx), f));

    if (p != null)
    {
      link(p) = get_avail();
      info(link(p)) = KANJI(cx) + kcat_code(kcatcodekey(cx)) * max_cjk_val;
    }
  }
  else
  {
    f = cur_font;
    p = new_character(f, cur_val);
  }

  if (p != 0)
  {
    x = x_height(f);
    s = slant(f) / ((real) 65536.0);
    a = char_width(f, char_info(f, character(p)));
    do_assignments();
    q = 0;
    f = cur_font;
    KANJI(cx) = 0;

    if ((cur_cmd == letter) || (cur_cmd == other_char))
      q = new_character(f, cur_chr);
    else if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
    {
      if (direction == dir_tate)
        f = cur_tfont;
      else
        f = cur_jfont;

      cx = cur_chr;
    }
    else if (cur_cmd == char_given)
    {
      if (check_echar_range(cur_chr))
        q = new_character(f, cur_chr);
      else
      {
        if (direction == dir_tate)
          f = cur_tfont;
        else
          f = cur_jfont;

        KANJI(cx) = cur_chr;
        cur_cmd = kcat_code(kcatcodekey(cx));
      }
    }
    else if (cur_cmd == char_num)
    {
      scan_char_num();

      if (check_echar_range(cur_val))
        q = new_character(f, cur_val);
      else
      {
        if (direction == dir_tate)
          f = cur_tfont;
        else
          f = cur_jfont;

        KANJI(cx) = cur_chr;
        cur_cmd = kcat_code(kcatcodekey(cx));
      }
    }
    else if (cur_cmd == kchar_given)
    {
      if (direction == dir_tate)
        f = cur_tfont;
      else
        f = cur_jfont;

      KANJI(cx) = cur_chr;
      cur_cmd = kcat_code(kcatcodekey(cx));
    }
    else if (cur_cmd == kchar_num)
    {
      scan_char_num();

      if (direction == dir_tate)
        f = cur_tfont;
      else
        f = cur_jfont;

      KANJI(cx) = cur_val;
      cur_cmd = kcat_code(kcatcodekey(cx));
    }
    else
      back_input();

    if (direction == dir_tate)
    {
      if (font_dir[f] == dir_tate)
        disp = 0;
      else if (font_dir[f] == dir_yoko)
        disp = t_baseline_shift - y_baseline_shift;
      else
        disp = t_baseline_shift;
    }
    else
    {
      if (font_dir[f] == dir_yoko)
        disp = 0;
      else if (font_dir[f] == dir_tate)
        disp = y_baseline_shift - t_baseline_shift;
      else
        disp = y_baseline_shift;
    }

    append_disp_node_at_begin();

    if (KANJI(cx) != 0)
    {
      q = new_character(f, get_jfm_pos(KANJI(cx), f));
      link(q) = get_avail();
      info(link(q)) = KANJI(cx) + kcat_code(kcatcodekey(cx)) * max_cjk_val;
      last_jchr = q;
    }

    if (q != 0)
    {
      t = slant(f) / ((real) 65536.0);
      i = char_info(f, character(q));
      w = char_width(f, i);
      h = char_height(f, height_depth(i));

      if (h != x)
      {
        delete_glue_ref(cur_kanji_skip);
        delete_glue_ref(cur_xkanji_skip);
        cur_kanji_skip = zero_glue;
        cur_xkanji_skip = zero_glue;
        add_glue_ref(cur_kanji_skip);
        add_glue_ref(cur_xkanji_skip);
        p = hpack(p, 0, 1);
        shift_amount(p) = x - h;
      }

      delta = round((w - a) / ((real) 2.0) + h * t - x * s);
      r = new_kern(delta);
      subtype(r) = acc_kern;
      link(tail) = r;
      link(r) = p;
      tail = new_kern(-a - delta);
      subtype(tail) = acc_kern;

      if (h == x)
      {
        if (font_dir[font(p)] != dir_default)
          link(link(p)) = tail;
        else
          link(p) = tail;
      }
      else
        link(p) = tail;

      p = q;
    }

    link(tail) = p;

    if (link(p) != null)
      tail = link(p);
    else
      tail = p;

    append_disp_node_at_end();
    space_factor = 1000;
    inhibit_glue_flag = false;
  }
}

static void align_error (void)
{
  if (abs(align_state) > 2)
  {
    print_err("Misplaced ");
    print_cmd_chr(cur_cmd, cur_chr);

    if (cur_tok == tab_token + '&')
    {
      help6("I can't figure out why you would want to use a tab mark",
          "here. If you just want an ampersand, the remedy is",
          "simple: Just type `I\\&' now. But if some right brace",
          "up above has ended a previous alignment prematurely,",
          "you're probably due for more error messages, and you",
          "might try typing `S' now just to see what is salvageable.");
    }
    else
    {
      help5("I can't figure out why you would want to use a tab mark",
          "or \\cr or \\span just now. If something like a right brace",
          "up above has ended a previous alignment prematurely,",
          "you're probably due for more error messages, and you",
          "might try typing `S' now just to see what is salvageable.");
    }

    error();
  }
  else
  {
    back_input();

    if (align_state < 0)
    {
      print_err("Missing { inserted");
      incr(align_state);
      cur_tok = left_brace_token + '{';
    }
    else
    {
      print_err("Missing } inserted");
      decr(align_state);
      cur_tok = right_brace_token + '}';
    }

    help3("I've put in what seems to be necessary to fix",
        "the current column of the current alignment.",
        "Try to go on, since this might almost work.");
    ins_error();
  }
}

static void no_align_error (void)
{
  print_err("Misplaced ");
  print_esc("noalign");
  help2("I expect to see \\noalign only after the \\cr of",
      "an alignment. Proceed, and I'll ignore this case.");
  error();
}

static void omit_error (void)
{
  print_err("Misplaced ");
  print_esc("omit");
  help2("I expect to see \\omit only after tab marks or the \\cr of",
      "an alignment. Proceed, and I'll ignore this case.");
  error();
}

static void do_endv (void)
{
  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;

  while ((input_stack[base_ptr].index_field != v_template) &&
    (input_stack[base_ptr].loc_field == null) &&
    (input_stack[base_ptr].state_field == token_list))
    decr(base_ptr);

  if ((input_stack[base_ptr].index_field != v_template) ||
    (input_stack[base_ptr].loc_field != null) ||
    (input_stack[base_ptr].state_field != token_list))
    fatal_error("(interwoven alignment preambles are not allowed)");

  if (cur_group == align_group)
  {
    end_graf();

    if (fin_col())
      fin_row();
  }
  else
    off_save();
}

static void cs_error (void)
{
  print_err("Extra ");
  print_esc("endcsname");
  help1("I'm ignoring this, since I wasn't doing a \\csname."); 
  error();
}

static void push_math (group_code c)
{
  push_nest();
  mode = -mmode;
  incompleat_noad = 0;
  new_save_level(c);
}

static void init_math (void)
{
  scaled w; // {new or partial |pre_display_size|}
  pointer j;  // {prototype box for display}
  integer x;  // {new |pre_display_direction|}
  scaled l; // {new |display_width|}
  scaled s; // {new |display_indent|}
  pointer p;  // {current node when calculating |pre_display_size|}
  pointer q;  // {glue specification when calculating |pre_display_size|}
  internal_font_number f; // {font in current |char_node|}
  integer n;  // {scope of paragraph shape specification}
  scaled v; // {|w| plus possible glue amount}
  scaled d; // {increment to |v|}

  get_token(); // {|get_x_token| would fail on \.{\\ifmmode}\thinspace!}

  if ((cur_cmd == math_shift) && (mode > 0))
  {
    // @<Go into display math mode@>
    j = null;
    w = -max_dimen;
    // {`\.{\\noindent\$\$}' or `\.{\$\${ }\$\$}'}
    if (head == tail)
    {
      // @<Prepare for display after an empty paragraph@>
      pop_nest();
      set_value_of_x();
    }
    else if ((link(head) == tail) && (!is_char_node(tail) && (type(tail) == disp_node)))
    {
      free_node(tail, small_node_size);
      tail = head;
      link(head) = null;
      pop_nest();
      set_value_of_x();
    } // { |disp_node|-only paragraphs are ignored }
    else
    {
      adjust_hlist(head, true);
      line_break(true);
      /*
        @<Calculate the natural width, |w|, by which the characters of the
        final line extend to the right of the reference point,
        plus two ems; or set |w:=max_dimen| if the non-blank information
        on that line is affected by stretching or shrinking@>
      */
      // @<Prepare for display after a non-empty paragraph@>
      if (eTeX_ex)
      {
        if (right_skip == zero_glue)
          j = new_kern(0);
        else
          j = new_param_glue(right_skip_code);

        if (left_skip == zero_glue)
          p = new_kern(0);
        else
          p = new_param_glue(left_skip_code);

        link(p) = j;
        j = new_null_box();
        width(j) = width(just_box);
        shift_amount(j) = shift_amount(just_box);
        list_ptr(j) = p;
        glue_order(j) = glue_order(just_box);
        glue_sign(j) = glue_sign(just_box);
        glue_set(j) = glue_set(just_box);
      }

      v = shift_amount(just_box);
      set_value_of_x();

      if (x >= 0)
      {
        p = list_ptr(just_box);
        link(temp_head) = null;
      }
      else
      {
        v = -v - width(just_box);
        p = new_math(0, begin_L_code);
        link(temp_head) = p;
        just_copy(list_ptr(just_box), p, new_math(0, end_L_code));
        cur_dir = right_to_left;
      }

      v = v + 2 * quad(cur_font);

      if (TeXXeT_en)
        put_LR(before);

      while (p != null)
      {
        /*
          @<Let |d| be the natural width of node |p|;
          if the node is ``visible,'' |goto found|;
          if the node is glue that stretches or shrinks, set |v:=max_dimen|@>
        */
reswitch:
        if (is_char_node(p))
        {
          f = font(p);
          d = char_width(f, char_info(f, character(p)));

          if (font_dir[f] != dir_default)
            p = link(p);

          goto found;
        }

        switch (type(p))
        {
          case hlist_node:
          case vlist_node:
          case dir_node:
          case rule_node:
            {
              d = width(p);
              goto found;
            }
            break;

          case ligature_node:
            {
              mem[lig_trick] = mem[lig_char(p)];
              link(lig_trick) = link(p);
              p = lig_trick;
              goto reswitch;
            }
            break;

          case kern_node:
            d = width(p);
            break;

          case math_node:
            {
              d = width(p);

              if (TeXXeT_en)
                if (end_LR(p))
                {
                  if (info(LR_ptr) == end_LR_type(p))
                    pop_LR();
                  else if (subtype(p) > L_code)
                  {
                    w = max_dimen;
                    goto done;
                  }
                }
                else
                {
                  push_LR(p);

                  if (LR_dir(p) != cur_dir)
                  {
                    just_reverse(p);
                    p = temp_head;
                  }
                }
              else if (subtype(p) >= L_code)
              {
                w = max_dimen;
                goto done;
              }
            }
            break;

          case edge_node:
            {
              d = width(p);
              cur_dir = subtype(p);
            }
            break;

          case glue_node:
            {
              q = glue_ptr(p);
              d = width(q);

              if (glue_sign(just_box) == stretching)
              {
                if ((glue_order(just_box) == stretch_order(q)) &&
                  (stretch(q) != 0))
                  v = max_dimen;
              }
              else if (glue_sign(just_box) == shrinking)
              {
                if ((glue_order(just_box) == shrink_order(q)) &&
                  (shrink(q) != 0))
                  v = max_dimen;
              }

              if (subtype(p) >= a_leaders)
                goto found;
            }
            break;

          case whatsit_node:
            d = 0;
            break;

          default:
            d = 0;
            break;
        }

        if (v < max_dimen)
          v = v + d;

        goto not_found;

found:
        if (v < max_dimen)
        {
          v = v + d;
          w = v;
        }
        else
        {
          w = max_dimen;
          goto done;
        }

not_found:
        p = link(p);
      }
done:
      if (TeXXeT_en)
      {
        while (LR_ptr != null)
          pop_LR();

        if (LR_problems != 0)
        {
          w = max_dimen;
          LR_problems = 0;
        }
      }

      cur_dir = left_to_right;
      flush_node_list(link(temp_head));
    }

    if (par_shape_ptr == 0)
      if ((hang_indent != 0) && (((hang_after >= 0) &&
        (prev_graf + 2 > hang_after)) || (prev_graf + 1 < -hang_after)))
      {
        l = hsize - abs(hang_indent);

        if (hang_indent > 0)
          s = hang_indent;
        else
          s = 0;
      }
      else
      {
        l = hsize;
        s = 0;
      }
    else
    {
      n = info(par_shape_ptr);

      if (prev_graf + 2 >= n)
        p = par_shape_ptr + 2 * n;
      else
        p = par_shape_ptr + 2 * (prev_graf + 2);

      s = mem[p - 1].cint;
      l = mem[p].cint;
    }

    push_math(math_shift_group);
    mode = mmode;
    eq_word_define(int_base + cur_fam_code, -1);
    eq_word_define(dimen_base + pre_display_size_code, w);
    LR_box = j;

    if (eTeX_ex)
      eq_word_define(int_base + pre_display_direction_code, x);

    eq_word_define(dimen_base + display_width_code, l);
    eq_word_define(dimen_base + display_indent_code, s);

    if (every_display != 0)
      begin_token_list(every_display, every_display_text);

    if (nest_ptr == 1)
      build_page();
  }
  else
  {
    back_input();
    // @<Go into ordinary math mode@>
    {
      push_math(math_shift_group);
      eq_word_define(int_base + cur_fam_code, -1);

      if (every_math != null)
        begin_token_list(every_math, every_math_text);
    }
  }

  direction = -abs(direction);
}

static void start_eq_no (void)
{
  saved(0) = cur_chr;
  incr(save_ptr);

  {
    push_math(math_shift_group);
    eq_word_define(int_base + cur_fam_code, -1);

    if (every_math != 0)
      begin_token_list(every_math, every_math_text);
  }
}

static void scan_math (pointer p, pointer q)
{
  integer c;  // {math character code}
  KANJI_code cx;  // {temporary register for KANJI}

  KANJI(cx) = 0;

restart:
  do {
    get_x_token();
  } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

reswitch:
  switch (cur_cmd)
  {
    case letter:
    case other_char:
    case char_given:
      if ((cur_chr >= 0) && (cur_chr <= 256))
      {
        c = math_code(cur_chr);

        if (c == 0100000)
        {
          // @<Treat |cur_chr| as an active character@>
          {
            cur_cs = cur_chr + active_base;
            cur_cmd = eq_type(cur_cs);
            cur_chr = equiv(cur_cs);
            x_token();
            back_input();
          }

          goto restart;
        }
      }
      else
        KANJI(cx) = cur_chr;
      break;

    case kanji:
    case kana:
    case other_kchar:
    case hangul:
      cx = cur_chr;
      break;

    case kchar_given:
      KANJI(cx) = cur_chr;
      break;

    case char_num:
      {
        scan_char_num();
        cur_chr = cur_val;
        cur_cmd = char_given;
        goto reswitch;
      }
      break;

    case kchar_num:
      {
        scan_char_num();
        cur_chr = cur_val;
        cur_cmd = kchar_given;
        goto reswitch;
      }
      break;

    case math_char_num:
      {
        scan_fifteen_bit_int();
        c = cur_val;
      }
      break;

    case math_given:
      c = cur_chr;
      break;

    case delim_num:
      {
        scan_twenty_seven_bit_int();
        c = cur_val / 010000;
      }
      break;

    default:
      // @<Scan a subformula enclosed in braces and |return|@>
      {
        back_input();
        scan_left_brace();
        saved(0) = p;
        incr(save_ptr);
        push_math(math_group);
        return;
      }
      break;
  }

  if (KANJI(cx) == 0)
  {
    math_type(p) = math_char;
    character(p) = c % 256;

    if ((c >= var_code) && fam_in_range)
      fam(p) = cur_fam;
    else
      fam(p) = (c / 256) % 16;

    if (font_dir[fam_fnt(fam(p) + cur_size)] != dir_default)
    {
      print_err("Not one-byte family");
      help1("IGNORE.");
      error();
    }
  }
  else
  {
    if (q == null)
    {
      math_type(p) = sub_mlist;
      info(p) = new_noad();
      p = nucleus(info(p));
      q = kcode_noad_nucleus(p);
    }

    math_type(p) = math_jchar;
    fam(p) = cur_jfam;
    character(p) = 0;
    math_kcode(p - 1) = KANJI(cx) + kcat_code(kcatcodekey(cx)) * max_cjk_val;

    if (font_dir[fam_fnt(fam(p) + cur_size)] == dir_default)
    {
      print_err("Not two-byte family");
      help1("IGNORE.");
      error();
    }
  }
}

static void set_math_char (integer c)
{
  pointer p;  // {the new noad}

  if (c >= 0100000)
  {
    cur_cs = cur_chr + active_base;
    cur_cmd = eq_type(cur_cs);
    cur_chr = equiv(cur_cs);
    x_token();
    back_input();
  }
  else
  {
    p = new_noad();
    math_type(nucleus(p)) = math_char;
    character(nucleus(p)) = c % 256;
    fam(nucleus(p)) = (c / 256) % 16;

    if (c >= var_code)
    {
      if (fam_in_range)
        fam(nucleus(p)) = cur_fam;

      type(p) = ord_noad;
    }
    else
      type(p) = ord_noad + (c / 010000);

    link(tail) = p;
    tail = p;

    if (font_dir[fam_fnt(fam(nucleus(p)) + cur_size)] != dir_default)
    {
      print_err("Not one-byte family");
      help1("IGNORE.");
      error();
    }
  }
}

static void math_limit_switch (void)
{
  if (head != tail)
    if (type(tail) == op_noad)
    {
      subtype(tail) = cur_chr;
      return;
    }

  print_err("Limit controls must follow a math operator");
  help1("I'm ignoring this misplaced \\limits or \\nolimits command.");
  error();
}

static void scan_delimiter (pointer p, boolean r)
{
   if (r)
     scan_twenty_seven_bit_int();
   else
   {
     do {
      get_x_token();
     } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

     switch (cur_cmd)
     {
       case letter:
       case other_char:
         cur_val = del_code(cur_chr);
         break;

       case delim_num:
         scan_twenty_seven_bit_int();
         break;

       default:
         cur_val = -1;
         break;
     }
   }

   if (cur_val < 0)
   {
     print_err("Missing delimiter (. inserted)");
     help6("I was expecting to see something like `(' or `\\{' or",
         "`\\}' here. If you typed, e.g., `{' instead of `\\{', you",
         "should probably delete the `{' by typing `1' now, so that",
         "braces don't get unbalanced. Otherwise just proceed.",
         "Acceptable delimiters are characters whose \\delcode is",
         "nonnegative, or you can use `\\delimiter <delimiter code>'.");
     back_error();
     cur_val = 0;
   }

   small_fam(p) = (cur_val / 04000000) % 16;
   small_char(p) = (cur_val / 010000) % 256;
   large_fam(p) = (cur_val / 256) % 16;
   large_char(p) = cur_val % 256;
}

static void math_radical (void)
{
  tail_append(get_node(radical_noad_size));
  type(tail) = radical_noad;
  subtype(tail) = normal;
  mem[nucleus(tail)].hh = empty_field;
  mem[subscr(tail)].hh = empty_field;
  mem[supscr(tail)].hh = empty_field;
  scan_delimiter(left_delimiter(tail), true);
  scan_math(nucleus(tail), kcode_noad(tail));
}

static void math_ac (void)
{
  if (cur_cmd == accent)
  {
    print_err("Please use ");
    print_esc("mathaccent");
    prints(" for accents in math mode");
    help2("I'm changing \\accent to \\mathaccent here; wish me luck.",
      "(Accents are not the same in formulas as they are in text.)");
    error();
  }

  tail_append(get_node(accent_noad_size));
  type(tail) = accent_noad;
  subtype(tail) = normal;
  mem[nucleus(tail)].hh = empty_field;
  mem[subscr(tail)].hh = empty_field;
  mem[supscr(tail)].hh = empty_field;
  math_type(accent_chr(tail)) = math_char;
  scan_fifteen_bit_int();
  character(accent_chr(tail)) = cur_val % 256;

  if ((cur_val >= var_code) && fam_in_range)
    fam(accent_chr(tail)) = cur_fam;
  else
    fam(accent_chr(tail)) = (cur_val / 256) % 16;

  scan_math(nucleus(tail), kcode_noad(tail));
}

static void append_choices (void)
{
  tail_append(new_choice());
  incr(save_ptr);
  saved(-1) = 0;
  push_math(math_choice_group);
  scan_left_brace();
}

static pointer fin_mlist (pointer p)
{
  pointer q; // {the mlist to return}

  if (incompleat_noad != null)
  {
    // @<Compleat the incompleat noad@>
    math_type(denominator(incompleat_noad)) = sub_mlist;
    info(denominator(incompleat_noad)) = link(head);

    if (p == null)
      q = incompleat_noad;
    else
    {
      q = info(numerator(incompleat_noad));

      if ((type(q) != left_noad) || (delim_ptr == null))
        confusion("right");

      info(numerator(incompleat_noad)) = link(delim_ptr);
      link(delim_ptr) = incompleat_noad;
      link(incompleat_noad) = p;
    }
  }
  else
  {
    link(tail) = p;
    q = link(head);
  }

  pop_nest();

  return q;
}

static void build_choices (void)
{
  pointer p;  // {the current mlist}

  unsave();
  p = fin_mlist(null);

  switch (saved(-1))
  {
    case 0:
      display_mlist(tail) = p;
      break;

    case 1:
      text_mlist(tail) = p;
      break;

    case 2:
      script_mlist(tail) = p;
      break;

    case 3:
      {
        script_script_mlist(tail) = p;
        decr(save_ptr);
        return;
      }
      break;
  }

  incr(saved(-1));
  push_math(math_choice_group);
  scan_left_brace();
}

static void sub_sup (void)
{
  small_number t; // {type of previous sub/superscript}
  pointer p;  // {field to be filled by |scan_math|}

  t = empty;
  p = null;

  if (tail != head)
    if (script_allowed(tail))
    {
      p = supscr(tail) + cur_cmd - sup_mark;  // {|supscr| or |subscr|}
      t = math_type(p);
    }

  if ((p == null) || (t != empty))
  {
    // @<Insert a dummy noad to be sub/superscripted@>
    tail_append(new_noad());
    p = supscr(tail) + cur_cmd - sup_mark;  // {|supscr| or |subscr|}

    if (t != empty)
    {
      if (cur_cmd == sup_mark)
      {
        print_err("Double superscript");
        help1("I treat `x^1^2' essentially like `x^1{}^2'.");
      }
      else
      {
        print_err("Double subscript");
        help1("I treat `x_1_2' essentially like `x_1{}_2'.");
      }

      error();
    }
  }

  scan_math(p, null);
}

static void package (small_number c)
{
  scaled h; // {height of box}
  pointer p;  // {first node in a box}
  scaled d; // {max depth}

  d = box_max_depth;
  delete_glue_ref(cur_kanji_skip);
  delete_glue_ref(cur_xkanji_skip);

  if (auto_spacing > 0)
    cur_kanji_skip = kanji_skip;
  else
    cur_kanji_skip = zero_glue;

  if (auto_xspacing > 0)
    cur_xkanji_skip = xkanji_skip;
  else
    cur_xkanji_skip = zero_glue;

  add_glue_ref(cur_kanji_skip);
  add_glue_ref(cur_xkanji_skip);
  unsave();
  save_ptr = save_ptr - 3;

  if (mode == -hmode)
  {
    cur_box = hpack(link(head), saved(2), saved(1));
    set_box_dir(cur_box, direction);
    pop_nest();
  }
  else
  {
    cur_box = vpackage(link(head), saved(2), saved(1), d);
    set_box_dir(cur_box, direction);
    pop_nest();

    if (c == vtop_code)
    {
      // @<Readjust the height and depth of |cur_box|, for \.{\\vtop}@>
      h = 0;
      p = list_ptr(cur_box);

      if (p != null)
        if (type(p) <= rule_node)
          h = height(p);

      depth(cur_box) = depth(cur_box) - h + height(cur_box);
      height(cur_box) = h;
    }
  }

  box_end(saved(0));
}

static void math_fraction (void)
{
  small_number c; // {the type of generalized fraction we are scanning}

  c = cur_chr;

  if (incompleat_noad != null)
  {
    // @<Ignore the fraction operation and complain about this ambiguous case@>
    if (c >= delimited_code)
    {
      scan_delimiter(garbage, false);
      scan_delimiter(garbage, false);
    }

    if (c % delimited_code == above_code)
      scan_normal_dimen();

    print_err("Ambiguous; you need another { and }");
    help3("I'm ignoring this fraction specification, since I don't",
      "know whether a construction like `x \\over y \\over z'",
      "means `{x \\over y} \\over z' or `x \\over {y \\over z}'.");
    error();
  }
  else
  {
    incompleat_noad = get_node(fraction_noad_size);
    type(incompleat_noad) = fraction_noad;
    subtype(incompleat_noad) = normal;
    math_type(numerator(incompleat_noad)) = sub_mlist;
    info(numerator(incompleat_noad)) = link(head);
    mem[denominator(incompleat_noad)].hh = empty_field;
    mem[left_delimiter(incompleat_noad)].qqqq = null_delimiter;
    mem[right_delimiter(incompleat_noad)].qqqq = null_delimiter;
    link(head) = null;
    tail = head;
    // @<Use code |c| to distinguish between generalized fractions@>
    if (c >= delimited_code)
    {
      scan_delimiter(left_delimiter(incompleat_noad), false);
      scan_delimiter(right_delimiter(incompleat_noad), false);
    }

    switch (c % delimited_code)
    {
      case above_code:
        scan_normal_dimen();
        thickness(incompleat_noad) = cur_val;
        break;

      case over_code:
        thickness(incompleat_noad) = default_code;
        break;

      case atop_code:
        thickness(incompleat_noad) = 0;
        break;
    }
  }
}

static void math_left_right (void)
{
  small_number t; // {|left_noad| or |right_noad|}
  pointer p;  // {new noad}
  pointer q;  // {resulting mlist}

  t = cur_chr;

  if ((t != left_noad) && (cur_group != math_left_group))
  {
    // @<Try to recover from mismatched \.{\\right}@>
    if (cur_group == math_shift_group)
    {
      scan_delimiter(garbage, false);
      print_err("Extra ");

      if (t == middle_noad)
      {
        print_esc("middle");
        help1("I'm ignoring a \\middle that had no matching \\left.");
      }
      else
      {
        print_esc("right");
        help1("I'm ignoring a \\right that had no matching \\left.");
      }

      error();
    }
    else
      off_save();
  }
  else
  {
    p = new_noad();
    type(p) = t;
    scan_delimiter(delimiter(p), false);

    if (t == middle_noad)
    {
      type(p) = right_noad;
      subtype(p) = middle_noad;
    }

    if (t == left_noad)
      q = p;
    else
    {
      q = fin_mlist(p);
      unsave(); // {end of |math_left_group|}
    }

    if (t != right_noad)
    {
      push_math(math_left_group);
      link(head) = q;
      tail = p;
      delim_ptr = p;
    }
    else
    {
      tail_append(new_noad());
      type(tail) = inner_noad;
      math_type(nucleus(tail)) = sub_mlist;
      info(nucleus(tail)) = q;
    }
  }
}

static void after_math (void)
{
  boolean l;  // {`\.{\\leqno}' instead of `\.{\\eqno}'}
  scaled disp;  // {displacement}
  boolean danger; // {not enough symbol fonts are present}
  integer m;  // {|mmode| or |-mmode|}
  pointer p;  // {the formula}
  pointer a;  // {box containing equation number}
  pointer b;  // {box containing the equation}
  scaled w; // {width of the equation}
  scaled z; // {width of the line}
  scaled e; // {width of equation number}
  scaled q; // {width of equation number plus space to separate from equation}
  scaled d; // {displacement of equation in the line}
  scaled s; // {move the line right this much}
  small_number g1, g2;  // {glue parameter codes for before and after}
  pointer r;  // {kern node used to position the display}
  pointer t;  // {tail of adjustment list}
  pointer j;  // {prototype box}

  danger = false;
  // @<Retrieve the prototype box@>
  if (mode == mmode)
    j = LR_box;
  /*
    @<Check that the necessary fonts for math symbols are present;
    if not, flush the current math lists and set |danger:=true|@>
  */
  if ((font_params[fam_fnt(2 + text_size)] < total_mathsy_params) ||
    (font_params[fam_fnt(2 + script_size)] < total_mathsy_params) ||
    (font_params[fam_fnt(2 + script_script_size)] < total_mathsy_params))
  {
    print_err("Math formula deleted: Insufficient symbol fonts");
    help3("Sorry, but I can't typeset math unless \\textfont 2",
        "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
        "the \\fontdimen values needed in math symbol fonts.");
    error();
    flush_math();
    danger = true;
  }
  else if ((font_params[fam_fnt(3 + text_size)] < total_mathex_params) ||
    (font_params[fam_fnt(3 + script_size)] < total_mathex_params) ||
    (font_params[fam_fnt(3 + script_script_size)] < total_mathex_params))
  {
    print_err("Math formula deleted: Insufficient extension fonts");
    help3("Sorry, but I can't typeset math unless \\textfont 3",
        "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
        "the \\fontdimen values needed in math extension fonts.");
    error();
    flush_math();
    danger = true;
  }

  delete_glue_ref(cur_kanji_skip);
  delete_glue_ref(cur_xkanji_skip);

  if (auto_spacing > 0)
    cur_kanji_skip = kanji_skip;
  else
    cur_kanji_skip = zero_glue;

  if (auto_xspacing > 0)
    cur_xkanji_skip = xkanji_skip;
  else
    cur_xkanji_skip = zero_glue;

  add_glue_ref(cur_kanji_skip);
  add_glue_ref(cur_xkanji_skip);
  m = mode;
  l = false;
  p = fin_mlist(null);  // {this pops the nest}
  // {end of equation number}
  if (mode == -m)
  {
    // @<Check that another \.\$ follows@>
    {
      get_x_token();

      if (cur_cmd != math_shift)
      {
        print_err("Display math should end with $$");
        help2("The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.");
        back_error();
      }
    }

    cur_mlist = p;
    cur_style = text_style;
    mlist_penalties = false;
    mlist_to_hlist();
    a = hpack(link(temp_head), 0, 1);
    set_box_lr(a, dlist);
    unsave();
    decr(save_ptr); // {now |cur_group=math_shift_group|}

    if (saved(0) == 1)
      l = true;

    danger = false;
    // @<Retrieve the prototype box@>
    if (mode == mmode)
      j = LR_box;
    /*
      @<Check that the necessary fonts for math symbols are present;
      if not, flush the current math lists and set |danger:=true|@>
    */
    if ((font_params[fam_fnt(2 + text_size)] < total_mathsy_params) ||
      (font_params[fam_fnt(2 + script_size)] < total_mathsy_params) ||
      (font_params[fam_fnt(2 + script_script_size)] < total_mathsy_params))
    {
      print_err("Math formula deleted: Insufficient symbol fonts");
      help3("Sorry, but I can't typeset math unless \\textfont 2",
          "and \\scriptfont 2 and \\scriptscriptfont 2 have all",
          "the \\fontdimen values needed in math symbol fonts.");
      error();
      flush_math();
      danger = true;
    }
    else if ((font_params[fam_fnt(3 + text_size)] < total_mathex_params) ||
      (font_params[fam_fnt(3 + script_size)] < total_mathex_params) ||
      (font_params[fam_fnt(3 + script_script_size)] < total_mathex_params))
    {
      print_err("Math formula deleted: Insufficient extension fonts");
      help3("Sorry, but I can't typeset math unless \\textfont 3",
        "and \\scriptfont 3 and \\scriptscriptfont 3 have all",
        "the \\fontdimen values needed in math extension fonts.");
      error();
      flush_math();
      danger = true;
    }

    m = mode;
    p = fin_mlist(null);
  }
  else
    a = null;

  if (m < 0)
  {
    // @<Finish math in text@>
    if (direction == dir_tate)
      disp = t_baseline_shift;
    else
      disp = y_baseline_shift;

    append_disp_node_at_begin();
    tail_append(new_math(math_surround, before));
    cur_mlist = p;
    cur_style = text_style;
    mlist_penalties = (mode > 0);
    mlist_to_hlist();
    link(tail) = link(temp_head);

    while (link(tail) != null)
      tail = link(tail);

    tail_append(new_math(math_surround, after));
    append_disp_node_at_end();
    space_factor = 1000;
    inhibit_glue_flag = false;
    unsave();
  }
  else
  {
    if (a == null)
    {
      get_x_token();

      if (cur_cmd != math_shift)
      {
        print_err("Display math should end with $$");
        help2("The `$' that I just saw supposedly matches a previous `$$'.",
            "So I shall assume that you typed `$$' both times.");
        back_error();
      }
    }
    // @<Finish displayed math@>
    cur_mlist = p;
    cur_style = display_style;
    mlist_penalties = false;
    mlist_to_hlist();
    p = link(temp_head);
    adjust_tail = adjust_head;
    b = hpack(p, 0, 1);
    p = list_ptr(b);
    t = adjust_tail;
    adjust_tail = null;
    w = width(b);
    z = display_width;
    s = display_indent;

    if (pre_display_direction < 0)
      s = -s - z;

    if ((a == null) || danger)
    {
      e = 0;
      q = 0;
    }
    else
    {
      e = width(a);
      q = e + math_quad(text_size);
    }

    if (w + q > z)
    {
      /*
        @<Squeeze the equation as much as possible; if there is an equation
        number that should go on a separate line by itself,
        set~|e:=0|@>
      */
      if ((e != 0) && ((w - total_shrink[normal] + q <= z) || (total_shrink[fil] != 0) ||
        (total_shrink[fill] != 0) || (total_shrink[filll] != 0)))
      {
        delete_glue_ref(space_ptr(b));
        delete_glue_ref(xspace_ptr(b));
        free_node(b, box_node_size);
        b = hpack(p, z - q, exactly);
      }
      else
      {
        e = 0;

        if (w > z)
        {
          delete_glue_ref(space_ptr(b));
          delete_glue_ref(xspace_ptr(b));
          free_node(b, box_node_size);
          b = hpack(p, z, exactly);
        }
      }

      w = width(b);
    }
    /*
      @<Determine the displacement, |d|, of the left edge of the equation, with
      respect to the line size |z|, assuming that |l=false|@>
    */
    set_box_lr(b, dlist);
    d = half(z - w);

    if ((e > 0) && (d < 2 * e))
    {
      d = half(z - w - e);

      if (p != null)
        if (!is_char_node(p))
          if (type(p) == glue_node)
            d = 0;
    }
    // @<Append the glue or equation number preceding the display@>
    tail_append(new_penalty(pre_display_penalty));
    // {not enough clearance}
    if ((d + s <= pre_display_size) || l)
    {
      g1 = above_display_skip_code;
      g2 = below_display_skip_code;
    }
    else
    {
      g1 = above_display_short_skip_code;
      g2 = below_display_short_skip_code;
    }
    // {it follows that |type(a)=hlist_node|}
    if (l && (e == 0))
    {
      app_display(j, a, 0);
      tail_append(new_penalty(inf_penalty));
    }
    else
      tail_append(new_param_glue(g1));
    // @<Append the display and perhaps also the equation number@>
    if (e != 0)
    {
      r = new_kern(z - w - e - d);

      if (l)
      {
        link(a) = r;
        link(r) = b;
        b = a;
        d = 0;
      }
      else
      {
        link(b) = r;
        link(r) = a;
      }

      b = hpack(b, 0, 1);
    }

    app_display(j, b, d);
    // @<Append the glue or equation number following the display@>
    if ((a != null) && (e == 0) && !l)
    {
      tail_append(new_penalty(inf_penalty));
      app_display(j, a, z - width(a));
      g2 = 0;
    }
    // {migrating material comes after equation number}
    if (t != adjust_head)
    {
      link(tail) = link(adjust_head);
      tail = t;
    }

    tail_append(new_penalty(post_display_penalty));

    if (g2 > 0)
      tail_append(new_param_glue(g2));
    // @<Flush the prototype box@>
    flush_node_list(j);
    resume_after_display();
  }
}

/* sec 1211 */
static void prefixed_command (void)
{
  small_number a;
  integer m;
  internal_font_number f;
  halfword j;
  font_index k;
  pointer p, q;
  integer n;
  boolean e;

  a = 0;

  while (cur_cmd == prefix)
  {
    if (!odd(a / cur_chr))
      a = a + cur_chr;

    do {
      get_x_token();
    } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

    if (cur_cmd <= max_non_prefixed_command)
    {
      print_err("You can't use a prefix with `");
      print_cmd_chr(cur_cmd, cur_chr);
      print_char('\'');
      help1("I'll pretend you didn't say \\long or \\outer or \\global.");

      if (eTeX_ex)
        help_line[0] = "I'll pretend you didn't say \\long or \\outer \\global or \\protected.";

      back_error();
      return;
    }

    if (tracing_commands > 2)
    {
      if (eTeX_ex)
        show_cur_cmd_chr();
    }
  }

  if (a >= 8)
  {
    j = protected_token;
    a = a - 8;
  }
  else
    j = 0;

  if ((cur_cmd != def) && ((a % 4 != 0) || (j != 0)))
  {
    print_err("You can't use `");
    print_esc("long");
    prints("' or `");
    print_esc("outer");
    help1("I'll pretend you didn't say \\long or \\outer here.");

    if (eTeX_ex)
    {
      help_line[0] = "I'll pretend you didn't say \\long or \\outer or \\protected here.";
      prints("' or `");
      print_esc("protected");
    }

    prints("' with `");
    print_cmd_chr(cur_cmd, cur_chr);
    print_char('\'');
    error();
  }

  if (global_defs != 0)
  {
    if (global_defs < 0)
    {
      if (global)
        a = a - 4;
    }
    else
    {
      if (!global)
        a = a + 4;
    }
  }

  switch (cur_cmd)
  {
    case set_font:
      {
        if (font_dir[cur_chr] == dir_yoko)
          define(cur_jfont_loc, data, cur_chr);
        else if (font_dir[cur_chr] == dir_tate)
          define(cur_tfont_loc, data, cur_chr);
        else
          define(cur_font_loc, data, cur_chr);
      }
      break;

    case def:
      {
        if (odd(cur_chr) && !global && (global_defs >= 0))
          a = a + 4;

        e = (cur_chr >= 2);
        get_r_token();
        p = cur_cs;
        q = scan_toks(true, e);

        if (j != 0)
        {
          q = get_avail();
          info(q) = j;
          link(q) = link(def_ref);
          link(def_ref) = q;
        }

        define(p, call + (a % 4), def_ref);
      }
      break;

    case let:
      {
        n = cur_chr;
        get_r_token();
        p = cur_cs;

        if (n == normal)
        {
          do {
            get_token();
          } while (!(cur_cmd != spacer));

          if (cur_tok == other_token + '=')
          {
            get_token();

            if (cur_cmd == spacer)
              get_token();
          }
        }
        else
        {
          get_token();
          q = cur_tok;
          get_token();
          back_input();
          cur_tok = q;
          back_input();
        }

        if (cur_cmd >= call)
          add_token_ref(cur_chr);
        else if ((cur_cmd == tex_register) || (cur_cmd == toks_register))
        {
          if ((cur_chr < mem_bot) || (cur_chr > lo_mem_stat_max))
            add_sa_ref(cur_chr);
        }

        define(p, cur_cmd, cur_chr);
      }
      break;

    case shorthand_def:
      {
        n = cur_chr;
        get_r_token();
        p = cur_cs;
        define(p, relax, 256);
        scan_optional_equals();

        switch (n)
        {
          case char_def_code:
            {
              scan_char_num();
              define(p, char_given, cur_val);
            }
            break;

          case kchar_def_code:
            {
              scan_char_num();
              define(p, kchar_given, cur_val);
            }
            break;

          case math_char_def_code:
            {
              scan_fifteen_bit_int();
              define(p, math_given, cur_val);
            }
            break;

          default:
            {
              scan_register_num();

              if (cur_val > 255)
              {
                j = n - count_def_code;

                if (j > mu_val)
                  j = tok_val;

                find_sa_element(j, cur_val, true);
                add_sa_ref(cur_ptr);

                if (j == tok_val)
                  j = toks_register;
                else
                  j = tex_register;

                define(p, j, cur_ptr);
              }
              else switch (n)
              {
                case count_def_code:
                  define(p, assign_int, count_base + cur_val);
                  break;

                case dimen_def_code:
                  define(p, assign_dimen, scaled_base + cur_val);
                  break;

                case skip_def_code:
                  define(p, assign_glue, skip_base + cur_val);
                  break;

                case mu_skip_def_code:
                  define(p, assign_mu_glue, mu_skip_base + cur_val);
                  break;

                case toks_def_code:
                  define(p, assign_toks, toks_base + cur_val);
                  break;
              }
            }
            break;
        }
      }
      break;

    case read_to_cs:
      {
        j = cur_chr;
        scan_int();
        n = cur_val;

        if (!scan_keyword("to"))
        {
          print_err("Missing `to' inserted");
          help2("You should have said `\\read<number> to \\cs'.",
              "I'm going to look for the \\cs now.");
          error();
        }

        get_r_token();
        p = cur_cs;
        read_toks(n, p, j);
        define(p, call, cur_val);
      }
      break;

    case toks_register:
    case assign_toks:
      {
        q = cur_cs;
        e = false;

        if (cur_cmd == toks_register)
        {
          if (cur_chr == mem_bot)
          {
            scan_register_num();

            if (cur_val > 255)
            {
              find_sa_element(tok_val, cur_val, true);
              cur_chr = cur_ptr;
              e = true;
            }
            else
              cur_chr = toks_base + cur_val;
          }
          else
            e = true;
        }

        p = cur_chr;
        scan_optional_equals();

        do {
          get_x_token();
        } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

        if (cur_cmd != left_brace)
        {
          if ((cur_cmd == toks_register) || (cur_cmd == assign_toks))
          {
            if (cur_cmd == toks_register)
            {
              if (cur_chr == mem_bot)
              {
                scan_register_num();

                if (cur_val < 256)
                  q = equiv(toks_base + cur_val);
                else
                {
                  find_sa_element(tok_val, cur_val, false);

                  if (cur_ptr == null)
                    q = null;
                  else
                    q = sa_ptr(cur_ptr);
                }
              }
              else
                q = sa_ptr(cur_chr);
            }
            else
              q = equiv(cur_chr);

            if (q == null)
              sa_define(p, null, p, undefined_cs, null);
            else
            {
              add_token_ref(q);
              sa_define(p, q, p, call, q);
            }

            goto done;
          }
        }

        back_input();
        cur_cs = q;
        q = scan_toks(false, false);

        if (link(def_ref) == 0)
        {
          sa_define(p, null, p, undefined_cs, 0);
          free_avail(def_ref);
        }
        else
        {
          if ((p == output_routine_loc) && !e)
          {
            link(q) = get_avail();
            q = link(q);
            info(q) = right_brace_token + '}';
            q = get_avail();
            info(q) = left_brace_token + '{';
            link(q) = link(def_ref);
            link(def_ref) = q;
          }

          sa_define(p, def_ref, p, call, def_ref);
        }
      }
      break;

    case assign_int:
      {
        p = cur_chr;
        scan_optional_equals();
        scan_int();

        if (p == int_base + cur_fam_code)
        {
          if (font_dir[fam_fnt(cur_val)] != dir_default)
            word_define(int_base + cur_jfam_code, cur_val);
          else
            word_define(p, cur_val);
        }
        else
          word_define(p, cur_val);
      }
      break;

    case assign_dimen:
      {
        p = cur_chr;
        scan_optional_equals();
        scan_normal_dimen();
        word_define(p, cur_val);
      }
      break;

    case assign_glue:
    case assign_mu_glue:
      {
        p = cur_chr;
        n = cur_cmd;
        scan_optional_equals();

        if (n == assign_mu_glue)
          scan_glue(mu_val);
        else
          scan_glue(glue_val);

        trap_zero_glue();
        define(p, glue_ref, cur_val);
      }
      break;

    case def_code:
      {
        if (cur_chr == kcat_code_base)
          m = not_cjk;
        else
          m = 0;

        if (cur_chr == cat_code_base)
          n = invalid_code;
        else if (cur_chr == kcat_code_base)
          n = max_char_code;
        else if (cur_chr == math_code_base)
          n = 32768; /* 2^15 */
        else if (cur_chr == sf_code_base)
          n = 32767; /* 2^15 - 1*/
        else if (cur_chr == del_code_base)
          n = 16777215; /* 2^24 - 1 */
        else
          n = 255;

        p = cur_chr;

        if (p == kcat_code_base)
        {
          scan_char_num();
          p = p + kcatcodekey(cur_val);
        }
        else
        {
          scan_ascii_num();
          p = p + cur_val;
        }

        scan_optional_equals();
        scan_int();

        if (((cur_val < m) && (p < del_code_base)) || (cur_val > n))
        {
          print_err("Invalid code(");
          print_int(cur_val);

          if (p < del_code_base)
          {
            prints("), should be in the range ");
            print_int(m);
            prints("..");
          }
          else
            prints("), should be at most ");

          print_int(n);

          if (m == 0)
          {
            help1("I'm going to use 0 instead of that illegal code value.");
            error();
            cur_val = 0;
          }
          else
          {
            help1("I'm going to use 16 instead of that illegal code value.");
            error();
            cur_val = 16;
          }
        }

        if (p < math_code_base)
          define(p, data, cur_val);
        else if (p < del_code_base)
          define(p, data, cur_val);
        else 
          word_define(p, cur_val);
      }
      break;

    case def_family:
      {
        p = cur_chr;
        scan_four_bit_int();
        p = p + cur_val;
        scan_optional_equals();
        scan_font_ident();
        define(p, data, cur_val);
      }
      break;

    case tex_register:
    case advance:
    case multiply:
    case divide:
      do_register_command(a);
      break;

    case set_box:
      {
        scan_register_num();

        if (global)
          n = global_box_flag + cur_val;
        else
          n = box_flag + cur_val;

        scan_optional_equals();

        if (set_box_allowed)
          scan_box(n);
        else
        {
          print_err("Improper ");
          print_esc("setbox");
          help2("Sorry, \\setbox is not allowed after \\halign in a display,",
              "or between \\accent and an accented character.");
          error();
        }
      }
      break;

    case set_aux:
      alter_aux();
      break;

    case set_prev_graf:
      alter_prev_graf();
      break;

    case set_page_dimen:
      alter_page_so_far();
      break;

    case set_page_int:
      alter_integer();
      break;

    case set_box_dimen:
      alter_box_dimen();
      break;

    case set_shape:
      {
        q = cur_chr;
        scan_optional_equals();
        scan_int();
        n = cur_val;

        if (n <= 0)
          p = null;
        else if (q > par_shape_loc)
        {
          n = (cur_val / 2) + 1;
          p = get_node(2 * n + 1);
          info(p) = n;
          n = cur_val;
          mem[p + 1].cint = n;

          for (j = p + 2; j <= p + n + 1; ++j)
          {
            scan_int();
            mem[j].cint = cur_val;
          }

          if (!odd(n))
            mem[p + n + 2].cint = 0;
        }
        else
        {
          p = get_node(2 * n + 1);
          info(p) = n;

          for (j = 1; j <= n; j++)
          {
            scan_normal_dimen();
            mem[p + 2 * j - 1].cint = cur_val;
            scan_normal_dimen();
            mem[p + 2 * j].cint = cur_val;
          }
        }

        define(q, shape_ref, p);
      }
      break;

    case hyph_data:
      if (cur_chr == 1)
      {
        if (aptex_env.flag_initex)
        {
          new_patterns();
          goto done;
        }

        print_err("Patterns can be loaded only by INITEX");
        help0();
        error();

        do {
          get_token();
        } while (!(cur_cmd == right_brace));

        return;
      }
      else
      {
        new_hyph_exceptions();
        goto done;
      }
      break;

    case assign_font_dimen:
      {
        find_font_dimen(true);
        k = cur_val;
        scan_optional_equals();
        scan_normal_dimen();
        font_info[k].cint = cur_val;
      }
      break;

    case assign_font_int:
      {
        n = cur_chr;
        scan_font_ident();
        f = cur_val;
        scan_optional_equals();
        scan_int();

        if (n == 0)
          hyphen_char[f] = cur_val;
        else
          skew_char[f] = cur_val;
      }
      break;

    case def_tfont:
    case def_jfont:
    case def_font:
      new_font(a);
      break;

    case set_interaction:
      new_interaction();
      break;

    case set_kansuji_char:
      {
        p = cur_chr;
        scan_int();
        n = cur_val;
        scan_optional_equals();
        scan_int();

        if (!is_char_kanji(cur_val))
        {
          print_err("Invalid KANSUJI char (");
          print_hex_safe(cur_val);
          print_char(')');
          help1("I'm skipping this control sequences.");
          error();
          return;
        }
        else if ((n < 0) || (n > 9))
        {
          print_err("Invalid KANSUJI number (");
          print_int(n);
          print_char(')');
          help1("I'm skipping this control sequences.");
          error();
          return;
        }
        else
          define(kansuji_base + n, n, tokanji(toDVI(cur_val)));
      }
      break;

    case set_auto_spacing:
      {
        if (cur_chr < 2)
          p = auto_spacing_code;
        else
        {
          p = auto_xspacing_code;
          cur_chr = (cur_chr % 2);
        }
        
        define(p, data, cur_chr);
      }
      break;

    case set_enable_cjk_token:
      define(enable_cjk_token_code, data, cur_chr);
      break;

    case assign_inhibit_xsp_code:
      {
        p = cur_chr;
        scan_int();
        n = cur_val;
        scan_optional_equals();
        scan_int();

        if (is_char_kanji(n))
        {
          j = get_inhibit_pos(tokanji(n), new_pos);

          if ((j != no_entry) && (cur_val > inhibit_after))
          {
            if (global || cur_level == level_one)
              cur_val = inhibit_unused;
            else
              cur_val = inhibit_none;
          }
          else if (j == no_entry)
          {
            print_err("Inhibit table is full!!");
            help1("I'm skipping this control sequences.");
            error();
            return;
          }

          define(inhibit_xsp_code_base + j, cur_val, n);
        }
        else
        {
          print_err("Invalid KANJI code (");
          print_hex_safe(n);
          print_char(')');
          help1("I'm skipping this control sequences.");
          error();
          return;
        }
      }
      break;

    case assign_kinsoku:
      {
        p = cur_chr;
        scan_int();
        n = cur_val;
        scan_optional_equals();
        scan_int();

        if (is_char_ascii(n) || is_char_kanji(n))
        {
          j = get_kinsoku_pos(tokanji(n), new_pos);

          if ((j != no_entry) && (cur_val == 0) && (global || cur_level == level_one))
          {
            define(kinsoku_base + j, kinsoku_unused_code, 0);
          }
          else
          {
            if (j == no_entry)
            {
              print_err("KINSOKU table is full!!");
              help1("I'm skipping this control sequences.");
              error();
              return;
            }

            if ((p == pre_break_penalty_code) || (p == post_break_penalty_code))
            {
              define(kinsoku_base + j, p, tokanji(n));
              word_define(kinsoku_penalty_base + j, cur_val);
            }
            else
              confusion("kinsoku");
          }
        }
        else
        {
          print_err("Invalid KANJI code for ");

          if (p == pre_break_penalty_code)
            prints("pre");
          else if (p == post_break_penalty_code)
            prints("post");
          else
            print_char('?');

          prints("breakpenalty (");
          print_hex_safe(n);
          print_char(')');
          help1("I'm skipping this control sequences.");
          error();
          return;
        }
      }
      break;

    default:
      confusion("prefix");
      break;
  }

done:
  if (after_token != 0)
  {
    cur_tok = after_token;
    back_input();
    after_token = 0;
  }
}

void resume_after_display (void)
{
  if (cur_group != math_shift_group)
    confusion("display");

  unsave();
  prev_graf = prev_graf + 3;
  push_nest();
  adjust_dir = direction;
  inhibit_glue_flag = false;
  mode = hmode;
  space_factor = 1000;
  set_cur_lang();
  clang = cur_lang;
  prev_graf = (norm_min(left_hyphen_min) * 0100 + norm_min(right_hyphen_min)) * 0200000 + cur_lang;
  // @<Scan an optional space@>
  {
    get_x_token();

    if (cur_cmd != spacer)
      back_input();
  }

  if (nest_ptr == 1)
    build_page();
}

void get_r_token (void)
{
restart:
  do {
    get_token();
  } while (!(cur_tok != space_token));

  if ((cur_cs == 0) || (cur_cs > frozen_control_sequence))
  {
    print_err("Missing control sequence inserted");
    help5("Please don't say `\\def cs{...}', say `\\def\\cs{...}'.",
      "I've inserted an inaccessible control sequence so that your",
      "definition will be completed without mixing me up too badly.",
      "You can recover graciously from this error, if you're",
      "careful; see exercise 27.2 in The TeXbook.");

    if (cur_cs == 0)
      back_input();

    cur_tok = cs_token_flag + frozen_protection;
    ins_error();
    goto restart;
  }
}

void trap_zero_glue (void)
{
  if ((width(cur_val) == 0) && (stretch(cur_val) == 0) && (shrink(cur_val) == 0))
  {
    add_glue_ref(zero_glue);
    delete_glue_ref(cur_val);
    cur_val = zero_glue;
  }
}

void do_register_command (small_number a)
{
  pointer l, q, r, s; // {for list manipulation}
  char p; // {type of register involved}
  boolean e;  // {does |l| refer to a sparse array element?}
  integer w;  // {integer or dimen value of |l|}

  q = cur_cmd;
  e = false;  // {just in case, will be set |true| for sparse array elements}
  // @<Compute the register location |l| and its type |p|; but |return| if invalid@>
  {
    if (q != tex_register)
    {
      get_x_token();

      if ((cur_cmd >= assign_int) && (cur_cmd <= assign_mu_glue))
      {
        l = cur_chr;
        p = cur_cmd - assign_int;
        goto found;
      }

      if (cur_cmd != tex_register)
      {
        print_err("You can't use `");
        print_cmd_chr(cur_cmd, cur_chr);
        prints("' after ");
        print_cmd_chr(q, 0);
        help1("I'm forgetting what you said and not changing anything.");
        error();
        return;
      }
    }

    if ((cur_chr < mem_bot) || (cur_chr > lo_mem_stat_max))
    {
      l = cur_chr;
      p = sa_type(l);
      e = true;
    }
    else
    {
      p = cur_chr - mem_bot;
      scan_register_num();

      if (cur_val > 255)
      {
        find_sa_element(p, cur_val, true);
        l = cur_ptr;
        e = true;
      }
      else switch (p)
      {
        case int_val:
          l = cur_val + count_base;
          break;

        case dimen_val:
          l = cur_val + scaled_base;
          break;

        case glue_val:
          l = cur_val + skip_base;
          break;

        case mu_val:
          l = cur_val + mu_skip_base;
          break;
      }
    }
  }

found:
  if (p < glue_val)
    if (e)
      w = sa_int(l);
    else
      w = eqtb[l].cint;
  else if (e)
    s = sa_ptr(l);
  else
    s = equiv(l);

  if (q == tex_register)
    scan_optional_equals();
  else if (scan_keyword("by"))
    do_nothing();

  arith_error = false;

  if (q < multiply)
    if (p < glue_val)
    {
      if (p == int_val)
        scan_int();
      else
        scan_normal_dimen();

      if (q == advance)
        cur_val = cur_val + w;
    }
    else
    {
      scan_glue(p);

      if (q == advance)
      {
        q = new_spec(cur_val);
        r = s;
        delete_glue_ref(cur_val);
        width(q) = width(q) + width(r);

        if (stretch(q) == 0)
          stretch_order(q) = normal;

        if (stretch_order(q) == stretch_order(r))
          stretch(q) = stretch(q) + stretch(r);
        else if ((stretch_order(q) < stretch_order(r)) && (stretch(r) != 0))
        {
          stretch(q) = stretch(r);
          stretch_order(q) = stretch_order(r);
        }

        if (shrink(q) == 0)
          shrink_order(q) = normal;

        if (shrink_order(q) == shrink_order(r))
          shrink(q) = shrink(q) + shrink(r);
        else if ((shrink_order(q) < shrink_order(r)) && (shrink(r) != 0))
        {
          shrink(q) = shrink(r);
          shrink_order(q) = shrink_order(r);
        }

        cur_val = q;
      }
    }
  else
  {
    scan_int();

    if (p < glue_val)
      if (q == multiply)
        if (p == int_val)
          cur_val = mult_integers(w, cur_val);
        else
          cur_val = nx_plus_y(w, cur_val, 0);
      else
        cur_val = x_over_n(w, cur_val);
    else
    {
      r = new_spec(s);

      if (q == multiply)
      {
        width(r) = nx_plus_y(width(s), cur_val, 0);
        stretch(r) = nx_plus_y(stretch(s), cur_val, 0);
        shrink(r) = nx_plus_y(shrink(s), cur_val, 0);
      }
      else
      {
        width(r) = x_over_n(width(s), cur_val);
        stretch(r) = x_over_n(stretch(s), cur_val);
        shrink(r) = x_over_n(shrink(s), cur_val);
      }

      cur_val = r;
    }
  }

  if (arith_error)
  {
    print_err("Arithmetic overflow");
    help2("I can't carry out that multiplication or division,",
        "since the result is out of range.");

    if (p >= glue_val)
      delete_glue_ref(cur_val);

    error();
    return;
  }

  if (p < glue_val)
    sa_word_define(l, cur_val);
  else
  {
    trap_zero_glue();
    sa_define(l, cur_val, l, glue_ref, cur_val);
  }
}

void alter_aux (void)
{
  halfword c; // {|hmode| or |vmode|

  if (cur_chr != abs(mode))
    report_illegal_case();
  else
  {
    c = cur_chr;
    scan_optional_equals();

    if (c == vmode)
    {
      scan_normal_dimen();
      prev_depth = cur_val;
    }
    else
    {
      scan_int();

      if ((cur_val <= 0) || (cur_val > 32767))
      {
        print_err("Bad space factor");
        help1("I allow only values in the range 1..32767 here.");
        int_error(cur_val);
      }
      else
        space_factor = cur_val;
    }
  }
}

void alter_prev_graf (void)
{
  integer p;  // {index into |nest|}

  nest[nest_ptr] = cur_list;
  p = nest_ptr;

  while (abs(nest[p].mode_field) != vmode)
    decr(p);

  scan_optional_equals();
  scan_int();

  if (cur_val < 0)
  {
    print_err("Bad ");
    print_esc("prevgraf");
    help1("I allow only nonnegative values here.");
    int_error(cur_val);
  }
  else
  {
    nest[p].pg_field = cur_val;
    cur_list = nest[nest_ptr];
  }
}

void alter_page_so_far (void)
{
  uint32_t c; // {index into |page_so_far|}

  c = cur_chr;
  scan_optional_equals();
  scan_normal_dimen();
  page_so_far[c] = cur_val;
}

void alter_integer (void)
{
  small_number c; // {0 for \.{\\deadcycles}, 1 for \.{\\insertpenalties}, etc.}

  c = cur_chr;
  scan_optional_equals();
  scan_int();

  if (c == 0)
    dead_cycles = cur_val;
  else if (c == 2)
  {
    if ((cur_val < batch_mode) || (cur_val > error_stop_mode))
    {
      print_err("Bad interaction mode");
      help2("Modes are 0=batch, 1=nonstop, 2=scroll, and",
        "3=errorstop. Proceed, and I'll ignore this case.");
      int_error(cur_val);
    }
    else
    {
      cur_chr = cur_val;
      new_interaction();
    }
  }
  else
    insert_penalties = cur_val;
}

void alter_box_dimen (void)
{
  small_number c; // {|width_offset| or |height_offset| or |depth_offset|}
  pointer p, q; // {temporary registers}
  pointer b;  // {box register}

  c = cur_chr;
  scan_register_num();
  fetch_box(b);
  scan_optional_equals();
  scan_normal_dimen();

  if (b != null)
  {
    q = b;
    p = link(q);

    while (p != null)
    {
      if (abs(direction) == abs(box_dir(p)))
        q = p;

      p = link(p);
    }

    if (abs(box_dir(q)) != abs(direction))
    {
      p = link(b);
      link(b) = null;
      q = new_dir_node(q, abs(direction));
      list_ptr(q) = null;
      link(q) = p;
      link(b) = q;
    }

    mem[q + c].cint = cur_val;
  }
}

static boolean scan_keyword_noexpand(const char * s)
{
  pointer p; // {tail of the backup list}
  pointer q; // {new node being added to the token list via |store_new_token|}
  const char * k; // {index into |str_pool|}

  p = backup_head;
  link(p) = null;
  k = s;

  while (*k)
  {
    get_token();
    if ((cur_cs == 0) && ((cur_chr == (*k)) || (cur_chr == (*k) - 'a' + 'A')))
    {
      store_new_token(cur_tok);
      incr(k);
    }
    else if ((cur_cmd != spacer) || (p != backup_head))
    {
      back_input();
      if (p != backup_head)
        back_list(link(backup_head));
      return false;
    }
  }
  flush_list(link(backup_head));
  return true;
}

void new_font (small_number a)
{
  pointer u;  // {user's font identifier}
  scaled s; // {stated ``at'' size, or negative of scaled magnification}
  internal_font_number f; // {runs through existing fonts}
  str_number t; // {name for the frozen font identifier}
  char old_setting; // {holds |selector| setting}
  str_number flushable_string;

  if (job_name == 0)
    open_log_file();  // {avoid confusing \.{texput} with the font name}

  // Scan the font encoding specification
  {
    jfm_enc = 0;
    if (scan_keyword_noexpand("in"))
    {
      if (scan_keyword_noexpand("jis"))
        jfm_enc = 1;
      else if (scan_keyword_noexpand("ucs"))
        jfm_enc = 2;
      else 
      {
        print_err("Unknown TFM encoding");
        help1("TFM encoding specification is ignored.");
        error();
      }
    }
  }
  get_r_token();
  u = cur_cs;

  if (u >= hash_base)
    t = text(u);
  else if (u >= single_base)
  {
    if (u == null_cs)
      t = 1213; /* FONT */
    else
      t = u - single_base;
  }
  else
  {
    old_setting = selector;
    selector = new_string;
    prints("FONT");
    print(u - active_base);
    selector = old_setting;
    str_room(1);
    t = make_string();
  }

  define(u, set_font, null_font);
  scan_optional_equals();
  scan_file_name();
  // @<Scan the font size specification@>
  name_in_progress = true;  // {this keeps |cur_name| from being changed}

  if (scan_keyword("at"))
  {
    // @<Put the \(p)(positive) `at' size into |s|@>
    scan_normal_dimen();
    s = cur_val; 

    if ((s <= 0) || (s >= 01000000000))
    {
      print_err("Improper `at' size (");
      print_scaled(s);
      prints("pt), replaced by 10pt");
      help2("I can only handle fonts at positive sizes that are",
        "less than 2048pt, so I've changed what you said to 10pt.");
      error();
      s = 10 * unity;
    }
  }
  else if (scan_keyword("scaled"))
  {
    scan_int();
    s = -cur_val;

    if ((cur_val <= 0) || (cur_val > 32768))
    {
      print_err("Illegal magnification has been changed to 1000");
      help1("The magnification ratio must be between 1 and 32768.");
      int_error(cur_val);
      s = -1000;
    }
  }
  else
    s = -1000;

  name_in_progress = false;
  /*
    @<If this font has already been loaded, set |f| to the internal
    font number and |goto common_ending|@>
  */
  flushable_string = str_ptr - 1;

  for (f = font_base + 1; f < font_ptr; f++)
  {
    if (str_eq_str(font_name[f], cur_name) && str_eq_str(font_area[f], cur_area))
    {
      if (cur_name == flushable_string)
      {
        flush_string();
        cur_name = font_name[f];
      }

      if (s > 0)
      {
        if (s == font_size[f])
          goto common_ending;
      }
      else if (font_size[f] == xn_over_d(font_dsize[f], -s, 1000))
        goto common_ending;
    }
  }

  f = read_font_info(u, cur_name, cur_area, s);

common_ending:
  define(u, set_font, f);
  eqtb[font_id_base + f] = eqtb[u];
  font_id_text(f) = t;
}

void new_interaction (void)
{
  print_ln();
  interaction = cur_chr;

  // @<Initialize the print |selector| based on |interaction|@>;
  if (interaction == batch_mode)
    selector = no_print;
  else
    selector = term_only;

  if (log_opened)
    selector = selector + 2;
}

void do_assignments (void)
{
  while (true)
  {
    do {
      get_x_token();
    } while (!((cur_cmd != spacer) && (cur_cmd != relax)));

    if (cur_cmd <= max_non_prefixed_command)
      return;

    set_box_allowed = false;
    prefixed_command();
    set_box_allowed = true;
  }
}

static void open_or_close_in (void)
{
  uint32_t c; // {1 for \.{\\openin}, 0 for \.{\\closein}}
  uint32_t n; // 1..15 {stream number}

  c = cur_chr;
  scan_four_bit_int();
  n = cur_val;

  if (read_open[n] != closed)
  {
    a_close(read_file[n]);
    read_open[n] = closed;
  }

  if (c != 0)
  {
    scan_optional_equals();
    scan_file_name();
    pack_cur_name();

    if ((cur_ext != 335) && a_open_in(read_file[n]))
      read_open[n] = just_open;
    else if ((cur_ext != 785) && (name_length + 5 < file_name_size))
    {
      strncpy((char *) name_of_file + name_length + 1, ".tex ", 5);
      name_length = name_length + 4;

      if (a_open_in(read_file[n]))
        read_open[n] = just_open;
      else
      {
        name_length = name_length - 4;
        name_of_file[name_length + 1] = ' ';

        if ((cur_ext == 335) && a_open_in(read_file[n]))
          read_open[n] = just_open;
      }
    }
  }
}

static void issue_message (void)
{
  char old_setting; // {holds |selector| setting}
  char c; // {identifies \.{\\message} and \.{\\errmessage}}
  str_number s; // {the message}

  c = cur_chr;
  link(garbage) = scan_toks(false, true);
  old_setting = selector;
  selector = new_string;
  token_show(def_ref);
  selector = old_setting;
  flush_list(def_ref);
  str_room(1);
  s = make_string();

  if (c == 0)
  {
    if (term_offset + length(s) > max_print_line - 2)
      print_ln();
    else if ((term_offset > 0) || (file_offset > 0))
      print_char(' ');

    slow_print(s);
    update_terminal();
  }
  else
  {
    print_err("");
    slow_print(s);

    if (err_help != 0)
      use_err_help = true;
    else if (long_help_seen)
      help1("(That was another \\errmessage.)");
    else
    {
      if (interaction < error_stop_mode)
        long_help_seen = true;

      help4("This error message was generated by an \\errmessage",
        "command, so I can't give any explicit help.",
        "Pretend that you're Hercule Poirot: Examine all clues,",
        "and deduce the truth by order and method.");
    }

    error();
    use_err_help = false;
  }

  flush_string();
}

static void shift_case (void)
{
  pointer b;
  pointer p;
  halfword t;
  eight_bits c;

  b = cur_chr;
  p = scan_toks(false, false);
  p = link(def_ref);

  while (p != 0)
  {
    t = info(p); 

    if ((t < cs_token_flag + single_base) && !check_kanji(t))
    {
      c = t % max_char_val;

      if (equiv(b + c) != 0)
        info(p) = t - c + equiv(b + c);
    }

    p = link(p);
  }

  back_list(link(def_ref));
  free_avail(def_ref);
}

static void show_whatever (void)
{
  pointer p;  // {tail of a token list to show}
  small_number t; // {type of conditional being shown}
  int m;  // {upper bound on |fi_or_else| codes}
  integer l;  // {line where that conditional began}
  integer n;  // {level of \.{\\if...\\fi} nesting}

  switch (cur_chr)
  {
    case show_lists_code:
      {
        // @<Adjust |selector| based on |show_stream|@>
        adjust_selector_based_on_show_stream();
        begin_diagnostic();
        show_activities();
      }
      break;

    case show_box_code:
      // @<Show the current contents of a box@>
      {
        scan_register_num();
        fetch_box(p);
        // @<Adjust |selector| based on |show_stream|@>
        adjust_selector_based_on_show_stream();
        begin_diagnostic();
        print_nl("> \\box");
        print_int(cur_val);
        print_char('=');

        if (p == null)
          prints("void");
        else
          show_box(p);
      }
      break;

    case show_code:
      // @<Show the current meaning of a token, then |goto common_ending|@>
      {
        get_token();
        // @<Adjust |selector| based on |show_stream|@>
        adjust_selector_based_on_show_stream();

        if (interaction == error_stop_mode)
          wake_up_terminal();

        print_nl("> ");

        if (cur_cs != 0)
        {
          sprint_cs(cur_cs);
          print_char('=');
        }

        print_meaning();
        goto common_ending;
      }
      break;

    case show_mode:
      // @<Show the current japanese processing mode@>
      {
        print_nl("> ");

        if (auto_spacing > 0)
          prints("auto spacing mode; ");
        else
          prints("no auto spacing mode; ");

        print_nl("> ");

        if (auto_xspacing > 0)
          prints("auto xspacing mode; ");
        else
          prints("no auto xspacing mode; ");

        goto common_ending;
      }
      break;

    case show_groups:
      {
        // @<Adjust |selector| based on |show_stream|@>
        adjust_selector_based_on_show_stream();
        begin_diagnostic();
        show_save_groups();
      }
      break;

    case show_ifs:
      {
        // @<Adjust |selector| based on |show_stream|@>
        adjust_selector_based_on_show_stream();
        begin_diagnostic();
        print_nl("");
        print_ln();

        if (cond_ptr == null)
        {
          print_nl("### ");
          prints("no active conditionals");
        }
        else
        {
          p = cond_ptr;
          n = 0;

          do {
            incr(n);
            p = link(p);
          } while (!(p == null));

          p = cond_ptr;
          t = cur_if;
          l = if_line;
          m = if_limit;

          do {
            print_nl("### level ");
            print_int(n);
            prints(": ");
            print_cmd_chr(if_test, t);

            if (m == fi_code)
              print_esc("else");

            print_if_line(l);
            decr(n);
            t = subtype(p);
            l = if_line_field(p);
            m = type(p);
            p = link(p);
          } while (!(p == null));
        }
      }
      break;

    default:
      /*
        @<Show the current value of some parameter or register,
        then |goto common_ending|@>
      */
      {
        p = the_toks();
        // @<Adjust |selector| based on |show_stream|@>
        adjust_selector_based_on_show_stream();

        if (interaction == error_stop_mode)
          wake_up_terminal();

        print_nl("> ");
        token_show(temp_head);
        flush_list(link(temp_head));
        goto common_ending;
      }
      break;
  }

  // @<Complete a potentially long \.{\\show} command@>
  end_diagnostic(true);
  print_err("OK");

  if (selector == term_and_log)
    if (tracing_online <= 0)
    {
      selector = term_only;
      prints(" (see the transcript file)");
      selector = term_and_log;
    }

common_ending:
  if (selector < no_print)
  {
    print_ln();

    // @<Initialize the print |selector| based on |interaction|@>;
    if (interaction == batch_mode)
      selector = no_print;
    else
      selector = term_only;

    if (log_opened)
      selector = selector + 2;
  }
  else
  {
    if (interaction < error_stop_mode)
    {
      help0();
      decr(error_count);
    }
    else if (tracing_online > 0)
    {
      help3("This isn't an error message; I'm just \\showing something.",
        "Type `I\\show...' to show more (e.g., \\show\\cs,",
        "\\showthe\\count10, \\showbox255, \\showlists).");
    }
    else
    {
      help5("This isn't an error message; I'm just \\showing something.",
        "Type `I\\show...' to show more (e.g., \\show\\cs,",
        "\\showthe\\count10, \\showbox255, \\showlists).",
        "And type `I\\tracingonline=1\\show...' to show boxes and",
        "lists on your terminal as well as in the transcript file.");
    }

    error();
  }
}

static void new_whatsit (small_number s, small_number w)
{
  pointer p;  // {the new node}

  p = get_node(w);
  type(p) = whatsit_node;
  subtype(p) = s;
  link(tail) = p;
  tail = p;
}

static void new_write_whatsit (small_number w)
{
  new_whatsit(cur_chr, w);

  if (w != write_node_size)
    scan_four_bit_int();
  else
  {
    scan_int();

    if (cur_val < 0)
      cur_val = 17;
    else if ((cur_val > 15) && (cur_val != 18))
      cur_val = 16;
  }

  write_stream(tail) = cur_val;
  inhibit_glue_flag = false;
}

static void do_extension (void)
{
  integer k;  // {all-purpose integers}
  pointer p;  // {all-purpose pointers}

  switch (cur_chr)
  {
    case open_node:
      // @<Implement \.{\\openout}@>
      {
        new_write_whatsit(open_node_size);
        scan_optional_equals();
        scan_file_name();
        open_name(tail) = cur_name;
        open_area(tail) = cur_area;
        open_ext(tail) = cur_ext;
      }
      break;

    case write_node:
      // @<Implement \.{\\write}@>
      {
        k = cur_cs;
        new_write_whatsit(write_node_size);
        cur_cs = k;
        p = scan_toks(false, false);
        write_tokens(tail) = def_ref;
      }
      break;

    case close_node:
      // @<Implement \.{\\closeout}@>
      {
        new_write_whatsit(write_node_size);
        write_tokens(tail) = 0;
      }
      break;

    case special_node:
      // @<Implement \.{\\special}@>
      {
        new_whatsit(special_node, write_node_size);
        write_stream(tail) = null;
        p = scan_toks(false, true);
        write_tokens(tail) = def_ref;
        inhibit_glue_flag = false;
      }
      break;

    case immediate_code:
      // @<Implement \.{\\immediate}@>
      {
        get_x_token();

        if ((cur_cmd == extension) && (cur_chr <= close_node))
        {
          k = inhibit_glue_flag;
          p = tail;
          do_extension();
          out_what(tail);
          flush_node_list(tail);
          tail = p;
          link(p) = null;
          inhibit_glue_flag = k;
        }
        else
          back_input();
      }
      break;

    case set_language_code:
      // @<Implement \.{\\setlanguage}@>
      if (abs(mode) != hmode)
        report_illegal_case();
      else
      {
        inhibit_glue_flag = false;
        new_whatsit(language_node, small_node_size);
        scan_int();

        if (cur_val <= 0)
          clang = 0;
        else if (cur_val > 255)
          clang = 0;
        else
          clang = cur_val;

        what_lang(tail) = clang;
        what_lhm(tail) = norm_min(left_hyphen_min);
        what_rhm(tail) = norm_min(right_hyphen_min);
      }
      break;

    case pdf_save_pos_node:
      {
        new_whatsit(pdf_save_pos_node, small_node_size);
        inhibit_glue_flag = false;
      }
      break;

    case reset_timer_code:
      {
        aptex_utils_get_seconds_and_micros(&epochseconds, &microseconds);
      }
      break;

    case set_random_seed_code:
      {
        scan_int();

        if (cur_val < 0)
          negate(cur_val);

        random_seed = cur_val;
        init_randoms(random_seed);
      }
      break;

    default:
      confusion("ext1");
      break;
  }
}

static void fix_language (void)
{
  /* ASCII_code l; */
  int l;

  if (language <= 0)
    l = 0; 
  else if (language > 255)
    l = 0;
  else
    l = language;

  if (l != clang)
  {
    inhibit_glue_flag = false;
    new_whatsit(language_node, small_node_size);
    what_lang(tail) = l;
    clang = l;
    what_lhm(tail) = norm_min(left_hyphen_min);
    what_rhm(tail) = norm_min(right_hyphen_min);
  }
}

static void handle_right_brace (void)
{
  pointer p, q; // {for short-term use}
  pointer r;    // {temporaly}
  scaled d;     // {holds |split_max_depth| in |insert_group|}
  integer f;    // {holds |floating_penalty| in |insert_group|}

  switch (cur_group)
  {
    case simple_group:
      unsave();
      break;

    case bottom_level:
      {
        print_err("Too many }'s");
        help2("You've closed more groups than you opened.",
          "Such booboos are generally harmless, so keep going.");
        error();
      }
      break;

    case semi_simple_group:
    case math_shift_group:
    case math_left_group:
      extra_right_brace();
      break;

    case hbox_group:
      {
        adjust_hlist(head, false);
        package(0);
      }
      break;

    case adjusted_hbox_group:
      {
        adjust_hlist(head, false);
        adjust_tail = adjust_head;
        package(0);
      }
      break;

    case vbox_group:
      if ((partoken_context > 0) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
      {
        end_graf();
        package(0);
      }
      break;

    case vtop_group:
      if ((partoken_context > 0) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
      {
        end_graf();
        package(vtop_code);
      }
      break;

    case insert_group:
      if ((partoken_context > 1) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
      {
        end_graf();
        q = split_top_skip;
        add_glue_ref(q);
        d = split_max_depth;
        f = floating_penalty;
        unsave();
        decr(save_ptr);
        p = vpackage(link(head), 0, 1, max_dimen);
        set_box_dir(p, direction);
        pop_nest();

        if (saved(0) < 255)
        {
          r = get_node(ins_node_size);
          type(r) = ins_node;
          subtype(r) = saved(0);
          height(r) = height(p) + depth(p);
          ins_ptr(r) = list_ptr(p);
          split_top_ptr(r) = q;
          depth(r) = d;
          float_cost(r) = f;
          set_ins_dir(r, box_dir(p));

          if (!is_char_node(tail) && (type(tail) == disp_node))
            prev_append(r);
          else
            tail_append(r);
        }
        else
        {
          if (abs(box_dir(p)) != abs(adjust_dir))
          {
            print_err("Direction Incompatible");
            help1("\\vadjust's argument and outer vlist must have same direction.");
            error();
            flush_node_list(list_ptr(p));
          }
          else
          {
            r = get_node(small_node_size);
            type(r) = adjust_node;
            subtype(r) = 0; // {the |subtype| is not used}
            adjust_ptr(r) = list_ptr(p);
            delete_glue_ref(q);

            if (!is_char_node(tail) && (type(tail) == disp_node))
              prev_append(r);
            else
              tail_append(r);
          }
        }

        delete_glue_ref(space_ptr(p));
        delete_glue_ref(xspace_ptr(p));
        free_node(p, box_node_size);

        if (nest_ptr == 0)
          build_page();
      }
      break;

    case output_group:
      if ((partoken_context > 1) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
      {
        if ((loc != 0) || ((token_type != output_text) && (token_type != backed_up)))
        {
          print_err("Unbalanced output routine");
          help2("Your sneaky output routine has problematic {'s and/or }'s.",
            "I can't handle that very well; good luck.");
          error();

          do {
            get_token();
          } while (!(loc == 0));
        }

        end_token_list();
        end_graf();
        unsave();
        output_active = false;
        insert_penalties = 0;

        if (box(255) != 0)
        {
          print_err("Output routine didn't use all of ");
          print_esc("box");
          print_int(255);
          help3("Your \\output commands should empty \\box255,",
            "e.g., by saying `\\shipout\\box255'.",
            "Proceed; I'll discard its present contents.");
          box_error(255);
        }

        if (tail != head)
        {
          link(page_tail) = link(head);
          page_tail = tail;
        }

        if (link(page_head) != 0)
        {
          if (link(contrib_head) == 0)
            contrib_tail = page_tail;

          link(page_tail) = link(contrib_head);
          link(contrib_head) = link(page_head);
          link(page_head) = 0;
          page_tail = page_head;
        }

        flush_node_list(page_disc);
        page_disc = null;
        pop_nest();
        build_page();
      }
      break;

    case disc_group:
      build_discretionary();
      break;

    case align_group:
      {
        back_input();
        cur_tok = cs_token_flag + frozen_cr;
        print_err("Missing ");
        print_esc("cr");
        prints("inserted");
        help1("I'm guessing that you meant to end an alignment here.");
        ins_error();
      }
      break;

    case no_align_group:
      if ((partoken_context > 1) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
      {
        end_graf();
        unsave();
        align_peek();
      }
      break;

    case vcenter_group:
      if ((partoken_context > 0) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
      {
        end_graf();
        unsave();
        save_ptr = save_ptr - 2;
        p = vpackage(link(head), saved(1), saved(0), max_dimen);
        set_box_dir(p, direction);
        pop_nest();

        if (abs(box_dir(p)) != abs(direction))
          p = new_dir_node(p, abs(direction));

        tail_append(new_noad());
        type(tail) = vcenter_noad;
        math_type(nucleus(tail)) = sub_box;
        info(nucleus(tail)) = p;
      }
      break;

    case math_choice_group:
      build_choices();
      break;

    case math_group:
      {
        unsave();
        decr(save_ptr);
        math_type(saved(0)) = sub_mlist;
        p = fin_mlist(0);
        info(saved(0)) = p;

        if (p != 0)
          if (link(p) == 0)
            if (type(p) == ord_noad)
            {
              if (math_type(subscr(p)) == 0)
              {
                if ((math_type(supscr(p)) == 0) && (math_kcode(p) == null))
                {
                  mem[saved(0)].hh = mem[nucleus(p)].hh;
                  free_node(p, noad_size);
                }
              }
            }
            else if (type(p) == accent_noad)
            {
              if (saved(0) == nucleus(tail))
              {
                if (type(tail) == ord_noad)
                {
                  q = head;

                  while (link(q) != tail)
                    q = link(q);

                  link(q) = p;
                  free_node(tail, noad_size);
                  tail = p;
                }
              }
            }
      }
      break;

    default:
      confusion("rightbrace");
      break;
  }
}

// governs \TeX's activities
void main_control (void) 
{
  integer t;      // {general-purpose temporary variable}
  KANJI_code cx;  // {kanji character}
  pointer kp;     // {kinsoku penalty register}
  pointer gp, gq; // {temporary registers for list manipulation}
  scaled disp;    // {displacement register}
  boolean ins_kp; // {whether insert kinsoku penalty}

  boolean bSuppress;

  if (every_job != 0)
    begin_token_list(every_job, every_job_text);

big_switch:
  get_x_token();

reswitch:
  if (interrupt != 0)
    if (OK_to_interrupt)
    {
      back_input();
      check_interrupt();
      goto big_switch;
    }

#ifdef APTEX_DEBUG
  if (panicking)
    check_mem(false);
#endif

  if (tracing_commands > 0)
    show_cur_cmd_chr();

  ins_kp = false;

  switch (abs(mode) + cur_cmd)
  {
    case hmode + letter:
    case hmode + other_char:
      goto main_loop;
      break;

    case hmode + kanji:
    case hmode + kana:
    case hmode + other_kchar:
    case hmode + hangul:
      goto main_loop_j;
      break;

    case hmode + char_given:
      if (check_echar_range(cur_chr))
        goto main_loop;
      else
      {
        cur_cmd = kcat_code(kcatcodekey(cur_chr));
        goto main_loop_j;
      }
      break;

    case hmode + kchar_given:
      {
        cur_cmd = kcat_code(kcatcodekey(cur_chr));
        goto main_loop_j;
      }
      break;

    case hmode + char_num:
      {
        scan_char_num();
        cur_chr = cur_val;

        if (check_echar_range(cur_chr))
          goto main_loop;
        else
        {
          cur_cmd = kcat_code(kcatcodekey(cur_chr));
          goto main_loop_j;
        }
      }
      break;

    case hmode + kchar_num:
      {
        scan_char_num();
        cur_chr = cur_val;
        cur_cmd = kcat_code(kcatcodekey(cur_chr));
        goto main_loop_j;
      }
      break;

    case hmode + no_boundary:
      {
        get_x_token();

        if ((cur_cmd == letter) || (cur_cmd == other_char) ||
            ((cur_cmd >= kanji) && (cur_cmd <= hangul)) ||
            (cur_cmd == char_given) || (cur_cmd == char_num) ||
            (cur_cmd == kchar_given) || (cur_cmd == kchar_num))
          cancel_boundary = true;

        goto reswitch;
      }
      break;

    case hmode + spacer:
      if (space_factor == 1000)
        goto append_normal_space;
      else
        app_space();
      break;

    case hmode + ex_space:
    case mmode + ex_space:
      goto append_normal_space;
      break;

    case any_mode(relax):
    case vmode + spacer:
    case mmode + spacer:
    case mmode + no_boundary:
      do_nothing();
      break;

    case any_mode(ignore_spaces):
      if (cur_chr == 0)
      {
        do {
          get_x_token();
        } while (!(cur_cmd != spacer));

        goto reswitch;
      }
      else
      {
        t = scanner_status;
        scanner_status = normal;
        get_next();
        scanner_status = t;

        if (cur_cs < hash_base)
          cur_cs = prim_lookup(cur_cs - single_base);
        else
          cur_cs = prim_lookup(text(cur_cs));

        if (cur_cs != undefined_primitive)
        {
          cur_cmd = prim_eq_type(cur_cs);
          cur_chr = prim_equiv(cur_cs);
          cur_tok = cs_token_flag + prim_eqtb_base + cur_cs;
          goto reswitch;
        }
      }
      break;

    case vmode + stop:
      if (its_all_over())
        return;
      break;

    case vmode + vmove:
    case hmode + hmove:
    case mmode + hmove:
    case any_mode(last_item):
    case vmode + vadjust:
    case vmode + ital_corr:
    case non_math(eq_no):
    case any_mode(mac_param):
      report_illegal_case();
      break;

    case non_math(sup_mark):
    case non_math(sub_mark):
    case non_math(math_char_num):
    case non_math(math_given):
    case non_math(math_comp):
    case non_math(delim_num):
    case non_math(left_right):
    case non_math(above):
    case non_math(radical):
    case non_math(math_style):
    case non_math(math_choice):
    case non_math(vcenter):
    case non_math(non_script):
    case non_math(mkern):
    case non_math(limit_switch):
    case non_math(mskip):
    case non_math(math_accent):
    case mmode + endv:
    case mmode + par_end:
    case mmode + stop:
    case mmode + vskip:
    case mmode + un_vbox:
    case mmode + valign:
    case mmode + hrule:
      insert_dollar_sign();
      break;

    case vmode + hrule:
    case hmode + vrule:
    case mmode + vrule:
      {
        tail_append(scan_rule_spec());
        inhibit_glue_flag = false;

        if (abs(mode) == vmode)
          prev_depth = ignore_depth;
        else if (abs(mode) == hmode)
          space_factor = 1000;
      }
      break;

    case vmode + vskip:
    case hmode + hskip:
    case mmode + hskip:
    case mmode + mskip:
      append_glue();
      break;

    case any_mode(kern):
    case mmode + mkern:
      append_kern();
      break;

    case non_math(left_brace):
      new_save_level(simple_group);
      break;

    case any_mode(begin_group):
      new_save_level(semi_simple_group);
      break;

    case any_mode(end_group):
      if (cur_group == semi_simple_group)
        unsave();
      else
        off_save();
      break;

    case any_mode(right_brace):
      handle_right_brace();
      break;

    case vmode + hmove:
    case hmode + vmove:
    case mmode + vmove:
      {
        t = cur_chr;
        scan_normal_dimen();

        if (t == 0)
          scan_box(cur_val);
        else
          scan_box(-cur_val);
      }
      break;

    case any_mode(leader_ship):
      scan_box(leader_flag - a_leaders + cur_chr);
      break;

    case any_mode(make_box):
      begin_box(0);
      break;

    case any_mode(chg_dir):
      {
        if (cur_group != align_group)
        {
          if (mode == hmode)
          {
            print_err("Improper `");
            print_cmd_chr(cur_cmd, cur_chr);
            prints("'");
            help2("You cannot change the direction in unrestricted",
              "horizontal mode.");
            error();
          }
          else if (abs(mode) == mmode)
          {
            print_err("Improper `");
            print_cmd_chr(cur_cmd, cur_chr);
            prints("'");
            help1("You cannot change the direction in math mode.");
            error();
          }
          else if (nest_ptr == 0)
            change_page_direction(cur_chr);
          else if (head == tail)
            direction = cur_chr;
          else
          {
            print_err("Use `");
            print_cmd_chr(cur_cmd, cur_chr);
            prints("' at top of list");
            help2("Direction change command is available only while",
              "current list is null.");
            error();
          }
        }
        else
        {
          print_err("You can't use `");
          print_cmd_chr(cur_cmd, cur_chr);
          prints("' in an align");
          help2("To change direction in an align,",
            "you shold use \\hbox or \\vbox with \\tate or \\yoko.");
          error();
        }
      }
      break;

    case vmode + start_par:
      new_graf(cur_chr > 0);
      break;

    case vmode + letter:
    case vmode + other_char:
    case vmode + char_num:
    case vmode + char_given:
    case vmode + kchar_num:
    case vmode + kchar_given:
    case vmode + math_shift:
    case vmode + un_hbox:
    case vmode + vrule:
    case vmode + accent:
    case vmode + discretionary:
    case vmode + hskip:
    case vmode + valign:
    case vmode + kanji:
    case vmode + kana:
    case vmode + other_kchar:
    case vmode + hangul:
    case vmode + ex_space:
    case vmode + no_boundary:
      {
        back_input();
        new_graf(true);
      }
      break;

    case hmode + start_par:
    case mmode + start_par:
      if (cur_chr != 2)
        indent_in_hmode();
      break;

    case vmode + par_end:
      {
        normal_paragraph();

        if (mode > 0)
          build_page();
      }
      break;

    case hmode + par_end:
      {
        if (align_state < 0)
          off_save();

        end_graf();

        if (mode == vmode)
          build_page();
      }
      break;

    case hmode + stop:
    case hmode + vskip:
    case hmode + hrule:
    case hmode + un_vbox:
    case hmode + halign:
      head_for_vmode();
      break;

    case any_mode(insert):
    case hmode + vadjust:
    case mmode + vadjust:
      begin_insert_or_adjust();
      break;

    case any_mode(mark):
      make_mark();
      break;

    case any_mode(break_penalty):
      append_penalty();
      break;

    case any_mode(remove_item):
      delete_last();
      break;

    case vmode + un_vbox:
    case hmode + un_hbox:
    case mmode + un_hbox:
      unpackage();
      break;

    case hmode + ital_corr:
      append_italic_correction();
      break;

    case mmode + ital_corr:
      tail_append(new_kern(0));
      break;

    case hmode + discretionary:
    case mmode + discretionary:
      append_discretionary();
      break;

    case hmode + accent:
      make_accent();
      break;

    case any_mode(car_ret):
    case any_mode(tab_mark):
      align_error();
      break;

    case any_mode(no_align):
      no_align_error();
      break;

    case any_mode(omit):
      omit_error();
      break;

    case vmode + halign:
      init_align();
      break;

    case hmode + valign:
      if (cur_chr > 0)
      {
        if (eTeX_enabled(TeXXeT_en, cur_cmd, cur_chr))
          tail_append(new_math(0, cur_chr));
      }
      else
        init_align();
      break;

    case mmode + halign:
      if (privileged())
      {
        if (cur_group == math_shift_group)
          init_align();
        else
          off_save();
      }
      break;

    case vmode + endv:
    case hmode + endv:
      if ((partoken_context > 1) && (mode == hmode))
      {
        back_input();
        cur_tok = par_token;
        back_input();
        token_type = inserted;
      }
      else
        do_endv();
      break;

    case any_mode(end_cs_name):
      cs_error();
      break;

    case hmode + math_shift:
      init_math();
      break;

    case mmode + eq_no:
      if (privileged())
      {
        if (cur_group == math_shift_group)
          start_eq_no();
        else
          off_save();
      }
      break;

    case mmode + left_brace:
      {
        tail_append(new_noad());
        back_input();
        scan_math(nucleus(tail), kcode_noad(tail));
      }
      break;

    case mmode + letter:
    case mmode + other_char:
    case mmode + char_given:
      if (check_echar_range(cur_chr))
      {
        if (cur_chr < 128)
          set_math_char(math_code(cur_chr));
        else
          set_math_char(cur_chr);
      }
      else
        set_math_kchar(cur_chr);
      break;

    case mmode + kanji:
    case mmode + kana:
    case mmode + other_kchar:
    case mmode + hangul:
      {
        cx = cur_chr;
        set_math_kchar(KANJI(cx));
      }
      break;

    case mmode + char_num:
      {
        scan_char_num();
        cur_chr = cur_val;

        if (check_echar_range(cur_chr))
        {
          if (cur_chr < 128)
            set_math_char(math_code(cur_chr));
          else
            set_math_char(cur_chr);
        }
        else
          set_math_kchar(cur_chr);
      }
      break;

      case mmode + kchar_given:
        set_math_kchar(cur_chr);
        break;

      case mmode + kchar_num: 
        {
          scan_char_num();
          cur_chr = cur_val;
          set_math_kchar(cur_chr);
        }
        break;

    case mmode + math_char_num:
      {
        scan_fifteen_bit_int();
        set_math_char(cur_val);
      }
      break;

    case mmode + math_given:
      set_math_char(cur_chr);
      break;

    case mmode + delim_num:
      {
        scan_twenty_seven_bit_int();
        set_math_char(cur_val / 4096);
      }
      break;

    case mmode + math_comp:
      {
        tail_append(new_noad());
        type(tail) = cur_chr;
        scan_math(nucleus(tail), kcode_noad(tail));
      }
      break;

    case mmode + limit_switch:
      math_limit_switch();
      break;

    case mmode + radical:
      math_radical();
      break;

    case mmode + accent:
    case mmode + math_accent:
      math_ac();
      break;

    case mmode + vcenter:
      {
        scan_spec(vcenter_group, false);
        normal_paragraph();
        push_nest();
        mode = -vmode;
        prev_depth = ignore_depth;

        if (every_vbox != 0)
          begin_token_list(every_vbox, every_vbox_text);
      }
      break;

    case mmode + math_style:
      tail_append(new_style(cur_chr));
      break;

    case mmode + non_script:
      {
        tail_append(new_glue(zero_glue));
        subtype(tail) = cond_math_glue;
      }
      break;

    case mmode + math_choice:
      append_choices();
      break;

    case mmode + sub_mark:
    case mmode + sup_mark:
      sub_sup();
      break;

    case mmode + above:
      math_fraction();
      break;

    case mmode + left_right:
      math_left_right();
      break;

    case mmode + math_shift:
      if (cur_group == math_shift_group)
        after_math();
      else
        off_save();
      break;

    case any_mode(assign_kinsoku):
    case any_mode(assign_inhibit_xsp_code):
    case any_mode(set_auto_spacing):
    case any_mode(set_enable_cjk_token):
    case any_mode(set_kansuji_char):
    case any_mode(toks_register):
    case any_mode(assign_toks):
    case any_mode(assign_int):
    case any_mode(def_jfont):
    case any_mode(def_tfont):
    case any_mode(assign_dimen):
    case any_mode(assign_glue):
    case any_mode(assign_mu_glue):
    case any_mode(assign_font_dimen):
    case any_mode(assign_font_int):
    case any_mode(set_aux):
    case any_mode(set_prev_graf):
    case any_mode(set_page_dimen):
    case any_mode(set_page_int):
    case any_mode(set_box_dimen):
    case any_mode(set_shape):
    case any_mode(def_code):
    case any_mode(def_family):
    case any_mode(set_font):
    case any_mode(def_font):
    case any_mode(tex_register):
    case any_mode(advance):
    case any_mode(multiply):
    case any_mode(divide):
    case any_mode(prefix):
    case any_mode(let):
    case any_mode(shorthand_def):
    case any_mode(read_to_cs):
    case any_mode(def):
    case any_mode(set_box):
    case any_mode(hyph_data):
    case any_mode(set_interaction):
      prefixed_command();
      break;

    case any_mode(after_assignment):
      {
        get_token();
        after_token = cur_tok;
      }
      break;

    case any_mode(after_group):
      {
        get_token();
        save_for_after(cur_tok);
      }
      break;

    case any_mode(partoken_name):
      {
        get_token();
        if (cur_cs > 0)
        {
          par_loc = cur_cs;
          par_token = cur_tok;
        }
      }
      break;

    case any_mode(in_stream):
      open_or_close_in();
      break;

    case any_mode(message):
      issue_message();
      break;

    case any_mode(case_shift):
      shift_case();
      break;

    case any_mode(xray):
      show_whatever();
      break;

    case any_mode(inhibit_glue):
      inhibit_glue_flag = (cur_chr == 0);
      break;

    case any_mode(extension):
      do_extension();
      break;
  }

  goto big_switch;

main_loop_j:
  append_kanji_to_hlist();

main_loop:
  inhibit_glue_flag = false;
  adjust_space_factor();

  if (direction == dir_tate)
    disp = t_baseline_shift;
  else
    disp = y_baseline_shift;

  append_disp_node_at_begin();
  main_f = cur_font;
  bchar = font_bchar[main_f];
  false_bchar = font_false_bchar[main_f];

  if (mode > 0)
  {
    if (language != clang)
      fix_language();
  }

  fast_get_avail(lig_stack);
  font(lig_stack) = main_f;
  cur_l = cur_chr;
  character(lig_stack) = cur_l;
  cur_q = tail;

  if (cancel_boundary)
  {
    cancel_boundary = false;
    main_k = non_address;
  }
  else
    main_k = bchar_label[main_f];

  if (main_k == non_address)
    goto main_loop_move_2;

  cur_r = cur_l;
  cur_l = non_char;
  goto main_lig_loop_1;

main_loop_wrapup: 
  wrapup(rt_hit);

main_loop_move:
  if (lig_stack == 0)
  {
    append_disp_node_at_end();
    goto reswitch;
  }

  cur_q = tail;
  cur_l = character(lig_stack);

main_loop_move_1:
  if (!is_char_node(lig_stack))
    goto main_loop_move_lig;

main_loop_move_2:
  if ((cur_chr < font_bc[main_f]) || (cur_chr > font_ec[main_f]))
  {
    char_warning(main_f, cur_chr);
    free_avail(lig_stack);
    goto big_switch;
  }

  main_i = char_info(main_f, cur_l);

  if (!char_exists(main_i))
  {
    char_warning(main_f, cur_chr);
    free_avail(lig_stack);
    goto big_switch; 
  }

  link(tail) = lig_stack;
  tail = lig_stack;

main_loop_lookahead:
  get_next();

  if (cur_cmd == letter)
    goto main_loop_lookahead_1;

  if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
  {
    goto_main_lig_loop();
  }

  if (cur_cmd == other_char)
    goto main_loop_lookahead_1;

  if (cur_cmd == char_given)
  {
    if (check_echar_range(cur_chr))
      goto main_loop_lookahead_1;
    else
    {
      cur_cmd = kcat_code(kcatcodekey(cur_chr));
      goto_main_lig_loop();
    }
  }

  if (cur_cmd == kchar_given)
  {
    cur_cmd = kcat_code(kcatcodekey(cur_chr));
    goto_main_lig_loop();
  }

  x_token();

  if (cur_cmd == letter)
    goto main_loop_lookahead_1;

  if ((cur_cmd >= kanji) && (cur_cmd <= hangul))
  {
    goto_main_lig_loop();
  }

  if (cur_cmd == other_char)
    goto main_loop_lookahead_1;

  if (cur_cmd == char_given)
  {
    if (check_echar_range(cur_chr))
      goto main_loop_lookahead_1;
    else
    {
      cur_cmd = kcat_code(kcatcodekey(cur_chr));
      goto_main_lig_loop();
    }
  }

  if (cur_cmd == kchar_given)
  {
    cur_cmd = kcat_code(kcatcodekey(cur_chr));
    goto_main_lig_loop();
  }

  if (cur_cmd == char_num)
  {
    scan_char_num();
    cur_chr = cur_val;

    if (check_echar_range(cur_chr))
      goto main_loop_lookahead_1;
    else
    {
      cur_cmd = kcat_code(kcatcodekey(cur_chr));
      goto_main_lig_loop();
    }
  }

  if (cur_cmd == kchar_num)
  {
    scan_char_num();
    cur_chr = cur_val;
    cur_cmd = kcat_code(kcatcodekey(cur_chr));
    goto_main_lig_loop();
  }

  if (cur_cmd == inhibit_glue)
  {
    inhibit_glue_flag = true;
    goto main_loop_lookahead;
  }

  if (cur_cmd == no_boundary)
    bchar = non_char;

  cur_r = bchar;
  lig_stack = 0;
  goto main_lig_loop;

main_loop_lookahead_1:
  adjust_space_factor();
  inhibit_glue_flag = false;
  fast_get_avail(lig_stack);
  font(lig_stack) = main_f;
  cur_r = cur_chr;
  character(lig_stack) = cur_r;

  if (cur_r == false_bchar)
    cur_r = non_char;

main_lig_loop:
  if (char_tag(main_i) != lig_tag)
    goto main_loop_wrapup;

  if (cur_r == non_char)
    goto main_loop_wrapup;

  main_k = lig_kern_start(main_f, main_i);
  main_j = font_info[main_k].qqqq;

  if (skip_byte(main_j) <= stop_flag)
    goto main_lig_loop_2;

  main_k = lig_kern_restart(main_f, main_j);

main_lig_loop_1:
  main_j = font_info[main_k].qqqq;

main_lig_loop_2:
  bSuppress = false;

  if (aptex_env.flag_suppress_f_ligs && next_char(main_j) == cur_r && op_byte(main_j) == no_tag)
  {
    if (cur_l == 'f')
      bSuppress = true;
  }

  if (next_char(main_j) == cur_r && bSuppress == false)
    if (skip_byte(main_j) <= stop_flag)
    {
      if (op_byte(main_j) >= kern_flag)
      {
        wrapup(rt_hit);
        tail_append(new_kern(char_kern(main_f, main_j)));
        goto main_loop_move;
      }

      if (cur_l == non_char)
        lft_hit = true;
      else if (lig_stack == 0)
        rt_hit = true;

      check_interrupt();

      switch (op_byte(main_j))
      {
        case 1:
        case 5:
          {
            cur_l = rem_byte(main_j);
            main_i = char_info(main_f, cur_l);
            ligature_present = true;
          }
          break;

        case 2:
        case 6:
          {
            cur_r = rem_byte(main_j);

            if (lig_stack == 0)
            {
              lig_stack = new_lig_item(cur_r);
              bchar = non_char;
            }
            else if (is_char_node(lig_stack))
            {
              main_p = lig_stack;
              lig_stack = new_lig_item(cur_r);
              lig_ptr(lig_stack) = main_p;
            }
            else
              character(lig_stack) = cur_r;
          }
          break;

        case 3:
          {
            cur_r = rem_byte(main_j);
            main_p = lig_stack;
            lig_stack = new_lig_item(cur_r);
            link(lig_stack) = main_p;
          }
          break;

        case 7:
        case 11:
          {
            wrapup(false);
            cur_q = tail;
            cur_l = rem_byte(main_j);
            main_i = char_info(main_f, cur_l);
            ligature_present = true;
          }
          break;

        default:
          {
            cur_l = rem_byte(main_j);
            ligature_present = true;
 
            if (lig_stack == 0)
              goto main_loop_wrapup;
            else
              goto main_loop_move_1;
          }
          break;
      }

      if (op_byte(main_j) > 4)
      {
        if (op_byte(main_j) != 7)
          goto main_loop_wrapup;
      }

      if (cur_l < non_char)
        goto main_lig_loop;

      main_k = bchar_label[main_f];
      goto main_lig_loop_1;
    }

  if (skip_byte(main_j) == 0)
    incr(main_k);
  else
  {
    if (skip_byte(main_j) >= stop_flag)
      goto main_loop_wrapup;

    main_k = main_k + skip_byte(main_j) + 1;
  }

  goto main_lig_loop_1;

main_loop_move_lig:
  main_p = lig_ptr(lig_stack);

  if (main_p != 0)
    tail_append(main_p);

  temp_ptr = lig_stack;
  lig_stack = link(temp_ptr);
  free_node(temp_ptr, small_node_size);
  main_i = char_info(main_f, cur_l);
  ligature_present = true;

  if (lig_stack == 0)
  {
    if (main_p != 0)
      goto main_loop_lookahead;
    else
      cur_r = bchar;
  }
  else
    cur_r = character(lig_stack);

  goto main_lig_loop;

append_normal_space:
  if (space_skip == zero_glue)
  {
    {
      main_p = font_glue[cur_font];

      if (main_p == 0)
      {
        main_p = new_spec(zero_glue);
        main_k = param_base[cur_font] + space_code;
        width(main_p) = font_info[main_k].cint;
        stretch(main_p) = font_info[main_k + 1].cint;
        shrink(main_p) = font_info[main_k + 2].cint;
        font_glue[cur_font] = main_p;
      }
    }

    temp_ptr = new_glue(main_p);
  }
  else
    temp_ptr = new_param_glue(space_skip_code);

  if (!is_char_node(tail) && (type(tail) == disp_node))
  {
    link(prev_node) = temp_ptr;
    link(temp_ptr) = tail;
    prev_node = temp_ptr;
  }
  else
  {
    link(tail) = temp_ptr;
    tail = temp_ptr;
  }

  goto big_switch;
}

void give_err_help (void)
{
  token_show(err_help);
}

boolean open_fmt_file (void)
{
  uint32_t j;

  j = loc;

  if (buffer[loc] == '&' || buffer[loc] == '+')
  {
    incr(loc);
    j = loc;
    buffer[last] = ' ';

    while (buffer[j] != ' ')
      incr(j);

    pack_buffered_name(0, loc, j - 1);

    if (w_open_in(fmt_file))
      goto found;
  
    if (aptex_env.flag_tex82)
    {
      wake_up_terminal();
      printf("Sorry, I can't find that format; will try the default.\n");
    }
    else
    {
      wake_up_terminal();
      name_of_file[name_length + 1] = '\0';
      printf("Sorry, I can't find that format (%s); will try the default.\n", name_of_file + 1);
      name_of_file[name_length + 1] = ' ';
      printf("(Perhaps your Asiatic pTeX's environment variable is not set correctly)\n");
    }

    update_terminal();
  }

  pack_buffered_name(format_default_length - 4, 1, 0);

  if (!w_open_in(fmt_file))
  {
    if (aptex_env.flag_tex82)
    {
      wake_up_terminal();
      printf("%s!\n", "I can't find the default format file");
    }
    else
    {
      wake_up_terminal();
      name_of_file[name_length + 1] = '\0';
      printf("I can't find the default format file (%s)!\n", name_of_file + 1);
      name_of_file[name_length + 1] = ' ';
      printf("(Perhaps your Asiatic pTeX's environment variable is not set correctly)\n");
    }

    return false;
  }

found:
  loc = j;

  return true;
}

#ifdef APTEX_EXTENSION
  #define ng_stat(a) (int)(aptex_env.trace_realloc ? current_##a : a)
#else
  #define ng_stat(a) (int)(a)
#endif

#define mem_size (mem_end + 1 - mem_min)

void close_files_and_terminate (void)
{
  integer k;  // {all-purpose index}

  // @<Finish the extensions@>
  for (k = 0; k <= 15; k++)
  {
    if (write_open[k])
      a_close(write_file[k]);
  }

  new_line_char = -1;

#ifdef STAT
  // @<Output statistics about this job@>
  if (tracing_stats > 0)
  {
    if (log_opened)
    {
      wlog_ln(" ");
      wlog_ln("Here is how much of TeX's memory you used:");
      write_log(" %d string", (int)(str_ptr - init_str_ptr));

      if (str_ptr != init_str_ptr + 1)
        wlog('s');

      wlog_ln(" out of %d", (int)(ng_stat(max_strings) - init_str_ptr));
      wlog_ln(" %d string characters out of %d", (int)(pool_ptr - init_pool_ptr), (int)(ng_stat(pool_size) - init_pool_ptr));
      wlog_ln(" %d words of memory out of %d", (int)(lo_mem_max - mem_min + mem_end - hi_mem_min + 2), ng_stat(mem_size));
      wlog_ln(" %d multiletter control sequences out of %d", (int)(cs_count), (int)(hash_size));
      write_log(" %d words of font info for %d font", (int)(fmem_ptr), (int)(font_ptr - font_base));

      if (font_ptr != 1)
        wlog('s');

      wlog_ln(", out of %d for %d", ng_stat(font_mem_size), (int)(font_max - font_base));
      write_log(" %"PRId64" hyphenation exception", hyph_count);

      if (hyph_count != 1)
        wlog('s');

      wlog_ln(" out of %"PRId64"", hyphen_prime);
      write_log(" %"PRId64"i,", max_in_stack);
      write_log("%"PRId64"n,", max_nest_stack);
      write_log("%"PRId64"p,", max_param_stack);
      write_log("%"PRId64"b,", max_buf_stack + 1);
      write_log("%"PRId64"s", max_save_stack + 6);
      write_log(" stack positions out of ");
      write_log("%di,", ng_stat(stack_size));
      write_log("%dn,", ng_stat(nest_size));
      write_log("%dp,", ng_stat(param_size));
      write_log("%db,", ng_stat(buf_size));
      write_log("%ds", ng_stat(save_size));
      write_log("\n");

      if (!aptex_env.flag_tex82)
      {
        write_log(" (i = in_stack, n = nest_stack, p = param_stack, b = buf_stack, s = save_stack)\n");
        write_log(" %d inputs open max out of %d\n", (int) high_in_open, (int) max_in_open);
      }

      if (aptex_env.trace_lbrk && (lbs_pass_fst > 0))
      {
        int first_count, second_count, third_count;

        write_log("\nSuccess at breaking %d paragraph%s:", lbs_pass_fst, (lbs_pass_fst == 1) ? "" : "s");

        if (lbs_sing_line > 0)
          write_log("\n %d single line `paragraph%s'", lbs_sing_line, (lbs_sing_line == 1) ? "" : "s");

        first_count = lbs_pass_fst - lbs_sing_line - lbs_pass_snd;

        if (first_count < 0)
          first_count = 0;

        second_count = lbs_pass_snd - lbs_pass_fin;
        third_count = lbs_pass_fin;

        if (lbs_pass_fst > 0)
          write_log("\n %d first pass (\\pretolerance = %d)", lbs_pass_fst, (int) pretolerance);

        if (lbs_pass_snd > 0)
          write_log("\n %d second pass (\\tolerance = %d)", lbs_pass_snd, (int) tolerance);

        if (lbs_pass_fin > 0 || emergency_stretch > 0)
          write_log("\n %d third pass (\\emergencystretch = %lgpt)",
            lbs_pass_fin, (double) emergency_stretch / 65536.0);

        wlog_cr();

        if (hps_overfull > 0)
          write_log("\n %d overfull \\hbox%s", hps_overfull, (hps_overfull > 1) ? "es" : "");

        if (hps_underfull > 0)
          write_log("\n %d underfull \\hbox%s", hps_underfull, (hps_underfull > 1) ? "es" : "");

        if (vps_overfull > 0)
          write_log("\n %d overfull \\vbox%s", vps_overfull, (vps_overfull > 1) ? "es" : "");

        if (vps_underfull > 0)
          write_log("\n %d underfull \\vbox%s", vps_underfull, (vps_underfull > 1) ? "es" : "");

        if (hps_overfull || hps_underfull || vps_overfull || vps_underfull)
          wlog_cr();
      }
    }
  }
#endif
  
  wake_up_terminal();

  // @<Finish the \.{DVI} file@>
  {
    while (cur_s > -1)
    {
      if (cur_s > 0)
        dvi_out(pop);
      else
      {
        dvi_out(eop);
        incr(total_pages);
      }

      decr(cur_s);
    }

    if (total_pages == 0)
      print_nl("No pages of output.");
    else
    {
      dvi_out(post);  // {beginning of the postamble}
      dvi_four(last_bop);
      last_bop = dvi_offset + dvi_ptr - 5;  // {|post| location}
      dvi_four(25400000);
      dvi_four(473628672);  // {conversion ratio for sp}
      prepare_mag();
      dvi_four(mag);  // {magnification factor}
      dvi_four(max_v);
      dvi_four(max_h);
      dvi_out(max_push / 256);
      dvi_out(max_push % 256);
      dvi_out((total_pages / 256) % 256);
      dvi_out(total_pages % 256);

      // @<Output the font definitions for all fonts that were used@>
      while (font_ptr > font_base)
      {
        if (font_used[font_ptr])
          dvi_font_def(font_ptr);

        decr(font_ptr);
      }

      dvi_out(post_post);
      dvi_four(last_bop);

      if (dir_used)
        dvi_out(ex_id_byte);
      else
        dvi_out(id_byte);

      k = 4 + ((dvi_buf_size - dvi_ptr) % 4); // {the number of 223's}

      while (k > 0)
      {
        dvi_out(223);
        decr(k);
      }

      // @<Empty the last bytes out of |dvi_buf|@>
      if (dvi_limit == half_buf)
        write_dvi(half_buf, dvi_buf_size - 1);

      if (dvi_ptr > 0)
        write_dvi(0, dvi_ptr - 1);

      print_nl("Output written on ");
      slow_print(output_file_name);
      prints(" (");
      print_int(total_pages);
      prints(" page");

      if (total_pages != 1)
        print_char('s');

      prints(", ");
      print_int(dvi_offset + dvi_ptr);
      prints(" bytes).");
      b_close(dvi_file);

#ifndef APTEX_DVI_ONLY
      {
        spc_exec_at_end_document();
        pdf_close_document();
        pdf_close_fontmaps();

        print_nl("Output written on ");
        prints(output_pdf_name);
        prints(" (");
        print_int(total_pages);
        prints(" page");
        free(output_pdf_name);
        free(output_dvi_name);

        if (total_pages != 1)
          print_char('s');

        prints(", ");
        print_int(pdf_output_stats());
        prints(" bytes).");
      }
#endif
    }
  }

  // @<Close {\sl Sync\TeX} file and write status@>
  synctex_terminate();

  if (log_opened)
  {
    wlog_cr();
    a_close(log_file);
    selector = selector - 2;

    if (selector == term_only)
    {
      print_nl("Transcript written on ");
      slow_print(log_name);
      print_char('.');
    }
  }

  print_ln();
}


#ifdef APTEX_DEBUG

#ifndef unix
  #define dumpcore() exit(EXIT_FAILURE)
#else
  #define dumpcore abort
#endif

/* sec 1338 */
void debug_help (void) 
{
  integer k, l, m, n;

  clear_terminal();

  while (true)
  {
    wake_up_terminal();
    print_nl(" debug # (-1 to exit):");
    update_terminal();
    scanf("%"PRId64, &m); // read(stdin, m);

    if (m < 0)
      return;
    else if (m == 0)
      dumpcore();
    else
    {
      scanf("%"PRId64, &n); // read(stdin, n);

      switch (m)
      {
        case 1:
          print_word(mem[n]);
          break;

        case 2:
          print_int(info(n));
          break;
          
        case 3:
          print_int(link(n));
          break;
        
        case 4:
          print_word(eqtb[n]);
          break;

        case 5:
          print_word(font_info[n]);
          break;
        
        case 6:
          print_word(save_stack[n]);
          break;
          
        case 7:
          show_box(n);
          break;
        
        case 8:
          {
            breadth_max = 10000;
#ifdef APTEX_EXTENSION
            if (pool_ptr + 32000 > current_pool_size)
              str_pool = realloc_str_pool (increment_pool_size);

            depth_threshold = current_pool_size - pool_ptr - 10;
#else
            depth_threshold = pool_size - pool_ptr - 10;
#endif
            show_node_list(n);
          }
          break;
        
        case 9:
          show_token_list(n, 0, 1000);
          break;
        
        case 10:
          slow_print(n);
          break;
        
        case 11:
          //check_mem(n > 0);
          break;
        
        case 12:
          //search_mem(n);
          break;
        
        case 13:
          {
            scanf("%"PRId64, &l); //read(stdin, l);
            print_cmd_chr(n, l);
          }
          break;
        
        case 14:
          {
            for (k = 0; k <= n; k++)
              print(buffer[k]);
          }
          break;
        
        case 15:
          {
            font_in_short_display = 0;
            short_display(n);
          }
          break;
        
        case 16:
          panicking = !panicking;
          break;
        
        default:
          print('?');
          break;
      }
    }
  }
}
#endif

pointer new_dir_node (pointer b, eight_bits dir)
{
  pointer p;  // {the new node}

  if (type(b) > vlist_node)
    confusion("new_dir_node:not box");

  p = new_null_box();
  type(p) = dir_node;
  set_box_dir(p, dir);

  switch (abs(box_dir(b)))
  {
    case dir_yoko:
      // @<Yoko to other direction@>
      switch (dir)
      {
        case dir_tate:
          {
            width(p) = height(b) + depth(b);
            depth(p) = width(b) / 2;
            height(p) = width(b) - depth(p);
          }
          break;
 
        case dir_dtou:
          {
            width(p) = height(b) + depth(b);
            depth(p) = 0;
            height(p) = width(b);
          }
          break;
 
        default:
          confusion("new_dir_node:y->?");
          break;
      }
      break;

    case dir_tate:
      // @<Tate to other direction@>
      switch (dir)
      {
        case dir_yoko:
          {
            width(p) = height(b) + depth(b);
            depth(p) = 0;
            height(p) = width(b);
          }
          break;

        case dir_dtou:
          {
            width(p) = width(b);
            depth(p) = height(b);
            height(p) = depth(b);
          }
          break;

        default:
          confusion("new_dir_node:t->?");
          break;
      }
      break;

    case dir_dtou:
      // @<DtoU to other direction@>
      switch (dir)
      {
        case dir_yoko:
          {
            width(p) = height(b) + depth(b);
            depth(p) = 0;
            height(p) = width(b);
          }
          break;

        case dir_tate:
          {
            width(p) = width(b);
            depth(p) = height(b);
            height(p) = depth(b);
          }
          break;

        default:
          confusion("new_dir_node:d->?");
          break;
      }
      break;

    default:
      confusion("new_dir_node:illegal dir");
      break;
  }

  link(b) = null;
  list_ptr(p) = b;

  return p;
}

eight_bits get_jfm_pos (KANJI_code kcode, internal_font_number f)
{
  KANJI_code jc;  // {temporary register for KANJI}
  pointer sp, mp, ep;

  if (f == null_font)
  {
    return kchar_type(null_font, 0);
  }

  jc = toDVI(kcode);
  sp = 1; // { start position }
  ep = font_num_ext[f] - 1; // { end position }

  if ((ep >= 1)) if (font_enc[f] == 0)
  {
    if ((kchar_code(f, sp) <= jc) && (jc <= kchar_code(f, ep)))
    {
      while (sp <= ep)
      {
        mp = sp + ((ep - sp) / 2);

        if (jc < kchar_code(f, mp))
          ep = mp - 1;
        else if (jc > kchar_code(f, mp))
          sp = mp + 1;
        else
        {
          return kchar_type(f, mp);
        }
      }
    }
  }
  else
  {
    while (sp <= ep)
    {
      if (jc == kchar_code(f, sp))
        return kchar_type(f, sp);
      else
        incr(sp);
    }
  }

  return kchar_type(f, 0);
}

pointer get_inhibit_pos (KANJI_code c, small_number n)
{
  pointer p, pp, s;

  s = calc_pos(c);
  p = s;
  pp = no_entry;

  if (n == new_pos)
  {
    do {
      if (inhibit_xsp_code(p) == c)
        goto done; // { found, update there }

      if (inhibit_xsp_code(p) == 0) // { no further scan needed }
      {
        if (pp != no_entry)
          p = pp;
        goto done;
      }
      
      if (inhibit_xsp_type(p) == inhibit_unused)
      {
        if (pp == no_entry)
          pp = p; // { save the nearest unused hash }
      }

      incr(p);
      
      if (p > 1023)
        p = 0;
    } while (!(s == p));

    p = pp;
  }
  else
  {
    do {
      if (inhibit_xsp_code(p) == 0)
        goto done1;

      if (inhibit_xsp_code(p) == c)
        goto done;

      incr(p);

      if (p > 1023)
        p = 0;
    } while (!(s == p));

done1:
    p = no_entry;
  }

done:
  return p;
}

pointer get_kinsoku_pos (KANJI_code c, small_number n)
{
  pointer p, pp, s;

  s = calc_pos(c);
  p = s;
  pp = no_entry;

#ifdef APTEX_DEBUG
  print_ln();
  prints("c:=");
  print_int(c);
  prints(", p:=");
  print_int(s);

  if (p + kinsoku_base < 0)
  {
    prints("p is negative value");
    print_ln();
  }
#endif

  if (n == new_pos)
  {
    do {
      if (kinsoku_code(p) == c) // { found, update there }
        goto done;

      if (kinsoku_type(p) == 0) // { no further scan needed }
      {
        if (pp != no_entry)
          p = pp;
        goto done;
      }

      if (kinsoku_type(p) == kinsoku_unused_code)
      {
        if (pp == no_entry)
          pp = p; // { save the nearest unused hash }
      }

      incr(p);

      if (p > 1023)
        p = 0;
    } while (!(s == p));

    p = pp;
  }
  else
  {
    do {
      if (kinsoku_type(p) == 0)
        goto done1;

      if (kinsoku_code(p) == c)
        goto done;

      incr(p);

      if (p > 1023)
        p = 0;
    } while (!(s == p));

done1:
    p = no_entry;
  }

done:
  return p;
}

boolean check_box (pointer box_p)
{
  pointer p;
  boolean flag;

  flag = false;
  p = box_p;

  while (p != null)
  {
    if (is_char_node(p))
    {
      do {
        if (find_first_char)
        {
          first_char = p;
          find_first_char = false;
        }

        last_char = p;
        flag = true;

        if (font_dir[font(p)] != dir_default)
          p = link(p);
          
        p = link(p);

        if (p == null)
          goto done;
      } while (!(!is_char_node(p)));
    }

    switch (type(p))
    {
      case hlist_node:
        {
          flag = true;

          if (shift_amount(p) == 0)
          {
            if (check_box(list_ptr(p)))
              flag = true;
          }
          else if (find_first_char)
            find_first_char = false;
          else
            last_char = null;
        }
        break;

      case ligature_node:
        if (check_box(lig_ptr(p)))
          flag = true;
        break;

      case ins_node:
      case disp_node:
      case mark_node:
      case adjust_node:
      case whatsit_node:
      case penalty_node:
        do_nothing();
        break;

      case math_node:
        if ((subtype(p) == before) || (subtype(p) == after))
        {
          if (find_first_char)
          {
            find_first_char = false;
            first_char = p;
          }

          last_char = p;
          flag = true;
        }
        else
          do_nothing();
        break;

      case kern_node:
        if (subtype(p) == acc_kern)
        {
          p = link(p);
          if (is_char_node(p))
            if (font_dir[font(p)] != dir_default)
              p = link(p);
          p = link(link(p));
          if (find_first_char)
          {
            find_first_char = false;
            first_char = p;
          }
          last_char = p;
          flag = true;
          if (font_dir[font(p)] != dir_default)
            p = link(p);
        }
        else
        {
          flag = true;
          if (find_first_char)
            find_first_char = false;
          else
            last_char = null;
        }
        break;

      default:
        {
          flag = true;

          if (find_first_char)
            find_first_char = false;
          else
            last_char = null;
        }
        break;
    }

    p = link(p);
  }

done:
  return flag;
}

void adjust_hlist (pointer p, boolean pf)
{
  pointer q, s, t, u, v, x, z;
  halfword i, k;
  pointer a;
  int insert_skip;
  KANJI_code cx;
  ASCII_code ax;
  boolean do_ins;

  k = 0;

  if (link(p) == null)
    goto exit;

  if (auto_spacing > 0)
  {
    delete_glue_ref(space_ptr(p));
    space_ptr(p) = kanji_skip;
    add_glue_ref(kanji_skip);
  }

  if (auto_xspacing > 0)
  {
    delete_glue_ref(xspace_ptr(p));
    xspace_ptr(p) = xkanji_skip;
    add_glue_ref(xkanji_skip);
  }

  u = space_ptr(p);
  add_glue_ref(u);
  s = xspace_ptr(p);
  add_glue_ref(s);

  if (!is_char_node(link(p)))
    if ((type(link(p)) == glue_node) && (subtype(link(p)) == jfm_skip + 1))
    {
      v = link(p);
      link(p) = link(v);
      fast_delete_glue_ref(glue_ptr(v));
      free_node(v, small_node_size);
    }
    else if ((type(link(p)) == penalty_node) && (subtype(link(p)) == kinsoku_pena))
    {
      v = link(link(p));
      if ((!is_char_node(v)) && (type(v) == glue_node) && (subtype(v) == jfm_skip + 1))
      {
        link(link(p)) = link(v);
        fast_delete_glue_ref(glue_ptr(v));
        free_node(v,small_node_size);
      }
    }

  i = 0;
  insert_skip = no_skip;
  p = link(p);
  v = p;
  q = p;

  while (p != null)
  {
    if (is_char_node(p))
    {
      do {
        insert_space_around_char();
        q = p;
        p = link(p);
        incr(i);

        if ((i > 5) && pf)
        {
          if (is_char_node(v))
            if (font_dir[font(v)] != dir_default)
              v = link(v);

          v = link(v);
        }
      } while (!(!is_char_node(p)));
    }
    else
    {
      switch (type(p))
      {
        case hlist_node:
          insert_hbox_surround_spacing();
          break;

        case ligature_node:
          insert_ligature_surround_spacing();
          break;

        case penalty_node:
        case disp_node:
          insert_penalty_or_displace_surround_spacing();
          break;

        case kern_node:
          if (subtype(p) == explicit)
            insert_skip = no_skip;
          else if (subtype(p) == acc_kern)
          {
            if (q == p)
            {
              t = link(p);

              if (is_char_node(t))
                if (font_dir[font(t)] != dir_default)
                  t = link(t);

              p = link(link(t));

              if (font_dir[font(p)] != dir_default)
              {
                p = link(p);
                insert_skip = after_wchar;
              }
              else
                insert_skip = after_schar;
            }
            else
            {
              a = p;
              t = link(p);

              if (is_char_node(t))
                if (font_dir[font(t)] != dir_default)
                  t = link(t);

              t = link(link(t));
              link(q) = t;
              p = t;
              insert_space_around_char();
              incr(i);

              if ((i > 5) && pf)
              {
                if (is_char_node(v))
                  if (font_dir[font(v)] != dir_default)
                    v = link(v);

                v = link(v);
              }

              if (link(q) != t)
                link(link(q)) = a;
              else
                link(q) = a;
            }
          }
          break;

        case math_node:
          insert_math_surround_spacing();
          break;

        case mark_node:
        case adjust_node:
        case ins_node:
        case whatsit_node:
          do_nothing();
          break;

        default:
          insert_skip = no_skip;
          break;
      }

      q = p;
      p = link(p);
    }
  }

  if (!is_char_node(q) && (type(q) == glue_node) &&
    (subtype(q) == jfm_skip + 1))
  {
    fast_delete_glue_ref(glue_ptr(q));
    glue_ptr(q) = zero_glue;
    add_glue_ref(zero_glue);
  }

  delete_glue_ref(u);
  delete_glue_ref(s);

  if ((v != null) && pf && (i > 5))
    make_jchr_widow_penalty_node();
exit:;
}

void set_math_kchar (integer c)
{
  pointer p;

  p = new_noad();
  math_type(nucleus(p)) = math_jchar;
  character(nucleus(p)) = 0;
  math_kcode(p) = c;
  fam(nucleus(p)) = cur_jfam;

  if (font_dir[fam_fnt(fam(nucleus(p)) + cur_size)] == dir_default)
  {
    print_err("Not two-byte family");
    help1("IGNORE.");
    error();
  }

  type(p) = ord_noad;
  link(tail) = p;
  tail = p;
}

void change_page_direction (halfword d)
{
  pointer p;
  boolean flag;

  flag = (page_contents == empty);
  if (flag && (head != tail))
  {
    p = link(head);
    while (p != null)
    {
      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case dir_node:
        case rule_node:
        case ins_node:
          {
            flag = false;
            goto done;
          }
          break;
        default:
          p = link(p);
          break;
      }
    }
done:
    do_nothing();
  }
  if (flag)
  {
    direction = d;
    page_dir = d;
  }
  else
  {
    print_err("Use `");
    print_cmd_chr(cur_cmd, d);
    prints("' at top of the page");
    help3("You can change the direction of the page only when",
      "the current page and recent contributions consist of only",
      "marks and whatsits.");
    error();
  }
}

boolean check_kcat_code (integer ct)
{
  if (((ct >= kanji) && (enable_cjk_token == 0)) || (enable_cjk_token == 2))
    return true;
  else
    return false;
}

boolean check_echar_range (integer c)
{
  if ((c >= 0) && (c < 256))
    return true;
  else
    return false;
}

// for eTeX
boolean eTeX_enabled (boolean b, quarterword j, halfword k)
{
  if (!b)
  {
    print_err("Improper ");
    print_cmd_chr(j, k);
    help1("Sorry, this optional e-TeX feature has been disabled.");
    error();
  }
  
  return b;
}

static void print_group (boolean e)
{
  switch (cur_group)
  {
    case bottom_level:
      {
        prints("bottom level");
        return;
      }
      break;

    case simple_group:
    case semi_simple_group:
      {
        if (cur_group == semi_simple_group)
          prints("semi ");

        prints("simple");
      }
      break;

    case hbox_group:
    case adjusted_hbox_group:
      {
        if (cur_group == adjusted_hbox_group)
          prints("adjusted ");

        prints("hbox");
      }
      break;

    case vbox_group:
      prints("vbox");
      break;

    case vtop_group:
      prints("vtop");
      break;

    case align_group:
    case no_align_group:
      {
        if (cur_group == no_align_group)
          prints("no ");

        prints("align");
      }
      break;

    case output_group:
      prints("output");
      break;

    case disc_group:
      prints("disc");
      break;

    case insert_group:
      prints("insert");
      break;

    case vcenter_group:
      prints("vcenter");
      break;

    case math_group:
    case math_choice_group:
    case math_shift_group:
    case math_left_group:
      {
        prints("math");

        if (cur_group == math_choice_group)
          prints(" choice");
        else if (cur_group == math_shift_group)
          prints(" shift");
        else if (cur_group == math_left_group)
          prints(" left");
      }
      break;
  }

  prints(" group (level ");
  print_int(cur_level);
  print_char(')');

  if (saved(-1) != 0)
  {
    if (e)
      prints(" entered at line ");
    else
      prints(" at line ");

    print_int(saved(-1));
  }
}

#ifdef STAT
void group_trace (boolean e)
{
  begin_diagnostic();
  print_char('{');

  if (e)
    prints("leaving ");
  else
    prints("entering ");

  print_group(e);
  print_char('}');
  end_diagnostic(false);
}
#endif

void show_save_groups (void)
{
  int p;
  int m;
  pointer v;
  quarterword l;
  group_code c;
  int a;
  integer i;
  quarterword j;
  const char * s;

  p = nest_ptr;
  nest[p] = cur_list;
  v = save_ptr;
  l = cur_level;
  c = cur_group;
  save_ptr = cur_boundary;
  decr(cur_level);
  a = 1;
  print_nl("");
  print_ln();

  while (true)
  {
    print_nl("### ");
    print_group(true);

    if (cur_group == bottom_level)
      goto done;

    do {
      m = nest[p].mode_field;

      if (p > 0)
        decr(p);
      else
        m = vmode;
    } while (!(m != hmode));

    prints(" (");

    switch (cur_group)
    {
      case simple_group:
        {
          incr(p);
          goto found2;
        }
        break;

      case hbox_group:
      case adjusted_hbox_group:
        s = "hbox";
        break;

      case vbox_group:
        s = "vbox";
        break;

      case vtop_group:
        s = "vtop";
        break;

      case align_group:
        if (a == 0)
        {
          if (m == -vmode)
            s = "halign";
          else
            s = "valign";

          a = 1;
          goto found1;
        }
        else
        {
          if (a == 1)
            prints("align entry");
          else
            print_esc("cr");

          if (p >= a)
            p = p - a;

          a = 0;
          goto found;
        }
        break;

      case no_align_group:
        {
          incr(p);
          a = -1;
          print_esc("noalign");
          goto found2;
        }
        break;

      case output_group:
        {
          print_esc("output");
          goto found;
        }
        break;
    
      case math_group:
        goto found2;
        break;
    
      case disc_group:
      case math_choice_group:
        {
          if (cur_group == disc_group)
            print_esc("discretionary");
          else
            print_esc("mathchoice");

          for (i = 1; i <= 3; ++i)
          {
            if (i <= saved(-2))
              prints("{}");
          }

          goto found2;
        }
        break;

      case insert_group:
        {
          if (saved(-2) == 255)
            print_esc("vadjust");
          else
          {
            print_esc("insert");
            print_int(saved(-2));
          }

          goto found2;
        }
        break;

      case vcenter_group:
        {
          s = "vcenter";
          goto found1;
        }

      case semi_simple_group:
        {
          incr(p);
          print_esc("begingroup");
          goto found;
        }
        break;

      case math_shift_group:
        {
          if (m ==mmode)
            print_char('$');
          else if (nest[p].mode_field == mmode)
          {
            print_cmd_chr(eq_no, saved(-2));
            goto found;
          }

          print_char('$');
          goto found;
        }
        break;

      case math_left_group:
        {
          if (type(nest[p + 1].eTeX_aux_field) == left_noad)
            print_esc("left");
          else
            print_esc("middle");

          goto found;
        }
        break;
    }
  }

  i = saved(-4);

  if (i != 0)
  {
    if (i < box_flag)
    {
      if (abs(nest[p].mode_field) == vmode)
        j = hmove;
      else
        j = vmove;

      if (i > 0)
        print_cmd_chr(j, 0);
      else
        print_cmd_chr(j, 1);

      print_scaled(abs(i));
      prints("pt");
    }
    else if (i < ship_out_flag)
    {
      if (i >= global_box_flag)
      {
        print_esc("global");
        i = i - (global_box_flag - box_flag);
      }

      print_esc("setbox");
      print_int(i - box_flag);
      print_char('=');
    }
    else
      print_cmd_chr(leader_ship, i - (leader_flag - a_leaders));
  }

found1:
  print_esc(s);

  if (saved(-2) != 0)
  {
    print_char(' ');

    if (saved(-3) == exactly)
      prints("to");
    else
      prints("spread");

    print_scaled(saved(-2));
    prints("pt");
  }

found2:
  print_char('{');

found:
  print_char(')');
  decr(cur_level);
  cur_group = save_level(save_ptr);
  save_ptr = save_index(save_ptr);

done:
  save_ptr = v;
  cur_level = l;
  cur_group = c;
}

void scan_general_text (void)
{
  int s;
  pointer w;
  pointer d;
  pointer p;
  pointer q;
  halfword unbalance;

  s = scanner_status;
  w = warning_index;
  d = def_ref;
  scanner_status = absorbing;
  warning_index = cur_cs;
  def_ref = get_avail();
  token_ref_count(def_ref) = null;
  p = def_ref;
  scan_left_brace();
  unbalance = 1;

  while (true)
  {
    get_token();

    if (cur_tok < right_brace_limit)
    {
      if (cur_cmd < right_brace)
        incr(unbalance);
      else
      {
        decr(unbalance);

        if (unbalance == 0)
          goto found;
      }
    }

    store_new_token(cur_tok);
  }

found:
  q = link(def_ref);
  free_avail(def_ref);

  if (q == null)
    cur_val = temp_head;
  else
    cur_val = p;

  link(temp_head) = q;
  scanner_status = s;
  warning_index = w;
  def_ref = d;
}

// create an edge nod
pointer new_edge (small_number s, scaled w)
{
  pointer p;

  p = get_node(edge_node_size);
  type(p) = edge_node;
  subtype(p) = s;
  width(p) = w;
  edge_dist(p) = 0;

  return p;
}

pointer reverse (pointer this_box, pointer t, scaled cur_g, real cur_glue)
{
  pointer l, la;
  scaled disp, disp2;
  boolean disped;
  pointer p;
  pointer q;
  glue_ord g_order;
  int g_sign;
  real glue_temp;
  halfword m, n;

  g_order = glue_order(this_box);
  g_sign = glue_sign(this_box);
  disp = revdisp;
  disped = false;
  l = t;
  p = temp_ptr;
  m = min_halfword;
  n = min_halfword;

  while (true)
  {
    while (p != null)
reswitch:
    if (is_char_node(p))
    {
      do {
        f = font(p);
        c = character(p);
        cur_h = cur_h + char_width(f, char_info(f, c));

        if (font_dir[f] != dir_default)
        {
          q = link(p);
          la = l;
          l = p;
          p = link(q);
          link(q) = la;
        }
        else
        {
          q = link(p);
          link(p) = l;
          l = p;
          p = q;
        }
      } while (!(!is_char_node(p)));
    }
    else
    {
      q = link(p);

      switch (type(p))
      {
        case hlist_node:
        case vlist_node:
        case rule_node:
        case kern_node:
          rule_wd = width(p);
          break;

        case glue_node:
          {
            round_glue();
            handle_a_glue_node();
          }
          break;

        case ligature_node:
          {
            flush_node_list(lig_ptr(p));
            temp_ptr = p;
            p = get_avail();
            mem[p] = mem[lig_char(temp_ptr)];
            link(p) = q;
            free_node(temp_ptr, small_node_size);
            goto reswitch;
          }
          break;

        case math_node:
          {
            rule_wd = width(p);

            if (end_LR(p))
            {
              if (info(LR_ptr) != end_LR_type(p))
              {
                type(p) = kern_node;
                incr(LR_problems);
              }
              else
              {
                pop_LR();

                if (n > min_halfword)
                {
                  decr(n);
                  decr(subtype(p));
                }
                else
                {
                  type(p) = kern_node;

                  if (m > min_halfword)
                    decr(m);
                  else
                  {
                    free_node(p, small_node_size);
                    link(t) = q;
                    width(t) = rule_wd;
                    edge_dist(t) = -cur_h - rule_wd;
                    goto done;
                  }
                }
              }
            }
            else
            {
              push_LR(p);

              if ((n > min_halfword) || (LR_dir(p) != cur_dir))
              {
                incr(n);
                incr(subtype(p));
              }
              else
              {
                type(p) = kern_node;
                incr(m);
              }
            }
          }
          break;

        case edge_node:
          confusion("LR2");
          break;
          
        case disp_node:
          {
            disp2 = disp_dimen(p);
            disp_dimen(p) = disp;
            disp = disp2;
          
            if (!disped)
              disped = true;
          }
          break;

        default:
          goto next_p;
          break;
      }

      cur_h = cur_h + rule_wd;

next_p:
      link(p) = l;

      if (type(p) == kern_node)
        if ((rule_wd == 0) || (l == null))
        {
          free_node(p, small_node_size);
          p = l;
        }

      l = p;
      p = q;
    }

    if ((t == null) && (m == min_halfword) && (n == min_halfword))
      goto done;

    p = new_math(0, info(LR_ptr));
    LR_problems = LR_problems + 10000;
  }

done:
  if ((l != null) && (type(l) != disp_node))
  {
    p = get_node(small_node_size);
    type(p) = disp_node;
    disp_dimen(p) = disp;
    link(p) = l;
    return p;
  }
  else
    return l;
}

// create a segment node
pointer new_segment (small_number s, pointer f)
{
  pointer p;

  p = get_node(segment_node_size);
  type(p) = segment_node;
  subtype(p) = s;
  width(p) = 0;
  segment_first(p) = f;
  segment_last(p) = f;

  return p;
}

void just_copy (pointer p, pointer h, pointer t)
{
  pointer r;
  int words;

  while (p != null)
  {
    words = 1;

    if (is_char_node(p))
      r = get_avail();
    else switch (type(p))
    {
      case dir_node:
      case hlist_node:
      case vlist_node:
        {
          r = get_node(box_node_size);
          mem[r + 7] = mem[p + 7];
          mem[r + 6] = mem[p + 6];
          mem[r + 5] = mem[p + 5];
          add_glue_ref(space_ptr(r));
          add_glue_ref(xspace_ptr(r));
          words = 5;
          list_ptr(r) = null;
        }
        break;

      case rule_node:
        {
          r = get_node(rule_node_size);
          words = rule_node_size;
        }
        break;

      case ligature_node:
        {
          r = get_avail();
          mem[r] = mem[lig_char(p)];
          goto found;
        }
        break;

      case kern_node:
      case math_node:
        {
          r = get_node(small_node_size);
          words = small_node_size;
        }
        break;

      case glue_node:
        {
          r = get_node(small_node_size);
          add_glue_ref(glue_ptr(p));
          glue_ptr(r) = glue_ptr(p);
          leader_ptr(r) = null;
        }
        break;

      case whatsit_node:
        switch (subtype(p))
        {
          case open_node:
            {
              r = get_node(open_node_size);
              words = open_node_size;
            }
            break;

          case write_node:
          case special_node:
            {
              r = get_node(write_node_size);
              add_token_ref(write_tokens(p));
              words = write_node_size;
            }
            break;

          case close_node:
          case language_node:
            {
              r = get_node(small_node_size);
              words = small_node_size;
            }
            break;

          case pdf_save_pos_node:
            r = get_node(small_node_size);
            break;

          default:
            confusion("ext2");
            break;
        }
        break;

      default:
        goto not_found;
        break;
    }

    while (words > 0)
    {
      decr(words);
      mem[r + words] = mem[p + words];
    }

found:
    link(h) = r;
    h = r;

not_found:
    p = link(p);
  }

  link(h) = t;
}

void just_reverse (pointer p)
{
  pointer l;
  pointer t;
  pointer q;
  halfword m, n;

  m = min_halfword;
  n = min_halfword;

  if (link(temp_head) == null)
  {
    just_copy(link(p), temp_head, null);
    q = link(temp_head);
  }
  else
  {
    q = link(p);
    link(p) = null;
    flush_node_list(link(temp_head));
  }

  t = new_edge(cur_dir, 0);
  l = t;
  cur_dir = reflected;

  while (q != null)
  {
    if (is_char_node(q))
    {
      do {
        p = q;
        q = link(p);
        link(p) = l;
        l = p;
      } while (!(!is_char_node(q)));
    }
    else
    {
      p = q;
      q = link(p);

      if (type(p) == math_node)
        adjust_the_LR_stack_j();

      link(p) = l;
      l = p;
    }
  }

  goto done;

found:
  width(t) = width(p);
  link(t) = q;
  free_node(p, small_node_size);

done:
  link(temp_head) = l;
}

void app_display (pointer j, pointer b, scaled d)
{
  scaled z;
  scaled s;
  scaled e;
  integer x;
  pointer p, q, r, t, u;

  s = display_indent;
  x = pre_display_direction;

  if (x == 0)
    shift_amount(b) = s + d;
  else
  {
    z = display_width;
    p = b;

    if (x > 0)
      e = z - d - width(p);
    else
    {
      e = d;
      d = z - e - width(p);
    }

    if (j != null)
    {
      b = copy_node_list(j);
      height(b) = height(p);
      depth(b) = depth(p);
      s = s - shift_amount(b);
      d = d + s;
      e = e + width(b) - z - s;
    }

    if (box_lr(p) == dlist)
      q = p;
    else
    {
      r = list_ptr(p);
      free_node(p, box_node_size);

      if (r == null)
        confusion("LR4");

      if (x > 0)
      {
        p = r;

        do {
          q = r;
          r = link(r);
        } while (!(r == null));
      }
      else
      {
        p = null;
        q = r;

        do {
          t = link(r);
          link(r) = p;
          p = r;
          r = t;
        } while (!(r == null));
      }
    }

    if (j == null)
    {
      r = new_kern(0);
      t = new_kern(0);
    }
    else
    {
      r = list_ptr(b);
      t = link(r);
    }

    u = new_math(0, end_M_code);

    if (type(t) == glue_node)
    {
      cancel_glue(right_skip_code, q, u, t, e);
      link(u) = t;
    }
    else
    {
      width(t) = e;
      link(t) = u;
      link(q) = t;
    }

    u = new_math(0, begin_M_code);

    if (type(r) == glue_node)
    {
      cancel_glue(left_skip_code, u, p, r, d);
      link(r) = u;
    }
    else
    {
      width(r) = d;
      link(r) = p;
      link(u) = r;

      if (j == null)
      {
        b = hpack(u, 0, 1);
        shift_amount(b) = s;
      }
      else
        list_ptr(b) = u;
    }
  }

  append_to_vlist(b);
}

void pseudo_start (void)
{
  int old_setting; // {holds |selector| setting}
  str_number s; // {string to be converted into a pseudo file}
  pool_pointer l, m;  // {indices into |str_pool|}
  pointer p, q, r;  // {for list construction}
  four_quarters w;  // 
  integer nl, sz; // {four ASCII codes}

  scan_general_text();
  old_setting = selector;
  selector = new_string;
  token_show(temp_head);
  selector = old_setting;
  flush_list(link(temp_head));
  str_room(1);
  s = make_string();
  //@<Convert string |s| into a new pseudo file@>;
  str_pool[pool_ptr] = ' ';
  l = str_start[s];
  nl = new_line_char;
  p = get_avail();
  q = p;

  while (l < pool_ptr)
  {
    m = l;

    while ((l < pool_ptr) && (str_pool[l] != nl))
      incr(l);

    sz = (l - m + 7) / 4;

    if (sz == 1)
      sz = 2;

    r = get_node(sz);
    link(q) = r;
    q = r;
    info(q) = sz;

    while (sz > 2)
    {
      decr(sz);
      incr(r);
      w.b0 = str_pool[m];
      w.b1 = str_pool[m + 1];
      w.b2 = str_pool[m + 2];
      w.b3 = str_pool[m + 3];
      mem[r].qqqq = w;
      m = m + 4;
    }

    w.b0 = ' ';
    w.b1 = ' ';
    w.b2 = ' ';
    w.b3 = ' ';

    if (l > m)
    {
      w.b0 = str_pool[m];

      if (l > m + 1)
      {
        w.b1 = str_pool[m + 1];

        if (l > m + 2)
        {
          w.b2 = str_pool[m + 2];

          if (l > m + 3)
            w.b3 = str_pool[m + 3];
        }
      }
    }

    mem[r + 1].qqqq = w;

    if (str_pool[l] == nl)
      incr(l);
  }

  info(p) = link(p);
  link(p) = pseudo_files;
  pseudo_files = p;
  flush_string();
  // @<Initiate input from new pseudo file@>;
  begin_file_reading();
  line = 0;
  limit = start;
  loc = limit + 1;

  if (tracing_scan_tokens > 0)
  {
    if (term_offset > max_print_line - 3)
      print_ln();
    else if ((term_offset > 0) || (file_offset > 0))
      print_char(' ');

    name = 19;
    prints("( ");
    incr(open_parens);
    update_terminal();
  }
  else
    name = 18;
}

// {inputs the next line or returns |false|}
boolean pseudo_input (void)
{
  pointer p;  // {current line from pseudo file}
  integer sz; // {size of node |p|}
  four_quarters w;  // {four ASCII codes}
  pointer r;  // {loop index}

  last = first; // {cf.\ Matthew 19\thinspace:\thinspace30}
  p = info(pseudo_files);

  if (p == null)
    return false;
  else
  {
    info(pseudo_files) = link(p);
    sz = info(p);

    if (4 * sz - 3 >= buf_size - last)
    {
      cur_input.loc_field = first;
      cur_input.limit_field = last - 1;
      overflow("buffer size", buf_size);
    }

    last = first;

    for (r = p + 1; r <= p + sz - 1; r++)
    {
      w = mem[r].qqqq;
      buffer[last] = w.b0;
      buffer[last + 1] = w.b1;
      buffer[last + 2] = w.b2;
      buffer[last + 3] = w.b3;
      last = last + 4;
    }

    if (last >= max_buf_stack)
      max_buf_stack = last + 1;

    while ((last > first) && (buffer[last - 1] == ' '))
      decr(last);

    free_node(p, sz);
    return true;
  }
}

// {close the top level pseudo file}
void pseudo_close (void)
{
  pointer p, q;

  p = link(pseudo_files);
  q = info(pseudo_files);
  free_avail(pseudo_files);
  pseudo_files = p;

  while (q != null)
  {
    p = q;
    q = link(p);
    free_node(p, info(p));
  }
}

/*
  {sets |cur_cmd|, |cur_chr|, |cur_tok|,
  and expands non-protected macros}
*/
void get_x_or_protected (void)
{
  while (true)
  {
    get_token();

    if (cur_cmd <= max_command)
      return;

    if ((cur_cmd >= call) && (cur_cmd < end_template))
      if (info(link(cur_chr)) == protected_token)
        return;

    expand();
  }
}

void group_warning (void)
{
  integer i;
  boolean w;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  i = in_open;
  w = false;

  while ((grp_stack[i] == cur_boundary) && (i > 0))
  {
    if (tracing_nesting > 0)
    {
      while ((input_stack[base_ptr].state_field == token_list) ||
        (input_stack[base_ptr].index_field > i))
        decr(base_ptr);

      if (input_stack[base_ptr].name_field > 17)
        w = true;
    }

    grp_stack[i] = save_index(save_ptr);
    decr(i);
  }

  if (w)
  {
    print_nl("Warning: end of ");
    print_group(true);
    prints(" of a different file");
    print_ln();

    if (tracing_nesting > 1)
      show_context();

    if (history == spotless)
      history = warning_issued;
  }
}

void if_warning (void)
{
  int i;
  boolean w;

  base_ptr = input_ptr;
  input_stack[base_ptr] = cur_input;
  i = in_open;
  w = false;

  while (if_stack[i] == cond_ptr)
  {
    if (tracing_nesting > 0)
    {
      while ((input_stack[base_ptr].state_field == token_list) ||
        (input_stack[base_ptr].index_field > i))
        decr(base_ptr);

      if (input_stack[base_ptr].name_field > 17)
        w = true;
    }

    if_stack[i] = link(cond_ptr);
    decr(i);
  }

  if (w)
  {
    print_nl("Warning: end of ");
    print_cmd_chr(if_test, cur_if);
    print_if_line(if_line);
    prints(" of a different file");
    print_ln();

    if (tracing_nesting > 1)
      show_context();

    if (history == spotless)
      history = warning_issued;
  }
}

void file_warning (void)
{
  pointer p;
  quarterword l;
  quarterword c;
  integer i;

  p = save_ptr;
  l = cur_level;
  c = cur_group;
  save_ptr = cur_boundary;

  while (grp_stack[in_open] != save_ptr)
  {
    decr(cur_level);
    print_nl("Warning: end of file when ");
    print_group(true);
    prints(" is incomplete");
    cur_group = save_level(save_ptr);
    save_ptr = save_index(save_ptr);
  }

  save_ptr = p;
  cur_level = l;
  cur_group = c;
  p = cond_ptr;
  l = if_limit;
  c = cur_if;
  i = if_line;

  while (if_stack[in_open] != cond_ptr)
  {
    print_nl("Warning: end of file when ");
    print_cmd_chr(if_test, cur_if);

    if (if_limit == fi_code)
      print_esc("else");

    print_if_line(if_line);
    prints(" is incomplete");
    if_line = if_line_field(cond_ptr);
    cur_if = subtype(cond_ptr);
    if_limit = type(cond_ptr);
    cond_ptr = link(cond_ptr);
  }

  cond_ptr = p;
  if_limit = l;
  cur_if = c;
  if_line = i;
  print_ln();

  if (tracing_nesting > 1)
    show_context();

  if (history == spotless)
    history = warning_issued;
}

void scan_expr (void)
{
  boolean a, b;
  small_number l;
  small_number r;
  small_number s;
  small_number o;
  integer e;
  integer t;
  integer f;
  integer n;
  pointer p;
  pointer q;

  l = cur_val_level;
  a = arith_error;
  b = false;
  p = null;

restart:
  r = expr_none;
  e = 0;
  s = expr_none;
  t = 0;
  n = 0;

continu:
  if (s == expr_none)
    o = l;
  else
    o = int_val;

  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok == other_token + '(')
  {
    q = get_node(expr_node_size);
    link(q) = p;
    type(q) = l;
    subtype(q) = 4 * s + r;
    expr_e_field(q) = e;
    expr_t_field(q) = t;
    expr_n_field(q) = n;
    p = q;
    l = o;
    goto restart;
  }

  back_input();

  if (o == int_val)
    scan_int();
  else if (o == dimen_val)
    scan_normal_dimen();
  else if (o == glue_val)
    scan_normal_glue();
  else
    scan_mu_glue();

  f = cur_val;

found:
  do {
    get_x_token();
  } while (!(cur_cmd != spacer));

  if (cur_tok == other_token + '+')
    o = expr_add;
  else if (cur_tok == other_token + '-')
    o = expr_sub;
  else if (cur_tok == other_token + '*')
    o = expr_mult;
  else if (cur_tok == other_token + '/')
    o = expr_div;
  else
  {
    o = expr_none;

    if (p == null)
    {
      if (cur_cmd != relax)
        back_input();
    }
    else if (cur_tok != other_token + ')')
    {
      print_err("Missing ) inserted for expression");
      help1("I was expecting to see `+', `-', `*', `/', or `)'. Didn't.");
      back_error();
    }
  }

  arith_error = b;

  if ((l == int_val) || (s > expr_sub))
  {
    if ((f > infinity) || (f < -infinity))
      num_error(f);
  }
  else if (l == dimen_val)
  {
    if (abs(f) > max_dimen)
      num_error(f);
  }
  else
  {
    if ((abs(width(f)) > max_dimen) ||
      (abs(stretch(f)) > max_dimen) ||
      (abs(shrink(f)) > max_dimen))
      glue_error(f);
  }

  switch (s)
  {
    case expr_none:
      if ((l >= glue_val) && (o != expr_none))
      {
        t = new_spec(f);
        delete_glue_ref(f);
        normalize_glue(t);
      }
      else
        t = f;
      break;

    case expr_mult:
      if (o == expr_div)
      {
        n = f;
        o = expr_scale;
      }
      else if (l == int_val)
        t = mult_integers(t, f);
      else if (l == dimen_val)
        expr_m(t);
      else
      {
        expr_m(width(t));
        expr_m(stretch(t));
        expr_m(shrink(t));
      }
      break;

    case expr_div:
      if (l < glue_val)
        expr_d(t);
      else
      {
        expr_d(width(t));
        expr_d(stretch(t));
        expr_d(shrink(t));
      }
      break;

    case expr_scale:
      if (l == int_val)
        t = fract(t, n, f, infinity);
      else if (l == dimen_val)
        expr_s(t);
      else
      {
        expr_s(width(t));
        expr_s(stretch(t));
        expr_s(shrink(t));
      }
      break;
  }

  if (o > expr_sub)
    s = o;
  else
  {
    s = expr_none;

    if (r == expr_none)
      e = t;
    else if (l == int_val)
      e = expr_add_sub(e, t, infinity);
    else if (l == dimen_val)
      e = expr_a(e, t);
    else
    {
      width(e) = expr_a(width(e), width(t));

      if (stretch_order(e) == stretch_order(t))
        stretch(e) = expr_a(stretch(e), stretch(t));
      else if ((stretch_order(e)<stretch_order(t)) && (stretch(t) != 0))
      {
        stretch(e) = stretch(t);
        stretch_order(e) = stretch_order(t);
      }

      if (shrink_order(e) == shrink_order(t))
        shrink(e) = expr_a(shrink(e), shrink(t));
      else if ((shrink_order(e)<shrink_order(t)) && (shrink(t) != 0))
      {
        shrink(e) = shrink(t);
        shrink_order(e) = shrink_order(t);
      }

      delete_glue_ref(t);
      normalize_glue(e);
    }

    r = o;
  }

  b = arith_error;

  if (o != expr_none)
    goto continu;

  if (p != null)
  {
    f = e; q = p;
    e = expr_e_field(q);
    t = expr_t_field(q);
    n = expr_n_field(q);
    s = subtype(q) / 4;
    r = subtype(q) % 4;
    l = type(q);
    p = link(q);
    free_node(q, expr_node_size);
    goto found;
  }

  if (b)
  {
    print_err("Arithmetic overflow");
    help2("I can't evaluate this expression,",
      "since the result is out of range.");
    error();

    if (l >= glue_val)
    {
      delete_glue_ref(e);
      e = zero_glue;
      add_glue_ref(e);
    }
    else
      e = 0;
  }

  arith_error = a;
  cur_val = e;
  cur_val_level = l;
}

void scan_normal_glue (void)
{
  scan_glue(glue_val);
}

void scan_mu_glue (void)
{
  scan_glue(mu_val);
}

integer add_or_sub (integer x, integer y, integer max_answer, boolean negative)
{
  integer a;

  if (negative)
    negate(y);

  if (x >= 0)
    if (y <= max_answer - x)
      a = x + y;
    else
      num_error(a);
  else if (y >= -max_answer - x)
    a = x + y;
  else
    num_error(a);

  return a;
}

integer quotient (integer n, integer d)
{
  boolean negative;
  integer a;

  if (d == 0)
    num_error(a);
  else
  {
    if (d > 0)
      negative = false;
    else
    {
      negate(d);
      negative = true;
    }

    if (n < 0)
    {
      negate(n);
      negative = !negative;
    }

    a = n / d;
    n = n - a * d;
    d = n - d;

    if (d + n >= 0)
      incr(a);

    if (negative)
      negate(a);
  }

  return a;
}

integer fract (integer x, integer n, integer d, integer max_answer)
{
  boolean negative;
  integer a;
  integer f;
  integer h;
  integer r;
  integer t;

  if (d == 0)
    goto too_big;

  a = 0;

  if (d > 0)
    negative = false;
  else
  {
    negate(d);
    negative = true;
  }

  if (x < 0)
  {
    negate(x);
    negative = !negative;
  }
  else if (x == 0)
    goto done;

  if (n < 0)
  {
    negate(n);
    negative = !negative;
  }

  t = n / d;

  if (t > max_answer / x)
    goto too_big;

  a = t * x;
  n = n - t * d;

  if (n == 0)
    goto found;

  t = x / d;

  if (t > (max_answer - a) / n)
    goto too_big;

  a = a + t * n;
  x = x - t * d;

  if (x == 0)
    goto found;

  if (x < n)
  {
    t = x;
    x = n;
    n = t;
  }

  f = 0;
  r = (d / 2) - d;
  h = -r;

  while (true)
  {
    if (odd(n))
    {
      r = r + x;

      if (r >= 0)
      {
        r = r - d;
        incr(f);
      }
    }

    n = n / 2;

    if (n == 0)
      goto found1;

    if (x < h)
      x = x + x;
    else
    {
      t = x - d;
      x = t + x;
      f = f + n;

      if (x < n)
      {
        if (x == 0)
          goto found1;

        t = x;
        x = n;
        n = t;
      }
    }
  }
found1:

  if (f > (max_answer - a))
    goto too_big;

  a = a + f;

found:
  if (negative)
    negate(a);

  goto done;

too_big:
  num_error(a);

done:
  return a;
}

void scan_register_num (void)
{
  scan_int();

  if ((cur_val < 0) || (cur_val > max_reg_num))
  {
    print_err("Bad register code");
    help2(max_reg_help_line, "I changed this one to zero.");
    int_error(cur_val);
    cur_val = 0;
  }
}

void new_index (quarterword i, pointer q)
{
  small_number k;

  cur_ptr = get_node(index_node_size);
  sa_index(cur_ptr) = i;
  sa_used(cur_ptr) = 0;
  link(cur_ptr) = q;

  for (k = 1; k <= index_node_size - 1; k++)
    mem[cur_ptr + k] = sa_null;
}

void find_sa_element (small_number t, halfword n, boolean w)
{
  pointer q;
  small_number i;

  cur_ptr = sa_root[t];
  if_cur_ptr_is_null_then_return_or_goto(not_found);
  q = cur_ptr;
  i = hex_dig1(n);
  get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found1);
  q = cur_ptr;
  i = hex_dig2(n);
  get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found2);
  q = cur_ptr;
  i = hex_dig3(n);
  get_sa_ptr();
  if_cur_ptr_is_null_then_return_or_goto(not_found3);
  q = cur_ptr;
  i = hex_dig4(n);
  get_sa_ptr();

  if ((cur_ptr == null) && w)
    goto not_found4;

  goto exit;

not_found:
  new_index(t, null);
  sa_root[t] = cur_ptr;
  q = cur_ptr;
  i = hex_dig1(n);

not_found1:
  new_index(i, q);
  add_sa_ptr();
  q = cur_ptr;
  i = hex_dig2(n);

not_found2:
  new_index(i, q);
  add_sa_ptr();
  q = cur_ptr;
  i = hex_dig3(n);

not_found3:
  new_index(i, q);
  add_sa_ptr();
  q = cur_ptr;
  i = hex_dig4(n);

not_found4:
  if (t == mark_val)
  {
    cur_ptr = get_node(mark_class_node_size);
    mem[cur_ptr + 1] = sa_null;
    mem[cur_ptr + 2] = sa_null;
    mem[cur_ptr + 3] = sa_null;
  }
  else
  {
    if (t <= dimen_val)
    {
      cur_ptr = get_node(word_node_size);
      sa_int(cur_ptr) = 0;
      sa_num(cur_ptr) = n;
    }
    else
    {
      cur_ptr = get_node(pointer_node_size);

      if (t <= mu_val)
      {
        sa_ptr(cur_ptr) = zero_glue;
        add_glue_ref(zero_glue);
      }
      else
        sa_ptr(cur_ptr) = null;
    }

    sa_ref(cur_ptr) = null;
  }

  sa_index(cur_ptr) = 16 * t + i;
  sa_lev(cur_ptr) = level_one;
  link(cur_ptr) = q;
  add_sa_ptr();
exit:;
}

void delete_sa_ref (pointer q)
{
  pointer p;
  small_number i;
  small_number s;

  decr(sa_ref(q));

  if (sa_ref(q) != null)
    return;

  if (sa_index(q) < dimen_val_limit)
  {
    if (sa_int(q) == 0)
      s = word_node_size;
    else
      return;
  }
  else
  {
    if (sa_index(q) < mu_val_limit)
    {
      if (sa_ptr(q) == zero_glue)
        delete_glue_ref(zero_glue);
      else
        return;
    }
    else if (sa_ptr(q) != null)
      return;

    s = pointer_node_size;
  }

  do {
    i = hex_dig4(sa_index(q));
    p = q;
    q = link(p);
    free_node(p, s);

    if (q == null)
    {
      sa_root[i] = null;
      return;
    }

    delete_sa_ptr(); s = index_node_size;
  } while (!(sa_used(q) > 0));
}

#ifdef STAT
void show_sa (pointer p, const char * s)
{
  small_number t;

  begin_diagnostic();
  print_char('{');
  prints(s);
  print_char(' ');

  if (p == null)
    print_char('?');
  else
  {
    t = sa_type(p);

    if (t < box_val)
      print_cmd_chr(tex_register, p);
    else if (t == box_val)
    {
      print_esc("box");
      print_sa_num(p);
    }
    else if (t == tok_val)
      print_cmd_chr(toks_register, p);
    else
      print_char('?');

    print_char('=');

    if (t == int_val)
      print_int(sa_int(p));
    else if (t == dimen_val)
    {
      print_scaled(sa_dim(p));
      prints("pt");
    }
    else
    {
      p = sa_ptr(p);

      if (t == glue_val)
        print_spec(p, "pt");
      else if (t == mu_val)
        print_spec(p, "mu");
      else if (t == box_val)
      {
        if (p == null)
          prints("void");
        else
        {
          depth_threshold = 0;
          breadth_max = 1;
          show_node_list(p);
        }
      }
      else if (t == tok_val)
      {
        if (p != null)
          show_token_list(link(p), null, 32);
      }
      else
        print_char('?');
    }
  }

  print_char('}');
  end_diagnostic(false);
}
#endif

boolean do_marks (small_number a, small_number l, pointer q)
{
  small_number i;

  if (l < 4)
  {
    for (i = 0; i <= 15; ++i)
    {
      get_sa_ptr();

      if (cur_ptr != null)
        if (do_marks(a, l + 1, cur_ptr))
          delete_sa_ptr();
    }

    if (sa_used(q) == 0)
    {
      free_node(q, index_node_size);
      q = null;
    }
  }
  else
  {
    switch (a)
    {
      case fire_up_init:
        if (sa_bot_mark(q) != null)
        {
          if (sa_top_mark(q) != null)
            delete_token_ref(sa_top_mark(q));

          delete_token_ref(sa_first_mark(q));
          sa_first_mark(q) = null;

          if (link(sa_bot_mark(q)) == null)
          {
            delete_token_ref(sa_bot_mark(q));
            sa_bot_mark(q) = null;
          }
          else
            add_token_ref(sa_bot_mark(q));

          sa_top_mark(q) = sa_bot_mark(q);
        }
        break;

      case fire_up_done:
        if ((sa_top_mark(q) != null) && (sa_first_mark(q) == null))
        {
          sa_first_mark(q) = sa_top_mark(q);
          add_token_ref(sa_top_mark(q));
        }
        break;

      case destroy_marks:
        for (i = top_mark_code; i <= split_bot_mark_code; ++i)
        {
          get_sa_ptr();

          if (cur_ptr != null)
          {
            delete_token_ref(cur_ptr);
            put_sa_ptr(null);
          }
        }
        break;
    }

    if (sa_bot_mark(q) == null)
    {
      if (sa_split_bot_mark(q) == null)
      {
        free_node(q, mark_class_node_size);
        q = null;
      }
    }
  }

  return (q == null);
}

void sa_save (pointer p)
{
  pointer q;
  quarterword i;

  if (cur_level != sa_level)
  {
    check_full_save_stack();
    save_type(save_ptr) = restore_sa;
    save_level(save_ptr) = sa_level;
    save_index(save_ptr) = sa_chain;
    incr(save_ptr);
    sa_chain = null;
    sa_level = cur_level;
  }

  i = sa_index(p);

  if (i < dimen_val_limit)
  {
    if (sa_int(p) == 0)
    {
      q = get_node(pointer_node_size);
      i = tok_val_limit;
    }
    else
    {
      q = get_node(word_node_size);
      sa_int(q) = sa_int(p);
    }

    sa_ptr(q) = null;
  }
  else
  {
    q = get_node(pointer_node_size);
    sa_ptr(q) = sa_ptr(p);
  }

  sa_loc(q) = p;
  sa_index(q) = i;
  sa_lev(q) = sa_lev(p);
  link(q) = sa_chain;
  sa_chain = q;
  add_sa_ref(p);
}

void sa_destroy (pointer p)
{
  if (sa_index(p) < mu_val_limit)
    delete_glue_ref(sa_ptr(p));
  else if (sa_ptr(p) != null)
  {
    if (sa_index(p) < box_val_limit)
      flush_node_list(sa_ptr(p));
    else
      delete_token_ref(sa_ptr(p));
  }
}

void sa_def (pointer p, halfword e)
{
  add_sa_ref(p);

  if (sa_ptr(p) == e)
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "reassigning");
#endif

    sa_destroy(p);
  }
  else
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "changing");
#endif

    if (sa_lev(p) == cur_level)
      sa_destroy(p);
    else
      sa_save(p);

    sa_lev(p) = cur_level;
    sa_ptr(p) = e;

#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "into");
#endif
  }

  delete_sa_ref(p);
}

void sa_w_def (pointer p, integer w)
{
  add_sa_ref(p);

  if (sa_int(p) == w)
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "reassigning");
#endif
  }
  else
  {
#ifdef STAT
    if (tracing_assigns > 0)
      show_sa(p, "changing");
#endif

    if (sa_lev(p) != cur_level)
      sa_save(p);

    sa_lev(p) = cur_level;
    sa_int(p) = w;

#ifdef STAT 
    if (tracing_assigns > 0)
      show_sa(p, "into");
#endif
  }

  delete_sa_ref(p);
}

void gsa_def (pointer p, halfword e)
{
  add_sa_ref(p);

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "globally changing");
#endif

  sa_destroy(p);
  sa_lev(p) = level_one;
  sa_ptr(p) = e;

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "into");
#endif

  delete_sa_ref(p);
}

void gsa_w_def (pointer p, integer w)
{
  add_sa_ref(p);

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "globally changing");
#endif

  sa_lev(p) = level_one;
  sa_int(p) = w;

#ifdef STAT
  if (tracing_assigns > 0)
    show_sa(p, "into");
#endif

  delete_sa_ref(p);
}

void sa_restore (void)
{
  pointer p;

  do {
    p = sa_loc(sa_chain);

    if (sa_lev(p) == level_one)
    {
      if (sa_index(p) >= dimen_val_limit)
        sa_destroy(sa_chain);

#ifdef STAT
      if (tracing_restores > 0)
        show_sa(p, "retaining");
#endif
    }
    else
    {
      if (sa_index(p) < dimen_val_limit)
      {
        if (sa_index(sa_chain) < dimen_val_limit)
          sa_int(p) = sa_int(sa_chain);
        else
          sa_int(p) = 0;
      }
      else
      {
        sa_destroy(p);
        sa_ptr(p) = sa_ptr(sa_chain);
      }

      sa_lev(p) = sa_lev(sa_chain);

#ifdef STAT
      if (tracing_restores > 0)
        show_sa(p, "restoring");
#endif
    }

    delete_sa_ref(p);
    p = sa_chain;
    sa_chain = link(p);

    if (sa_index(p) < dimen_val_limit)
      free_node(p, word_node_size);
    else
      free_node(p, pointer_node_size);
  } while (!(sa_chain == null));
}

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
static boolean synctex_flag_ready   = false; /* content_ready */
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
          if (synctex_mag == 0)
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

static void * synctex_prepare_content (void)
{
  if (synctex_flag_ready)
  {
    return synctex_file;
  }

  if ((NULL != synctex_dot_open())
    && (SYNCTEX_NO_ERROR == synctex_record_settings())
    && (SYNCTEX_NO_ERROR == synctex_record_content()))
  {
    synctex_flag_ready = true;
    return synctex_file;
  }

  synctex_abort();
  return NULL;
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
#ifdef USE_KPATHSEA
    synctex_root_name = kpse_find_file(name_mbcs, kpse_tex_format, false);
#else
    synctex_root_name = name_mbcs;
#endif
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

  if (total_pages == 0)
  {
    if (sync_mag > 0)
      synctex_mag = sync_mag;
  }

  if (NULL != synctex_prepare_content())
  {
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
