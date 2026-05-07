/*
 * Micro-benchmark harness for pTeX-ng internal functions.
 * Measures execution time of hot-path functions in isolation.
 *
 * Build: gcc -O2 -o bench_engine bench_engine.c -lm
 * Run:   ./bench_engine [iterations]
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <math.h>

typedef int64_t integer;
typedef int32_t halfword;
typedef uint16_t quarterword;
typedef double glue_ratio;
typedef integer scaled;

enum { inf_bad = 10000 };

/* ========== Functions under test (copied from aptex-src.c) ========== */

static halfword badness(scaled t, scaled s)
{
  integer r;
  if (t == 0) return 0;
  else if (s <= 0) return inf_bad;
  else {
    if (t <= 7230584)
      r = (t * 297) / s;
    else if (s >= 1663497)
      r = t / (s / 297);
    else
      r = t;
    if (r > 1290) return inf_bad;
    else return (r * r * r + 0400000) / 01000000;
  }
}

/* Minimal memory simulation for get_avail / get_node benchmarks */
typedef union {
  integer       cint;
  glue_ratio    gr;
  struct { halfword rh; halfword lh; } hh;
  struct { quarterword b0, b1, b2, b3; } qqqq;
} memory_word;

#define MEM_SIZE 1000000
static memory_word mem_array[MEM_SIZE];
#define mem mem_array
#define link(p) mem[p].hh.rh
#define info(p) mem[p].hh.lh

static halfword avail = 0;
static halfword hi_mem_min = MEM_SIZE / 2;

static void init_avail_pool(void)
{
  avail = hi_mem_min;
  for (halfword i = hi_mem_min; i < MEM_SIZE - 1; i++)
    link(i) = i + 1;
  link(MEM_SIZE - 1) = 0;
}

static halfword get_avail(void)
{
  halfword p = avail;
  if (p != 0)
    avail = link(p);
  else
    p = 0; // would normally trigger realloc
  link(p) = 0;
  return p;
}

static void free_avail(halfword p)
{
  link(p) = avail;
  avail = p;
}

/* ========== Benchmark harness ========== */

typedef struct {
  const char *name;
  double elapsed_ns;
  long iterations;
} bench_result;

static double get_time_ns(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1e9 + ts.tv_nsec;
}

#define BENCH(name, setup, body, iters) \
  do { \
    setup; \
    double start = get_time_ns(); \
    for (long _i = 0; _i < (iters); _i++) { body; } \
    double end = get_time_ns(); \
    results[nresults++] = (bench_result){ \
      name, (end - start) / (iters), (iters) \
    }; \
  } while(0)

static bench_result results[32];
static int nresults = 0;

/* ========== Benchmark cases ========== */

static volatile halfword sink;

static void bench_badness_typical(long iters)
{
  BENCH("badness (typical: t=32768, s=65536)",
    (void)0,
    { sink = badness(32768, 65536); },
    iters);
}

static void bench_badness_zero(long iters)
{
  BENCH("badness (zero target)",
    (void)0,
    { sink = badness(0, 65536); },
    iters);
}

static void bench_badness_large(long iters)
{
  BENCH("badness (large t: 8000000)",
    (void)0,
    { sink = badness(8000000, 8000000); },
    iters);
}

static void bench_badness_mixed(long iters)
{
  BENCH("badness (mixed inputs)",
    (void)0,
    {
      sink = badness((_i * 1000) % 100000, 65536);
    },
    iters);
}

static void bench_get_avail(long iters)
{
  BENCH("get_avail + free_avail",
    init_avail_pool(),
    {
      halfword p = get_avail();
      free_avail(p);
    },
    iters);
}

static void bench_get_avail_burst(long iters)
{
  BENCH("get_avail burst (100 alloc, 100 free)",
    init_avail_pool(),
    {
      halfword nodes[100];
      for (int j = 0; j < 100; j++) nodes[j] = get_avail();
      for (int j = 99; j >= 0; j--) free_avail(nodes[j]);
    },
    iters / 100);
}

static void bench_linked_list_traverse(long iters)
{
  // Build a linked list of 1000 nodes
  init_avail_pool();
  halfword head = get_avail();
  halfword tail = head;
  for (int i = 0; i < 999; i++) {
    halfword p = get_avail();
    link(tail) = p;
    tail = p;
  }
  link(tail) = 0;

  BENCH("linked list traverse (1000 nodes)",
    (void)0,
    {
      halfword p = head;
      int count = 0;
      while (p != 0) { count++; p = link(p); }
      sink = count;
    },
    iters);
}

/* ========== Main ========== */

int main(int argc, char **argv)
{
  long iters = 1000000;
  if (argc > 1) iters = atol(argv[1]);

  printf("=== pTeX-ng Micro-Benchmark ===\n");
  printf("Iterations per test: %ld\n\n", iters);

  bench_badness_typical(iters);
  bench_badness_zero(iters);
  bench_badness_large(iters);
  bench_badness_mixed(iters);
  bench_get_avail(iters);
  bench_get_avail_burst(iters);
  bench_linked_list_traverse(iters);

  printf("%-50s %12s %12s\n", "Benchmark", "ns/op", "ops/sec");
  printf("%-50s %12s %12s\n",
    "--------------------------------------------------",
    "------------", "------------");
  for (int i = 0; i < nresults; i++) {
    printf("%-50s %12.2f %12.0f\n",
      results[i].name,
      results[i].elapsed_ns,
      1e9 / results[i].elapsed_ns);
  }
  printf("\n");
  return 0;
}
