#define UNITY_INCLUDE_DOUBLE
#include "vendor/unity/unity.h"
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

typedef int64_t integer;
typedef int32_t halfword;
typedef uint16_t quarterword;
typedef double glue_ratio;
typedef bool boolean;
typedef halfword pointer;

typedef union {
  integer       cint;
  glue_ratio    gr;
  struct { halfword rh; union { halfword lh; struct { quarterword b0, b1; }; }; } hh;
  struct { quarterword b0, b1, b2, b3; } qqqq;
} memory_word;

#define MEM_SIZE 10000
static memory_word mem[MEM_SIZE];

#define link(p) mem[p].hh.rh
#define info(p) mem[p].hh.lh
#define type(p) mem[p].hh.b0
#define subtype(p) mem[p].hh.b1
#define node_size info
#define is_empty(p) (link(p) == 0x3FFFFFFF)
#define llink(p) info(p+1)
#define rlink(p) link(p+1)

enum { empty_flag = 0x3FFFFFFF };

/* Simulated single-word node allocator (avail stack) */
static halfword avail;
static halfword hi_mem_min = 5000;
static halfword mem_end = MEM_SIZE - 1;

static void init_avail(void)
{
  avail = hi_mem_min;
  for (halfword i = hi_mem_min; i < mem_end; i++)
    link(i) = i + 1;
  link(mem_end) = 0;
}

static halfword get_avail(void)
{
  halfword p = avail;
  if (p == 0) return 0; // out of memory
  avail = link(p);
  link(p) = 0;
  return p;
}

static void free_avail(halfword p)
{
  link(p) = avail;
  avail = p;
}

/* Simulated multi-word node allocator (rover doubly-linked free list) */
static halfword lo_mem_max = 4999;
static halfword rover;

static void init_rover(void)
{
  // Single free block from index 2 to lo_mem_max
  halfword block_start = 2;
  halfword block_size = lo_mem_max - block_start + 1;

  link(block_start) = empty_flag;
  node_size(block_start) = block_size;
  llink(block_start) = block_start;
  rlink(block_start) = block_start;
  rover = block_start;
}

static pointer get_node(halfword s)
{
  pointer p = rover;
  halfword start = p;

  do {
    halfword size = node_size(p);
    if (size >= s) {
      // Found a large enough block
      if (size == s) {
        // Exact fit: remove from free list
        halfword prev = llink(p);
        halfword next = rlink(p);
        if (prev == p) {
          // Last block
          rover = 0;
        } else {
          rlink(prev) = next;
          llink(next) = prev;
          rover = next;
        }
      } else {
        // Split: take from end of block
        halfword new_size = size - s;
        node_size(p) = new_size;
        p = p + new_size;
        rover = llink(p - new_size + 1); // stay on same block if possible
        // Actually just keep rover at the reduced block
        rover = p - new_size;
      }
      link(p) = 0;
      return p;
    }
    p = rlink(p);
  } while (p != start && p != 0);

  return 0; // out of memory
}

static void free_node(pointer p, halfword s)
{
  // Simple version: just mark as free, insert into rover list
  node_size(p) = s;
  link(p) = empty_flag;
  if (rover == 0) {
    llink(p) = p;
    rlink(p) = p;
    rover = p;
  } else {
    halfword next = rlink(rover);
    rlink(rover) = p;
    llink(p) = rover;
    rlink(p) = next;
    llink(next) = p;
  }
}

void setUp(void) { memset(mem, 0, sizeof(mem)); }
void tearDown(void) {}

/* ===== get_avail tests ===== */

void test_avail_basic(void)
{
  init_avail();
  halfword p = get_avail();
  TEST_ASSERT_TRUE(p >= hi_mem_min);
  TEST_ASSERT_TRUE(p <= mem_end);
}

void test_avail_sequence(void)
{
  init_avail();
  halfword p1 = get_avail();
  halfword p2 = get_avail();
  halfword p3 = get_avail();
  TEST_ASSERT_NOT_EQUAL(p1, p2);
  TEST_ASSERT_NOT_EQUAL(p2, p3);
  TEST_ASSERT_NOT_EQUAL(p1, p3);
}

void test_avail_free_reuse(void)
{
  init_avail();
  halfword p1 = get_avail();
  free_avail(p1);
  halfword p2 = get_avail();
  TEST_ASSERT_EQUAL_INT32(p1, p2); // LIFO: freed node comes back first
}

void test_avail_exhaustion(void)
{
  init_avail();
  int count = 0;
  while (get_avail() != 0) count++;
  TEST_ASSERT_EQUAL_INT(mem_end - hi_mem_min + 1, count);
}

/* ===== get_node tests ===== */

void test_node_basic(void)
{
  init_rover();
  pointer p = get_node(4);
  TEST_ASSERT_TRUE(p > 0);
}

void test_node_sizes(void)
{
  init_rover();
  pointer p1 = get_node(2); // small node
  pointer p2 = get_node(4); // medium node
  pointer p3 = get_node(10); // large node (box)
  TEST_ASSERT_TRUE(p1 > 0);
  TEST_ASSERT_TRUE(p2 > 0);
  TEST_ASSERT_TRUE(p3 > 0);
  // Nodes should not overlap
  TEST_ASSERT_TRUE(p1 + 2 <= p2 || p2 + 4 <= p1);
}

void test_node_free_and_realloc(void)
{
  init_rover();
  pointer p1 = get_node(4);
  free_node(p1, 4);
  pointer p2 = get_node(4);
  // Should reuse the freed block
  TEST_ASSERT_TRUE(p2 > 0);
}

void test_node_multiple_alloc_free(void)
{
  init_rover();
  pointer nodes[100];
  // Allocate 100 small nodes
  for (int i = 0; i < 100; i++) {
    nodes[i] = get_node(2);
    TEST_ASSERT_TRUE(nodes[i] > 0);
  }
  // Free them all
  for (int i = 0; i < 100; i++) {
    free_node(nodes[i], 2);
  }
  // Should be able to allocate again
  pointer p = get_node(2);
  TEST_ASSERT_TRUE(p > 0);
}

void test_node_exhaustion(void)
{
  init_rover();
  // Try to allocate more than available
  pointer p = get_node(lo_mem_max); // way too large
  TEST_ASSERT_EQUAL_INT32(0, p);
}

/* ===== Node type simulation ===== */

void test_build_char_node(void)
{
  init_avail();
  halfword p = get_avail();
  // Simulate char_node: font in b0, character in b1
  mem[p].hh.b0 = 1;   // font = 1
  mem[p].hh.b1 = 65;  // character = 'A'
  TEST_ASSERT_EQUAL_UINT16(1, type(p));
  TEST_ASSERT_EQUAL_UINT16(65, subtype(p));
}

void test_build_glue_node(void)
{
  init_rover();
  pointer p = get_node(4); // medium_node_size
  type(p) = 12;   // glue_node
  subtype(p) = 0; // normal
  link(p) = 0;

  // glue_ptr stored in info(p+1)
  pointer spec = get_node(4); // glue_spec
  info(p + 1) = spec;

  // Fill glue_spec: width=10pt, stretch=5pt, shrink=3pt
  mem[spec + 1].cint = 10 * 65536;  // width
  mem[spec + 2].cint = 5 * 65536;   // stretch
  mem[spec + 3].cint = 3 * 65536;   // shrink

  TEST_ASSERT_EQUAL_UINT16(12, type(p));
  TEST_ASSERT_EQUAL_INT64(10 * 65536, mem[spec + 1].cint);
  TEST_ASSERT_EQUAL_INT64(5 * 65536, mem[spec + 2].cint);
  TEST_ASSERT_EQUAL_INT64(3 * 65536, mem[spec + 3].cint);
}

void test_build_linked_hlist(void)
{
  init_avail();
  // Build: char 'H' -> char 'i' -> null
  halfword h = get_avail();
  halfword i = get_avail();
  mem[h].hh.b0 = 1; mem[h].hh.b1 = 'H';
  mem[i].hh.b0 = 1; mem[i].hh.b1 = 'i';
  link(h) = i;
  link(i) = 0;

  // Traverse and count
  int count = 0;
  halfword p = h;
  while (p != 0) { count++; p = link(p); }
  TEST_ASSERT_EQUAL_INT(2, count);
}

int main(void)
{
  UNITY_BEGIN();
  RUN_TEST(test_avail_basic);
  RUN_TEST(test_avail_sequence);
  RUN_TEST(test_avail_free_reuse);
  RUN_TEST(test_avail_exhaustion);
  RUN_TEST(test_node_basic);
  RUN_TEST(test_node_sizes);
  RUN_TEST(test_node_free_and_realloc);
  RUN_TEST(test_node_multiple_alloc_free);
  RUN_TEST(test_node_exhaustion);
  RUN_TEST(test_build_char_node);
  RUN_TEST(test_build_glue_node);
  RUN_TEST(test_build_linked_hlist);
  return UNITY_END();
}
