#define UNITY_INCLUDE_DOUBLE
#include "vendor/unity/unity.h"
#include <stdint.h>
#include <string.h>

typedef int64_t integer;
typedef int32_t halfword;
typedef uint16_t quarterword;
typedef double glue_ratio;

typedef struct {
  halfword rh;
  union {
    halfword lh;
    struct {
      quarterword b0, b1;
    };
  };
} two_halves;

typedef struct {
  quarterword b0, b1, b2, b3;
} four_quarters;

typedef union {
  integer       cint;
  glue_ratio    gr;
  two_halves    hh;
  four_quarters qqqq;
} memory_word;

#define link(p)    mem[p].hh.rh
#define info(p)    mem[p].hh.lh
#define type(p)    mem[p].hh.b0
#define subtype(p) mem[p].hh.b1

static memory_word mem[100];

void setUp(void) { memset(mem, 0, sizeof(mem)); }
void tearDown(void) {}

void test_memory_word_size(void)
{
  // memory_word should be exactly 2 * sizeof(halfword) in the extension model
  // Actually it's max(sizeof(integer), sizeof(glue_ratio), sizeof(two_halves), sizeof(four_quarters))
  // Under APTEX_EXTENSION: integer=8, glue_ratio=8, two_halves=8, four_quarters=8
  TEST_ASSERT_EQUAL_size_t(sizeof(integer), sizeof(memory_word));
  TEST_ASSERT_EQUAL_size_t(sizeof(glue_ratio), sizeof(memory_word));
}

void test_two_halves_layout(void)
{
  two_halves h;
  memset(&h, 0, sizeof(h));
  h.rh = 42;
  h.lh = 99;
  TEST_ASSERT_EQUAL_INT32(42, h.rh);
  TEST_ASSERT_EQUAL_INT32(99, h.lh);
}

void test_two_halves_b0_b1_overlap_lh(void)
{
  two_halves h;
  memset(&h, 0, sizeof(h));
  h.b0 = 3;
  h.b1 = 7;
  // b0 and b1 share storage with lh
  quarterword check_b0 = h.b0;
  quarterword check_b1 = h.b1;
  TEST_ASSERT_EQUAL_UINT16(3, check_b0);
  TEST_ASSERT_EQUAL_UINT16(7, check_b1);
}

void test_link_info_macros(void)
{
  mem[5].hh.rh = 10;  // link
  mem[5].hh.lh = 20;  // info
  TEST_ASSERT_EQUAL_INT32(10, link(5));
  TEST_ASSERT_EQUAL_INT32(20, info(5));
}

void test_type_subtype_macros(void)
{
  mem[3].hh.b0 = 12;  // type = glue_node
  mem[3].hh.b1 = 1;   // subtype = a_leaders
  TEST_ASSERT_EQUAL_UINT16(12, type(3));
  TEST_ASSERT_EQUAL_UINT16(1, subtype(3));
}

void test_four_quarters_access(void)
{
  mem[7].qqqq.b0 = 100;
  mem[7].qqqq.b1 = 200;
  mem[7].qqqq.b2 = 300;
  mem[7].qqqq.b3 = 400;
  TEST_ASSERT_EQUAL_UINT16(100, mem[7].qqqq.b0);
  TEST_ASSERT_EQUAL_UINT16(200, mem[7].qqqq.b1);
  TEST_ASSERT_EQUAL_UINT16(300, mem[7].qqqq.b2);
  TEST_ASSERT_EQUAL_UINT16(400, mem[7].qqqq.b3);
}

void test_cint_access(void)
{
  mem[0].cint = 123456789012LL;
  TEST_ASSERT_EQUAL_INT64(123456789012LL, mem[0].cint);
}

void test_glue_ratio_access(void)
{
  mem[1].gr = 3.14159;
  TEST_ASSERT_DOUBLE_WITHIN(0.00001, 3.14159, mem[1].gr);
}

void test_simple_linked_list(void)
{
  // Simulate: node 1 -> node 3 -> node 5 -> null(0)
  link(1) = 3;
  link(3) = 5;
  link(5) = 0;

  int count = 0;
  halfword p = 1;
  while (p != 0) {
    count++;
    p = link(p);
  }
  TEST_ASSERT_EQUAL_INT(3, count);
}

int main(void)
{
  UNITY_BEGIN();
  RUN_TEST(test_memory_word_size);
  RUN_TEST(test_two_halves_layout);
  RUN_TEST(test_two_halves_b0_b1_overlap_lh);
  RUN_TEST(test_link_info_macros);
  RUN_TEST(test_type_subtype_macros);
  RUN_TEST(test_four_quarters_access);
  RUN_TEST(test_cint_access);
  RUN_TEST(test_glue_ratio_access);
  RUN_TEST(test_simple_linked_list);
  return UNITY_END();
}
