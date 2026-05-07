#include "vendor/unity/unity.h"
#include <stdint.h>

typedef int64_t integer;
typedef int32_t halfword;
typedef integer scaled;

enum { inf_bad = 10000 };

static halfword badness(scaled t, scaled s)
{
  integer r;

  if (t == 0)
    return 0;
  else if (s <= 0)
    return inf_bad;
  else
  {
    if (t <= 7230584)
      r = (t * 297) / s;
    else if (s >= 1663497)
      r = t / (s / 297);
    else
      r = t;

    if (r > 1290)
      return inf_bad;
    else
      return (r * r * r + 0400000) / 01000000;
  }
}

void setUp(void) {}
void tearDown(void) {}

void test_badness_zero_target(void)
{
  TEST_ASSERT_EQUAL_INT(0, badness(0, 100));
  TEST_ASSERT_EQUAL_INT(0, badness(0, 1));
  TEST_ASSERT_EQUAL_INT(0, badness(0, 999999));
}

void test_badness_zero_stretch(void)
{
  TEST_ASSERT_EQUAL_INT(inf_bad, badness(100, 0));
  TEST_ASSERT_EQUAL_INT(inf_bad, badness(1, 0));
  TEST_ASSERT_EQUAL_INT(inf_bad, badness(100, -1));
}

void test_badness_perfect_fit(void)
{
  TEST_ASSERT_EQUAL_INT(0, badness(0, 65536));
}

void test_badness_known_values(void)
{
  // badness(t, s) ~ 100 * (t/s)^3
  // When t == s, badness should be approximately 100
  halfword b = badness(65536, 65536);
  TEST_ASSERT_INT_WITHIN(2, 100, b);

  // When t = s/2, badness ~ 100 * (0.5)^3 = 12.5
  b = badness(32768, 65536);
  TEST_ASSERT_INT_WITHIN(2, 12, b);

  // When t = 2*s, badness ~ 100 * 8 = 800
  b = badness(131072, 65536);
  TEST_ASSERT_INT_WITHIN(10, 800, b);
}

void test_badness_inf_bad_threshold(void)
{
  // Very large ratio should return inf_bad
  TEST_ASSERT_EQUAL_INT(inf_bad, badness(65536, 1));
  TEST_ASSERT_EQUAL_INT(inf_bad, badness(9999999, 100));
}

void test_badness_large_t_path(void)
{
  // t > 7230584, exercises the second branch
  halfword b = badness(8000000, 8000000);
  TEST_ASSERT_INT_WITHIN(5, 100, b);

  b = badness(10000000, 10000000);
  TEST_ASSERT_INT_WITHIN(5, 100, b);
}

void test_badness_large_t_small_s(void)
{
  // t > 7230584 and s < 1663497, exercises third branch (r = t)
  TEST_ASSERT_EQUAL_INT(inf_bad, badness(8000000, 1000000));
}

void test_badness_monotonic(void)
{
  // badness should increase as t/s ratio increases
  halfword b1 = badness(10000, 100000);
  halfword b2 = badness(20000, 100000);
  halfword b3 = badness(50000, 100000);
  TEST_ASSERT_TRUE(b1 <= b2);
  TEST_ASSERT_TRUE(b2 <= b3);
}

void test_badness_symmetric_scaling(void)
{
  // badness(k*t, k*s) == badness(t, s) for small k (ratio preserved)
  halfword b1 = badness(1000, 10000);
  halfword b2 = badness(2000, 20000);
  halfword b3 = badness(4000, 40000);
  TEST_ASSERT_EQUAL_INT(b1, b2);
  TEST_ASSERT_EQUAL_INT(b2, b3);
}

int main(void)
{
  UNITY_BEGIN();
  RUN_TEST(test_badness_zero_target);
  RUN_TEST(test_badness_zero_stretch);
  RUN_TEST(test_badness_perfect_fit);
  RUN_TEST(test_badness_known_values);
  RUN_TEST(test_badness_inf_bad_threshold);
  RUN_TEST(test_badness_large_t_path);
  RUN_TEST(test_badness_large_t_small_s);
  RUN_TEST(test_badness_monotonic);
  RUN_TEST(test_badness_symmetric_scaling);
  return UNITY_END();
}
