#define UNITY_INCLUDE_DOUBLE
#include "vendor/unity/unity.h"
#include <stdint.h>
#include <stdbool.h>

typedef int64_t integer;
typedef int32_t halfword;
typedef integer scaled;
typedef integer nonnegative_integer;
typedef bool boolean;

#define negate(x) ((x) = -(x))

static boolean arith_error;
static integer ng_remainder;

static scaled mult_and_add(integer n, scaled x, scaled y, scaled max_answer)
{
  if (n < 0) { negate(x); negate(n); }
  if (n == 0) return y;
  else if (((x <= (max_answer - y) / n) && (-x <= (max_answer + y) / n)))
    return (n * x + y);
  else { arith_error = true; return 0; }
}

static scaled x_over_n(scaled x, integer n)
{
  scaled Result;
  boolean negative = false;

  if (n == 0) { arith_error = true; Result = 0; ng_remainder = x; }
  else {
    if (n < 0) { negate(x); negate(n); negative = true; }
    if (x >= 0) { Result = x / n; ng_remainder = x % n; }
    else { Result = -((-x) / n); ng_remainder = -((-x) % n); }
  }
  if (negative) negate(ng_remainder);
  return Result;
}

static scaled xn_over_d(scaled x, integer n, integer d)
{
  scaled Result;
  boolean positive;
  nonnegative_integer t, u, v;

  if (x >= 0) positive = true;
  else { negate(x); positive = false; }

  t = (x % 0100000) * n;
  u = (x / 0100000) * n + (t / 0100000);
  v = (u % d) * 0100000 + (t % 0100000);

  if (u / d >= 0100000) arith_error = true;
  else u = 0100000 * (u / d) + (v / d);

  if (positive) { Result = u; ng_remainder = v % d; }
  else { Result = -u; ng_remainder = -(v % d); }
  return Result;
}

void setUp(void) { arith_error = false; ng_remainder = 0; }
void tearDown(void) {}

/* ===== mult_and_add tests ===== */

void test_mult_and_add_basic(void)
{
  // 3 * 100 + 5 = 305
  scaled r = mult_and_add(3, 100, 5, 1000000);
  TEST_ASSERT_EQUAL_INT64(305, r);
  TEST_ASSERT_FALSE(arith_error);
}

void test_mult_and_add_zero_n(void)
{
  // 0 * x + y = y
  scaled r = mult_and_add(0, 999999, 42, 1000000);
  TEST_ASSERT_EQUAL_INT64(42, r);
}

void test_mult_and_add_negative_n(void)
{
  // -2 * 50 + 10 = -2 * 50 + 10 (negates both n and x)
  // => 2 * (-50) + 10 => n=2, x=-50 => 2*(-50)+10 = -90
  scaled r = mult_and_add(-2, 50, 10, 1000000);
  TEST_ASSERT_EQUAL_INT64(-90, r);
  TEST_ASSERT_FALSE(arith_error);
}

void test_mult_and_add_overflow(void)
{
  // Should trigger arith_error when result would exceed max_answer
  mult_and_add(1000000, 1000000, 0, 999999);
  TEST_ASSERT_TRUE(arith_error);
}

/* ===== x_over_n tests ===== */

void test_x_over_n_basic(void)
{
  scaled r = x_over_n(100, 3);
  TEST_ASSERT_EQUAL_INT64(33, r);
  TEST_ASSERT_EQUAL_INT64(1, ng_remainder);
}

void test_x_over_n_exact(void)
{
  scaled r = x_over_n(100, 4);
  TEST_ASSERT_EQUAL_INT64(25, r);
  TEST_ASSERT_EQUAL_INT64(0, ng_remainder);
}

void test_x_over_n_negative_x(void)
{
  scaled r = x_over_n(-100, 3);
  TEST_ASSERT_EQUAL_INT64(-33, r);
  TEST_ASSERT_EQUAL_INT64(-1, ng_remainder);
}

void test_x_over_n_negative_n(void)
{
  scaled r = x_over_n(100, -3);
  TEST_ASSERT_EQUAL_INT64(-33, r);
  // remainder is negated when n<0: x=-100,n=3 => rem=-1, then negate => 1
  TEST_ASSERT_EQUAL_INT64(1, ng_remainder);
}

void test_x_over_n_divide_by_zero(void)
{
  x_over_n(100, 0);
  TEST_ASSERT_TRUE(arith_error);
}

void test_x_over_n_scaled_point(void)
{
  // 1pt = 65536 (scaled units). 65536 / 2 = 32768 (0.5pt)
  scaled r = x_over_n(65536, 2);
  TEST_ASSERT_EQUAL_INT64(32768, r);
  TEST_ASSERT_EQUAL_INT64(0, ng_remainder);
}

/* ===== xn_over_d tests ===== */

void test_xn_over_d_basic(void)
{
  // x*n/d = 100*3/4 = 75
  scaled r = xn_over_d(100, 3, 4);
  TEST_ASSERT_EQUAL_INT64(75, r);
  TEST_ASSERT_EQUAL_INT64(0, ng_remainder);
}

void test_xn_over_d_with_remainder(void)
{
  // x*n/d = 100*1/3 = 33 remainder 1
  scaled r = xn_over_d(100, 1, 3);
  TEST_ASSERT_EQUAL_INT64(33, r);
  TEST_ASSERT_EQUAL_INT64(1, ng_remainder);
}

void test_xn_over_d_negative(void)
{
  scaled r = xn_over_d(-100, 3, 4);
  TEST_ASSERT_EQUAL_INT64(-75, r);
}

void test_xn_over_d_large_values(void)
{
  // Typical TeX usage: scaling a dimension
  // 10pt * 1000 / 1000 = 10pt (identity scaling)
  scaled ten_pt = 10 * 65536;
  scaled r = xn_over_d(ten_pt, 1000, 1000);
  TEST_ASSERT_EQUAL_INT64(ten_pt, r);
  TEST_ASSERT_FALSE(arith_error);
}

void test_xn_over_d_half(void)
{
  // 10pt * 1 / 2 = 5pt
  scaled ten_pt = 10 * 65536;
  scaled r = xn_over_d(ten_pt, 1, 2);
  TEST_ASSERT_EQUAL_INT64(5 * 65536, r);
}

void test_xn_over_d_magnification(void)
{
  // Simulate \magnification=1200: 1pt * 1200 / 1000 = 1.2pt
  scaled one_pt = 65536;
  scaled r = xn_over_d(one_pt, 1200, 1000);
  // 65536 * 1200 / 1000 = 78643.2 -> 78643
  TEST_ASSERT_EQUAL_INT64(78643, r);
  TEST_ASSERT_FALSE(arith_error);
}

int main(void)
{
  UNITY_BEGIN();
  RUN_TEST(test_mult_and_add_basic);
  RUN_TEST(test_mult_and_add_zero_n);
  RUN_TEST(test_mult_and_add_negative_n);
  RUN_TEST(test_mult_and_add_overflow);
  RUN_TEST(test_x_over_n_basic);
  RUN_TEST(test_x_over_n_exact);
  RUN_TEST(test_x_over_n_negative_x);
  RUN_TEST(test_x_over_n_negative_n);
  RUN_TEST(test_x_over_n_divide_by_zero);
  RUN_TEST(test_x_over_n_scaled_point);
  RUN_TEST(test_xn_over_d_basic);
  RUN_TEST(test_xn_over_d_with_remainder);
  RUN_TEST(test_xn_over_d_negative);
  RUN_TEST(test_xn_over_d_large_values);
  RUN_TEST(test_xn_over_d_half);
  RUN_TEST(test_xn_over_d_magnification);
  return UNITY_END();
}
