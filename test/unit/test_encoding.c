#include "vendor/unity/unity.h"
#include <ptexenc/ptexenc.h>
#include <ptexenc/unicode.h>
#include <string.h>
#include <stdbool.h>

void setUp(void)
{
  enable_UPTEX(true);
  set_enc_string("utf8", "uptex");
}

void tearDown(void) {}

void test_uptex_mode_enabled(void)
{
  TEST_ASSERT_TRUE(is_internalUPTEX());
}

void test_toDVI_identity_in_uptex(void)
{
  // In upTeX mode, toDVI should preserve Unicode code points
  long result = toDVI(0x4E00); // CJK Unified Ideograph "一"
  TEST_ASSERT_EQUAL_HEX32(0x4E00, result);

  result = toDVI(0x3042); // Hiragana "あ"
  TEST_ASSERT_EQUAL_HEX32(0x3042, result);
}

void test_fromDVI_identity_in_uptex(void)
{
  long result = fromDVI(0x4E00);
  TEST_ASSERT_EQUAL_HEX32(0x4E00, result);

  result = fromDVI(0x0041); // 'A'
  TEST_ASSERT_EQUAL_HEX32(0x0041, result);
}

void test_toDVI_fromDVI_roundtrip(void)
{
  long codepoints[] = {0x4E00, 0x3042, 0x30A2, 0xAC00, 0x2F800, 0x0041};
  int n = sizeof(codepoints) / sizeof(codepoints[0]);
  for (int i = 0; i < n; i++) {
    long dvi = toDVI(codepoints[i]);
    long back = fromDVI(dvi);
    TEST_ASSERT_EQUAL_HEX32(codepoints[i], back);
  }
}

void test_toJIS_cjk_unified(void)
{
  // "一" U+4E00 -> JIS 0x306C
  long jis = toJIS(0x4E00);
  TEST_ASSERT_EQUAL_HEX32(0x306C, jis);
}

void test_toJIS_hiragana(void)
{
  // "あ" U+3042 -> JIS 0x2422
  long jis = toJIS(0x3042);
  TEST_ASSERT_EQUAL_HEX32(0x2422, jis);
}

void test_fromJIS_roundtrip(void)
{
  // Round-trip for characters in JIS range
  long jis = toJIS(0x4E00);
  long ucs = fromJIS(jis);
  TEST_ASSERT_EQUAL_HEX32(0x4E00, ucs);
}

void test_fromBUFF_utf8_ascii(void)
{
  unsigned char buf[] = "Hello";
  long code = fromBUFF(buf, 5, 0);
  TEST_ASSERT_EQUAL_HEX32('H', code);
}

void test_fromBUFF_utf8_cjk(void)
{
  // "一" = U+4E00 = UTF-8: E4 B8 80
  unsigned char buf[] = {0xE4, 0xB8, 0x80, 0};
  long code = fromBUFF(buf, 3, 0);
  TEST_ASSERT_EQUAL_HEX32(0x4E00, code);
}

void test_fromBUFF_utf8_hiragana(void)
{
  // "あ" = U+3042 = UTF-8: E3 81 82
  unsigned char buf[] = {0xE3, 0x81, 0x82, 0};
  long code = fromBUFF(buf, 3, 0);
  TEST_ASSERT_EQUAL_HEX32(0x3042, code);
}

void test_multistrlen_ascii(void)
{
  unsigned char buf[] = "abc";
  int len = multistrlen(buf, 3, 0);
  TEST_ASSERT_EQUAL_INT(1, len);
}

void test_multistrlen_utf8_3byte(void)
{
  // "一" = E4 B8 80 (3 bytes)
  unsigned char buf[] = {0xE4, 0xB8, 0x80, 0};
  int len = multistrlen(buf, 3, 0);
  TEST_ASSERT_EQUAL_INT(3, len);
}

void test_multistrlen_utf8_4byte(void)
{
  // U+2F800 = UTF-8: F0 AF A0 80 (4 bytes)
  unsigned char buf[] = {0xF0, 0xAF, 0xA0, 0x80, 0};
  int len = multistrlen(buf, 4, 0);
  TEST_ASSERT_EQUAL_INT(4, len);
}

int main(void)
{
  UNITY_BEGIN();
  RUN_TEST(test_uptex_mode_enabled);
  RUN_TEST(test_toDVI_identity_in_uptex);
  RUN_TEST(test_fromDVI_identity_in_uptex);
  RUN_TEST(test_toDVI_fromDVI_roundtrip);
  RUN_TEST(test_toJIS_cjk_unified);
  RUN_TEST(test_toJIS_hiragana);
  RUN_TEST(test_fromJIS_roundtrip);
  RUN_TEST(test_fromBUFF_utf8_ascii);
  RUN_TEST(test_fromBUFF_utf8_cjk);
  RUN_TEST(test_fromBUFF_utf8_hiragana);
  RUN_TEST(test_multistrlen_ascii);
  RUN_TEST(test_multistrlen_utf8_3byte);
  RUN_TEST(test_multistrlen_utf8_4byte);
  return UNITY_END();
}
