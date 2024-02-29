/*************************************************************************
** UtilityTest.cpp                                                      **
**                                                                      **
** This file is part of dvisvgm -- a fast DVI to SVG converter          **
** Copyright (C) 2005-2024 Martin Gieseking <martin.gieseking@uos.de>   **
**                                                                      **
** This program is free software; you can redistribute it and/or        **
** modify it under the terms of the GNU General Public License as       **
** published by the Free Software Foundation; either version 3 of       **
** the License, or (at your option) any later version.                  **
**                                                                      **
** This program is distributed in the hope that it will be useful, but  **
** WITHOUT ANY WARRANTY; without even the implied warranty of           **
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the         **
** GNU General Public License for more details.                         **
**                                                                      **
** You should have received a copy of the GNU General Public License    **
** along with this program; if not, see <http://www.gnu.org/licenses/>. **
*************************************************************************/

#include <gtest/gtest.h>
#include <cmath>
#include <limits>
#include <sstream>
#include "utility.hpp"

using namespace util;
using namespace std;


TEST(UtilityTest, trim) {
	ASSERT_EQ(trim(""), "");
	ASSERT_EQ(trim("   "), "");
	ASSERT_EQ(trim(" \n \t  "), "");
	ASSERT_EQ(trim("abcdefg"), "abcdefg");
	ASSERT_EQ(trim("  abcdefg"), "abcdefg");
	ASSERT_EQ(trim("abcdefg  "), "abcdefg");
	ASSERT_EQ(trim("   abcdefg  "), "abcdefg");
	ASSERT_EQ(trim("   abc   defg  "), "abc   defg");
	ASSERT_EQ(trim("   abc \n  defg  \n \r"), "abc \n  defg");
}


TEST(UtilityTest, normalize_space) {
	ASSERT_EQ(normalize_space(""), "");
	ASSERT_EQ(normalize_space("   "), "");
	ASSERT_EQ(normalize_space(" \n \t  "), "");
	ASSERT_EQ(normalize_space("abcdefg"), "abcdefg");
	ASSERT_EQ(normalize_space("  abcdefg"), "abcdefg");
	ASSERT_EQ(normalize_space("abcdefg  "), "abcdefg");
	ASSERT_EQ(normalize_space("   abcdefg  "), "abcdefg");
	ASSERT_EQ(normalize_space("   abc   defg  "), "abc defg");
	ASSERT_EQ(normalize_space("   abc \n  defg  \n \r"), "abc defg");
	ASSERT_EQ(normalize_space("   abc \n  de\r\ffg  \n \r"), "abc de fg");
}


TEST(UtilityTest, tolower) {
	ASSERT_EQ(tolower(""), "");
	ASSERT_EQ(tolower("abcdefg"), "abcdefg");
	ASSERT_EQ(tolower("ABCDEFG"), "abcdefg");
	ASSERT_EQ(tolower("123XyZ456"), "123xyz456");
	ASSERT_EQ(tolower("123\nXyZ 456"), "123\nxyz 456");
}


TEST(UtilityTest, replace) {
	ASSERT_EQ(replace("abcdebcxyb", "bc", ","), "a,de,xyb");
	ASSERT_EQ(replace("abcdebcxyb", "bc", " : "), "a : de : xyb");
	ASSERT_EQ(replace("abcdebcxyb", "", ","), "abcdebcxyb");
	ASSERT_EQ(replace("abcdebcxyb", "bc", ""), "abcdebcxyb");
	ASSERT_EQ(replace("abcdebcxyb", "b", " B "), "a B cde B cxy B ");
	ASSERT_EQ(replace("", "b", " B "), "");
	ASSERT_EQ(replace("a,b, c ,d , e", " ,", ","), "a,b, c,d, e");
}


TEST(UtilityTest, split) {
	vector<string> parts = split("", ",");
	ASSERT_EQ(parts.size(), 1u);
	EXPECT_EQ(parts[0], "");

	parts = split("", ",", true);
	ASSERT_EQ(parts.size(), 0u);

	parts = split("abcde", "");
	ASSERT_EQ(parts.size(), 1u);
	EXPECT_EQ(parts[0], "abcde");

	parts = split("abcde", ",");
	ASSERT_EQ(parts.size(), 1u);
	EXPECT_EQ(parts[0], "abcde");

	parts = split("1,2 , 3, 4 ,,5", ",");
	ASSERT_EQ(parts.size(), 6u);
	size_t count=0;
	for (string part : {"1", "2 ", " 3", " 4 ", "", "5"}) {
		EXPECT_EQ(parts[count++], part);
	}

	parts = split("1 sep2sep3, sep", "sep");
	ASSERT_EQ(parts.size(), 4u);
	count=0;
	for (string part : {"1 ", "2", "3, ", ""}) {
		EXPECT_EQ(parts[count++], part);
	}
}


TEST(UtilityTest, ilog10) {
	ASSERT_EQ(ilog10(-10), 0);
	ASSERT_EQ(ilog10(-1), 0);
	ASSERT_EQ(ilog10(0), 0);
	for (int i=1; i <= 1000; i++) {
		ASSERT_EQ(ilog10(i), static_cast<int>(log10(i))) << "i=" << i;
	}
	for (int i=1000; i < numeric_limits<int>::max()/10; i*=10) {
		ASSERT_EQ(ilog10(i-1), static_cast<int>(log10(i-1))) << "i=" << i;
		ASSERT_EQ(ilog10(i), static_cast<int>(log10(i))) << "i=" << i;
		ASSERT_EQ(ilog10(i+1), static_cast<int>(log10(i+1))) << "i=" << i;
	}
}


TEST(UtilityTest, to_string) {
	ASSERT_EQ(util::to_string(0), "0");
	ASSERT_EQ(util::to_string(1), "1");
	ASSERT_EQ(util::to_string(100), "100");
	ASSERT_EQ(util::to_string(-1), "-1");
	ASSERT_EQ(util::to_string(2.123), "2.123");
	ASSERT_EQ(util::to_string(-2.123), "-2.123");
	ASSERT_EQ(util::to_string(1.500e-5), "0.000015");
}


static string base64 (const string &str) {
	istringstream iss(str);
	ostringstream oss;
	base64_copy(istreambuf_iterator<char>(iss), istreambuf_iterator<char>(), ostreambuf_iterator<char>(oss));
	return oss.str();
}


TEST(UtilityTest, base64_copy) {
	ASSERT_EQ(base64(""), "");
	ASSERT_EQ(base64("a"), "YQ==");
	ASSERT_EQ(base64("ab"), "YWI=");
	ASSERT_EQ(base64("abc"), "YWJj");
	ASSERT_EQ(base64("abcd"), "YWJjZA==");
	ASSERT_EQ(base64("aBcDe\nFgHiJ\n"), "YUJjRGUKRmdIaUoK");
}


TEST(UtilityTest, count_leading_zeros) {
	EXPECT_EQ(count_leading_zeros(int8_t(0)), 8);
	EXPECT_EQ(count_leading_zeros(int16_t(0)), 16);
	EXPECT_EQ(count_leading_zeros(int32_t(0)), 32);
	for (int i=0; i < 8; i++) {
		EXPECT_EQ(count_leading_zeros(uint8_t(1 << i)), 7-i);
		EXPECT_EQ(count_leading_zeros(uint8_t((1 << i) | 1)), 7-i);
		EXPECT_EQ(count_leading_zeros(uint8_t(0xff >> i)), i);
	}
	for (int i=0; i < 16; i++) {
		EXPECT_EQ(count_leading_zeros(uint16_t(1 << i)), 15-i);
		EXPECT_EQ(count_leading_zeros(uint16_t((1 << i) | 1)), 15-i);
		EXPECT_EQ(count_leading_zeros(uint16_t(0xffff >> i)), i);
	}
	for (int i=0; i < 32; i++) {
		EXPECT_EQ(count_leading_zeros(uint32_t(1 << i)), 31-i);
		EXPECT_EQ(count_leading_zeros(uint32_t((1 << i) | 1)), 31-i);
		EXPECT_EQ(count_leading_zeros(uint32_t(0xffffffff >> i)), i);
	}
}


TEST(UtilityTest, ilog2) {
	EXPECT_EQ(ilog2(0), -1);
	for (int i=0; i < 8; i++) {
		EXPECT_EQ(ilog2(uint8_t(1 << i)), i);
		EXPECT_EQ(ilog2(uint8_t((1 << i)|1)), i);
		EXPECT_EQ(ilog2(uint8_t(0xff >> i)), 7-i);
	}
	for (int i=0; i < 16; i++) {
		EXPECT_EQ(ilog2(uint16_t(1 << i)), i);
		EXPECT_EQ(ilog2(uint16_t((1 << i)|1)), i);
		EXPECT_EQ(ilog2(uint16_t(0xffff >> i)), 15-i);
	}
	for (int i=0; i < 32; i++) {
		EXPECT_EQ(ilog2(uint32_t(1 << i)), i);
		EXPECT_EQ(ilog2(uint32_t((1 << i)|1)), i);
		EXPECT_EQ(ilog2(uint32_t(0xffffffff >> i)), 31-i);
	}
}


TEST(UtilityTest, read_double) {
	istringstream iss("123 -456 .123 -.456 0.123 -0.456 +123.456 -123.456 10e-3 -1e-3 4.5e2 1.23e-3 2.30.4");
	double value=0;
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, 123.0, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, -456.0, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, .123, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, -.456, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, .123, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, -.456, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, 123.456, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, -123.456, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, 10e-3, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, -1e-3, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, 4.5e2, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, 1.23e-3, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, 2.3, 0.000001);
	EXPECT_TRUE(util::read_double(iss, value));
	EXPECT_NEAR(value, .4, 0.000001);
	EXPECT_FALSE(util::read_double(iss, value));  // eof, no further characters to read
}