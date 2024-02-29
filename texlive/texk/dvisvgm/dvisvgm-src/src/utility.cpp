/*************************************************************************
** utility.cpp                                                          **
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

#include <algorithm>
#include <cctype>
#include <cmath>
#include <fstream>
#include <functional>
#include <iterator>
#include "utility.hpp"

using namespace std;

/** Computes the singular value decomposition of a given 2x2 matrix M
 *  so that M = rotate(phi)*scale(sx,sy)*rotate(theta), where
 *  rotate(t):={{cos t, -sin t}, {sin t, cos t}} and scale(sx, sy):={{sx, 0}, {0, sy}}.
 *  The corresponding math can be found in Jim Blinn: "Consider the Lowly 2x2 Matrix"
 *  https://ieeexplore.ieee.org/document/486688
 *  Also published in "Jim Blinn's Corner: Notation, Notation, Notation", pp. 69--95.
 *  @param[in] m matrix {{m00, m01}, {m10, m11}} where the inner pairs denote the rows
 *  @return vector {phi, sx, sy, theta} */
vector<double> math::svd (const double (&m)[2][2]) {
	double phi=0, theta=0, sx=0, sy=0;
	if (m[0][0] != 0 || m[0][1] != 0 || m[1][0] != 0 || m[1][1] != 0) {
		double e = (m[0][0] + m[1][1])/2;  // = cos(phi+theta)*(sx+sy)/2
		double f = (m[0][0] - m[1][1])/2;  // = cos(phi-theta)*(sx-sy)/2
		double g = (m[1][0] + m[0][1])/2;  // = sin(phi-theta)*(sx-sy)/2
		double h = (m[1][0] - m[0][1])/2;  // = sin(phi+theta)*(sx+sy)/2
		double hyp1 = hypot(e, h);  // = (sx+sy)/2
		double hyp2 = hypot(f, g);  // = (sx-sy)/2
		sx = hyp1+hyp2;
		sy = hyp1-hyp2;
		if (hyp2 == 0)              // uniformly scaled rotation?
			theta = atan2(h, e);
		else if (hyp1 == 0)         // uniformly scaled reflection?
			theta = -atan2(g, f);
		else {
			double a1 = atan2(g, f); // = phi-theta (g/f = tan(phi-theta))
			double a2 = atan2(h, e); // = phi+theta (h/e = tan(phi+theta))
			phi = (a2+a1)/2;
			theta = (a2-a1)/2;
		}
	}
	return {phi, sx, sy, theta};
}


/** Returns the value of the definite integral of f:R->R over the interval [t0,t1]
 *  using a simple Simpson/Runge-Kutta (rk4) approximation.
 *  @param[in] t0 lower interval boundary
 *  @param[in] t1 upper interval boundary
 *  @param[in] n number of slices the interval is divided into
 *  @param[in] f function to integrate */
double math::integral (double t0, double t1, int n, const function<double(double)> &f) {
	double ti = t0, ui=0;
	double h = (t1-t0)/n;
	for (int i=0; i < n; i++) {
		double k1 = f(ti);
		double k2 = f(ti + h/2);
		double k4 = f(ti + h);
		ui += h*(k1 + 4*k2 + k4)/6;
		ti += h;
	}
	return ui;
}


/** Normalizes an angle to the interval [-mod, mod). */
double math::normalize_angle (double angle, double mod) {
	angle = fmod(angle+mod, 2.0*mod);
	if (angle < 0)
		angle += 2.0*mod;
	return angle-mod;
}


double math::normalize_0_2pi (double rad) {
	rad = fmod(rad, TWO_PI);
	if (rad < 0)
		rad += TWO_PI;
	return rad;
}


/** Returns a given string with leading and trailing whitespace removed.
 *  @param[in] str the string to process
 *  @param[in] ws characters treated as whitespace
 *  @return the trimmed string */
string util::trim (const string &str, const char *ws) {
	auto first = str.find_first_not_of(ws);
	if (first == string::npos)
		return "";
	auto last = str.find_last_not_of(ws);
	return str.substr(first, last-first+1);
}


/** Removes leading and trailing whitespace from a given string, and replaces
 *  all other whitespace sequences by single spaces.
 *  @param[in] str the string to process
 *  @param[in] ws characters treated as whitespace
 *  @return the normalized string */
string util::normalize_space (string str, const char *ws) {
	str = trim(str);
	auto first = str.find_first_of(ws);
	while (first != string::npos) {
		auto last = str.find_first_not_of(ws, first);
		str.replace(first, last-first, " ");
		first = str.find_first_of(ws, first+1);
	}
	return str;
}


/** Replaces all occurences of a substring with another string.
 *  @param[in] str string to search through
 *  @param[in] find string to look for
 *  @param[in] repl replacement for "find"
 *  @return the resulting string */
string util::replace (string str, const string &find, const string &repl) {
	if (!find.empty() && !repl.empty()) {
		auto first = str.find(find);
		while (first != string::npos) {
			str.replace(first, find.length(), repl);
			first = str.find(find, first+repl.length());
		}
	}
	return str;
}


/** Splits a string at all occurences of a given separator string and
 *  returns the substrings.
 *  @param[in] str string to split
 *  @param[in] sep separator to look for
 *  @param[in] allowEmptyResult if true, the result vector is empty if str is empty; otherwise an empty string is inserted
 *  @return the substrings between the separators */
vector<string> util::split (const string &str, const string &sep, bool allowEmptyResult) {
	vector<string> parts;
	if ((str.empty() && !allowEmptyResult) || sep.empty())
		parts.push_back(str);
	else if (!str.empty()) {
		string::size_type left=0;
		while (left <= str.length()) {
			auto right = str.find(sep, left);
			if (right == string::npos) {
				parts.push_back(str.substr(left));
				left = string::npos;
			}
			else {
				parts.push_back(str.substr(left, right-left));
				left = right+sep.length();
			}
		}
	}
	return parts;
}


string util::tolower (const string &str) {
	string ret=str;
	transform(str.begin(), str.end(), ret.begin(), ::tolower);
	return ret;
}


/** Converts a double to a string and strips redundant trailing digits/dots. */
string util::to_string (double val) {
	string str = std::to_string(val);
	str.erase(str.find_last_not_of('0')+1, string::npos);
	str.erase(str.find_last_not_of('.')+1, string::npos);
	return str;
}


/** Returns the integer part of log10 of a given integer \f$n>0\f$.
 *  If \f$n<0\f$, the result is 0. */
int util::ilog10 (int n) {
	int result = 0;
	while (n >= 10) {
		result++;
		n /= 10;
	}
	return result;
}


/** Reads an integer string of the form (+|-)?[0-9]+ from an input stream
 *  ana appends it to a given one.
 *  @param[in] is stream to read from
 *  @param[out] str the read string is appended to this one
 *  @param[in] allow_leading_sign true if the first character may be '+' or '-'
 *  @return true if the string coule be read successfully */
static bool read_int_string (istream &is, string &str, bool allow_leading_sign=true) {
	string intstr;
	if (is.peek() == '-' || is.peek() == '+') {
		if (!allow_leading_sign)
			return false;
		intstr += char(is.get());
		if (!isdigit(is.peek()))
			return false;
	}
	while (isdigit(is.peek()))
		intstr += char(is.get());
	str += intstr;
	return !intstr.empty();
}


/** Reads and parses a double from an input stream.
 *  @param[in] is stream to read from
 *  @param[out] value the read double value
 *  @return true if the value was read and parsed successfully */
bool util::read_double (istream &is, double &value) {
	is >> ws;
	string numstr;
	// read optional leading sign
	bool plusminus = (is.peek() == '-' || is.peek() == '+');
	if (plusminus)
		numstr += char(is.get());
	// read optional integer part (before decimal dot)
	read_int_string(is, numstr, !plusminus);
	if (is.peek() == '.') {
		numstr += char(is.get());
		// read fractional part (after decimal dot)
		if (!read_int_string(is, numstr, false))
			return false;
	}
	// read optional exponential part
	if (std::tolower(is.peek()) == 'e') {
		numstr += char(is.get());
		if (!read_int_string(is, numstr))
			return false;
	}
	try {
		size_t count;
		value = stod(numstr, &count);
		return count == numstr.length();
	}
	catch (...) {
		return false;
	}
}


/** Returns the contents of a file.
 *  @param[in] fname name/path of the file */
string util::read_file_contents (const string &fname) {
	ifstream ifs(fname, ios::binary);
	return {istreambuf_iterator<char>(ifs.rdbuf()), istreambuf_iterator<char>()};
}


/** Writes a sequence of bytes given as a string to a file.
 *  @param[in] name/path of the file to write
 *  @param[in] start iterator pointing to the begin of the byte sequence
 *  @param[in] end iterator pointing to the first byte after the byte sequence to write */
void util::write_file_contents (const string &fname, string::iterator start, string::iterator end) {
	ofstream ofs(fname, ios::binary);
	copy(start, end, ostream_iterator<char>(ofs));
}


string util::mimetype (const string &fname) {
	string ret;
	auto pos = fname.rfind('.');
	if (pos != string::npos) {
		string suffix = fname.substr(pos+1);
		if (suffix == "svg")
			ret = "svg+xml";
		else if (suffix == "png" || suffix == "gif")
			ret = suffix;
		else if (suffix == "jpg" || suffix == "jpeg")
			ret = "jpeg";
		else if (suffix == "tif" || suffix == "tiff")
			ret = "tiff";
	}
	if (!ret.empty())
		ret = "image/"+ret;
	return ret;
}

///////////////////////////////////////////////////////////////////////

static bool is_leap_year (int year) {
	return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}


/** Returns the number of leap years in the interval [year1, year2]. */
static size_t number_of_leap_years (int year1, int year2) {
	year1--;
	size_t ly1 = year1/4 - year1/100 + year1/400;
	size_t ly2 = year2/4 - year2/100 + year2/400;
	return ly2-ly1;
}


static size_t number_of_days (int year, int month1, int month2) {
	const int mdays[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
	size_t days = is_leap_year(year) ? 366 : 365;
	for (int i=0; i < month1; i++)
		days -= mdays[i];
	for (int i=month2+1; i < 12; i++)
		days -= mdays[i];
	return days;
}


static size_t number_of_days (int year1, int month1, int year2, int month2) {
	size_t days = 0;
	if (year1 == year2)
		days = number_of_days(year1, month1, month2);
	else {
		if (year2-year1 > 1)
			days = (year2-year1-1)*365 + number_of_leap_years(year1+1, year2-1);
		days += number_of_days(year1, month1, 11);
		days += number_of_days(year2, 0, month2);
	}
	return days;
}


/** Returns the number of days spanning the interval from this date up to another one. */
size_t util::Date::operator - (Date date2) const {
	Date date1 = *this;
	if (date2 < date1)
		std::swap(date1, date2);
	size_t days = ::number_of_days(date1._year, date1._month, date2._year, date2._month-1);
	days += date2._day - date1._day + 1;
	return days;
}


bool util::Date::operator < (const Date &date) const {
	if (_year < date._year)	return true;
	if (_year > date._year)	return false;
	if (_month < date._month) return true;
	if (_month > date._month) return false;
	return _day < date._day;
}