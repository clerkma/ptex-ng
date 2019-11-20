/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf -DClt -L ANSI-C -Nlookup_ftag -I -Kdict --output-file=lsotfea-ftag.c lsotfea-ftag.gperf  */
/* Computed positions: -k'1-4' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gperf@gnu.org>."
#endif

#line 1 "lsotfea-ftag.gperf"
struct otlex { char * dict; char * mean; };
#include <string.h>

#define TOTAL_KEYWORDS 237
#define MIN_WORD_LENGTH 4
#define MAX_WORD_LENGTH 4
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 750
/* maximum key range = 747, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
/*ARGSUSED*/
static unsigned int
hash (register const char *str, register size_t len)
{
  static const unsigned short asso_values[] =
    {
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 189,  44,
       50,  30, 195,  24,   4,  90,  70,  10,   0,  95,
        3, 104,  79,  74, 210, 205, 215, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 210,  43,   5,
      164,   5,  10,  60, 190, 234, 105, 125, 125,  40,
       95, 195, 240, 225, 144,   0,   0,   0,  30,   0,
       15, 128,   5,  28, 235, 129, 155, 145,   9, 751,
      751,  65, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751, 751, 751, 751, 751, 751,
      751, 751, 751, 751, 751
    };
  return asso_values[(unsigned char)str[3]] + asso_values[(unsigned char)str[2]+9] + asso_values[(unsigned char)str[1]+1] + asso_values[(unsigned char)str[0]];
}

const struct otlex *
lookup_ftag (register const char *str, register size_t len)
{
  static const unsigned char lengthtable[] =
    {
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4
    };
  static const struct otlex wordlist[] =
    {
#line 213 "lsotfea-ftag.gperf"
      {"ss16", "Stylistic Set 16"},
#line 37 "lsotfea-ftag.gperf"
      {"cv16", "Character Variants 16"},
#line 216 "lsotfea-ftag.gperf"
      {"ss19", "Stylistic Set 19"},
#line 57 "lsotfea-ftag.gperf"
      {"cv36", "Character Variants 36"},
#line 203 "lsotfea-ftag.gperf"
      {"ss06", "Stylistic Set 6"},
#line 40 "lsotfea-ftag.gperf"
      {"cv19", "Character Variants 19"},
#line 60 "lsotfea-ftag.gperf"
      {"cv39", "Character Variants 39"},
#line 27 "lsotfea-ftag.gperf"
      {"cv06", "Character Variants 06"},
#line 206 "lsotfea-ftag.gperf"
      {"ss09", "Stylistic Set 9"},
#line 212 "lsotfea-ftag.gperf"
      {"ss15", "Stylistic Set 15"},
#line 30 "lsotfea-ftag.gperf"
      {"cv09", "Character Variants 09"},
#line 36 "lsotfea-ftag.gperf"
      {"cv15", "Character Variants 15"},
#line 210 "lsotfea-ftag.gperf"
      {"ss13", "Stylistic Set 13"},
#line 56 "lsotfea-ftag.gperf"
      {"cv35", "Character Variants 35"},
#line 202 "lsotfea-ftag.gperf"
      {"ss05", "Stylistic Set 5"},
#line 34 "lsotfea-ftag.gperf"
      {"cv13", "Character Variants 13"},
#line 54 "lsotfea-ftag.gperf"
      {"cv33", "Character Variants 33"},
#line 26 "lsotfea-ftag.gperf"
      {"cv05", "Character Variants 05"},
#line 200 "lsotfea-ftag.gperf"
      {"ss03", "Stylistic Set 3"},
#line 193 "lsotfea-ftag.gperf"
      {"salt", "Stylistic Alternates"},
#line 208 "lsotfea-ftag.gperf"
      {"ss11", "Stylistic Set 11"},
#line 24 "lsotfea-ftag.gperf"
      {"cv03", "Character Variants 03"},
#line 12 "lsotfea-ftag.gperf"
      {"calt", "Contextual Alternates"},
#line 32 "lsotfea-ftag.gperf"
      {"cv11", "Character Variants 11"},
#line 209 "lsotfea-ftag.gperf"
      {"ss12", "Stylistic Set 12"},
#line 52 "lsotfea-ftag.gperf"
      {"cv31", "Character Variants 31"},
#line 128 "lsotfea-ftag.gperf"
      {"falt", "Final Glyph on Line Alternates"},
#line 198 "lsotfea-ftag.gperf"
      {"ss01", "Stylistic Set 1"},
#line 33 "lsotfea-ftag.gperf"
      {"cv12", "Character Variants 12"},
#line 53 "lsotfea-ftag.gperf"
      {"cv32", "Character Variants 32"},
#line 22 "lsotfea-ftag.gperf"
      {"cv01", "Character Variants 01"},
#line 199 "lsotfea-ftag.gperf"
      {"ss02", "Stylistic Set 2"},
#line 21 "lsotfea-ftag.gperf"
      {"curs", "Cursive Positioning"},
#line 23 "lsotfea-ftag.gperf"
      {"cv02", "Character Variants 02"},
#line 232 "lsotfea-ftag.gperf"
      {"vert", "Vertical Writing"},
#line 215 "lsotfea-ftag.gperf"
      {"ss18", "Stylistic Set 18"},
#line 230 "lsotfea-ftag.gperf"
      {"valt", "Alternate Vertical Metrics"},
#line 39 "lsotfea-ftag.gperf"
      {"cv18", "Character Variants 18"},
#line 59 "lsotfea-ftag.gperf"
      {"cv38", "Character Variants 38"},
#line 205 "lsotfea-ftag.gperf"
      {"ss08", "Stylistic Set 8"},
#line 87 "lsotfea-ftag.gperf"
      {"cv66", "Character Variants 66"},
#line 29 "lsotfea-ftag.gperf"
      {"cv08", "Character Variants 08"},
#line 77 "lsotfea-ftag.gperf"
      {"cv56", "Character Variants 56"},
#line 90 "lsotfea-ftag.gperf"
      {"cv69", "Character Variants 69"},
#line 214 "lsotfea-ftag.gperf"
      {"ss17", "Stylistic Set 17"},
#line 11 "lsotfea-ftag.gperf"
      {"blws", "Below-base Substitutions"},
#line 80 "lsotfea-ftag.gperf"
      {"cv59", "Character Variants 59"},
#line 38 "lsotfea-ftag.gperf"
      {"cv17", "Character Variants 17"},
#line 58 "lsotfea-ftag.gperf"
      {"cv37", "Character Variants 37"},
#line 204 "lsotfea-ftag.gperf"
      {"ss07", "Stylistic Set 7"},
#line 9 "lsotfea-ftag.gperf"
      {"blwf", "Below-base Forms"},
#line 86 "lsotfea-ftag.gperf"
      {"cv65", "Character Variants 65"},
#line 47 "lsotfea-ftag.gperf"
      {"cv26", "Character Variants 26"},
#line 28 "lsotfea-ftag.gperf"
      {"cv07", "Character Variants 07"},
#line 76 "lsotfea-ftag.gperf"
      {"cv55", "Character Variants 55"},
#line 84 "lsotfea-ftag.gperf"
      {"cv63", "Character Variants 63"},
#line 50 "lsotfea-ftag.gperf"
      {"cv29", "Character Variants 29"},
#line 67 "lsotfea-ftag.gperf"
      {"cv46", "Character Variants 46"},
#line 74 "lsotfea-ftag.gperf"
      {"cv53", "Character Variants 53"},
#line 194 "lsotfea-ftag.gperf"
      {"sinf", "Scientific Inferiors"},
#line 70 "lsotfea-ftag.gperf"
      {"cv49", "Character Variants 49"},
#line 133 "lsotfea-ftag.gperf"
      {"frac", "Fractions"},
#line 82 "lsotfea-ftag.gperf"
      {"cv61", "Character Variants 61"},
#line 46 "lsotfea-ftag.gperf"
      {"cv25", "Character Variants 25"},
#line 168 "lsotfea-ftag.gperf"
      {"nukt", "Nukta Forms"},
#line 72 "lsotfea-ftag.gperf"
      {"cv51", "Character Variants 51"},
#line 83 "lsotfea-ftag.gperf"
      {"cv62", "Character Variants 62"},
#line 44 "lsotfea-ftag.gperf"
      {"cv23", "Character Variants 23"},
#line 10 "lsotfea-ftag.gperf"
      {"blwm", "Below-base Mark Positioning"},
#line 66 "lsotfea-ftag.gperf"
      {"cv45", "Character Variants 45"},
#line 73 "lsotfea-ftag.gperf"
      {"cv52", "Character Variants 52"},
#line 165 "lsotfea-ftag.gperf"
      {"mset", "Mark Positioning via Substitution"},
#line 166 "lsotfea-ftag.gperf"
      {"nalt", "Alternate Annotation Forms"},
#line 64 "lsotfea-ftag.gperf"
      {"cv43", "Character Variants 43"},
#line 161 "lsotfea-ftag.gperf"
      {"med2", "Medial Forms #2"},
#line 42 "lsotfea-ftag.gperf"
      {"cv21", "Character Variants 21"},
#line 130 "lsotfea-ftag.gperf"
      {"fin3", "Terminal Forms #3"},
#line 147 "lsotfea-ftag.gperf"
      {"jalt", "Justification Alternates"},
#line 89 "lsotfea-ftag.gperf"
      {"cv68", "Character Variants 68"},
#line 43 "lsotfea-ftag.gperf"
      {"cv22", "Character Variants 22"},
#line 62 "lsotfea-ftag.gperf"
      {"cv41", "Character Variants 41"},
#line 79 "lsotfea-ftag.gperf"
      {"cv58", "Character Variants 58"},
#line 220 "lsotfea-ftag.gperf"
      {"subs", "Subscript"},
#line 221 "lsotfea-ftag.gperf"
      {"sups", "Superscript"},
#line 63 "lsotfea-ftag.gperf"
      {"cv42", "Character Variants 42"},
#line 132 "lsotfea-ftag.gperf"
      {"flac", "Flattened accent forms"},
#line 126 "lsotfea-ftag.gperf"
      {"dtls", "Dotless Forms"},
#line 129 "lsotfea-ftag.gperf"
      {"fin2", "Terminal Forms #2"},
#line 121 "lsotfea-ftag.gperf"
      {"c2pc", "Petite Capitals From Capitals"},
#line 88 "lsotfea-ftag.gperf"
      {"cv67", "Character Variants 67"},
#line 49 "lsotfea-ftag.gperf"
      {"cv28", "Character Variants 28"},
#line 78 "lsotfea-ftag.gperf"
      {"cv57", "Character Variants 57"},
#line 195 "lsotfea-ftag.gperf"
      {"size", "Optical size"},
#line 69 "lsotfea-ftag.gperf"
      {"cv48", "Character Variants 48"},
#line 191 "lsotfea-ftag.gperf"
      {"rtlm", "Right-to-left mirrored forms"},
#line 207 "lsotfea-ftag.gperf"
      {"ss10", "Stylistic Set 10"},
#line 48 "lsotfea-ftag.gperf"
      {"cv27", "Character Variants 27"},
#line 159 "lsotfea-ftag.gperf"
      {"ltrm", "Left-to-right mirrored forms"},
#line 31 "lsotfea-ftag.gperf"
      {"cv10", "Character Variants 10"},
#line 211 "lsotfea-ftag.gperf"
      {"ss14", "Stylistic Set 14"},
#line 51 "lsotfea-ftag.gperf"
      {"cv30", "Character Variants 30"},
#line 68 "lsotfea-ftag.gperf"
      {"cv47", "Character Variants 47"},
#line 35 "lsotfea-ftag.gperf"
      {"cv14", "Character Variants 14"},
#line 231 "lsotfea-ftag.gperf"
      {"vatu", "Vattu Variants"},
#line 55 "lsotfea-ftag.gperf"
      {"cv34", "Character Variants 34"},
#line 20 "lsotfea-ftag.gperf"
      {"cswh", "Contextual Swash"},
#line 201 "lsotfea-ftag.gperf"
      {"ss04", "Stylistic Set 4"},
#line 238 "lsotfea-ftag.gperf"
      {"vrt2", "Vertical Alternates and Rotation"},
#line 25 "lsotfea-ftag.gperf"
      {"cv04", "Character Variants 04"},
#line 107 "lsotfea-ftag.gperf"
      {"cv86", "Character Variants 86"},
#line 97 "lsotfea-ftag.gperf"
      {"cv76", "Character Variants 76"},
#line 110 "lsotfea-ftag.gperf"
      {"cv89", "Character Variants 89"},
#line 117 "lsotfea-ftag.gperf"
      {"cv96", "Character Variants 96"},
#line 100 "lsotfea-ftag.gperf"
      {"cv79", "Character Variants 79"},
#line 120 "lsotfea-ftag.gperf"
      {"cv99", "Character Variants 99"},
#line 137 "lsotfea-ftag.gperf"
      {"halt", "Alternate Half Widths"},
#line 106 "lsotfea-ftag.gperf"
      {"cv85", "Character Variants 85"},
#line 173 "lsotfea-ftag.gperf"
      {"ornm", "Ornaments"},
#line 160 "lsotfea-ftag.gperf"
      {"mark", "Mark Positioning"},
#line 239 "lsotfea-ftag.gperf"
      {"zero", "Slashed Zero"},
#line 96 "lsotfea-ftag.gperf"
      {"cv75", "Character Variants 75"},
#line 104 "lsotfea-ftag.gperf"
      {"cv83", "Character Variants 83"},
#line 135 "lsotfea-ftag.gperf"
      {"half", "Half Forms"},
#line 116 "lsotfea-ftag.gperf"
      {"cv95", "Character Variants 95"},
#line 94 "lsotfea-ftag.gperf"
      {"cv73", "Character Variants 73"},
#line 17 "lsotfea-ftag.gperf"
      {"clig", "Contextual Ligatures"},
#line 114 "lsotfea-ftag.gperf"
      {"cv93", "Character Variants 93"},
#line 3 "lsotfea-ftag.gperf"
      {"aalt", "Access All Alternates"},
#line 102 "lsotfea-ftag.gperf"
      {"cv81", "Character Variants 81"},
#line 16 "lsotfea-ftag.gperf"
      {"cjct", "Conjunct Forms"},
#line 218 "lsotfea-ftag.gperf"
      {"ssty", "Math script style alternates"},
#line 152 "lsotfea-ftag.gperf"
      {"kern", "Kerning"},
#line 92 "lsotfea-ftag.gperf"
      {"cv71", "Character Variants 71"},
#line 103 "lsotfea-ftag.gperf"
      {"cv82", "Character Variants 82"},
#line 127 "lsotfea-ftag.gperf"
      {"expt", "Expert Forms"},
#line 112 "lsotfea-ftag.gperf"
      {"cv91", "Character Variants 91"},
#line 93 "lsotfea-ftag.gperf"
      {"cv72", "Character Variants 72"},
#line 81 "lsotfea-ftag.gperf"
      {"cv60", "Character Variants 60"},
#line 227 "lsotfea-ftag.gperf"
      {"trad", "Traditional Forms"},
#line 113 "lsotfea-ftag.gperf"
      {"cv92", "Character Variants 92"},
#line 71 "lsotfea-ftag.gperf"
      {"cv50", "Character Variants 50"},
#line 85 "lsotfea-ftag.gperf"
      {"cv64", "Character Variants 64"},
#line 122 "lsotfea-ftag.gperf"
      {"c2sc", "Small Capitals From Capitals"},
#line 236 "lsotfea-ftag.gperf"
      {"vkrn", "Vertical Kerning"},
#line 75 "lsotfea-ftag.gperf"
      {"cv54", "Character Variants 54"},
#line 109 "lsotfea-ftag.gperf"
      {"cv88", "Character Variants 88"},
#line 174 "lsotfea-ftag.gperf"
      {"palt", "Proportional Alternate Widths"},
#line 217 "lsotfea-ftag.gperf"
      {"ss20", "Stylistic Set 20"},
#line 99 "lsotfea-ftag.gperf"
      {"cv78", "Character Variants 78"},
#line 13 "lsotfea-ftag.gperf"
      {"case", "Case-Sensitive Forms"},
#line 41 "lsotfea-ftag.gperf"
      {"cv20", "Character Variants 20"},
#line 119 "lsotfea-ftag.gperf"
      {"cv98", "Character Variants 98"},
#line 45 "lsotfea-ftag.gperf"
      {"cv24", "Character Variants 24"},
#line 61 "lsotfea-ftag.gperf"
      {"cv40", "Character Variants 40"},
#line 169 "lsotfea-ftag.gperf"
      {"numr", "Numerators"},
#line 108 "lsotfea-ftag.gperf"
      {"cv87", "Character Variants 87"},
#line 7 "lsotfea-ftag.gperf"
      {"afrc", "Alternative Fractions"},
#line 65 "lsotfea-ftag.gperf"
      {"cv44", "Character Variants 44"},
#line 98 "lsotfea-ftag.gperf"
      {"cv77", "Character Variants 77"},
#line 186 "lsotfea-ftag.gperf"
      {"rkrf", "Rakar Forms"},
#line 185 "lsotfea-ftag.gperf"
      {"rclt", "Required Contextual Alternates"},
#line 118 "lsotfea-ftag.gperf"
      {"cv97", "Character Variants 97"},
#line 15 "lsotfea-ftag.gperf"
      {"cfar", "Conjunct Form After Ro"},
#line 219 "lsotfea-ftag.gperf"
      {"stch", "Stretching Glyph Decomposition"},
#line 164 "lsotfea-ftag.gperf"
      {"mkmk", "Mark to Mark Positioning"},
#line 228 "lsotfea-ftag.gperf"
      {"twid", "Third Widths"},
#line 162 "lsotfea-ftag.gperf"
      {"medi", "Medial Forms"},
#line 131 "lsotfea-ftag.gperf"
      {"fina", "Terminal Forms"},
#line 136 "lsotfea-ftag.gperf"
      {"haln", "Halant Forms"},
#line 172 "lsotfea-ftag.gperf"
      {"ordn", "Ordinals"},
#line 134 "lsotfea-ftag.gperf"
      {"fwid", "Full Widths"},
#line 179 "lsotfea-ftag.gperf"
      {"pres", "Pre-base Substitutions"},
#line 225 "lsotfea-ftag.gperf"
      {"tnam", "Traditional Name Forms"},
#line 229 "lsotfea-ftag.gperf"
      {"unic", "Unicase"},
#line 178 "lsotfea-ftag.gperf"
      {"pref", "Pre-Base Forms"},
#line 197 "lsotfea-ftag.gperf"
      {"smpl", "Simplified Forms"},
#line 224 "lsotfea-ftag.gperf"
      {"tjmo", "Trailing Jamo Forms"},
#line 184 "lsotfea-ftag.gperf"
      {"rand", "Randomize"},
#line 190 "lsotfea-ftag.gperf"
      {"rtla", "Right-to-left alternates"},
#line 18 "lsotfea-ftag.gperf"
      {"cpct", "Centered CJK Punctuation"},
#line 223 "lsotfea-ftag.gperf"
      {"titl", "Titling"},
#line 6 "lsotfea-ftag.gperf"
      {"abvs", "Above-base Substitutions"},
#line 158 "lsotfea-ftag.gperf"
      {"ltra", "Left-to-right alternates"},
#line 235 "lsotfea-ftag.gperf"
      {"vkna", "Vertical Kana Alternates"},
#line 181 "lsotfea-ftag.gperf"
      {"psts", "Post-base Substitutions"},
#line 4 "lsotfea-ftag.gperf"
      {"abvf", "Above-base Forms"},
#line 145 "lsotfea-ftag.gperf"
      {"isol", "Isolated Forms"},
#line 180 "lsotfea-ftag.gperf"
      {"pstf", "Post-base Forms"},
#line 234 "lsotfea-ftag.gperf"
      {"vjmo", "Vowel Jamo Forms"},
#line 163 "lsotfea-ftag.gperf"
      {"mgrk", "Mathematical Greek"},
#line 167 "lsotfea-ftag.gperf"
      {"nlck", "NLC Kanji Forms"},
#line 187 "lsotfea-ftag.gperf"
      {"rlig", "Required Ligatures"},
#line 226 "lsotfea-ftag.gperf"
      {"tnum", "Tabular Figures"},
#line 101 "lsotfea-ftag.gperf"
      {"cv80", "Character Variants 80"},
#line 5 "lsotfea-ftag.gperf"
      {"abvm", "Above-base Mark Positioning"},
#line 91 "lsotfea-ftag.gperf"
      {"cv70", "Character Variants 70"},
#line 105 "lsotfea-ftag.gperf"
      {"cv84", "Character Variants 84"},
#line 124 "lsotfea-ftag.gperf"
      {"dlig", "Discretionary Ligatures"},
#line 111 "lsotfea-ftag.gperf"
      {"cv90", "Character Variants 90"},
#line 95 "lsotfea-ftag.gperf"
      {"cv74", "Character Variants 74"},
#line 125 "lsotfea-ftag.gperf"
      {"dnom", "Denominators"},
#line 115 "lsotfea-ftag.gperf"
      {"cv94", "Character Variants 94"},
#line 192 "lsotfea-ftag.gperf"
      {"ruby", "Ruby Notation Forms"},
#line 189 "lsotfea-ftag.gperf"
      {"rtbd", "Right Bounds"},
#line 140 "lsotfea-ftag.gperf"
      {"hlig", "Historical Ligatures"},
#line 14 "lsotfea-ftag.gperf"
      {"ccmp", "Glyph Composition / Decomposition"},
#line 222 "lsotfea-ftag.gperf"
      {"swsh", "Swash"},
#line 196 "lsotfea-ftag.gperf"
      {"smcp", "Small Capitals"},
#line 146 "lsotfea-ftag.gperf"
      {"ital", "Italics"},
#line 153 "lsotfea-ftag.gperf"
      {"lfbd", "Left Bounds"},
#line 155 "lsotfea-ftag.gperf"
      {"ljmo", "Leading Jamo Forms"},
#line 237 "lsotfea-ftag.gperf"
      {"vpal", "Proportional Alternate Vertical Metrics"},
#line 233 "lsotfea-ftag.gperf"
      {"vhal", "Alternate Vertical Half Metrics"},
#line 123 "lsotfea-ftag.gperf"
      {"dist", "Distances"},
#line 143 "lsotfea-ftag.gperf"
      {"hwid", "Half Widths"},
#line 156 "lsotfea-ftag.gperf"
      {"lnum", "Lining Figures"},
#line 139 "lsotfea-ftag.gperf"
      {"hkna", "Horizontal Kana Alternates"},
#line 138 "lsotfea-ftag.gperf"
      {"hist", "Historical Forms"},
#line 151 "lsotfea-ftag.gperf"
      {"jp04", "JIS2004 Forms"},
#line 183 "lsotfea-ftag.gperf"
      {"qwid", "Quarter Widths"},
#line 182 "lsotfea-ftag.gperf"
      {"pwid", "Proportional Widths"},
#line 149 "lsotfea-ftag.gperf"
      {"jp83", "JIS83 Forms"},
#line 144 "lsotfea-ftag.gperf"
      {"init", "Initial Forms"},
#line 176 "lsotfea-ftag.gperf"
      {"pkna", "Proportional Kana"},
#line 170 "lsotfea-ftag.gperf"
      {"onum", "Oldstyle Figures"},
#line 188 "lsotfea-ftag.gperf"
      {"rphf", "Reph Forms"},
#line 148 "lsotfea-ftag.gperf"
      {"jp78", "JIS78 Forms"},
#line 157 "lsotfea-ftag.gperf"
      {"locl", "Localized Forms"},
#line 142 "lsotfea-ftag.gperf"
      {"hojo", "Hojo Kanji Forms (JIS X 0212-1990 Kanji Forms)"},
#line 177 "lsotfea-ftag.gperf"
      {"pnum", "Proportional Figures"},
#line 8 "lsotfea-ftag.gperf"
      {"akhn", "Akhands"},
#line 154 "lsotfea-ftag.gperf"
      {"liga", "Standard Ligatures"},
#line 19 "lsotfea-ftag.gperf"
      {"cpsp", "Capital Spacing"},
#line 171 "lsotfea-ftag.gperf"
      {"opbd", "Optical Bounds"},
#line 150 "lsotfea-ftag.gperf"
      {"jp90", "JIS90 Forms"},
#line 175 "lsotfea-ftag.gperf"
      {"pcap", "Petite Capitals"},
#line 141 "lsotfea-ftag.gperf"
      {"hngl", "Hangul"}
    };

  static const short lookup[] =
    {
       -1,  -1,  -1,  -1,   0,  -1,  -1,  -1,  -1,   1,
        2,  -1,   3,  -1,   4,   5,  -1,  -1,   6,   7,
        8,  -1,  -1,  -1,   9,  10,  -1,  -1,  -1,  11,
       12,  -1,  13,  -1,  14,  15,  -1,  -1,  16,  17,
       18,  -1,  -1,  19,  20,  21,  -1,  -1,  22,  23,
       24,  -1,  25,  26,  27,  28,  -1,  -1,  29,  30,
       31,  -1,  -1,  32,  -1,  33,  -1,  -1,  34,  -1,
       35,  -1,  -1,  36,  -1,  37,  -1,  -1,  38,  -1,
       39,  -1,  -1,  40,  -1,  41,  -1,  -1,  42,  43,
       44,  -1,  45,  -1,  46,  47,  -1,  -1,  48,  -1,
       49,  -1,  50,  51,  52,  53,  -1,  -1,  54,  55,
       56,  -1,  -1,  57,  58,  59,  -1,  -1,  -1,  60,
       61,  -1,  -1,  62,  63,  64,  -1,  -1,  65,  66,
       67,  -1,  68,  69,  70,  71,  -1,  -1,  72,  73,
       74,  -1,  -1,  -1,  75,  76,  -1,  -1,  77,  78,
       79,  -1,  -1,  80,  81,  82,  -1,  -1,  83,  84,
       85,  -1,  -1,  -1,  86,  87,  -1,  -1,  88,  89,
       90,  -1,  -1,  -1,  91,  92,  -1,  -1,  -1,  93,
       -1,  -1,  -1,  -1,  94,  -1,  -1,  -1,  -1,  95,
       96,  -1,  -1,  97,  98,  99,  -1, 100,  -1, 101,
      102,  -1, 103, 104, 105, 106,  -1,  -1,  -1, 107,
      108,  -1,  -1,  -1, 109,  -1,  -1,  -1,  -1, 110,
      111,  -1,  -1,  -1, 112, 113,  -1,  -1,  -1,  -1,
      114,  -1,  -1, 115, 116, 117, 118,  -1, 119, 120,
      121,  -1,  -1, 122, 123, 124,  -1,  -1,  -1, 125,
      126,  -1,  -1, 127, 128, 129,  -1, 130, 131, 132,
      133, 134,  -1,  -1, 135, 136,  -1,  -1, 137, 138,
      139,  -1,  -1, 140, 141, 142,  -1,  -1, 143, 144,
      145,  -1,  -1, 146, 147, 148,  -1,  -1, 149, 150,
      151,  -1,  -1,  -1,  -1, 152,  -1,  -1, 153, 154,
      155,  -1,  -1, 156, 157, 158,  -1, 159, 160,  -1,
      161,  -1,  -1,  -1, 162, 163,  -1,  -1,  -1,  -1,
      164,  -1,  -1, 165, 166, 167,  -1,  -1, 168,  -1,
      169,  -1,  -1, 170,  -1, 171,  -1,  -1,  -1,  -1,
      172,  -1,  -1,  -1, 173, 174,  -1,  -1, 175,  -1,
      176, 177,  -1,  -1, 178, 179,  -1,  -1,  -1, 180,
      181,  -1,  -1, 182,  -1, 183,  -1,  -1,  -1, 184,
      185,  -1,  -1,  -1, 186,  -1,  -1,  -1,  -1, 187,
      188,  -1,  -1, 189,  -1, 190,  -1,  -1, 191,  -1,
      192,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 193,
      194,  -1,  -1,  -1, 195, 196,  -1,  -1, 197, 198,
      199,  -1,  -1,  -1, 200, 201,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 202,  -1,  -1,
       -1,  -1,  -1, 203, 204,  -1,  -1,  -1,  -1, 205,
      206,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      207,  -1,  -1,  -1, 208,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 209, 210,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 211,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 212,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 213,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 214,  -1, 215,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 216,  -1,  -1,  -1,  -1,
      217,  -1,  -1,  -1,  -1, 218,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 219,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 220,  -1, 221,  -1,  -1,  -1,  -1,
       -1,  -1,  -1, 222,  -1, 223,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 224,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 225,  -1,  -1,  -1,  -1,  -1,
      226,  -1,  -1,  -1,  -1, 227,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 228,  -1,  -1,  -1,  -1,
      229,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 230,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      231,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 232,  -1,  -1,  -1, 233,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 234,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 235,
      236
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register int index = lookup[key];

          if (index >= 0)
            {
              if (len == lengthtable[index])
                {
                  register const char *s = wordlist[index].dict;

                  if (*str == *s && !memcmp (str + 1, s + 1, len - 1))
                    return &wordlist[index];
                }
            }
        }
    }
  return 0;
}
