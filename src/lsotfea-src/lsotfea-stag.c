/* ANSI-C code produced by gperf version 3.1 */
/* Command-line: gperf -DClt -L ANSI-C -Nlookup_stag -I -Kdict --output-file=lsotfea-stag.c lsotfea-stag.gperf  */
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

#line 1 "lsotfea-stag.gperf"
struct otlex { char * dict; char * mean; };
#include <string.h>

#define TOTAL_KEYWORDS 149
#define MIN_WORD_LENGTH 4
#define MAX_WORD_LENGTH 4
#define MIN_HASH_VALUE 0
#define MAX_HASH_VALUE 719
/* maximum key range = 720, duplicates = 0 */

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
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 135, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      164, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720,   0, 720,
        0, 720, 720, 720, 720, 720,   0, 720, 720, 720,
      720, 720, 720, 720,   0, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720,   5,  40, 230,
       95, 220,  20,  95,  70,   0,  84,  34,   5, 125,
       10, 195,  19, 720,  15,   4,  25, 150, 240, 204,
       70, 230,  10, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720, 720, 720, 720, 720,
      720, 720, 720, 720, 720, 720
    };
  return asso_values[(unsigned char)str[3]] + asso_values[(unsigned char)str[2]] + asso_values[(unsigned char)str[1]] + asso_values[(unsigned char)str[0]];
}

const struct otlex *
lookup_stag (register const char *str, register size_t len)
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
       4,  4,  4,  4,  4,  4,  4,  4,  4
    };
  static const struct otlex wordlist[] =
    {
#line 32 "lsotfea-stag.gperf"
      {"DFLT", "Default"},
#line 71 "lsotfea-stag.gperf"
      {"lina", "Linear A"},
#line 135 "lsotfea-stag.gperf"
      {"lana", "Tai Tham (Lanna)"},
#line 103 "lsotfea-stag.gperf"
      {"ital", "Old Italic"},
#line 62 "lsotfea-stag.gperf"
      {"kali", "Kayah Li"},
#line 68 "lsotfea-stag.gperf"
      {"latn", "Latin"},
#line 9 "lsotfea-stag.gperf"
      {"bali", "Balinese"},
#line 11 "lsotfea-stag.gperf"
      {"bass", "Bassa Vah"},
#line 54 "lsotfea-stag.gperf"
      {"kana", "Hiragana/Katakana"},
#line 72 "lsotfea-stag.gperf"
      {"linb", "Linear B"},
#line 57 "lsotfea-stag.gperf"
      {"prti", "Inscriptional Parthian"},
#line 18 "lsotfea-stag.gperf"
      {"brai", "Braille"},
#line 108 "lsotfea-stag.gperf"
      {"sarb", "Old South Arabian"},
#line 6 "lsotfea-stag.gperf"
      {"arab", "Arabic"},
#line 105 "lsotfea-stag.gperf"
      {"narb", "Old North Arabian"},
#line 137 "lsotfea-stag.gperf"
      {"takr", "Takri"},
#line 95 "lsotfea-stag.gperf"
      {"nbat", "Nabataean"},
#line 126 "lsotfea-stag.gperf"
      {"sinh", "Sinhala"},
#line 28 "lsotfea-stag.gperf"
      {"hani", "CJK Ideographic"},
#line 145 "lsotfea-stag.gperf"
      {"tibt", "Tibetan"},
#line 56 "lsotfea-stag.gperf"
      {"phli", "Inscriptional Pahlavi"},
#line 144 "lsotfea-stag.gperf"
      {"thai", "Thai"},
#line 12 "lsotfea-stag.gperf"
      {"batk", "Batak"},
#line 143 "lsotfea-stag.gperf"
      {"thaa", "Thaana"},
#line 66 "lsotfea-stag.gperf"
      {"sind", "Khudawadi"},
#line 147 "lsotfea-stag.gperf"
      {"tirh", "Tirhuta"},
#line 117 "lsotfea-stag.gperf"
      {"phlp", "Psalter Pahlavi"},
#line 52 "lsotfea-stag.gperf"
      {"hatr", "Hatran"},
#line 63 "lsotfea-stag.gperf"
      {"khar", "Kharosthi"},
#line 43 "lsotfea-stag.gperf"
      {"gran", "Grantha"},
#line 59 "lsotfea-stag.gperf"
      {"kthi", "Kaithi"},
#line 17 "lsotfea-stag.gperf"
      {"brah", "Brahmi"},
#line 87 "lsotfea-stag.gperf"
      {"plrd", "Miao"},
#line 140 "lsotfea-stag.gperf"
      {"tang", "Tangut"},
#line 33 "lsotfea-stag.gperf"
      {"dsrt", "Deseret"},
#line 81 "lsotfea-stag.gperf"
      {"mani", "Manichaean"},
#line 60 "lsotfea-stag.gperf"
      {"knda", "Kannada"},
#line 55 "lsotfea-stag.gperf"
      {"armi", "Imperial Aramaic"},
#line 15 "lsotfea-stag.gperf"
      {"bhks", "Bhaiksuki"},
#line 120 "lsotfea-stag.gperf"
      {"samr", "Samaritan"},
#line 146 "lsotfea-stag.gperf"
      {"tfng", "Tifinagh"},
#line 113 "lsotfea-stag.gperf"
      {"palm", "Palmyrene"},
#line 7 "lsotfea-stag.gperf"
      {"armn", "Armenian"},
#line 73 "lsotfea-stag.gperf"
      {"lisu", "Lisu (Fraser)"},
#line 138 "lsotfea-stag.gperf"
      {"taml", "Tamil"},
#line 133 "lsotfea-stag.gperf"
      {"tagb", "Tagbanwa"},
#line 116 "lsotfea-stag.gperf"
      {"phnx", "Phoenician"},
#line 70 "lsotfea-stag.gperf"
      {"limb", "Limbu"},
#line 121 "lsotfea-stag.gperf"
      {"saur", "Saurashtra"},
#line 49 "lsotfea-stag.gperf"
      {"hang", "Hangul"},
#line 122 "lsotfea-stag.gperf"
      {"shrd", "Sharada"},
#line 97 "lsotfea-stag.gperf"
      {"talu", "New Tai Lue"},
#line 115 "lsotfea-stag.gperf"
      {"phag", "Phags-pa"},
#line 119 "lsotfea-stag.gperf"
      {"runr", "Runic"},
#line 124 "lsotfea-stag.gperf"
      {"sidd", "Siddham"},
#line 41 "lsotfea-stag.gperf"
      {"glag", "Glagolitic"},
#line 118 "lsotfea-stag.gperf"
      {"rjng", "Rejang"},
#line 24 "lsotfea-stag.gperf"
      {"aghb", "Caucasian Albanian"},
#line 127 "lsotfea-stag.gperf"
      {"sora", "Sora Sompeng"},
#line 132 "lsotfea-stag.gperf"
      {"tglg", "Tagalog"},
#line 82 "lsotfea-stag.gperf"
      {"math", "Mathematical Alphanumeric Symbols"},
#line 150 "lsotfea-stag.gperf"
      {"wara", "Warang Citi"},
#line 3 "lsotfea-stag.gperf"
      {"adlm", "Adlam"},
#line 80 "lsotfea-stag.gperf"
      {"mand", "Mandaic, Mandaean"},
#line 64 "lsotfea-stag.gperf"
      {"khmr", "Khmer"},
#line 22 "lsotfea-stag.gperf"
      {"cans", "Canadian Syllabics"},
#line 23 "lsotfea-stag.gperf"
      {"cari", "Carian"},
#line 134 "lsotfea-stag.gperf"
      {"tale", "Tai Le"},
#line 129 "lsotfea-stag.gperf"
      {"sund", "Sundanese"},
#line 148 "lsotfea-stag.gperf"
      {"ugar", "Ugaritic Cuneiform"},
#line 36 "lsotfea-stag.gperf"
      {"dupl", "Duployan"},
#line 38 "lsotfea-stag.gperf"
      {"elba", "Elbasan"},
#line 8 "lsotfea-stag.gperf"
      {"avst", "Avestan"},
#line 51 "lsotfea-stag.gperf"
      {"hano", "Hanunoo"},
#line 123 "lsotfea-stag.gperf"
      {"shaw", "Shavian"},
#line 76 "lsotfea-stag.gperf"
      {"mahj", "Mahajani"},
#line 19 "lsotfea-stag.gperf"
      {"bugi", "Buginese"},
#line 30 "lsotfea-stag.gperf"
      {"cprt", "Cypriot Syllabary"},
#line 128 "lsotfea-stag.gperf"
      {"xsux", "Sumero-Akkadian Cuneiform"},
#line 136 "lsotfea-stag.gperf"
      {"tavt", "Tai Viet"},
#line 112 "lsotfea-stag.gperf"
      {"hmng", "Pahawh Hmong"},
#line 61 "lsotfea-stag.gperf"
      {"knd2", "Kannada v.2"},
#line 91 "lsotfea-stag.gperf"
      {"mult", "Multani"},
#line 14 "lsotfea-stag.gperf"
      {"bng2", "Bengali v.2"},
#line 125 "lsotfea-stag.gperf"
      {"sgnw", "Sign Writing"},
#line 109 "lsotfea-stag.gperf"
      {"orkh", "Old Turkic, Orkhon Runic"},
#line 39 "lsotfea-stag.gperf"
      {"ethi", "Ethiopic"},
#line 139 "lsotfea-stag.gperf"
      {"tml2", "Tamil v.2"},
#line 10 "lsotfea-stag.gperf"
      {"bamu", "Bamum"},
#line 104 "lsotfea-stag.gperf"
      {"hung", "Old Hungarian"},
#line 111 "lsotfea-stag.gperf"
      {"osma", "Osmanya"},
#line 75 "lsotfea-stag.gperf"
      {"lydi", "Lydian"},
#line 58 "lsotfea-stag.gperf"
      {"java", "Javanese"},
#line 67 "lsotfea-stag.gperf"
      {"lao ", "Lao"},
#line 45 "lsotfea-stag.gperf"
      {"gujr", "Gujarati"},
#line 53 "lsotfea-stag.gperf"
      {"hebr", "Hebrew"},
#line 20 "lsotfea-stag.gperf"
      {"buhd", "Buhid"},
#line 46 "lsotfea-stag.gperf"
      {"gjr2", "Gujarati v.2"},
#line 44 "lsotfea-stag.gperf"
      {"grek", "Greek"},
#line 13 "lsotfea-stag.gperf"
      {"beng", "Bengali"},
#line 83 "lsotfea-stag.gperf"
      {"mtei", "Meitei Mayek (Meithei, Meetei)"},
#line 98 "lsotfea-stag.gperf"
      {"nko ", "N'Ko"},
#line 77 "lsotfea-stag.gperf"
      {"marc", "Marchen"},
#line 106 "lsotfea-stag.gperf"
      {"perm", "Old Permic"},
#line 149 "lsotfea-stag.gperf"
      {"vai ", "Vai"},
#line 65 "lsotfea-stag.gperf"
      {"khoj", "Khojki"},
#line 42 "lsotfea-stag.gperf"
      {"goth", "Gothic"},
#line 25 "lsotfea-stag.gperf"
      {"cakm", "Chakma"},
#line 4 "lsotfea-stag.gperf"
      {"ahom", "Ahom"},
#line 141 "lsotfea-stag.gperf"
      {"telu", "Telugu"},
#line 114 "lsotfea-stag.gperf"
      {"pauc", "Pau Cin Hau"},
#line 21 "lsotfea-stag.gperf"
      {"byzm", "Byzantine Music"},
#line 50 "lsotfea-stag.gperf"
      {"jamo", "Hangul Jamo"},
#line 47 "lsotfea-stag.gperf"
      {"guru", "Gurmukhi"},
#line 142 "lsotfea-stag.gperf"
      {"tel2", "Telugu v.2"},
#line 88 "lsotfea-stag.gperf"
      {"modi", "Modi"},
#line 79 "lsotfea-stag.gperf"
      {"mlm2", "Malayalam v.2"},
#line 101 "lsotfea-stag.gperf"
      {"ogam", "Ogham"},
#line 48 "lsotfea-stag.gperf"
      {"gur2", "Gurmukhi v.2"},
#line 89 "lsotfea-stag.gperf"
      {"mong", "Mongolian"},
#line 5 "lsotfea-stag.gperf"
      {"hluw", "Anatolian Hieroglyphs"},
#line 26 "lsotfea-stag.gperf"
      {"cham", "Cham"},
#line 130 "lsotfea-stag.gperf"
      {"sylo", "Syloti Nagri"},
#line 96 "lsotfea-stag.gperf"
      {"newa", "Newa"},
#line 99 "lsotfea-stag.gperf"
      {"orya", "Odia (formerly Oriya)"},
#line 16 "lsotfea-stag.gperf"
      {"bopo", "Bopomofo"},
#line 84 "lsotfea-stag.gperf"
      {"mend", "Mende Kikakui"},
#line 102 "lsotfea-stag.gperf"
      {"olck", "Ol Chiki"},
#line 74 "lsotfea-stag.gperf"
      {"lyci", "Lycian"},
#line 29 "lsotfea-stag.gperf"
      {"copt", "Coptic"},
#line 69 "lsotfea-stag.gperf"
      {"lepc", "Lepcha"},
#line 131 "lsotfea-stag.gperf"
      {"syrc", "Syriac"},
#line 31 "lsotfea-stag.gperf"
      {"cyrl", "Cyrillic"},
#line 78 "lsotfea-stag.gperf"
      {"mlym", "Malayalam"},
#line 93 "lsotfea-stag.gperf"
      {"mymr", "Myanmar"},
#line 151 "lsotfea-stag.gperf"
      {"yi  ", "Yi"},
#line 107 "lsotfea-stag.gperf"
      {"xpeo", "Old Persian Cuneiform"},
#line 92 "lsotfea-stag.gperf"
      {"musc", "Musical Symbols"},
#line 110 "lsotfea-stag.gperf"
      {"osge", "Osage"},
#line 40 "lsotfea-stag.gperf"
      {"geor", "Georgian"},
#line 90 "lsotfea-stag.gperf"
      {"mroo", "Mro"},
#line 27 "lsotfea-stag.gperf"
      {"cher", "Cherokee"},
#line 86 "lsotfea-stag.gperf"
      {"mero", "Meroitic Hieroglyphs"},
#line 34 "lsotfea-stag.gperf"
      {"deva", "Devanagari"},
#line 37 "lsotfea-stag.gperf"
      {"egyp", "Egyptian Hieroglyphs"},
#line 85 "lsotfea-stag.gperf"
      {"merc", "Meroitic Cursive"},
#line 100 "lsotfea-stag.gperf"
      {"ory2", "Odia v.2 (formerly Oriya v.2)"},
#line 94 "lsotfea-stag.gperf"
      {"mym2", "Myanmar v.2"},
#line 35 "lsotfea-stag.gperf"
      {"dev2", "Devanagari v.2"}
    };

  static const short lookup[] =
    {
        0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
        1,  -1,  -1,  -1,  -1,   2,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,   3,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,   4,   5,  -1,  -1,  -1,  -1,
        6,  -1,  -1,   7,   8,   9,  -1,  -1,  -1,  10,
       11,  -1,  -1,  -1,  12,  13,  -1,  -1,  -1,  -1,
       14,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  15,
       16,  -1,  -1,  -1,  17,  18,  -1,  -1,  -1,  -1,
       19,  -1,  -1,  -1,  20,  -1,  -1,  -1,  -1,  -1,
       21,  -1,  -1,  -1,  22,  23,  -1,  -1,  -1,  24,
       25,  -1,  -1,  26,  -1,  27,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  28,  29,  -1,  -1,  -1,  30,
       31,  -1,  -1,  -1,  32,  33,  -1,  -1,  -1,  34,
       35,  -1,  -1,  -1,  36,  37,  -1,  -1,  38,  39,
       40,  -1,  -1,  -1,  41,  42,  -1,  -1,  -1,  43,
       44,  -1,  -1,  -1,  -1,  45,  -1,  -1,  -1,  46,
       47,  -1,  -1,  -1,  48,  -1,  -1,  -1,  -1,  -1,
       49,  -1,  -1,  -1,  50,  51,  -1,  -1,  -1,  52,
       53,  -1,  -1,  -1,  54,  -1,  -1,  -1,  -1,  -1,
       55,  -1,  -1,  -1,  56,  -1,  -1,  -1,  -1,  -1,
       57,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  58,
       59,  -1,  -1,  -1,  -1,  60,  -1,  -1,  -1,  61,
       62,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  64,  -1,  -1,  -1,  -1,  65,
       66,  -1,  -1,  -1,  -1,  67,  -1,  -1,  -1,  68,
       -1,  -1,  -1,  -1,  -1,  69,  -1,  -1,  -1,  70,
       71,  -1,  -1,  -1,  72,  -1,  -1,  -1,  -1,  -1,
       73,  -1,  -1,  74,  75,  76,  -1,  -1,  -1,  77,
       -1,  -1,  -1,  -1,  78,  79,  -1,  -1,  -1,  -1,
       80,  -1,  -1,  81,  -1,  82,  -1,  -1,  -1,  83,
       -1,  -1,  -1,  84,  85,  86,  -1,  -1,  -1,  87,
       88,  -1,  -1,  -1,  -1,  89,  -1,  -1,  -1,  90,
       91,  -1,  -1,  -1,  92,  -1,  -1,  -1,  -1,  -1,
       93,  -1,  -1,  -1,  94,  95,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  96,  -1,  -1,  97,  -1,
       -1,  -1,  -1,  -1,  98,  99,  -1,  -1,  -1,  -1,
      100,  -1,  -1,  -1, 101, 102,  -1,  -1,  -1, 103,
      104,  -1,  -1, 105,  -1, 106,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 107, 108,  -1,  -1,  -1,  -1,
      109,  -1,  -1,  -1, 110, 111,  -1,  -1,  -1, 112,
      113,  -1,  -1,  -1, 114, 115,  -1,  -1,  -1, 116,
      117,  -1,  -1,  -1, 118, 119,  -1,  -1,  -1, 120,
      121,  -1,  -1,  -1, 122,  -1,  -1,  -1,  -1, 123,
       -1,  -1,  -1,  -1,  -1, 124,  -1,  -1,  -1, 125,
      126,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 127, 128,  -1,  -1,  -1, 129,
       -1,  -1,  -1,  -1, 130,  -1,  -1,  -1,  -1, 131,
      132,  -1,  -1,  -1,  -1, 133,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 134,  -1,  -1,  -1,  -1,
      135,  -1,  -1,  -1, 136,  -1,  -1,  -1,  -1, 137,
       -1,  -1,  -1,  -1, 138,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 139,  -1,  -1,  -1,  -1,
      140,  -1,  -1,  -1,  -1, 141,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 142,  -1,  -1,  -1,  -1,
      143,  -1,  -1,  -1, 144,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      145,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 146,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1, 147,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1, 148
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
