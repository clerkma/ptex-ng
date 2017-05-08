/* ANSI-C code produced by gperf version 3.0.4 */
/* Command-line: gperf -DClt -L ANSI-C -Nlookup_ltag -I -Kdict --output-file=lsotfea-ltag.c lsotfea-ltag.gperf  */
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
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 1 "lsotfea-ltag.gperf"
struct otlex { char * dict; char * mean; };
#include <string.h>

#define TOTAL_KEYWORDS 583
#define MIN_WORD_LENGTH 4
#define MAX_WORD_LENGTH 4
#define MIN_HASH_VALUE 50
#define MAX_HASH_VALUE 2775
/* maximum key range = 2726, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
/*ARGSUSED*/
static unsigned int
hash (register const char *str, register unsigned int len)
{
  static const unsigned short asso_values[] =
    {
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776,    0, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,   46, 2776,
      2776, 2776, 2776, 2776,   10, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,  120,   66,
       441,   86,  816,   95,  416,  440,   70,  476,  351,  426,
       995,  865,  205,  530,   40,  515,  855,  920,   45,  931,
         1,  925,   47,  640,   21,   11,   15,  535,  245,   41,
         5,  765,  260,  465,    0,  225,   20,  270,  130,  935,
       580,   10,   25,  160,    2,   30,   31,  585,   12,  375,
       960,  610,  310,   11,  490,  980,  815, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776, 2776,
      2776, 2776
    };
  return asso_values[(unsigned char)str[3]] + asso_values[(unsigned char)str[2]+22] + asso_values[(unsigned char)str[1]+3] + asso_values[(unsigned char)str[0]+36];
}

#ifdef __GNUC__
__inline
#if defined __GNUC_STDC_INLINE__ || defined __GNUC_GNU_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
const struct otlex *
lookup_ltag (register const char *str, register unsigned int len)
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
#line 120 "lsotfea-ltag.gperf"
      {"DUN ", "Dungan"},
#line 401 "lsotfea-ltag.gperf"
      {"NYN ", "Norwegian Nynorsk (Nynorsk, Norwegian)"},
#line 284 "lsotfea-ltag.gperf"
      {"KUR ", "Kurdish"},
#line 119 "lsotfea-ltag.gperf"
      {"DUJ ", "Dhuwal"},
#line 455 "lsotfea-ltag.gperf"
      {"RUN ", "Rundi"},
#line 286 "lsotfea-ltag.gperf"
      {"KUY ", "Kuy"},
#line 281 "lsotfea-ltag.gperf"
      {"KUI ", "Kui"},
#line 246 "lsotfea-ltag.gperf"
      {"KIR ", "Kirghiz (Kyrgyz)"},
#line 400 "lsotfea-ltag.gperf"
      {"NYM ", "Nyamwezi"},
#line 318 "lsotfea-ltag.gperf"
      {"LUH ", "Luyia"},
#line 141 "lsotfea-ltag.gperf"
      {"FIN ", "Finnish"},
#line 283 "lsotfea-ltag.gperf"
      {"KUM ", "Kumyk"},
#line 301 "lsotfea-ltag.gperf"
      {"LIN ", "Lingala"},
#line 446 "lsotfea-ltag.gperf"
      {"RIT ", "Ritarungo"},
#line 299 "lsotfea-ltag.gperf"
      {"LIJ ", "Ligurian"},
#line 316 "lsotfea-ltag.gperf"
      {"LUB ", "Luba-Katanga"},
#line 103 "lsotfea-ltag.gperf"
      {"DAR ", "Dargwa"},
#line 422 "lsotfea-ltag.gperf"
      {"PIH ", "Norfolk"},
#line 102 "lsotfea-ltag.gperf"
      {"DAN ", "Danish"},
#line 372 "lsotfea-ltag.gperf"
      {"NAN ", "Nanai"},
#line 445 "lsotfea-ltag.gperf"
      {"RIF ", "Tarifit"},
#line 230 "lsotfea-ltag.gperf"
      {"KAR ", "Karachay"},
#line 559 "lsotfea-ltag.gperf"
      {"WAR ", "Waray-Waray"},
#line 440 "lsotfea-ltag.gperf"
      {"RAR ", "Rarotongan"},
#line 378 "lsotfea-ltag.gperf"
      {"NDB ", "Ndebele"},
#line 229 "lsotfea-ltag.gperf"
      {"KAN ", "Kannada"},
#line 300 "lsotfea-ltag.gperf"
      {"LIM ", "Limburgish"},
#line 139 "lsotfea-ltag.gperf"
      {"FAR ", "Persian"},
#line 557 "lsotfea-ltag.gperf"
      {"WA  ", "Wa"},
#line 371 "lsotfea-ltag.gperf"
      {"NAH ", "Nahuatl"},
#line 137 "lsotfea-ltag.gperf"
      {"FAN ", "French Antillean"},
#line 289 "lsotfea-ltag.gperf"
      {"LAD ", "Ladin"},
#line 439 "lsotfea-ltag.gperf"
      {"RAJ ", "Rajasthani"},
#line 231 "lsotfea-ltag.gperf"
      {"KAT ", "Georgian"},
#line 232 "lsotfea-ltag.gperf"
      {"KAZ ", "Kazakh"},
#line 412 "lsotfea-ltag.gperf"
      {"PAN ", "Punjabi"},
#line 140 "lsotfea-ltag.gperf"
      {"FAT ", "Fanti"},
#line 294 "lsotfea-ltag.gperf"
      {"LAT ", "Latin"},
#line 290 "lsotfea-ltag.gperf"
      {"LAH ", "Lahuli"},
#line 295 "lsotfea-ltag.gperf"
      {"LAZ ", "Laz"},
#line 225 "lsotfea-ltag.gperf"
      {"KAB ", "Kabardian"},
#line 196 "lsotfea-ltag.gperf"
      {"HUN ", "Hungarian"},
#line 292 "lsotfea-ltag.gperf"
      {"LAM ", "Lambani"},
#line 138 "lsotfea-ltag.gperf"
      {"FAN0", "Fang"},
#line 411 "lsotfea-ltag.gperf"
      {"PAM ", "Pampangan"},
#line 154 "lsotfea-ltag.gperf"
      {"FUV ", "Nigerian Fulfulde"},
#line 112 "lsotfea-ltag.gperf"
      {"DIV ", "Divehi (Dhivehi, Maldivian)"},
#line 185 "lsotfea-ltag.gperf"
      {"HBN ", "Hammer-Banna"},
#line 188 "lsotfea-ltag.gperf"
      {"HIN ", "Hindi"},
#line 394 "lsotfea-ltag.gperf"
      {"NOR ", "Norwegian"},
#line 364 "lsotfea-ltag.gperf"
      {"MUN ", "Mundari"},
#line 368 "lsotfea-ltag.gperf"
      {"MYN ", "Mayan"},
#line 265 "lsotfea-ltag.gperf"
      {"KOR ", "Korean"},
#line 258 "lsotfea-ltag.gperf"
      {"KOD ", "Kodagu"},
#line 261 "lsotfea-ltag.gperf"
      {"KON ", "Kikongo"},
#line 226 "lsotfea-ltag.gperf"
      {"KAB0", "Kabyle"},
#line 451 "lsotfea-ltag.gperf"
      {"ROY ", "Romany"},
#line 145 "lsotfea-ltag.gperf"
      {"FON ", "Fon"},
#line 332 "lsotfea-ltag.gperf"
      {"MBN ", "Mbundu"},
#line 340 "lsotfea-ltag.gperf"
      {"MIN ", "Minangkabau"},
#line 259 "lsotfea-ltag.gperf"
      {"KOH ", "Korean Old Hangul"},
#line 267 "lsotfea-ltag.gperf"
      {"KOZ ", "Komi-Zyrian"},
#line 429 "lsotfea-ltag.gperf"
      {"PON ", "Pohnpeian"},
#line 336 "lsotfea-ltag.gperf"
      {"MDR ", "Mandar"},
#line 180 "lsotfea-ltag.gperf"
      {"HAR ", "Harauti"},
#line 376 "lsotfea-ltag.gperf"
      {"NAV ", "Navajo"},
#line 341 "lsotfea-ltag.gperf"
      {"MIZ ", "Mizo"},
#line 262 "lsotfea-ltag.gperf"
      {"KOM ", "Komi"},
#line 428 "lsotfea-ltag.gperf"
      {"POH ", "Pocomchi"},
#line 450 "lsotfea-ltag.gperf"
      {"ROM ", "Romanian"},
#line 183 "lsotfea-ltag.gperf"
      {"HAY ", "Haya"},
#line 178 "lsotfea-ltag.gperf"
      {"HAI ", "Haitian (Haitian Creole)"},
#line 263 "lsotfea-ltag.gperf"
      {"KON0", "Kongo"},
#line 309 "lsotfea-ltag.gperf"
      {"LOM ", "Loma"},
#line 184 "lsotfea-ltag.gperf"
      {"HAZ ", "Hazaragi"},
#line 330 "lsotfea-ltag.gperf"
      {"MAR ", "Marathi"},
#line 321 "lsotfea-ltag.gperf"
      {"MAD ", "Madura"},
#line 457 "lsotfea-ltag.gperf"
      {"RUS ", "Russian"},
#line 328 "lsotfea-ltag.gperf"
      {"MAN ", "Mansi"},
#line 324 "lsotfea-ltag.gperf"
      {"MAJ ", "Majang"},
#line 387 "lsotfea-ltag.gperf"
      {"NIS ", "Nisi"},
#line 282 "lsotfea-ltag.gperf"
      {"KUL ", "Kulvi"},
#line 323 "lsotfea-ltag.gperf"
      {"MAH ", "Marshallese"},
#line 122 "lsotfea-ltag.gperf"
      {"EBI ", "Ebira"},
#line 247 "lsotfea-ltag.gperf"
      {"KIS ", "Kisii"},
#line 153 "lsotfea-ltag.gperf"
      {"FUL ", "Fulah"},
#line 381 "lsotfea-ltag.gperf"
      {"NDS ", "Low Saxon"},
#line 456 "lsotfea-ltag.gperf"
      {"RUP ", "Aromanian"},
#line 302 "lsotfea-ltag.gperf"
      {"LIS ", "Lisu"},
#line 327 "lsotfea-ltag.gperf"
      {"MAM ", "Mam"},
#line 285 "lsotfea-ltag.gperf"
      {"KUU ", "Kurukh"},
#line 38 "lsotfea-ltag.gperf"
      {"BBR ", "Berber"},
#line 288 "lsotfea-ltag.gperf"
      {"KYU ", "Western Kayah"},
#line 176 "lsotfea-ltag.gperf"
      {"GUJ ", "Gujarati"},
#line 125 "lsotfea-ltag.gperf"
      {"EFI ", "Efik"},
#line 395 "lsotfea-ltag.gperf"
      {"NOV ", "Novial"},
#line 177 "lsotfea-ltag.gperf"
      {"GUZ ", "Gusii"},
#line 388 "lsotfea-ltag.gperf"
      {"NIU ", "Niuean"},
#line 193 "lsotfea-ltag.gperf"
      {"HO  ", "Ho"},
#line 423 "lsotfea-ltag.gperf"
      {"PIL ", "Filipino"},
#line 374 "lsotfea-ltag.gperf"
      {"NAS ", "Naskapi"},
#line 441 "lsotfea-ltag.gperf"
      {"RBU ", "Russian Buriat"},
#line 248 "lsotfea-ltag.gperf"
      {"KIU ", "Kirmanjki"},
#line 41 "lsotfea-ltag.gperf"
      {"BDY ", "Bandjalang"},
#line 162 "lsotfea-ltag.gperf"
      {"GIH ", "Githabul"},
#line 175 "lsotfea-ltag.gperf"
      {"GUF ", "Gupapuyngu"},
#line 359 "lsotfea-ltag.gperf"
      {"MOR ", "Moroccan"},
#line 251 "lsotfea-ltag.gperf"
      {"KKN ", "Kokni"},
#line 358 "lsotfea-ltag.gperf"
      {"MON ", "Mon"},
#line 228 "lsotfea-ltag.gperf"
      {"KAL ", "Kalenjin"},
#line 415 "lsotfea-ltag.gperf"
      {"PAS ", "Pashto"},
#line 35 "lsotfea-ltag.gperf"
      {"BAR ", "Bavarian"},
#line 30 "lsotfea-ltag.gperf"
      {"BAD ", "Badaga"},
#line 373 "lsotfea-ltag.gperf"
      {"NAP ", "Neapolitan"},
#line 34 "lsotfea-ltag.gperf"
      {"BAN ", "Balinese"},
#line 355 "lsotfea-ltag.gperf"
      {"MOH ", "Mohawk"},
#line 304 "lsotfea-ltag.gperf"
      {"LKI ", "Laki"},
#line 159 "lsotfea-ltag.gperf"
      {"GAR ", "Garshuni"},
#line 155 "lsotfea-ltag.gperf"
      {"GAD ", "Ga"},
#line 375 "lsotfea-ltag.gperf"
      {"NAU ", "Nauruan"},
#line 410 "lsotfea-ltag.gperf"
      {"PAL ", "Pali"},
#line 554 "lsotfea-ltag.gperf"
      {"VIT ", "Vietnamese"},
#line 504 "lsotfea-ltag.gperf"
      {"SWZ ", "Swati"},
#line 366 "lsotfea-ltag.gperf"
      {"MWL ", "Mirandese"},
#line 413 "lsotfea-ltag.gperf"
      {"PAP ", "Palpa"},
#line 108 "lsotfea-ltag.gperf"
      {"DGR ", "Dogri"},
#line 385 "lsotfea-ltag.gperf"
      {"NGR ", "Nagari"},
#line 499 "lsotfea-ltag.gperf"
      {"SUR ", "Suri"},
#line 416 "lsotfea-ltag.gperf"
      {"PAU ", "Palauan"},
#line 508 "lsotfea-ltag.gperf"
      {"SYR ", "Syriac"},
#line 498 "lsotfea-ltag.gperf"
      {"SUN ", "Sundanese"},
#line 489 "lsotfea-ltag.gperf"
      {"SQI ", "Albanian"},
#line 31 "lsotfea-ltag.gperf"
      {"BAD0", "Banda"},
#line 391 "lsotfea-ltag.gperf"
      {"NLD ", "Dutch"},
#line 365 "lsotfea-ltag.gperf"
      {"MUS ", "Muscogee"},
#line 266 "lsotfea-ltag.gperf"
      {"KOS ", "Kosraean"},
#line 105 "lsotfea-ltag.gperf"
      {"DCR ", "Woods Cree"},
#line 562 "lsotfea-ltag.gperf"
      {"WLN ", "Walloon"},
#line 377 "lsotfea-ltag.gperf"
      {"NCR ", "N-Cree"},
#line 187 "lsotfea-ltag.gperf"
      {"HIL ", "Hiligaynon"},
#line 473 "lsotfea-ltag.gperf"
      {"SID ", "Sidamo"},
#line 420 "lsotfea-ltag.gperf"
      {"PGR ", "Polytonic Greek"},
#line 146 "lsotfea-ltag.gperf"
      {"FOS ", "Faroese"},
#line 560 "lsotfea-ltag.gperf"
      {"WCR ", "West-Cree"},
#line 442 "lsotfea-ltag.gperf"
      {"RCR ", "R-Cree"},
#line 414 "lsotfea-ltag.gperf"
      {"PAP0", "Papiamentu"},
#line 296 "lsotfea-ltag.gperf"
      {"LCR ", "L-Cree"},
#line 242 "lsotfea-ltag.gperf"
      {"KHT ", "Khamti Shan"},
#line 403 "lsotfea-ltag.gperf"
      {"OCR ", "Oji-Cree"},
#line 418 "lsotfea-ltag.gperf"
      {"PCD ", "Picard"},
#line 264 "lsotfea-ltag.gperf"
      {"KOP ", "Komi-Permyak"},
#line 113 "lsotfea-ltag.gperf"
      {"DJR ", "Zarma"},
#line 252 "lsotfea-ltag.gperf"
      {"KLM ", "Kalmyk"},
#line 170 "lsotfea-ltag.gperf"
      {"GON ", "Gondi"},
#line 402 "lsotfea-ltag.gperf"
      {"OCI ", "Occitan"},
#line 563 "lsotfea-ltag.gperf"
      {"WLF ", "Wolof"},
#line 249 "lsotfea-ltag.gperf"
      {"KJD ", "Southern Kiwai"},
#line 472 "lsotfea-ltag.gperf"
      {"SIB ", "Sibe"},
#line 240 "lsotfea-ltag.gperf"
      {"KHM ", "Khmer"},
#line 179 "lsotfea-ltag.gperf"
      {"HAL ", "Halam"},
#line 458 "lsotfea-ltag.gperf"
      {"SAD ", "Sadri"},
#line 459 "lsotfea-ltag.gperf"
      {"SAN ", "Sanskrit"},
#line 462 "lsotfea-ltag.gperf"
      {"SAY ", "Sayisi"},
#line 142 "lsotfea-ltag.gperf"
      {"FJI ", "Fijian"},
#line 343 "lsotfea-ltag.gperf"
      {"MKR ", "Makasar"},
#line 342 "lsotfea-ltag.gperf"
      {"MKD ", "Macedonian"},
#line 461 "lsotfea-ltag.gperf"
      {"SAT ", "Santali"},
#line 181 "lsotfea-ltag.gperf"
      {"HAU ", "Hausa"},
#line 114 "lsotfea-ltag.gperf"
      {"DJR0", "Djambarrpuyngu"},
#line 326 "lsotfea-ltag.gperf"
      {"MAL ", "Malayalam"},
#line 118 "lsotfea-ltag.gperf"
      {"DRI ", "Dari"},
#line 273 "lsotfea-ltag.gperf"
      {"KRN ", "Karen"},
#line 111 "lsotfea-ltag.gperf"
      {"DIQ ", "Dimli"},
#line 269 "lsotfea-ltag.gperf"
      {"KRI ", "Krio"},
#line 329 "lsotfea-ltag.gperf"
      {"MAP ", "Mapudungun"},
#line 274 "lsotfea-ltag.gperf"
      {"KRT ", "Koorete"},
#line 149 "lsotfea-ltag.gperf"
      {"FRI ", "Frisian"},
#line 404 "lsotfea-ltag.gperf"
      {"OJB ", "Ojibway"},
#line 52 "lsotfea-ltag.gperf"
      {"BIS ", "Bislama"},
#line 405 "lsotfea-ltag.gperf"
      {"ORI ", "Odia (formerly Oriya)"},
#line 564 "lsotfea-ltag.gperf"
      {"XBD ", "Lü"},
#line 272 "lsotfea-ltag.gperf"
      {"KRM ", "Karaim"},
#line 110 "lsotfea-ltag.gperf"
      {"DHV ", "Divehi (Dhivehi, Maldivian) (deprecated)"},
#line 51 "lsotfea-ltag.gperf"
      {"BIL ", "Bilen"},
#line 243 "lsotfea-ltag.gperf"
      {"KHV ", "Khanty-Vakhi"},
#line 163 "lsotfea-ltag.gperf"
      {"GIL ", "Gilyak"},
#line 348 "lsotfea-ltag.gperf"
      {"MLR ", "Malayalam Reformed"},
#line 360 "lsotfea-ltag.gperf"
      {"MOS ", "Mossi"},
#line 347 "lsotfea-ltag.gperf"
      {"MLN ", "Malinke"},
#line 287 "lsotfea-ltag.gperf"
      {"KYK ", "Koryak"},
#line 349 "lsotfea-ltag.gperf"
      {"MLY ", "Malay"},
#line 389 "lsotfea-ltag.gperf"
      {"NKL ", "Nyankole"},
#line 488 "lsotfea-ltag.gperf"
      {"SOT ", "Sotho, Southern"},
#line 334 "lsotfea-ltag.gperf"
      {"MCR ", "Moose Cree"},
#line 73 "lsotfea-ltag.gperf"
      {"CAT ", "Catalan"},
#line 438 "lsotfea-ltag.gperf"
      {"QWH ", "Quechua (Peru)"},
#line 357 "lsotfea-ltag.gperf"
      {"MOL ", "Moldavian"},
#line 245 "lsotfea-ltag.gperf"
      {"KIK ", "Kikuyu (Gikuyu)"},
#line 333 "lsotfea-ltag.gperf"
      {"MCH ", "Manchu"},
#line 33 "lsotfea-ltag.gperf"
      {"BAL ", "Balkar"},
#line 223 "lsotfea-ltag.gperf"
      {"JUD ", "Ladino"},
#line 164 "lsotfea-ltag.gperf"
      {"GIL0", "Kiribati (Gilbertese)"},
#line 158 "lsotfea-ltag.gperf"
      {"GAL ", "Galician"},
#line 121 "lsotfea-ltag.gperf"
      {"DZN ", "Dzongkha"},
#line 297 "lsotfea-ltag.gperf"
      {"LDK ", "Ladakhi"},
#line 36 "lsotfea-ltag.gperf"
      {"BAU ", "Baulé"},
#line 435 "lsotfea-ltag.gperf"
      {"QUH ", "Quechua (Bolivia)"},
#line 436 "lsotfea-ltag.gperf"
      {"QUZ ", "Quechua"},
#line 194 "lsotfea-ltag.gperf"
      {"HRI ", "Harari"},
#line 54 "lsotfea-ltag.gperf"
      {"BKF ", "Blackfoot"},
#line 433 "lsotfea-ltag.gperf"
      {"QIN ", "Chin"},
#line 222 "lsotfea-ltag.gperf"
      {"JII ", "Yiddish"},
#line 123 "lsotfea-ltag.gperf"
      {"ECR ", "Eastern Cree"},
#line 507 "lsotfea-ltag.gperf"
      {"SYL ", "Sylheti"},
#line 91 "lsotfea-ltag.gperf"
      {"COR ", "Cornish"},
#line 241 "lsotfea-ltag.gperf"
      {"KHS ", "Khanty-Shurishkar"},
#line 47 "lsotfea-ltag.gperf"
      {"BGR ", "Bulgarian"},
#line 291 "lsotfea-ltag.gperf"
      {"LAK ", "Lak"},
#line 361 "lsotfea-ltag.gperf"
      {"MRI ", "Maori"},
#line 61 "lsotfea-ltag.gperf"
      {"BOS ", "Bosnian"},
#line 57 "lsotfea-ltag.gperf"
      {"BLN ", "Balante"},
#line 55 "lsotfea-ltag.gperf"
      {"BLI ", "Baluchi"},
#line 104 "lsotfea-ltag.gperf"
      {"DAX ", "Dayi"},
#line 40 "lsotfea-ltag.gperf"
      {"BCR ", "Bible Cree"},
#line 219 "lsotfea-ltag.gperf"
      {"JAN ", "Japanese"},
#line 58 "lsotfea-ltag.gperf"
      {"BLT ", "Balti"},
#line 317 "lsotfea-ltag.gperf"
      {"LUG ", "Ganda"},
#line 48 "lsotfea-ltag.gperf"
      {"BHI ", "Bhili"},
#line 460 "lsotfea-ltag.gperf"
      {"SAS ", "Sasak"},
#line 39 "lsotfea-ltag.gperf"
      {"BCH ", "Bench"},
#line 380 "lsotfea-ltag.gperf"
      {"NDG ", "Ndonga"},
#line 133 "lsotfea-ltag.gperf"
      {"EUQ ", "Basque"},
#line 476 "lsotfea-ltag.gperf"
      {"SKY ", "Slovak"},
#line 545 "lsotfea-ltag.gperf"
      {"UDM ", "Udmurt"},
#line 218 "lsotfea-ltag.gperf"
      {"JAM ", "Jamaican Creole"},
#line 250 "lsotfea-ltag.gperf"
      {"KJP ", "Eastern Pwo Karen"},
#line 260 "lsotfea-ltag.gperf"
      {"KOK ", "Konkani"},
#line 53 "lsotfea-ltag.gperf"
      {"BJJ ", "Kanauji"},
#line 555 "lsotfea-ltag.gperf"
      {"VOL ", "Volapük"},
#line 303 "lsotfea-ltag.gperf"
      {"LJP ", "Lampung"},
#line 370 "lsotfea-ltag.gperf"
      {"NAG ", "Naga-Assamese"},
#line 129 "lsotfea-ltag.gperf"
      {"ERZ ", "Erzya"},
#line 271 "lsotfea-ltag.gperf"
      {"KRL ", "Karelian"},
#line 558 "lsotfea-ltag.gperf"
      {"WAG ", "Wagdi"},
#line 195 "lsotfea-ltag.gperf"
      {"HRV ", "Croatian"},
#line 150 "lsotfea-ltag.gperf"
      {"FRL ", "Friulian"},
#line 268 "lsotfea-ltag.gperf"
      {"KPL ", "Kpelle"},
#line 65 "lsotfea-ltag.gperf"
      {"BRI ", "Braj Bhasha"},
#line 171 "lsotfea-ltag.gperf"
      {"GRN ", "Greenlandic"},
#line 409 "lsotfea-ltag.gperf"
      {"PAG ", "Pangasinan"},
#line 151 "lsotfea-ltag.gperf"
      {"FRP ", "Arpitan"},
#line 64 "lsotfea-ltag.gperf"
      {"BRH ", "Brahui"},
#line 432 "lsotfea-ltag.gperf"
      {"PWO ", "Western Pwo Karen"},
#line 62 "lsotfea-ltag.gperf"
      {"BPY ", "Bishnupriya Manipuri"},
#line 369 "lsotfea-ltag.gperf"
      {"MZN ", "Mazanderani"},
#line 325 "lsotfea-ltag.gperf"
      {"MAK ", "Makhuwa"},
#line 66 "lsotfea-ltag.gperf"
      {"BRM ", "Burmese"},
#line 471 "lsotfea-ltag.gperf"
      {"SHN ", "Shan"},
#line 463 "lsotfea-ltag.gperf"
      {"SCN ", "Sicilian"},
#line 470 "lsotfea-ltag.gperf"
      {"SHI ", "Tachelhit"},
#line 220 "lsotfea-ltag.gperf"
      {"JAV ", "Javanese"},
#line 319 "lsotfea-ltag.gperf"
      {"LUO ", "Luo"},
#line 487 "lsotfea-ltag.gperf"
      {"SOP ", "Songe"},
#line 443 "lsotfea-ltag.gperf"
      {"REJ ", "Rejang"},
#line 393 "lsotfea-ltag.gperf"
      {"NOG ", "Nogai"},
#line 27 "lsotfea-ltag.gperf"
      {"AYM ", "Aymara"},
#line 9 "lsotfea-ltag.gperf"
      {"AFR ", "Afar"},
#line 7 "lsotfea-ltag.gperf"
      {"ADY ", "Adyghe"},
#line 50 "lsotfea-ltag.gperf"
      {"BIK ", "Bikol"},
#line 298 "lsotfea-ltag.gperf"
      {"LEZ ", "Lezgi"},
#line 224 "lsotfea-ltag.gperf"
      {"JUL ", "Jula"},
#line 235 "lsotfea-ltag.gperf"
      {"KEB ", "Kebena"},
#line 257 "lsotfea-ltag.gperf"
      {"KNR ", "Kanuri"},
#line 277 "lsotfea-ltag.gperf"
      {"KSI ", "Khasi"},
#line 452 "lsotfea-ltag.gperf"
      {"RSY ", "Rusyn"},
#line 116 "lsotfea-ltag.gperf"
      {"DNJ ", "Dan"},
#line 165 "lsotfea-ltag.gperf"
      {"GKP ", "Kpelle (Guinea)"},
#line 275 "lsotfea-ltag.gperf"
      {"KSH ", "Kashmiri"},
#line 493 "lsotfea-ltag.gperf"
      {"SRR ", "Serer"},
#line 491 "lsotfea-ltag.gperf"
      {"SRD ", "Sardinian"},
#line 92 "lsotfea-ltag.gperf"
      {"COS ", "Corsican"},
#line 126 "lsotfea-ltag.gperf"
      {"ELL ", "Greek"},
#line 396 "lsotfea-ltag.gperf"
      {"NSM ", "Northern Sami"},
#line 356 "lsotfea-ltag.gperf"
      {"MOK ", "Moksha"},
#line 84 "lsotfea-ltag.gperf"
      {"CHR ", "Cherokee"},
#line 278 "lsotfea-ltag.gperf"
      {"KSM ", "Kildin Sami"},
#line 293 "lsotfea-ltag.gperf"
      {"LAO ", "Lao"},
#line 503 "lsotfea-ltag.gperf"
      {"SWK ", "Swahili"},
#line 87 "lsotfea-ltag.gperf"
      {"CHY ", "Cheyenne"},
#line 79 "lsotfea-ltag.gperf"
      {"CHI ", "Chichewa (Chewa, Nyanja)"},
#line 322 "lsotfea-ltag.gperf"
      {"MAG ", "Magahi"},
#line 312 "lsotfea-ltag.gperf"
      {"LSM ", "Lule Sami"},
#line 311 "lsotfea-ltag.gperf"
      {"LSB ", "Lower Sorbian"},
#line 78 "lsotfea-ltag.gperf"
      {"CHH ", "Chattisgarhi"},
#line 90 "lsotfea-ltag.gperf"
      {"COP ", "Coptic"},
#line 478 "lsotfea-ltag.gperf"
      {"SLV ", "Slovenian"},
#line 276 "lsotfea-ltag.gperf"
      {"KSH0", "Ripuarian"},
#line 490 "lsotfea-ltag.gperf"
      {"SRB ", "Serbian"},
#line 217 "lsotfea-ltag.gperf"
      {"IWR ", "Hebrew"},
#line 427 "lsotfea-ltag.gperf"
      {"PNB ", "Western Panjabi"},
#line 71 "lsotfea-ltag.gperf"
      {"BUG ", "Bugis"},
#line 186 "lsotfea-ltag.gperf"
      {"HER ", "Herero"},
#line 475 "lsotfea-ltag.gperf"
      {"SKS ", "Skolt Sami"},
#line 497 "lsotfea-ltag.gperf"
      {"SUK ", "Sukuma"},
#line 546 "lsotfea-ltag.gperf"
      {"UKR ", "Ukrainian"},
#line 313 "lsotfea-ltag.gperf"
      {"LTH ", "Lithuanian"},
#line 320 "lsotfea-ltag.gperf"
      {"LVI ", "Latvian"},
#line 314 "lsotfea-ltag.gperf"
      {"LTZ ", "Luxembourgish"},
#line 541 "lsotfea-ltag.gperf"
      {"TWI ", "Twi"},
#line 453 "lsotfea-ltag.gperf"
      {"RTM ", "Rotuman"},
#line 280 "lsotfea-ltag.gperf"
      {"KUA ", "Kuanyama"},
#line 338 "lsotfea-ltag.gperf"
      {"MER ", "Meru"},
#line 454 "lsotfea-ltag.gperf"
      {"RUA ", "Kinyarwanda"},
#line 95 "lsotfea-ltag.gperf"
      {"CRR ", "Carrier"},
#line 337 "lsotfea-ltag.gperf"
      {"MEN ", "Me'en"},
#line 239 "lsotfea-ltag.gperf"
      {"KHK ", "Khanty-Kazim"},
#line 315 "lsotfea-ltag.gperf"
      {"LUA ", "Luba-Lulua"},
#line 425 "lsotfea-ltag.gperf"
      {"PLK ", "Polish"},
#line 192 "lsotfea-ltag.gperf"
      {"HND ", "Hindko"},
#line 96 "lsotfea-ltag.gperf"
      {"CRT ", "Crimean Tatar"},
#line 444 "lsotfea-ltag.gperf"
      {"RIA ", "Riang"},
#line 421 "lsotfea-ltag.gperf"
      {"PHK ", "Phake"},
#line 254 "lsotfea-ltag.gperf"
      {"KMN ", "Kumaoni"},
#line 566 "lsotfea-ltag.gperf"
      {"XJB ", "Minjangbal"},
#line 469 "lsotfea-ltag.gperf"
      {"SGS ", "Samogitian"},
#line 449 "lsotfea-ltag.gperf"
      {"RMY ", "Vlax Romani"},
#line 379 "lsotfea-ltag.gperf"
      {"NDC ", "Ndau"},
#line 32 "lsotfea-ltag.gperf"
      {"BAG ", "Baghelkhandi"},
#line 233 "lsotfea-ltag.gperf"
      {"KDE ", "Makonde"},
#line 350 "lsotfea-ltag.gperf"
      {"MND ", "Mandinka"},
#line 157 "lsotfea-ltag.gperf"
      {"GAG ", "Gagauz"},
#line 542 "lsotfea-ltag.gperf"
      {"TYZ ", "Tày"},
#line 352 "lsotfea-ltag.gperf"
      {"MNI ", "Manipuri"},
#line 101 "lsotfea-ltag.gperf"
      {"CUK ", "San Blas Kuna"},
#line 200 "lsotfea-ltag.gperf"
      {"IBB ", "Ibibio"},
#line 515 "lsotfea-ltag.gperf"
      {"TDD ", "Dehong Dai"},
#line 419 "lsotfea-ltag.gperf"
      {"PDC ", "Pennsylvania German"},
#line 253 "lsotfea-ltag.gperf"
      {"KMB ", "Kamba"},
#line 270 "lsotfea-ltag.gperf"
      {"KRK ", "Karakalpak"},
#line 227 "lsotfea-ltag.gperf"
      {"KAC ", "Kachchi"},
#line 306 "lsotfea-ltag.gperf"
      {"LMB ", "Limbu"},
#line 74 "lsotfea-ltag.gperf"
      {"CBK ", "Zamboanga Chavacano"},
#line 408 "lsotfea-ltag.gperf"
      {"PAA ", "Palestinian Aramaic"},
#line 561 "lsotfea-ltag.gperf"
      {"WEL ", "Welsh"},
#line 524 "lsotfea-ltag.gperf"
      {"TIB ", "Tibetan"},
#line 124 "lsotfea-ltag.gperf"
      {"EDO ", "Edo"},
#line 382 "lsotfea-ltag.gperf"
      {"NEP ", "Nepali"},
#line 109 "lsotfea-ltag.gperf"
      {"DHG ", "Dhangu"},
#line 44 "lsotfea-ltag.gperf"
      {"BEN ", "Bengali"},
#line 474 "lsotfea-ltag.gperf"
      {"SIG ", "Silte Gurage"},
#line 106 "lsotfea-ltag.gperf"
      {"DEU ", "German"},
#line 575 "lsotfea-ltag.gperf"
      {"YIM ", "Yi Modern"},
#line 511 "lsotfea-ltag.gperf"
      {"TAJ ", "Tajiki"},
#line 367 "lsotfea-ltag.gperf"
      {"MWW ", "Hmong Daw"},
#line 424 "lsotfea-ltag.gperf"
      {"PLG ", "Palaung"},
#line 513 "lsotfea-ltag.gperf"
      {"TAT ", "Tatar"},
#line 362 "lsotfea-ltag.gperf"
      {"MTH ", "Maithili"},
#line 197 "lsotfea-ltag.gperf"
      {"HYE ", "Armenian"},
#line 584 "lsotfea-ltag.gperf"
      {"ZUL ", "Zulu"},
#line 407 "lsotfea-ltag.gperf"
      {"OSS ", "Ossetian"},
#line 161 "lsotfea-ltag.gperf"
      {"GEZ ", "Ge'ez"},
#line 169 "lsotfea-ltag.gperf"
      {"GOG ", "Gogo"},
#line 43 "lsotfea-ltag.gperf"
      {"BEM ", "Bemba"},
#line 390 "lsotfea-ltag.gperf"
      {"NKO ", "N'Ko"},
#line 72 "lsotfea-ltag.gperf"
      {"CAK ", "Kaqchikel"},
#line 512 "lsotfea-ltag.gperf"
      {"TAM ", "Tamil"},
#line 510 "lsotfea-ltag.gperf"
      {"TAB ", "Tabasaran"},
#line 548 "lsotfea-ltag.gperf"
      {"URD ", "Urdu"},
#line 190 "lsotfea-ltag.gperf"
      {"HMN ", "Hmong"},
#line 553 "lsotfea-ltag.gperf"
      {"VEN ", "Venda"},
#line 392 "lsotfea-ltag.gperf"
      {"NOE ", "Nimadi"},
#line 539 "lsotfea-ltag.gperf"
      {"TUV ", "Tuvin"},
#line 68 "lsotfea-ltag.gperf"
      {"BSH ", "Bashkir"},
#line 168 "lsotfea-ltag.gperf"
      {"GNN ", "Gumatj"},
#line 46 "lsotfea-ltag.gperf"
      {"BGQ ", "Bagri"},
#line 198 "lsotfea-ltag.gperf"
      {"HYE0", "Armenian East"},
#line 136 "lsotfea-ltag.gperf"
      {"EWE ", "Ewe"},
#line 565 "lsotfea-ltag.gperf"
      {"XHS ", "Xhosa"},
#line 132 "lsotfea-ltag.gperf"
      {"ETI ", "Estonian"},
#line 525 "lsotfea-ltag.gperf"
      {"TIV ", "Tiv"},
#line 135 "lsotfea-ltag.gperf"
      {"EVN ", "Even"},
#line 83 "lsotfea-ltag.gperf"
      {"CHP ", "Chipewyan"},
#line 532 "lsotfea-ltag.gperf"
      {"TOD ", "Todo"},
#line 335 "lsotfea-ltag.gperf"
      {"MDE ", "Mende"},
#line 86 "lsotfea-ltag.gperf"
      {"CHU ", "Chuvash"},
#line 339 "lsotfea-ltag.gperf"
      {"MFE ", "Morisyen"},
#line 107 "lsotfea-ltag.gperf"
      {"DGO ", "Dogri"},
#line 182 "lsotfea-ltag.gperf"
      {"HAW ", "Hawaiian"},
#line 69 "lsotfea-ltag.gperf"
      {"BTI ", "Beti"},
#line 6 "lsotfea-ltag.gperf"
      {"ACR ", "Achi "},
#line 14 "lsotfea-ltag.gperf"
      {"ALT ", "Altai"},
#line 533 "lsotfea-ltag.gperf"
      {"TOD0", "Toma"},
#line 331 "lsotfea-ltag.gperf"
      {"MAW ", "Marwari"},
#line 486 "lsotfea-ltag.gperf"
      {"SOG ", "Sodo Gurage"},
#line 56 "lsotfea-ltag.gperf"
      {"BLK ", "Pa'o Karen"},
#line 346 "lsotfea-ltag.gperf"
      {"MLG ", "Malagasy"},
#line 5 "lsotfea-ltag.gperf"
      {"ACH ", "Acholi"},
#line 256 "lsotfea-ltag.gperf"
      {"KMS ", "Komso"},
#line 166 "lsotfea-ltag.gperf"
      {"GLK ", "Gilaki"},
#line 448 "lsotfea-ltag.gperf"
      {"RMS ", "Romansh"},
#line 173 "lsotfea-ltag.gperf"
      {"GUA ", "Guarani"},
#line 483 "lsotfea-ltag.gperf"
      {"SND ", "Sindhi"},
#line 174 "lsotfea-ltag.gperf"
      {"GUC ", "Wayuu"},
#line 538 "lsotfea-ltag.gperf"
      {"TUL ", "Tulu"},
#line 426 "lsotfea-ltag.gperf"
      {"PMS ", "Piemontese"},
#line 577 "lsotfea-ltag.gperf"
      {"ZGH ", "Standard Morrocan Tamazigh"},
#line 93 "lsotfea-ltag.gperf"
      {"CPP ", "Creoles"},
#line 37 "lsotfea-ltag.gperf"
      {"BBC ", "Batak Toba"},
#line 509 "lsotfea-ltag.gperf"
      {"SZL ", "Silesian"},
#line 484 "lsotfea-ltag.gperf"
      {"SNH ", "Sinhala (Sinhalese)"},
#line 495 "lsotfea-ltag.gperf"
      {"SSM ", "Southern Sami"},
#line 582 "lsotfea-ltag.gperf"
      {"ZHT ", "Chinese Traditional"},
#line 579 "lsotfea-ltag.gperf"
      {"ZHH ", "Chinese, Hong Kong SAR"},
#line 167 "lsotfea-ltag.gperf"
      {"GMZ ", "Gumuz"},
#line 20 "lsotfea-ltag.gperf"
      {"ARI ", "Aari"},
#line 551 "lsotfea-ltag.gperf"
      {"UZB ", "Uzbek"},
#line 502 "lsotfea-ltag.gperf"
      {"SWA ", "Swadaya Aramaic"},
#line 550 "lsotfea-ltag.gperf"
      {"UYG ", "Uyghur"},
#line 447 "lsotfea-ltag.gperf"
      {"RKW ", "Arakwal"},
#line 59 "lsotfea-ltag.gperf"
      {"BMB ", "Bambara (Bamanankan)"},
#line 363 "lsotfea-ltag.gperf"
      {"MTS ", "Maltese"},
#line 406 "lsotfea-ltag.gperf"
      {"ORO ", "Oromo"},
#line 430 "lsotfea-ltag.gperf"
      {"PRO ", "Provencal"},
#line 156 "lsotfea-ltag.gperf"
      {"GAE ", "Scottish Gaelic (Gaelic)"},
#line 505 "lsotfea-ltag.gperf"
      {"SXT ", "Sutu"},
#line 42 "lsotfea-ltag.gperf"
      {"BEL ", "Belarussian"},
#line 160 "lsotfea-ltag.gperf"
      {"GAW ", "Garhwali"},
#line 75 "lsotfea-ltag.gperf"
      {"CEB ", "Cebuano"},
#line 99 "lsotfea-ltag.gperf"
      {"CSY ", "Czech"},
#line 567 "lsotfea-ltag.gperf"
      {"XOG ", "Soga"},
#line 384 "lsotfea-ltag.gperf"
      {"NGA ", "Ngbaka"},
#line 130 "lsotfea-ltag.gperf"
      {"ESP ", "Spanish"},
#line 131 "lsotfea-ltag.gperf"
      {"ESU ", "Central Yupik"},
#line 237 "lsotfea-ltag.gperf"
      {"KGE ", "Khutsuri Georgian"},
#line 526 "lsotfea-ltag.gperf"
      {"TKM ", "Turkmen"},
#line 67 "lsotfea-ltag.gperf"
      {"BRX ", "Bodo"},
#line 571 "lsotfea-ltag.gperf"
      {"YAP ", "Yapese"},
#line 236 "lsotfea-ltag.gperf"
      {"KEK ", "Kekchi"},
#line 97 "lsotfea-ltag.gperf"
      {"CSB ", "Kashubian"},
#line 4 "lsotfea-ltag.gperf"
      {"ABK ", "Abkhazian"},
#line 238 "lsotfea-ltag.gperf"
      {"KHA ", "Khakass"},
#line 143 "lsotfea-ltag.gperf"
      {"FLE ", "Dutch (Flemish)"},
#line 386 "lsotfea-ltag.gperf"
      {"NHC ", "Norway House Cree"},
#line 520 "lsotfea-ltag.gperf"
      {"TGR ", "Tigre"},
#line 519 "lsotfea-ltag.gperf"
      {"TGN ", "Tongan"},
#line 244 "lsotfea-ltag.gperf"
      {"KHW ", "Khowar"},
#line 521 "lsotfea-ltag.gperf"
      {"TGY ", "Tigrinya"},
#line 8 "lsotfea-ltag.gperf"
      {"AFK ", "Afrikaans"},
#line 117 "lsotfea-ltag.gperf"
      {"DNK ", "Dinka"},
#line 514 "lsotfea-ltag.gperf"
      {"TCR ", "TH-Cree"},
#line 417 "lsotfea-ltag.gperf"
      {"PCC ", "Bouyei"},
#line 70 "lsotfea-ltag.gperf"
      {"BTS ", "Batak Simalungun"},
#line 221 "lsotfea-ltag.gperf"
      {"JBO ", "Lojban"},
#line 13 "lsotfea-ltag.gperf"
      {"ALS ", "Alsatian"},
#line 523 "lsotfea-ltag.gperf"
      {"THT ", "Tahitian"},
#line 573 "lsotfea-ltag.gperf"
      {"YCR ", "Y-Cree"},
#line 492 "lsotfea-ltag.gperf"
      {"SRK ", "Saraiki"},
#line 466 "lsotfea-ltag.gperf"
      {"SEL ", "Selkup"},
#line 80 "lsotfea-ltag.gperf"
      {"CHK ", "Chukchi"},
#line 344 "lsotfea-ltag.gperf"
      {"MKW ", "Kituba"},
#line 28 "lsotfea-ltag.gperf"
      {"AZB ", "Torki"},
#line 147 "lsotfea-ltag.gperf"
      {"FRA ", "French"},
#line 89 "lsotfea-ltag.gperf"
      {"CMR ", "Comorian"},
#line 212 "lsotfea-ltag.gperf"
      {"IRI ", "Irish"},
#line 49 "lsotfea-ltag.gperf"
      {"BHO ", "Bhojpuri"},
#line 148 "lsotfea-ltag.gperf"
      {"FRC ", "Cajun French"},
#line 213 "lsotfea-ltag.gperf"
      {"IRT ", "Irish Traditional"},
#line 310 "lsotfea-ltag.gperf"
      {"LRC ", "Luri"},
#line 494 "lsotfea-ltag.gperf"
      {"SSL ", "South Slavey"},
#line 581 "lsotfea-ltag.gperf"
      {"ZHS ", "Chinese Simplified"},
#line 81 "lsotfea-ltag.gperf"
      {"CHK0", "Chuukese"},
#line 60 "lsotfea-ltag.gperf"
      {"BML ", "Bamileke"},
#line 534 "lsotfea-ltag.gperf"
      {"TPI ", "Tok Pisin"},
#line 115 "lsotfea-ltag.gperf"
      {"DNG ", "Dangme"},
#line 549 "lsotfea-ltag.gperf"
      {"USB ", "Upper Sorbian"},
#line 345 "lsotfea-ltag.gperf"
      {"MLE ", "Male"},
#line 580 "lsotfea-ltag.gperf"
      {"ZHP ", "Chinese Phonetic"},
#line 88 "lsotfea-ltag.gperf"
      {"CGG ", "Chiga"},
#line 437 "lsotfea-ltag.gperf"
      {"QVI ", "Quechua (Ecuador)"},
#line 77 "lsotfea-ltag.gperf"
      {"CHG ", "Chaha Gurage"},
#line 172 "lsotfea-ltag.gperf"
      {"GRO ", "Garo"},
#line 468 "lsotfea-ltag.gperf"
      {"SGO ", "Sango"},
#line 353 "lsotfea-ltag.gperf"
      {"MNK ", "Maninka"},
#line 434 "lsotfea-ltag.gperf"
      {"QUC ", "K’iche’"},
#line 98 "lsotfea-ltag.gperf"
      {"CSL ", "Church Slavonic"},
#line 506 "lsotfea-ltag.gperf"
      {"SXU ", "Upper Saxon"},
#line 464 "lsotfea-ltag.gperf"
      {"SCO ", "Scots"},
#line 556 "lsotfea-ltag.gperf"
      {"VRO ", "V?ro"},
#line 431 "lsotfea-ltag.gperf"
      {"PTG ", "Portuguese"},
#line 11 "lsotfea-ltag.gperf"
      {"AIO ", "Aiton"},
#line 354 "lsotfea-ltag.gperf"
      {"MNX ", "Manx"},
#line 45 "lsotfea-ltag.gperf"
      {"BGC ", "Haryanvi"},
#line 479 "lsotfea-ltag.gperf"
      {"SML ", "Somali"},
#line 518 "lsotfea-ltag.gperf"
      {"TGL ", "Tagalog"},
#line 397 "lsotfea-ltag.gperf"
      {"NSO ", "Sotho, Northern"},
#line 569 "lsotfea-ltag.gperf"
      {"YAK ", "Sakha"},
#line 23 "lsotfea-ltag.gperf"
      {"AST ", "Asturian"},
#line 543 "lsotfea-ltag.gperf"
      {"TZM ", "Tamazight"},
#line 547 "lsotfea-ltag.gperf"
      {"UMB ", "Umbundu"},
#line 22 "lsotfea-ltag.gperf"
      {"ASM ", "Assamese"},
#line 351 "lsotfea-ltag.gperf"
      {"MNG ", "Mongolian"},
#line 82 "lsotfea-ltag.gperf"
      {"CHO ", "Choctaw"},
#line 583 "lsotfea-ltag.gperf"
      {"ZND ", "Zande"},
#line 26 "lsotfea-ltag.gperf"
      {"AWA ", "Awadhi"},
#line 399 "lsotfea-ltag.gperf"
      {"NTO ", "Esperanto"},
#line 134 "lsotfea-ltag.gperf"
      {"EVK ", "Evenki"},
#line 25 "lsotfea-ltag.gperf"
      {"AVR ", "Avar"},
#line 63 "lsotfea-ltag.gperf"
      {"BRE ", "Breton"},
#line 24 "lsotfea-ltag.gperf"
      {"ATH ", "Athapaskan"},
#line 467 "lsotfea-ltag.gperf"
      {"SGA ", "Old Irish"},
#line 477 "lsotfea-ltag.gperf"
      {"SLA ", "Slavey"},
#line 465 "lsotfea-ltag.gperf"
      {"SEK ", "Sekota"},
#line 128 "lsotfea-ltag.gperf"
      {"ENG ", "English"},
#line 234 "lsotfea-ltag.gperf"
      {"KEA ", "Kabuverdianu (Crioulo)"},
#line 3 "lsotfea-ltag.gperf"
      {"ABA ", "Abaza"},
#line 383 "lsotfea-ltag.gperf"
      {"NEW ", "Newari"},
#line 127 "lsotfea-ltag.gperf"
      {"EMK ", "Eastern Maninkakan"},
#line 496 "lsotfea-ltag.gperf"
      {"STQ ", "Saterland Frisian"},
#line 201 "lsotfea-ltag.gperf"
      {"IBO ", "Igbo"},
#line 255 "lsotfea-ltag.gperf"
      {"KMO ", "Komo"},
#line 485 "lsotfea-ltag.gperf"
      {"SNK ", "Soninke"},
#line 15 "lsotfea-ltag.gperf"
      {"AMH ", "Amharic"},
#line 307 "lsotfea-ltag.gperf"
      {"LMO ", "Lombard"},
#line 203 "lsotfea-ltag.gperf"
      {"IDO ", "Ido"},
#line 517 "lsotfea-ltag.gperf"
      {"TET ", "Tetum"},
#line 279 "lsotfea-ltag.gperf"
      {"KSW ", "S’gaw Karen"},
#line 207 "lsotfea-ltag.gperf"
      {"IND ", "Indonesian"},
#line 144 "lsotfea-ltag.gperf"
      {"FNE ", "Forest Nenets"},
#line 21 "lsotfea-ltag.gperf"
      {"ARK ", "Rakhine"},
#line 85 "lsotfea-ltag.gperf"
      {"CHA ", "Chamorro"},
#line 76 "lsotfea-ltag.gperf"
      {"CHE ", "Chechen"},
#line 215 "lsotfea-ltag.gperf"
      {"ISM ", "Inari Sami"},
#line 398 "lsotfea-ltag.gperf"
      {"NTA ", "Northern Tai"},
#line 152 "lsotfea-ltag.gperf"
      {"FTA ", "Futa"},
#line 570 "lsotfea-ltag.gperf"
      {"YAO ", "Yao"},
#line 191 "lsotfea-ltag.gperf"
      {"HMO ", "Hiri Motu"},
#line 94 "lsotfea-ltag.gperf"
      {"CRE ", "Cree"},
#line 19 "lsotfea-ltag.gperf"
      {"ARG ", "Aragonese"},
#line 199 "lsotfea-ltag.gperf"
      {"IBA ", "Iban"},
#line 537 "lsotfea-ltag.gperf"
      {"TUA ", "Turoyo Aramaic"},
#line 305 "lsotfea-ltag.gperf"
      {"LMA ", "Low Mari"},
#line 568 "lsotfea-ltag.gperf"
      {"XPE ", "Kpelle (Liberia)"},
#line 308 "lsotfea-ltag.gperf"
      {"LMW ", "Lomwe"},
#line 528 "lsotfea-ltag.gperf"
      {"TMN ", "Temne"},
#line 572 "lsotfea-ltag.gperf"
      {"YBA ", "Yoruba"},
#line 527 "lsotfea-ltag.gperf"
      {"TMH ", "Tamashek"},
#line 574 "lsotfea-ltag.gperf"
      {"YIC ", "Yi Classic"},
#line 210 "lsotfea-ltag.gperf"
      {"IPK ", "Inupiat"},
#line 535 "lsotfea-ltag.gperf"
      {"TRK ", "Turkish"},
#line 516 "lsotfea-ltag.gperf"
      {"TEL ", "Telugu"},
#line 100 "lsotfea-ltag.gperf"
      {"CTG ", "Chittagonian"},
#line 214 "lsotfea-ltag.gperf"
      {"ISL ", "Icelandic"},
#line 12 "lsotfea-ltag.gperf"
      {"AKA ", "Akan"},
#line 189 "lsotfea-ltag.gperf"
      {"HMA ", "High Mari"},
#line 552 "lsotfea-ltag.gperf"
      {"VEC ", "Venetian"},
#line 209 "lsotfea-ltag.gperf"
      {"INU ", "Inuktitut"},
#line 10 "lsotfea-ltag.gperf"
      {"AGW ", "Agaw"},
#line 205 "lsotfea-ltag.gperf"
      {"ILO ", "Ilokano"},
#line 480 "lsotfea-ltag.gperf"
      {"SMO ", "Samoan"},
#line 540 "lsotfea-ltag.gperf"
      {"TVL ", "Tuvalu"},
#line 481 "lsotfea-ltag.gperf"
      {"SNA ", "Sena"},
#line 578 "lsotfea-ltag.gperf"
      {"ZHA ", "Zhuang"},
#line 202 "lsotfea-ltag.gperf"
      {"IJO ", "Ijo languages"},
#line 18 "lsotfea-ltag.gperf"
      {"ARA ", "Arabic"},
#line 482 "lsotfea-ltag.gperf"
      {"SNA0", "Shona"},
#line 500 "lsotfea-ltag.gperf"
      {"SVA ", "Svan"},
#line 501 "lsotfea-ltag.gperf"
      {"SVE ", "Swedish"},
#line 16 "lsotfea-ltag.gperf"
      {"ANG ", "Anglo-Saxon"},
#line 204 "lsotfea-ltag.gperf"
      {"ILE ", "Interlingue"},
#line 522 "lsotfea-ltag.gperf"
      {"THA ", "Thai"},
#line 29 "lsotfea-ltag.gperf"
      {"AZE ", "Azerbaijani"},
#line 544 "lsotfea-ltag.gperf"
      {"TZO ", "Tzotzil"},
#line 17 "lsotfea-ltag.gperf"
      {"APPH", "Phonetic transcription—Americanist conventions"},
#line 585 "lsotfea-ltag.gperf"
      {"ZZA ", "Zazaki"},
#line 208 "lsotfea-ltag.gperf"
      {"ING ", "Ingush"},
#line 536 "lsotfea-ltag.gperf"
      {"TSG ", "Tsonga"},
#line 531 "lsotfea-ltag.gperf"
      {"TNG ", "Tonga"},
#line 211 "lsotfea-ltag.gperf"
      {"IPPH", "Phonetic transcription—IPA conventions"},
#line 576 "lsotfea-ltag.gperf"
      {"ZEA ", "Zealandic"},
#line 206 "lsotfea-ltag.gperf"
      {"INA ", "Interlingua"},
#line 529 "lsotfea-ltag.gperf"
      {"TNA ", "Tswana"},
#line 530 "lsotfea-ltag.gperf"
      {"TNE ", "Tundra Nenets"},
#line 216 "lsotfea-ltag.gperf"
      {"ITA ", "Italian"}
    };

  static const short lookup[] =
    {
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
        0,  -1,  -1,  -1,   1,   2,  -1,  -1,  -1,  -1,
        3,  -1,   4,  -1,  -1,   5,   6,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
        7,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
        8,   9,  -1,  -1,  -1,  10,  11,  -1,  -1,  -1,
       12,  -1,  13,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       14,  -1,  -1,  -1,  -1,  15,  -1,  -1,  -1,  -1,
       16,  -1,  17,  -1,  -1,  18,  -1,  19,  -1,  20,
       21,  22,  23,  24,  -1,  25,  26,  -1,  -1,  -1,
       27,  28,  -1,  29,  -1,  30,  31,  32,  -1,  -1,
       33,  -1,  -1,  -1,  -1,  34,  35,  -1,  -1,  -1,
       36,  -1,  -1,  -1,  -1,  37,  38,  -1,  -1,  -1,
       39,  -1,  -1,  -1,  -1,  40,  -1,  -1,  -1,  -1,
       41,  -1,  -1,  -1,  -1,  -1,  42,  -1,  -1,  -1,
       -1,  43,  44,  -1,  -1,  45,  -1,  -1,  -1,  -1,
       46,  47,  -1,  -1,  -1,  48,  -1,  49,  -1,  -1,
       50,  -1,  51,  -1,  -1,  52,  53,  -1,  -1,  -1,
       54,  55,  -1,  -1,  -1,  -1,  -1,  56,  -1,  -1,
       57,  58,  -1,  -1,  -1,  59,  60,  -1,  -1,  -1,
       61,  62,  -1,  -1,  -1,  -1,  63,  -1,  -1,  -1,
       64,  -1,  65,  -1,  -1,  66,  67,  68,  69,  -1,
       70,  71,  -1,  -1,  -1,  -1,  72,  -1,  -1,  -1,
       -1,  73,  -1,  -1,  -1,  74,  -1,  -1,  -1,  -1,
       75,  76,  77,  -1,  -1,  78,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  79,  -1,  80,  -1,  -1,
       81,  82,  83,  -1,  -1,  84,  -1,  -1,  -1,  -1,
       85,  -1,  -1,  86,  -1,  -1,  -1,  87,  -1,  -1,
       88,  89,  -1,  -1,  -1,  90,  91,  92,  -1,  -1,
       93,  94,  -1,  -1,  -1,  -1,  -1,  95,  -1,  -1,
       96,  -1,  97,  -1,  -1,  98,  99, 100, 101,  -1,
      102,  -1,  -1,  -1,  -1,  -1, 103,  -1,  -1,  -1,
       -1, 104, 105,  -1,  -1, 106, 107,  -1,  -1,  -1,
      108,  -1,  -1,  -1,  -1, 109, 110,  -1,  -1,  -1,
      111, 112, 113,  -1,  -1, 114, 115, 116,  -1,  -1,
      117, 118, 119,  -1,  -1,  -1, 120,  -1,  -1,  -1,
      121, 122,  -1,  -1,  -1,  -1, 123,  -1,  -1,  -1,
       -1, 124,  -1,  -1,  -1,  -1, 125,  -1, 126,  -1,
      127, 128, 129,  -1,  -1, 130, 131, 132,  -1, 133,
      134,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      135, 136, 137, 138,  -1, 139, 140, 141,  -1,  -1,
      142,  -1, 143, 144,  -1,  -1,  -1, 145,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 146,  -1,  -1,  -1,
      147, 148,  -1, 149,  -1, 150, 151, 152,  -1,  -1,
      153,  -1, 154,  -1, 155,  -1,  -1, 156,  -1,  -1,
      157, 158,  -1,  -1,  -1, 159, 160,  -1,  -1,  -1,
      161,  -1,  -1,  -1,  -1, 162,  -1, 163,  -1,  -1,
       -1, 164, 165,  -1,  -1, 166,  -1,  -1,  -1,  -1,
      167,  -1, 168,  -1,  -1, 169, 170,  -1,  -1,  -1,
      171,  -1,  -1,  -1,  -1, 172, 173,  -1,  -1,  -1,
      174,  -1,  -1,  -1,  -1, 175, 176,  -1,  -1,  -1,
       -1, 177,  -1,  -1,  -1, 178, 179, 180,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 181,  -1,  -1,  -1,
      182,  -1,  -1,  -1,  -1, 183,  -1,  -1,  -1,  -1,
      184,  -1,  -1,  -1,  -1, 185, 186,  -1,  -1,  -1,
      187, 188, 189,  -1,  -1,  -1, 190,  -1, 191,  -1,
      192, 193,  -1,  -1,  -1, 194,  -1, 195,  -1,  -1,
      196,  -1,  -1,  -1,  -1, 197,  -1,  -1,  -1,  -1,
       -1,  -1, 198,  -1,  -1, 199, 200,  -1,  -1,  -1,
       -1, 201,  -1,  -1,  -1, 202,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 203, 204,  -1,  -1,  -1,
      205, 206,  -1,  -1,  -1, 207, 208,  -1, 209,  -1,
      210, 211,  -1,  -1,  -1,  -1, 212, 213,  -1,  -1,
      214,  -1,  -1,  -1,  -1, 215, 216,  -1,  -1,  -1,
      217,  -1,  -1,  -1,  -1,  -1, 218,  -1,  -1,  -1,
      219, 220,  -1,  -1,  -1,  -1,  -1, 221,  -1,  -1,
      222, 223,  -1,  -1,  -1, 224, 225,  -1,  -1,  -1,
      226, 227,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      228,  -1, 229,  -1,  -1,  -1,  -1,  -1, 230,  -1,
       -1,  -1,  -1,  -1,  -1, 231, 232, 233,  -1,  -1,
       -1, 234,  -1,  -1,  -1,  -1, 235,  -1,  -1,  -1,
      236, 237,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      238, 239, 240,  -1,  -1, 241,  -1,  -1,  -1,  -1,
      242, 243,  -1,  -1,  -1, 244,  -1,  -1,  -1,  -1,
      245,  -1,  -1,  -1,  -1, 246, 247,  -1,  -1,  -1,
      248, 249,  -1,  -1,  -1, 250, 251, 252,  -1,  -1,
      253,  -1,  -1,  -1,  -1, 254,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 255, 256,  -1,  -1,  -1,
      257, 258,  -1,  -1,  -1,  -1, 259,  -1,  -1,  -1,
      260,  -1,  -1,  -1,  -1, 261,  -1,  -1,  -1,  -1,
      262,  -1,  -1, 263,  -1,  -1,  -1, 264,  -1,  -1,
       -1,  -1,  -1, 265,  -1,  -1,  -1,  -1,  -1,  -1,
      266, 267,  -1,  -1,  -1, 268, 269,  -1,  -1,  -1,
      270, 271,  -1,  -1,  -1, 272, 273, 274,  -1,  -1,
      275, 276,  -1,  -1,  -1,  -1, 277,  -1,  -1,  -1,
      278, 279,  -1,  -1,  -1, 280, 281,  -1, 282,  -1,
      283,  -1,  -1,  -1,  -1, 284, 285,  -1,  -1,  -1,
      286, 287,  -1,  -1,  -1, 288, 289,  -1,  -1,  -1,
      290, 291,  -1,  -1,  -1, 292, 293,  -1,  -1,  -1,
      294, 295, 296,  -1,  -1, 297, 298,  -1,  -1,  -1,
       -1, 299,  -1,  -1,  -1, 300, 301,  -1,  -1,  -1,
       -1, 302,  -1,  -1,  -1, 303,  -1,  -1,  -1,  -1,
       -1, 304,  -1,  -1,  -1,  -1, 305, 306,  -1,  -1,
      307,  -1, 308, 309,  -1, 310, 311, 312,  -1,  -1,
      313, 314,  -1,  -1,  -1, 315,  -1,  -1,  -1,  -1,
      316,  -1, 317,  -1,  -1,  -1, 318,  -1,  -1,  -1,
      319,  -1, 320,  -1,  -1,  -1, 321,  -1,  -1,  -1,
      322, 323,  -1,  -1,  -1,  -1, 324, 325,  -1, 326,
      327, 328,  -1,  -1,  -1,  -1, 329,  -1,  -1,  -1,
      330,  -1, 331,  -1,  -1,  -1, 332,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 333, 334, 335, 336,  -1,
      337,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      338, 339,  -1,  -1,  -1, 340, 341,  -1,  -1,  -1,
       -1, 342, 343,  -1,  -1, 344, 345,  -1, 346,  -1,
      347, 348,  -1,  -1,  -1, 349, 350,  -1,  -1,  -1,
       -1, 351,  -1,  -1,  -1, 352, 353, 354,  -1,  -1,
      355, 356, 357,  -1,  -1, 358,  -1,  -1,  -1,  -1,
      359, 360,  -1,  -1,  -1, 361,  -1, 362, 363,  -1,
      364, 365,  -1,  -1,  -1, 366, 367,  -1,  -1,  -1,
      368, 369, 370,  -1,  -1, 371, 372,  -1,  -1,  -1,
      373, 374,  -1,  -1,  -1,  -1,  -1,  -1, 375,  -1,
       -1, 376,  -1,  -1,  -1, 377, 378,  -1,  -1,  -1,
      379, 380,  -1,  -1,  -1, 381, 382,  -1,  -1,  -1,
       -1, 383,  -1,  -1,  -1, 384,  -1,  -1,  -1,  -1,
      385, 386,  -1,  -1,  -1, 387,  -1,  -1,  -1,  -1,
       -1, 388,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 389,  -1,  -1,  -1,
       -1, 390, 391,  -1,  -1, 392,  -1,  -1,  -1,  -1,
      393, 394,  -1,  -1,  -1,  -1, 395, 396,  -1,  -1,
      397, 398, 399,  -1,  -1, 400,  -1,  -1,  -1,  -1,
       -1, 401,  -1,  -1,  -1,  -1, 402,  -1,  -1,  -1,
      403, 404, 405,  -1,  -1, 406,  -1, 407,  -1,  -1,
      408, 409,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 410,  -1,  -1,  -1, 411, 412,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      413, 414,  -1,  -1,  -1, 415, 416, 417, 418,  -1,
      419,  -1,  -1,  -1,  -1, 420,  -1,  -1,  -1,  -1,
      421, 422,  -1,  -1,  -1, 423,  -1,  -1,  -1,  -1,
      424, 425,  -1,  -1,  -1, 426, 427,  -1,  -1,  -1,
      428,  -1,  -1,  -1,  -1, 429,  -1,  -1, 430,  -1,
      431,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      432, 433, 434,  -1,  -1, 435,  -1,  -1,  -1,  -1,
      436, 437,  -1,  -1,  -1, 438, 439,  -1,  -1,  -1,
      440, 441,  -1, 442,  -1,  -1, 443,  -1,  -1,  -1,
       -1, 444,  -1,  -1,  -1, 445, 446,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 447,  -1,  -1,  -1,  -1,
      448, 449,  -1, 450,  -1, 451,  -1,  -1,  -1,  -1,
       -1, 452,  -1,  -1,  -1,  -1, 453,  -1,  -1,  -1,
      454, 455,  -1,  -1,  -1, 456,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 457,  -1,  -1,  -1,
      458,  -1,  -1,  -1,  -1,  -1, 459,  -1,  -1,  -1,
      460,  -1,  -1,  -1,  -1, 461,  -1,  -1,  -1,  -1,
      462, 463,  -1,  -1,  -1, 464, 465,  -1,  -1,  -1,
      466, 467,  -1,  -1,  -1, 468,  -1,  -1,  -1,  -1,
      469,  -1,  -1,  -1,  -1,  -1, 470,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      471, 472,  -1,  -1,  -1, 473,  -1,  -1,  -1,  -1,
      474, 475,  -1,  -1,  -1, 476,  -1,  -1,  -1,  -1,
       -1, 477,  -1,  -1,  -1,  -1,  -1, 478,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 479,  -1,  -1,  -1,  -1,
      480,  -1,  -1,  -1,  -1,  -1, 481,  -1,  -1,  -1,
      482, 483,  -1,  -1,  -1, 484,  -1,  -1,  -1,  -1,
      485,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 486,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      487, 488,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      489,  -1,  -1,  -1,  -1, 490,  -1, 491,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 492,  -1,  -1,  -1,  -1,
       -1, 493, 494,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 495,  -1,  -1,  -1,  -1,
      496, 497,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      498,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 499,  -1,  -1,  -1, 500,  -1,  -1,  -1,  -1,
      501,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 502,  -1,  -1,  -1,  -1, 503, 504,  -1,  -1,
       -1, 505,  -1,  -1,  -1,  -1, 506,  -1,  -1,  -1,
      507,  -1,  -1,  -1,  -1,  -1, 508,  -1,  -1,  -1,
       -1, 509,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 510,  -1,  -1,  -1,  -1, 511,  -1,  -1,  -1,
      512,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 513,  -1,  -1,  -1,
       -1, 514,  -1, 515,  -1, 516,  -1,  -1,  -1,  -1,
      517,  -1,  -1,  -1,  -1,  -1, 518,  -1,  -1,  -1,
      519,  -1,  -1,  -1,  -1, 520,  -1,  -1,  -1,  -1,
       -1, 521,  -1,  -1,  -1, 522, 523,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 524,  -1,  -1,  -1,
      525, 526,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      527,  -1,  -1,  -1,  -1, 528,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 529,  -1,  -1,  -1,  -1,
      530, 531,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1, 532,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      533,  -1,  -1,  -1,  -1, 534,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      535,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 536,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      537, 538,  -1,  -1,  -1, 539,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      540,  -1,  -1,  -1,  -1, 541,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 542,  -1,  -1,  -1,  -1,
      543,  -1,  -1,  -1,  -1,  -1, 544,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 545,  -1,  -1,  -1,
       -1, 546,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      547,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      548,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 549,  -1,  -1,  -1, 550,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 551, 552,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 553,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1, 554,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      555,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 556,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 557,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 558, 559,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      560,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 561, 562,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      563,  -1,  -1,  -1,  -1,  -1, 564,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 565,  -1,  -1,  -1,
       -1, 566,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      567,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1, 568,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      569,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      570,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 571,  -1,  -1,  -1,  -1,
       -1, 572,  -1,  -1,  -1, 573,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      574,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 575,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 576,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 577,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1, 578,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
      579,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 580,  -1,  -1,  -1,  -1,
      581,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,
       -1,  -1,  -1,  -1,  -1, 582
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
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
