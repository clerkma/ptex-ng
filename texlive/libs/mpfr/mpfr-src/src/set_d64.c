/* mpfr_set_decimal64 -- convert an IEEE 754-2008 decimal64 float to
                         a multiple precision floating-point number

See https://gcc.gnu.org/legacy-ml/gcc/2006-06/msg00691.html,
https://gcc.gnu.org/onlinedocs/gcc/Decimal-Float.html,
and TR 24732 <https://www.open-std.org/jtc1/sc22/wg14/www/projects#24732>.

Copyright 2006-2025 Free Software Foundation, Inc.
Contributed by the Pascaline and Caramba projects, INRIA.

This file is part of the GNU MPFR Library.

The GNU MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MPFR Library; see the file COPYING.LESSER.
If not, see <https://www.gnu.org/licenses/>. */

#define MPFR_NEED_LONGLONG_H
#include "mpfr-impl.h"

#ifdef MPFR_WANT_DECIMAL_FLOATS

#ifdef DECIMAL_DPD_FORMAT
  /* conversion 10-bits to 3 digits */
static unsigned int T[1024] = {
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 80, 81, 800, 801, 880, 881, 10, 11, 12, 13,
  14, 15, 16, 17, 18, 19, 90, 91, 810, 811, 890, 891, 20, 21, 22, 23, 24, 25,
  26, 27, 28, 29, 82, 83, 820, 821, 808, 809, 30, 31, 32, 33, 34, 35, 36, 37,
  38, 39, 92, 93, 830, 831, 818, 819, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
  84, 85, 840, 841, 88, 89, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 94, 95,
  850, 851, 98, 99, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 86, 87, 860, 861,
  888, 889, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 96, 97, 870, 871, 898,
  899, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 180, 181, 900, 901,
  980, 981, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 190, 191, 910,
  911, 990, 991, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 182, 183,
  920, 921, 908, 909, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 192,
  193, 930, 931, 918, 919, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
  184, 185, 940, 941, 188, 189, 150, 151, 152, 153, 154, 155, 156, 157, 158,
  159, 194, 195, 950, 951, 198, 199, 160, 161, 162, 163, 164, 165, 166, 167,
  168, 169, 186, 187, 960, 961, 988, 989, 170, 171, 172, 173, 174, 175, 176,
  177, 178, 179, 196, 197, 970, 971, 998, 999, 200, 201, 202, 203, 204, 205,
  206, 207, 208, 209, 280, 281, 802, 803, 882, 883, 210, 211, 212, 213, 214,
  215, 216, 217, 218, 219, 290, 291, 812, 813, 892, 893, 220, 221, 222, 223,
  224, 225, 226, 227, 228, 229, 282, 283, 822, 823, 828, 829, 230, 231, 232,
  233, 234, 235, 236, 237, 238, 239, 292, 293, 832, 833, 838, 839, 240, 241,
  242, 243, 244, 245, 246, 247, 248, 249, 284, 285, 842, 843, 288, 289, 250,
  251, 252, 253, 254, 255, 256, 257, 258, 259, 294, 295, 852, 853, 298, 299,
  260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 286, 287, 862, 863, 888,
  889, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 296, 297, 872, 873,
  898, 899, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 380, 381, 902,
  903, 982, 983, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 390, 391,
  912, 913, 992, 993, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 382,
  383, 922, 923, 928, 929, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339,
  392, 393, 932, 933, 938, 939, 340, 341, 342, 343, 344, 345, 346, 347, 348,
  349, 384, 385, 942, 943, 388, 389, 350, 351, 352, 353, 354, 355, 356, 357,
  358, 359, 394, 395, 952, 953, 398, 399, 360, 361, 362, 363, 364, 365, 366,
  367, 368, 369, 386, 387, 962, 963, 988, 989, 370, 371, 372, 373, 374, 375,
  376, 377, 378, 379, 396, 397, 972, 973, 998, 999, 400, 401, 402, 403, 404,
  405, 406, 407, 408, 409, 480, 481, 804, 805, 884, 885, 410, 411, 412, 413,
  414, 415, 416, 417, 418, 419, 490, 491, 814, 815, 894, 895, 420, 421, 422,
  423, 424, 425, 426, 427, 428, 429, 482, 483, 824, 825, 848, 849, 430, 431,
  432, 433, 434, 435, 436, 437, 438, 439, 492, 493, 834, 835, 858, 859, 440,
  441, 442, 443, 444, 445, 446, 447, 448, 449, 484, 485, 844, 845, 488, 489,
  450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 494, 495, 854, 855, 498,
  499, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 486, 487, 864, 865,
  888, 889, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 496, 497, 874,
  875, 898, 899, 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 580, 581,
  904, 905, 984, 985, 510, 511, 512, 513, 514, 515, 516, 517, 518, 519, 590,
  591, 914, 915, 994, 995, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529,
  582, 583, 924, 925, 948, 949, 530, 531, 532, 533, 534, 535, 536, 537, 538,
  539, 592, 593, 934, 935, 958, 959, 540, 541, 542, 543, 544, 545, 546, 547,
  548, 549, 584, 585, 944, 945, 588, 589, 550, 551, 552, 553, 554, 555, 556,
  557, 558, 559, 594, 595, 954, 955, 598, 599, 560, 561, 562, 563, 564, 565,
  566, 567, 568, 569, 586, 587, 964, 965, 988, 989, 570, 571, 572, 573, 574,
  575, 576, 577, 578, 579, 596, 597, 974, 975, 998, 999, 600, 601, 602, 603,
  604, 605, 606, 607, 608, 609, 680, 681, 806, 807, 886, 887, 610, 611, 612,
  613, 614, 615, 616, 617, 618, 619, 690, 691, 816, 817, 896, 897, 620, 621,
  622, 623, 624, 625, 626, 627, 628, 629, 682, 683, 826, 827, 868, 869, 630,
  631, 632, 633, 634, 635, 636, 637, 638, 639, 692, 693, 836, 837, 878, 879,
  640, 641, 642, 643, 644, 645, 646, 647, 648, 649, 684, 685, 846, 847, 688,
  689, 650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 694, 695, 856, 857,
  698, 699, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 686, 687, 866,
  867, 888, 889, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679, 696, 697,
  876, 877, 898, 899, 700, 701, 702, 703, 704, 705, 706, 707, 708, 709, 780,
  781, 906, 907, 986, 987, 710, 711, 712, 713, 714, 715, 716, 717, 718, 719,
  790, 791, 916, 917, 996, 997, 720, 721, 722, 723, 724, 725, 726, 727, 728,
  729, 782, 783, 926, 927, 968, 969, 730, 731, 732, 733, 734, 735, 736, 737,
  738, 739, 792, 793, 936, 937, 978, 979, 740, 741, 742, 743, 744, 745, 746,
  747, 748, 749, 784, 785, 946, 947, 788, 789, 750, 751, 752, 753, 754, 755,
  756, 757, 758, 759, 794, 795, 956, 957, 798, 799, 760, 761, 762, 763, 764,
  765, 766, 767, 768, 769, 786, 787, 966, 967, 988, 989, 770, 771, 772, 773,
  774, 775, 776, 777, 778, 779, 796, 797, 976, 977, 998, 999 };
#endif

#if _MPFR_IEEE_FLOATS && !defined(DECIMAL_GENERIC_CODE)

/* Convert d to a decimal string (one-to-one correspondence, no rounding).
   The string s needs to have at least 25 characters (with the final '\0'):
   * 1 for the sign '-'
   * 2 for the leading '0.'
   * 16 for the significand
   * 5 for the exponent (for example 'E-100')
   * 1 for the final '\0'
 */
static void
decimal64_to_string (char *s, _Decimal64 d)
{
  union mpfr_ieee_double_extract x;
  union ieee_double_decimal64 y;
  char *t;
  unsigned int Gh; /* most 5 significant bits from combination field */
  int exp; /* exponent */
  unsigned int i;

#ifdef DECIMAL_DPD_FORMAT
  unsigned int d0, d1, d2, d3, d4, d5;
#else /* BID */
#if GMP_NUMB_BITS >= 64
  mp_limb_t rp[2];
#else
  unsigned long rp[2]; /* rp[0] and rp[1] should contain at least 32 bits */
#endif
#define NLIMBS (64 / GMP_NUMB_BITS)
  mp_limb_t sp[NLIMBS];
  mp_size_t sn;
#endif

  /* end of declarations */

  /* Memory representation of the _Decimal64 argument. */
  MPFR_LOG_MSG (("d = { %02X, %02X, %02X, %02X, %02X, %02X, %02X, %02X }\n",
                 ((unsigned char *) &d)[0],
                 ((unsigned char *) &d)[1],
                 ((unsigned char *) &d)[2],
                 ((unsigned char *) &d)[3],
                 ((unsigned char *) &d)[4],
                 ((unsigned char *) &d)[5],
                 ((unsigned char *) &d)[6],
                 ((unsigned char *) &d)[7]));

  /* now convert BID or DPD to string */
  y.d64 = d;
  x.d = y.d;
  MPFR_LOG_MSG (("x = { .sig = %u, .exp = %u, "
                 ".manh = 0x%05lX = %lu, .manl = 0x%08lX = %lu }\n",
                 (unsigned int) x.s.sig, (unsigned int) x.s.exp,
                 (unsigned long) x.s.manh, (unsigned long) x.s.manh,
                 (unsigned long) x.s.manl, (unsigned long) x.s.manl));
  Gh = x.s.exp >> 6;
  if (Gh == 31)
    {
      sprintf (s, "NaN");
      return;
    }
  else if (Gh == 30)
    {
      if (x.s.sig == 0)
        sprintf (s, "Inf");
      else
        sprintf (s, "-Inf");
      return;
    }
  t = s;
  if (x.s.sig)
    *t++ = '-';

  /* both the decimal64 DPD and BID encodings consist of:
   * a sign bit of 1 bit
   * a combination field of 13=5+8 bits
   * a trailing significand field of 50 bits
   */
#ifdef DECIMAL_DPD_FORMAT
  /* the most significant 5 bits of the combination field give the first digit
     of the significand, and leading bits of the biased exponent (0, 1, 2). */
  if (Gh < 24)
    {
      exp = (x.s.exp >> 1) & 768;
      d0 = Gh & 7; /* first digit is in 0..7 */
    }
  else
    {
      exp = (x.s.exp & 384) << 1;
      d0 = 8 | (Gh & 1); /* first digit is 8 or 9 */
    }
  exp |= (x.s.exp & 63) << 2;
  exp |= x.s.manh >> 18;
  d1 = (x.s.manh >> 8) & 1023;
  d2 = ((x.s.manh << 2) | (x.s.manl >> 30)) & 1023;
  d3 = (x.s.manl >> 20) & 1023;
  d4 = (x.s.manl >> 10) & 1023;
  d5 = x.s.manl & 1023;
  sprintf (t, "%1u%3u%3u%3u%3u%3u", d0, T[d1], T[d2], T[d3], T[d4], T[d5]);
  /* Warning: some characters may be blank */
  for (i = 0; i < 16; i++)
    if (t[i] == ' ')
      t[i] = '0';
  t += 16;
#else /* BID */
  /* IEEE 754-2008 specifies that if the decoded significand exceeds the
     maximum, i.e. here if it is >= 10^16, then the value is zero. */
  if (Gh < 24)
    {
      /* the biased exponent E is formed from G[0] to G[9] and the
         significand from bits G[10] through the end of the decoding */
      exp = x.s.exp >> 1;
      /* manh has 20 bits, manl has 32 bits */
      rp[1] = ((x.s.exp & 1) << 20) | x.s.manh;
      rp[0] = x.s.manl;
    }
  else
    {
      /* the biased exponent is formed from G[2] to G[11] */
      exp = (x.s.exp & 511) << 1;
      rp[1] = x.s.manh;
      rp[0] = x.s.manl;
      exp |= rp[1] >> 19;
      rp[1] &= 524287; /* 2^19-1: cancel G[11] */
      rp[1] |= 2097152; /* add 2^21 */
    }
  /* now convert {rp, 2} to {sp, NLIMBS} */
#if GMP_NUMB_BITS >= 64
  sp[0] = MPFR_LIMB(rp[0]) | MPFR_LIMB_LSHIFT(rp[1],32);
#elif GMP_NUMB_BITS == 32
  sp[0] = rp[0];
  sp[1] = rp[1];
#elif GMP_NUMB_BITS == 16
  sp[0] = MPFR_LIMB(rp[0]);
  sp[1] = MPFR_LIMB(rp[0] >> 16);
  sp[2] = MPFR_LIMB(rp[1]);
  sp[3] = MPFR_LIMB(rp[1] >> 16);
#elif GMP_NUMB_BITS == 8
  sp[0] = MPFR_LIMB(rp[0]);
  sp[1] = MPFR_LIMB(rp[0] >> 8);
  sp[2] = MPFR_LIMB(rp[0] >> 16);
  sp[3] = MPFR_LIMB(rp[0] >> 24);
  sp[4] = MPFR_LIMB(rp[1]);
  sp[5] = MPFR_LIMB(rp[1] >> 8);
  sp[6] = MPFR_LIMB(rp[1] >> 16);
  sp[7] = MPFR_LIMB(rp[1] >> 24);
#else
#error "GMP_NUMB_BITS should be 8, 16, 32, or >= 64"
#endif
  sn = NLIMBS;
  while (sn > 0 && sp[sn - 1] == 0)
    sn --;
  if (sn == 0)
    {
    zero:
      *t = 0;
      i = 1;
    }
  else
    {
      i = mpn_get_str ((unsigned char*) t, 10, sp, sn);
      if (i > 16) /* non-canonical encoding: return zero */
        goto zero;
    }
  /* convert the values from mpn_get_str (0, 1, ..., 9) to digits: */
  while (i-- > 0)
    *t++ += '0';
#endif /* DPD or BID */

  exp -= 398; /* unbiased exponent: -398 = emin - (p-1) where
                 emin = 1-emax = 1-384 = -383 and p=16 */
  sprintf (t, "E%d", exp);
}

#else  /* portable version */

#ifndef DEC64_MAX
# define DEC64_MAX 9.999999999999999E384dd
#endif

static void
decimal64_to_string (char *s, _Decimal64 d)
{
  int sign = 0, n;
  int exp = 0;

  if (MPFR_UNLIKELY (DOUBLE_ISNAN (d))) /* NaN */
    {
      /* we don't propagate the sign bit */
      sprintf (s, "NaN"); /* sprintf puts a final '\0' */
      return;
    }
  else if (MPFR_UNLIKELY (d > DEC64_MAX)) /* +Inf */
    {
      sprintf (s, "Inf");
      return;
    }
  else if (MPFR_UNLIKELY (d < -DEC64_MAX)) /* -Inf */
    {
      sprintf (s, "-Inf");
      return;
    }

  /* now d is neither NaN nor +Inf nor -Inf */

  if (d < (_Decimal64) 0.0)
    {
      sign = 1;
      d = -d;
    }
  else if (d == (_Decimal64) -0.0)
    { /* Warning: the comparison d == -0.0 returns true for d = 0.0 too,
         copy code from set_d.c here. We first compare to the +0.0 bitstring,
         in case +0.0 and -0.0 are represented identically. */
      double dd = (double) d, poszero = +0.0, negzero = DBL_NEG_ZERO;
      if (memcmp (&dd, &poszero, sizeof(double)) != 0 &&
          memcmp (&dd, &negzero, sizeof(double)) == 0)
        {
          sign = 1;
          d = -d;
        }
    }

  /* now normalize d in [0.1, 1[ */
  if (d >= (_Decimal64) 1.0)
    {
      _Decimal64 ten16 = (double) 1e16; /* 10^16 is exactly representable
                                           in binary64 */
      _Decimal64 ten32 = ten16 * ten16;
      _Decimal64 ten64 = ten32 * ten32;
      _Decimal64 ten128 = ten64 * ten64;
      _Decimal64 ten256 = ten128 * ten128;

      if (d >= ten256)
        {
          d /= ten256;
          exp += 256;
        }
      if (d >= ten128)
        {
          d /= ten128;
          exp += 128;
        }
      if (d >= ten64)
        {
          d /= ten64;
          exp += 64;
        }
      if (d >= ten32)
        {
          d /= ten32;
          exp += 32;
        }
      if (d >= (_Decimal64) 10000000000000000.0)
        {
          d /= (_Decimal64) 10000000000000000.0;
          exp += 16;
        }
      if (d >= (_Decimal64) 100000000.0)
        {
          d /= (_Decimal64) 100000000.0;
          exp += 8;
        }
      if (d >= (_Decimal64) 10000.0)
        {
          d /= (_Decimal64) 10000.0;
          exp += 4;
        }
      if (d >= (_Decimal64) 100.0)
        {
          d /= (_Decimal64) 100.0;
          exp += 2;
        }
      if (d >= (_Decimal64) 10.0)
        {
          d /= (_Decimal64) 10.0;
          exp += 1;
        }
      if (d >= (_Decimal64) 1.0)
        {
          d /= (_Decimal64) 10.0;
          exp += 1;
        }
    }
  else /* d < 1.0 */
    {
      _Decimal64 ten16, ten32, ten64, ten128, ten256;

      ten16 = (double) 1e16; /* 10^16 is exactly representable in binary64 */
      ten16 = (_Decimal64) 1.0 / ten16; /* 10^(-16), exact */
      ten32 = ten16 * ten16;
      ten64 = ten32 * ten32;
      ten128 = ten64 * ten64;
      ten256 = ten128 * ten128;

      if (d < ten256)
        {
          d /= ten256;
          exp -= 256;
        }
      if (d < ten128)
        {
          d /= ten128;
          exp -= 128;
        }
      if (d < ten64)
        {
          d /= ten64;
          exp -= 64;
        }
      if (d < ten32)
        {
          d /= ten32;
          exp -= 32;
        }
      /* the double constant 0.0000000000000001 is 2028240960365167/2^104,
         which should be rounded to 1e-16 in _Decimal64 */
      if (d < (_Decimal64) 0.0000000000000001)
        {
          d *= (_Decimal64) 10000000000000000.0;
          exp -= 16;
        }
      /* the double constant 0.00000001 is 3022314549036573/2^78,
         which should be rounded to 1e-8 in _Decimal64 */
      if (d < (_Decimal64) 0.00000001)
        {
          d *= (_Decimal64) 100000000.0;
          exp -= 8;
        }
      /* the double constant 0.0001 is 7378697629483821/2^66,
         which should be rounded to 1e-4 in _Decimal64 */
      if (d < (_Decimal64) 0.0001)
        {
          d *= (_Decimal64) 10000.0;
          exp -= 4;
        }
      /* the double constant 0.01 is 5764607523034235/2^59,
         which should be rounded to 1e-2 in _Decimal64 */
      if (d < (_Decimal64) 0.01)
        {
          d *= (_Decimal64) 100.0;
          exp -= 2;
        }
      /* the double constant 0.1 is 3602879701896397/2^55,
         which should be rounded to 1e-1 in _Decimal64 */
      if (d < (_Decimal64) 0.1)
        {
          d *= (_Decimal64) 10.0;
          exp -= 1;
        }
    }

  /* now 0.1 <= d < 1 */
  if (sign == 1)
    *s++ = '-';
  *s++ = '0';
  *s++ = '.';
  for (n = 0; n < 16; n++)
    {
      double e;
      int r;

      d *= (_Decimal64) 10.0;
      e = (double) d;
      r = (int) e;
      *s++ = '0' + r;
      d -= (_Decimal64) r;
    }
  MPFR_ASSERTN(d == (_Decimal64) 0.0);
  if (exp != 0)
    sprintf (s, "E%d", exp); /* adds a final '\0' */
  else
    *s = '\0';
}

#endif  /* definition of decimal64_to_string (DPD, BID, or portable) */

/* the IEEE754-2008 decimal64 format has 16 digits, with emax=384,
   emin=1-emax=-383 */
int
mpfr_set_decimal64 (mpfr_ptr r, _Decimal64 d, mpfr_rnd_t rnd_mode)
{
  char s[25]; /* need 1 character for sign,
                      2 characters for '0.'
                     16 characters for significand,
                      1 character for exponent 'E',
                      4 characters for exponent (including sign),
                      1 character for terminating \0. */

  decimal64_to_string (s, d);
  MPFR_LOG_MSG (("string: %s\n", s));
  return mpfr_strtofr (r, s, NULL, 10, rnd_mode);
}

#endif /* MPFR_WANT_DECIMAL_FLOATS */
