/* round_prec.c -- Change interval's precision with rounding.

Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2010,
                     Spaces project, Inria Lorraine
                     and Salsa project, INRIA Rocquencourt,
                     and Arenaire project, Inria Rhone-Alpes, France
                     and Lab. ANO, USTL (Univ. of Lille),  France

This file is part of the MPFI Library.

The MPFI Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPFI Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPFI Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

#include "mpfi-impl.h"

/* The precision of the interval x is set to prec,     */
/* the previous value of x is kept.                    */
int
mpfi_round_prec (mpfi_ptr x, mpfr_prec_t prec)
{
  int inexact_left, inexact_right, inexact = 0;

  inexact_left = mpfr_round_prec (&(x->left), MPFI_RNDD, prec);
  inexact_right = mpfr_round_prec (&(x->right), MPFI_RNDU, prec);

  if ( MPFI_NAN_P(x) )
    MPFR_RET_NAN;

  if (inexact_left)
    inexact += 1;
  if (inexact_right)
    inexact += 2;
  return inexact;
}
