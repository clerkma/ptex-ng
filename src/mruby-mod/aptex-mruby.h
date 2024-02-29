/*
   Copyright 2017-2023 Clerk Ma
 
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
 
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA.
*/

#ifndef APTEX_MRUBY_H
#define APTEX_MRUBY_H

#include "mruby.h"

void mrb_mruby_aptex_gem_init (mrb_state * mrb);
void mrb_mruby_aptex_gem_final (mrb_state * mrb);

#endif /* APTEX_MRUBY_H */
