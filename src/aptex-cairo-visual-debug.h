/*
   Copyright 2017 Clerk Ma

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

void aptex_vdbg_ship_open (const char * out_name);
void aptex_vdbg_ship_close (void);
void aptex_vdbg_bop (void);
void aptex_vdbg_eop (void);
void aptex_vdbg_node_char (int32_t dir, int32_t x, int32_t y, int32_t w, int32_t h, int32_t d);
void aptex_vdbg_node_rule (int32_t dir, int32_t x, int32_t y, int32_t w, int32_t h);
