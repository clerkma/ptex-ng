/*
   Copyright 2019-2024 Clerk Ma

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

void aptex_utils_get_seconds_and_micros (int64_t * s, int64_t * m);
void aptex_utils_init_start_time (void);
char * aptex_utils_get_creation_date(void);
char * aptex_utils_get_file_mod_date (char * file_name);
char * aptex_utils_get_file_size (char * file_name);
char * aptex_utils_get_md5_sum (char * file_name, uint32_t file_or_str);
char * aptex_utils_get_file_dump (char * file_name, uint32_t s, uint32_t l);

const char * aptex_unicode_version (void);
