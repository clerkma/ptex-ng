/*
   Copyright 2019 Clerk Ma

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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <errno.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <io.h>
#else
#include <sys/time.h>
#include <unistd.h>
#endif

#include "md5.h"

void aptex_utils_get_seconds_and_micros (uint64_t * s, uint64_t * m)
{
#ifdef _WIN32
  uint64_t intervals;
  FILETIME ft;

  GetSystemTimeAsFileTime(&ft);
  intervals = ((uint64_t) ft.dwHighDateTime << 32) | ft.dwLowDateTime;
  intervals -= 116444736000000000ULL;
  *s = intervals / 10000000L;
  *m = (intervals % 10000000L) / 10;
#else
  struct timeval tv;

  gettimeofday(&tv, NULL);
  *s = tv.tv_sec;
  *m = tv.tv_usec;
#endif
}

static int32_t start_time_set = 0;
static time_t start_time = 0;

static char start_time_str[38];
static char time_str[38];

static uint32_t SOURCE_DATE_EPOCH_set = 0;
static uint32_t FORCE_SOURCE_DATE_set = 0;

/* see PDF-1.7-8.3.1, PDF-2.0-7.9.4 */
static void make_asn1_date (time_t t, char * time_str, int32_t utc)
{
  struct tm gmt, lt;
  size_t size;
  int off, off_hours, off_mins;

  if (utc)
    lt = *gmtime(&t);
  else
    lt = *localtime(&t);

  size = strftime(time_str, 30, "D:%Y%m%d%H%M%S", &lt);

  if (size == 0)
    time_str[0] = '\0';

  if (time_str[14] == '6')
  {
    time_str[14] = '5';
    time_str[15] = '9';
    time_str[16] = '\0';
  }

  gmt = *gmtime(&t);
  off = 60 * (lt.tm_hour - gmt.tm_hour) + lt.tm_min - gmt.tm_min;

  if (lt.tm_year != gmt.tm_year)
  {
    off += (lt.tm_year > gmt.tm_year) ? 1440 : -1440;
  }
  else if (lt.tm_yday != gmt.tm_yday)
  {
    off += (lt.tm_yday > gmt.tm_yday) ? 1440 : -1440;
  }

  if (off == 0)
  {
    time_str[size++] = 'Z';
    time_str[size] = 0;
  }
  else
  {
    off_hours = off / 60;
    off_mins = abs(off - off_hours * 60);
    snprintf(&time_str[size], 22, "%+03d'%02d'", off_hours, off_mins);
  }
}

void aptex_utils_init_start_time (void)
{
  char * source_date_epoch = NULL;
  char * endptr;
  unsigned long long epoch;

  if (!start_time_set)
  {
    start_time_set = 1;
    source_date_epoch = getenv("SOURCE_DATE_EPOCH");

    if (source_date_epoch)
    {
      errno = 0;
      epoch = strtoull(source_date_epoch, &endptr, 10);

      if (*endptr != '\0' || errno != 0)
      {
        fprintf(stderr, "invalid $SOURCE_DATE_EPOCH: %s", source_date_epoch);
        exit(EXIT_FAILURE);
      }

      start_time = epoch;
      SOURCE_DATE_EPOCH_set = 1;
    }
    else
    {
      start_time = time((time_t *) NULL);
    }

    if (source_date_epoch)
      make_asn1_date(start_time, start_time_str, 1);
    else
      make_asn1_date(start_time, start_time_str, 0);
  }
}

char * aptex_utils_get_creation_date(void)
{
  if (!start_time_set)
  {
    char * source_date_epoch = NULL;

    source_date_epoch = getenv("SOURCE_DATE_EPOCH");
    aptex_utils_init_start_time();

    if (source_date_epoch)
      make_asn1_date(start_time, start_time_str, 1);
    else
      make_asn1_date(start_time, start_time_str, 0);
  }

  return start_time_str;
}

char * aptex_utils_get_file_mod_date (char * file_name)
{
  struct stat file_stat;

  if (stat(file_name, &file_stat) == 0)
    make_asn1_date(file_stat.st_mtime, time_str, 0);
  else
    time_str[0] = '\0';

  return time_str;
}

char * aptex_utils_get_file_size (char * file_name)
{
  struct stat file_stat;
  char * file_size_str;

  if (stat(file_name, &file_stat) == 0)
  {
    file_size_str = calloc(sizeof(char), 20);
    snprintf(file_size_str, 20, "%lu", (long unsigned int) file_stat.st_size);
  }
  else
  {
    file_size_str = NULL;
  }  

  return file_size_str;
}

static char * gen_hex_dump (char * in, uint32_t len)
{
  char * output_buffer;
  uint32_t step;

  output_buffer = calloc(sizeof(char), len * 2 + 1);

  if (output_buffer != NULL)
  {
    for (step = 0; step < len; step++)
    {
      sprintf(&output_buffer[2 * step], "%02X", (unsigned int) (unsigned char) in[step]);
    }
  }

  return output_buffer;
}

char * aptex_utils_get_md5_sum (char * file_name, uint32_t file_or_str)
{
  md5_state_t state;
  md5_byte_t digest[16];

  if (file_or_str == 1)
  {
    char file_buffer[1024];
    size_t buffer_len;
    FILE * f;

    f = fopen(file_name, "rb");

    if (f == NULL)
      return NULL;
    else
    {
      md5_init(&state);

      while ((buffer_len = fread(&file_buffer, sizeof(char), 1024, f)) > 0)
        md5_append(&state, (const md5_byte_t *) file_buffer, buffer_len);

      md5_finish(&state, digest);
      fclose(f);
      return gen_hex_dump(digest, 16);
    }
  }
  else
  {
    md5_init(&state);
    md5_append(&state, (const md5_byte_t *) file_name, strlen(file_name));
    md5_finish(&state, digest);
    return gen_hex_dump(digest, 16);
  }

  return NULL;
}

char * aptex_utils_get_file_dump (char * file_name, uint32_t s, uint32_t l)
{
  struct stat file_stat;
  int readable = 0;

#ifdef _WIN32
  readable = (stat(file_name, &file_stat) == 0 && _access(file_name, 4) == 0);
#else
  readable = (stat(file_name, &file_stat) == 0 && access(file_name, R_OK) == 0);
#endif

  if (readable != 0 && (file_stat.st_size > s + l))
  {
    FILE * f;
    char * file_buffer;
    char * buffer_hex_dump;

    f = fopen(file_name, "rb");
    file_buffer = calloc(sizeof(char), l);

    if (file_buffer != NULL)
    {
      fseek(f, s, SEEK_SET);

      if (fread(file_buffer, sizeof(char), l, f) == l)
      {
        buffer_hex_dump = gen_hex_dump(file_buffer, l);
        free(file_buffer);

        return buffer_hex_dump;
      }
    }
  }

  return NULL;
}
