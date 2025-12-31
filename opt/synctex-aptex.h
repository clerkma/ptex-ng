// Copyright 2025-2026 LdBeth, Clerk Ma.
#include <kpathsea/c-fopen.h>
#define __SyncTeX__
#define EXTERN extern
#define EXPORT extern
#include "aptex.h"
#undef EXTERN
extern char *synctex_get_job_name();
extern char *synctex_get_log_name();
extern char *synctex_get_current_name();
#undef mag
#undef name
#undef kern
#define synctex_options synctex_option
#define SYNCTEX_VALUE synctex
#define SYNCTEX_GET_JOB_NAME synctex_get_job_name
#define SYNCTEX_GET_LOG_NAME synctex_get_log_name
#define SYNCTEX_GET_CURRENT_NAME synctex_get_current_name
#define SYNCTEX_GET_TOTAL_PAGES() (total_pages)
#define SYNCTEX_CURRENT_TAG synctex_tag
#define SYNCTEX_CURH cur_h
#define SYNCTEX_CURV cur_v
#define SYNCTEX_RULE_HT rule_ht
#define SYNCTEX_RULE_DP rule_dp
#define SYNCTEX_RULE_WD rule_wd
#define mem mem
// Work around
static char *output_directory=NULL;
