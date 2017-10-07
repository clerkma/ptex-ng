// simple config.h for liboft -- Clerk Ma

#ifdef _WIN32
  #define strncasecmp strnicmp
  #define alloca      _alloca
#else
  #define HAVE_ALLOCA_H 1
#endif
