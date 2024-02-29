/*
 *  KANJI Code conversion routines.
 */

#include <kpathsea/config.h>
#include <kpathsea/c-memstr.h>
#include <kpathsea/variable.h>
#include <kpathsea/readable.h>
#include <kpathsea/c-limits.h>
#include <kpathsea/c-pathmx.h>

#include <ptexenc/c-auto.h>
#include <ptexenc/ptexenc.h>
#include <ptexenc/kanjicnv.h>
#include <ptexenc/unicode.h>
#include <ptexenc/unicode-jp.h>

#include <ctype.h>
#include <sys/stat.h>

#define ENC_UNKNOWN  0
#define ENC_JIS      1
#define ENC_EUC      2
#define ENC_SJIS     3
#define ENC_UTF8     4
#define ENC_UPTEX    5

static int default_kanji_enc;
static boolean UPTEX_enabled;
static boolean ptex_mode = false;
static boolean prior_file_enc = false;

#define ESC '\033'

#ifndef NOFILE
# ifndef OPEN_MAX
#  define OPEN_MAX 132 /* sup_max_in_open(127) +alpha */
# endif
# define NOFILE OPEN_MAX
#endif

const char *ptexenc_version_string = PTEXENCVERSION;
#if defined(WIN32)
FILE *Poptr;
#endif
int infile_enc_auto = 2;
/* 0: guess disabled, 1: guess enabled, 2: unspecified */

static int     file_enc = ENC_UNKNOWN;
static int internal_enc = ENC_UNKNOWN;
static int terminal_enc = ENC_UNKNOWN;
static int    guess_enc = ENC_UNKNOWN;

const_string enc_to_string(int enc)
{
    switch (enc) {
    case ENC_JIS:  return "jis";
    case ENC_EUC:  return "euc";
    case ENC_SJIS: return "sjis";
    case ENC_UTF8: return "utf8";
    case ENC_UPTEX: if (UPTEX_enabled) return "uptex";
    default:       return "?";
    }
}

static int string_to_enc(const_string str)
{
    if (str == NULL)                    return ENC_UNKNOWN;
    if (strcasecmp(str, "default")== 0) return default_kanji_enc;
    if (strcasecmp(str, "jis")    == 0) return ENC_JIS;
    if (strcasecmp(str, "euc")    == 0) return ENC_EUC;
    if (strcasecmp(str, "sjis")   == 0) return ENC_SJIS;
    if (strcasecmp(str, "utf8")   == 0) return ENC_UTF8;
    if (UPTEX_enabled && strcasecmp(str, "uptex")  == 0) return ENC_UPTEX;

    if (strncasecmp(str, "ASCII", 5)== 0)      return file_enc;
    if (strncasecmp(str, "AMBIGUOUS", 9) == 0) return guess_enc;
    if (strncasecmp(str, "BINARY", 6) == 0)       return ENC_JIS;
    if (strncasecmp(str, "ISO-2022-JP", 11) == 0) return ENC_JIS;
    if (strncasecmp(str, "EUC-JP", 6) == 0)       return ENC_EUC;
    if (strncasecmp(str, "Shift_JIS", 9)   == 0)  return ENC_SJIS;
    if (strncasecmp(str, "UTF-8", 5)   == 0)      return ENC_UTF8;
    if (strncasecmp(str, "ISO-8859", 8) == 0)     return ENC_JIS;
    return -1; /* error */
}

static int get_default_enc(void)
{
    /* kpse_var_value("PTEX_KANJI_ENC") aborts
       if 'kpse_program_name' is empty.  It typically occurs
       when 'ptex' and 'jmpost' print version messages. */
    string var = getenv("PTEX_KANJI_ENC");
    int enc = string_to_enc(var);
    if (enc < 0) {
        fprintf(stderr, "Warning: Unknown environment value "
                "PTEX_KANJI_ENC='%s'\n", var);
    } else if (enc != ENC_UNKNOWN) {
        return enc;
    }
    return default_kanji_enc;
}

static void set_file_enc(int enc)
{
    if (enc == ENC_UPTEX) file_enc = ENC_UTF8;
    else /* rest */       file_enc = enc;
}

static void set_internal_enc(int enc)
{
    if      (enc == ENC_SJIS)  internal_enc = ENC_SJIS;
    else if (UPTEX_enabled && enc == ENC_UPTEX) internal_enc = ENC_UPTEX;
    else /* EUC, JIS, UTF8 */  internal_enc = ENC_EUC;
}

static int get_file_enc(void)
{
    if (file_enc == ENC_UNKNOWN) set_file_enc(get_default_enc());
    return file_enc;
}

int get_internal_enc(void)
{
    if (internal_enc == ENC_UNKNOWN) set_internal_enc(get_default_enc());
    return internal_enc;
}

static int get_terminal_enc(void)
{
    if (terminal_enc == ENC_UNKNOWN) {
        char lang[16];  /* enough large space */
        const char *s    = getenv("LC_ALL");
        if (s == NULL) s = getenv("LC_MESSAGES");
        if (s == NULL) s = getenv("LANG");
        if (s == NULL) s = getenv("LANGUAGE");
        if (s == NULL) s = "";
        if (strrchr(s, '.') != NULL) s = strrchr(s, '.') + 1;
        strncpy(lang, s, sizeof(lang) - 1);
        lang[sizeof(lang) - 1] = '\0';
        if      (strcasecmp(lang, "euc")  == 0) terminal_enc = ENC_EUC;
        else if (strcasecmp(lang, "eucJP")== 0) terminal_enc = ENC_EUC;
        else if (strcasecmp(lang, "ujis") == 0) terminal_enc = ENC_EUC;
        else if (strcasecmp(lang, "sjis") == 0) terminal_enc = ENC_SJIS;
        else if (strcasecmp(lang, "utf8") == 0) terminal_enc = ENC_UTF8;
        else if (strcasecmp(lang, "UTF-8")== 0) terminal_enc = ENC_UTF8;
        else if (strcasecmp(lang, "jis")  == 0) terminal_enc = ENC_JIS;
        else if (strcasecmp(lang, "ISO-2022-JP")== 0) terminal_enc = ENC_JIS;
        else terminal_enc = get_file_enc();
    }
    return terminal_enc;
}

void set_guess_file_enc(boolean enable)
{
    infile_enc_auto = (int) enable;
}

/* enable ptex mode (use flag 0x100 for Japanese char) */
void ptenc_ptex_mode (const boolean enable)
{
   //fprintf(stderr, "ptenc_ptex_mode is called! (%d)\n", enable);
   ptex_mode = enable;
}

/* enable/disable UPTEX */
void enable_UPTEX (boolean enable)
{
    UPTEX_enabled = enable;
    if (enable) {
        default_kanji_enc = ENC_UPTEX;
        internal_enc = ENC_UPTEX;
    } else {
#ifdef WIN32
        default_kanji_enc = ENC_UTF8;
        internal_enc = ENC_SJIS;
#else
        default_kanji_enc = ENC_UTF8;
        internal_enc = ENC_EUC;
#endif
    }
}

void set_prior_file_enc(void)
{
    prior_file_enc = true;
}

const_string get_enc_string(void)
{
    static char buffer[20]; /* enough large space */

    if (get_file_enc() == get_internal_enc()) {
        return enc_to_string(get_file_enc());
    } else {
        sprintf(buffer, "%s.%s",
                enc_to_string(get_file_enc()),
                enc_to_string(get_internal_enc()));
        return buffer;
    }
}

boolean set_enc_string(const_string file_str, const_string internal_str)
{
    int file     = string_to_enc(file_str);
    int internal = string_to_enc(internal_str);

    if (file < 0 || internal < 0) return false; /* error */
    if (file     != ENC_UNKNOWN) {
        set_file_enc(file);
#if !defined(WIN32)
        nkf_disable();
#endif
    }
    if (internal != ENC_UNKNOWN) set_internal_enc(internal);
    return true;
}

boolean is_internalSJIS(void)
{
    return (internal_enc == ENC_SJIS);
}

boolean is_internalEUC(void)
{
    return (internal_enc == ENC_EUC);
}

boolean is_internalUPTEX(void)
{
    return (internal_enc == ENC_UPTEX);
}

boolean is_terminalUTF8(void)
{
#ifdef WIN32
    return false;
#else
    get_terminal_enc(); return (terminal_enc == ENC_UTF8);
#endif
}


/* check char range */
boolean ismultichr (int length, int nth, int c)
{
    if (is_internalUPTEX()) return isUTF8(length, nth, c);
    if (length == 2) {
        if (nth == 1) {
            if (is_internalSJIS()) return isSJISkanji1(c);
            /* EUC */              return isEUCkanji1(c);
        } else if (nth == 2) {
            if (is_internalSJIS()) return isSJISkanji2(c);
            /* EUC */              return isEUCkanji2(c);
        }
    }
    if ((length == 3 || length == 4) &&
        (0 < nth && nth <= length)) return false;
    fprintf(stderr, "ismultichr: unexpected param length=%d, nth=%d\n",
            length, nth);
    return false;
}

/* check char range (kanji 1st) */
boolean iskanji1(int c)
{
    if (is_internalUPTEX()) return (isUTF8(2,1,c) ||
                                    isUTF8(3,1,c) ||
                                    isUTF8(4,1,c));
    if (is_internalSJIS()) return isSJISkanji1(c);
    /* EUC */              return isEUCkanji1(c);
}

/* check char range (kanji 2nd) */
boolean iskanji2(int c)
{
    if (is_internalSJIS()) return isSJISkanji2(c);
    /* EUC */              return isEUCkanji2(c);
}

/* multi-byte char length in s[pos] */
#define DEFINE_MULTISTRLEN(SUFF,TYPE) \
int multistrlen ## SUFF(TYPE *s, int len, int pos) \
{ \
    s += pos; len -= pos; \
    if (is_internalUPTEX()) { \
        int ret = UTF8Slength ## SUFF(s, len); \
        if (ret < 0) return 1; \
        return ret; \
    } \
    if (len < 2) return 1; \
    if (is_internalSJIS()) { \
        if (isSJISkanji1(s[0]) && isSJISkanji2(s[1])) return 2; \
    } else { /* EUC */ \
        if (isEUCkanji1(s[0])  && isEUCkanji2(s[1]))  return 2; \
    } \
    return 1; \
}
DEFINE_MULTISTRLEN(,unsigned char);
DEFINE_MULTISTRLEN(short,unsigned short);

/* for outputting filename (*s) to the terminal */
int multistrlenfilename(unsigned short *s, int len, int pos)
{
    s += pos; len -= pos;
    if (terminal_enc == ENC_UTF8) {
        int ret = UTF8Slengthshort(s, len);
        if (ret < 0) return 1;
        return ret;
    }
    if (len < 2) return 1;
    if (terminal_enc == ENC_SJIS) {
        if (isSJISkanji1(s[0]) && isSJISkanji2(s[1])) return 2;
    } else { /* EUC */
        if (isEUCkanji1(s[0])  && isEUCkanji2(s[1]))  return 2;
    }
    return 1;
}

/* with not so strict range check */
int multibytelen (int first_byte)
{
    if (is_internalUPTEX()) {
        return UTF8length(first_byte);
    } else if (is_internalSJIS()) {
        if (isSJISkanji1(first_byte)) return 2;
    } else { /* EUC */
        if (isEUCkanji1(first_byte))  return 2;
    }
    return 1;
}

/* buffer (EUC/SJIS/UTF-8) to internal (EUC/SJIS/UPTEX) code conversion */
long fromBUFF(unsigned char *s, int len, int pos)
{
    s += pos; len -= pos;
    if (is_internalUPTEX()) {
        if (UTF8Slength(s, len) < 0) return s[0];
        return UCStoUPTEX(UTF8StoUCS(s));
    }
    if (len < 2) return s[0];
    if (is_internalSJIS()) {
        if (isSJISkanji1(s[0]) && isSJISkanji2(s[1])) return HILO(s[0], s[1]);
    } else { /* EUC */
        if (isEUCkanji1(s[0])  && isEUCkanji2(s[1]))  return HILO(s[0], s[1]);
    }
    return s[0];
}

long fromBUFFshort(unsigned short *s, int len, int pos)
{
    int i;
    unsigned char sc[6];
    s += pos; len -= pos;
    for (i=0;i<(len<6 ? len : 6);i++) sc[i]=0xFF&s[i];
    return fromBUFF(sc, (len<6 ? len : 6), 0);
}

/* internal (EUC/SJIS/UPTEX) to buffer (EUC/SJIS/UTF-8) code conversion */
long toBUFF(long kcode)
{
    if (is_internalUPTEX()) kcode = UCStoUTF8(UPTEXtoUCS(kcode));
    return kcode;
}

/* DVI (JIS/UCS) to internal (EUC/SJIS/UPTEX) code conversion */
long fromDVI (long kcode)
{
    if (is_internalUPTEX()) return UCStoUPTEX(kcode);
    if (is_internalSJIS())  return JIStoSJIS(kcode);
    /* EUC */               return JIStoEUC(kcode);
}

/* internal (EUC/SJIS/UPTEX) to DVI (JIS/UCS) code conversion */
long toDVI (long kcode)
{
    if (is_internalUPTEX()) return UPTEXtoUCS(kcode);
    if (is_internalSJIS())  return SJIStoJIS(kcode);
    /* EUC */               return EUCtoJIS(kcode);
}

/* JIS to internal (EUC/SJIS/UPTEX) code conversion */
long fromJIS(long kcode)
{
    if (is_internalUPTEX()) return UCStoUPTEX(JIStoUCS2(kcode));
    if (is_internalSJIS())  return JIStoSJIS(kcode);
    /* EUC */               return JIStoEUC(kcode);
}

/* internal (EUC/SJIS/UPTEX) to JIS code conversion */
long toJIS(long kcode)
{
    if (is_internalUPTEX()) return UCS2toJIS(UPTEXtoUCS(kcode));
    if (is_internalSJIS())  return SJIStoJIS(kcode);
    /* EUC */               return EUCtoJIS(kcode);
}


/* EUC to internal (EUC/SJIS/UPTEX) code conversion */
long fromEUC(long kcode)
{
    if (!is_internalUPTEX() && !is_internalSJIS()) return kcode;
    return fromJIS(EUCtoJIS(kcode));
}

/* internal (EUC/SJIS/UPTEX) to EUC code conversion */
static long toEUC(long kcode)
{
    if (!is_internalUPTEX() && !is_internalSJIS()) return kcode;
    return JIStoEUC(toJIS(kcode));
}


/* SJIS to internal (EUC/SJIS/UPTEX) code conversion */
long fromSJIS(long kcode)
{
    if (is_internalSJIS()) return kcode;
    return fromJIS(SJIStoJIS(kcode));
}

/* internal (EUC/SJIS/UPTEX) to SJIS code conversion */
static long toSJIS(long kcode)
{
    if (is_internalSJIS()) return kcode;
    return JIStoSJIS(toJIS(kcode));
}


/* KUTEN to internal (EUC/SJIS/UPTEX) code conversion */
long fromKUTEN(long kcode)
{
    return fromJIS(KUTENtoJIS(kcode));
}


/* UCS to internal (EUC/SJIS/UPTEX) code conversion */
long fromUCS(long kcode)
{
    if (is_internalUPTEX()) return UCStoUPTEX(kcode);
    kcode = UCS2toJIS(kcode);
    if (kcode == 0) return 0;
    return fromJIS(kcode);
}

/* internal (EUC/SJIS/UPTEX) to UCS code conversion */
long toUCS(long kcode)
{
    if (is_internalUPTEX()) return UPTEXtoUCS(kcode);
    return JIStoUCS2(toJIS(kcode));
}

/* internal (EUC/SJIS/UPTEX) to UTF-8 code conversion */
static long toUTF8 (long kcode)
{
    return UCStoUTF8(toUCS(kcode));
}

/* internal (EUC/SJIS/UPTEX) to 'enc' code conversion */
static long toENC(long kcode, int enc)
{
    switch (enc) {
    case ENC_UTF8: return toUTF8(kcode);
    case ENC_JIS:  return toJIS(kcode);
    case ENC_EUC:  return toEUC(kcode);
    case ENC_SJIS: return toSJIS(kcode);
    default:
        fprintf(stderr, "toENC: unknown enc (%d).\n", enc);
        return 0;
    }
}

#define KANJI_IN   LONG(0, ESC, '$', 'B')
#define KANJI_OUT  LONG(0, ESC, '(', 'B')

static int put_multibyte(long c, FILE *fp) {
#ifdef WIN32
    const int fd = fileno(fp);

    if ((fd == fileno(stdout) || fd == fileno(stderr)) && _isatty(fd)) {
       HANDLE hStdout;
       DWORD ret, wclen;
       UINT cp;
       wchar_t buff[2];
       char str[4];
       int mblen;

       if (fd == fileno(stdout))
           hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
       else
           hStdout = GetStdHandle(STD_ERROR_HANDLE);

       mblen=0;
       if (BYTE1(c) != 0) str[mblen++]=BYTE1(c);
       if (BYTE2(c) != 0) str[mblen++]=BYTE2(c);
       if (BYTE3(c) != 0) str[mblen++]=BYTE3(c);
       /* always */       str[mblen++]=BYTE4(c);

#define CP_UTF8    65001

       cp = CP_UTF8;
       if (MultiByteToWideChar(cp, 0, str, mblen, buff, 2) == 0)
           return EOF;

       wclen = mblen > 3 ? 2 : 1;
       if (WriteConsoleW(hStdout, buff, wclen, &ret, NULL) == 0)
           return EOF;

       return BYTE4(c);
    }
#endif

    if (BYTE1(c) != 0 && putc(BYTE1(c), fp) == EOF) return EOF;
    if (BYTE2(c) != 0 && putc(BYTE2(c), fp) == EOF) return EOF;
    if (BYTE3(c) != 0 && putc(BYTE3(c), fp) == EOF) return EOF;
    /* always */  return putc(BYTE4(c), fp);
}

static int flush (unsigned char *buff, int num, FILE *fp)
{
    int i, ret = EOF;

    /* fprintf(stderr, "putc2: unexpected chars. ( ");
       for (i=0; i<num; i++) fprintf(stderr, "%02X ", buff[i]);
       fprintf(stderr, ")\n");
    */
    for (i=0; i<num; i++) ret = putc(buff[i], fp);
    return ret;
}

/* putc() with code conversion */
int putc2(int c, FILE *fp)
/*
  c in [0,255]:  writes the character c, without code conversion
  c in [256,511]: writes the character c-256, with code conversion
*/
{
    static int num[NOFILE];
        /* 0    : not in Kanji
           1..4 : in JIS Kanji and num[] bytes are in store[][]
           -1   : in JIS Kanji and store[][] is empty */
    static unsigned char store[NOFILE][4];
    const int fd = fileno(fp);
    int ret = c, output_enc;
#ifdef WIN32
    if ((fp == stdout || fp == stderr) && (_isatty(fd) || !prior_file_enc)) {
        output_enc = ENC_UTF8;
     } else
        output_enc = get_file_enc();
#else
    if ((fp == stdout || fp == stderr) && !prior_file_enc) {
        output_enc = get_terminal_enc();
    } else
        output_enc = get_file_enc();
#endif
    if (ptex_mode && (c<256)) {
        if (num[fd] < 0 && output_enc == ENC_JIS) {
            put_multibyte(KANJI_OUT, fp);
        }
        ret = putc(c, fp);
        num[fd] = 0;
    } else {
        c &= 0xFF;
        if (num[fd] > 0) {        /* multi-byte char */
            if (is_internalUPTEX() && iskanji1(c)) { /* error */
                ret = flush(store[fd], num[fd], fp);
                num[fd] = 0;
            }
            store[fd][num[fd]] = c;
            num[fd]++;
            if (multistrlen(store[fd], num[fd], 0) == num[fd]) {
                long i = fromBUFF(store[fd], num[fd], 0);
                ret = put_multibyte(toENC(i, output_enc), fp);
                num[fd] = -1;
            } else if ((is_internalUPTEX() && num[fd] == 4) ||
                (!is_internalUPTEX() && num[fd] == 2)) { /* error */
                ret = flush(store[fd], num[fd], fp);
                num[fd] = -1;
            }
        } else if (iskanji1(c)) { /* first multi-byte char */
            if (num[fd] == 0 && output_enc == ENC_JIS) {
                ret = put_multibyte(KANJI_IN, fp);
            }
            store[fd][0] = c;
            num[fd] = 1;
        } else {                  /* ASCII */
            if (num[fd] < 0 && output_enc == ENC_JIS) {
                put_multibyte(KANJI_OUT, fp);
            }
            ret = putc(c, fp);
            num[fd] = 0;
        }
    }
    return ret;
}

/* fputs() with code conversion */
int fputs2(const char *s, FILE *fp)
{
    while (*s != '\0') {
        int ret = putc2((unsigned char)*s, fp);
        if (ret == EOF) return EOF;
        s++;
    }
    return 1;
}


static struct unget_st {
    int size;
    int buff[4];
} ungetbuff[NOFILE];

static int getc4(FILE *fp)
{
    struct unget_st *p = &ungetbuff[fileno(fp)];

    if (p->size == 0)
#ifdef WIN32
    {
        const int fd = fileno(fp);
        HANDLE hStdin;
        DWORD ret;
        wchar_t wc[2];
        long c;
        static wchar_t wcbuf = L'\0';

        if (!(fd == fileno(stdin) && _isatty(fd) && is_internalUPTEX()))
            return getc(fp);

        hStdin = GetStdHandle(STD_INPUT_HANDLE);
        if (wcbuf) {
            wc[0] = wcbuf;
            wcbuf = L'\0';
        }
        else if (ReadConsoleW(hStdin, wc, 1, &ret, NULL) == 0)
            return EOF;
        if (0xd800<=wc[0] && wc[0]<0xdc00) {
            if (ReadConsoleW(hStdin, wc+1, 1, &ret, NULL) == 0)
                return EOF;
            if (0xdc00<=wc[1] && wc[1]<0xe000) {
                c = UTF16StoUTF32(wc[0], wc[1]);
            } else {
                wcbuf = wc[1];
                c = U_REPLACEMENT_CHARACTER;  /* illegal upper surrogate pair */
            }
        } else if (0xdc00<=wc[0] && wc[0]<0xe000) {
            c = U_REPLACEMENT_CHARACTER;      /* illegal lower surrogate pair */
        } else {
            c = wc[0];
        }
        c = UCStoUTF8(c);
        /* always */       p->buff[p->size++]=BYTE4(c);
        if (BYTE3(c) != 0) p->buff[p->size++]=BYTE3(c);
        if (BYTE2(c) != 0) p->buff[p->size++]=BYTE2(c);
        if (BYTE1(c) != 0) p->buff[p->size++]=BYTE1(c);
    }
#else
        return getc(fp);
#endif
    return p->buff[--p->size];
}

static int ungetc4(int c, FILE *fp)
{
    struct unget_st *p = &ungetbuff[fileno(fp)];

    if (p->size >= 4) return EOF;
    return p->buff[p->size++] = c;
}


static unsigned char *buffer;
static long first, last;
static boolean combin_voiced_sound(boolean semi)
{
    int i, mblen;

    mblen = is_internalUPTEX() ? 3 : 2;
    if (last-mblen < first) return false;
    if (multistrlen(buffer,last,last-mblen) != mblen) return false;
    i = toUCS(fromBUFF(buffer,last,last-mblen));
    i = get_voiced_sound(i, semi);
    if (i == 0) return false;
    i = toBUFF(fromUCS(i));
    if (BYTE2(i) != 0) buffer[last-3] = BYTE2(i);
    /* always */       buffer[last-2] = BYTE3(i);
    /* always */       buffer[last-1] = BYTE4(i);
    return true;
}

static void write_multibyte(long i)
{
    if (BYTE1(i) != 0) buffer[last++] = BYTE1(i);
    if (BYTE2(i) != 0) buffer[last++] = BYTE2(i);
    /* always */       buffer[last++] = BYTE3(i);
    /* always */       buffer[last++] = BYTE4(i);
}

static void write_hex(int i)
{
    sprintf((char *) buffer + last, "^^%02x", i);
    last += 4;
}

/* getc() with check of broken encoding of UTF-8 */
static int getcUTF8(FILE *fp)
{
    int c = getc4(fp);

    if (isUTF8(2,2,c)) return c;
    ungetc4(c, fp);
    return EOF;
}

static void get_utf8(int i, FILE *fp)
{
    long u = 0, j;
    int i2 = EOF, i3 = EOF, i4 = EOF;

    switch (UTF8length(i)) {
    case 2:
        i2 = getcUTF8(fp); if (i2 == EOF) break;
        u = UTF8BtoUCS(i, i2);
        break;
    case 3:
        i2 = getcUTF8(fp); if (i2 == EOF) break;
        i3 = getcUTF8(fp); if (i3 == EOF) break;
        u = UTF8CtoUCS(i, i2, i3);
        if (u == U_BOM) return; /* just ignore */
        if (u == U_VOICED      && combin_voiced_sound(false)) return;
        if (u == U_SEMI_VOICED && combin_voiced_sound(true))  return;
        break;
    case 4:
        i2 = getcUTF8(fp); if (i2 == EOF) break;
        i3 = getcUTF8(fp); if (i3 == EOF) break;
        i4 = getcUTF8(fp); if (i4 == EOF) break;
        u = UTF8DtoUCS(i, i2, i3, i4);
        break;
    default:
        u = U_REPLACEMENT_CHARACTER;
        break;
    }

    j = (u != 0) ? toBUFF(fromUCS(u)) : 0;
    if (j == 0) { /* can't represent (typically umlaut o in EUC) */
        write_hex(i);
        if (i2 != EOF) write_hex(i2);
        if (i3 != EOF) write_hex(i3);
        if (i4 != EOF) write_hex(i4);
    } else {
        write_multibyte(j);
    }
}

static void get_euc(int i, FILE *fp)
{
    int j = getc4(fp);

    if (isEUCkanji2(j)) {
        write_multibyte(toBUFF(fromEUC(HILO(i,j))));
    } else {
        buffer[last++] = i;
        ungetc4(j, fp);
    }
}

static void get_sjis(int i, FILE *fp)
{
    int j = getc4(fp);

    if (isSJISkanji2(j)) {
        write_multibyte(toBUFF(fromSJIS(HILO(i,j))));
    } else {
        buffer[last++] = i;
        ungetc4(j, fp);
    }
}

static boolean is_tail(long *c, FILE *fp)
{
    if (*c == EOF) return true;
    if (*c == '\n') return true;
    if (*c == '\r') {
        int d = getc4(fp);
        if (d == '\n') *c = d;
        else ungetc4(d, fp);
        return true;
    }
    return false;
}

#define MARK_LEN 4
static int bom_u[MARK_LEN] = { 0xEF, 0xBB, 0xBF, 0x7E };
static int bom_l[MARK_LEN] = { 0xEF, 0xBB, 0xBF, 0x01 };

/* if stream begins with BOM + 7bit char */
static boolean isUTF8Nstream(FILE *fp)
{
    int i;
    int c[MARK_LEN];

    for (i=0; i<MARK_LEN; i++) {
        c[i] = getc4(fp);
        if (!(bom_l[i] <= c[i] && c[i] <= bom_u[i])) {
            do { ungetc4(c[i], fp); } while (i-- > 0);
            return false;
        }
    }
    ungetc4(c[MARK_LEN-1], fp);
    return true;
}

static int infile_enc[NOFILE]; /* ENC_UNKNOWN (=0): not determined
                                  other: determined */

/* guess file encoding */
/*
    assumption:
      No halfwidth katakana in Shift_JIS
      No SS2 nor SS3 in EUC-JP
      JIS X 0208 only and no platform dependent characters in Shift_JIS, EUC-JP
      ISO-8859 may have 0xA0..0xFF, may not have 0x80..0x9F
*/
char *ptenc_guess_enc(FILE *fp, boolean chk_bom, boolean chk_nl)
{
    char *enc;
    int k0, k1, k2, cdb[2], cu8[4], len_utf8;
    int nl0=0, nl_cr=0, nl_lf=0, nl_crlf=0;
    int is_ascii=1, lbyte=0;
    int maybe_sjis=1, maybe_euc=1, maybe_utf8=1, maybe_iso8859=1, pos_db=0, pos_utf8=0;
    int ch_sjis=0, ch_euc=0, ch_utf8=0, ch_iso8859=0, bom=0;
#ifdef DEBUG
    int i;
    unsigned char str0[5];
#endif /* DEBUG */
    enc = xmalloc(sizeof(char)*32);

    while ((k0 = fgetc(fp)) != EOF &&
           (maybe_sjis+maybe_euc+maybe_utf8+maybe_iso8859>1 || pos_db || pos_utf8
            || lbyte<320 || k0=='\r')) {
        if (chk_nl) {
            if (k0 == '\r') nl_cr++;
            if (k0 == '\n') {
                if (nl0 == '\r') { nl_cr--;  nl_crlf++; }
                else             { nl_lf++;             }
            }
        }
        if (maybe_iso8859 && maybe_sjis+maybe_euc+maybe_utf8==1 && !pos_db && !pos_utf8
            && ch_iso8859>=2000 && k0!='\r') {
            break;
        }
        nl0 = k0;
        if (chk_bom && lbyte<4 && bom_l[lbyte] <= k0 && k0 <= bom_u[lbyte]) bom++;
        lbyte++;
        if (k0==ESC) {
            k0 = fgetc(fp);
            if (k0=='$') {
                k0 = fgetc(fp);
                if (k0=='@' || k0=='B') {
                    strcpy(enc,"ISO-2022-JP");
                    goto post_process;
                }
            }
            if (k0>0x7F) {
                strcpy(enc,"BINARY");
                goto post_process;
            } else if (k0==EOF) {
                break;
            }
            continue;
        } else if (k0==0x00) {
            strcpy(enc,"BINARY");
            goto post_process;
        } else if (k0<0x80) {
            if (pos_utf8>0) {
                maybe_utf8 = 0;
                pos_utf8 = 0;
            }
            if (pos_db==1) {
                maybe_euc = 0;
                pos_db = 0;
                if (maybe_sjis) {
                    cdb[1] = k0;
                    k1 = JIStoUCS2(SJIStoJIS(HILO(cdb[0],cdb[1])));
#ifdef DEBUG
                    fprintf(stderr, "Character for guess encoding: 0x%02X%02X", cdb[0], cdb[1]);
                    if (k1) {
                        i = UCStoUTF8S(k1, str0);
                        str0[i] = '\0';
                        fprintf(stderr, " sjis (%s)\n", str0);
                    } else {
                        fprintf(stderr, " not sjis\n");
                    }
#endif /* DEBUG */
                    if (k1) {
                        ch_sjis++;
                        continue;
                    }
                }
                maybe_sjis = 0;
            }
            if (is_ascii && lbyte>10000) {
                /* guess ASCII if we did not find 8bit chars in head 10000 bytes */
                strcpy(enc,"ASCII");
                goto post_process;
            }
            continue;
        }

        if (!isISO8859(k0))
            maybe_iso8859 = 0;
        else
            ch_iso8859++;
        is_ascii = 0;
        if (pos_db==0) {
            cdb[0] = k0;
            cdb[1] = 0;
            pos_db = 1;
        }
        else if (pos_db==1 && (maybe_sjis || maybe_euc)) {
            cdb[1] = k0;
            k1 = JIStoUCS2(SJIStoJIS(HILO(cdb[0],cdb[1])));
            k2 = JIStoUCS2(EUCtoJIS(HILO(cdb[0],cdb[1])));
            if (maybe_sjis) {
                if (!k1)
                    maybe_sjis = 0;
                else
                    ch_sjis++;
            }
            if (maybe_euc) {
                if (!k2)
                    maybe_euc = 0;
                else
                    ch_euc++;
            }
            pos_db = 0;
#ifdef DEBUG
            fprintf(stderr, "Character for guess encoding: 0x%02X%02X", cdb[0], cdb[1]);
            if (maybe_sjis || maybe_euc) {
                if (maybe_sjis) {
                    i = UCStoUTF8S(k1, str0);
                    str0[i] = '\0';
                    fprintf(stderr, " sjis (%s)", str0);
                }
                if (maybe_euc) {
                    i = UCStoUTF8S(k2, str0);
                    str0[i] = '\0';
                    fprintf(stderr, " euc (%s)", str0);
                }
                fprintf(stderr, "\n");
            } else {
                fprintf(stderr, " not sjis nor euc\n");
            }
#endif /* DEBUG */
        }
        if (pos_utf8==0) {
            len_utf8 = UTF8length(k0);
            if (len_utf8<2) {
                maybe_utf8 = 0;
                pos_utf8 = 0;
                continue;
            }
            cu8[0] = k0;
            pos_utf8 = 1;
        }
        else if (pos_utf8>0 && maybe_utf8) {
            if (k0>0xBF) {
                maybe_utf8 = 0;
                pos_utf8 = 0;
                continue;
            }
            cu8[pos_utf8] = k0;
            if (pos_utf8==1) {
                if ((cu8[0]==0xE0 && cu8[1]<0xA0) ||
                    (cu8[0]==0xED && cu8[1]>0x9F) ||
                    (cu8[0]==0xF0 && cu8[1]<0x90) ||
                    (cu8[0]==0xF4 && cu8[1]>0x8F)) { /* illegal combination in UTF-8 */
                    maybe_utf8 = 0;
                    pos_utf8 = 0;
                    continue;
                }
            }
            pos_utf8++;
            if (pos_utf8==len_utf8) {
#ifdef DEBUG
                for (i=0; i<len_utf8; i++) str0[i] = cu8[i];
                str0[i] = '\0';
                fprintf(stderr, "Character for guess encoding: 0x");
                for (i=0; i<len_utf8; i++) fprintf(stderr, "%02X", cu8[i]);
                fprintf(stderr, " U+%06lX (%s)\n", UTF8StoUCS(str0), str0);
#endif /* DEBUG */
                ch_utf8++;
                len_utf8 = 0;
                pos_utf8 = 0;
                cu8[0]=cu8[1]=cu8[2]=cu8[3]=0;
            }
        }
    }

    if (k0==EOF) {
        if (pos_db)   maybe_sjis = maybe_euc = 0;
        if (pos_utf8) maybe_utf8 = 0;
    }
#ifdef DEBUG
    if (maybe_sjis+maybe_euc+maybe_utf8+maybe_iso8859>1) {
        fprintf(stderr,
           "Maybe(%d, %d, %d, %d), Multibyte characters(%d, %d, %d, %d), lbyte(%d)\n",
           maybe_sjis, maybe_euc, maybe_utf8, maybe_iso8859, ch_sjis, ch_euc, ch_utf8, ch_iso8859, lbyte);
    }
#endif /* DEBUG */
    if (maybe_iso8859) {
        /* The threshold of ch_* to judge ISO-8859 or not is heuristic, not strict */
        if ((maybe_sjis && ch_sjis>=16 && ch_sjis*1.3<=ch_iso8859) ||
            (maybe_euc  && ch_euc >=8  && ch_euc *2  ==ch_iso8859) ||
            (maybe_utf8 && ch_utf8>=8  && ch_utf8*2  <=ch_iso8859) ||
             bom==4)
               maybe_iso8859 = 0;
    }
    if (is_ascii)
        strcpy(enc,"ASCII");
    else if (maybe_sjis+maybe_euc+maybe_utf8+maybe_iso8859>1) {
        guess_enc = ENC_UNKNOWN;
        strcpy(enc,"AMBIGUOUS(");
        if (maybe_sjis) {
            enc = strcat(enc,"s");
            if (file_enc == ENC_SJIS) guess_enc = ENC_SJIS;
        }
        if (maybe_euc) {
            enc = strcat(enc, maybe_sjis ? ",e" : "e");
            if (file_enc == ENC_EUC)  guess_enc = ENC_EUC;
        }
        if (maybe_utf8) {
            enc = strcat(enc, maybe_sjis || maybe_euc ? ",u" : "u");
            if (file_enc == ENC_UTF8) guess_enc = ENC_UTF8;
        }
        if (maybe_iso8859)
            enc = strcat(enc, ",i");
        enc = strcat(enc,")");
        if (maybe_iso8859 && maybe_euc+maybe_sjis+maybe_utf8==1) {
            if (maybe_sjis) guess_enc = ENC_SJIS;
            if (maybe_euc)  guess_enc = ENC_EUC;
            if (maybe_utf8) guess_enc = ENC_UTF8;
        }
#ifdef WIN32
        if (guess_enc == ENC_UNKNOWN) guess_enc = ENC_UTF8;
#else
        if (guess_enc == ENC_UNKNOWN) guess_enc = get_terminal_enc();
#endif
    }
    else if (maybe_sjis)
        strcpy(enc,"Shift_JIS");
    else if (maybe_euc)
        strcpy(enc,"EUC-JP");
    else if (maybe_utf8) {
        strcpy(enc,"UTF-8");
        if (bom>=3) strcat(enc," (BOM)");
    }
    else if (maybe_iso8859)
        strcpy(enc,"ISO-8859");
    else
        strcpy(enc,"BINARY");
  post_process:
    if (chk_nl && (nl_cr || nl_lf || nl_crlf)) {
        if      (nl_lf+nl_crlf==0) strcat(enc," (CR)");
        else if (nl_cr+nl_crlf==0) strcat(enc," (LF)");
        else if (nl_cr+nl_lf  ==0) strcat(enc," (CRLF)");
        else                       strcat(enc," (MIXED NL)");
    }
    rewind (fp);
    return enc;
}

void ptenc_set_infile_enc_auto(void)
{
   char *p;
   if (infile_enc_auto == 2) {
     p = kpse_var_value ("guess_input_kanji_encoding");
     if (p) {
       if (*p == '1' || *p == 'y' || *p == 't')  infile_enc_auto = 1;
       free(p);
     }
   }
   if (infile_enc_auto == 2) infile_enc_auto = 0;
}

/* input line with encoding conversion */
long input_line2(FILE *fp, unsigned char *buff, unsigned char *buff2,
                 long pos, const long buffsize, int *lastchar)
{
    long i = 0;
    static boolean injis = false;
    const int fd = fileno(fp);

    buffer = buff;
    first = last = pos;

    if (infile_enc[fd] == ENC_UNKNOWN) { /* just after opened */
        ungetbuff[fd].size = 0;
        if (isUTF8Nstream(fp)) {
            infile_enc[fd] = ENC_UTF8;
#ifdef DEBUG
            fprintf(stderr, "Detect UTF-8 with BOM #%d\n", fd);
#endif /* DEBUG */
        }
        else {
          struct stat st;
          if (infile_enc_auto == 2) ptenc_set_infile_enc_auto();
#ifdef DEBUG
          if (infile_enc_auto) {
            fprintf(stderr, "\nInput fd: %d, stdin?: %d, pipe?: %d\n", fd,
                 fd==fileno(stdin), (fstat(fd, &st)==0 && S_ISFIFO(st.st_mode)));
          }
#endif /* DEBUG */
          if (infile_enc_auto && fd != fileno(stdin)
              && !(fstat(fd, &st)==0 && S_ISFIFO(st.st_mode))) {
            char *enc;
            getc4(fp);
            getc4(fp);
            getc4(fp);
            getc4(fp);
            rewind(fp);
            enc = ptenc_guess_enc(fp, 0, 0);
            if (string_to_enc(enc) > 0) {
                infile_enc[fd] = string_to_enc(enc);
                fprintf(stderr, "(guessed encoding #%d: %s = %s)", fd, enc, enc_to_string(infile_enc[fd]));
            } else {
                infile_enc[fd] = get_file_enc();
            }
            if (enc) free(enc);
          }
          else infile_enc[fd] = get_file_enc();
        }
    }

    while (last < buffsize-30 && (i=getc4(fp)) != EOF && i!='\n' && i!='\r') {
        /* 30 is enough large size for one char */
        /* attention: 4 times of write_hex() eats 16byte */
#ifdef WIN32
        if (i == 0x1a && first == last &&
            fd == fileno(stdin) && _isatty(fd)) { /* Ctrl+Z on console */
                i = EOF;
                break;
        } else
#endif
        if (i == ESC) {
            if ((i=getc4(fp)) == '$') { /* ESC '$' (Kanji-in) */
                i = getc4(fp);
                if (i == '@' || i == 'B') {
                    injis = true;
                } else {               /* broken Kanji-in */
                    buffer[last++] = ESC;
                    buffer[last++] = '$';
                    if (is_tail(&i, fp)) break;
                    buffer[last++] = i;
                }
            } else if (i == '(') {     /* ESC '(' (Kanji-out) */
                i = getc4(fp);
                if (i == 'J' || i == 'B' || i == 'H') {
                    injis = false;
                } else {               /* broken Kanji-out */
                    buffer[last++] = ESC;
                    buffer[last++] = '(';
                    if (is_tail(&i, fp)) break;
                    buffer[last++] = i;
                }
            } else { /* broken ESC */
                buffer[last++] = ESC;
                if (is_tail(&i, fp)) break;
                buffer[last++] = i;
            }
        } else { /* rather than ESC */
            if (injis) { /* in JIS */
                long j = getc4(fp);
                if (is_tail(&j, fp)) {
                    buffer[last++] = i;
                    i = j;
                    break;
                } else { /* JIS encoding */
                    i = fromJIS(HILO(i,j));
                    if (i == 0) i = fromUCS(U_REPLACEMENT_CHARACTER);
                    write_multibyte(toBUFF(i));
                }
            } else {  /* normal */
                if        (infile_enc[fd] == ENC_SJIS && isSJISkanji1(i)) {
                    get_sjis(i, fp);
                } else if (infile_enc[fd] == ENC_EUC  && isEUCkanji1(i)) {
                    get_euc(i, fp);
                } else if (infile_enc[fd] == ENC_UTF8 && UTF8length(i) > 1) {
                    get_utf8(i, fp);
                } else {
                    buffer[last++] = i;
                }
            }
        }
    }

    if (i != EOF || first != last) buffer[last] = '\0';
    if (i == EOF || i == '\n' || i == '\r') injis = false;
    if (lastchar != NULL) *lastchar = i;

    if (buff2!= NULL) for (i=pos; i<=last; i++) buff2[i] = 0;
    /* buff2 is initialized */

    return last;
}

/* set encode of stdin if fp = NULL */
boolean setinfileenc(FILE *fp, const char *str)
{
    int enc;
    enc = string_to_enc(str);
    if (enc < 0) return false;
    infile_enc[fileno(fp)] = enc;
    return true;
}

boolean setstdinenc(const char *str)
{
    int enc;
    enc = string_to_enc(str);
    if (enc < 0) return false;
    infile_enc[fileno(stdin)] = enc;
    return true;
}

boolean setfileenc(const char *str)
{
    int enc;
    enc = string_to_enc(str);
    if (enc < 0) return false;
    file_enc = enc;
    return true;
}

#ifdef WIN32
void clear_infile_enc(FILE *fp)
{
    infile_enc[fileno(fp)] = ENC_UNKNOWN;
}
long ptenc_conv_first_line(long pos, long last, unsigned char *buff, const long buffsize)
{
   return last;
}
#else /* !WIN32 */
static const_string in_filter = NULL;
static FILE *piped_fp[NOFILE];
static int piped_num = 0;

void nkf_disable(void)
{
    in_filter = "";
}

#ifdef NKF_TEST
static void nkf_check(void)
{
    if (piped_num > 0) {
        fprintf(stderr, "nkf_check: %d nkf_open() did not closed.\n",
                piped_num);
    } else {
        fprintf(stderr, "nkf_check: nkf_open() OK.\n");
    }
}
#endif /* NKF_TEST */

/* 'mode' must be read */
FILE *nkf_open(const char *path, const char *mode) {
    char buff[PATH_MAX * 2 + 20];  /* 20 is enough gaps */
    char *name;
    FILE *fp;

    if (in_filter == NULL) {
        in_filter = kpse_var_value("PTEX_IN_FILTER");
        if (in_filter == NULL || strcasecmp(in_filter, "no") == 0) {
            nkf_disable();
        }
#ifdef NKF_TEST
        atexit(nkf_check);
#endif /* NKF_TEST */
    }

    if (in_filter[0] == '\0') return fopen(path, mode);
    name = xstrdup(path);
    if (kpse_readable_file(name) == NULL) {
        free(name);
        return NULL; /* can't read */
    }

    sprintf(buff, "%.*s < '%.*s'", PATH_MAX, in_filter, PATH_MAX, path);
    free(name);
    /* fprintf(stderr, "\n`%s`", buff); */
    fp = popen(buff , "r");
    if (piped_num < NOFILE) piped_fp[piped_num++] = fp;
    return fp;
}

/* we must close in stack order (FILO) or in queue order (FIFO) */
int nkf_close(FILE *fp) {
    infile_enc[fileno(fp)] = ENC_UNKNOWN;
    if (piped_num > 0) {
        if (fp == piped_fp[piped_num-1]) {  /* for FILO */
            piped_num--;
            return pclose(fp);
        }
        if (fp == piped_fp[0]) {  /* for FIFO */
            int i;
            piped_num--;
            for (i=0; i<piped_num; i++) piped_fp[i] = piped_fp[i+1];
            return pclose(fp);
        }
    }
    return fclose(fp);
}

#define break_if_bad_utf8_second(k) if ((k<0x80)||(k>0xBF)) { i--; k='\0'; break; }
#define write_hex_if_not_ascii(c) \
   if ((c>=0x20)&&(c<=0x7E)) buffer[last++]=c; else write_hex(c);
unsigned char *ptenc_from_utf8_string_to_internal_enc(const unsigned char *is)
{
    int i;
    long u = 0, j, len;
    int i1, i2, i3, i4;
    unsigned char *buf, *buf_bak;
    long first_bak, last_bak;

    if (terminal_enc != ENC_UTF8 || is_internalUPTEX()) return NULL;
    buf_bak = buffer;
    first_bak = first;
    last_bak = last;

    len = strlen(is)+1;
    buffer = buf = xmalloc(len);
    first = last = 0;

    for (i=0; i<strlen(is); i++) {
        i1 = is[i]; i2 = i3 = i4 = '\0';
        switch (UTF8length(i1)) {
        case 1:
            buffer[last++] = i1; /* ASCII */
            if (i1 == '\0') goto end;
            continue;
        case 2:
            i2 = is[++i]; break_if_bad_utf8_second(i2);
            u = UTF8BtoUCS(i1, i2);
            break;
        case 3:
            i2 = is[++i]; break_if_bad_utf8_second(i2);
            i3 = is[++i]; break_if_bad_utf8_second(i3);
            u = UTF8CtoUCS(i1, i2, i3);
            if (u == U_BOM) continue; /* just ignore */
            if (u == U_VOICED      && combin_voiced_sound(false)) continue;
            if (u == U_SEMI_VOICED && combin_voiced_sound(true))  continue;
            break;
        case 4:
            i2 = is[++i]; break_if_bad_utf8_second(i2);
            i3 = is[++i]; break_if_bad_utf8_second(i3);
            i4 = is[++i]; break_if_bad_utf8_second(i4);
            u = UTF8DtoUCS(i1, i2, i3, i4);
            break;
        default:
            u = U_REPLACEMENT_CHARACTER;
            break;
        }

        j = (u != 0) ? toBUFF(fromUCS(u)) : 0;
        if (j == 0) { /* can't represent in EUC/SJIS */
            if (last+16>=len) buffer = buf = xrealloc(buffer, len=last+64);
            write_hex_if_not_ascii(i1);
            if (i2 != '\0') write_hex_if_not_ascii(i2);
            if (i3 != '\0') write_hex_if_not_ascii(i3);
            if (i4 != '\0') write_hex_if_not_ascii(i4);
        } else {
            write_multibyte(j);
        }
    }
    buffer[last] = '\0';
 end:
    buffer = buf_bak;
    first = first_bak;
    last = last_bak;
    return buf;
}

unsigned char *ptenc_from_internal_enc_string_to_utf8(const unsigned char *is)
{
    int i;
    long u = 0, len;
    int i1 = EOF, i2 = EOF;
    unsigned char *buf, *buf_bak;
    long first_bak, last_bak;

    if (terminal_enc != ENC_UTF8 || is_internalUPTEX()) return NULL;
    buf_bak = buffer;
    first_bak = first;
    last_bak = last;

    len = strlen(is)+1;
    buffer = buf = xmalloc(len*4);
    first = last = 0;

    for (i=0; i<strlen(is); i++) {
        i1 = is[i];
        switch (multibytelen(i1)) {
        case 1:
            buffer[last++] = i1; /* ASCII */
            if (i1 == '\0') goto end;
            continue;
        case 2: /* i1: not ASCII */
            i2 = is[++i];
            if (i2 == '\0') {
              write_hex(i1); continue;
            } else {
              u = JIStoUCS2(toJIS(HILO(i1,i2)));
              if (u==0) {
                write_hex(i1); write_hex_if_not_ascii(i2); continue;
              }
            }
            break;
        default: /* reachable only if internal code is uptex */
            u = U_REPLACEMENT_CHARACTER;
            break;
        }
        write_multibyte(UCStoUTF8(u));
    }
    buffer[last] = '\0';
 end:
    buffer = buf_bak;
    first = first_bak;
    last = last_bak;
    return buf;
}

long ptenc_conv_first_line(long pos, long last, unsigned char *buff, const long buffsize)
  /* return new last */
{
    unsigned char *old, *new_buf; long new_last, i;
    if (internal_enc==ENC_UPTEX) return last; /* no conversion needed */
    old = xmalloc(last-pos+2);
    if (old==NULL) return last;
    strncpy(old, buff+pos, last-pos+1); old[last-pos+1]='\0';
    new_buf = ptenc_from_utf8_string_to_internal_enc(old);
    if (new_buf==NULL) { free(old); return last; }
    new_last=pos+strlen(new_buf)-1;
    if (new_last>=buffsize) new_last=buffsize-1;
    for (i=0;i<strlen(new_buf); i++) buff[pos+i]=new_buf[i];
    free(old); free(new_buf);
    return new_last;
}

#endif /* !WIN32 */
