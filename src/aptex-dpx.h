//
// This is a bridge header over dvipdfm-x.
// PDF output functions from texlive/texk/dvipdfm-x
//

/* from "dvipdfm-x/fontmap.h" */
extern void pdf_init_fontmaps (void);
extern void pdf_close_fontmaps (void);
extern int pdf_load_fontmap_file (const char *filename, int map_mode);

/* from "dvipdfm-x/pdfdoc.h" */
struct pdf_dev_setting {
    double      dvi2pts;   /* conversion unit */
    int         precision; /* number of decimal digits kept */
    int         ignore_colors; /* 1 for black or white */
};

struct pdf_enc_setting {
    int         key_size;
    uint32_t    permission;
    const char *uplain, *oplain; /* password */
    int         use_aes;
    int         encrypt_metadata;
};

struct pdf_obj_setting {
    int         enable_objstm;
    int         enable_predictor;
    int         compression_level;
};

struct pdf_setting
{
    int    ver_major, ver_minor;
    double media_width, media_height;
    struct {
      double x, y;
    } annot_grow_amount;
    int    outline_open_depth;
    int    check_gotos;
    int    enable_manual_thumb;
    int    enable_encrypt;
    struct pdf_enc_setting encrypt;
    struct pdf_dev_setting device;
    struct pdf_obj_setting object;
};

extern void pdf_open_document (const char *filename, const char * creator,
  const unsigned char * id1, const unsigned char * id2, struct pdf_setting settings);
extern void pdf_close_document (void);
extern void pdf_doc_begin_page (double scale, double x_origin, double y_origin);
extern void pdf_doc_end_page (void);
typedef struct pdf_rect {
  double llx, lly, urx, ury;
} pdf_rect;
extern void pdf_doc_set_mediabox (unsigned page_no, const pdf_rect *mediabox);

/* from "dvipdfm-x/pdfobj.h" */
extern long pdf_output_stats (void);

/* from "dvipdfm-x/pdfdev.h" */
extern void graphics_mode (void);
typedef signed long spt_t;
extern void pdf_dev_set_rule (spt_t xpos, spt_t ypos, spt_t width, spt_t height);
extern void pdf_dev_set_dirmode (int dir_mode);
extern void pdf_dev_begin_actualtext (uint16_t * unicodes, int len);
extern void pdf_dev_end_actualtext (void);

/* from "dvipdfm-x/pdflimits.h" */
#define PDF_VERSION_MIN 13
#define PDF_VERSION_MAX 20

/* from "dvipdfm-x/specials.h" */
extern int spc_exec_at_begin_document (void);
extern void spc_exec_at_end_document (void);
extern int spc_exec_at_begin_page (void);
extern int spc_exec_at_end_page (void);
extern int spc_exec_special (const char *buffer, long size,
 double x_user, double y_user, double dpx_mag, int * is_drawable, pdf_rect *rect);

/* from "dvipdfm-x/dvi.c" */
extern int dvi_locate_font (const char *tfm_name, spt_t ptsize);
extern int dvi_locate_native_font (const char *filename, uint32_t fidx,
  spt_t ptsize, int layout_dir, int extend, int slant, int embolden);

/* from "src/libdpx/ng/dvi_ng.c" */
extern void ng_set (int32_t ch, int ng_font_id, int32_t h, int32_t v);
extern void ng_gid (uint16_t gid, int ng_font_id, int32_t h, int32_t v);
extern void ng_layer (uint16_t gid, int ng_font_id, int32_t h, int32_t v, uint8_t r, uint8_t g, uint8_t b);
extern void spc_moveto (int32_t, int32_t);

/* dvipdfm-x/dvipdfmx.c */
extern int dpx_util_format_asn_date (char *, int);
typedef struct {
  uint32_t A, B, C, D;
  size_t nblocks;
  unsigned char buf[64];
  int count;
} MD5_CONTEXT;
extern void MD5_init (MD5_CONTEXT *);
extern void MD5_write (MD5_CONTEXT *, const unsigned char *, unsigned int);
extern void MD5_final (unsigned char *, MD5_CONTEXT *);
