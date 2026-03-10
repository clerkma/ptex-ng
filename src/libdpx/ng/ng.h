#define SIGNED_QUAD int32_t
typedef long long scaled;
extern void pdf_rule_out (scaled rule_wd, scaled rule_ht);
extern spt_t ng_packet_width (SIGNED_QUAD ch, int ng_font_id, int extend);
extern void ng_set (SIGNED_QUAD ch, int ng_font_id, SIGNED_QUAD h, SIGNED_QUAD v, int extend);

#define dvi_yoko 0
#define dvi_tate 1
#define dvi_dtou 3

extern void ng_set_packet (int32_t ch, int vf_font, int32_t h, int32_t v, int extend);
