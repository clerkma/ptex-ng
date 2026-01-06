#if 0
cc -W -Wall -O2 -I/opt/pkg/include/freetype2/ -L/opt/pkg/lib/ -o `basename $0 .c` $0 -lfreetype;exit $?
#endif
#include<stdio.h>
#include<ft2build.h>
#include FT_FREETYPE_H
#include FT_BBOX_H
typedef int I;typedef unsigned int UI;typedef short S;typedef const char*C;typedef FT_Face F;
#define $(c,a,b) if(c){a;}else{b;}
#define E(a...) dprintf(2,a)
#define g f->glyph
#define ng [n]=g->
#define P printf
#define R return
#define G(w) FT_##w##_Glyph
#define _ static
#define M(b...) for(n=0;n<256;++n){b;}
#define FN(n) _ void n(F f)
_ FT_Error err;_ I w[256];_ S h[256],d[256];
FN(mt){UI n;M($(G(Load)(f,n,FT_LOAD_NO_SCALE),E("ld gyp %d failed\n",n),);
  w ng advance.x;FT_Set_Pixel_Sizes(f,10,0);G(Load)(f,n,0);
  G(Render)(g,FT_RENDER_MODE_LIGHT);h ng bitmap_top;d ng bitmap.rows-h[n]);}
I main(I ac,C*av){FT_Library l;FT_Init_FreeType(&l);F f;$(ac<2,P("input\n");R 1,);
  $(FT_Err_Unknown_File_Format==(err=FT_New_Face(l,av[1],0,&f)),
    E("fmt err: %s\n",av[1]);R 3,$(err,E("other err: %d\n",err);R 4,));
  P("num g: %ld\n",f->num_glyphs);mt(f);UI n;FT_Done_FreeType(l);
  M(P("(COMMENT c: %d, wd: %d, ht: %d, dp: %d)\n",n,w[n],h[n],d[n]));}
