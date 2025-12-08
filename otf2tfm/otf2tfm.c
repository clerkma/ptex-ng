#if 0
set -x
cc -W -Wall -O2 -I/opt/pkg/include/freetype2/ -L/opt/pkg/lib/ -o `basename $0 .c` $0 -lfreetype
exit $?
#endif
#include<stdio.h>
#include<ft2build.h>
#include FT_FREETYPE_H
#include FT_BBOX_H
typedef int I;typedef unsigned int UI;typedef const char* S;typedef FT_Face F;
#define FONT av[1]
#define b(c,a,b) if(c){a;}else{b;}
#define g f->glyph
#define p printf
#define R return
#define _I static I
#define UK FT_Err_Unknown_File_Format
#define M(b...) for(n=0;n<256;++n){b;}
#define FN(n) static void n(F f)
#define inpp /(double)f->units_per_EM
static FT_Error err;I wd[256],ht[256],dp[256];
_I cd(I *a){I n,j,c=0;M(for(j=n+1;j<256&&a[n]!=a[j];j++);c+=j==256;);R c;}
FN(met){UI n;FT_BBox b;M(b(FT_Load_Glyph(f,n,FT_LOAD_NO_SCALE),p("load glyph %d failed\n",n),);
  wd[n]=g->advance.x;FT_Outline_Get_BBox(&g->outline,&b);ht[n]=b.yMax,dp[n]=b.yMin;);}
I main(I ac,S av[]){FT_Library l;FT_Init_FreeType(&l);F f;
  b(ac<2,p("input\n");R -1,);
  b(UK==(err=FT_New_Face(l,FONT,0,&f)),p("fmt err: %s\n",FONT);R -3,
    b(err,p("other err: %d",err);R -4,));/*p("%ld\n",f->num_glyphs);*/met(f);UI n;
  M(p("(COMMENT c: %d, wd: %g, ht: %g, dp: %d)\n",n,wd[n]inpp,ht[n]inpp,dp[n]););
  p("distinct ht: %d\n",cd(ht));p("distinct dp: %d\n",cd(dp));
  FT_Done_FreeType(l);}
