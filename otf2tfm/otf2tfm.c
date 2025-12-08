// cc -O2 -I /opt/pkg/include/freetype2/ -L /opt/pkg/lib/ otf2tfm.c -lfreetype
#include<stdio.h>
#include<ft2build.h>
#include FT_FREETYPE_H
#include FT_BBOX_H
typedef int I;typedef unsigned int UI;typedef char* S;typedef FT_Face F;
#define FONT "/Users/ldbeth/Library/Fonts/Bookerly-Regular.ttf"
#define b(c,a,b) if(c){a;}else{b;}
#define g f->glyph
#define p printf
#define R return
#define UK FT_Err_Unknown_File_Format
#define M(b...) for(n=0;n<256;++n){b;}
#define _ static void
#define FN(n) _ n(F f)
#define inpp /(double)ppem
F f;static FT_Error err;I wd[256];I ht[256];I dp[256];
static I cd(I *a){I c=0;I n;
  M(I j;for(j=n+1;j<256;j++)if(a[n]==a[j])break;if(j==256)c++;);
  R c;}
_ bbox(FT_Outline* o,int *h, int* d){FT_BBox b;
  FT_Outline_Get_BBox(o,&b);*h=b.yMax;*d=b.yMin;}
FN(met){UI n;
  M(err=FT_Load_Glyph(f,n,FT_LOAD_NO_SCALE);
    b(err,p("load glyph %d failed\n",n),);
    wd[n]=g->advance.x;
    bbox(&g->outline,&ht[n],&dp[n]););}
I main(I ac,S av[]){FT_Library l;
  FT_Init_FreeType(&l);
  err=FT_New_Face(l,FONT,0,&f);
  b(err==UK,p("fmt err: %s\n",FONT),b(err,p("other err: %d",err),));
  // p("%ld\n",f->num_glyphs);
  met(f);
  I ppem=f->units_per_EM;UI n;
  M(p("(COMMENT c: %d, wd: %g, ht: %g, dp: %d)\n",
      n,wd[n]inpp,ht[n]inpp,dp[n]););
  p("distinct ht: %d\n",cd(ht));
  p("distinct dp: %d\n",cd(dp));
  FT_Done_FreeType(l);}
