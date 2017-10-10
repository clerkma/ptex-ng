#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cairo-pdf.h"

int main (void)
{
  cairo_surface_t * out_surface;
  cairo_t * out_ctx;
  
  out_surface = cairo_pdf_surface_create("test.pdf", 60.0, 60.0);
  out_ctx = cairo_create(out_surface);
  cairo_rectangle(out_ctx, 20.0, 20.0, 20.0, 20.0);
  cairo_set_source_rgb(out_ctx, 1.0, 0.0, 0.0);
  cairo_fill(out_ctx);
  cairo_destroy(out_ctx);
  cairo_surface_destroy(out_surface);
  
  return 0;
}
