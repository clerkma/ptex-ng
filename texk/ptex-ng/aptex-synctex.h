/*
  Copyright (c) 2015 Clerk Ma

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE

  Except as contained in this notice, the name of the copyright holder
  shall not be used in advertising or otherwise to promote the sale,
  use or other dealings in this Software without prior written
  authorization from the copyright holder.
*/

#ifndef APTEX_SYNCTEX
#define APTEX_SYNCTEX

void synctex_init (void);
void synctex_terminate (void);
void synctex_start_input (void);
void synctex_sheet (integer sync_mag);
void synctex_teehs (void);
void synctex_vlist (pointer this_box);
void synctex_tsilv (pointer this_box);
void synctex_void_vlist (pointer p, pointer this_box);
void synctex_hlist (pointer this_box);
void synctex_tsilh (pointer this_box);
void synctex_void_hlist (pointer p, pointer this_box);
void synctex_math (pointer p, pointer this_box);
void synctex_horizontal_rule_or_glue (pointer p, pointer this_box);
void synctex_kern (pointer p, pointer this_box);
void synctex_char (pointer p, pointer this_box);
void synctex_node (pointer p, pointer this_box);
void synctex_current (void);

#endif /* APTEX_SYNCTEX */
