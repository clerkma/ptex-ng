/*
  Copyright (c) 2023 Clerk Ma

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
*/

#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "mruby.h"
#include "mruby/irep.h"

#include "mlayout-tool-tag.c"
#include "mlayout-tool-otf.c"
#include "mlayout-tool-main.c"

/*
  mrbc -o mlayout-tool-tag.c -Bmlayout_tool_tag tag.rb
  mrbc -o mlayout-tool-otf.c -Bmlayout_tool_otf otf.rb
  mrbc -o mlayout-tool-main.c -Bmlayout_tool_main main.rb
*/

int main (int ac, char **av)
{
  if (ac < 2)
  {
    printf("Usage: mlayout-tool tool args...\n");
    printf(" tool: file filename [index]\n");
    printf("       dir  dirname\n");
    return 0;
  }

  mrb_state *mrb = mrb_open();
  mrb_load_irep(mrb, mlayout_tool_tag);
  mrb_load_irep(mrb, mlayout_tool_otf);
  mrb_load_irep(mrb, mlayout_tool_main);
  if (!mrb)
  {
    printf("Faile to load program.\n");
    return 1;
  }
  mrb_value self = mrb_obj_value(mrb->top_self);
  if (strcmp(av[1], "file") == 0)
  {
    if (ac == 3)
      mrb_funcall(mrb, self, "parse_font", 1, mrb_str_new_cstr(mrb, av[2]));
    else if (ac == 4)
      mrb_funcall(mrb, self, "parse_font", 2, mrb_str_new_cstr(mrb, av[2]),
        mrb_str_new_cstr(mrb, av[3]));
  }
  else if (strcmp(av[1], "dir") == 0)
  {
    if (ac == 3)
      mrb_funcall(mrb, self, "parse_directory", 1, mrb_str_new_cstr(mrb, av[2]));
  }
  mrb_close(mrb);
  return 0;
}
