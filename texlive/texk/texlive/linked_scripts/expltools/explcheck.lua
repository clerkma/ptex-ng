#!/usr/bin/env texlua
-- A command-line interface for the static analyzer explcheck.
local using_luatex, kpse = pcall(require, "kpse")
if using_luatex then
  kpse.set_program_name("texlua", "explcheck")
end
require("explcheck-cli")
