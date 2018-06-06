copy ..\src\mruby mruby\ /e/h
set MRUBY_CONFIG=appveyor_config.rb
set CFLAGS=-nologo -c -O2 -Oy
ruby .\minirake all
