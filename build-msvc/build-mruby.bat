if exist mruby (
  rd /s /q mruby
)
xcopy ..\src\mruby mruby\ /e/h
set MRUBY_CONFIG=appveyor_config.rb
set CFLAGS=-nologo -c -O2 -Oy
cd mruby
ruby .\minirake all
copy build\host\lib\libmruby.lib ..\libmruby.lib
cd ..
