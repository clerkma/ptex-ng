if exist build\mruby (
  rd /s /q build\mruby
)
xcopy ..\src\mruby build\mruby\ /e/h
set MRUBY_CONFIG=ci/msvc
set CFLAGS=-nologo -c -O2 -Oy
cd build\mruby
ruby .\minirake all
copy build\host\lib\libmruby.lib ..\..\libmruby.lib
cd ..\..
