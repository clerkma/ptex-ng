if exist build\mruby (
  rd /s /q build\mruby
) else (
  md build\mruby
)
set MRUBY_SOURCE=%CD%\..\src\mruby
set PREFIX=%CD%\build\mruby
set MRUBY_CONFIG=ci/msvc
set CFLAGS=-nologo -c -O2 -Oy
set BUILD_CMD=ruby %MRUBY_SOURCE%\minirake -f %MRUBY_SOURCE%\Rakefile
%BUILD_CMD% all
%BUILD_CMD% install
%BUILD_CMD% clean
copy build\mruby\lib\libmruby.lib .