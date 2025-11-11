@echo off
@REM set web2c variables
set TEXMFCNF=%CD%\cnf
set APTEX=..\build-msvc\aptex
@echo on
%APTEX% -ini plain.ini
@echo off
move /Y plain.fmt fmt
move /Y plain.log fmt
@echo on
%APTEX% +plain test.tex
@echo off
del *.log *.dvi
@REM recover variables (to make texlive safe)
set TEXMFCNF=
set APTEX=
@echo on
