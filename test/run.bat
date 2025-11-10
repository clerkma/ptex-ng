@echo off
@REM set web2c variables
set TEXMFCNF=%CD%\cnf
@echo on
..\build-msvc\aptex -ini "\input plain\dump"
@echo off
move /Y plain.fmt fmt
move /Y plain.log fmt
@echo on
..\build-msvc\aptex +plain test.tex
@echo off
del *.log *.dvi
@REM recover variables (to make texlive safe)
set TEXMFCNF=
@echo on
