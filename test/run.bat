@echo off
@REM set web2c variables
set TEXINPUTS=.;%CD%\tex
set TFMFONTS=.;%CD%\tfm
set TEXFORMATS=.;%CD%\fmt
set TEXMFCNF=%CD%\cnf
@echo on
..\build-msvc\aptex -ini "\input plain\dump"
@echo off
move /Y plain.fmt fmt
move /Y plain.log fmt
@echo on
..\build-msvc\aptex +plain test.tex
del *.log *.dvi
@REM recover variables (to make texlive safe)
@echo off
set TEXINPUTS=
set TFMFONTS=
set TEXFORMATS=
set TEXMFCNF=
@echo on
