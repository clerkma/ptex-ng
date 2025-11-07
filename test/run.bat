@echo off
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
