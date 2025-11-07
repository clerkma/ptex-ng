export TEXINPUTS=.:`pwd`/tex
export TFMFONTS=.:`pwd`/tfm
export TEXFORMATS=.:`pwd`/fmt
export TEXMFCNF=`pwd`/cnf
../src/aptex -ini "\input plain\dump"
mv plain.fmt fmt
mv plain.log fmt
../src/aptex +plain test.tex
rm *.log *.dvi
