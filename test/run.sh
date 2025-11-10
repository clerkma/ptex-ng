export TEXMFCNF=`pwd`/cnf
../src/aptex -ini "\input plain\dump"
mv plain.fmt fmt
mv plain.log fmt
../src/aptex +plain test.tex
rm *.log *.dvi
