export TEXMFCNF=`pwd`/cnf
export APTEX=../src/aptex
$APTEX -ini plain.ini
mv plain.fmt fmt
mv plain.log fmt
$APTEX +plain test.tex
rm *.log *.dvi
