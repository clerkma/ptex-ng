export TEXMFCNF=`pwd`/cnf
export APTEX=../src/aptex
$APTEX -ini plain.ini
mv plain.fmt fmt
mv plain.log fmt
$APTEX +plain test.tex
$APTEX +plain test-hz.tex
rm *.log *.dvi
