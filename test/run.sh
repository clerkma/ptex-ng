export TEXMFCNF=`pwd`/cnf
export APTEX=../src/aptex
$APTEX -ini plain.ini
mv plain.fmt fmt
mv plain.log fmt
$APTEX +plain test.tex
$APTEX +plain test-hz.tex
$APTEX +plain test-lpcode-rpcode.tex
$APTEX +plain test-pdffontexpand.tex
$APTEX +plain test-pdfprotrudechars.tex
rm *.log *.dvi
