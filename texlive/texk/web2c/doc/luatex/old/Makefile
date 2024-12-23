# $Id: Makefile 4106 2011-04-10 12:51:54Z hhenkel $

pdffiles=	luatexref-t.pdf functionref.pdf

all:	$(pdffiles)	

luatexref-t.pdf:	luatexref-t.tex

functionref.pdf:	functionref.tex fdata.lua fdata_epdf.lua fdata_img.lua

%.pdf:	%.tex
	context $<

clean:
	- rm -f *.log
	- rm -f *.tuc

allclean:
	make clean
	- rm -f $(pdffiles)
