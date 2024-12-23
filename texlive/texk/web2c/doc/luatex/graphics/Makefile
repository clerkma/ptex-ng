#$Id: Makefile,v 1.1 2009/01/13 23:33:53 hahe Exp hahe $

all:	fontdir.pdf

%.pdf:	%.fig
	fig2dev -Lpdf $< > $@

clean:
	- rm -f *.bak

allclean:
	make clean
	- rm -f *.pdf
