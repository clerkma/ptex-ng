# -----------------------------------*- mode: Makefile; -*--
# Makefile for ps2eps documentation
# ----------------------------------------------------------
# $Id: Makefile 139 2012-06-14 08:40:32Z bless $
# $Source: /home/bless/src/ps2eps/cvs-repository/ps2eps/doc/Makefile,v $
# ----------------------------------------------------------
# `$@' The file name of the target of the rule.
# `$%' The target member name, when the target is an archive member.
# `$<' The name of the first prerequisite.
# `$?' The names of all the prerequisites that are newer than the target, with spaces between them.
# `$^' The names of all the prerequisites, with spaces between them.

.PHONY: manpages pdf html

DOCBOOK_FLAGS=-c /usr/share/sgml/docbook/dtd/catalog

DOCBOOK_TO_MAN=docbook2man
DOCBOOK_TO_HTML=docbook2html
DOCBOOK_TO_PDF=docbook2pdf   #needs pdfjadetex

MANPAGES=ps2eps.1 bbox.1
PDF=ps2eps.pdf bbox.pdf
HTML=ps2eps.html bbox.html

all: manpages pdf html

manpages: $(MANPAGES)
pdf: $(PDF)
html: $(HTML)

%.pdf : %.1
	groff -m mandoc $< | ps2pdf - $@  # when having .1
#	$(DOCBOOK_TO_PDF) $(DOCBOOK_FLAGS) $<

%.html : %.sgml
#	groff -m mandoc -Thtml $< >$@ # when having .1
	$(DOCBOOK_TO_HTML) $(DOCBOOK_FLAGS) $<
	mv index.html $@

%.1 : %.sgml
	$(RM) $@
	$(DOCBOOK_TO_MAN) $(DOCBOOK_FLAGS) $<

clean:
	rm $(MANPAGES) $(PDF) $(HTML) manpage.refs manpage.links

# --------------------------------------------------------
