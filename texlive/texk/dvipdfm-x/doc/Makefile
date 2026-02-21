# $Id: Makefile 77942 2026-02-18 15:37:23Z karl $
# Minimal Makefile for dvipdfmx manual.
# Public domain. Originally written by Karl Berry, 2026.

texfot = texfot \
  --ignore 'Package microtype Warning: Unable to apply'

doc = dvipdfmx
$(doc).pdf: $(doc).tex fdl-1.3.tex
	$(texfot) xelatex $(doc).tex
