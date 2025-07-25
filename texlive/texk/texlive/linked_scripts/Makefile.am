## $Id: Makefile.am 75830 2025-07-19 19:59:24Z karl $
## Makefile.am for the TeX Live subdirectory texk/texlive/linked_scripts/
##
## Copyright 2016-2025 Karl Berry <tex-live@tug.org>
## Copyright 2009-2015 Peter Breitenlohner <tex-live@tug.org>
## You may freely use, modify and/or distribute this file.
##
## TeX Live's extra scripts.

# Build lists of scripts.
scripts-list: Makefile
	@echo "Create $@"
	@echo "texmf_scripts='" >$@
	@for f in $(nobase_dist_texmf_scripts_SCRIPTS); do echo "$$f"; done >>$@
	@echo "'" >>$@
	cmp -s $(srcdir)/scripts.lst $@ || cp -f $@ $(srcdir)/scripts.lst

# Hard to remember the target name different from final file name.
scripts-lst:
	$(MAKE) scripts-list

if MAINTAINER_MODE
all-local: scripts-list
else !MAINTAINER_MODE
all-local:
endif !MAINTAINER_MODE

DISTCLEANFILES = scripts-list

EXTRA_DIST = scripts.lst

## Make sure $(bindir) exists
##
installdirs-local:
	$(MKDIR_P) $(DESTDIR)$(bindir)

# Copies of these scripts exist here for the sake of avoiding
# dangling symlinks produced by `make install'.
# The instances in texmf* are the masters (except when it is CTAN).
#
## We distinguish classes of scripts: `shell' and `other'.
## 
## On Unix systems a symlink from $(bindir) to the script is installed
## for all of them; on Windows nothing is installed in $(bindir) for
## `shell' scripts whereas for `other' (usually perl, lua, ...) scripts
## the standard wrapper ../$(WIN_WRAPPER)/runscript.exe is installed.
##
## WIN_WRAPPER is windows_wrapper (64-bit).
## 
## The "shell" list aren't just shell scripts, in either direction. Some
## /bin/sh scripts are listed as `other' scripts because they are
## correctly handled by the wrapper binaries. Conversely, some non-sh
## scripts are listed as `shell' because they depend on shell scripts or
## otherwise aren't useful on Windows.
## 
texmf_scriptsdir = $(datarootdir)/texmf-dist/scripts
texmf_shell_scripts = \
	adhocfilelist/adhocfilelist.sh \
	bibexport/bibexport.sh \
	checklistings/checklistings.sh \
	ctanbib/ctanbib \
	dtxgen/dtxgen \
	git-latexdiff/git-latexdiff \
	installfont/installfont-tl \
	latexfileversion/latexfileversion \
	listbib/listbib \
	listings-ext/listings-ext.sh \
	ltxfileinfo/ltxfileinfo \
	pagelayout/pagelayoutapi \
	pagelayout/textestvis \
	pdfbook2/pdfbook2 \
	pdfjam/pdfjam \
	pdftex-quiet/pdftex-quiet \
	pdfxup/pdfxup \
	pst-pdf/ps4pdf \
	simpdftex/simpdftex \
	texaccents/texaccents.sno \
	texlive-extra/allcm.sh \
	texlive-extra/allneeded.sh \
	texlive-extra/dvi2fax.sh \
	texlive-extra/dvired.sh \
	texlive-extra/fontinst.sh \
	texlive-extra/kpsetool.sh \
	texlive-extra/kpsewhere.sh \
	texlive-extra/ps2frag.sh \
	texlive-extra/pslatex.sh \
	texlive-extra/rubibtex.sh \
	texlive-extra/rumakeindex.sh \
	texlive-extra/texconfig-dialog.sh \
	texlive-extra/texconfig-sys.sh \
	texlive-extra/texconfig.sh \
	texlive-extra/texlinks.sh \
	texlive-extra/xetex-unsafe.sh \
	texlive-extra/xelatex-unsafe.sh \
	texosquery/texosquery.sh \
	texosquery/texosquery-jre5.sh \
	texosquery/texosquery-jre8.sh \
	tikztosvg/tikztosvg \
	typeoutfileinfo/typeoutfileinfo.sh \
	wordcount/wordcount.sh

# 
texmf_other_scripts = \
	a2ping/a2ping.pl \
	accfonts/mkt1font \
	accfonts/vpl2ovp \
	accfonts/vpl2vpl \
	albatross/albatross.sh \
	aomart/aom-fullref.pl \
	arara/arara.sh \
	attachfile2/pdfatfi.pl \
	authorindex/authorindex \
	bib2gls/bib2gls.sh \
	bib2gls/convertgls2bib.sh \
	bib2gls/datatool2bib.sh \
	bibcop/bibcop.pl \
	bibtexperllibs/ltx2unitxt \
	bookshelf/bookshelf-listallfonts \
	bookshelf/bookshelf-mkfontsel \
	bundledoc/arlatex \
	bundledoc/bundledoc \
	cachepic/cachepic.tlu \
	checkcites/checkcites.lua \
	citation-style-language/citeproc-lua.lua \
	chklref/chklref.pl \
	cjk-gs-integrate/cjk-gs-integrate.pl \
	clojure-pamphlet/pamphletangler \
	cluttex/cluttex.lua \
	context/perl/mptopdf.pl \
	convbkmk/convbkmk.rb \
	crossrefware/bbl2bib.pl \
	crossrefware/bibdoiadd.pl \
	crossrefware/bibmradd.pl \
	crossrefware/biburl2doi.pl \
	crossrefware/bibzbladd.pl \
	crossrefware/ltx2crossrefxml.pl \
	ctan-o-mat/ctan-o-mat.pl \
	ctanify/ctanify \
	ctanupload/ctanupload.pl \
	de-macro/de-macro \
	diadia/diadia.lua \
	digestif/digestif.texlua \
	dosepsbin/dosepsbin.pl \
	dviasm/dviasm.py \
	dviinfox/dviinfox.pl \
	easydtx/edtx2dtx.pl \
	ebong/ebong.py \
	eolang/eolang.pl \
	epspdf/epspdf.tlu \
	epspdf/epspdftk.tcl \
	epstopdf/epstopdf.pl \
	exceltex/exceltex \
	expltools/explcheck.lua \
	extractbb/extractbb.lua \
	fig4latex/fig4latex \
	findhyph/findhyph \
	fontools/afm2afm \
	fontools/autoinst \
	fontools/ot2kpx \
	fragmaster/fragmaster.pl \
	getmap/getmapdl.lua \
	glossaries/makeglossaries \
	glossaries/makeglossaries-lite.lua \
	hyperxmp/hyperxmp-add-bytecount.pl \
	jfmutil/jfmutil.pl \
	ketcindy/ketcindy.pl \
	kotex-utils/jamo-normalize.pl \
	kotex-utils/komkindex.pl \
	kotex-utils/ttf2kotexfont.pl \
	l3build/l3build.lua \
	l3sys-query/l3sys-query.lua \
	latex-git-log/latex-git-log \
	latex-papersize/latex-papersize.py \
	latex2man/latex2man \
	latex2nemeth/latex2nemeth \
	latexdiff/latexdiff-vc.pl \
	latexdiff/latexdiff.pl \
	latexdiff/latexrevise.pl \
	latexindent/latexindent.pl \
	latexmk/latexmk.pl \
	latexpand/latexpand \
	light-latex-make/llmk.lua \
	lilyglyphs/lily-glyph-commands.py \
	lilyglyphs/lily-image-commands.py \
	lilyglyphs/lily-rebuild-pdfs.py \
	ltximg/ltximg.pl \
	luafindfont/luafindfont.lua \
	luaotfload/luaotfload-tool.lua \
	lwarp/lwarpmk.lua \
	m-tx/m-tx.lua \
	makedtx/makedtx.pl \
	make4ht/make4ht \
	markdown/markdown2tex.lua \
	match_parens/match_parens \
	mathspic/mathspic.pl \
	memoize/memoize-clean.pl \
	memoize/memoize-clean.py \
	memoize/memoize-extract.pl \
	memoize/memoize-extract.py \
	mf2pt1/mf2pt1.pl \
	minted/latexminted.py \
	mkgrkindex/mkgrkindex \
	mkjobtexmf/mkjobtexmf.pl \
	mkpic/mkpic \
	multibibliography/multibibliography.pl \
	musixtex/musixflx.lua \
	musixtex/musixtex.lua \
	optexcount/optexcount \
	pax/pdfannotextractor.pl \
	pdfcrop/pdfcrop.pl \
	pdflatexpicscale/pdflatexpicscale.pl \
	pedigree-perl/pedigree.pl \
	perltex/perltex.pl \
	petri-nets/pn2pdf \
	pfarrei/a5toa4.tlu \
	pfarrei/pfarrei.tlu \
	pkfix-helper/pkfix-helper \
	pkfix/pkfix.pl \
	pmxchords/pmxchords.lua \
	ppmcheckpdf/ppmcheckpdf.lua \
	pst2pdf/pst2pdf.pl \
	ptex-fontmaps/kanji-config-updmap-sys.sh \
	ptex-fontmaps/kanji-config-updmap-user.sh \
	ptex-fontmaps/kanji-config-updmap.pl \
	ptex-fontmaps/kanji-fontmap-creator.pl \
	ptex2pdf/ptex2pdf.lua \
	purifyeps/purifyeps \
	pygmentex/pygmentex.py \
	pythontex/depythontex.py \
	pythontex/pythontex.py \
	rubik/rubikrotation.pl \
	runtexfile/runtexfile.lua \
	runtexshebang/runtexshebang.lua \
	show-pdf-tags/show-pdf-tags.lua \
	spix/spix.py \
	splitindex/splitindex.pl \
	sqltex/sqltex \
	srcredact/srcredact.pl \
	sty2dtx/sty2dtx.pl \
	svn-multi/svn-multi.pl \
	tex4ebook/tex4ebook \
	texblend/texblend \
	texcount/texcount.pl \
	texdef/texdef.pl \
	texdiff/texdiff \
	texdirflatten/texdirflatten \
	texfindpkg/texfindpkg.lua \
	texdoc/texdoc.tlu \
	texfot/texfot.pl \
	texlive/fmtutil-sys.sh \
	texlive/fmtutil-user.sh \
	texlive/fmtutil.pl \
	texlive/mktexlsr \
	texlive/mktexmf \
	texlive/mktexpk \
	texlive/mktextfm \
	texlive/rungs.lua \
	texlive/tlmgr.pl \
	texlive/updmap-sys.sh \
	texlive/updmap-user.sh \
	texlive/updmap.pl \
	texlive-extra/e2pall.pl \
	texliveonfly/texliveonfly.py \
	texloganalyser/texloganalyser \
	texlogfilter/texlogfilter \
	texlogsieve/texlogsieve \
	texplate/texplate.sh \
	thumbpdf/thumbpdf.pl \
	tlcockpit/tlcockpit.sh \
	tlshell/tlshell.tcl \
	typog/typog-grep.pl \
	ulqda/ulqda.pl \
	urlbst/urlbst \
	vpe/vpe.pl \
	webquiz/webquiz.py \
	xindex/xindex.lua \
	yplan/yplan

nobase_dist_texmf_scripts_SCRIPTS = \
	$(texmf_shell_scripts) \
	$(texmf_other_scripts)

## Symlinks within $(bindir): FILE:LINK here means "ln -s FILE LINK" is done.
bin_links = \
	allcm:allec \
	cluttex:clxelatex \
	cluttex:cllualatex \
	epstopdf:repstopdf \
	extractbb:ebb \
	fmtutil:mktexfmt \
	kpsetool:kpsexpand \
	kpsetool:kpsepath \
	mktexlsr:texhash \
	pdfcrop:rpdfcrop \
	texdef:latexdef

# 
# The idea is to install the scripts themselves in texmf*/scripts, and
# have bin/arch/foo be a symlink to, say,
# ../../texmf-dist/scripts/foo/foo.pl.  That way we save a bit of disk
# space, but more importantly omit duplication, and most
# importantly of all make it possible to invoke the same Perl script on
# Windows (see ../$(WIN_WRAPPER)).
#
# The installation into texmf*/scripts is ideally done with ctan2tl.
# The copy of the wrapper for Windows is also done by ctan2tl.  (And
# for shell scripts, its addition must be undone by hand.)  The
# check-wrapper-consistency script checks after the fact for problems.
#
# We also keep a copy of all the scripts here and install into the runtime.
# This is purely for other distro builders, so symlinks are not dangling in
# the inst/bin dir (it is meaningless in native TL).  We have not yet
# written the auto-update to ensure linked_scripts is actually up to date
# with the masters in Master/texmf*/scripts, but one day.
#
# We support native builds, multiplatform or not, as well as distro builds.
#
EXTRA_DIST += ../../../build-aux/relpath
relpath = $(SHELL) $(top_srcdir)/../../build-aux/relpath
runscript = $(top_srcdir)/$(WIN_WRAPPER)/runscript.exe

# Make a copy of all targets and link names, for distros etc., so they
# don't have to duplicate the special cases for memoize* etc. below. It
# would be cleaner to make the file and then use it in both the install
# and uninstall hooks, but we'll leave that for another time.
linkfile = $(abs_builddir)/scripts-targets.lst

install-data-hook: installdirs-local
if WIN32
	@for s in $(texmf_other_scripts); do \
	  t=`basename $$s | sed 's,\.[^/]*$$,,' | tr '[A-Z]' '[a-z]'`; \
	  echo "$(INSTALL_SCRIPT) '$(runscript)' '$(DESTDIR)$(bindir)/$$t.exe'"; \
	  $(INSTALL_SCRIPT) $(runscript) $(DESTDIR)$(bindir)/$$t.exe; \
	done
	$(INSTALL_SCRIPT) $(top_srcdir)/$(WIN_WRAPPER)/runscript.dll $(DESTDIR)$(bindir)
	$(INSTALL_SCRIPT) $(top_srcdir)/$(WIN_WRAPPER)/runscript.exe $(DESTDIR)$(bindir)
	$(INSTALL_SCRIPT) $(top_srcdir)/$(WIN_WRAPPER)/runscript.tlu $(DESTDIR)$(bindir)
else !WIN32
	>$(linkfile)
# Name the link as the basename, removing any extension,
# except for memoize* and listings-ext.sh, which are documented to be
# used with extension. Downcase for the sake of TeXcount.pl -> texcount.
# Unfortunately this logic is duplicated in uninstall-hook.
	@REL=`$(relpath) '$(DESTDIR)' '$(bindir)' '$(datarootdir)'`; \
	if test -z "$$REL"; then \
	  echo 'unable to compute relative path for linked scripts' >&2; \
	  exit 1; \
	fi; \
	cd $(DESTDIR)$(bindir) && \
	  for s in $(nobase_dist_texmf_scripts_SCRIPTS); do \
	    target=`basename $$s | tr '[A-Z]' '[a-z]'`; \
## we can't portably use egrep or grep -E or ere patterns.
	    echo "$$s" | grep 'memoize-.*\.' >/dev/null \
	    || echo "$$s" | grep 'listings-ext.sh' >/dev/null \
	    || target=`echo $$target | sed 's,\.[^/]*$$,,'`; \
	    rm -f $$target; \
	    echo "creating link '$$target' -> '$$REL/texmf-dist/scripts/$$s'";\
	    $(LN_S) $$REL/texmf-dist/scripts/$$s $$target || exit 1; \
	    echo texmf-dist/scripts/$$s $$target >>$(linkfile); \
	  done && \
	  for s in $(bin_links); do \
	    link=`echo $$s | sed 's,.*:,,'`; \
	    file=`echo $$s | sed 's,:.*,,'`; \
	    rm -f $$link; \
	    echo "creating bin link '$$link' -> '$$file'"; \
	    $(LN_S) $$file $$link || exit 1; \
	    echo $$file $$link >>$(linkfile); \
	  done && \
## man dir link for those mans which can use it, but don't overwrite a
## real file, e.g., /usr/bin/man.
	  if test -d "$$REL/texmf-dist/doc/man" \
	     && test -z "$(TL_INSTALL_OMIT_MAN_LINK)" \
	     && (test -h man || test ! -e man); then \
	    echo "creating link 'man' -> '$$REL/texmf-dist/doc/man'"; \
	    rm -f man; \
	    (ln -s "$$REL/texmf-dist/doc/man" man || :); \
	  else :; fi
# If we got to the end, update the $(linkfile) in srcdir if needed.
	cmp -s $(srcdir)/`basename $(linkfile)` $(linkfile) \
	|| cp -f $(linkfile) $(srcdir)/`basename $(linkfile)`
endif !WIN32

.PHONY: install-links
install-links:

# 
# For WIN32 (MinGW or native) we have wrapper binaries with .exe,
# for Cygwin and Unix we have symlinks without .exe.
if WIN32
WINEXT = $(EXEEXT)
endif WIN32

uninstall-hook:
	@for s in $(nobase_dist_texmf_scripts_SCRIPTS); do \
	  target=`basename $$s | tr '[A-Z]' '[a-z]'`; \
	  echo "$$s" | grep 'memoize-.*\.' >/dev/null \
	  || echo "$$s" | grep 'listings-ext.sh' >/dev/null \
	  || target=`echo $$target | sed 's,\.[^/]*$$,,'`; \
	  rm -f $(DESTDIR)$(bindir)/$$target$(WINEXT); \
	done
	@for s in $(bin_links); do \
	  link=`echo $$s | sed 's,.*:,,'`; \
	  rm -f $(DESTDIR)$(bindir)/$$link$(WINEXT); \
	done
if !WIN32
	if test -h $(DESTDIR)$(bindir)/man; then \
	  rm -f $(DESTDIR)$(bindir)/man; \
	else :; fi
endif !WIN32

# 
# This target is invoked by tl-check-wrapper-consistency (run from cron).
.PHONY: echo-shell-scripts
shell_scripts = $(texmf_shell_scripts)
echo-shell-scripts:
	@echo $(shell_scripts)

# just a shorter name for all the scripts for convenience.
all_scripts = $(nobase_dist_texmf_scripts_SCRIPTS)

# Compare the list of scripts to the extant files in the repo;
# should be exactly the same. Run from cron.
check-make-vs-files:
# We descend two levels here because of context/perl/mptopdf.pl.
	find "$(srcdir)"/*/* -type f | sed "s,$(srcdir)/,," | sort \
	  >tmplist.files
	echo $(all_scripts) | tr ' ' '\n' | sort >tmplist.make
	comm -3 tmplist.files tmplist.make
