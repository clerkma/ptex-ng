#!/bin/sh
##
## This is `typeoutfileinfo.sh', a minimalist shell script for Unices.
##
##     ./typeoutfileinfo.sh [FILENAME].[EXT]
##
## runs latex with the readprov package:
##
##     https://ctan.org/pkg/readprov
##
## in order to display [FILENAME].[TXT]'s FILE INFO (\listfile entry)
## using \typeout. This requires that [FILENAME].[EXT] contains a
## \ProvidesFile, \ProvidesPackage, or \ProvidesClass command.
latex \\RequirePackage{readprov}\\ReadFileInfos{$1}\
\\typeout{^^J^^J + $1 info: + \\csname ver@$1\\endcsname^^J}\
\\batchmode\\stop
##
## Copyright © 2012 Uwe Lück (deceased)
##
## This program may be distributed and/or modified under the
## conditions of the LaTeX Project Public License, either version 1.3c
## of this license or (at your option) any later version.
## The latest version of this license is in
##   https://www.latex-project.org/lppl.txt
## and version 1.3c or later is part of all distributions of LaTeX
## version 1999/12/01 or later.
##
## There is NO WARRANTY.
##
## This is package version v0.32 as of 2023-06-30.
##
## CREDITS: This work derived from Harald Harders' `latexfileversion'
## as a simplification using my `readprov'. Reinhard Kotucha
## improved the code, see HISTORY.
##
## PURPOSE/BACKGROUND: A package like this, `latexfileversion' or
## `ltxfileinfo' when you wonder which version of a source file
## (package, document component) is available in some directory --
## I typically test package changes by symbolic links in single
## project directories before installing them globally, and then
## sometimes don't remember ... when I get an "undefined" error,
## I wonder whether I have installed the symbolic link or whether
## I just forgot to define this, or whether I lost the most recent
## version ...
##
## HISTORY:
## v0.1a  2012/03/16
## v0.2   2012/09/16  adds \batchmode thanks to Heiko Oberdiek on texhax
## v0.21  2012/09/19  shebang line corrected/improved by Reinhard Kotucha
## v0.3   2012/09/27  long line instead of here document,
##                      different spacing, * -> + (misinterpreded)
## v0.31  2012/09/28  code line broken by `\', thanks to Reinhard Kotucha
## v0.32  2023-06-30  Uwe Lück is deceased:
##                      https://www.webdesign-bu.de/uwe_lueck/contact.html