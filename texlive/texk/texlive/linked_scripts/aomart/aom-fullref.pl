#!/usr/bin/perl
#
# Convert the refences to \fullref.  Usage:
# perl fullref.pl original.tex > converted.tex
#
#
# Copyright (C) 2010-2021 Annals of Mathematics.  Licenses under CC0
#
# Author: Boris Veytsman
#
# Version: 0.9, 2010/12/04

use strict;

# List of patterns for 'equation' words
my @eqsynonyms = ('\S*equa\S*', 'relat\S*',  'item\S*',
		  'condition\S*',
		  '\S*propert\S*');

# First we skip preamble
while(<>) {
    print;
    last if (/\\begin\{document\}/);
}

# This is a trick to catch \ref being the first word on the line
my $prevline = "";

# Processing document
while (<>) {
    if ((/^[\s~]*\(?\\ref/) || (/^[\s~]*\\eqref/)) {
	chomp $prevline;
	$_ = "$prevline $_";
    } else {
	print $prevline;
    }
    
    # Now the meat of the substitutions
    # We add initial space, so all words have space before them.
    $_ = " $_";
    foreach my $synonym (@eqsynonyms) {
	s/(\s)($synonym)[\s~]*\(\\ref\{([^\}]+)\}\)/$1\\pfullref{$2}{$3}/gi;
	s/(\s)($synonym)[\s~]*\\eqref\{([^\}]+)\}/$1\\eqfullref{$2}{$3}/gi;
    }
    s/(\s)([^\s\\]\S*[^~\s\(\)\[\]])[\s~]*\[\\ref\{([^\}]+)\}\]/$1\\bfullref{$2}{$3}/g;
    s/(\s)([^\s\\]\S*[^~\s\(\)\[\]])[\s~]*\\ref\{([^\}]+)\}/$1\\fullref{$2}{$3}/g;
    s/(\s)([^\s\\]\S*[^~\s\(\)\[\]])[\s~]*\\pageref\{([^\}]+)\}/$1\\fullpageref[$2]{$3}/g;
    # Now delete the extra space
    s/^ //;
    $prevline=$_;
    if (/\\end\{document\}/) {
	print;
	last;
    }
}

# And processing whatever is left
while (<>) {
    print;
}

exit 0;
