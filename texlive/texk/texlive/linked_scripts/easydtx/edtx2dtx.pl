#!/usr/bin/env perl

# This file is a part of TeX package EasyDTX, available at
# https://ctan.org/pkg/easydtx and https://github.com/sasozivanovic/easydtx.
#
# Copyright (c) 2023- Saso Zivanovic <saso.zivanovic@guest.arnes.si>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# The files belonging to this program and covered by GPL are listed in
# <texmf>/doc/support/easydtx/FILES.

use strict;
use Getopt::Long;

my $usage = <<END;
EasyDTX: convert .edtx into .dtx
Usage: edtx2dtx [options] filename.edtx > filename.dtx
Options: TODO
END

my $VERSION = '0.2.0';
my ($help,  $print_version);

my $comment = '%';
my $begin_macrocode = '^%    \\\\begin{macrocode}';
my $end_macrocode = '%    \\\\end{macrocode}';
my $Begin_macrocode = '';
my $End_macrocode = '';
my $strip = '';

Getopt::Long::Configure (qw/bundling no_ignore_case/);
GetOptions(
    "comment|c=s"  => \$comment,
    "begin-macrocode|b=s"  => \$begin_macrocode,
    "end-macrocode|e=s"  => \$end_macrocode,
    "Begin-macrocode|B=s"  => \$Begin_macrocode,
    "End-macrocode|E=s"  => \$End_macrocode,
    "strip-empty|s"  => \$strip,
    "help|h|?"  => \$help,
    "version|V"  => \$print_version,
    ) or die $usage;
if ($help) {print($usage); exit 0}
if ($print_version) { print("edtx2dtx $VERSION\n"); exit 0 }
die $usage unless @ARGV == 1;

my $keep_begin_macrocode = '';
my $keep_end_macrocode = '';
if ($Begin_macrocode) {
    $begin_macrocode = $Begin_macrocode;
    $keep_begin_macrocode = 1;
}
if ($End_macrocode) {
    $end_macrocode = $End_macrocode;
    $keep_end_macrocode = 1;
}

sub begin_macrocode { print("%    \\begin{macrocode}\n"); }
sub end_macrocode   { print("%    \\end{macrocode}\n");   }

# doc: replace initial in-comments with out-comments
sub convert_comments { s/^\Q$comment\E+// && print('%' x length($&)); }

my $indoc;
sub process_edtx {
    my $done;
    my $empty;
    while (<>) {
	if (/$end_macrocode/) { # trailer starts here
	    if ($keep_end_macrocode) {
		$done = 1;
	    } else {
		end_macrocode unless $indoc;
		last;
	    }
	} elsif (/$begin_macrocode/) {
	    die "Nested '$begin_macrocode'";
	}
	if (/^ *(\Q$comment$comment\E+)/) { # code: multiple comments
	    begin_macrocode if $indoc;
	    print($empty); $empty = '';
	    print;
	    $indoc = 0;
	} elsif (/^\Q$comment\E(<[^>]*>)(.*)$/) { # code: unindented guard
	    begin_macrocode if $indoc;
	    print;
	    $indoc = 0;
	} elsif (/^( *)\Q$comment\E(<[^>]*>) *(.*)$/) { # code: indented guard
	    begin_macrocode if $indoc;
	    print("%$2$1$3\n");
	    $indoc = 0;
	} elsif (/^\Q$comment\E *(.*)$/) { # doc: unindented comment
	    end_macrocode unless $indoc;
	    convert_comments;
	    print;
	    $indoc = 1;
	} elsif (/^( ?)( *)\Q$comment\E *(.*)$/) { # doc: indented comment
	    end_macrocode unless $indoc;
	    print "%$2$3\n";
	    $indoc = 1;
	} elsif (/^\s*$/) { # code: empty line
	    if ($strip) {
		$empty .= $_ unless $indoc;
	    } else {
		print;
	    }
	} else { # code
	    print($empty); $empty = '';
	    begin_macrocode if $indoc;
	    print;
	    $indoc = 0;
	}
	if ($done) {
	    end_macrocode unless $indoc;
	    last;
	}
    }
}

my $edtx = @ARGV[0];
my $dtx = $edtx;
$dtx =~ s/\.edtx$/.dtx/;

my $first = 1;
print("% \\iffalse\n%\n");
while (<>) {
    if ($first and s/$edtx +(.*)/$dtx (generated from $edtx by edtx2dtx)/) {
	print;
	$first = 0;
    } elsif (/$begin_macrocode/) {
	print("% \\fi\n%\n");
	if ($keep_begin_macrocode) {
	    $indoc = 1 if /^\Q$comment\E/;
	    begin_macrocode unless $indoc;
	    print;
	} else {
	    $indoc = 1;
	}
	process_edtx;
    } elsif (/$end_macrocode/) {
	die "'$end_macrocode' without the opening '$begin_macrocode'!";
    } else {
	convert_comments;
	print;
    }
}
