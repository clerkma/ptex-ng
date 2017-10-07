#!/usr/bin/env perl
# $Id: afm2tfm-test.pl 17798 2010-04-11 00:54:05Z karl $
# Public domain.  Originally written 2010, Karl Berry.
# Check that afm2tfm functions at all.

BEGIN { chomp ($srcdir = $ENV{"srcdir"} || `dirname $0`); }
require "$srcdir/../tests/common-test.pl";

exit (&main ());

sub main
{
  my $outfile = "afmtest.tfm";
  my @args = ("afmtest.afm", $outfile);  # should be found via kpse
  my $ret = &test_run ("./afm2tfm", @args);

  my $bad = ! -f $outfile;  # tfm should have been created
  return $bad;
}
