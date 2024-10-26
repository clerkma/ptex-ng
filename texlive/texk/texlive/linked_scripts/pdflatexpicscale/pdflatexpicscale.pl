#!/usr/bin/perl -w
# Scale pictures to be included into a LaTeX generated PDF file.
# written by peter willadt 
# contact: willadt at t-online.de
# this software is subject to the LaTeX project public license.
# changes:
# 2016-07-27 first public release
# 2016-08-02 changed regex to prevent problem with long filenames
# 2016-08-02 changed > to gt (shame on me)
# 2016-10-20 corrected behaviour when program gets called without arguments
#            (shame on me, again)
# 2016-10-20 added undocumented switch --help to show usage.
# 2017-01-23 Updated documentation (see pdflatexpicscale.tex), 
#            and corrected version number 
#            Please note: version numbers will be updated to keep sync
#            with the documentation, even when the perl code does not change.
# 2017-02-01 pdflatexpicscale did not work correct when log lines were 
#            wrapped twice. This is fixed.
# 2024-10-18 Replaced dependency of external ImageMagick software in favor
#            of GD library.
#
use strict;

use GD;
use Image::ExifTool;

use File::Basename;
use File::Spec;
use File::Copy;

use Getopt::Long;

my $version = '0.50';
my $versiondate = '2024-10-18';           #version %version
my $showversion;

# command line inouts
my $verbose = 0;
my $help = 0;
my $draftoption = 0;
my $finaloption = 0;
my $TeXproject;

# folders for scaled graphics, relative names
my $outputfolderprefix = undef;
my $srcfolder          = '';

my %customsettings = (
    'name' => 'from command line',
    'outputdpi' => 300,
    'outputfolder' => 'printimg',
    'jpegquality' => 80,
    'outputbwdpi' => 1200
    );

my %finalsettings = (
    'name' => 'final',
    'outputdpi' => 300,
    'jpegquality' => 80,
    'outputfolder' => 'finalimg',
    'outputbwdpi' => 1200
    );

my %draftsettings = (
    'name' => 'draft',
    'outputdpi' => 96,
    'jpegquality' => 50,
    'outputfolder' => 'webimg',
    'outputbwdpi' => 300
    );

my $outputdpi   = undef;     # final resolution. 
my $outputbwdpi = undef;      # final resolution for pure b/w art
my $tolerance  = 20;         # avoid resampling of almost fitting images
my $jpegquality = undef;     # 50 is mostly acceptable, 80 is fine.

# supported file formats (by extension)
my %canhandle =
    (
    'JPEG' => '1',
     'JPG' => '1',
     'PNG' => '1',
    );

# bookkeeping
my %processedimages;
my $copied  = 0;
my $scaled  = 0;
my $skipped = 0;

# the following is required by Image::ExifTool
our $AUTOLOAD;
sub AUTOLOAD
{
    Image::ExifTool::DoAutoLoad($AUTOLOAD, @_);
}

sub printError
{
    my $filename = shift;
    my $kind = shift;
    if ($kind eq 'i') {
	print "I will not do inplace modification of graphics:\n" .
	    "($filename) will not be processed. (Missing path?)\n";
    } elsif ($kind eq 'm') {
	print "I can't find ($filename). (What have you done?)\n";
    } elsif ($kind eq 'f') {
	print "File $filename has an unexpected or wrong format.\n";
    } elsif ($kind eq 's') {
	print "Trying to copy '$filename' leads to system error # $_.\n";
    } elsif ($kind eq 'd') {
	print "Directory '$filename' does not exist\n"
	    . "in actual folder and I can't create it (sys error # $_).\n"
	    . "Please check permissions. Aborting.\n";
    } else {
	print "I got trouble with '$filename'.\n";
    }
}

sub getDestFilename
{
    my @dstdirs;
    my $filename = shift;
    my $dstfilename = undef;;
    $dstdirs[0] = '.';
    $dstdirs[1] = $outputfolderprefix;
    $dstfilename = File::Spec->catfile(@dstdirs, basename($filename));
    return $dstfilename;
}

sub getSourceFilename
{
    my @srcdirs;
    my $filename = shift;
    my $srcfilename = undef;
    if (defined($srcfolder) and $srcfolder gt '') {
	$srcdirs[0] = '$srcfolder';
	$srcfilename = File::Spec->catfile(@srcdirs, basename($filename));
	$filename = $srcfilename;
    }
    unless (-e $filename){
	printError($filename, 'm');
	return undef;
    }
    return $filename;
}

sub fileYounger
{
    my @filename = @_;
    my ($mtime1, $mtime2, $size1, $size2, $idstring2, $existwidth);
    my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size,
	$atime, $ctime, $blksize, $blocks);
    ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size,
     $atime, $mtime1, $ctime, $blksize, $blocks)
	= stat($filename[0]);
    ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size,
     $atime, $mtime2, $ctime, $blksize, $blocks)
	= stat($filename[1]);
    if($mtime1 >= $mtime2) {
	return 1;
    }
    return 0;
    
}

sub numberInRange
{
    my $realwidth = shift;
    my $wantedwidth = shift;
    if (($realwidth >= $wantedwidth * 100 / (100 + $tolerance)) and
	($realwidth <= $wantedwidth * (100 + $tolerance) / 100)) {
	return 1;
    } else {
	return 0;
    }
}

sub loadImage
{
    my %result = (
	'filename' => undef,
	'image' => undef, 'canhandle' => 0,
	'format' => undef, 'width' => 0,
	'height' => 0, 'monochrome' => 0,
	'quality' => 50
    );
    $result{'filename'} = shift;
    my $maketruecolor = shift || 0;
    my $exifTool = Image::ExifTool->new;
    my $imageinfo = $exifTool->ImageInfo($result{'filename'});
#    foreach (sort keys %$imageinfo) {
#	print "$_ => $$imageinfo{$_}\n";
#    }
    $result{'image'} = GD::Image->new($result{'filename'});
    $result{'width'} = $$imageinfo{'ImageWidth'};
    $result{'height'} = $$imageinfo{'ImageHeight'};
    if ( ($$imageinfo{'FileType'} eq 'PNG') or
	 ($$imageinfo{'FileType'} eq 'JPEG')) {
	$result{'format'} = $$imageinfo{'FileType'};
	$result{'canhandle'} = 1;
    }
    if (defined($$imageinfo{'BitDepth'}) and $$imageinfo{'BitDepth'} == 1) {
	$result{'monochrome'} = 1;
    } elsif (defined($result{'image'})) {
	$result{'canhandle'} = 1;
	if (defined ($result{'image'}->colorsTotal) and
	    $result{'image'}->colorsTotal < 3) {
	    $result{'monochrome'} = 1;
	}
	unless (defined($result{'width'})) {
	    $result{'width'} = $result{'image'}->width;
	}
	unless (defined($result{'height'})) {
	    $result{'height'} = $result{'image'}->height;
	}
    }
    return %result;
}

sub writeImage
{
    my %dst = @_;
    my $imagedata = undef;
    my $realfilename = $dst{'filename'};
    open(OUT, '>', $realfilename) or die;
    binmode OUT;
    if ($dst{'format'} eq 'PNG') {
	$dst{'image'}->interlaced(undef); # else PFDTEX has to do it.
	$imagedata = $dst{'image'}->png(9);
    } elsif ($dst{'format'} eq 'JPEG') {
	$imagedata = $dst{'image'}->jpeg($jpegquality);
    }
    print OUT $imagedata;
    close(OUT);
    return 1;
}

sub GDscale
{
    my $srcfilename = shift;
    my $dstfilename = shift;
    my $dstwidth = shift;
    my $dstheight = shift;
    # my $interpolationmethod = GD_BICUBIC;
    # Unfortunately not exported by GD and item of an enum.
    # So I guess:
    my $interpolationmethod = 4;
    my %src = loadImage($srcfilename, 0);
    $src{'destwidth'} = $dstwidth;
    $src{'destheight'} = $dstheight;
    $src{'image'}->interpolationMethod($interpolationmethod);
    # what I will not copy: image, width, height, filename 
    my @properties = qw ( 
	canhandle format monochrome
	);
    my %dstimage = ();
    foreach (@properties) {
	$dstimage{$_} = $src{$_};
    }
    $dstimage{'width'} = $src{'destwidth'};
    $dstimage{'height'} = $src{'destheight'};
    $dstimage{'filename'} = $dstfilename;
    $dstimage{'image'} = $src{'image'}->copyScaleInterpolated(
	$dstimage{'width'}, $dstimage{'height'});
    writeImage(%dstimage);
    return 1;
}

sub handleImage
{
    my $filename  = shift;
    my $dstwidth  = shift;
    my $dstheight = shift;
    my ($srcwidth, $srcheight, $srcdepth, $monochrome, $existwidth);
    my ($dstfilename, $xfilename);
    my @convertargs;
    my %srcimage = ();
    my %dstimage = ();
    print "$filename, " if $verbose;
    $dstfilename = getDestFilename($filename);
    $xfilename = getSourceFilename($filename);
    if (defined($xfilename)) {
	$filename = $xfilename;
    } else {
	printError($filename, 'm');
	$skipped++;
	return 0;
    }
    if($filename eq $dstfilename) {
	printError($filename, 'i');
	$skipped++;
	return 0;
    }
    # check for image width and height
    %srcimage = loadImage($filename);
    if ($srcimage{'canhandle'} == 0 or ! defined($srcimage{'image'})) {
	# copy from source to dest.
	print "I Can't deal with $filename, so I will just\n"
	    . "copy it to $dstfilename\n" if $verbose;
	copy($filename, $dstfilename);
	$copied++;
	return 1;
    }
    $srcwidth = $srcimage{'width'};
    $srcheight = $srcimage{'height'};
    $monochrome = $srcimage{'monochrome'};
    if($monochrome == 1) { # hires for bw pix
	$dstwidth  = int($dstwidth  * $outputbwdpi / 72.27 + .5);
	$dstheight = int($dstheight * $outputbwdpi / 72.27 + .5);
    }else {              # moderate res for any other
	$dstwidth  = int($dstwidth  * $outputdpi / 72.27 + .5);
	$dstheight = int($dstheight * $outputdpi / 72.27 + .5);
    }
    if(-e $dstfilename and fileYounger($dstfilename, $filename)) {
	# if requested size already processed: skip
	if((defined $processedimages{$filename}) && 
	   ($processedimages{$filename} >= $dstwidth)) {
	    # if known, already processed and large enough: skip
	    $skipped++;
	    return 1;
	}
	%dstimage = loadImage($dstfilename);
	$existwidth = $dstimage{'width'};
	# if requested size already there: make known and skip
	if(numberInRange($existwidth, $dstwidth)) {
	    $processedimages{$filename} = $existwidth;
	    $skipped++;
	    return 1;
	}
    }
    if (numberInRange($dstwidth, $srcwidth) or $dstwidth > $srcwidth) {
	# we do no upscaling, but will probably copy the file.
	$processedimages{$filename} = $srcwidth;
	print "will copy $filename to $dstfilename\n" if $verbose;
	copy($filename, $dstfilename);
	$copied++;
	return 1;
    }else {
	GDscale($xfilename, $dstfilename, $dstwidth, $dstheight);
	$scaled++;
    }
    return 1;
}

sub createOutputFolder
{
    my @dstdirs;
    $dstdirs[0] = '.';
    my $dstdirname = File::Spec->catfile(@dstdirs, $outputfolderprefix);
    if (mkdir($dstdirname) == 0) {
	printError($dstdirname, 'd');
	return 0;
    }
    if ($verbose) {
	print "Created directory $outputfolderprefix\n";
    }
    return 1;
}

sub readlog
{
    my $logfilename = shift;
    $logfilename .= '.log';
    my ($picname, $picext, $picwidth, $picheight);
    my ($picextu, $state, $buffer);
    open LOGFILE, "<$logfilename" or die "Cannot read $logfilename.\n"
	. 'Please run pdfLaTeX before pdflatexpicscale.';
    $state = 0;
    unless (-d $outputfolderprefix) {
	if (createOutputFolder() == 0) {
	    close LOGFILE;
	    return;
	}
    }
    print "Processing " if $verbose;
    while (<LOGFILE>){
	if (/^Package\spdftex\.def\sInfo\:\s/){
	    $buffer = $_;
	    unless ($buffer =~ /\sused/){
		chomp $buffer;
		$buffer .= <LOGFILE>;
	    } # twice ought to be enough
	    unless ($buffer =~ /\sused/){
		chomp $buffer;
		$buffer .= <LOGFILE>;
	    }
	    if($buffer =~ /Info:\s(\S*)\.(\w+)\s+used/){
		$picname = $1;
		$picext = $2;
		($picextu = $picext) =~ tr/a-z/A-Z/;
		if(defined $canhandle{$picextu}){
		    $state = 1;
		    next;  # width/height information will follow
		}
	    } else {
		print  "Log file apparently damaged at line $.\n";
	    }
	}
	next unless ($state == 1);
	next unless /Requested size: (\d+\.\d+)pt x (\d+\.\d+)pt/;
	$picwidth  = $1;
	$picheight = $2;
	handleImage("$picname.$picext", $picwidth, $picheight);
	$state = 0;
    }
    close LOGFILE;
}

sub initializeGlobals
{
    my $settinghash = undef;
    if($draftoption == 1) {
	$settinghash = \%draftsettings;
    } elsif ($finaloption == 1) {
	$settinghash = \%finalsettings;
    } else {
	$settinghash = \%customsettings;
    }
    if ($verbose != 0) {
	print 'Using settings: ' . $$settinghash{'name'} . "\n";
    }
    unless (defined ($outputdpi)) {
	$outputdpi = $$settinghash{'outputdpi'} || 300;
    }
    unless (defined ($outputbwdpi)) {
	$outputbwdpi = 4 * $outputdpi;
    }
    unless (defined ($outputfolderprefix)) {
	$outputfolderprefix = $$settinghash{'outputfolder'} || 'printimg';
    }
    unless (defined ($jpegquality)) {
	$jpegquality = $$settinghash{'jpegquality'} || 80;
    }
}

sub usage
{
    print <<'_endusagehead';
pdflatexpicscale will downscale large raster images for your TeX project
to reasonable size. 
Basic usage:
   pdflatexpicscale myarticle
where your main LaTeX file is called 'myarticle.tex' and a log file exists.
General options:
--help           (show this message)
--usage          (show this message)
--verbose        (let the script tell, what it attempts to do)
--version        (show software version)
If you are comfortable with defaults (see manual), you may use one of the
following options:
_endusagehead
    my %optiondumper = (
	'name' => 'Option name',
	'outputfolder' => 'Output folder',
	'outputdpi' => 'Output density',
	'outputbwdpi' => 'same for B/W',
	'jpegquality' => 'JPEG quality'
);
    print <<'_endusagefoot';
Option name     --final          --draft
Output folder   finalimg         webimg
JPEG quality    80%              50%
output density  300 dpi          96 dpi
same for B/W    1200 dpi         300 dpi
tolerance       20%              20%
If not, you may do some fine tuning:
--destdir=folder (relative path, must exist, 
                  defaults to 'printimg' subfolder of current folder)
--srcdir=folder  (folder to find original images, if this software 
                  can't figure it out on it's own, because you already
                  named the target directory inside your LaTex document.)
--printdpi=XXX   (default 300)
--tolerance=XX   (default 20 percent over printdpi to ignore scaling)
_endusagefoot
}

GetOptions('verbose'       => \$verbose,
	   'printdpi=i'    => \$outputdpi,
	   'destdir=s'     => \$outputfolderprefix,
	   'tolerance=i'   => \$tolerance,
           'srcdir=s'      => \$srcfolder,
	   'draft'         => \$draftoption,
	   'final'         => \$finaloption,
           'version'       => \$showversion,
	   'help'          => \$help,
	   'usage'         => \$help,
    );


if($showversion){
    print "pdflatexpicscale Version $version $versiondate\n";
}

if($help){
    usage();
    exit(0);
}

my $settinghash = \%customsettings;

initializeGlobals();

$TeXproject = shift;

if((defined $TeXproject) && ($TeXproject gt ' ')){
    readlog($TeXproject);
    if($verbose) {
	print "\npdflatexpicscale Version $version:\n"
	    . "$copied file(s) copied, $scaled file(s) converted " .
	    "and $skipped occurence(s) skipped for:\n${TeXproject}.log.\n";
    }
}else {    
    usage();
    exit(1);
}
