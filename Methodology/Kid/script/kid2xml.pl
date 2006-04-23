#: kid2xml.pl
#: Convert .kid source to .xml listing
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-23 2006-04-23

use strict;
use warnings;

use FindBin;
#use lib "$FindBin::Bin/../lib";
use Getopt::Std;
use Kid::XML;

my %opts;
getopts('o:', \%opts);
my $infile = shift;
die "Usage: kid2xml [-o <outfile>] <infile>\n" if !$infile;
my $outfile;
if (($outfile = $infile) !~ s/\.kid$/.xml/i) {
    $outfile .= '.xml';
}

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";
my $src;
{
    local $/;
    $src = <$in>;
}
close $in;

my $perl = Kid::XML::translate($src);
defined $perl or
    die "Can't generate XML code due to compilation errors.\n";

open my $out, "> $outfile" or
    die "error: Can't open $outfile for writing: $!\n";
print $out $perl;
close $out;
print "$outfile generated.\n";
