#: kid2pl.pl
#: Convert .kid source to .pl source
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-22 2006-04-22

use strict;
use warnings;

use FindBin;
use lib "$FindBin::Bin/../lib";
use Getopt::Std;
use Kid::Perl;

my %opts;
getopts('o:', \%opts);
my $infile = shift;
die "Usage: kid2pl [-o <outfile>] <infile>\n" if !$infile;
my $outfile;
if (($outfile = $infile) !~ s/\.kid$/.pl/i) {
    $outfile .= '.pl';
}

open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";
my $src;
{
    local $/;
    $src = <$in>;
}
close $in;

my $perl = Kid::Perl::translate($src);
defined $perl or
    die "Can't generate Perl code due to compilation errors.\n";

open my $out, "> $outfile" or
    die "error: Can't open $outfile for writing: $!\n";
print $out $perl;
close $out;
print "$outfile generated.\n";
