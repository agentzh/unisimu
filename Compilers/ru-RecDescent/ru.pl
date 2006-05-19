# ru.pl

use strict;
use warnings;

use Getopt::Std;
use ru::Parser;

my %opts;
getopts('t', \%opts);

my $infile = shift || die "error: No grammar file specified.\n";
open my $in, $infile or
    die "error: Can't open $infile for reading: $!\n";
my $src;
{
    local $/;
    $src = <$in>;
}
close $in;

$::RD_TRACE = $opts{t};
my $parser = ru::Parser->new;
my $ptree = $parser->grammar($src);
if (! defined $ptree) {
    die "Can't construct a parse tree from the source\n";
}
