#: fplot.pl
#: Flowchart plotter
#: Coyright (c) 2006 Agent Zhang
#: 2006-03-06 2006-03-08

use strict;
use warnings;
use FAST;
use Getopt::Std;

my %opts;
getopts('ho:', \%opts);

if ($opts{h}) { Usage(0); }

my $infile = shift;
if (not $infile) { Usage(1); }

my $g = FAST->new($infile);
my $outfile = $opts{o} || "$infile.png";
$g->as_png($outfile);

sub Usage {
    my $retval = shift;
    my $msg = "Usage: fplot [-o <outfile>] <infile> ...\n";
    if ($retval == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($retval);
    }
}
