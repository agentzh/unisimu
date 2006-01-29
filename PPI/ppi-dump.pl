use strict;
use warnings;

use PPI;
use PPI::Dumper;

use Getopt::Std;

my %opts;
getopts('hf:', \%opts);
Usage(0) if $opts{h};

my $arg = $opts{f};
if (not $arg) {
    @ARGV or Usage(1);
    $arg = \$ARGV[0];
}
#warn $arg;

my $dom = PPI::Document->new( $arg );

# Create the dumper
my $dumper = PPI::Dumper->new( $dom );

# Dump the document
$dumper->print;

sub Usage {
    my $retval = shift;
    my $msg = "Usage: ppi-dump [-f <file>] [<code>]\n";
    if ($retval == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($retval);
    }
}
