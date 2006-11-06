# Perform the vectorization process using CLIPS

use strict;
use warnings;

use Getopt::Std;
use CLIPS;

my %opts;
getopts('v', \%opts) or help();
$CLIPS::Verbose = $opts{v};

my $infile = shift or help();;
my $clips = CLIPS->new('vectorize.clp', $infile, 'defs2.clp');
$clips->watch('rules');
$clips->watch('facts');
$clips->reset;
$clips->focus('Vectorize');
$clips->rules if $opts{v};
$clips->facts;
$clips->get_current_module;
$clips->agenda;
$clips->run;
$clips->facts('Eval', \my $facts);
$clips->eof;
#warn "FACTS: ", $facts;
while ($facts =~ /\(vector-relation ([^\)]+)\)/g) {
    print "$1\n";
}

sub help {
    die "usage: $0 [-v] infile\n";
}
