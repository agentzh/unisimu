# Perform the vectorization process using CLIPS

use strict;
use warnings;

use Getopt::Std;
use CLIPS;

my %opts;
getopts('v', \%opts) or help();
$CLIPS::Verbose = $opts{v};

my $infile = shift or help();;
my $clips = CLIPS->new('defs.clp', 'preprocess.clp', 'vectorize.clp', $infile, 'defs2.clp');
$clips->watch('rules');
$clips->watch('facts');
$clips->reset;
$clips->focus('VECTORIZE');
$clips->rules if $opts{v};
$clips->facts;
$clips->get_current_module;
$clips->agenda;
$clips->run;
$clips->facts('EVAL', \my $facts);
$clips->eof;
#warn "FACTS: ", $facts;
while ($facts =~ /\(vector-relation ([^\)]+)\)/g) {
    print "$1\n";
}

sub help {
    die "usage: $0 [-v] infile\n";
}
