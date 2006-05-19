#!perl
#: re2dfa.pl
#: regex -> DFA converter
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-17 2006-05-18

use strict;
use warnings;

use Getopt::Std;
use re::DFA;
use re::DFA::Min;

my %opts;
getopts('h', \%opts);

if ($opts{h}) {
    Usage(0);
}

if (! @ARGV) {
    warn "No regex specified.\n\n";
    Usage(1);
}

sub Usage {
    my $code = shift;
    my $msg = "Usage: re2dfa [-h] <regex>\n";
    if ($code == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($code);
    }
}

my $regex = shift;

my $dfa = re::DFA->transform($regex);
if ($dfa) {
    my $outfile = 'DFA.png';
    $dfa->normalize->as_png($outfile);
    print "$outfile generated.\n";
} else {
    exit(1);
}

my $min_dfa = re::DFA::Min->transform($regex);
if ($min_dfa) {
    my $outfile = 'DFA.min.png';
    $min_dfa->normalize->as_png($outfile);
    print "$outfile generated.\n";
} else {
    exit(1);
}
