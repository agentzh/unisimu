#!perl
#: re2dfa.pl
#: regex -> DFA converter
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-17 2006-05-17

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

my $outfile = 'DFA.png';
if (re::DFA::translate($regex, $outfile)) {
    print "$outfile generated.\n";
} else {
    exit(1);
}

$outfile = 'DFA.min.png';
if (re::DFA::Min::translate($regex, $outfile)) {
    print "$outfile generated.\n";
} else {
    exit(1);
}
