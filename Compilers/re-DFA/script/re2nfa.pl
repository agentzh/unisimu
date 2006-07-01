#!perl
#: re2nfa.pl
#: regex -> NFA converter
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-17 2006-06-30

use strict;
use warnings;

use Getopt::Std;
use re::NFA;

my %opts;
getopts('s:h', \%opts);

if ($opts{h}) {
    Usage(0);
}

if (! @ARGV) {
    Usage(1);
}

my ($width, $height) = split 'x', $opts{s} if $opts{s};

sub Usage {
    my $code = shift;
    my $msg = "Usage: re2nfa [-h] [-s <w>x<h>] <regex>\n";
    if ($code == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($code);
    }
}

my $regex = shift;

my $nfa = re::NFA->transform($regex);
if ($nfa) {
    my $outfile = 'NFA.png';
    my $gv = $nfa->as_graphviz(
        width  => $width,
        height => $height,
    );
    $gv->as_png($outfile);
    print "$outfile generated.\n";
} else {
    exit(1);
}
