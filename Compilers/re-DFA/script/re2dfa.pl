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
getopts('s:hr', \%opts);

if ($opts{h}) {
    Usage(0);
}

if (! @ARGV) {
    warn "No regex specified.\n\n";
    Usage(1);
}

my $raw = $opts{r};
my ($width, $height) = split 'x', $opts{s} if $opts{s};

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

if (!$raw) { $dfa = $dfa->normalize; }
if ($dfa) {
    my $outfile = 'DFA.png';
    my $gv = $dfa->as_graphviz(
        width  => $width,
        height => $height,
    );
    $gv->as_png($outfile);
    print "$outfile generated.\n";
} else {
    exit(1);
}

my $min_dfa = re::DFA::Min->transform($regex);
if (!$raw) { $min_dfa = $min_dfa->normalize; }
if ($min_dfa) {
    my $outfile = 'DFA.min.png';
    my $gv = $min_dfa->as_graphviz(
        width  => $width,
        height => $height,
    );
    $gv->as_png($outfile);
    print "$outfile generated.\n";
} else {
    exit(1);
}
