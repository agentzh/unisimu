#: evalre.pl
#: evaluate regex using Perl or C
#: 2006-05-19 2006-05-19

use strict;
use warnings;

use re::DFA::Perl;
use re::DFA::C;
use Getopt::Std;

my %opts;
getopts('hpc', \%opts);

if ($opts{h}) {
    Usage(0);
}

if (@ARGV != 2) {
    Usage(1);
}

sub Usage {
    my $code = shift;
    my $msg = "Usage: evalre [-p] [-c] <regex> <text>\n";
    if ($code == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($code);
    }
}

if ($opts{p} and $opts{c}) {
    die "error: Only one of the C and Perl emitters should be selected.\n";
}

my ($regex, $text) = @ARGV;

my $match;
if ($opts{c}) {
    *match = re::DFA::C->as_method($regex);
    $match = match($text);
} else {
    *match = re::DFA::Perl->as_method($regex);
    $match = match($text);
}

if (defined $match) {
    $match =~ s/'/\\'/g;
    print "match: '$match'\n";
} else {
    print "fail to match";
    exit(1);
}
