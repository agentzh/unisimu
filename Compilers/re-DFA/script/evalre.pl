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

if (! @ARGV) {
    Usage(1);
}

sub Usage {
    my $code = shift;
    my $msg = "Usage: re2c [-p] [-c] <regex> <text>\n";
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

if ($opts{c}) {
    *match = re::DFA::C->as_method($regex);
    my $match = match($text);
    if (defined $match) {
        print $match;
        exit(0);
    } else {
        exit(1);
    }
}

*match = re::DFA::Perl->as_method($regex);
my $match = match($text);
if (defined $match) {
    print $match;
} else {
    exit(1);
}
