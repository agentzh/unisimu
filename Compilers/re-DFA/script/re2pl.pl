#: re2pl.pl
#: Compile regex to perl lexing code
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-19 2006-05-19

use strict;
use warnings;

use Getopt::Std;
use re::DFA::Perl;

my %opts;
getopts('hn:', \%opts);

if ($opts{h}) {
    Usage(0);
}

if (! @ARGV) {
    Usage(1);
}

sub Usage {
    my $code = shift;
    my $msg = "Usage: re2pl [-h] [-n <sub-name>] <regex>\n";
    if ($code == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($code);
    }
}

my $subname = $opts{n};
my $regex = shift;
my $code = re::DFA::Perl->as_code($regex, $subname);
if (defined $code) {
    print $code;
} else {
    exit(1);
}
