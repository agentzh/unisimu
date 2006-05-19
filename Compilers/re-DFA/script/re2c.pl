#: re2c.pl
#: Compile regex to C lexing code
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-19 2006-05-19

use strict;
use warnings;

use Getopt::Std;
use re::DFA::C;

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
    my $msg = "Usage: re2c [-h] [-n <sub-name>] <regex>\n";
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
my $code = re::DFA::C->as_code($regex, $subname);
if (defined $code) {
    print $code;
} else {
    exit(1);
}
