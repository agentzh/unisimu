#!perl
#: re2xml.pl
#: regex -> XML converter
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-17 2006-05-17

use strict;
use warnings;

use Getopt::Std;
use re::XML;

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
    my $msg = "Usage: re2xml [-h] <regex>\n";
    if ($code == 0) {
        print $msg;
        exit(0);
    } else {
        warn $msg;
        exit($code);
    }
}

my $regex = shift;
my $res = re::XML->translate($regex);
if (defined $res) {
    print "$res";
} else {
    exit(1);
}
