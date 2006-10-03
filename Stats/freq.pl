#: freq.pl
#: Count occurrences of substring.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-3-31 2005-3-31

use strict;
use warnings;
use Getopt::Std;

my $VERSION = '0.03';

my %opts;
getopts('h', \%opts);

Usage() if $opts{h} or !@ARGV;

local $/;
my $txt = <STDIN>;
foreach my $pat (@ARGV) {
    my $count = 0;
    while ($txt =~ /$pat/gs) {
        $count++;
    }
    print "$pat: $count\n";
}

sub Usage {
    print <<"_EOC_";
Freq Version $VERSION
Count occurrences of substring.
Copyright 2005 Agent Zhang. All rights reserved.

Usage:
    your-tool | freq <string1> <string2> <string3> ...
  or
    freq <string1> <string2> <string3> ... < your-file

Report bugs to Agent Zhang <agnet2002\@126.com>.
_EOC_
    exit;
}
