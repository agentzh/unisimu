#: plate.pl
#: Example C of Section 1.4, Page 10
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-3-30 2005-3-31

use strict;
use warnings;

my @Res;
sub genres {
    my ($rplaces, $buf) = @_;
    my @places = @$rplaces;
    unless (defined $buf) {
        $buf = '';
        @Res = ();
    }
    my $pos = length($buf);
    if ($pos == @places) {
        push @Res, $buf;
        return;
    }
    if ($places[$pos] eq 'Number') {
        foreach my $val (0..9) {
            genres($rplaces, $buf.$val);
        }
    } elsif ($places[$pos] eq 'Letter') {
        foreach my $val ('A'..'Z') {
            genres($rplaces, $buf.$val);
        }
    } else {
        die "$places[$pos] at $pos";
    }
}

genres [qw(Letter Letter Number Number)];
$" = "\n";
warn "@Res\n\n";
print scalar(@Res), " distinct such plates are possible.\n",
    "    (", 26**2 * 10**2, " expected.)\n";

__END__

Example C

In some states, license plates have six characters: three
letters followed by three numbers. How many distinct such
plates are possible?

This corresponds to sampling with replacement. There are
26^3 = 17,576 different ways to choose the letters and
10^3 = 1000 ways to choose the numbers. Using the
multiplication principle again, we find there are 17,576
x 1000 = 17,576,000 different plates.
