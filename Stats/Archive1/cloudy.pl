#: cloudy.pl
#: Example B of Section 1.5, Page 17.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-09 2005-03-30

use strict;
use warnings;

my $nexps = shift;
$nexps = 400_000 unless defined $nexps;

my $freq = 0;
foreach (1..$nexps) {
    my $cloudy = gen_event(0.2);
    next unless $cloudy;
    my $raining = gen_event(0.3);
    ++$freq if $raining;
}

print "The probablity that it is coudy and raining is\n  ",
    trim($freq/$nexps), " (0.06 expected).\n";

sub gen_event {
    my $prob = shift;
    return rand(1) < $prob;
}

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example B

Suppose that if it is cloudy (B), the probability that
it is raining (A) is .3, and that the probability that
it is cloudy is P(B) = .2. The probability that it is
cloudy and raining is

    P(A กษ B) = P(A | B) P(B) = .3 x .2 = .06
