#: balls2.pl
#: Example C of Section 1.5, Page 18.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-09 2005-03-30

use strict;
use warnings;

my $nexps = shift;
$nexps = 400_000 unless defined $nexps;

my @colors = qw(Red Red Red Blue);
my $freq = 0;

foreach (1..$nexps) {
    # First trial
    my $ball = int(rand scalar(@colors));

    my @saved_colors = @colors;
    splice @colors, $ball, 1;

    #Second trial:
    $ball = int(rand scalar(@colors));
    ++$freq if $colors[$ball] eq 'Red';
    @colors = @saved_colors;
}

print "The probability that a red ball is ",
    "selected on\n  the second draw is ",
    trim($freq/$nexps), " (0.75 expected)\n";

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example C

Referring to Example A, what is the probability that a
red ball is selected on the second draw?

The answer may or may not be intuitively obvious--that
depends on your intuition. On the one hand, you could
argue that it is "clear from symmetry" that P(R2) =
P(R1) = 3/4. On the other hand, you could say that it
is obvious that a red ball is likely to be selected on
the first draw, leaving fewer red balls for the second
draw, so that P(R2) < P(R1). The answer can be derived
easily by using the law of total probability:

    P(R2) = P(R2 | R1)P(R1) + P(R2 | B1)P(B1)
          = 2/3 x 3/4 + 1 x 1/4 = 3/4

where B1 denotes the event that a blue ball is drawn
on the first trial.
