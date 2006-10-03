#: balls.pl
#: Example A of Section 1.5, Page 17.
#: StatSim v0.04
#: Copyright 2005, 2006 Agent Zhang.
#: 2005-03-09 2006-10-13

use strict;
use warnings;

my $nexps = shift;
$nexps = 100_000 unless defined $nexps;

my @colors = qw(Red Red Red Blue);
my $freq = 0;
for (1..$nexps) {
    # First trial:
    my $ball = int(rand scalar(@colors));
    next unless $colors[$ball] eq 'Red';

    my @saved_colors = @colors;
    splice @colors, $ball, 1;

    #Second trial:
    $ball = int(rand scalar(@colors));
    ++$freq if $colors[$ball] eq 'Red';
    @colors = @saved_colors;
}

print "The probability that they are both red is\n  ",
    trim($freq/$nexps), " (0.5 expected)\n";

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example A

An urn contains three red balls and one blue ball. Two
balls are selected without replacement. What is the
probability that they are both red?

Let R1 and R2 denote the events that a red ball is drawn
on the first trial and on the second trial, respectively.
From the multiplication law,

    P(R1 กษ R2) = P(R1) P(R2 | R1)

P(R1) is clearly 3/4, and if a red ball has been removed
on the first trial, there are two red balls and one blue
ball left. Therefore, P(R2 | R1) = 2/3. Thus, P(R1 กษ R2)
= 1/2.
