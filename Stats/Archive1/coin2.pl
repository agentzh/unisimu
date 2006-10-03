#: coin2.pl
#: Example A of Section 1.3, Page 6.
#: Use CPAN Module Math::Random
#: Require:
#:     Math::Random
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-08 2005-03-30

use strict;
use warnings;
use Math::Random;

my $nexps = shift;  # Number of experiments
$nexps = 100000 unless defined $nexps;

my $seed = shift;
if (defined $seed) {
    random_set_seed_from_phrase($seed);
}

my @Omega = qw(hh ht th tt);
my %freqs = (A => 0, B => 0, C => 0);
foreach (1..$nexps) {
    my $index = random_uniform_integer(1, 0, @Omega-1);
    my $outcome = $Omega[$index];
    if ($outcome =~ m/^h/) {
        ++$freqs{A};
    }
    if ($outcome =~ m/^.h/) {
        ++$freqs{B};
    }
    if ($outcome =~ m/^h|^.h/) {
        ++$freqs{C};
    }
}

my %probs;
foreach my $event (keys %freqs) {
    $probs{$event} = trim($freqs{$event} / $nexps);
}
print "P(A) = ", $probs{A}, " (0.5 expected)\n",
      "P(B) = ", $probs{B}, " (0.5 expected)\n",
      "P(C) = ", $probs{C}, " (0.75 expected) !=\n  ",
        $probs{A} + $probs{B}, " (1 expected) = P(A) + P(B)\n";

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example A

Suppose that a fair coin is thrown twice. Let A denote the
event of heads on the first toss and B, the event of heads
on the second toss. The sample space is

    Omega = {hh, ht, th, tt}

We assume that each elementary outcome in Omega is equally
likely and has probability 1/4. C = A กศ B is the event
that heads comes up on the first toss or on the second toss.
Clearly, P(C) <> P(A) + P(B) = 1. Rather, since A กษ B is the
event that heads comes up on the first toss and on the
second toss,

    P(C) = P(A) + P(B) - P(A กษ B) = .5 + .5 - .25 = .75
