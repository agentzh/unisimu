#: occupation2.pl
#: Example D of Section 1.5, Page 18-19.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-31 2005-04-02

use strict;
use warnings;

my $nexps = shift;
$nexps = 500_000 unless defined $nexps;

my %P = (
    'U2|U1' => .45, 'M2|U1' => .48, 'L2|U1' => .07,
    'U2|M1' => .05, 'M2|M1' => .70, 'L2|M1' => .25,
    'U2|L1' => .01, 'M2|L1' => .50, 'L2|L1' => .49,
);

# counter for conditional probability:
my $cond_count = 0;
# number of experiments for conditional probability:
my $cond_nexps = 0;
foreach (1..$nexps) {
    my $father = gen_event_X(
        U1 => 0.10, M1 => 0.40, L1 => 0.50,
    );
    my $son = gen_event_X(
        U2 => $P{"U2|$father"},
        M2 => $P{"M2|$father"},
        L2 => $P{"L2|$father"},
    );
    if ($son eq 'U2') {
        $cond_nexps++;
        $cond_count++ if $father eq 'U1';
    }
}

my $freq = trim($cond_count/$cond_nexps);
print "The probability that the son's father had occupational\n",
      "status U1 is $freq (0.6429 expected).\n";

sub gen_event_X {
    my $offset = 0;
    my $randval = rand(1);
    my $default;
    while (1) {
        my ($e, $p) = (shift, shift);
        last unless $e;
        unless (defined $p) {
            $default = $e;
            next;
        }
        if ($randval >= $offset and
                $randval < $offset + $p) {
            return $e;
        }
        $offset += $p;
    }
    return $default;
}

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example D

Suppose that occupations are grouped into upper (U), middle (M), and
lower (L) levels. U1 will denote the event that a father's occupation
is upper-level; U2 will denote the event that a son's occupation is
upper-level, etc. (the subscripts index generations). Glass and Hall
(1954) compiled the following statistics on occupational mobility in
England and Wales:

              U2    M2    L2
       -----------------------
        U1   .45   .48   .07
        M1   .05   .70   .25
        L1   .01   .50   .49

Such a table, which is called a matrix of transition probabilities,
is to be read in the following way: If a father is in U, the
probability that his son is in U is .45, the probability that his son
is in M is .48, etc. The table thus gives conditional probabilities;
for example, P(U2|U1) = .45. Examination of the table reveals that
there is more upward mobility from L into M than from M into U. Suppose
that of the father's generation, 10% are in U, 40% in M, and 50% in L.
What is the probability that a son in the next generation is in U?

Applying the law of total probability, we have

    P(U2) = P(U2|U1)P(U1) + P(U2|M1)P(M1) + P(U2|L1)P(L1)
          = .45 x .10 + .05 x .40 + .01 x .50 = .07

P(M2) and P(L2) can be worked out similarly.

Continuing with Example D, suppose we ask a different question: If a
son has occupational status U2, what is the probability that his
father had occupational status U1? Compared to the question asked in
Example D, this is an "inverse" problem; we are given an "effect" and
are asked to find the probability of a particular "cause". In situations
like this, Bayes' rule, which we state shortly, is useful. Before
stating the rule, we will see what it amounts to in this particular
case.

We wish to find P(U1|U2). By definition,

    P(U1|U2) = P(U1 กษ U2) / P(U2)
             = P(U2|U1) / (P(U2|U1)P(U1) + P(U2|M1)P(M1) + P(U2|L1)P(L1))

Here we used the multiplication law to re-express the numerator and
the law of total probability to restate the denominator. The value
of the numerator is P(U2|U1)P(U1) = .45 x .10 = .045, and we calculated
the denominator in Example D to be .07, so we find that P(U1|U2) = .64.
In other words, 64% of the sons who are in upper-level occupations have
fathers who were in upper-level occupations.
