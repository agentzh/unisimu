#: occupation.pl
#: Example D of Section 1.5, Page 18-19.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-31 2005-04-02

use strict;
use warnings;

my $nexps = shift;
$nexps = 100_000 unless defined $nexps;

my %P = (
    'U2|U1' => .45, 'M2|U1' => .48, 'L2|U1' => .07,
    'U2|M1' => .05, 'M2|M1' => .70, 'L2|M1' => .25,
    'U2|L1' => .01, 'M2|L1' => .50, 'L2|L1' => .49,
);

my $count = 0;
foreach (1..$nexps) {
    my $father = gen_event_X(
        U1 => 0.10, M1 => 0.40, L1 => 0.50,
    );
    my $son = gen_event($P{"U2|$father"});
    $count++ if $son;
}

my $freq = trim($count/$nexps);
print "The probability that a son in the next generation\n",
      "is in U is $freq (0.07 expected).\n";

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

sub gen_event {
    my $prob = shift;
    return rand(1) < $prob;
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

           |   U2    M2    L2
       -----------------------
        U1 |  .45   .48   .07
        M1 |  .05   .70   .25
        L1 |  .01   .50   .49

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
