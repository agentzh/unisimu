#: birthday.pl
#: Example E of Section 1.4, page 10-11
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang
#: 2005-3-31 2005-3-31

use strict;
use warnings;

my $n = shift;
$n = 40 unless defined $n;
my $nexps = shift;
$nexps = 300_000 unless defined $nexps;

my $count = 0;
foreach (1..$nexps) {
    my @days;
    for (my $i = 0; $i < $n; $i++) {
        $days[$i] = int(rand 365);
    }
    $count++ unless distinct(@days);
}

my $prob = trim($count/$nexps);
print "The probability that at least two of $n persons\n",
      "have a common birthday is $prob (", trim(theo_prob($n)),
      " expected).\n";

sub theo_prob {
    my $n = shift;
    return 1 - A(365, $n)/(365**$n);
}

sub A {
    my ($n, $m) = @_;
    my $product = 1;
    foreach my $factor ($n-$m+1..$n) {
        $product *= $factor;
    }
    return $product;
}

sub distinct {
    my @list = sort @_;
    my $prev;
    foreach my $elem (@list) {
        return undef if $prev and $elem eq $prev;
        $prev = $elem;
    }
    return 1;
}

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example E

I<Birthday Problem>

Suppose that a room contains n people. What is the probability
that at least tow of them have a common birthday?

This is a famous problem with a counterintuitive answer. Assume
that every day of the year is equally likely to be a birthday,
disregard leap years, and denote by A the event that there are
at least two people with a common birthday. As is sometimes the
case, it is easier to fined P(A^C) than to find P(A). This is
because A can happen in many ways, whereas A^C is much simpler.
There are 365^n possible outcomes, and A^C can happen in 365 x
364 x ... x (365 - n + 1) ways. Thus,

    P(A^C) = 365 x 364 x ... x (365 - n + 1) / 365^n

and

    P(A) = 1 - 365 x 364 x ... x (365 - n + 1) / 365^n

The following table exhibits the latter probabilities for various
values of n:

        n     P(A)
    ------------------
        4     .016
       16     .284
       23     .507
       32     .753
       40     .891
       56     .988

From the table, we see that if there are only 23 people, the
probability of at least one match exceeds .5.
