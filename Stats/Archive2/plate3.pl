#: plate3.pl
#: Example D of Section 1.4, Page 10.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang
#: 2005-3-31 2005-3-31

use strict;
use warnings;
use Math::Random;

my $nexps = shift;
$nexps = 300_000 unless defined $nexps;

my $count = 0;
foreach (1..$nexps) {
    my @seq = rand_seq('A'..'Z', 3);
    push @seq, rand_seq(0..9, 3);
    #print @seq, "\n";
    if (distinct(@seq)) {
        #warn @seq, "\n";
        ++$count;
    }
}

my $freq = trim($count/$nexps);
print "The probability that the license plate for a\n",
      "new car will contain no duplicate letters or\n",
      "numbers is $freq (0.64 expected).\n";

sub rand_seq {
    my $m = pop;
    my @elems = @_;
    my $len = @elems;
    my @seq;
    #foreach (1..$m) {
    #    my $index = int(rand $len);
    #    push @seq, $elems[$index];
    #}
    @seq = random_uniform_integer($m, 0, $len-1);
    map { $_ = $elems[$_]; } @seq;
    return @seq;
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

Example D

If all sequence of six characters (three letters followed by three
numbers) are equally likely, what is the probability that the license
plate for a new car will contain no duplicate letters or numbers?

Call the desired event A; Omega consists of all 17,576,000 possible
sequences. Since these are all equally likely, the probability of A
is the ratio of the number of ways that A can occur to the total number
of possible outcomes. There are 26 choices for the first letter, 25
for the second, 24 for the third, and hence 26 x 25 x 24 = 15,600 ways
to choose the letters without duplication (doing so corresponds to
sampling without replacement) and 10 x 9 x 8 = 720 ways to choose
the numbers without duplication. From the multiplication principle,
there are 15,600 x 720 = 11,232,000 nonrepeating sequences. The
probability of A is thus

    P(A) = 11,232,000 / 17,576,000 = .64
