#: AIDS.pl
#: Example B of Section 1.3, Page 6.
#: Example D of Section 1.6, Page 23.
#: Agent2002. All rights reserved.
#: 2005-03-09 2005-03-13

use strict;
use warnings;

my $nexps = shift;
$nexps = 20_000 unless defined $nexps;
my $nacts = shift;
$nacts = 500 unless defined $nacts;

my $prob = 1/500; # probability of transmisson in any one act
my $freq = 0; # frequency of transmission
foreach (1..$nexps) {
    foreach my $i (1..$nacts) {
        if (gen_event($prob)) {
            warn "Infected after $i act(s).\n";
            ++$freq;
            last;
        }
    }
}
warn "\n";
print "$nacts acts of intercourse lead to a ",
    trim($freq/$nexps), " (",
    trim(1-(1-$prob)**$nacts),
    " expected)\n  probability of infection.\n";

sub gen_event {
    my $prob = shift;
    return rand(1) < $prob;
}

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example B

An article in the Los Angeles Times (August 24, 1984) discussed
the statistical risks of AIDS infection:

    Several studies of sexual partners of people infected with
    the virus show that a single act of unprotected vaginal
    intercourse has a surprisingly low risk of infecting the
    uninfected partner--perhaps one in 100 to one in 1000. For
    an average, consider the risk to be one in 500. If there
    are 100 acts of intercourse with an infected partner, the
    odds of infection increase to one in five.
        Statistically, 500 acts of intercourse with one infected
    partner or 100 acts with five partners lead to a 100%
    probability of infection (statistically, not necessarily in
    reality).

Following this reasoning, 1000 acts of intercourse with one
infected partner would lead to a probability of infection equal
to 2 (statistically, not necessarily in reality). To see the
flaw in the reasoning that leads to this conclusion, consider
two acts of intercourse. Let A1 denote the event that infection
occurs one the first act and let A2 denote the event that
infection occurs on the second act. Then the event that infection
occurs is B = A1 กศ A2 and

    P(B) = P(A1) + P(A2) - P(A1 กษ A2) <= P(A1) + P(A2) = 2/500


Example D

We return to Example B of Section 1.3 (infectivity of AIDS).
Suppose that virus transmissions in 500 acts of intercourse
are mutually independent events and that the probability of
transmission in any one act is 1/500. Under this model, what
is the probability of infection? It is easier to first find
the probability of the complement of this event. Let C1, C2,
..., C500 denote the events that that virus transmission
does not occur during encounters 1, 2, ..., 500. Then the
probability of no infection is

    P(C1 กษ C2 กษ ... กษ C500) = (1 - 1/500)^500 = .37

so the probability of infection is 1 - .37 = .63, not 1,
the answer produced by incorrectly adding probabilities.
