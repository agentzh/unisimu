#: coin3.pl
#: Example A of Section 1.4, Page 7.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-09 2005-03-30

use strict;
use warnings;

my $NEXPS = shift;  # Number of experiments
$NEXPS = 100000 unless defined $NEXPS;

my $seed = shift;
$seed = time unless defined $seed;
srand $seed;

my @Omega = qw(hh ht th tt);
my $freq_A = 0;
foreach (1..$NEXPS) {
    my $index = int(rand scalar(@Omega));
    my $outcome = $Omega[$index];
    if ($outcome =~ m/h/) {
        ++$freq_A;
    }
}

print "P(A) = ", trim($freq_A / $NEXPS), " (0.75 expected)\n";

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example A

Suppose that a fair coin is thrown twice and the sequence
of heads and tails is recorded. The sample space is

    Omega = { hh, ht, th, tt }

As in Example A of the previous section, we assume that
each outcome in Omega has probability .25. Let A denote
the event that at least one head is thrown. Then A = 
{hh, ht, th}, and P(A) = .75.
