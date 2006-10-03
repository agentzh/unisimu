#: birthday2.pl
#: Example F of Section 1.4, Page 11.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang
#: 2005-03-30 2005-04-02

use strict;
use warnings;

my $npersons = shift;
$npersons = 253 unless defined $npersons;
my $nexps = shift;
$nexps = 30_000 unless defined $nexps;

my $mybirth = rand_birthday();
my $count = 0;
foreach (1..$nexps) {
    foreach (1..$npersons) {
        my $yourbirth = rand_birthday();
        #warn "$yourbirth vs. $mybirth";
        if ($yourbirth eq $mybirth) {
            $count++;
            last;
        }
    }
}

my $prob = trim($count/$nexps);
print "The chance of finding someone who shares your\n",
      "birthday when you ask $npersons people is $prob.\n",
      "(0.5 expected.)\n";

sub rand_birthday {
    return int(rand 364) + 1;
}

sub trim {
    return sprintf("%.4f", $_[0]);
}

__END__

Example F

How many people must you ask in order to have a 50:50 chance of finding
someone who shares your birthday?

Suppose that you ask n people; let A denote the event that someone's
birthday is the same as yours. Again, it is easier to work with A^C.
The total number of outcomes is 365^n, and the total number of ways that
A^C can happen is 364^n. Thus,

    P(A^C) = 364^n / 365^n

and

    P(A) = 1 - 364^n / 365^n

In order for the latter probability to be .5, n should be 353, which may
seem counterintuitive.
