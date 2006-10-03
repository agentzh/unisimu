#: artery2.pl
#: Example E of Section 1.5, Page 20.
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-04-01 2005-04-02

use strict;
use warnings;

my $nexps = shift;
$nexps = 500_000 unless defined $nexps;

my %P = (
    'T0|D+' => .42, 'To|D-' => .96,
    'T1|D+' => .24, 'T1|D-' => .02,
    'T2|D+' => .20, 'T2|D-' => .02,
    'T3|D+' => .15, 'T3|D-' => .00,
);

my ($cond_nexps0, $cond_nexps1) = (0, 0);
my ($cond_count0, $cond_count1) = (0, 0);
foreach (1..$nexps) {
    my $D = gen_event_X('D+' => .92, 'D-');
    my $T = gen_event_X(
        T0 => $P{"T0|$D"},
        T1 => $P{"T1|$D"},
    );
    next unless $T;
    if ($T eq 'T0') {
        $cond_nexps0++;
        $cond_count0++ if $D eq 'D+';
    } else { # $T eq 'T1'
        $cond_nexps1++;
        $cond_count1++ if $D eq 'D+';
    }
}

my $freq0 = trim($cond_count0/$cond_nexps0);
my $freq1 = trim($cond_count1/$cond_nexps1);
print "P(D+|T0) = $freq0 (0.83 expected)\n";
print "P(D+|T1) = $freq1 (0.99 expected)\n";

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

Diamond and Forrester (1979) applied Bayes' rule to the
diagnosis of coronary artery disease. A procedure called
cardiac fluoroscopy is used to determine whether there
is calcification of coronary arteries and thereby to
diagnose coronary artery disease. From the test, it can
be determined if 0, 1, 2, or 3 coronary arteries are
calcified. Let T0, T1, T2, T3 denote these events. Let
D+ or D- denote the event that disease is present or
absent, respectively. Diamond and Forrester presented the
following table, based on medical studies:

            i     P(Ti|D+)     P(Ti|D-)
         ----------------------------------
            0       .42           .96
            1       .24           .02
            2       .20           .02
            3       .15           .00

According to Bayes' rule,

    P(D+|Ti) = P(Ti|D+)P(D+) / P(Ti|D+)P(D+) + P(Ti|D-)P(D-)

Thus, if the initial probabilities P(D+) and P(D-) are known,
the probability that a patient has coronary artery disease can
be calculated.

Let us consider two specific cases.

...

As a second case, suppose that the patient is a male between ages
50 and 59 which suffers typical angina. For such a patient,
P(D+) = .92. For him, we find that

    P(D+|T0) = .42 x .92 / (.42 x .92 + .96 x .08) = .83
    P(D+|T1) = .24 x .92 / (.24 x .92 + .02 x .08) = .99

Comparing the two patients, we see the strong influence of the
prior probability, P(D+).
