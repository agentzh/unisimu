#: randtest.pl
#: Test the sub gen_event_X
#: StatSim v0.04
#: Copyright 1984-2005 Agent Zhang.
#: 2005-03-31 2005-04-02

use strict;
use warnings;

my $nexps = shift;
$nexps = 300_000 unless defined $nexps;

my %count = (U => 0,  M => 0,   L => 0);
my %probs = (U => .1, M => .38, L => .52);

foreach (1..$nexps) {
    my $event = gen_event_X(%probs);
    $count{$event}++;
}

while (my ($e, $c) = each %count) {
    my $freq = trim($c/$nexps);
    my $prob = $probs{$e};
    print "Event $e: $freq ($prob expected)\n";
}

print '----' x 10, "\n";

%count = (U => 0,  M => 0,   L => 0);
foreach (1..$nexps) {
    my $event = gen_event_X(U => .1, M => .38, 'L');
    $count{$event}++;
}

while (my ($e, $c) = each %count) {
    my $freq = trim($c/$nexps);
    my $prob = $probs{$e};
    print "Event $e: $freq ($prob expected)\n";
}

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
