#: Clock.pm
#: Clock for simulation in Tesla
#: v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-09

package Clock;

use strict;
use warnings;
use Carp;

our $DEBUG = 0;

my $time = 0;  # in nano-seconds

sub push_to {
    shift;
    if ($_[0] < $time) {
        croak "Clock cannot be pushed backwards";
    }
    warn "Pushing time to $_[0] ns...\n" if $DEBUG;
    $time = $_[0];
}

sub reading {
    return $time;
}

sub reset {
    $time = 0;
}

1;

