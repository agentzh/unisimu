#: Tesla.pm
#: Simulator for Tesla
#: v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-12-04

package Tesla;

our $VERSION = '0.05';

use strict;
use warnings;

use EventConsole;
use Gate;
use Signal;
use Clock;

our @sigs;

sub run {
    shift;
    my $limit = shift;
    my $time;
    # warn EventConsole->time_of_next_event;
    while (defined($time = EventConsole->time_of_next_event)) {
        # warn "HERE!";
        last if $time > $limit;
        # warn "pushing clock...";
        Clock->push_to($time);
        EventConsole->raise_event;
    }
}

sub reg_sig {
    shift;
    my $sig = shift;
    push @sigs, $sig;
}

sub reset {
    Clock->reset;
    EventConsole->reset;
    foreach my $sig (@sigs) {
        $sig->hist([]);
        $sig->force;
    }
}

sub clear {
    my $class = shift;
    $class->reset;
    foreach my $sig (@sigs) {
        $sig->dests([]);
    }
    $AND::delay = 0;
    $OR::delay = 0;
    $NAND::delay = 0;
    $NOR::delay = 0;
    $XOR::delay = 0;
    $NOT::delay = 0;
}

sub dumpstate {
    shift;
    my @strs;
    foreach my $sig (@sigs) {
        push @strs, $sig->name . "=" . $sig->value if $sig->value ne 'U';
    } 
    warn "STATE : " . join( ',', @strs ) . "\n";
}

1;

