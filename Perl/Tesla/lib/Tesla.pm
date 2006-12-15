#: Tesla.pm
#: Simulator for Tesla
#: v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-12-04

package Tesla;

our $VERSION = '0.05';

use strict;
use warnings;

use base 'Sim::Dispatcher';
use Gate;
use Signal;

our @sigs;
our $DEBUG = 0;

sub schedule {
    my $class = shift;
	my ($times, $signal, $values) = @_;
    #warn "schedule: ", join (" ", @$times), " => $signal => ", join(' ', @$values), "\n";
	if (!ref $times) {
        $times = [$times];
    }
    if (!ref $values) {
        $values = [$values];
    }
    my $len = @$times;
    for my $i (0..$len-1) {
        my ($time, $value) = ($times->[$i], $values->[$i]);
        warn "Adding event ", $signal->name, " <== $value at $time\n" if $DEBUG;
        $class->SUPER::schedule(
            $time => 
            sub {
                $signal->value($value);
                $class->dumpstate() if $DEBUG;
            }
        );
	}
}

sub run {
    my $class = shift;
    #warn "HERE!";
    my $limit = shift;
    while (defined(my $time = $class->time_of_next)) {
        last if $time > $limit;
        warn "run: $time < $limit\n" if $DEBUG;
        warn "pushing clock to $time...\n" if $DEBUG;
        $class->fire_next;
    }
}

sub reg_sig {
    shift;
    my $sig = shift;
    push @sigs, $sig;
}

sub reset {
    my $class = shift;
    $class->SUPER::reset;
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

