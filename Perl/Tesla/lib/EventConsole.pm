#: EventConsole.pm
#: Event console for Tesla
#: v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-09

package EventConsole;

use strict;
use warnings;

use Carp;
use Clock;
use Tesla;

our $DEBUG = 0;

# ordered list for events:
our @event_list = ();

sub add_event {
    shift;
    my ($time, $signal, $new_val) = @_;
    return 0 unless $time >= Clock->reading;

    warn "Adding event ", $signal->name, " <== ", $new_val, " at ", $time, "\n" if $DEBUG;
    my $new_event = [ $time, $signal, $new_val ];
    for (my $i = 0; $i < @event_list; $i++) {
        if ($time < $event_list[$i]->[0]) {
            splice( @event_list, $i, 0, $new_event );
            return 1;
        }
    }
    push @event_list, $new_event;
    return 1;
}

sub add_events {
    my $self = shift;
    my $rtimes = shift;
    my $signal = shift;
    my $rvals = shift;
    croak "number of times not equal to that of values"
        if (@$rtimes != @$rvals);
    for (my $i = 0; $i < @$rtimes; $i++) {
        $self->add_event( $rtimes->[$i], $signal, $rvals->[$i] );
    }
}

sub raise_event {
    while (@event_list) {
        if (time_of_next_event() == Clock->reading) {
            my $event = shift @event_list;
            my ($time, $signal, $new_val) = @$event;
            $signal->value($new_val);
            Tesla->dumpstate() if $DEBUG;
        } else {
            last;
        }
    }
}

sub time_of_next_event {
    return unless defined $event_list[0];
    return $event_list[0]->[0];
}

sub reset {
    undef @event_list;
}

1;

