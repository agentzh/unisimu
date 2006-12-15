package Sim::Dispatcher;

use strict;
use warnings;

use Carp 'carp';

our $DEBUG = 0;

sub new ($$) {
    my $class = ref $_[0] ? ref shift : shift;
    my $clock = shift;
    return bless {
        queue => [],
        clock => $clock,
    }, $class;
}

sub clock ($) {
    return $_[0]->{clock};
}

sub schedule {
    my $self = shift;
    my %events = @_;
    while (my ($time, $handle) = each %events) {
        if ($time < $self->clock->now) {
            carp "out-dated event [$time => $handle] ignored";
            next;
        }
        $self->_insert_event([$time => $handle]);
    }
}

sub _queue {
    $_[0]->{queue};
}

sub _insert_event {
    my ($self, $event) = @_;
    my $queue = $self->_queue;
    for (my $i = 0; $i < @$queue; $i++) {
        if ($event->[0] < $queue->[$i]->[0]) {
            splice( @$queue, $i, 0, $event );
            return;
        }
    }
    push @$queue, $event;
}

sub fire_next ($) {
    my $self = shift;
    my $queue = $self->_queue;
    return undef if @$queue == 0;
    my $event = shift @$queue;
    my ($time, $handle) = @$event;
    my $now = $self->clock->now;
    if ($time >= $now) {
        $self->clock->push_to($time);
        $handle->();
    } else {
        die "Clock modified outside of the dispatcher: next event is at $time while now is $now";
    }
    return 1;
}

sub run ($@) {
    my $self = shift;
    my $count = defined $_[0] ? shift : 1000;
    my $i = 0;
    while (1) {
        #warn "run: next!";
        last if $i++ >= $count or !defined $self->fire_next;
    }
}

sub time_of_next ($) {
    my $self = shift;
    my $queue = $self->_queue;
    return @$queue ? $queue->[0]->[0] : undef;
}

sub reset ($) {
    my $self = shift;
    my $queue = $self->_queue;
    @$queue = ();
    $self->clock->reset();
}

1;
