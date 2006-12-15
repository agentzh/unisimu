package Sim::Dispatcher;

use strict;
use warnings;

use Carp 'carp';
use Sim::Clock;

our $DEBUG = 0;
my @Queue;
my $Clock = Sim::Clock->new;

sub now {
    $Clock->now;
}

sub schedule {
    my $class = shift;
    my %events = @_;
    while (my ($time, $handle) = each %events) {
        if ($time < $class->now) {
            carp "out-dated event [$time => $handle] ignored";
            next;
        }
        $class->_insert_event([$time => $handle]);
    }
}

sub _insert_event {
    my ($class, $event) = @_;
    for (my $i = 0; $i < @Queue; $i++) {
        if ($event->[0] < $Queue[$i]->[0]) {
            splice( @Queue, $i, 0, $event );
            return;
        }
    }
    push @Queue, $event;
}

sub fire_next ($) {
    my $class = shift;
    return undef if @Queue == 0;
    my $event = shift @Queue;
    my ($time, $handle) = @$event;
    my $now = $class->now;
    if ($time >= $now) {
        $Clock->push_to($time);
        $handle->();
    } else {
        die "Clock modified outside of the dispatcher: next event is at $time while now is $now";
    }
    return 1;
}

sub run ($@) {
    my $class = shift;
    my $count = defined $_[0] ? shift : 1000;
    my $i = 0;
    while (1) {
        #warn "run: next!";
        last if $i++ >= $count or !defined $class->fire_next;
    }
}

sub time_of_next ($) {
    my $class = shift;
    return @Queue ? $Queue[0]->[0] : undef;
}

sub reset ($) {
    my $class = shift;
    @Queue = ();
    $Clock->reset();
}

1;
