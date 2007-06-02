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
    my %opts = @_;
    my $end_time = $class->now + $opts{duration} if defined $opts{duration};
    my $fires    = $opts{fires} || 100_000_000;
    my $i = 0;
    while (1) {
        #warn "run: next!";
        last if ++$i > $fires;
        my $t = $class->time_of_next;
        last if !defined $t or (defined $end_time and $t > $end_time);
        $class->fire_next;
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
__END__

=head1 NAME

Sim::Dispatcher - Event dispatcher for simulators

=head1 SYNOPSIS

    use Sim::Dispatcher;
    Sim::Dispatcher->schedule(
       32.5 => sub { print "hello, world!" }
    );
    Sim::Dispatcher->run( duration => 50 );
    # or Sim::Dispatcher->run( fires => 1 );

=head1 DESCRIPTION

This class implements the most important component in the whole Sim library,
the event dispatcher. Basically, every activites should be coordinated by
this dispatcher. Every other objects in a simulator either register an
event scheduled to happen at some point in the "future", or iterate through
the dispatching steps.

=head1 METHODS

=over

=item C<< CLASS->schedule( $time => $handle, ... ) >>

You can use this method to register events scheduled for the future, where
$time is the timestamp and $handle is an anonymous sub which will be invoked
by the dispatcher when the simulation time is at $time.

=item C<< CLASS->run( duration => $time, fires => $count ) >>

Runs the dispatcher according to the time duration and event firing count.
both of these named parameters are optional. When none is specified,
C<< fires => 100_000_000 >> will be assumed.

=item C<< CLASS->fire_next() >>

This method allows you to iterate through the dispatcher running process yourself.
You should only call C<fire_next> by hand if you've found the limitation criteria
given by the C<run> method can't fit your needs.

=item C<< CLASS->now() >>

Reads the value of the simulation time.

=item C<< CLASS->time_of_next() >>

Gets the timestamp of the next (or nearest) coming event, which is always a bit
greater or equal to "now".

=item C<< CLASS->reset() >>

Clears the internal event queue of the dispatcher and resets the internal simulation
clock too.

=back

=head1 AUTHOR

Agent Zhang E<lt>agentzh@gmail.comE<gt>

=head1 COPYRIGHT

Copyright 2006 by Agent Zhang. All rights reserved.

This library is free software; you can modify and/or modify it under the same terms as
Perl itself.

=head1 SEE ALSO

L<Sim::Clock>.
