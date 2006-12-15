package SM::Server;

use strict;
use warnings;
use SM::Simulator;
use overload '""' => sub { $_[0]->id };

my $Counter;

sub new {
    my $class = ref $_[0] ? ref shift : shift;
    bless {
        id    => $Counter++,
        queue => [],
        busy  => 0,
    }, $class;
}

sub _serve_next {
    my $self = shift;
    my $queue = $self->_queue;
    if ($self->busy and @$queue) {
        my $client = shift @$queue;
        $self->log("Client $client leaves the queue.");
    }
    if (@$queue) {
        $self->{busy} = 1;
        $self->log("Starts to serve client $queue->[0].");
        my $serve_time = $self->gen_serve_time();
        my $now = SM::Simulator->now;
        SM::Simulator->schedule(
            $now + $serve_time,
            =>
            sub { $self->_serve_next }
        );
    } else {
        $self->{busy} = 0;
    }
}

sub id {
    $_[0]->{id};
}

sub busy {
    $_[0]->{busy};
}

sub gen_serve_time {
    2;
}

sub _queue {
    $_[0]->{queue};
}

sub join_queue {
    my $self = shift;
    $self->log("Client @_ joins the queue.");
    push @{ $self->{queue} }, @_;
    if (not $self->busy) { $self->_serve_next; }
}

sub queue_len {
    my $self = shift;
    scalar(@{$self->_queue});
}

sub log {
    my $self = shift;
    SM::Simulator::log("<Server $self> @_");
}

1;
