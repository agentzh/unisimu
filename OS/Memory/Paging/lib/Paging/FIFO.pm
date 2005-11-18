#: Paging/FIFO.pm
#: 2005-11-18 2005-11-18

package Paging::FIFO;

use strict;
use warnings;
use base 'Paging';

sub access {
    my ($self, $page) = @_;
    foreach ($self->queue) {
        return 1 if $_ eq $page;
    }
    return undef;
}

sub alloc {
    my ($self, $page) = @_;
    my @queue = $self->queue;
    my $old;
    if (@queue < $self->mem_size) {
        push @queue, $page;
    } else {
        $old = shift @queue;
        push @queue, $page;
    }
    $self->{_queue} = \@queue;
    return $old;
}

1;
