#: Paging/LRU.pm
#: Copyright (c) Agent Zhang
#: 2005-11-17 2005-11-18

package Paging::LRU;

use strict;
use warnings;
use base 'Paging';

sub access {
    my ($self, $page) = @_;
    my @queue = $self->queue;
    for(my $i = 0; $i < @queue; $i++) {
        if ($queue[$i] eq $page) {
            splice @queue, $i, 1;
            push @queue, $page;
            $self->{_queue} = \@queue;
            return 1;
        }
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
