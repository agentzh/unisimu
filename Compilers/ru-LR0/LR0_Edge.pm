#: LR0_Edge.pm
#: LR(0) Edge Class
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-20 2006-06-20

package LR0::Edge;

use strict;
use warnings;

sub new {
    my ($proto, $weight, $next_node) = @_;
    my $class = ref $proto || $proto;
    my $self = bless {
        weight => $weight,
        next_node => $next_node
    }, $class;
    $self;
}

sub weight {
    my $self = shift;
    $self->{weight};
}

sub next_node {
    my $self = shift;
    $self->{next_node};
}

1;