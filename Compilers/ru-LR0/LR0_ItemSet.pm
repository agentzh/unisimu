#: LR0_ItemSet.pm
#: LR(0) ItemSet Class
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-20 2006-06-20

package LR0::ItemSet;

use strict;
use warnings;

sub new {
    my $proto = shift;
    my @items = @_;
    my $class = ref $proto || $proto;
    my $self = bless {
        items => \@items,
        edges => []
    }, $class;
    $self;
}

sub items {
    my $self = shift;
    @{ $self->{items} };
}

sub edges {
    my $self = shift;
    @{ $self->{edges} };
}

sub add_item {
    my $self = shift;
    my @items = @_;
    push @{ $self->{items} }, @items;
}

sub add_edge {
    my $self = shift;
    my @edges = @_;
    push @{ $self->{edges} }, @edges;
}

sub complete_items {
    my $self = shift;
    grep { $_->is_complete } @{ $self->{items} };
}

1;
