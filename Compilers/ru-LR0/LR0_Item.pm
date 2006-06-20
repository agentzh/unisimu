#: LR0_Item.pm
#: LR(0) Item Class
#: Copyright (c) 2006 Agent Zhang
#: 2006-06-20 2006-06-20

package LR0::Item;

use strict;
use warnings;

sub new {
    my ($proto, $lhs, $rhs, $pos) = @_;
    my $class = ref $proto || $proto;
    $rhs ||= [];
    my $self = bless {
        lhs => $lhs,
        rhs => $rhs,
        pos => $pos || 0,
    }, $class;
    $self;
}

sub lhs {
    my ($self) = @_;
    $self->{lhs};
}

sub rhs {
    my $self = shift;
    @{ $self->{rhs} };
}

sub pos {
    my ($self) = @_;
    $self->{pos};
}

sub next_item {
    my ($self) = @_;
    my @rhs = $self->rhs;
    my $pos = $self->pos;
    if ($pos == @rhs) {
        return wantarray ? () : undef;
    }
    my $next = $self->new($self->lhs, [@rhs], $pos+1);
    wantarray ? ($next, $rhs[$pos]) : $next;
}

sub is_complete {
    my $self = shift;
    $self->rhs == $self->pos;
}

1;
