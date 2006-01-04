# GetPoint/Groups.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-04 2006-01-04

package GetPoint::Groups;

use strict;
use warnings;
use List::MoreUtils qw(first_index);

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = bless {
    }, $class;
    return $self;
}

sub addGroup {
    my $self = shift;
    foreach my $group (@_) {
        return undef if $self->{$group};
        $self->{$group} = [];
        #warn "addGroup: groups: ", join('*', keys %$self);
    }
    return 1;
}

sub addItem {
    my $self = shift;
    my $group = shift;
    $self->{$group} ||= [];
    foreach my $item (@_) {
        #warn "addItem: pushing $item";
        push @{ $self->{$group} }, $item;
    }
    return 1;
}

sub getGroups {
    my $self = shift;
    my @groups = keys %$self;
    #warn "getGroups: @groups";
    return wantarray ? @groups : \@groups;
}

sub getItems {
    my $self = shift;
    my $group = shift;
    return wantarray ? @{ $self->{$group} } : $self->{$group};
}

sub removeGroup {
    my $self = shift;
    foreach my $group (@_) {
        delete $self->{$group};
    }
}

sub removeItem {
    my $self = shift;
    my $group = shift;
    my $data = $self->{$group};
    foreach my $item (@_) {
        my $idx = first_index { $_ eq $item } @$data;
        splice @$data, $idx, 1;
    }
}

1;
