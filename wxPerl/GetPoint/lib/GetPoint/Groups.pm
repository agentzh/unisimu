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

sub AddGroup {
    my $self = shift;
    foreach my $group (@_) {
        return undef if $self->{$group};
        $self->{$group} = [];
        #warn "AddGroup: groups: ", join('*', keys %$self);
    }
    return 1;
}

sub AddItem {
    my $self = shift;
    my $group = shift;
    $self->{$group} ||= [];
    foreach my $item (@_) {
        #warn "AddItem: pushing $item";
        push @{ $self->{$group} }, $item;
    }
    return 1;
}

sub GetGroups {
    my $self = shift;
    my @groups = keys %$self;
    #warn "GetGroups: @groups";
    return wantarray ? @groups : \@groups;
}

sub GetItems {
    my $self = shift;
    my $group = shift;
    my $data = $self->{$group};
    $data ||= [];
    return wantarray ? @$data : $data;
}

sub RemoveGroup {
    my $self = shift;
    foreach my $group (@_) {
        delete $self->{$group};
    }
}

sub SetItem {
    my ($self, $group, $index, $newval) = @_;
    return undef if not $self->{$group};
    $self->{$group}->[$index] = $newval;
    return 1;
}

sub RenameGroup {
    my ($self, $old, $new) = @_;
    return undef if not $self->{$old} or $self->{$new};
    $self->{$new} = $self->{$old};
    delete $self->{$old};
    return 1;
}

sub RemoveItem {
    my $self = shift;
    my $group = shift;
    my $data = $self->{$group};
    foreach my $item (@_) {
        my $idx = first_index { $_ eq $item } @$data;
        splice @$data, $idx, 1;
    }
}

1;
