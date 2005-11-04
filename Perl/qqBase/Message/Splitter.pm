#: Message/Splitter.pm
#: 2005-11-03 2005-11-04

package Message::Splitter;

use strict;
use warnings;
use Data::Dumper;

our $Debug = 0;

sub new {
    my $class = shift;
    my $gap = shift;
    return bless {
        _prev_a => undef,
        _prev_b => undef,
        _prev => '',
        _delta_a => 0,
        _delta_b => 0,
        _a => undef,
        _b => undef,
        _gap => $gap,
    }, $class;
}

sub add {
    my $self = shift;
    my %hash = @_;
    if (exists $hash{a}) {
        $self->prev_a($self->a);
        $self->a($hash{a});
        if ($self->prev eq 'a') { # a a
            $self->delta_a($self->a - $self->prev_a);
            $self->delta_b($self->delta_b - $self->delta_a);
        } elsif ($self->prev eq 'b') { # b a
            $self->delta_a($self->a - $self->prev_a + $self->delta_a)
                if defined $self->prev_a;
            $self->delta_b(0);
        }
        $self->prev('a');
    } elsif (exists $hash{b}) {
        $self->prev_b($self->b);
        $self->b($hash{b});
        if ($self->prev eq 'a') { # a b
            $self->delta_a(0);
            $self->delta_b($self->b - $self->prev_b + $self->delta_b)
                if defined $self->prev_b;
        } elsif ($self->prev eq 'b') { # b b
            $self->delta_a($self->delta_a - $self->delta_b);
            $self->delta_b($self->b - $self->prev_b);
        }
        $self->prev('b');
    }
    print Data::Dumper->Dump([$self],[qw(self)]) if $Debug;
}

sub should_split {
    my $self = shift;
    return 1 if !$self->prev;
    if ($self->prev eq 'a' and defined $self->a and defined $self->b) {
        my $delta = $self->a - $self->b;
        if ($delta < $self->gap and $delta >= 0) {
            #$self->delta_a($delta) if $delta < $self->delta_a;
            return undef;
        }
    } elsif ($self->prev eq 'b' and defined $self->b and defined $self->a) {
        my $delta = $self->b - $self->a;
        if ($delta < $self->gap and $delta >= 0) {
            #$self->delta_b($delta) if $delta < $self->delta_b;
            return undef;
        }
    }
    if ($self->delta_a >= $self->gap and defined $self->prev_a) {
        $self->clear;
        return 1;
    } elsif ($self->delta_b >= $self->gap and defined $self->prev_b) {
        $self->clear;
        return 1;
    } else {
        return undef;
    }
}

sub clear {
    my $self = shift;
    $self->delta_a(0);
    $self->delta_b(0);
    if ($self->prev eq 'a') {
        $self->b(undef);
        $self->prev_b(undef);
        $self->prev_a(undef);
    } else {
        $self->a(undef);
        $self->prev_a(undef);
        $self->prev_b(undef);
    }
}

sub gap {
    return shift->{_gap};
}

sub delta_a {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_delta_a} = $val;
    } else {
        return $self->{_delta_a};
    }
}

sub delta_b {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_delta_b} = $val;
    } else {
        return $self->{_delta_b};
    }
}

sub a {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_a} = $val;
    } else {
        return $self->{_a};
    }
}

sub prev_a {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_prev_a} = $val;
    } else {
        return $self->{_prev_a};
    }
}

sub b {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_b} = $val;
    } else {
        return $self->{_b};
    }
}

sub prev_b {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_prev_b} = $val;
    } else {
        return $self->{_prev_b};
    }
}

sub prev {
    my ($self, $val) = @_;
    if (@_ == 2) {
        $self->{_prev} = $val;
    } else {
        return $self->{_prev};
    }
}

1;
