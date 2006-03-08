#: FAST/Element.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

package FAST::Element;

use strict;
use warnings;

sub new {
    my ($proto) = @_;
    my $class = ref $proto || $proto;
    my $self = bless {
        id => undef,
    }, $class;
    $self->{id} = "$self";
    return $self;
}

sub clone {
    my $self = shift;
    my $clone = bless {%$self}, ref($self);
    $clone->{id} = "$clone";
    return $clone;
}

sub might_pass { die; }

sub must_pass { die; }

sub id {
    return $_[0]->{id};
}

sub entry { die; }

sub exit { die; }

sub visualize { die; }

sub as_c { die; }

sub as_png { die; }

1;
__END__

=head1 NAME

FAST::Element - Common virtual class for FAST DOM tree structures

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
