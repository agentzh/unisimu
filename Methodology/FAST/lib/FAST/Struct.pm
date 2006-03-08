#: FAST/Struct.pm
#: Base class for various FAST DOM tree structures
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

package FAST::Struct;

use strict;
use warnings;
use base 'FAST::Element';

our $VERSION = '0.01';

sub _set_elems {
    my $self = shift;
    $self->{elems} = [@_];
}

sub elems {
    my $self = shift;
    return wantarray ? @{ $self->{elems} } : $self->{elems};
}

# Substitue $label with $dest in the current FAST::Struct
sub subs {
    my ($self, $label, $dest) = @_;
    $dest = $self->_node($dest);
    my $relems = $self->elems;
    my $done;
    for my $e (@$relems) {
        if ($e->isa('FAST::Node')) {
            if ($e->label eq $label) {
                $e = $dest;
                $done = 1;
            }
        } else {
            $done ||= $e->subs($label, $dest);
        }
    }
    return $done;
}

# FAST::Node wrapper
sub _node {
    my ($self, $n) = @_;
    return ref($n) ? $n : FAST::Node->new($n);
}

1;
__END__

=head1 NAME

FAST::Struct - Base class for various FAST DOM tree structures

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
