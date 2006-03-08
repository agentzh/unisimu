#: FAST/Node.pm
#: Non-structured FAST node
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

package FAST::Node;

use strict;
use warnings;
use base 'FAST::Element';

our $VERSION = '0.01';

sub new {
    my ($proto, $label) = @_;
    my $self = $proto->SUPER::new;
    $self->{label} = $label;
    return $self;
}

sub label {
    return $_[0]->{label};
}

1;
__END__

=head1 NAME

FAST::Node - Non-structured FAST node class

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
