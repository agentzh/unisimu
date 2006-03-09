#: FAST/Element.pm
#: Common base class for all FAST DOM tree entities
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-09

package FAST::Element;

use strict;
use warnings;
use base 'Clone';

sub new {
    my ($proto) = @_;
    my $class = ref $proto || $proto;
    my $self = bless {
        id => undef,
    }, $class;
    $self->{id} = "$self";
    return $self;
}

sub might_pass { die; }

sub must_pass { die; }

sub id {
    return $_[0]->{id};
}

sub clone {
    my $self = shift;
    my $clone = $self->SUPER::clone;
    $clone->{id} = "$clone";
    return $clone;
}

sub entry { die; }

sub exit { die; }

sub visualize { die; }

sub as_c { die; }

sub as_png {
    my ($self, $outfile) = @_;
    my %edge_from = %{ $self->{edge_from} };
    my %edge_to   = %{ $self->{edge_to} };

    my $gv = GraphViz->new(
        layout => 'dot',
        edge => {color => 'red'},
        node => {
            fillcolor => '#f1e1f4',
            color => '#918194',
            style => 'filled',
        },
    );

    $self->visualize($gv);
    require 'FAST.pm';
    FAST->plot_node($gv, 'entry');
    FAST->plot_node($gv, 'exit');
    $gv->add_edge('entry' => $self->entry);
    $gv->add_edge('exit'  => $self->exit);

    $gv->as_png($outfile);
}

1;
__END__

=head1 NAME

FAST::Element - Common virtual class for FAST DOM tree structures

=head1 INHERITANCE

    FAST::Element
        isa Clone

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
