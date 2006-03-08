#: FAST/Struct/If.pm
#: Branching structure in FAST DOM tree
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

package FAST::Struct::If;

use strict;
use warnings;

use base 'FAST::Struct';
use FAST::Node;

our $VERSION = '0.01';

sub new {
    my ($proto, $cond, $yes, $no, $tail) = @_;
    $tail = _node($tail);
    my $self = $proto->SUPER::new;
    $self->_set_elems(
        $self->_node($cond),
        $self->_node($yes),
        $self->_node($no),
        $self->_node($tail),
    );
    return $self;
}

# Return the conditional statement in the branching structure:
sub condition {
    my $self = shift;
    return ($self->elems)[0];
}

# Return the `true branch' statement in the branching structure:
sub true_branch {
    my $self = shift;
    return ($self->elems)[1];
}

# Return the `false branch' statement in the branching structure:
sub false_branch {
    my $self = shift;
    return ($self->elems)[2];
}

sub tail {
    my $self = shift;
    return ($self->elems)[3];
}

sub entry {
    return $_[0]->condition;
}

sub exit {
    return $_[0]->tail;
}

sub must_pass {
    my ($self, $label) = @_;
    return $self->condition->must_pass($label) or
        ($self->true_branch->must_pass($label) and
         $self->false_branch->must_pass($label);
}

1;
__END__

=head1 NAME

FAST::Struct::If - Branching structure in FAST DOM tree

=head1 SYNOPSIS

    use FAST::Struct::If;

    $if = FAST::Struct::If->new('<p>', '[f]', '[L:=3]');
    print $if->condition->label;
    print $if->true_branch->label;
    print $if->false_branch->label;
    @elems = $if->elems;
    $sucess = $if->subs('[f]', '[L:=3]');
    print $if->must_pass('[f]'); # false
    print $if->must_pass('<p>'); # true
    print $if->might_pass('[L:=3]'); # true

=head1 INHERITANCE

    FAST::Struct::If
        isa FAST::Struct
            isa FAST::Element

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
