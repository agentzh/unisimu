use strict;
use warnings;

=head1 NAME

Qooqle::Action::Search

=cut

package Qooqle::Action::Search;
use base qw/Qooqle::Action Jifty::Action/;

=head2 arguments

=cut

sub arguments {
    return (
        {
            search_keys => {
                label          => '',
                length         => 55,
            },
            wholeword_only => {
                type    => 'checkbox',
                label   => 'Wholeword only',
                default => 0,
            },
        }
    );
}

=head2 take_action

=cut

sub take_action {
    my $self = shift;
    
    # Custom action code
    
    $self->report_success if not $self->result->failure;
    
    return 1;
}

=head2 report_success

=cut

sub report_success {
    my $self = shift;
    # Your success message here
    $self->result->message('Success');
}

1;

