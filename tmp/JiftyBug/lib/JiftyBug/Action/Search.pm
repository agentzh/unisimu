use strict;
use warnings;

=head1 NAME

JiftyBug::Action::Search

=cut

package JiftyBug::Action::Search;
use base qw/JiftyBug::Action Jifty::Action/;

use Jifty::Param::Schema;
use Jifty::Action schema {

param search_keys =>
    label is '',
    hints are 'Enter keywords here';

};

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

