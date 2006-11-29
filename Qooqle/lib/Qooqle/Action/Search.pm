=head1 NAME

Qooqle::Action::Search

=cut

package Qooqle::Action::Search;
use base qw/Qooqle::Action Jifty::Action/;

use strict;
use warnings;

use Jifty::Param::Schema;
use Jifty::Action schema {

param keys =>
    hints are 'Enter your search keys here',
    focus is 1;
};

=head2 arguments

=cut

=head2 take_action

=cut

use Encode qw(encode decode);

sub take_action {
    my $self = shift;
    my $keys = $self->argument_value('keys');
    $self->result->content(keys => $keys);
    warn "User searches for [", encode('GBK', $keys), "]\n";
    #warn "From Action::Search: (GBK): ", decode('GBK', $keys), "\n";
    return 1;
}

=head2 report_success

=cut

sub validate_keys {
   my ($self, $keys) = @_;
   if ($keys eq '') {
       return $self->validation_error(
           keys => "Forgot to enter your search keys above? *cough*"
       );
   }
   return $self->validation_ok('keys');
}

1;

