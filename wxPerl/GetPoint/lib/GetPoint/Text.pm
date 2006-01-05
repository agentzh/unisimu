package GetPoint::Text;

use strict;
use warnings;

use Wx;
use YAML;
use base qw(Wx::TextCtrl);

sub Refresh {
    my $self = shift;
    $self->Clear;
    $self->AppendText( YAML::Dump($App::Groups) );
    $self->SUPER::Refresh(@_);
}

1;
