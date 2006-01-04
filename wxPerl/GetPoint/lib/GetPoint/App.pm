# GetPoint/App.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-03

package GetPoint::App;

use strict;
use warnings;
use Carp qw(croak);

use base qw(Exporter Wx::App);
our @EXPORT_OK = qw(
    setAppMode testAppMode
);

require GetPoint::Frame;

our $Mode = 'drag';

@App::Group = ( [ 'Default Group' => [] ] );

sub OnInit {
    my $self = shift;
    my $frame = GetPoint::Frame->new(undef, -1, 'Scrolled Picture example');
    $self->SetTopWindow($frame);
    $frame->Show(1);
}

sub setAppMode {
    my $new = lc shift;
    croak "Unrecognized app mode: $new" if $new ne 'drag' and $new ne 'select';
    $Mode = $new;
}

sub testAppMode {
    my $tar = lc shift;
    croak "Unrecognized app mode: $tar" if $tar ne 'drag' and $tar ne 'select';
    return $Mode eq $tar;
}

1;
