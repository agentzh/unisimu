# GetPoint/App.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-04

package GetPoint::App;

use strict;
use warnings;

use GetPoint::Groups;
use GetPoint::Frame;
use Carp qw(croak);

use base qw(Wx::App);

our $DefaultGroup = 'Default Group';

sub OnInit {
    my $self = shift;
    init_groups();
    my $frame = GetPoint::Frame->new(undef, -1, 'Scrolled Picture example');
    $self->SetTopWindow($frame);
    $frame->Show(1);
}

sub init_groups {
    $App::Groups = GetPoint::Groups->new;
    $App::Groups->AddGroup($DefaultGroup);
    $App::ActiveGroup = $DefaultGroup;
    $App::Initialized = 1;
    my $frame = $App::Frame;
    return if not $frame;
    $frame->yaml->Refresh;
    $frame->canvas->Refresh;
    $frame->tree->Refresh;
}

1;
