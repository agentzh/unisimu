# GetPoint/Toolbar.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-04

package GetPoint::Toolbar;

use strict;
use warnings;

use Wx qw(wxHORIZONTAL wxALL);
use Wx::Event qw(EVT_BUTTON);

use base 'Wx::Panel';

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    my $btn_add_g = Wx::Button->new( $self, -1, "Add Group" );
    my $btn_rem_g = Wx::Button->new( $self, -1, "Remove Group" );
    my $btn_add_p = Wx::Button->new( $self, -1, "Add Point" );
    my $btn_rem_p = Wx::Button->new( $self, -1, "Remove Point" );
    my $btn_refresh = Wx::Button->new( $self, -1, "Refresh" );

    my $buttons = Wx::BoxSizer->new( wxHORIZONTAL );
    $buttons->Add( $btn_add_g, 0, wxALL, 5 );
    $buttons->Add( $btn_rem_g, 0, wxALL, 5 );
    $buttons->Add( $btn_add_p, 0, wxALL, 5 );
    $buttons->Add( $btn_rem_p, 0, wxALL, 5 );
    $buttons->Add( $btn_refresh, 0, wxALL, 5 );

    $self->SetSizer( $buttons );
    $self->SetAutoLayout( 1 );

    EVT_BUTTON( $self, $btn_add_g, \&OnAddGroup );
    EVT_BUTTON( $self, $btn_rem_g, \&OnRemGroup );
    EVT_BUTTON( $self, $btn_add_p, \&OnAddPoint );
    EVT_BUTTON( $self, $btn_rem_p, \&OnRemPoint );
    EVT_BUTTON( $self, $btn_refresh, \&OnRefresh );

    return $self;
}

sub OnAddGroup {
    my ($self, $event) = @_;
}

sub OnRemGroup {
}

sub OnAddPoint {
    my ($self, $event) = @_;
    my $frame = $App::Frame;
    my $canvas = $frame->{Canvas};
    my $tree = $frame->{Tree};
    my $src = $frame->{Source};
    my $point = $canvas->{PendingPoint};
    undef $canvas->{PendingPoint};
    my ($x, $y) = @$point;
    $App::Points ||= [];
    push @{$App::Points}, [$x, $y];
    $canvas->Refresh;
    Wx::LogMessage( "Point ($x,$y) added." );
}

sub OnRemPoint {
}

sub OnRefresh {
    my ($self, $event) = @_;
    my $frame = $App::Frame;
    my $canvas = $frame->{Canvas};
    $canvas->Refresh;
}

1;
