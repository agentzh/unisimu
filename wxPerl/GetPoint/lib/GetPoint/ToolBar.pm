# GetPoint/ToolBar.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-04

package GetPoint::ToolBar;

use strict;
use warnings;

use Wx qw(wxHORIZONTAL wxALL);
use Wx::Event qw(EVT_BUTTON);

use base 'Wx::Panel';

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    my $btn_refresh = Wx::Button->new( $self, -1, "Refresh" );
    my $btn_add_g = Wx::Button->new( $self, -1, "Add Group" );
    my $btn_add_p = Wx::Button->new( $self, -1, "Add Point" );

    my $buttons = Wx::BoxSizer->new( wxHORIZONTAL );
    $buttons->Add( $btn_refresh, 0, wxALL, 5 );
    $buttons->Add( $btn_add_g, 0, wxALL, 5 );
    $buttons->Add( $btn_add_p, 0, wxALL, 5 );

    $self->SetSizer( $buttons );
    $self->SetAutoLayout( 1 );

    EVT_BUTTON( $self, $btn_refresh, \&OnRefresh );
    EVT_BUTTON( $self, $btn_add_g, \&OnAddGroup );
    EVT_BUTTON( $self, $btn_add_p, \&OnAddPoint );

    return $self;
}

sub OnAddGroup {
    my ($self, $event) = @_;
    my $frame = $App::Frame;
    my $tree = $frame->tree;
    $tree->addGroup;
}

sub OnAddPoint {
    my ($self, $event) = @_;
    my $frame = $App::Frame;
    my $canvas = $frame->canvas;
    my $tree = $frame->tree;
    my $yaml = $frame->yaml;
    my $point = $canvas->{PendingPoint};
    if (not $point) {
        Wx::LogMessage("error: no point to add");
        return;
    }
    undef $canvas->{PendingPoint};
    my ($x, $y) = @$point;
    #Wx::LogMessage("OnAddPoint: ActiveGroup: $App::ActiveGroup");
    $App::Groups->AddItem($App::ActiveGroup, "$x $y");
    $canvas->Refresh;
    $yaml->Refresh;
    $tree->Refresh;
    #Wx::LogMessage( "Point ($x,$y) added." );
}

sub OnRefresh {
    my ($self, $event) = @_;
    my $frame = $App::Frame;
    $frame->canvas->Refresh;
    $frame->tree->Refresh;
    $frame->yaml->Refresh;
}

1;
