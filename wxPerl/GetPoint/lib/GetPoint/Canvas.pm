# GetPoint/Canvas.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-04

package GetPoint::Canvas;

use strict;
use warnings;
use Cwd;
use GetPoint::Groups;
use GetPoint::App;

use File::Spec;
use Wx::Perl::Imagick;
use Wx qw(wxCURSOR_HAND wxCURSOR_ARROW wxWHITE wxSOLID wxRED wxGREEN
          wxBLUE wxTRANSPARENT wxTRANSPARENT_BRUSH);
use Wx::Event qw(EVT_MOTION EVT_LEFT_DOWN EVT_LEFT_UP EVT_RIGHT_DOWN);

use base qw(Wx::ScrolledWindow);

our $PointSize = 1;
our $MarkSize = 10;
our $PenWidth = 4;

sub load {
    my ($self, $file) = @_;
    # set the numer of pixels the window will scroll at a time
    $self->SetScrollRate( 5, 5 );

    $self->SetBackgroundColour( wxWHITE );

    #warn "pwd : ", Cwd::cwd;
    if ($file =~ s/(.*[\\\/])(.+)$/$2/) {
        my $dir = $1;
        warn "Dir: $dir";
        chdir $dir;
    }
    warn "File: $file";
    my $image = Wx::Perl::Imagick->new($file);
    if (not $image) {
        Wx::LogMessage("error: Can't open $file with Wx::Perl::Imagick.");
        return;
    }
    my $bmp = $image->ConvertToBitmap();
    $self->{IMAGE} = $bmp;
    my ($w, $h) = $self->GetSizeWH();
    #if ($w < $bmp->GetWidth and $h < $bmp->GetHeight) {
        $self->SetVirtualSize( $bmp->GetWidth, $bmp->GetHeight );
    #}

    EVT_LEFT_DOWN($self, \&OnLeftDown);
    EVT_LEFT_UP($self, \&OnLeftUp);
    EVT_RIGHT_DOWN($self, \&OnRightDown);
    EVT_MOTION( $self, \&OnMouseMove );

    GetPoint::App::init_groups();
}

sub OnDraw {
    my( $self, $dc ) = @_;

    my $bmp = $self->{IMAGE};
    return if (not $bmp);
    $dc->DrawBitmap($bmp,0,0,1);

    #warn "@{$self->{Clicked}}";
    #warn "Yeah!";
    foreach my $group ($App::Groups->GetGroups) {
        foreach my $point ( $App::Groups->GetItems($group) ) {
            #warn "replot @$point";
            $self->draw_old_point($dc, split(/ /, $point), wxGREEN);
        }
    }
    my $pending = $self->{PendingPoint};
    if ($pending) {
        $self->draw_old_point($dc, @$pending);
    }
    my $point = $self->{MarkedPoint};
    return if not $point;
    my ($x, $y) = @$point;
    $dc->SetPen( Wx::Pen->new( wxRED, $PenWidth, 0 ) );
    $dc->SetBrush( wxTRANSPARENT_BRUSH );
    $dc->DrawCircle($x, $y, $MarkSize);
}

sub draw_new_point {
    my ($self, $event) = @_;
    my $dc = Wx::ClientDC->new( $self );
    $self->PrepareDC( $dc );
    my $pos = $event->GetLogicalPosition( $dc );
    my ($x, $y) = ($pos->x, $pos->y);
    #warn "Plot new point ($x, $y)";
    $dc->SetPen( Wx::Pen->new( wxRED, $PenWidth, 0 ) );
    $dc->SetBrush( Wx::Brush->new( wxRED, wxSOLID ) );
    $dc->DrawCircle($x, $y, $PointSize);
    $self->{PendingPoint} = [$x, $y];
}

sub draw_old_point {
    my $self = shift;
    my ($dc, $x, $y, $color) = @_;
    $color = wxRED if not defined $color;
    if (defined $x and defined $y) {
        $dc->SetPen( Wx::Pen->new( $color, $PenWidth, 0 ) );
        $dc->SetBrush( Wx::Brush->new( $color, wxSOLID ) );
        $dc->DrawCircle($x, $y, $PointSize);
    }
}

sub OnMouseMove {
    my( $self, $event ) = @_;

    return unless $event->Dragging and $event->LeftIsDown;

    my $prev = $self->{Point};
    return unless $prev;

    my $x_diff = $prev->[0] - $event->GetX;
    my $y_diff = $prev->[1] - $event->GetY;

    my ($x, $y) = $self->GetViewStart;
    $self->Scroll($x + $x_diff, $y + $y_diff);
    $self->{Point} = [$event->GetX, $event->GetY];
}

sub OnLeftDown {
    my ($self, $event) = @_;
    #warn $event;
    my ($x, $y) = ($event->GetX, $event->GetY);
    $self->{Point} = [$x, $y];
    #Wx::LogMessage( "Dragging the image..." );
    $self->SetCursor( Wx::Cursor->new( wxCURSOR_HAND ) );
    $event->Skip;
}

sub OnLeftUp {
    my ($self, $event) = @_;
    #warn $event;
    $self->SetCursor( Wx::Cursor->new( wxCURSOR_ARROW ) );
    $event->Skip;
}

sub OnRightDown {
    my ($self, $event) = @_;
    #warn $event;
    my ($x, $y) = ($event->GetX, $event->GetY);
    $self->{Point} = [$x, $y];

    #Wx::LogMessage( "Right clicked on point ($x, $y)." );
    $self->draw_new_point($event);
    #$self->Refresh;
    $event->Skip;
}

sub markPoint {
    my $self = shift;
    my ($x, $y) = @_;
    warn "Hey: ($x, $y)";
    $self->{MarkedPoint} = [$x, $y];
    $self->Refresh
}

1;
