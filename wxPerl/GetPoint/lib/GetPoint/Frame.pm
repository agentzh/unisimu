# GetPoint/Frame.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-03

package GetPoint::Frame;

use strict;
use warnings;

use GetPoint::App qw(setAppMode testAppMode);
use GetPoint::Canvas;
use GetPoint::Toolbar;

use Wx::Html;
use Wx qw(:textctrl :sizer :window);
use Wx qw(wxDefaultPosition wxDefaultSize 
          wxDEFAULT_FRAME_STYLE wxNO_FULL_REPAINT_ON_RESIZE
          wxCLIP_CHILDREN wxOPEN wxFILE_MUST_EXIST wxCHANGE_DIR
          wxID_CANCEL wxCURSOR_WAIT wxCURSOR_ARROW);
use Wx::Event qw(EVT_LEFT_DOWN EVT_MENU);

use base qw/Wx::Frame/;

my ($ID_OPEN_IMAGE, $ID_DRAG, $ID_SELECT) = (1..3);

our $PrevDir = '';
our $PrevFile = '';

sub new {
    my $class = shift;
    my $self = $class->SUPER::new( undef, -1, "GetPoint", wxDefaultPosition,
                                   [ 700, 600 ], wxDEFAULT_FRAME_STYLE
                                   | wxNO_FULL_REPAINT_ON_RESIZE|wxCLIP_CHILDREN
                                  );

    my $border_mask = ~( wxSTATIC_BORDER|wxSIMPLE_BORDER|wxDOUBLE_BORDER|
                         wxSUNKEN_BORDER|wxRAISED_BORDER);

    $self->SetIcon( Wx::GetWxPerlIcon() );

    # Setup menus:

    my $file  = Wx::Menu->new;
    $file->Append( $ID_OPEN_IMAGE, "&Open" );

    my $mode  = Wx::Menu->new;
    $mode->AppendCheckItem( $ID_DRAG, "&Drag" );
    $mode->AppendCheckItem( $ID_SELECT, "&Select" );

    my $menu = Wx::MenuBar->new;
    $menu->Append( $file, "&File" );
    $menu->Append( $mode, "&Mode" );

    $self->SetMenuBar( $menu );

    $self->{ModeMenu} = $mode;
    $mode->Enable($ID_DRAG, 0);
    $mode->Enable($ID_SELECT, 0);

    EVT_MENU( $self, $ID_OPEN_IMAGE, \&OnOpenImage );
    EVT_MENU( $self, $ID_DRAG, \&OnDragMode );
    EVT_MENU( $self, $ID_SELECT, \&OnSelectMode );

    # Setup the toplevel layout:

    my $split0 = Wx::SplitterWindow->new( $self, -1, wxDefaultPosition,
                                          wxDefaultSize,
                                          wxNO_FULL_REPAINT_ON_RESIZE
                                         |wxCLIP_CHILDREN);

    my $split1 = Wx::SplitterWindow->new( $split0, -1, wxDefaultPosition,
                                          wxDefaultSize,
                                          wxNO_FULL_REPAINT_ON_RESIZE
                                         |wxCLIP_CHILDREN);
    my $split2 = Wx::SplitterWindow->new( $split1, -1, wxDefaultPosition,
                                          wxDefaultSize,
                                          wxNO_FULL_REPAINT_ON_RESIZE
                                         |wxCLIP_CHILDREN );
    my $tree = Wx::TreeCtrl->new( $split1, -1 );
    my $text = Wx::TextCtrl->new( $split2, -1, "Welcome to GetPoint powered by wxPerl.\n",
                                  wxDefaultPosition, wxDefaultSize,
                                  wxTE_READONLY|wxTE_MULTILINE
                                 |wxNO_FULL_REPAINT_ON_RESIZE );
    my $log = Wx::LogTextCtrl->new( $text );
    $self->{OLDLOG} = Wx::Log::SetActiveTarget( $log );

    # create the main notebook

    my $notebook = Wx::Notebook->new( $split2, -1, wxDefaultPosition, wxDefaultSize,
                                wxNO_FULL_REPAINT_ON_RESIZE|wxCLIP_CHILDREN );
    my $canvas = GetPoint::Canvas->new( $notebook, -1, wxDefaultPosition, wxDefaultSize,
                                    wxNO_FULL_REPAINT_ON_RESIZE
                                   |wxCLIP_CHILDREN );
    my $src = Wx::TextCtrl->new( $notebook, -1, '', wxDefaultPosition,
                                  wxDefaultSize, wxTE_READONLY|wxTE_MULTILINE
                                  |wxNO_FULL_REPAINT_ON_RESIZE );

    $notebook->AddPage( $canvas, "Image File", 0 );
    $notebook->AddPage( $src, "Data File", 0 );

    my $toolbar = GetPoint::Toolbar->new($split0, -1);

    $split0->SplitHorizontally( $toolbar, $split1, 30 );
    $split1->SplitVertically( $tree, $split2, 150 );
    $split2->SplitHorizontally( $notebook, $text, 460 );

    $self->{Notebook} = $notebook;
    $self->{Canvas}   = $canvas;
    $self->{Source}   = $src;
    $self->{Tree}     = $tree;

    $App::Frame = $self;

    return $self;
}

sub OnOpenImage {
    my( $self, $event ) = @_;
    my $dialog = Wx::FileDialog->new(
        $self, "Open Image File", $PrevFile, $PrevDir,
        "Image File (*.bmp;*.jpg;*.tif;*.png)|*.bmp;*.jpg;*.tif;*.png|".
        "All File (*.*)|*.*",
        wxOPEN|wxFILE_MUST_EXIST|wxCHANGE_DIR
    );

    my $path;
    if( $dialog->ShowModal == wxID_CANCEL ) {
        Wx::LogMessage( "User cancelled the Open Image File dialog" );
    } else {
        Wx::LogMessage( "Wildcard: %s", $dialog->GetWildcard);
        $path = $dialog->GetPath;
        $self->SetCursor( Wx::Cursor->new( wxCURSOR_WAIT ) );
        $self->{Canvas}->load($path);
        $self->{Canvas}->Refresh;
        my $mode = $self->{ModeMenu};
        $mode->Enable($ID_DRAG, 1);
        $mode->Enable($ID_SELECT, 1);
        if (testAppMode('Drag')) {
            $mode->Check($ID_DRAG, 1);
        } else {
            $mode->Check($ID_SELECT, 1);
        }
        $self->SetCursor( Wx::Cursor->new( wxCURSOR_ARROW ) );
    }

    if( $path ) {
        Wx::LogMessage( "File \"$path\" selected" );
    } else {
        Wx::LogMessage( "No file selected" );
    }

    $dialog->Destroy;
}

sub OnDragMode {
    my ( $self, $event ) = @_;
    my $menu = $self->{ModeMenu};
    if ($menu->IsChecked($ID_DRAG)) {
        Wx::LogMessage( "Current mode is Drag." );
        #$self->Check($ID_DRAG, 0);
        $menu->Check($ID_SELECT, 0);
        setAppMode('Drag');
    } else {
        Wx::LogMessage( "Current mode is Select." );
        #$self->Check($ID_DRAG, 1);
        $menu->Check($ID_SELECT, 1);
        setAppMode('Select');
    }
    my $canvas = $self->{Canvas};
    $canvas->set_cursor;
    $event->Skip;
}

sub OnSelectMode {
    my ( $self, $event ) = @_;
    my $menu = $self->{ModeMenu};
    if ($menu->IsChecked($ID_SELECT)) {
        Wx::LogMessage( "Current mode is Select." );
        #$menu->Check($ID_SELECT, 0);
        $menu->Check($ID_DRAG, 0);
        setAppMode('Select');
    } else {
        Wx::LogMessage( "Current mode is Drag." );
        #$menu->Check($ID_SELECT, 1);
        $menu->Check($ID_DRAG, 1);
        setAppMode('Drag');
    }
    my $canvas = $self->{Canvas};
    $canvas->set_cursor;
    $event->Skip;
}

1;
