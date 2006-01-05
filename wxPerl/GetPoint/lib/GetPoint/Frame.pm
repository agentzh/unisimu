# GetPoint/Frame.pm
# Copyright (c) 2006 Agent Zhang
# 2006-01-03 2006-01-03

package GetPoint::Frame;

use strict;
use warnings;

use GetPoint::App;
use GetPoint::ToolBar;
use GetPoint::Canvas;
use GetPoint::Text;
use GetPoint::Tree;

use Wx::Html;
use Wx qw(:textctrl :sizer :window);
use Wx qw(wxDefaultPosition wxDefaultSize 
          wxDEFAULT_FRAME_STYLE wxNO_FULL_REPAINT_ON_RESIZE
          wxCLIP_CHILDREN wxOPEN wxFILE_MUST_EXIST wxCHANGE_DIR
          wxID_CANCEL wxCURSOR_WAIT wxCURSOR_ARROW);
use Wx::Event qw(EVT_LEFT_DOWN EVT_MENU);

use base qw/Wx::Frame/;

my ($ID_OPEN_IMAGE, $ID_TREE) = (1..2);

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
    $file->Append( $ID_OPEN_IMAGE, "&Open Image" );

    my $menu = Wx::MenuBar->new;
    $menu->Append( $file, "&File" );

    $self->SetMenuBar( $menu );

    EVT_MENU( $self, $ID_OPEN_IMAGE, \&OnOpenImage );

    # Setup the toplevel layout:

    my $split0 = Wx::SplitterWindow->new( $self, -1, wxDefaultPosition,
                                          wxDefaultSize,
                                          wxNO_FULL_REPAINT_ON_RESIZE
                                         |wxCLIP_CHILDREN);

    my $split1 = Wx::SplitterWindow->new( $split0, -1, wxDefaultPosition,
                                          wxDefaultSize,
                                          wxNO_FULL_REPAINT_ON_RESIZE
                                         |wxCLIP_CHILDREN);

    my $tree = GetPoint::Tree->new( $split1, $ID_TREE );
    #$split1->{Tree} = $tree;
    my $notebook = Wx::Notebook->new( $split1, -1, wxDefaultPosition, wxDefaultSize,
                                wxNO_FULL_REPAINT_ON_RESIZE|wxCLIP_CHILDREN );

    my $canvas = GetPoint::Canvas->new( $notebook, -1, wxDefaultPosition, wxDefaultSize,
                                    wxNO_FULL_REPAINT_ON_RESIZE
                                   |wxCLIP_CHILDREN );
    my $yaml = GetPoint::Text->new( $notebook, -1, '', wxDefaultPosition,
                                  wxDefaultSize, wxTE_READONLY|wxTE_MULTILINE
                                  |wxNO_FULL_REPAINT_ON_RESIZE );

    $notebook->AddPage( $canvas, "Image File", 0 );
    $notebook->AddPage( $yaml, "YAML File", 0 );

    my $toolbar = GetPoint::ToolBar->new($split0, -1);

    $split0->SplitHorizontally( $toolbar, $split1, 30 );
    $split1->SplitVertically( $tree, $notebook, 150 );

    $self->{Notebook} = $notebook;
    $self->{Canvas}   = $canvas;
    $self->{Yaml}     = $yaml;
    $self->{Tree}     = $tree;

    $App::Frame = $self;

    return $self;
}

sub notebook {
    return $_[0]->{Notebook};
}

sub canvas {
    return $_[0]->{Canvas};
}

sub yaml {
    return $_[0]->{Yaml};
}

sub tree {
    return $_[0]->{Tree};
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
        #Wx::LogMessage( "User cancelled the Open Image File dialog" );
    } else {
        #Wx::LogMessage( "Wildcard: %s", $dialog->GetWildcard);
        $path = $dialog->GetPath;
        $self->SetCursor( Wx::Cursor->new( wxCURSOR_WAIT ) );
        $self->{Canvas}->load($path);
        $self->{Canvas}->Refresh;
        $self->SetCursor( Wx::Cursor->new( wxCURSOR_ARROW ) );
    }

    if( $path ) {
        #Wx::LogMessage( "File \"$path\" selected" );
    } else {
        #Wx::LogMessage( "No file selected" );
    }

    $dialog->Destroy;
}

1;
