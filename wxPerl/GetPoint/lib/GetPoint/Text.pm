package GetPoint::Text;

use strict;
use warnings;

use Wx qw(wxSAVE wxCHANGE_DIR wxID_CANCEL wxDefaultPosition
          wxDefaultSize wxTE_READONLY wxTE_MULTILINE
          wxNO_FULL_REPAINT_ON_RESIZE);
use Wx::Event qw(EVT_TEXT);
use YAML;
use base qw(Wx::TextCtrl);

sub new {
    my $proto = shift;
    my ($parent, $id) = @_;
    #warn "$proto $parent $id";
    my $self = $proto->SUPER::new(
        $parent, $id, YAML::Dump($App::Groups),
        wxDefaultPosition, wxDefaultSize,
        wxTE_READONLY|wxTE_MULTILINE|wxNO_FULL_REPAINT_ON_RESIZE,
    );
    EVT_TEXT($self, $id, \&OnChanged);
    return $self;
}

sub Refresh {
    my $self = shift;
    $self->Clear;
    $self->AppendText( YAML::Dump($App::Groups) );
    $self->SUPER::Refresh(@_);
}

sub path {
    my $self = shift;
    if (@_) { $self->{Path} = shift; }
    else    { return $self->{Path};  }
}

sub label {
    my $self = shift;
    if (@_) { $self->{Label} = shift; }
    else    { return $self->{Label};  }
}

sub save {
    my ($self, $saveas) = @_;
    my $path = $self->path;
    my $frame = $App::Frame;
    if ($path and not $saveas) {
        $self->SaveFile($path);
        $frame->notebook->SetPageText(1, $self->label);
        return;
    }
    my $dialog = Wx::FileDialog->new(
        $self, "Save YAML File", '', '',
        "YAML File (*.yml)|*.yml|All File (*.*)|*.*",
        wxCHANGE_DIR|wxSAVE
    );
    if ($dialog->ShowModal == wxID_CANCEL ) {
        return;
    } else {
        $path = $dialog->GetPath;
        $self->SaveFile($path);
        $self->path($path);
        my ($volume, $dir, $fname) = File::Spec->splitpath($path);
        $frame->notebook->SetPageText(1, $fname);
        $frame->notebook->Refresh;
        $self->label($fname);
    }
}

sub OnChanged {
    my ($parent, $event) = @_;
    my $frame = $App::Frame;
    return if not $frame;
    my $yaml = $frame->yaml;
    return if not $yaml->label;
    $frame->notebook->SetPageText(1, $yaml->label . '*');
    #$frame->notebook->Refresh;
}

sub newYaml {
    my $self = shift;
    $self->label(undef);
    $self->path(undef);
    my $frame = $App::Frame or return;
    $frame->notebook->SetPageText(1, 'YAML File');
}

1;
