package GetPoint::Tree;

use strict;
use warnings;

use Wx;
use base 'Wx::TreeCtrl';
use Wx qw(wxDefaultPosition wxDefaultSize wxTR_EDIT_LABELS
          wxTR_HAS_BUTTONS wxTR_HIDE_ROOT);
use Wx::Event qw(EVT_TREE_END_LABEL_EDIT EVT_TREE_END_LABEL_EDIT
                 EVT_TREE_KEY_DOWN EVT_TREE_BEGIN_LABEL_EDIT
                 EVT_TREE_SEL_CHANGED);

our $OldLabel;
our $AddingGroup;
our $Counter = 1;
our $GROUP = 'GROUP';
our $POINT = 'POINT';
our $NewGroup;

sub new {
    my $proto = shift;
    my ($parent, $id) = @_;
    my $self = $proto->SUPER::new($parent, $id,
        wxDefaultPosition, wxDefaultSize,
        wxTR_EDIT_LABELS|wxTR_HAS_BUTTONS
    );
    $self->Refresh;

    EVT_TREE_BEGIN_LABEL_EDIT($parent, $id, \&BeginChangeItem);
    EVT_TREE_END_LABEL_EDIT($parent, $id, \&EndChangeItem);
    EVT_TREE_SEL_CHANGED($parent, $id, \&OnItemChanged);
    EVT_TREE_KEY_DOWN( $parent, $id, \&OnKeyDown );
    EVT_TREE_SEL_CHANGED( $parent, $id, \&OnSelChanged );
    return $self;
}

sub Refresh {
    my $self = shift;
    #Wx::LogMessage( "Refreshing tree control..." );
    $self->DeleteAllItems;
    my $root_id = $self->AddRoot( 'Point Groups', -1, -1, d( undef ) );
    for my $group (sort $App::Groups->GetGroups) {
        my $group_id = $self->AppendItem( $root_id, $group, -1, -1, d( [$GROUP] ) );
        my $i = 0;
        for my $point ($App::Groups->GetItems($group)) {
            $self->AppendItem( $group_id, $point, -1, -1, d( [$POINT, $group, $i++] ) );
        }
        $self->Expand( $group_id );
    }
    $self->Expand($root_id);
    $self->SelectItem( $App::ActiveID ) if $App::ActiveID;
    $self->SUPER::Refresh(@_);
}

sub d { Wx::TreeItemData->new( $_[0] ) }

sub BeginChangeItem {
    my ($self, $event) = @_;
    $OldLabel = $event->GetLabel;
    #Wx::LogMessage( "Save $OldValue" );
}

sub EndChangeItem {
    my ($self, $event) = @_;

    if ($AddingGroup) {
        OnItemCreate($event);
    } else {
        OnItemRename($event);
    }
}

sub OnItemRename {
    my $event = shift;
    my $frame = $App::Frame;
    my $tree = $frame->tree;
    #print "$tree";
    my $label = $event->IsEditCancelled ? $OldLabel : $event->GetLabel;
    my $id = $event->GetItem;
    my $data = $tree->GetPlData($id);
    my $group;

    #Wx::LogMessage( "OnItemRename: New labe: $label <=> $id" );
    if ($data and $data->[0] eq $POINT) {
        if ($label !~ /^\d+\s+\d+$/) {
            Wx::LogMessage( "error: OnItemRename: The new coordinate is malformed." );
            return;
        }
        $App::Groups->SetItem($data->[1], $data->[2], $label);
    } elsif (!$data or $data->[0] eq $GROUP) {
        #QQQ set tree leaf data to [group, idx]
        if ($label =~ /^\s*$/) {
            Wx::LogMessage( "error: OnItemRename: Group name can't be empty." );
            return;
        }
        if (not $App::Groups->RenameGroup($OldLabel, $label)) {
            Wx::LogMessage( "error: OnItemRename: Renaming group $OldLabel to $label failed." );
            return;
        }
        $group = $label;
    } else {
        Wx::LogMessage( "error: OnItemRename: Unknown node type: $data->[0]." );
    }
    $tree->SelectItem($id);
    $App::ActiveGroup = $group if $group;

    # refresh other controls
    $frame->yaml->Refresh;
    $frame->canvas->Refresh;
    $event->Skip;
}

sub OnItemCreate {
    $AddingGroup = 0;
    my $event = shift;
    my $frame = $App::Frame;
    my $tree = $frame->tree;
    my $old_id = $event->GetOldItem;
    my $old_label = $tree->GetItemText($old_id);
    #print "$tree";
    my $id = $event->GetItem;
    my $label = $event->IsEditCancelled ? $NewGroup : $event->GetLabel;
    #Wx::LogMessage( "OnItemCreate: Label currently is $label" );
    my $data = $tree->GetPlData($id);

    if ($label =~ /^\s*$/) {
        Wx::LogMessage( "error: OnItemCreate: group name can't be empty" );
        return;
    }
    if (not $App::Groups->AddGroup($label)) {
        Wx::LogMessage( "error: OnItemCreate: adding new group $label failed" );
        return;
    }
    $App::ActiveGroup = $label;
    $App::ActiveGroupID = $id;
    $App::ActiveID = $id;
    $tree->SelectItem($id);

    # refresh other controls
    $frame->yaml->Refresh;
    $frame->canvas->Refresh;
    #$event->Skip;
}

sub OnKeyDown {
    my ($parent, $event) = @_;
    #Wx::LogMessage( "Stroke the key " . $event->GetKeyCode . " to tree" );
    #Wx::LogMessage( "Keystroke: " . $event->GetKeyCode );
    my $id = $App::ActiveID;
    return if not $id;
    #undef $App::ActiveID;
    my $group = $App::ActiveGroup;
    my $frame = $App::Frame;
    return if not $frame;
    my $tree = $frame->tree;
    #Wx::LogMessage( "OnKeyDown: ID: $id" );
    #Wx::LogMessage( "OnKeyKown: Root ID: ".$tree->GetRootItem);

    if ($event->GetKeyCode eq 127) { # <DEL>
        my $data = $tree->GetPlData($id);
        if (not $data) {
            Wx::LogMessage( "error: You can't remove root. :=)" );
            return;
        }
        if ($data->[0] eq $GROUP) {
            #Wx::LogMessage( "Deleting $group" );
            $App::Groups->RemoveGroup($group);
        } elsif ($data->[0] eq $POINT) {
            my $point = $tree->GetItemText($id);
            #Wx::LogMessage( "Deleting point $point in group $data->[1]" );
            $App::Groups->RemoveItem($data->[1], $point);
        } else {
            Wx::LogMessage( "error: Unknown node type: $data->[0]" );
            return;
        }
    }
    $tree->Refresh;
    $event->Skip;
}

sub OnSelChanged {
    my( $parent, $event ) = @_;
    return if not $App::Initialized;
    my $id = $event->GetItem;

    my $frame = $App::Frame;
    return if not $frame;
    my $tree = $frame->tree;
    return if not $tree;
    my $canvas = $frame->canvas;
    return if not $canvas;

    my $data = $tree->GetPlData( $id );
    return if not $data;

    if ($data->[0] eq $GROUP) {
        $App::ActiveGroup = $tree->GetItemText($id);
        $App::ActiveID = $id;
    } elsif ($data->[0] eq $POINT) {
        $App::ActiveGroup = $data->[1];
        $App::ActiveID = $id;
        $canvas->markPoint(split /\s+/, $tree->GetItemText($id));
        #$canvas->Refresh;
    } else {
        Wx::LogMessage( "OnSelChanged: Unknown node type $data->[0]" );
        return;
    }
    #Wx::LogMessage( "Switch to $App::ActiveGroup ($App::ActiveID" );
}

sub addGroup {
    my $self = shift;
    $NewGroup = "New Group ".gen_id();
    my $group_id = $self->AppendItem(
        $self->GetRootItem,
        $NewGroup,
        -1, -1, d( undef )
    );
    $AddingGroup = 1;
    $self->EditLabel($group_id);
}

sub gen_id {
    return $Counter++;
}

1;
