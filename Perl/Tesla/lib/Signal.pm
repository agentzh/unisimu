#: Signal.pm
#: provide Signal class for Tesla
#: v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-12-04

package Signal;

use strict;
use warnings;

use Tesla;
use Data::Dumper;

$Data::Dumper::Indent = 1;
$Data::Dumper::Purity = 0;
$Data::Dumper::Useqq = 1;

our $DEGUB = 0;

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;

    my $name = shift if @_;
    my $self = {
        _name  => $name,
        _dests => [],
        _value => 'U',
        _hist  => [],
    };
    $self = bless $self, $class;
    $self->{_name} = "$self" unless defined $name;
    Tesla->reg_sig($self);
    return $self;
}

sub dests {
    my $self = shift;
    if (@_) {
        $self->{_dests} = shift;
        return;
    }
    return @{$self->{_dests}};
}

sub add_dest {
    my $self = shift;
    push @{$self->{_dests}}, @_;
}

sub name {
    my $self = shift;
    if (@_) {
        $self->{_name} = shift;
        return;
    }
    return $self->{_name};
}

sub hist {
    my $self = shift;
    if (@_) {
        $self->{_hist} = shift;
        return;
    }
    return @{$self->{_hist}};
}

sub histp {
    my $self = shift;
    my @events = $self->hist;
    my @list;
    my $prev;
    while (@events) {
        my $val = shift @events;
        my $time = shift @events;
        if (defined $prev and $prev eq $time) {
            pop @list;
            pop @list;
            push @list, $val, $time;
            next;
        }
        push @list, $val, $time;
        $prev = $time;
    }
    @events = @list;
    undef @list;
    undef $prev;
    
    while (@events) {
        my $val = shift @events;
        my $time = shift @events;
        if (defined $prev and $prev eq $val) {
            next;
        }
        push @list, "${val}\@$time";
        $prev = $val;
    }
    return join( ',', @list );
}

sub value {
    my $self = shift;
    if (@_) {
        my $val = shift;
        return if $val eq 'U';
        return if $val eq $self->{_value};

        $self->{_value} = $val;

        # append record to this signal's history:
        push @{$self->{_hist}}, $val, Tesla->now;

        # print info to stderr if in DEBUG mode:
        if ($Signal::DEBUG) {
            warn "$self->{_name} <== $val at " . Tesla->now . " ns\n";
        }
        
        # inform all the gates which this signal flows into:
        my $rdests = $self->{_dests};
        map { $_->activate } @$rdests;

        return;
    }
    return $self->{_value};
}

sub dump {
    my $self = shift;
    print Data::Dumper->Dump( [$self], [$self->name] ), "\n";
}

sub force {
    my $self = shift;
    if (@_) { $self->{_value} = shift; }
    else { $self->{_value} = 'U'; }
}

1;

