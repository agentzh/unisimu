#: re/Graph.pm
#: Graph data structures used by re::NFA and such
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-15 2006-05-17

package re;

our $eps;

sub eps {
    \$eps;
}

package re::Graph;

use strict;
use warnings;

use Carp qw( croak carp );
use GraphViz;
use Encode 'decode';
#use Data::Dumper::Simple;
use Perl6::Attributes;
use List::MoreUtils 'uniq';

sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $self = bless {
        node_to => {},
        entry   => undef,
        exit    => [],
    }, $class;
    if (@_) {
        #warn "@_";
        ./add_edge(@_);
        ./entry( $_[0] );
        ./exit( $_[2] );
    }
    $self;
}

sub merge {
    my ($self, $other) = @_;
    carp('Oh, dear!') if ! $other;
    my $new = bless {
        node_to => { %.node_to, %{$other->{node_to}} },
        entry   => undef,
        exit    => undef,
    }, ref $self;
    $new;
}

sub entry {
    my $self = shift;
    if (@_) {
        my $node = shift;
        $.node_to{$node} ||= [];
        $.entry = $node;
    }
    else    { $.entry; }
}

sub exit {
    my $self = shift;
    if (@_) {
        for my $node (@_) {
            $.node_to{$node} ||= [];
        }
        @.exit = @_;
    }
    else    { wantarray ? @.exit : $.exit[0]; }
}

sub add_exit {
    my ($self, $node) = @_;
    $.exit ||= [];
    $.node_to{$node} ||= [];
    push @.exit, $node;
}

sub is_exit {
    my ($self, $node) = @_;
    for my $exit (@.exit) {
        return 1 if $exit eq $node;
    }
    return undef;
}

sub nodes {
    my $self = shift;
    keys %.node_to;
}

sub node2edges {
    my ($self, $node) = @_;
    #warn Dumper($self, $node);
    my $edges = $.node_to{$node};
    #warn Dumper($node, $edges);
    if ($edges) {
        return @$edges;
    } else {
        return ();
    }
}

# returns all the weight of edges:
sub weight_list {
    my $self = shift;
    my @w;
    for my $node (./nodes) {
        my @edges = ./node2edges($node);
        for my $edge (@edges) {
            push @w, $edge->[0];
        }
    }
    @w = uniq @w;
    wantarray ? @w : \@w;
}

# return the next node of the given node with the weight
sub next_node {
    my ($self, $node, $w) = @_;
    for my $e (./node2edges($node)) {
        if ($e->[0] eq $w) {
            return $e->[1];
        }
    }
    undef;
}

sub add_edge {
    my ($self, $node1, $weight, $node2) = @_;
    my $edge = [ $weight, $node2 ];
    #push @.edges, $edge;
    $.node_to{$node1} ||= [];
    $.node_to{$node2} ||= [];
    push @{ $.node_to{$node1} }, $edge;
}

sub visualize {
    my ($self, $gv) = @_;
    my @nodes = ./nodes;
    for my $node (@nodes) {
        my @edges = ./node2edges($node);
        for my $edge (@edges) {
            my $weight = $edge->[0];
            $weight = '¦Å' if $weight eq re::eps();
            $gv->add_edge($node => $edge->[1], label => decode('GBK', " $weight "));
        }
    }
    $gv;
}

sub as_png {
    my $self = shift;
    my $gv = GraphViz->new(
        font => 'simsun.ttc',
        layout => 'dot',
        node => {
            shape => 'circle',
            style => 'filled',
            fillcolor => 'yellow',
        },
        edge => {
            color => 'red',
        },
    );
    $self->visualize($gv);
    $gv->add_node('-1', label => ' ', shape => 'plaintext', fillcolor => 'white');
    $gv->add_edge('-1' => $self->entry);
    for my $exit ($self->exit) {
        $gv->add_node($exit, shape => 'doublecircle');
    }
    $gv->as_png(@_);
}

sub build {
    my $self = shift;
    my $src = shift;
    open my $in, '<', \$src;
    local $/ = "\n";
    my $g = $self->new;
    while (<$in>) {
        next if /^\s*$/;
        if (/^\s* entry \s* : \s* (\S+)/x) {
            $g->entry($1);
        } elsif (/^\s* exit \s* : \s* (\S+)/x) {
            my $s = $1;
            my @exits = split /\s*,\s*/, $s;
            $g->exit(@exits);
        } elsif (/^\s* (\S+) \s* , \s* (\S+) \s* : \s* (\S+)/x) {
            my ($a, $b, $w) = ($1, $2, $3);
            $w = re::eps if $w eq 'eps';
            $g->add_edge($a, $w, $b);
        } else {
            chomp;
            croak "build: Syntax error: $_";
        }
    }
    $g;
}

sub normalize {
    my $self = shift;
    my $new_g = $self->new;
    my $c = 0;
    my $entry = $self->entry;
    $new_g->entry(1);
    my @queue = $entry;
    my %visited = ($entry => ++$c);;
    while (@queue) {
        my $node = shift @queue;
        my $node_id = $visited{$node};
        if ($self->is_exit($node)) {
            $new_g->add_exit($node_id);
        }
        my @edges = $self->node2edges($node);
        @edges = sort { $a->[0] cmp $b->[0] } @edges;
        for my $edge (@edges) {
            my $child = $edge->[1];
            if (! $visited{$child}) {
                $visited{$child} = ++$c;
                push @queue, $child;
            }
            my $child_id = $visited{$child};
            $new_g->add_edge($node_id, $edge->[0], $child_id);
        }
    }
    $new_g;
}

1;
