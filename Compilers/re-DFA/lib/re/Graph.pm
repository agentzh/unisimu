# re/Graph.pm

package re::Graph;

use strict;
use warnings;
use GraphViz;
use Encode 'decode';
#use Data::Dumper::Simple;
use Perl6::Attributes;

sub new {
    my ($proto, $node1, $weight, $node2) = @_;
    my $class = ref $proto || $proto;
    my $edge = [ $weight, $node2 ];
    my $self = bless {
        node_to => { $node1 => [$edge], $node2 => [] },
        entry   => $node1,
        exit    => $node2,
    }, $class;
    $self;
}

sub merge {
    my ($self, $other) = @_;
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
        $.entry = $node;
        $.node_to{$node} ||= [];
    }
    else    { $.entry; }
}

sub exit {
    my $self = shift;
    if (@_) { 
        my $node = shift;
        $.exit = $node;
        $.node_to{$node} ||= [];
    }
    else    { $.exit; }
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

sub add_edge {
    my ($self, $node1, $weight, $node2) = @_;
    my $edge = [ $weight, $node2 ];
    push @.edges, $edge;
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
            $gv->add_edge($node => $edge->[1], label => decode('GBK', $weight));
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
    $gv->add_node($self->exit, shape => 'doublecircle');

    $gv->as_png(@_);
}

package re;

our $eps;

sub eps {
    \$eps;
}

1;
