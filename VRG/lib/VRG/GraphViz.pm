package VRG::GraphViz;

use strict;
use warnings;
use GraphViz;

my %format_rel = (
    parallel     => '//',
    orthogonal   => 'T',
    cross        => 'X',
);

my %NodeStyle =
(
    shape => 'ellipse',
    style => 'filled',
    fillcolor => '#f5f694',
);

my %EdgeStyle =
(
    dir   => 'none',
    color => 'grey',
    #style => 'bold',
);

my %InitArgs = (
    layout => 'circo',
    node => \%NodeStyle,
    edge => \%EdgeStyle,
);

sub new ($$) {
    my $class = ref $_[0] ? ref shift : shift;
    my $facts = shift;
    open my $in, '<', \$facts or die;
    my (%nodes, %edges);
    while (<$in>) {
        if (/(?x) ^ f-\d+ \s+ \(vector-relation \s+ (\S+) \s+ (\S+) \s+ (\S+) \)$/) {
            my $rel = $1;
            my ($a, $b) = sort $2, $3;
            $rel = format_rel($rel);
            #warn "$a $rel $b\n";
            my $key = join ' ', $a, $b;
            if (my $prev_rel = $edges{$key}) {
                if ($rel ne $prev_rel and $rel =~ m{^(?:T|//|X)$}) {
                    #warn "  replace ($prev_rel $key) with ($rel $key)...\n";
                    $edges{$key} = $rel;
                } else {
                    #warn "  ignoring ($rel $key) due to ($prev_rel $key).\n";
                }
            } else {
                #warn "adding ($rel $key)...\n";
                $nodes{$a} = $nodes{$b} = 1;
                $edges{$key} = $rel;
            }
        }
    }
    my @edges;
    while (my ($key, $val) = each %edges) {
        my ($a, $b) = split ' ', $key;
        push @edges, [$val, $a, $b];
    }
    close $in;
    bless {
        nodes => [keys %nodes],
        edges => \@edges,
    }, $class;
}

sub as_png ($$) {
    my ($self, $fname) = @_;
    my @nodes = @{ $self->{nodes} };
    my @edges = @{ $self->{edges} };
    my $gv = GraphViz->new(%InitArgs);
    for my $node (@nodes) {
        $gv->add_node($node);
    }
    for my $edge (@edges) {
        my ($rel, $a, $b) = @$edge;
        my %style;
        if ($rel eq '~//') {
            %style = (color => 'red', style => 'dashed');
        } elsif ($rel eq 'T') {
            %style = (color => 'black');
        } elsif ($rel eq '//') {
            %style = (color => 'red');
        } elsif ($rel eq 'X') {
            %style = (color => 'black', style => 'dashed');
        } else {
            %style = (label => $rel);
        }
        $gv->add_edge($a => $b, %style);
    }
    $gv->as_png($fname);
}

sub format_rel ($) {
    my $s = shift;
    my $retval;
    if ($s =~ s/^not_//) {
        $retval = '~';
    }
    $retval .= $format_rel{$s};
}

1;
