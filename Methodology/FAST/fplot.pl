#: fplot.pl
#: Flowchart plotter
#: Coyright (c) 2006 Agent Zhang
#: 2006-03-06 2006-03-06

use strict;
use warnings;
use GraphViz;

my %edge_from;

my %FluxNodeStyle = (
    shape => 'circle',
    style => 'filled',
    filllcolor => 'yellow',
);

my @files = @ARGV;
if (!@files) {
    die "Usage: fplot <infile1> <infile2> ...\n";
}

for my $file (@files) {
    my $gv = GraphViz->new(
        layout => 'neato',
        edge => {color => 'red'},
        node => {
            style => 'filled',
            fillcolor => 'yellow',
        },
    );
    plot($file, $gv);
    $gv->as_png("$file.png");
}

sub plot {
    my ($fname, $gv) = @_;
    parse($fname);
    my $c = 0;
    while (my ($key, $val) = each %edge_from) {
        if (@$val > 1) {
            my $flux_node = "flux_" . $c++;
            $gv->add_node($flux_node, label => '', %FluxNodeStyle);
            plot_node($gv, $key);
            $gv->add_edge($flux_node => $key);
            for my $from (@$val) {
                $gv->add_edge($from, $flux_node);
            }
        } elsif (@$val == 1) {
            plot_node($gv, $key);
            $gv->add_edge($val->[0] => $key);
        } else {
            plot_node($gv, $key);
        }
    }
}

sub parse {
    my ($fname) = @_;
    open my $in, $fname or
        die "error: Can't open $fname for reading: $!\n";
    while (<$in>) {
        next if /^\s*$/;
        if (/^\s* (\S+) \s* => \s* (\S+) \s*$/xo) {
            my ($from, $to) = ($1, $2);
            $edge_from{$to} ||= [];
            $edge_from{$from} ||= [];
            push @{ $edge_from{$to} }, $from;
        } else {
            die "$fname: line $.: syntax error: $_";
        }
    }
    close $in;
}

sub plot_node {
    my ($gv, $node) = @_;
    if ($node =~ /^\[(.*)\]$/) {
        $gv->add_node($node, label => $1, shape => 'box');
    } elsif ($node =~ /^<(.*)>$/) {
        $gv->add_node($node, label => $1, shape => 'diamond');
    } else {
        $gv->add_node(
            $node,
            shape => 'plaintext',
            style => 'filled',
            fillcolor => 'white',
        );
    }
}
