#: FAST.pm
#: Global application class for FAST
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-10

package FAST;

use 5.006001;
use strict;
use warnings;

use GraphViz;
use FAST::Struct::Seq;
use FAST::Struct::While;
use FAST::Struct::If;

use Data::Dumper::Simple;

our $VERSION = '0.01';

our %FluxNodeStyle = (
    shape => 'circle',
    style => 'filled',
    filllcolor => 'yellow',
);

our $Error;

sub new {
    my ($proto, $src) = @_;
    if (not $src) {
        $Error = "FAST::new: No input source specified.";
        return undef;
    }
    my $class = ref $proto || $proto;
    my $self = bless {
    }, $class;
    return $self->parse($src) ? $self : undef;
}

sub parse {
    my ($self, $src) = @_;
    my ($fname, $in);
    if (ref $src) {
        open $in, '<', $src;
        $fname = 'STRING';
    } else {
        $fname = $src;
        if (not open $in, $fname) {
            $Error = "FAST::parse: Can't open `$fname' for reading: $!";
            return undef;
        }
    }
    my (%edge_from, %edge_to);
    local $/ = "\n";
    while (<$in>) {
        next if /^\s*$/;
        if (/^\s* (\S+) \s* => \s* (\S+) \s*$/xo) {
            my ($from, $to) = ($1, $2);
            $edge_from{$to} ||= [];
            $edge_from{$from} ||= [];
            push @{ $edge_from{$to} }, $from;
            $edge_to{$from} ||= [];
            $edge_to{$to}   ||= [];
            push @{ $edge_to{$from} }, $to;
        } else {
            $Error = "FAST::parse: $fname: line $.: syntax error: $_";
            close $in;
            return undef;
        }
    }
    close $in;
    $self->{edge_from} = \%edge_from;
    $self->{edge_to}   = \%edge_to;
    return 1;
}

sub error {
    return $Error;
}

sub as_png {
    my ($self, $outfile) = @_;
    my %edge_from = %{ $self->{edge_from} };
    my %edge_to   = %{ $self->{edge_to} };

    my $gv = GraphViz->new(
        layout => 'neato',
        edge => {color => 'red'},
        node => {
            fillcolor => '#f1e1f4',
            color => '#918194',
            style => 'filled',
        },
    );

    my $c = 0;
    while (my ($key, $val) = each %edge_from) {
        if (@$val > 1) {
            my $flux_node = "flux_" . $c++;
            $self->plot_node($gv, '', $flux_node);
            $self->plot_node($gv, $key);
            $gv->add_edge($flux_node => $key);
            for my $from (@$val) {
                if ($edge_to{$from}->[0] eq $key) {
                    $edge_to{$from}->[0] = $flux_node;
                }
                $self->_plot_edge($gv, $from => $flux_node);
            }
        } elsif (@$val == 1) {
            $self->plot_node($gv, $key);
            $self->_plot_edge($gv, $val->[0] => $key);
        } else {
            $self->plot_node($gv, $key);
        }
    }

    $gv->as_png($outfile);
}

sub _plot_edge {
    my ($self, $gv, $from, $to) = @_;
    my @to_nodes = @{ $self->{edge_to}->{$from} };
    my $label;
    if (@to_nodes > 1) {
        $label = $to eq $to_nodes[0] ? 'Y' : 'N';
    }
    if ($label) {
        $gv->add_edge($from => $to, label => $label);
    } else {
        $gv->add_edge($from => $to);
    }
}

sub plot_node {
    my ($self, $gv, $node, $id) = @_;
    $id = $node if not defined $id;
    if ($node =~ /^\s*$/) {
        $gv->add_node($id, label => ' ', %FluxNodeStyle);
    } elsif ($node =~ /^\[(.*)\]$/) {
        $gv->add_node($id, label => $1, shape => 'box');
    } elsif ($node =~ /^<(.*)>$/) {
        $gv->add_node($id, label => $1, shape => 'diamond');
    } else {
        $gv->add_node(
            $id,
            label => $node,
            shape => 'plaintext',
            style => 'filled',
            fillcolor => 'white',
        );
    }
}

sub as_asm {
    my ($self, $outfile) = @_;
    my $out;
    my $buf;
    if ($outfile) {
        if (!open $out, ">$outfile") {
            $Error = "as_asm: Can't open `$outfile' for writing: $!";
        }
    } else {
        open $out, '>', \$buf;
    }

    my %edge_from = %{ $self->{edge_from} };
    my %edge_to   = %{ $self->{edge_to} };

    my (%labels, %visited, @tasks);
    if (! $edge_to{entry}) {
        $Error = "as_asm: No `entry' node found.";
        return undef;
    }
    my $c = 1;
    my $cur = $edge_to{entry}->[0];
    my $head = 1;
    while ($cur) {
        if ($visited{$cur}) {
            my $label = $labels{$cur};
            #warn "JMP!!! $prev - $cur - $label";
            if (!$head) {
                print $out "    jmp  $label\n";
                $head = 1;
            }
            $cur = shift @tasks;
            next;
        }
        if (@{ $edge_from{$cur} } > 1) {
            $labels{$cur} ||= 'L' . $c++;
            my $label = $labels{$cur};
            print $out "$label:\n";
            $visited{$cur} = 1;
        } elsif ($labels{$cur}) {
            print $out "$labels{$cur}:\n";
        }
        my $cmd = $self->node2asm($cur);
        print $out "    $cmd\n";
        $head = $cmd eq 'exit';
        my @next = @{ $edge_to{$cur} };
        if (@next > 1) {
            $labels{$next[1]} ||= 'L'.$c++;
            my $label = $labels{$next[1]};
            print $out "    jno  $label\n";
            push @tasks, $next[1];
            $cur = $next[0];
        } elsif (@next == 1) {
            $cur = $next[0];
        } else {
            $cur = shift @tasks;
        }
    }
    close $out;
    return $outfile ? 1 : $buf;
}

sub node2asm {
    my ($self, $node) = @_;
    if ($node =~ /^<(.*)>$/) {
        return "test $1";
    } elsif ($node =~ /^\[(.*)\]$/) {
        return "do   $1";
    } else {
        return $node;
    }
}

sub structured {
    my $self = shift;
    my %opts = @_;
    if (%opts and !$opts{optimized}) {
        die "FAST::structured: Options %opts not recognized";
    }
    my %edge_to   = %{ $self->{edge_to} };
    my $entry = $edge_to{entry}->[0];
    my @nodes =
        grep { $_ ne $entry and (/^\[.*\]$/ or /^<.*>$/) }
            keys %edge_to;
    @nodes = sort { _core_label($a) cmp _core_label($b) } @nodes;
    unshift @nodes, $entry;
    #warn Dumper(@nodes);
    my %ids;
    for (0..$#nodes) {
        $ids{ $nodes[$_] } = $_ + 1;
    }
    #warn Dumper(%ids);

    my @g;
    my $i = 1;
    for my $node (@nodes) {
        my @next = map { $ids{$_} || 0 } @{ $edge_to{$node} };
        #warn "$node with @next";
        if (@next == 1) {
            $g[$i] = FAST::Struct::Seq->new($node, "[L:=$next[0]]");
        } else {
            $g[$i] = FAST::Struct::If->new($node, "[L:=$next[0]]", "[L:=$next[1]]");
        }
        $i++;
    }
    if (not $opts{optimized}) {
        return _gen_unoptimized_ast(@g);
    } else {
        return _gen_optimized_ast(@g);
    }
}

sub _core_label {
    my $node = shift;
    if ($node =~ /^<(.*)>$/) {
        return $1;
    } elsif ($node =~ /^\[(.*)\]$/) {
        return $1;
    } else {
        return $node;
    }
}

sub _gen_unoptimized_ast {
    my @g = @_;
    my $i = $#g;
    my $prev = '';
    while ($i >= 1) {
        $prev = FAST::Struct::If->new("<L=$i>", $g[$i], $prev);
        $i--;
    }
    my $loop = FAST::Struct::While->new('<L>0>', $prev);
    my $ast = FAST::Struct::Seq->new('[L:=1]', $loop);
    return $ast;
}

sub _gen_optimized_ast {
    my @g = @_;
    my @new_g;
    my $i = $#g;
    while ($i > 1) {
        if ($g[$i]->might_pass("[L:=$i]")) {
            push @new_g, $g[$i];
            next;
        }
        map { $_->subs("[L:=$i]", $g[$i]) } @g[1..$i-1];
        $i--;
    }
    if (@new_g > 1) {
        return _gen_unoptimized_ast(@new_g);
    }
    my $g = $g[1];
    if ($g->must_pass('[L:=0]')) {
        $g->subs('[L:=0]', '');
        return $g;
    }
    $g->subs('[L:=1]', '');
    my $loop = FAST::Struct::While->new('<L>0>', $g);
    my $ast = FAST::Struct::Seq->new('[L:=1]', $loop);
    return $ast;
}

1;
__END__

=head1 NAME

FAST - Library for Flowchart Abstract Syntax Tree

=head1 SYNOPSIS

    use FAST;

    $src = <<'.';
    entry => <p>
    <p> => [c]
    [c] => <q>
    <p> => [a]
    <q> => <p>
    <q> => exit
    [a] => [b]
    [b] => exit
    .

    # Load from string:
    $g = FAST->new(\$src) or
        die FAST->error;

    # Load from disk file:
    $g = FAST->new('foo.in') or
        die FAST->error;

    # Generate PNG image:
    $g->as_png('blah.png');

    # Or return the image data directly:
    $bin_data = $g->as_png;

    # Generate pseud assembly code dipicting the flowchart:
    $g->as_asm('blah.asm');

    # Or return the ASM code directly:
    $asm_src = $g->as_asm;

=head1 DESCRIPTION

=head1 AUTHOR

Agent Zhang L<mailto:agentzh@gmail.com>

=head1 COPYRIGHT

Copyright (c) 2006 Agent Zhang

This library is free software. You can redistribute it and/or
modify it under the same terms as Perl itself.
