#: re/DFA/Min.pm
#: emit minimized DFA from raw DFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-15 2006-05-18

package re;

our $err;

sub err {
    \$err;
}

package re::DFA::Min;

use strict;
use warnings;

use re::DFA;
use List::Util 'first';
use List::MoreUtils 'any';
#use Data::Dumper::Simple;

sub translate {
    my ($self, $src, $imfile) = @_;
    my $min_dfa = $self->transform($src);
    return undef if ! $min_dfa;
    $min_dfa->normalize->as_png($imfile);
    1;
}

sub transform {
    my $self = shift;
    my $dfa = re::DFA->transform(@_);
    $self->emit($dfa);
}

sub emit {
    my ($self, $dfa) = @_;
    my $accept   = [];
    my $unaccept = [];
    for my $state ($dfa->nodes) {
        if ($dfa->is_exit($state)) {
            push @$accept, $state;
        } else {
            push @$unaccept, $state;
        }
    }
    my @level = ($accept, $unaccept);
    while ( process_level(\@level, $dfa) ) {
        #warn "process once more";
    }
    #warn Dumper(@level);
    build_min_dfa($dfa, \@level);
}


sub process_level {
    my ($level, $dfa) = @_;
    my @new_level;
    my $modified;
    my $alphabet = $dfa->weight_list;
    for my $set (@$level) {
        if ($modified or @$set <= 1) {
            push @new_level, $set;
            next;
        }
        my @sets = split_set($set, $level, $dfa, $alphabet);
        #warn Dumper(@sets);
        $modified = 1 if @sets > 1;
        push @new_level, @sets;
    }
    @$level = @new_level if $modified;
    #warn "modified: $modified";
    #warn "@$level";
    $modified;
}

sub split_set {
    my ($set, $level, $graph, $alphabet) = @_;
    for my $char (@$alphabet) {
        my %equiv;
        for my $state (@$set) {
            my $next_state = $graph->next_node($state, $char);
            my $image_set;
            if (!defined $next_state) {
                $next_state = re::err;
                $image_set = re::err;
            }
            $image_set ||= first { contains($next_state, @$_) } @$level;
            $equiv{$image_set} ||= [];
            push @{ $equiv{$image_set} }, $state;
        }
        if (keys %equiv > 1) {
            return values %equiv;
        }
    }
    return ($set);
}

sub contains {
    my $elem = shift;
    for my $e (@_) {
        return 1 if $e eq $elem;
    }
    undef;
}

sub build_min_dfa {
    my ($dfa, $level) = @_;
    my $new_dfa = re::Graph->new;
    my %nodes;
    my $c = 0;
    #warn "@$level";
    for my $set (@$level) {
        #warn @$set;
        $c++;
        for my $state (@$set) {
            #warn "build_min_dfa: $state";
            $nodes{$state} = $c;
            if ($dfa->is_exit($state)) {
                $new_dfa->add_exit($c);
            }
        }
    }
    $new_dfa->entry( $nodes{ $dfa->entry } );
    my %edges;
    while (my ($state, $id) = each %nodes) {
        my @edges = $dfa->node2edges($state);
        for my $edge (@edges) {
            my $sub_id = $nodes{$edge->[1]};
            my $edge_id = "$id $edge->[0] $id";
            if (! $edges{$edge_id}) {
                $new_dfa->add_edge($id, $edge->[0], $sub_id);
                $edges{$edge_id} = 1;
            }
        }
    }
    $new_dfa;
}

1;
__END__
