#: re/DFA/Min.pm
#: emit minimized DFA from raw DFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-15 2006-05-17

package re;

our $err;

sub err {
    \$err;
}

package re::DFA::Min;

use strict;
use warnings;

use re;
use re::DFA;
use Set::Scalar;
use List::Util 'first';

sub translate {
    my ($src, $imfile) = @_;
    #warn $src;
    my $parser = re::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    my $nfa =re::NFA::emit($ptree);
    my $dfa = re::DFA::emit($nfa);
    my $min_dfa = re::DFA::Min::emit($dfa);
    $min_dfa->as_png($imfile);
}

sub emit {
    my $dfa = shift;
    my $accept   = Set::Scalar->new;
    my $unaccept = Set::Scalar->new;
    for my $state ($dfa->nodes) {
        if ($dfa->is_exit($state)) {
            $accept->insert($state);
        } else {
            $unaccept->insert($state);
        }
    }
    my @level = ($accept, $unaccept);
    while ( process_level(\@level, $dfa) ) {
        # do nothing here
    }
    build_min_dfa($dfa, \@level);
}


sub process_level {
    my ($level, $dfa) = @_;
    my @new_level;
    my $modified;
    my $alphabet = $dfa->weight_list;
    for my $set (@$level) {
        if ($modified or $set->size <= 1) {
            push @new_level, $set;
            next;
        }
        my @sets = split_set($set, $level, $dfa, $alphabet);
        $modified = 1 if @sets > 1;
        push @new_level, @sets;
    }
    @$level = @new_level if $modified;
    $modified;
}

sub split_set {
    my ($set, $level, $graph, $alphabet) = @_;
    for my $char (@$alphabet) {
        my %equiv;
        while (defined(my $state = $set->each)) {
            my $next_state = $graph->next_node($state, $char);
            my $image_set;
            if (!defined $next_state) {
                $next_state = re::err;
                $image_set = re::err;
            }
            $image_set ||= first { $_->has($next_state) } @$level;
            $equiv{$image_set} ||= [];
            push @{ $equiv{$image_set} }, $state;
        }
        if (keys %equiv > 1) {
            return map { Set::Scalar->new(@$_) } values %equiv;
        }
    }
    return ($set);
}

sub build_min_dfa {
    my ($dfa, $level) = @_;
    my $new_dfa = re::Graph->new;
    my %nodes;
    my $c = 0;
    for my $set (@$level) {
        $c++;
        while (defined(my $state = $set->each)) {
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