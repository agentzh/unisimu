#: re/DFA.pm
#: DFA emitter for NFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-15 2006-05-15

package re::DFA;

use strict;
use warnings;

use re;
use re::NFA;
use Set::Scalar;

sub translate {
    my ($src, $imfile) = @_;
    #warn $src;
    my $parser = re::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    my $nfa =re::NFA::emit($ptree);
    my $dfa = emit($nfa);
    $dfa->as_png($imfile);
    1;
}

sub emit {
    my $nfa = shift;
    my $c = 1;
    my $dfa = re::Graph->new;
    $dfa->entry($c);
    my %visited;
    my $cache = {};
    my @tasks = eps_closure($nfa->entry, $nfa, $cache);
    while (@tasks) {
        #warn "@tasks";
        my $set = shift @tasks;
        my %trans;
        while (defined(my $state = $set->each)) {
            my @edges = $nfa->node2edges($state);
            for my $edge (@edges) {
                my $c = $edge->[0];
                next if $c eq re::eps;
                $trans{$c} ||= Set::Scalar->new;
                $trans{$c}->insert($edge->[1]);
            }
        }
        my $id = $visited{ fmt($set) } || $c;
        for my $key (keys %trans) {
            my $subset = set_eps_closure($trans{$key}, $nfa, $cache);
            $trans{$key} = $subset;
            my $str = fmt($subset);
            #warn "KEY: $key\n";
            #warn "VALUE: $id\n";
            my $sub_id = $visited{$str};
            if (! $sub_id) {
                #warn "Hey!";
                push @tasks, $subset;
                $sub_id = ++$c;
                $visited{$str} = $sub_id;
            }
            $dfa->add_edge($id, $key, $sub_id);
        }
        if ( $set->has($nfa->exit) ) {
            $dfa->add_exit($id);
        }
    }
    $dfa;
}

# calculate the eps-closure of a set
sub set_eps_closure {
    my ($set, $g, $cache) = @_;
    my $retval = Set::Scalar->new;
    while (defined(my $state = $set->each)) {
        $retval += eps_closure($state, $g, $cache);
    }
    $retval;
}

sub fmt {
    my $set = shift;
    join(',', ordered( $set->elements ));
}

sub ordered {
    sort { $a <=> $b } @_;
}

sub eps_closure {
    my ($state, $graph, $cache) = @_;
    my $retval = $cache->{$state};
    if ($retval) { 
        #warn "short-cut!!!";
        return $retval;
    }
    # break potential cycles
    if (exists $cache->{$state}) {
        #warn "eps loop detected!!!";
        return Set::Scalar->new;
    }
    my @edges = $graph->node2edges($state);
    my $eps = re::eps;
    $retval = Set::Scalar->new($state);
    $cache->{$state} = undef;
    for my $edge (@edges) {
        next if $edge->[0] ne $eps;
        $retval += eps_closure($edge->[1], $graph, $cache);
    }
    $cache->{$state} = $retval;
    $retval;
}

1;
__END__
