#: re/DFA/Util.pm
#: Utilities used by re::DFA
#: Copyright (c) 2006 Agent Zhang
#: 2006-05-15 2006-05-15

package re::DFA::Util;

use strict;
use warnings;
use base 'Exporter';
use re::Graph;
use Set::Scalar;

our @EXPORT_OK = qw(
    eps_closure
);

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
