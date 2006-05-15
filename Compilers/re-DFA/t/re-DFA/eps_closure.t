#: eps_closure.t
#: test the eps_closure function in re::DFA

use strict;
use warnings;

use re::Graph;
#use Data::Dumper::Simple;
use Test::More 'no_plan';

BEGIN { use_ok('re::DFA::Util'); }

sub ordered {
    sort { $a <=> $b } @_;
}

*gen = \&re::DFA::Util::eps_closure;

my $g = re::Graph->build( <<'_EOC_' );

entry: 1
exit:  10

1,2: eps
2,4: eps
2,3: eps
3,9: a
9,8: eps
4,5: b
5,6: eps
6,7: a
7,8: eps
8,2: eps
8,10: eps
1,10: eps

_EOC_

ok $g;

#$g->as_png('nfa.png');

my $cache = {};
my $set = gen(1, $g, $cache);
my @elems = $set->elements;
is join(' ', ordered @elems), '1 2 3 4 10';
is join(' ', ordered keys %$cache), '1 2 3 4 10';

$set = gen(9, $g, $cache);
@elems = $set->elements;
is join(' ', ordered @elems), '2 3 4 8 9 10';
is join(' ', ordered keys %$cache), '1 2 3 4 8 9 10';

$set = gen(5, $g, $cache);
@elems = $set->elements;
is join(' ', ordered @elems), '5 6';
is join(' ', ordered keys %$cache), '1 2 3 4 5 6 8 9 10';

$set = gen(7, $g, $cache);
@elems = $set->elements;
is join(' ', ordered @elems), '2 3 4 7 8 10';
is join(' ', ordered keys %$cache), '1 2 3 4 5 6 7 8 9 10';

# create eps loop by s/3 --> a --> 9/3 --> eps --> 9/
my @edges = $g->node2edges(3);
$edges[0]->[0] = re::eps;
#$g->as_png('nfa.png');

$cache = {};
$set = gen(2, $g, $cache);
@elems = $set->elements;
is join(' ', ordered @elems), '2 3 4 8 9 10';
is join(' ', ordered keys %$cache), '2 3 4 8 9 10';
