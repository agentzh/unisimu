#: unit.t
#: unit tests for re::DFA::Min

use strict;
use warnings;

use Set::Scalar;
use re::Graph;
use Test::More tests => 8;

BEGIN { use_ok('re::DFA::Min'); }

*split_set = \&re::DFA::Min::split_set;

sub fmt {
    my @s;
    for my $set (@_) {
        push @s, '(' . join(' ',sort @$set) . ')';
    }
    @s = sort @s;
    local $" = " ";
    "@s";
}

###################

my $g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

1,2: b
2,3: b
3,1: b

_EOC_

my $alph = $g->weight_list;
my $set1 = [1,2,3];
my $set2 = [5,6];
my $set3 = [4];
my $level = [$set1, $set2, $set3];

my @sets = split_set($set1, $level, $g, $alph);
is( fmt(@sets), '(1 2 3)', 'no split happened' );

###################

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

1,2: b
2,3: b
3,5: b

_EOC_

@sets = split_set($set1, $level, $g, $alph);
is fmt(@sets), "(1 2) (3)", '(1 2 3) splits to (1 2) and (3)';

###################

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

_EOC_

@sets = split_set($set1, $level, $g, $alph);
is fmt(@sets), "(1 2 3)", 'no splits happen';

###################

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

1,4: b
2,3: b
3,5: b

_EOC_

@sets = split_set($set1, $level, $g, $alph);
is fmt(@sets), "(1) (2) (3)", '(1 2 3) splits to (1), (2) and (3)';

###################

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

5,6: b

_EOC_

@sets = split_set($set1, $level, $g, $alph);
is fmt(@sets), "(1 2 3)", 'no splits happen';

###################

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

1,3: b
3,3: b

_EOC_

@sets = split_set($set1, $level, $g, $alph);
is fmt(@sets), "(1 3) (2)", '(1 2 3) splits to (1 3) and (2) (error state distinguishes';

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  1

_EOC_

@sets = split_set([1], [[],[1]], $g, []);
is fmt(@sets), "(1)", '(1) remains';
