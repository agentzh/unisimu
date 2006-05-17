#: unit.t
#: unit tests for re::DFA::Min

use strict;
use warnings;

use Set::Scalar;
use re::Graph;
use Test::More 'no_plan';

BEGIN { use_ok('re::DFA::Min'); }

*split_set = \&re::DFA::Min::split_set;

sub use_fmt {
    Set::Scalar->as_string_callback(
        sub { '(' . join(' ',sort $_[0]->elements) . ')' }
    );
}

sub no_fmt {
    Set::Scalar->as_string_callback(undef);
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
my $set1 = Set::Scalar->new(1,2,3);
my $set2 = Set::Scalar->new(5,6);
my $set3 = Set::Scalar->new(4);
my $level = [$set1, $set2, $set3];
my @sets = split_set($set1, $level, $g, $alph);

use_fmt;
is( "@sets", '(1 2 3)', 'no split happened' );
no_fmt;

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
use_fmt;
@sets = sort { "$a" cmp "$b" } @sets;
is "@sets", "(1 2) (3)", '(1 2 3) splits to (1 2) and (3)';
no_fmt;

###################

$g = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4

1,5: a
2,5: a
3,6: a

_EOC_

@sets = split_set($set1, $level, $g, $alph);
use_fmt;
@sets = sort { "$a" cmp "$b" } @sets;
is "@sets", "(1 2 3)", 'no splits happen';
no_fmt;

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
use_fmt;
@sets = sort { "$a" cmp "$b" } @sets;
is "@sets", "(1) (2) (3)", '(1 2 3) splits to (1), (2) and (3)';
no_fmt;

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
use_fmt;
@sets = sort { "$a" cmp "$b" } @sets;
is "@sets", "(1 2 3)", 'no splits happen';
no_fmt;

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
use_fmt;
@sets = sort { "$a" cmp "$b" } @sets;
is "@sets", "(1 3) (2)", '(1 2 3) splits to (1 3) and (2) (error state distinguishes';
no_fmt;
