# table.t
# Test re::Graph::table

use strict;
use warnings;

use Test::More tests => 3;
use Test::Differences;
use re::Graph;

my $graph = re::Graph->build( <<_EOC_ );

entry: 1
exit:  4
1, 2: a
1, 3: b
3, 2: c
3, 4: d

_EOC_

ok $graph, 'graph ok';

my $tb = $graph->as_table;
my $s = "$tb";
$s =~ s/\s+\n/\n/sg;
eq_or_diff $s, <<'END_TABLE', 'table contents ok';
State a b c d
1     2 3
2
3         2 4
4
END_TABLE

isa_ok $tb, 'Text::Table';
