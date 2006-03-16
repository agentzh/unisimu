#: errors.t
#: Test error handling of FAST parser
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-12 2006-03-16

use Test::Base;
use FAST;

plan tests => 2 * blocks;

run {
    my $block = shift;
    my $src = $block->src;
    my $g = FAST->new(\$src);
    ok( !defined $g, $block->name );
    is( (!$g ? FAST->error()."\n" : undef), $block->err, $block->name );
};

__DATA__

=== TEST 1: random rats
--- src
entry => [a]
[a] => [b]
sdw dss
--- err
FAST::parse: STRING: line 3: syntax error: `sdw dss'.



=== TEST 2: Node names can only be `[..]', `<..>', `exit', and `entry'
--- src
entry => [b]
a => [b]
[b] => exit
--- err
FAST::parse: STRING: line 2: syntax error: Use of invalid node name `a'.



=== TEST 3: ditto, but right-hand
--- src
entry => [a]
[a] => b
[a] => exit
--- err
FAST::parse: STRING: line 2: syntax error: Use of invalid node name `b'.



=== TEST 4: at least one `entry' must be present
--- src
[a] => [b]
[b] => exit
--- err
FAST::parse: STRING: error: No `entry' node found.



=== TEST 5: at most one `entry' can be present
--- src
[a] => [b]
entry => [a]
entry => [b]
[b] => exit
--- err
FAST::parse: STRING: line 3: error: More than one `entry' node specified.



=== TEST 6: `entry' node can't be a rval
--- src
[a] => entry
--- err
FAST::parse: STRING: line 1: syntax error: `entry' node used on right-hand-side.



=== TEST 7: `exit' node can't be a lval
--- src
exit => [b]
--- err
FAST::parse: STRING: line 1: syntax error: `exit' node used on left-hand-side.



=== TEST 8: at least one `exit' must be present
--- src
[a] => [b]
entry => [a]
--- err
FAST::parse: STRING: error: No `exit' node found.



=== TEST 9: <..> node must have at least two children (1)
--- src
<a> => [b]
entry => <a>
[b] => exit
--- err
FAST::parse: STRING: error: Predicate node `<a>' has only one descendant.



=== TEST 10: <..> node must have at least two children (0)
--- src
entry => <a>
[b] => exit
--- err
FAST::parse: STRING: error: Predicate node `<a>' has no descendants.



=== TEST 11: <..> node can have at most 2 children
--- src
entry => <a>
<a> => exit
<a> => [b]
<a> => [c]
[c] => exit
[b] => exit
--- err
FAST::parse: STRING: line 4: error: Predicate node `<a>' has more than two descendants.



=== TEST 12: [..] node should have at least one descendant
--- src
entry => <p>
<p> => [a]
<p> => exit
--- err
FAST::parse: STRING: error: Function node `[a]' has no descendants.



=== TEST 13: [..] node cannot have more than 1 descendant
--- src
entry => [a]
[a] => [b]
[a] => exit
--- err
FAST::parse: STRING: line 3: error: Function node `[a]' has more than one descendant.



=== TEST 14: node name can't be empty (lhs)
--- src
entry => [a]
 => exit
[a] => exit
--- err
FAST::parse: STRING: line 2: syntax error: ` => exit'.



=== TEST 15: node name can't be empty (rhs)
--- src
[b] => 
entry => [b]
[a] => exit
--- err
FAST::parse: STRING: line 1: syntax error: `[b] => '.
