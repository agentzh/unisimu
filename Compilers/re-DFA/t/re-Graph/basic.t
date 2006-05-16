# basic.t

use strict;
use warnings;
#use Data::Dumper::Simple;
use File::Compare;

use Test::More tests => 54;
BEGIN { use_ok('re::Graph'); }

my $a = re::Graph->new(3, 'a', 9);
ok $a;
isa_ok $a, 're::Graph';

my $name = '3 -> a -> 9';
is_deeply [$a->node2edges(3)], [['a',9]], "$name, 3";
is_deeply [$a->node2edges(9)], [], "$name, 9";
is_deeply [$a->node2edges(4)], [], "$name, 4";
is $a->entry, 3, "$name, entry";
is $a->exit,  9, "$name, exit";
is join(' ', sort $a->nodes), '3 9';

my $b = re::Graph->new(4, 'b', 5);
ok $b;
isa_ok $b, 're::Graph';
#warn Dumper($b);
$name = '4 -> b -> 5';
is_deeply [$b->node2edges(4)], [['b',5]], "$name, 4";
is_deeply [$b->node2edges(5)], [], "$name, 5";
is_deeply [$b->node2edges(3)], [], "$name, 3";
is $b->entry, 4, "$name, entry";
is $b->exit,  5, "$name, exit";
is join(' ', sort $b->nodes), '4 5';

my $aa = re::Graph->new(6, 'a', 7);
ok $aa;
isa_ok $aa, 're::Graph';
$name = '6 -> a -> 7';
is_deeply [$aa->node2edges(6)], [['a',7]], $name;
is_deeply [$aa->node2edges(7)], [], $name;
is_deeply [$aa->node2edges(5)], [], $name;
is $aa->entry, 6, "$name, entry";
is $aa->exit,  7, "$name, exit";
is join(' ', sort $aa->nodes), '6 7';

my $g47 = $b->merge($aa);
$name = '4 -> b -> 5 -> eps -> 6 -> a -> 7';
ok $g47;
isa_ok $g47, 're::Graph';
#warn $g47->entry(), " ", $g47->exit();
ok !defined $g47->entry(), "$name, undef entry";
ok !defined $g47->exit(),  "$name, undef exit";

is_deeply [$g47->node2edges(5)], [], "$name, 5";
$g47->add_edge(5, re::eps, 6);
is_deeply [$g47->node2edges(5)], [[re::eps, 6]], "$name, 5";

is_deeply [$g47->node2edges(4)], [['b',5]], "$name, 4";

is_deeply [$g47->node2edges(3)], [], "$name, 3";
is_deeply [$g47->node2edges(6)], [['a',7]], "$name, 6";
is_deeply [$g47->node2edges(7)], [], "$name, 7";
is join(' ', sort $g47->nodes), '4 5 6 7';
$g47->entry(4);
is $g47->entry, 4, "$name, entry -> 4";
$g47->exit(7);
is $g47->exit, 7, "$name, exit -> 7";

my $g47_new = re::Graph->build( <<_EOC_ );

entry: 4
exit:  7
4,5: b
5,6: eps
6,7: a

_EOC_

#warn Dumper($g47);
is_deeply( $g47_new, $g47, "test Graph->build using $g47" );

my $g28 = $g47->merge($a);
$name = '2->...->8';
ok $g28;
isa_ok $g28, 're::Graph';

is_deeply [$g28->node2edges(2)], [], "$name, 2";
$g28->add_edge(2, re::eps, 4);
is_deeply [$g28->node2edges(2)], [[re::eps, 4]], "$name, 2";

$g28->add_edge(7, re::eps, 8);
is_deeply [$g28->node2edges(7)], [[re::eps, 8]], "$name, 7";

$g28->add_edge(2, re::eps, 3);
is_deeply [$g28->node2edges(2)], [[re::eps, 4], [re::eps, 3]], "$name, 2";

$g28->add_edge(9, re::eps, 8);
is_deeply [$g28->node2edges(9)], [[re::eps, 8]], "$name, 9";

$g28->entry(2);
is $g28->entry, 2, "$name, entry -> 2";
$g28->exit(8);
is $g28->exit, 8, "$name, exit -> 8";

my $file = 't/re-Graph/g28.png';
unlink $file if -f $file;
$g28->as_png($file);
ok -f $file;
is File::Compare::compare($file, 't/re-Graph/~g28.png'), 0, "$file ok";

$g28->add_exit(9);
is join(' ', $g28->exit), '8 9', "multiple accepting nodes";

ok $g28->is_exit(9);
ok $g28->is_exit(8);
ok ! $g28->is_exit(2);
