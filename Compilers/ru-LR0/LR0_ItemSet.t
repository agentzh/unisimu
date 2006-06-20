# LR0_ItemSet.t
# Test LR0_ItemSet.pm

use strict;
use warnings;
use Test::More 'no_plan';
BEGIN { use_ok('LR0_ItemSet'); }
use LR0_Item;

my $set = LR0::ItemSet->new;
ok defined $set, 'obj defined';
isa_ok $set, 'LR0::ItemSet';
is join(' ', $set->items), '';

$set->add_item('dog');
is join(' ', $set->items), 'dog';

$set = LR0::ItemSet->new('A', 'B', 'C');
ok $set;
is join(' ', $set->items), 'A B C';

$set->add_item('clover');
is join(' ', $set->items), 'A B C clover';
$set->add_item('cat', 'mouse');
is join(' ', $set->items), 'A B C clover cat mouse';

$set->add_edge('clover', 'agent');
is join(' ', $set->edges), 'clover agent';
$set->add_edge('xunxin');
is join(' ', $set->edges), 'clover agent xunxin';

my $item1 = LR0::Item->new(A => ['(', 'A', ')'], 1);
my $item2 = LR0::Item->new('term' => ['term', '*', 'factor'], 0);
my $item3 = LR0::Item->new('expr' => ['expr', '+', 'term'], 3);
my $item4 = LR0::Item->new('if' => ['cond', 'stmt'], 2);

$set = LR0::ItemSet->new($item1, $item2, $item3, $item4);
is join(' ', $set->items), "$item1 $item2 $item3 $item4";

my @items = $set->complete_items;
is join(' ', $set->complete_items), "$item3 $item4";

