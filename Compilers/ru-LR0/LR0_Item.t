# LR0_Item.t
# Test LR0_Item.pm

use strict;
use warnings;
use Test::More tests => 33;
BEGIN { use_ok('LR0_Item'); }

my $item = LR0::Item->new;
ok defined $item, 'item defined';
isa_ok $item, 'LR0::Item';
is $item->lhs, undef;
is join(' ', $item->rhs), '';
is $item->pos, 0;

$item = LR0::Item->new('A', ['(', 'A', ')'], 0);
is $item->lhs, 'A', "lhs == 'A'";

ok ! $item->is_complete;

is join(' ', $item->rhs), '( A )', 'rhs ok';

is $item->pos, 0, 'pos is 0';

$item = LR0::Item->new('term' => ['term', "'+'", 'factor'], 1);
is $item->lhs, 'term';
is join(' ', $item->rhs), "term '+' factor";
is $item->pos, 1;

my $item2 = $item->next_item;
ok defined $item2, 'next item defined';
isa_ok $item2, 'LR0::Item';
isnt $item2, $item, 'next_item creates a new obj';
is $item2->lhs, 'term';
is join(' ', $item->rhs), "term '+' factor";
is $item->pos, 1;
is $item2->pos, 2;

$item2 = $item2->next_item;
ok defined $item2;
is $item2->lhs, 'term';
is $item2->pos, 3;

ok $item2->is_complete, "now it's a complete item";

$item2 = $item2->next_item;
ok !defined $item2, "complete item's next item should be undef";

my $w;
($item2, $w) = $item->next_item;
ok defined $item2, 'next item defined';
isa_ok $item2, 'LR0::Item';
isnt $item2, $item, 'next_item creates a new obj';
is $item2->lhs, 'term';
is join(' ', $item->rhs), "term '+' factor";
is $item->pos, 1;
is $item2->pos, 2;
is $w, "'+'", "weight is ok";

