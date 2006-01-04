use strict;
use Test::Deep;
use Test::More tests => 24;
BEGIN { use_ok('GetPoint::Groups'); }

my $pack = 'GetPoint::Groups';

my $groups = $pack->new;
ok $groups;
isa_ok $groups, $pack;

ok $groups->addGroup('Group 1');
ok not $groups->addGroup('Group 1');
ok $groups->addItem('Group 1', '1 2');
ok $groups->addItem('Group 1', '5 6');
#ok $groups->addItem('5 6');

my @groups = $groups->getGroups;
cmp_deeply \@groups, ['Group 1'];
#warn "@groups";

my $grps = $groups->getGroups;
cmp_deeply $grps, ['Group 1'];

my $items = $groups->getItems('Group 1');
cmp_deeply($items, ['1 2', '5 6']);

my @items = $groups->getItems('Group 1');
cmp_deeply(\@items, ['1 2', '5 6']);

ok $groups->addGroup('Group 2');
@groups = $groups->getGroups;
cmp_deeply(\@groups, bag('Group 1', 'Group 2'));

@items = $groups->getItems('Group 2');
cmp_deeply(\@items, []);

ok $groups->addGroup(qw(A B C D));
@groups = $groups->getGroups;
cmp_deeply \@groups, bag('Group 1', 'Group 2', qw(A B C D));

ok $groups->addItem('A', qw(1 2 3));
@items = $groups->getItems('A');
cmp_deeply(\@items, [1,2,3]);

ok $groups->addItem('MM', qw(32 103 56 78));
@items = $groups->getItems('MM');
cmp_deeply(\@items, [32, 103, 56, 78]);

@groups = $groups->getGroups;
cmp_deeply \@groups, bag('Group 1', 'Group 2', qw(A B C D MM));

$groups->removeGroup('Group 1');
@groups = $groups->getGroups;
cmp_deeply \@groups, bag('Group 2', qw(A B C D MM));

$groups->removeGroup(qw(A D));
@groups = $groups->getGroups;
cmp_deeply \@groups, bag('Group 2', qw(B C MM));

$groups->removeItem('MM', 32, 78);
@items = $groups->getItems('MM');
cmp_deeply \@items, [103, 56];
