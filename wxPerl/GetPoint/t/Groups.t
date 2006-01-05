use strict;
use Test::Deep;
use Test::More tests => 33;
BEGIN { use_ok('GetPoint::Groups'); }

my $pack = 'GetPoint::Groups';

my $groups = $pack->new;
ok $groups;
isa_ok $groups, $pack;

ok $groups->AddGroup('Group 1');
ok not $groups->AddGroup('Group 1');
ok $groups->AddItem('Group 1', '1 2');
ok $groups->AddItem('Group 1', '5 6');
#ok $groups->AddItem('5 6');

my @groups = $groups->GetGroups;
cmp_deeply \@groups, ['Group 1'];
#warn "@groups";

my $grps = $groups->GetGroups;
cmp_deeply $grps, ['Group 1'];

my $items = $groups->GetItems('Group 1');
cmp_deeply($items, ['1 2', '5 6']);

my @items = $groups->GetItems('Group 1');
cmp_deeply(\@items, ['1 2', '5 6']);

ok $groups->AddGroup('Group 2');
@groups = $groups->GetGroups;
cmp_deeply(\@groups, bag('Group 1', 'Group 2'));

@items = $groups->GetItems('Group 2');
cmp_deeply(\@items, []);

ok $groups->AddGroup(qw(A B C D));
@groups = $groups->GetGroups;
cmp_deeply \@groups, bag('Group 1', 'Group 2', qw(A B C D));

ok $groups->AddItem('A', qw(1 2 3));
@items = $groups->GetItems('A');
cmp_deeply(\@items, [1,2,3]);

ok $groups->AddItem('MM', qw(32 103 56 78));
@items = $groups->GetItems('MM');
cmp_deeply(\@items, [32, 103, 56, 78]);

@groups = $groups->GetGroups;
cmp_deeply \@groups, bag('Group 1', 'Group 2', qw(A B C D MM));

$groups->RemoveGroup('Group 1');
@groups = $groups->GetGroups;
cmp_deeply \@groups, bag('Group 2', qw(A B C D MM));

$groups->RemoveGroup(qw(A D));
@groups = $groups->GetGroups;
cmp_deeply \@groups, bag('Group 2', qw(B C MM));

$groups->RemoveItem('MM', 32, 78);
@items = $groups->GetItems('MM');
cmp_deeply \@items, [103, 56];

@items = $groups->GetItems('blahblah');
cmp_deeply \@items, [];

ok $groups->SetItem('MM', 1, 156);
@items = $groups->GetItems('MM');
cmp_deeply \@items, [103, 156];

ok not $groups->SetItem('afsdsd', 2, 32);

ok $groups->RenameGroup('MM', 'love');
@items = $groups->GetItems('love');
cmp_deeply \@items, [103, 156];

ok not $groups->RenameGroup('afsdfwewe', '23');
ok not $groups->RenameGroup('love', 'Group 2');

@items = $groups->GetItems('MM');
cmp_deeply \@items, [];
