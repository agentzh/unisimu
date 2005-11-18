use strict;
use warnings;

use Test::More tests => 91;
use Test::Deep;

my $pack;
BEGIN { $pack = 'Paging::LRU'; use_ok($pack) };

my $lru = $pack->new(mem_size => 8);
ok $lru;
isa_ok $lru, 'Paging';
isa_ok $lru, $pack;

is $lru->mem_size, 8;
cmp_deeply [$lru->queue], [];
for (1..8) {
    ok not $lru->access($_);
    ok not $lru->alloc($_);
    cmp_deeply [$lru->queue], [1..$_];
    ok $lru->access($_);
}

ok $lru->access(1);
cmp_deeply [$lru->queue], [2,3,4,5,6,7,8,1];

ok $lru->access(5);
cmp_deeply [$lru->queue], [2,3,4,6,7,8,1,5];

my $old = $lru->alloc(9);
is $old, 2;
cmp_deeply [$lru->queue], [3,4,6,7,8,1,5,9];

ok not $lru->access(13);
$old = $lru->alloc(13);
is $old, 3;
cmp_deeply [$lru->queue], [4,6,7,8,1,5,9,13];

$lru = $lru->new;
ok $lru;
isa_ok $lru, $pack;
is $lru->mem_size, 4;

sub new_ok {
    my $page = shift;
    ok not $lru->access($page);
    ok not $lru->alloc($page);
    my @queue = $lru->queue;
    is $queue[-1], $page;
}

sub replace_ok {
    my $page = shift;
    my $page2 = shift;
    ok not $lru->access($page);
    my $old = $lru->alloc($page);
    is $old, $page2;
    my $len = $lru->queue;
    is $len, $lru->mem_size;
}

for (1..4) {
    new_ok($_);
}

ok $lru->access(2);
ok $lru->access(1);

replace_ok(5,3);
replace_ok(6,4);

ok $lru->access(2);
ok $lru->access(1);
ok $lru->access(2);

replace_ok(3,5);
replace_ok(7,6);
replace_ok(6,1);

ok $lru->access(3);
ok $lru->access(2);

replace_ok(1,7);

ok $lru->access(2);
ok $lru->access(3);
ok $lru->access(6);

$lru = $lru->new;
my @reqs = qw(1 2 3 4 2 1 5 6 2 1 2 3 7 6 3 2 1 2 3 6);
my $miss = 0;
foreach (@reqs) {
    if (not $lru->access($_)) {
        $lru->alloc($_);
        $miss++;
    }
}
cmp_ok $miss/@reqs, '==', 0.50;
