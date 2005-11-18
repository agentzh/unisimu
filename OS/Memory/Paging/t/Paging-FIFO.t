use strict;
use warnings;

use Test::More tests => 97;
use Test::Deep;

my $pack;
BEGIN { $pack = 'Paging::FIFO'; use_ok($pack) };


my $fifo = $pack->new(mem_size => 8);
ok $fifo;
isa_ok $fifo, 'Paging';
isa_ok $fifo, $pack;

is $fifo->mem_size, 8;
cmp_deeply [$fifo->queue], [];
for (1..8) {
    ok not $fifo->access($_);
    ok not $fifo->alloc($_);
    cmp_deeply [$fifo->queue], [1..$_];
    ok $fifo->access($_);
}

ok $fifo->access(3);
ok $fifo->access(1);
my $old = $fifo->alloc(9);
is $old, 1;
cmp_deeply [$fifo->queue], [2..9];
ok not $fifo->access(13);
$old = $fifo->alloc(13);
is $old, 2;
cmp_deeply [$fifo->queue], [3..9, 13];

$fifo = $fifo->new;
ok $fifo;
isa_ok $fifo, $pack;
is $fifo->mem_size, 4;

sub new_ok {
    my $page = shift;
    ok not $fifo->access($page);
    ok not $fifo->alloc($page);
    my @queue = $fifo->queue;
    is $queue[-1], $page;
}

sub replace_ok {
    my $page = shift;
    my $page2 = shift;
    ok not $fifo->access($page);
    my $old = $fifo->alloc($page);
    is $old, $page2;
    my $len = $fifo->queue;
    is $len, $fifo->mem_size;
}

for (1..4) {
    new_ok($_);
}
ok $fifo->access(2);
ok $fifo->access(1);
$old = 1;
for (5,6,2,1) {
    replace_ok($_, $old++);
}
ok $fifo->access(2);
replace_ok(3,5);
replace_ok(7,6);
replace_ok(6,2);
ok $fifo->access(3);
replace_ok(2,1);
replace_ok(1,3);
ok $fifo->access(2);
replace_ok(3,7);
ok $fifo->access(6);

$fifo = $fifo->new;
my @reqs = qw(1 2 3 4 2 1 5 6 2 1 2 3 7 6 3 2 1 2 3 6);
my $miss = 0;
foreach (@reqs) {
    if (not $fifo->access($_)) {
        $fifo->alloc($_);
        $miss++;
    }
}
cmp_ok $miss/@reqs, '==', 0.70;
