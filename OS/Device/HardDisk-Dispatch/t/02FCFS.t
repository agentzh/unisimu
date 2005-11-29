#: 02FCFS.t

use strict;
use warnings;

use Test::Deep;
use Test::More tests => 23;

my $pack;
BEGIN {
    $pack = 'HardDisk::Dispatch::FCFS';
    use_ok($pack);
}

my @plan = qw(55 58 39 18 90 160 150 38 184);

my $disp = $pack->new(
    pos => 100,
    dir => '+',
    plan => join(' ', @plan),
);

cmp_deeply [$disp->plan], \@plan;
is $disp->dir, '+';
is $disp->pos, 100;

foreach (@plan) {
    my $pos = $disp->move_next;
    ok $pos;
    is $pos, $_;
}

ok not $disp->move_next;
