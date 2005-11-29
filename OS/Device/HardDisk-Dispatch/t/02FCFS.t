#: 02FCFS.t

use strict;
use warnings;

use Test::Deep;
use Test::More tests => 62;

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
is $disp->distance_moved, 0;

foreach (@plan) {
    my $pos = $disp->move_next;
    ok $pos;
    is $pos, $_;
    is $disp->pos, $_;
}

is $disp->distance_moved, 498;

ok not $disp->move_next;

$disp->start;

is $disp->dir, '+';
is $disp->pos, 100;
is $disp->distance_moved, 0;

my $pos = $disp->move_next;
ok $pos;
is $pos, 55;
is $disp->pos, 55;
is $disp->dir, '-';
is $disp->distance_moved, 45;

$pos = $disp->move_next;
ok $pos;
is $pos, 58;
is $disp->pos, 58;
is $disp->dir, '+';
is $disp->distance_moved, 45+3;

$pos = $disp->move_next;
ok $pos;
is $pos, 39;
is $disp->pos, 39;
is $disp->dir, '-';
is $disp->distance_moved, 45+3+19;

$pos = $disp->move_next;
ok $pos;
is $pos, 18;
is $disp->pos, 18;
is $disp->dir, '-';
is $disp->distance_moved, 45+3+19+21;

$pos = $disp->move_next;
ok $pos;
is $pos, 90;
is $disp->pos, 90;
is $disp->dir, '+';
is $disp->distance_moved, 45+3+19+21+72;

#ok not $disp->move_next;
