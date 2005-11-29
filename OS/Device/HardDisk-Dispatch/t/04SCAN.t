#: 04SCAN.t

use strict;
use warnings;

use Test::Deep;
use Test::More tests => 83;

my $pack;
BEGIN {
    $pack = 'HardDisk::Dispatch::SCAN';
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

my @hist = qw(150 160 184 90 58 55 39 38 18);

foreach (@hist) {
    my $pos = $disp->move_next;
    ok $pos;
    is $pos, $_;
    is $disp->pos, $_;
}

is $disp->distance_moved, 50+10+24+94+32+3+16+1+20;

ok not $disp->move_next;

$disp->start;

is $disp->dir, '+';
is $disp->pos, 100;
is $disp->distance_moved, 0;

my $pos = $disp->move_next;
ok $pos;
is $pos, 150;
is $disp->pos, 150;
is $disp->dir, '+';
is $disp->distance_moved, 50;

$pos = $disp->move_next;
ok $pos;
is $pos, 160;
is $disp->pos, 160;
is $disp->dir, '+';
is $disp->distance_moved, 50+10;

$pos = $disp->move_next;
ok $pos;
is $pos, 184;
is $disp->pos, 184;
is $disp->dir, '+';
is $disp->distance_moved, 50+10+24;

$pos = $disp->move_next;
ok $pos;
is $pos, 90;
is $disp->pos, 90;
is $disp->dir, '-';
is $disp->distance_moved, 50+10+24+94;

$pos = $disp->move_next;
ok $pos;
is $pos, 58;
is $disp->pos, 58;
is $disp->dir, '-';
is $disp->distance_moved, 50+10+24+94+32;

$pos = $disp->move_next;
ok $pos;
is $pos, 55;
is $disp->pos, 55;
is $disp->dir, '-';
is $disp->distance_moved, 50+10+24+94+32+3;

$pos = $disp->move_next;
ok $pos;
is $pos, 39;
is $disp->pos, 39;
is $disp->dir, '-';
is $disp->distance_moved, 50+10+24+94+32+3+16;

$pos = $disp->move_next;
ok $pos;
is $pos, 38;
is $disp->pos, 38;
is $disp->dir, '-';
is $disp->distance_moved, 50+10+24+94+32+3+16+1;

$pos = $disp->move_next;
ok $pos;
is $pos, 18;
is $disp->pos, 18;
is $disp->dir, '-';
is $disp->distance_moved, 50+10+24+94+32+3+16+1+20;

ok not $disp->move_next;
