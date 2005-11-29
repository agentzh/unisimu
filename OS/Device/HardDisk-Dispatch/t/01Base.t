# 01Base.t

use strict;
use warnings;
use Data::Dumper;

use Test::Deep;
use Test::More tests => 5;

my $pack;
BEGIN {
    $pack = 'HardDisk::Dispatch';
    use_ok($pack);
}

my @plan = qw(55 58 39 18 90 160 150 38 184);

my $disp = $pack->new(
    pos => 100,
    dir => '+',
    plan => join(' ', @plan),
);

my @layout = (@plan, 100);
@layout = reverse sort { $a <=> $b } @layout;
cmp_deeply [$disp->plan], \@plan;
cmp_deeply $disp->{layout}, [@layout];

#warn Data::Dumper->Dump([$disp->{layout}], [qw(layout)]);

is $disp->dir, '+';
is $disp->pos, 100;
