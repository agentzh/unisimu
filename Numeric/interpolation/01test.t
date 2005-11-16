#: 01test.t

use strict;
use warnings;

use Test::More tests => 6;
use Test::Deep;
BEGIN { use_ok('Interp::Newton'); }

my $newton = Interp::Newton->new(
    30 => '1/2',
    45 => 'sqrt(2)/2',
    60 => 'sqrt(3)/2',
);
ok $newton;
isa_ok $newton, 'Interp::Newton';

cmp_deeply [$newton->Xs], [30,45,60];
cmp_deeply [$newton->Ys], [qw!1/2 sqrt(2)/2 sqrt(3)/2!];
cmp_deeply [$newton->diff_quot],
  [
    [$newton->Ys],
    [qw!1/30*2^(1/2)-1/30 1/30*3^(1/2)-1/30*2^(1/2)!],
    [qw!1/900*3^(1/2)-1/450*2^(1/2)+1/900!],
  ];
