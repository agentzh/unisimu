#: 01test.t

use strict;
use warnings;
use Data::Dumper;
use List::Util 'shuffle';

use Test::More tests => 168;
use Test::Deep;
#use Time::HiRes 'sleep';
BEGIN { use_ok('Interp::Newton::Diffs'); }

my $newton = Interp::Newton::Diffs->new(
    30 => '1/2',
    45 => 'sqrt(2)/2',
    60 => 'sqrt(3)/2',
);
ok $newton;
isa_ok $newton, 'Interp::Newton::Diffs';

my @Xs = (30,45,60);
my @Ys = qw!1/2 sqrt(2)/2 sqrt(3)/2!;
cmp_deeply [$newton->Xs], [@Xs];
cmp_deeply [$newton->Ys], [@Ys];
cmp_deeply [$newton->diffs],
  [
    [$newton->Ys],
    [qw!1/2*2^(1/2)-1/2 1/2*3^(1/2)-1/2*2^(1/2)!],
    [qw!1/2*3^(1/2)-2^(1/2)+1/2!],
  ];
#warn Data::Dumper->Dump([$newton->diffs], [qw(diffs)]);

$newton = Interp::Newton::Diffs->new(
    Xs => [30, 45, 60],
    Ys => ['1/2', 'sqrt(2)/2', 'sqrt(3)/2'],
);
ok $newton;
isa_ok $newton, 'Interp::Newton::Diffs';

cmp_deeply [$newton->Xs], [30,45,60];
cmp_deeply [$newton->Ys], [qw!1/2 sqrt(2)/2 sqrt(3)/2!];
cmp_deeply [$newton->diffs],
  [
    [$newton->Ys],
    [qw!1/2*2^(1/2)-1/2 1/2*3^(1/2)-1/2*2^(1/2)!],
    [qw!1/2*3^(1/2)-2^(1/2)+1/2!],
  ];

my $maple = $newton->maple();
ok $maple;
#warn $newton->polynomial;
my $poly = $newton->polynomial or die $newton->error;
ok $poly;
#warn $poly;
foreach (0..@Xs-1) {
    is $maple->eval("testeq(eval($poly, x=$Xs[$_]), $Ys[$_])"), 'true';
}

ok $newton->test_polynomial($poly);
(my $poly2 = $poly) =~ s/2/3/;
ok !$newton->test_polynomial($poly2);
my $s = $maple->eval("$poly");
die $newton->error() unless defined $s;
print "\n*$s*\n";

for (1..50) {
    #sleep(0.1);
    my $count = int rand(20) + 2;
    my (@xs, @ys);
    my $x;
    my $h = (int rand(15)) + 1;
    for (1..$count) {
        if (not defined $x) {
            $x = int rand(204);
        } else {
            $x += $h;
        }
        push @xs, $x;
        push @ys, int rand(204);
    }
    #@xs = shuffle @xs;
    #warn "Xs = @xs\n";
    #warn "Ys = @ys\n";
    my $newton = Interp::Newton::Diffs->new(
        Xs => [@xs],
        Ys => [@ys],
    );
    ok $newton, "@xs - @ys";
    warn $newton->error if $newton->error;
    $poly = $newton->polynomial;
    #warn $poly, "\n";
    ok $poly;
    my $res = $newton->test_polynomial($poly);
    ok $res, "@xs - @ys";
    #warn "  $poly\n";
}
