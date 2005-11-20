use strict;
use warnings;

use Test::More tests => 151;
BEGIN { use_ok('Interp::Lagrange'); }

for (1..50) {
    my $count = int rand(20) + 2;
    my (@xs, @ys);
    my %xs;
    for (1..$count) {
        my $x = int rand(204);
        while (exists $xs{$x}) {
            $x = int rand(204);
        }
        $xs{$x} = 1;
        push @xs, $x;
        push @ys, int rand(204);
    }
    #warn "Xs = @xs\n";
    #warn "Ys = @ys\n";
    my $newton = Interp::Lagrange->new(
        Xs => [@xs],
        Ys => [@ys],
    );
    ok $newton;
    my $poly = $newton->polynomial;
    #warn $poly, "\n";
    ok $poly;
    my $res = $newton->test_polynomial($poly);
    ok $res, "@xs - @ys";
    #warn "  $poly\n";
}
