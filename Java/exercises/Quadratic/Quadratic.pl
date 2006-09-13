# Quadratic.pl

use v6-alpha;

@*ARGS == 3 or
    die "Usage: $?FILE <a> <b> <c>\n";
my ($a, $b, $c) = @*ARGS;

if $a == 0 { die "Error: It's not a quadratic equation.\n" }

my $delta = $b * $b - 4 * $a * $c;
if $delta < 0 { die "Error: No solution found.\n" }

my $p = -$b / (2*$a);
my $q = sqrt($delta) / (2*$a);

my ($x1, $x2) = $p + $q, $p - $q;

say "Solution found: $x1, $x2";
