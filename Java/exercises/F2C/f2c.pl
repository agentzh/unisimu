#!perl
#: f2c.pl

use v6-alpha;

my $F = shift @*ARGS err
    die "Usage: $?FILE <num>\n";
my $C = 5 / 9 * ($F - 32);
say $C;
