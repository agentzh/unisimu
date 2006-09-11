#: rand-stats.pl

use v6-alpha;

my ($max, $min, $nbig);

for 1..100 {
    my $e = rand 100;
    $max = $e if !$max.defined or $max < $e;
    $min = $e if !$min.defined or $min > $e;
    $nbig++ if $e > 60;
}
say "Maximum is $max.";
say "Minimum is $min.";
say "Count of nums greater than 60 is $nbig.";
