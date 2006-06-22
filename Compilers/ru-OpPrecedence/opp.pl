use strict;
use warnings;
use opp;

$| = 1;
print "Input: ";
while (my $expr = <STDIN>) {
    chomp $expr;
    last if $expr eq 'quit' or $expr eq 'exit' or !$expr;
    $X::str = $expr;
    $X::pos = 0;
    print "\t", parse, "\n";
    print "Input: ";
}
