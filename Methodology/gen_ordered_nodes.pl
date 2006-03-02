#: gen_ordered_nodes.pl
#: Generate ordered_nodes.dat for binary_tree_set.t
#: Copyright (c) 2006 Agent Zhang
#: 2006-02-28 2006-02-28

use strict;
use warnings;

my $MAX_LEVELS = shift || 11;
my %tree;

gen_tree(1);
my @nodes = sort { $a <=> $b } keys %tree;
print "@nodes\n";

sub gen_tree {
    my ($x, $level) = @_;
    $level ||= 0;
    return $x if $level >= $MAX_LEVELS;
    $tree{$x} = [
        gen_tree(2*$x+1, $level+1),
        gen_tree(3*$x+1, $level+1),
    ];
    return $x;
}
