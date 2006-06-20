# LR0_Edge.t
# Test LR0_Edge.pm
 
use strict;
use warnings;
use Test::More tests => 5;
BEGIN { use_ok('LR0_Edge'); }

my $edge = LR0::Edge->new;
ok defined $edge, 'edge defined';
isa_ok $edge, 'LR0::Edge';

$edge = LR0::Edge->new('A', 1);
is $edge->weight, 'A', "weight == 'A'";
is $edge->next_node, 1, "next_node == 1";
