#: andgate.t
#: Simple test for Tesla v0.05
#: Agent2002. All rights reserved.
#: 04-11-09 04-12-04

use strict;
use warnings;

use Test::More tests => 3;

use Data::Dumper;
BEGIN { use_ok('Tesla'); }

# $Signal::DEBUG = 1;

# -- AND: 0;
$AND::delay = 0.2;

# -- Signal A, B, C;
my $A = Signal->new('A');
my $B = Signal->new('B');
my $C = Signal->new('C');

# -- C <= AND( A, B );
my $gate = AND->new;
$gate->output($C);
$gate->inputs($A,$B);
$A->add_dest($gate);
$B->add_dest($gate);

# -- A << [ 0@0, 1@2.2, 0@6.5, 1@12.2, 1@15 ];
EventConsole->add_events( [0,2.2,6.5,12.2,15], $A, [0,1,0,1,1] );

# -- B << [ 0@0, 1@1, 0@2, 1@3, 0@4, 1@5, 0@6, 1@7, 0@8, 1@9, 0@10, 1@11, 0@12, 1@13, 0@14, 1@15 ];
my @vals;
foreach (1..8) { push @vals, 0, 1; }
EventConsole->add_events( [0..15], $B, \@vals );

# die "events: @EventConsole::event_list\n";
# print Data::Dumper->Dump([\@EventConsole::event_list],[qw(*)]);

# -- run 16
Tesla->run(16);

ok(eq_array( [$C->hist], [0,0.2,1,3.2,0,4.2,1,5.2,0,6.2,1,13.2,0,14.2,1,15.2] ));
is( $C->histp, '0@0.2,1@3.2,0@4.2,1@5.2,0@6.2,1@13.2,0@14.2,1@15.2' );

0;

