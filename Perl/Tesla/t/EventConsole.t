#: EventConsole.t
#: Test script for EventConsole.pm
#: Tesla v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-09

use strict;
use warnings;
use Test::More tests => 19;
BEGIN { use_ok('EventConsole'); }

sub checklist {
    my ($pack, $file, $line) = caller;
    ok(eq_array( [@EventConsole::event_list], [@_] ), "-- ln $line");
}

my $class = 'EventConsole';
checklist();
ok(!defined $class->time_of_next_event);

$class->add_event( 0.2, 'A', 2.7);
checklist( [0.2, 'A', 2.7] );
is( $class->time_of_next_event, 0.2 );

$class->add_event( 0.2, 'B', 3.8 );
checklist( [0.2, 'A', 2.7], [0.2, 'B', 3.8] );
is( $class->time_of_next_event, 0.2 );

$class->add_event( 0.2, 'A', 1.2 );
checklist( [0.2, 'A', 2.7], [0.2, 'B', 3.8], [0.2, 'A', 1.2] );
is( $class->time_of_next_event, 0.2 );

$class->add_event( 0.1, 'C', 4.2 );
checklist( [0.1, 'C', 4.2], [0.2, 'A', 2.7],
    [0.2, 'B', 3.8], [0.2, 'A', 1.2] );
is( $class->time_of_next_event, 0.1 );

$class->add_event( 0.15, 'D', 5.1 );
checklist( [0.1, 'C', 4.2], [0.15, 'D', 5.1], [0.2, 'A', 2.7],
    [0.2, 'B', 3.8], [0.2, 'A', 1.2] );
is( $class->time_of_next_event, 0.1 );

$class->add_event( 0.21, 'B', 2 );
checklist( [0.1, 'C', 4.2], [0.15, 'D', 5.1], [0.2, 'A', 2.7],
    [0.2, 'B', 3.8], [0.2, 'A', 1.2], [0.21, 'B', 2] );
is( $class->time_of_next_event, 0.1 );

$class->reset;
checklist();

$class->add_events( [0,2.2,6.5,12.2,15], 'A', [0,1,0,1,1] );
my @vals;
foreach (1..8) { push @vals, 0, 1; }
$class->add_events( [0..15], 'B', \@vals ); 
checklist( [0,'A',0],
    [0,'B',0],
    [1,'B',1],
    [2,'B',0], [2.2,'A',1],
    [3,'B',1],
    [4,'B',0],
    [5,'B',1],
    [6,'B',0], [6.5,'A',0],
    [7,'B',1],
    [8,'B',0],
    [9,'B',1],
    [10,'B',0],
    [11,'B',1],
    [12,'B',0], [12.2,'A',1],
    [13,'B',1],
    [14,'B',0], [15,'A',1],
    [15,'B',1]
);
is( $class->time_of_next_event, 0 );

$class->reset;
checklist();

0;

