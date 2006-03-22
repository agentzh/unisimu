#: Clock.t
#: Test script forr Clock.pm
#: Tesla v0.05
#: Agent2002. All rights reserved.
#: 04-11-08 04-11-09

use strict;
use warnings;
use Test::More tests => 5;
BEGIN { use_ok('Clock'); }

is( Clock->reading, 0 );

Clock->push_to(3.2);
is( Clock->reading, 3.2 );

Clock->push_to(4.5);
is( Clock->reading, 4.5 );

Clock->reset;
is( Clock->reading, 0 );

0;

