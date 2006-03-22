#: 00test.pl
#: test script for Tesla.
#: v0.05
#: Agent2002. All rights reserved.
#: 2004-12-04 2006-03-22

use File::Copy;
use Test::More tests => 2;

ok( system( "perl script/tslc.pl -o lib/std.pm lib/std.tm" ) == 0, 'Generate std.pm' );
ok( system( "perl script/tslc.pl -o t/std.t t/std.t.tsl" ) == 0, 'Generate std.t' );

0;
