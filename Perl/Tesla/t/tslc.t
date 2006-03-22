#: tslc.t
#: Test script for tslc.pl
#: Telsa v0.05
#: Agent2002. All rights reserved.
#: 2004-11-10 2005-03-22

use strict;
use warnings;
use Test::More tests => 11;

is( system('perl script/tslc.pl -o t/simple.pl t/simple.tsl'), 0 );

my $in;
ok(open($in,'perl t/simple.pl|'));

undef $/;
is( <$in>, <<'_EOC_' );
C : 0@0.2,1@1.2,0@2.2,1@2.4,0@6.7,1@7.2,0@8.2,1@9.2,0@10.2,1@11.2,0@12.2,1@12.4

X : 0@0,1@1,0@2,1@3,0@4,1@5

_EOC_

close $in;

is( system('perl script/tslc.pl -o t/comp.pl t/comp.tsl'), 0 );

ok(open($in,'perl t/comp.pl|'));

undef $/;
is( <$in>, <<'_EOC_' );
C : 0@1.5,1@13,0@31.5
D : 1@0,0@11.5,1@33

C : 0@1.5,1@83
D : 1@0,0@81.5

G : 0@1.5,1@13,0@31.5
H : 1@0,0@11.5,1@33

_EOC_

close $in;

is( system('perl script/tslc.pl -o t/SR_latch.pl t/SR_latch.tsl'), 0 );

ok(open($in,'perl t/SR_latch.pl|'));

undef $/;
is( <$in>, <<'_EOC_' );
Q : 0@0,1@13,0@31.5
Qp : 1@1.5,0@11.5,1@33

Q : 0@1.5,1@83
Qp : 1@0,0@81.5

C : 0@1.5,1@13,0@31.5
D : 1@0,0@11.5,1@33

C : 0@1.5,1@13,0@31.5
D : 1@0,0@11.5,1@33

_EOC_

close $in;

is( system("perl script/tslc.pl -o lib/std.pm lib/std.tm"), 0 );
is( system("perl script/tslc.pl -o t/std.t t/std.t.tsl"), 0 );
# system( "perl std.t" );

0;
