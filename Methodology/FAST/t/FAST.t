#: FAST.t
#: Test lib/FAST.pm
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-08 2006-03-08

use strict;
use warnings;

use Test::More tests => 3;

BEGIN { use_ok('FAST'); }

my $g = FAST->new('foo');
ok $g;
isa_ok $g, 'FAST';
