use strict;
use warnings;

use Test::More tests => 5;
use Test::Deep;
BEGIN { use_ok('Paging') };

# Test the base class Paging:
my $obj = Paging->new(mem_size=>8);
ok $obj;
isa_ok $obj, 'Paging';
is $obj->mem_size, 8;
cmp_deeply [$obj->queue], [];
