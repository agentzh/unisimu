#: assign.t

use t::Kid_Logic_Disjoint;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- logic_disjoint
(or
(and a:=3+5*2)
)



=== TEST 2
--- kid
a:=3+2; b:=5*6  ;c:=1*2
--- logic_disjoint
(or
(and a:=3+2 b:=5*6 c:=1*2)
)
