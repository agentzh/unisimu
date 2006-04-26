#: assign.t

use t::Kid_Logic;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- logic
a:=3+5*2



=== TEST 2
--- kid
a:=3+2; b:=5*6  ;c:=1*2
--- logic
(and (and a:=3+2 b:=5*6) c:=1*2)



=== TEST 11
--- kid
x:=-y*z-6; y:=53+(-x/z+5);
--- logic
(and x:=-y*z-6 y:=53+(-x/z+5))
