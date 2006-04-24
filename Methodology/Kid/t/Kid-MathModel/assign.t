#: assign.t

use t::Kid_MathModel;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- mathmodel
{ a_1=3+5*2 };



=== TEST 2
--- kid
a:=3+2; b:=5*6  ;c:=1*2+a
--- mathmodel
{ a_1=3+2, b_1=5*6, c_1=1*2+a_1 };
