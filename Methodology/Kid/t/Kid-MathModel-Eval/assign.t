#: assign.t

use t::Kid_MathModel_Eval;

plan tests => 1 * blocks();

no_diff;

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- mathmodel_eval
--
 -
 - a := 13



=== TEST 2
--- kid
a:=3+2; b:=5*6  ;c:=1*2+a
--- mathmodel_eval
--
 -
 - a, b, c := 5, 30, 7



=== TEST 3
--- kid
a:=a+1; a:=a+a; b:=b+a; c:=c+c+c+a+a+b
--- mathmodel_eval
--
 -
 - a, b, c := 2*a+2, b+2*a+2, 3*c+6*a+6+b
