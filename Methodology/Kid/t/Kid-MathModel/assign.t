#: assign.t

use t::Kid_MathModel;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- mathmodel
--
 - a_0
 - a_1
 -
 - a_1:=3+5*2



=== TEST 2
--- kid
a:=3+2; b:=5*6  ;c:=1*2+a
--- mathmodel
--
 - a_0, b_0, c_0
 - a_1, b_1, c_1
 -
 - a_1:=3+2, b_1:=5*6, c_1:=1*2+a_1



=== TEST 3
--- kid
a:=a+1; a:=a+a; b:=b+a; c:=c+c+c+a+a+b
--- mathmodel
--
 - a_0, b_0, c_0
 - a_2, b_1, c_1
 -
 - a_1:=a_0+1, a_2:=a_1+a_1, b_1:=b_0+a_2, c_1:=c_0+c_0+c_0+a_2+a_2+b_1



=== TEST 4
--- kid
x:=-y*z-6; y:=53+(-x/z+5);
--- mathmodel
--
 - x_0, y_0, z_0
 - x_1, y_1, z_0
 -
 - x_1:=-y_0*z_0-6, y_1:=53+(-x_1/z_0+5)
