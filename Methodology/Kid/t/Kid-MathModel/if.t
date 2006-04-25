#: if.t

use t::Kid_MathModel;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
if (x > 5) { x:=x+1 }
--- mathmodel
--
 - x_0
 - x_1
 - x_0>5
 - x_1:=x_0+1
--
 - x_0
 - x_0
 - x_0<=5
 -



=== TEST 2
--- kid
if (x + 3*(y - 6.7)<= 4*x/(52.1 - 3) ) {
    y := x-5 + y;
    x := x - y;
}
--- mathmodel
--
 - x_0, y_0
 - x_1, y_1
 - x_0+3*(y_0-6.7)<=4*x_0/(52.1-3)
 - y_1:=x_0-5+y_0, x_1:=x_0-y_1
--
 - x_0, y_0
 - x_0, y_0
 - x_0+3*(y_0-6.7)>4*x_0/(52.1-3)
 -



=== TEST 3
--- kid
if (5 <> x) { x:= 3; } else {
    y:=x-1; x:=x+1 }
--- mathmodel
--
 - x_0
 - x_1
 - 5<>x_0
 - x_1:=3
--
 - x_0, y_0
 - x_1, y_1
 - 5=x_0
 - y_1:=x_0-1, x_1:=x_0+1



=== TEST 4
--- kid
if (x > 0) {
    x := x + 2;
    if (x < y) {
        y := x;
    }
} else {
    if (y + 3 < x * 5) {
        y := x / 2
    } else {
        y := y + 1
    }
}
--- mathmodel
--
 - x_0, y_0
 - x_1, y_1
 - x_0>0, x_1<y_0
 - x_1:=x_0+2, y_1:=x_1
--
 - x_0, y_0
 - x_1, y_0
 - x_0>0, x_1>=y_0
 - x_1:=x_0+2
--
 - x_0, y_0
 - x_0, y_1
 - x_0<=0, y_0+3<x_0*5
 - y_1:=x_0/2
--
 - x_0, y_0
 - x_0, y_1
 - x_0<=0, y_0+3>=x_0*5
 - y_1:=y_0+1



=== TEST 5
--- kid
if (6.3<= 0.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- mathmodel
--
 - x_0, x__0, yylex_0
 - x_1, x__0, yylex_1
 - 6.3<=0.232, 5=x__0
 - x_1:=2, yylex_1:=1
--
 - x_0, x__0
 - x_1, x__0
 - 6.3<=0.232, 5<>x__0
 - x_1:=2
--
 - x__0, yylex_0
 - x__0, yylex_1
 - 6.3>0.232, 5=x__0
 - yylex_1:=1
--
 - x__0
 - x__0
 - 6.3>0.232, 5<>x__0
 -
