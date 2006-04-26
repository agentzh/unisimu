#: if.t

use t::Kid_MathModel_Eval;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
if (x > 5) { x:=x+1 }
--- mathmodel_eval
--
 - 5<x
 - x := x+1



=== TEST 2
--- kid
if (x + 3*(y - 6.7)<= 4*x/(52.1 - 3) ) {
    y := x-5 + y;
    x := x - y;
}
--- mathmodel_eval
--
 - .9185336049*x+3.*y-20.10000000<=0.
 - x, y := 5-y, x-5+y



=== TEST 3
--- kid
if (5 <> x) { x:= 3; } else {
    y:=x-1; x:=x+1 }
--- mathmodel_eval
--
 - 5<>x
 - x := 3
--
 - x=5
 - x, y := x+1, x-1



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
--- mathmodel_eval
--
 - 0<x, 2<y, x-y<-2
 - x, y := x+2, x+2
--
 - -2-x+y<=0, 0<x
 - x := x+2
--
 - x<0, y-5*x<-3, y<-3
 - y := 1/2*x
--
 - -y-3+5*x<=0, x<=0
 - y := y+1



=== TEST 5
--- kid
if (6.3<= 0.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- mathmodel_eval
--
 - x_=5.
 - yylex := 1



=== TEST 6
--- kid
if (6.3<= 10.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- mathmodel_eval
--
 - x_=5.
 - x, yylex := 2, 1
--
 - 5<>x_
 - x := 2
