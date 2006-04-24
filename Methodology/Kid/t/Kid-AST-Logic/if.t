#: if.t

use t::Kid_AST_Logic;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
if (x > 5) { x:=x+1 }
--- ast_logic
(or (and x>5 x:=x+1) (not x>5))



=== TEST 2
--- kid
if (x + 3*(y - 6.7)<= 4*x/(52.1 - 3) ) {
    y := x-5 + y;
    x := x - y;
}
--- ast_logic
(or (and x+3*(y-6.7)<=4*x/(52.1-3) (and y:=x-5+y x:=x-y)) (not x+3*(y-6.7)<=4*x/(52.1-3)))



=== TEST 3
--- kid
if (5 <> x) { x:= 3; } else {
    y:=x-1; x:=x+1 }
--- ast_logic
(or (and 5<>x x:=3) (and (not 5<>x) (and y:=x-1 x:=x+1)))



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
--- ast_logic
(or (and x>0 (and x:=x+2 (or (and x<y y:=x) (not x<y)))) (and (not x>0) (or (and y+3<x*5 y:=x/2) (and (not y+3<x*5) y:=y+1))))



=== TEST 5
--- kid
if (6.3<= 0.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- ast_logic
(and (or (and 6.3<=0.232 x:=2) (not 6.3<=0.232)) (or (and 5=x_ yylex:=1) (not 5=x_)))
