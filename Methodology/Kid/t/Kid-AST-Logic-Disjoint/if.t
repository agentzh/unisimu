#: if.t

use t::Kid_AST_Logic_Disjoint;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
if (x > 5) { x:=x+1 }
--- ast_logic_disjoint
(or
(and x>5 x:=x+1)
(and x<=5)
)



=== TEST 2
--- kid
if (x + 3*(y - 6.7)<= 4*x/(52.1 - 3) ) {
    y := x-5 + y;
    x := x - y;
}
--- ast_logic_disjoint
(or
(and x+3*(y-6.7)<=4*x/(52.1-3) y:=x-5+y x:=x-y)
(and x+3*(y-6.7)>4*x/(52.1-3))
)



=== TEST 3
--- kid
if (5 <> x) { x:= 3; } else {
    y:=x-1; x:=x+1 }
--- ast_logic_disjoint
(or
(and 5<>x x:=3)
(and 5=x y:=x-1 x:=x+1)
)



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
--- ast_logic_disjoint
(or
(and x>0 x:=x+2 x<y y:=x)
(and x>0 x:=x+2 x>=y)
(and x<=0 y+3<x*5 y:=x/2)
(and x<=0 y+3>=x*5 y:=y+1)
)



=== TEST 5
--- kid
if (6.3<= 0.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- ast_logic_disjoint
(or
(and 6.3<=0.232 x:=2 5=x_ yylex:=1)
(and 6.3<=0.232 x:=2 5<>x_)
(and 6.3>0.232 5=x_ yylex:=1)
(and 6.3>0.232 5<>x_)
)
