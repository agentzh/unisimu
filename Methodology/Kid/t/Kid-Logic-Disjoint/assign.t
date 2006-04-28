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



=== TEST 3
--- kid
x:=-y*z-6; y:=53+(-x/z+5);
--- logic_disjoint
(or
(and x:=-y*z-6 y:=53+(-x/z+5))
)



=== TEST 4
--- kid
proc foo(x) {
    foo:=x+1;
}

y:=3*foo(x+1)-1;

--- logic_disjoint
(or
(and _foo_1_x:=x+1 _foo_1_foo:=_foo_1_x+1 y:=3*_foo_1_foo-1)
)
