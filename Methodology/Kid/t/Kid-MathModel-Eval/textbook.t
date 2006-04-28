#: textbook.t
#: Examples extracted from my Programming
#:   Methodology textbook
#: 2006-04-27 2006-04-27

use t::Kid_MathModel_Eval;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1: Page 180, tracing table
--- kid
x:=x+y; y:=x-y; x:=x-y
--- mathmodel_eval
--
 -
 - x, y := y, x



=== TEST 2: Page 181, polynomial evaluation
--- kid
y:=a
y:=x*y+b
y:=x*y+c
y:=x*y+d
--- mathmodel_eval
--
 -
 - y := x^3*a+x^2*b+x*c+d



=== TEST 3: Page 182, disjoint rules

(x>0 -> (y>0 -> z:=x*y | y<0 -> z:=-x*y) |
 x<0 -> (y>0 -> z:=-x*y | y<0 -> z:=x*y))

--- kid

if (x>0) {
    if (y>0) {
        z:=x*y
    }
    if (y<0) {
        z:=-x*y
    }
}
if (x<0) {
    if (y>0) {
        z:=-x*y
    }
    if (y<0) {
        z:=x*y
    }
}
--- mathmodel_eval
--
 - 0<x, 0<y
 - z := x*y
--
 - 0<x, y<0
 - z := -x*y
--
 - 0<y, x<0
 - z := -x*y
--
 - x<0, y<0
 - z := x*y



=== TEST 4: Page 182, disjoint rules

    (x>0 -> (y>0 -> z:=x*y | y<0 -> z:=-x*y) |
     x<0 -> (y>0 -> z:=-x*y | y<0 -> z:=x*y))

    =>

    (x>0 and y>0 -> z:=x*y  |
     x>0 and y<0 -> z:=-x*y |
     x<0 and y>0 -> z:=-x*y |
     x<0 and y<0 -> z:=x*y)

--- kid

if (x>0) {
    if (y>0)
        z:=x*y;
    else if (y<0)
        z:=-x*y;
} else if (x<0) {
    if (y>0)
        z:=-x*y;
    else if (y<0)
        z:=x*y;
}
--- mathmodel_eval
--
 - 0<x, 0<y
 - z := x*y
--
 - 0<x, y<0
 - z := -x*y
--
 - 0<y, x<0
 - z := -x*y
--
 - x<0, y<0
 - z := x*y



=== TEST 5: Page 182, non-disjoint rules

    (x>0 -> (x>y -> z:=x | x<y -> z:=y) |
     y>0 -> (x<y -> z:=x | x>y -> z:=y))

--- kid

if (x > 0) {
    if (x > y) z := x
    if (x < y) z := y
}
if (y > 0) {
    if (x < y) z := x
    if (x > y) z := y
}

--- mathmodel_eval
--
 - 0<x, 0<y, y-x<0
 - z := y
--
 - 0<x, y-x<0, y<0
 - z := x
--
 - 0<x, 0<y, x-y<0
 - z := x
--
 - 0<y, x-y<0, x<0
 - z := x



=== TEST 6: Page 182, non-disjoint rules

    (x>0 -> (x>y -> z:=x | x<y -> z:=y) |
     y>0 -> (x<y -> z:=x | x>y -> z:=y))

    =>

    (x>0 and x>y -> z:=x | x>0 and x<y -> z:=y |
     x<0 and y>0 and x<y -> z:=x)

--- kid

if (x>0) {
    if (x>y) z:=x
    else if (x<y) z:=y
} else if (y>0) {
    if (x<y) z:=x
    else if (x>y) z:=y
}

--- mathmodel_eval
--
 - 0<x, y-x<0
 - z := x
--
 - 0<x, 0<y, x-y<0
 - z := y
--
 - 0<y, x-y<0, x<0
 - z := x



=== TEST 7: Page 182, non-disjoint rules

  (x>0 -> z:=max(x,y) | y>0 -> z:=min(x,y))

--- kid

if (x > 0) {
    if (x >= y) z := x;
    else        z := y;
} else if (y > 0) {
    if (x <= y) z := x;
    else        z := y;
}
--- mathmodel_eval
--
 - 0<x, y-x<=0
 - z := x
--
 - 0<x, 0<y, x-y<0
 - z := y
--
 - 0<y, x-y<=0, x<0
 - z := x



=== TEST 8: Page 182, non-disjoint rules

  (x>0 -> z:=max(x,y) | y>0 -> z:=min(x,y))

--- kid

if (x > 0) {
    z := max(x, y);
} else if (y > 0) {
    z := min(x, y);
}

proc max(x, y) {
    if (x >= y) max := x;
    else        max := y;
}

proc min(x, y) {
    if (x <= y) min := x;
    else        min := y;
}
--- mathmodel_eval
--
 - 0<x, y-x<=0
 - z := x
--
 - 0<x, 0<y, x-y<0
 - z := y
--
 - 0<y, x-y<=0, x<0
 - z := x
