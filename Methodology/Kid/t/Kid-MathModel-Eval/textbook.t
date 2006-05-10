#: textbook.t
#: Examples extracted from my Programming
#:   Methodology textbook
#: 2006-04-27 2006-04-27

use t::Kid_MathModel_Eval;

plan tests => 1 * blocks();

#no_diff;

run_tests;

__DATA__

=== TEST 1: Page 180, tracing table
--- kid
# very simple one

x:=x+y; y:=x-y; x:=x-y
--- mathmodel_eval
--
 -
 - x, y := y, x



=== TEST 2: Page 180, ditto
--- kid
x, y := y, x
--- mathmodel_eval
--
 -
 - x, y := y, x



=== TEST 3: Page 181, polynomial evaluation
--- kid
y:=a
y:=x*y+b
y:=x*y+c
y:=x*y+d
--- mathmodel_eval
--
 -
 - y := x^3*a+x^2*b+x*c+d



=== TEST 4: Page 182, disjoint rules

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



=== TEST 5: Page 182, disjoint rules

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



=== TEST 6: Page 182, non-disjoint rules

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
 - 0<x, 0<y, y<x
 - z := y
--
 - 0<x, y<0, y<x
 - z := x
--
 - 0<x, 0<y, x<y
 - z := x
--
 - 0<y, x<0, x<y
 - z := x



=== TEST 7: Page 182, non-disjoint rules

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
 - 0<x, y<x
 - z := x
--
 - 0<x, 0<y, x<y
 - z := y
--
 - 0<y, x<0, x<y
 - z := x



=== TEST 8: Page 182, non-disjoint rules

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
 - 0<x, y<=x
 - z := x
--
 - 0<x, 0<y, x<y
 - z := y
--
 - 0<y, x<0, x<=y
 - z := x



=== TEST 9: Page 182, non-disjoint rules

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
 - 0<x, y<=x
 - z := x
--
 - 0<x, 0<y, x<y
 - z := y
--
 - 0<y, x<0, x<=y
 - z := x



=== TEST 10: Page 183, Example 6.1

  f=(x:=x)
  P:  if x>0 then x:=x-2*x else x:=x+2*abs(x) fi

--- kid

proc abs (x) {
    if (x>=0) abs:=x;
    else      abs:=-x;
}

if (x>0) x:=x-2*x else x:=x+2*abs(x)

--- mathmodel_eval
--
 - 0<x
 - x := -x
--
 - x=0
 - x := 0
--
 - x<0
 - x := -x



=== TEST 11: Page 184, Example 6.3

  (x>y -> x,y:=x+abs(y),x-abs(y) |
   x<y -> x,y:=y+abs(x),y-abs(x))
 =>
   x,y:=max(x,y)min(x,y)
   x,y:=max(x-y,x+y),min(x-y,x+y)
   x,y:=max(x,y),min(x,y)

--- kid

if (x>y) {
    _a:=x+abs(y); _b:=x-abs(y);
    x:=_a; y:=_b;
} else if (x<y) {
    _a:=y+abs(x); _b:=y-abs(x);
    x:=_a; y:=_b;
}

proc abs (x) {
    if (x>=0) abs:=x;
    else      abs:=-x;
}
--- mathmodel_eval
--
 - 0<x, 0<y, y<x
 - x, y := x+y, x-y
--
 - y<0, y<x
 - x, y := x-y, x+y
--
 - 0<x, 0<y, x<y
 - x, y := x+y, y-x
--
 - x<0, x<y
 - x, y := y-x, x+y



=== TEST 12: Page 184, Example 6.3

  (x>y -> x,y:=x+abs(y),x-abs(y) |
   x<y -> x,y:=y+abs(x),y-abs(x))
 =>
   x,y:=max(x,y)min(x,y)
   x,y:=max(x-y,x+y),min(x-y,x+y)
   x,y:=max(x,y),min(x,y)

--- kid

if (x>y) {
    x,y:=x+abs(y),x-abs(y);
} else if (x<y) {
    x,y:=y+abs(x),y-abs(x);
}

proc abs (x) {
    if (x>=0) abs:=x;
    else      abs:=-x;
}
--- mathmodel_eval
--
 - 0<x, 0<y, y<x
 - x, y := x+y, x-y
--
 - y<0, y<x
 - x, y := x-y, x+y
--
 - 0<x, 0<y, x<y
 - x, y := x+y, y-x
--
 - x<0, x<y
 - x, y := y-x, x+y



=== TEST 13: Page 184, Example 6.3 (ditto)

  (x>y -> x,y:=x+abs(y),x-abs(y) |
   x<y -> x,y:=y+abs(x),y-abs(x))
 =>
   x,y:=max(x,y),min(x,y)
   x,y:=max(x-y,x+y),min(x-y,x+y)
   x,y:=max(x,y),min(x,y)

(this test runs too slowly, so I skip it for now)

--- kid

_temp:=max(x,y); y:=min(x,y);
x:=_temp;

_temp:=max(x-y,x+y); y:=min(x-y,x+y);
x:=_temp;

_temp:=max(x,y); y:=min(x,y);
x:=_temp;

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
 - x=y, y=0
 - x, y := 0, 0
--
 - x<0, y=x
 - x, y := 0, 2*x
--
 - 0<y, x=y
 - x, y := 2*x, 0
--
 - y-x<0, y<0
 - x, y := x-y, x+y
--
 - 0<x, 0<y, y-x<0
 - x, y := x+y, x-y
--
 - x-y<0, x<0
 - x, y := y-x, x+y
--
 - 0<x, 0<y, x<y
 - x, y := x+y, y-x
--- SKIP



=== TEST 14: Page 184, Example 6.3 (ditto)

  (x>y -> x,y:=x+abs(y),x-abs(y) |
   x<y -> x,y:=y+abs(x),y-abs(x))
 =>
   x,y:=max(x,y),min(x,y)
   x,y:=max(x-y,x+y),min(x-y,x+y)
   x,y:=max(x,y),min(x,y)

(this test runs too slowly, so I skip it for now)

--- kid

x,y:=max(x,y),min(x,y)
x,y:=max(x-y,x+y),min(x-y,x+y)
x,y:=max(x,y),min(x,y)

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
 - x=y, y=0
 - x, y := 0, 0
--
 - 0<y, x=y
 - x, y := 2*x, 0
--
 - x=y, y<0
 - x, y := 0, 2*x
--
 - 0<x, 0<y, x<y
 - x, y := y+x, y-x
--
 - x<0, x<y
 - x, y := y-x, y+x
--
 - 0<x, x-y<y+x, y<x
 - x, y := y+x, x-y
--
 - y+x<x-y, y<x
 - x, y := x-y, y+x
--- SKIP



=== TEST 15: Page 190, Problem 3 (2)

    x,y,z:=z,x,y
    x,y,z:=y,z,x
    x,y,z:=z,y,x
    x,y,z:=y,x,z
    x,y,z:=x,z,y

  =>

    x,y,z := y,x,z

--- kid

x,y,z:=z,x,y
x,y,z:=y,z,x
x,y,z:=z,y,x
x,y,z:=y,x,z
x,y,z:=x,z,y

--- mathmodel_eval
--
 -
 - x, y := y, x



=== TEST 16: Page 190, Problem 3 (3)

    if x<0 then y:=x+y else x:=x-y fi
    if y<0 then x:=y+x else y:=y-x fi
    if x+y>0 then x,y:=x-y,y-x fi

--- kid

if (x<0) y:=x+y else x:=x-y
if (y<0) x:=y+x else y:=y-x
if (x+y>0) { _t:=y-x; x:=x-y; y:=_t }

--- mathmodel_eval
--
 - 3*x+2*y<=0, x+y<0, x<0
 - x, y := 2*x+y, x+y
--
 - 0<x+y, 0<y, x<0
 - x, y := x-y, y-x
--
 - 0<x, 0<x+y, y<0
 - x, y := x-y, y-x
--
 - 0<x, 0<y
 - x, y := 2*x-3*y, 3*y-2*x
--
 - 0<=x, y=0
 - x, y := x, -x



=== TEST 17: Page 190, Problem 3 (3)

    if x<0 then y:=x+y else x:=x-y fi
    if y<0 then x:=y+x else y:=y-x fi
    if x+y>0 then x,y:=x-y,y-x fi

--- kid

if (x<0) y:=x+y else x:=x-y
if (y<0) x:=y+x else y:=y-x
if (x+y>0) x,y:=x-y,y-x

--- mathmodel_eval
--
 - 3*x+2*y<=0, x+y<0, x<0
 - x, y := 2*x+y, x+y
--
 - 0<x+y, 0<y, x<0
 - x, y := x-y, y-x
--
 - 0<x, 0<x+y, y<0
 - x, y := x-y, y-x
--
 - 0<x, 0<y
 - x, y := 2*x-3*y, 3*y-2*x
--
 - 0<=x, y=0
 - x, y := x, -x



=== TEST 18: Page 190, Problem 4

    f=(x:=-abs(x))

    P:
        if x>0 then x:=x-2*x fi

--- kid
x:=-abs(x)

proc abs (x) {
    if (x>=0) abs:=x;
    else      abs:=-x;
}
--- mathmodel_eval
--
 - 0<=x
 - x := -x



=== TEST 19: Page 190, Problem 4

    f=(x:=-abs(x))

    P:
        if x>0 then x:=x-2*x fi

--- kid
if (x>0) x:=x-2*x
--- mathmodel_eval
--
 - 0<x
 - x := -x



=== TEST 20:
--- kid
if (x<>0) {
    x,y:=x-1,y+a;
    if (x>0) x,y,a:=0,a*x+y,a;
}
--- mathmodel_eval
--
 - 1<x, x<>0
 - x, y := 0, a*x+y
--
 - x<=1
 - x, y := x-1, y+a



=== TEST 21: Page 183: Example 6.1

Expected function:
    (x>0 -> x,y,a:=0,a*x+y,a)

Program P:
    while x<>0 do x,y=x-1,y+a od

--- kid

if (x>0) {
    if (x<>0) {
        x,y:=x-1,y+a;
        x,y,a:=0,a*x+y,a;
    }
}

--- mathmodel_eval
--
 - 0<x, x<>0
 - x, y := 0, a*x+y



=== TEST 22: Page 183: Exaple 6.1

Expected function:
    (x>0 -> x,y,a:=0,a*x+y,a)

Program P:
    while x<>0 do x,y=x-1,y+a od

--- kid

if (x>0) x,y,a:=0,a*x+y,a;

--- mathmodel_eval
--
 - 0<x
 - x, y := 0, a*x+y
