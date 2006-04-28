#: if.t

use t::Kid_Proc;

plan tests => 1 * blocks();

filters {
    proc => 'unindent',
};

run_tests;

__DATA__

=== TEST 1
--- kid
proc foo (x) {
    foo:=x+1;
}

if (x > 5) { x:=foo(x) }

--- proc
if (x>5) {
    {
        _foo_1_x:=x;
        {
            _foo_1_foo:=_foo_1_x+1;
        }
        x:=_foo_1_foo;
    }
}



=== TEST 2
--- kid

proc bar (y) {
    bar := y - 6.7;
}

proc baz (a) {
    baz := 52.1 * a - 3;
}

if (1=1) {
    if (x + 3*bar(y) <= 4*x / baz(3) ) {
        y := x-5 + y;
        x := x - y;
    }
}
--- proc
if (1=1) {
    {
        _bar_1_y:=y;
        {
            _bar_1_bar:=_bar_1_y-6.7;
        }
        {
            _baz_1_a:=3;
            {
                _baz_1_baz:=52.1*_baz_1_a-3;
            }
            if (x+3*_bar_1_bar<=4*x/_baz_1_baz) {
                y:=x-5+y;
                x:=x-y;
            }
        }
    }
}



=== TEST 3
--- kid
if (5 <> x) { x:= 3; } else {
    y := dec(x); x := dec(x) }

proc inc (x) {
    inc := x + 1;
}

proc dec (x) {
    dec := x - 1;
}

--- proc
if (5<>x) {
    x:=3;
}
else {
    {
        _dec_1_x:=x;
        {
            _dec_1_dec:=_dec_1_x-1;
        }
        y:=_dec_1_dec;
    }
    {
        _dec_2_x:=x;
        {
            _dec_2_dec:=_dec_2_x-1;
        }
        x:=_dec_2_dec;
    }
}

=== TEST 3
--- kid
if (5 <> x) { x:= 3; } else y := dec(x);

proc inc (x) {
    inc := x + 1;
}

proc dec (x) {
    dec := x - 1;
}

--- proc
if (5<>x) {
    x:=3;
}
else {
    _dec_1_x:=x;
    {
        _dec_1_dec:=_dec_1_x-1;
    }
    y:=_dec_1_dec;
}
