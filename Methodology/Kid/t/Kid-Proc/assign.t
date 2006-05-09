#: assign.t

use t::Kid_Proc;

plan tests => 1 * blocks();

filters {
    proc => 'unindent',
};

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + abs(x*y)

proc abs(x) {
    if (x>=0) abs:=x
    else      abs:=-x
}
--- proc
{
    _abs_1_x:=x*y;
    {
        if (_abs_1_x>=0)
            _abs_1_abs:=_abs_1_x;
        else
            _abs_1_abs:=-_abs_1_x;

    }
    a:=3+_abs_1_abs;
}



=== TEST 2
--- kid
proc sum(a,b) { sum := a+b }

a := sum(2,3) + abs(x*y)

proc abs(x) {
    if (x>=0) abs:=x
    else      abs:=-x
}
--- proc
{
    _sum_1_a:=2;
    _sum_1_b:=3;
    {
        _sum_1_sum:=_sum_1_a+_sum_1_b;
    }
    {
        _abs_1_x:=x*y;
        {
            if (_abs_1_x>=0)
                _abs_1_abs:=_abs_1_x;
            else
                _abs_1_abs:=-_abs_1_x;
        }
        a:=_sum_1_sum+_abs_1_abs;
    }
}



=== TEST 3
--- kid
b := 5 - 3;
a := 3 + abs(x*abs(y));
c := a / b;

proc abs(x) {
    if (x>=0) abs:=x
    else      abs:=-x
}
--- proc
b:=5-3;
{
    _abs_1_x:=y;
    {
        if (_abs_1_x>=0)
            _abs_1_abs:=_abs_1_x;
        else
            _abs_1_abs:=-_abs_1_x;
    }
    {
        _abs_2_x:=x*_abs_1_abs;
        {
            if (_abs_2_x>=0)
                _abs_2_abs:=_abs_2_x;
            else
                _abs_2_abs:=-_abs_2_x;
        }
        a:=3+_abs_2_abs;
    }
}
c:=a/b;



=== TEST 4
--- kid
a := min(x+6, x+y);

proc min (a, b) {
    if (a<=b) min:=a
    else      min:=b
}

proc abs(x) {
    if (x>=0) abs:=x
    else      abs:=-x
}
--- proc
{
    _min_1_a:=x+6;
    _min_1_b:=x+y;
    {
        if (_min_1_a<=_min_1_b)
            _min_1_min:=_min_1_a;
        else
            _min_1_min:=_min_1_b;
    }
    a:=_min_1_min;
}



=== TEST 5: List assignment
--- kid
a, b := x, y;
--- proc
{
    _kid_la_1:=y;
    a:=x;
    b:=_kid_la_1;
}



=== TEST 6: List assignment
--- kid
x,y,z:=x-y,x+y,x-z;
--- proc
{
    _kid_la_1:=x+y;
    _kid_la_2:=x-z;
    x:=x-y;
    y:=_kid_la_1;
    z:=_kid_la_2;
}



==== TEST 8: List assignments
--- kid
if (x>0) {
    a, b := x, y;
    x,y,z:=x-y,x+y,x-z;
}
--- proc
if (x>0) {
    {
        _kid_la_1:=y;
        a:=x;
        b:=_kid_la_1;
    }
    {
        _kid_la_1:=x+y;
        _kid_la_2:=x-z;
        x:=x-y;
        y:=_kid_la_1;
        z:=_kid_la_2;
    }
}
