#: assign.t

use t::Kid_Kid;

plan tests => 1 * blocks();

filters {
    kid_kid => 'unindent',
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
--- kid_kid
a:=3+abs(x*y);
proc abs(x) {
    if (x>=0)
        abs:=x;
    else
        abs:=-x;
}



=== TEST 2
--- kid
a := 3 + abs(x*abs(y))

proc abs(x) {
    if (x>=0) abs:=x
    else      abs:=-x
}
--- kid_kid
a:=3+abs(x*abs(y));
proc abs(x) {
    if (x>=0)
        abs:=x;
    else
        abs:=-x;
}



=== TEST 3
--- kid
d := abs(3 + x) - abs(5);

proc abs(x) {

    if (x>=0) abs:=x
    else      abs:=-x

}
--- kid_kid
d:=abs(3+x)-abs(5);
proc abs(x) {
    if (x>=0)
        abs:=x;
    else
        abs:=-x;
}



=== TEST 4
--- kid
a := min(x+6, x+y);

proc min (a, b) {
    if (a<=b) min:=a
    else      min:=b
}

proc abs (x) {
    if (x>=0) abs:=x
    else      abs:=-x
}
--- kid_kid
a:=min(x+6,x+y);
proc min(a,b) {
    if (a<=b)
        min:=a;
    else
        min:=b;
}
proc abs(x) {
    if (x>=0)
        abs:=x;
    else
        abs:=-x;
}
