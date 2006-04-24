#: assign.t

use t::Kid_Maple;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- maple
a:=3+5*2;



=== TEST 2
--- kid
a:=1+3-     2 + 5- 6- 8
--- maple
a:=1+3-2+5-6-8;



=== TEST 3
--- kid
d := 5
--- maple
d:=5;



=== TEST 4
--- kid
ebc := (25)
--- maple
ebc:=25;



=== TEST 5
--- kid
a_b := ((25+5123))
--- maple
a_b:=(25+5123);



=== TEST 6
--- kid
e := 3/5
--- maple
e:=3/5;



=== TEST 7
--- kid
e := 3/(a+5)
--- maple
e:=3/(a+5);



=== TEST 8
--- kid
e := 5+(3*6+3/5 ) * bb-3*c
--- maple
e:=5+(3*6+3/5)*bb-3*c;



=== TEST 9
--- kid
e := 5*(3-5-(7+3))
--- maple
e:=5*(3-5-(7+3));



=== TEST 10
--- kid
a:=3+2; b:=5*6  ;c:=1*2
--- maple
a:=3+2;
b:=5*6;
c:=1*2;
