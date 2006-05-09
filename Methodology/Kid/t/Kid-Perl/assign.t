#: assign.t

use t::Kid_Perl;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- perl
$a=3+5*2;



=== TEST 2
--- kid
a:=1+3-     2 + 5- 6- 8
--- perl
$a=1+3-2+5-6-8;



=== TEST 3
--- kid
d := 5
--- perl
$d=5;



=== TEST 4
--- kid
ebc := (25)
--- perl
$ebc=25;



=== TEST 5
--- kid
a_b := ((25+5123))
--- perl
$a_b=(25+5123);



=== TEST 6
--- kid
e := 3/5
--- perl
$e=3/5;



=== TEST 7
--- kid
e := 3/(a+5)
--- perl
$e=3/($a+5);



=== TEST 8
--- kid
e := 5+(3*6+3/5 ) * bb-3*c
--- perl
$e=5+(3*6+3/5)*$bb-3*$c;



=== TEST 9
--- kid
e := 5*(3-5-(7+3))
--- perl
$e=5*(3-5-(7+3));



=== TEST 10
--- kid
a:=3+2; b:=5*6  ;c:=1*2
--- perl
$a=3+2;
$b=5*6;
$c=1*2;



=== TEST 11
--- kid
x:=-y*z-6; y:=53+(-x/z+5);
--- perl
$x=-$y*$z-6;
$y=53+(-$x/$z+5);




=== TEST 12: List assignment
--- kid
a, b := x, y;
--- perl
($a,$b)=($x,$y);



=== TEST 13: List assignment
--- kid
x,y,z:=x-y,x+y,x-z;
--- perl
($x,$y,$z)=($x-$y,$x+$y,$x-$z);
