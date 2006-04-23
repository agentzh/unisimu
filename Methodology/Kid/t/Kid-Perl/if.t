#: if.t

use strict;
use warnings;

use t::Kid_Perl;

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid
if (x > 5) { x:=x+1 }
--- perl
if($x>5){
$x=$x+1;
}



=== TEST 2
--- kid
if (x + 3*(y - 6.7)<= 4*x/(52.1 - 3) ) {
    y := x-5 + y;
    x := x - y;
}
--- perl
if($x+3*($y-6.7)<=4*$x/(52.1-3)){
$y=$x-5+$y;
$x=$x-$y;
}



=== TEST 3
--- kid
if (5>x) { x:= 3; } else {
    y:=x-1; x:=x+1 }
--- perl
if(5>$x){
$x=3;
}
else{
$y=$x-1;
$x=$x+1;
}




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
--- perl
if($x>0){
$x=$x+2;
if($x<$y){
$y=$x;
}
}
else{
if($y+3<$x*5){
$y=$x/2;
}
else{
$y=$y+1;
}
}



=== TEST 5
--- kid
if (6.3<= 0.232) {
    x:=2;
}
if (5 = x_) { yylex:=1 }
--- perl
if(6.3<=0.232){
$x=2;
}
if(5=$x_){
$yylex=1;
}
