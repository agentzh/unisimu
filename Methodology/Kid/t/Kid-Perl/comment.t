#: comment.t

use t::Kid_Perl;

plan tests => 1 * blocks();

filters {
    perl => 'unindent',
};

run_tests;

__DATA__

=== TEST 1
--- kid

# this is a comment
a := 3 + x*y

--- perl
# this is a comment
$a=3+$x*$y;



=== TEST 2
--- kid

a := 3 + x*y # this is also a comment

--- perl
$a=3+$x*$y;
# this is also a comment



=== TEST 3
--- kid

if (x>0) { # so does this one!
    a := a + 1;
}

--- perl
if($x>0){
    # so does this one!
    $a=$a+1;
}
