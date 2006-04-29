#: comment.t

use t::Kid_Proc;

filters {
    proc => 'unindent',
};

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid

# this is a comment
a := 3 + x*y

--- proc
a:=3+x*y;



=== TEST 2
--- kid

a := 3 + x*y # this is also a comment

--- proc
a:=3+x*y;



=== TEST 3
--- kid

if (x>0) { # so does this one!
    a := a + 1;
}

--- proc
if (x>0) {
    a:=a+1;
}
