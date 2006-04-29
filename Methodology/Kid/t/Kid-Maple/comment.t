#: comment.t

use t::Kid_Maple;

filters {
    maple => 'unindent',
};

plan tests => 1 * blocks();

run_tests;

__DATA__

=== TEST 1
--- kid

# this is a comment
a := 3 + x*y

--- maple
# this is a comment
a:=3+x*y;



=== TEST 2
--- kid

a := 3 + x*y # this is also a comment

--- maple
a:=3+x*y;
# this is also a comment



=== TEST 3
--- kid

if (x>0) { # so does this one!
    a := a + 1;
}

--- maple
if x>0 then
    # so does this one!
    a:=a+1;
end if;
