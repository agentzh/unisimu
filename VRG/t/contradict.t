use t::VRG;

plan tests => 5 * blocks();

run_tests;

__DATA__

=== TEST 1: a T b, a // b
--- vrg

line a, b;

a T b, a//b => a//b

--- ans
Contradiction detected: a <//> b, a <~//> b.



=== TEST 2: indirect conflict
--- vrg

line a, b, c, d;

a T b, b // c, c // a => a // c

--- ans
Contradiction detected: b <//> c, b <~//> c.



=== TEST 3: l on alpha, l // alpha
This test reveals a deep bug in our knowledge base
the temporary plane is problematic.
we should not assume alpha \= beta when alpha and beta are two planes.
--- vrg

line l; plane alpha;
l on alpha, l // alpha => l // alpha

--- ans
Contradiction detected: l [on] alpha, l [~on] alpha.



=== TEST 4:
--- vrg

plane a, b;

a T b, a // b => a T b

--- ans
Contradiction detected: a <//> b, a <~//> b.



=== TEST 5:
--- vrg

line l;
plane alpha;

l // alpha, l X alpha => l // alpha

--- ans
Contradiction detected: l <T> alpha, l <X> alpha.
