use t::VRG;

plan tests => 5 * blocks();

run_tests;

__DATA__

=== TEST 1: a T b, a // b
--- vrg

line a, b;

a T b, a//b => a//b

--- ans
Contradiction detected. (Check the relationships between a and b.)



=== TEST 2: indirect conflict
--- vrg

line a, b, c, d;

a T b, b // c, c // a => a // c

--- ans
Contradiction detected. (Check the relationships between b and c.)



=== TEST 3: l on alpha, l // alpha
This test reveals a deep bug in our knowledge base
the temporary plane is problematic.
we should not assume alpha \= beta when alpha and beta are two planes.
--- vrg

line l; plane alpha;
l on alpha, l // alpha => l // alpha

--- ans
Contradiction detected. (Check the relationships between l and alpha.)
--- SKIP
