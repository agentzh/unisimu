use t::VRG;

plan tests => 5 * blocks();

run_tests;

__DATA__

=== TEST 1: the auxiliary plane problem
--- vrg

line a, b; plane alpha;
point P;
a on alpha, b on alpha, meet(a, b, P) => a // alpha

--- ans
No.
Hint: a [~X] alpha, a [~T] alpha, a [on] alpha
