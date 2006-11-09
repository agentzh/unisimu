use t::VRG;

plan tests => 9 * blocks() - 8;

run_tests();

__DATA__

=== TEST 1: a <T> b, a <T> c =\=> b <//> c
若直线 l // 平面 alpha, 则 l 平行于 alpha 内的所有直线吗？

--- vrg
line l, m;
plane alpha;
l // alpha, m on alpha => l // m;

--- vectorize
l <T> alpha
m <T> alpha

--- ans
No.



=== TEST 2: ditto, but we have "alpha // l" instead of "l // alpha"
若直线 l // 平面 alpha, 则 l 平行于 alpha 内的所有直线吗？

--- vrg
line l, m;
plane alpha;
alpha // l, m on alpha => l // m;

--- vectorize
l <T> alpha
m <T> alpha

--- ans
No.



=== TEST 3:
若直线 l // 平面 alpha, 则 l 平行于过 l 的平面与 alpha 的交线吗？

--- vrg
line l, m;
plane alpha, theta;

l // alpha, l on theta, meet(theta, alpha, m)
=> l // m;

--- vectorize
l <T> alpha
l <T> theta
theta <~//> alpha
m <T> theta
m <T> alpha

--- eval
l <//> m

--- antivec
l [//] m

--- ans
Yes.



=== TEST 4: \a, #alpha, a <T> alpha =\=> a [//] alpha (form 1)
设 alpha、beta 表示平面，a、b 表示直线，则 a // alpha 的一个充分条件
是不是 alpha T beta, 且 a T beta ?

--- vrg
plane alpha, beta;
line a, b;

alpha T beta, a T beta => a // alpha

--- vectorize
alpha <T> beta
a <//> beta

--- eval
a <T> alpha

--- antivec
a [~T] alpha
a [~X] alpha

--- ans
No.



=== TEST 5: \a, #alpha, a <T> alpha =\=> a [//] alpha (form 2)
设 alpha、beta 表示平面，a、b 表示直线，则 a // alpha 的一个充分条件
是不是 alpha 交 beta 于 b, 且 a // b ?

--- vrg
plane alpha, beta;
line a, b;

meet(alpha, beta, b), a // b => a // alpha;

--- vectorize
b <T> alpha
b <T> beta
alpha <~//> beta
a <//> b

--- eval
a <T> alpha

--- antivec
a [~T] alpha
a [~X] alpha

--- ans
No.



=== TEST 6: \a, #alpha, a <T> alpha =\=> a [//] alpha (form 3)
设 alpha、beta 表示平面，a、b 表示直线，则 a // alpha 的一个充分条件
是不是 a // b, 且 b // alpha ?
--- vrg

plane alpha, beta;
line a, b;

a // b, b // alpha => a // alpha

--- vectorize
a <//> b
b <T> alpha

--- eval
a <T> alpha

--- antivec
a [~X] alpha
a [~T] alpha

--- ans
No.



=== TEST 7: VRG can tell a [//] alpha when it has enough info. :)
设 alpha、beta 表示平面，a、b 表示直线，则 a // alpha 的一个充分条件
是不是 alpha // beta, 且 a on beta ?

--- vrg
plane alpha, beta;
line a, b;

alpha // beta, a on beta => a // alpha

--- vectorize
alpha <//> beta
a <T> beta

--- eval
a <T> alpha

--- antivec
a [//] alpha

--- ans
Yes.
