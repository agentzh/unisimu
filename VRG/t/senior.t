use t::VRG;

plan tests => 144;

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



=== TEST 8:
垂直于同一条直线的两个平面平行吗？

--- vrg
plane alpha, beta;
line l;
alpha T l, beta T l => alpha // beta;

--- vectorize
l <//> alpha
l <//> beta

--- eval
beta <//> alpha

--- antivec
alpha [//] beta

--- ans
Yes.



=== TEST 9: a <T> b, b <T> c =\=> a ?R c
平行于同一条直线的两个平面平行吗？

--- vrg
plane alpha, beta;
line l;

alpha // l, beta // l => alpha // beta;

--- vectorize
l <T> alpha
l <T> beta

--- ans
No.



=== TEST 10:
平行于同一个平面的两个平面平行

--- vrg
plane alpha, beta, theta;
alpha // theta, beta // theta => alpha // beta

--- vectorize
alpha <//> theta
beta <//> theta

--- eval
alpha <//> beta

--- antivec
alpha [//] beta

--- ans
Yes.



=== TEST 11:
一个平面内的两相交直线与另一个平面内的两条相交直线分别平行，
则这两个平面平行吗？

--- vrg
line l1, l2, l3, l4;
plane alpha, beta;
point P, Q;

l1 on alpha, l2 on alpha, meet(l1, l2, Q),
l3 on beta, l4 on beta, meet(l3, l4, Q),
l1 // l3, l2 // l4 => alpha // beta

--- vectorize
l1 <~//> l2
l1 <T> alpha
l2 <T> alpha
l1 <T> gen2
l2 <T> gen2
l3 <T> beta
l4 <T> beta
l3 <T> gen1
l4 <T> gen1
l3 <~//> l4
l2 <//> l4
l1 <//> l3

--- eval
alpha <//> beta

--- antivec
alpha [//] beta



=== TEST 12:
两个平面分别与第三个平面相交所得的两条交线平行，则这两个平面平行

--- vrg
plane alpha, beta, theta;
line l, m;

meet(alpha, theta, l), meet(beta, theta, m), l // m => alpha // beta;

--- vectorize
l <T> alpha
l <T> theta
alpha <~//> theta
m <T> beta
m <T> theta
beta <~//> theta
l <//> m

--- ans
No.



=== TEST 13:
若平面 alpha T 平面 beta, 直线 n 在 alpha 上，直线 m 在 beta 上，
m T n, 则有 n T beta 吗？
--- vrg

plane alpha, beta;
line m, n;

alpha T beta, n on alpha, m on beta, m T n => n T beta;

--- vectorize
alpha <T> beta
n <T> alpha
m <T> beta
m <T> n

--- ans
No.



=== TEST 14:
若平面 alpha T 平面 beta, 直线 n 在 alpha 上，直线 m 在 beta 上，
m T n, 则有 m T alpha 吗？
--- vrg

plane alpha, beta;
line m, n;

alpha T beta, n on alpha, m on beta, m T n => m T alpha;

--- vectorize
alpha <T> beta
n <T> alpha
m <T> beta
m <T> n

--- ans
No.



=== TEST 15:
若平面 alpha T 平面 beta, 直线 n 在 alpha 上，直线 m 在 beta 上，
m T n, 则同时有 n T beta 和 m T alpha 成立吗？

--- vrg

plane alpha, beta;
line m, n;

alpha T beta, n on alpha, m on beta, m T n => n T beta, m T alpha;

--- ans
No.



=== TEST 15:
若平面 alpha T 平面 beta, 直线 n 在 alpha 上，直线 m 在 beta 上，
m T n, 则 n T beta 和 m T alpha 当中至少有一个成立，对吗？

--- vrg

plane alpha, beta;
line m, n;

alpha T beta, n on alpha, m on beta, m T n => n ~T beta, m ~T alpha;

--- ans
No.



=== TEST 16: multiple goals
--- vrg

plane alpha, beta;
line m, n;

alpha T beta, n on alpha, m on beta, m T n => beta T alpha, m T n

--- ans
Yes.



=== TEST 17: multiple goals
multiple goals is yet not (really) supported.
--- vrg

plane alpha, beta;
line m, n;

alpha T beta, n on alpha, m on beta, m T n => beta T alpha, m // n

--- ans
No.
--- SKIP



=== TEST 18: (常规方法中，处理面面垂直问题多半是要作出交线的垂线的。）
已知平面 alpha, beta 都垂直于面 gama, 交线分别为 a, b,
若 a // b, 求证：alpha // beta.

--- vrg

plane alpha, beta, gama;
line a, b;

alpha T gama, beta T gama,
meet(alpha, gama, a),
meet(beta, gama, b),
a // b => alpha // beta;

--- vectorize
alpha <T> gama
beta <T> gama
alpha <~//> gama
a <T> alpha
a <T> gama
beta <~//> gama
b <T> beta
b <T> gama
a <//> b

--- ans
Yes.



=== TEST 19:
已知 a, b, c 是直线，alpha、beta 是平面，
则由 a T b, a T c, b 在 alpha 上，
c 在 alpha 上能否得出直线 a T 平面 alpha？

--- vrg

line a, b, c;
plane alpha, beta;
a T b, a T c, b on alpha, c on alpha => a T alpha;

--- ans
No.



=== TEST 20:
已知 a, b, c 是直线，alpha、beta 是平面，
则由 a T b, b // alpha 能否得出直线 a T 平面 alpha？
--- vrg

line a, b, c;
plane alpha, beta;
a T b, b // alpha => a T alpha;

--- ans
No.



=== TEST 21:
已知 a, b, c 是直线，alpha、beta 是平面，则
由 alpha T beta, b // beta 能否得出直线 a T 平面 alpha？

--- vrg
line a, b, c;
plane alpha, beta;

alpha T beta, a // beta => a T alpha;

--- ans
No.



=== TEST 22:
已知 a, b, c 是直线，alpha、beta 是平面，
则由 a // b, b T alpha 能否得出直线 a T 平面 alpha？

--- vrg
line a, b, c;
plane alpha, beta;

a // b, b T alpha => a T alpha;

--- ans
Yes.

