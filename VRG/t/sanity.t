use t::VRG;

plan tests => 9 * blocks();

run_tests();

__DATA__

=== TEST 1: 平行公理
--- vrg

line a, b, c;
a // b, c // b => a // c;

--- xclp
include "vrg-sugar.xclp"

\a, \b, \c.
a [//] b, c [//] b.
a *[//] c.

--- vectorize
a <//> b
c <//> b

--- eval
a <//> c

--- final
a [//] c



=== TEST 2: 直线和平面平行的判定定理
--- vrg

line a, b;
plane alpha;
a ~on alpha, b on alpha, a // b => a // alpha;

--- xclp
include "vrg-sugar.xclp"

\a, \b.
#alpha.
a [~on] alpha, b [on] alpha, a [//] b.
a *[//] alpha.

--- vectorize
b <T> alpha
a <//> b

--- eval
a <T> alpha

--- final
a [//] alpha



=== TEST 3: 直线和平面平行的性质定理
--- vrg

line a, b;
plane alpha, beta;
a // alpha, a on beta, meet(alpha, beta, b) => a // b;

--- xclp
include "vrg-sugar.xclp"

\a, \b.
#alpha, #beta.
a [//] alpha, a [on] beta, meet(alpha, beta, b).
a *[//] b.

--- vectorize
a <T> alpha
a <T> beta
b <T> alpha
b <T> beta
alpha <~//> beta

--- eval
a <//> b

--- final
a [//] b



=== TEST 4: 直线和平面垂直的判定定理
--- vrg

line m, n, l;
point B;
plane alpha;

m on alpha, n on alpha, meet(m, n, B), l T m, l T n => l T alpha;

--- xclp
include "vrg-sugar.xclp"

\m, \n, \l.
#alpha.
m [on] alpha, n [on] alpha, meet(m, n, B), l [T] m, l [T] n.
l *[T] alpha.

--- vectorize
m <T> alpha
n <T> alpha
m <~//> n
m <T> gen1
n <T> gen1
l <T> m
l <T> n

--- eval
l <//> alpha

--- final
l [T] alpha



=== TEST 5: 直线和平面垂直的判定定理 II
--- vrg

line a, b;
plane alpha;
a // b, a T alpha => b T alpha;

--- xclp

include "vrg-sugar.xclp"

\a, \b.
#alpha.
a [//] b, a [T] alpha.
b *[T] alpha.

--- vectorize
a <//> b
a <//> alpha

--- eval
b <//> alpha

--- final
b [T] alpha



=== TEST 6: 直线和平面垂直的性质定理
--- vrg

line a, b;
plane alpha;

a T alpha,
b T alpha 
=>
a // b;

--- xclp

include "vrg-sugar.xclp"

\a, \b.
#alpha.
a [T] alpha, b [T] alpha.
a *[//] b.

--- vectorize
a <//> alpha
b <//> alpha

--- eval
a <//> b

--- final
a [//] b



=== TEST 7: 平行线组定理 (1)
--- vrg

line a, b, c, d;
a // b, c // d, a T c => b T d;

--- xclp

include "vrg-sugar.xclp"

\a, \b, \c, \d.
a [//] b, c [//] d, a [T] c.
b *[T] d.

--- vectorize
a <//> b
c <//> d
a <T> c

--- eval
b <T> d

--- final
b [T] d



=== TEST 8: 平行线组定理 (2)
--- vrg

line a, b, c, d;
a // b, c // d, a X c => a X c;

--- xclp

include "vrg-sugar.xclp"

\a, \b, \c, \d.
a [//] b, c [//] d, a [X] c.
a *[X] c.

--- vectorize
a <//> b
c <//> d
a <X> c

--- eval
a <X> c

--- final
a [X] c



=== TEST 9: 两个平面平行的判定定理
--- vrg

line a, b;
plane alpha, beta;
point P;
meet(a, b, P), a on beta, b on beta, a // alpha, b // alpha
=> alpha // beta

--- xclp

include "vrg-sugar.xclp"

\a, \b.
#alpha, #beta.
meet(a, b, P), a [on] beta, b [on] beta, a [//] alpha, b [//] alpha.
alpha *[//] beta.

--- vectorize
a <T> alpha
b <T> alpha
a <T> beta
b <T> beta
a <~//> b
a <T> gen1
b <T> gen1

--- eval
alpha <//> beta

--- final
alpha [//] beta



=== TEST 10: 两个平面平行的性质定理
--- vrg

plane alpha, beta, theta;
line a, b;
alpha // beta, meet(alpha, theta, a), meet(beta, theta, b)
=> a // b;

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta, #theta.
\a, \b.
alpha [//] beta, meet(alpha, theta, a), meet(beta, theta, b).
a *[//] b.

--- vectorize
a <T> alpha
b <T> beta
a <T> theta
b <T> theta
alpha <//> beta
alpha <~//> theta
beta <~//> theta

--- eval
a <//> b

--- final
a [//] b



=== TEST 11: 两个平面平行的性质定理 2
--- vrg

plane alpha, beta;
line l;
alpha // beta, l on alpha => l // beta;

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta.
\l.
alpha [//] beta, l [on] alpha.
l *[//] beta.

--- vectorize
l <T> alpha
alpha <//> beta

--- eval
l <T> beta

--- final
l [//] beta



=== TEST 12: 两个平面平行的性质定理 3
--- vrg

plane alpha, beta;
line l;
alpha // beta, l T alpha => l T beta

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta.
\l.
alpha [//] beta, l [T] alpha.
l *[T] beta.

--- vectorize
l <//> alpha
alpha <//> beta

--- eval
l <//> beta

--- final
l [T] beta



=== TEST 13: 直线和平面垂直的性质定理 2
--- vrg

line l1, l2;
plane alpha;
l1 T alpha, l2 on alpha => l1 T l2;

--- xclp
include "vrg-sugar.xclp"

\l1, \l2.
#alpha.
l1 [T] alpha, l2 [on] alpha.
l1 *[T] l2.

--- vectorize
l2 <T> alpha
l1 <//> alpha

--- eval
l1 <T> l2

--- final
l1 [T] l2



=== TEST 14: 两个平面垂直的判定定理
--- vrg

plane alpha, beta; line l;
l T alpha, l on beta => alpha T beta;

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta.
\l.
l [T] alpha, l [on] beta.
alpha *[T] beta.

--- vectorize
l <//> alpha
l <T> beta

--- eval
alpha <T> beta

--- final
alpha [T] beta



=== TEST 15: 两个平面垂直的性质定理
--- vrg

plane alpha, beta;
line l1, l2;
alpha T beta, meet(alpha, beta, l1), l2 on alpha, l2 T l1
=> l2 T beta;

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta.
\l1, \l2.
alpha [T] beta, meet(alpha, beta, l1), l2 [on] alpha, l2 [T] l1.
l2 *[T] beta.

--- vectorize
l2 <T> l1
l1 <T> alpha
l1 <T> beta
l2 <T> alpha
alpha <T> beta
alpha <~//> beta

--- eval
l2 <//> beta

--- final
l2 [T] beta



=== TEST 16: 三垂线定理
PA、PO 分别是平面 alpha 的垂线、斜线，AO 是 PO 在平面 alpha 内的射影，
且 a 在 alpha 上，a 垂直于 AO，则 a 垂直于 PQ.
--- vrg

plane alpha;
line a;
line b; # line PA
line d; # line AO
line c; # line PO
b T alpha, project(c, alpha, d), a on alpha, a T d
=>
a T c;

--- xclp
include "vrg-sugar.xclp"

#alpha.
\a.
\b.
/* line PA */
\d.
/* line AO */
\c.
/* line PO */
b [T] alpha, project(c, alpha, d), a [on] alpha, a [T] d.
a *[T] c.

--- vectorize
a <T> d
a <T> alpha
gen1 <T> alpha
d <T> alpha
d <T> gen1
gen1 <~//> alpha
c <T> gen1
c <X> alpha
b <//> alpha

--- eval
a <T> c

--- final
a [T] c



=== TEST 17: 三垂线定理逆定理
--- vrg

plane alpha;
line d, c, a;

project(c, alpha, d), a on alpha, a T c => a T d;

--- xclp
include "vrg-sugar.xclp"

#alpha.
\d, \c, \a.
project(c, alpha, d), a [on] alpha, a [T] c.
a *[T] d.

--- vectorize
c <X> alpha
c <T> gen1
d <T> gen1
d <T> alpha
gen1 <~//> alpha
gen1 <T> alpha
a <T> alpha
a <T> c

--- eval
a <T> d

--- final
a [T] d
