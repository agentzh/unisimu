use Test::Base;
use IPC::Run3;
use File::Slurp;

plan tests => 3 * blocks();
my $count = 0;

my $xclips = "$^X xclips.pl";

#no_diff;

run {
    my $block = shift;
    my $name = $block->name;
    my $id = sprintf("%03d", ++$count);
    write_file("$id.xclp", "/* $name */\n\n", $block->xclp);
    ok system(split(/\s+/, $xclips), "$id.xclp") == 0, "$name - invoking $xclips ok";
    my ($stdout, $stderr);
    ok run3(
            [$^X, 'vrg-run.pl', "$id.clp"],
            \undef,
            \$stdout,
            \$stderr,
        ),
        "$name - vrg-run.pl ok";
    warn $stderr if $stderr;
    my $got = sort_list($stdout);
    my $expected = sort_list($block->vectorized);
    is $got, $expected, "$name - vectorization ok";
};

sub sort_list {
    my $s = shift;
    my @ln = split /\n/, $s;
    join( "\n", sort @ln )."\n";
}

__DATA__

=== TEST 1: 平行公理
--- vrg

line a, b, c;
a // b, c // b => a// c;

--- xclp
include "vrg-sugar.xclp"

\ a, \ b, \ c.
a [//] b, c [//] b.
--- vectorized
parallel a b
parallel c b



=== TEST 2: 直线和平面平行的判定定理
--- vrg

line a, b;
plan alpha;
a ~on alpha, b on alpha, a // b => a // alpha;

--- xclp
include "vrg-sugar.xclp"

\ a, \ b.
# alpha.
a [~on] alpha, b [on] alpha, a [//] b.

--- vectorized
not_orthogonal a alpha
orthogonal b alpha
parallel a b



=== TEST 3: 直线和平面平行的性质定理
--- vrg

line a, b;
plane alpha, beta;
a // alpha, a on beta, meet(alpha, beta, b) => a // b;

--- xclp
include "vrg-sugar.xclp"

\ a, \ b.
# alpha, # beta.
a [//] alpha, a [on] beta, meet(alpha, beta, b).

--- vectorized
orthogonal a alpha
orthogonal a beta
orthogonal b alpha
orthogonal b beta
not_parallel alpha beta



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
--- vectorized
orthogonal m alpha
orthogonal n alpha
not_parallel m n
orthogonal m gen1
orthogonal n gen1
orthogonal l m
orthogonal l n



=== TEST 5: 直线和平面垂直的判定定理 II
--- vrg

line a, b;
plane alpha;
a // b, a T alpha => b T alpha;

--- xclp

include "vrg-sugar.xclp"

\ a, \ b.
# alpha.
a [//] b, a [T] alpha.

--- vectorized
parallel a b
parallel a alpha



=== TEST 6: 直线和平面垂直的性质定理
--- vrg

line a, b;
plane alpha;

a [T] alpha, b [T] alpha => a // b;

--- xclp

include "vrg-sugar.xclp"

\a, \b.
#alpha.

a [T] alpha, b [T] alpha.

--- vectorized
parallel a alpha
parallel b alpha



=== TEST 7: 平行线组定理 (1)
--- vrg

line a, b, c, d;
a // b, c // d, a T c => b T d;

--- xclp

include "vrg-sugar.xclp"

\a, \b, \c, \d.
a [//] b, c [//] d, a [T] c.

--- vectorized
parallel a b
parallel c d
orthogonal a c



=== TEST 8: 平行线组定理 (2)
--- vrg

line a, b, c, d;
a // b, c // d, a X c => a X c;

--- xclp

include "vrg-sugar.xclp"

\a, \b, \c, \d.
a [//] b, c [//] d, a [X] c.

--- vectorized
parallel a b
parallel c d
cross a c



=== TEST 9: 两个平面平行的判定定理
--- vrg

line a, b;
plan alpha, beta;
point P;
meet(a, b, P), a on beta, b on beta, a // alpha, b // alpha
=> alpha // beta

--- xclp
include "vrg-sugar.xclp"

\ a, \ b.
# alpha, # beta.
meet(a, b, P), a [on] beta, b [on] beta, a [//] alpha, b [//] alpha.

--- vectorized
orthogonal a alpha
orthogonal b alpha
orthogonal a beta
orthogonal b beta
not_parallel a b
orthogonal a gen1
orthogonal b gen1



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

--- vectorized
orthogonal a alpha
orthogonal b beta
orthogonal a theta
orthogonal b theta
parallel alpha beta
not_parallel alpha theta
not_parallel beta theta



=== TEST 11: 两个平面平行的性质定理 2
--- vrg
include "vrg-sugar.xclp"

plane alpha, beta;
line l;
alpha // beta, l on alpha => l // beta.

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta.
\l.
alpha [//] beta, l [on] alpha.

--- vectorized
orthogonal l alpha
parallel alpha beta



=== TEST 12: 两个平面平行的性质定理 3
--- vrg
include "vrg-sugar.xclp"

plane alpha, beta;
line l;
alpha // beta, l T alpha => l T beta

--- xclp
include "vrg-sugar.xclp"

#alpha, #beta.
\l.
alpha [//] beta, l [T] alpha.

--- vectorized
parallel l alpha
parallel alpha beta



=== TEST 13: 直线和平面垂直的性质定理 2
--- vrg
include "vrg-sugar.xclp"

line l1, l2;
plane alpha;
l1 T alpha, l2 on alpha => l1 T l2;

--- xclp
include "vrg-sugar.xclp"

\l1, \l2.
#alpha.
l1 [T] alpha, l2 [on] alpha.

--- vectorized
orthogonal l2 alpha
parallel l1 alpha



=== TEST 14: 两个平面垂直的性质定理
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

--- vectorized
orthogonal l2 l1
orthogonal l1 alpha
orthogonal l1 beta
orthogonal l2 alpha
orthogonal alpha beta
not_parallel alpha beta



=== TEST 15: 三垂线定理
PA、PO 分别是平面 alpha 的垂线、斜线，AO 是 PO 在平面 alpha 内的射影，
且 a 在 alpha 上，a 垂直于 AO，则 a 垂直于 PQ.
--- vrg
include "vrg-sugar.xclp"

line a;
line b; # line PA
line d; # line AO
line c; # line PO
point P, A, O;
project(c, alpha, d), a on alpha, a T d
=>
a T c;

--- xclp
include "vrg-sugar.xclp"

#alpha.
\a.
\b. /* line PA */
\d. /* line AO */
\c. /* line PO */
b [T] alpha, project(c, alpha, d), a [on] alpha, a [T] d.

--- vectorized
orthogonal a d
orthogonal a alpha
orthogonal gen1 alpha
orthogonal d alpha
orthogonal d gen1
not_parallel gen1 alpha
orthogonal c gen1
cross c alpha
parallel b alpha
