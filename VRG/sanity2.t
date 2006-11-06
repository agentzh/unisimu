use Test::Base;
use IPC::Run3;
use File::Slurp;

plan tests => 3 * blocks();
my $count = 0;

#no_diff;

run {
    my $block = shift;
    my $name = $block->name;
    my $id = sprintf("%03d", ++$count);
    write_file("$id.xclp", "/* $name */\n\n", $block->xclp);
    ok system($^X, "xclp.pl", "$id.xclp") == 0, "$name - xclp.pl ok";
    my ($stdout, $stderr);
    ok run3(
            [$^X, 'vectorize-run.pl', "$id.clp"],
            \undef,
            \$stdout,
            \$stderr,
        ),
        "$name - vectorize-run.pl ok";
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

=== TEST 1: 两个平面平行的判定定理
--- vrg

line a, b;
plan alpha, beta;
point P;
meet(a, b, P), a on beta, b on beta, a // alpha, b // alpha
=> alpha // beta

--- xclp
include "vrg-sugar.xclp".

\a, \b.
#alpha, #beta.
meet(a, b, P), a %on beta, b %on beta, a %// alpha, b %// alpha.

--- vectorized
orthogonal a alpha
orthogonal b alpha
orthogonal a beta
orthogonal b beta
unparallel a b



=== TEST 2: 两个平面平行的性质定理
--- vrg

plane alpha, beta, theta;
line a, b;
alpha // beta, meet(alpha, theta, a), meet(beta, theta, b)
=> a // b;

--- xclp
include "vrg-sugar.xclp".

#alpha, #beta, #theta.
\a, \b.
alpha %// beta, meet(alpha, theta, a), meet(beta, theta, b).

--- vectorized
orthogonal a alpha
orthogonal b beta
orthogonal a theta
orthogonal b theta
parallel alpha beta
unparallel alpha theta
unparallel beta theta



=== TEST 3: 两个平面平行的性质定理 2
--- vrg
include "vrg-sugar.xclp".

plane alpha, beta;
line l;
alpha // beta, l on alpha => l // beta.

--- xclp
include "vrg-sugar.xclp".

#alpha, #beta.
\l.
alpha %// beta, l %on alpha.

--- vectorized
orthogonal l alpha
parallel alpha beta



=== TEST 4: 两个平面平行的性质定理 3
--- vrg
include "vrg-sugar.xclp".

plane alpha, beta;
line l;
alpha // beta, l T alpha => l T beta

--- xclp
include "vrg-sugar.xclp".

#alpha, #beta.
\l.
alpha %// beta, l %T alpha.

--- vectorized
parallel l alpha
parallel alpha beta



=== TEST 5: 直线和平面垂直的性质定理 2
--- vrg
include "vrg-sugar.xclp".

line l1, l2;
plane alpha;
l1 T alpha, l2 on alpha => l1 T l2;

--- xclp
include "vrg-sugar.xclp".

\l1, \l2.
#alpha.
l1 %T alpha, l2 %on alpha.

--- vectorized
orthogonal l2 alpha
parallel l1 alpha



=== TEST 6: 两个平面垂直的性质定理
--- vrg

plane alpha, beta;
line l1, l2;
alpha T beta, meet(alpha, beta, l1), l2 on alpha, l2 T l1
=> l2 T beta;

--- xclp
include "vrg-sugar.xclp".

#alpha, #beta.
\l1, \l2.
alpha %T beta, meet(alpha, beta, l1), l2 %on alpha, l2 %T l1.

--- vectorized
orthogonal l2 l1
orthogonal l1 alpha
orthogonal l1 beta
orthogonal l2 alpha
orthogonal alpha beta
unparallel alpha beta
--- LAST


=== TEST 7: 三垂线定理
PA、PO 分别是平面 alpha 的垂线、斜线，AO 是 PO 在平面 alpha 内的射影，
且 a 在 alpha 上，a 垂直于 AO，则 a 垂直于 PQ.
--- vrg
include "vrg-sugar.xclp".

line a;
line b; # line PA
line d; # line AO
line c; # line PO
point P, A, O;
b T alpha, project(c, alpha, d), a on alpha, a T d
=>
a T c;

--- xclp
include "vrg-sugar.xclp".

\c A #alpha.
\c on #theta.
meet(#theta, #alpha, \d).
#theta T #alpha.
\a on #alpha.
\a T \d.

--- vectorized
orthogonal a d
orthogonal d theta
orthogonal d alpha
orthogonal c theta
orthogonal a alpha
orthogonal theta alpha
oblique c alpha
unparallel theta alpha
