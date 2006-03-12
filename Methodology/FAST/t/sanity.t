#: sanity.t
#: Test the basic flowchart programs using FAST's as_c
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-12 2006-03-12

use strict;
use warnings;

use Test::Base;
use FAST;

plan tests => 3 * blocks;

run {
    my $block = shift;
    my $src = $block->src;
    my $g = FAST->new(\$src);
    ok $g, 'obj ok - '.$block->name;
    warn FAST->error() if not $g;
    $g->as_png('tmp.png');
    my $ast = $g->structured(optimized => 0);
    is( $block->unopt, $ast->as_c, 'unopt ok - '.$block->name );
    $ast = $g->structured(optimized => 1);
    is( $block->opt, $ast->as_c, 'opt ok - '.$block->name );
};

__DATA__

=== TEST 1: program with single func node
--- src
entry => [a]
[a] => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        do a
        do L:=0
    }
}
--- opt
do a



=== TEST 2: program with two func nodes
--- src
entry => [a]
[a] => [b]
[b] => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        do a
        do L:=2
    } else {
        if (L=2) {
            do b
            do L:=0
        }
    }
}
--- opt
do a
do b



=== TEST 3: program with single predicate node
--- src
entry => <p>
<p> => <p>
<p> => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (p) {
            do L:=1
        } else {
            do L:=0
        }
    }
}
--- opt
do L:=1
while (L>0) {
    if (p) {
    } else {
        do L:=0
    }
}



=== TEST 4: Test a special program with 3 recursive basic (composite) programs
--- src
entry => <g>
<g> => <p>
<g> => <q>
<p> => <p>
<p> => <g>
<q> => <q>
<q> => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (g) {
            do L:=2
        } else {
            do L:=3
        }
    } else {
        if (L=2) {
            if (p) {
                do L:=2
            } else {
                do L:=1
            }
        } else {
            if (L=3) {
                if (q) {
                    do L:=3
                } else {
                    do L:=0
                }
            }
        }
    }
}
--- opt
if (g) {
    do L:=2
} else {
    do L:=3
}
while (L>0) {
    if (L=2) {
        if (p) {
            do L:=2
        } else {
            if (g) {
                do L:=2
            } else {
                do L:=3
            }
        }
    } else {
        if (L=3) {
            if (q) {
                do L:=3
            } else {
                do L:=0
            }
        }
    }
}
