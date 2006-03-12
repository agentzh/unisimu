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
