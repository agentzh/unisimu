#: basic.t
#: Test the basic  programs
#: Copyright (c) 2006 Wan Xunxin
#: 2006-03-15 2006-03-15

use strict;
use warnings;

use Test::Base;
use FAST;

plan tests => 3 * blocks();

no_diff();

run {
    my $block = shift;
    my $src = $block->src;
    my $g = FAST->new(\$src);
    ok $g, 'obj ok - '.$block->name;
    warn FAST->error() if not $g;
    #$g->as_png('tmp1.png');
    is( $g->as_asm, $block->asm ) if defined $block->asm;
    my $ast = $g->structured(optimized => 0);
    is( $ast->as_c, $block->unopt, 'unopt ok - '.$block->name );
    #$ast->as_png('tmp2.png');
    $ast = $g->structured(optimized => 1);
    is( $ast->as_c, $block->opt, 'opt ok - '.$block->name );
    #$ast->as_png('tmp3.png');
};

__DATA__

=== TEST 1: basic program with if_then
--- src
entry => <p>
<p> => [f]
[f] => exit
<p> => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (p) {
            do L:=2
        } else {
            do L:=0
        }
    } else {
        if (L=2) {
            do f
            do L:=0
        }
    }
}
--- opt
if (p) {
    do f
}




=== TEST 2: basic program with if_then_else
--- src
entry => <p>
<p> => [f]
[f] => exit
<p> => [g]
[g] => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (p) {
            do L:=2
        } else {
            do L:=3
        }
    } else {
        if (L=2) {
            do f
            do L:=0
        } else {
            if (L=3) {
                do g
                do L:=0
            }
        }
    }
}
--- opt
if (p) {
    do f
} else {
    do g
}



=== TEST 3: basic porgram with while_do 
--- src
entry => <p> 
<p> => [f]
[f] => <p>
<p> => exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (p) {
            do L:=2
        } else {
            do L:=0
        }
    } else {
        if (L=2) {
            do f
            do L:=1
        }
    }
}
--- opt
do L:=1
while (L>0) {
    if (p) {
        do f
    } else {
        do L:=0
    }
}



=== TEST 4: basic program with do_until
--- src
entry => [f]
[f] => <p> 
<p> => exit
<p> => [f]
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        do f
        do L:=2
    } else {
        if (L=2) {
            if (p) {
                do L:=0
            } else {
                do L:=1
            }
        }
    }
}
--- opt
do L:=1
while (L>0) {
    do f
    if (p) {
        do L:=0
    }
}




=== TEST 5: basic program with do_while_do
--- src
entry => [f]
[f] => <p>
<p> => [g]
<p> => exit
[g] => [f]
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        do f
        do L:=3
    } else {
        if (L=2) {
            do g
            do L:=1
        } else {
            if (L=3) {
                if (p) {
                    do L:=2
                } else {
                    do L:=0
                }
            }
        }
    }
}
--- opt
do L:=1
while (L>0) {
    do f
    if (p) {
        do g
    } else {
        do L:=0
    }
}
