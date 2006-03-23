#: basic.t
#: Test the basic  programs
#: Copyright (c) 2006 Wan Xunxin
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-15 2006-03-23

use t::FAST;

plan tests => 4 * blocks();

run_tests;

__DATA__

=== TEST 1: basic program with if_then
--- src
entry => <p>
<p> => [f]
[f] => exit
<p> => exit
--- asm
    test p
    jno  L1
    do   f
L1:
    exit
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
--- asm
    test p
    jno  L1
    do   f
L2:
    exit
L1:
    do   g
    jmp  L2
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
--- asm
L1:
    test p
    jno  L2
    do   f
    jmp  L1
L2:
    exit
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
--- asm
L1:
    do   f
    test p
    jno  L1
    exit
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
--- asm
L1:
    do   f
    test p
    jno  L2
    do   g
    jmp  L1
L2:
    exit
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



=== TEST 6: func nodes with IDs
--- src
entry => [1:a]
[1:a] => [2:a]
[2:a] => exit
--- asm
    do   a
    do   a
    exit
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        do a
        do L:=2
    } else {
        if (L=2) {
            do a
            do L:=0
        }
    }
}
--- opt
do a
do a



=== TEST 6: predicate nodes with IDs
[TODO] There're obviously some potential optimzations here
--- src
entry => <1:p>
<1:p> => <2:p>
<1:p> => exit
<2:p> => <1:p>
<2:p> => exit
--- asm
L1:
    test p
    jno  L2
    test p
    jno  L2
    jmp  L1
L2:
    exit
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
            if (p) {
                do L:=1
            } else {
                do L:=0
            }
        }
    }
}
--- opt
do L:=1
while (L>0) {
    if (p) {
        if (p) {
        } else {
            do L:=0
        }
    } else {
        do L:=0
    }
}
