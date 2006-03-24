#: sanity.t
#: Test the basic flowchart programs using FAST's as_c
#: Copyright (c) 2006 Agent Zhang
#: 2006-03-12 2006-03-24

use t::FAST;

plan tests => 4 * blocks();

#debug(1);

run_tests;

__DATA__

=== TEST 1: program with a single func node
--- src
entry => [a]
[a] => exit
--- asm
    do   a
    exit
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
--- asm
    do   a
    do   b
    exit
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
--- asm
L1:
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
--- asm
L1:
    test g
    jno  L2
L3:
    test p
    jno  L1
    jmp  L3
L2:
    test q
    jno  L4
    jmp  L2
L4:
    exit
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



=== TEST 5: Test the bug concerning `entry => exit'
This is a bug reported by xiaoke++ and xunxin++
--- src
entry => exit
<a> => exit
<a> => [b]
[b] => <a>
--- asm
    exit
--- unopt
--- opt



=== TEST 6: Test <b> and [b]:
This test case is contributed by xiaoke++
--- src
entry =><b>
<b> => exit
<b> => [b]
[b] => exit
--- asm
    test b
    jno  L1
L2:
    exit
L1:
    do   b
    jmp  L2
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (b) {
            do L:=0
        } else {
            do L:=2
        }
    } else {
        if (L=2) {
            do b
            do L:=0
        }
    }
}
--- opt
if (b) {
} else {
    do b
}



=== TEST 7: Example 2.1 in our textbook
contributed by xiaoke++
--- src
entry => <p>
<p> => [e]
<p> => <q>
<q> => exit
<q> => [h]
[h] => <p>
[e] => exit
--- asm
L1:
    test p
    jno  L2
    do   e
L3:
    exit
L2:
    test q
    jno  L4
    jmp  L3
L4:
    do   h
    jmp  L1
--- unopt
do L:=1
while (L>0) {
    if (L=1) {
        if (p) {
            do L:=2
        } else {
            do L:=4
        }
    } else {
        if (L=2) {
            do e
            do L:=0
        } else {
            if (L=3) {
                do h
                do L:=1
            } else {
                if (L=4) {
                    if (q) {
                        do L:=0
                    } else {
                        do L:=3
                    }
                }
            }
        }
    }
}
--- opt
do L:=1
while (L>0) {
    if (p) {
        do e
        do L:=0
    } else {
        if (q) {
            do L:=0
        } else {
            do h
        }
    }
}



=== TEST 8: Example 2.2 in our textbook
contributed by xiaoke++
--- src
entry => <p> 
<p> => [f]
[f] => <q>
<q> => [h]
<q> => <r>
<p> => [g]
[g] => <r>
<r> => [h]
<r> => [k]
[h] => exit
[k] => exit
--- asm
    test p
    jno  L1
    do   f
    test q
    jno  L2
L3:
    do   h
L4:
    exit
L1:
    do   g
L2:
    test r
    jno  L5
    jmp  L3
L5:
    do   k
    jmp  L4
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
            do L:=6
        } else {
            if (L=3) {
                do g
                do L:=7
            } else {
                if (L=4) {
                    do h
                    do L:=0
                } else {
                    if (L=5) {
                        do k
                        do L:=0
                    } else {
                        if (L=6) {
                            if (q) {
                                do L:=4
                            } else {
                                do L:=7
                            }
                        } else {
                            if (L=7) {
                                if (r) {
                                    do L:=4
                                } else {
                                    do L:=5
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
--- opt
if (p) {
    do f
    if (q) {
        do h
    } else {
        if (r) {
            do h
        } else {
            do k
        }
    }
} else {
    do g
    if (r) {
        do h
    } else {
        do k
    }
}
