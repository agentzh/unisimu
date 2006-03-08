L1:
    test p
    jno  L2
    do   c
    test q
    jno  L3
    jmp  L1
L2:
    do   a
    do   b
L3:
    exit
