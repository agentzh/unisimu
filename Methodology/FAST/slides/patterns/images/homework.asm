    test p
    jno  L1
    do   f
L2:
    do   h
L3:
    exit
L1:
    do   g
    test q
    jno  L3
    jmp  L2
