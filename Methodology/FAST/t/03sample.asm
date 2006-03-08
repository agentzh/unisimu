L1:
    do   f
    test p
    jno  L2
    do   g
    test q
    jno  L3
    jmp  L1
L2:
    do   h
L3:
    exit
