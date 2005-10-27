; abs-cmp.asm
; 2005-10-14 2005-10-14

code segment
    assume cs:code
start:
    mov ax, -1
    mov bx, -2
    call abs_cmp
    ; say "ax = ", ax, " (2)"
    ; say "bx = ", bx, " (1)"

    mov ax, 5
    mov bx, -2
    call abs_cmp
    ; say "\nax = ", ax, ' (5)'
    ; say "bx = ", bx, " (2)"

    mov ax, 5
    mov bx, -7
    call abs_cmp
    ; say "\nax = ", ax, " (7)"
    ; say 'bx = ', bx, " (5)"

    mov ax, -7
    mov bx, -5
    call abs_cmp
    ; say "\nax = ", ax, " (7)"
    ; say "bx = ", bx, " (5)\n"

    mov ax, -7
    mov bx, -5
    call abs_cmp
    ; say "ax = ", ax, " (7)"
    ; say "bx = ", bx, " (5)"

    mov ax, 4c00h
    int 21h

abs_cmp proc near
    test ax, ax
    jge L1
    neg ax
L1:
    test bx, bx
    jge L2
    neg bx
L2:
    cmp ax, bx
    jge L3
    xchg ax, bx
L3:
    ret
abs_cmp endp

code ends
     end start
