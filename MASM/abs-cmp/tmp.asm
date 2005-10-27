; abs-cmp.asm
; 2005-10-14 2005-10-14
data segment
    NUL db "NUL"
data ends

code segment
    assume cs:code, ds:data
start:
    mov ax, data
    mov ds, ax

    mov ax, -1
    mov bx, -2
    call abs_cmp
    mov ax, 0abcdh
    ; say "ax = ", ax, " (2)\n"
    call _masmd_P1
    ; say "bx = ", bx, " (1)\n"

    mov ax, 5
    mov bx, -2
    call abs_cmp
    ; say "ax = ", ax, " (5)\n"
    ; say "bx = ", bx, " (2)\n"

    mov ax, 5
    mov bx, -7
    call abs_cmp
    ; say "ax = ", ax, " (7)\n"
    ; say "bx = ", bx, " (5)\n"

    mov ax, -7
    mov bx, -5
    call abs_cmp
    ; say "ax = ", ax, " (7)\n"
    ; say "bx = ", bx, " (5)\n"

    mov ax, -7
    mov bx, -5
    call abs_cmp
    ; say "ax = ", ax, " (7)\n"
    ; say "bx = ", bx, " (5)\n"

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

_masmd_P1 proc
    jmp _masmd_L1

    _masmd_s1 db "ax = $"
    _masmd_s2 db " (2)", 0dh, 0ah, '$'

_masmd_L1:
    push dx
    push ax
    push ds

    mov ax, cs
    mov ds, ax

    mov dx, offset _masmd_s1
    mov ah, 09h
    int 21h

    pop ds
    pop ax
    pop dx

    call _masmd_outw

    push dx
    push ax
    push ds

    mov ax, cs
    mov ds, ax

    mov dx, cs: offset _masmd_s2
    mov ah, 09h
    int 21h

    pop ds
    pop ax
    pop dx

    ret
_masmd_P1 endp

_masmd_outw proc
    push ax
    push cx
    push dx
    push bx

    mov bx, ax
    mov cl, 16-4

_masmd_outw1:
    mov ax, bx
    shr ax, cl

    mov dl, al
    and dl, 0fh

    cmp dl, 9
    jg _masmd_outw2

    add dl, 30h
    jmp _masmd_outw3

_masmd_outw2:
    add dl, 41h-0ah

_masmd_outw3:
    mov ah, 02h
    int 21h

    sub cl, 4
    jge _masmd_outw1

    pop bx
    pop dx
    pop cx
    pop ax
    ret
_masmd_outw endp

code ends
     end start
