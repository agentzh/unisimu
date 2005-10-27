data segment
    ARRAY dw 21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2
    ODD dw ?
data ends

code segment
    assume cs:code, ds:data
start:
    xor dx, dx
    xor di, di
    lea bp, ARRAY
L1:
    cmp di, 20*2
    jge L3
    mov ax, [bp+di]

    ; say "Checking elem ", ax, " at ", di, "..."

    ; Jump aside if it's an even number
    test ax, 1
    jz L2

    ; Jump in if dx is unset:
    test dx, dx
    jz L4

    ; Jump aside if it's less than dx
    cmp ax, dx
    jge L2
L4:
    ; say "    Moving ", ax, " to dx..."
    mov dx, ax
L2:
    add di, 2
    jmp L1
L3:
    mov ODD, dx

    mov ax, 4c00h
    int 21h
code ends
     end start
