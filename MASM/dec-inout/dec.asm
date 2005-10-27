data segment
    buffer db 10, ?, 10 dup (?)
    is_neg dw 0
    first_time dw 1
    value dw ?
data ends

code segment
    assume cs:code,ds:data
start:
    mov ax, data
    mov ds, ax

    ; input a string to buffer:
    lea dx, buffer
    mov ah, 0ah
    int 21h

    xor cl, cl
    lea si, buffer+2
    mov bx, 1  ; is the first time?

;----- start loop -----

next:
    mov bx, [first_time]
    mov dl, [si]

    xor dh, dh
    ; say "\nChecking char ", dx
    test bx, bx
    jz not_first_time

    ; say "  this is the first time!"
    cmp dl, '-'
    jne unset_first

    ; say "  it's a minus sign!"
    mov [is_neg], 1
    jmp incr_ptr

unset_first:
    mov [first_time], 0

not_first_time:

    cmp dl, '0'
    jl quit
    cmp dl, '9'
    jg quit

    ; say "  Here!"
    ; say "  dx = ", dx
    sub dl, 30h
    xor dh, dh
    ; say "  dx = ", dx

    push dx
    mov ax, [value]
    ; say "  old value = ", value
    xor dx,dx
    mov bx, 10
    imul bx
    ; say "  product is ", ax
    pop dx

    ; say "  dx = ", dx
    xor dh, dh
    ; say "  dx = ", dx
    add ax, dx
    mov [value], ax
    ; say "  new value = ", value

incr_ptr:
    inc si
    inc cl
    mov bl, [buffer+1]
    xor bh, bh
    ; "The number of input chars is ", bx
    cmp cl, bl
    jl next

;----- end loop -----

quit:
    mov ax, [is_neg]
    test ax, ax
    jz L1
    mov ax, [value]
    neg ax
    mov [value], ax
L1:
    ; say "value = ", value
    mov ax, 4c00h
    int 21h
code ends
     end start
