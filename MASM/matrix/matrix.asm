code segment
    assume cs:code
start:
    mov cx,15
    mov bl,10h
one:
    push cx
    mov cx,16
two:
    ; output the current ASCII char:
    mov dl,bl
    mov ah,02h
    int 21h

    ; output one white space
    mov dl,20h
    mov ah,02h
    int 21h

    ; generate the next ASCII char:
    inc bl

    loop two
    pop cx

    ; output CR:
    mov dl,0dh
    mov ah,02h
    int 21h

    ; output LF:
    mov dl,0ah
    mov ah,02h
    int 21h

    loop one

    mov     ax,4c00h
    int     21h
code ends
     end start
