data segment
    buffer  db 14, ?, 14 dup (?)
    nothers  dw 0
    ndigits  dw 0
    nletters dw 0
    prompt  db "Enter a string please: $"
    prom_d  db 0ah, 0dh, "Number of digits: $"
    prom_l  db 0ah, 0dh, "Number of letters: $"
    prom_o  db 0ah, 0dh, "Number of other chars: $"
data ends

code segment
    assume cs:code, ds:data
start:
    mov ax,data
    mov ds,ax
    mov si, 0 ;!!!
    lea dx,prompt
    mov ah,09h
    int 21h

    lea dx,buffer
    mov ah,0ah
    int 21h   

    mov al, [buffer+1]
    xor ah, ah
    ; say "\nn = ", ax

    ; i = 0
    xor bx,bx
L1:
    ; i < n
    mov al,[buffer+1]
    xor ah,ah
    cmp bx,ax
    jge Lend

    ; say "\nbx = ", bx

    mov al,[bx+buffer+2]
    xor ah, ah
    ; say "Loading char ", ax, "..."

    ; if (is_digit(A[i]))
    cmp al,'0'
    jl L2
    cmp al,'9'
    jg L2

    ; ndigits++
    ; say "ndigits++"
    mov dx, [ndigits]
    inc dx
    mov [ndigits], dx
    ; say "ndigits = ", ndigits
    jmp L6

    ; else if (is_letter(A[i]))
L2:
    ; al >= 'a' && al <= 'z'
    cmp al,'a'
    jl L3
    cmp al,'z'
    jle L4

L3:
    ; al >= 'A' && al <= 'Z'
    cmp al,'A'
    jl L5
    cmp al,'Z'
    jg L5

L4:
    ; nletters++
    ; say "nletters++"
    mov dx, [nletters]
    inc dx
    mov [nletters], dx
    ; say "nletters = ", nletters
    jmp L6

L5:
    ; otherwise
    ; say "nothers++"
    mov dx, [nothers]
    inc dx
    ; say "nothers = ", nothers
    mov [nothers], dx

L6:
    inc bx
    jmp L1

Lend:
    ; print ndigits
    lea dx,prom_d
    mov ah,09h
    int 21h

    mov dx, [ndigits]
    add dl, 30h
    mov ah, 02h
    int 21h

    ; print nletters
    lea dx,prom_l
    mov ah,09h
    int 21h

    mov dx,[nletters]
    add dl,30h
    mov ah,02h
    int 21h
    
    ; print nothers
    lea dx,prom_o
    mov ah,09h
    int 21h

    mov dx,[nothers]
    add dl, 30h
    mov ah, 02h
    int 21h

    mov ax, 4c00h
    int 21h
code ends
     end start
