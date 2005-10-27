data segment
	RESULT dw ?
data ends

code segment
	assume cs:code, ds:data
start:
	mov ax, data
	mov ds, ax

	mov dx, 0
	mov cx, (100-1)/3+1
	mov bx, 1
next:
	add dx, bx
	add bx, 3
	loop next

	mov RESULT, dx

	mov ax, 4c00h
	int 21h
code ends
	 end start
