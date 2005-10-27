STEP equ 11

data segment
	RESULT dw ?
data ends

code segment
	assume cs:code, ds:data
start:
	mov ax, data
	mov ds, ax

	mov dx, 0
	mov cx, (99-11)/STEP+1
	mov bx, 11
next:
	add dx, bx
	add bx, STEP
	loop next

	mov RESULT, dx

	mov ax, 4c00h
	int 21h
code ends
	 end start
