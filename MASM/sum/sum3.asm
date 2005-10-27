data segment
	RESULT dw ?
data ends

code segment
	assume cs:code, ds:data
start:
	mov ax, data
	mov ds, ax

	mov dx, 0
	mov cx, 100
next:
	add dx, cx
	dec cx
	loop next
outer:
	mov RESULT, dx

	mov ax, 4c00h
	int 21h
code ends
	 end start
