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
	dec cx
	add dx, cx
	loop next

	mov RESULT, dx

	mov ax, 4c00h
	int 21h
code ends
	 end start
