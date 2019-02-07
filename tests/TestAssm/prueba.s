.section	.rodata

.align 16
.type L8, @object
.size L8, 16
L8:
	.quad 18
	.ascii "Reinas de la noche"

.align 16
.type L9, @object
.size L9, 16
L9:
	.quad 6
	.ascii "Burras"

.section	.text.startup,"ax",@progbits

.globl L0
.type L0,@function
L0:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L16:

	movq %rbp, %r10

	movq $-8, %r11

	addq %r11, %r10

	movq %rdi, (%r10)

	movq %rsi, %r10

	movq $0, %r11

	cmpq %r11, %r10

	je L3

	L4:

	movq %r10, %r13

	movq %rbp, %r11

	movq $-8, %r12

	addq %r12, %r11

	movq (%r11), %rdi

	movq %r10, %rsi

	movq $1, %r10

	subq %r10, %rsi

	call L0

	movq %rax, %r11

	movq %r13, %r10

	imul %r11, %r10

	movq %r10, %r10

	L5:

	movq %r10, %rax

	jmp L15

	L3:

	movq $1, %r10

	jmp L5

	L15:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl _tigermain
.type _tigermain,@function
_tigermain:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L14:

	movq %rbp, %rdi

	movq $4, %rsi

	call L0

	movq %rax, %r11

	movq $24, %r10

	cmpq %r10, %r11

	je L10

	L11:

	movq $L9, %rdi

	call print

	L12:

	movq $0, %rax

	jmp L13

	L10:

	movq $L8, %rdi

	call print

	jmp L12

	L13:




	movq %rbp, %rsp
	popq %rbp
	ret

