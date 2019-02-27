.section	.rodata

.align 16
.type L2, @object
.size L2, 16
L2:
	.quad 2
	.ascii "OK"

.align 16
.type L3, @object
.size L3, 16
L3:
	.quad 5
	.ascii "no OK"

.section	.text.startup,"ax",@progbits

.globl _tigermain
.type _tigermain,@function
_tigermain:

	pushq %rbp

	movq %rsp, %rbp

	subq $112, %rsp

	L8:

	movq $1, %r13

	movq $2, %r11

	movq $3, %rcx

	movq $4, %rdi

	movq $5, %rdx

	movq $6, %rsi

	movq $7, %r10

	movq %r10, -40(%rbp)

	movq $8, %r10

	movq %r10, -32(%rbp)

	movq $9, %r10

	movq %r10, -24(%rbp)

	movq $10, %r10

	movq %r10, -16(%rbp)

	movq $11, %r12

	movq $12, %r14

	movq $13, %r15

	movq $14, %r8

	movq $15, %r9

	movq $16, %rax

	movq $136, %rbx

	addq %r11, %r13

	addq %rcx, %r13

	addq %rdi, %r13

	addq %rdx, %r13

	addq %rsi, %r13

	movq -40(%rbp), %r10

	addq %r10, %r13

	movq -32(%rbp), %r10

	addq %r10, %r13

	movq -24(%rbp), %r10

	addq %r10, %r13

	movq -16(%rbp), %r10

	addq %r10, %r13

	addq %r12, %r13

	addq %r14, %r13

	addq %r15, %r13

	addq %r8, %r13

	addq %r9, %r13

	addq %rax, %r13

	cmpq %rbx, %r13

	je L4

	L5:

	movq $L3, %rdi

	call print

	L6:

	movq $0, %rax

	jmp L7

	L4:

	movq $L2, %rdi

	call print

	jmp L6

	L7:

	movq %rbp, %rsp

	pop %rbp

	ret




