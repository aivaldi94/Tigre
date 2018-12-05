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
	popq %rcx
	pushq %rcx
	movq %rsp, %rcx
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L16:

	movq (%rcx), %rdx
	movq %rbp,  -32(%rbp)
	movq $0, %r8
	movq (%rcx), %rdx
	
	movq %r8,  -40(%rbp)
	movq  -40(%rbp), %r9
	movq  -32(%rbp), %r8
	addq %r9, %r8
	movq (%rcx), %rdx
	
	movq %r8,  -32(%rbp)
	movq  -32(%rbp), %r8
	movq %rdi, (%r8)
	movq (%rcx), %rdx
	
	movq %rsi,  -8(%rbp)
	movq $0, %r8

	movq %r8,  -48(%rbp)
	movq  -48(%rbp), %r9
	movq  -8(%rbp), %r8
	cmpq %r9, %r8
	movq (%rcx), %rdx
	je L3

	L4:

	movq %rbp,  -56(%rbp)
	movq $0, %r8
	movq (%rcx), %rdx
	
	movq %r8,  -64(%rbp)
	movq  -64(%rbp), %r9
	movq  -56(%rbp), %r8
	addq %r9, %r8
	movq (%rcx), %rdx
	
	movq %r8,  -56(%rbp)
	movq  -56(%rbp), %r8
	movq (%r8), %rdi
	movq (%rcx), %rdx

	movq  -8(%rbp), %r8
	movq %r8, %rsi
	movq (%rcx), %rdx

	movq $1, %r8

	movq %r8,  -16(%rbp)
	movq  -16(%rbp), %r8
	subq %r8, %rsi
	movq (%rcx), %rdx
	
	call L0

	movq (%rcx), %rdx
	movq %rax, %r8

	movq %r8,  -24(%rbp)
	L5:

	movq (%rcx), %rdx
	movq  -24(%rbp), %rax
	jmp L15

	L3:

	movq $1, %r8
	movq (%rcx), %rdx
	movq %r8,  -24(%rbp)
	jmp L5

	L15:

	movq %rbp, %rsp
	popq %rbp
	movq (%rsp), %rdx
	ret
	
.globl _tigermain
.type _tigermain,@function
_tigermain:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L14:

	movq %rbp, %rdi

	movq $1, %rsi

	call L0

	movq %rax, %r8

	movq %r8,  -16(%rbp)
	movq $1, %r8

	movq %r8,  -8(%rbp)
	movq  -8(%rbp), %r8
	movq  -16(%rbp), %r9
	cmpq %r8, %r9

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

