.section	.rodata

.align 16
.type L0, @object
.size L0, 16
L0:
	.quad 1
	.ascii " "

.align 16
.type L1, @object
.size L1, 16
L1:
	.quad 1
	.ascii " "

.align 16
.type L4, @object
.size L4, 16
L4:
	.quad 2
	.ascii "OK"

.align 16
.type L5, @object
.size L5, 16
L5:
	.quad 4
	.ascii "NOOK"

.section	.text.startup,"ax",@progbits

.globl _tigermain
.type _tigermain,@function
_tigermain:

	pushq %rbp

	movq %rsp, %rbp

	subq $80, %rsp

	L10:

	movq $0, %r10

	movq $L0, %rdi

	movq $L1, %rsi

	call _stringCompare

	movq $0, %r10

	cmpq %r10, %rax

	je L6

	L7:

	movq $L5, %rdi

	call print

	L8:

	movq $0, %rax

	jmp L9

	L6:

	movq $L4, %rdi

	call print

	jmp L8

	L9:

	movq %rbp, %rsp

	pop %rbp

	ret




