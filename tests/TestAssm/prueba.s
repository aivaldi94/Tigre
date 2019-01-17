.section	.rodata

.align 16
.type L0, @object
.size L0, 16
L0:
	.quad 87
	.ascii "Ingresar lista de manera ordenada, al terminar presione una letra. Ej: 1 13 44 68 h\x0a"

.align 16
.type L4, @object
.size L4, 16
L4:
	.quad 1
	.ascii "0"

.align 16
.type L7, @object
.size L7, 16
L7:
	.quad 1
	.ascii "9"

.align 16
.type L15, @object
.size L15, 16
L15:
	.quad 1
	.ascii " "

.align 16
.type L18, @object
.size L18, 16
L18:
	.quad 4
	.ascii "\x0a"

.align 16
.type L30, @object
.size L30, 16
L30:
	.quad 1
	.ascii "0"

.align 16
.type L58, @object
.size L58, 16
L58:
	.quad 1
	.ascii "0"

.align 16
.type L63, @object
.size L63, 16
L63:
	.quad 1
	.ascii "-"

.align 16
.type L66, @object
.size L66, 16
L66:
	.quad 1
	.ascii "0"

.align 16
.type L75, @object
.size L75, 16
L75:
	.quad 4
	.ascii "\x0a"

.align 16
.type L76, @object
.size L76, 16
L76:
	.quad 1
	.ascii " "

.align 16
.type L80, @object
.size L80, 16
L80:
	.quad 12
	.ascii "Listas: \x0a"

.align 16
.type L81, @object
.size L81, 16
L81:
	.quad 20
	.ascii "Lista mergeada: \x0a"

.section	.text.startup,"ax",@progbits

.globl L2
.type L2,@function
L2:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L99:

	movq %rbp, -88(%rbp)
	movq $-8, %r10

	movq %r10, -96(%rbp)
	movq -96(%rbp), %r11
	movq -88(%rbp), %r10
	addq %r11, %r10

	movq %r10, -88(%rbp)
	movq -88(%rbp), %r10
	movq %rdi, (%r10)

	movq %rsi, -16(%rbp)
	movq $L4, %rdi

	call ord

	movq %rax, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %rax
	movq %rax, -56(%rbp)
	movq -16(%rbp), %rdi
	call ord

	movq %rax, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %r10
	movq -56(%rbp), %r11
	cmpq %r10, %r11

	jle L12

	L13:

	movq $0, %r10

	movq %r10, -32(%rbp)
	L14:

	movq -32(%rbp), %rax
	jmp L98

	L12:

	movq $1, %r10

	movq %r10, -24(%rbp)
	movq -16(%rbp), %rdi
	call ord

	movq %rax, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -80(%rbp)
	movq $L7, %rdi

	call ord

	movq %rax, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r10
	movq -80(%rbp), %r11
	cmpq %r10, %r11

	jle L10

	L11:

	movq $0, %r10

	movq %r10, -24(%rbp)
	L10:

	movq -24(%rbp), %rax
	movq %rax, -32(%rbp)
	jmp L14

	L98:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L3
.type L3,@function
L3:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L97:

	movq %rbp, -128(%rbp)
	movq $-8, %r10

	movq %r10, -136(%rbp)
	movq -136(%rbp), %r11
	movq -128(%rbp), %r10
	addq %r11, %r10

	movq %r10, -128(%rbp)
	movq -128(%rbp), %r10
	movq %rdi, (%r10)

	L27:

	movq -8(%rbp), %r10

	movq %r10, -160(%rbp)
	movq -160(%rbp), %r11
	movq -8(%r11), %r10

	movq %r10, -152(%rbp)
	movq -152(%rbp), %r11
	movq %r11, %r10

	movq %r10, -144(%rbp)
	movq -144(%rbp), %r10
	addq $-16, %r10

	movq %r10, -144(%rbp)
	movq -144(%rbp), %r10
	movq (%r10), %rdi

	movq $L15, %rsi

	call _stringCompare

	movq %rax, %r10

	movq %r10, -96(%rbp)
	movq $0, %r10

	movq %r10, -168(%rbp)
	movq -168(%rbp), %r11
	movq -96(%rbp), %r10
	cmpq %r11, %r10

	je L23

	L24:

	movq $1, %r10

	movq %r10, -16(%rbp)
	movq -8(%rbp), %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r11
	movq -8(%r11), %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r11
	movq %r11, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r10
	addq $-16, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r10
	movq (%r10), %rdi

	movq $L18, %rsi

	call _stringCompare

	movq %rax, %r10

	movq %r10, -104(%rbp)
	movq $0, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %r10
	movq -104(%rbp), %r11
	cmpq %r10, %r11

	je L21

	L22:

	movq $0, %r10

	movq %r10, -16(%rbp)
	L21:

	movq -16(%rbp), %rax
	movq %rax, -88(%rbp)
	L25:

	movq $0, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r10
	movq -88(%rbp), %r11
	cmpq %r10, %r11

	jne L28

	L26:

	jmp L96

	L23:

	movq $1, %r10

	movq %r10, -88(%rbp)
	jmp L25

	L28:

	movq -8(%rbp), %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r11
	movq -8(%r11), %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r11
	movq %r11, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %r10
	addq $-16, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -120(%rbp)
	call getstr

	movq %rax, %r10

	movq %r10, -112(%rbp)
	movq -112(%rbp), %r10
	movq -120(%rbp), %r11
	movq %r10, (%r11)

	jmp L27

	L96:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L1
.type L1,@function
L1:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L95:

	movq %rbp, -24(%rbp)
	movq $-8, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r11
	movq -24(%rbp), %r10
	addq %r11, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r10
	movq %rdi, (%r10)

	movq %rsi, -16(%rbp)
	movq $0, %r10

	movq %r10, -168(%rbp)
	movq %rbp, %rdi

	call L3

	movq -16(%rbp), %rax
	movq %rax, -120(%rbp)
	movq $0, %r10

	movq %r10, -160(%rbp)
	movq -120(%rbp), %rdi
	call _checkNil

	movq -120(%rbp), %rax
	movq %rax, -40(%rbp)
	movq -160(%rbp), %r11
	movq %r11, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %r10
	imul $8, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %r11
	movq -40(%rbp), %r10
	addq %r11, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %rax
	movq %rax, -184(%rbp)
	movq %rbp, %rdi

	movq -8(%rbp), %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %r11
	movq %r11, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r10
	addq $-16, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r10
	movq (%r10), %rsi

	call L2

	movq %rax, %r10

	movq %r10, -176(%rbp)
	movq -176(%rbp), %r10
	movq -184(%rbp), %r11
	movq %r10, (%r11)

	L31:

	movq %rbp, %rdi

	movq -8(%rbp), %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r11
	movq %r11, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r10
	addq $-16, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r10
	movq (%r10), %rsi

	call L2

	movq %rax, %r10

	movq %r10, -192(%rbp)
	movq $0, %r10

	movq %r10, -88(%rbp)
	movq -88(%rbp), %r10
	movq -192(%rbp), %r11
	cmpq %r10, %r11

	jne L32

	L29:

	movq -168(%rbp), %rax
	jmp L94

	L32:

	movq -168(%rbp), %r11
	movq %r11, %r10

	movq %r10, -96(%rbp)
	movq -96(%rbp), %r10
	imul $10, %r10

	movq %r10, -96(%rbp)
	movq -96(%rbp), %rax
	movq %rax, -208(%rbp)
	movq -8(%rbp), %r10

	movq %r10, -112(%rbp)
	movq -112(%rbp), %r11
	movq %r11, %r10

	movq %r10, -104(%rbp)
	movq -104(%rbp), %r10
	addq $-16, %r10

	movq %r10, -104(%rbp)
	movq -104(%rbp), %r10
	movq (%r10), %rdi

	call ord

	movq %rax, %r10

	movq %r10, -200(%rbp)
	movq -208(%rbp), %rax
	movq %rax, -128(%rbp)
	movq -200(%rbp), %r11
	movq -128(%rbp), %r10
	addq %r11, %r10

	movq %r10, -128(%rbp)
	movq -128(%rbp), %rax
	movq %rax, -224(%rbp)
	movq $L30, %rdi

	call ord

	movq %rax, %r10

	movq %r10, -216(%rbp)
	movq -224(%rbp), %rax
	movq %rax, -136(%rbp)
	movq -216(%rbp), %r11
	movq -136(%rbp), %r10
	subq %r11, %r10

	movq %r10, -136(%rbp)
	movq -136(%rbp), %rax
	movq %rax, -168(%rbp)
	movq -8(%rbp), %r10

	movq %r10, -152(%rbp)
	movq -152(%rbp), %r11
	movq %r11, %r10

	movq %r10, -144(%rbp)
	movq -144(%rbp), %r10
	addq $-16, %r10

	movq %r10, -144(%rbp)
	movq -144(%rbp), %rax
	movq %rax, -240(%rbp)
	call getstr

	movq %rax, %r10

	movq %r10, -232(%rbp)
	movq -232(%rbp), %r10
	movq -240(%rbp), %r11
	movq %r10, (%r11)

	jmp L31

	L94:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L33
.type L33,@function
L33:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L93:

	movq %rbp, -16(%rbp)
	movq $-8, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r11
	movq -16(%rbp), %r10
	addq %r11, %r10

	movq %r10, -16(%rbp)
	movq -16(%rbp), %r10
	movq %rdi, (%r10)

	movq $1, %rdi

	movq $0, %rsi

	call _allocRecord

	movq %rax, %r10

	movq %r10, -96(%rbp)
	movq %rbp, -32(%rbp)
	movq $-8, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r11
	movq -32(%rbp), %r10
	addq %r11, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r10
	movq (%r10), %rdi

	movq -96(%rbp), %rsi
	call L1

	movq %rax, %r10

	movq %r10, -104(%rbp)
	movq -96(%rbp), %rax
	movq %rax, -112(%rbp)
	movq $0, %r10

	movq %r10, -120(%rbp)
	movq -112(%rbp), %rdi
	call _checkNil

	movq $0, %r10

	movq %r10, -48(%rbp)
	movq -112(%rbp), %rax
	movq %rax, -64(%rbp)
	movq -120(%rbp), %r11
	movq %r11, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r10
	imul $8, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r11
	movq -64(%rbp), %r10
	addq %r11, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %r11
	movq (%r11), %r10

	movq %r10, -56(%rbp)
	movq -48(%rbp), %r10
	movq -56(%rbp), %r11
	cmpq %r10, %r11

	jne L37

	L38:

	movq $0, %r10

	movq %r10, -128(%rbp)
	L39:

	movq -128(%rbp), %rax
	jmp L92

	L37:

	movq -104(%rbp), %rax
	movq %rax, -144(%rbp)
	movq %rbp, -80(%rbp)
	movq $-8, %r10

	movq %r10, -88(%rbp)
	movq -88(%rbp), %r11
	movq -80(%rbp), %r10
	addq %r11, %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r10
	movq (%r10), %rdi

	call L33

	movq %rax, %r10

	movq %r10, -136(%rbp)
	movq $2, %rdi

	movq -144(%rbp), %rsi
	movq -136(%rbp), %rdx
	call _allocRecord

	movq %rax, %r10

	movq %r10, -128(%rbp)
	jmp L39

	L92:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L34
.type L34,@function
L34:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L91:

	movq %rbp, -16(%rbp)
	movq $-8, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r11
	movq -16(%rbp), %r10
	addq %r11, %r10

	movq %r10, -16(%rbp)
	movq -16(%rbp), %r10
	movq %rdi, (%r10)

	movq %rsi, -64(%rbp)
	movq %rdx, -152(%rbp)
	movq $0, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r10
	movq -64(%rbp), %r11
	cmpq %r10, %r11

	je L52

	L53:

	movq $0, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r10
	movq -152(%rbp), %r11
	cmpq %r10, %r11

	je L49

	L50:

	movq -64(%rbp), %rax
	movq %rax, -208(%rbp)
	movq $0, %r10

	movq %r10, -216(%rbp)
	movq -208(%rbp), %rdi
	call _checkNil

	movq -208(%rbp), %rax
	movq %rax, -56(%rbp)
	movq -216(%rbp), %r11
	movq %r11, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r10
	imul $8, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r11
	movq -56(%rbp), %r10
	addq %r11, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r11
	movq (%r11), %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %rax
	movq %rax, -328(%rbp)
	movq -152(%rbp), %rax
	movq %rax, -224(%rbp)
	movq $0, %r10

	movq %r10, -232(%rbp)
	movq -224(%rbp), %rdi
	call _checkNil

	movq -224(%rbp), %rax
	movq %rax, -88(%rbp)
	movq -232(%rbp), %r11
	movq %r11, %r10

	movq %r10, -96(%rbp)
	movq -96(%rbp), %r10
	imul $8, %r10

	movq %r10, -96(%rbp)
	movq -96(%rbp), %r11
	movq -88(%rbp), %r10
	addq %r11, %r10

	movq %r10, -88(%rbp)
	movq -88(%rbp), %r11
	movq (%r11), %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r10
	movq -328(%rbp), %r11
	cmpq %r10, %r11

	jl L46

	L47:

	movq -152(%rbp), %rax
	movq %rax, -272(%rbp)
	movq $0, %r10

	movq %r10, -280(%rbp)
	movq -272(%rbp), %rdi
	call _checkNil

	movq -272(%rbp), %rax
	movq %rax, -112(%rbp)
	movq -280(%rbp), %r11
	movq %r11, %r10

	movq %r10, -120(%rbp)
	movq -120(%rbp), %r10
	imul $8, %r10

	movq %r10, -120(%rbp)
	movq -120(%rbp), %r11
	movq -112(%rbp), %r10
	addq %r11, %r10

	movq %r10, -112(%rbp)
	movq -112(%rbp), %r11
	movq (%r11), %r10

	movq %r10, -104(%rbp)
	movq -104(%rbp), %rax
	movq %rax, -384(%rbp)
	movq -8(%rbp), %r10

	movq %r10, -128(%rbp)
	movq -128(%rbp), %rax
	movq %rax, -376(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -368(%rbp)
	movq -152(%rbp), %rax
	movq %rax, -288(%rbp)
	movq $1, %r10

	movq %r10, -296(%rbp)
	movq -288(%rbp), %rdi
	call _checkNil

	movq -376(%rbp), %rdi
	movq -368(%rbp), %rsi
	movq -288(%rbp), %rax
	movq %rax, -136(%rbp)
	movq -296(%rbp), %r11
	movq %r11, %r10

	movq %r10, -144(%rbp)
	movq -144(%rbp), %r10
	imul $8, %r10

	movq %r10, -144(%rbp)
	movq -144(%rbp), %r11
	movq -136(%rbp), %r10
	addq %r11, %r10

	movq %r10, -136(%rbp)
	movq -136(%rbp), %r10
	movq (%r10), %rdx

	call L34

	movq %rax, %r10

	movq %r10, -360(%rbp)
	movq $2, %rdi

	movq -384(%rbp), %rsi
	movq -360(%rbp), %rdx
	call _allocRecord

	movq %rax, %r10

	movq %r10, -304(%rbp)
	L48:

	movq -304(%rbp), %rax
	movq %rax, -312(%rbp)
	L51:

	movq -312(%rbp), %rax
	movq %rax, -320(%rbp)
	L54:

	movq -320(%rbp), %rax
	jmp L90

	L52:

	movq -152(%rbp), %rax
	movq %rax, -320(%rbp)
	jmp L54

	L49:

	movq -64(%rbp), %rax
	movq %rax, -312(%rbp)
	jmp L51

	L46:

	movq -64(%rbp), %rax
	movq %rax, -240(%rbp)
	movq $0, %r10

	movq %r10, -248(%rbp)
	movq -240(%rbp), %rdi
	call _checkNil

	movq -240(%rbp), %rax
	movq %rax, -168(%rbp)
	movq -248(%rbp), %r11
	movq %r11, %r10

	movq %r10, -176(%rbp)
	movq -176(%rbp), %r10
	imul $8, %r10

	movq %r10, -176(%rbp)
	movq -176(%rbp), %r11
	movq -168(%rbp), %r10
	addq %r11, %r10

	movq %r10, -168(%rbp)
	movq -168(%rbp), %r11
	movq (%r11), %r10

	movq %r10, -160(%rbp)
	movq -160(%rbp), %rax
	movq %rax, -352(%rbp)
	movq -8(%rbp), %r10

	movq %r10, -184(%rbp)
	movq -184(%rbp), %rax
	movq %rax, -344(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -256(%rbp)
	movq $1, %r10

	movq %r10, -264(%rbp)
	movq -256(%rbp), %rdi
	call _checkNil

	movq -344(%rbp), %rdi
	movq -256(%rbp), %rax
	movq %rax, -192(%rbp)
	movq -264(%rbp), %r11
	movq %r11, %r10

	movq %r10, -200(%rbp)
	movq -200(%rbp), %r10
	imul $8, %r10

	movq %r10, -200(%rbp)
	movq -200(%rbp), %r11
	movq -192(%rbp), %r10
	addq %r11, %r10

	movq %r10, -192(%rbp)
	movq -192(%rbp), %r10
	movq (%r10), %rsi

	movq -152(%rbp), %rdx
	call L34

	movq %rax, %r10

	movq %r10, -336(%rbp)
	movq $2, %rdi

	movq -352(%rbp), %rsi
	movq -336(%rbp), %rdx
	call _allocRecord

	movq %rax, %r10

	movq %r10, -304(%rbp)
	jmp L48

	L90:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L55
.type L55,@function
L55:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L89:

	movq %rbp, -16(%rbp)
	movq $-8, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r11
	movq -16(%rbp), %r10
	addq %r11, %r10

	movq %r10, -16(%rbp)
	movq -16(%rbp), %r10
	movq %rdi, (%r10)

	movq %rsi, -96(%rbp)
	movq $0, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r10
	movq -96(%rbp), %r11
	cmpq %r10, %r11

	jg L59

	L60:

	jmp L88

	L59:

	movq %rbp, -40(%rbp)
	movq $-8, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %r11
	movq -40(%rbp), %r10
	addq %r11, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r10
	movq (%r10), %rdi

	movq $10, %r10

	movq %r10, -56(%rbp)
	movq -96(%rbp), %r11
	movq -56(%rbp), %r10
	movq %r11,%rax; cqto; idiv %r10; movq %rax, %rdx; movq %rax, %rsi

	call L55

	movq -96(%rbp), %rax
	movq %rax, -64(%rbp)
	movq $10, %r10

	movq %r10, -88(%rbp)
	movq -96(%rbp), %r12
	movq -88(%rbp), %r11
	movq %r12,%rax; cqto; idiv %r11; movq %rax, %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r11
	movq %r11, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r10
	imul $10, %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %r11
	movq -64(%rbp), %r10
	subq %r11, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %rax
	movq %rax, -120(%rbp)
	movq $L58, %rdi

	call ord

	movq %rax, %r10

	movq %r10, -112(%rbp)
	movq -120(%rbp), %r10
	movq %r10, %rdi

	movq -112(%rbp), %r10
	addq %r10, %rdi

	call chr

	movq %rax, %r10

	movq %r10, -104(%rbp)
	movq -104(%rbp), %rdi
	call print

	jmp L60

	L88:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L35
.type L35,@function
L35:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L87:

	movq %rbp, -24(%rbp)
	movq $-8, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r11
	movq -24(%rbp), %r10
	addq %r11, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r10
	movq %rdi, (%r10)

	movq %rsi, -16(%rbp)
	movq $0, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r11
	movq -16(%rbp), %r10
	cmpq %r11, %r10

	jl L70

	L71:

	movq $0, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %r11
	movq -16(%rbp), %r10
	cmpq %r11, %r10

	jg L67

	L68:

	movq $L66, %rdi

	call print

	L69:

	L72:

	jmp L86

	L70:

	movq $L63, %rdi

	call print

	movq %rbp, %rdi

	movq $0, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r10
	movq %r10, %rsi

	movq -16(%rbp), %r10
	subq %r10, %rsi

	call L55

	jmp L72

	L67:

	movq %rbp, %rdi

	movq -16(%rbp), %rsi
	call L55

	jmp L69

	L86:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl L36
.type L36,@function
L36:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L85:

	movq %rbp, -24(%rbp)
	movq $-8, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %r11
	movq -24(%rbp), %r10
	addq %r11, %r10

	movq %r10, -24(%rbp)
	movq -24(%rbp), %r10
	movq %rdi, (%r10)

	movq %rsi, -16(%rbp)
	movq $0, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r11
	movq -16(%rbp), %r10
	cmpq %r11, %r10

	je L77

	L78:

	movq -8(%rbp), %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %rax
	movq %rax, -128(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -96(%rbp)
	movq $0, %r10

	movq %r10, -104(%rbp)
	movq -96(%rbp), %rdi
	call _checkNil

	movq -128(%rbp), %rdi
	movq -96(%rbp), %rax
	movq %rax, -56(%rbp)
	movq -104(%rbp), %r11
	movq %r11, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %r10
	imul $8, %r10

	movq %r10, -64(%rbp)
	movq -64(%rbp), %r11
	movq -56(%rbp), %r10
	addq %r11, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r10
	movq (%r10), %rsi

	call L35

	movq $L76, %rdi

	call print

	movq -8(%rbp), %r10

	movq %r10, -72(%rbp)
	movq -72(%rbp), %rax
	movq %rax, -136(%rbp)
	movq -16(%rbp), %rax
	movq %rax, -112(%rbp)
	movq $1, %r10

	movq %r10, -120(%rbp)
	movq -112(%rbp), %rdi
	call _checkNil

	movq -136(%rbp), %rdi
	movq -112(%rbp), %rax
	movq %rax, -80(%rbp)
	movq -120(%rbp), %r11
	movq %r11, %r10

	movq %r10, -88(%rbp)
	movq -88(%rbp), %r10
	imul $8, %r10

	movq %r10, -88(%rbp)
	movq -88(%rbp), %r11
	movq -80(%rbp), %r10
	addq %r11, %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r10
	movq (%r10), %rsi

	call L36

	L79:

	jmp L84

	L77:

	movq $L75, %rdi

	call print

	jmp L79

	L84:




	movq %rbp, %rsp
	popq %rbp
	ret
.globl _tigermain
.type _tigermain,@function
_tigermain:
	pushq %rbp
	movq %rsp, %rbp
	subq $1024, %rsp


	L83:

	movq $L0, %rdi

	call print

	movq $0, %r10

	movq %r10, -24(%rbp)
	movq %rbp, -32(%rbp)
	movq $-16, %r10

	movq %r10, -40(%rbp)
	movq -40(%rbp), %r11
	movq -32(%rbp), %r10
	addq %r11, %r10

	movq %r10, -32(%rbp)
	movq -32(%rbp), %rax
	movq %rax, -88(%rbp)
	call getstr

	movq %rax, %r10

	movq %r10, -80(%rbp)
	movq -80(%rbp), %r10
	movq -88(%rbp), %r11
	movq %r10, (%r11)

	movq %rbp, %rdi

	call L33

	movq %rax, %r10

	movq %r10, -64(%rbp)
	movq %rbp, -48(%rbp)
	movq $-16, %r10

	movq %r10, -56(%rbp)
	movq -56(%rbp), %r11
	movq -48(%rbp), %r10
	addq %r11, %r10

	movq %r10, -48(%rbp)
	movq -48(%rbp), %rax
	movq %rax, -104(%rbp)
	call getstr

	movq %rax, %r10

	movq %r10, -96(%rbp)
	movq -96(%rbp), %r10
	movq -104(%rbp), %r11
	movq %r10, (%r11)

	movq %rbp, %rdi

	call L33

	movq %rax, %r10

	movq %r10, -72(%rbp)
	movq $L80, %rdi

	call print

	movq %rbp, %rdi

	movq -64(%rbp), %rsi
	call L36

	movq %rbp, %rdi

	movq -72(%rbp), %rsi
	call L36

	movq $L81, %rdi

	call print

	movq %rbp, -120(%rbp)
	movq %rbp, %rdi

	movq -64(%rbp), %rsi
	movq -72(%rbp), %rdx
	call L34

	movq %rax, %r10

	movq %r10, -112(%rbp)
	movq -120(%rbp), %rdi
	movq -112(%rbp), %rsi
	call L36

	movq $0, %rax

	jmp L82

	L82:




	movq %rbp, %rsp
	popq %rbp
	ret

