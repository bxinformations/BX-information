	.section .rodata
.lprintfmt:
	.string "%ld\n"
	.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $224, %rsp
	movq $0, -8(%rbp)
	movq $0, -16(%rbp)
	movq $10, -8(%rbp)
	movq $2, -24(%rbp)
	movq -8(%rbp), %r11
	movq %r11, -32(%rbp)
	movq -24(%rbp), %rax
	imulq -32(%rbp)
	movq %rax, -16(%rbp)
	movq -16(%rbp), %r11
	movq %r11, -40(%rbp)
	movq -16(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -40(%rbp), %rax
	imulq -48(%rbp)
	movq %rax, -56(%rbp)
	movq $2, -64(%rbp)
	movq -56(%rbp), %rax
	cqto
	idivq -64(%rbp)
	movq %rax, -8(%rbp)
	movq $9, -72(%rbp)
	movq -8(%rbp), %r11
	movq %r11, -80(%rbp)
	movq -72(%rbp), %rax
	imulq -80(%rbp)
	movq %rax, -88(%rbp)
	movq -8(%rbp), %r11
	movq %r11, -96(%rbp)
	movq -88(%rbp), %rax
	imulq -96(%rbp)
	movq %rax, -104(%rbp)
	movq $3, -112(%rbp)
	movq -8(%rbp), %r11
	movq %r11, -120(%rbp)
	movq -112(%rbp), %rax
	imulq -120(%rbp)
	movq %rax, -128(%rbp)
	movq -104(%rbp), %r11
	addq -128(%rbp), %r11
	movq %r11, -136(%rbp)
	movq $8, -144(%rbp)
	movq -136(%rbp), %r11
	subq -144(%rbp), %r11
	movq %r11, -152(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -152(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -8(%rbp), %r11
	movq %r11, -160(%rbp)
	movq -16(%rbp), %r11
	movq %r11, -168(%rbp)
	movq -160(%rbp), %r11
	addq -168(%rbp), %r11
	movq %r11, -176(%rbp)
	movq $2, -184(%rbp)
	movq -176(%rbp), %rax
	imulq -184(%rbp)
	movq %rax, -192(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -192(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -8(%rbp), %r11
	movq %r11, -200(%rbp)
	movq $5, -208(%rbp)
	movq -208(%rbp), %r11
	negq %r11
	movq %r11, -216(%rbp)
	movq -200(%rbp), %rax
	imulq -216(%rbp)
	movq %rax, -16(%rbp)
	movq -16(%rbp), %r11
	movq %r11, -224(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -224(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq %rbp, %rsp
	popq %rbp
	xorq %rax, %rax
	retq
