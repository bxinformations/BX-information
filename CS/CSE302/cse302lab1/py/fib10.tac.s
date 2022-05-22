	.section .rodata
.lprintfmt:
	.string "%ld\n"
	.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $120, %rsp
	movq $0, -8(%rbp)
	movq -8(%rbp), %r11
	movq %r11, -16(%rbp)
	movq $0, -24(%rbp)
	movq -24(%rbp), %r11
	movq %r11, -32(%rbp)
	movq $0, -40(%rbp)
	movq -40(%rbp), %r11
	movq %r11, -48(%rbp)
	movq $1, -56(%rbp)
	movq -56(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -64(%rbp)
	movq -64(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -72(%rbp)
	movq -72(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -80(%rbp)
	movq -80(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -88(%rbp)
	movq -88(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -96(%rbp)
	movq -96(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -104(%rbp)
	movq -104(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -112(%rbp)
	movq -112(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq -16(%rbp), %r11
	addq -32(%rbp), %r11
	movq %r11, -120(%rbp)
	movq -120(%rbp), %r11
	movq %r11, -48(%rbp)
	movq -32(%rbp), %r11
	movq %r11, -16(%rbp)
	movq -48(%rbp), %r11
	movq %r11, -32(%rbp)
	leaq .lprintfmt(%rip), %rdi
	movq -16(%rbp), %rsi
	xorq %rax, %rax
	callq printf@PLT
	movq %rbp, %rsp
	popq %rbp
	xorq %rax, %rax
	retq
