	.globl main
	.text
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $88, %rsp
	movabsq $20, %rax
	movq %rax, -8(%rbp)
	movabsq $0, %rax
	movq %rax, -16(%rbp)
	movabsq $1, %rax
	movq %rax, -24(%rbp)
	movabsq $0, %rax
	movq %rax, -32(%rbp)
.L0:
	movq -8(%rbp), %r8
	movq %r8, -40(%rbp)
	movabsq $0, %rax
	movq %rax, -48(%rbp)
	movq -40(%rbp), %r8
	subq -48(%rbp), %r8
	movq %r8, -40(%rbp)
	movq -40(%rbp),%rax
	cmpq $0, %rax
	jnle .L1
	jmp .L2
.L1:
	movq -8(%rbp), %r8
	movq %r8, -56(%rbp)
	movabsq $1, %rax
	movq %rax, -64(%rbp)
	movq -56(%rbp), %r8
	subq -64(%rbp), %r8
	movq %r8, -8(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -72(%rbp)
	movq -72(%rbp), %rdi
	callq bx_print_int
	movq -16(%rbp), %r8
	movq %r8, -80(%rbp)
	movq -24(%rbp), %r8
	movq %r8, -88(%rbp)
	movq -80(%rbp), %r8
	addq -88(%rbp), %r8
	movq %r8, -32(%rbp)
	movq -24(%rbp), %r8
	movq %r8, -16(%rbp)
	movq -32(%rbp), %r8
	movq %r8, -24(%rbp)
	jmp .L0
.L2:
	movq %rbp, %rsp 
	popq %rbp 
	movq $0, %rax
	retq
