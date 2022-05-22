	.globl main
	.text
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $1656, %rsp
	movabsq $10, %rax
	movq %rax, -8(%rbp)
	movabsq $20, %rax
	movq %rax, -16(%rbp)
	movq -8(%rbp), %r8
	movq %r8, -24(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -32(%rbp)
	movq -24(%rbp), %r8
	subq -32(%rbp), %r8
	movq %r8, -24(%rbp)
	movq -24(%rbp),%rax
	cmpq $0, %rax
	jz .L11
	jmp .L10
.L11:
	movq -8(%rbp), %r8
	movq %r8, -40(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -48(%rbp)
	movq -40(%rbp), %r8
	subq -48(%rbp), %r8
	movq %r8, -40(%rbp)
	movq -40(%rbp),%rax
	cmpq $0, %rax
	jz .L2
	jmp .L10
.L10:
	movq -8(%rbp), %r8
	movq %r8, -56(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -64(%rbp)
	movq -56(%rbp), %r8
	subq -64(%rbp), %r8
	movq %r8, -56(%rbp)
	movq -56(%rbp),%rax
	cmpq $0, %rax
	jnz .L9
	jmp .L2
.L9:
	movq -8(%rbp), %r8
	movq %r8, -72(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -80(%rbp)
	movq -72(%rbp), %r8
	subq -80(%rbp), %r8
	movq %r8, -72(%rbp)
	movq -72(%rbp),%rax
	cmpq $0, %rax
	jnz .L8
	jmp .L2
.L8:
	movq -8(%rbp), %r8
	movq %r8, -88(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -96(%rbp)
	movq -88(%rbp), %r8
	subq -96(%rbp), %r8
	movq %r8, -88(%rbp)
	movq -88(%rbp),%rax
	cmpq $0, %rax
	jz .L2
	jmp .L7
.L7:
	movq -8(%rbp), %r8
	movq %r8, -104(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -112(%rbp)
	movq -104(%rbp), %r8
	subq -112(%rbp), %r8
	movq %r8, -104(%rbp)
	movq -104(%rbp),%rax
	cmpq $0, %rax
	jz .L13
	jmp .L6
.L13:
	movq -8(%rbp), %r8
	movq %r8, -120(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -128(%rbp)
	movq -120(%rbp), %r8
	subq -128(%rbp), %r8
	movq %r8, -120(%rbp)
	movq -120(%rbp),%rax
	cmpq $0, %rax
	jnz .L12
	jmp .L6
.L12:
	movq -8(%rbp), %r8
	movq %r8, -136(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -144(%rbp)
	movq -136(%rbp), %r8
	subq -144(%rbp), %r8
	movq %r8, -136(%rbp)
	movq -136(%rbp),%rax
	cmpq $0, %rax
	jnz .L14
	jmp .L15
.L15:
	movq -8(%rbp), %r8
	movq %r8, -152(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -160(%rbp)
	movq -152(%rbp), %r8
	subq -160(%rbp), %r8
	movq %r8, -152(%rbp)
	movq -152(%rbp),%rax
	cmpq $0, %rax
	jz .L14
	jmp .L2
.L14:
	movq -8(%rbp), %r8
	movq %r8, -168(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -176(%rbp)
	movq -168(%rbp), %r8
	subq -176(%rbp), %r8
	movq %r8, -168(%rbp)
	movq -168(%rbp),%rax
	cmpq $0, %rax
	jnz .L6
	jmp .L16
.L16:
	movq -8(%rbp), %r8
	movq %r8, -184(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -192(%rbp)
	movq -184(%rbp), %r8
	subq -192(%rbp), %r8
	movq %r8, -184(%rbp)
	movq -184(%rbp),%rax
	cmpq $0, %rax
	jz .L6
	jmp .L2
.L6:
	movq -8(%rbp), %r8
	movq %r8, -200(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -208(%rbp)
	movq -200(%rbp), %r8
	subq -208(%rbp), %r8
	movq %r8, -200(%rbp)
	movq -200(%rbp),%rax
	cmpq $0, %rax
	jnz .L22
	jmp .L5
.L22:
	movq -8(%rbp), %r8
	movq %r8, -216(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -224(%rbp)
	movq -216(%rbp), %r8
	subq -224(%rbp), %r8
	movq %r8, -216(%rbp)
	movq -216(%rbp),%rax
	cmpq $0, %rax
	jnz .L23
	jmp .L21
.L23:
	movq -8(%rbp), %r8
	movq %r8, -232(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -240(%rbp)
	movq -232(%rbp), %r8
	subq -240(%rbp), %r8
	movq %r8, -232(%rbp)
	movq -232(%rbp),%rax
	cmpq $0, %rax
	jnz .L5
	jmp .L21
.L21:
	movq -8(%rbp), %r8
	movq %r8, -248(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -256(%rbp)
	movq -248(%rbp), %r8
	subq -256(%rbp), %r8
	movq %r8, -248(%rbp)
	movq -248(%rbp),%rax
	cmpq $0, %rax
	jnz .L24
	jmp .L20
.L24:
	movq -8(%rbp), %r8
	movq %r8, -264(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -272(%rbp)
	movq -264(%rbp), %r8
	subq -272(%rbp), %r8
	movq %r8, -264(%rbp)
	movq -264(%rbp),%rax
	cmpq $0, %rax
	jz .L5
	jmp .L20
.L20:
	movq -8(%rbp), %r8
	movq %r8, -280(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -288(%rbp)
	movq -280(%rbp), %r8
	subq -288(%rbp), %r8
	movq %r8, -280(%rbp)
	movq -280(%rbp),%rax
	cmpq $0, %rax
	jnz .L5
	jmp .L19
.L19:
	movq -8(%rbp), %r8
	movq %r8, -296(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -304(%rbp)
	movq -296(%rbp), %r8
	subq -304(%rbp), %r8
	movq %r8, -296(%rbp)
	movq -296(%rbp),%rax
	cmpq $0, %rax
	jz .L5
	jmp .L18
.L18:
	movq -8(%rbp), %r8
	movq %r8, -312(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -320(%rbp)
	movq -312(%rbp), %r8
	subq -320(%rbp), %r8
	movq %r8, -312(%rbp)
	movq -312(%rbp),%rax
	cmpq $0, %rax
	jz .L25
	jmp .L26
.L26:
	movq -8(%rbp), %r8
	movq %r8, -328(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -336(%rbp)
	movq -328(%rbp), %r8
	subq -336(%rbp), %r8
	movq %r8, -328(%rbp)
	movq -328(%rbp),%rax
	cmpq $0, %rax
	jnz .L25
	jmp .L17
.L25:
	movq -8(%rbp), %r8
	movq %r8, -344(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -352(%rbp)
	movq -344(%rbp), %r8
	subq -352(%rbp), %r8
	movq %r8, -344(%rbp)
	movq -344(%rbp),%rax
	cmpq $0, %rax
	jz .L5
	jmp .L27
.L27:
	movq -8(%rbp), %r8
	movq %r8, -360(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -368(%rbp)
	movq -360(%rbp), %r8
	subq -368(%rbp), %r8
	movq %r8, -360(%rbp)
	movq -360(%rbp),%rax
	cmpq $0, %rax
	jnz .L5
	jmp .L17
.L17:
	movq -8(%rbp), %r8
	movq %r8, -376(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -384(%rbp)
	movq -376(%rbp), %r8
	subq -384(%rbp), %r8
	movq %r8, -376(%rbp)
	movq -376(%rbp),%rax
	cmpq $0, %rax
	jnz .L5
	jmp .L2
.L5:
	movq -8(%rbp), %r8
	movq %r8, -392(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -400(%rbp)
	movq -392(%rbp), %r8
	subq -400(%rbp), %r8
	movq %r8, -392(%rbp)
	movq -392(%rbp),%rax
	cmpq $0, %rax
	jnz .L29
	jmp .L2
.L29:
	movq -8(%rbp), %r8
	movq %r8, -408(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -416(%rbp)
	movq -408(%rbp), %r8
	subq -416(%rbp), %r8
	movq %r8, -408(%rbp)
	movq -408(%rbp),%rax
	cmpq $0, %rax
	jnz .L28
	jmp .L2
.L28:
	movq -8(%rbp), %r8
	movq %r8, -424(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -432(%rbp)
	movq -424(%rbp), %r8
	subq -432(%rbp), %r8
	movq %r8, -424(%rbp)
	movq -424(%rbp),%rax
	cmpq $0, %rax
	jnz .L31
	jmp .L4
.L31:
	movq -8(%rbp), %r8
	movq %r8, -440(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -448(%rbp)
	movq -440(%rbp), %r8
	subq -448(%rbp), %r8
	movq %r8, -440(%rbp)
	movq -440(%rbp),%rax
	cmpq $0, %rax
	jnz .L30
	jmp .L4
.L30:
	movq -8(%rbp), %r8
	movq %r8, -456(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -464(%rbp)
	movq -456(%rbp), %r8
	subq -464(%rbp), %r8
	movq %r8, -456(%rbp)
	movq -456(%rbp),%rax
	cmpq $0, %rax
	jnz .L4
	jmp .L2
.L4:
	movq -8(%rbp), %r8
	movq %r8, -472(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -480(%rbp)
	movq -472(%rbp), %r8
	subq -480(%rbp), %r8
	movq %r8, -472(%rbp)
	movq -472(%rbp),%rax
	cmpq $0, %rax
	jnz .L35
	jmp .L3
.L35:
	movq -8(%rbp), %r8
	movq %r8, -488(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -496(%rbp)
	movq -488(%rbp), %r8
	subq -496(%rbp), %r8
	movq %r8, -488(%rbp)
	movq -488(%rbp),%rax
	cmpq $0, %rax
	jz .L34
	jmp .L3
.L34:
	movq -8(%rbp), %r8
	movq %r8, -504(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -512(%rbp)
	movq -504(%rbp), %r8
	subq -512(%rbp), %r8
	movq %r8, -504(%rbp)
	movq -504(%rbp),%rax
	cmpq $0, %rax
	jnz .L33
	jmp .L3
.L33:
	movq -8(%rbp), %r8
	movq %r8, -520(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -528(%rbp)
	movq -520(%rbp), %r8
	subq -528(%rbp), %r8
	movq %r8, -520(%rbp)
	movq -520(%rbp),%rax
	cmpq $0, %rax
	jz .L32
	jmp .L3
.L32:
	movq -8(%rbp), %r8
	movq %r8, -536(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -544(%rbp)
	movq -536(%rbp), %r8
	subq -544(%rbp), %r8
	movq %r8, -536(%rbp)
	movq -536(%rbp),%rax
	cmpq $0, %rax
	jnz .L36
	jmp .L2
.L36:
	movq -8(%rbp), %r8
	movq %r8, -552(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -560(%rbp)
	movq -552(%rbp), %r8
	subq -560(%rbp), %r8
	movq %r8, -552(%rbp)
	movq -552(%rbp),%rax
	cmpq $0, %rax
	jnz .L3
	jmp .L2
.L3:
	movq -8(%rbp), %r8
	movq %r8, -568(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -576(%rbp)
	movq -568(%rbp), %r8
	subq -576(%rbp), %r8
	movq %r8, -568(%rbp)
	movq -568(%rbp),%rax
	cmpq $0, %rax
	jnz .L42
	jmp .L44
.L44:
	movq -8(%rbp), %r8
	movq %r8, -584(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -592(%rbp)
	movq -584(%rbp), %r8
	subq -592(%rbp), %r8
	movq %r8, -584(%rbp)
	movq -584(%rbp),%rax
	cmpq $0, %rax
	jz .L42
	jmp .L43
.L43:
	movq -8(%rbp), %r8
	movq %r8, -600(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -608(%rbp)
	movq -600(%rbp), %r8
	subq -608(%rbp), %r8
	movq %r8, -600(%rbp)
	movq -600(%rbp),%rax
	cmpq $0, %rax
	jnz .L37
	jmp .L46
.L46:
	movq -8(%rbp), %r8
	movq %r8, -616(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -624(%rbp)
	movq -616(%rbp), %r8
	subq -624(%rbp), %r8
	movq %r8, -616(%rbp)
	movq -616(%rbp),%rax
	cmpq $0, %rax
	jnz .L37
	jmp .L45
.L45:
	movq -8(%rbp), %r8
	movq %r8, -632(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -640(%rbp)
	movq -632(%rbp), %r8
	subq -640(%rbp), %r8
	movq %r8, -632(%rbp)
	movq -632(%rbp),%rax
	cmpq $0, %rax
	jnz .L42
	jmp .L37
.L42:
	movq -8(%rbp), %r8
	movq %r8, -648(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -656(%rbp)
	movq -648(%rbp), %r8
	subq -656(%rbp), %r8
	movq %r8, -648(%rbp)
	movq -648(%rbp),%rax
	cmpq $0, %rax
	jz .L49
	jmp .L48
.L49:
	movq -8(%rbp), %r8
	movq %r8, -664(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -672(%rbp)
	movq -664(%rbp), %r8
	subq -672(%rbp), %r8
	movq %r8, -664(%rbp)
	movq -664(%rbp),%rax
	cmpq $0, %rax
	jz .L47
	jmp .L48
.L48:
	movq -8(%rbp), %r8
	movq %r8, -680(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -688(%rbp)
	movq -680(%rbp), %r8
	subq -688(%rbp), %r8
	movq %r8, -680(%rbp)
	movq -680(%rbp),%rax
	cmpq $0, %rax
	jnz .L50
	jmp .L41
.L50:
	movq -8(%rbp), %r8
	movq %r8, -696(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -704(%rbp)
	movq -696(%rbp), %r8
	subq -704(%rbp), %r8
	movq %r8, -696(%rbp)
	movq -696(%rbp),%rax
	cmpq $0, %rax
	jz .L47
	jmp .L41
.L47:
	movq -8(%rbp), %r8
	movq %r8, -712(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -720(%rbp)
	movq -712(%rbp), %r8
	subq -720(%rbp), %r8
	movq %r8, -712(%rbp)
	movq -712(%rbp),%rax
	cmpq $0, %rax
	jnz .L51
	jmp .L37
.L51:
	movq -8(%rbp), %r8
	movq %r8, -728(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -736(%rbp)
	movq -728(%rbp), %r8
	subq -736(%rbp), %r8
	movq %r8, -728(%rbp)
	movq -728(%rbp),%rax
	cmpq $0, %rax
	jnz .L41
	jmp .L37
.L41:
	movq -8(%rbp), %r8
	movq %r8, -744(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -752(%rbp)
	movq -744(%rbp), %r8
	subq -752(%rbp), %r8
	movq %r8, -744(%rbp)
	movq -744(%rbp),%rax
	cmpq $0, %rax
	jnz .L37
	jmp .L52
.L52:
	movq -8(%rbp), %r8
	movq %r8, -760(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -768(%rbp)
	movq -760(%rbp), %r8
	subq -768(%rbp), %r8
	movq %r8, -760(%rbp)
	movq -760(%rbp),%rax
	cmpq $0, %rax
	jnz .L40
	jmp .L53
.L53:
	movq -8(%rbp), %r8
	movq %r8, -776(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -784(%rbp)
	movq -776(%rbp), %r8
	subq -784(%rbp), %r8
	movq %r8, -776(%rbp)
	movq -776(%rbp),%rax
	cmpq $0, %rax
	jz .L40
	jmp .L37
.L40:
	movq -8(%rbp), %r8
	movq %r8, -792(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -800(%rbp)
	movq -792(%rbp), %r8
	subq -800(%rbp), %r8
	movq %r8, -792(%rbp)
	movq -792(%rbp),%rax
	cmpq $0, %rax
	jz .L55
	jmp .L56
.L56:
	movq -8(%rbp), %r8
	movq %r8, -808(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -816(%rbp)
	movq -808(%rbp), %r8
	subq -816(%rbp), %r8
	movq %r8, -808(%rbp)
	movq -808(%rbp),%rax
	cmpq $0, %rax
	jnz .L55
	jmp .L37
.L55:
	movq -8(%rbp), %r8
	movq %r8, -824(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -832(%rbp)
	movq -824(%rbp), %r8
	subq -832(%rbp), %r8
	movq %r8, -824(%rbp)
	movq -824(%rbp),%rax
	cmpq $0, %rax
	jz .L54
	jmp .L37
.L54:
	movq -8(%rbp), %r8
	movq %r8, -840(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -848(%rbp)
	movq -840(%rbp), %r8
	subq -848(%rbp), %r8
	movq %r8, -840(%rbp)
	movq -840(%rbp),%rax
	cmpq $0, %rax
	jnz .L39
	jmp .L37
.L39:
	movq -8(%rbp), %r8
	movq %r8, -856(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -864(%rbp)
	movq -856(%rbp), %r8
	subq -864(%rbp), %r8
	movq %r8, -856(%rbp)
	movq -856(%rbp),%rax
	cmpq $0, %rax
	jz .L60
	jmp .L38
.L60:
	movq -8(%rbp), %r8
	movq %r8, -872(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -880(%rbp)
	movq -872(%rbp), %r8
	subq -880(%rbp), %r8
	movq %r8, -872(%rbp)
	movq -872(%rbp),%rax
	cmpq $0, %rax
	jnz .L59
	jmp .L38
.L59:
	movq -8(%rbp), %r8
	movq %r8, -888(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -896(%rbp)
	movq -888(%rbp), %r8
	subq -896(%rbp), %r8
	movq %r8, -888(%rbp)
	movq -888(%rbp),%rax
	cmpq $0, %rax
	jz .L38
	jmp .L58
.L58:
	movq -8(%rbp), %r8
	movq %r8, -904(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -912(%rbp)
	movq -904(%rbp), %r8
	subq -912(%rbp), %r8
	movq %r8, -904(%rbp)
	movq -904(%rbp),%rax
	cmpq $0, %rax
	jnz .L38
	jmp .L57
.L57:
	movq -8(%rbp), %r8
	movq %r8, -920(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -928(%rbp)
	movq -920(%rbp), %r8
	subq -928(%rbp), %r8
	movq %r8, -920(%rbp)
	movq -920(%rbp),%rax
	cmpq $0, %rax
	jnz .L37
	jmp .L38
.L38:
	movq -8(%rbp), %r8
	movq %r8, -936(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -944(%rbp)
	movq -936(%rbp), %r8
	subq -944(%rbp), %r8
	movq %r8, -936(%rbp)
	movq -936(%rbp),%rax
	cmpq $0, %rax
	jnz .L0
	jmp .L61
.L61:
	movq -8(%rbp), %r8
	movq %r8, -952(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -960(%rbp)
	movq -952(%rbp), %r8
	subq -960(%rbp), %r8
	movq %r8, -952(%rbp)
	movq -952(%rbp),%rax
	cmpq $0, %rax
	jz .L37
	jmp .L62
.L62:
	movq -8(%rbp), %r8
	movq %r8, -968(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -976(%rbp)
	movq -968(%rbp), %r8
	subq -976(%rbp), %r8
	movq %r8, -968(%rbp)
	movq -968(%rbp),%rax
	cmpq $0, %rax
	jz .L37
	jmp .L0
.L37:
	movq -8(%rbp), %r8
	movq %r8, -984(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -992(%rbp)
	movq -984(%rbp), %r8
	subq -992(%rbp), %r8
	movq %r8, -984(%rbp)
	movq -984(%rbp),%rax
	cmpq $0, %rax
	jz .L66
	jmp .L64
.L66:
	movq -8(%rbp), %r8
	movq %r8, -1000(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1008(%rbp)
	movq -1000(%rbp), %r8
	subq -1008(%rbp), %r8
	movq %r8, -1000(%rbp)
	movq -1000(%rbp),%rax
	cmpq $0, %rax
	jz .L64
	jmp .L65
.L65:
	movq -8(%rbp), %r8
	movq %r8, -1016(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1024(%rbp)
	movq -1016(%rbp), %r8
	subq -1024(%rbp), %r8
	movq %r8, -1016(%rbp)
	movq -1016(%rbp),%rax
	cmpq $0, %rax
	jz .L64
	jmp .L63
.L64:
	movq -8(%rbp), %r8
	movq %r8, -1032(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1040(%rbp)
	movq -1032(%rbp), %r8
	subq -1040(%rbp), %r8
	movq %r8, -1032(%rbp)
	movq -1032(%rbp),%rax
	cmpq $0, %rax
	jnz .L67
	jmp .L2
.L67:
	movq -8(%rbp), %r8
	movq %r8, -1048(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1056(%rbp)
	movq -1048(%rbp), %r8
	subq -1056(%rbp), %r8
	movq %r8, -1048(%rbp)
	movq -1048(%rbp),%rax
	cmpq $0, %rax
	jnz .L68
	jmp .L63
.L68:
	movq -8(%rbp), %r8
	movq %r8, -1064(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1072(%rbp)
	movq -1064(%rbp), %r8
	subq -1072(%rbp), %r8
	movq %r8, -1064(%rbp)
	movq -1064(%rbp),%rax
	cmpq $0, %rax
	jz .L2
	jmp .L63
.L63:
	movq -8(%rbp), %r8
	movq %r8, -1080(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1088(%rbp)
	movq -1080(%rbp), %r8
	subq -1088(%rbp), %r8
	movq %r8, -1080(%rbp)
	movq -1080(%rbp),%rax
	cmpq $0, %rax
	jnz .L69
	jmp .L71
.L71:
	movq -8(%rbp), %r8
	movq %r8, -1096(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1104(%rbp)
	movq -1096(%rbp), %r8
	subq -1104(%rbp), %r8
	movq %r8, -1096(%rbp)
	movq -1096(%rbp),%rax
	cmpq $0, %rax
	jnz .L69
	jmp .L70
.L70:
	movq -8(%rbp), %r8
	movq %r8, -1112(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1120(%rbp)
	movq -1112(%rbp), %r8
	subq -1120(%rbp), %r8
	movq %r8, -1112(%rbp)
	movq -1112(%rbp),%rax
	cmpq $0, %rax
	jnz .L72
	jmp .L0
.L72:
	movq -8(%rbp), %r8
	movq %r8, -1128(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1136(%rbp)
	movq -1128(%rbp), %r8
	subq -1136(%rbp), %r8
	movq %r8, -1128(%rbp)
	movq -1128(%rbp),%rax
	cmpq $0, %rax
	jz .L69
	jmp .L0
.L69:
	movq -8(%rbp), %r8
	movq %r8, -1144(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1152(%rbp)
	movq -1144(%rbp), %r8
	subq -1152(%rbp), %r8
	movq %r8, -1144(%rbp)
	movq -1144(%rbp),%rax
	cmpq $0, %rax
	jnz .L75
	jmp .L74
.L75:
	movq -8(%rbp), %r8
	movq %r8, -1160(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1168(%rbp)
	movq -1160(%rbp), %r8
	subq -1168(%rbp), %r8
	movq %r8, -1160(%rbp)
	movq -1160(%rbp),%rax
	cmpq $0, %rax
	jz .L2
	jmp .L74
.L74:
	movq -8(%rbp), %r8
	movq %r8, -1176(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1184(%rbp)
	movq -1176(%rbp), %r8
	subq -1184(%rbp), %r8
	movq %r8, -1176(%rbp)
	movq -1176(%rbp),%rax
	cmpq $0, %rax
	jnz .L2
	jmp .L73
.L73:
	movq -8(%rbp), %r8
	movq %r8, -1192(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1200(%rbp)
	movq -1192(%rbp), %r8
	subq -1200(%rbp), %r8
	movq %r8, -1192(%rbp)
	movq -1192(%rbp),%rax
	cmpq $0, %rax
	jnz .L2
	jmp .L0
.L2:
	movq -8(%rbp), %r8
	movq %r8, -1208(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1216(%rbp)
	movq -1208(%rbp), %r8
	subq -1216(%rbp), %r8
	movq %r8, -1208(%rbp)
	movq -1208(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L84
.L84:
	movq -8(%rbp), %r8
	movq %r8, -1224(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1232(%rbp)
	movq -1224(%rbp), %r8
	subq -1232(%rbp), %r8
	movq %r8, -1224(%rbp)
	movq -1224(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L83
.L83:
	movq -8(%rbp), %r8
	movq %r8, -1240(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1248(%rbp)
	movq -1240(%rbp), %r8
	subq -1248(%rbp), %r8
	movq %r8, -1240(%rbp)
	movq -1240(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L82
.L82:
	movq -8(%rbp), %r8
	movq %r8, -1256(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1264(%rbp)
	movq -1256(%rbp), %r8
	subq -1264(%rbp), %r8
	movq %r8, -1256(%rbp)
	movq -1256(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L81
.L81:
	movq -8(%rbp), %r8
	movq %r8, -1272(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1280(%rbp)
	movq -1272(%rbp), %r8
	subq -1280(%rbp), %r8
	movq %r8, -1272(%rbp)
	movq -1272(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L80
.L80:
	movq -8(%rbp), %r8
	movq %r8, -1288(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1296(%rbp)
	movq -1288(%rbp), %r8
	subq -1296(%rbp), %r8
	movq %r8, -1288(%rbp)
	movq -1288(%rbp),%rax
	cmpq $0, %rax
	jz .L76
	jmp .L79
.L79:
	movq -8(%rbp), %r8
	movq %r8, -1304(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1312(%rbp)
	movq -1304(%rbp), %r8
	subq -1312(%rbp), %r8
	movq %r8, -1304(%rbp)
	movq -1304(%rbp),%rax
	cmpq $0, %rax
	jz .L78
	jmp .L76
.L78:
	movq -8(%rbp), %r8
	movq %r8, -1320(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1328(%rbp)
	movq -1320(%rbp), %r8
	subq -1328(%rbp), %r8
	movq %r8, -1320(%rbp)
	movq -1320(%rbp),%rax
	cmpq $0, %rax
	jnz .L85
	jmp .L86
.L86:
	movq -8(%rbp), %r8
	movq %r8, -1336(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1344(%rbp)
	movq -1336(%rbp), %r8
	subq -1344(%rbp), %r8
	movq %r8, -1336(%rbp)
	movq -1336(%rbp),%rax
	cmpq $0, %rax
	jz .L85
	jmp .L77
.L85:
	movq -8(%rbp), %r8
	movq %r8, -1352(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1360(%rbp)
	movq -1352(%rbp), %r8
	subq -1360(%rbp), %r8
	movq %r8, -1352(%rbp)
	movq -1352(%rbp),%rax
	cmpq $0, %rax
	jz .L77
	jmp .L76
.L77:
	movq -8(%rbp), %r8
	movq %r8, -1368(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1376(%rbp)
	movq -1368(%rbp), %r8
	subq -1376(%rbp), %r8
	movq %r8, -1368(%rbp)
	movq -1368(%rbp),%rax
	cmpq $0, %rax
	jnz .L87
	jmp .L88
.L88:
	movq -8(%rbp), %r8
	movq %r8, -1384(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1392(%rbp)
	movq -1384(%rbp), %r8
	subq -1392(%rbp), %r8
	movq %r8, -1384(%rbp)
	movq -1384(%rbp),%rax
	cmpq $0, %rax
	jnz .L87
	jmp .L1
.L87:
	movq -8(%rbp), %r8
	movq %r8, -1400(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1408(%rbp)
	movq -1400(%rbp), %r8
	subq -1408(%rbp), %r8
	movq %r8, -1400(%rbp)
	movq -1400(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L89
.L89:
	movq -8(%rbp), %r8
	movq %r8, -1416(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1424(%rbp)
	movq -1416(%rbp), %r8
	subq -1424(%rbp), %r8
	movq %r8, -1416(%rbp)
	movq -1416(%rbp),%rax
	cmpq $0, %rax
	jnz .L76
	jmp .L1
.L76:
	movq -8(%rbp), %r8
	movq %r8, -1432(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1440(%rbp)
	movq -1432(%rbp), %r8
	subq -1440(%rbp), %r8
	movq %r8, -1432(%rbp)
	movq -1432(%rbp),%rax
	cmpq $0, %rax
	jz .L90
	jmp .L93
.L93:
	movq -8(%rbp), %r8
	movq %r8, -1448(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1456(%rbp)
	movq -1448(%rbp), %r8
	subq -1456(%rbp), %r8
	movq %r8, -1448(%rbp)
	movq -1448(%rbp),%rax
	cmpq $0, %rax
	jnz .L90
	jmp .L92
.L92:
	movq -8(%rbp), %r8
	movq %r8, -1464(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1472(%rbp)
	movq -1464(%rbp), %r8
	subq -1472(%rbp), %r8
	movq %r8, -1464(%rbp)
	movq -1464(%rbp),%rax
	cmpq $0, %rax
	jnz .L91
	jmp .L94
.L94:
	movq -8(%rbp), %r8
	movq %r8, -1480(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1488(%rbp)
	movq -1480(%rbp), %r8
	subq -1488(%rbp), %r8
	movq %r8, -1480(%rbp)
	movq -1480(%rbp),%rax
	cmpq $0, %rax
	jz .L91
	jmp .L90
.L91:
	movq -8(%rbp), %r8
	movq %r8, -1496(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1504(%rbp)
	movq -1496(%rbp), %r8
	subq -1504(%rbp), %r8
	movq %r8, -1496(%rbp)
	movq -1496(%rbp),%rax
	cmpq $0, %rax
	jnz .L90
	jmp .L0
.L90:
	movq -8(%rbp), %r8
	movq %r8, -1512(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1520(%rbp)
	movq -1512(%rbp), %r8
	subq -1520(%rbp), %r8
	movq %r8, -1512(%rbp)
	movq -1512(%rbp),%rax
	cmpq $0, %rax
	jnz .L95
	jmp .L99
.L99:
	movq -8(%rbp), %r8
	movq %r8, -1528(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1536(%rbp)
	movq -1528(%rbp), %r8
	subq -1536(%rbp), %r8
	movq %r8, -1528(%rbp)
	movq -1528(%rbp),%rax
	cmpq $0, %rax
	jnz .L95
	jmp .L98
.L98:
	movq -8(%rbp), %r8
	movq %r8, -1544(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1552(%rbp)
	movq -1544(%rbp), %r8
	subq -1552(%rbp), %r8
	movq %r8, -1544(%rbp)
	movq -1544(%rbp),%rax
	cmpq $0, %rax
	jnz .L97
	jmp .L100
.L100:
	movq -8(%rbp), %r8
	movq %r8, -1560(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1568(%rbp)
	movq -1560(%rbp), %r8
	subq -1568(%rbp), %r8
	movq %r8, -1560(%rbp)
	movq -1560(%rbp),%rax
	cmpq $0, %rax
	jnz .L97
	jmp .L95
.L97:
	movq -8(%rbp), %r8
	movq %r8, -1576(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1584(%rbp)
	movq -1576(%rbp), %r8
	subq -1584(%rbp), %r8
	movq %r8, -1576(%rbp)
	movq -1576(%rbp),%rax
	cmpq $0, %rax
	jnz .L96
	jmp .L95
.L96:
	movq -8(%rbp), %r8
	movq %r8, -1592(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1600(%rbp)
	movq -1592(%rbp), %r8
	subq -1600(%rbp), %r8
	movq %r8, -1592(%rbp)
	movq -1592(%rbp),%rax
	cmpq $0, %rax
	jnz .L1
	jmp .L95
.L95:
	movq -8(%rbp), %r8
	movq %r8, -1608(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1616(%rbp)
	movq -1608(%rbp), %r8
	subq -1616(%rbp), %r8
	movq %r8, -1608(%rbp)
	movq -1608(%rbp),%rax
	cmpq $0, %rax
	jnz .L101
	jmp .L0
.L101:
	movq -8(%rbp), %r8
	movq %r8, -1624(%rbp)
	movq -16(%rbp), %r8
	movq %r8, -1632(%rbp)
	movq -1624(%rbp), %r8
	subq -1632(%rbp), %r8
	movq %r8, -1624(%rbp)
	movq -1624(%rbp),%rax
	cmpq $0, %rax
	jz .L1
	jmp .L0
.L0:
	movabsq $42, %rax
	movq %rax, -1640(%rbp)
	movq -1640(%rbp), %rdi
	callq bx_print_int
	jmp .L102
.L1:
	movabsq $42, %rax
	movq %rax, -1648(%rbp)
	movq -1648(%rbp), %r8
	negq %r8
	movq %r8, -1656(%rbp)
	movq -1656(%rbp), %rdi
	callq bx_print_int
.L102:
	movq %rbp, %rsp 
	popq %rbp 
	movq $0, %rax
	retq
