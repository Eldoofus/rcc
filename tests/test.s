	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$72, %rsp
	movq	$42, %rax
	cqo
	movq	$13, %r10
	idivq	%r10
	movq	%rdx, -8(%rbp)
	movq	$21, -16(%rbp)
	negq	-16(%rbp)
	movq	-16(%rbp), %r10
	movq	%r10, -24(%rbp)
	notq	-24(%rbp)
	movq	$12, %rax
	cqo
	movq	$4, %r10
	idivq	%r10
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %r10
	movq	%r10, -40(%rbp)
	movq	-32(%rbp), %r10
	addq	%r10, -40(%rbp)
	movq	-8(%rbp), %r10
	movq	%r10, -48(%rbp)
	movq	-48(%rbp), %r11
	imulq	-40(%rbp), %r11
	movq	%r11, -48(%rbp)
	movq	$1, %r11
	cmpq	$0, %r11
	jne	.L1
	movq	$37, %r11
	cmpq	$73, %r11
	movq	$0, -56(%rbp)
	setge	-56(%rbp)
	cmpq	$0, -56(%rbp)
	je	.L1
	movq	$0, -64(%rbp)
	jmp	.L2
	.L1:
	movq	$1, -64(%rbp)
	.L2:
	movq	-48(%rbp), %r10
	movq	%r10, -72(%rbp)
	movq	-72(%rbp), %r11
	imulq	-64(%rbp), %r11
	movq	%r11, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.section	.note.GNU-stack,"",@progbits

