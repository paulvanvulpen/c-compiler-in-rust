    .globl main
main:
    pushq	%rbp
    movq	%rsp, %rbp
    subq	$192, %rsp
    movl	$250, -4(%rbp)
    movl	$200, -8(%rbp)
    movl	$100, -12(%rbp)
    movl	$75, -16(%rbp)
    movl	$50, -20(%rbp)
    movl	$25, -24(%rbp)
    movl	$10, -28(%rbp)
    movl	$1, -32(%rbp)
    movl	$0, -36(%rbp)
    movl	$0, -40(%rbp)
    movl	-20(%rbp), %r10d
    movl	%r10d, -16(%rbp)
    movl	-4(%rbp), %r10d
    movl	%r10d, -44(%rbp)
    movl	-16(%rbp), %r10d
    andl	%r10d, -44(%rbp)
    movl	-44(%rbp), %r10d
    movl	%r10d, -4(%rbp)
    movl	-44(%rbp), %r10d
    movl	%r10d, -40(%rbp)
    cmpl	$40, -4(%rbp)
    movl	$0, -48(%rbp)
    sete	-48(%rbp)
    movl	-48(%rbp), %r10d
    movl	%r10d, -52(%rbp)
    cmpl	$0, -52(%rbp)
    je	.Lfalse16
    cmpl	$21800, -8(%rbp)
    movl	$0, -56(%rbp)
    sete	-56(%rbp)
    movl	-56(%rbp), %r10d
    movl	%r10d, -60(%rbp)
    cmpl	$0, -60(%rbp)
    je	.Lfalse16
    movl	$1, -64(%rbp)
    jmp	.Lend17
    .Lfalse16:
    movl	$0, -64(%rbp)
    .Lend17:
    movl	-64(%rbp), %r10d
    movl	%r10d, -68(%rbp)
    cmpl	$0, -68(%rbp)
    je	.Lfalse14
    cmpl	$109, -12(%rbp)
    movl	$0, -72(%rbp)
    sete	-72(%rbp)
    movl	-72(%rbp), %r10d
    movl	%r10d, -76(%rbp)
    cmpl	$0, -76(%rbp)
    je	.Lfalse14
    movl	$1, -80(%rbp)
    jmp	.Lend15
    .Lfalse14:
    movl	$0, -80(%rbp)
    .Lend15:
    movl	-80(%rbp), %r10d
    movl	%r10d, -84(%rbp)
    cmpl	$0, -84(%rbp)
    je	.Lfalse12
    cmpl	$41, -16(%rbp)
    movl	$0, -88(%rbp)
    sete	-88(%rbp)
    movl	-88(%rbp), %r10d
    movl	%r10d, -92(%rbp)
    cmpl	$0, -92(%rbp)
    je	.Lfalse12
    movl	$1, -96(%rbp)
    jmp	.Lend13
    .Lfalse12:
    movl	$0, -96(%rbp)
    .Lend13:
    movl	-96(%rbp), %r10d
    movl	%r10d, -100(%rbp)
    cmpl	$0, -100(%rbp)
    je	.Lfalse10
    cmpl	$41, -20(%rbp)
    movl	$0, -104(%rbp)
    sete	-104(%rbp)
    movl	-104(%rbp), %r10d
    movl	%r10d, -108(%rbp)
    cmpl	$0, -108(%rbp)
    je	.Lfalse10
    movl	$1, -112(%rbp)
    jmp	.Lend11
    .Lfalse10:
    movl	$0, -112(%rbp)
    .Lend11:
    movl	-112(%rbp), %r10d
    movl	%r10d, -116(%rbp)
    cmpl	$0, -116(%rbp)
    je	.Lfalse8
    cmpl	$27, -24(%rbp)
    movl	$0, -120(%rbp)
    sete	-120(%rbp)
    movl	-120(%rbp), %r10d
    movl	%r10d, -124(%rbp)
    cmpl	$0, -124(%rbp)
    je	.Lfalse8
    movl	$1, -128(%rbp)
    jmp	.Lend9
    .Lfalse8:
    movl	$0, -128(%rbp)
    .Lend9:
    movl	-128(%rbp), %r10d
    movl	%r10d, -132(%rbp)
    cmpl	$0, -132(%rbp)
    je	.Lfalse6
    cmpl	$2, -28(%rbp)
    movl	$0, -136(%rbp)
    sete	-136(%rbp)
    movl	-136(%rbp), %r10d
    movl	%r10d, -140(%rbp)
    cmpl	$0, -140(%rbp)
    je	.Lfalse6
    movl	$1, -144(%rbp)
    jmp	.Lend7
    .Lfalse6:
    movl	$0, -144(%rbp)
    .Lend7:
    movl	-144(%rbp), %r10d
    movl	%r10d, -148(%rbp)
    cmpl	$0, -148(%rbp)
    je	.Lfalse4
    cmpl	$2, -32(%rbp)
    movl	$0, -152(%rbp)
    sete	-152(%rbp)
    movl	-152(%rbp), %r10d
    movl	%r10d, -156(%rbp)
    cmpl	$0, -156(%rbp)
    je	.Lfalse4
    movl	$1, -160(%rbp)
    jmp	.Lend5
    .Lfalse4:
    movl	$0, -160(%rbp)
    .Lend5:
    movl	-160(%rbp), %r10d
    movl	%r10d, -164(%rbp)
    cmpl	$0, -164(%rbp)
    je	.Lfalse2
    cmpl	$1, -36(%rbp)
    movl	$0, -168(%rbp)
    sete	-168(%rbp)
    movl	-168(%rbp), %r10d
    movl	%r10d, -172(%rbp)
    cmpl	$0, -172(%rbp)
    je	.Lfalse2
    movl	$1, -176(%rbp)
    jmp	.Lend3
    .Lfalse2:
    movl	$0, -176(%rbp)
    .Lend3:
    movl	-176(%rbp), %r10d
    movl	%r10d, -180(%rbp)
    cmpl	$0, -180(%rbp)
    je	.Lfalse0
    cmpl	$40, -40(%rbp)
    movl	$0, -184(%rbp)
    sete	-184(%rbp)
    movl	-184(%rbp), %r10d
    movl	%r10d, -188(%rbp)
    cmpl	$0, -188(%rbp)
    je	.Lfalse0
    movl	$1, -192(%rbp)
    jmp	.Lend1
    .Lfalse0:
    movl	$0, -192(%rbp)
    .Lend1:
    movl	-192(%rbp), %eax
    movq	%rbp, %rsp
    popq	%rbp
    ret
    movl	$0, %eax
    movq	%rbp, %rsp
    popq	%rbp
    ret

.section .note.GNU-stack,"",@progbits
