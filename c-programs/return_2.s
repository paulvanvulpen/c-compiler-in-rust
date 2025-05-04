    .globl main
main:
    pushq	%rbp
    movq	%rsp, %rbp
    subq	$20, %rsp
    movl	$80, -4(%rbp)
    sarl	$2, -4(%rbp)
    movl	$7, -8(%rbp)
    sall	$1, -8(%rbp)
    movl	$5, -12(%rbp)
    movl	-8(%rbp), %r10d
    andl	%r10d, -12(%rbp)
    movl	$1, -16(%rbp)
    movl	-12(%rbp), %r10d
    xorl	%r10d, -16(%rbp)
    movl	-4(%rbp), %r10d
    movl	%r10d, -20(%rbp)
    movl	-16(%rbp), %r10d
    orl 	%r10d, -20(%rbp)
    movl	-20(%rbp), %eax
    movq	%rbp, %rsp
    popq	%rbp
    ret

.section .note.GNU-stack,"",@progbits
