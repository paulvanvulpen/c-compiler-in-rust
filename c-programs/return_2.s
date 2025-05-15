    .globl main
main:
    pushq	%rbp
    movq	%rsp, %rbp
    subq	$4, %rsp
    movl	$1, %r11d
    cmpl	$2, %r11d
    movl	$0, -4(%rbp)
    setge	-4(%rbp)
    movl	-4(%rbp), %eax
    movq	%rbp, %rsp
    popq	%rbp
    ret

.section .note.GNU-stack,"",@progbits
