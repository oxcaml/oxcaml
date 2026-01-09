// Text section only - no data section

    .text
    .globl _test_func
    .p2align 2
_test_func:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    mov x0, #42
    bl _external_func
    ldp x29, x30, [sp], #16
    ret
