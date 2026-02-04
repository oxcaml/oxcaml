// Reference assembly file for binary emitter verification tests
// Contains: text section, data section, text relocations, data relocations

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

    .data
    .globl _test_data
    .p2align 3
_test_data:
    .quad _external_data
    .quad 0x123456789abcdef0
