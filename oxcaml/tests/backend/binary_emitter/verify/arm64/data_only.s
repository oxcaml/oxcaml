// Data section only - no text section

    .data
    .globl _test_data
    .p2align 3
_test_data:
    .quad _external_data
    .quad 0x123456789abcdef0
