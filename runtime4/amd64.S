/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2003 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

/* PIC mode support based on contribution by Paul Stravers (see PR#4795) */

#include "caml/m.h"

#if defined(SYS_macosx)

#define LBL(x) L##x
#define G(r) _##r
#define GREL(r) _##r@GOTPCREL
#define GCALL(r) _##r
#define TEXT_SECTION(name) .text
#define FUNCTION_ALIGN 2
#define EIGHT_ALIGN 3
#define SIXTEEN_ALIGN 4
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#elif defined(SYS_mingw64) || defined(SYS_cygwin)

#define LBL(x) .L##x
#define G(r) r
#undef  GREL
#define GCALL(r) r
#define TEXT_SECTION(name)
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define LBL(x) .L##x
#define G(r) r
#define GREL(r) r@GOTPCREL
#define GCALL(r) r@PLT
#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .type name,@function; \
        .align FUNCTION_ALIGN; \
        name:

#endif

#if defined(SYS_linux) || defined(SYS_gnu)
#define ENDFUNCTION(name) \
        .size name, . - name
#else
#define ENDFUNCTION(name)
#endif

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_OFFSET(r, n) .cfi_offset r, n
#define CFI_SAME_VALUE(r) .cfi_same_value r
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_OFFSET(r, n)
#define CFI_SAME_VALUE(r)
#endif

#ifdef WITH_FRAME_POINTERS

#define ENTER_FUNCTION \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp
#define LEAVE_FUNCTION \
        popq    %rbp; CFI_ADJUST(-8);

#else

#define ENTER_FUNCTION \
        subq    $8, %rsp; CFI_ADJUST (8);
#define LEAVE_FUNCTION \
        addq    $8, %rsp; CFI_ADJUST (-8);

#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime4/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) (8*domain_field_caml_##var)(%r14)

#if defined(__PIC__) && !defined(SYS_mingw64) && !defined(SYS_cygwin)

/* Position-independent operations on global variables. */

/* Store [srcreg] in global [dstlabel].  Clobbers %r11. */
#define STORE_VAR(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movq    srcreg, (%r11)

#define STORE_VAR32(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movl    srcreg, (%r11)

/* Load global [srclabel] in register [dstreg].  Clobbers %r11. */
#define LOAD_VAR(srclabel,dstreg) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        movq    (%r11), dstreg

/* Compare global [label] with register [reg].  Clobbers %rax. */
#define CMP_VAR(label,reg) \
        movq    GREL(label)(%rip), %rax ; \
        cmpq    (%rax), reg

/* Test 32-bit global [label] against mask [imm].  Clobbers %r11. */
#define TESTL_VAR(imm,label) \
        movq    GREL(label)(%rip), %r11 ; \
        testl   imm, (%r11)

/* Push global [label] on stack.  Clobbers %r11. */
#define PUSH_VAR(srclabel) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        pushq   (%r11); CFI_ADJUST (8)

/* Pop global [label] off stack.  Clobbers %r11. */
#define POP_VAR(dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        popq    (%r11);  CFI_ADJUST (-8)

/* Load address of global [label] in register [dst]. */
#define LEA_VAR(label,dst) \
        movq    GREL(label)(%rip), dst

#else

/* Non-PIC operations on global variables.  Slightly faster. */

#define STORE_VAR(srcreg,dstlabel) \
        movq    srcreg, G(dstlabel)(%rip)

#define STORE_VAR32(srcreg,dstlabel) \
        movl    srcreg, G(dstlabel)(%rip)

#define LOAD_VAR(srclabel,dstreg) \
        movq    G(srclabel)(%rip), dstreg

#define CMP_VAR(label,reg) \
        cmpq    G(label)(%rip), %r15

#define TESTL_VAR(imm,label) \
        testl   imm, G(label)(%rip)

#define PUSH_VAR(srclabel) \
        pushq   G(srclabel)(%rip) ; CFI_ADJUST(8)

#define POP_VAR(dstlabel) \
        popq    G(dstlabel)(%rip); CFI_ADJUST(-8)

#define LEA_VAR(label,dst) \
        leaq    G(label)(%rip), dst
#endif

/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. */

#if defined(SYS_mingw64) || defined(SYS_cygwin)

/* Win64 API: callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 */

// CR-someday mslater: support wider SIMD on Windows

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST (8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST (8); CFI_OFFSET(rbp, -24); \
                      /* Allows debugger to walk the stack */ \
        pushq   %rsi; CFI_ADJUST (8); CFI_OFFSET(rsi, -32); \
        pushq   %rdi; CFI_ADJUST (8); CFI_OFFSET(rdi, -40); \
        pushq   %r12; CFI_ADJUST (8); CFI_OFFSET(r12, -48); \
        pushq   %r13; CFI_ADJUST (8); CFI_OFFSET(r13, -56); \
        pushq   %r14; CFI_ADJUST (8); CFI_OFFSET(r14, -64); \
        pushq   %r15; CFI_ADJUST (8); CFI_OFFSET(r15, -72); \
        subq    $(8+10*16), %rsp; CFI_ADJUST (8+10*16); \
        movupd  %xmm6, 0*16(%rsp); \
        movupd  %xmm7, 1*16(%rsp); \
        movupd  %xmm8, 2*16(%rsp); \
        movupd  %xmm9, 3*16(%rsp); \
        movupd  %xmm10, 4*16(%rsp); \
        movupd  %xmm11, 5*16(%rsp); \
        movupd  %xmm12, 6*16(%rsp); \
        movupd  %xmm13, 7*16(%rsp); \
        movupd  %xmm14, 8*16(%rsp); \
        movupd  %xmm15, 9*16(%rsp)

#define POP_CALLEE_SAVE_REGS \
        movupd  0*16(%rsp), %xmm6; \
        movupd  1*16(%rsp), %xmm7; \
        movupd  2*16(%rsp), %xmm8; \
        movupd  3*16(%rsp), %xmm9; \
        movupd  4*16(%rsp), %xmm10; \
        movupd  5*16(%rsp), %xmm11; \
        movupd  6*16(%rsp), %xmm12; \
        movupd  7*16(%rsp), %xmm13; \
        movupd  8*16(%rsp), %xmm14; \
        movupd  9*16(%rsp), %xmm15; \
        addq    $(8+10*16), %rsp; CFI_ADJUST (-8-10*16); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rdi; CFI_ADJUST(-8); CFI_SAME_VALUE(rdi); \
        popq    %rsi; CFI_ADJUST(-8); CFI_SAME_VALUE(rsi); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#else

/* Unix API: callee-save regs are rbx, rbp, r12-r15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST(8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST(8); CFI_OFFSET(rbp, -24); \
        pushq   %r12; CFI_ADJUST(8); CFI_OFFSET(r12, -32); \
        pushq   %r13; CFI_ADJUST(8); CFI_OFFSET(r13, -40); \
        pushq   %r14; CFI_ADJUST(8); CFI_OFFSET(r14, -48); \
        pushq   %r15; CFI_ADJUST(8); CFI_OFFSET(r15, -56); \
        subq    $8, %rsp; CFI_ADJUST(8)

#define POP_CALLEE_SAVE_REGS \
        addq    $8, %rsp; CFI_ADJUST(-8); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#endif

#if defined(SYS_mingw64) || defined (SYS_cygwin)
   /* Calls from OCaml to C must reserve 32 bytes of extra stack space */
#  define PREPARE_FOR_C_CALL subq $32, %rsp; CFI_ADJUST(32)
#  define CLEANUP_AFTER_C_CALL addq $32, %rsp; CFI_ADJUST(-32)
   /* Stack probing mustn't be larger than the page size */
#  define STACK_PROBE_SIZE 4096
#else
#  define PREPARE_FOR_C_CALL
#  define CLEANUP_AFTER_C_CALL
#  define STACK_PROBE_SIZE 4096
#endif

/* Registers holding arguments of C functions. */

#if defined(SYS_mingw64) || defined(SYS_cygwin)
#define C_ARG_1 %rcx
#define C_ARG_2 %rdx
#define C_ARG_3 %r8
#define C_ARG_4 %r9
#else
#define C_ARG_1 %rdi
#define C_ARG_2 %rsi
#define C_ARG_3 %rdx
#define C_ARG_4 %rcx
#endif

        .text

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot__code_begin)
        .globl  G(caml_hot__code_begin)
G(caml_hot__code_begin):

        TEXT_SECTION(caml_hot__code_end)
        .globl  G(caml_hot__code_end)
G(caml_hot__code_end):
#endif

        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):
        ret  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */

/* Allocation */

#define DO_0_15(X) \
X(0) X(1) X(2) X(3) X(4) X(5) X(6) X(7) X(8) X(9) X(10) X(11) X(12) X(13) X(14) X(15)

#define DO_0_31(X) \
DO_0_15(X) \
X(16) X(17) X(18) X(19) X(20) X(21) X(22) X(23) X(24) X(25) X(26) X(27) X(27) X(29) X(30) X(31)

#define SAVE_XMM(i) movupd %xmm##i, (i*16)(%rsp);
#define SAVE_XMM_REGS                            \
        subq $(16*16), %rsp; CFI_ADJUST (16*16); \
        DO_0_15(SAVE_XMM)

#define RESTORE_XMM(i) movupd  i*16(%rsp), %xmm##i;
#define RESTORE_XMM_REGS                         \
        DO_0_15(RESTORE_XMM)                     \
        addq $(16*16), %rsp; CFI_ADJUST(-16*16);

#define SAVE_YMM(i) vmovupd %ymm##i, (i*32)(%rsp);
#define SAVE_YMM_REGS                            \
        subq $(16*32), %rsp; CFI_ADJUST (16*32); \
        DO_0_15(SAVE_YMM)

#define RESTORE_YMM(i) vmovupd i*32(%rsp), %ymm##i;
#define RESTORE_YMM_REGS                         \
        DO_0_15(RESTORE_YMM)                     \
        addq $(16*32), %rsp; CFI_ADJUST(-16*32);

#define SAVE_ZMM(i) vmovupd %zmm##i, (i*64)(%rsp);
#define SAVE_ZMM_REGS                            \
        subq $(32*64), %rsp; CFI_ADJUST (32*64); \
        DO_0_31(SAVE_ZMM)

#define RESTORE_ZMM(i) vmovupd i*64(%rsp), %zmm##i;
#define RESTORE_ZMM_REGS                         \
        DO_0_31(RESTORE_ZMM)                     \
        addq $(32*64), %rsp; CFI_ADJUST(-32*64);

#define SAVE_SIMD_REGS(simd)    SAVE_ ## simd ## _REGS
#define RESTORE_SIMD_REGS(simd) RESTORE_ ## simd ## _REGS

#ifdef WITH_FRAME_POINTERS
#define SAVE_FP         ENTER_FUNCTION;
#define RESTORE_FP      LEAVE_FUNCTION;
#else
#define SAVE_FP         pushq %rbp; CFI_ADJUST(8);
#define RESTORE_FP      popq  %rbp; CFI_ADJUST(-8);
#endif

#define Make_call_gc(suffix, simd) \
    FUNCTION(G(caml_call_gc##suffix)); \
    CFI_STARTPROC; \
LBL(caml_call_gc##suffix): \
        /* Record lowest stack address and return address. */ \
        movq    (%rsp), %r11; \
        movq    %r11, Caml_state(last_return_address); \
        leaq    8(%rsp), %r11; \
        movq    %r11, Caml_state(bottom_of_stack); \
        /* Touch the stack to trigger a recoverable segfault \
           if insufficient space remains */ \
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE); \
        movq    %r11, 0(%rsp); \
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE); \
        /* Build array of registers, save it into Caml_state->gc_regs */ \
        SAVE_FP; \
        pushq   %r11; CFI_ADJUST (8); \
        pushq   %r10; CFI_ADJUST (8); \
        pushq   %r13; CFI_ADJUST (8); \
        pushq   %r12; CFI_ADJUST (8); \
        pushq   %r9; CFI_ADJUST (8); \
        pushq   %r8; CFI_ADJUST (8); \
        pushq   %rcx; CFI_ADJUST (8); \
        pushq   %rdx; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %rbx; CFI_ADJUST (8); \
        pushq   %rax; CFI_ADJUST (8); \
        movq    %rsp, Caml_state(gc_regs); \
        /* Save young_ptr */ \
        movq    %r15, Caml_state(young_ptr); \
        /* Save floating-point registers */ \
        SAVE_SIMD_REGS(simd); \
        /* Call the garbage collector */ \
        PREPARE_FOR_C_CALL; \
        call    GCALL(caml_garbage_collection); \
        CLEANUP_AFTER_C_CALL; \
        /* Restore young_ptr */ \
        movq    Caml_state(young_ptr), %r15; \
        /* Restore all regs used by the code generator */ \
        RESTORE_SIMD_REGS(simd); \
        popq    %rax; CFI_ADJUST(-8); \
        popq    %rbx; CFI_ADJUST(-8); \
        popq    %rdi; CFI_ADJUST(-8); \
        popq    %rsi; CFI_ADJUST(-8); \
        popq    %rdx; CFI_ADJUST(-8); \
        popq    %rcx; CFI_ADJUST(-8); \
        popq    %r8; CFI_ADJUST(-8); \
        popq    %r9; CFI_ADJUST(-8); \
        popq    %r12; CFI_ADJUST(-8); \
        popq    %r13; CFI_ADJUST(-8); \
        popq    %r10; CFI_ADJUST(-8); \
        popq    %r11; CFI_ADJUST(-8); \
        RESTORE_FP; \
        /* Return to caller */ \
        ret; \
    CFI_ENDPROC; \
    ENDFUNCTION(G(caml_call_gc ## suffix))

Make_call_gc(, XMM)
Make_call_gc(_avx, YMM)
Make_call_gc(_avx512, ZMM)

#define r15_sub_words(n) subq $8+8*n, %r15
#define r15_sub_1 r15_sub_words(1)
#define r15_sub_2 r15_sub_words(2)
#define r15_sub_3 r15_sub_words(3)
#define r15_sub_N /* do nothing */

#define Make_alloc_fun(base_name, words, simd) \
FUNCTION(G(base_name ## simd)) \
CFI_STARTPROC; \
        r15_sub_ ## words; \
        cmpq    Caml_state(young_limit), %r15; \
        jb      LBL(caml_call_gc ## simd); \
        ret; \
CFI_ENDPROC; \
ENDFUNCTION(G(base_name ## simd));

#define Make_alloc_funs(words) \
Make_alloc_fun(caml_alloc ## words, words,) \
Make_alloc_fun(caml_alloc ## words, words, _avx) \
Make_alloc_fun(caml_alloc ## words, words, _avx512)

Make_alloc_funs(1) /* caml_alloc1* */
Make_alloc_funs(2) /* caml_alloc2* */
Make_alloc_funs(3) /* caml_alloc3* */
Make_alloc_funs(N) /* caml_allocN* */

#define Make_call_local_realloc(suffix, simd) \
    FUNCTION(G(caml_call_local_realloc ## suffix)); \
    CFI_STARTPROC; \
        /* Touch the stack to trigger a recoverable segfault \
           if insufficient space remains */ \
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE); \
        movq    %r11, 0(%rsp); \
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE); \
        /* Build array of registers, save it into Caml_state->gc_regs */ \
        SAVE_FP; \
        pushq   %r11; CFI_ADJUST (8); \
        pushq   %r10; CFI_ADJUST (8); \
        pushq   %r13; CFI_ADJUST (8); \
        pushq   %r12; CFI_ADJUST (8); \
        pushq   %r9; CFI_ADJUST (8); \
        pushq   %r8; CFI_ADJUST (8); \
        pushq   %rcx; CFI_ADJUST (8); \
        pushq   %rdx; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %rbx; CFI_ADJUST (8); \
        pushq   %rax; CFI_ADJUST (8); \
        movq    %rsp, Caml_state(gc_regs); \
        /* Save young_ptr */ \
        movq    %r15, Caml_state(young_ptr); \
        /* Save floating-point registers */ \
        SAVE_SIMD_REGS(simd); \
        /* Call the garbage collector */ \
        PREPARE_FOR_C_CALL; \
        call    GCALL(caml_local_realloc); \
        CLEANUP_AFTER_C_CALL; \
        /* Restore young_ptr */ \
        movq    Caml_state(young_ptr), %r15; \
        /* Restore all regs used by the code generator */ \
        RESTORE_SIMD_REGS(simd); \
        popq    %rax; CFI_ADJUST(-8); \
        popq    %rbx; CFI_ADJUST(-8); \
        popq    %rdi; CFI_ADJUST(-8); \
        popq    %rsi; CFI_ADJUST(-8); \
        popq    %rdx; CFI_ADJUST(-8); \
        popq    %rcx; CFI_ADJUST(-8); \
        popq    %r8; CFI_ADJUST(-8); \
        popq    %r9; CFI_ADJUST(-8); \
        popq    %r12; CFI_ADJUST(-8); \
        popq    %r13; CFI_ADJUST(-8); \
        popq    %r10; CFI_ADJUST(-8); \
        popq    %r11; CFI_ADJUST(-8); \
        RESTORE_FP; \
        /* Return to caller */ \
        ret; \
   CFI_ENDPROC; \
   ENDFUNCTION(G(caml_call_local_realloc ## suffix))

Make_call_local_realloc(, XMM)
Make_call_local_realloc(_avx, YMM)
Make_call_local_realloc(_avx512, ZMM)

/* Call a C function from OCaml */

FUNCTION(G(caml_c_call))
CFI_STARTPROC
LBL(caml_c_call):
    /* Record lowest stack address and return address */
        popq    Caml_state(last_return_address); CFI_ADJUST(-8)
        movq    %rsp, Caml_state(bottom_of_stack)
    /* equivalent to pushing last return address */
        subq    $8, %rsp; CFI_ADJUST(8)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE);
        movq    %rax, 0(%rsp)
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE);
    /* Make the alloc ptr available to the C code */
        movq    %r15, Caml_state(young_ptr)
    /* Call the function (address in %rax) */
    /* No need to PREPARE_FOR_C_CALL since the caller already
       reserved the stack space if needed (cf. amd64/proc.ml) */
        jmp    *%rax
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call))

#define copy_stack_correct_rsp          /* nothing */
#define copy_stack_correct_rsp_avx      andq $-32, %rsp
#define copy_stack_correct_rsp_avx512   andq $-64, %rsp

#define Make_c_call_copy_stack_args(suffix) \
    FUNCTION(G(caml_c_call_copy_stack_args ## suffix)); \
    CFI_STARTPROC; \
    /* Set up a frame pointer even without WITH_FRAME_POINTERS, \
       which we use to pop an unknown number of arguments later */ \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp; \
        .cfi_def_cfa_register 6; \
    /* Copy arguments to aligned stack */ \
        cmpq   %r13, %r12; \
        je      LBL(caml_c_call_copy_stack_args ## suffix ## _1); \
        copy_stack_correct_rsp ## suffix; \
LBL(caml_c_call_copy_stack_args ## suffix ## _0): \
        subq    $8, %r12; \
        cmpq    %r13, %r12; \
        jb      LBL(caml_c_call_copy_stack_args ## suffix ## _1); \
        push    (%r12); \
        jmp     LBL(caml_c_call_copy_stack_args ## suffix ## _0); \
LBL(caml_c_call_copy_stack_args ## suffix ## _1): \
    /* Call the function (address in %rax) */ \
        call    *%rax; \
    /* Pop arguments back off the stack */ \
        movq    %rbp, %rsp; \
        .cfi_def_cfa_register 7; \
        popq    %rbp; CFI_ADJUST(-8); \
        ret; \
    CFI_ENDPROC; \
    ENDFUNCTION(G(caml_c_call_copy_stack_args ## suffix))

Make_c_call_copy_stack_args()
Make_c_call_copy_stack_args(_avx)
Make_c_call_copy_stack_args(_avx512)

#define Make_c_call_stack_args(suffix) \
    FUNCTION(G(caml_c_call_stack_args ## suffix)); \
    CFI_STARTPROC; \
    /* Arguments: \
        C arguments         : %rdi, %rsi, %rdx, %rcx, %r8, and %r9 \
        C function          : %rax \
        C stack args        : begin=%r13 end=%r12 */ \
    /* Record lowest stack address and return address */ \
        popq    Caml_state(last_return_address); CFI_ADJUST(-8); \
        movq    %rsp, Caml_state(bottom_of_stack); \
    /* equivalent to pushing last return address */ \
        subq    $8, %rsp; CFI_ADJUST(8); \
    /* Touch the stack to trigger a recoverable segfault \
       if insufficient space remains */ \
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE); \
        movq    %rax, 0(%rsp); \
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE); \
    /* Make the alloc ptr available to the C code */ \
        movq    %r15, Caml_state(young_ptr); \
    /* Copy the arguments and call */ \
        call   (GCALL(caml_c_call_copy_stack_args ## suffix)); \
    /* Prepare for return to OCaml */ \
        movq    Caml_state(young_ptr), %r15; \
    /* Return to OCaml caller */ \
        ret; \
    CFI_ENDPROC; \
    ENDFUNCTION(G(caml_c_call_stack_args ## suffix))

Make_c_call_stack_args()
Make_c_call_stack_args(_avx)
Make_c_call_stack_args(_avx512)

/* Start the OCaml program */

FUNCTION(G(caml_start_program))
       CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Load Caml_state into r14 (was passed as an argument from C) */
        movq    C_ARG_1, %r14
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, %r12)
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Build a callback link */
        pushq   Caml_state(async_exception_pointer); CFI_ADJUST (8)
        /* Stack is 16-aligned at this point */
        pushq   Caml_state(gc_regs); CFI_ADJUST(8)
        pushq   Caml_state(last_return_address); CFI_ADJUST(8)
        pushq   Caml_state(bottom_of_stack); CFI_ADJUST(8)
    /* Setup alloc ptr */
        movq    Caml_state(young_ptr), %r15
    /* Build an exception handler */
        lea     LBL(108)(%rip), %r13
        pushq   %r13; CFI_ADJUST(8)
        pushq   Caml_state(exn_handler); CFI_ADJUST(8)
        movq    %rsp, Caml_state(exn_handler)
        movq    %rsp, Caml_state(async_exception_pointer)
    /* Call the OCaml code */
        call    *%r12
LBL(107):
    /* Pop the exception handler */
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        popq    %r12; CFI_ADJUST(-8)   /* dummy register */
LBL(109):
    /* Update alloc ptr */
        movq    %r15, Caml_state(young_ptr)
    /* Pop the callback link, restoring the global variables */
        popq    Caml_state(bottom_of_stack); CFI_ADJUST(-8)
        popq    Caml_state(last_return_address); CFI_ADJUST(-8)
        popq    Caml_state(gc_regs); CFI_ADJUST(-8)
        popq    Caml_state(async_exception_pointer); CFI_ADJUST(-8)
    /* Restore callee-save registers. */
        POP_CALLEE_SAVE_REGS
    /* Return to caller. */
        ret
LBL(108):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orq     $2, %rax
        jmp     LBL(109)
CFI_ENDPROC
ENDFUNCTION(G(caml_start_program))

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
        testq   $1, Caml_state(backtrace_active)
        jne     LBL(110)
        movq    Caml_state(exn_handler), %rsp
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        ret
LBL(110):
        movq    %rax, %r12            /* Save exception bucket */
        movq    %rax, C_ARG_1         /* arg 1: exception bucket */
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION
        movq    8(%rsp), C_ARG_2      /* arg 2: pc of raise */
        leaq    16(%rsp), C_ARG_3     /* arg 3: sp at raise */
#else
        popq    C_ARG_2               /* arg 2: pc of raise */
        movq    %rsp, C_ARG_3         /* arg 3: sp at raise */
#endif
        /* arg 4: sp of handler */
        movq    Caml_state(exn_handler), C_ARG_4
        /* PR#5700: thanks to popq above, stack is now 16-aligned */
        /* Thanks to ENTER_FUNCTION, stack is now 16-aligned */
        PREPARE_FOR_C_CALL            /* no need to cleanup after */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    Caml_state(exn_handler), %rsp
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exn))

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
        movq    C_ARG_1, %r14   /* Caml_state */
        testq   $1, Caml_state(backtrace_active)
        jne     LBL(112)
        movq    C_ARG_2, %rax
        movq    Caml_state(exn_handler), %rsp  /* Cut stack */
        /* Recover previous exception handler */
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        movq    Caml_state(young_ptr), %r15 /* Reload alloc ptr */
        ret
LBL(112):
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION          ;
#endif
        /* Save exception bucket. Caml_state in r14 saved across C calls. */
        movq    C_ARG_2, %r12
        /* arg 1: exception bucket */
        movq    C_ARG_2, C_ARG_1
        /* arg 2: pc of raise */
        movq    Caml_state(last_return_address), C_ARG_2
        /* arg 3: sp of raise */
        movq    Caml_state(bottom_of_stack), C_ARG_3
        /* arg 4: sp of handler */
        movq    Caml_state(exn_handler), C_ARG_4
#ifndef WITH_FRAME_POINTERS
        subq    $8, %rsp              /* PR#5700: maintain stack alignment */
#endif
        PREPARE_FOR_C_CALL            /* no need to cleanup after */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    Caml_state(exn_handler), %rsp
     /* Recover previous exception handler */
        popq    Caml_state(exn_handler); CFI_ADJUST(-8)
        movq    Caml_state(young_ptr), %r15 /* Reload alloc ptr */
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exception))

/* Raise a Stack_overflow exception on return from segv_handler()
   (in runtime/signals_nat.c).  On entry, the stack is full, so we
   cannot record a backtrace.
   No CFI information here since this function disrupts the stack
   backtrace anyway. */

FUNCTION(G(caml_stack_overflow))
        movq    C_ARG_1, %r14                 /* Caml_state */
        LEA_VAR(caml_exn_Stack_overflow, %rax)
        movq    Caml_state(async_exception_pointer), %rsp /* cut the stack */
     /* Recover previous exn handler */
        popq    Caml_state(exn_handler)
        ret                                   /* jump to handler's code */
ENDFUNCTION(G(caml_stack_overflow))

/* Callback from C to OCaml */

FUNCTION(G(caml_callback_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rbx      /* closure */
        movq    0(C_ARG_3), %rax   /* argument */
        movq    0(%rbx), %r12      /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback_asm))

FUNCTION(G(caml_callback2_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rdi      /* closure */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        LEA_VAR(caml_apply2L, %r12)/* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback2_asm))

FUNCTION(G(caml_callback3_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        movq    C_ARG_2, %rsi      /* closure */
        movq    16(C_ARG_3), %rdi  /* third argument */
        LEA_VAR(caml_apply3L, %r12)/* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback3_asm))

FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        LEA_VAR(caml_array_bound_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_bound_error))

FUNCTION(G(caml_ml_array_align_error))
CFI_STARTPROC
        LEA_VAR(caml_array_align_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_align_error))

        TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
        .align  EIGHT_ALIGN
G(caml_system__frametable):
        .quad   1           /* one descriptor */
        .4byte   LBL(107) - .    /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN

#if defined(SYS_macosx)
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section .rdata,"dr"
#else
        .section    .rodata.cst16,"aM",@progbits,16
#endif
        .globl  G(caml_negf_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf_mask):
        .quad   0x8000000000000000, 0
        .globl  G(caml_absf_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf_mask):
        .quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF
        .globl  G(caml_negf32_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf32_mask):
        .quad   0x80000000, 0
        .globl  G(caml_absf32_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf32_mask):
        .quad   0xFFFFFFFF7FFFFFFFL, 0xFFFFFFFFFFFFFFFF

#if defined(SYS_linux)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
