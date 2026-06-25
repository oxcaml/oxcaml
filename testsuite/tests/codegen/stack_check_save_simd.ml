(* TEST
 flags += " -O3";
 only-stack-checks-codegen;
 expect.opt;
*)

(* Reproduction of a [Cfg_stack_checks] codegen bug.

   The float [x] is live (in an xmm register) across the non-tail recursive
   call, hence across the stack check inserted at the start of the block. The
   stack-realloc handler should therefore preserve xmm registers. But
   [Cfg_stack_checks] gives the inserted [Stack_check] an empty [live] set, so
   [save_simd] sees no live SIMD register and the [.L9] trailer below calls the
   plain [caml_call_realloc_stack] (which saves no xmm/ymm/zmm) instead of
   [caml_call_realloc_stack_sse]. The realloc handler may then clobber [x].

   [%%expect_asm_full] is used (rather than [%%expect_asm]) so that the
   captured assembly includes the cold stack-realloc handler. *)
let[@inline never] rec f (a : float array) (n : int) : float =
  let x = Array.unsafe_get a 0 in
  if n = 0 then x
  else
    let r = f a (n - 1) in
    x +. r
[%%expect_asm_full X86_64{|
f:
  subq  $8, %rsp
  vmovsd (%rax), %xmm0
  cmpq  $1, %rbx
  jne   .L1
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L7
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L1:
  pushq %r10
  leaq  -376(%rsp), %r10
  cmpq  40(%r14), %r10
  popq  %r10
  jb    .L9
.L2:
  vmovsd %xmm0, (%rsp)
  addq  $-2, %rbx
  call  camlTOP1__f_0_1_code@PLT
.L3:
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L5
.L4:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rsp), %xmm0
  vaddsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L5:
  call  .Lcaml_call_gc_
.L6:
  jmp   .L4
.L7:
  call  .Lcaml_call_gc_sse_
.L8:
  jmp   .L0
.L9:
  push  $34
  call  caml_call_realloc_stack@PLT
  addq  $8, %rsp
  jmp   .L2
|}]
