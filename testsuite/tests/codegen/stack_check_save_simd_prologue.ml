(* TEST
 flags += " -O3";
 only-stack-checks-codegen;
 expect.opt;
*)

(* The unboxed float [x] is live (in an xmm register) on entry to the [else]
   block, and dies at that block's first "real" instruction. The prologue is
   shrink-wrapped into that block (the [then] branch needs no frame), and the
   block is also the lowest common dominator of the two call-containing
   sub-branches, so the stack check is inserted at its start, before the
   [Prologue] instruction and before the last use of [x]. The stack-realloc
   handler must therefore preserve xmm registers: it must call
   [caml_call_realloc_stack_sse], not the plain [caml_call_realloc_stack].
   This is a regression test for [Cfg_stack_checks] recovering the block's
   live-in by skipping the [Prologue] instruction, whose [live] field does not
   account for registers dying at the following instruction. *)

external to_float : float# -> float = "%box_float"

let[@inline never] g (n : int) : float = float_of_int n

let[@inline never] f (x : float#) (b : bool) (n : int) : float =
  if b
  then 0.0
  else
    let y = to_float x +. to_float x in
    if n > 0 then y +. g n else y -. g (n + 1)
[%%expect_asm_full X86_64{|
f:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  $1, %rdi
  jne   .L6
  pushq %r10
  leaq  -384(%rsp), %r10
  cmpq  40(%r14), %r10
  popq  %r10
  jb    .L11
.L0:
  subq  $8, %rsp
  vaddsd %xmm0, %xmm0, %xmm0
  cmpq  $1, %rax
  jle   .L3
  vmovsd %xmm0, (%rsp)
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  24(%rbx), %rbx
  movq  (%rbx), %rdi
  call  *%rdi
.L1:
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L9
.L2:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rsp), %xmm0
  vaddsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L3:
  vmovsd %xmm0, (%rsp)
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  24(%rbx), %rbx
  addq  $2, %rax
  movq  (%rbx), %rdi
  call  *%rdi
.L4:
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L7
.L5:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rsp), %xmm0
  vsubsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L6:
  leaq  <hidden PC-relative offset>(%rip), %rax
  ret
.L7:
  call  .Lcaml_call_gc_
.L8:
  jmp   .L5
.L9:
  call  .Lcaml_call_gc_
.L10:
  jmp   .L2
.L11:
  push  $34
  call  caml_call_realloc_stack_sse@PLT
  addq  $8, %rsp
  jmp   .L0
|}]
