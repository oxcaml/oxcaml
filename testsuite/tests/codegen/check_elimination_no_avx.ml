(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 flags += " -fno-avx";
 arch_amd64;
 only-default-codegen;
 expect.opt;
*)

(* CR ttebbi: Without AVX, the destructive [cmpsd] clobbers one of the input
   registers, so we refrain from folding it into the branch. With proper SSA,
   this will be straightforward to optimize.
*)
let fold_float_compare_into_branch (z : int) (x : float) (y : float) t f =
  let b = z = 0 && x < y in
  if b then t () else f ()
[%%expect_asm X86_64{|
fold_float_compare_into_branch:
  movq  %rbx, %rcx
  movq  %rdx, %rbx
  cmpq  $1, %rax
  jne   .L0
  movsd (%rdi), %xmm1
  movsd (%rcx), %xmm0
  cmpltsd %xmm1, %xmm0
  movq  %xmm0, %rax
  neg   %rax
  testq %rax, %rax
  je    .L0
  movl  $1, %eax
  movq  (%rsi), %rdi
  movq  %rsi, %rbx
  jmp   *%rdi
.L0:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]
