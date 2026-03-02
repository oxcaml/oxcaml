(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

(* CR ttebbi: We could do a single allocation and reset the bump pointer in the
    smaller case.
*)
let one_or_two_element_list b x = let l = [x] in if b then x :: l else l
[%%expect_asm X86_64{|
one_or_two_element_list:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L110
.L112:
  leaq  8(%r15), %rdi
  movq  $2048, -8(%rdi)
  movq  %rbx, (%rdi)
  movq  $1, 8(%rdi)
  cmpq  $1, %rax
  jne   .L106
  movq  %rdi, %rax
  addq  $8, %rsp
  ret
.L106:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L113
.L115:
  leaq  8(%r15), %rax
  movq  $2048, -8(%rax)
  movq  %rbx, (%rax)
  movq  %rdi, 8(%rax)
  addq  $8, %rsp
  ret
|}]
