(* TEST
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
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
  jb    .L115
.L117:
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
  jb    .L118
.L120:
  leaq  8(%r15), %rax
  movq  $2048, -8(%rax)
  movq  %rbx, (%rax)
  movq  %rdi, 8(%rax)
  addq  $8, %rsp
  ret
|}]


(* CR ttebbi: https://github.com/oxcaml/oxcaml/issues/1783 *)
type t = {x1: float#; x2: float#; x3: float#; x4: float#}
let spill_slot_lifetime () =
  let[@inline never] get_one () = #1. in
  let x1 = get_one () in
  let x2 = get_one () in
  let x3 = get_one () in
  let x4 = get_one () in
  {x1; x2; x3; x4}
[%%expect_asm X86_64{|
spill_slot_lifetime:
  subq  $24, %rsp
  movl  $1, %eax
  call  camlTOP3__get_one_3_5_code@PLT
.L122:
  vmovsd %xmm0, (%rsp)
  movl  $1, %eax
  call  camlTOP3__get_one_3_5_code@PLT
.L123:
  vmovsd %xmm0, 8(%rsp)
  movl  $1, %eax
  call  camlTOP3__get_one_3_5_code@PLT
.L124:
  vmovsd %xmm0, 16(%rsp)
  movl  $1, %eax
  call  camlTOP3__get_one_3_5_code@PLT
.L125:
  subq  $40, %r15
  cmpq  (%r14), %r15
  jb    .L126
.L128:
  leaq  8(%r15), %rax
  movq  $4350, -8(%rax)
  vmovsd (%rsp), %xmm1
  vmovsd %xmm1, (%rax)
  vmovsd 8(%rsp), %xmm1
  vmovsd %xmm1, 8(%rax)
  vmovsd 16(%rsp), %xmm1
  vmovsd %xmm1, 16(%rax)
  vmovsd %xmm0, 24(%rax)
  addq  $24, %rsp
  ret

spill_slot_lifetime.get_one:
  vmovsd .L133(%rip), %xmm0
  ret
|}]
