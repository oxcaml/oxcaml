(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 flags += " -g -gdwarf-inlined-frames";
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
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rdi
  movq  $2048, -8(%rdi)
  movq  %rbx, (%rdi)
  movq  $1, 8(%rdi)
  cmpq  $1, %rax
  jne   .L1
  movq  %rdi, %rax
  addq  $8, %rsp
  ret
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2048, -8(%rax)
  movq  %rbx, (%rax)
  movq  %rdi, 8(%rax)
  addq  $8, %rsp
  ret
|}, Arm64{|
one_or_two_element_list:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  ldr   x16, [x28, #0]
  sub   x27, x27, #24
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L1:
  add   x2, x27, #8
  orr   x3, xzr, #2048
  str   x3, [x2, #-8]
  str   x1, [x2, #0]
  orr   x3, xzr, #1
  str   x3, [x2, #8]
  subs  xzr, x0, #1
  b.ne  .L2
  orr   x0, xzr, x2
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L2:
  ldr   x16, [x28, #0]
  sub   x27, x27, #24
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L3:
  add   x0, x27, #8
  orr   x3, xzr, #2048
  str   x3, [x0, #-8]
  str   x1, [x0, #0]
  str   x2, [x0, #8]
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L4:
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
  call  camlTOP3__get_one_2_5_code@PLT
.L0:
  vmovsd %xmm0, (%rsp)
  movl  $1, %eax
  call  camlTOP3__get_one_2_5_code@PLT
.L1:
  vmovsd %xmm0, 8(%rsp)
  movl  $1, %eax
  call  camlTOP3__get_one_2_5_code@PLT
.L2:
  vmovsd %xmm0, 16(%rsp)
  movl  $1, %eax
  call  camlTOP3__get_one_2_5_code@PLT
.L3:
  subq  $40, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L4:
  leaq  8(%r15), %rax
  movabsq $72057594037932032, %rbx
  movq  %rbx, -8(%rax)
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
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
|}, Arm64{|
spill_slot_lifetime:
  sub   sp, sp, #32
  str   lr, [sp, #24]
.L0:
  orr   x0, xzr, #1
  bl    camlTOP3__get_one_2_5_code
.L1:
  str   d0, [sp, #0]
  orr   x0, xzr, #1
  bl    camlTOP3__get_one_2_5_code
.L2:
  str   d0, [sp, #8]
  orr   x0, xzr, #1
  bl    camlTOP3__get_one_2_5_code
.L3:
  str   d0, [sp, #16]
  orr   x0, xzr, #1
  bl    camlTOP3__get_one_2_5_code
.L4:
  ldr   x16, [x28, #0]
  sub   x27, x27, #40
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L5:
  add   x0, x27, #8
  movz  x1, #4096, lsl #0
  movk  x1, #256, lsl #48
  str   x1, [x0, #-8]
  ldr   d1, [sp, #0]
  str   d1, [x0, #0]
  ldr   d1, [sp, #8]
  str   d1, [x0, #8]
  ldr   d1, [sp, #16]
  str   d1, [x0, #16]
  str   d0, [x0, #24]
  ldr   lr, [sp, #24]
  add   sp, sp, #32
  ret
.L6:

spill_slot_lifetime.get_one:
  fmov  d0, #1.0000000
  ret
|}]
