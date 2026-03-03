(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

open Intrinsics

let neg x = Float32_u.neg x
[%%expect_asm X86_64{|
neg:
  vxorps caml_negf32_mask(%rip), %xmm0, %xmm0
  ret
|}]

let add x y = Float32_u.add x y
[%%expect_asm X86_64{|
add:
  vaddss %xmm1, %xmm0, %xmm0
  ret
|}]

let sub x y = Float32_u.sub x y
[%%expect_asm X86_64{|
sub:
  vsubss %xmm1, %xmm0, %xmm0
  ret
|}]

let mul x y = Float32_u.mul x y
[%%expect_asm X86_64{|
mul:
  vmulss %xmm1, %xmm0, %xmm0
  ret
|}]

let div x y = Float32_u.div x y
[%%expect_asm X86_64{|
div:
  vdivss %xmm1, %xmm0, %xmm0
  ret
|}]

let abs x = Float32_u.abs x
[%%expect_asm X86_64{|
abs:
  vandps caml_absf32_mask(%rip), %xmm0, %xmm0
  ret
|}]

(* CR ttebbi: Should use vsqrtss instead of calling sqrtf. *)
let sqrt x = Float32_u.sqrt x
[%%expect_asm X86_64{|
sqrt:
  subq  $8, %rsp
  call  sqrtf@PLT
  addq  $8, %rsp
  ret
|}]

let of_float x = Float32_u.of_float x
[%%expect_asm X86_64{|
of_float:
  vmovsd (%rax), %xmm0
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  ret
|}]

let to_float x = Float32_u.to_float x
[%%expect_asm X86_64{|
to_float:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtss2sd %xmm0, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

let of_int x = Float32_u.of_int x
[%%expect_asm X86_64{|
of_int:
  sarq  $1, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

let to_int x = Float32_u.to_int x
[%%expect_asm X86_64{|
to_int:
  vcvttss2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: The 32bit bitmask can be turned into a boolean using either
   (x >> 30) | 1 or using the negated comparison and sign_extend(x)*2+3 *)
let eq x y = Float32_u.eq x y
[%%expect_asm X86_64{|
eq:
  vcmpss $0, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let lt x y = Float32_u.lt x y
[%%expect_asm X86_64{|
lt:
  vcmpss $1, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]
