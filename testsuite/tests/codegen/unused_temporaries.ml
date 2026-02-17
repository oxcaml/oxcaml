(* TEST
 include stdlib_upstream_compatible;
 flags += "-O3";
 only-default-codegen;
 expect.opt;
*)

open Stdlib_upstream_compatible

let is_nan (x: Float_u.t) =
  Float.is_nan (Float_u.to_float x)
[%%expect{|
val is_nan : Stdlib_upstream_compatible.Float_u.t -> bool = <fun>
|}]
[%%expect_asm X86_64{|
is_nan:
  vcmpsd $4, %xmm0, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let[@inline always] compare x y =
  Float.compare (Float_u.to_float x) (Float_u.to_float y)
[%%expect{|
val compare :
  Stdlib_upstream_compatible.Float_u.t ->
  Stdlib_upstream_compatible.Float_u.t -> int = <fun>
|}]
[%%expect_asm X86_64{|
compare:
  vcmpsd $0, %xmm1, %xmm1, %xmm2
  vmovq %xmm2, %rbx
  neg   %rbx
  vcmpsd $0, %xmm0, %xmm0, %xmm2
  vmovq %xmm2, %rax
  neg   %rax
  subq  %rbx, %rax
  vcmpsd $1, %xmm1, %xmm0, %xmm2
  vmovq %xmm2, %rdi
  neg   %rdi
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rbx
  neg   %rbx
  subq  %rdi, %rbx
  addq  %rax, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]
