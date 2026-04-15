(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 expect.opt;
*)

open Intrinsics

let add x y = Nativeint_u.add x y
[%%expect_asm X86_64{|
add:
  addq  %rbx, %rax
  ret
|}]

let sub x y = Nativeint_u.sub x y
[%%expect_asm X86_64{|
sub:
  subq  %rbx, %rax
  ret
|}]

(* CR ttebbi: This should use the neg instruction. *)
let neg x = Nativeint_u.neg x
[%%expect_asm X86_64{|
neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

let mul x y = Nativeint_u.mul x y
[%%expect_asm X86_64{|
mul:
  imulq %rbx, %rax
  ret
|}]

let logand x y = Nativeint_u.logand x y
[%%expect_asm X86_64{|
logand:
  andq  %rbx, %rax
  ret
|}]

let logor x y = Nativeint_u.logor x y
[%%expect_asm X86_64{|
logor:
  orq   %rbx, %rax
  ret
|}]

let logxor x y = Nativeint_u.logxor x y
[%%expect_asm X86_64{|
logxor:
  xorq  %rbx, %rax
  ret
|}]

(* CR ttebbi: We could use sarx / sarxl to save the move and relax register
   constraints. *)
let shift_left x y = Nativeint_u.shift_left x (Nativeint_u.to_int y)
[%%expect_asm X86_64{|
shift_left:
  movq  %rbx, %rcx
  salq  %cl, %rax
  ret
|}]

let shift_right x y = Nativeint_u.shift_right x (Nativeint_u.to_int y)
[%%expect_asm X86_64{|
shift_right:
  movq  %rbx, %rcx
  sarq  %cl, %rax
  ret
|}]

let of_int x = Nativeint_u.of_int x
[%%expect_asm X86_64{|
of_int:
  sarq  $1, %rax
  ret
|}]

let to_int x = Nativeint_u.to_int x
[%%expect_asm X86_64{|
to_int:
  leaq  1(%rax,%rax), %rax
  ret
|}]

let of_int32 x = Nativeint_u.of_int32 (Int32_u.to_int32 x)
[%%expect_asm X86_64{|
of_int32:
  ret
|}]

let to_int32 x = Int32_u.of_int32 (Nativeint_u.to_int32 x)
[%%expect_asm X86_64{|
to_int32:
  movslq %eax, %rax
  ret
|}]

let bswap x = Nativeint_u.bswap x
[%%expect_asm X86_64{|
bswap:
  bswap %rax
  ret
|}]
