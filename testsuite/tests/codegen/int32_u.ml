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

(* CR ttebbi: We should use 32bit instructions. If we change the register
   representation of 32bit values, we could also remove the sign extension.
*)
let add x y = Int32_u.add x y
[%%expect_asm X86_64{|
add:
  addq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

(* CR ttebbi: Unnecessary moves at the beginning. *)
let min x y = Int32_u.min x y
[%%expect_asm X86_64{|
min:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jg    .L102
  movq  %rdi, %rax
  ret
.L102:
  ret
|}]

let bswap x = Int32_u.bswap x
[%%expect_asm X86_64{|
bswap:
  bswap %eax
  movslq %eax, %rax
  ret
|}]

(* CR ttebbi: Subtraction should be done on byte registers. *)
let compare x y = Int32_u.compare x y
[%%expect_asm X86_64{|
compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: This should be right-shift by 1 followed by sign extension. *)
let of_int x = Int32_u.of_int x
[%%expect_asm X86_64{|
of_int:
  salq  $31, %rax
  sarq  $32, %rax
  ret
|}]

let to_int x = Int32_u.to_int x
[%%expect_asm X86_64{|
to_int:
  leaq  1(%rax,%rax), %rax
  ret
|}]
