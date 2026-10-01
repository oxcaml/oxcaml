(* TEST
 readonly_files = "intrinsics.ml stubs.c";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml stubs.c";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-cse-join-points";
 expect.opt;
*)

(* On amd64, [Cfg_cse] does not rematerialize an integer constant that does not
   fit in a 32-bit immediate: loading it takes a 10-byte [movabs], so a register
   that already holds it is reused instead (see [CSE.is_cheap_operation] in
   [backend/amd64]). Constants that fit in 32 bits stay cheap and are reloaded
   rather than kept live. *)

open Intrinsics

(* Straight-line code: the sentinel [min_int] is compared against twice, for
   two conditional moves, and is loaded once. *)
let sentinel_twice x y =
  Builtins.select (x = min_int) 0 x + Builtins.select (y = min_int) 70000000 y
[%%expect_asm X86_64{|
sentinel_twice:
  movl  $140000001, %edx
  movabsq $-9223372036854775807, %rsi
  movq  %rbx, %rdi
  cmpq  %rsi, %rbx
  cmove %rdx, %rdi
  movl  $1, %edx
  movq  %rax, %rbx
  cmpq  %rsi, %rax
  cmove %rdx, %rbx
  leaq  -1(%rbx,%rdi), %rax
  ret
|}]

(* The 32-bit constant [70000000] is needed twice as well, but reloading it is
   as short as copying it, so it is not shared. *)
let small_constant_twice x y =
  Builtins.select (x = min_int) 70000000 x
  + Builtins.select (y = min_int) 70000000 y
[%%expect_asm X86_64{|
small_constant_twice:
  movl  $140000001, %edx
  movabsq $-9223372036854775807, %rsi
  movq  %rbx, %rdi
  cmpq  %rsi, %rbx
  cmove %rdx, %rdi
  movl  $140000001, %edx
  movq  %rax, %rbx
  cmpq  %rsi, %rax
  cmove %rdx, %rbx
  leaq  -1(%rbx,%rdi), %rax
  ret
|}]

(* Across a join point: the sentinel loaded before the first branch is still
   available after it. *)
let sentinel_across_join x y =
  (if x = min_int then 0 else x) + (if y = min_int then 70000000 else y)
[%%expect_asm X86_64{|
sentinel_across_join:
  movabsq $-9223372036854775807, %rdi
  cmpq  %rdi, %rbx
  jne   .L0
  movl  $140000001, %ebx
  cmpq  %rdi, %rax
  jne   .L2
  jmp   .L1
.L0:
  cmpq  %rdi, %rax
  jne   .L2
.L1:
  movl  $1, %eax
.L2:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]

(* The same, at source level: the defaulting function is inlined twice, so the
   sentinel is compared against on both sides of a join point. The bindings are
   local because the toplevel compiles each item on its own and does not inline
   across items. *)
let combine a b =
  let absent = min_int in
  let or_default t ~default = if t = absent then default else t in
  or_default a ~default:0 + or_default b ~default:12345678
[%%expect_asm X86_64{|
combine:
  movabsq $-9223372036854775807, %rdi
  cmpq  %rdi, %rbx
  jne   .L0
  movl  $24691357, %ebx
  cmpq  %rdi, %rax
  jne   .L2
  jmp   .L1
.L0:
  cmpq  %rdi, %rax
  jne   .L2
.L1:
  movl  $1, %eax
.L2:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]
