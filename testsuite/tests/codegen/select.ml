(* TEST
 readonly_files = "intrinsics.ml stubs.c";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml stubs.c";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -I ocamlopt.opt";
 expect.opt;
*)


(* CR ttebbi: This use of select is the identity on the value representation *)
let select_identity x = Intrinsics.select x 1 0
[%%expect_asm X86_64{|
select_identity:
  movq  %rax, %rbx
  movl  $1, %eax
  movl  $3, %edi
  cmpq  $1, %rbx
  cmovne %rdi, %rax
  ret
|}]


(* CR ttebbi: This could use fewer instructions by flipping
   the condition and cmov registers. *)
let select_cmp (x : int) = Intrinsics.select (x > 10) x 55
[%%expect_asm X86_64{|
select_cmp:
  movq  %rax, %rbx
  movl  $111, %eax
  cmpq  $21, %rbx
  cmovg %rbx, %rax
  ret
|}]


(* CR ttebbi: We could constant-fold this. *)
let select_constant (x : int) = Intrinsics.select true x 55
[%%expect_asm X86_64{|
select_constant:
  ret
|}]
