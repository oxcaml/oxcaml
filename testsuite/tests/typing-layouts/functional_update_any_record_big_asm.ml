(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 readonly_files = "functional_update_any_record_big.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "functional_update_any_record_big.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags += " -O3 -I ocamlopt.opt";
 expect.opt;
*)

open Functional_update_any_record_big

(* Same representation: duplicate the record with Pduprecord, then update y. *)
let update_same (r : int big) = { r with y = 1000 }
[%%expect_asm X86_64{|
update_same:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  caml_obj_dup@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  movq  $2001, 8(%rax)
  addq  $8, %rsp
  ret
|}]

(* Changing x from an immediate to a pointer only changes its scannable axes,
   not the field layout, so this also uses Pduprecord. *)
let update_scannable_axes (r : int big) (x : string) : string big =
  { r with x }
[%%expect_asm X86_64{|
update_scannable_axes:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, (%rsp)
  movq  caml_obj_dup@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L0:
  movq  %rax, %rbx
  movq  %rbx, %rdi
  movq  (%rsp), %rsi
  call  caml_modify@PLT
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]
