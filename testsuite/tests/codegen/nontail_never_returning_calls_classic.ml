(* TEST
 readonly_files = "nontail_never_returning_calls_helper.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "nontail_never_returning_calls_helper.ml";
 compile_only = "true";
 flags = "-O3";
 ocamlopt.opt;
 compile_only = "false";
 program = "nontail_never_returning_calls_helper.cmxs";
 flags = "-shared";
 all_modules = "nontail_never_returning_calls_helper.cmx";
 ocamlopt.opt;
 only-default-codegen;
 flags = "-Oclassic -inline 20 -regalloc cfg -cfg-merge-blocks";
 expect.opt;
*)

#directory "ocamlopt.opt";;
#load "nontail_never_returning_calls_helper.cmxs";;

(* Non-tail calls to a function that never returns, inlined from a module
   compiled with -O3, where they are known to never return *)

external unbox_float : float -> float# = "%unbox_float"

let is_enter (o : string option) =
  String.equal "Enter" (Nontail_never_returning_calls_helper.value_exn o)
[%%expect_asm X86_64{|
is_enter:
  subq  $8, %rsp
  testb $1, %al
  je    .L1
  movl  $1, %eax
  call  camlNontail_never_returning_calls_helper__raise_e_0_3_code@PLT
.L0:
  jmp   .L2
.L1:
  movq  (%rax), %rax
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  %rbx, %rdi
  movq  %rax, %rsi
  call  caml_string_equal@PLT
.L2:
  movq  %rax, %rbx
  addq  $8, %rsp
  ret
|}]

let get_float (o : float option) : float# =
  unbox_float (Nontail_never_returning_calls_helper.value_exn o)
[%%expect_asm X86_64{|
get_float:
  testb $1, %al
  je    .L1
  subq  $8, %rsp
  movl  $1, %eax
  call  camlNontail_never_returning_calls_helper__raise_e_0_3_code@PLT
.L0:
  movq  %rax, %rbx
  addq  $8, %rsp
  ret
.L1:
  movq  (%rax), %rax
  vmovsd (%rax), %xmm0
  ret
|}]
