(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 expect.opt;
*)

open Intrinsics

(* Record field access *)

type t = { x : int; y : int }

let get_x r = r.x
[%%expect_asm X86_64{|
get_x:
  movq  (%rax), %rax
  ret
|}]

let get_y r = r.y
[%%expect_asm X86_64{|
get_y:
  movq  8(%rax), %rax
  ret
|}]

type mut = { mutable a : int; mutable b : string }

let get_a r = r.a
[%%expect_asm X86_64{|
get_a:
  movq  (%rax), %rax
  ret
|}]

let set_a r v = r.a <- v
[%%expect_asm X86_64{|
set_a:
  movq  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

let set_b r v = r.b <- v
[%%expect_asm X86_64{|
set_b:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rsi
  addq  $8, %rdi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

(* Ref incr/decr *)

let incr r = incr r
[%%expect_asm X86_64{|
incr:
  addq  $2, (%rax)
  movl  $1, %eax
  ret
|}]

let decr r = decr r
[%%expect_asm X86_64{|
decr:
  addq  $-2, (%rax)
  movl  $1, %eax
  ret
|}]

(* get_header *)

let header x = get_header x
[%%expect_asm X86_64{|
header:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  -8(%rbx), %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

(* int_as_pointer *)

let as_ptr x = int_as_pointer x
[%%expect_asm X86_64{|
as_ptr:
  decq  %rax
  ret
|}]

(* Tuple access *)

let get_fst p = fst p
[%%expect_asm X86_64{|
get_fst:
  movq  (%rax), %rax
  ret
|}]

let get_snd p = snd p
[%%expect_asm X86_64{|
get_snd:
  movq  8(%rax), %rax
  ret
|}]

(* Ref operations *)

let make_ref x = ref x
[%%expect_asm X86_64{|
make_ref:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L104
.L106:
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

let deref r = !r
[%%expect_asm X86_64{|
deref:
  movq  (%rax), %rax
  ret
|}]

let assign r v = r := v
[%%expect_asm X86_64{|
assign:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rsi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

(* Ignore *)

let do_ignore x = ignore x
[%%expect_asm X86_64{|
do_ignore:
  movl  $1, %eax
  ret
|}]
