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

(* CR ttebbi: We could use lea to shorten the write barrier calling sequence. *)
let set_b r v = r.b <- v
[%%expect_asm X86_64{|
set_b:
  subq  $8, %rsp
  leaq  8(%rax), %rdi
  movq  %rbx, %rsi
  call  caml_modify@PLT
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

(* Ref incr/decr *)

let do_incr r = incr r
[%%expect_asm X86_64{|
do_incr:
  addq  $2, (%rax)
  movl  $1, %eax
  ret
|}]

let do_decr r = decr r
[%%expect_asm X86_64{|
do_decr:
  addq  $-2, (%rax)
  movl  $1, %eax
  ret
|}]

(* get_header *)

let header x = get_header x
[%%expect_asm X86_64{|
header:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L108
.L110:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movq  -8(%rax), %rax
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
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
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L106
.L108:
  leaq  8(%r15), %rbx
  movq  $1024, -8(%rbx)
  movq  %rax, (%rbx)
  movq  %rbx, %rax
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

(* Records with unboxed fields *)

type unboxed_int64 = { a : int; b : int64#; c : int }

let get_unboxed_int64 (r : unboxed_int64) = r.b
[%%expect_asm X86_64{|
get_unboxed_int64:
  movq  16(%rax), %rax
  ret
|}]

let get_after_unboxed (r : unboxed_int64) = r.c
[%%expect_asm X86_64{|
get_after_unboxed:
  movq  8(%rax), %rax
  ret
|}]

type unboxed_float = { x : float#; y : int }

let get_unboxed_float (r : unboxed_float) = r.x
[%%expect_asm X86_64{|
get_unboxed_float:
  vmovsd 8(%rax), %xmm0
  ret
|}]

let get_after_float (r : unboxed_float) = r.y
[%%expect_asm X86_64{|
get_after_float:
  movq  (%rax), %rax
  ret
|}]

type unboxed_int32 = { i : int32#; j : int }

let get_unboxed_int32 (r : unboxed_int32) = r.i
[%%expect_asm X86_64{|
get_unboxed_int32:
  movslq 8(%rax), %rax
  ret
|}]

type mutable_unboxed = { mutable p : int64#; q : int }

let get_mut_unboxed (r : mutable_unboxed) = r.p
[%%expect_asm X86_64{|
get_mut_unboxed:
  movq  8(%rax), %rax
  ret
|}]

let set_mut_unboxed (r : mutable_unboxed) (v : int64#) = r.p <- v
[%%expect_asm X86_64{|
set_mut_unboxed:
  movq  %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]
