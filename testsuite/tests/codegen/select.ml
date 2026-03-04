(* TEST
 readonly_files = "intrinsics.ml stubs.c";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml stubs.c";
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


(* CR ttebbi: This use of select is the identity on the value representation *)
let select_identity x = Builtins.select x 1 0
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
let select_cmp (x : int) = Builtins.select (x > 10) x 55
[%%expect_asm X86_64{|
select_cmp:
  movq  %rax, %rbx
  movl  $111, %eax
  cmpq  $21, %rbx
  cmovg %rbx, %rax
  ret
|}]


(* CR ttebbi: We could constant-fold this. *)
let select_constant (x : int) = Builtins.select true x 55
[%%expect_asm X86_64{|
select_constant:
  ret
|}]


(* CR ttebbi: Unnecessary sign extension. *)
let select_int32 b (x : int32#) (y : int32#) =
  Builtins.select_int32 b x y
[%%expect_asm X86_64{|
select_int32:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movslq %edi, %rax
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_int64 b (x : int64#) (y : int64#) =
  Builtins.select_int64 b x y
[%%expect_asm X86_64{|
select_int64:
  movq  %rax, %rsi
  movq  %rdi, %rax
  cmpq  $1, %rsi
  cmovne %rbx, %rax
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_nativeint b (x : nativeint#) (y : nativeint#) =
  Builtins.select_nativeint b x y
[%%expect_asm X86_64{|
select_nativeint:
  movq  %rax, %rsi
  movq  %rdi, %rax
  cmpq  $1, %rsi
  cmovne %rbx, %rax
  ret
|}]

(* CR ttebbi: We should not materialize a boolean. *)
let repeated_select_shared x y z w  a b =
  let c = (Int64_u.to_int64 x) < (Int64_u.to_int64 y) in
  let q = Builtins.select_int64 c z w in
  let r = Builtins.select_int64 c a b in
  #(q,r)
[%%expect_asm X86_64{|
repeated_select_shared:
  movq  %rbx, %r8
  movq  %rcx, %rbx
  cmpq  %r8, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rcx
  movq  %rsi, %rax
  cmpq  $1, %rcx
  cmovne %rdi, %rax
  cmpq  $1, %rcx
  cmovne %rdx, %rbx
  ret
|}]

(* CR ttebbi: We should not materialize the boolean, ideally even share the cmpq. *)
let repeated_select_repeated x y z w  a b =
  let q =
    Builtins.select_int64 ((Int64_u.to_int64 x) < (Int64_u.to_int64 y)) z w
  in
  let r =
    Builtins.select_int64 ((Int64_u.to_int64 x) < (Int64_u.to_int64 y)) a b
  in
  #(q,r)
[%%expect_asm X86_64{|
repeated_select_repeated:
  movq  %rbx, %r8
  movq  %rcx, %rbx
  cmpq  %r8, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rcx
  movq  %rsi, %rax
  cmpq  $1, %rcx
  cmovne %rdi, %rax
  cmpq  $1, %rcx
  cmovne %rdx, %rbx
  ret
|}]
