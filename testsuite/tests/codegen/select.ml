(* TEST
 readonly_files = "intrinsics.ml stubs.c";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml stubs.c";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -experimental-optimizations";
 expect.opt;
*)

open Intrinsics


(* CR ttebbi: This use of select is the identity on the value representation *)
let select_identity x = Builtins.select x 1 0
[%%expect_asm X86_64{|
select_identity:
  movl  $1, %ebx
  movl  $3, %edi
  cmpq  $1, %rax
  cmovne %rdi, %rbx
  movq  %rbx, %rax
  ret
|}]


(* CR ttebbi: This could use fewer instructions by flipping
   the condition and cmov registers. *)
let select_cmp (x : int) = Builtins.select (x > 10) x 55
[%%expect_asm X86_64{|
select_cmp:
  movl  $111, %ebx
  cmpq  $21, %rax
  cmovg %rax, %rbx
  movq  %rbx, %rax
  ret
|}]

(* CR ttebbi: We shouldn't materialize the bit, and ideally even share the
   cmp instructions. *)
let select_cmp_twice (x : int) (y: int) =
  (Builtins.select (x < y) x y) + (Builtins.select (x < y) 10 20)
[%%expect_asm X86_64{|
select_cmp_twice:
  movq  %rax, %rdi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setl  %al
  leaq  1(%rax,%rax), %rsi
  movl  $41, %eax
  movl  $21, %edx
  cmpq  $1, %rsi
  cmovne %rdx, %rax
  cmpq  $1, %rsi
  cmovne %rdi, %rbx
  leaq  -1(%rbx,%rax), %rax
  ret
|}]


(* CR ttebbi: We could constant-fold this. *)
let select_constant (x : int) = Builtins.select true x 55
[%%expect_asm X86_64{|
select_constant:
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_int32 b (x : int32#) (y : int32#) =
  Builtins.select_int32 b x y
[%%expect_asm X86_64{|
select_int32:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_int64 b (x : int64#) (y : int64#) =
  Builtins.select_int64 b x y
[%%expect_asm X86_64{|
select_int64:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_nativeint b (x : nativeint#) (y : nativeint#) =
  Builtins.select_nativeint b x y
[%%expect_asm X86_64{|
select_nativeint:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
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
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %r8
  movq  %rsi, %rax
  cmpq  $1, %r8
  cmovne %rdi, %rax
  movq  %rcx, %rbx
  cmpq  $1, %r8
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
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %r8
  movq  %rsi, %rax
  cmpq  $1, %r8
  cmovne %rdi, %rax
  movq  %rcx, %rbx
  cmpq  $1, %r8
  cmovne %rdx, %rbx
  ret
|}]


(* CR ttebbi: select blocks automatic unboxing. *)
let unboxing_through_select b x y =
  Builtins.select b (Int64_u.to_int64 x) (Int64_u.to_int64 y) |> Int64_u.of_int64
[%%expect_asm X86_64{|
unboxing_through_select:
  subq  $8, %rsp
  movq  64(%r14), %rcx
  movq  64(%r14), %rsi
  subq  $48, %rsi
  movq  %rsi, 64(%r14)
  cmpq  80(%r14), %rsi
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rsi
  addq  $8, %rsi
  addq  $24, %rsi
  movq  $3071, -8(%rsi)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdx
  movq  %rdx, (%rsi)
  movq  %rdi, 8(%rsi)
  leaq  -24(%rsi), %rdi
  movq  $3071, -8(%rdi)
  movq  %rdx, (%rdi)
  movq  %rbx, 8(%rdi)
  cmpq  $1, %rax
  cmovne %rdi, %rsi
  movq  8(%rsi), %rax
  movq  %rcx, 64(%r14)
  addq  $8, %rsp
  ret
|}]
