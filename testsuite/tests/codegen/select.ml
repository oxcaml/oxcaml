(* TEST
 readonly_files = "intrinsics.ml stubs.c";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml stubs.c";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -experimental-optimizations";
 flags += " -g -gdwarf-inlined-frames";
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

let select_cmp_twice (x : int) (y: int) =
  (Builtins.select (x < y) x y) + (Builtins.select (x < y) 10 20)
[%%expect_asm X86_64{|
select_cmp_twice:
  movl  $41, %edi
  movl  $21, %esi
  cmpq  %rbx, %rax
  cmovl %rsi, %rdi
  cmovl %rax, %rbx
  leaq  -1(%rbx,%rdi), %rax
  ret
|}, AArch64{|
select_cmp_twice:
  movz  x2, #41, lsl #0
  movz  x3, #21, lsl #0
  subs  xzr, x0, x1
  csel  x2, x3, x2, lt
  subs  xzr, x0, x1
  csel  x0, x0, x1, lt
  add   x0, x0, x2
  sub   x0, x0, #1
  ret
|}]


let select_constant (x : int) = Builtins.select true x 55
[%%expect_asm X86_64{|
select_constant:
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_int32 b (x : int32_u) (y : int32_u) =
  Builtins.select_int32 b x y
[%%expect_asm X86_64{|
select_int32:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_int64 b (x : int64_u) (y : int64_u) =
  Builtins.select_int64 b x y
[%%expect_asm X86_64{|
select_int64:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
  ret
|}]


(* CR ttebbi: Unnecessary moves. *)
let select_nativeint b (x : nativeint_u) (y : nativeint_u) =
  Builtins.select_nativeint b x y
[%%expect_asm X86_64{|
select_nativeint:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
  ret
|}]

let repeated_select_shared x y z w  a b =
  let c = (Int64_u.to_int64 x) < (Int64_u.to_int64 y) in
  let q = Builtins.select_int64 c z w in
  let r = Builtins.select_int64 c a b in
  #(q,r)
[%%expect_asm X86_64{|
repeated_select_shared:
  cmpq  %rbx, %rax
  cmovl %rdi, %rsi
  cmovl %rdx, %rcx
  movq  %rsi, %rax
  movq  %rcx, %rbx
  ret
|}, AArch64{|
repeated_select_shared:
  subs  xzr, x0, x1
  csel  x2, x2, x3, lt
  subs  xzr, x0, x1
  csel  x1, x4, x5, lt
  orr   x0, xzr, x2
  ret
|}]

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
  cmovl %rdi, %rsi
  cmovl %rdx, %rcx
  movq  %rsi, %rax
  movq  %rcx, %rbx
  ret
|}, AArch64{|
repeated_select_repeated:
  subs  xzr, x0, x1
  csel  x2, x2, x3, lt
  subs  xzr, x0, x1
  csel  x1, x4, x5, lt
  orr   x0, xzr, x2
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

(* Both arms are the same constant, so no csel is needed. *)
let select_same_constant x = Builtins.select x 0 0
[%%expect_asm X86_64{|
select_same_constant:
  movl  $1, %eax
  ret
|}]

(* Both arms are the same value, so no csel is needed. *)
let select_same_arg x y = Builtins.select x y y
[%%expect_asm X86_64{|
select_same_arg:
  movq  %rbx, %rax
  ret
|}]

(* When the condition holds the two arms are equal, so this is the identity
   on [y]. *)
let select_equal (x : int) (y : int) = Builtins.select (x = y) x y
[%%expect_asm X86_64{|
select_equal:
  movq  %rbx, %rax
  ret
|}]

(* CR ttebbi: Having both the test/cmov and cmp/jump is unnecessary. Ideally,
   the jump is eliminated and the cmov selects between [g] and [h] *)
let select_and_match x g h =
  match Builtins.select (Int64_u.equal x #0L) true false with
  | true -> g #()
  | false -> h #()
[%%expect_asm X86_64{|
select_and_match:
  movq  %rax, %rsi
  movq  %rbx, %rax
  movl  $1, %ebx
  movl  $3, %edx
  testq %rsi, %rsi
  cmove %rdx, %rbx
  cmpq  $1, %rbx
  jne   .L0
  movq  (%rdi), %rbx
  movq  %rdi, %rax
  jmp   *%rbx
.L0:
  movq  (%rax), %rbx
  jmp   *%rbx
|}]

(* CR ttebbi: Having both the test/cmov and cmp/jump is unnecessary. Ideally,
   the jump is eliminated and the cmov selects between [g] and [h] *)
let select_and_if x g h =
  if Builtins.select (Int64_u.equal x #0L) true false then g #() else h #()
[%%expect_asm X86_64{|
select_and_if:
  movq  %rax, %rsi
  movq  %rbx, %rax
  movl  $1, %ebx
  movl  $3, %edx
  testq %rsi, %rsi
  cmove %rdx, %rbx
  cmpq  $1, %rbx
  jne   .L0
  movq  (%rdi), %rbx
  movq  %rdi, %rax
  jmp   *%rbx
.L0:
  movq  (%rax), %rbx
  jmp   *%rbx
|}]

(* The inlined [min] and [max] have distinct DWARF ranges, but the peephole
   optimizer should still remove the redundant comparison. *)
let min_max (x : int) y =
  let[@inline always] min x y = Builtins.select (x <= y) x y in
  let[@inline always] max x y = Builtins.select (x >= y) x y in
  #(min x y, max x y)
[%%expect_asm X86_64{|
min_max:
  movq  %rbx, %rdi
  cmpq  %rbx, %rax
  cmovge %rax, %rdi
  cmovle %rax, %rbx
  movq  %rbx, %rax
  movq  %rdi, %rbx
  ret
|}]


(* Disjunctions of null tests on [or_null] values that are written without
   branching: [x = 0 || y = 0] is computed as [umin x y = 0], and neither the
   null tests nor their disjunction should be materialized unless the result
   is needed as a value. *)

external is_null : ('a : value_or_null). 'a @ local -> bool = "%is_null"

external select_or_null :
  ('a : value_or_null).
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  = "caml_csel_value"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

let either_null (a : int or_null) (b : int or_null) =
  Bool.logor (is_null a) (is_null b)
[%%expect_asm X86_64{|
either_null:
  cmpq  %rbx, %rax
  cmovb %rax, %rbx
  xorl  %eax, %eax
  testq %rbx, %rbx
  sete  %al
  leaq  1(%rax,%rax), %rax
  ret
|}, AArch64{|
either_null:
  subs  xzr, x0, x1
  csel  x0, x0, x1, cc
  subs  xzr, x0, #0
  csinc x0, xzr, xzr, ne
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
|}]

let either_null_branch (a : int or_null) (b : int or_null) =
  if Bool.logor (is_null a) (is_null b) then 10 else 20
[%%expect_asm X86_64{|
either_null_branch:
  cmpq  %rbx, %rax
  cmovb %rax, %rbx
  testq %rbx, %rbx
  jne   .L0
  movl  $21, %eax
  ret
.L0:
  movl  $41, %eax
  ret
|}, AArch64{|
either_null_branch:
  subs  xzr, x0, x1
  csel  x0, x0, x1, cc
  subs  xzr, x0, #0
  b.ne  .L0
  movz  x0, #21, lsl #0
  ret
.L0:
  movz  x0, #41, lsl #0
  ret
|}]

(* The null test that feeds the select is shared with the disjunction; it
   should be recomputed from the flags rather than kept as a boolean. *)
let either_null_logor (a : int or_null) (b : int or_null) =
  let a_null = is_null a in
  let b_null = is_null b in
  if Bool.logor a_null b_null then select_or_null a_null b a else a
[%%expect_asm X86_64{|
either_null_logor:
  movq  %rbx, %rdi
  cmpq  %rbx, %rax
  cmovb %rax, %rdi
  testq %rdi, %rdi
  jne   .L0
  testq %rax, %rax
  cmove %rbx, %rax
  ret
.L0:
  ret
|}, AArch64{|
either_null_logor:
  subs  xzr, x0, x1
  csel  x2, x0, x1, cc
  subs  xzr, x2, #0
  b.ne  .L0
  subs  xzr, x0, #0
  csel  x0, x1, x0, eq
  ret
.L0:
  ret
|}]

(* As [either_null_logor], with the disjunction written using unsafe
   conversions between booleans and integers. *)
external unsafe_int_of_bool : bool -> int = "%identity"

external unsafe_bool_of_int : int -> bool = "%identity"

let either_null_identity (a : int or_null) (b : int or_null) =
  let a_null = is_null a in
  let b_null = is_null b in
  let either =
    unsafe_bool_of_int (unsafe_int_of_bool a_null lor unsafe_int_of_bool b_null)
  in
  if either then select_or_null a_null b a else a
[%%expect_asm X86_64{|
either_null_identity:
  movq  %rbx, %rdi
  cmpq  %rbx, %rax
  cmovb %rax, %rdi
  testq %rdi, %rdi
  jne   .L0
  testq %rax, %rax
  cmove %rbx, %rax
  ret
.L0:
  ret
|}, AArch64{|
either_null_identity:
  subs  xzr, x0, x1
  csel  x2, x0, x1, cc
  subs  xzr, x2, #0
  b.ne  .L0
  subs  xzr, x0, #0
  csel  x0, x1, x0, eq
  ret
.L0:
  ret
|}]

(* As [either_null_logor], with the integer turned back into a boolean by a
   test against zero. *)
let either_null_test_zero (a : int or_null) (b : int or_null) =
  let[@inline] bool_of_int (i : int) : bool = i <> 0 in
  let a_null = is_null a in
  let b_null = is_null b in
  if bool_of_int (Bool.to_int a_null lor Bool.to_int b_null)
  then select_or_null a_null b a
  else a
[%%expect_asm X86_64{|
either_null_test_zero:
  movq  %rbx, %rdi
  cmpq  %rbx, %rax
  cmovb %rax, %rdi
  testq %rdi, %rdi
  jne   .L0
  testq %rax, %rax
  cmove %rbx, %rax
  ret
.L0:
  ret
|}, AArch64{|
either_null_test_zero:
  subs  xzr, x0, x1
  csel  x2, x0, x1, cc
  subs  xzr, x2, #0
  b.ne  .L0
  subs  xzr, x0, #0
  csel  x0, x1, x0, eq
  ret
.L0:
  ret
|}]
