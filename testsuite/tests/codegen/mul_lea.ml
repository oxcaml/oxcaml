(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Multiplication by 3, 5 and 9 should be strength-reduced to a single [lea],
   even when the operand is a computed expression (not just a variable) and for
   [int32_u] / [int64_u], where it previously emitted [imul]. *)

let mul5_var (x : int) = x * 5
[%%expect_asm X86_64{|
mul5_var:
  leaq  -4(%rax,%rax,4), %rax
  ret
|}]

let mul5_expr (x : int) = (x lxor 3) * 5
[%%expect_asm X86_64{|
mul5_expr:
  xorq  $7, %rax
  orq   $1, %rax
  leaq  (%rax,%rax,4), %rax
  addq  $-4, %rax
  ret
|}]

let mul3_expr (x : int) (y : int) = (x + y) * 3
[%%expect_asm X86_64{|
mul3_expr:
  addq  %rbx, %rax
  leaq  (%rax,%rax,2), %rax
  addq  $-5, %rax
  ret
|}]

let mul9_expr (x : int) (y : int) = (x + y) * 9
[%%expect_asm X86_64{|
mul9_expr:
  addq  %rbx, %rax
  leaq  (%rax,%rax,8), %rax
  addq  $-17, %rax
  ret
|}]

(* For the unboxed-int cases the operand must be a computed expression: with a
   simple variable, instruction selection already produces a [lea] directly. *)

external box_int64 : int64_u -> int64 = "%box_int64" [@@warning "-187"]
external unbox_int64 : int64 -> int64_u = "%unbox_int64" [@@warning "-187"]

let mul5_int64 (x : int64_u) (y : int64_u) =
  unbox_int64 (Int64.mul (Int64.add (box_int64 x) (box_int64 y)) 5L)
[%%expect_asm X86_64{|
mul5_int64:
  addq  %rbx, %rax
  leaq  (%rax,%rax,4), %rax
  ret
|}]

external box_int32 : int32_u -> int32 = "%box_int32" [@@warning "-187"]
external unbox_int32 : int32 -> int32_u = "%unbox_int32" [@@warning "-187"]

let mul5_int32 (x : int32_u) (y : int32_u) =
  unbox_int32 (Int32.mul (Int32.add (box_int32 x) (box_int32 y)) 5l)
[%%expect_asm X86_64{|
mul5_int32:
  addq  %rbx, %rax
  leaq  (%rax,%rax,4), %rax
  movslq %eax, %rax
  ret
|}]

let mul7_expr (x : int) (y : int) = (x + y) * 7
[%%expect_asm X86_64{|
mul7_expr:
  addq  %rbx, %rax
  imulq $7, %rax
  addq  $-13, %rax
  ret
|}]
