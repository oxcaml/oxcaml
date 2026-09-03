(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -experimental-optimizations";
 expect.opt;
*)

open Intrinsics

(* Codegen tests for stores of small integers. Only the low bits of the stored
   value end up in memory, so sign extensions (and other operations only
   affecting the high bits) of the stored value are unnecessary. *)

(* Unboxed small integer arrays *)

let int8_array_set_add (a : int8# array) (i : int) (x : int8#) (y : int8#) =
  Array.unsafe_set a i (Int8_u.add x y)
[%%expect_asm X86_64{|
int8_array_set_add:
  sarq  $1, %rbx
  addq  %rsi, %rdi
  movb  %dil, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let int16_array_set_add (a : int16# array) (i : int) (x : int16#) (y : int16#) =
  Array.unsafe_set a i (Int16_u.add x y)
[%%expect_asm X86_64{|
int16_array_set_add:
  addq  %rsi, %rdi
  movw  %di, -1(%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let int32_array_set_add (a : int32# array) (i : int) (x : int32#) (y : int32#) =
  Array.unsafe_set a i (Int32_u.add x y)
[%%expect_asm X86_64{|
int32_array_set_add:
  addq  %rsi, %rdi
  movl  %edi, -2(%rax,%rbx,2)
  movl  $1, %eax
  ret
|}]

let int8_array_set_of_int (a : int8# array) (i : int) (x : int) =
  Array.unsafe_set a i (Int8_u.of_int x)
[%%expect_asm X86_64{|
int8_array_set_of_int:
  sarq  $1, %rbx
  sarq  $1, %rdi
  movb  %dil, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let int16_array_set_of_int (a : int16# array) (i : int) (x : int) =
  Array.unsafe_set a i (Int16_u.of_int x)
[%%expect_asm X86_64{|
int16_array_set_of_int:
  sarq  $1, %rdi
  movw  %di, -1(%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let int32_array_set_of_int (a : int32# array) (i : int) (x : int) =
  Array.unsafe_set a i (Int32_u.of_int x)
[%%expect_asm X86_64{|
int32_array_set_of_int:
  sarq  $1, %rdi
  movl  %edi, -2(%rax,%rbx,2)
  movl  $1, %eax
  ret
|}]

(* Bytes *)

let bytes_set_int8_add (buf : bytes) (i : int) (x : int8#) (y : int8#) =
  Bytes.unsafe_set buf i (Int8_u.to_int (Int8_u.add x y))
[%%expect_asm X86_64{|
bytes_set_int8_add:
  sarq  $1, %rbx
  addq  %rsi, %rdi
  movb  %dil, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_set_int16_add (buf : bytes) (i : int) (x : int16#) (y : int16#) =
  Bytes.unsafe_set_uint16_ne buf i (Int16_u.to_int (Int16_u.add x y))
[%%expect_asm X86_64{|
bytes_set_int16_add:
  sarq  $1, %rbx
  addq  %rsi, %rdi
  movw  %di, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_set_int32_add (buf : bytes) (i : int) (x : int32#) (y : int32#) =
  Bytes.unsafe_set_int32_ne buf i (Int32_u.add x y)
[%%expect_asm X86_64{|
bytes_set_int32_add:
  sarq  $1, %rbx
  addq  %rsi, %rdi
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_set_int32_of_int (buf : bytes) (i : int) (x : int) =
  Bytes.unsafe_set_int32_ne buf i (Int32_u.of_int x)
[%%expect_asm X86_64{|
bytes_set_int32_of_int:
  sarq  $1, %rbx
  sarq  $1, %rdi
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

let bytes_set_int32_indexed_by_int64_add
    (buf : bytes) (i : Int64_u.t) (x : int32#) (y : int32#) =
  Bytes.unsafe_set_int32_ne_indexed_by_int64 buf i (Int32_u.add x y)
[%%expect_asm X86_64{|
bytes_set_int32_indexed_by_int64_add:
  addq  %rsi, %rdi
  movl  %edi, (%rax,%rbx)
  movl  $1, %eax
  ret
|}]

(* Mixed blocks *)

type mixed = { mutable i8 : int8#; mutable i16 : int16#; mutable i32 : int32# }

let mixed_set_int8_add (r : mixed) (x : int8#) (y : int8#) =
  r.i8 <- Int8_u.add x y
[%%expect_asm X86_64{|
mixed_set_int8_add:
  addq  %rdi, %rbx
  movb  %bl, (%rax)
  movl  $1, %eax
  ret
|}]

let mixed_set_int16_add (r : mixed) (x : int16#) (y : int16#) =
  r.i16 <- Int16_u.add x y
[%%expect_asm X86_64{|
mixed_set_int16_add:
  addq  %rdi, %rbx
  movw  %bx, 8(%rax)
  movl  $1, %eax
  ret
|}]

let mixed_set_int32_add (r : mixed) (x : int32#) (y : int32#) =
  r.i32 <- Int32_u.add x y
[%%expect_asm X86_64{|
mixed_set_int32_add:
  addq  %rdi, %rbx
  movl  %ebx, 16(%rax)
  movl  $1, %eax
  ret
|}]
