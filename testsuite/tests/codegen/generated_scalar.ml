(* TEST
 (* All of this test (except this TEST stanza) is auto-generated. *)
 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 generate-scalar-codegen;
*)

external float_u_abs : float# -> float# = "%float#_abs"
let float_u_abs x = float_u_abs x
[%%expect_asm X86_64{|
float_u_abs:
  vandpd caml_absf_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float_u_add : float# -> float# -> float# = "%float#_add"
let float_u_add x y = float_u_add x y
[%%expect_asm X86_64{|
float_u_add:
  vaddsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_compare : float# -> float# -> int = "%float#_compare"
let float_u_compare x y = float_u_compare x y
[%%expect_asm X86_64{|
float_u_compare:
  vcmpsd $0, %xmm1, %xmm1, %xmm2
  vmovq %xmm2, %rbx
  neg   %rbx
  vcmpsd $0, %xmm0, %xmm0, %xmm2
  vmovq %xmm2, %rax
  neg   %rax
  subq  %rbx, %rax
  vcmpsd $1, %xmm1, %xmm0, %xmm2
  vmovq %xmm2, %rdi
  neg   %rdi
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rbx
  neg   %rbx
  subq  %rdi, %rbx
  addq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_div : float# -> float# -> float# = "%float#_div"
let float_u_div x y = float_u_div x y
[%%expect_asm X86_64{|
float_u_div:
  vdivsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_mul : float# -> float# -> float# = "%float#_mul"
let float_u_mul x y = float_u_mul x y
[%%expect_asm X86_64{|
float_u_mul:
  vmulsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_neg : float# -> float# = "%float#_neg"
let float_u_neg x = float_u_neg x
[%%expect_asm X86_64{|
float_u_neg:
  vxorpd caml_negf_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float_u_of_float : float -> float# = "%float#_of_float"
let float_u_of_float x = float_u_of_float x
[%%expect_asm X86_64{|
float_u_of_float:
  vmovsd (%rax), %xmm0
  ret
|}]

external float_u_of_float32 : float32 -> float# = "%float#_of_float32"
let float_u_of_float32 x = float_u_of_float32 x
[%%expect_asm X86_64{|
float_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvtss2sd %xmm0, %xmm0, %xmm0
  ret
|}]

external float_u_of_float32_u : float32# -> float# = "%float#_of_float32#"
let float_u_of_float32_u x = float_u_of_float32_u x
[%%expect_asm X86_64{|
float_u_of_float32_u:
  vcvtss2sd %xmm0, %xmm0, %xmm0
  ret
|}]

external float_u_of_int : int -> float# = "%float#_of_int"
let float_u_of_int x = float_u_of_int x
[%%expect_asm X86_64{|
float_u_of_int:
  sarq  $1, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int_u : int# -> float# = "%float#_of_int#"
let float_u_of_int_u x = float_u_of_int_u x
[%%expect_asm X86_64{|
float_u_of_int_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int16 : int16 -> float# = "%float#_of_int16"
let float_u_of_int16 x = float_u_of_int16 x
[%%expect_asm X86_64{|
float_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int16_u : int16# -> float# = "%float#_of_int16#"
let float_u_of_int16_u x = float_u_of_int16_u x
[%%expect_asm X86_64{|
float_u_of_int16_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int32 : int32 -> float# = "%float#_of_int32"
let float_u_of_int32 x = float_u_of_int32 x
[%%expect_asm X86_64{|
float_u_of_int32:
  movslq 8(%rax), %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int32_u : int32# -> float# = "%float#_of_int32#"
let float_u_of_int32_u x = float_u_of_int32_u x
[%%expect_asm X86_64{|
float_u_of_int32_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int64 : int64 -> float# = "%float#_of_int64"
let float_u_of_int64 x = float_u_of_int64 x
[%%expect_asm X86_64{|
float_u_of_int64:
  movq  8(%rax), %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int64_u : int64# -> float# = "%float#_of_int64#"
let float_u_of_int64_u x = float_u_of_int64_u x
[%%expect_asm X86_64{|
float_u_of_int64_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int8 : int8 -> float# = "%float#_of_int8"
let float_u_of_int8 x = float_u_of_int8 x
[%%expect_asm X86_64{|
float_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int8_u : int8# -> float# = "%float#_of_int8#"
let float_u_of_int8_u x = float_u_of_int8_u x
[%%expect_asm X86_64{|
float_u_of_int8_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_nativeint : nativeint -> float# = "%float#_of_nativeint"
let float_u_of_nativeint x = float_u_of_nativeint x
[%%expect_asm X86_64{|
float_u_of_nativeint:
  movq  8(%rax), %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_nativeint_u : nativeint# -> float# = "%float#_of_nativeint#"
let float_u_of_nativeint_u x = float_u_of_nativeint_u x
[%%expect_asm X86_64{|
float_u_of_nativeint_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_ordered_and_equal : float# -> float# -> bool = "%float#_ordered_and_equal"
let float_u_ordered_and_equal x y = float_u_ordered_and_equal x y
[%%expect_asm X86_64{|
float_u_ordered_and_equal:
  vcmpsd $0, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_greaterequal : float# -> float# -> bool = "%float#_ordered_and_greaterequal"
let float_u_ordered_and_greaterequal x y = float_u_ordered_and_greaterequal x y
[%%expect_asm X86_64{|
float_u_ordered_and_greaterequal:
  vcmpsd $2, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_greaterthan : float# -> float# -> bool = "%float#_ordered_and_greaterthan"
let float_u_ordered_and_greaterthan x y = float_u_ordered_and_greaterthan x y
[%%expect_asm X86_64{|
float_u_ordered_and_greaterthan:
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_lessequal : float# -> float# -> bool = "%float#_ordered_and_lessequal"
let float_u_ordered_and_lessequal x y = float_u_ordered_and_lessequal x y
[%%expect_asm X86_64{|
float_u_ordered_and_lessequal:
  vcmpsd $2, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_lessthan : float# -> float# -> bool = "%float#_ordered_and_lessthan"
let float_u_ordered_and_lessthan x y = float_u_ordered_and_lessthan x y
[%%expect_asm X86_64{|
float_u_ordered_and_lessthan:
  vcmpsd $1, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_sub : float# -> float# -> float# = "%float#_sub"
let float_u_sub x y = float_u_sub x y
[%%expect_asm X86_64{|
float_u_sub:
  vsubsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_unordered_or_greaterequal : float# -> float# -> bool = "%float#_unordered_or_greaterequal"
let float_u_unordered_or_greaterequal x y = float_u_unordered_or_greaterequal x y
[%%expect_asm X86_64{|
float_u_unordered_or_greaterequal:
  vcmpsd $5, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_greaterthan : float# -> float# -> bool = "%float#_unordered_or_greaterthan"
let float_u_unordered_or_greaterthan x y = float_u_unordered_or_greaterthan x y
[%%expect_asm X86_64{|
float_u_unordered_or_greaterthan:
  vcmpsd $6, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_lessequal : float# -> float# -> bool = "%float#_unordered_or_lessequal"
let float_u_unordered_or_lessequal x y = float_u_unordered_or_lessequal x y
[%%expect_asm X86_64{|
float_u_unordered_or_lessequal:
  vcmpsd $5, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_lessthan : float# -> float# -> bool = "%float#_unordered_or_lessthan"
let float_u_unordered_or_lessthan x y = float_u_unordered_or_lessthan x y
[%%expect_asm X86_64{|
float_u_unordered_or_lessthan:
  vcmpsd $6, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_notequal : float# -> float# -> bool = "%float#_unordered_or_notequal"
let float_u_unordered_or_notequal x y = float_u_unordered_or_notequal x y
[%%expect_asm X86_64{|
float_u_unordered_or_notequal:
  vcmpsd $4, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_abs : float32# -> float32# = "%float32#_abs"
let float32_u_abs x = float32_u_abs x
[%%expect_asm X86_64{|
float32_u_abs:
  vandps caml_absf32_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float32_u_add : float32# -> float32# -> float32# = "%float32#_add"
let float32_u_add x y = float32_u_add x y
[%%expect_asm X86_64{|
float32_u_add:
  vaddss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_compare : float32# -> float32# -> int = "%float32#_compare"
let float32_u_compare x y = float32_u_compare x y
[%%expect_asm X86_64{|
float32_u_compare:
  vcmpss $0, %xmm1, %xmm1, %xmm2
  vmovd %xmm2, %ebx
  movslq %ebx, %rbx
  neg   %rbx
  vcmpss $0, %xmm0, %xmm0, %xmm2
  vmovd %xmm2, %eax
  movslq %eax, %rax
  neg   %rax
  subq  %rbx, %rax
  vcmpss $1, %xmm1, %xmm0, %xmm2
  vmovd %xmm2, %edi
  movslq %edi, %rdi
  neg   %rdi
  vcmpss $1, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %ebx
  movslq %ebx, %rbx
  neg   %rbx
  subq  %rdi, %rbx
  addq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_div : float32# -> float32# -> float32# = "%float32#_div"
let float32_u_div x y = float32_u_div x y
[%%expect_asm X86_64{|
float32_u_div:
  vdivss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_mul : float32# -> float32# -> float32# = "%float32#_mul"
let float32_u_mul x y = float32_u_mul x y
[%%expect_asm X86_64{|
float32_u_mul:
  vmulss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_neg : float32# -> float32# = "%float32#_neg"
let float32_u_neg x = float32_u_neg x
[%%expect_asm X86_64{|
float32_u_neg:
  vxorps caml_negf32_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float32_u_of_float : float -> float32# = "%float32#_of_float"
let float32_u_of_float x = float32_u_of_float x
[%%expect_asm X86_64{|
float32_u_of_float:
  vmovsd (%rax), %xmm0
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  ret
|}]

external float32_u_of_float_u : float# -> float32# = "%float32#_of_float#"
let float32_u_of_float_u x = float32_u_of_float_u x
[%%expect_asm X86_64{|
float32_u_of_float_u:
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  ret
|}]

external float32_u_of_float32 : float32 -> float32# = "%float32#_of_float32"
let float32_u_of_float32 x = float32_u_of_float32 x
[%%expect_asm X86_64{|
float32_u_of_float32:
  vmovss 8(%rax), %xmm0
  ret
|}]

external float32_u_of_int : int -> float32# = "%float32#_of_int"
let float32_u_of_int x = float32_u_of_int x
[%%expect_asm X86_64{|
float32_u_of_int:
  sarq  $1, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int_u : int# -> float32# = "%float32#_of_int#"
let float32_u_of_int_u x = float32_u_of_int_u x
[%%expect_asm X86_64{|
float32_u_of_int_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int16 : int16 -> float32# = "%float32#_of_int16"
let float32_u_of_int16 x = float32_u_of_int16 x
[%%expect_asm X86_64{|
float32_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int16_u : int16# -> float32# = "%float32#_of_int16#"
let float32_u_of_int16_u x = float32_u_of_int16_u x
[%%expect_asm X86_64{|
float32_u_of_int16_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int32 : int32 -> float32# = "%float32#_of_int32"
let float32_u_of_int32 x = float32_u_of_int32 x
[%%expect_asm X86_64{|
float32_u_of_int32:
  movslq 8(%rax), %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int32_u : int32# -> float32# = "%float32#_of_int32#"
let float32_u_of_int32_u x = float32_u_of_int32_u x
[%%expect_asm X86_64{|
float32_u_of_int32_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int64 : int64 -> float32# = "%float32#_of_int64"
let float32_u_of_int64 x = float32_u_of_int64 x
[%%expect_asm X86_64{|
float32_u_of_int64:
  movq  8(%rax), %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int64_u : int64# -> float32# = "%float32#_of_int64#"
let float32_u_of_int64_u x = float32_u_of_int64_u x
[%%expect_asm X86_64{|
float32_u_of_int64_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int8 : int8 -> float32# = "%float32#_of_int8"
let float32_u_of_int8 x = float32_u_of_int8 x
[%%expect_asm X86_64{|
float32_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int8_u : int8# -> float32# = "%float32#_of_int8#"
let float32_u_of_int8_u x = float32_u_of_int8_u x
[%%expect_asm X86_64{|
float32_u_of_int8_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_nativeint : nativeint -> float32# = "%float32#_of_nativeint"
let float32_u_of_nativeint x = float32_u_of_nativeint x
[%%expect_asm X86_64{|
float32_u_of_nativeint:
  movq  8(%rax), %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_nativeint_u : nativeint# -> float32# = "%float32#_of_nativeint#"
let float32_u_of_nativeint_u x = float32_u_of_nativeint_u x
[%%expect_asm X86_64{|
float32_u_of_nativeint_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_ordered_and_equal : float32# -> float32# -> bool = "%float32#_ordered_and_equal"
let float32_u_ordered_and_equal x y = float32_u_ordered_and_equal x y
[%%expect_asm X86_64{|
float32_u_ordered_and_equal:
  vcmpss $0, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_greaterequal : float32# -> float32# -> bool = "%float32#_ordered_and_greaterequal"
let float32_u_ordered_and_greaterequal x y = float32_u_ordered_and_greaterequal x y
[%%expect_asm X86_64{|
float32_u_ordered_and_greaterequal:
  vcmpss $2, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_greaterthan : float32# -> float32# -> bool = "%float32#_ordered_and_greaterthan"
let float32_u_ordered_and_greaterthan x y = float32_u_ordered_and_greaterthan x y
[%%expect_asm X86_64{|
float32_u_ordered_and_greaterthan:
  vcmpss $1, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_lessequal : float32# -> float32# -> bool = "%float32#_ordered_and_lessequal"
let float32_u_ordered_and_lessequal x y = float32_u_ordered_and_lessequal x y
[%%expect_asm X86_64{|
float32_u_ordered_and_lessequal:
  vcmpss $2, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_lessthan : float32# -> float32# -> bool = "%float32#_ordered_and_lessthan"
let float32_u_ordered_and_lessthan x y = float32_u_ordered_and_lessthan x y
[%%expect_asm X86_64{|
float32_u_ordered_and_lessthan:
  vcmpss $1, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_sub : float32# -> float32# -> float32# = "%float32#_sub"
let float32_u_sub x y = float32_u_sub x y
[%%expect_asm X86_64{|
float32_u_sub:
  vsubss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_unordered_or_greaterequal : float32# -> float32# -> bool = "%float32#_unordered_or_greaterequal"
let float32_u_unordered_or_greaterequal x y = float32_u_unordered_or_greaterequal x y
[%%expect_asm X86_64{|
float32_u_unordered_or_greaterequal:
  vcmpss $5, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_greaterthan : float32# -> float32# -> bool = "%float32#_unordered_or_greaterthan"
let float32_u_unordered_or_greaterthan x y = float32_u_unordered_or_greaterthan x y
[%%expect_asm X86_64{|
float32_u_unordered_or_greaterthan:
  vcmpss $6, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_lessequal : float32# -> float32# -> bool = "%float32#_unordered_or_lessequal"
let float32_u_unordered_or_lessequal x y = float32_u_unordered_or_lessequal x y
[%%expect_asm X86_64{|
float32_u_unordered_or_lessequal:
  vcmpss $5, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_lessthan : float32# -> float32# -> bool = "%float32#_unordered_or_lessthan"
let float32_u_unordered_or_lessthan x y = float32_u_unordered_or_lessthan x y
[%%expect_asm X86_64{|
float32_u_unordered_or_lessthan:
  vcmpss $6, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_notequal : float32# -> float32# -> bool = "%float32#_unordered_or_notequal"
let float32_u_unordered_or_notequal x y = float32_u_unordered_or_notequal x y
[%%expect_asm X86_64{|
float32_u_unordered_or_notequal:
  vcmpss $4, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_abs : float32 -> float32 = "%float32_abs"
let float32_abs x = float32_abs x
[%%expect_asm X86_64{|
float32_abs:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovss 8(%rbx), %xmm0
  vandps caml_absf32_mask(%rip), %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_add : float32 -> float32 -> float32 = "%float32_add"
let float32_add x y = float32_add x y
[%%expect_asm X86_64{|
float32_add:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  vmovss 8(%rdi), %xmm0
  vaddss 8(%rbx), %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_compare : float32 -> float32 -> int = "%float32_compare"
let float32_compare x y = float32_compare x y
[%%expect_asm X86_64{|
float32_compare:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $0, %xmm0, %xmm0, %xmm2
  vmovd %xmm2, %ebx
  movslq %ebx, %rbx
  neg   %rbx
  vcmpss $0, %xmm1, %xmm1, %xmm2
  vmovd %xmm2, %eax
  movslq %eax, %rax
  neg   %rax
  subq  %rbx, %rax
  vcmpss $1, %xmm0, %xmm1, %xmm2
  vmovd %xmm2, %edi
  movslq %edi, %rdi
  neg   %rdi
  vcmpss $1, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %ebx
  movslq %ebx, %rbx
  neg   %rbx
  subq  %rdi, %rbx
  addq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_div : float32 -> float32 -> float32 = "%float32_div"
let float32_div x y = float32_div x y
[%%expect_asm X86_64{|
float32_div:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  vmovss 8(%rdi), %xmm0
  vdivss 8(%rbx), %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_mul : float32 -> float32 -> float32 = "%float32_mul"
let float32_mul x y = float32_mul x y
[%%expect_asm X86_64{|
float32_mul:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  vmovss 8(%rdi), %xmm0
  vmulss 8(%rbx), %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_neg : float32 -> float32 = "%float32_neg"
let float32_neg x = float32_neg x
[%%expect_asm X86_64{|
float32_neg:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovss 8(%rbx), %xmm0
  vxorps caml_negf32_mask(%rip), %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_float : float -> float32 = "%float32_of_float"
let float32_of_float x = float32_of_float x
[%%expect_asm X86_64{|
float32_of_float:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovsd (%rbx), %xmm0
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_float_u : float# -> float32 = "%float32_of_float#"
let float32_of_float_u x = float32_of_float_u x
[%%expect_asm X86_64{|
float32_of_float_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_float32_u : float32# -> float32 = "%float32_of_float32#"
let float32_of_float32_u x = float32_of_float32_u x
[%%expect_asm X86_64{|
float32_of_float32_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int : int -> float32 = "%float32_of_int"
let float32_of_int x = float32_of_int x
[%%expect_asm X86_64{|
float32_of_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  sarq  $1, %rbx
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int_u : int# -> float32 = "%float32_of_int#"
let float32_of_int_u x = float32_of_int_u x
[%%expect_asm X86_64{|
float32_of_int_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int16 : int16 -> float32 = "%float32_of_int16"
let float32_of_int16 x = float32_of_int16 x
[%%expect_asm X86_64{|
float32_of_int16:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $47, %rbx
  sarq  $48, %rbx
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int16_u : int16# -> float32 = "%float32_of_int16#"
let float32_of_int16_u x = float32_of_int16_u x
[%%expect_asm X86_64{|
float32_of_int16_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int32 : int32 -> float32 = "%float32_of_int32"
let float32_of_int32 x = float32_of_int32 x
[%%expect_asm X86_64{|
float32_of_int32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rbx
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int32_u : int32# -> float32 = "%float32_of_int32#"
let float32_of_int32_u x = float32_of_int32_u x
[%%expect_asm X86_64{|
float32_of_int32_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int64 : int64 -> float32 = "%float32_of_int64"
let float32_of_int64 x = float32_of_int64 x
[%%expect_asm X86_64{|
float32_of_int64:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int64_u : int64# -> float32 = "%float32_of_int64#"
let float32_of_int64_u x = float32_of_int64_u x
[%%expect_asm X86_64{|
float32_of_int64_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int8 : int8 -> float32 = "%float32_of_int8"
let float32_of_int8 x = float32_of_int8 x
[%%expect_asm X86_64{|
float32_of_int8:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $55, %rbx
  sarq  $56, %rbx
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_int8_u : int8# -> float32 = "%float32_of_int8#"
let float32_of_int8_u x = float32_of_int8_u x
[%%expect_asm X86_64{|
float32_of_int8_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_nativeint : nativeint -> float32 = "%float32_of_nativeint"
let float32_of_nativeint x = float32_of_nativeint x
[%%expect_asm X86_64{|
float32_of_nativeint:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_of_nativeint_u : nativeint# -> float32 = "%float32_of_nativeint#"
let float32_of_nativeint_u x = float32_of_nativeint_u x
[%%expect_asm X86_64{|
float32_of_nativeint_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_ordered_and_equal : float32 -> float32 -> bool = "%float32_ordered_and_equal"
let float32_ordered_and_equal x y = float32_ordered_and_equal x y
[%%expect_asm X86_64{|
float32_ordered_and_equal:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $0, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_ordered_and_greaterequal : float32 -> float32 -> bool = "%float32_ordered_and_greaterequal"
let float32_ordered_and_greaterequal x y = float32_ordered_and_greaterequal x y
[%%expect_asm X86_64{|
float32_ordered_and_greaterequal:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $2, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_ordered_and_greaterthan : float32 -> float32 -> bool = "%float32_ordered_and_greaterthan"
let float32_ordered_and_greaterthan x y = float32_ordered_and_greaterthan x y
[%%expect_asm X86_64{|
float32_ordered_and_greaterthan:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $1, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_ordered_and_lessequal : float32 -> float32 -> bool = "%float32_ordered_and_lessequal"
let float32_ordered_and_lessequal x y = float32_ordered_and_lessequal x y
[%%expect_asm X86_64{|
float32_ordered_and_lessequal:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $2, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_ordered_and_lessthan : float32 -> float32 -> bool = "%float32_ordered_and_lessthan"
let float32_ordered_and_lessthan x y = float32_ordered_and_lessthan x y
[%%expect_asm X86_64{|
float32_ordered_and_lessthan:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $1, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_sub : float32 -> float32 -> float32 = "%float32_sub"
let float32_sub x y = float32_sub x y
[%%expect_asm X86_64{|
float32_sub:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  vmovss 8(%rdi), %xmm0
  vsubss 8(%rbx), %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external float32_unordered_or_greaterequal : float32 -> float32 -> bool = "%float32_unordered_or_greaterequal"
let float32_unordered_or_greaterequal x y = float32_unordered_or_greaterequal x y
[%%expect_asm X86_64{|
float32_unordered_or_greaterequal:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $5, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_unordered_or_greaterthan : float32 -> float32 -> bool = "%float32_unordered_or_greaterthan"
let float32_unordered_or_greaterthan x y = float32_unordered_or_greaterthan x y
[%%expect_asm X86_64{|
float32_unordered_or_greaterthan:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $6, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_unordered_or_lessequal : float32 -> float32 -> bool = "%float32_unordered_or_lessequal"
let float32_unordered_or_lessequal x y = float32_unordered_or_lessequal x y
[%%expect_asm X86_64{|
float32_unordered_or_lessequal:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $5, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_unordered_or_lessthan : float32 -> float32 -> bool = "%float32_unordered_or_lessthan"
let float32_unordered_or_lessthan x y = float32_unordered_or_lessthan x y
[%%expect_asm X86_64{|
float32_unordered_or_lessthan:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $6, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_unordered_or_notequal : float32 -> float32 -> bool = "%float32_unordered_or_notequal"
let float32_unordered_or_notequal x y = float32_unordered_or_notequal x y
[%%expect_asm X86_64{|
float32_unordered_or_notequal:
  vmovss 8(%rbx), %xmm0
  vmovss 8(%rax), %xmm1
  vcmpss $4, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_abs : float -> float = "%float_abs"
let float_abs x = float_abs x
[%%expect_asm X86_64{|
float_abs:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rbx), %xmm0
  vandpd caml_absf_mask(%rip), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_add : float -> float -> float = "%float_add"
let float_add x y = float_add x y
[%%expect_asm X86_64{|
float_add:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rdi), %xmm0
  vaddsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_compare : float -> float -> int = "%float_compare"
let float_compare x y = float_compare x y
[%%expect_asm X86_64{|
float_compare:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $0, %xmm0, %xmm0, %xmm2
  vmovq %xmm2, %rbx
  neg   %rbx
  vcmpsd $0, %xmm1, %xmm1, %xmm2
  vmovq %xmm2, %rax
  neg   %rax
  subq  %rbx, %rax
  vcmpsd $1, %xmm0, %xmm1, %xmm2
  vmovq %xmm2, %rdi
  neg   %rdi
  vcmpsd $1, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rbx
  neg   %rbx
  subq  %rdi, %rbx
  addq  %rbx, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_div : float -> float -> float = "%float_div"
let float_div x y = float_div x y
[%%expect_asm X86_64{|
float_div:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rdi), %xmm0
  vdivsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_mul : float -> float -> float = "%float_mul"
let float_mul x y = float_mul x y
[%%expect_asm X86_64{|
float_mul:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rdi), %xmm0
  vmulsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_neg : float -> float = "%float_neg"
let float_neg x = float_neg x
[%%expect_asm X86_64{|
float_neg:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rbx), %xmm0
  vxorpd caml_negf_mask(%rip), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_float_u : float# -> float = "%float_of_float#"
let float_of_float_u x = float_of_float_u x
[%%expect_asm X86_64{|
float_of_float_u:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_float32 : float32 -> float = "%float_of_float32"
let float_of_float32 x = float_of_float32 x
[%%expect_asm X86_64{|
float_of_float32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovss 8(%rbx), %xmm0
  vcvtss2sd %xmm0, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_float32_u : float32# -> float = "%float_of_float32#"
let float_of_float32_u x = float_of_float32_u x
[%%expect_asm X86_64{|
float_of_float32_u:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtss2sd %xmm0, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int : int -> float = "%float_of_int"
let float_of_int x = float_of_int x
[%%expect_asm X86_64{|
float_of_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  sarq  $1, %rbx
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int_u : int# -> float = "%float_of_int#"
let float_of_int_u x = float_of_int_u x
[%%expect_asm X86_64{|
float_of_int_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int16 : int16 -> float = "%float_of_int16"
let float_of_int16 x = float_of_int16 x
[%%expect_asm X86_64{|
float_of_int16:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  salq  $47, %rbx
  sarq  $48, %rbx
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int16_u : int16# -> float = "%float_of_int16#"
let float_of_int16_u x = float_of_int16_u x
[%%expect_asm X86_64{|
float_of_int16_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int32 : int32 -> float = "%float_of_int32"
let float_of_int32 x = float_of_int32 x
[%%expect_asm X86_64{|
float_of_int32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  movslq 8(%rbx), %rbx
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int32_u : int32# -> float = "%float_of_int32#"
let float_of_int32_u x = float_of_int32_u x
[%%expect_asm X86_64{|
float_of_int32_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int64 : int64 -> float = "%float_of_int64"
let float_of_int64 x = float_of_int64 x
[%%expect_asm X86_64{|
float_of_int64:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  movq  8(%rbx), %rbx
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int64_u : int64# -> float = "%float_of_int64#"
let float_of_int64_u x = float_of_int64_u x
[%%expect_asm X86_64{|
float_of_int64_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int8 : int8 -> float = "%float_of_int8"
let float_of_int8 x = float_of_int8 x
[%%expect_asm X86_64{|
float_of_int8:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  salq  $55, %rbx
  sarq  $56, %rbx
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_int8_u : int8# -> float = "%float_of_int8#"
let float_of_int8_u x = float_of_int8_u x
[%%expect_asm X86_64{|
float_of_int8_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_nativeint : nativeint -> float = "%float_of_nativeint"
let float_of_nativeint x = float_of_nativeint x
[%%expect_asm X86_64{|
float_of_nativeint:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  movq  8(%rbx), %rbx
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_of_nativeint_u : nativeint# -> float = "%float_of_nativeint#"
let float_of_nativeint_u x = float_of_nativeint_u x
[%%expect_asm X86_64{|
float_of_nativeint_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_ordered_and_equal : float -> float -> bool = "%float_ordered_and_equal"
let float_ordered_and_equal x y = float_ordered_and_equal x y
[%%expect_asm X86_64{|
float_ordered_and_equal:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $0, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_greaterequal : float -> float -> bool = "%float_ordered_and_greaterequal"
let float_ordered_and_greaterequal x y = float_ordered_and_greaterequal x y
[%%expect_asm X86_64{|
float_ordered_and_greaterequal:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $2, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_greaterthan : float -> float -> bool = "%float_ordered_and_greaterthan"
let float_ordered_and_greaterthan x y = float_ordered_and_greaterthan x y
[%%expect_asm X86_64{|
float_ordered_and_greaterthan:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $1, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_lessequal : float -> float -> bool = "%float_ordered_and_lessequal"
let float_ordered_and_lessequal x y = float_ordered_and_lessequal x y
[%%expect_asm X86_64{|
float_ordered_and_lessequal:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $2, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_lessthan : float -> float -> bool = "%float_ordered_and_lessthan"
let float_ordered_and_lessthan x y = float_ordered_and_lessthan x y
[%%expect_asm X86_64{|
float_ordered_and_lessthan:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_sub : float -> float -> float = "%float_sub"
let float_sub x y = float_sub x y
[%%expect_asm X86_64{|
float_sub:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rdi), %xmm0
  vsubsd (%rbx), %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

external float_unordered_or_greaterequal : float -> float -> bool = "%float_unordered_or_greaterequal"
let float_unordered_or_greaterequal x y = float_unordered_or_greaterequal x y
[%%expect_asm X86_64{|
float_unordered_or_greaterequal:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $5, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_greaterthan : float -> float -> bool = "%float_unordered_or_greaterthan"
let float_unordered_or_greaterthan x y = float_unordered_or_greaterthan x y
[%%expect_asm X86_64{|
float_unordered_or_greaterthan:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $6, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_lessequal : float -> float -> bool = "%float_unordered_or_lessequal"
let float_unordered_or_lessequal x y = float_unordered_or_lessequal x y
[%%expect_asm X86_64{|
float_unordered_or_lessequal:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $5, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_lessthan : float -> float -> bool = "%float_unordered_or_lessthan"
let float_unordered_or_lessthan x y = float_unordered_or_lessthan x y
[%%expect_asm X86_64{|
float_unordered_or_lessthan:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $6, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_notequal : float -> float -> bool = "%float_unordered_or_notequal"
let float_unordered_or_notequal x y = float_unordered_or_notequal x y
[%%expect_asm X86_64{|
float_unordered_or_notequal:
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $4, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_add : int# -> int# -> int# = "%int#_add"
let int_u_add x y = int_u_add x y
[%%expect_asm X86_64{|
int_u_add:
  addq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_and : int# -> int# -> int# = "%int#_and"
let int_u_and x y = int_u_and x y
[%%expect_asm X86_64{|
int_u_and:
  andq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_asr : int# -> int -> int# = "%int#_asr"
let int_u_asr x y = int_u_asr x y
[%%expect_asm X86_64{|
int_u_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int_u_bswap : int# -> int# = "%int#_bswap"
let int_u_bswap x = int_u_bswap x
[%%expect_asm X86_64{|
int_u_bswap:
  salq  $48, %rax
  sarq  $48, %rax
  xchg  %ah, %al
  movzwq %ax, %rax
  andl  $65535, %eax
  ret
|}]

external int_u_compare : int# -> int# -> bool = "%int#_compare"
let int_u_compare x y = int_u_compare x y
[%%expect_asm X86_64{|
int_u_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int_u_div : int# -> int# -> int# = "%int#_div"
let int_u_div x y = int_u_div x y
[%%expect_asm X86_64{|
int_u_div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  salq  $1, %rax
  sarq  $1, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int_u_equal : int# -> int# -> bool = "%int#_equal"
let int_u_equal x y = int_u_equal x y
[%%expect_asm X86_64{|
int_u_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_greaterequal : int# -> int# -> bool = "%int#_greaterequal"
let int_u_greaterequal x y = int_u_greaterequal x y
[%%expect_asm X86_64{|
int_u_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_greaterthan : int# -> int# -> bool = "%int#_greaterthan"
let int_u_greaterthan x y = int_u_greaterthan x y
[%%expect_asm X86_64{|
int_u_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_lessequal : int# -> int# -> bool = "%int#_lessequal"
let int_u_lessequal x y = int_u_lessequal x y
[%%expect_asm X86_64{|
int_u_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_lessthan : int# -> int# -> bool = "%int#_lessthan"
let int_u_lessthan x y = int_u_lessthan x y
[%%expect_asm X86_64{|
int_u_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_lsl : int# -> int -> int# = "%int#_lsl"
let int_u_lsl x y = int_u_lsl x y
[%%expect_asm X86_64{|
int_u_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_lsr : int# -> int -> int# = "%int#_lsr"
let int_u_lsr x y = int_u_lsr x y
[%%expect_asm X86_64{|
int_u_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  movabsq $9223372036854775807, %rbx
  andq  %rbx, %rax
  shrq  %cl, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_mod : int# -> int# -> int# = "%int#_mod"
let int_u_mod x y = int_u_mod x y
[%%expect_asm X86_64{|
int_u_mod:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int_u_mul : int# -> int# -> int# = "%int#_mul"
let int_u_mul x y = int_u_mul x y
[%%expect_asm X86_64{|
int_u_mul:
  imulq %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_neg : int# -> int# = "%int#_neg"
let int_u_neg x = int_u_neg x
[%%expect_asm X86_64{|
int_u_neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_notequal : int# -> int# -> bool = "%int#_notequal"
let int_u_notequal x y = int_u_notequal x y
[%%expect_asm X86_64{|
int_u_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_of_float : float -> int# = "%int#_of_float"
let int_u_of_float x = int_u_of_float x
[%%expect_asm X86_64{|
int_u_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_float_u : float# -> int# = "%int#_of_float#"
let int_u_of_float_u x = int_u_of_float_u x
[%%expect_asm X86_64{|
int_u_of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_float32 : float32 -> int# = "%int#_of_float32"
let int_u_of_float32 x = int_u_of_float32 x
[%%expect_asm X86_64{|
int_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_float32_u : float32# -> int# = "%int#_of_float32#"
let int_u_of_float32_u x = int_u_of_float32_u x
[%%expect_asm X86_64{|
int_u_of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_int : int -> int# = "%int#_of_int"
let int_u_of_int x = int_u_of_int x
[%%expect_asm X86_64{|
int_u_of_int:
  sarq  $1, %rax
  ret
|}]

external int_u_of_int16 : int16 -> int# = "%int#_of_int16"
let int_u_of_int16 x = int_u_of_int16 x
[%%expect_asm X86_64{|
int_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int_u_of_int16_u : int16# -> int# = "%int#_of_int16#"
let int_u_of_int16_u x = int_u_of_int16_u x
[%%expect_asm X86_64{|
int_u_of_int16_u:
  ret
|}]

external int_u_of_int32 : int32 -> int# = "%int#_of_int32"
let int_u_of_int32 x = int_u_of_int32 x
[%%expect_asm X86_64{|
int_u_of_int32:
  movslq 8(%rax), %rax
  ret
|}]

external int_u_of_int32_u : int32# -> int# = "%int#_of_int32#"
let int_u_of_int32_u x = int_u_of_int32_u x
[%%expect_asm X86_64{|
int_u_of_int32_u:
  ret
|}]

external int_u_of_int64 : int64 -> int# = "%int#_of_int64"
let int_u_of_int64 x = int_u_of_int64 x
[%%expect_asm X86_64{|
int_u_of_int64:
  movq  8(%rax), %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_int64_u : int64# -> int# = "%int#_of_int64#"
let int_u_of_int64_u x = int_u_of_int64_u x
[%%expect_asm X86_64{|
int_u_of_int64_u:
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_int8 : int8 -> int# = "%int#_of_int8"
let int_u_of_int8 x = int_u_of_int8 x
[%%expect_asm X86_64{|
int_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int_u_of_int8_u : int8# -> int# = "%int#_of_int8#"
let int_u_of_int8_u x = int_u_of_int8_u x
[%%expect_asm X86_64{|
int_u_of_int8_u:
  ret
|}]

external int_u_of_nativeint : nativeint -> int# = "%int#_of_nativeint"
let int_u_of_nativeint x = int_u_of_nativeint x
[%%expect_asm X86_64{|
int_u_of_nativeint:
  movq  8(%rax), %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_nativeint_u : nativeint# -> int# = "%int#_of_nativeint#"
let int_u_of_nativeint_u x = int_u_of_nativeint_u x
[%%expect_asm X86_64{|
int_u_of_nativeint_u:
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_or : int# -> int# -> int# = "%int#_or"
let int_u_or x y = int_u_or x y
[%%expect_asm X86_64{|
int_u_or:
  orq   %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_pred : int# -> int# = "%int#_pred"
let int_u_pred x = int_u_pred x
[%%expect_asm X86_64{|
int_u_pred:
  decq  %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_sub : int# -> int# -> int# = "%int#_sub"
let int_u_sub x y = int_u_sub x y
[%%expect_asm X86_64{|
int_u_sub:
  subq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_succ : int# -> int# = "%int#_succ"
let int_u_succ x = int_u_succ x
[%%expect_asm X86_64{|
int_u_succ:
  incq  %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_unsafe_div : int# -> int# -> int# = "%int#_unsafe_div"
let int_u_unsafe_div x y = int_u_unsafe_div x y
[%%expect_asm X86_64{|
int_u_unsafe_div:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_unsafe_mod : int# -> int# -> int# = "%int#_unsafe_mod"
let int_u_unsafe_mod x y = int_u_unsafe_mod x y
[%%expect_asm X86_64{|
int_u_unsafe_mod:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_unsigned_compare : int# -> int# -> bool = "%int#_unsigned_compare"
let int_u_unsigned_compare x y = int_u_unsigned_compare x y
[%%expect_asm X86_64{|
int_u_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int_u_unsigned_greaterequal : int# -> int# -> bool = "%int#_unsigned_greaterequal"
let int_u_unsigned_greaterequal x y = int_u_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int_u_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_unsigned_greaterthan : int# -> int# -> bool = "%int#_unsigned_greaterthan"
let int_u_unsigned_greaterthan x y = int_u_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int_u_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_unsigned_lessequal : int# -> int# -> bool = "%int#_unsigned_lessequal"
let int_u_unsigned_lessequal x y = int_u_unsigned_lessequal x y
[%%expect_asm X86_64{|
int_u_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_unsigned_lessthan : int# -> int# -> bool = "%int#_unsigned_lessthan"
let int_u_unsigned_lessthan x y = int_u_unsigned_lessthan x y
[%%expect_asm X86_64{|
int_u_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_xor : int# -> int# -> int# = "%int#_xor"
let int_u_xor x y = int_u_xor x y
[%%expect_asm X86_64{|
int_u_xor:
  xorq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int16_u_add : int16# -> int16# -> int16# = "%int16#_add"
let int16_u_add x y = int16_u_add x y
[%%expect_asm X86_64{|
int16_u_add:
  addq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_and : int16# -> int16# -> int16# = "%int16#_and"
let int16_u_and x y = int16_u_and x y
[%%expect_asm X86_64{|
int16_u_and:
  andq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_asr : int16# -> int -> int16# = "%int16#_asr"
let int16_u_asr x y = int16_u_asr x y
[%%expect_asm X86_64{|
int16_u_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int16_u_bswap : int16# -> int16# = "%int16#_bswap"
let int16_u_bswap x = int16_u_bswap x
[%%expect_asm X86_64{|
int16_u_bswap:
  xchg  %ah, %al
  movzwq %ax, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_compare : int16# -> int16# -> bool = "%int16#_compare"
let int16_u_compare x y = int16_u_compare x y
[%%expect_asm X86_64{|
int16_u_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int16_u_div : int16# -> int16# -> int16# = "%int16#_div"
let int16_u_div x y = int16_u_div x y
[%%expect_asm X86_64{|
int16_u_div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int16_u_equal : int16# -> int16# -> bool = "%int16#_equal"
let int16_u_equal x y = int16_u_equal x y
[%%expect_asm X86_64{|
int16_u_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_greaterequal : int16# -> int16# -> bool = "%int16#_greaterequal"
let int16_u_greaterequal x y = int16_u_greaterequal x y
[%%expect_asm X86_64{|
int16_u_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_greaterthan : int16# -> int16# -> bool = "%int16#_greaterthan"
let int16_u_greaterthan x y = int16_u_greaterthan x y
[%%expect_asm X86_64{|
int16_u_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_lessequal : int16# -> int16# -> bool = "%int16#_lessequal"
let int16_u_lessequal x y = int16_u_lessequal x y
[%%expect_asm X86_64{|
int16_u_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_lessthan : int16# -> int16# -> bool = "%int16#_lessthan"
let int16_u_lessthan x y = int16_u_lessthan x y
[%%expect_asm X86_64{|
int16_u_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_lsl : int16# -> int -> int16# = "%int16#_lsl"
let int16_u_lsl x y = int16_u_lsl x y
[%%expect_asm X86_64{|
int16_u_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_lsr : int16# -> int -> int16# = "%int16#_lsr"
let int16_u_lsr x y = int16_u_lsr x y
[%%expect_asm X86_64{|
int16_u_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  andl  $65535, %eax
  shrq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_mod : int16# -> int16# -> int16# = "%int16#_mod"
let int16_u_mod x y = int16_u_mod x y
[%%expect_asm X86_64{|
int16_u_mod:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int16_u_mul : int16# -> int16# -> int16# = "%int16#_mul"
let int16_u_mul x y = int16_u_mul x y
[%%expect_asm X86_64{|
int16_u_mul:
  imulq %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_neg : int16# -> int16# = "%int16#_neg"
let int16_u_neg x = int16_u_neg x
[%%expect_asm X86_64{|
int16_u_neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_notequal : int16# -> int16# -> bool = "%int16#_notequal"
let int16_u_notequal x y = int16_u_notequal x y
[%%expect_asm X86_64{|
int16_u_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_of_float : float -> int16# = "%int16#_of_float"
let int16_u_of_float x = int16_u_of_float x
[%%expect_asm X86_64{|
int16_u_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_float_u : float# -> int16# = "%int16#_of_float#"
let int16_u_of_float_u x = int16_u_of_float_u x
[%%expect_asm X86_64{|
int16_u_of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_float32 : float32 -> int16# = "%int16#_of_float32"
let int16_u_of_float32 x = int16_u_of_float32 x
[%%expect_asm X86_64{|
int16_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_float32_u : float32# -> int16# = "%int16#_of_float32#"
let int16_u_of_float32_u x = int16_u_of_float32_u x
[%%expect_asm X86_64{|
int16_u_of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int : int -> int16# = "%int16#_of_int"
let int16_u_of_int x = int16_u_of_int x
[%%expect_asm X86_64{|
int16_u_of_int:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int_u : int# -> int16# = "%int16#_of_int#"
let int16_u_of_int_u x = int16_u_of_int_u x
[%%expect_asm X86_64{|
int16_u_of_int_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int16 : int16 -> int16# = "%int16#_of_int16"
let int16_u_of_int16 x = int16_u_of_int16 x
[%%expect_asm X86_64{|
int16_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int32 : int32 -> int16# = "%int16#_of_int32"
let int16_u_of_int32 x = int16_u_of_int32 x
[%%expect_asm X86_64{|
int16_u_of_int32:
  movslq 8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int32_u : int32# -> int16# = "%int16#_of_int32#"
let int16_u_of_int32_u x = int16_u_of_int32_u x
[%%expect_asm X86_64{|
int16_u_of_int32_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int64 : int64 -> int16# = "%int16#_of_int64"
let int16_u_of_int64 x = int16_u_of_int64 x
[%%expect_asm X86_64{|
int16_u_of_int64:
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int64_u : int64# -> int16# = "%int16#_of_int64#"
let int16_u_of_int64_u x = int16_u_of_int64_u x
[%%expect_asm X86_64{|
int16_u_of_int64_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int8 : int8 -> int16# = "%int16#_of_int8"
let int16_u_of_int8 x = int16_u_of_int8 x
[%%expect_asm X86_64{|
int16_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int16_u_of_int8_u : int8# -> int16# = "%int16#_of_int8#"
let int16_u_of_int8_u x = int16_u_of_int8_u x
[%%expect_asm X86_64{|
int16_u_of_int8_u:
  ret
|}]

external int16_u_of_nativeint : nativeint -> int16# = "%int16#_of_nativeint"
let int16_u_of_nativeint x = int16_u_of_nativeint x
[%%expect_asm X86_64{|
int16_u_of_nativeint:
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_nativeint_u : nativeint# -> int16# = "%int16#_of_nativeint#"
let int16_u_of_nativeint_u x = int16_u_of_nativeint_u x
[%%expect_asm X86_64{|
int16_u_of_nativeint_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_or : int16# -> int16# -> int16# = "%int16#_or"
let int16_u_or x y = int16_u_or x y
[%%expect_asm X86_64{|
int16_u_or:
  orq   %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_pred : int16# -> int16# = "%int16#_pred"
let int16_u_pred x = int16_u_pred x
[%%expect_asm X86_64{|
int16_u_pred:
  decq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_sub : int16# -> int16# -> int16# = "%int16#_sub"
let int16_u_sub x y = int16_u_sub x y
[%%expect_asm X86_64{|
int16_u_sub:
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_succ : int16# -> int16# = "%int16#_succ"
let int16_u_succ x = int16_u_succ x
[%%expect_asm X86_64{|
int16_u_succ:
  incq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_unsafe_div : int16# -> int16# -> int16# = "%int16#_unsafe_div"
let int16_u_unsafe_div x y = int16_u_unsafe_div x y
[%%expect_asm X86_64{|
int16_u_unsafe_div:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_unsafe_mod : int16# -> int16# -> int16# = "%int16#_unsafe_mod"
let int16_u_unsafe_mod x y = int16_u_unsafe_mod x y
[%%expect_asm X86_64{|
int16_u_unsafe_mod:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_unsigned_compare : int16# -> int16# -> bool = "%int16#_unsigned_compare"
let int16_u_unsigned_compare x y = int16_u_unsigned_compare x y
[%%expect_asm X86_64{|
int16_u_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int16_u_unsigned_greaterequal : int16# -> int16# -> bool = "%int16#_unsigned_greaterequal"
let int16_u_unsigned_greaterequal x y = int16_u_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int16_u_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_unsigned_greaterthan : int16# -> int16# -> bool = "%int16#_unsigned_greaterthan"
let int16_u_unsigned_greaterthan x y = int16_u_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int16_u_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_unsigned_lessequal : int16# -> int16# -> bool = "%int16#_unsigned_lessequal"
let int16_u_unsigned_lessequal x y = int16_u_unsigned_lessequal x y
[%%expect_asm X86_64{|
int16_u_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_unsigned_lessthan : int16# -> int16# -> bool = "%int16#_unsigned_lessthan"
let int16_u_unsigned_lessthan x y = int16_u_unsigned_lessthan x y
[%%expect_asm X86_64{|
int16_u_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_xor : int16# -> int16# -> int16# = "%int16#_xor"
let int16_u_xor x y = int16_u_xor x y
[%%expect_asm X86_64{|
int16_u_xor:
  xorq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_add : int16 -> int16 -> int16 = "%int16_add"
let int16_add x y = int16_add x y
[%%expect_asm X86_64{|
int16_add:
  sarq  $1, %rbx
  sarq  $1, %rax
  addq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_and : int16 -> int16 -> int16 = "%int16_and"
let int16_and x y = int16_and x y
[%%expect_asm X86_64{|
int16_and:
  sarq  $1, %rbx
  sarq  $1, %rax
  andq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_asr : int16 -> int -> int16 = "%int16_asr"
let int16_asr x y = int16_asr x y
[%%expect_asm X86_64{|
int16_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  $47, %rax
  sarq  %cl, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_bswap : int16 -> int16 = "%int16_bswap"
let int16_bswap x = int16_bswap x
[%%expect_asm X86_64{|
int16_bswap:
  salq  $47, %rax
  sarq  $48, %rax
  xchg  %ah, %al
  movzwq %ax, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_compare : int16 -> int16 -> bool = "%int16_compare"
let int16_compare x y = int16_compare x y
[%%expect_asm X86_64{|
int16_compare:
  movq  %rax, %rdi
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rdi
  sarq  $48, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int16_div : int16 -> int16 -> int16 = "%int16_div"
let int16_div x y = int16_div x y
[%%expect_asm X86_64{|
int16_div:
  movq  %rbx, %rcx
  salq  $47, %rcx
  sarq  $48, %rcx
  testq %rcx, %rcx
  je    .L0
  salq  $47, %rax
  sarq  $48, %rax
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int16_equal : int16 -> int16 -> bool = "%int16_equal"
let int16_equal x y = int16_equal x y
[%%expect_asm X86_64{|
int16_equal:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_greaterequal : int16 -> int16 -> bool = "%int16_greaterequal"
let int16_greaterequal x y = int16_greaterequal x y
[%%expect_asm X86_64{|
int16_greaterequal:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_greaterthan : int16 -> int16 -> bool = "%int16_greaterthan"
let int16_greaterthan x y = int16_greaterthan x y
[%%expect_asm X86_64{|
int16_greaterthan:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_lessequal : int16 -> int16 -> bool = "%int16_lessequal"
let int16_lessequal x y = int16_lessequal x y
[%%expect_asm X86_64{|
int16_lessequal:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_lessthan : int16 -> int16 -> bool = "%int16_lessthan"
let int16_lessthan x y = int16_lessthan x y
[%%expect_asm X86_64{|
int16_lessthan:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_lsl : int16 -> int -> int16 = "%int16_lsl"
let int16_lsl x y = int16_lsl x y
[%%expect_asm X86_64{|
int16_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  $47, %rax
  sarq  $48, %rax
  salq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_lsr : int16 -> int -> int16 = "%int16_lsr"
let int16_lsr x y = int16_lsr x y
[%%expect_asm X86_64{|
int16_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  $1, %rax
  andl  $65535, %eax
  shrq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_mod : int16 -> int16 -> int16 = "%int16_mod"
let int16_mod x y = int16_mod x y
[%%expect_asm X86_64{|
int16_mod:
  movq  %rbx, %rcx
  salq  $47, %rcx
  sarq  $48, %rcx
  testq %rcx, %rcx
  je    .L0
  salq  $47, %rax
  sarq  $48, %rax
  cqto
  idivq %rcx
  salq  $48, %rdx
  sarq  $48, %rdx
  leaq  1(%rdx,%rdx), %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int16_mul : int16 -> int16 -> int16 = "%int16_mul"
let int16_mul x y = int16_mul x y
[%%expect_asm X86_64{|
int16_mul:
  sarq  $1, %rbx
  sarq  $1, %rax
  imulq %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_neg : int16 -> int16 = "%int16_neg"
let int16_neg x = int16_neg x
[%%expect_asm X86_64{|
int16_neg:
  sarq  $1, %rax
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  salq  $48, %rbx
  sarq  $48, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

external int16_notequal : int16 -> int16 -> bool = "%int16_notequal"
let int16_notequal x y = int16_notequal x y
[%%expect_asm X86_64{|
int16_notequal:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float : float -> int16 = "%int16_of_float"
let int16_of_float x = int16_of_float x
[%%expect_asm X86_64{|
int16_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float_u : float# -> int16 = "%int16_of_float#"
let int16_of_float_u x = int16_of_float_u x
[%%expect_asm X86_64{|
int16_of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float32 : float32 -> int16 = "%int16_of_float32"
let int16_of_float32 x = int16_of_float32 x
[%%expect_asm X86_64{|
int16_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float32_u : float32# -> int16 = "%int16_of_float32#"
let int16_of_float32_u x = int16_of_float32_u x
[%%expect_asm X86_64{|
int16_of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int : int -> int16 = "%int16_of_int"
let int16_of_int x = int16_of_int x
[%%expect_asm X86_64{|
int16_of_int:
  salq  $47, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int_u : int# -> int16 = "%int16_of_int#"
let int16_of_int_u x = int16_of_int_u x
[%%expect_asm X86_64{|
int16_of_int_u:
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int16_u : int16# -> int16 = "%int16_of_int16#"
let int16_of_int16_u x = int16_of_int16_u x
[%%expect_asm X86_64{|
int16_of_int16_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int32 : int32 -> int16 = "%int16_of_int32"
let int16_of_int32 x = int16_of_int32 x
[%%expect_asm X86_64{|
int16_of_int32:
  movslq 8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int32_u : int32# -> int16 = "%int16_of_int32#"
let int16_of_int32_u x = int16_of_int32_u x
[%%expect_asm X86_64{|
int16_of_int32_u:
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int64 : int64 -> int16 = "%int16_of_int64"
let int16_of_int64 x = int16_of_int64 x
[%%expect_asm X86_64{|
int16_of_int64:
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int64_u : int64# -> int16 = "%int16_of_int64#"
let int16_of_int64_u x = int16_of_int64_u x
[%%expect_asm X86_64{|
int16_of_int64_u:
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int8 : int8 -> int16 = "%int16_of_int8"
let int16_of_int8 x = int16_of_int8 x
[%%expect_asm X86_64{|
int16_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int8_u : int8# -> int16 = "%int16_of_int8#"
let int16_of_int8_u x = int16_of_int8_u x
[%%expect_asm X86_64{|
int16_of_int8_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_nativeint : nativeint -> int16 = "%int16_of_nativeint"
let int16_of_nativeint x = int16_of_nativeint x
[%%expect_asm X86_64{|
int16_of_nativeint:
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_nativeint_u : nativeint# -> int16 = "%int16_of_nativeint#"
let int16_of_nativeint_u x = int16_of_nativeint_u x
[%%expect_asm X86_64{|
int16_of_nativeint_u:
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_or : int16 -> int16 -> int16 = "%int16_or"
let int16_or x y = int16_or x y
[%%expect_asm X86_64{|
int16_or:
  sarq  $1, %rbx
  sarq  $1, %rax
  orq   %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_pred : int16 -> int16 = "%int16_pred"
let int16_pred x = int16_pred x
[%%expect_asm X86_64{|
int16_pred:
  sarq  $1, %rax
  decq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_sub : int16 -> int16 -> int16 = "%int16_sub"
let int16_sub x y = int16_sub x y
[%%expect_asm X86_64{|
int16_sub:
  sarq  $1, %rbx
  sarq  $1, %rax
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_succ : int16 -> int16 = "%int16_succ"
let int16_succ x = int16_succ x
[%%expect_asm X86_64{|
int16_succ:
  sarq  $1, %rax
  incq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_unsafe_div : int16 -> int16 -> int16 = "%int16_unsafe_div"
let int16_unsafe_div x y = int16_unsafe_div x y
[%%expect_asm X86_64{|
int16_unsafe_div:
  movq  %rbx, %rcx
  salq  $47, %rcx
  sarq  $48, %rcx
  salq  $47, %rax
  sarq  $48, %rax
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_unsafe_mod : int16 -> int16 -> int16 = "%int16_unsafe_mod"
let int16_unsafe_mod x y = int16_unsafe_mod x y
[%%expect_asm X86_64{|
int16_unsafe_mod:
  movq  %rbx, %rcx
  salq  $47, %rcx
  sarq  $48, %rcx
  salq  $47, %rax
  sarq  $48, %rax
  cqto
  idivq %rcx
  salq  $48, %rdx
  sarq  $48, %rdx
  leaq  1(%rdx,%rdx), %rax
  ret
|}]

external int16_unsigned_compare : int16 -> int16 -> bool = "%int16_unsigned_compare"
let int16_unsigned_compare x y = int16_unsigned_compare x y
[%%expect_asm X86_64{|
int16_unsigned_compare:
  movq  %rax, %rdi
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rdi
  sarq  $48, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int16_unsigned_greaterequal : int16 -> int16 -> bool = "%int16_unsigned_greaterequal"
let int16_unsigned_greaterequal x y = int16_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int16_unsigned_greaterequal:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_unsigned_greaterthan : int16 -> int16 -> bool = "%int16_unsigned_greaterthan"
let int16_unsigned_greaterthan x y = int16_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int16_unsigned_greaterthan:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_unsigned_lessequal : int16 -> int16 -> bool = "%int16_unsigned_lessequal"
let int16_unsigned_lessequal x y = int16_unsigned_lessequal x y
[%%expect_asm X86_64{|
int16_unsigned_lessequal:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_unsigned_lessthan : int16 -> int16 -> bool = "%int16_unsigned_lessthan"
let int16_unsigned_lessthan x y = int16_unsigned_lessthan x y
[%%expect_asm X86_64{|
int16_unsigned_lessthan:
  salq  $47, %rbx
  sarq  $48, %rbx
  salq  $47, %rax
  sarq  $48, %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_xor : int16 -> int16 -> int16 = "%int16_xor"
let int16_xor x y = int16_xor x y
[%%expect_asm X86_64{|
int16_xor:
  sarq  $1, %rbx
  sarq  $1, %rax
  xorq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_add : int32# -> int32# -> int32# = "%int32#_add"
let int32_u_add x y = int32_u_add x y
[%%expect_asm X86_64{|
int32_u_add:
  addq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_and : int32# -> int32# -> int32# = "%int32#_and"
let int32_u_and x y = int32_u_and x y
[%%expect_asm X86_64{|
int32_u_and:
  andq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_asr : int32# -> int -> int32# = "%int32#_asr"
let int32_u_asr x y = int32_u_asr x y
[%%expect_asm X86_64{|
int32_u_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int32_u_bswap : int32# -> int32# = "%int32#_bswap"
let int32_u_bswap x = int32_u_bswap x
[%%expect_asm X86_64{|
int32_u_bswap:
  bswap %eax
  movslq %eax, %rax
  ret
|}]

external int32_u_compare : int32# -> int32# -> bool = "%int32#_compare"
let int32_u_compare x y = int32_u_compare x y
[%%expect_asm X86_64{|
int32_u_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int32_u_div : int32# -> int32# -> int32# = "%int32#_div"
let int32_u_div x y = int32_u_div x y
[%%expect_asm X86_64{|
int32_u_div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  movslq %eax, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int32_u_equal : int32# -> int32# -> bool = "%int32#_equal"
let int32_u_equal x y = int32_u_equal x y
[%%expect_asm X86_64{|
int32_u_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_greaterequal : int32# -> int32# -> bool = "%int32#_greaterequal"
let int32_u_greaterequal x y = int32_u_greaterequal x y
[%%expect_asm X86_64{|
int32_u_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_greaterthan : int32# -> int32# -> bool = "%int32#_greaterthan"
let int32_u_greaterthan x y = int32_u_greaterthan x y
[%%expect_asm X86_64{|
int32_u_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_lessequal : int32# -> int32# -> bool = "%int32#_lessequal"
let int32_u_lessequal x y = int32_u_lessequal x y
[%%expect_asm X86_64{|
int32_u_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_lessthan : int32# -> int32# -> bool = "%int32#_lessthan"
let int32_u_lessthan x y = int32_u_lessthan x y
[%%expect_asm X86_64{|
int32_u_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_lsl : int32# -> int -> int32# = "%int32#_lsl"
let int32_u_lsl x y = int32_u_lsl x y
[%%expect_asm X86_64{|
int32_u_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_lsr : int32# -> int -> int32# = "%int32#_lsr"
let int32_u_lsr x y = int32_u_lsr x y
[%%expect_asm X86_64{|
int32_u_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  movl  %eax, %eax
  shrq  %cl, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_mod : int32# -> int32# -> int32# = "%int32#_mod"
let int32_u_mod x y = int32_u_mod x y
[%%expect_asm X86_64{|
int32_u_mod:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  movslq %edx, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int32_u_mul : int32# -> int32# -> int32# = "%int32#_mul"
let int32_u_mul x y = int32_u_mul x y
[%%expect_asm X86_64{|
int32_u_mul:
  imulq %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_neg : int32# -> int32# = "%int32#_neg"
let int32_u_neg x = int32_u_neg x
[%%expect_asm X86_64{|
int32_u_neg:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  movslq %ebx, %rax
  ret
|}]

external int32_u_notequal : int32# -> int32# -> bool = "%int32#_notequal"
let int32_u_notequal x y = int32_u_notequal x y
[%%expect_asm X86_64{|
int32_u_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_of_float : float -> int32# = "%int32#_of_float"
let int32_u_of_float x = int32_u_of_float x
[%%expect_asm X86_64{|
int32_u_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_float_u : float# -> int32# = "%int32#_of_float#"
let int32_u_of_float_u x = int32_u_of_float_u x
[%%expect_asm X86_64{|
int32_u_of_float_u:
  vcvttsd2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_float32 : float32 -> int32# = "%int32#_of_float32"
let int32_u_of_float32 x = int32_u_of_float32 x
[%%expect_asm X86_64{|
int32_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_float32_u : float32# -> int32# = "%int32#_of_float32#"
let int32_u_of_float32_u x = int32_u_of_float32_u x
[%%expect_asm X86_64{|
int32_u_of_float32_u:
  vcvttss2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int : int -> int32# = "%int32#_of_int"
let int32_u_of_int x = int32_u_of_int x
[%%expect_asm X86_64{|
int32_u_of_int:
  salq  $31, %rax
  sarq  $32, %rax
  ret
|}]

external int32_u_of_int_u : int# -> int32# = "%int32#_of_int#"
let int32_u_of_int_u x = int32_u_of_int_u x
[%%expect_asm X86_64{|
int32_u_of_int_u:
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int16 : int16 -> int32# = "%int32#_of_int16"
let int32_u_of_int16 x = int32_u_of_int16 x
[%%expect_asm X86_64{|
int32_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int32_u_of_int16_u : int16# -> int32# = "%int32#_of_int16#"
let int32_u_of_int16_u x = int32_u_of_int16_u x
[%%expect_asm X86_64{|
int32_u_of_int16_u:
  ret
|}]

external int32_u_of_int32 : int32 -> int32# = "%int32#_of_int32"
let int32_u_of_int32 x = int32_u_of_int32 x
[%%expect_asm X86_64{|
int32_u_of_int32:
  movslq 8(%rax), %rax
  ret
|}]

external int32_u_of_int64 : int64 -> int32# = "%int32#_of_int64"
let int32_u_of_int64 x = int32_u_of_int64 x
[%%expect_asm X86_64{|
int32_u_of_int64:
  movq  8(%rax), %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int64_u : int64# -> int32# = "%int32#_of_int64#"
let int32_u_of_int64_u x = int32_u_of_int64_u x
[%%expect_asm X86_64{|
int32_u_of_int64_u:
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int8 : int8 -> int32# = "%int32#_of_int8"
let int32_u_of_int8 x = int32_u_of_int8 x
[%%expect_asm X86_64{|
int32_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int32_u_of_int8_u : int8# -> int32# = "%int32#_of_int8#"
let int32_u_of_int8_u x = int32_u_of_int8_u x
[%%expect_asm X86_64{|
int32_u_of_int8_u:
  ret
|}]

external int32_u_of_nativeint : nativeint -> int32# = "%int32#_of_nativeint"
let int32_u_of_nativeint x = int32_u_of_nativeint x
[%%expect_asm X86_64{|
int32_u_of_nativeint:
  movq  8(%rax), %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_nativeint_u : nativeint# -> int32# = "%int32#_of_nativeint#"
let int32_u_of_nativeint_u x = int32_u_of_nativeint_u x
[%%expect_asm X86_64{|
int32_u_of_nativeint_u:
  movslq %eax, %rax
  ret
|}]

external int32_u_or : int32# -> int32# -> int32# = "%int32#_or"
let int32_u_or x y = int32_u_or x y
[%%expect_asm X86_64{|
int32_u_or:
  orq   %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_pred : int32# -> int32# = "%int32#_pred"
let int32_u_pred x = int32_u_pred x
[%%expect_asm X86_64{|
int32_u_pred:
  decq  %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_sub : int32# -> int32# -> int32# = "%int32#_sub"
let int32_u_sub x y = int32_u_sub x y
[%%expect_asm X86_64{|
int32_u_sub:
  subq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_succ : int32# -> int32# = "%int32#_succ"
let int32_u_succ x = int32_u_succ x
[%%expect_asm X86_64{|
int32_u_succ:
  incq  %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_unsafe_div : int32# -> int32# -> int32# = "%int32#_unsafe_div"
let int32_u_unsafe_div x y = int32_u_unsafe_div x y
[%%expect_asm X86_64{|
int32_u_unsafe_div:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movslq %eax, %rax
  ret
|}]

external int32_u_unsafe_mod : int32# -> int32# -> int32# = "%int32#_unsafe_mod"
let int32_u_unsafe_mod x y = int32_u_unsafe_mod x y
[%%expect_asm X86_64{|
int32_u_unsafe_mod:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movslq %edx, %rax
  ret
|}]

external int32_u_unsigned_compare : int32# -> int32# -> bool = "%int32#_unsigned_compare"
let int32_u_unsigned_compare x y = int32_u_unsigned_compare x y
[%%expect_asm X86_64{|
int32_u_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int32_u_unsigned_greaterequal : int32# -> int32# -> bool = "%int32#_unsigned_greaterequal"
let int32_u_unsigned_greaterequal x y = int32_u_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int32_u_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_unsigned_greaterthan : int32# -> int32# -> bool = "%int32#_unsigned_greaterthan"
let int32_u_unsigned_greaterthan x y = int32_u_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int32_u_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_unsigned_lessequal : int32# -> int32# -> bool = "%int32#_unsigned_lessequal"
let int32_u_unsigned_lessequal x y = int32_u_unsigned_lessequal x y
[%%expect_asm X86_64{|
int32_u_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_unsigned_lessthan : int32# -> int32# -> bool = "%int32#_unsigned_lessthan"
let int32_u_unsigned_lessthan x y = int32_u_unsigned_lessthan x y
[%%expect_asm X86_64{|
int32_u_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_xor : int32# -> int32# -> int32# = "%int32#_xor"
let int32_u_xor x y = int32_u_xor x y
[%%expect_asm X86_64{|
int32_u_xor:
  xorq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_add : int32 -> int32 -> int32 = "%int32_add"
let int32_add x y = int32_add x y
[%%expect_asm X86_64{|
int32_add:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movslq 8(%rbx), %rbx
  movslq 8(%rdi), %rdi
  addq  %rdi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_and : int32 -> int32 -> int32 = "%int32_and"
let int32_and x y = int32_and x y
[%%expect_asm X86_64{|
int32_and:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movslq 8(%rbx), %rsi
  movslq 8(%rdi), %rbx
  andq  %rsi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_asr : int32 -> int -> int32 = "%int32_asr"
let int32_asr x y = int32_asr x y
[%%expect_asm X86_64{|
int32_asr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movslq 8(%rdi), %rbx
  sarq  %cl, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_bswap : int32 -> int32 = "%int32_bswap"
let int32_bswap x = int32_bswap x
[%%expect_asm X86_64{|
int32_bswap:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rbx
  bswap %ebx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_compare : int32 -> int32 -> bool = "%int32_compare"
let int32_compare x y = int32_compare x y
[%%expect_asm X86_64{|
int32_compare:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int32_div : int32 -> int32 -> int32 = "%int32_div"
let int32_div x y = int32_div x y
[%%expect_asm X86_64{|
int32_div:
  subq  $8, %rsp
  movslq 8(%rbx), %rcx
  testq %rcx, %rcx
  je    .L1
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movslq 8(%rax), %rax
  cqto
  idivq %rcx
  movslq %eax, %rax
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
.L1:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int32_equal : int32 -> int32 -> bool = "%int32_equal"
let int32_equal x y = int32_equal x y
[%%expect_asm X86_64{|
int32_equal:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_greaterequal : int32 -> int32 -> bool = "%int32_greaterequal"
let int32_greaterequal x y = int32_greaterequal x y
[%%expect_asm X86_64{|
int32_greaterequal:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_greaterthan : int32 -> int32 -> bool = "%int32_greaterthan"
let int32_greaterthan x y = int32_greaterthan x y
[%%expect_asm X86_64{|
int32_greaterthan:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_lessequal : int32 -> int32 -> bool = "%int32_lessequal"
let int32_lessequal x y = int32_lessequal x y
[%%expect_asm X86_64{|
int32_lessequal:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_lessthan : int32 -> int32 -> bool = "%int32_lessthan"
let int32_lessthan x y = int32_lessthan x y
[%%expect_asm X86_64{|
int32_lessthan:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_lsl : int32 -> int -> int32 = "%int32_lsl"
let int32_lsl x y = int32_lsl x y
[%%expect_asm X86_64{|
int32_lsl:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movslq 8(%rdi), %rbx
  salq  %cl, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_lsr : int32 -> int -> int32 = "%int32_lsr"
let int32_lsr x y = int32_lsr x y
[%%expect_asm X86_64{|
int32_lsr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movl  8(%rdi), %ebx
  shrq  %cl, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_mod : int32 -> int32 -> int32 = "%int32_mod"
let int32_mod x y = int32_mod x y
[%%expect_asm X86_64{|
int32_mod:
  subq  $8, %rsp
  movslq 8(%rbx), %rcx
  testq %rcx, %rcx
  je    .L1
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movslq 8(%rax), %rax
  cqto
  idivq %rcx
  movslq %edx, %rax
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
.L1:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int32_mul : int32 -> int32 -> int32 = "%int32_mul"
let int32_mul x y = int32_mul x y
[%%expect_asm X86_64{|
int32_mul:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movslq 8(%rbx), %rsi
  movslq 8(%rdi), %rbx
  imulq %rsi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_neg : int32 -> int32 = "%int32_neg"
let int32_neg x = int32_neg x
[%%expect_asm X86_64{|
int32_neg:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rdi
  xorl  %ebx, %ebx
  subq  %rdi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_notequal : int32 -> int32 -> bool = "%int32_notequal"
let int32_notequal x y = int32_notequal x y
[%%expect_asm X86_64{|
int32_notequal:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_of_float : float -> int32 = "%int32_of_float"
let int32_of_float x = int32_of_float x
[%%expect_asm X86_64{|
int32_of_float:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovsd (%rbx), %xmm0
  vcvttsd2si %xmm0, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_float_u : float# -> int32 = "%int32_of_float#"
let int32_of_float_u x = int32_of_float_u x
[%%expect_asm X86_64{|
int32_of_float_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvttsd2si %xmm0, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_float32 : float32 -> int32 = "%int32_of_float32"
let int32_of_float32 x = int32_of_float32 x
[%%expect_asm X86_64{|
int32_of_float32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovss 8(%rbx), %xmm0
  vcvttss2si %xmm0, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_float32_u : float32# -> int32 = "%int32_of_float32#"
let int32_of_float32_u x = int32_of_float32_u x
[%%expect_asm X86_64{|
int32_of_float32_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvttss2si %xmm0, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int : int -> int32 = "%int32_of_int"
let int32_of_int x = int32_of_int x
[%%expect_asm X86_64{|
int32_of_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $31, %rbx
  sarq  $32, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int_u : int# -> int32 = "%int32_of_int#"
let int32_of_int_u x = int32_of_int_u x
[%%expect_asm X86_64{|
int32_of_int_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int16 : int16 -> int32 = "%int32_of_int16"
let int32_of_int16 x = int32_of_int16 x
[%%expect_asm X86_64{|
int32_of_int16:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $47, %rbx
  sarq  $48, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int16_u : int16# -> int32 = "%int32_of_int16#"
let int32_of_int16_u x = int32_of_int16_u x
[%%expect_asm X86_64{|
int32_of_int16_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int32_u : int32# -> int32 = "%int32_of_int32#"
let int32_of_int32_u x = int32_of_int32_u x
[%%expect_asm X86_64{|
int32_of_int32_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int64 : int64 -> int32 = "%int32_of_int64"
let int32_of_int64 x = int32_of_int64 x
[%%expect_asm X86_64{|
int32_of_int64:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int64_u : int64# -> int32 = "%int32_of_int64#"
let int32_of_int64_u x = int32_of_int64_u x
[%%expect_asm X86_64{|
int32_of_int64_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int8 : int8 -> int32 = "%int32_of_int8"
let int32_of_int8 x = int32_of_int8 x
[%%expect_asm X86_64{|
int32_of_int8:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $55, %rbx
  sarq  $56, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_int8_u : int8# -> int32 = "%int32_of_int8#"
let int32_of_int8_u x = int32_of_int8_u x
[%%expect_asm X86_64{|
int32_of_int8_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_nativeint : nativeint -> int32 = "%int32_of_nativeint"
let int32_of_nativeint x = int32_of_nativeint x
[%%expect_asm X86_64{|
int32_of_nativeint:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_of_nativeint_u : nativeint# -> int32 = "%int32_of_nativeint#"
let int32_of_nativeint_u x = int32_of_nativeint_u x
[%%expect_asm X86_64{|
int32_of_nativeint_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_or : int32 -> int32 -> int32 = "%int32_or"
let int32_or x y = int32_or x y
[%%expect_asm X86_64{|
int32_or:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movslq 8(%rbx), %rsi
  movslq 8(%rdi), %rbx
  orq   %rsi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_pred : int32 -> int32 = "%int32_pred"
let int32_pred x = int32_pred x
[%%expect_asm X86_64{|
int32_pred:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rbx
  decq  %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_sub : int32 -> int32 -> int32 = "%int32_sub"
let int32_sub x y = int32_sub x y
[%%expect_asm X86_64{|
int32_sub:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movslq 8(%rbx), %rsi
  movslq 8(%rdi), %rbx
  subq  %rsi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_succ : int32 -> int32 = "%int32_succ"
let int32_succ x = int32_succ x
[%%expect_asm X86_64{|
int32_succ:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rbx
  incq  %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int32_unsafe_div : int32 -> int32 -> int32 = "%int32_unsafe_div"
let int32_unsafe_div x y = int32_unsafe_div x y
[%%expect_asm X86_64{|
int32_unsafe_div:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rdi
  movq  $2303, -8(%rdi)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rdi)
  movslq 8(%rbx), %rcx
  movslq 8(%rax), %rax
  cqto
  idivq %rcx
  movslq %eax, %rax
  movq  %rax, 8(%rdi)
  movq  %rdi, %rax
  addq  $8, %rsp
  ret
|}]

external int32_unsafe_mod : int32 -> int32 -> int32 = "%int32_unsafe_mod"
let int32_unsafe_mod x y = int32_unsafe_mod x y
[%%expect_asm X86_64{|
int32_unsafe_mod:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rdi
  movq  $2303, -8(%rdi)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rdi)
  movslq 8(%rbx), %rcx
  movslq 8(%rax), %rax
  cqto
  idivq %rcx
  movslq %edx, %rax
  movq  %rax, 8(%rdi)
  movq  %rdi, %rax
  addq  $8, %rsp
  ret
|}]

external int32_unsigned_compare : int32 -> int32 -> bool = "%int32_unsigned_compare"
let int32_unsigned_compare x y = int32_unsigned_compare x y
[%%expect_asm X86_64{|
int32_unsigned_compare:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int32_unsigned_greaterequal : int32 -> int32 -> bool = "%int32_unsigned_greaterequal"
let int32_unsigned_greaterequal x y = int32_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int32_unsigned_greaterequal:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_unsigned_greaterthan : int32 -> int32 -> bool = "%int32_unsigned_greaterthan"
let int32_unsigned_greaterthan x y = int32_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int32_unsigned_greaterthan:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_unsigned_lessequal : int32 -> int32 -> bool = "%int32_unsigned_lessequal"
let int32_unsigned_lessequal x y = int32_unsigned_lessequal x y
[%%expect_asm X86_64{|
int32_unsigned_lessequal:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_unsigned_lessthan : int32 -> int32 -> bool = "%int32_unsigned_lessthan"
let int32_unsigned_lessthan x y = int32_unsigned_lessthan x y
[%%expect_asm X86_64{|
int32_unsigned_lessthan:
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_xor : int32 -> int32 -> int32 = "%int32_xor"
let int32_xor x y = int32_xor x y
[%%expect_asm X86_64{|
int32_xor:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movslq 8(%rbx), %rsi
  movslq 8(%rdi), %rbx
  xorq  %rsi, %rbx
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_u_add : int64# -> int64# -> int64# = "%int64#_add"
let int64_u_add x y = int64_u_add x y
[%%expect_asm X86_64{|
int64_u_add:
  addq  %rbx, %rax
  ret
|}]

external int64_u_and : int64# -> int64# -> int64# = "%int64#_and"
let int64_u_and x y = int64_u_and x y
[%%expect_asm X86_64{|
int64_u_and:
  andq  %rbx, %rax
  ret
|}]

external int64_u_asr : int64# -> int -> int64# = "%int64#_asr"
let int64_u_asr x y = int64_u_asr x y
[%%expect_asm X86_64{|
int64_u_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int64_u_bswap : int64# -> int64# = "%int64#_bswap"
let int64_u_bswap x = int64_u_bswap x
[%%expect_asm X86_64{|
int64_u_bswap:
  bswap %rax
  ret
|}]

external int64_u_compare : int64# -> int64# -> bool = "%int64#_compare"
let int64_u_compare x y = int64_u_compare x y
[%%expect_asm X86_64{|
int64_u_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int64_u_div : int64# -> int64# -> int64# = "%int64#_div"
let int64_u_div x y = int64_u_div x y
[%%expect_asm X86_64{|
int64_u_div:
  movq  %rax, %rdi
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L1
  cmpq  $-1, %rcx
  je    .L0
  movq  %rdi, %rax
  cqto
  idivq %rcx
  ret
.L0:
  xorl  %eax, %eax
  subq  %rdi, %rax
  ret
.L1:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int64_u_equal : int64# -> int64# -> bool = "%int64#_equal"
let int64_u_equal x y = int64_u_equal x y
[%%expect_asm X86_64{|
int64_u_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_greaterequal : int64# -> int64# -> bool = "%int64#_greaterequal"
let int64_u_greaterequal x y = int64_u_greaterequal x y
[%%expect_asm X86_64{|
int64_u_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_greaterthan : int64# -> int64# -> bool = "%int64#_greaterthan"
let int64_u_greaterthan x y = int64_u_greaterthan x y
[%%expect_asm X86_64{|
int64_u_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_lessequal : int64# -> int64# -> bool = "%int64#_lessequal"
let int64_u_lessequal x y = int64_u_lessequal x y
[%%expect_asm X86_64{|
int64_u_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_lessthan : int64# -> int64# -> bool = "%int64#_lessthan"
let int64_u_lessthan x y = int64_u_lessthan x y
[%%expect_asm X86_64{|
int64_u_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_lsl : int64# -> int -> int64# = "%int64#_lsl"
let int64_u_lsl x y = int64_u_lsl x y
[%%expect_asm X86_64{|
int64_u_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  ret
|}]

external int64_u_lsr : int64# -> int -> int64# = "%int64#_lsr"
let int64_u_lsr x y = int64_u_lsr x y
[%%expect_asm X86_64{|
int64_u_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  ret
|}]

external int64_u_mod : int64# -> int64# -> int64# = "%int64#_mod"
let int64_u_mod x y = int64_u_mod x y
[%%expect_asm X86_64{|
int64_u_mod:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L1
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  ret
.L0:
  xorl  %eax, %eax
  ret
.L1:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int64_u_mul : int64# -> int64# -> int64# = "%int64#_mul"
let int64_u_mul x y = int64_u_mul x y
[%%expect_asm X86_64{|
int64_u_mul:
  imulq %rbx, %rax
  ret
|}]

external int64_u_neg : int64# -> int64# = "%int64#_neg"
let int64_u_neg x = int64_u_neg x
[%%expect_asm X86_64{|
int64_u_neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

external int64_u_notequal : int64# -> int64# -> bool = "%int64#_notequal"
let int64_u_notequal x y = int64_u_notequal x y
[%%expect_asm X86_64{|
int64_u_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_of_float : float -> int64# = "%int64#_of_float"
let int64_u_of_float x = int64_u_of_float x
[%%expect_asm X86_64{|
int64_u_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  ret
|}]

external int64_u_of_float_u : float# -> int64# = "%int64#_of_float#"
let int64_u_of_float_u x = int64_u_of_float_u x
[%%expect_asm X86_64{|
int64_u_of_float_u:
  vcvttsd2si %xmm0, %rax
  ret
|}]

external int64_u_of_float32 : float32 -> int64# = "%int64#_of_float32"
let int64_u_of_float32 x = int64_u_of_float32 x
[%%expect_asm X86_64{|
int64_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  ret
|}]

external int64_u_of_float32_u : float32# -> int64# = "%int64#_of_float32#"
let int64_u_of_float32_u x = int64_u_of_float32_u x
[%%expect_asm X86_64{|
int64_u_of_float32_u:
  vcvttss2si %xmm0, %rax
  ret
|}]

external int64_u_of_int : int -> int64# = "%int64#_of_int"
let int64_u_of_int x = int64_u_of_int x
[%%expect_asm X86_64{|
int64_u_of_int:
  sarq  $1, %rax
  ret
|}]

external int64_u_of_int_u : int# -> int64# = "%int64#_of_int#"
let int64_u_of_int_u x = int64_u_of_int_u x
[%%expect_asm X86_64{|
int64_u_of_int_u:
  ret
|}]

external int64_u_of_int16 : int16 -> int64# = "%int64#_of_int16"
let int64_u_of_int16 x = int64_u_of_int16 x
[%%expect_asm X86_64{|
int64_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int64_u_of_int16_u : int16# -> int64# = "%int64#_of_int16#"
let int64_u_of_int16_u x = int64_u_of_int16_u x
[%%expect_asm X86_64{|
int64_u_of_int16_u:
  ret
|}]

external int64_u_of_int32 : int32 -> int64# = "%int64#_of_int32"
let int64_u_of_int32 x = int64_u_of_int32 x
[%%expect_asm X86_64{|
int64_u_of_int32:
  movslq 8(%rax), %rax
  ret
|}]

external int64_u_of_int32_u : int32# -> int64# = "%int64#_of_int32#"
let int64_u_of_int32_u x = int64_u_of_int32_u x
[%%expect_asm X86_64{|
int64_u_of_int32_u:
  ret
|}]

external int64_u_of_int64 : int64 -> int64# = "%int64#_of_int64"
let int64_u_of_int64 x = int64_u_of_int64 x
[%%expect_asm X86_64{|
int64_u_of_int64:
  movq  8(%rax), %rax
  ret
|}]

external int64_u_of_int8 : int8 -> int64# = "%int64#_of_int8"
let int64_u_of_int8 x = int64_u_of_int8 x
[%%expect_asm X86_64{|
int64_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int64_u_of_int8_u : int8# -> int64# = "%int64#_of_int8#"
let int64_u_of_int8_u x = int64_u_of_int8_u x
[%%expect_asm X86_64{|
int64_u_of_int8_u:
  ret
|}]

external int64_u_of_nativeint : nativeint -> int64# = "%int64#_of_nativeint"
let int64_u_of_nativeint x = int64_u_of_nativeint x
[%%expect_asm X86_64{|
int64_u_of_nativeint:
  movq  8(%rax), %rax
  ret
|}]

external int64_u_of_nativeint_u : nativeint# -> int64# = "%int64#_of_nativeint#"
let int64_u_of_nativeint_u x = int64_u_of_nativeint_u x
[%%expect_asm X86_64{|
int64_u_of_nativeint_u:
  ret
|}]

external int64_u_or : int64# -> int64# -> int64# = "%int64#_or"
let int64_u_or x y = int64_u_or x y
[%%expect_asm X86_64{|
int64_u_or:
  orq   %rbx, %rax
  ret
|}]

external int64_u_pred : int64# -> int64# = "%int64#_pred"
let int64_u_pred x = int64_u_pred x
[%%expect_asm X86_64{|
int64_u_pred:
  decq  %rax
  ret
|}]

external int64_u_sub : int64# -> int64# -> int64# = "%int64#_sub"
let int64_u_sub x y = int64_u_sub x y
[%%expect_asm X86_64{|
int64_u_sub:
  subq  %rbx, %rax
  ret
|}]

external int64_u_succ : int64# -> int64# = "%int64#_succ"
let int64_u_succ x = int64_u_succ x
[%%expect_asm X86_64{|
int64_u_succ:
  incq  %rax
  ret
|}]

external int64_u_unsafe_div : int64# -> int64# -> int64# = "%int64#_unsafe_div"
let int64_u_unsafe_div x y = int64_u_unsafe_div x y
[%%expect_asm X86_64{|
int64_u_unsafe_div:
  movq  %rax, %rdi
  movq  %rbx, %rcx
  cmpq  $-1, %rcx
  je    .L0
  movq  %rdi, %rax
  cqto
  idivq %rcx
  ret
.L0:
  xorl  %eax, %eax
  subq  %rdi, %rax
  ret
|}]

external int64_u_unsafe_mod : int64# -> int64# -> int64# = "%int64#_unsafe_mod"
let int64_u_unsafe_mod x y = int64_u_unsafe_mod x y
[%%expect_asm X86_64{|
int64_u_unsafe_mod:
  movq  %rbx, %rcx
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  ret
.L0:
  xorl  %eax, %eax
  ret
|}]

external int64_u_unsigned_compare : int64# -> int64# -> bool = "%int64#_unsigned_compare"
let int64_u_unsigned_compare x y = int64_u_unsigned_compare x y
[%%expect_asm X86_64{|
int64_u_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int64_u_unsigned_greaterequal : int64# -> int64# -> bool = "%int64#_unsigned_greaterequal"
let int64_u_unsigned_greaterequal x y = int64_u_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int64_u_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_unsigned_greaterthan : int64# -> int64# -> bool = "%int64#_unsigned_greaterthan"
let int64_u_unsigned_greaterthan x y = int64_u_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int64_u_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_unsigned_lessequal : int64# -> int64# -> bool = "%int64#_unsigned_lessequal"
let int64_u_unsigned_lessequal x y = int64_u_unsigned_lessequal x y
[%%expect_asm X86_64{|
int64_u_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_unsigned_lessthan : int64# -> int64# -> bool = "%int64#_unsigned_lessthan"
let int64_u_unsigned_lessthan x y = int64_u_unsigned_lessthan x y
[%%expect_asm X86_64{|
int64_u_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_xor : int64# -> int64# -> int64# = "%int64#_xor"
let int64_u_xor x y = int64_u_xor x y
[%%expect_asm X86_64{|
int64_u_xor:
  xorq  %rbx, %rax
  ret
|}]

external int64_add : int64 -> int64 -> int64 = "%int64_add"
let int64_add x y = int64_add x y
[%%expect_asm X86_64{|
int64_add:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rbx
  movq  8(%rdi), %rdi
  addq  %rdi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_and : int64 -> int64 -> int64 = "%int64_and"
let int64_and x y = int64_and x y
[%%expect_asm X86_64{|
int64_and:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  andq  %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_asr : int64 -> int -> int64 = "%int64_asr"
let int64_asr x y = int64_asr x y
[%%expect_asm X86_64{|
int64_asr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movq  8(%rdi), %rbx
  sarq  %cl, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_bswap : int64 -> int64 = "%int64_bswap"
let int64_bswap x = int64_bswap x
[%%expect_asm X86_64{|
int64_bswap:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  bswap %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_compare : int64 -> int64 -> bool = "%int64_compare"
let int64_compare x y = int64_compare x y
[%%expect_asm X86_64{|
int64_compare:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int64_div : int64 -> int64 -> int64 = "%int64_div"
let int64_div x y = int64_div x y
[%%expect_asm X86_64{|
int64_div:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  testq %rcx, %rcx
  je    .L3
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rax, %rbx
  jmp   .L1
.L0:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
.L3:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int64_equal : int64 -> int64 -> bool = "%int64_equal"
let int64_equal x y = int64_equal x y
[%%expect_asm X86_64{|
int64_equal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_greaterequal : int64 -> int64 -> bool = "%int64_greaterequal"
let int64_greaterequal x y = int64_greaterequal x y
[%%expect_asm X86_64{|
int64_greaterequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_greaterthan : int64 -> int64 -> bool = "%int64_greaterthan"
let int64_greaterthan x y = int64_greaterthan x y
[%%expect_asm X86_64{|
int64_greaterthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_lessequal : int64 -> int64 -> bool = "%int64_lessequal"
let int64_lessequal x y = int64_lessequal x y
[%%expect_asm X86_64{|
int64_lessequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_lessthan : int64 -> int64 -> bool = "%int64_lessthan"
let int64_lessthan x y = int64_lessthan x y
[%%expect_asm X86_64{|
int64_lessthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_lsl : int64 -> int -> int64 = "%int64_lsl"
let int64_lsl x y = int64_lsl x y
[%%expect_asm X86_64{|
int64_lsl:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movq  8(%rdi), %rbx
  salq  %cl, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_lsr : int64 -> int -> int64 = "%int64_lsr"
let int64_lsr x y = int64_lsr x y
[%%expect_asm X86_64{|
int64_lsr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movq  8(%rdi), %rbx
  shrq  %cl, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_mod : int64 -> int64 -> int64 = "%int64_mod"
let int64_mod x y = int64_mod x y
[%%expect_asm X86_64{|
int64_mod:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  testq %rcx, %rcx
  je    .L3
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  jmp   .L1
.L0:
  xorl  %edx, %edx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  movq  %rdx, 8(%rax)
  addq  $8, %rsp
  ret
.L3:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int64_mul : int64 -> int64 -> int64 = "%int64_mul"
let int64_mul x y = int64_mul x y
[%%expect_asm X86_64{|
int64_mul:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  imulq %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_neg : int64 -> int64 = "%int64_neg"
let int64_neg x = int64_neg x
[%%expect_asm X86_64{|
int64_neg:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rdi
  xorl  %ebx, %ebx
  subq  %rdi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_notequal : int64 -> int64 -> bool = "%int64_notequal"
let int64_notequal x y = int64_notequal x y
[%%expect_asm X86_64{|
int64_notequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_of_float : float -> int64 = "%int64_of_float"
let int64_of_float x = int64_of_float x
[%%expect_asm X86_64{|
int64_of_float:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovsd (%rbx), %xmm0
  vcvttsd2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_float_u : float# -> int64 = "%int64_of_float#"
let int64_of_float_u x = int64_of_float_u x
[%%expect_asm X86_64{|
int64_of_float_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvttsd2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_float32 : float32 -> int64 = "%int64_of_float32"
let int64_of_float32 x = int64_of_float32 x
[%%expect_asm X86_64{|
int64_of_float32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovss 8(%rbx), %xmm0
  vcvttss2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_float32_u : float32# -> int64 = "%int64_of_float32#"
let int64_of_float32_u x = int64_of_float32_u x
[%%expect_asm X86_64{|
int64_of_float32_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvttss2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int : int -> int64 = "%int64_of_int"
let int64_of_int x = int64_of_int x
[%%expect_asm X86_64{|
int64_of_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  sarq  $1, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int_u : int# -> int64 = "%int64_of_int#"
let int64_of_int_u x = int64_of_int_u x
[%%expect_asm X86_64{|
int64_of_int_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int16 : int16 -> int64 = "%int64_of_int16"
let int64_of_int16 x = int64_of_int16 x
[%%expect_asm X86_64{|
int64_of_int16:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $47, %rbx
  sarq  $48, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int16_u : int16# -> int64 = "%int64_of_int16#"
let int64_of_int16_u x = int64_of_int16_u x
[%%expect_asm X86_64{|
int64_of_int16_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int32 : int32 -> int64 = "%int64_of_int32"
let int64_of_int32 x = int64_of_int32 x
[%%expect_asm X86_64{|
int64_of_int32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int32_u : int32# -> int64 = "%int64_of_int32#"
let int64_of_int32_u x = int64_of_int32_u x
[%%expect_asm X86_64{|
int64_of_int32_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int64_u : int64# -> int64 = "%int64_of_int64#"
let int64_of_int64_u x = int64_of_int64_u x
[%%expect_asm X86_64{|
int64_of_int64_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int8 : int8 -> int64 = "%int64_of_int8"
let int64_of_int8 x = int64_of_int8 x
[%%expect_asm X86_64{|
int64_of_int8:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $55, %rbx
  sarq  $56, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_int8_u : int8# -> int64 = "%int64_of_int8#"
let int64_of_int8_u x = int64_of_int8_u x
[%%expect_asm X86_64{|
int64_of_int8_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
let int64_of_nativeint x = int64_of_nativeint x
[%%expect_asm X86_64{|
int64_of_nativeint:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_of_nativeint_u : nativeint# -> int64 = "%int64_of_nativeint#"
let int64_of_nativeint_u x = int64_of_nativeint_u x
[%%expect_asm X86_64{|
int64_of_nativeint_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_or : int64 -> int64 -> int64 = "%int64_or"
let int64_or x y = int64_or x y
[%%expect_asm X86_64{|
int64_or:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  orq   %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_pred : int64 -> int64 = "%int64_pred"
let int64_pred x = int64_pred x
[%%expect_asm X86_64{|
int64_pred:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  decq  %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_sub : int64 -> int64 -> int64 = "%int64_sub"
let int64_sub x y = int64_sub x y
[%%expect_asm X86_64{|
int64_sub:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  subq  %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_succ : int64 -> int64 = "%int64_succ"
let int64_succ x = int64_succ x
[%%expect_asm X86_64{|
int64_succ:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  incq  %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_unsafe_div : int64 -> int64 -> int64 = "%int64_unsafe_div"
let int64_unsafe_div x y = int64_unsafe_div x y
[%%expect_asm X86_64{|
int64_unsafe_div:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rax, %rbx
  jmp   .L1
.L0:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_unsafe_mod : int64 -> int64 -> int64 = "%int64_unsafe_mod"
let int64_unsafe_mod x y = int64_unsafe_mod x y
[%%expect_asm X86_64{|
int64_unsafe_mod:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  jmp   .L1
.L0:
  xorl  %edx, %edx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  movq  %rdx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int64_unsigned_compare : int64 -> int64 -> bool = "%int64_unsigned_compare"
let int64_unsigned_compare x y = int64_unsigned_compare x y
[%%expect_asm X86_64{|
int64_unsigned_compare:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int64_unsigned_greaterequal : int64 -> int64 -> bool = "%int64_unsigned_greaterequal"
let int64_unsigned_greaterequal x y = int64_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int64_unsigned_greaterequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_unsigned_greaterthan : int64 -> int64 -> bool = "%int64_unsigned_greaterthan"
let int64_unsigned_greaterthan x y = int64_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int64_unsigned_greaterthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_unsigned_lessequal : int64 -> int64 -> bool = "%int64_unsigned_lessequal"
let int64_unsigned_lessequal x y = int64_unsigned_lessequal x y
[%%expect_asm X86_64{|
int64_unsigned_lessequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_unsigned_lessthan : int64 -> int64 -> bool = "%int64_unsigned_lessthan"
let int64_unsigned_lessthan x y = int64_unsigned_lessthan x y
[%%expect_asm X86_64{|
int64_unsigned_lessthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_xor : int64 -> int64 -> int64 = "%int64_xor"
let int64_xor x y = int64_xor x y
[%%expect_asm X86_64{|
int64_xor:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  xorq  %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external int8_u_add : int8# -> int8# -> int8# = "%int8#_add"
let int8_u_add x y = int8_u_add x y
[%%expect_asm X86_64{|
int8_u_add:
  addq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_and : int8# -> int8# -> int8# = "%int8#_and"
let int8_u_and x y = int8_u_and x y
[%%expect_asm X86_64{|
int8_u_and:
  andq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_asr : int8# -> int -> int8# = "%int8#_asr"
let int8_u_asr x y = int8_u_asr x y
[%%expect_asm X86_64{|
int8_u_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int8_u_bswap : int8# -> int8# = "%int8#_bswap"
let int8_u_bswap x = int8_u_bswap x
[%%expect_asm X86_64{|
int8_u_bswap:
  ret
|}]

external int8_u_compare : int8# -> int8# -> bool = "%int8#_compare"
let int8_u_compare x y = int8_u_compare x y
[%%expect_asm X86_64{|
int8_u_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int8_u_div : int8# -> int8# -> int8# = "%int8#_div"
let int8_u_div x y = int8_u_div x y
[%%expect_asm X86_64{|
int8_u_div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int8_u_equal : int8# -> int8# -> bool = "%int8#_equal"
let int8_u_equal x y = int8_u_equal x y
[%%expect_asm X86_64{|
int8_u_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_greaterequal : int8# -> int8# -> bool = "%int8#_greaterequal"
let int8_u_greaterequal x y = int8_u_greaterequal x y
[%%expect_asm X86_64{|
int8_u_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_greaterthan : int8# -> int8# -> bool = "%int8#_greaterthan"
let int8_u_greaterthan x y = int8_u_greaterthan x y
[%%expect_asm X86_64{|
int8_u_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_lessequal : int8# -> int8# -> bool = "%int8#_lessequal"
let int8_u_lessequal x y = int8_u_lessequal x y
[%%expect_asm X86_64{|
int8_u_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_lessthan : int8# -> int8# -> bool = "%int8#_lessthan"
let int8_u_lessthan x y = int8_u_lessthan x y
[%%expect_asm X86_64{|
int8_u_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_lsl : int8# -> int -> int8# = "%int8#_lsl"
let int8_u_lsl x y = int8_u_lsl x y
[%%expect_asm X86_64{|
int8_u_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_lsr : int8# -> int -> int8# = "%int8#_lsr"
let int8_u_lsr x y = int8_u_lsr x y
[%%expect_asm X86_64{|
int8_u_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  andl  $255, %eax
  shrq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_mod : int8# -> int8# -> int8# = "%int8#_mod"
let int8_u_mod x y = int8_u_mod x y
[%%expect_asm X86_64{|
int8_u_mod:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int8_u_mul : int8# -> int8# -> int8# = "%int8#_mul"
let int8_u_mul x y = int8_u_mul x y
[%%expect_asm X86_64{|
int8_u_mul:
  imulq %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_neg : int8# -> int8# = "%int8#_neg"
let int8_u_neg x = int8_u_neg x
[%%expect_asm X86_64{|
int8_u_neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_notequal : int8# -> int8# -> bool = "%int8#_notequal"
let int8_u_notequal x y = int8_u_notequal x y
[%%expect_asm X86_64{|
int8_u_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_of_float : float -> int8# = "%int8#_of_float"
let int8_u_of_float x = int8_u_of_float x
[%%expect_asm X86_64{|
int8_u_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_float_u : float# -> int8# = "%int8#_of_float#"
let int8_u_of_float_u x = int8_u_of_float_u x
[%%expect_asm X86_64{|
int8_u_of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_float32 : float32 -> int8# = "%int8#_of_float32"
let int8_u_of_float32 x = int8_u_of_float32 x
[%%expect_asm X86_64{|
int8_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_float32_u : float32# -> int8# = "%int8#_of_float32#"
let int8_u_of_float32_u x = int8_u_of_float32_u x
[%%expect_asm X86_64{|
int8_u_of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int : int -> int8# = "%int8#_of_int"
let int8_u_of_int x = int8_u_of_int x
[%%expect_asm X86_64{|
int8_u_of_int:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int_u : int# -> int8# = "%int8#_of_int#"
let int8_u_of_int_u x = int8_u_of_int_u x
[%%expect_asm X86_64{|
int8_u_of_int_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int16 : int16 -> int8# = "%int8#_of_int16"
let int8_u_of_int16 x = int8_u_of_int16 x
[%%expect_asm X86_64{|
int8_u_of_int16:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int16_u : int16# -> int8# = "%int8#_of_int16#"
let int8_u_of_int16_u x = int8_u_of_int16_u x
[%%expect_asm X86_64{|
int8_u_of_int16_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int32 : int32 -> int8# = "%int8#_of_int32"
let int8_u_of_int32 x = int8_u_of_int32 x
[%%expect_asm X86_64{|
int8_u_of_int32:
  movslq 8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int32_u : int32# -> int8# = "%int8#_of_int32#"
let int8_u_of_int32_u x = int8_u_of_int32_u x
[%%expect_asm X86_64{|
int8_u_of_int32_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int64 : int64 -> int8# = "%int8#_of_int64"
let int8_u_of_int64 x = int8_u_of_int64 x
[%%expect_asm X86_64{|
int8_u_of_int64:
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int64_u : int64# -> int8# = "%int8#_of_int64#"
let int8_u_of_int64_u x = int8_u_of_int64_u x
[%%expect_asm X86_64{|
int8_u_of_int64_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int8 : int8 -> int8# = "%int8#_of_int8"
let int8_u_of_int8 x = int8_u_of_int8 x
[%%expect_asm X86_64{|
int8_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_nativeint : nativeint -> int8# = "%int8#_of_nativeint"
let int8_u_of_nativeint x = int8_u_of_nativeint x
[%%expect_asm X86_64{|
int8_u_of_nativeint:
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_nativeint_u : nativeint# -> int8# = "%int8#_of_nativeint#"
let int8_u_of_nativeint_u x = int8_u_of_nativeint_u x
[%%expect_asm X86_64{|
int8_u_of_nativeint_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_or : int8# -> int8# -> int8# = "%int8#_or"
let int8_u_or x y = int8_u_or x y
[%%expect_asm X86_64{|
int8_u_or:
  orq   %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_pred : int8# -> int8# = "%int8#_pred"
let int8_u_pred x = int8_u_pred x
[%%expect_asm X86_64{|
int8_u_pred:
  decq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_sub : int8# -> int8# -> int8# = "%int8#_sub"
let int8_u_sub x y = int8_u_sub x y
[%%expect_asm X86_64{|
int8_u_sub:
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_succ : int8# -> int8# = "%int8#_succ"
let int8_u_succ x = int8_u_succ x
[%%expect_asm X86_64{|
int8_u_succ:
  incq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_unsafe_div : int8# -> int8# -> int8# = "%int8#_unsafe_div"
let int8_u_unsafe_div x y = int8_u_unsafe_div x y
[%%expect_asm X86_64{|
int8_u_unsafe_div:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_unsafe_mod : int8# -> int8# -> int8# = "%int8#_unsafe_mod"
let int8_u_unsafe_mod x y = int8_u_unsafe_mod x y
[%%expect_asm X86_64{|
int8_u_unsafe_mod:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_unsigned_compare : int8# -> int8# -> bool = "%int8#_unsigned_compare"
let int8_u_unsigned_compare x y = int8_u_unsigned_compare x y
[%%expect_asm X86_64{|
int8_u_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int8_u_unsigned_greaterequal : int8# -> int8# -> bool = "%int8#_unsigned_greaterequal"
let int8_u_unsigned_greaterequal x y = int8_u_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int8_u_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_unsigned_greaterthan : int8# -> int8# -> bool = "%int8#_unsigned_greaterthan"
let int8_u_unsigned_greaterthan x y = int8_u_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int8_u_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_unsigned_lessequal : int8# -> int8# -> bool = "%int8#_unsigned_lessequal"
let int8_u_unsigned_lessequal x y = int8_u_unsigned_lessequal x y
[%%expect_asm X86_64{|
int8_u_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_unsigned_lessthan : int8# -> int8# -> bool = "%int8#_unsigned_lessthan"
let int8_u_unsigned_lessthan x y = int8_u_unsigned_lessthan x y
[%%expect_asm X86_64{|
int8_u_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_xor : int8# -> int8# -> int8# = "%int8#_xor"
let int8_u_xor x y = int8_u_xor x y
[%%expect_asm X86_64{|
int8_u_xor:
  xorq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_add : int8 -> int8 -> int8 = "%int8_add"
let int8_add x y = int8_add x y
[%%expect_asm X86_64{|
int8_add:
  sarq  $1, %rbx
  sarq  $1, %rax
  addq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_and : int8 -> int8 -> int8 = "%int8_and"
let int8_and x y = int8_and x y
[%%expect_asm X86_64{|
int8_and:
  sarq  $1, %rbx
  sarq  $1, %rax
  andq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_asr : int8 -> int -> int8 = "%int8_asr"
let int8_asr x y = int8_asr x y
[%%expect_asm X86_64{|
int8_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  $55, %rax
  sarq  %cl, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_bswap : int8 -> int8 = "%int8_bswap"
let int8_bswap x = int8_bswap x
[%%expect_asm X86_64{|
int8_bswap:
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_compare : int8 -> int8 -> bool = "%int8_compare"
let int8_compare x y = int8_compare x y
[%%expect_asm X86_64{|
int8_compare:
  movq  %rax, %rdi
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rdi
  sarq  $56, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int8_div : int8 -> int8 -> int8 = "%int8_div"
let int8_div x y = int8_div x y
[%%expect_asm X86_64{|
int8_div:
  movq  %rbx, %rcx
  salq  $55, %rcx
  sarq  $56, %rcx
  testq %rcx, %rcx
  je    .L0
  salq  $55, %rax
  sarq  $56, %rax
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int8_equal : int8 -> int8 -> bool = "%int8_equal"
let int8_equal x y = int8_equal x y
[%%expect_asm X86_64{|
int8_equal:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_greaterequal : int8 -> int8 -> bool = "%int8_greaterequal"
let int8_greaterequal x y = int8_greaterequal x y
[%%expect_asm X86_64{|
int8_greaterequal:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_greaterthan : int8 -> int8 -> bool = "%int8_greaterthan"
let int8_greaterthan x y = int8_greaterthan x y
[%%expect_asm X86_64{|
int8_greaterthan:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_lessequal : int8 -> int8 -> bool = "%int8_lessequal"
let int8_lessequal x y = int8_lessequal x y
[%%expect_asm X86_64{|
int8_lessequal:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_lessthan : int8 -> int8 -> bool = "%int8_lessthan"
let int8_lessthan x y = int8_lessthan x y
[%%expect_asm X86_64{|
int8_lessthan:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_lsl : int8 -> int -> int8 = "%int8_lsl"
let int8_lsl x y = int8_lsl x y
[%%expect_asm X86_64{|
int8_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  $55, %rax
  sarq  $56, %rax
  salq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_lsr : int8 -> int -> int8 = "%int8_lsr"
let int8_lsr x y = int8_lsr x y
[%%expect_asm X86_64{|
int8_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  $1, %rax
  andl  $255, %eax
  shrq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_mod : int8 -> int8 -> int8 = "%int8_mod"
let int8_mod x y = int8_mod x y
[%%expect_asm X86_64{|
int8_mod:
  movq  %rbx, %rcx
  salq  $55, %rcx
  sarq  $56, %rcx
  testq %rcx, %rcx
  je    .L0
  salq  $55, %rax
  sarq  $56, %rax
  cqto
  idivq %rcx
  salq  $56, %rdx
  sarq  $56, %rdx
  leaq  1(%rdx,%rdx), %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int8_mul : int8 -> int8 -> int8 = "%int8_mul"
let int8_mul x y = int8_mul x y
[%%expect_asm X86_64{|
int8_mul:
  sarq  $1, %rbx
  sarq  $1, %rax
  imulq %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_neg : int8 -> int8 = "%int8_neg"
let int8_neg x = int8_neg x
[%%expect_asm X86_64{|
int8_neg:
  sarq  $1, %rax
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  salq  $56, %rbx
  sarq  $56, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

external int8_notequal : int8 -> int8 -> bool = "%int8_notequal"
let int8_notequal x y = int8_notequal x y
[%%expect_asm X86_64{|
int8_notequal:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float : float -> int8 = "%int8_of_float"
let int8_of_float x = int8_of_float x
[%%expect_asm X86_64{|
int8_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float_u : float# -> int8 = "%int8_of_float#"
let int8_of_float_u x = int8_of_float_u x
[%%expect_asm X86_64{|
int8_of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float32 : float32 -> int8 = "%int8_of_float32"
let int8_of_float32 x = int8_of_float32 x
[%%expect_asm X86_64{|
int8_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float32_u : float32# -> int8 = "%int8_of_float32#"
let int8_of_float32_u x = int8_of_float32_u x
[%%expect_asm X86_64{|
int8_of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int : int -> int8 = "%int8_of_int"
let int8_of_int x = int8_of_int x
[%%expect_asm X86_64{|
int8_of_int:
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int_u : int# -> int8 = "%int8_of_int#"
let int8_of_int_u x = int8_of_int_u x
[%%expect_asm X86_64{|
int8_of_int_u:
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int16 : int16 -> int8 = "%int8_of_int16"
let int8_of_int16 x = int8_of_int16 x
[%%expect_asm X86_64{|
int8_of_int16:
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int16_u : int16# -> int8 = "%int8_of_int16#"
let int8_of_int16_u x = int8_of_int16_u x
[%%expect_asm X86_64{|
int8_of_int16_u:
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int32 : int32 -> int8 = "%int8_of_int32"
let int8_of_int32 x = int8_of_int32 x
[%%expect_asm X86_64{|
int8_of_int32:
  movslq 8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int32_u : int32# -> int8 = "%int8_of_int32#"
let int8_of_int32_u x = int8_of_int32_u x
[%%expect_asm X86_64{|
int8_of_int32_u:
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int64 : int64 -> int8 = "%int8_of_int64"
let int8_of_int64 x = int8_of_int64 x
[%%expect_asm X86_64{|
int8_of_int64:
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int64_u : int64# -> int8 = "%int8_of_int64#"
let int8_of_int64_u x = int8_of_int64_u x
[%%expect_asm X86_64{|
int8_of_int64_u:
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int8_u : int8# -> int8 = "%int8_of_int8#"
let int8_of_int8_u x = int8_of_int8_u x
[%%expect_asm X86_64{|
int8_of_int8_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_nativeint : nativeint -> int8 = "%int8_of_nativeint"
let int8_of_nativeint x = int8_of_nativeint x
[%%expect_asm X86_64{|
int8_of_nativeint:
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_nativeint_u : nativeint# -> int8 = "%int8_of_nativeint#"
let int8_of_nativeint_u x = int8_of_nativeint_u x
[%%expect_asm X86_64{|
int8_of_nativeint_u:
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_or : int8 -> int8 -> int8 = "%int8_or"
let int8_or x y = int8_or x y
[%%expect_asm X86_64{|
int8_or:
  sarq  $1, %rbx
  sarq  $1, %rax
  orq   %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_pred : int8 -> int8 = "%int8_pred"
let int8_pred x = int8_pred x
[%%expect_asm X86_64{|
int8_pred:
  sarq  $1, %rax
  decq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_sub : int8 -> int8 -> int8 = "%int8_sub"
let int8_sub x y = int8_sub x y
[%%expect_asm X86_64{|
int8_sub:
  sarq  $1, %rbx
  sarq  $1, %rax
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_succ : int8 -> int8 = "%int8_succ"
let int8_succ x = int8_succ x
[%%expect_asm X86_64{|
int8_succ:
  sarq  $1, %rax
  incq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_unsafe_div : int8 -> int8 -> int8 = "%int8_unsafe_div"
let int8_unsafe_div x y = int8_unsafe_div x y
[%%expect_asm X86_64{|
int8_unsafe_div:
  movq  %rbx, %rcx
  salq  $55, %rcx
  sarq  $56, %rcx
  salq  $55, %rax
  sarq  $56, %rax
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_unsafe_mod : int8 -> int8 -> int8 = "%int8_unsafe_mod"
let int8_unsafe_mod x y = int8_unsafe_mod x y
[%%expect_asm X86_64{|
int8_unsafe_mod:
  movq  %rbx, %rcx
  salq  $55, %rcx
  sarq  $56, %rcx
  salq  $55, %rax
  sarq  $56, %rax
  cqto
  idivq %rcx
  salq  $56, %rdx
  sarq  $56, %rdx
  leaq  1(%rdx,%rdx), %rax
  ret
|}]

external int8_unsigned_compare : int8 -> int8 -> bool = "%int8_unsigned_compare"
let int8_unsigned_compare x y = int8_unsigned_compare x y
[%%expect_asm X86_64{|
int8_unsigned_compare:
  movq  %rax, %rdi
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rdi
  sarq  $56, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int8_unsigned_greaterequal : int8 -> int8 -> bool = "%int8_unsigned_greaterequal"
let int8_unsigned_greaterequal x y = int8_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int8_unsigned_greaterequal:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_unsigned_greaterthan : int8 -> int8 -> bool = "%int8_unsigned_greaterthan"
let int8_unsigned_greaterthan x y = int8_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int8_unsigned_greaterthan:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_unsigned_lessequal : int8 -> int8 -> bool = "%int8_unsigned_lessequal"
let int8_unsigned_lessequal x y = int8_unsigned_lessequal x y
[%%expect_asm X86_64{|
int8_unsigned_lessequal:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_unsigned_lessthan : int8 -> int8 -> bool = "%int8_unsigned_lessthan"
let int8_unsigned_lessthan x y = int8_unsigned_lessthan x y
[%%expect_asm X86_64{|
int8_unsigned_lessthan:
  salq  $55, %rbx
  sarq  $56, %rbx
  salq  $55, %rax
  sarq  $56, %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_xor : int8 -> int8 -> int8 = "%int8_xor"
let int8_xor x y = int8_xor x y
[%%expect_asm X86_64{|
int8_xor:
  sarq  $1, %rbx
  sarq  $1, %rax
  xorq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_add : int -> int -> int = "%int_add"
let int_add x y = int_add x y
[%%expect_asm X86_64{|
int_add:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]

external int_and : int -> int -> int = "%int_and"
let int_and x y = int_and x y
[%%expect_asm X86_64{|
int_and:
  andq  %rbx, %rax
  ret
|}]

external int_asr : int -> int -> int = "%int_asr"
let int_asr x y = int_asr x y
[%%expect_asm X86_64{|
int_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  orq   $1, %rax
  ret
|}]

external int_bswap : int -> int = "%int_bswap"
let int_bswap x = int_bswap x
[%%expect_asm X86_64{|
int_bswap:
  sarq  $1, %rax
  xchg  %ah, %al
  movzwq %ax, %rax
  andl  $65535, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_compare : int -> int -> bool = "%int_compare"
let int_compare x y = int_compare x y
[%%expect_asm X86_64{|
int_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int_div : int -> int -> int = "%int_div"
let int_div x y = int_div x y
[%%expect_asm X86_64{|
int_div:
  movq  %rbx, %rcx
  cmpq  $1, %rcx
  je    .L0
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rax,%rax), %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int_equal : int -> int -> bool = "%int_equal"
let int_equal x y = int_equal x y
[%%expect_asm X86_64{|
int_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_greaterequal : int -> int -> bool = "%int_greaterequal"
let int_greaterequal x y = int_greaterequal x y
[%%expect_asm X86_64{|
int_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_greaterthan : int -> int -> bool = "%int_greaterthan"
let int_greaterthan x y = int_greaterthan x y
[%%expect_asm X86_64{|
int_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_lessequal : int -> int -> bool = "%int_lessequal"
let int_lessequal x y = int_lessequal x y
[%%expect_asm X86_64{|
int_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_lessthan : int -> int -> bool = "%int_lessthan"
let int_lessthan x y = int_lessthan x y
[%%expect_asm X86_64{|
int_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_lsl : int -> int -> int = "%int_lsl"
let int_lsl x y = int_lsl x y
[%%expect_asm X86_64{|
int_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  decq  %rax
  salq  %cl, %rax
  incq  %rax
  ret
|}]

external int_lsr : int -> int -> int = "%int_lsr"
let int_lsr x y = int_lsr x y
[%%expect_asm X86_64{|
int_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  orq   $1, %rax
  ret
|}]

external int_mod : int -> int -> int = "%int_mod"
let int_mod x y = int_mod x y
[%%expect_asm X86_64{|
int_mod:
  movq  %rbx, %rcx
  cmpq  $1, %rcx
  je    .L0
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rdx,%rdx), %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external int_mul : int -> int -> int = "%int_mul"
let int_mul x y = int_mul x y
[%%expect_asm X86_64{|
int_mul:
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  ret
|}]

external int_neg : int -> int = "%int_neg"
let int_neg x = int_neg x
[%%expect_asm X86_64{|
int_neg:
  movq  %rax, %rbx
  movl  $2, %eax
  subq  %rbx, %rax
  ret
|}]

external int_notequal : int -> int -> bool = "%int_notequal"
let int_notequal x y = int_notequal x y
[%%expect_asm X86_64{|
int_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float : float -> int = "%int_of_float"
let int_of_float x = int_of_float x
[%%expect_asm X86_64{|
int_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float_u : float# -> int = "%int_of_float#"
let int_of_float_u x = int_of_float_u x
[%%expect_asm X86_64{|
int_of_float_u:
  vcvttsd2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float32 : float32 -> int = "%int_of_float32"
let int_of_float32 x = int_of_float32 x
[%%expect_asm X86_64{|
int_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float32_u : float32# -> int = "%int_of_float32#"
let int_of_float32_u x = int_of_float32_u x
[%%expect_asm X86_64{|
int_of_float32_u:
  vcvttss2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int_u : int# -> int = "%int_of_int#"
let int_of_int_u x = int_of_int_u x
[%%expect_asm X86_64{|
int_of_int_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int16 : int16 -> int = "%int_of_int16"
let int_of_int16 x = int_of_int16 x
[%%expect_asm X86_64{|
int_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int16_u : int16# -> int = "%int_of_int16#"
let int_of_int16_u x = int_of_int16_u x
[%%expect_asm X86_64{|
int_of_int16_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int32 : int32 -> int = "%int_of_int32"
let int_of_int32 x = int_of_int32 x
[%%expect_asm X86_64{|
int_of_int32:
  movslq 8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int32_u : int32# -> int = "%int_of_int32#"
let int_of_int32_u x = int_of_int32_u x
[%%expect_asm X86_64{|
int_of_int32_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int64 : int64 -> int = "%int_of_int64"
let int_of_int64 x = int_of_int64 x
[%%expect_asm X86_64{|
int_of_int64:
  movq  8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int64_u : int64# -> int = "%int_of_int64#"
let int_of_int64_u x = int_of_int64_u x
[%%expect_asm X86_64{|
int_of_int64_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int8 : int8 -> int = "%int_of_int8"
let int_of_int8 x = int_of_int8 x
[%%expect_asm X86_64{|
int_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int8_u : int8# -> int = "%int_of_int8#"
let int_of_int8_u x = int_of_int8_u x
[%%expect_asm X86_64{|
int_of_int8_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_nativeint : nativeint -> int = "%int_of_nativeint"
let int_of_nativeint x = int_of_nativeint x
[%%expect_asm X86_64{|
int_of_nativeint:
  movq  8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_nativeint_u : nativeint# -> int = "%int_of_nativeint#"
let int_of_nativeint_u x = int_of_nativeint_u x
[%%expect_asm X86_64{|
int_of_nativeint_u:
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_or : int -> int -> int = "%int_or"
let int_or x y = int_or x y
[%%expect_asm X86_64{|
int_or:
  orq   %rbx, %rax
  ret
|}]

external int_pred : int -> int = "%int_pred"
let int_pred x = int_pred x
[%%expect_asm X86_64{|
int_pred:
  addq  $-2, %rax
  ret
|}]

external int_sub : int -> int -> int = "%int_sub"
let int_sub x y = int_sub x y
[%%expect_asm X86_64{|
int_sub:
  subq  %rbx, %rax
  incq  %rax
  ret
|}]

external int_succ : int -> int = "%int_succ"
let int_succ x = int_succ x
[%%expect_asm X86_64{|
int_succ:
  addq  $2, %rax
  ret
|}]

external int_unsafe_div : int -> int -> int = "%int_unsafe_div"
let int_unsafe_div x y = int_unsafe_div x y
[%%expect_asm X86_64{|
int_unsafe_div:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsafe_mod : int -> int -> int = "%int_unsafe_mod"
let int_unsafe_mod x y = int_unsafe_mod x y
[%%expect_asm X86_64{|
int_unsafe_mod:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rdx,%rdx), %rax
  ret
|}]

external int_unsigned_compare : int -> int -> bool = "%int_unsigned_compare"
let int_unsigned_compare x y = int_unsigned_compare x y
[%%expect_asm X86_64{|
int_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external int_unsigned_greaterequal : int -> int -> bool = "%int_unsigned_greaterequal"
let int_unsigned_greaterequal x y = int_unsigned_greaterequal x y
[%%expect_asm X86_64{|
int_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsigned_greaterthan : int -> int -> bool = "%int_unsigned_greaterthan"
let int_unsigned_greaterthan x y = int_unsigned_greaterthan x y
[%%expect_asm X86_64{|
int_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsigned_lessequal : int -> int -> bool = "%int_unsigned_lessequal"
let int_unsigned_lessequal x y = int_unsigned_lessequal x y
[%%expect_asm X86_64{|
int_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsigned_lessthan : int -> int -> bool = "%int_unsigned_lessthan"
let int_unsigned_lessthan x y = int_unsigned_lessthan x y
[%%expect_asm X86_64{|
int_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_xor : int -> int -> int = "%int_xor"
let int_xor x y = int_xor x y
[%%expect_asm X86_64{|
int_xor:
  xorq  %rbx, %rax
  orq   $1, %rax
  ret
|}]

external nativeint_u_add : nativeint# -> nativeint# -> nativeint# = "%nativeint#_add"
let nativeint_u_add x y = nativeint_u_add x y
[%%expect_asm X86_64{|
nativeint_u_add:
  addq  %rbx, %rax
  ret
|}]

external nativeint_u_and : nativeint# -> nativeint# -> nativeint# = "%nativeint#_and"
let nativeint_u_and x y = nativeint_u_and x y
[%%expect_asm X86_64{|
nativeint_u_and:
  andq  %rbx, %rax
  ret
|}]

external nativeint_u_asr : nativeint# -> int -> nativeint# = "%nativeint#_asr"
let nativeint_u_asr x y = nativeint_u_asr x y
[%%expect_asm X86_64{|
nativeint_u_asr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external nativeint_u_bswap : nativeint# -> nativeint# = "%nativeint#_bswap"
let nativeint_u_bswap x = nativeint_u_bswap x
[%%expect_asm X86_64{|
nativeint_u_bswap:
  bswap %rax
  ret
|}]

external nativeint_u_compare : nativeint# -> nativeint# -> bool = "%nativeint#_compare"
let nativeint_u_compare x y = nativeint_u_compare x y
[%%expect_asm X86_64{|
nativeint_u_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external nativeint_u_div : nativeint# -> nativeint# -> nativeint# = "%nativeint#_div"
let nativeint_u_div x y = nativeint_u_div x y
[%%expect_asm X86_64{|
nativeint_u_div:
  movq  %rax, %rdi
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L1
  cmpq  $-1, %rcx
  je    .L0
  movq  %rdi, %rax
  cqto
  idivq %rcx
  ret
.L0:
  xorl  %eax, %eax
  subq  %rdi, %rax
  ret
.L1:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external nativeint_u_equal : nativeint# -> nativeint# -> bool = "%nativeint#_equal"
let nativeint_u_equal x y = nativeint_u_equal x y
[%%expect_asm X86_64{|
nativeint_u_equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_greaterequal : nativeint# -> nativeint# -> bool = "%nativeint#_greaterequal"
let nativeint_u_greaterequal x y = nativeint_u_greaterequal x y
[%%expect_asm X86_64{|
nativeint_u_greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_greaterthan : nativeint# -> nativeint# -> bool = "%nativeint#_greaterthan"
let nativeint_u_greaterthan x y = nativeint_u_greaterthan x y
[%%expect_asm X86_64{|
nativeint_u_greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_lessequal : nativeint# -> nativeint# -> bool = "%nativeint#_lessequal"
let nativeint_u_lessequal x y = nativeint_u_lessequal x y
[%%expect_asm X86_64{|
nativeint_u_lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_lessthan : nativeint# -> nativeint# -> bool = "%nativeint#_lessthan"
let nativeint_u_lessthan x y = nativeint_u_lessthan x y
[%%expect_asm X86_64{|
nativeint_u_lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_lsl : nativeint# -> int -> nativeint# = "%nativeint#_lsl"
let nativeint_u_lsl x y = nativeint_u_lsl x y
[%%expect_asm X86_64{|
nativeint_u_lsl:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  ret
|}]

external nativeint_u_lsr : nativeint# -> int -> nativeint# = "%nativeint#_lsr"
let nativeint_u_lsr x y = nativeint_u_lsr x y
[%%expect_asm X86_64{|
nativeint_u_lsr:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  ret
|}]

external nativeint_u_mod : nativeint# -> nativeint# -> nativeint# = "%nativeint#_mod"
let nativeint_u_mod x y = nativeint_u_mod x y
[%%expect_asm X86_64{|
nativeint_u_mod:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L1
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  ret
.L0:
  xorl  %eax, %eax
  ret
.L1:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external nativeint_u_mul : nativeint# -> nativeint# -> nativeint# = "%nativeint#_mul"
let nativeint_u_mul x y = nativeint_u_mul x y
[%%expect_asm X86_64{|
nativeint_u_mul:
  imulq %rbx, %rax
  ret
|}]

external nativeint_u_neg : nativeint# -> nativeint# = "%nativeint#_neg"
let nativeint_u_neg x = nativeint_u_neg x
[%%expect_asm X86_64{|
nativeint_u_neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

external nativeint_u_notequal : nativeint# -> nativeint# -> bool = "%nativeint#_notequal"
let nativeint_u_notequal x y = nativeint_u_notequal x y
[%%expect_asm X86_64{|
nativeint_u_notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_of_float : float -> nativeint# = "%nativeint#_of_float"
let nativeint_u_of_float x = nativeint_u_of_float x
[%%expect_asm X86_64{|
nativeint_u_of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_float_u : float# -> nativeint# = "%nativeint#_of_float#"
let nativeint_u_of_float_u x = nativeint_u_of_float_u x
[%%expect_asm X86_64{|
nativeint_u_of_float_u:
  vcvttsd2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_float32 : float32 -> nativeint# = "%nativeint#_of_float32"
let nativeint_u_of_float32 x = nativeint_u_of_float32 x
[%%expect_asm X86_64{|
nativeint_u_of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_float32_u : float32# -> nativeint# = "%nativeint#_of_float32#"
let nativeint_u_of_float32_u x = nativeint_u_of_float32_u x
[%%expect_asm X86_64{|
nativeint_u_of_float32_u:
  vcvttss2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_int : int -> nativeint# = "%nativeint#_of_int"
let nativeint_u_of_int x = nativeint_u_of_int x
[%%expect_asm X86_64{|
nativeint_u_of_int:
  sarq  $1, %rax
  ret
|}]

external nativeint_u_of_int_u : int# -> nativeint# = "%nativeint#_of_int#"
let nativeint_u_of_int_u x = nativeint_u_of_int_u x
[%%expect_asm X86_64{|
nativeint_u_of_int_u:
  ret
|}]

external nativeint_u_of_int16 : int16 -> nativeint# = "%nativeint#_of_int16"
let nativeint_u_of_int16 x = nativeint_u_of_int16 x
[%%expect_asm X86_64{|
nativeint_u_of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external nativeint_u_of_int16_u : int16# -> nativeint# = "%nativeint#_of_int16#"
let nativeint_u_of_int16_u x = nativeint_u_of_int16_u x
[%%expect_asm X86_64{|
nativeint_u_of_int16_u:
  ret
|}]

external nativeint_u_of_int32 : int32 -> nativeint# = "%nativeint#_of_int32"
let nativeint_u_of_int32 x = nativeint_u_of_int32 x
[%%expect_asm X86_64{|
nativeint_u_of_int32:
  movslq 8(%rax), %rax
  ret
|}]

external nativeint_u_of_int32_u : int32# -> nativeint# = "%nativeint#_of_int32#"
let nativeint_u_of_int32_u x = nativeint_u_of_int32_u x
[%%expect_asm X86_64{|
nativeint_u_of_int32_u:
  ret
|}]

external nativeint_u_of_int64 : int64 -> nativeint# = "%nativeint#_of_int64"
let nativeint_u_of_int64 x = nativeint_u_of_int64 x
[%%expect_asm X86_64{|
nativeint_u_of_int64:
  movq  8(%rax), %rax
  ret
|}]

external nativeint_u_of_int64_u : int64# -> nativeint# = "%nativeint#_of_int64#"
let nativeint_u_of_int64_u x = nativeint_u_of_int64_u x
[%%expect_asm X86_64{|
nativeint_u_of_int64_u:
  ret
|}]

external nativeint_u_of_int8 : int8 -> nativeint# = "%nativeint#_of_int8"
let nativeint_u_of_int8 x = nativeint_u_of_int8 x
[%%expect_asm X86_64{|
nativeint_u_of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external nativeint_u_of_int8_u : int8# -> nativeint# = "%nativeint#_of_int8#"
let nativeint_u_of_int8_u x = nativeint_u_of_int8_u x
[%%expect_asm X86_64{|
nativeint_u_of_int8_u:
  ret
|}]

external nativeint_u_of_nativeint : nativeint -> nativeint# = "%nativeint#_of_nativeint"
let nativeint_u_of_nativeint x = nativeint_u_of_nativeint x
[%%expect_asm X86_64{|
nativeint_u_of_nativeint:
  movq  8(%rax), %rax
  ret
|}]

external nativeint_u_or : nativeint# -> nativeint# -> nativeint# = "%nativeint#_or"
let nativeint_u_or x y = nativeint_u_or x y
[%%expect_asm X86_64{|
nativeint_u_or:
  orq   %rbx, %rax
  ret
|}]

external nativeint_u_pred : nativeint# -> nativeint# = "%nativeint#_pred"
let nativeint_u_pred x = nativeint_u_pred x
[%%expect_asm X86_64{|
nativeint_u_pred:
  decq  %rax
  ret
|}]

external nativeint_u_sub : nativeint# -> nativeint# -> nativeint# = "%nativeint#_sub"
let nativeint_u_sub x y = nativeint_u_sub x y
[%%expect_asm X86_64{|
nativeint_u_sub:
  subq  %rbx, %rax
  ret
|}]

external nativeint_u_succ : nativeint# -> nativeint# = "%nativeint#_succ"
let nativeint_u_succ x = nativeint_u_succ x
[%%expect_asm X86_64{|
nativeint_u_succ:
  incq  %rax
  ret
|}]

external nativeint_u_unsafe_div : nativeint# -> nativeint# -> nativeint# = "%nativeint#_unsafe_div"
let nativeint_u_unsafe_div x y = nativeint_u_unsafe_div x y
[%%expect_asm X86_64{|
nativeint_u_unsafe_div:
  movq  %rax, %rdi
  movq  %rbx, %rcx
  cmpq  $-1, %rcx
  je    .L0
  movq  %rdi, %rax
  cqto
  idivq %rcx
  ret
.L0:
  xorl  %eax, %eax
  subq  %rdi, %rax
  ret
|}]

external nativeint_u_unsafe_mod : nativeint# -> nativeint# -> nativeint# = "%nativeint#_unsafe_mod"
let nativeint_u_unsafe_mod x y = nativeint_u_unsafe_mod x y
[%%expect_asm X86_64{|
nativeint_u_unsafe_mod:
  movq  %rbx, %rcx
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rdx, %rax
  ret
.L0:
  xorl  %eax, %eax
  ret
|}]

external nativeint_u_unsigned_compare : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_compare"
let nativeint_u_unsigned_compare x y = nativeint_u_unsigned_compare x y
[%%expect_asm X86_64{|
nativeint_u_unsigned_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external nativeint_u_unsigned_greaterequal : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_greaterequal"
let nativeint_u_unsigned_greaterequal x y = nativeint_u_unsigned_greaterequal x y
[%%expect_asm X86_64{|
nativeint_u_unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_unsigned_greaterthan : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_greaterthan"
let nativeint_u_unsigned_greaterthan x y = nativeint_u_unsigned_greaterthan x y
[%%expect_asm X86_64{|
nativeint_u_unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_unsigned_lessequal : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_lessequal"
let nativeint_u_unsigned_lessequal x y = nativeint_u_unsigned_lessequal x y
[%%expect_asm X86_64{|
nativeint_u_unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_unsigned_lessthan : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_lessthan"
let nativeint_u_unsigned_lessthan x y = nativeint_u_unsigned_lessthan x y
[%%expect_asm X86_64{|
nativeint_u_unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_xor : nativeint# -> nativeint# -> nativeint# = "%nativeint#_xor"
let nativeint_u_xor x y = nativeint_u_xor x y
[%%expect_asm X86_64{|
nativeint_u_xor:
  xorq  %rbx, %rax
  ret
|}]

external nativeint_add : nativeint -> nativeint -> nativeint = "%nativeint_add"
let nativeint_add x y = nativeint_add x y
[%%expect_asm X86_64{|
nativeint_add:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rbx
  movq  8(%rdi), %rdi
  addq  %rdi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_and : nativeint -> nativeint -> nativeint = "%nativeint_and"
let nativeint_and x y = nativeint_and x y
[%%expect_asm X86_64{|
nativeint_and:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  andq  %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_asr : nativeint -> int -> nativeint = "%nativeint_asr"
let nativeint_asr x y = nativeint_asr x y
[%%expect_asm X86_64{|
nativeint_asr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movq  8(%rdi), %rbx
  sarq  %cl, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_bswap : nativeint -> nativeint = "%nativeint_bswap"
let nativeint_bswap x = nativeint_bswap x
[%%expect_asm X86_64{|
nativeint_bswap:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  bswap %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_compare : nativeint -> nativeint -> bool = "%nativeint_compare"
let nativeint_compare x y = nativeint_compare x y
[%%expect_asm X86_64{|
nativeint_compare:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external nativeint_div : nativeint -> nativeint -> nativeint = "%nativeint_div"
let nativeint_div x y = nativeint_div x y
[%%expect_asm X86_64{|
nativeint_div:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  testq %rcx, %rcx
  je    .L3
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rax, %rbx
  jmp   .L1
.L0:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
.L3:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external nativeint_equal : nativeint -> nativeint -> bool = "%nativeint_equal"
let nativeint_equal x y = nativeint_equal x y
[%%expect_asm X86_64{|
nativeint_equal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_greaterequal : nativeint -> nativeint -> bool = "%nativeint_greaterequal"
let nativeint_greaterequal x y = nativeint_greaterequal x y
[%%expect_asm X86_64{|
nativeint_greaterequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_greaterthan : nativeint -> nativeint -> bool = "%nativeint_greaterthan"
let nativeint_greaterthan x y = nativeint_greaterthan x y
[%%expect_asm X86_64{|
nativeint_greaterthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_lessequal : nativeint -> nativeint -> bool = "%nativeint_lessequal"
let nativeint_lessequal x y = nativeint_lessequal x y
[%%expect_asm X86_64{|
nativeint_lessequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_lessthan : nativeint -> nativeint -> bool = "%nativeint_lessthan"
let nativeint_lessthan x y = nativeint_lessthan x y
[%%expect_asm X86_64{|
nativeint_lessthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_lsl : nativeint -> int -> nativeint = "%nativeint_lsl"
let nativeint_lsl x y = nativeint_lsl x y
[%%expect_asm X86_64{|
nativeint_lsl:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movq  8(%rdi), %rbx
  salq  %cl, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_lsr : nativeint -> int -> nativeint = "%nativeint_lsr"
let nativeint_lsr x y = nativeint_lsr x y
[%%expect_asm X86_64{|
nativeint_lsr:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rcx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  sarq  $1, %rcx
  movq  8(%rdi), %rbx
  shrq  %cl, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_mod : nativeint -> nativeint -> nativeint = "%nativeint_mod"
let nativeint_mod x y = nativeint_mod x y
[%%expect_asm X86_64{|
nativeint_mod:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  testq %rcx, %rcx
  je    .L3
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  jmp   .L1
.L0:
  xorl  %edx, %edx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  movq  %rdx, 8(%rax)
  addq  $8, %rsp
  ret
.L3:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

external nativeint_mul : nativeint -> nativeint -> nativeint = "%nativeint_mul"
let nativeint_mul x y = nativeint_mul x y
[%%expect_asm X86_64{|
nativeint_mul:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  imulq %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_neg : nativeint -> nativeint = "%nativeint_neg"
let nativeint_neg x = nativeint_neg x
[%%expect_asm X86_64{|
nativeint_neg:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rdi
  xorl  %ebx, %ebx
  subq  %rdi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_notequal : nativeint -> nativeint -> bool = "%nativeint_notequal"
let nativeint_notequal x y = nativeint_notequal x y
[%%expect_asm X86_64{|
nativeint_notequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_of_float : float -> nativeint = "%nativeint_of_float"
let nativeint_of_float x = nativeint_of_float x
[%%expect_asm X86_64{|
nativeint_of_float:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovsd (%rbx), %xmm0
  vcvttsd2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_float_u : float# -> nativeint = "%nativeint_of_float#"
let nativeint_of_float_u x = nativeint_of_float_u x
[%%expect_asm X86_64{|
nativeint_of_float_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvttsd2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_float32 : float32 -> nativeint = "%nativeint_of_float32"
let nativeint_of_float32 x = nativeint_of_float32 x
[%%expect_asm X86_64{|
nativeint_of_float32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vmovss 8(%rbx), %xmm0
  vcvttss2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_float32_u : float32# -> nativeint = "%nativeint_of_float32#"
let nativeint_of_float32_u x = nativeint_of_float32_u x
[%%expect_asm X86_64{|
nativeint_of_float32_u:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  vcvttss2si %xmm0, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int : int -> nativeint = "%nativeint_of_int"
let nativeint_of_int x = nativeint_of_int x
[%%expect_asm X86_64{|
nativeint_of_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  sarq  $1, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int_u : int# -> nativeint = "%nativeint_of_int#"
let nativeint_of_int_u x = nativeint_of_int_u x
[%%expect_asm X86_64{|
nativeint_of_int_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int16 : int16 -> nativeint = "%nativeint_of_int16"
let nativeint_of_int16 x = nativeint_of_int16 x
[%%expect_asm X86_64{|
nativeint_of_int16:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $47, %rbx
  sarq  $48, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int16_u : int16# -> nativeint = "%nativeint_of_int16#"
let nativeint_of_int16_u x = nativeint_of_int16_u x
[%%expect_asm X86_64{|
nativeint_of_int16_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int32 : int32 -> nativeint = "%nativeint_of_int32"
let nativeint_of_int32 x = nativeint_of_int32 x
[%%expect_asm X86_64{|
nativeint_of_int32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq 8(%rbx), %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int32_u : int32# -> nativeint = "%nativeint_of_int32#"
let nativeint_of_int32_u x = nativeint_of_int32_u x
[%%expect_asm X86_64{|
nativeint_of_int32_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int64 : int64 -> nativeint = "%nativeint_of_int64"
let nativeint_of_int64 x = nativeint_of_int64 x
[%%expect_asm X86_64{|
nativeint_of_int64:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int64_u : int64# -> nativeint = "%nativeint_of_int64#"
let nativeint_of_int64_u x = nativeint_of_int64_u x
[%%expect_asm X86_64{|
nativeint_of_int64_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int8 : int8 -> nativeint = "%nativeint_of_int8"
let nativeint_of_int8 x = nativeint_of_int8 x
[%%expect_asm X86_64{|
nativeint_of_int8:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  salq  $55, %rbx
  sarq  $56, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_int8_u : int8# -> nativeint = "%nativeint_of_int8#"
let nativeint_of_int8_u x = nativeint_of_int8_u x
[%%expect_asm X86_64{|
nativeint_of_int8_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_of_nativeint_u : nativeint# -> nativeint = "%nativeint_of_nativeint#"
let nativeint_of_nativeint_u x = nativeint_of_nativeint_u x
[%%expect_asm X86_64{|
nativeint_of_nativeint_u:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_or : nativeint -> nativeint -> nativeint = "%nativeint_or"
let nativeint_or x y = nativeint_or x y
[%%expect_asm X86_64{|
nativeint_or:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  orq   %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_pred : nativeint -> nativeint = "%nativeint_pred"
let nativeint_pred x = nativeint_pred x
[%%expect_asm X86_64{|
nativeint_pred:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  decq  %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_sub : nativeint -> nativeint -> nativeint = "%nativeint_sub"
let nativeint_sub x y = nativeint_sub x y
[%%expect_asm X86_64{|
nativeint_sub:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  subq  %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_succ : nativeint -> nativeint = "%nativeint_succ"
let nativeint_succ x = nativeint_succ x
[%%expect_asm X86_64{|
nativeint_succ:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  8(%rbx), %rbx
  incq  %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_unsafe_div : nativeint -> nativeint -> nativeint = "%nativeint_unsafe_div"
let nativeint_unsafe_div x y = nativeint_unsafe_div x y
[%%expect_asm X86_64{|
nativeint_unsafe_div:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  movq  %rax, %rbx
  jmp   .L1
.L0:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_unsafe_mod : nativeint -> nativeint -> nativeint = "%nativeint_unsafe_mod"
let nativeint_unsafe_mod x y = nativeint_unsafe_mod x y
[%%expect_asm X86_64{|
nativeint_unsafe_mod:
  subq  $8, %rsp
  movq  8(%rbx), %rcx
  movq  8(%rax), %rax
  cmpq  $-1, %rcx
  je    .L0
  cqto
  idivq %rcx
  jmp   .L1
.L0:
  xorl  %edx, %edx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  movq  %rdx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

external nativeint_unsigned_compare : nativeint -> nativeint -> bool = "%nativeint_unsigned_compare"
let nativeint_unsigned_compare x y = nativeint_unsigned_compare x y
[%%expect_asm X86_64{|
nativeint_unsigned_compare:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  seta  %al
  cmovae %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

external nativeint_unsigned_greaterequal : nativeint -> nativeint -> bool = "%nativeint_unsigned_greaterequal"
let nativeint_unsigned_greaterequal x y = nativeint_unsigned_greaterequal x y
[%%expect_asm X86_64{|
nativeint_unsigned_greaterequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_unsigned_greaterthan : nativeint -> nativeint -> bool = "%nativeint_unsigned_greaterthan"
let nativeint_unsigned_greaterthan x y = nativeint_unsigned_greaterthan x y
[%%expect_asm X86_64{|
nativeint_unsigned_greaterthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_unsigned_lessequal : nativeint -> nativeint -> bool = "%nativeint_unsigned_lessequal"
let nativeint_unsigned_lessequal x y = nativeint_unsigned_lessequal x y
[%%expect_asm X86_64{|
nativeint_unsigned_lessequal:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_unsigned_lessthan : nativeint -> nativeint -> bool = "%nativeint_unsigned_lessthan"
let nativeint_unsigned_lessthan x y = nativeint_unsigned_lessthan x y
[%%expect_asm X86_64{|
nativeint_unsigned_lessthan:
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_xor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
let nativeint_xor x y = nativeint_xor x y
[%%expect_asm X86_64{|
nativeint_xor:
  subq  $8, %rsp
  movq  %rax, %rdi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rax)
  movq  8(%rbx), %rsi
  movq  8(%rdi), %rbx
  xorq  %rsi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]
