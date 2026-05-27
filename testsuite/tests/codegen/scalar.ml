(* TEST
 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

external float_u_abs : float# -> float# = "%float#_abs"
let float_u_abs = float_u_abs
[%%expect_asm X86_64{|
float_u_abs:
  vandpd caml_absf_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float_u_add : float# -> float# -> float# = "%float#_add"
let float_u_add = float_u_add
[%%expect_asm X86_64{|
float_u_add.(partial):
  vaddsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_compare : float# -> float# -> int = "%float#_compare"
let float_u_compare = float_u_compare
[%%expect_asm X86_64{|
float_u_compare.(partial):
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
let float_u_div = float_u_div
[%%expect_asm X86_64{|
float_u_div.(partial):
  vdivsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_mul : float# -> float# -> float# = "%float#_mul"
let float_u_mul = float_u_mul
[%%expect_asm X86_64{|
float_u_mul.(partial):
  vmulsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_neg : float# -> float# = "%float#_neg"
let float_u_neg = float_u_neg
[%%expect_asm X86_64{|
float_u_neg.(partial):
  vxorpd caml_negf_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float_u_of_float : float -> float# = "%float#_of_float"
let float_u_of_float = float_u_of_float
[%%expect_asm X86_64{|
float_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  ret
|}]

external float_u_of_float32 : float32 -> float# = "%float#_of_float32"
let float_u_of_float32 = float_u_of_float32
[%%expect_asm X86_64{|
float_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvtss2sd %xmm0, %xmm0, %xmm0
  ret
|}]

external float_u_of_float32_u : float32# -> float# = "%float#_of_float32#"
let float_u_of_float32_u = float_u_of_float32_u
[%%expect_asm X86_64{|
float_u_of_float32_u.(partial):
  vcvtss2sd %xmm0, %xmm0, %xmm0
  ret
|}]

external float_u_of_int : int -> float# = "%float#_of_int"
let float_u_of_int = float_u_of_int
[%%expect_asm X86_64{|
float_u_of_int.(partial):
  sarq  $1, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int_u : int# -> float# = "%float#_of_int#"
let float_u_of_int_u = float_u_of_int_u
[%%expect_asm X86_64{|
float_u_of_int_u.(partial):
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int16 : int16 -> float# = "%float#_of_int16"
let float_u_of_int16 = float_u_of_int16
[%%expect_asm X86_64{|
float_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int16_u : int16# -> float# = "%float#_of_int16#"
let float_u_of_int16_u = float_u_of_int16_u
[%%expect_asm X86_64{|
float_u_of_int16_u.(partial):
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int32 : int32 -> float# = "%float#_of_int32"
let float_u_of_int32 = float_u_of_int32
[%%expect_asm X86_64{|
float_u_of_int32.(partial):
  movslq 8(%rax), %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int32_u : int32# -> float# = "%float#_of_int32#"
let float_u_of_int32_u = float_u_of_int32_u
[%%expect_asm X86_64{|
float_u_of_int32_u.(partial):
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int64 : int64 -> float# = "%float#_of_int64"
let float_u_of_int64 = float_u_of_int64
[%%expect_asm X86_64{|
float_u_of_int64.(partial):
  movq  8(%rax), %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int64_u : int64# -> float# = "%float#_of_int64#"
let float_u_of_int64_u = float_u_of_int64_u
[%%expect_asm X86_64{|
float_u_of_int64_u.(partial):
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int8 : int8 -> float# = "%float#_of_int8"
let float_u_of_int8 = float_u_of_int8
[%%expect_asm X86_64{|
float_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_int8_u : int8# -> float# = "%float#_of_int8#"
let float_u_of_int8_u = float_u_of_int8_u
[%%expect_asm X86_64{|
float_u_of_int8_u.(partial):
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_nativeint : nativeint -> float# = "%float#_of_nativeint"
let float_u_of_nativeint = float_u_of_nativeint
[%%expect_asm X86_64{|
float_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_of_nativeint_u : nativeint# -> float# = "%float#_of_nativeint#"
let float_u_of_nativeint_u = float_u_of_nativeint_u
[%%expect_asm X86_64{|
float_u_of_nativeint_u.(partial):
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

external float_u_ordered_and_equal : float# -> float# -> bool = "%float#_ordered_and_equal"
let float_u_ordered_and_equal = float_u_ordered_and_equal
[%%expect_asm X86_64{|
float_u_ordered_and_equal.(partial):
  vcmpsd $0, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_greaterequal : float# -> float# -> bool = "%float#_ordered_and_greaterequal"
let float_u_ordered_and_greaterequal = float_u_ordered_and_greaterequal
[%%expect_asm X86_64{|
float_u_ordered_and_greaterequal.(partial):
  vcmpsd $2, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_greaterthan : float# -> float# -> bool = "%float#_ordered_and_greaterthan"
let float_u_ordered_and_greaterthan = float_u_ordered_and_greaterthan
[%%expect_asm X86_64{|
float_u_ordered_and_greaterthan.(partial):
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_lessequal : float# -> float# -> bool = "%float#_ordered_and_lessequal"
let float_u_ordered_and_lessequal = float_u_ordered_and_lessequal
[%%expect_asm X86_64{|
float_u_ordered_and_lessequal.(partial):
  vcmpsd $2, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_ordered_and_lessthan : float# -> float# -> bool = "%float#_ordered_and_lessthan"
let float_u_ordered_and_lessthan = float_u_ordered_and_lessthan
[%%expect_asm X86_64{|
float_u_ordered_and_lessthan.(partial):
  vcmpsd $1, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_sub : float# -> float# -> float# = "%float#_sub"
let float_u_sub = float_u_sub
[%%expect_asm X86_64{|
float_u_sub.(partial):
  vsubsd %xmm1, %xmm0, %xmm0
  ret
|}]

external float_u_unordered_or_greaterequal : float# -> float# -> bool = "%float#_unordered_or_greaterequal"
let float_u_unordered_or_greaterequal = float_u_unordered_or_greaterequal
[%%expect_asm X86_64{|
float_u_unordered_or_greaterequal.(partial):
  vcmpsd $5, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_greaterthan : float# -> float# -> bool = "%float#_unordered_or_greaterthan"
let float_u_unordered_or_greaterthan = float_u_unordered_or_greaterthan
[%%expect_asm X86_64{|
float_u_unordered_or_greaterthan.(partial):
  vcmpsd $6, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_lessequal : float# -> float# -> bool = "%float#_unordered_or_lessequal"
let float_u_unordered_or_lessequal = float_u_unordered_or_lessequal
[%%expect_asm X86_64{|
float_u_unordered_or_lessequal.(partial):
  vcmpsd $5, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_lessthan : float# -> float# -> bool = "%float#_unordered_or_lessthan"
let float_u_unordered_or_lessthan = float_u_unordered_or_lessthan
[%%expect_asm X86_64{|
float_u_unordered_or_lessthan.(partial):
  vcmpsd $6, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_u_unordered_or_notequal : float# -> float# -> bool = "%float#_unordered_or_notequal"
let float_u_unordered_or_notequal = float_u_unordered_or_notequal
[%%expect_asm X86_64{|
float_u_unordered_or_notequal.(partial):
  vcmpsd $4, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_abs : float32# -> float32# = "%float32#_abs"
let float32_u_abs = float32_u_abs
[%%expect_asm X86_64{|
float32_u_abs.(partial):
  vandps caml_absf32_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float32_u_add : float32# -> float32# -> float32# = "%float32#_add"
let float32_u_add = float32_u_add
[%%expect_asm X86_64{|
float32_u_add.(partial):
  vaddss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_compare : float32# -> float32# -> int = "%float32#_compare"
let float32_u_compare = float32_u_compare
[%%expect_asm X86_64{|
float32_u_compare.(partial):
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
let float32_u_div = float32_u_div
[%%expect_asm X86_64{|
float32_u_div.(partial):
  vdivss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_mul : float32# -> float32# -> float32# = "%float32#_mul"
let float32_u_mul = float32_u_mul
[%%expect_asm X86_64{|
float32_u_mul.(partial):
  vmulss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_neg : float32# -> float32# = "%float32#_neg"
let float32_u_neg = float32_u_neg
[%%expect_asm X86_64{|
float32_u_neg.(partial):
  vxorps caml_negf32_mask(%rip), %xmm0, %xmm0
  ret
|}]

external float32_u_of_float : float -> float32# = "%float32#_of_float"
let float32_u_of_float = float32_u_of_float
[%%expect_asm X86_64{|
float32_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  ret
|}]

external float32_u_of_float_u : float# -> float32# = "%float32#_of_float#"
let float32_u_of_float_u = float32_u_of_float_u
[%%expect_asm X86_64{|
float32_u_of_float_u.(partial):
  vcvtsd2ss %xmm0, %xmm0, %xmm0
  ret
|}]

external float32_u_of_float32 : float32 -> float32# = "%float32#_of_float32"
let float32_u_of_float32 = float32_u_of_float32
[%%expect_asm X86_64{|
float32_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  ret
|}]

external float32_u_of_int : int -> float32# = "%float32#_of_int"
let float32_u_of_int = float32_u_of_int
[%%expect_asm X86_64{|
float32_u_of_int.(partial):
  sarq  $1, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int_u : int# -> float32# = "%float32#_of_int#"
let float32_u_of_int_u = float32_u_of_int_u
[%%expect_asm X86_64{|
float32_u_of_int_u.(partial):
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int16 : int16 -> float32# = "%float32#_of_int16"
let float32_u_of_int16 = float32_u_of_int16
[%%expect_asm X86_64{|
float32_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int16_u : int16# -> float32# = "%float32#_of_int16#"
let float32_u_of_int16_u = float32_u_of_int16_u
[%%expect_asm X86_64{|
float32_u_of_int16_u.(partial):
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int32 : int32 -> float32# = "%float32#_of_int32"
let float32_u_of_int32 = float32_u_of_int32
[%%expect_asm X86_64{|
float32_u_of_int32.(partial):
  movslq 8(%rax), %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int32_u : int32# -> float32# = "%float32#_of_int32#"
let float32_u_of_int32_u = float32_u_of_int32_u
[%%expect_asm X86_64{|
float32_u_of_int32_u.(partial):
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int64 : int64 -> float32# = "%float32#_of_int64"
let float32_u_of_int64 = float32_u_of_int64
[%%expect_asm X86_64{|
float32_u_of_int64.(partial):
  movq  8(%rax), %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int64_u : int64# -> float32# = "%float32#_of_int64#"
let float32_u_of_int64_u = float32_u_of_int64_u
[%%expect_asm X86_64{|
float32_u_of_int64_u.(partial):
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int8 : int8 -> float32# = "%float32#_of_int8"
let float32_u_of_int8 = float32_u_of_int8
[%%expect_asm X86_64{|
float32_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_int8_u : int8# -> float32# = "%float32#_of_int8#"
let float32_u_of_int8_u = float32_u_of_int8_u
[%%expect_asm X86_64{|
float32_u_of_int8_u.(partial):
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_nativeint : nativeint -> float32# = "%float32#_of_nativeint"
let float32_u_of_nativeint = float32_u_of_nativeint
[%%expect_asm X86_64{|
float32_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_of_nativeint_u : nativeint# -> float32# = "%float32#_of_nativeint#"
let float32_u_of_nativeint_u = float32_u_of_nativeint_u
[%%expect_asm X86_64{|
float32_u_of_nativeint_u.(partial):
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

external float32_u_ordered_and_equal : float32# -> float32# -> bool = "%float32#_ordered_and_equal"
let float32_u_ordered_and_equal = float32_u_ordered_and_equal
[%%expect_asm X86_64{|
float32_u_ordered_and_equal.(partial):
  vcmpss $0, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_greaterequal : float32# -> float32# -> bool = "%float32#_ordered_and_greaterequal"
let float32_u_ordered_and_greaterequal = float32_u_ordered_and_greaterequal
[%%expect_asm X86_64{|
float32_u_ordered_and_greaterequal.(partial):
  vcmpss $2, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_greaterthan : float32# -> float32# -> bool = "%float32#_ordered_and_greaterthan"
let float32_u_ordered_and_greaterthan = float32_u_ordered_and_greaterthan
[%%expect_asm X86_64{|
float32_u_ordered_and_greaterthan.(partial):
  vcmpss $1, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_lessequal : float32# -> float32# -> bool = "%float32#_ordered_and_lessequal"
let float32_u_ordered_and_lessequal = float32_u_ordered_and_lessequal
[%%expect_asm X86_64{|
float32_u_ordered_and_lessequal.(partial):
  vcmpss $2, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_ordered_and_lessthan : float32# -> float32# -> bool = "%float32#_ordered_and_lessthan"
let float32_u_ordered_and_lessthan = float32_u_ordered_and_lessthan
[%%expect_asm X86_64{|
float32_u_ordered_and_lessthan.(partial):
  vcmpss $1, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_sub : float32# -> float32# -> float32# = "%float32#_sub"
let float32_u_sub = float32_u_sub
[%%expect_asm X86_64{|
float32_u_sub.(partial):
  vsubss %xmm1, %xmm0, %xmm0
  ret
|}]

external float32_u_unordered_or_greaterequal : float32# -> float32# -> bool = "%float32#_unordered_or_greaterequal"
let float32_u_unordered_or_greaterequal = float32_u_unordered_or_greaterequal
[%%expect_asm X86_64{|
float32_u_unordered_or_greaterequal.(partial):
  vcmpss $5, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_greaterthan : float32# -> float32# -> bool = "%float32#_unordered_or_greaterthan"
let float32_u_unordered_or_greaterthan = float32_u_unordered_or_greaterthan
[%%expect_asm X86_64{|
float32_u_unordered_or_greaterthan.(partial):
  vcmpss $6, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_lessequal : float32# -> float32# -> bool = "%float32#_unordered_or_lessequal"
let float32_u_unordered_or_lessequal = float32_u_unordered_or_lessequal
[%%expect_asm X86_64{|
float32_u_unordered_or_lessequal.(partial):
  vcmpss $5, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_lessthan : float32# -> float32# -> bool = "%float32#_unordered_or_lessthan"
let float32_u_unordered_or_lessthan = float32_u_unordered_or_lessthan
[%%expect_asm X86_64{|
float32_u_unordered_or_lessthan.(partial):
  vcmpss $6, %xmm0, %xmm1, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_u_unordered_or_notequal : float32# -> float32# -> bool = "%float32#_unordered_or_notequal"
let float32_u_unordered_or_notequal = float32_u_unordered_or_notequal
[%%expect_asm X86_64{|
float32_u_unordered_or_notequal.(partial):
  vcmpss $4, %xmm1, %xmm0, %xmm0
  vmovd %xmm0, %eax
  movslq %eax, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float32_abs : float32 -> float32 = "%float32_abs"
let float32_abs = float32_abs
[%%expect_asm X86_64{|
float32_abs.(partial):
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
let float32_add = float32_add
[%%expect_asm X86_64{|
float32_add.(partial):
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
let float32_compare = float32_compare
[%%expect_asm X86_64{|
float32_compare.(partial):
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
let float32_div = float32_div
[%%expect_asm X86_64{|
float32_div.(partial):
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
let float32_mul = float32_mul
[%%expect_asm X86_64{|
float32_mul.(partial):
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
let float32_neg = float32_neg
[%%expect_asm X86_64{|
float32_neg.(partial):
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
let float32_of_float = float32_of_float
[%%expect_asm X86_64{|
float32_of_float.(partial):
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
let float32_of_float_u = float32_of_float_u
[%%expect_asm X86_64{|
float32_of_float_u.(partial):
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
let float32_of_float32_u = float32_of_float32_u
[%%expect_asm X86_64{|
float32_of_float32_u.(partial):
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
let float32_of_int = float32_of_int
[%%expect_asm X86_64{|
float32_of_int.(partial):
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
let float32_of_int_u = float32_of_int_u
[%%expect_asm X86_64{|
float32_of_int_u.(partial):
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
let float32_of_int16 = float32_of_int16
[%%expect_asm X86_64{|
float32_of_int16.(partial):
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
let float32_of_int16_u = float32_of_int16_u
[%%expect_asm X86_64{|
float32_of_int16_u.(partial):
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
let float32_of_int32 = float32_of_int32
[%%expect_asm X86_64{|
float32_of_int32.(partial):
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
let float32_of_int32_u = float32_of_int32_u
[%%expect_asm X86_64{|
float32_of_int32_u.(partial):
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
let float32_of_int64 = float32_of_int64
[%%expect_asm X86_64{|
float32_of_int64.(partial):
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
let float32_of_int64_u = float32_of_int64_u
[%%expect_asm X86_64{|
float32_of_int64_u.(partial):
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
let float32_of_int8 = float32_of_int8
[%%expect_asm X86_64{|
float32_of_int8.(partial):
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
let float32_of_int8_u = float32_of_int8_u
[%%expect_asm X86_64{|
float32_of_int8_u.(partial):
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
let float32_of_nativeint = float32_of_nativeint
[%%expect_asm X86_64{|
float32_of_nativeint.(partial):
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
let float32_of_nativeint_u = float32_of_nativeint_u
[%%expect_asm X86_64{|
float32_of_nativeint_u.(partial):
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
let float32_ordered_and_equal = float32_ordered_and_equal
[%%expect_asm X86_64{|
float32_ordered_and_equal.(partial):
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
let float32_ordered_and_greaterequal = float32_ordered_and_greaterequal
[%%expect_asm X86_64{|
float32_ordered_and_greaterequal.(partial):
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
let float32_ordered_and_greaterthan = float32_ordered_and_greaterthan
[%%expect_asm X86_64{|
float32_ordered_and_greaterthan.(partial):
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
let float32_ordered_and_lessequal = float32_ordered_and_lessequal
[%%expect_asm X86_64{|
float32_ordered_and_lessequal.(partial):
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
let float32_ordered_and_lessthan = float32_ordered_and_lessthan
[%%expect_asm X86_64{|
float32_ordered_and_lessthan.(partial):
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
let float32_sub = float32_sub
[%%expect_asm X86_64{|
float32_sub.(partial):
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
let float32_unordered_or_greaterequal = float32_unordered_or_greaterequal
[%%expect_asm X86_64{|
float32_unordered_or_greaterequal.(partial):
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
let float32_unordered_or_greaterthan = float32_unordered_or_greaterthan
[%%expect_asm X86_64{|
float32_unordered_or_greaterthan.(partial):
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
let float32_unordered_or_lessequal = float32_unordered_or_lessequal
[%%expect_asm X86_64{|
float32_unordered_or_lessequal.(partial):
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
let float32_unordered_or_lessthan = float32_unordered_or_lessthan
[%%expect_asm X86_64{|
float32_unordered_or_lessthan.(partial):
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
let float32_unordered_or_notequal = float32_unordered_or_notequal
[%%expect_asm X86_64{|
float32_unordered_or_notequal.(partial):
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
let float_abs = float_abs
[%%expect_asm X86_64{|
float_abs.(partial):
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
let float_add = float_add
[%%expect_asm X86_64{|
float_add.(partial):
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
let float_compare = float_compare
[%%expect_asm X86_64{|
float_compare.(partial):
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
let float_div = float_div
[%%expect_asm X86_64{|
float_div.(partial):
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
let float_mul = float_mul
[%%expect_asm X86_64{|
float_mul.(partial):
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
let float_neg = float_neg
[%%expect_asm X86_64{|
float_neg.(partial):
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
let float_of_float_u = float_of_float_u
[%%expect_asm X86_64{|
float_of_float_u.(partial):
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
let float_of_float32 = float_of_float32
[%%expect_asm X86_64{|
float_of_float32.(partial):
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
let float_of_float32_u = float_of_float32_u
[%%expect_asm X86_64{|
float_of_float32_u.(partial):
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
let float_of_int = float_of_int
[%%expect_asm X86_64{|
float_of_int.(partial):
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
let float_of_int_u = float_of_int_u
[%%expect_asm X86_64{|
float_of_int_u.(partial):
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
let float_of_int16 = float_of_int16
[%%expect_asm X86_64{|
float_of_int16.(partial):
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
let float_of_int16_u = float_of_int16_u
[%%expect_asm X86_64{|
float_of_int16_u.(partial):
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
let float_of_int32 = float_of_int32
[%%expect_asm X86_64{|
float_of_int32.(partial):
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
let float_of_int32_u = float_of_int32_u
[%%expect_asm X86_64{|
float_of_int32_u.(partial):
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
let float_of_int64 = float_of_int64
[%%expect_asm X86_64{|
float_of_int64.(partial):
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
let float_of_int64_u = float_of_int64_u
[%%expect_asm X86_64{|
float_of_int64_u.(partial):
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
let float_of_int8 = float_of_int8
[%%expect_asm X86_64{|
float_of_int8.(partial):
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
let float_of_int8_u = float_of_int8_u
[%%expect_asm X86_64{|
float_of_int8_u.(partial):
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
let float_of_nativeint = float_of_nativeint
[%%expect_asm X86_64{|
float_of_nativeint.(partial):
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
let float_of_nativeint_u = float_of_nativeint_u
[%%expect_asm X86_64{|
float_of_nativeint_u.(partial):
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
let float_ordered_and_equal = float_ordered_and_equal
[%%expect_asm X86_64{|
float_ordered_and_equal.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $0, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_greaterequal : float -> float -> bool = "%float_ordered_and_greaterequal"
let float_ordered_and_greaterequal = float_ordered_and_greaterequal
[%%expect_asm X86_64{|
float_ordered_and_greaterequal.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $2, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_greaterthan : float -> float -> bool = "%float_ordered_and_greaterthan"
let float_ordered_and_greaterthan = float_ordered_and_greaterthan
[%%expect_asm X86_64{|
float_ordered_and_greaterthan.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $1, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_lessequal : float -> float -> bool = "%float_ordered_and_lessequal"
let float_ordered_and_lessequal = float_ordered_and_lessequal
[%%expect_asm X86_64{|
float_ordered_and_lessequal.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $2, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_ordered_and_lessthan : float -> float -> bool = "%float_ordered_and_lessthan"
let float_ordered_and_lessthan = float_ordered_and_lessthan
[%%expect_asm X86_64{|
float_ordered_and_lessthan.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $1, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_sub : float -> float -> float = "%float_sub"
let float_sub = float_sub
[%%expect_asm X86_64{|
float_sub.(partial):
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
let float_unordered_or_greaterequal = float_unordered_or_greaterequal
[%%expect_asm X86_64{|
float_unordered_or_greaterequal.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $5, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_greaterthan : float -> float -> bool = "%float_unordered_or_greaterthan"
let float_unordered_or_greaterthan = float_unordered_or_greaterthan
[%%expect_asm X86_64{|
float_unordered_or_greaterthan.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $6, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_lessequal : float -> float -> bool = "%float_unordered_or_lessequal"
let float_unordered_or_lessequal = float_unordered_or_lessequal
[%%expect_asm X86_64{|
float_unordered_or_lessequal.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $5, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_lessthan : float -> float -> bool = "%float_unordered_or_lessthan"
let float_unordered_or_lessthan = float_unordered_or_lessthan
[%%expect_asm X86_64{|
float_unordered_or_lessthan.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $6, %xmm1, %xmm0, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external float_unordered_or_notequal : float -> float -> bool = "%float_unordered_or_notequal"
let float_unordered_or_notequal = float_unordered_or_notequal
[%%expect_asm X86_64{|
float_unordered_or_notequal.(partial):
  vmovsd (%rbx), %xmm0
  vmovsd (%rax), %xmm1
  vcmpsd $4, %xmm0, %xmm1, %xmm0
  vmovq %xmm0, %rax
  neg   %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_add : int# -> int# -> int# = "%int#_add"
let int_u_add = int_u_add
[%%expect_asm X86_64{|
int_u_add.(partial):
  addq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_and : int# -> int# -> int# = "%int#_and"
let int_u_and = int_u_and
[%%expect_asm X86_64{|
int_u_and.(partial):
  andq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_asr : int# -> int -> int# = "%int#_asr"
let int_u_asr = int_u_asr
[%%expect_asm X86_64{|
int_u_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int_u_bswap : int# -> int# = "%int#_bswap"
let int_u_bswap = int_u_bswap
[%%expect_asm X86_64{|
int_u_bswap.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  xchg  %ah, %al
  movzwq %ax, %rax
  andl  $65535, %eax
  ret
|}]

external int_u_compare : int# -> int# -> bool = "%int#_compare"
let int_u_compare = int_u_compare
[%%expect_asm X86_64{|
int_u_compare.(partial):
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
let int_u_div = int_u_div
[%%expect_asm X86_64{|
int_u_div.(partial):
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
let int_u_equal = int_u_equal
[%%expect_asm X86_64{|
int_u_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_greaterequal : int# -> int# -> bool = "%int#_greaterequal"
let int_u_greaterequal = int_u_greaterequal
[%%expect_asm X86_64{|
int_u_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_greaterthan : int# -> int# -> bool = "%int#_greaterthan"
let int_u_greaterthan = int_u_greaterthan
[%%expect_asm X86_64{|
int_u_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_lessequal : int# -> int# -> bool = "%int#_lessequal"
let int_u_lessequal = int_u_lessequal
[%%expect_asm X86_64{|
int_u_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_lessthan : int# -> int# -> bool = "%int#_lessthan"
let int_u_lessthan = int_u_lessthan
[%%expect_asm X86_64{|
int_u_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_lsl : int# -> int -> int# = "%int#_lsl"
let int_u_lsl = int_u_lsl
[%%expect_asm X86_64{|
int_u_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_lsr : int# -> int -> int# = "%int#_lsr"
let int_u_lsr = int_u_lsr
[%%expect_asm X86_64{|
int_u_lsr.(partial):
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
let int_u_mod = int_u_mod
[%%expect_asm X86_64{|
int_u_mod.(partial):
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
let int_u_mul = int_u_mul
[%%expect_asm X86_64{|
int_u_mul.(partial):
  imulq %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_neg : int# -> int# = "%int#_neg"
let int_u_neg = int_u_neg
[%%expect_asm X86_64{|
int_u_neg.(partial):
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_notequal : int# -> int# -> bool = "%int#_notequal"
let int_u_notequal = int_u_notequal
[%%expect_asm X86_64{|
int_u_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_of_float : float -> int# = "%int#_of_float"
let int_u_of_float = int_u_of_float
[%%expect_asm X86_64{|
int_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_float_u : float# -> int# = "%int#_of_float#"
let int_u_of_float_u = int_u_of_float_u
[%%expect_asm X86_64{|
int_u_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_float32 : float32 -> int# = "%int#_of_float32"
let int_u_of_float32 = int_u_of_float32
[%%expect_asm X86_64{|
int_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_float32_u : float32# -> int# = "%int#_of_float32#"
let int_u_of_float32_u = int_u_of_float32_u
[%%expect_asm X86_64{|
int_u_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_int : int -> int# = "%int#_of_int"
let int_u_of_int = int_u_of_int
[%%expect_asm X86_64{|
int_u_of_int.(partial):
  sarq  $1, %rax
  ret
|}]

external int_u_of_int16 : int16 -> int# = "%int#_of_int16"
let int_u_of_int16 = int_u_of_int16
[%%expect_asm X86_64{|
int_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int_u_of_int16_u : int16# -> int# = "%int#_of_int16#"
let int_u_of_int16_u = int_u_of_int16_u
[%%expect_asm X86_64{|
int_u_of_int16_u.(partial):
  ret
|}]

external int_u_of_int32 : int32 -> int# = "%int#_of_int32"
let int_u_of_int32 = int_u_of_int32
[%%expect_asm X86_64{|
int_u_of_int32.(partial):
  movslq 8(%rax), %rax
  ret
|}]

external int_u_of_int32_u : int32# -> int# = "%int#_of_int32#"
let int_u_of_int32_u = int_u_of_int32_u
[%%expect_asm X86_64{|
int_u_of_int32_u.(partial):
  ret
|}]

external int_u_of_int64 : int64 -> int# = "%int#_of_int64"
let int_u_of_int64 = int_u_of_int64
[%%expect_asm X86_64{|
int_u_of_int64.(partial):
  movq  8(%rax), %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_int64_u : int64# -> int# = "%int#_of_int64#"
let int_u_of_int64_u = int_u_of_int64_u
[%%expect_asm X86_64{|
int_u_of_int64_u.(partial):
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_int8 : int8 -> int# = "%int#_of_int8"
let int_u_of_int8 = int_u_of_int8
[%%expect_asm X86_64{|
int_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int_u_of_int8_u : int8# -> int# = "%int#_of_int8#"
let int_u_of_int8_u = int_u_of_int8_u
[%%expect_asm X86_64{|
int_u_of_int8_u.(partial):
  ret
|}]

external int_u_of_nativeint : nativeint -> int# = "%int#_of_nativeint"
let int_u_of_nativeint = int_u_of_nativeint
[%%expect_asm X86_64{|
int_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_of_nativeint_u : nativeint# -> int# = "%int#_of_nativeint#"
let int_u_of_nativeint_u = int_u_of_nativeint_u
[%%expect_asm X86_64{|
int_u_of_nativeint_u.(partial):
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_or : int# -> int# -> int# = "%int#_or"
let int_u_or = int_u_or
[%%expect_asm X86_64{|
int_u_or.(partial):
  orq   %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_pred : int# -> int# = "%int#_pred"
let int_u_pred = int_u_pred
[%%expect_asm X86_64{|
int_u_pred.(partial):
  decq  %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_sub : int# -> int# -> int# = "%int#_sub"
let int_u_sub = int_u_sub
[%%expect_asm X86_64{|
int_u_sub.(partial):
  subq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_succ : int# -> int# = "%int#_succ"
let int_u_succ = int_u_succ
[%%expect_asm X86_64{|
int_u_succ.(partial):
  incq  %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_unsafe_div : int# -> int# -> int# = "%int#_unsafe_div"
let int_u_unsafe_div = int_u_unsafe_div
[%%expect_asm X86_64{|
int_u_unsafe_div.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_unsafe_mod : int# -> int# -> int# = "%int#_unsafe_mod"
let int_u_unsafe_mod = int_u_unsafe_mod
[%%expect_asm X86_64{|
int_u_unsafe_mod.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int_u_unsigned_compare : int# -> int# -> bool = "%int#_unsigned_compare"
let int_u_unsigned_compare = int_u_unsigned_compare
[%%expect_asm X86_64{|
int_u_unsigned_compare.(partial):
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
let int_u_unsigned_greaterequal = int_u_unsigned_greaterequal
[%%expect_asm X86_64{|
int_u_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_unsigned_greaterthan : int# -> int# -> bool = "%int#_unsigned_greaterthan"
let int_u_unsigned_greaterthan = int_u_unsigned_greaterthan
[%%expect_asm X86_64{|
int_u_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_unsigned_lessequal : int# -> int# -> bool = "%int#_unsigned_lessequal"
let int_u_unsigned_lessequal = int_u_unsigned_lessequal
[%%expect_asm X86_64{|
int_u_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_unsigned_lessthan : int# -> int# -> bool = "%int#_unsigned_lessthan"
let int_u_unsigned_lessthan = int_u_unsigned_lessthan
[%%expect_asm X86_64{|
int_u_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_u_xor : int# -> int# -> int# = "%int#_xor"
let int_u_xor = int_u_xor
[%%expect_asm X86_64{|
int_u_xor.(partial):
  xorq  %rbx, %rax
  salq  $1, %rax
  sarq  $1, %rax
  ret
|}]

external int16_u_add : int16# -> int16# -> int16# = "%int16#_add"
let int16_u_add = int16_u_add
[%%expect_asm X86_64{|
int16_u_add.(partial):
  addq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_and : int16# -> int16# -> int16# = "%int16#_and"
let int16_u_and = int16_u_and
[%%expect_asm X86_64{|
int16_u_and.(partial):
  andq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_asr : int16# -> int -> int16# = "%int16#_asr"
let int16_u_asr = int16_u_asr
[%%expect_asm X86_64{|
int16_u_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int16_u_bswap : int16# -> int16# = "%int16#_bswap"
let int16_u_bswap = int16_u_bswap
[%%expect_asm X86_64{|
int16_u_bswap.(partial):
  xchg  %ah, %al
  movzwq %ax, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_compare : int16# -> int16# -> bool = "%int16#_compare"
let int16_u_compare = int16_u_compare
[%%expect_asm X86_64{|
int16_u_compare.(partial):
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
let int16_u_div = int16_u_div
[%%expect_asm X86_64{|
int16_u_div.(partial):
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
let int16_u_equal = int16_u_equal
[%%expect_asm X86_64{|
int16_u_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_greaterequal : int16# -> int16# -> bool = "%int16#_greaterequal"
let int16_u_greaterequal = int16_u_greaterequal
[%%expect_asm X86_64{|
int16_u_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_greaterthan : int16# -> int16# -> bool = "%int16#_greaterthan"
let int16_u_greaterthan = int16_u_greaterthan
[%%expect_asm X86_64{|
int16_u_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_lessequal : int16# -> int16# -> bool = "%int16#_lessequal"
let int16_u_lessequal = int16_u_lessequal
[%%expect_asm X86_64{|
int16_u_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_lessthan : int16# -> int16# -> bool = "%int16#_lessthan"
let int16_u_lessthan = int16_u_lessthan
[%%expect_asm X86_64{|
int16_u_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_lsl : int16# -> int -> int16# = "%int16#_lsl"
let int16_u_lsl = int16_u_lsl
[%%expect_asm X86_64{|
int16_u_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_lsr : int16# -> int -> int16# = "%int16#_lsr"
let int16_u_lsr = int16_u_lsr
[%%expect_asm X86_64{|
int16_u_lsr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  andl  $65535, %eax
  shrq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_mod : int16# -> int16# -> int16# = "%int16#_mod"
let int16_u_mod = int16_u_mod
[%%expect_asm X86_64{|
int16_u_mod.(partial):
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
let int16_u_mul = int16_u_mul
[%%expect_asm X86_64{|
int16_u_mul.(partial):
  imulq %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_neg : int16# -> int16# = "%int16#_neg"
let int16_u_neg = int16_u_neg
[%%expect_asm X86_64{|
int16_u_neg.(partial):
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_notequal : int16# -> int16# -> bool = "%int16#_notequal"
let int16_u_notequal = int16_u_notequal
[%%expect_asm X86_64{|
int16_u_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_of_float : float -> int16# = "%int16#_of_float"
let int16_u_of_float = int16_u_of_float
[%%expect_asm X86_64{|
int16_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_float_u : float# -> int16# = "%int16#_of_float#"
let int16_u_of_float_u = int16_u_of_float_u
[%%expect_asm X86_64{|
int16_u_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_float32 : float32 -> int16# = "%int16#_of_float32"
let int16_u_of_float32 = int16_u_of_float32
[%%expect_asm X86_64{|
int16_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_float32_u : float32# -> int16# = "%int16#_of_float32#"
let int16_u_of_float32_u = int16_u_of_float32_u
[%%expect_asm X86_64{|
int16_u_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int : int -> int16# = "%int16#_of_int"
let int16_u_of_int = int16_u_of_int
[%%expect_asm X86_64{|
int16_u_of_int.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int_u : int# -> int16# = "%int16#_of_int#"
let int16_u_of_int_u = int16_u_of_int_u
[%%expect_asm X86_64{|
int16_u_of_int_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int16 : int16 -> int16# = "%int16#_of_int16"
let int16_u_of_int16 = int16_u_of_int16
[%%expect_asm X86_64{|
int16_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int32 : int32 -> int16# = "%int16#_of_int32"
let int16_u_of_int32 = int16_u_of_int32
[%%expect_asm X86_64{|
int16_u_of_int32.(partial):
  movslq 8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int32_u : int32# -> int16# = "%int16#_of_int32#"
let int16_u_of_int32_u = int16_u_of_int32_u
[%%expect_asm X86_64{|
int16_u_of_int32_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int64 : int64 -> int16# = "%int16#_of_int64"
let int16_u_of_int64 = int16_u_of_int64
[%%expect_asm X86_64{|
int16_u_of_int64.(partial):
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int64_u : int64# -> int16# = "%int16#_of_int64#"
let int16_u_of_int64_u = int16_u_of_int64_u
[%%expect_asm X86_64{|
int16_u_of_int64_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int8 : int8 -> int16# = "%int16#_of_int8"
let int16_u_of_int8 = int16_u_of_int8
[%%expect_asm X86_64{|
int16_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int16_u_of_int8_u : int8# -> int16# = "%int16#_of_int8#"
let int16_u_of_int8_u = int16_u_of_int8_u
[%%expect_asm X86_64{|
int16_u_of_int8_u.(partial):
  ret
|}]

external int16_u_of_nativeint : nativeint -> int16# = "%int16#_of_nativeint"
let int16_u_of_nativeint = int16_u_of_nativeint
[%%expect_asm X86_64{|
int16_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_nativeint_u : nativeint# -> int16# = "%int16#_of_nativeint#"
let int16_u_of_nativeint_u = int16_u_of_nativeint_u
[%%expect_asm X86_64{|
int16_u_of_nativeint_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_or : int16# -> int16# -> int16# = "%int16#_or"
let int16_u_or = int16_u_or
[%%expect_asm X86_64{|
int16_u_or.(partial):
  orq   %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_pred : int16# -> int16# = "%int16#_pred"
let int16_u_pred = int16_u_pred
[%%expect_asm X86_64{|
int16_u_pred.(partial):
  decq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_sub : int16# -> int16# -> int16# = "%int16#_sub"
let int16_u_sub = int16_u_sub
[%%expect_asm X86_64{|
int16_u_sub.(partial):
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_succ : int16# -> int16# = "%int16#_succ"
let int16_u_succ = int16_u_succ
[%%expect_asm X86_64{|
int16_u_succ.(partial):
  incq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_unsafe_div : int16# -> int16# -> int16# = "%int16#_unsafe_div"
let int16_u_unsafe_div = int16_u_unsafe_div
[%%expect_asm X86_64{|
int16_u_unsafe_div.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_unsafe_mod : int16# -> int16# -> int16# = "%int16#_unsafe_mod"
let int16_u_unsafe_mod = int16_u_unsafe_mod
[%%expect_asm X86_64{|
int16_u_unsafe_mod.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_unsigned_compare : int16# -> int16# -> bool = "%int16#_unsigned_compare"
let int16_u_unsigned_compare = int16_u_unsigned_compare
[%%expect_asm X86_64{|
int16_u_unsigned_compare.(partial):
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
let int16_u_unsigned_greaterequal = int16_u_unsigned_greaterequal
[%%expect_asm X86_64{|
int16_u_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_unsigned_greaterthan : int16# -> int16# -> bool = "%int16#_unsigned_greaterthan"
let int16_u_unsigned_greaterthan = int16_u_unsigned_greaterthan
[%%expect_asm X86_64{|
int16_u_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_unsigned_lessequal : int16# -> int16# -> bool = "%int16#_unsigned_lessequal"
let int16_u_unsigned_lessequal = int16_u_unsigned_lessequal
[%%expect_asm X86_64{|
int16_u_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_unsigned_lessthan : int16# -> int16# -> bool = "%int16#_unsigned_lessthan"
let int16_u_unsigned_lessthan = int16_u_unsigned_lessthan
[%%expect_asm X86_64{|
int16_u_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_u_xor : int16# -> int16# -> int16# = "%int16#_xor"
let int16_u_xor = int16_u_xor
[%%expect_asm X86_64{|
int16_u_xor.(partial):
  xorq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_add : int16 -> int16 -> int16 = "%int16_add"
let int16_add = int16_add
[%%expect_asm X86_64{|
int16_add.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  addq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_and : int16 -> int16 -> int16 = "%int16_and"
let int16_and = int16_and
[%%expect_asm X86_64{|
int16_and.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  andq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_asr : int16 -> int -> int16 = "%int16_asr"
let int16_asr = int16_asr
[%%expect_asm X86_64{|
int16_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  $47, %rax
  sarq  %cl, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_bswap : int16 -> int16 = "%int16_bswap"
let int16_bswap = int16_bswap
[%%expect_asm X86_64{|
int16_bswap.(partial):
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
let int16_compare = int16_compare
[%%expect_asm X86_64{|
int16_compare.(partial):
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
let int16_div = int16_div
[%%expect_asm X86_64{|
int16_div.(partial):
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
let int16_equal = int16_equal
[%%expect_asm X86_64{|
int16_equal.(partial):
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
let int16_greaterequal = int16_greaterequal
[%%expect_asm X86_64{|
int16_greaterequal.(partial):
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
let int16_greaterthan = int16_greaterthan
[%%expect_asm X86_64{|
int16_greaterthan.(partial):
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
let int16_lessequal = int16_lessequal
[%%expect_asm X86_64{|
int16_lessequal.(partial):
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
let int16_lessthan = int16_lessthan
[%%expect_asm X86_64{|
int16_lessthan.(partial):
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
let int16_lsl = int16_lsl
[%%expect_asm X86_64{|
int16_lsl.(partial):
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
let int16_lsr = int16_lsr
[%%expect_asm X86_64{|
int16_lsr.(partial):
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
let int16_mod = int16_mod
[%%expect_asm X86_64{|
int16_mod.(partial):
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
let int16_mul = int16_mul
[%%expect_asm X86_64{|
int16_mul.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  imulq %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_neg : int16 -> int16 = "%int16_neg"
let int16_neg = int16_neg
[%%expect_asm X86_64{|
int16_neg.(partial):
  sarq  $1, %rax
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  salq  $48, %rbx
  sarq  $48, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

external int16_notequal : int16 -> int16 -> bool = "%int16_notequal"
let int16_notequal = int16_notequal
[%%expect_asm X86_64{|
int16_notequal.(partial):
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
let int16_of_float = int16_of_float
[%%expect_asm X86_64{|
int16_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float_u : float# -> int16 = "%int16_of_float#"
let int16_of_float_u = int16_of_float_u
[%%expect_asm X86_64{|
int16_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float32 : float32 -> int16 = "%int16_of_float32"
let int16_of_float32 = int16_of_float32
[%%expect_asm X86_64{|
int16_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_float32_u : float32# -> int16 = "%int16_of_float32#"
let int16_of_float32_u = int16_of_float32_u
[%%expect_asm X86_64{|
int16_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int : int -> int16 = "%int16_of_int"
let int16_of_int = int16_of_int
[%%expect_asm X86_64{|
int16_of_int.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int_u : int# -> int16 = "%int16_of_int#"
let int16_of_int_u = int16_of_int_u
[%%expect_asm X86_64{|
int16_of_int_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int16_u : int16# -> int16 = "%int16_of_int16#"
let int16_of_int16_u = int16_of_int16_u
[%%expect_asm X86_64{|
int16_of_int16_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int32 : int32 -> int16 = "%int16_of_int32"
let int16_of_int32 = int16_of_int32
[%%expect_asm X86_64{|
int16_of_int32.(partial):
  movslq 8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int32_u : int32# -> int16 = "%int16_of_int32#"
let int16_of_int32_u = int16_of_int32_u
[%%expect_asm X86_64{|
int16_of_int32_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int64 : int64 -> int16 = "%int16_of_int64"
let int16_of_int64 = int16_of_int64
[%%expect_asm X86_64{|
int16_of_int64.(partial):
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int64_u : int64# -> int16 = "%int16_of_int64#"
let int16_of_int64_u = int16_of_int64_u
[%%expect_asm X86_64{|
int16_of_int64_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int8 : int8 -> int16 = "%int16_of_int8"
let int16_of_int8 = int16_of_int8
[%%expect_asm X86_64{|
int16_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_int8_u : int8# -> int16 = "%int16_of_int8#"
let int16_of_int8_u = int16_of_int8_u
[%%expect_asm X86_64{|
int16_of_int8_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_nativeint : nativeint -> int16 = "%int16_of_nativeint"
let int16_of_nativeint = int16_of_nativeint
[%%expect_asm X86_64{|
int16_of_nativeint.(partial):
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_of_nativeint_u : nativeint# -> int16 = "%int16_of_nativeint#"
let int16_of_nativeint_u = int16_of_nativeint_u
[%%expect_asm X86_64{|
int16_of_nativeint_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_or : int16 -> int16 -> int16 = "%int16_or"
let int16_or = int16_or
[%%expect_asm X86_64{|
int16_or.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  orq   %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_pred : int16 -> int16 = "%int16_pred"
let int16_pred = int16_pred
[%%expect_asm X86_64{|
int16_pred.(partial):
  sarq  $1, %rax
  decq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_sub : int16 -> int16 -> int16 = "%int16_sub"
let int16_sub = int16_sub
[%%expect_asm X86_64{|
int16_sub.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_succ : int16 -> int16 = "%int16_succ"
let int16_succ = int16_succ
[%%expect_asm X86_64{|
int16_succ.(partial):
  sarq  $1, %rax
  incq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int16_unsafe_div : int16 -> int16 -> int16 = "%int16_unsafe_div"
let int16_unsafe_div = int16_unsafe_div
[%%expect_asm X86_64{|
int16_unsafe_div.(partial):
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
let int16_unsafe_mod = int16_unsafe_mod
[%%expect_asm X86_64{|
int16_unsafe_mod.(partial):
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
let int16_unsigned_compare = int16_unsigned_compare
[%%expect_asm X86_64{|
int16_unsigned_compare.(partial):
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
let int16_unsigned_greaterequal = int16_unsigned_greaterequal
[%%expect_asm X86_64{|
int16_unsigned_greaterequal.(partial):
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
let int16_unsigned_greaterthan = int16_unsigned_greaterthan
[%%expect_asm X86_64{|
int16_unsigned_greaterthan.(partial):
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
let int16_unsigned_lessequal = int16_unsigned_lessequal
[%%expect_asm X86_64{|
int16_unsigned_lessequal.(partial):
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
let int16_unsigned_lessthan = int16_unsigned_lessthan
[%%expect_asm X86_64{|
int16_unsigned_lessthan.(partial):
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
let int16_xor = int16_xor
[%%expect_asm X86_64{|
int16_xor.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  xorq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_add : int32# -> int32# -> int32# = "%int32#_add"
let int32_u_add = int32_u_add
[%%expect_asm X86_64{|
int32_u_add.(partial):
  addq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_and : int32# -> int32# -> int32# = "%int32#_and"
let int32_u_and = int32_u_and
[%%expect_asm X86_64{|
int32_u_and.(partial):
  andq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_asr : int32# -> int -> int32# = "%int32#_asr"
let int32_u_asr = int32_u_asr
[%%expect_asm X86_64{|
int32_u_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int32_u_bswap : int32# -> int32# = "%int32#_bswap"
let int32_u_bswap = int32_u_bswap
[%%expect_asm X86_64{|
int32_u_bswap.(partial):
  bswap %eax
  movslq %eax, %rax
  ret
|}]

external int32_u_compare : int32# -> int32# -> bool = "%int32#_compare"
let int32_u_compare = int32_u_compare
[%%expect_asm X86_64{|
int32_u_compare.(partial):
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
let int32_u_div = int32_u_div
[%%expect_asm X86_64{|
int32_u_div.(partial):
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
let int32_u_equal = int32_u_equal
[%%expect_asm X86_64{|
int32_u_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_greaterequal : int32# -> int32# -> bool = "%int32#_greaterequal"
let int32_u_greaterequal = int32_u_greaterequal
[%%expect_asm X86_64{|
int32_u_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_greaterthan : int32# -> int32# -> bool = "%int32#_greaterthan"
let int32_u_greaterthan = int32_u_greaterthan
[%%expect_asm X86_64{|
int32_u_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_lessequal : int32# -> int32# -> bool = "%int32#_lessequal"
let int32_u_lessequal = int32_u_lessequal
[%%expect_asm X86_64{|
int32_u_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_lessthan : int32# -> int32# -> bool = "%int32#_lessthan"
let int32_u_lessthan = int32_u_lessthan
[%%expect_asm X86_64{|
int32_u_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_lsl : int32# -> int -> int32# = "%int32#_lsl"
let int32_u_lsl = int32_u_lsl
[%%expect_asm X86_64{|
int32_u_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_lsr : int32# -> int -> int32# = "%int32#_lsr"
let int32_u_lsr = int32_u_lsr
[%%expect_asm X86_64{|
int32_u_lsr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  movl  %eax, %eax
  shrq  %cl, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_mod : int32# -> int32# -> int32# = "%int32#_mod"
let int32_u_mod = int32_u_mod
[%%expect_asm X86_64{|
int32_u_mod.(partial):
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
let int32_u_mul = int32_u_mul
[%%expect_asm X86_64{|
int32_u_mul.(partial):
  imulq %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_neg : int32# -> int32# = "%int32#_neg"
let int32_u_neg = int32_u_neg
[%%expect_asm X86_64{|
int32_u_neg.(partial):
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  movslq %ebx, %rax
  ret
|}]

external int32_u_notequal : int32# -> int32# -> bool = "%int32#_notequal"
let int32_u_notequal = int32_u_notequal
[%%expect_asm X86_64{|
int32_u_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_of_float : float -> int32# = "%int32#_of_float"
let int32_u_of_float = int32_u_of_float
[%%expect_asm X86_64{|
int32_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_float_u : float# -> int32# = "%int32#_of_float#"
let int32_u_of_float_u = int32_u_of_float_u
[%%expect_asm X86_64{|
int32_u_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_float32 : float32 -> int32# = "%int32#_of_float32"
let int32_u_of_float32 = int32_u_of_float32
[%%expect_asm X86_64{|
int32_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_float32_u : float32# -> int32# = "%int32#_of_float32#"
let int32_u_of_float32_u = int32_u_of_float32_u
[%%expect_asm X86_64{|
int32_u_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int : int -> int32# = "%int32#_of_int"
let int32_u_of_int = int32_u_of_int
[%%expect_asm X86_64{|
int32_u_of_int.(partial):
  salq  $31, %rax
  sarq  $32, %rax
  ret
|}]

external int32_u_of_int_u : int# -> int32# = "%int32#_of_int#"
let int32_u_of_int_u = int32_u_of_int_u
[%%expect_asm X86_64{|
int32_u_of_int_u.(partial):
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int16 : int16 -> int32# = "%int32#_of_int16"
let int32_u_of_int16 = int32_u_of_int16
[%%expect_asm X86_64{|
int32_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int32_u_of_int16_u : int16# -> int32# = "%int32#_of_int16#"
let int32_u_of_int16_u = int32_u_of_int16_u
[%%expect_asm X86_64{|
int32_u_of_int16_u.(partial):
  ret
|}]

external int32_u_of_int32 : int32 -> int32# = "%int32#_of_int32"
let int32_u_of_int32 = int32_u_of_int32
[%%expect_asm X86_64{|
int32_u_of_int32.(partial):
  movslq 8(%rax), %rax
  ret
|}]

external int32_u_of_int64 : int64 -> int32# = "%int32#_of_int64"
let int32_u_of_int64 = int32_u_of_int64
[%%expect_asm X86_64{|
int32_u_of_int64.(partial):
  movq  8(%rax), %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int64_u : int64# -> int32# = "%int32#_of_int64#"
let int32_u_of_int64_u = int32_u_of_int64_u
[%%expect_asm X86_64{|
int32_u_of_int64_u.(partial):
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int8 : int8 -> int32# = "%int32#_of_int8"
let int32_u_of_int8 = int32_u_of_int8
[%%expect_asm X86_64{|
int32_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int32_u_of_int8_u : int8# -> int32# = "%int32#_of_int8#"
let int32_u_of_int8_u = int32_u_of_int8_u
[%%expect_asm X86_64{|
int32_u_of_int8_u.(partial):
  ret
|}]

external int32_u_of_nativeint : nativeint -> int32# = "%int32#_of_nativeint"
let int32_u_of_nativeint = int32_u_of_nativeint
[%%expect_asm X86_64{|
int32_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_of_nativeint_u : nativeint# -> int32# = "%int32#_of_nativeint#"
let int32_u_of_nativeint_u = int32_u_of_nativeint_u
[%%expect_asm X86_64{|
int32_u_of_nativeint_u.(partial):
  movslq %eax, %rax
  ret
|}]

external int32_u_or : int32# -> int32# -> int32# = "%int32#_or"
let int32_u_or = int32_u_or
[%%expect_asm X86_64{|
int32_u_or.(partial):
  orq   %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_pred : int32# -> int32# = "%int32#_pred"
let int32_u_pred = int32_u_pred
[%%expect_asm X86_64{|
int32_u_pred.(partial):
  decq  %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_sub : int32# -> int32# -> int32# = "%int32#_sub"
let int32_u_sub = int32_u_sub
[%%expect_asm X86_64{|
int32_u_sub.(partial):
  subq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_succ : int32# -> int32# = "%int32#_succ"
let int32_u_succ = int32_u_succ
[%%expect_asm X86_64{|
int32_u_succ.(partial):
  incq  %rax
  movslq %eax, %rax
  ret
|}]

external int32_u_unsafe_div : int32# -> int32# -> int32# = "%int32#_unsafe_div"
let int32_u_unsafe_div = int32_u_unsafe_div
[%%expect_asm X86_64{|
int32_u_unsafe_div.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movslq %eax, %rax
  ret
|}]

external int32_u_unsafe_mod : int32# -> int32# -> int32# = "%int32#_unsafe_mod"
let int32_u_unsafe_mod = int32_u_unsafe_mod
[%%expect_asm X86_64{|
int32_u_unsafe_mod.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movslq %edx, %rax
  ret
|}]

external int32_u_unsigned_compare : int32# -> int32# -> bool = "%int32#_unsigned_compare"
let int32_u_unsigned_compare = int32_u_unsigned_compare
[%%expect_asm X86_64{|
int32_u_unsigned_compare.(partial):
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
let int32_u_unsigned_greaterequal = int32_u_unsigned_greaterequal
[%%expect_asm X86_64{|
int32_u_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_unsigned_greaterthan : int32# -> int32# -> bool = "%int32#_unsigned_greaterthan"
let int32_u_unsigned_greaterthan = int32_u_unsigned_greaterthan
[%%expect_asm X86_64{|
int32_u_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_unsigned_lessequal : int32# -> int32# -> bool = "%int32#_unsigned_lessequal"
let int32_u_unsigned_lessequal = int32_u_unsigned_lessequal
[%%expect_asm X86_64{|
int32_u_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_unsigned_lessthan : int32# -> int32# -> bool = "%int32#_unsigned_lessthan"
let int32_u_unsigned_lessthan = int32_u_unsigned_lessthan
[%%expect_asm X86_64{|
int32_u_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_u_xor : int32# -> int32# -> int32# = "%int32#_xor"
let int32_u_xor = int32_u_xor
[%%expect_asm X86_64{|
int32_u_xor.(partial):
  xorq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

external int32_add : int32 -> int32 -> int32 = "%int32_add"
let int32_add = int32_add
[%%expect_asm X86_64{|
int32_add.(partial):
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
let int32_and = int32_and
[%%expect_asm X86_64{|
int32_and.(partial):
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
let int32_asr = int32_asr
[%%expect_asm X86_64{|
int32_asr.(partial):
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
let int32_bswap = int32_bswap
[%%expect_asm X86_64{|
int32_bswap.(partial):
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
let int32_compare = int32_compare
[%%expect_asm X86_64{|
int32_compare.(partial):
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
let int32_div = int32_div
[%%expect_asm X86_64{|
int32_div.(partial):
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
let int32_equal = int32_equal
[%%expect_asm X86_64{|
int32_equal.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_greaterequal : int32 -> int32 -> bool = "%int32_greaterequal"
let int32_greaterequal = int32_greaterequal
[%%expect_asm X86_64{|
int32_greaterequal.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_greaterthan : int32 -> int32 -> bool = "%int32_greaterthan"
let int32_greaterthan = int32_greaterthan
[%%expect_asm X86_64{|
int32_greaterthan.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_lessequal : int32 -> int32 -> bool = "%int32_lessequal"
let int32_lessequal = int32_lessequal
[%%expect_asm X86_64{|
int32_lessequal.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_lessthan : int32 -> int32 -> bool = "%int32_lessthan"
let int32_lessthan = int32_lessthan
[%%expect_asm X86_64{|
int32_lessthan.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_lsl : int32 -> int -> int32 = "%int32_lsl"
let int32_lsl = int32_lsl
[%%expect_asm X86_64{|
int32_lsl.(partial):
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
let int32_lsr = int32_lsr
[%%expect_asm X86_64{|
int32_lsr.(partial):
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
let int32_mod = int32_mod
[%%expect_asm X86_64{|
int32_mod.(partial):
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
let int32_mul = int32_mul
[%%expect_asm X86_64{|
int32_mul.(partial):
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
let int32_neg = int32_neg
[%%expect_asm X86_64{|
int32_neg.(partial):
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
let int32_notequal = int32_notequal
[%%expect_asm X86_64{|
int32_notequal.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_of_float : float -> int32 = "%int32_of_float"
let int32_of_float = int32_of_float
[%%expect_asm X86_64{|
int32_of_float.(partial):
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
let int32_of_float_u = int32_of_float_u
[%%expect_asm X86_64{|
int32_of_float_u.(partial):
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
let int32_of_float32 = int32_of_float32
[%%expect_asm X86_64{|
int32_of_float32.(partial):
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
let int32_of_float32_u = int32_of_float32_u
[%%expect_asm X86_64{|
int32_of_float32_u.(partial):
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
let int32_of_int = int32_of_int
[%%expect_asm X86_64{|
int32_of_int.(partial):
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
let int32_of_int_u = int32_of_int_u
[%%expect_asm X86_64{|
int32_of_int_u.(partial):
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
let int32_of_int16 = int32_of_int16
[%%expect_asm X86_64{|
int32_of_int16.(partial):
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
let int32_of_int16_u = int32_of_int16_u
[%%expect_asm X86_64{|
int32_of_int16_u.(partial):
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
let int32_of_int32_u = int32_of_int32_u
[%%expect_asm X86_64{|
int32_of_int32_u.(partial):
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
let int32_of_int64 = int32_of_int64
[%%expect_asm X86_64{|
int32_of_int64.(partial):
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
let int32_of_int64_u = int32_of_int64_u
[%%expect_asm X86_64{|
int32_of_int64_u.(partial):
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
let int32_of_int8 = int32_of_int8
[%%expect_asm X86_64{|
int32_of_int8.(partial):
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
let int32_of_int8_u = int32_of_int8_u
[%%expect_asm X86_64{|
int32_of_int8_u.(partial):
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
let int32_of_nativeint = int32_of_nativeint
[%%expect_asm X86_64{|
int32_of_nativeint.(partial):
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
let int32_of_nativeint_u = int32_of_nativeint_u
[%%expect_asm X86_64{|
int32_of_nativeint_u.(partial):
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
let int32_or = int32_or
[%%expect_asm X86_64{|
int32_or.(partial):
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
let int32_pred = int32_pred
[%%expect_asm X86_64{|
int32_pred.(partial):
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
let int32_sub = int32_sub
[%%expect_asm X86_64{|
int32_sub.(partial):
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
let int32_succ = int32_succ
[%%expect_asm X86_64{|
int32_succ.(partial):
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
let int32_unsafe_div = int32_unsafe_div
[%%expect_asm X86_64{|
int32_unsafe_div.(partial):
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
let int32_unsafe_mod = int32_unsafe_mod
[%%expect_asm X86_64{|
int32_unsafe_mod.(partial):
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
let int32_unsigned_compare = int32_unsigned_compare
[%%expect_asm X86_64{|
int32_unsigned_compare.(partial):
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
let int32_unsigned_greaterequal = int32_unsigned_greaterequal
[%%expect_asm X86_64{|
int32_unsigned_greaterequal.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_unsigned_greaterthan : int32 -> int32 -> bool = "%int32_unsigned_greaterthan"
let int32_unsigned_greaterthan = int32_unsigned_greaterthan
[%%expect_asm X86_64{|
int32_unsigned_greaterthan.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_unsigned_lessequal : int32 -> int32 -> bool = "%int32_unsigned_lessequal"
let int32_unsigned_lessequal = int32_unsigned_lessequal
[%%expect_asm X86_64{|
int32_unsigned_lessequal.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_unsigned_lessthan : int32 -> int32 -> bool = "%int32_unsigned_lessthan"
let int32_unsigned_lessthan = int32_unsigned_lessthan
[%%expect_asm X86_64{|
int32_unsigned_lessthan.(partial):
  movslq 8(%rbx), %rbx
  movslq 8(%rax), %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int32_xor : int32 -> int32 -> int32 = "%int32_xor"
let int32_xor = int32_xor
[%%expect_asm X86_64{|
int32_xor.(partial):
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
let int64_u_add = int64_u_add
[%%expect_asm X86_64{|
int64_u_add.(partial):
  addq  %rbx, %rax
  ret
|}]

external int64_u_and : int64# -> int64# -> int64# = "%int64#_and"
let int64_u_and = int64_u_and
[%%expect_asm X86_64{|
int64_u_and.(partial):
  andq  %rbx, %rax
  ret
|}]

external int64_u_asr : int64# -> int -> int64# = "%int64#_asr"
let int64_u_asr = int64_u_asr
[%%expect_asm X86_64{|
int64_u_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int64_u_bswap : int64# -> int64# = "%int64#_bswap"
let int64_u_bswap = int64_u_bswap
[%%expect_asm X86_64{|
int64_u_bswap.(partial):
  bswap %rax
  ret
|}]

external int64_u_compare : int64# -> int64# -> bool = "%int64#_compare"
let int64_u_compare = int64_u_compare
[%%expect_asm X86_64{|
int64_u_compare.(partial):
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
let int64_u_div = int64_u_div
[%%expect_asm X86_64{|
int64_u_div.(partial):
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
let int64_u_equal = int64_u_equal
[%%expect_asm X86_64{|
int64_u_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_greaterequal : int64# -> int64# -> bool = "%int64#_greaterequal"
let int64_u_greaterequal = int64_u_greaterequal
[%%expect_asm X86_64{|
int64_u_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_greaterthan : int64# -> int64# -> bool = "%int64#_greaterthan"
let int64_u_greaterthan = int64_u_greaterthan
[%%expect_asm X86_64{|
int64_u_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_lessequal : int64# -> int64# -> bool = "%int64#_lessequal"
let int64_u_lessequal = int64_u_lessequal
[%%expect_asm X86_64{|
int64_u_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_lessthan : int64# -> int64# -> bool = "%int64#_lessthan"
let int64_u_lessthan = int64_u_lessthan
[%%expect_asm X86_64{|
int64_u_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_lsl : int64# -> int -> int64# = "%int64#_lsl"
let int64_u_lsl = int64_u_lsl
[%%expect_asm X86_64{|
int64_u_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  ret
|}]

external int64_u_lsr : int64# -> int -> int64# = "%int64#_lsr"
let int64_u_lsr = int64_u_lsr
[%%expect_asm X86_64{|
int64_u_lsr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  ret
|}]

external int64_u_mod : int64# -> int64# -> int64# = "%int64#_mod"
let int64_u_mod = int64_u_mod
[%%expect_asm X86_64{|
int64_u_mod.(partial):
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
let int64_u_mul = int64_u_mul
[%%expect_asm X86_64{|
int64_u_mul.(partial):
  imulq %rbx, %rax
  ret
|}]

external int64_u_neg : int64# -> int64# = "%int64#_neg"
let int64_u_neg = int64_u_neg
[%%expect_asm X86_64{|
int64_u_neg.(partial):
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

external int64_u_notequal : int64# -> int64# -> bool = "%int64#_notequal"
let int64_u_notequal = int64_u_notequal
[%%expect_asm X86_64{|
int64_u_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_of_float : float -> int64# = "%int64#_of_float"
let int64_u_of_float = int64_u_of_float
[%%expect_asm X86_64{|
int64_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  ret
|}]

external int64_u_of_float_u : float# -> int64# = "%int64#_of_float#"
let int64_u_of_float_u = int64_u_of_float_u
[%%expect_asm X86_64{|
int64_u_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  ret
|}]

external int64_u_of_float32 : float32 -> int64# = "%int64#_of_float32"
let int64_u_of_float32 = int64_u_of_float32
[%%expect_asm X86_64{|
int64_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  ret
|}]

external int64_u_of_float32_u : float32# -> int64# = "%int64#_of_float32#"
let int64_u_of_float32_u = int64_u_of_float32_u
[%%expect_asm X86_64{|
int64_u_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  ret
|}]

external int64_u_of_int : int -> int64# = "%int64#_of_int"
let int64_u_of_int = int64_u_of_int
[%%expect_asm X86_64{|
int64_u_of_int.(partial):
  sarq  $1, %rax
  ret
|}]

external int64_u_of_int_u : int# -> int64# = "%int64#_of_int#"
let int64_u_of_int_u = int64_u_of_int_u
[%%expect_asm X86_64{|
int64_u_of_int_u.(partial):
  ret
|}]

external int64_u_of_int16 : int16 -> int64# = "%int64#_of_int16"
let int64_u_of_int16 = int64_u_of_int16
[%%expect_asm X86_64{|
int64_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external int64_u_of_int16_u : int16# -> int64# = "%int64#_of_int16#"
let int64_u_of_int16_u = int64_u_of_int16_u
[%%expect_asm X86_64{|
int64_u_of_int16_u.(partial):
  ret
|}]

external int64_u_of_int32 : int32 -> int64# = "%int64#_of_int32"
let int64_u_of_int32 = int64_u_of_int32
[%%expect_asm X86_64{|
int64_u_of_int32.(partial):
  movslq 8(%rax), %rax
  ret
|}]

external int64_u_of_int32_u : int32# -> int64# = "%int64#_of_int32#"
let int64_u_of_int32_u = int64_u_of_int32_u
[%%expect_asm X86_64{|
int64_u_of_int32_u.(partial):
  ret
|}]

external int64_u_of_int64 : int64 -> int64# = "%int64#_of_int64"
let int64_u_of_int64 = int64_u_of_int64
[%%expect_asm X86_64{|
int64_u_of_int64.(partial):
  movq  8(%rax), %rax
  ret
|}]

external int64_u_of_int8 : int8 -> int64# = "%int64#_of_int8"
let int64_u_of_int8 = int64_u_of_int8
[%%expect_asm X86_64{|
int64_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int64_u_of_int8_u : int8# -> int64# = "%int64#_of_int8#"
let int64_u_of_int8_u = int64_u_of_int8_u
[%%expect_asm X86_64{|
int64_u_of_int8_u.(partial):
  ret
|}]

external int64_u_of_nativeint : nativeint -> int64# = "%int64#_of_nativeint"
let int64_u_of_nativeint = int64_u_of_nativeint
[%%expect_asm X86_64{|
int64_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  ret
|}]

external int64_u_of_nativeint_u : nativeint# -> int64# = "%int64#_of_nativeint#"
let int64_u_of_nativeint_u = int64_u_of_nativeint_u
[%%expect_asm X86_64{|
int64_u_of_nativeint_u.(partial):
  ret
|}]

external int64_u_or : int64# -> int64# -> int64# = "%int64#_or"
let int64_u_or = int64_u_or
[%%expect_asm X86_64{|
int64_u_or.(partial):
  orq   %rbx, %rax
  ret
|}]

external int64_u_pred : int64# -> int64# = "%int64#_pred"
let int64_u_pred = int64_u_pred
[%%expect_asm X86_64{|
int64_u_pred.(partial):
  decq  %rax
  ret
|}]

external int64_u_sub : int64# -> int64# -> int64# = "%int64#_sub"
let int64_u_sub = int64_u_sub
[%%expect_asm X86_64{|
int64_u_sub.(partial):
  subq  %rbx, %rax
  ret
|}]

external int64_u_succ : int64# -> int64# = "%int64#_succ"
let int64_u_succ = int64_u_succ
[%%expect_asm X86_64{|
int64_u_succ.(partial):
  incq  %rax
  ret
|}]

external int64_u_unsafe_div : int64# -> int64# -> int64# = "%int64#_unsafe_div"
let int64_u_unsafe_div = int64_u_unsafe_div
[%%expect_asm X86_64{|
int64_u_unsafe_div.(partial):
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
let int64_u_unsafe_mod = int64_u_unsafe_mod
[%%expect_asm X86_64{|
int64_u_unsafe_mod.(partial):
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
let int64_u_unsigned_compare = int64_u_unsigned_compare
[%%expect_asm X86_64{|
int64_u_unsigned_compare.(partial):
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
let int64_u_unsigned_greaterequal = int64_u_unsigned_greaterequal
[%%expect_asm X86_64{|
int64_u_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_unsigned_greaterthan : int64# -> int64# -> bool = "%int64#_unsigned_greaterthan"
let int64_u_unsigned_greaterthan = int64_u_unsigned_greaterthan
[%%expect_asm X86_64{|
int64_u_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_unsigned_lessequal : int64# -> int64# -> bool = "%int64#_unsigned_lessequal"
let int64_u_unsigned_lessequal = int64_u_unsigned_lessequal
[%%expect_asm X86_64{|
int64_u_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_unsigned_lessthan : int64# -> int64# -> bool = "%int64#_unsigned_lessthan"
let int64_u_unsigned_lessthan = int64_u_unsigned_lessthan
[%%expect_asm X86_64{|
int64_u_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_u_xor : int64# -> int64# -> int64# = "%int64#_xor"
let int64_u_xor = int64_u_xor
[%%expect_asm X86_64{|
int64_u_xor.(partial):
  xorq  %rbx, %rax
  ret
|}]

external int64_add : int64 -> int64 -> int64 = "%int64_add"
let int64_add = int64_add
[%%expect_asm X86_64{|
int64_add.(partial):
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
let int64_and = int64_and
[%%expect_asm X86_64{|
int64_and.(partial):
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
let int64_asr = int64_asr
[%%expect_asm X86_64{|
int64_asr.(partial):
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
let int64_bswap = int64_bswap
[%%expect_asm X86_64{|
int64_bswap.(partial):
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
let int64_compare = int64_compare
[%%expect_asm X86_64{|
int64_compare.(partial):
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
let int64_div = int64_div
[%%expect_asm X86_64{|
int64_div.(partial):
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
let int64_equal = int64_equal
[%%expect_asm X86_64{|
int64_equal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_greaterequal : int64 -> int64 -> bool = "%int64_greaterequal"
let int64_greaterequal = int64_greaterequal
[%%expect_asm X86_64{|
int64_greaterequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_greaterthan : int64 -> int64 -> bool = "%int64_greaterthan"
let int64_greaterthan = int64_greaterthan
[%%expect_asm X86_64{|
int64_greaterthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_lessequal : int64 -> int64 -> bool = "%int64_lessequal"
let int64_lessequal = int64_lessequal
[%%expect_asm X86_64{|
int64_lessequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_lessthan : int64 -> int64 -> bool = "%int64_lessthan"
let int64_lessthan = int64_lessthan
[%%expect_asm X86_64{|
int64_lessthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_lsl : int64 -> int -> int64 = "%int64_lsl"
let int64_lsl = int64_lsl
[%%expect_asm X86_64{|
int64_lsl.(partial):
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
let int64_lsr = int64_lsr
[%%expect_asm X86_64{|
int64_lsr.(partial):
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
let int64_mod = int64_mod
[%%expect_asm X86_64{|
int64_mod.(partial):
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
let int64_mul = int64_mul
[%%expect_asm X86_64{|
int64_mul.(partial):
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
let int64_neg = int64_neg
[%%expect_asm X86_64{|
int64_neg.(partial):
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
let int64_notequal = int64_notequal
[%%expect_asm X86_64{|
int64_notequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_of_float : float -> int64 = "%int64_of_float"
let int64_of_float = int64_of_float
[%%expect_asm X86_64{|
int64_of_float.(partial):
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
let int64_of_float_u = int64_of_float_u
[%%expect_asm X86_64{|
int64_of_float_u.(partial):
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
let int64_of_float32 = int64_of_float32
[%%expect_asm X86_64{|
int64_of_float32.(partial):
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
let int64_of_float32_u = int64_of_float32_u
[%%expect_asm X86_64{|
int64_of_float32_u.(partial):
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
let int64_of_int = int64_of_int
[%%expect_asm X86_64{|
int64_of_int.(partial):
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
let int64_of_int_u = int64_of_int_u
[%%expect_asm X86_64{|
int64_of_int_u.(partial):
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
let int64_of_int16 = int64_of_int16
[%%expect_asm X86_64{|
int64_of_int16.(partial):
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
let int64_of_int16_u = int64_of_int16_u
[%%expect_asm X86_64{|
int64_of_int16_u.(partial):
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
let int64_of_int32 = int64_of_int32
[%%expect_asm X86_64{|
int64_of_int32.(partial):
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
let int64_of_int32_u = int64_of_int32_u
[%%expect_asm X86_64{|
int64_of_int32_u.(partial):
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
let int64_of_int64_u = int64_of_int64_u
[%%expect_asm X86_64{|
int64_of_int64_u.(partial):
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
let int64_of_int8 = int64_of_int8
[%%expect_asm X86_64{|
int64_of_int8.(partial):
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
let int64_of_int8_u = int64_of_int8_u
[%%expect_asm X86_64{|
int64_of_int8_u.(partial):
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
let int64_of_nativeint = int64_of_nativeint
[%%expect_asm X86_64{|
int64_of_nativeint.(partial):
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
let int64_of_nativeint_u = int64_of_nativeint_u
[%%expect_asm X86_64{|
int64_of_nativeint_u.(partial):
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
let int64_or = int64_or
[%%expect_asm X86_64{|
int64_or.(partial):
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
let int64_pred = int64_pred
[%%expect_asm X86_64{|
int64_pred.(partial):
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
let int64_sub = int64_sub
[%%expect_asm X86_64{|
int64_sub.(partial):
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
let int64_succ = int64_succ
[%%expect_asm X86_64{|
int64_succ.(partial):
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
let int64_unsafe_div = int64_unsafe_div
[%%expect_asm X86_64{|
int64_unsafe_div.(partial):
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
let int64_unsafe_mod = int64_unsafe_mod
[%%expect_asm X86_64{|
int64_unsafe_mod.(partial):
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
let int64_unsigned_compare = int64_unsigned_compare
[%%expect_asm X86_64{|
int64_unsigned_compare.(partial):
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
let int64_unsigned_greaterequal = int64_unsigned_greaterequal
[%%expect_asm X86_64{|
int64_unsigned_greaterequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_unsigned_greaterthan : int64 -> int64 -> bool = "%int64_unsigned_greaterthan"
let int64_unsigned_greaterthan = int64_unsigned_greaterthan
[%%expect_asm X86_64{|
int64_unsigned_greaterthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_unsigned_lessequal : int64 -> int64 -> bool = "%int64_unsigned_lessequal"
let int64_unsigned_lessequal = int64_unsigned_lessequal
[%%expect_asm X86_64{|
int64_unsigned_lessequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_unsigned_lessthan : int64 -> int64 -> bool = "%int64_unsigned_lessthan"
let int64_unsigned_lessthan = int64_unsigned_lessthan
[%%expect_asm X86_64{|
int64_unsigned_lessthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int64_xor : int64 -> int64 -> int64 = "%int64_xor"
let int64_xor = int64_xor
[%%expect_asm X86_64{|
int64_xor.(partial):
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
let int8_u_add = int8_u_add
[%%expect_asm X86_64{|
int8_u_add.(partial):
  addq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_and : int8# -> int8# -> int8# = "%int8#_and"
let int8_u_and = int8_u_and
[%%expect_asm X86_64{|
int8_u_and.(partial):
  andq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_asr : int8# -> int -> int8# = "%int8#_asr"
let int8_u_asr = int8_u_asr
[%%expect_asm X86_64{|
int8_u_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external int8_u_bswap : int8# -> int8# = "%int8#_bswap"
let int8_u_bswap = int8_u_bswap
[%%expect_asm X86_64{|
int8_u_bswap.(partial):
  ret
|}]

external int8_u_compare : int8# -> int8# -> bool = "%int8#_compare"
let int8_u_compare = int8_u_compare
[%%expect_asm X86_64{|
int8_u_compare.(partial):
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
let int8_u_div = int8_u_div
[%%expect_asm X86_64{|
int8_u_div.(partial):
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
let int8_u_equal = int8_u_equal
[%%expect_asm X86_64{|
int8_u_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_greaterequal : int8# -> int8# -> bool = "%int8#_greaterequal"
let int8_u_greaterequal = int8_u_greaterequal
[%%expect_asm X86_64{|
int8_u_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_greaterthan : int8# -> int8# -> bool = "%int8#_greaterthan"
let int8_u_greaterthan = int8_u_greaterthan
[%%expect_asm X86_64{|
int8_u_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_lessequal : int8# -> int8# -> bool = "%int8#_lessequal"
let int8_u_lessequal = int8_u_lessequal
[%%expect_asm X86_64{|
int8_u_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_lessthan : int8# -> int8# -> bool = "%int8#_lessthan"
let int8_u_lessthan = int8_u_lessthan
[%%expect_asm X86_64{|
int8_u_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_lsl : int8# -> int -> int8# = "%int8#_lsl"
let int8_u_lsl = int8_u_lsl
[%%expect_asm X86_64{|
int8_u_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_lsr : int8# -> int -> int8# = "%int8#_lsr"
let int8_u_lsr = int8_u_lsr
[%%expect_asm X86_64{|
int8_u_lsr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  andl  $255, %eax
  shrq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_mod : int8# -> int8# -> int8# = "%int8#_mod"
let int8_u_mod = int8_u_mod
[%%expect_asm X86_64{|
int8_u_mod.(partial):
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
let int8_u_mul = int8_u_mul
[%%expect_asm X86_64{|
int8_u_mul.(partial):
  imulq %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_neg : int8# -> int8# = "%int8#_neg"
let int8_u_neg = int8_u_neg
[%%expect_asm X86_64{|
int8_u_neg.(partial):
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_notequal : int8# -> int8# -> bool = "%int8#_notequal"
let int8_u_notequal = int8_u_notequal
[%%expect_asm X86_64{|
int8_u_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_of_float : float -> int8# = "%int8#_of_float"
let int8_u_of_float = int8_u_of_float
[%%expect_asm X86_64{|
int8_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_float_u : float# -> int8# = "%int8#_of_float#"
let int8_u_of_float_u = int8_u_of_float_u
[%%expect_asm X86_64{|
int8_u_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_float32 : float32 -> int8# = "%int8#_of_float32"
let int8_u_of_float32 = int8_u_of_float32
[%%expect_asm X86_64{|
int8_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_float32_u : float32# -> int8# = "%int8#_of_float32#"
let int8_u_of_float32_u = int8_u_of_float32_u
[%%expect_asm X86_64{|
int8_u_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int : int -> int8# = "%int8#_of_int"
let int8_u_of_int = int8_u_of_int
[%%expect_asm X86_64{|
int8_u_of_int.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int_u : int# -> int8# = "%int8#_of_int#"
let int8_u_of_int_u = int8_u_of_int_u
[%%expect_asm X86_64{|
int8_u_of_int_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int16 : int16 -> int8# = "%int8#_of_int16"
let int8_u_of_int16 = int8_u_of_int16
[%%expect_asm X86_64{|
int8_u_of_int16.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int16_u : int16# -> int8# = "%int8#_of_int16#"
let int8_u_of_int16_u = int8_u_of_int16_u
[%%expect_asm X86_64{|
int8_u_of_int16_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int32 : int32 -> int8# = "%int8#_of_int32"
let int8_u_of_int32 = int8_u_of_int32
[%%expect_asm X86_64{|
int8_u_of_int32.(partial):
  movslq 8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int32_u : int32# -> int8# = "%int8#_of_int32#"
let int8_u_of_int32_u = int8_u_of_int32_u
[%%expect_asm X86_64{|
int8_u_of_int32_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int64 : int64 -> int8# = "%int8#_of_int64"
let int8_u_of_int64 = int8_u_of_int64
[%%expect_asm X86_64{|
int8_u_of_int64.(partial):
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int64_u : int64# -> int8# = "%int8#_of_int64#"
let int8_u_of_int64_u = int8_u_of_int64_u
[%%expect_asm X86_64{|
int8_u_of_int64_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_int8 : int8 -> int8# = "%int8#_of_int8"
let int8_u_of_int8 = int8_u_of_int8
[%%expect_asm X86_64{|
int8_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_nativeint : nativeint -> int8# = "%int8#_of_nativeint"
let int8_u_of_nativeint = int8_u_of_nativeint
[%%expect_asm X86_64{|
int8_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_of_nativeint_u : nativeint# -> int8# = "%int8#_of_nativeint#"
let int8_u_of_nativeint_u = int8_u_of_nativeint_u
[%%expect_asm X86_64{|
int8_u_of_nativeint_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_or : int8# -> int8# -> int8# = "%int8#_or"
let int8_u_or = int8_u_or
[%%expect_asm X86_64{|
int8_u_or.(partial):
  orq   %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_pred : int8# -> int8# = "%int8#_pred"
let int8_u_pred = int8_u_pred
[%%expect_asm X86_64{|
int8_u_pred.(partial):
  decq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_sub : int8# -> int8# -> int8# = "%int8#_sub"
let int8_u_sub = int8_u_sub
[%%expect_asm X86_64{|
int8_u_sub.(partial):
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_succ : int8# -> int8# = "%int8#_succ"
let int8_u_succ = int8_u_succ
[%%expect_asm X86_64{|
int8_u_succ.(partial):
  incq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_unsafe_div : int8# -> int8# -> int8# = "%int8#_unsafe_div"
let int8_u_unsafe_div = int8_u_unsafe_div
[%%expect_asm X86_64{|
int8_u_unsafe_div.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_unsafe_mod : int8# -> int8# -> int8# = "%int8#_unsafe_mod"
let int8_u_unsafe_mod = int8_u_unsafe_mod
[%%expect_asm X86_64{|
int8_u_unsafe_mod.(partial):
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_u_unsigned_compare : int8# -> int8# -> bool = "%int8#_unsigned_compare"
let int8_u_unsigned_compare = int8_u_unsigned_compare
[%%expect_asm X86_64{|
int8_u_unsigned_compare.(partial):
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
let int8_u_unsigned_greaterequal = int8_u_unsigned_greaterequal
[%%expect_asm X86_64{|
int8_u_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_unsigned_greaterthan : int8# -> int8# -> bool = "%int8#_unsigned_greaterthan"
let int8_u_unsigned_greaterthan = int8_u_unsigned_greaterthan
[%%expect_asm X86_64{|
int8_u_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_unsigned_lessequal : int8# -> int8# -> bool = "%int8#_unsigned_lessequal"
let int8_u_unsigned_lessequal = int8_u_unsigned_lessequal
[%%expect_asm X86_64{|
int8_u_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_unsigned_lessthan : int8# -> int8# -> bool = "%int8#_unsigned_lessthan"
let int8_u_unsigned_lessthan = int8_u_unsigned_lessthan
[%%expect_asm X86_64{|
int8_u_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_u_xor : int8# -> int8# -> int8# = "%int8#_xor"
let int8_u_xor = int8_u_xor
[%%expect_asm X86_64{|
int8_u_xor.(partial):
  xorq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

external int8_add : int8 -> int8 -> int8 = "%int8_add"
let int8_add = int8_add
[%%expect_asm X86_64{|
int8_add.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  addq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_and : int8 -> int8 -> int8 = "%int8_and"
let int8_and = int8_and
[%%expect_asm X86_64{|
int8_and.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  andq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_asr : int8 -> int -> int8 = "%int8_asr"
let int8_asr = int8_asr
[%%expect_asm X86_64{|
int8_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  $55, %rax
  sarq  %cl, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_bswap : int8 -> int8 = "%int8_bswap"
let int8_bswap = int8_bswap
[%%expect_asm X86_64{|
int8_bswap.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_compare : int8 -> int8 -> bool = "%int8_compare"
let int8_compare = int8_compare
[%%expect_asm X86_64{|
int8_compare.(partial):
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
let int8_div = int8_div
[%%expect_asm X86_64{|
int8_div.(partial):
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
let int8_equal = int8_equal
[%%expect_asm X86_64{|
int8_equal.(partial):
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
let int8_greaterequal = int8_greaterequal
[%%expect_asm X86_64{|
int8_greaterequal.(partial):
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
let int8_greaterthan = int8_greaterthan
[%%expect_asm X86_64{|
int8_greaterthan.(partial):
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
let int8_lessequal = int8_lessequal
[%%expect_asm X86_64{|
int8_lessequal.(partial):
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
let int8_lessthan = int8_lessthan
[%%expect_asm X86_64{|
int8_lessthan.(partial):
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
let int8_lsl = int8_lsl
[%%expect_asm X86_64{|
int8_lsl.(partial):
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
let int8_lsr = int8_lsr
[%%expect_asm X86_64{|
int8_lsr.(partial):
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
let int8_mod = int8_mod
[%%expect_asm X86_64{|
int8_mod.(partial):
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
let int8_mul = int8_mul
[%%expect_asm X86_64{|
int8_mul.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  imulq %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_neg : int8 -> int8 = "%int8_neg"
let int8_neg = int8_neg
[%%expect_asm X86_64{|
int8_neg.(partial):
  sarq  $1, %rax
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  salq  $56, %rbx
  sarq  $56, %rbx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

external int8_notequal : int8 -> int8 -> bool = "%int8_notequal"
let int8_notequal = int8_notequal
[%%expect_asm X86_64{|
int8_notequal.(partial):
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
let int8_of_float = int8_of_float
[%%expect_asm X86_64{|
int8_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float_u : float# -> int8 = "%int8_of_float#"
let int8_of_float_u = int8_of_float_u
[%%expect_asm X86_64{|
int8_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float32 : float32 -> int8 = "%int8_of_float32"
let int8_of_float32 = int8_of_float32
[%%expect_asm X86_64{|
int8_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_float32_u : float32# -> int8 = "%int8_of_float32#"
let int8_of_float32_u = int8_of_float32_u
[%%expect_asm X86_64{|
int8_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int : int -> int8 = "%int8_of_int"
let int8_of_int = int8_of_int
[%%expect_asm X86_64{|
int8_of_int.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int_u : int# -> int8 = "%int8_of_int#"
let int8_of_int_u = int8_of_int_u
[%%expect_asm X86_64{|
int8_of_int_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int16 : int16 -> int8 = "%int8_of_int16"
let int8_of_int16 = int8_of_int16
[%%expect_asm X86_64{|
int8_of_int16.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int16_u : int16# -> int8 = "%int8_of_int16#"
let int8_of_int16_u = int8_of_int16_u
[%%expect_asm X86_64{|
int8_of_int16_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int32 : int32 -> int8 = "%int8_of_int32"
let int8_of_int32 = int8_of_int32
[%%expect_asm X86_64{|
int8_of_int32.(partial):
  movslq 8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int32_u : int32# -> int8 = "%int8_of_int32#"
let int8_of_int32_u = int8_of_int32_u
[%%expect_asm X86_64{|
int8_of_int32_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int64 : int64 -> int8 = "%int8_of_int64"
let int8_of_int64 = int8_of_int64
[%%expect_asm X86_64{|
int8_of_int64.(partial):
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int64_u : int64# -> int8 = "%int8_of_int64#"
let int8_of_int64_u = int8_of_int64_u
[%%expect_asm X86_64{|
int8_of_int64_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_int8_u : int8# -> int8 = "%int8_of_int8#"
let int8_of_int8_u = int8_of_int8_u
[%%expect_asm X86_64{|
int8_of_int8_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_nativeint : nativeint -> int8 = "%int8_of_nativeint"
let int8_of_nativeint = int8_of_nativeint
[%%expect_asm X86_64{|
int8_of_nativeint.(partial):
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_of_nativeint_u : nativeint# -> int8 = "%int8_of_nativeint#"
let int8_of_nativeint_u = int8_of_nativeint_u
[%%expect_asm X86_64{|
int8_of_nativeint_u.(partial):
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_or : int8 -> int8 -> int8 = "%int8_or"
let int8_or = int8_or
[%%expect_asm X86_64{|
int8_or.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  orq   %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_pred : int8 -> int8 = "%int8_pred"
let int8_pred = int8_pred
[%%expect_asm X86_64{|
int8_pred.(partial):
  sarq  $1, %rax
  decq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_sub : int8 -> int8 -> int8 = "%int8_sub"
let int8_sub = int8_sub
[%%expect_asm X86_64{|
int8_sub.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_succ : int8 -> int8 = "%int8_succ"
let int8_succ = int8_succ
[%%expect_asm X86_64{|
int8_succ.(partial):
  sarq  $1, %rax
  incq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int8_unsafe_div : int8 -> int8 -> int8 = "%int8_unsafe_div"
let int8_unsafe_div = int8_unsafe_div
[%%expect_asm X86_64{|
int8_unsafe_div.(partial):
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
let int8_unsafe_mod = int8_unsafe_mod
[%%expect_asm X86_64{|
int8_unsafe_mod.(partial):
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
let int8_unsigned_compare = int8_unsigned_compare
[%%expect_asm X86_64{|
int8_unsigned_compare.(partial):
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
let int8_unsigned_greaterequal = int8_unsigned_greaterequal
[%%expect_asm X86_64{|
int8_unsigned_greaterequal.(partial):
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
let int8_unsigned_greaterthan = int8_unsigned_greaterthan
[%%expect_asm X86_64{|
int8_unsigned_greaterthan.(partial):
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
let int8_unsigned_lessequal = int8_unsigned_lessequal
[%%expect_asm X86_64{|
int8_unsigned_lessequal.(partial):
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
let int8_unsigned_lessthan = int8_unsigned_lessthan
[%%expect_asm X86_64{|
int8_unsigned_lessthan.(partial):
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
let int8_xor = int8_xor
[%%expect_asm X86_64{|
int8_xor.(partial):
  sarq  $1, %rbx
  sarq  $1, %rax
  xorq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_add : int -> int -> int = "%int_add"
let int_add = int_add
[%%expect_asm X86_64{|
int_add.(partial):
  leaq  -1(%rax,%rbx), %rax
  ret
|}]

external int_and : int -> int -> int = "%int_and"
let int_and = int_and
[%%expect_asm X86_64{|
int_and.(partial):
  andq  %rbx, %rax
  ret
|}]

external int_asr : int -> int -> int = "%int_asr"
let int_asr = int_asr
[%%expect_asm X86_64{|
int_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  orq   $1, %rax
  ret
|}]

external int_bswap : int -> int = "%int_bswap"
let int_bswap = int_bswap
[%%expect_asm X86_64{|
int_bswap.(partial):
  sarq  $1, %rax
  xchg  %ah, %al
  movzwq %ax, %rax
  andl  $65535, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_compare : int -> int -> bool = "%int_compare"
let int_compare = int_compare
[%%expect_asm X86_64{|
int_compare.(partial):
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
let int_div = int_div
[%%expect_asm X86_64{|
int_div.(partial):
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
let int_equal = int_equal
[%%expect_asm X86_64{|
int_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_greaterequal : int -> int -> bool = "%int_greaterequal"
let int_greaterequal = int_greaterequal
[%%expect_asm X86_64{|
int_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_greaterthan : int -> int -> bool = "%int_greaterthan"
let int_greaterthan = int_greaterthan
[%%expect_asm X86_64{|
int_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_lessequal : int -> int -> bool = "%int_lessequal"
let int_lessequal = int_lessequal
[%%expect_asm X86_64{|
int_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_lessthan : int -> int -> bool = "%int_lessthan"
let int_lessthan = int_lessthan
[%%expect_asm X86_64{|
int_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_lsl : int -> int -> int = "%int_lsl"
let int_lsl = int_lsl
[%%expect_asm X86_64{|
int_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  decq  %rax
  salq  %cl, %rax
  incq  %rax
  ret
|}]

external int_lsr : int -> int -> int = "%int_lsr"
let int_lsr = int_lsr
[%%expect_asm X86_64{|
int_lsr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  orq   $1, %rax
  ret
|}]

external int_mod : int -> int -> int = "%int_mod"
let int_mod = int_mod
[%%expect_asm X86_64{|
int_mod.(partial):
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
let int_mul = int_mul
[%%expect_asm X86_64{|
int_mul.(partial):
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  incq  %rax
  ret
|}]

external int_neg : int -> int = "%int_neg"
let int_neg = int_neg
[%%expect_asm X86_64{|
int_neg.(partial):
  movq  %rax, %rbx
  movl  $2, %eax
  subq  %rbx, %rax
  ret
|}]

external int_notequal : int -> int -> bool = "%int_notequal"
let int_notequal = int_notequal
[%%expect_asm X86_64{|
int_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float : float -> int = "%int_of_float"
let int_of_float = int_of_float
[%%expect_asm X86_64{|
int_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float_u : float# -> int = "%int_of_float#"
let int_of_float_u = int_of_float_u
[%%expect_asm X86_64{|
int_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float32 : float32 -> int = "%int_of_float32"
let int_of_float32 = int_of_float32
[%%expect_asm X86_64{|
int_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_float32_u : float32# -> int = "%int_of_float32#"
let int_of_float32_u = int_of_float32_u
[%%expect_asm X86_64{|
int_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int_u : int# -> int = "%int_of_int#"
let int_of_int_u = int_of_int_u
[%%expect_asm X86_64{|
int_of_int_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int16 : int16 -> int = "%int_of_int16"
let int_of_int16 = int_of_int16
[%%expect_asm X86_64{|
int_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int16_u : int16# -> int = "%int_of_int16#"
let int_of_int16_u = int_of_int16_u
[%%expect_asm X86_64{|
int_of_int16_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int32 : int32 -> int = "%int_of_int32"
let int_of_int32 = int_of_int32
[%%expect_asm X86_64{|
int_of_int32.(partial):
  movslq 8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int32_u : int32# -> int = "%int_of_int32#"
let int_of_int32_u = int_of_int32_u
[%%expect_asm X86_64{|
int_of_int32_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int64 : int64 -> int = "%int_of_int64"
let int_of_int64 = int_of_int64
[%%expect_asm X86_64{|
int_of_int64.(partial):
  movq  8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int64_u : int64# -> int = "%int_of_int64#"
let int_of_int64_u = int_of_int64_u
[%%expect_asm X86_64{|
int_of_int64_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int8 : int8 -> int = "%int_of_int8"
let int_of_int8 = int_of_int8
[%%expect_asm X86_64{|
int_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_int8_u : int8# -> int = "%int_of_int8#"
let int_of_int8_u = int_of_int8_u
[%%expect_asm X86_64{|
int_of_int8_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_nativeint : nativeint -> int = "%int_of_nativeint"
let int_of_nativeint = int_of_nativeint
[%%expect_asm X86_64{|
int_of_nativeint.(partial):
  movq  8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_of_nativeint_u : nativeint# -> int = "%int_of_nativeint#"
let int_of_nativeint_u = int_of_nativeint_u
[%%expect_asm X86_64{|
int_of_nativeint_u.(partial):
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_or : int -> int -> int = "%int_or"
let int_or = int_or
[%%expect_asm X86_64{|
int_or.(partial):
  orq   %rbx, %rax
  ret
|}]

external int_pred : int -> int = "%int_pred"
let int_pred = int_pred
[%%expect_asm X86_64{|
int_pred.(partial):
  addq  $-2, %rax
  ret
|}]

external int_sub : int -> int -> int = "%int_sub"
let int_sub = int_sub
[%%expect_asm X86_64{|
int_sub.(partial):
  subq  %rbx, %rax
  incq  %rax
  ret
|}]

external int_succ : int -> int = "%int_succ"
let int_succ = int_succ
[%%expect_asm X86_64{|
int_succ.(partial):
  addq  $2, %rax
  ret
|}]

external int_unsafe_div : int -> int -> int = "%int_unsafe_div"
let int_unsafe_div = int_unsafe_div
[%%expect_asm X86_64{|
int_unsafe_div.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsafe_mod : int -> int -> int = "%int_unsafe_mod"
let int_unsafe_mod = int_unsafe_mod
[%%expect_asm X86_64{|
int_unsafe_mod.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  $1, %rax
  cqto
  idivq %rcx
  leaq  1(%rdx,%rdx), %rax
  ret
|}]

external int_unsigned_compare : int -> int -> bool = "%int_unsigned_compare"
let int_unsigned_compare = int_unsigned_compare
[%%expect_asm X86_64{|
int_unsigned_compare.(partial):
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
let int_unsigned_greaterequal = int_unsigned_greaterequal
[%%expect_asm X86_64{|
int_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsigned_greaterthan : int -> int -> bool = "%int_unsigned_greaterthan"
let int_unsigned_greaterthan = int_unsigned_greaterthan
[%%expect_asm X86_64{|
int_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsigned_lessequal : int -> int -> bool = "%int_unsigned_lessequal"
let int_unsigned_lessequal = int_unsigned_lessequal
[%%expect_asm X86_64{|
int_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_unsigned_lessthan : int -> int -> bool = "%int_unsigned_lessthan"
let int_unsigned_lessthan = int_unsigned_lessthan
[%%expect_asm X86_64{|
int_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external int_xor : int -> int -> int = "%int_xor"
let int_xor = int_xor
[%%expect_asm X86_64{|
int_xor.(partial):
  xorq  %rbx, %rax
  orq   $1, %rax
  ret
|}]

external nativeint_u_add : nativeint# -> nativeint# -> nativeint# = "%nativeint#_add"
let nativeint_u_add = nativeint_u_add
[%%expect_asm X86_64{|
nativeint_u_add.(partial):
  addq  %rbx, %rax
  ret
|}]

external nativeint_u_and : nativeint# -> nativeint# -> nativeint# = "%nativeint#_and"
let nativeint_u_and = nativeint_u_and
[%%expect_asm X86_64{|
nativeint_u_and.(partial):
  andq  %rbx, %rax
  ret
|}]

external nativeint_u_asr : nativeint# -> int -> nativeint# = "%nativeint#_asr"
let nativeint_u_asr = nativeint_u_asr
[%%expect_asm X86_64{|
nativeint_u_asr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

external nativeint_u_bswap : nativeint# -> nativeint# = "%nativeint#_bswap"
let nativeint_u_bswap = nativeint_u_bswap
[%%expect_asm X86_64{|
nativeint_u_bswap.(partial):
  bswap %rax
  ret
|}]

external nativeint_u_compare : nativeint# -> nativeint# -> bool = "%nativeint#_compare"
let nativeint_u_compare = nativeint_u_compare
[%%expect_asm X86_64{|
nativeint_u_compare.(partial):
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
let nativeint_u_div = nativeint_u_div
[%%expect_asm X86_64{|
nativeint_u_div.(partial):
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
let nativeint_u_equal = nativeint_u_equal
[%%expect_asm X86_64{|
nativeint_u_equal.(partial):
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_greaterequal : nativeint# -> nativeint# -> bool = "%nativeint#_greaterequal"
let nativeint_u_greaterequal = nativeint_u_greaterequal
[%%expect_asm X86_64{|
nativeint_u_greaterequal.(partial):
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_greaterthan : nativeint# -> nativeint# -> bool = "%nativeint#_greaterthan"
let nativeint_u_greaterthan = nativeint_u_greaterthan
[%%expect_asm X86_64{|
nativeint_u_greaterthan.(partial):
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_lessequal : nativeint# -> nativeint# -> bool = "%nativeint#_lessequal"
let nativeint_u_lessequal = nativeint_u_lessequal
[%%expect_asm X86_64{|
nativeint_u_lessequal.(partial):
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_lessthan : nativeint# -> nativeint# -> bool = "%nativeint#_lessthan"
let nativeint_u_lessthan = nativeint_u_lessthan
[%%expect_asm X86_64{|
nativeint_u_lessthan.(partial):
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_lsl : nativeint# -> int -> nativeint# = "%nativeint#_lsl"
let nativeint_u_lsl = nativeint_u_lsl
[%%expect_asm X86_64{|
nativeint_u_lsl.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  ret
|}]

external nativeint_u_lsr : nativeint# -> int -> nativeint# = "%nativeint#_lsr"
let nativeint_u_lsr = nativeint_u_lsr
[%%expect_asm X86_64{|
nativeint_u_lsr.(partial):
  movq  %rbx, %rcx
  sarq  $1, %rcx
  shrq  %cl, %rax
  ret
|}]

external nativeint_u_mod : nativeint# -> nativeint# -> nativeint# = "%nativeint#_mod"
let nativeint_u_mod = nativeint_u_mod
[%%expect_asm X86_64{|
nativeint_u_mod.(partial):
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
let nativeint_u_mul = nativeint_u_mul
[%%expect_asm X86_64{|
nativeint_u_mul.(partial):
  imulq %rbx, %rax
  ret
|}]

external nativeint_u_neg : nativeint# -> nativeint# = "%nativeint#_neg"
let nativeint_u_neg = nativeint_u_neg
[%%expect_asm X86_64{|
nativeint_u_neg.(partial):
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

external nativeint_u_notequal : nativeint# -> nativeint# -> bool = "%nativeint#_notequal"
let nativeint_u_notequal = nativeint_u_notequal
[%%expect_asm X86_64{|
nativeint_u_notequal.(partial):
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_of_float : float -> nativeint# = "%nativeint#_of_float"
let nativeint_u_of_float = nativeint_u_of_float
[%%expect_asm X86_64{|
nativeint_u_of_float.(partial):
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_float_u : float# -> nativeint# = "%nativeint#_of_float#"
let nativeint_u_of_float_u = nativeint_u_of_float_u
[%%expect_asm X86_64{|
nativeint_u_of_float_u.(partial):
  vcvttsd2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_float32 : float32 -> nativeint# = "%nativeint#_of_float32"
let nativeint_u_of_float32 = nativeint_u_of_float32
[%%expect_asm X86_64{|
nativeint_u_of_float32.(partial):
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_float32_u : float32# -> nativeint# = "%nativeint#_of_float32#"
let nativeint_u_of_float32_u = nativeint_u_of_float32_u
[%%expect_asm X86_64{|
nativeint_u_of_float32_u.(partial):
  vcvttss2si %xmm0, %rax
  ret
|}]

external nativeint_u_of_int : int -> nativeint# = "%nativeint#_of_int"
let nativeint_u_of_int = nativeint_u_of_int
[%%expect_asm X86_64{|
nativeint_u_of_int.(partial):
  sarq  $1, %rax
  ret
|}]

external nativeint_u_of_int_u : int# -> nativeint# = "%nativeint#_of_int#"
let nativeint_u_of_int_u = nativeint_u_of_int_u
[%%expect_asm X86_64{|
nativeint_u_of_int_u.(partial):
  ret
|}]

external nativeint_u_of_int16 : int16 -> nativeint# = "%nativeint#_of_int16"
let nativeint_u_of_int16 = nativeint_u_of_int16
[%%expect_asm X86_64{|
nativeint_u_of_int16.(partial):
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

external nativeint_u_of_int16_u : int16# -> nativeint# = "%nativeint#_of_int16#"
let nativeint_u_of_int16_u = nativeint_u_of_int16_u
[%%expect_asm X86_64{|
nativeint_u_of_int16_u.(partial):
  ret
|}]

external nativeint_u_of_int32 : int32 -> nativeint# = "%nativeint#_of_int32"
let nativeint_u_of_int32 = nativeint_u_of_int32
[%%expect_asm X86_64{|
nativeint_u_of_int32.(partial):
  movslq 8(%rax), %rax
  ret
|}]

external nativeint_u_of_int32_u : int32# -> nativeint# = "%nativeint#_of_int32#"
let nativeint_u_of_int32_u = nativeint_u_of_int32_u
[%%expect_asm X86_64{|
nativeint_u_of_int32_u.(partial):
  ret
|}]

external nativeint_u_of_int64 : int64 -> nativeint# = "%nativeint#_of_int64"
let nativeint_u_of_int64 = nativeint_u_of_int64
[%%expect_asm X86_64{|
nativeint_u_of_int64.(partial):
  movq  8(%rax), %rax
  ret
|}]

external nativeint_u_of_int64_u : int64# -> nativeint# = "%nativeint#_of_int64#"
let nativeint_u_of_int64_u = nativeint_u_of_int64_u
[%%expect_asm X86_64{|
nativeint_u_of_int64_u.(partial):
  ret
|}]

external nativeint_u_of_int8 : int8 -> nativeint# = "%nativeint#_of_int8"
let nativeint_u_of_int8 = nativeint_u_of_int8
[%%expect_asm X86_64{|
nativeint_u_of_int8.(partial):
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

external nativeint_u_of_int8_u : int8# -> nativeint# = "%nativeint#_of_int8#"
let nativeint_u_of_int8_u = nativeint_u_of_int8_u
[%%expect_asm X86_64{|
nativeint_u_of_int8_u.(partial):
  ret
|}]

external nativeint_u_of_nativeint : nativeint -> nativeint# = "%nativeint#_of_nativeint"
let nativeint_u_of_nativeint = nativeint_u_of_nativeint
[%%expect_asm X86_64{|
nativeint_u_of_nativeint.(partial):
  movq  8(%rax), %rax
  ret
|}]

external nativeint_u_or : nativeint# -> nativeint# -> nativeint# = "%nativeint#_or"
let nativeint_u_or = nativeint_u_or
[%%expect_asm X86_64{|
nativeint_u_or.(partial):
  orq   %rbx, %rax
  ret
|}]

external nativeint_u_pred : nativeint# -> nativeint# = "%nativeint#_pred"
let nativeint_u_pred = nativeint_u_pred
[%%expect_asm X86_64{|
nativeint_u_pred.(partial):
  decq  %rax
  ret
|}]

external nativeint_u_sub : nativeint# -> nativeint# -> nativeint# = "%nativeint#_sub"
let nativeint_u_sub = nativeint_u_sub
[%%expect_asm X86_64{|
nativeint_u_sub.(partial):
  subq  %rbx, %rax
  ret
|}]

external nativeint_u_succ : nativeint# -> nativeint# = "%nativeint#_succ"
let nativeint_u_succ = nativeint_u_succ
[%%expect_asm X86_64{|
nativeint_u_succ.(partial):
  incq  %rax
  ret
|}]

external nativeint_u_unsafe_div : nativeint# -> nativeint# -> nativeint# = "%nativeint#_unsafe_div"
let nativeint_u_unsafe_div = nativeint_u_unsafe_div
[%%expect_asm X86_64{|
nativeint_u_unsafe_div.(partial):
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
let nativeint_u_unsafe_mod = nativeint_u_unsafe_mod
[%%expect_asm X86_64{|
nativeint_u_unsafe_mod.(partial):
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
let nativeint_u_unsigned_compare = nativeint_u_unsigned_compare
[%%expect_asm X86_64{|
nativeint_u_unsigned_compare.(partial):
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
let nativeint_u_unsigned_greaterequal = nativeint_u_unsigned_greaterequal
[%%expect_asm X86_64{|
nativeint_u_unsigned_greaterequal.(partial):
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_unsigned_greaterthan : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_greaterthan"
let nativeint_u_unsigned_greaterthan = nativeint_u_unsigned_greaterthan
[%%expect_asm X86_64{|
nativeint_u_unsigned_greaterthan.(partial):
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_unsigned_lessequal : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_lessequal"
let nativeint_u_unsigned_lessequal = nativeint_u_unsigned_lessequal
[%%expect_asm X86_64{|
nativeint_u_unsigned_lessequal.(partial):
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_unsigned_lessthan : nativeint# -> nativeint# -> bool = "%nativeint#_unsigned_lessthan"
let nativeint_u_unsigned_lessthan = nativeint_u_unsigned_lessthan
[%%expect_asm X86_64{|
nativeint_u_unsigned_lessthan.(partial):
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_u_xor : nativeint# -> nativeint# -> nativeint# = "%nativeint#_xor"
let nativeint_u_xor = nativeint_u_xor
[%%expect_asm X86_64{|
nativeint_u_xor.(partial):
  xorq  %rbx, %rax
  ret
|}]

external nativeint_add : nativeint -> nativeint -> nativeint = "%nativeint_add"
let nativeint_add = nativeint_add
[%%expect_asm X86_64{|
nativeint_add.(partial):
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
let nativeint_and = nativeint_and
[%%expect_asm X86_64{|
nativeint_and.(partial):
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
let nativeint_asr = nativeint_asr
[%%expect_asm X86_64{|
nativeint_asr.(partial):
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
let nativeint_bswap = nativeint_bswap
[%%expect_asm X86_64{|
nativeint_bswap.(partial):
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
let nativeint_compare = nativeint_compare
[%%expect_asm X86_64{|
nativeint_compare.(partial):
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
let nativeint_div = nativeint_div
[%%expect_asm X86_64{|
nativeint_div.(partial):
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
let nativeint_equal = nativeint_equal
[%%expect_asm X86_64{|
nativeint_equal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_greaterequal : nativeint -> nativeint -> bool = "%nativeint_greaterequal"
let nativeint_greaterequal = nativeint_greaterequal
[%%expect_asm X86_64{|
nativeint_greaterequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_greaterthan : nativeint -> nativeint -> bool = "%nativeint_greaterthan"
let nativeint_greaterthan = nativeint_greaterthan
[%%expect_asm X86_64{|
nativeint_greaterthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_lessequal : nativeint -> nativeint -> bool = "%nativeint_lessequal"
let nativeint_lessequal = nativeint_lessequal
[%%expect_asm X86_64{|
nativeint_lessequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_lessthan : nativeint -> nativeint -> bool = "%nativeint_lessthan"
let nativeint_lessthan = nativeint_lessthan
[%%expect_asm X86_64{|
nativeint_lessthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_lsl : nativeint -> int -> nativeint = "%nativeint_lsl"
let nativeint_lsl = nativeint_lsl
[%%expect_asm X86_64{|
nativeint_lsl.(partial):
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
let nativeint_lsr = nativeint_lsr
[%%expect_asm X86_64{|
nativeint_lsr.(partial):
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
let nativeint_mod = nativeint_mod
[%%expect_asm X86_64{|
nativeint_mod.(partial):
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
let nativeint_mul = nativeint_mul
[%%expect_asm X86_64{|
nativeint_mul.(partial):
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
let nativeint_neg = nativeint_neg
[%%expect_asm X86_64{|
nativeint_neg.(partial):
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
let nativeint_notequal = nativeint_notequal
[%%expect_asm X86_64{|
nativeint_notequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_of_float : float -> nativeint = "%nativeint_of_float"
let nativeint_of_float = nativeint_of_float
[%%expect_asm X86_64{|
nativeint_of_float.(partial):
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
let nativeint_of_float_u = nativeint_of_float_u
[%%expect_asm X86_64{|
nativeint_of_float_u.(partial):
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
let nativeint_of_float32 = nativeint_of_float32
[%%expect_asm X86_64{|
nativeint_of_float32.(partial):
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
let nativeint_of_float32_u = nativeint_of_float32_u
[%%expect_asm X86_64{|
nativeint_of_float32_u.(partial):
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
let nativeint_of_int = nativeint_of_int
[%%expect_asm X86_64{|
nativeint_of_int.(partial):
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
let nativeint_of_int_u = nativeint_of_int_u
[%%expect_asm X86_64{|
nativeint_of_int_u.(partial):
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
let nativeint_of_int16 = nativeint_of_int16
[%%expect_asm X86_64{|
nativeint_of_int16.(partial):
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
let nativeint_of_int16_u = nativeint_of_int16_u
[%%expect_asm X86_64{|
nativeint_of_int16_u.(partial):
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
let nativeint_of_int32 = nativeint_of_int32
[%%expect_asm X86_64{|
nativeint_of_int32.(partial):
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
let nativeint_of_int32_u = nativeint_of_int32_u
[%%expect_asm X86_64{|
nativeint_of_int32_u.(partial):
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
let nativeint_of_int64 = nativeint_of_int64
[%%expect_asm X86_64{|
nativeint_of_int64.(partial):
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
let nativeint_of_int64_u = nativeint_of_int64_u
[%%expect_asm X86_64{|
nativeint_of_int64_u.(partial):
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
let nativeint_of_int8 = nativeint_of_int8
[%%expect_asm X86_64{|
nativeint_of_int8.(partial):
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
let nativeint_of_int8_u = nativeint_of_int8_u
[%%expect_asm X86_64{|
nativeint_of_int8_u.(partial):
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
let nativeint_of_nativeint_u = nativeint_of_nativeint_u
[%%expect_asm X86_64{|
nativeint_of_nativeint_u.(partial):
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
let nativeint_or = nativeint_or
[%%expect_asm X86_64{|
nativeint_or.(partial):
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
let nativeint_pred = nativeint_pred
[%%expect_asm X86_64{|
nativeint_pred.(partial):
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
let nativeint_sub = nativeint_sub
[%%expect_asm X86_64{|
nativeint_sub.(partial):
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
let nativeint_succ = nativeint_succ
[%%expect_asm X86_64{|
nativeint_succ.(partial):
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
let nativeint_unsafe_div = nativeint_unsafe_div
[%%expect_asm X86_64{|
nativeint_unsafe_div.(partial):
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
let nativeint_unsafe_mod = nativeint_unsafe_mod
[%%expect_asm X86_64{|
nativeint_unsafe_mod.(partial):
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
let nativeint_unsigned_compare = nativeint_unsigned_compare
[%%expect_asm X86_64{|
nativeint_unsigned_compare.(partial):
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
let nativeint_unsigned_greaterequal = nativeint_unsigned_greaterequal
[%%expect_asm X86_64{|
nativeint_unsigned_greaterequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_unsigned_greaterthan : nativeint -> nativeint -> bool = "%nativeint_unsigned_greaterthan"
let nativeint_unsigned_greaterthan = nativeint_unsigned_greaterthan
[%%expect_asm X86_64{|
nativeint_unsigned_greaterthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_unsigned_lessequal : nativeint -> nativeint -> bool = "%nativeint_unsigned_lessequal"
let nativeint_unsigned_lessequal = nativeint_unsigned_lessequal
[%%expect_asm X86_64{|
nativeint_unsigned_lessequal.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_unsigned_lessthan : nativeint -> nativeint -> bool = "%nativeint_unsigned_lessthan"
let nativeint_unsigned_lessthan = nativeint_unsigned_lessthan
[%%expect_asm X86_64{|
nativeint_unsigned_lessthan.(partial):
  movq  8(%rbx), %rbx
  movq  8(%rax), %rax
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

external nativeint_xor : nativeint -> nativeint -> nativeint = "%nativeint_xor"
let nativeint_xor = nativeint_xor
[%%expect_asm X86_64{|
nativeint_xor.(partial):
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
