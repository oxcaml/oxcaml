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
