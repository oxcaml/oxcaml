external int_u_add : int# -> int# -> int# = "%int#_add"
let int_u_add x y = int_u_add x y
[%%expect_asm X86_64{|
int_u_add:
  addq  %rbx, %rax
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

external int32_u_add : int32# -> int32# -> int32# = "%int32#_add"
let int32_u_add x y = int32_u_add x y
[%%expect_asm X86_64{|
int32_u_add:
  addq  %rbx, %rax
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

external int64_u_add : int64# -> int64# -> int64# = "%int64#_add"
let int64_u_add x y = int64_u_add x y
[%%expect_asm X86_64{|
int64_u_add:
  addq  %rbx, %rax
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

external int8_u_add : int8# -> int8# -> int8# = "%int8#_add"
let int8_u_add x y = int8_u_add x y
[%%expect_asm X86_64{|
int8_u_add:
  addq  %rbx, %rax
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

external int_add : int -> int -> int = "%int_add"
let int_add x y = int_add x y
[%%expect_asm X86_64{|
int_add:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]

external nativeint_u_add : nativeint# -> nativeint# -> nativeint# = "%nativeint#_add"
let nativeint_u_add x y = nativeint_u_add x y
[%%expect_asm X86_64{|
nativeint_u_add:
  addq  %rbx, %rax
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
