(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
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

(* Codegen tests for Int8_u operations *)
(* Also see the codegen problems in Int64_u and Int16_u *)

let add x y = Int8_u.add x y
[%%expect_asm X86_64{|
add:
  addq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let succ x = Int8_u.succ x
[%%expect_asm X86_64{|
succ:
  incq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let mul x y = Int8_u.mul x y
[%%expect_asm X86_64{|
mul:
  imulq %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let equal x y = Int8_u.equal x y
[%%expect_asm X86_64{|
equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let notequal x y = Int8_u.notequal x y
[%%expect_asm X86_64{|
notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let shift_left x y = Int8_u.shift_left x y
[%%expect_asm X86_64{|
shift_left:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

let shift_right x y = Int8_u.shift_right x y
[%%expect_asm X86_64{|
shift_right:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let logical_shift_right x y = Int8_u.logical_shift_right x y
[%%expect_asm X86_64{|
logical_shift_right:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  andl  $255, %eax
  shrq  %cl, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let bit_and x y = Int8_u.bit_and x y
[%%expect_asm X86_64{|
bit_and:
  andq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let bit_or x y = Int8_u.bit_or x y
[%%expect_asm X86_64{|
bit_or:
  orq   %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let bit_xor x y = Int8_u.bit_xor x y
[%%expect_asm X86_64{|
bit_xor:
  xorq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let bswap x = Int8_u.bswap x
[%%expect_asm X86_64{|
bswap:
  ret
|}]

let neg x = Int8_u.neg x
[%%expect_asm X86_64{|
neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let pred x = Int8_u.pred x
[%%expect_asm X86_64{|
pred:
  decq  %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let sub x y = Int8_u.sub x y
[%%expect_asm X86_64{|
sub:
  subq  %rbx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let div x y = Int8_u.div x y
[%%expect_asm X86_64{|
div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L114
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  ret
.L114:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  16(%r14), %rsp
  popq  16(%r14)
  popq  %r11
  jmp   *%r11
|}]

let rem x y = Int8_u.rem x y
[%%expect_asm X86_64{|
rem:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L114
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
.L114:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  16(%r14), %rsp
  popq  16(%r14)
  popq  %r11
  jmp   *%r11
|}]

let unsafe_div x y = Int8_u.unsafe_div x y
[%%expect_asm X86_64{|
unsafe_div:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let unsafe_rem x y = Int8_u.unsafe_rem x y
[%%expect_asm X86_64{|
unsafe_rem:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let compare x y = Int8_u.compare x y
[%%expect_asm X86_64{|
compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  cmpq  %rbx, %rdi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let greaterequal x y = Int8_u.greaterequal x y
[%%expect_asm X86_64{|
greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let greaterthan x y = Int8_u.greaterthan x y
[%%expect_asm X86_64{|
greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let lessequal x y = Int8_u.lessequal x y
[%%expect_asm X86_64{|
lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let lessthan x y = Int8_u.lessthan x y
[%%expect_asm X86_64{|
lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_compare x y = Int8_u.unsigned_compare x y
[%%expect_asm X86_64{|
unsigned_compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setb  %al
  movzbq %al, %rsi
  cmpq  %rbx, %rdi
  seta  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_greaterequal x y = Int8_u.unsigned_greaterequal x y
[%%expect_asm X86_64{|
unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_greaterthan x y = Int8_u.unsigned_greaterthan x y
[%%expect_asm X86_64{|
unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_lessequal x y = Int8_u.unsigned_lessequal x y
[%%expect_asm X86_64{|
unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_lessthan x y = Int8_u.unsigned_lessthan x y
[%%expect_asm X86_64{|
unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let of_float x = Int8_u.of_float x
[%%expect_asm X86_64{|
of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_float_u x = Int8_u.of_float_u x
[%%expect_asm X86_64{|
of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_float32 x = Int8_u.of_float32 x
[%%expect_asm X86_64{|
of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_float32_u x = Int8_u.of_float32_u x
[%%expect_asm X86_64{|
of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int x = Int8_u.of_int x
[%%expect_asm X86_64{|
of_int:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

let of_int_u x = Int8_u.of_int_u x
[%%expect_asm X86_64{|
of_int_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int16 x = Int8_u.of_int16 x
[%%expect_asm X86_64{|
of_int16:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

let of_int16_u x = Int8_u.of_int16_u x
[%%expect_asm X86_64{|
of_int16_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int32 x = Int8_u.of_int32 x
[%%expect_asm X86_64{|
of_int32:
  movslq 8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int32_u x = Int8_u.of_int32_u x
[%%expect_asm X86_64{|
of_int32_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int64 x = Int8_u.of_int64 x
[%%expect_asm X86_64{|
of_int64:
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int64_u x = Int8_u.of_int64_u x
[%%expect_asm X86_64{|
of_int64_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_int8 x = Int8_u.of_int8 x
[%%expect_asm X86_64{|
of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

let of_nativeint x = Int8_u.of_nativeint x
[%%expect_asm X86_64{|
of_nativeint:
  movq  8(%rax), %rax
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let of_nativeint_u x = Int8_u.of_nativeint_u x
[%%expect_asm X86_64{|
of_nativeint_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let to_float x = Int8_u.to_float x
[%%expect_asm X86_64{|
to_float:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vcvtsi2sdq %rbx, %xmm0, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

let to_float_u x = Int8_u.to_float_u x
[%%expect_asm X86_64{|
to_float_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

let to_float32 x = Int8_u.to_float32 x
[%%expect_asm X86_64{|
to_float32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  vcvtsi2ssq %rbx, %xmm0, %xmm0
  vmovss %xmm0, 8(%rax)
  addq  $8, %rsp
  ret
|}]

let to_float32_u x = Int8_u.to_float32_u x
[%%expect_asm X86_64{|
to_float32_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

let to_int x = Int8_u.to_int x
[%%expect_asm X86_64{|
to_int:
  leaq  1(%rax,%rax), %rax
  ret
|}]

let to_int_u x = Int8_u.to_int_u x
[%%expect_asm X86_64{|
to_int_u:
  ret
|}]

let to_int16 x = Int8_u.to_int16 x
[%%expect_asm X86_64{|
to_int16:
  leaq  1(%rax,%rax), %rax
  ret
|}]

let to_int16_u x = Int8_u.to_int16_u x
[%%expect_asm X86_64{|
to_int16_u:
  ret
|}]

let to_int32 x = Int8_u.to_int32 x
[%%expect_asm X86_64{|
to_int32:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movslq %ebx, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

let to_int32_u x = Int8_u.to_int32_u x
[%%expect_asm X86_64{|
to_int32_u:
  ret
|}]

let to_int64 x = Int8_u.to_int64 x
[%%expect_asm X86_64{|
to_int64:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L104
.L106:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

let to_int64_u x = Int8_u.to_int64_u x
[%%expect_asm X86_64{|
to_int64_u:
  ret
|}]

let to_int8 x = Int8_u.to_int8 x
[%%expect_asm X86_64{|
to_int8:
  leaq  1(%rax,%rax), %rax
  ret
|}]

let to_nativeint x = Int8_u.to_nativeint x
[%%expect_asm X86_64{|
to_nativeint:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L104
.L106:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]

let to_nativeint_u x = Int8_u.to_nativeint_u x
[%%expect_asm X86_64{|
to_nativeint_u:
  ret
|}]
