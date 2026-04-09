(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

open Intrinsics

(* Codegen tests for Int16_u operations *)
(* Also see the codegen problems in Int64_u *)

(* CR-someday jrayman: Potential improvements:
   * Use 32-bit registers/operations instead of 64-bit.
   * Sign extend lazily rather than eagerly.
   * Use [movsx] instead of [salq; sarq].
*)

let add x y = Int16_u.add x y
[%%expect_asm X86_64{|
add:
  addq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let succ x = Int16_u.succ x
[%%expect_asm X86_64{|
succ:
  incq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let mul x y = Int16_u.mul x y
[%%expect_asm X86_64{|
mul:
  imulq %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let equal x y = Int16_u.equal x y
[%%expect_asm X86_64{|
equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let notequal x y = Int16_u.notequal x y
[%%expect_asm X86_64{|
notequal:
  cmpq  %rbx, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let shift_left x y = Int16_u.shift_left x y
[%%expect_asm X86_64{|
shift_left:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

let shift_right x y = Int16_u.shift_right x y
[%%expect_asm X86_64{|
shift_right:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let logical_shift_right x y = Int16_u.logical_shift_right x y
[%%expect_asm X86_64{|
logical_shift_right:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  andl  $65535, %eax
  shrq  %cl, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let bit_and x y = Int16_u.bit_and x y
[%%expect_asm X86_64{|
bit_and:
  andq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let bit_or x y = Int16_u.bit_or x y
[%%expect_asm X86_64{|
bit_or:
  orq   %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let bit_xor x y = Int16_u.bit_xor x y
[%%expect_asm X86_64{|
bit_xor:
  xorq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

(* CR-someday jrayman: Both gcc and clang recommend [rolw $8, %ax]
   over [xchg] *)
let bswap x = Int16_u.bswap x
[%%expect_asm X86_64{|
bswap:
  xchg  %ah, %al
  movzwq %ax, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

(* CR-someday jrayman: Could use a [neg] instruction instead *)
let neg x = Int16_u.neg x
[%%expect_asm X86_64{|
neg:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  movq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let pred x = Int16_u.pred x
[%%expect_asm X86_64{|
pred:
  decq  %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let sub x y = Int16_u.sub x y
[%%expect_asm X86_64{|
sub:
  subq  %rbx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let div x y = Int16_u.div x y
[%%expect_asm X86_64{|
div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L101
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  ret
.L101:
  subq  $8, %rsp
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let rem x y = Int16_u.rem x y
[%%expect_asm X86_64{|
rem:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L101
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
.L101:
  subq  $8, %rsp
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let unsafe_div x y = Int16_u.unsafe_div x y
[%%expect_asm X86_64{|
unsafe_div:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let unsafe_rem x y = Int16_u.unsafe_rem x y
[%%expect_asm X86_64{|
unsafe_rem:
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rdx, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let compare x y = Int16_u.compare x y
[%%expect_asm X86_64{|
compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let greaterequal x y = Int16_u.greaterequal x y
[%%expect_asm X86_64{|
greaterequal:
  cmpq  %rbx, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let greaterthan x y = Int16_u.greaterthan x y
[%%expect_asm X86_64{|
greaterthan:
  cmpq  %rbx, %rax
  setg  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let lessequal x y = Int16_u.lessequal x y
[%%expect_asm X86_64{|
lessequal:
  cmpq  %rbx, %rax
  setle %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let lessthan x y = Int16_u.lessthan x y
[%%expect_asm X86_64{|
lessthan:
  cmpq  %rbx, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_compare x y = Int16_u.unsigned_compare x y
[%%expect_asm X86_64{|
unsigned_compare:
  movq  %rax, %rdi
  cmpq  %rbx, %rdi
  setb  %al
  movzbq %al, %rsi
  seta  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_greaterequal x y = Int16_u.unsigned_greaterequal x y
[%%expect_asm X86_64{|
unsigned_greaterequal:
  cmpq  %rbx, %rax
  setae %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_greaterthan x y = Int16_u.unsigned_greaterthan x y
[%%expect_asm X86_64{|
unsigned_greaterthan:
  cmpq  %rbx, %rax
  seta  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_lessequal x y = Int16_u.unsigned_lessequal x y
[%%expect_asm X86_64{|
unsigned_lessequal:
  cmpq  %rbx, %rax
  setbe %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_lessthan x y = Int16_u.unsigned_lessthan x y
[%%expect_asm X86_64{|
unsigned_lessthan:
  cmpq  %rbx, %rax
  setb  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let of_float x = Int16_u.of_float x
[%%expect_asm X86_64{|
of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_float_u x = Int16_u.of_float_u x
[%%expect_asm X86_64{|
of_float_u:
  vcvttsd2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_float32 x = Int16_u.of_float32 x
[%%expect_asm X86_64{|
of_float32:
  vmovss 8(%rax), %xmm0
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_float32_u x = Int16_u.of_float32_u x
[%%expect_asm X86_64{|
of_float32_u:
  vcvttss2si %xmm0, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_int x = Int16_u.of_int x
[%%expect_asm X86_64{|
of_int:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

let of_int_u x = Int16_u.of_int_u x
[%%expect_asm X86_64{|
of_int_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_int16 x = Int16_u.of_int16 x
[%%expect_asm X86_64{|
of_int16:
  salq  $47, %rax
  sarq  $48, %rax
  ret
|}]

let of_int32 x = Int16_u.of_int32 x
[%%expect_asm X86_64{|
of_int32:
  movslq 8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_int32_u x = Int16_u.of_int32_u x
[%%expect_asm X86_64{|
of_int32_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_int64 x = Int16_u.of_int64 x
[%%expect_asm X86_64{|
of_int64:
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_int64_u x = Int16_u.of_int64_u x
[%%expect_asm X86_64{|
of_int64_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_int8 x = Int16_u.of_int8 x
[%%expect_asm X86_64{|
of_int8:
  salq  $55, %rax
  sarq  $56, %rax
  ret
|}]

let of_int8_u x = Int16_u.of_int8_u x
[%%expect_asm X86_64{|
of_int8_u:
  ret
|}]

let of_nativeint x = Int16_u.of_nativeint x
[%%expect_asm X86_64{|
of_nativeint:
  movq  8(%rax), %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let of_nativeint_u x = Int16_u.of_nativeint_u x
[%%expect_asm X86_64{|
of_nativeint_u:
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let to_float x = Int16_u.to_float x
[%%expect_asm X86_64{|
to_float:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L103
.L105:
  leaq  8(%r15), %rbx
  movq  $1277, -8(%rbx)
  vcvtsi2sdq %rax, %xmm0, %xmm0
  vmovsd %xmm0, (%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let to_float_u x = Int16_u.to_float_u x
[%%expect_asm X86_64{|
to_float_u:
  vcvtsi2sdq %rax, %xmm0, %xmm0
  ret
|}]

let to_float32 x = Int16_u.to_float32 x
[%%expect_asm X86_64{|
to_float32:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L103
.L105:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_float32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  vcvtsi2ssq %rax, %xmm0, %xmm0
  vmovss %xmm0, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let to_float32_u x = Int16_u.to_float32_u x
[%%expect_asm X86_64{|
to_float32_u:
  vcvtsi2ssq %rax, %xmm0, %xmm0
  ret
|}]

let to_int x = Int16_u.to_int x
[%%expect_asm X86_64{|
to_int:
  leaq  1(%rax,%rax), %rax
  ret
|}]

let to_int_u x = Int16_u.to_int_u x
[%%expect_asm X86_64{|
to_int_u:
  ret
|}]

let to_int16 x = Int16_u.to_int16 x
[%%expect_asm X86_64{|
to_int16:
  leaq  1(%rax,%rax), %rax
  ret
|}]

let to_int32 x = Int16_u.to_int32 x
[%%expect_asm X86_64{|
to_int32:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L103
.L105:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int32_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movslq %eax, %rax
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let to_int32_u x = Int16_u.to_int32_u x
[%%expect_asm X86_64{|
to_int32_u:
  ret
|}]

let to_int64 x = Int16_u.to_int64 x
[%%expect_asm X86_64{|
to_int64:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L102
.L104:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let to_int64_u x = Int16_u.to_int64_u x
[%%expect_asm X86_64{|
to_int64_u:
  ret
|}]

let to_int8 x = Int16_u.to_int8 x
[%%expect_asm X86_64{|
to_int8:
  salq  $56, %rax
  sarq  $56, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let to_int8_u x = Int16_u.to_int8_u x
[%%expect_asm X86_64{|
to_int8_u:
  salq  $56, %rax
  sarq  $56, %rax
  ret
|}]

let to_nativeint x = Int16_u.to_nativeint x
[%%expect_asm X86_64{|
to_nativeint:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L102
.L104:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

let to_nativeint_u x = Int16_u.to_nativeint_u x
[%%expect_asm X86_64{|
to_nativeint_u:
  ret
|}]

let popcount x = Int16_u.popcount x
[%%expect_asm X86_64{|
popcount:
  popcnt %ax, %ax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let ctz x = Int16_u.ctz x
[%%expect_asm X86_64{|
ctz:
  lzcnt %ax, %ax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let clz x = Int16_u.clz x
[%%expect_asm X86_64{|
clz:
  tzcnt %ax, %ax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

let select x y z = Int16_u.select x y z
[%%expect_asm X86_64{|
select:
  cmpq  $1, %rax
  cmovne %rbx, %rdi
  movq  %rdi, %rax
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]
