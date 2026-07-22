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

(* Codegen tests for Int64_u operations *)

(* CR ttebbi: This should use the neg instruction. *)
let neg x = Int64_u.neg x
[%%expect_asm X86_64{|
neg:
  movq  %rax, %rbx
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

let add x y = Int64_u.add x y
[%%expect_asm X86_64{|
add:
  addq  %rbx, %rax
  ret
|}]

let sub x y = Int64_u.sub x y
[%%expect_asm X86_64{|
sub:
  subq  %rbx, %rax
  ret
|}]

let mul x y = Int64_u.mul x y
[%%expect_asm X86_64{|
mul:
  imulq %rbx, %rax
  ret
|}]

(* CR ttebbi: imul could be replaced with lea (x*2+x) *)
let mul_3 x = Int64_u.mul x #3L
[%%expect_asm X86_64{|
mul_3:
  leaq  (%rax,%rax,2), %rax
  ret
|}]

let div x y = Int64_u.div x y
[%%expect_asm X86_64{|
div:
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

let div_by_constant x = Int64_u.div x #1234L
[%%expect_asm X86_64{|
div_by_constant:
  movq  %rax, %rbx
  shrq  $63, %rbx
  movabsq $7653754429286296943, %rdi
  imulq %rdi
  sarq  $9, %rdx
  leaq  (%rdx,%rbx), %rax
  ret
|}]

let unsigned_div x y = Int64_u.unsigned_div x y
[%%expect_asm X86_64{|
unsigned_div:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  xorl  %edx, %edx
  divq  %rcx
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let rem x y = Int64_u.rem x y
[%%expect_asm X86_64{|
rem:
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

let unsigned_rem x y = Int64_u.unsigned_rem x y
[%%expect_asm X86_64{|
unsigned_rem:
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L0
  xorl  %edx, %edx
  divq  %rcx
  movq  %rdx, %rax
  ret
.L0:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

let unsafe_unsigned_div x y = Int64_u.unsafe_unsigned_div x y
[%%expect_asm X86_64{|
unsafe_unsigned_div:
  movq  %rbx, %rcx
  xorl  %edx, %edx
  divq  %rcx
  ret
|}]

let unsafe_unsigned_rem x y = Int64_u.unsafe_unsigned_rem x y
[%%expect_asm X86_64{|
unsafe_unsigned_rem:
  movq  %rbx, %rcx
  xorl  %edx, %edx
  divq  %rcx
  movq  %rdx, %rax
  ret
|}]

let unsigned_div_1 x = Int64_u.unsigned_div x #1L
[%%expect_asm X86_64{|
unsigned_div_1:
  ret
|}]

let unsigned_rem_1 x = Int64_u.unsigned_rem x #1L
[%%expect_asm X86_64{|
unsigned_rem_1:
  xorl  %eax, %eax
  ret
|}]

let unsigned_div_2 x = Int64_u.unsigned_div x #2L
[%%expect_asm X86_64{|
unsigned_div_2:
  shrq  $1, %rax
  ret
|}]

let unsigned_rem_2 x = Int64_u.unsigned_rem x #2L
[%%expect_asm X86_64{|
unsigned_rem_2:
  andl  $1, %eax
  ret
|}]

let unsigned_div_128 x = Int64_u.unsigned_div x #128L
[%%expect_asm X86_64{|
unsigned_div_128:
  shrq  $7, %rax
  ret
|}]

let unsigned_rem_128 x = Int64_u.unsigned_rem x #128L
[%%expect_asm X86_64{|
unsigned_rem_128:
  andl  $127, %eax
  ret
|}]

let unsigned_div_7 x = Int64_u.unsigned_div x #7L
[%%expect_asm X86_64{|
unsigned_div_7:
  movq  %rax, %rbx
  movabsq $2635249153387078803, %rdi
  movq  %rbx, %rax
  mulq  %rdi
  movq  %rdx, %rax
  subq  %rax, %rbx
  shrq  $1, %rbx
  leaq  (%rbx,%rdx), %rax
  shrq  $2, %rax
  ret
|}]

let unsigned_rem_7 x = Int64_u.unsigned_rem x #7L
[%%expect_asm X86_64{|
unsigned_rem_7:
  movq  %rax, %rbx
  movabsq $2635249153387078803, %rdi
  movq  %rbx, %rax
  mulq  %rdi
  movq  %rdx, %rax
  movq  %rbx, %rdi
  subq  %rax, %rdi
  shrq  $1, %rdi
  addq  %rdx, %rdi
  shrq  $2, %rdi
  imulq $7, %rdi
  movq  %rbx, %rax
  subq  %rdi, %rax
  ret
|}]

(* CR ttebbi: Could instead start with [cmpq $-1, %rax] *)
let unsigned_div_umaxint x = Int64_u.unsigned_div x (-#1L)
[%%expect_asm X86_64{|
unsigned_div_umaxint:
  movq  $-1, %rbx
  cmpq  %rbx, %rax
  jne   .L0
  movl  $1, %eax
  ret
.L0:
  xorl  %eax, %eax
  ret
|}]

let unsigned_rem_umaxint x = Int64_u.unsigned_rem x (-#1L)
[%%expect_asm X86_64{|
unsigned_rem_umaxint:
  movq  $-1, %rbx
  cmpq  %rbx, %rax
  jne   .L0
  xorl  %eax, %eax
  ret
.L0:
  ret
|}]

let succ x = Int64_u.succ x
[%%expect_asm X86_64{|
succ:
  incq  %rax
  ret
|}]

let pred x = Int64_u.pred x
[%%expect_asm X86_64{|
pred:
  decq  %rax
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let abs x = Int64_u.abs x
[%%expect_asm X86_64{|
abs:
  movq  %rax, %rbx
  cmpq  $0, %rbx
  jl    .L0
  movq  %rbx, %rax
  ret
.L0:
  xorl  %eax, %eax
  subq  %rbx, %rax
  ret
|}]

let logand x y = Int64_u.logand x y
[%%expect_asm X86_64{|
logand:
  andq  %rbx, %rax
  ret
|}]

let logor x y = Int64_u.logor x y
[%%expect_asm X86_64{|
logor:
  orq   %rbx, %rax
  ret
|}]

let logxor x y = Int64_u.logxor x y
[%%expect_asm X86_64{|
logxor:
  xorq  %rbx, %rax
  ret
|}]

let lognot x = Int64_u.lognot x
[%%expect_asm X86_64{|
lognot:
  xorq  $-1, %rax
  ret
|}]

let shift_left x y = Int64_u.shift_left x (Int64_u.to_int y)
[%%expect_asm X86_64{|
shift_left:
  movq  %rbx, %rcx
  salq  %cl, %rax
  ret
|}]

let shift_right x y = Int64_u.shift_right x (Int64_u.to_int y)
[%%expect_asm X86_64{|
shift_right:
  movq  %rbx, %rcx
  sarq  %cl, %rax
  ret
|}]

let shift_right_logical x y = Int64_u.shift_right_logical x (Int64_u.to_int y)
[%%expect_asm X86_64{|
shift_right_logical:
  movq  %rbx, %rcx
  shrq  %cl, %rax
  ret
|}]

let of_int x = Int64_u.of_int x
[%%expect_asm X86_64{|
of_int:
  sarq  $1, %rax
  ret
|}]

let to_int x = Int64_u.to_int x
[%%expect_asm X86_64{|
to_int:
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: This is the identity. *)
let int_roundtrip x = Int64_u.of_int x |> Int64_u.to_int
[%%expect_asm X86_64{|
int_roundtrip:
  sarq  $1, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let unsigned_to_int x = Int64_u.unsigned_to_int x
[%%expect_asm X86_64{|
unsigned_to_int:
  movq  %rax, %rbx
  cmpq  $0, %rbx
  jl    .L1
  movabsq $4611686018427387903, %rax
  cmpq  %rax, %rbx
  jg    .L1
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  leaq  1(%rbx,%rbx), %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
.L1:
  movl  $1, %eax
  ret
|}]

let of_float x = Int64_u.of_float x
[%%expect_asm X86_64{|
of_float:
  vmovsd (%rax), %xmm0
  vcvttsd2si %xmm0, %rax
  ret
|}]

let to_float x = Int64_u.to_float x
[%%expect_asm X86_64{|
to_float:
  subq  $8, %rsp
  vcvtsi2sdq %rax, %xmm0, %xmm0
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

let of_int32 x = Int64_u.of_int32 x
[%%expect_asm X86_64{|
of_int32:
  movslq 8(%rax), %rax
  ret
|}]

let to_int32 x = Int64_u.to_int32 x
[%%expect_asm X86_64{|
to_int32:
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

let of_nativeint x = Int64_u.of_nativeint x
[%%expect_asm X86_64{|
of_nativeint:
  movq  8(%rax), %rax
  ret
|}]

let to_nativeint x = Int64_u.to_nativeint x
[%%expect_asm X86_64{|
to_nativeint:
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

let of_int32_u x = Int64_u.of_int32_u x
[%%expect_asm X86_64{|
of_int32_u:
  ret
|}]

let to_int32_u x = Int64_u.to_int32_u x
[%%expect_asm X86_64{|
to_int32_u:
  movslq %eax, %rax
  ret
|}]

let of_nativeint_u x = Int64_u.of_nativeint_u x
[%%expect_asm X86_64{|
of_nativeint_u:
  ret
|}]

let to_nativeint_u x = Int64_u.to_nativeint_u x
[%%expect_asm X86_64{|
to_nativeint_u:
  ret
|}]

let bits_of_float x = Int64_u.bits_of_float x
[%%expect_asm X86_64{|
bits_of_float:
  vmovsd (%rax), %xmm0
  vmovq %xmm0, %rax
  ret
|}]

let float_of_bits x = Int64_u.float_of_bits x
[%%expect_asm X86_64{|
float_of_bits:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovq %rbx, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

(* CR ttebbi: Subtraction should be done on byte registers. *)
let compare x y = Int64_u.compare x y
[%%expect_asm X86_64{|
compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

let unsigned_compare x y = Int64_u.unsigned_compare x y
[%%expect_asm X86_64{|
unsigned_compare:
  movq  %rax, %rdi
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rbx
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

let equal x y = Int64_u.equal x y
[%%expect_asm X86_64{|
equal:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: This is very inefficient, should be like `equal`. *)
let equal_using_compare x y = Int64_u.compare x y = 0
[%%expect_asm X86_64{|
equal_using_compare:
  movq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  cmpq  $1, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let min x y = Int64_u.min x y
[%%expect_asm X86_64{|
min:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jg    .L0
  movq  %rdi, %rax
  ret
.L0:
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let max x y = Int64_u.max x y
[%%expect_asm X86_64{|
max:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jl    .L0
  movq  %rdi, %rax
  ret
.L0:
  ret
|}]

let to_int64 x = Int64_u.to_int64 x
[%%expect_asm X86_64{|
to_int64:
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

let of_int64 x = Int64_u.of_int64 x
[%%expect_asm X86_64{|
of_int64:
  movq  8(%rax), %rax
  ret
|}]

let bswap64 x = Int64_u.bswap x
[%%expect_asm X86_64{|
bswap64:
  bswap %rax
  ret
|}]

let bytes_get_int64_bswap (buf : bytes) (i : int) =
  Int64_u.bswap (Bytes.unsafe_get_int64_ne buf i)
[%%expect_asm X86_64{|
bytes_get_int64_bswap:
  sarq  $1, %rbx
  movq  (%rax,%rbx), %rax
  bswap %rax
  ret
|}]
