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
 flags += " -cfg-merge-blocks";
 expect.opt;
*)

open Intrinsics

(* Codegen tests for Int64_u operations *)

(* CR ttebbi: This should use the neg instruction. *)
let neg x = Int64_u.neg x
[%%expect_asm X86_64{|
neg:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  movq  %rbx, %rax
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
  testq %rbx, %rbx
  je    .L118
  cmpq  $-1, %rbx
  je    .L111
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  ret
.L111:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  movq  %rbx, %rax
  ret
.L118:
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

(* CR ttebbi: These are way too many instructions. *)
let unsigned_div x y = Int64_u.unsigned_div x y
[%%expect_asm X86_64{|
unsigned_div:
  movq  %rax, %r8
  cmpq  $0, %rbx
  jge   .L115
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rbx
  movabsq $-9223372036854775808, %rax
  subq  %rax, %r8
  cmpq  %rbx, %r8
  jge   .L109
  xorl  %eax, %eax
  ret
.L109:
  movl  $1, %eax
  ret
.L115:
  testq %rbx, %rbx
  je    .L141
  movq  %r8, %rax
  shrq  $1, %rax
  cmpq  $-1, %rbx
  je    .L123
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  jmp   .L126
.L123:
  xorl  %edi, %edi
  subq  %rax, %rdi
  movq  %rdi, %rax
.L126:
  salq  $1, %rax
  movabsq $-9223372036854775808, %rsi
  movq  %rbx, %rdi
  subq  %rsi, %rdi
  movabsq $-9223372036854775808, %rdx
  movq  %rax, %rsi
  imulq %rbx, %rsi
  subq  %rsi, %r8
  subq  %rdx, %r8
  cmpq  %rdi, %r8
  jge   .L134
  ret
.L134:
  incq  %rax
  ret
.L141:
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
  je    .L117
  cmpq  $-1, %rcx
  je    .L111
  cqto
  idivq %rcx
  movq  %rdx, %rax
  ret
.L111:
  xorl  %eax, %eax
  ret
.L117:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

(* CR ttebbi: These are way too many instructions. *)
let unsigned_rem x y = Int64_u.unsigned_rem x y
[%%expect_asm X86_64{|
unsigned_rem:
  movq  %rax, %rsi
  cmpq  $0, %rbx
  jge   .L118
  movabsq $-9223372036854775808, %rdi
  movq  %rbx, %rax
  subq  %rdi, %rax
  movabsq $-9223372036854775808, %rdx
  movq  %rsi, %rdi
  subq  %rdx, %rdi
  cmpq  %rax, %rdi
  jge   .L112
  xorl  %edi, %edi
  jmp   .L148
.L112:
  movl  $1, %edi
  jmp   .L148
.L118:
  testq %rbx, %rbx
  je    .L144
  movq  %rsi, %rax
  shrq  $1, %rax
  cmpq  $-1, %rbx
  je    .L126
  movq  %rbx, %rcx
  cqto
  idivq %rcx
  movq  %rax, %rdi
  jmp   .L129
.L126:
  xorl  %edi, %edi
  subq  %rax, %rdi
.L129:
  salq  $1, %rdi
  movabsq $-9223372036854775808, %rax
  movq  %rbx, %rdx
  subq  %rax, %rdx
  movabsq $-9223372036854775808, %r8
  movq  %rdi, %rcx
  imulq %rbx, %rcx
  movq  %rsi, %rax
  subq  %rcx, %rax
  subq  %r8, %rax
  cmpq  %rdx, %rax
  jl    .L148
  incq  %rdi
  jmp   .L148
.L144:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L148:
  imulq %rbx, %rdi
  movq  %rsi, %rax
  subq  %rdi, %rax
  ret
|}]

(* CR ttebbi: This could be just a bitwise and. *)
let unsigned_rem_2 x = Int64_u.unsigned_rem x #2L
[%%expect_asm X86_64{|
unsigned_rem_2:
  movq  %rax, %rbx
  shrq  $1, %rbx
  movq  %rbx, %rdi
  shrq  $63, %rdi
  addq  %rbx, %rdi
  sarq  $1, %rdi
  salq  $1, %rdi
  movabsq $-9223372036854775806, %rdx
  movabsq $-9223372036854775808, %rcx
  movq  %rdi, %rbx
  salq  $1, %rbx
  movq  %rax, %rsi
  subq  %rbx, %rsi
  subq  %rcx, %rsi
  cmpq  %rdx, %rsi
  jl    .L120
  incq  %rdi
.L120:
  salq  $1, %rdi
  subq  %rdi, %rax
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
  cmpq  $0, %rax
  jl    .L105
  ret
.L105:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
  movq  %rbx, %rax
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
  cmpq  $0, %rax
  jl    .L112
  movabsq $4611686018427387903, %rbx
  cmpq  %rbx, %rax
  jg    .L112
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L115
.L117:
  leaq  8(%r15), %rbx
  movq  $1024, -8(%rbx)
  leaq  1(%rax,%rax), %rax
  movq  %rax, (%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
.L112:
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
  jb    .L105
.L107:
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
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L106
.L108:
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
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L104
.L106:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_nativeint_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
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
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rbx
  movq  $1277, -8(%rbx)
  vmovq %rax, %xmm0
  vmovsd %xmm0, (%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}]

(* CR ttebbi: Subtraction should be done on byte registers. *)
let compare x y = Int64_u.compare x y
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

let unsigned_compare x y = Int64_u.unsigned_compare x y
[%%expect_asm X86_64{|
unsigned_compare:
  movabsq $-9223372036854775808, %rdi
  subq  %rdi, %rbx
  movabsq $-9223372036854775808, %rsi
  movq  %rax, %rdi
  subq  %rsi, %rdi
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
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
  cmpq  %rbx, %rdi
  setl  %al
  movzbq %al, %rsi
  setg  %al
  movzbq %al, %rax
  subq  %rsi, %rax
  leaq  1(%rax,%rax), %rax
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
  cmpq  %rbx, %rax
  jg    .L105
  ret
.L105:
  movq  %rbx, %rax
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let max x y = Int64_u.max x y
[%%expect_asm X86_64{|
max:
  cmpq  %rbx, %rax
  jl    .L105
  ret
.L105:
  movq  %rbx, %rax
  ret
|}]

let to_int64 x = Int64_u.to_int64 x
[%%expect_asm X86_64{|
to_int64:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L104
.L106:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rbx)
  movq  %rax, 8(%rbx)
  movq  %rbx, %rax
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
