(* TEST
 flags += " -O3 -extension-universe upstream_compatible";
 include stdlib_upstream_compatible;
 only-default-codegen;
 expect.opt;
*)

open Stdlib_upstream_compatible

(* Codegen tests for Int64_u operations *)

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
  imulq $3, %rax
  ret
|}]

let div x y = Int64_u.div x y
[%%expect_asm X86_64{|
div:
  movq  %rax, %rdi
  movq  %rbx, %rcx
  testq %rcx, %rcx
  je    .L118
  cmpq  $-1, %rcx
  je    .L111
  movq  %rdi, %rax
  cqto
  idivq %rcx
  ret
.L111:
  xorl  %eax, %eax
  subq  %rdi, %rax
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
  movq  %rdx, %rax
  sarq  $9, %rax
  addq  %rbx, %rax
  ret
|}]

(* CR ttebbi: These are way too many instructions. *)
let unsigned_div x y = Int64_u.unsigned_div x y
[%%expect_asm X86_64{|
unsigned_div:
  movq  %rax, %rdi
  movq  %rbx, %rcx
  cmpq  $0, %rcx
  jge   .L115
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rcx
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rdi
  cmpq  %rcx, %rdi
  jge   .L109
  xorl  %eax, %eax
  ret
.L109:
  movl  $1, %eax
  ret
.L115:
  testq %rcx, %rcx
  je    .L141
  movq  %rdi, %rbx
  shrq  $1, %rbx
  cmpq  $-1, %rcx
  je    .L123
  movq  %rbx, %rax
  cqto
  idivq %rcx
  jmp   .L126
.L123:
  xorl  %eax, %eax
  subq  %rbx, %rax
.L126:
  salq  $1, %rax
  movabsq $-9223372036854775808, %rsi
  movq  %rcx, %rbx
  subq  %rsi, %rbx
  movabsq $-9223372036854775808, %rdx
  movq  %rax, %rsi
  imulq %rcx, %rsi
  subq  %rsi, %rdi
  subq  %rdx, %rdi
  cmpq  %rbx, %rdi
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
  movq  %rax, %rdi
  movq  %rbx, %rcx
  cmpq  $0, %rcx
  jge   .L118
  movabsq $-9223372036854775808, %rbx
  movq  %rcx, %rax
  subq  %rbx, %rax
  movabsq $-9223372036854775808, %rsi
  movq  %rdi, %rbx
  subq  %rsi, %rbx
  cmpq  %rax, %rbx
  jge   .L112
  xorl  %ebx, %ebx
  jmp   .L148
.L112:
  movl  $1, %ebx
  jmp   .L148
.L118:
  testq %rcx, %rcx
  je    .L144
  movq  %rdi, %rax
  shrq  $1, %rax
  cmpq  $-1, %rcx
  je    .L126
  cqto
  idivq %rcx
  movq  %rax, %rbx
  jmp   .L129
.L126:
  xorl  %ebx, %ebx
  subq  %rax, %rbx
.L129:
  salq  $1, %rbx
  movabsq $-9223372036854775808, %rsi
  movq  %rcx, %rax
  subq  %rsi, %rax
  movabsq $-9223372036854775808, %r8
  movq  %rbx, %rsi
  imulq %rcx, %rsi
  movq  %rdi, %rdx
  subq  %rsi, %rdx
  subq  %r8, %rdx
  cmpq  %rax, %rdx
  jl    .L148
  incq  %rbx
  jmp   .L148
.L144:
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L148:
  imulq %rcx, %rbx
  movq  %rdi, %rax
  subq  %rbx, %rax
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
  addq  %rdi, %rbx
  sarq  $1, %rbx
  salq  $1, %rbx
  movabsq $-9223372036854775806, %rdx
  movabsq $-9223372036854775808, %rcx
  movq  %rbx, %rdi
  salq  $1, %rdi
  movq  %rax, %rsi
  subq  %rdi, %rsi
  subq  %rcx, %rsi
  cmpq  %rdx, %rsi
  jl    .L120
  incq  %rbx
.L120:
  salq  $1, %rbx
  subq  %rbx, %rax
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
  jl    .L105
  movq  %rbx, %rax
  ret
.L105:
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

let shift_left x y = Int64_u.shift_left x y
[%%expect_asm X86_64{|
shift_left:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  salq  %cl, %rax
  ret
|}]

let shift_right x y = Int64_u.shift_right x y
[%%expect_asm X86_64{|
shift_right:
  movq  %rbx, %rcx
  sarq  $1, %rcx
  sarq  %cl, %rax
  ret
|}]

let shift_right_logical x y = Int64_u.shift_right_logical x y
[%%expect_asm X86_64{|
shift_right_logical:
  movq  %rbx, %rcx
  sarq  $1, %rcx
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

let unsigned_to_int x = Int64_u.unsigned_to_int x
[%%expect_asm X86_64{|
unsigned_to_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  cmpq  $0, %rbx
  jl    .L112
  movabsq $4611686018427387903, %rax
  cmpq  %rax, %rbx
  jg    .L109
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L115
.L117:
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  leaq  1(%rbx,%rbx), %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
.L109:
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L112:
  movl  $1, %eax
  addq  $8, %rsp
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
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L106
.L108:
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

let of_string x = Int64_u.of_string x
[%%expect_asm X86_64{|
of_string:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  caml_int64_of_string_unboxed@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L105:
  addq  $8, %rsp
  ret
|}]

let to_string x = Int64_u.to_string x
[%%expect_asm X86_64{|
to_string:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    .L106
.L108:
  leaq  8(%r15), %rsi
  movq  $2303, -8(%rsi)
  movq  caml_int64_ops@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rsi)
  movq  %rax, 8(%rsi)
  movq  camlStdlib__Int64__immstring90@GOTPCREL(%rip), %rdi
  movq  caml_int64_format@GOTPCREL(%rip), %rax
  call  caml_c_call@PLT
.L109:
  addq  $8, %rsp
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
  jb    .L105
.L107:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovq %rbx, %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

let compare x y = Int64_u.compare x y
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

let unsigned_compare x y = Int64_u.unsigned_compare x y
[%%expect_asm X86_64{|
unsigned_compare:
  movq  %rax, %rdi
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rbx
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rdi
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
  cmpq  %rbx, %rdi
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
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jg    .L105
  movq  %rdi, %rax
  ret
.L105:
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let max x y = Int64_u.max x y
[%%expect_asm X86_64{|
max:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jl    .L105
  movq  %rdi, %rax
  ret
.L105:
  ret
|}]

let to_int64 x = Int64_u.to_int64 x
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

let of_int64 x = Int64_u.of_int64 x
[%%expect_asm X86_64{|
of_int64:
  movq  8(%rax), %rax
  ret
|}]
