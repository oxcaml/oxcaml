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
  movq  %rax, %rdi
  movq  %rbx, %rcx
  testq %rcx, %rcx
<<<<<<< HEAD
  je    .L1
||||||| parent of 42782c097b (passes testsuite)
  je    .L118
=======
  je    .L101
>>>>>>> 42782c097b (passes testsuite)
  cmpq  $-1, %rcx
<<<<<<< HEAD
  je    .L0
||||||| parent of 42782c097b (passes testsuite)
  je    .L111
=======
  je    .L106
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, %rax
  cqto
  idivq %rcx
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L111:
=======
.L106:
>>>>>>> 42782c097b (passes testsuite)
  xorl  %eax, %eax
  subq  %rdi, %rax
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L118:
=======
.L101:
<<<<<<< HEAD
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  subq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
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
  movq  %rax, %rdi
  movq  %rbx, %rcx
  cmpq  $0, %rcx
<<<<<<< HEAD
  jge   .L1
||||||| parent of 42782c097b (passes testsuite)
  jge   .L115
=======
  jge   .L108
>>>>>>> 42782c097b (passes testsuite)
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rcx
  movabsq $-9223372036854775808, %rax
  subq  %rax, %rdi
  cmpq  %rcx, %rdi
<<<<<<< HEAD
  jge   .L0
||||||| parent of 42782c097b (passes testsuite)
  jge   .L109
=======
  jge   .L106
>>>>>>> 42782c097b (passes testsuite)
  xorl  %eax, %eax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L109:
=======
.L106:
>>>>>>> 42782c097b (passes testsuite)
  movl  $1, %eax
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L115:
=======
.L108:
>>>>>>> 42782c097b (passes testsuite)
  testq %rcx, %rcx
<<<<<<< HEAD
  je    .L5
||||||| parent of 42782c097b (passes testsuite)
  je    .L141
=======
  je    .L107
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, %rbx
  shrq  $1, %rbx
  cmpq  $-1, %rcx
<<<<<<< HEAD
  je    .L2
||||||| parent of 42782c097b (passes testsuite)
  je    .L123
=======
  je    .L113
>>>>>>> 42782c097b (passes testsuite)
  movq  %rbx, %rax
  cqto
  idivq %rcx
<<<<<<< HEAD
  jmp   .L3
.L2:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L126
.L123:
=======
  jmp   .L116
.L113:
>>>>>>> 42782c097b (passes testsuite)
  xorl  %eax, %eax
  subq  %rbx, %rax
<<<<<<< HEAD
.L3:
||||||| parent of 42782c097b (passes testsuite)
.L126:
=======
.L116:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jge   .L4
||||||| parent of 42782c097b (passes testsuite)
  jge   .L134
=======
  jge   .L123
>>>>>>> 42782c097b (passes testsuite)
  ret
<<<<<<< HEAD
.L4:
||||||| parent of 42782c097b (passes testsuite)
.L134:
=======
.L123:
>>>>>>> 42782c097b (passes testsuite)
  incq  %rax
  ret
<<<<<<< HEAD
.L5:
||||||| parent of 42782c097b (passes testsuite)
.L141:
=======
.L107:
<<<<<<< HEAD
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  subq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
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
<<<<<<< HEAD
  je    .L1
||||||| parent of 42782c097b (passes testsuite)
  je    .L117
=======
  je    .L101
>>>>>>> 42782c097b (passes testsuite)
  cmpq  $-1, %rcx
<<<<<<< HEAD
  je    .L0
||||||| parent of 42782c097b (passes testsuite)
  je    .L111
=======
  je    .L106
>>>>>>> 42782c097b (passes testsuite)
  cqto
  idivq %rcx
  movq  %rdx, %rax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L111:
=======
.L106:
>>>>>>> 42782c097b (passes testsuite)
  xorl  %eax, %eax
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L117:
=======
.L101:
<<<<<<< HEAD
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  subq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
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
<<<<<<< HEAD
  jge   .L1
||||||| parent of 42782c097b (passes testsuite)
  jge   .L118
=======
  jge   .L110
>>>>>>> 42782c097b (passes testsuite)
  movabsq $-9223372036854775808, %rbx
  movq  %rcx, %rax
  subq  %rbx, %rax
  movabsq $-9223372036854775808, %rsi
  movq  %rdi, %rbx
  subq  %rsi, %rbx
  cmpq  %rax, %rbx
<<<<<<< HEAD
  jge   .L0
||||||| parent of 42782c097b (passes testsuite)
  jge   .L112
=======
  jge   .L108
>>>>>>> 42782c097b (passes testsuite)
  xorl  %ebx, %ebx
<<<<<<< HEAD
  jmp   .L5
.L0:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L148
.L112:
=======
  jmp   .L101
.L108:
>>>>>>> 42782c097b (passes testsuite)
  movl  $1, %ebx
<<<<<<< HEAD
  jmp   .L5
.L1:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L148
.L118:
=======
  jmp   .L101
.L110:
>>>>>>> 42782c097b (passes testsuite)
  testq %rcx, %rcx
<<<<<<< HEAD
  je    .L4
||||||| parent of 42782c097b (passes testsuite)
  je    .L144
=======
  je    .L109
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, %rax
  shrq  $1, %rax
  cmpq  $-1, %rcx
<<<<<<< HEAD
  je    .L2
||||||| parent of 42782c097b (passes testsuite)
  je    .L126
=======
  je    .L115
>>>>>>> 42782c097b (passes testsuite)
  cqto
  idivq %rcx
  movq  %rax, %rbx
<<<<<<< HEAD
  jmp   .L3
.L2:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L129
.L126:
=======
  jmp   .L118
.L115:
>>>>>>> 42782c097b (passes testsuite)
  xorl  %ebx, %ebx
  subq  %rax, %rbx
<<<<<<< HEAD
.L3:
||||||| parent of 42782c097b (passes testsuite)
.L129:
=======
.L118:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jl    .L5
||||||| parent of 42782c097b (passes testsuite)
  jl    .L148
=======
  jl    .L101
>>>>>>> 42782c097b (passes testsuite)
  incq  %rbx
<<<<<<< HEAD
  jmp   .L5
.L4:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L148
.L144:
=======
  jmp   .L101
.L109:
<<<<<<< HEAD
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  subq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
  movq  caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
<<<<<<< HEAD
.L5:
||||||| parent of 42782c097b (passes testsuite)
.L148:
=======
.L101:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jl    .L0
||||||| parent of 42782c097b (passes testsuite)
  jl    .L120
=======
  jl    .L101
>>>>>>> 42782c097b (passes testsuite)
  incq  %rbx
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L120:
=======
.L101:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jl    .L0
||||||| parent of 42782c097b (passes testsuite)
  jl    .L105
=======
  jl    .L102
>>>>>>> 42782c097b (passes testsuite)
  movq  %rbx, %rax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L105:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
  jl    .L1
||||||| parent of 42782c097b (passes testsuite)
  jl    .L112
=======
  jl    .L104
>>>>>>> 42782c097b (passes testsuite)
  movabsq $4611686018427387903, %rax
  cmpq  %rax, %rbx
<<<<<<< HEAD
  jg    .L1
||||||| parent of 42782c097b (passes testsuite)
  jg    .L112
=======
  jg    .L104
>>>>>>> 42782c097b (passes testsuite)
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L115
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L115
.L117:
=======
  jb    .L122
.L124:
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L122
.L124:
=======
  jb    .L107
.L109:
>>>>>>> 42782c097b (passes testsuite)
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  leaq  1(%rbx,%rbx), %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L112:
=======
.L104:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L105
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L105
.L107:
=======
  jb    .L108
.L110:
>>>>>>> de3d4ac415 (working prototype)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L108
.L110:
=======
  jb    .L103
.L105:
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L106
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L106
.L108:
=======
  jb    .L110
.L112:
>>>>>>> de3d4ac415 (working prototype)
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L110
.L112:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
=======
  jb    .L104
.L106:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L104
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L104
.L106:
=======
  jb    .L106
.L108:
>>>>>>> de3d4ac415 (working prototype)
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L106
.L108:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
=======
  jb    .L102
.L104:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
>>>>>>> 42782c097b (passes testsuite)
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
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L105
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L105
.L107:
=======
  jb    .L108
.L110:
>>>>>>> de3d4ac415 (working prototype)
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovq %rbx, %xmm0
  vmovsd %xmm0, (%rax)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L108
.L110:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovq %rbx, %xmm0
  vmovsd %xmm0, (%rax)
=======
  jb    .L103
.L105:
  leaq  8(%r15), %rbx
  movq  $1277, -8(%rbx)
  vmovq %rax, %xmm0
  vmovsd %xmm0, (%rbx)
  movq  %rbx, %rax
>>>>>>> 42782c097b (passes testsuite)
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
  movabsq $-9223372036854775808, %rdi
  subq  %rdi, %rbx
  movabsq $-9223372036854775808, %rsi
  movq  %rax, %rdi
  subq  %rsi, %rdi
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
<<<<<<< HEAD
  jg    .L0
||||||| parent of 42782c097b (passes testsuite)
  jg    .L105
=======
  jg    .L102
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, %rax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L105:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
  ret
|}]

(* CR ttebbi: This should be branchfree. *)
let max x y = Int64_u.max x y
[%%expect_asm X86_64{|
max:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
<<<<<<< HEAD
  jl    .L0
||||||| parent of 42782c097b (passes testsuite)
  jl    .L105
=======
  jl    .L102
>>>>>>> 42782c097b (passes testsuite)
  movq  %rdi, %rax
  ret
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L105:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
  ret
|}]

let to_int64 x = Int64_u.to_int64 x
[%%expect_asm X86_64{|
to_int64:
  subq  $8, %rsp
  subq  $24, %r15
  cmpq  (%r14), %r15
<<<<<<< HEAD
<<<<<<< HEAD
  jb    .L104
.L0:
||||||| parent of de3d4ac415 (working prototype)
  jb    .L104
.L106:
=======
  jb    .L106
.L108:
>>>>>>> de3d4ac415 (working prototype)
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
||||||| parent of 42782c097b (passes testsuite)
  jb    .L106
.L108:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
=======
  jb    .L102
.L104:
  leaq  8(%r15), %rbx
  movq  $2303, -8(%rbx)
>>>>>>> 42782c097b (passes testsuite)
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
