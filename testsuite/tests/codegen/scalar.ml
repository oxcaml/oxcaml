(* TEST
 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

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

external int_u_of_int16_u : int16# -> int# = "%int#_of_int16#"
let int_u_of_int16_u = int_u_of_int16_u
[%%expect_asm X86_64{|
int_u_of_int16_u.(partial):
  ret
|}]

external int_u_of_int32_u : int32# -> int# = "%int#_of_int32#"
let int_u_of_int32_u = int_u_of_int32_u
[%%expect_asm X86_64{|
int_u_of_int32_u.(partial):
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

external int_u_of_int8_u : int8# -> int# = "%int#_of_int8#"
let int_u_of_int8_u = int_u_of_int8_u
[%%expect_asm X86_64{|
int_u_of_int8_u.(partial):
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

external int16_u_of_int_u : int# -> int16# = "%int16#_of_int#"
let int16_u_of_int_u = int16_u_of_int_u
[%%expect_asm X86_64{|
int16_u_of_int_u.(partial):
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

external int16_u_of_int64_u : int64# -> int16# = "%int16#_of_int64#"
let int16_u_of_int64_u = int16_u_of_int64_u
[%%expect_asm X86_64{|
int16_u_of_int64_u.(partial):
  salq  $48, %rax
  sarq  $48, %rax
  ret
|}]

external int16_u_of_int8_u : int8# -> int16# = "%int16#_of_int8#"
let int16_u_of_int8_u = int16_u_of_int8_u
[%%expect_asm X86_64{|
int16_u_of_int8_u.(partial):
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

external int32_u_of_int_u : int# -> int32# = "%int32#_of_int#"
let int32_u_of_int_u = int32_u_of_int_u
[%%expect_asm X86_64{|
int32_u_of_int_u.(partial):
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int16_u : int16# -> int32# = "%int32#_of_int16#"
let int32_u_of_int16_u = int32_u_of_int16_u
[%%expect_asm X86_64{|
int32_u_of_int16_u.(partial):
  ret
|}]

external int32_u_of_int64_u : int64# -> int32# = "%int32#_of_int64#"
let int32_u_of_int64_u = int32_u_of_int64_u
[%%expect_asm X86_64{|
int32_u_of_int64_u.(partial):
  movslq %eax, %rax
  ret
|}]

external int32_u_of_int8_u : int8# -> int32# = "%int32#_of_int8#"
let int32_u_of_int8_u = int32_u_of_int8_u
[%%expect_asm X86_64{|
int32_u_of_int8_u.(partial):
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

external int64_u_of_int_u : int# -> int64# = "%int64#_of_int#"
let int64_u_of_int_u = int64_u_of_int_u
[%%expect_asm X86_64{|
int64_u_of_int_u.(partial):
  ret
|}]

external int64_u_of_int16_u : int16# -> int64# = "%int64#_of_int16#"
let int64_u_of_int16_u = int64_u_of_int16_u
[%%expect_asm X86_64{|
int64_u_of_int16_u.(partial):
  ret
|}]

external int64_u_of_int32_u : int32# -> int64# = "%int64#_of_int32#"
let int64_u_of_int32_u = int64_u_of_int32_u
[%%expect_asm X86_64{|
int64_u_of_int32_u.(partial):
  ret
|}]

external int64_u_of_int8_u : int8# -> int64# = "%int64#_of_int8#"
let int64_u_of_int8_u = int64_u_of_int8_u
[%%expect_asm X86_64{|
int64_u_of_int8_u.(partial):
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

external int8_u_of_int_u : int# -> int8# = "%int8#_of_int#"
let int8_u_of_int_u = int8_u_of_int_u
[%%expect_asm X86_64{|
int8_u_of_int_u.(partial):
  salq  $56, %rax
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

external int8_u_of_int32_u : int32# -> int8# = "%int8#_of_int32#"
let int8_u_of_int32_u = int8_u_of_int32_u
[%%expect_asm X86_64{|
int8_u_of_int32_u.(partial):
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

external nativeint_u_of_int_u : int# -> nativeint# = "%nativeint#_of_int#"
let nativeint_u_of_int_u = nativeint_u_of_int_u
[%%expect_asm X86_64{|
nativeint_u_of_int_u.(partial):
  ret
|}]

external nativeint_u_of_int16_u : int16# -> nativeint# = "%nativeint#_of_int16#"
let nativeint_u_of_int16_u = nativeint_u_of_int16_u
[%%expect_asm X86_64{|
nativeint_u_of_int16_u.(partial):
  ret
|}]

external nativeint_u_of_int32_u : int32# -> nativeint# = "%nativeint#_of_int32#"
let nativeint_u_of_int32_u = nativeint_u_of_int32_u
[%%expect_asm X86_64{|
nativeint_u_of_int32_u.(partial):
  ret
|}]

external nativeint_u_of_int64_u : int64# -> nativeint# = "%nativeint#_of_int64#"
let nativeint_u_of_int64_u = nativeint_u_of_int64_u
[%%expect_asm X86_64{|
nativeint_u_of_int64_u.(partial):
  ret
|}]

external nativeint_u_of_int8_u : int8# -> nativeint# = "%nativeint#_of_int8#"
let nativeint_u_of_int8_u = nativeint_u_of_int8_u
[%%expect_asm X86_64{|
nativeint_u_of_int8_u.(partial):
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
