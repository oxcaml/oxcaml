(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for equality tests against transformed values *)

let add_const_eq x = x + 3 = 10
[%%expect_asm X86_64{|
add_const_eq:
  cmpq  $15, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let sub_const_ne x = x - 3 <> 10
[%%expect_asm X86_64{|
sub_const_ne:
  cmpq  $27, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let const_sub_eq x = 3 - x = 10
[%%expect_asm X86_64{|
const_sub_eq:
  cmpq  $-13, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let xor_const_eq x = x lxor 5 = 9
[%%expect_asm X86_64{|
xor_const_eq:
  xorq  $11, %rax
  shrq  $1, %rax
  cmpq  $9, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let char_eq (s : string) i = String.unsafe_get s i = ']'
[%%expect_asm X86_64{|
char_eq:
  sarq  $1, %rbx
  movzbq (%rax,%rbx), %rax
  cmpq  $93, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let if_char_ne (s : string) i = if String.unsafe_get s i <> ']' then 1 else 2
[%%expect_asm X86_64{|
if_char_ne:
  sarq  $1, %rbx
  movzbq (%rax,%rbx), %rax
  cmpq  $93, %rax
  je    .L0
  movl  $3, %eax
  ret
.L0:
  movl  $5, %eax
  ret
|}]

let chars_eq (s : string) i (t : string) j =
  String.unsafe_get s i = String.unsafe_get t j
[%%expect_asm X86_64{|
chars_eq:
  sarq  $1, %rsi
  movzbq (%rdi,%rsi), %rdi
  sarq  $1, %rbx
  movzbq (%rax,%rbx), %rax
  cmpq  %rdi, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let lor_const_eq x = (x lsr 60) lor 1 = 5
[%%expect_asm X86_64{|
lor_const_eq:
  shrq  $62, %rax
  cmpq  $2, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* [x lor 1] is odd, so the test never holds. *)
let lor_const_impossible x = x lor 1 = 4
[%%expect_asm X86_64{|
lor_const_impossible:
  movl  $1, %eax
  ret
|}]

(* [x lsl 2] is a multiple of 4, so the test never holds. *)
let lsl_const_impossible x = x lsl 2 = 5
[%%expect_asm X86_64{|
lsl_const_impossible:
  movl  $1, %eax
  ret
|}]
