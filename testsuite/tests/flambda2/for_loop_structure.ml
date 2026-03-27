(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

let f bound r t =
  for x = 0 to bound do
    t := !t + x
  done
[%%expect_asm X86_64{|
f:
  cmpq  $1, %rax
  jl    .L120
  movl  $1, %ebx
.L108:
  movq  (%rdi), %rsi
  leaq  -1(%rsi,%rbx), %rsi
  movq  %rsi, (%rdi)
  cmpq  %rax, %rbx
  je    .L116
  addq  $2, %rbx
  jmp   .L108
.L116:
  movl  $1, %eax
  ret
.L120:
  movl  $1, %eax
  ret
|}]
