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
<<<<<<< HEAD
  jl    .L1
||||||| parent of 42782c097b (passes testsuite)
  jl    .L121
=======
  jl    .L102
<<<<<<< HEAD
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
||||||| parent of fa03a226ba (fixes and cleanups, make ci passes now)
  subq  $8, %rsp
=======
>>>>>>> fa03a226ba (fixes and cleanups, make ci passes now)
  sarq  $1, %rax
  xorl  %ebx, %ebx
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L109:
=======
.L104:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rdi), %rsi
  leaq  (%rsi,%rbx,2), %rsi
  movq  %rsi, (%rdi)
  incq  %rbx
  cmpq  %rax, %rbx
<<<<<<< HEAD
  jle   .L0
||||||| parent of 42782c097b (passes testsuite)
  jle   .L109
=======
  jle   .L104
>>>>>>> 42782c097b (passes testsuite)
  movl  $1, %eax
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L121:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
  movl  $1, %eax
  ret
|}]
