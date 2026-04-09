(* TEST
 only-default-codegen;
 flags = " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 expect.opt;
*)

let immutable_load l = (List.hd l) + (List.hd l)
[%%expect_asm X86_64{|
immutable_load:
  testb $1, %al
<<<<<<< HEAD
  je    .L0
||||||| parent of 42782c097b (passes testsuite)
  je    .L105
=======
  je    .L102
  subq  $8, %rsp
>>>>>>> 42782c097b (passes testsuite)
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
<<<<<<< HEAD
.L0:
||||||| parent of 42782c097b (passes testsuite)
.L105:
=======
.L102:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rax), %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]


let mutable_load r = !r + !r
[%%expect_asm X86_64{|
mutable_load:
  movq  (%rax), %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: There is no need to load the stored value. *)
let write_then_read r = r := 5; !r
[%%expect_asm X86_64{|
write_then_read:
  movq  $11, (%rax)
  movq  (%rax), %rax
  ret
|}]

let mutable_load_branch r b =
  let x = !r in
  x + if b then !r else 7
[%%expect_asm X86_64{|
mutable_load_branch:
  movq  (%rax), %rdi
  cmpq  $1, %rbx
<<<<<<< HEAD
  jne   .L0
  movl  $15, %ebx
  jmp   .L1
.L0:
  movq  %rax, %rbx
.L1:
  leaq  -1(%rax,%rbx), %rax
||||||| parent of 42782c097b (passes testsuite)
  jne   .L109
  movl  $15, %ebx
  jmp   .L113
.L109:
  movq  %rax, %rbx
.L113:
  leaq  -1(%rax,%rbx), %rax
=======
  jne   .L105
  movl  $15, %eax
  jmp   .L102
.L105:
  movq  %rdi, %rax
.L102:
  leaq  -1(%rdi,%rax), %rax
>>>>>>> 42782c097b (passes testsuite)
  ret
|}]

type t = {a: int; b: string}
let immutable_load_loop (t: t) =
  let rec foo i acc =
    if i == 0
    then acc
    else foo (i - 1) (acc + t.a)
  in
  foo 10 (t.a)
[%%expect_asm X86_64{|
immutable_load_loop:
<<<<<<< HEAD
  movq  (%rax), %rbx
  movl  $21, %edi
  movq  %rbx, %rax
  jmp   .L1
.L0:
||||||| parent of 42782c097b (passes testsuite)
  movq  (%rax), %rbx
  movl  $21, %edi
  movq  %rbx, %rax
  jmp   .L111
.L109:
=======
  movq  (%rax), %rdi
  movl  $21, %ebx
  movq  %rdi, %rax
  jmp   .L105
.L104:
>>>>>>> 42782c097b (passes testsuite)
  ret
<<<<<<< HEAD
.L1:
  leaq  -1(%rax,%rbx), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jne   .L1
  jmp   .L0
||||||| parent of 42782c097b (passes testsuite)
.L111:
  leaq  -1(%rax,%rbx), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jne   .L111
  jmp   .L109
=======
.L105:
  leaq  -1(%rax,%rdi), %rax
  addq  $-2, %rbx
  cmpq  $1, %rbx
  jne   .L105
  jmp   .L104
>>>>>>> 42782c097b (passes testsuite)
|}]

(* CR ttebbi: Load elimination inside the loop is not working. *)
let mutable_load_loop r =
  let rec foo i acc = if i == 0 then acc else foo (i - 1) (acc + !r) in
  foo 10 !r
[%%expect_asm X86_64{|
mutable_load_loop:
  movq  %rax, %rbx
  movq  (%rbx), %rax
  movl  $21, %edi
<<<<<<< HEAD
  jmp   .L1
.L0:
||||||| parent of 42782c097b (passes testsuite)
  jmp   .L111
.L109:
=======
  jmp   .L105
.L104:
>>>>>>> 42782c097b (passes testsuite)
  ret
<<<<<<< HEAD
.L1:
||||||| parent of 42782c097b (passes testsuite)
.L111:
=======
.L105:
>>>>>>> 42782c097b (passes testsuite)
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
<<<<<<< HEAD
  jne   .L1
  jmp   .L0
||||||| parent of 42782c097b (passes testsuite)
  jne   .L111
  jmp   .L109
=======
  jne   .L105
  jmp   .L104
>>>>>>> 42782c097b (passes testsuite)
|}]

(* CR ttebbi: We should figure out that the store and the load cannot alias. *)
let reload_after_nonaliasing_store r out =
  let load r = out := true; !r in
  load r + load r
[%%expect_asm X86_64{|
reload_after_nonaliasing_store:
  movq  $3, (%rbx)
  movq  (%rax), %rdi
  movq  $3, (%rbx)
  movq  (%rax), %rax
  leaq  -1(%rax,%rdi), %rax
  ret
|}]
