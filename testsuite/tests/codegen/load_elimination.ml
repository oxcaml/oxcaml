(* TEST
 only-default-codegen;
 flags = " -O3";
 expect.opt;
*)

let immutable_load l = (List.hd l) + (List.hd l)
[%%expect_asm X86_64{|
immutable_load:
  testb $1, %al
  je    .L105
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L105:
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

let mutable_load_branch r b =
  let x = !r in
  x + if b then !r else 7
[%%expect_asm X86_64{|
mutable_load_branch:
  movq  (%rax), %rax
  cmpq  $1, %rbx
  jne   .L109
  movl  $15, %ebx
  jmp   .L113
.L109:
  movq  %rax, %rbx
.L113:
  leaq  -1(%rax,%rbx), %rax
  ret
|}]

let immutable_load_loop l =
  let rec foo i acc = if i == 0 then acc else foo (i - 1) (acc + List.hd l) in
  foo 10 (List.hd l)
[%%expect_asm X86_64{|
immutable_load_loop:
  testb $1, %al
  je    .L105
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L105:
  movq  (%rax), %rbx
  movl  $21, %edi
  movq  %rbx, %rax
  jmp   .L115
.L113:
  ret
.L115:
  leaq  -1(%rax,%rbx), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jne   .L115
  jmp   .L113
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
  jmp   .L111
.L109:
  ret
.L111:
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jne   .L111
  jmp   .L109
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
