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
  je    .L104
  movq  camlStdlib__List__Pmakeblock2305@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L104:
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
  jne   .L107
  jmp   .L106
.L104:
  leaq  -1(%rdi,%rax), %rax
  ret
.L106:
  movl  $15, %eax
  jmp   .L104
.L107:
  movq  %rdi, %rax
  jmp   .L104
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
  movq  (%rax), %rdi
  movl  $21, %ebx
  movq  %rdi, %rax
  jmp   .L107
.L106:
  ret
.L107:
  leaq  -1(%rax,%rdi), %rax
  addq  $-2, %rbx
  cmpq  $1, %rbx
  jne   .L107
  jmp   .L106
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
  jmp   .L108
.L107:
  ret
.L108:
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jne   .L108
  jmp   .L107
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
