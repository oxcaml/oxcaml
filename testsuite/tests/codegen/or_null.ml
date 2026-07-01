(* TEST
 include stdlib_stable;
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)

let is_null (x : int or_null) =
  match x with Null -> true | This _ -> false
[%%expect_asm X86_64{|
is_null:
  cmpq  $0, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let is_this (x : int or_null) =
  match x with Null -> false | This _ -> true
[%%expect_asm X86_64{|
is_this:
  cmpq  $0, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]


let get_or (x : int or_null) ~default =
    match x with Null -> default | This v -> v
[%%expect_asm X86_64{|
get_or:
  movq  %rax, %rdi
  movq  %rbx, %rax
  testq %rdi, %rdi
  jne   .L0
  ret
.L0:
  movq  %rdi, %rax
  ret
|}]

(* CR ttebbi: This should simplify to a single comparison. *)
let equal_int (a : int or_null) (b : int or_null) =
  let equal eq t0 t1 =
    match t0, t1 with
    | Null, Null -> true
    | This v0, This v1 -> eq v0 v1
    | _ -> false
  in
  equal Int.equal a b
[%%expect_asm X86_64{|
equal_int:
  testq %rax, %rax
  jne   .L0
  xorl  %eax, %eax
  cmpq  $0, %rbx
  sete  %al
  leaq  1(%rax,%rax), %rax
  ret
.L0:
  testq %rbx, %rbx
  jne   .L1
  movl  $1, %eax
  ret
.L1:
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]
