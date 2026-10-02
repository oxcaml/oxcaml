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
  testq %rax, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let is_this (x : int or_null) =
  match x with Null -> false | This _ -> true
[%%expect_asm X86_64{|
is_this:
  testq %rax, %rax
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]


let get_or (x : int or_null) ~default =
    match x with Null -> default | This v -> v
[%%expect_asm X86_64{|
get_or:
  testq %rax, %rax
  jne   .L0
  movq  %rbx, %rax
  ret
.L0:
  ret
|}]

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
  cmpq  %rbx, %rax
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* A non-null sentinel, inverted tests, and swapped equality operands. *)
let equal_with_sentinel (a : int) (b : int) =
  if 0 <> a then
    if 0 <> b then b = a else false
  else 0 = b
[%%expect_asm X86_64{|
equal_with_sentinel:
  cmpq  %rax, %rbx
  sete  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]
