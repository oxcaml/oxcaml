(* TEST
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for the deletion of redundant constant moves by
   [Simplify_terminator]: a move of a constant to a register statically known
   to already contain that constant is deleted. The constant below is too wide
   for an immediate operand, so every use requires a move to a register. *)

(* The constant is moved to a single register, used twice: the second move is
   deleted. *)
let[@inline never] same_constant x y =
  (x + 0x11223344556677) lxor (y + 0x11223344556677)
[%%expect_asm X86_64{|
same_constant:
  movabsq $9645356378410222, %rdi
  addq  %rdi, %rbx
  addq  %rdi, %rax
  xorq  %rbx, %rax
  orq   $1, %rax
  ret
|}]

(* Both moves are kept: when the second move is reached, the register contains
   a different constant. *)
let[@inline never] different_constants x y =
  (x + 0x11223344556677) lxor (y + 0x77665544332211)
[%%expect_asm X86_64{|
different_constants:
  movabsq $67216077262046242, %rdi
  addq  %rdi, %rbx
  movabsq $9645356378410222, %rdi
  addq  %rdi, %rax
  xorq  %rbx, %rax
  orq   $1, %rax
  ret
|}]

(* Both moves are kept: the second move is in another block, and register
   contents are not tracked across block boundaries (here, a call). *)
let[@inline never] callee _ = ()
[%%expect_asm X86_64{|
callee:
  movl  $1, %eax
  ret
|}]

let[@inline never] call_between x y =
  let a = x + 0x11223344556677 in
  callee a;
  a lxor (y + 0x11223344556677)
[%%expect_asm X86_64{|
call_between:
  subq  $24, %rsp
  movq  %rbx, (%rsp)
  movabsq $9645356378410222, %rbx
  addq  %rbx, %rax
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  24(%rbx), %rbx
  movq  (%rbx), %rdi
  movq  %rax, 8(%rsp)
  call  *%rdi
.L0:
  movabsq $9645356378410222, %rax
  movq  (%rsp), %rbx
  addq  %rax, %rbx
  movq  8(%rsp), %rax
  xorq  %rbx, %rax
  orq   $1, %rax
  addq  $24, %rsp
  ret
|}]

(* The second move is deleted: the instructions in between do not modify the
   register containing the constant. *)
let[@inline never] div_const_between x y =
  let a = x + 0x11223344556677 in
  let b = a / 3 in
  b lxor (y + 0x11223344556677)
[%%expect_asm X86_64{|
div_const_between:
  movabsq $9645356378410222, %rdi
  addq  %rdi, %rax
  sarq  $1, %rax
  addq  %rbx, %rdi
  movq  %rax, %rbx
  shrq  $63, %rbx
  movabsq $6148914691236517206, %rsi
  imulq %rsi
  leaq  (%rdx,%rbx), %rax
  leaq  1(%rax,%rax), %rax
  xorq  %rdi, %rax
  orq   $1, %rax
  ret
|}]

(* The second move is deleted, in a block ending with an unconditional jump to
   a non-empty block (the join point); the deletion used to be performed only
   for some shapes of terminators. *)
let[@inline never] branch_then_join p x y =
  let r =
    if p
    then (x + 0x11223344556677) lxor (y + 0x11223344556677)
    else x * y
  in
  r + 1
[%%expect_asm X86_64{|
branch_then_join:
  cmpq  $1, %rax
  jne   .L0
  sarq  $1, %rdi
  leaq  -1(%rbx), %rax
  imulq %rdi, %rax
  incq  %rax
  jmp   .L1
.L0:
  movabsq $9645356378410222, %rax
  addq  %rax, %rdi
  addq  %rbx, %rax
  xorq  %rdi, %rax
  orq   $1, %rax
.L1:
  addq  $2, %rax
  ret
|}]
