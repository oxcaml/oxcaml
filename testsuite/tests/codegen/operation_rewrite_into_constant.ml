(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for the rewrite of integer operations by
   [Simplify_terminator]: a (pure) integer operation whose result is statically
   known is deleted when its destination register already holds the result,
   and otherwise rewritten into a materialization of the result -- unless the
   result is expensive to materialize (it does not fit in 32 bits, signed), in
   which case the operation is kept. In the tests below, the value of the
   argument register is known from the branch condition
   (`-cfg-value-propagation-flow`, implied by `-experimental-optimizations`). *)

(* In the "then" branch, the register holding `x` is known to contain 11 (the
   tagged representation of 5), so the tagged addition (`addq $2`) is rewritten
   into a materialization of its result, 13. *)
let[@inline never] fold_to_constant x =
  if x = 5 then x + 1 else x - 1
[%%expect_asm X86_64{|
fold_to_constant:
  cmpq  $11, %rax
  jne   .L0
  movl  $13, %eax
  ret
.L0:
  addq  $-2, %rax
  ret
|}]

(* The tagged `lor` (`orq $11`) is deleted: its result, 11, is already the
   known content of its destination register. *)
let[@inline never] fold_deleted x =
  if x = 5 then x lor 5 else 0
[%%expect_asm X86_64{|
fold_deleted:
  cmpq  $11, %rax
  jne   .L0
  ret
.L0:
  movl  $1, %eax
  ret
|}]

(* The operations computing `x * 0x100000000` are kept although their results
   are statically known: the final result does not fit in 32 bits (signed), so
   materializing it could be more expensive than the operations themselves. *)
let[@inline never] fold_wide_kept x =
  if x = 5 then x * 0x100000000 else x
[%%expect_asm X86_64{|
fold_wide_kept:
  cmpq  $11, %rax
  jne   .L0
  movabsq $-4294967295, %rbx
  salq  $32, %rax
  addq  %rbx, %rax
  ret
.L0:
  ret
|}]
