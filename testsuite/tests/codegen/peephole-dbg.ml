(* TEST
 only-default-codegen;
 flags = " -O3";
 flags += " -gdwarf-may-alter-codegen";
 expect.opt;
*)

(* The peephole optimizer fuses adjacent instructions into a single one, which
   can only carry the debug info of one of them. With -gdwarf-may-alter-codegen
   the user has asked for debugging to take precedence over code generation, so
   instructions coming from distinct source lines must be kept apart for a
   debugger to be able to step from one to the other. Without the flag, each
   function below compiles to a single arithmetic instruction (see [bump_twice]
   in load_elimination.ml). *)

type cursor = { mutable pos : int }

(* [merge_adjacent_specific_operations]: two read-modify-write additions to the
   same location, otherwise merged into [addq $4]. *)
let bump_twice c =
  c.pos <- c.pos + 1;
  c.pos <- c.pos + 1
[%%expect_asm X86_64{|
bump_twice:
  addq  $2, (%rax)
  addq  $2, (%rax)
  movl  $1, %eax
  ret
|}]

(* [fold_intop_imm]: two multiplications by a constant, otherwise folded into
   [imulq $15]. *)
let mul_twice (x : int64) =
  let y = Int64.mul x 3L in
  Int64.to_int (Int64.mul y 5L)
[%%expect_asm X86_64{|
mul_twice:
  movq  8(%rax), %rax
  imulq $3, %rax
  imulq $5, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}]

(* [fold_intop_imm_into_specific]: an addition of a constant followed by a [lea]
   reading its result, otherwise folded into [leaq 3(%rax,%rax)]. *)
let bump_then_double x =
  let y = x + 1 in
  y + y
[%%expect_asm X86_64{|
bump_then_double:
  addq  $2, %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]
