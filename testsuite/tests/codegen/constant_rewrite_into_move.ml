(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for the rewrite of constant materializations into
   register-to-register moves by [Simplify_terminator]: when a constant that is
   expensive to materialize (it does not fit in 32 bits, signed) is written to a
   register while another register is statically known to already contain it,
   the materialization is rewritten into a register-to-register move. *)

(* The lowering of `unsigned_compare` subtracts `min_int` from both arguments,
   materializing it twice; register pressure sends the two materializations to
   different registers (the same-register case is covered by the deletion in
   `constant_move_elimination.ml`). The first `movabsq` is kept and the second
   one is rewritten into `movq %rdi, %rsi`, the first register still being
   known to contain the constant. *)
let[@inline never] unsigned_compare_int64 (x : int64) (y : int64) =
  Int64.unsigned_compare x y
[%%expect_asm X86_64{|
unsigned_compare_int64:
  movabsq $-9223372036854775808, %rdi
  movq  8(%rbx), %rbx
  subq  %rdi, %rbx
  movq  %rdi, %rsi
  movq  8(%rax), %rdi
  subq  %rsi, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rbx, %rdi
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

(* Both materializations of the magic constant are kept: the register holding
   it for the first division (`%rsi`) is overwritten by the `leaq` computing
   the first quotient before the second materialization, so the constant is no
   longer available in any register at that point. *)
let[@inline never] two_divisions x y =
  (x / 3) + (y / 3)
[%%expect_asm X86_64{|
two_divisions:
  movq  %rax, %rdi
  movq  %rbx, %rax
  sarq  $1, %rax
  sarq  $1, %rdi
  movq  %rax, %rbx
  shrq  $63, %rbx
  movabsq $6148914691236517206, %rsi
  imulq %rsi
  leaq  (%rdx,%rbx), %rsi
  movq  %rdi, %rbx
  shrq  $63, %rbx
  movabsq $6148914691236517206, %rdx
  movq  %rdi, %rax
  imulq %rdx
  leaq  (%rdx,%rbx), %rax
  salq  $1, %rax
  leaq  1(%rax,%rsi,2), %rax
  ret
|}]
