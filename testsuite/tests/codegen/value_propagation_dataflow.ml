(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* Codegen tests for the propagation of known values across blocks by the
   dataflow analysis of [Simplify_terminator] (under
   `-cfg-value-propagation-dataflow`, implied by `-experimental-optimizations`):
   a register holding a constant on all
   paths to a block is known to hold it at the start of that block, allowing
   the block-local deletion/rewrite of constant materializations to apply
   across block boundaries. *)

(* The comparison of `x` with the wide constant materializes it into `%rsi` in
   the entry block; neither branch of the `if` writes to that register, so the
   "must" join at the block computing `Int64.add z ...` keeps the value, and
   the materialization of the constant in that block is rewritten into
   `movq %rsi, %rax`. Without the dataflow analysis, each block starts with no
   known values and the constant is rematerialized with a `movabsq`. *)
let[@inline never] diamond (x : int64) (y : int64) (z : int64) =
  let a = if Int64.equal x 0x11223344556677L then y else Int64.neg y in
  Int64.compare a (Int64.add z 0x11223344556677L)
[%%expect_asm X86_64{|
diamond:
  movabsq $4822678189205111, %rsi
  movq  8(%rax), %rax
  cmpq  %rsi, %rax
  jne   .L0
  movq  8(%rbx), %rbx
  jmp   .L1
.L0:
  movq  8(%rbx), %rbx
  neg   %rbx
.L1:
  movq  %rsi, %rax
  movq  8(%rdi), %rdi
  addq  %rax, %rdi
  movq  $-1, %rsi
  xorl  %eax, %eax
  cmpq  %rdi, %rbx
  setg  %al
  cmovge %rax, %rsi
  leaq  1(%rsi,%rsi), %rax
  ret
|}]

(* The constant is materialized again after the `if`: one of the paths to the
   final block goes through a call, and the analysis forgets the values held
   in registers when control is transferred to other code. *)
let[@inline never] callee () = ()

let[@inline never] call_on_one_path (x : int64) (y : int64) =
  if Int64.equal x 0x11223344556677L then callee ();
  Int64.add y 0x11223344556677L
[%%expect_asm X86_64{|
call_on_one_path:
  subq  $8, %rsp
  movabsq $4822678189205111, %rdi
  movq  8(%rax), %rax
  cmpq  %rdi, %rax
  jne   .L1
  movq  %rbx, (%rsp)
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  24(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L0:
  movq  (%rsp), %rbx
.L1:
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rax
  movq  $2303, -8(%rax)
  movq  caml_int64_ops@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movabsq $4822678189205111, %rdi
  movq  8(%rbx), %rbx
  addq  %rdi, %rbx
  movq  %rbx, 8(%rax)
  addq  $8, %rsp
  ret
|}]
