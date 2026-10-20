(* TEST
 flags += " -O3";
 only-default-codegen;
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

type nothing = |

(* A tail call, through a partial application, to a function that never
   returns *)
module Tail_call = struct
  let[@inline never] wrong_type (_ : int) : nothing = failwith "wrong type"

  let rec read_ab : 'p. 'p -> bytes -> pos:int ref -> int -> int =
    fun _p _buf ~pos _tag -> match wrong_type !pos with (_ : nothing) -> .

  and read_a : 'p. 'p -> bytes -> pos:int ref -> int -> int =
    fun p buf ~pos tag -> (read_ab p) buf ~pos tag

  and read_a_non_tail : 'p. 'p -> bytes -> pos:int ref -> int -> int =
    fun p buf ~pos tag -> 1 + (read_ab p) buf ~pos tag
end
[%%expect_asm X86_64{|
Tail_call.wrong_type:
  subq  $8, %rsp
  leaq  <hidden PC-relative offset>(%rip), %rax
  call  caml_reraise_exn@PLT
.L0:

Tail_call.read_ab:
  movq  (%rdi), %rax
  jmp   camlTOP2__wrong_type_0_5_code@PLT

Tail_call.read_a:
  jmp   camlTOP2__read_ab_1_6_code@PLT

Tail_call.read_a_non_tail:
  subq  $8, %rsp
  call  camlTOP2__read_ab_1_6_code@PLT
.L0:
  addq  $2, %rax
  addq  $8, %rsp
  ret
|}]

(* The curry helper of a function that never returns, with an unboxed result *)
module Curry = struct
  let[@inline never] fail3 (_ : int) (_ : int) (_ : int) : float# =
    assert false

  let partial_fail3 x = fail3 x
end
[%%expect_asm X86_64{|
Curry.fail3:
  subq  $8, %rsp
  leaq  <hidden PC-relative offset>(%rip), %rax
  movq  $0, 272(%r14)
  call  caml_raise_exn@PLT
.L0:

Curry.partial_fail3:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $48, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $5367, -8(%rax)
  movq  caml_curry2_RF@GOTPCREL(%rip), %rdi
  movq  %rdi, (%rax)
  movabsq $180143985094819849, %rdi
  movq  %rdi, 8(%rax)
  movq  <hidden PC-relative offset>(%rip), %rdi
  movq  %rdi, 16(%rax)
  movq  %rbx, 24(%rax)
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  %rbx, 32(%rax)
  addq  $8, %rsp
  ret

Curry.partial_fail3:
  movq  %rax, %rdx
  movq  %rbx, %rcx
  movq  32(%rdi), %rsi
  movq  24(%rdi), %rax
  movq  16(%rsi), %r8
  movq  %rdx, %rbx
  movq  %rcx, %rdi
  jmp   *%r8
|}]
