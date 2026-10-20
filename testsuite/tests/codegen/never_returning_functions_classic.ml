(* TEST
 flags += " -Oclassic";
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
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2048, -8(%rax)
  movq  caml_exn_Failure@GOTPCREL(%rip), %rbx
  movq  %rbx, (%rax)
  leaq  <hidden PC-relative offset>(%rip), %rbx
  movq  %rbx, 8(%rax)
  call  caml_reraise_exn@PLT
.L1:

Tail_call.read_ab:
  subq  $8, %rsp
  movq  %rdi, %rax
  movq  (%rax), %rax
  call  camlTOP2__wrong_type_0_0@PLT
.L0:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  $0, 272(%r14)
  call  caml_raise_exn@PLT
.L1:

Tail_call.read_a:
  movq  %rax, %rdx
  movq  %rbx, %rcx
  movq  %rdi, %r8
  movq  %rsi, %rax
  movq  24(%rax), %rax
  movq  %rdx, %rbx
  movq  %rcx, %rdi
  movq  %r8, %rsi
  jmp   camlTOP2__read_ab_1_1@PLT

Tail_call.read_a:
  subq  $8, %rsp
  subq  $40, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rdx
  movq  $4343, -8(%rdx)
  movq  caml_curry3@GOTPCREL(%rip), %rcx
  movq  %rcx, (%rdx)
  movabsq $252201579132747783, %rcx
  movq  %rcx, 8(%rdx)
  leaq  <hidden PC-relative offset>(%rip), %rcx
  movq  %rcx, 16(%rdx)
  movq  %rax, 24(%rdx)
  movq  %rdx, %rax
  movq  24(%rax), %rax
  addq  $8, %rsp
  jmp   camlTOP2__read_ab_1_1@PLT

Tail_call.read_a_non_tail:
  subq  $8, %rsp
  call  camlTOP2__read_ab_1_1@PLT
.L0:
  addq  $2, %rax
  addq  $8, %rsp
  ret
|}]

(* A function whose body is a call to a partial application of a function that
   never returns, inlined at a non-tail call *)
module Reinlined = struct
  exception Never

  let[@inline never] never (_ : int) (_ : int) : int = raise Never

  let partial = never 0

  let[@inline always] through_partial x = partial x

  let non_tail x = (if x > 0 then through_partial x else x) + 1
end
[%%expect_asm X86_64{|
Reinlined.never:
  subq  $8, %rsp
  movq  <hidden PC-relative offset>(%rip), %rax
  call  caml_reraise_exn@PLT
.L0:

Reinlined.partial:
  movq  %rax, %rbx
  movl  $1, %eax
  jmp   camlTOP3__never_5_5@PLT

Reinlined.through_partial:
  movq  %rax, %rbx
  movl  $1, %eax
  jmp   camlTOP3__never_5_5@PLT

Reinlined.non_tail:
  subq  $8, %rsp
  movq  %rax, %rbx
  cmpq  $1, %rbx
  jle   .L1
  movl  $1, %eax
  call  camlTOP3__never_5_5@PLT
.L0:
  jmp   .L2
.L1:
  movq  %rbx, %rax
.L2:
  addq  $2, %rax
  addq  $8, %rsp
  ret
|}]
