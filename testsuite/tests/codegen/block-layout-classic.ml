(* TEST
 flags += " -Oclassic -cfg-block-layout -dcfg-invariants";
 only-default-codegen;
 expect.opt;
*)

(* Same as the cold-call tests in [block-layout.ml], in classic mode: Simplify
   does not run, so the call site is marked as cold in closure conversion. *)

let without_cold_call (r : int ref) x y =
  if x > y then begin
    r := x;
    x + y
  end
  else x - y
[%%expect_asm X86_64{|
without_cold_call:
  cmpq  %rdi, %rbx
  jle   .L0
  movq  %rbx, (%rax)
  leaq  -1(%rbx,%rdi), %rax
  ret
.L0:
  movq  %rbx, %rax
  subq  %rdi, %rax
  incq  %rax
  ret
|}]

let with_cold_call (r : int ref) x y =
  let[@inline] [@cold] mark_cold () = () in
  if x > y then begin
    mark_cold ();
    r := x;
    x + y
  end
  else x - y
[%%expect_asm X86_64{|
with_cold_call:
  movq  64(%r14), %rsi
  cmpq  %rdi, %rbx
  jg    .L1
  movq  %rbx, %rax
  subq  %rdi, %rax
  incq  %rax
.L0:
  movq  %rsi, 64(%r14)
  ret
.L1:
  movq  %rbx, (%rax)
  leaq  -1(%rbx,%rdi), %rax
  jmp   .L0

with_cold_call.mark_cold:
  movl  $1, %eax
  ret
|}]

let with_cold_call_not_inlined (r : int ref) x y =
  let[@cold] cold_not_inlined () = () in
  if x > y then begin
    cold_not_inlined ();
    r := x;
    x + y
  end
  else x - y
[%%expect_asm X86_64{|
with_cold_call_not_inlined:
  subq  $40, %rsp
  movq  %rax, (%rsp)
  movq  %rbx, %rax
  movq  %rax, 8(%rsp)
  movq  %rdi, %rax
  movq  %rax, 16(%rsp)
  movq  64(%r14), %rax
  movq  %rax, 24(%rsp)
  movq  8(%rsp), %rax
  movq  16(%rsp), %rbx
  cmpq  %rbx, %rax
  jg    .L1
  movq  8(%rsp), %rax
  subq  16(%rsp), %rax
  incq  %rax
.L0:
  movq  24(%rsp), %rbx
  movq  %rbx, 64(%r14)
  addq  $40, %rsp
  ret
.L1:
  movl  $1, %eax
  call  .LcamlTOP3__cold_not_inlined_4_4
.L2:
  movq  8(%rsp), %rax
  movq  (%rsp), %rbx
  movq  %rax, (%rbx)
  movq  8(%rsp), %rax
  movq  16(%rsp), %rbx
  leaq  -1(%rax,%rbx), %rax
  jmp   .L0

with_cold_call_not_inlined.cold_not_inlined:
  movl  $1, %eax
  ret
|}]
