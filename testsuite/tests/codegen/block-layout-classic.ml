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

(* Coldness propagates forwards: all control flow following the cold call (the
   nested conditional and its join point) is only reachable through the cold
   block, so it is laid out after the hot [else] branch too. *)

let cold_call_followed_by_branch (r : int ref) x y =
  let[@inline] [@cold] mark_cold () = () in
  if x > y then begin
    mark_cold ();
    if x > 100 then r := x else r := y;
    x + y
  end
  else x - y
[%%expect_asm X86_64{|
cold_call_followed_by_branch:
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
  cmpq  $201, %rbx
  jle   .L2
  movq  %rbx, (%rax)
  jmp   .L3
.L2:
  movq  %rdi, (%rax)
.L3:
  leaq  -1(%rbx,%rdi), %rax
  jmp   .L0

cold_call_followed_by_branch.mark_cold:
  movl  $1, %eax
  ret
|}]

(* Coldness does not propagate through a join point that also has a hot
   predecessor: only the cold [then] branch is sunk, while the code after the
   conditional, which is reached from the hot [else] branch too, stays in place
   (and the cold block jumps back into it). *)

let cold_branch_rejoins_hot_path (r : int ref) x y =
  let[@inline] [@cold] mark_cold () = () in
  if x > y then begin
    mark_cold ();
    r := x
  end
  else r := y;
  let v = !r * x in
  if v > 100 then v + x else v - y
[%%expect_asm X86_64{|
cold_branch_rejoins_hot_path:
  movq  64(%r14), %rsi
  cmpq  %rdi, %rbx
  jg    .L3
  movq  %rdi, (%rax)
.L0:
  movq  %rbx, %rdx
  sarq  $1, %rdx
  movq  (%rax), %rax
  decq  %rax
  imulq %rdx, %rax
  incq  %rax
  cmpq  $201, %rax
  jle   .L2
  leaq  -1(%rax,%rbx), %rax
.L1:
  movq  %rsi, 64(%r14)
  ret
.L2:
  subq  %rdi, %rax
  incq  %rax
  jmp   .L1
.L3:
  movq  %rbx, (%rax)
  jmp   .L0

cold_branch_rejoins_hot_path.mark_cold:
  movl  $1, %eax
  ret
|}]
