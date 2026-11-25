(* TEST
 only-default-codegen;
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

(* Regression tests for CSE across load barriers: a mutable load must not be
   satisfied by an equation over a load that precedes an atomic load
   (acquire) or a cpu-relax hint, since that would effectively hoist the load
   above the barrier.

   The fence intrinsics ("caml_load_fence" etc.) cannot be tested here: this
   machinery executes each phrase in a native toplevel, and those externals
   have no C implementation in the process (they are only rewritten to fence
   instructions at direct call sites). See oxcaml/tests/backend/cse for the
   corresponding assembly tests. *)

external cpu_relax : unit -> unit = "%cpu_relax"

(* Baseline: without a barrier, the second load is CSE'd. *)
let no_barrier r = !r + !r
[%%expect_asm X86_64{|
no_barrier:
  movq  (%rax), %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}, Arm64{|
no_barrier:
.L0:
  ldr   x0, [x0, #0]
  add   x0, x0, x0
  sub   x0, x0, #1
  ret
.L1:
|}]

(* The load after [Atomic.get] must not be merged with the one before. *)
let load_across_atomic_get (r : int ref) (flag : int Atomic.t) =
  let a = !r in
  if Atomic.get flag = 1 then a + !r else a
[%%expect_asm X86_64{|
load_across_atomic_get:
  movq  %rax, %rdi
  movq  (%rdi), %rax
  movq  (%rbx), %rbx
  cmpq  $3, %rbx
  jne   .L0
  movq  (%rdi), %rbx
  leaq  -1(%rax,%rbx), %rax
  ret
.L0:
  ret
|}, Arm64{|
load_across_atomic_get:
  orr   x2, xzr, x0
.L0:
  ldr   x0, [x2, #0]
  dmb   ishld
  ldar  x1, [x1]
  subs  xzr, x1, #3
  b.ne  .L1
  ldr   x1, [x2, #0]
  add   x0, x0, x1
  sub   x0, x0, #1
  ret
.L1:
  ret
.L2:
|}]

(* [cpu_relax] is used to spin on a memory location, so the load after it
   must not be merged with the one before. *)
let load_across_cpu_relax (r : int ref) =
  let a = !r in
  cpu_relax ();
  a + !r
[%%expect_asm X86_64{|
load_across_cpu_relax:
  subq  $8, %rsp
  movq  (%rax), %rbx
  pause
  cmpq  (%r14), %r15
  jbe   <hidden GC jump pad>
.L0:
  movq  (%rax), %rax
  leaq  -1(%rbx,%rax), %rax
  addq  $8, %rsp
  ret
|}, Arm64{|
load_across_cpu_relax:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  ldr   x1, [x0, #0]
  yield
  ldr   x16, [x28, #0]
  subs  xzr, x27, x16
  b.ls  <hidden GC jump pad>
.L1:
  ldr   x0, [x0, #0]
  add   x0, x1, x0
  sub   x0, x0, #1
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L2:
|}]
