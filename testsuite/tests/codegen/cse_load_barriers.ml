(* TEST
 only-default-codegen;
 expect.opt;
*)

(* Regression tests for CSE across load barriers: a mutable load must not be
   satisfied by an equation over a load that precedes an atomic load
   (acquire) or a cpu-relax hint, since that would effectively hoist the load
   above the barrier.

   The expected output for [load_across_atomic_get] below captures the
   current, incorrect behaviour: the load following [Atomic.get] is merged
   with the load preceding it.

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
|}]

(* The load after [Atomic.get] must not be merged with the one before. The
   expected output below shows the current, incorrect behaviour: there is a
   single load, and the sum is computed as [a + a]
   ([leaq -1(%rax,%rax)]). *)
let load_across_atomic_get (r : int ref) (flag : int Atomic.t) =
  let a = !r in
  if Atomic.get flag = 1 then a + !r else a
[%%expect_asm X86_64{|
load_across_atomic_get:
  movq  (%rax), %rax
  movq  (%rbx), %rbx
  cmpq  $3, %rbx
  jne   .L0
  leaq  -1(%rax,%rax), %rax
  ret
.L0:
  ret
|}]

(* [cpu_relax] is used to spin on a memory location, so the load after it
   must not be merged with the one before (which is already the case). *)
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
|}]
