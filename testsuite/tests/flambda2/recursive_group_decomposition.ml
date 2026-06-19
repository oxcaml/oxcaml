(* TEST
 flambda2;
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)

(* This test exercises the decomposition of [let rec] groups performed during
   closure conversion (see [cps_function_bindings] in [lambda_to_flambda.ml]).

   Functions written in a single [let rec ... and ...] are split into the
   strongly connected components of their dependency graph: genuinely mutually
   recursive functions are kept together in one set of closures, whereas
   functions that merely happen to be written together are placed in separate
   sets. The components are emitted in dependency order, so that a function's
   callees are bound in an enclosing scope. Splitting into the smallest possible
   groups in particular lets a function that is only self-recursive be loopified
   even when it was written alongside unrelated functions.

   The [%%expect_asm] blocks are intentionally left blank; they are to be filled
   in on an x86-64 machine with [make promote]. *)

(* Genuinely mutually recursive: [f] and [g] call each other, so they form a
   single cycle and stay together in one recursive set of closures, each
   tail-jumping to the other. *)
let rec f () = g ()
and g () = f ()
[%%expect_asm X86_64{|
f:
  movl  $1, %eax
  jmp   camlTOP1__g_1_3_code@PLT

g:
  movl  $1, %eax
  jmp   camlTOP1__f_0_2_code@PLT
|}]

(* Not actually mutually recursive: [h] and [i] each only call themselves.
   Although written as one [let rec ... and ...], they have no cross-references,
   so they are split into two independent single-recursive functions, each of
   which is loopified into a self-jump. *)
let rec h () = h ()
and i () = i ()
[%%expect_asm X86_64{|
h:
.L0:
  jmp   .L0

i:
.L0:
  jmp   .L0
|}]

(* A single tail-recursive function: one recursive set of closures containing
   just [j], loopified into a tight loop. *)
let rec j () = j ()
[%%expect_asm X86_64{|
j:
.L0:
  jmp   .L0
|}]

(* Non-tail self-recursion: [nsum] is recursive, but its recursive call is not
   in tail position, so it is not loopified and remains a genuine recursive
   call (contrast with [j]). *)
let rec nsum n = if n = 0 then 0 else n + nsum (n - 1)
[%%expect_asm X86_64{|
nsum:
  movq  %rax, %rbx
  cmpq  $1, %rbx
  jne   .L0
  movl  $1, %eax
  ret
.L0:
  subq  $8, %rsp
  leaq  -2(%rbx), %rax
  movq  %rbx, (%rsp)
  call  camlTOP4__nsum_10_11_code@PLT
.L1:
  movq  (%rsp), %rbx
  leaq  -1(%rbx,%rax), %rax
  addq  $8, %rsp
  ret
|}]

(* Self-recursive but annotated [@loop never]: still a single recursive set of
   closures, but loopification is suppressed, so the recursion stays a self-call
   rather than becoming a loop (contrast with [j]). *)
let[@loop never] rec spin () = spin ()
[%%expect_asm X86_64{|
spin:
.L0:
  movl  $1, %eax
  jmp   .L0
|}]

(* One-directional dependency inside a [let rec]: [p] calls [q] but [q] only
   calls itself. Only [q] is recursive; [p] merely depends on it. The group is
   therefore split into [{q}] (a recursive set) and [{p}] (a non-recursive set
   referring to [q]), with [q]'s set enclosing [p]'s. *)
let rec p () = q ()
and q () = q ()
[%%expect_asm X86_64{|
p:
.L0:
  jmp   .L0

q:
.L0:
  jmp   .L0
|}]

(* A mutually recursive pair plus a one-way dependent: [r] and [s] call each
   other and stay together; [t] calls [r] but nothing calls [t], so [t] is
   placed in its own set, bound after the [{r; s}] set. *)
let rec r () = s ()
and s () = r ()
and t () = r ()
[%%expect_asm X86_64{|
r:
  movl  $1, %eax
  jmp   camlTOP7__s_19_22_code@PLT

s:
  movl  $1, %eax
  jmp   camlTOP7__r_18_21_code@PLT

t:
  movl  $1, %eax
  jmp   camlTOP7__r_18_21_code@PLT
|}]

(* A dependency chain with no cycles other than a final self-loop: [u] calls
   [v], [v] calls [w], and [w] calls itself. This splits into three sets [{w}],
   [{v}] and [{u}], emitted in that (leaf-to-root) order so that each function's
   callee is already in scope. *)
let rec u () = v ()
and v () = w ()
and w () = w ()
[%%expect_asm X86_64{|
u:
.L0:
  jmp   .L0

v:
.L0:
  jmp   .L0

w:
.L0:
  jmp   .L0
|}]

(* Optional arguments, self-recursive. [split_default_wrapper] turns [fac] into
   an outer wrapper (which fills in the default for [acc]) and an inner function.
   The inner function's recursive calls go back through the wrapper, so the
   wrapper and inner form a genuine cycle and are kept together in one recursive
   set of closures: they need no special treatment, behaving exactly like any
   other mutually recursive pair. *)
let rec fac ?(acc = 1) n = if n <= 1 then acc else fac ~acc:(acc * n) (n - 1)
[%%expect_asm X86_64{|
fac:
  movq  %rax, %rdi
  cmpq  $3, %rbx
  jg    .L0
  movq  %rdi, %rax
  ret
.L0:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L1:
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  movq  %rbx, %rsi
  sarq  $1, %rsi
  decq  %rdi
  imulq %rsi, %rdi
  incq  %rdi
  movq  %rdi, (%rax)
  movq  camlTOP9__fac_33@GOTPCREL(%rip), %rdi
  addq  $-2, %rbx
  addq  $8, %rsp
  jmp   camlTOP9__fac_30_31_code@PLT

fac:
  testb $1, %al
  je    .L0
  movl  $3, %eax
  jmp   .L1
.L0:
  movq  (%rax), %rax
.L1:
  addq  $-32, %rdi
  movq  16(%rdi), %rsi
  jmp   *%rsi
|}]

(* Optional arguments participating in a larger recursive group. [ping] has an
   optional argument, so it expands to a wrapper and an inner function; both end
   up in the same component as [pong], since the wrapper, the inner function and
   [pong] all (transitively) call one another. *)
let rec ping ?(n = 0) () = pong (n + 1)
and pong n = if n > 100 then n else ping ~n ()
[%%expect_asm X86_64{|
ping:
  addq  $2, %rax
  jmp   camlTOP10__pong_36_37_code@PLT

ping:
  testb $1, %al
  je    .L0
  movl  $1, %eax
  jmp   .L1
.L0:
  movq  (%rax), %rax
.L1:
  addq  $-32, %rdi
  movq  16(%rdi), %rsi
  jmp   *%rsi

pong:
  movq  %rax, %rbx
  cmpq  $201, %rbx
  jle   .L0
  movq  %rbx, %rax
  ret
.L0:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L1:
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  movq  %rbx, (%rax)
  movq  camlTOP10__ping_39@GOTPCREL(%rip), %rdi
  movl  $1, %ebx
  addq  $8, %rsp
  jmp   camlTOP10__ping_34_35_code@PLT
|}]
