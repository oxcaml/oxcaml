(* TEST
 flags += " -O3 -cfg-prologue-shrink-wrap";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc cfg";
 flags += " -extension-universe upstream_compatible";
 include stdlib_upstream_compatible;
 only-default-codegen;
 expect.opt;
*)

open Stdlib_upstream_compatible


(* CR ttebbi:
  - The cold branch should be moved to the end.
  - CFG prologue shrink wrap is not working
*)
let spill_cold_path x =
  let[@cold] cold () = () in
  let x = x + 1 in
  if x = 100 then cold();
  x + 2

[%%expect_asm X86_64{|
spill_cold_path:
  subq  $8, %rsp
  addq  $2, %rax
  cmpq  $201, %rax
  jne   .L113
  movq  %rax, (%rsp)
  movl  $1, %eax
  call  camlTOP2__cold_1_3_code@PLT
.L119:
  movq  (%rsp), %rax
.L113:
  addq  $4, %rax
  addq  $8, %rsp
  ret

spill_cold_path.cold:
  movl  $1, %eax
  ret
|}]

type t =
  { mutable a : int
  ; mutable b : int
  }

let[@cold] reduce t =
  let a = t.a in
  let b = t.b in
  t.a <- a + 1;
  a + b
;;

[%%expect_asm X86_64{|
reduce:
  movq  (%rax), %rbx
  movq  8(%rax), %rdi
  movq  %rbx, %rsi
  addq  $2, %rsi
  movq  %rsi, (%rax)
  leaq  -1(%rbx,%rdi), %rax
  ret
|}]

(* CR ttebbi: This could be:
  movq  camlTOP7__useless_movs_11@GOTPCREL(%rip), %rdi
  movq  24(%rdi), %rdi
  movq  %rax, %rsi
  neg   %rbx
  leaq  1(%rax,%rbx) %rax
  movq  %rsi, %rbx
  jmp   caml_apply2@PLT
*)

let[@cold] sink _x _y = ()
let useless_movs x y = sink (x - y) x
[%%expect_asm X86_64{|
useless_movs:
  movq  %rax, %rsi
  movq  camlTOP6__useless_movs_9@GOTPCREL(%rip), %rax
  movq  24(%rax), %rdi
  movq  %rsi, %rax
  subq  %rbx, %rax
  incq  %rax
  movq  %rsi, %rbx
  jmp   caml_apply2@PLT
|}]


(* CR ttebbi: This could benefit from callee-save registers. Also, we are using
    two stack slots when we could do with one.
*)
let f x =
  let[@inline never] g x = x + 1 in
  g x + g x
;;
[%%expect_asm X86_64{|
f:
  subq  $24, %rsp
  movq  %rax, (%rsp)
  call  camlTOP7__g_11_13_code@PLT
.L108:
  movq  %rax, 8(%rsp)
  movq  (%rsp), %rax
  call  camlTOP7__g_11_13_code@PLT
.L109:
  movq  8(%rsp), %rbx
  leaq  -1(%rax,%rbx), %rax
  addq  $24, %rsp
  ret

f.g:
  addq  $2, %rax
  ret
|}]


(* CR ttebbi: n gets spilled on every loop iteration. *)
let spill_in_loop n =
  let[@inline never] g x y = x - 1 in
  let rec loop x =
      if x < 0 then n + x else loop (g x n)
  in
  loop n
;;
[%%expect_asm X86_64{|
spill_in_loop:
  subq  $8, %rsp
  movq  %rax, %rbx
  cmpq  $1, %rax
  jge   .L111
.L108:
  leaq  -1(%rbx,%rax), %rax
  addq  $8, %rsp
  ret
.L111:
  movq  %rbx, (%rsp)
  call  camlTOP8__g_15_18_code@PLT
.L117:
  movq  (%rsp), %rbx
  cmpq  $1, %rax
  jge   .L111
  jmp   .L108

spill_in_loop.g:
  addq  $-2, %rax
  ret
|}]

(* CR ttebbi: The movq is unnecessary if incq was replaced with lea. *)
let f a b c = if a > 10 then b - c else a - b
[%%expect_asm X86_64{|
f:
  cmpq  $21, %rax
  jle   .L107
  movq  %rbx, %rax
  subq  %rdi, %rax
  incq  %rax
  ret
.L107:
  subq  %rbx, %rax
  incq  %rax
  ret
|}]


(* CR ttebbi: The write barrier doesn't really need xmm registers. If we
    implemented an assembly fast-path or somehow assert that it doesn't access
    xmm registers, we could treat them as callee-saved.
    Even if we don't do that, spilling the unboxed float in addition to keeping
    the boxed version in a callee-saved register is unnecessary.
*)
type t = { mutable unboxed : float }
let spill_xmm_on_caml_modify (a : float) (t : t) r =
  let b = a +. 1. in
  t.unboxed <- b;
  r := a;
  t.unboxed <- b;
  t.unboxed <- a;
  a
;;
[%%expect_asm X86_64{|
spill_xmm_on_caml_modify:
  subq  $24, %rsp
  movq  %rax, %r12
  vmovsd (%r12), %xmm0
  vmovsd %xmm0, (%rsp)
  vmovsd .L109(%rip), %xmm0
  vmovsd (%rsp), %xmm1
  vaddsd %xmm0, %xmm1, %xmm0
  vmovsd %xmm0, 8(%rsp)
  vmovsd %xmm0, (%rbx)
  movq  %r12, %rsi
  call  caml_modify@PLT
  vmovsd 8(%rsp), %xmm0
  vmovsd %xmm0, (%rbx)
  vmovsd (%rsp), %xmm0
  vmovsd %xmm0, (%rbx)
  movq  %r12, %rax
  addq  $24, %rsp
  ret
|}]


(* CR ttebbi: The register allocator puts a bunch of loads in the beginning
    of the function, even though they are unnecessary on most paths.
*)
let unnecessary_moves (a : int) (b : int) (c : int) (d : int) f =
  let x = a + b in
  if a < b then a else if c < d then (f b; x) else x
;;
[%%expect_asm X86_64{|
unnecessary_moves:
  movq  %rax, %rcx
  movq  %rbx, %r8
  movq  %rdx, %rbx
  leaq  -1(%rcx,%r8), %rax
  cmpq  %r8, %rcx
  jge   .L106
  movq  %rcx, %rax
  ret
.L106:
  cmpq  %rsi, %rdi
  jge   .L112
  subq  $8, %rsp
  movq  %rax, (%rsp)
  movq  (%rbx), %rdi
  movq  %r8, %rax
  call  *%rdi
.L117:
  movq  (%rsp), %rax
  addq  $8, %rsp
  ret
.L112:
  ret
|}]


(* CR ttebbi: Moving the addition to after the call causes two spills instead
    of one. `movq  %rdi, %rbx` is also unnecessary.
*)
let spill_one_or_two (a : int) (b : int) f =
  let x = a + b in f (); x
;;
[%%expect_asm X86_64{|
spill_one_or_two:
  subq  $24, %rsp
  movq  %rax, (%rsp)
  movq  %rbx, 8(%rsp)
  movq  %rdi, %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L107:
  movq  (%rsp), %rax
  movq  8(%rsp), %rbx
  leaq  -1(%rax,%rbx), %rax
  addq  $24, %rsp
  ret
|}]
