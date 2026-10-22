(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

(* When stack checks are disabled, a function whose frame is at least
   [Stack_guard_stride] bytes probes the prospective frame at stride
   intervals, so that a frame big enough to step over the stack's guard page
   faults in it instead.  To get a large frame with a small body, each
   function here calls a callee returning a large unboxed product: a few of
   its unboxed floats come back in registers and the domain-state area, the
   rest in a large outgoing stack area in the caller's frame.  The leading
   [float#] makes the leaf projection shallow. *)

type t2 = #(float# * float#)
type t4 = #(t2 * t2)
type t8 = #(t4 * t4)
type t16 = #(t8 * t8)
type t32 = #(t16 * t16)
type t64 = #(t32 * t32)
type t128 = #(t64 * t64)
type t256 = #(t128 * t128)
type t512 = #(t256 * t256)
type t1024 = #(t512 * t512)
type t2048 = #(t1024 * t1024)
type t1025 = #(float# * #(t512 * t512))
type t4097 = #(float# * #(t2048 * t2048))

let[@inline never] g1025 (x : float#) : t1025 =
  let p2 = #(x, x) in
  let p4 = #(p2, p2) in
  let p8 = #(p4, p4) in
  let p16 = #(p8, p8) in
  let p32 = #(p16, p16) in
  let p64 = #(p32, p32) in
  let p128 = #(p64, p64) in
  let p256 = #(p128, p128) in
  let p512 = #(p256, p256) in
  #(x, #(p512, p512))

(* A result area of one whole stride plus a remainder: few enough
   strides to unroll.  The entry prologue has already moved %rsp by a
   few untouched bytes, so the probe at offset 0 re-anchors the chain
   below it; the final probe covers the frame's last partial stride. *)
let[@inline never] f_unrolled (x : float#) : float# =
  let #(y, _) = g1025 x in
  y
[%%expect_asm X86_64{|
f_unrolled:
  subq  $8, %rsp
  orq   $0, (%rsp)
  orq   $0, -4096(%rsp)
  orq   $0, -7632(%rsp)
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rax
  movq  (%rax), %rbx
  subq  $7616, %rsp
  call  *%rbx
.L0:
  addq  $7616, %rsp
  addq  $8, %rsp
  ret
|}]

let[@inline never] g4097 (x : float#) : t4097 =
  let p2 = #(x, x) in
  let p4 = #(p2, p2) in
  let p8 = #(p4, p4) in
  let p16 = #(p8, p8) in
  let p32 = #(p16, p16) in
  let p64 = #(p32, p32) in
  let p128 = #(p64, p64) in
  let p256 = #(p128, p128) in
  let p512 = #(p256, p256) in
  let p1024 = #(p512, p512) in
  let p2048 = #(p1024, p1024) in
  #(x, #(p2048, p2048))

(* A result area of several strides: too many to unroll, so the probes
   become a loop stepping %r10 downwards.  Both branches make a large
   call, so the single stack check sits at their common dominator,
   probing the function-wide maximum frame (the [else] branch's smaller
   call is covered by the same probes).  The prologue has already run,
   so %r10 may be live: the push saving it doubles as the probe
   anchoring the loop at offset 0. *)
let[@inline never] f_loop (b : bool) (x : float#) : float# =
  if b
  then
    let #(y, _) = g4097 x in
    y
  else
    let #(y, _) = g1025 x in
    y
[%%expect_asm X86_64{|
f_loop:
  subq  $8, %rsp
  pushq %r10
  movq  $-4088, %r10
.L0:
  orq   $0, (%rsp,%r10)
  subq  $4096, %r10
  cmpq  $-28664, %r10
  jge   .L0
  popq  %r10
  orq   $0, -32208(%rsp)
  cmpq  $1, %rax
  jne   .L2
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  32(%rax), %rax
  movq  (%rax), %rbx
  subq  $7616, %rsp
  call  *%rbx
.L1:
  addq  $7616, %rsp
  addq  $8, %rsp
  ret
.L2:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  24(%rax), %rax
  movq  (%rax), %rbx
  subq  $32192, %rsp
  call  *%rbx
.L3:
  addq  $32192, %rsp
  addq  $8, %rsp
  ret
|}]

(* The fast path returns without a frame, so the stack check sinks into
   the [else] branch, where %r10 may be live: the probe loop saves it
   with a push, which doubles as the anchoring probe at offset 0 (the
   bias keeps the probed addresses the same as in [f_loop]'s loop). *)
let[@inline never] f_sunk (b : bool) (x : float#) : float# =
  if b
  then x
  else
    let #(y, _) = g4097 x in
    y
[%%expect_asm X86_64{|
f_sunk:
  cmpq  $1, %rax
  jne   .L2
  pushq %r10
  movq  $-4088, %r10
.L0:
  orq   $0, (%rsp,%r10)
  subq  $4096, %r10
  cmpq  $-28664, %r10
  jge   .L0
  popq  %r10
  orq   $0, -32208(%rsp)
  subq  $8, %rsp
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  24(%rax), %rax
  movq  (%rax), %rbx
  subq  $32192, %rsp
  call  *%rbx
.L1:
  addq  $32192, %rsp
  addq  $8, %rsp
  ret
.L2:
  ret
|}]
