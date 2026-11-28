(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -experimental-optimizations";
 flags += " -no-flambda2-simplify-stubs";
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

open Intrinsics


(* CR ttebbi: WhatWe could have been a nice dense loop got
   interrupted by the loop exit path. In addition, we should do
   loop rotation to remove the unconditional jump. *)
let loop_code_layout n =
  let[@cold] cold () = () in
  let sum = ref 0 in
  let rec loop x =
    sum := !sum + x;
    if x = 0 then (cold (); !sum) else loop (x - 1)
  in
  loop n
[%%expect_asm X86_64{|
loop_code_layout:
  subq  $8, %rsp
  movl  $1, %ebx
.L0:
  leaq  -1(%rbx,%rax), %rbx
  cmpq  $1, %rax
  jne   .L2
  movq  %rbx, (%rsp)
  movl  $1, %eax
  call  camlTOP2__cold_1_4_code@PLT
.L1:
  movq  (%rsp), %rax
  addq  $8, %rsp
  ret
.L2:
  addq  $-2, %rax
  jmp   .L0

loop_code_layout.cold:
  movl  $1, %eax
  ret
|}, Arm64{|
loop_code_layout:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  orr   x1, xzr, #1
.L1:
.L2:
  add   x1, x1, x0
  sub   x1, x1, #1
  subs  xzr, x0, #1
  b.ne  .L4
  str   x1, [sp, #0]
  orr   x0, xzr, #1
  bl    camlTOP2__cold_1_4_code
.L3:
  ldr   x0, [sp, #0]
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L4:
  sub   x0, x0, #2
  b     .L1
.L5:

loop_code_layout.cold:
  orr   x0, xzr, #1
  ret
|}]

let for_loop_layout n f =
  for i = 0 to n do
    f()
  done
[%%expect_asm X86_64{|
for_loop_layout:
  cmpq  $1, %rax
  jl    .L2
  subq  $24, %rsp
  movq  %rbx, (%rsp)
  sarq  $1, %rax
  movq  %rax, 16(%rsp)
  xorl  %eax, %eax
.L0:
  movq  %rax, 8(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L1:
  movq  8(%rsp), %rax
  incq  %rax
  movq  (%rsp), %rbx
  movq  16(%rsp), %rdi
  cmpq  %rdi, %rax
  jle   .L0
  movl  $1, %eax
  addq  $24, %rsp
  ret
.L2:
  movl  $1, %eax
  ret
|}, Arm64{|
for_loop_layout:
.L0:
  subs  xzr, x0, #1
  b.lt  .L3
  sub   sp, sp, #32
  str   lr, [sp, #24]
  str   x1, [sp, #0]
  sbfm  x0, x0, #1, #63
  str   x0, [sp, #16]
  orr   x0, xzr, xzr
.L1:
  str   x0, [sp, #8]
  orr   x0, xzr, #1
  ldr   x2, [x1, #0]
  blr   x2
.L2:
  ldr   x0, [sp, #8]
  add   x0, x0, #1
  ldr   x1, [sp, #0]
  ldr   x2, [sp, #16]
  subs  xzr, x0, x2
  b.le  .L1
  orr   x0, xzr, #1
  ldr   lr, [sp, #24]
  add   sp, sp, #32
  ret
.L3:
  orr   x0, xzr, #1
  ret
.L4:
|}]

(* CR ttebbi: loop peeling could avoid repeating List.hd *)
let loop_with_non_dominating_load x l =
  let[@inline always] rec loop i acc =
    if i > 0 then loop (i - 1) (acc + List.hd l)
    else acc
  in
  loop 100 0
[%%expect_asm X86_64{|
loop_with_non_dominating_load:
  subq  $8, %rsp
  movl  $1, %eax
  movl  $201, %edi
.L0:
  testb $1, %bl
  je    .L2
  movq  camlStdlib__List__Pmakeblock2543_19@GOTPCREL(%rip), %rax
  call  caml_reraise_exn@PLT
.L1:
.L2:
  movq  (%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rdi
  cmpq  $1, %rdi
  jg    .L0
  addq  $8, %rsp
  ret
|}, Arm64{|
loop_with_non_dominating_load:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  orr   x0, xzr, #1
  movz  x2, #201, lsl #0
.L1:
.L2:
  tbz   x1, #0, .L5
.L3:
  adrp  x0, :got:camlStdlib__List__Pmakeblock2543_19
  ldr   x0, [x0, :got_lo12:camlStdlib__List__Pmakeblock2543_19]
  bl    caml_reraise_exn
.L4:
.L5:
.L6:
  ldr   x3, [x1, #0]
.L7:
  add   x0, x0, x3
  sub   x0, x0, #1
  sub   x2, x2, #2
  subs  xzr, x2, #1
  b.gt  .L1
.L8:
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L9:
|}]


(* CR ttebbi: Closure values are reloaded in the loop,
   despite having a dominating use. *)
let f x =
  let[@cold] do_work () =
    let sum = ref (x + x) in
    for i = 1 to 100 do
      sum := !sum + x
    done;
    !sum
  in
  do_work ()
[%%expect_asm X86_64{|
f:
  subq  $8, %rsp
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $3319, -8(%rbx)
  movq  <hidden PC-relative offset>(%rip), %rdi
  movq  %rdi, (%rbx)
  movabsq $108086391056891911, %rdi
  movq  %rdi, 8(%rbx)
  movq  %rax, 16(%rbx)
  movl  $1, %eax
  addq  $8, %rsp
  jmp   camlTOP5__do_work_7_15_code@PLT

f.do_work:
  movq  16(%rbx), %rax
  leaq  -1(%rax,%rax), %rax
  movl  $1, %edi
.L0:
  movq  16(%rbx), %rsi
  leaq  -1(%rax,%rsi), %rax
  incq  %rdi
  cmpq  $100, %rdi
  jle   .L0
  ret
|}, Arm64{|
f:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  ldr   x16, [x28, #0]
  sub   x27, x27, #32
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L1:
  add   x1, x27, #8
  movz  x2, #3319, lsl #0
  str   x2, [x1, #-8]
  adrp  x2, <hidden PC-relative offset>
  ldr   x2, [x2, <hidden PC-relative offset>]
  str   x2, [x1, #0]
  movz  x2, #7, lsl #0
  movk  x2, #384, lsl #48
  str   x2, [x1, #8]
  str   x0, [x1, #16]
  orr   x0, xzr, #1
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  b     camlTOP5__do_work_7_15_code
.L2:

f.do_work:
  ldr   x0, [x1, #16]
.L0:
  add   x0, x0, x0
  sub   x0, x0, #1
  orr   x2, xzr, #1
.L1:
  ldr   x3, [x1, #16]
  add   x0, x0, x3
  sub   x0, x0, #1
  add   x2, x2, #1
  subs  xzr, x2, #100
  b.le  .L1
  ret
.L2:
|}]


(* CR ttebbi: noop loop could be eliminated
   https://github.com/oxcaml/oxcaml/issues/4752 *)
let noop_loop lo hi = for i = lo to hi do () done
[%%expect_asm X86_64{|
noop_loop:
  cmpq  %rbx, %rax
  jg    .L1
  sarq  $1, %rax
  sarq  $1, %rbx
.L0:
  incq  %rax
  cmpq  %rbx, %rax
  jle   .L0
.L1:
  movl  $1, %eax
  ret
|}, Arm64{|
noop_loop:
.L0:
  subs  xzr, x0, x1
  b.gt  .L2
  sbfm  x0, x0, #1, #63
  sbfm  x1, x1, #1, #63
.L1:
  add   x0, x0, #1
  subs  xzr, x0, x1
  b.le  .L1
.L2:
  orr   x0, xzr, #1
  ret
.L3:
|}]


(* CR ttebbi: We should replace a multiplication with a constant in a loop with
    an addition in an extra register. First is a minimal example and then a
    more realistic one.
*)

let f n =
  let sum = ref 0 in
  for i = 0 to n do
    sum := !sum + 3 * i;
  done;
  !sum
[%%expect_asm X86_64{|
f:
  cmpq  $1, %rax
  jl    .L1
  movq  %rax, %rbx
  sarq  $1, %rbx
  movl  $1, %eax
  xorl  %edi, %edi
.L0:
  movq  %rdi, %rsi
  imulq $6, %rsi
  addq  %rsi, %rax
  incq  %rdi
  cmpq  %rbx, %rdi
  jle   .L0
  ret
.L1:
  movl  $1, %eax
  ret
|}, Arm64{|
f:
.L0:
  subs  xzr, x0, #1
  b.lt  .L2
  sbfm  x2, x0, #1, #63
  orr   x0, xzr, #1
  orr   x1, xzr, xzr
.L1:
  ubfm  x3, x1, #63, #62
  orr   x4, xzr, #3
  madd  x0, x4, x3, x0
  add   x1, x1, #1
  subs  xzr, x1, x2
  b.le  .L1
  ret
.L2:
  orr   x0, xzr, #1
  ret
.L3:
|}]

module M = struct
  type f64 = Float_u.t
  type t = #(f64 * f64 * f64) array

  external unsafe_get :
    (t[@local_opt]) -> (int[@local_opt]) -> #(f64 * f64 * f64)
    @@ portable = "%array_unsafe_get"

  external length :
    (t[@local_opt]) @ immutable -> int @@ portable = "%array_length"

  let f (t : t) =
    let sum = ref 0.0 in
    for i = 0 to length t - 1 do
      let #(x,y,z) = unsafe_get t i in
      sum :=
        Float_u.(!sum +. to_float x *. to_float y *. to_float z)
    done;
    Float_u.of_float (!sum)
end
[%%expect_asm X86_64{|
M.f:
  movq  %rax, %rsi
  movq  -8(%rsi), %rax
  salq  $8, %rax
  shrq  $18, %rax
  movq  %rax, %rbx
  shrq  $63, %rbx
  movabsq $6148914691236517206, %rdi
  imulq %rdi
  leaq  (%rdx,%rbx), %rax
  leaq  -1(%rax,%rax), %rax
  cmpq  $1, %rax
  jl    .L1
  sarq  $1, %rax
  vxorpd %xmm0, %xmm0, %xmm0
  xorl  %ebx, %ebx
.L0:
  movq  %rbx, %rdi
  imulq $6, %rdi
  incq  %rdi
  vmovsd -4(%rsi,%rdi,4), %xmm1
  vmulsd 4(%rsi,%rdi,4), %xmm1, %xmm1
  vmulsd 12(%rsi,%rdi,4), %xmm1, %xmm1
  vaddsd %xmm1, %xmm0, %xmm0
  incq  %rbx
  cmpq  %rax, %rbx
  jle   .L0
  ret
.L1:
  vxorpd %xmm0, %xmm0, %xmm0
  ret
|}, Arm64{|
M.f:
.L0:
  ldr   x1, [x0, #-8]
  ubfm  x1, x1, #56, #55
  ubfm  x1, x1, #18, #63
  ubfm  x2, x1, #63, #63
  movz  x3, #21846, lsl #0
  movk  x3, #21845, lsl #16
  movk  x3, #21845, lsl #32
  movk  x3, #21845, lsl #48
  smulh x1, x1, x3
  add   x1, x1, x2
  movn  x2, #0
  add   x1, x2, x1, lsl #1
  subs  xzr, x1, #1
  b.lt  .L2
  sbfm  x3, x1, #1, #63
  fmov  d0, xzr
  orr   x1, xzr, xzr
.L1:
  orr   x2, xzr, #1
  orr   x4, xzr, #3
  ubfm  x5, x1, #63, #62
  madd  x2, x5, x4, x2
  add   x2, x0, x2, lsl #2
  ldr   d1, [x2, #12]
  ldr   d2, [x2, #4]
  ldr   d3, [x2, #-4]
  fmul  d2, d3, d2
  fmadd d0, d2, d1, d0
  add   x1, x1, #1
  subs  xzr, x1, x3
  b.le  .L1
  ret
.L2:
  fmov  d0, xzr
  ret
.L3:
|}]


(* CR ttebbi: We should branch first and loop afterwards.*)
let loop_invariant_code (should_call_f : bool) (f : unit -> unit) =
  for _ = 0 to 9 do
    if should_call_f then f ()
  done
[%%expect_asm X86_64{|
loop_invariant_code:
  subq  $24, %rsp
  movq  %rax, (%rsp)
  movq  %rbx, 8(%rsp)
  xorl  %edi, %edi
  cmpq  $1, %rax
  je    .L3
  jmp   .L1
.L0:
  cmpq  $1, %rax
  je    .L3
.L1:
  movq  %rdi, 16(%rsp)
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L2:
  movq  (%rsp), %rax
  movq  8(%rsp), %rbx
  movq  16(%rsp), %rdi
.L3:
  incq  %rdi
  cmpq  $9, %rdi
  jle   .L0
  movl  $1, %eax
  addq  $24, %rsp
  ret
|}, Arm64{|
loop_invariant_code:
  sub   sp, sp, #32
  str   lr, [sp, #24]
  str   x0, [sp, #0]
  str   x1, [sp, #8]
  orr   x2, xzr, xzr
  subs  xzr, x0, #1
  b.eq  .L4
  b     .L1
.L0:
  subs  xzr, x0, #1
  b.eq  .L4
.L1:
.L2:
  str   x2, [sp, #16]
  orr   x0, xzr, #1
  ldr   x2, [x1, #0]
  blr   x2
.L3:
  ldr   x0, [sp, #0]
  ldr   x1, [sp, #8]
  ldr   x2, [sp, #16]
.L4:
  add   x2, x2, #1
  subs  xzr, x2, #9
  b.le  .L0
  orr   x0, xzr, #1
  ldr   lr, [sp, #24]
  add   sp, sp, #32
  ret
.L5:
|}]

(* CR ttebbi: The loop should have a fall-through check. *)
type t = { mutable next : t or_null }
let rec match_loop t = match t.next with
 | Null -> t
 | This l -> match_loop l
[%%expect_asm X86_64{|
match_loop:
.L0:
  movq  (%rax), %rbx
  testq %rbx, %rbx
  jne   .L1
  ret
.L1:
  movq  %rbx, %rax
  jmp   .L0
|}, Arm64{|
match_loop:
.L0:
.L1:
  ldr   x1, [x0, #0]
  subs  xzr, x1, #0
  b.ne  .L2
  ret
.L2:
  orr   x0, xzr, x1
  b     .L0
.L3:
|}]
