(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

open Intrinsics

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
  leaq  2(%rbx), %rsi
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


let loop_readonly_use_spilled_var n =
  let[@inline never] g x y = x - 1 in
  let rec loop x =
      if x < 0 then n + x else loop (g x n)
  in
  loop n
;;
[%%expect_asm X86_64{|
loop_readonly_use_spilled_var:
  subq  $8, %rsp
  movq  %rax, %rbx
  movq  %rbx, (%rsp)
  movq  %rbx, %rax
  cmpq  $1, %rax
  jge   .L111
.L108:
  leaq  -1(%rbx,%rax), %rax
  addq  $8, %rsp
  ret
.L111:
  call  camlTOP8__g_15_18_code@PLT
.L117:
  movq  (%rsp), %rbx
  cmpq  $1, %rax
  jge   .L111
  jmp   .L108

loop_readonly_use_spilled_var.g:
  addq  $-2, %rax
  ret
|}]

let spill_unspill_loop_movement not_used_in_loop read_in_loop =
  let[@inline never] f x = x in
  let written_in_loop = ref 0 in
  for i = 1 to read_in_loop do
    written_in_loop := f read_in_loop;
    if i > 5 then
      (let _ =  f read_in_loop in ())
  done;
  not_used_in_loop + !written_in_loop
[%%expect_asm X86_64{|
spill_unspill_loop_movement:
  subq  $40, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  $3, %rax
  jl    .L135
  movq  %rax, (%rsp)
  movq  %rdi, 24(%rsp)
  movq  %rax, %rbx
  sarq  $1, %rbx
  movq  %rbx, 8(%rsp)
  movl  $1, %edi
.L112:
  movq  %rdi, 16(%rsp)
  call  camlTOP9__f_20_23_code@PLT
.L145:
  movq  %rax, %rsi
  movq  16(%rsp), %rdi
  movq  %rdi, %rdx
  salq  $1, %rdx
  movq  (%rsp), %rax
  movq  8(%rsp), %rbx
  cmpq  $11, %rdx
  jle   .L125
  movq  %rsi, 32(%rsp)
  movq  %rdi, 16(%rsp)
  call  camlTOP9__f_20_23_code@PLT
.L146:
  movq  (%rsp), %rax
  movq  8(%rsp), %rbx
  movq  16(%rsp), %rdi
  movq  32(%rsp), %rsi
.L125:
  incq  %rdi
  cmpq  %rbx, %rdi
  jle   .L112
  movq  24(%rsp), %rdi
  jmp   .L138
.L135:
  movl  $1, %esi
.L138:
  leaq  -1(%rdi,%rsi), %rax
  addq  $40, %rsp
  ret

spill_unspill_loop_movement.f:
  ret
|}]

let f a b c = if a > 10 then b - c else a - b
[%%expect_asm X86_64{|
f:
  cmpq  $21, %rax
  jle   .L107
  subq  %rdi, %rbx
  leaq  1(%rbx), %rax
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


(* This triggers a rare path where the closure register spill is hoisted
   out of the loop but also pushed into the loop, and then a spill-unspill
   pair gets eliminated, resulting in a spill without explicit unspill.
*)
let double_loop_no_definition_at_beginning array n list =
  let rec iter f = function
    [] -> ()
    | a::l -> f a; iter f l
  in
  for i = 0 to n do
    let[@inline never] f x = array.(x) <- i in
    iter f list;
  done
[%%expect_asm X86_64{|
double_loop_no_definition_at_beginning:
  subq  $72, %rsp
  movq  %rbx, %rsi
  movq  64(%r14), %rbx
  cmpq  $1, %rsi
  jl    .L149
  movq  %rbx, 16(%rsp)
  movq  %rdi, 32(%rsp)
  movq  %rax, 24(%rsp)
  sarq  $1, %rsi
  movq  %rsi, 40(%rsp)
  xorl  %edx, %edx
.L113:
  movq  %rdx, (%rsp)
  movq  64(%r14), %rbx
  movq  %rbx, 8(%rsp)
  movq  64(%r14), %rbx
  subq  $40, %rbx
  movq  %rbx, 64(%r14)
  cmpq  80(%r14), %rbx
  jl    .L156
.L157:
  addq  72(%r14), %rbx
  addq  $8, %rbx
  movq  $5111, -8(%rbx)
  movq  camlTOP15__f_33_37_code@GOTPCREL(%rip), %rcx
  movq  %rcx, (%rbx)
  movabsq $108086391056891911, %rcx
  movq  %rcx, 8(%rbx)
  leaq  1(%rdx,%rdx), %rdx
  movq  %rdx, 16(%rbx)
  movq  %rax, 24(%rbx)
  movq  %rbx, 48(%rsp)
  movq  %rdi, %rdx
  testb $1, %dl
  jne   .L135
.L128:
  movq  %rdx, 56(%rsp)
  movq  (%rdx), %rax
  call  camlTOP15__f_33_37_code@PLT
.L158:
  movq  56(%rsp), %rdx
  movq  8(%rdx), %rdx
  movq  24(%rsp), %rax
  movq  32(%rsp), %rdi
  movq  40(%rsp), %rsi
  movq  48(%rsp), %rbx
  testb $1, %dl
  je    .L128
.L135:
  movq  8(%rsp), %rbx
  movq  %rbx, 64(%r14)
  movq  (%rsp), %rdx
  incq  %rdx
  cmpq  %rsi, %rdx
  jle   .L113
  movq  16(%rsp), %rbx
.L149:
  movq  %rbx, 64(%r14)
  movl  $1, %eax
  addq  $72, %rsp
  ret

double_loop_no_definition_at_beginning.f:
  movq  24(%rbx), %rsi
  movq  -8(%rsi), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rax
  jae   .L177
  movq  16(%rbx), %rbx
  movq  %rbx, -4(%rsi,%rax,4)
  movl  $1, %eax
  ret
.L177:
  movq  camlTOP15__block741@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]


(* CR ttebbi: https://github.com/oxcaml/oxcaml/issues/5115 *)
let spilled_phi_merge cond callback f a b c d e =
  let _ : _ = Stdlib.Sys.time () in
  if cond then callback ();
  f a b c d e
[%%expect_asm X86_64{|
spilled_phi_merge:
  subq  $56, %rsp
  movq  %rax, 8(%rsp)
  movq  %rbx, 48(%rsp)
  movq  %rdi, (%rsp)
  movq  %rsi, %rbp
  movq  %rdx, %rbx
  movq  %rcx, 24(%rsp)
  movq  %r8, %r12
  movq  %r9, %r13
  movl  $1, %edi
  call  caml_sys_time_unboxed@PLT
  movq  8(%rsp), %rax
  cmpq  $1, %rax
  je    .L114
  movq  %r13, 40(%rsp)
  movq  %r12, 32(%rsp)
  movq  24(%rsp), %rax
  movq  %rbx, 16(%rsp)
  movq  %rbp, 8(%rsp)
  movq  (%rsp), %rax
  movl  $1, %eax
  movq  48(%rsp), %rbx
  movq  (%rbx), %rdi
  movq  48(%rsp), %rbx
  call  *%rdi
.L120:
  movq  (%rsp), %rsi
  movq  8(%rsp), %rbp
  movq  16(%rsp), %rbx
  movq  24(%rsp), %rdx
  movq  32(%rsp), %rax
  movq  40(%rsp), %rdi
  movq  %rsi, (%rsp)
  movq  %rdx, 24(%rsp)
  movq  %rax, %r12
  movq  %rdi, %r13
.L114:
  movq  %rbp, %rax
  movq  24(%rsp), %rdi
  movq  %r12, %rsi
  movq  %r13, %rdx
  movq  (%rsp), %rcx
  addq  $56, %rsp
  jmp   caml_apply5@PLT
|}]


(* CR ttebbi: https://github.com/oxcaml/oxcaml/issues/2441 *)
let spill_slot_lifetime () =
  let[@inline never] get_one () = #1. in
  let acc = #0. in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  let one = get_one () in
  let acc = Float_u.add acc one in
  acc
[%%expect_asm X86_64{|
spill_slot_lifetime:
  subq  $56, %rsp
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L127:
  vmovsd %xmm0, (%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L128:
  vmovsd %xmm0, 8(%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L129:
  vmovsd %xmm0, 16(%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L130:
  vmovsd %xmm0, 24(%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L131:
  vmovsd %xmm0, 32(%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L132:
  vmovsd %xmm0, 40(%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L133:
  vmovsd %xmm0, 48(%rsp)
  movl  $1, %eax
  call  camlTOP17__get_one_39_43_code@PLT
.L134:
  vxorpd %xmm1, %xmm1, %xmm1
  vmovsd (%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vmovsd 8(%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vmovsd 16(%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vmovsd 24(%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vmovsd 32(%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vmovsd 40(%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vmovsd 48(%rsp), %xmm2
  vaddsd %xmm2, %xmm1, %xmm1
  vaddsd %xmm0, %xmm1, %xmm0
  addq  $56, %rsp
  ret

spill_slot_lifetime.get_one:
  vmovsd .L138(%rip), %xmm0
  ret
|}]


(* CR ttebbi: https://github.com/oxcaml/oxcaml/issues/2288 *)
let f ~(s: int64#) (t : int64#) =
  Int64_u.sub t (Int64_u.mul t s)
[%%expect_asm X86_64{|
f:
  movq  %rax, %rdi
  movq  %rbx, %rax
  imulq %rdi, %rbx
  subq  %rbx, %rax
  ret
|}]
