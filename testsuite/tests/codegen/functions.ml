(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

let[@inline never] inline_never _x = ()

(* CR ttebbi: missing tail-call optimization. *)
let f_unboxed_unit #() : unit# = inline_never(); #()
[%%expect_asm X86_64{|
f_unboxed_unit:
  subq  $8, %rsp
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L0:
  addq  $8, %rsp
  ret
|}, Arm64{|
f_unboxed_unit:
  sub   sp, sp, #16
  str   lr, [sp, #8]
  adrp  x0, <hidden PC-relative offset>
  ldr   x0, [x0, <hidden PC-relative offset>]
  ldr   x1, [x0, #16]
.L0:
  orr   x0, xzr, #1
  ldr   x2, [x1, #0]
  blr   x2
.L1:
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L2:
|}]


(* CR ttebbi: This could be a loop to avoid closures. *)
let mutual_recursion n m =
  let rec f x = if x < 0 then x else g (x - m)
  and g x = if x < 0 then x else f (x - 30)
  in f n
;;
[%%expect_asm X86_64{|
mutual_recursion:
  subq  $8, %rsp
  subq  $56, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rdi
  movq  $6391, -8(%rdi)
  movq  <hidden PC-relative offset>(%rip), %rsi
  movq  %rsi, (%rdi)
  movabsq $72057594037927949, %rsi
  movq  %rsi, 8(%rdi)
  movq  $3321, 16(%rdi)
  movq  <hidden PC-relative offset>(%rip), %rsi
  movq  %rsi, 24(%rdi)
  movabsq $108086391056891911, %rsi
  movq  %rsi, 32(%rdi)
  movq  %rbx, 40(%rdi)
  leaq  24(%rdi), %rbx
  addq  $8, %rsp
  jmp   camlTOP3__f_3_8_code@PLT

mutual_recursion.f:
  movq  %rbx, %rdi
  cmpq  $1, %rax
  jge   .L0
  ret
.L0:
  leaq  -24(%rdi), %rbx
  movq  16(%rdi), %rdi
  subq  %rdi, %rax
  incq  %rax
  jmp   camlTOP3__g_4_9_code@PLT

mutual_recursion.g:
  cmpq  $1, %rax
  jge   .L0
  ret
.L0:
  addq  $24, %rbx
  addq  $-60, %rax
  jmp   camlTOP3__f_3_8_code@PLT
|}, Arm64{|
mutual_recursion:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  ldr   x16, [x28, #0]
  sub   x27, x27, #56
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L1:
  add   x2, x27, #8
  movz  x3, #6391, lsl #0
  str   x3, [x2, #-8]
  adrp  x3, <hidden PC-relative offset>
  ldr   x3, [x3, <hidden PC-relative offset>]
  str   x3, [x2, #0]
  movz  x3, #13, lsl #0
  movk  x3, #256, lsl #48
  str   x3, [x2, #8]
  movz  x3, #3321, lsl #0
  str   x3, [x2, #16]
  adrp  x3, <hidden PC-relative offset>
  ldr   x3, [x3, <hidden PC-relative offset>]
  str   x3, [x2, #24]
  movz  x3, #7, lsl #0
  movk  x3, #384, lsl #48
  str   x3, [x2, #32]
  str   x1, [x2, #40]
  add   x1, x2, #24
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  b     camlTOP3__f_3_8_code
.L2:

mutual_recursion.f:
  orr   x2, xzr, x1
.L0:
  subs  xzr, x0, #1
  b.ge  .L1
  ret
.L1:
  sub   x1, x2, #24
  ldr   x2, [x2, #16]
  sub   x0, x0, x2
  add   x0, x0, #1
  b     camlTOP3__g_4_9_code
.L2:

mutual_recursion.g:
.L0:
  subs  xzr, x0, #1
  b.ge  .L1
  ret
.L1:
  add   x1, x1, #24
  sub   x0, x0, #60
  b     camlTOP3__f_3_8_code
.L2:
|}]


(* CR ttebbi: This could be a loop to avoid closures. *)
let rec f x = (if x < 0 then (fun () -> f (x-100)) else (fun () -> x)) ()
[%%expect_asm X86_64{|
f.(fun):
  movq  16(%rbx), %rax
  addq  $-200, %rax
  jmp   camlTOP4__f_5_13_code@PLT

f.(fun):
  movq  16(%rbx), %rax
  ret

f:
  subq  $8, %rsp
  cmpq  $1, %rax
  jge   .L1
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $3319, -8(%rbx)
  leaq  <hidden PC-relative offset>(%rip), %rdi
  movq  %rdi, (%rbx)
  movabsq $108086391056891911, %rdi
  movq  %rdi, 8(%rbx)
  movq  %rax, 16(%rbx)
  jmp   .L3
.L1:
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rbx
  movq  $3319, -8(%rbx)
  leaq  <hidden PC-relative offset>(%rip), %rdi
  movq  %rdi, (%rbx)
  movabsq $108086391056891911, %rdi
  movq  %rdi, 8(%rbx)
  movq  %rax, 16(%rbx)
.L3:
  movl  $1, %eax
  movq  (%rbx), %rdi
  addq  $8, %rsp
  jmp   *%rdi
|}, Arm64{|
f.(fun):
  ldr   x0, [x1, #16]
.L0:
  sub   x0, x0, #200
  b     camlTOP4__f_5_13_code
.L1:

f.(fun):
  ldr   x0, [x1, #16]
  ret

f:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  subs  xzr, x0, #1
  b.ge  .L2
  ldr   x16, [x28, #0]
  sub   x27, x27, #32
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L1:
  add   x1, x27, #8
  movz  x2, #3319, lsl #0
  str   x2, [x1, #-8]
  adrp  x2, <hidden PC-relative offset>
  add   x2, x2, <hidden PC-relative offset>
  str   x2, [x1, #0]
  movz  x2, #7, lsl #0
  movk  x2, #384, lsl #48
  str   x2, [x1, #8]
  str   x0, [x1, #16]
  b     .L4
.L2:
  ldr   x16, [x28, #0]
  sub   x27, x27, #32
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L3:
  add   x1, x27, #8
  movz  x2, #3319, lsl #0
  str   x2, [x1, #-8]
  adrp  x2, <hidden PC-relative offset>
  add   x2, x2, <hidden PC-relative offset>
  str   x2, [x1, #0]
  movz  x2, #7, lsl #0
  movk  x2, #384, lsl #48
  str   x2, [x1, #8]
  str   x0, [x1, #16]
.L4:
  orr   x0, xzr, #1
  ldr   x2, [x1, #0]
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  br    x2
.L5:
|}]


let inline_identical x =
  let f () = x + 1 in
  if x > 0 then let _ = f() in f() else f()
[%%expect_asm X86_64{|
inline_identical:
  addq  $2, %rax
  ret
|}, Arm64{|
inline_identical:
.L0:
  add   x0, x0, #2
  ret
.L1:
|}]


(* CR ttebbi: The rsp adjustment could be done in the GC trampoline. *)
let just_one_allocation (x: int) : int option = Some x
[%%expect_asm X86_64{|
just_one_allocation:
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rbx
  movq  $1024, -8(%rbx)
  movq  %rax, (%rbx)
  movq  %rbx, %rax
  addq  $8, %rsp
  ret
|}, Arm64{|
just_one_allocation:
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  ldr   x16, [x28, #0]
  sub   x27, x27, #16
  subs  xzr, x27, x16
  b.cc  <hidden GC jump pad>
.L1:
  add   x1, x27, #8
  orr   x2, xzr, #1024
  str   x2, [x1, #-8]
  str   x0, [x1, #0]
  orr   x0, xzr, x1
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L2:
|}]


let catch_exit =
  let[@inline never] exit () = raise Exit in
  fun () -> try exit () with _ -> ()
[%%expect_asm X86_64{|
catch_exit.exit:
  subq  $8, %rsp
  movq  camlStdlib__Exit1460_42@GOTPCREL(%rip), %rax
  call  caml_reraise_exn@PLT
.L0:

catch_exit.(fun):
  subq  $8, %rsp
  movq  64(%r14), %rax
  movq  %rax, (%rsp)
.L0:
  leaq  <hidden PC-relative offset>(%rip), %r11
  pushq %r11
  pushq 48(%r14)
  movq  %rsp, 48(%r14)
  movl  $1, %eax
  call  camlTOP7__exit_11_24_code@PLT
.L1:
.L2:
  movq  (%rsp), %rax
  movq  %rax, 64(%r14)
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}, Arm64{|
catch_exit.exit:
  sub   sp, sp, #16
  str   lr, [sp, #8]
  adrp  x0, :got:camlStdlib__Exit1460_42
  ldr   x0, [x0, :got_lo12:camlStdlib__Exit1460_42]
.L0:
  bl    caml_reraise_exn
.L1:
.L2:

catch_exit.(fun):
  sub   sp, sp, #16
  str   lr, [sp, #8]
  ldr   x0, [x28, #64]
  str   x0, [sp, #0]
  adr   x16, .L2
  stp   x26, x16, [sp, #-16]!
  add   x26, sp, #0
.L0:
  orr   x0, xzr, #1
  bl    camlTOP7__exit_11_24_code
.L1:
.L2:
  ldr   x0, [sp, #0]
  str   x0, [x28, #64]
  orr   x0, xzr, #1
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L3:
|}]

let nontail_exit =
  let[@inline never] exit () = raise Exit in
  fun () -> exit () [@nontail]
[%%expect_asm X86_64{|
nontail_exit.exit:
  subq  $8, %rsp
  movq  camlStdlib__Exit1460_42@GOTPCREL(%rip), %rax
  call  caml_reraise_exn@PLT
.L0:

nontail_exit.(fun):
  subq  $8, %rsp
  movl  $1, %eax
  call  camlTOP8__exit_13_28_code@PLT
.L0:
|}, Arm64{|
nontail_exit.exit:
  sub   sp, sp, #16
  str   lr, [sp, #8]
  adrp  x0, :got:camlStdlib__Exit1460_42
  ldr   x0, [x0, :got_lo12:camlStdlib__Exit1460_42]
.L0:
  bl    caml_reraise_exn
.L1:
.L2:

nontail_exit.(fun):
  sub   sp, sp, #16
  str   lr, [sp, #8]
.L0:
  orr   x0, xzr, #1
  bl    camlTOP8__exit_13_28_code
.L1:
.L2:
|}]

type p = #(int * int * int * int * int)
type q = #(p * p * p * p * p)

let tailcall_nonreturning_large_result =
  let[@inline never] rec loop () : #(q * q * q) = loop () in
  fun () -> loop ()
[%%expect_asm X86_64{|
tailcall_nonreturning_large_result.loop:
.L0:
  jmp   .L0

tailcall_nonreturning_large_result.(fun):
  movl  $1, %eax
  jmp   camlTOP11__loop_15_32_code@PLT
|}, Arm64{|
tailcall_nonreturning_large_result.loop:
.L0:
  b     .L0

tailcall_nonreturning_large_result.(fun):
.L0:
  orr   x0, xzr, #1
  b     camlTOP11__loop_15_32_code
.L1:
|}]
