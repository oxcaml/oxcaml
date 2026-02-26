(* TEST
 flags += " -O3";
 only-default-codegen;
 expect.opt;
*)

let[@inline never] inline_never _x = ()

(* CR ttebbi: missing tail-call optimization. *)
let f_unboxed_unit #() : unit# = inline_never(); #()
[%%expect_asm X86_64{|
f_unboxed_unit:
  subq  $8, %rsp
  movq  camlTOP2__f_unboxed_unit_3@GOTPCREL(%rip), %rax
  movq  16(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L107:
  addq  $8, %rsp
  ret
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
  movq  %rbx, %rdi
  subq  $56, %r15
  cmpq  (%r14), %r15
  jb    .L106
.L108:
  leaq  8(%r15), %rbx
  movq  $6391, -8(%rbx)
  movq  camlTOP3__g_6_9_code@GOTPCREL(%rip), %rsi
  movq  %rsi, (%rbx)
  movabsq $72057594037927949, %rsi
  movq  %rsi, 8(%rbx)
  movq  $3321, 16(%rbx)
  movq  camlTOP3__f_5_8_code@GOTPCREL(%rip), %rsi
  movq  %rsi, 24(%rbx)
  movabsq $108086391056891911, %rsi
  movq  %rsi, 32(%rbx)
  movq  %rdi, 40(%rbx)
  addq  $24, %rbx
  addq  $8, %rsp
  jmp   camlTOP3__f_5_8_code@PLT

mutual_recursion.f:
  movq  %rbx, %rdi
  cmpq  $1, %rax
  jge   .L114
  ret
.L114:
  movq  %rdi, %rbx
  addq  $-24, %rbx
  movq  16(%rdi), %rdi
  subq  %rdi, %rax
  incq  %rax
  jmp   camlTOP3__g_6_9_code@PLT

mutual_recursion.g:
  cmpq  $1, %rax
  jge   .L127
  ret
.L127:
  addq  $24, %rbx
  addq  $-60, %rax
  jmp   camlTOP3__f_5_8_code@PLT
|}]


(* CR ttebbi: This could be a loop to avoid closures. *)
let rec f x = (if x < 0 then (fun () -> f (x-100)) else (fun () -> x)) ()
[%%expect_asm X86_64{|
f.(fun):
  movq  16(%rbx), %rax
  addq  $-200, %rax
  jmp   camlTOP4__f_8_13_code@PLT

f.(fun):
  movq  16(%rbx), %rax
  ret

f:
  subq  $8, %rsp
  cmpq  $1, %rax
  jge   .L119
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    .L128
.L130:
  leaq  8(%r15), %rbx
  movq  $3319, -8(%rbx)
  leaq  .LcamlTOP4__fn$5b$3a1$2c29$2d$2d50$5d_10_15_code(%rip), %rdi
  movq  %rdi, (%rbx)
  movabsq $108086391056891911, %rdi
  movq  %rdi, 8(%rbx)
  movq  %rax, 16(%rbx)
  jmp   .L123
.L119:
  subq  $32, %r15
  cmpq  (%r14), %r15
  jb    .L131
.L133:
  leaq  8(%r15), %rbx
  movq  $3319, -8(%rbx)
  leaq  .LcamlTOP4__fn$5b$3a1$2c56$2d$2d69$5d_9_14_code(%rip), %rdi
  movq  %rdi, (%rbx)
  movabsq $108086391056891911, %rdi
  movq  %rdi, 8(%rbx)
  movq  %rax, 16(%rbx)
.L123:
  movl  $1, %eax
  movq  (%rbx), %rdi
  addq  $8, %rsp
  jmp   *%rdi
|}]


(* CR ttebbi: The two branches end up equivalent and could be merged. *)
let inline_identical x =
  let f () = x + 1 in
  if x > 0 then let _ = f() in f() else f()
[%%expect_asm X86_64{|
inline_identical:
  cmpq  $1, %rax
  jle   .L106
  addq  $2, %rax
  ret
.L106:
  addq  $2, %rax
  ret
|}]
