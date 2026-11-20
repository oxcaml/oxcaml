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
|}]


let inline_identical x =
  let f () = x + 1 in
  if x > 0 then let _ = f() in f() else f()
[%%expect_asm X86_64{|
inline_identical:
  addq  $2, %rax
  ret
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
  popq  48(%r14)
  addq  $8, %rsp
  addq  $8, %rsp
  ret
.L3:
  movq  (%rsp), %rax
  movq  %rax, 64(%r14)
  movl  $1, %eax
  addq  $8, %rsp
  ret
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
  addq  $8, %rsp
  ret
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
  subq  $504, %rsp
  movl  $1, %eax
  subq  $16, %rsp
  call  camlTOP11__loop_15_32_code@PLT
.L0:
  movq  632(%r14), %r10
  movq  %r10, 16(%rsp)
  movq  640(%r14), %r10
  movq  %r10, 24(%rsp)
  movq  648(%r14), %r10
  movq  %r10, 32(%rsp)
  movq  656(%r14), %r10
  movq  %r10, 40(%rsp)
  movq  664(%r14), %r10
  movq  %r10, 48(%rsp)
  movq  672(%r14), %r10
  movq  %r10, 56(%rsp)
  movq  680(%r14), %r10
  movq  %r10, 64(%rsp)
  movq  688(%r14), %r10
  movq  %r10, 72(%rsp)
  movq  696(%r14), %r10
  movq  %r10, 80(%rsp)
  movq  704(%r14), %r10
  movq  %r10, 88(%rsp)
  movq  712(%r14), %r10
  movq  %r10, 96(%rsp)
  movq  720(%r14), %r10
  movq  %r10, 104(%rsp)
  movq  728(%r14), %r10
  movq  %r10, 112(%rsp)
  movq  736(%r14), %r10
  movq  %r10, 120(%rsp)
  movq  744(%r14), %r10
  movq  %r10, 128(%rsp)
  movq  752(%r14), %r10
  movq  %r10, 136(%rsp)
  movq  760(%r14), %r10
  movq  %r10, 144(%rsp)
  movq  768(%r14), %r10
  movq  %r10, 152(%rsp)
  movq  776(%r14), %r10
  movq  %r10, 160(%rsp)
  movq  784(%r14), %r10
  movq  %r10, 168(%rsp)
  movq  792(%r14), %r10
  movq  %r10, 176(%rsp)
  movq  800(%r14), %r10
  movq  %r10, 184(%rsp)
  movq  808(%r14), %r10
  movq  %r10, 192(%rsp)
  movq  816(%r14), %r10
  movq  %r10, 200(%rsp)
  movq  824(%r14), %r10
  movq  %r10, 208(%rsp)
  movq  832(%r14), %r10
  movq  %r10, 216(%rsp)
  movq  840(%r14), %r10
  movq  %r10, 224(%rsp)
  movq  848(%r14), %r10
  movq  %r10, 232(%rsp)
  movq  856(%r14), %r10
  movq  %r10, 240(%rsp)
  movq  864(%r14), %r10
  movq  %r10, 248(%rsp)
  movq  872(%r14), %r10
  movq  %r10, 256(%rsp)
  movq  880(%r14), %r10
  movq  %r10, 264(%rsp)
  movq  888(%r14), %r10
  movq  %r10, 272(%rsp)
  movq  896(%r14), %r10
  movq  %r10, 280(%rsp)
  movq  904(%r14), %r10
  movq  %r10, 288(%rsp)
  movq  912(%r14), %r10
  movq  %r10, 296(%rsp)
  movq  920(%r14), %r10
  movq  %r10, 304(%rsp)
  movq  928(%r14), %r10
  movq  %r10, 312(%rsp)
  movq  936(%r14), %r10
  movq  %r10, 320(%rsp)
  movq  944(%r14), %r10
  movq  %r10, 328(%rsp)
  movq  952(%r14), %r10
  movq  %r10, 336(%rsp)
  movq  960(%r14), %r10
  movq  %r10, 344(%rsp)
  movq  968(%r14), %r10
  movq  %r10, 352(%rsp)
  movq  976(%r14), %r10
  movq  %r10, 360(%rsp)
  movq  984(%r14), %r10
  movq  %r10, 368(%rsp)
  movq  992(%r14), %r10
  movq  %r10, 376(%rsp)
  movq  1000(%r14), %r10
  movq  %r10, 384(%rsp)
  movq  1008(%r14), %r10
  movq  %r10, 392(%rsp)
  movq  1016(%r14), %r10
  movq  %r10, 400(%rsp)
  movq  1024(%r14), %r10
  movq  %r10, 408(%rsp)
  movq  1032(%r14), %r10
  movq  %r10, 416(%rsp)
  movq  1040(%r14), %r10
  movq  %r10, 424(%rsp)
  movq  1048(%r14), %r10
  movq  %r10, 432(%rsp)
  movq  1056(%r14), %r10
  movq  %r10, 440(%rsp)
  movq  1064(%r14), %r10
  movq  %r10, 448(%rsp)
  movq  1072(%r14), %r10
  movq  %r10, 456(%rsp)
  movq  1080(%r14), %r10
  movq  %r10, 464(%rsp)
  movq  1088(%r14), %r10
  movq  %r10, 472(%rsp)
  movq  1096(%r14), %r10
  movq  %r10, 480(%rsp)
  movq  1104(%r14), %r10
  movq  %r10, 488(%rsp)
  movq  1112(%r14), %r10
  movq  %r10, 496(%rsp)
  movq  1120(%r14), %r10
  movq  %r10, 504(%rsp)
  movq  1128(%r14), %r10
  movq  %r10, 512(%rsp)
  movq  1136(%r14), %r10
  movq  (%rsp), %r11
  addq  $16, %rsp
  movq  (%rsp), %rbp
  movq  %rbp, 632(%r14)
  movq  8(%rsp), %rbp
  movq  %rbp, 640(%r14)
  movq  16(%rsp), %rbp
  movq  %rbp, 648(%r14)
  movq  24(%rsp), %rbp
  movq  %rbp, 656(%r14)
  movq  32(%rsp), %rbp
  movq  %rbp, 664(%r14)
  movq  40(%rsp), %rbp
  movq  %rbp, 672(%r14)
  movq  48(%rsp), %rbp
  movq  %rbp, 680(%r14)
  movq  56(%rsp), %rbp
  movq  %rbp, 688(%r14)
  movq  64(%rsp), %rbp
  movq  %rbp, 696(%r14)
  movq  72(%rsp), %rbp
  movq  %rbp, 704(%r14)
  movq  80(%rsp), %rbp
  movq  %rbp, 712(%r14)
  movq  88(%rsp), %rbp
  movq  %rbp, 720(%r14)
  movq  96(%rsp), %rbp
  movq  %rbp, 728(%r14)
  movq  104(%rsp), %rbp
  movq  %rbp, 736(%r14)
  movq  112(%rsp), %rbp
  movq  %rbp, 744(%r14)
  movq  120(%rsp), %rbp
  movq  %rbp, 752(%r14)
  movq  128(%rsp), %rbp
  movq  %rbp, 760(%r14)
  movq  136(%rsp), %rbp
  movq  %rbp, 768(%r14)
  movq  144(%rsp), %rbp
  movq  %rbp, 776(%r14)
  movq  152(%rsp), %rbp
  movq  %rbp, 784(%r14)
  movq  160(%rsp), %rbp
  movq  %rbp, 792(%r14)
  movq  168(%rsp), %rbp
  movq  %rbp, 800(%r14)
  movq  176(%rsp), %rbp
  movq  %rbp, 808(%r14)
  movq  184(%rsp), %rbp
  movq  %rbp, 816(%r14)
  movq  192(%rsp), %rbp
  movq  %rbp, 824(%r14)
  movq  200(%rsp), %rbp
  movq  %rbp, 832(%r14)
  movq  208(%rsp), %rbp
  movq  %rbp, 840(%r14)
  movq  216(%rsp), %rbp
  movq  %rbp, 848(%r14)
  movq  224(%rsp), %rbp
  movq  %rbp, 856(%r14)
  movq  232(%rsp), %rbp
  movq  %rbp, 864(%r14)
  movq  240(%rsp), %rbp
  movq  %rbp, 872(%r14)
  movq  248(%rsp), %rbp
  movq  %rbp, 880(%r14)
  movq  256(%rsp), %rbp
  movq  %rbp, 888(%r14)
  movq  264(%rsp), %rbp
  movq  %rbp, 896(%r14)
  movq  272(%rsp), %rbp
  movq  %rbp, 904(%r14)
  movq  280(%rsp), %rbp
  movq  %rbp, 912(%r14)
  movq  288(%rsp), %rbp
  movq  %rbp, 920(%r14)
  movq  296(%rsp), %rbp
  movq  %rbp, 928(%r14)
  movq  304(%rsp), %rbp
  movq  %rbp, 936(%r14)
  movq  312(%rsp), %rbp
  movq  %rbp, 944(%r14)
  movq  320(%rsp), %rbp
  movq  %rbp, 952(%r14)
  movq  328(%rsp), %rbp
  movq  %rbp, 960(%r14)
  movq  336(%rsp), %rbp
  movq  %rbp, 968(%r14)
  movq  344(%rsp), %rbp
  movq  %rbp, 976(%r14)
  movq  352(%rsp), %rbp
  movq  %rbp, 984(%r14)
  movq  360(%rsp), %rbp
  movq  %rbp, 992(%r14)
  movq  368(%rsp), %rbp
  movq  %rbp, 1000(%r14)
  movq  376(%rsp), %rbp
  movq  %rbp, 1008(%r14)
  movq  384(%rsp), %rbp
  movq  %rbp, 1016(%r14)
  movq  392(%rsp), %rbp
  movq  %rbp, 1024(%r14)
  movq  400(%rsp), %rbp
  movq  %rbp, 1032(%r14)
  movq  408(%rsp), %rbp
  movq  %rbp, 1040(%r14)
  movq  416(%rsp), %rbp
  movq  %rbp, 1048(%r14)
  movq  424(%rsp), %rbp
  movq  %rbp, 1056(%r14)
  movq  432(%rsp), %rbp
  movq  %rbp, 1064(%r14)
  movq  440(%rsp), %rbp
  movq  %rbp, 1072(%r14)
  movq  448(%rsp), %rbp
  movq  %rbp, 1080(%r14)
  movq  456(%rsp), %rbp
  movq  %rbp, 1088(%r14)
  movq  464(%rsp), %rbp
  movq  %rbp, 1096(%r14)
  movq  472(%rsp), %rbp
  movq  %rbp, 1104(%r14)
  movq  480(%rsp), %rbp
  movq  %rbp, 1112(%r14)
  movq  488(%rsp), %rbp
  movq  %rbp, 1120(%r14)
  movq  496(%rsp), %rbp
  movq  %rbp, 1128(%r14)
  movq  %r10, 1136(%r14)
  movq  %r11, 512(%rsp)
  addq  $504, %rsp
  ret
|}]
