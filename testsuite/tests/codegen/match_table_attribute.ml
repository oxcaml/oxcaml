(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 expect.opt;
*)

(* The [@table] attribute forces the dispatch of a match through a (dense)
   jump table, where the default heuristics would build a tree of
   comparisons. *)

let with_table x =
  match[@table] x with
  | 0 -> 10
  | 7 -> 20
  | 15 -> 30
  | 23 -> 40
  | 31 -> 50
  | _ -> 0
[%%expect_asm X86_64{|
with_table:
  cmpq  $63, %rax
  jbe   .L0
  movl  $1, %eax
  ret
.L0:
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]

(* Same match without the attribute: the sparse constants are dispatched
   through comparisons. *)
let without_table x =
  match x with
  | 0 -> 10
  | 7 -> 20
  | 15 -> 30
  | 23 -> 40
  | 31 -> 50
  | _ -> 0
[%%expect_asm X86_64{|
without_table:
  cmpq  $31, %rax
  jl    .L2
  cmpq  $47, %rax
  je    .L1
  cmpq  $63, %rax
  je    .L0
  cmpq  $33, %rax
  jge   .L3
  movl  $61, %eax
  ret
.L0:
  movl  $101, %eax
  ret
.L1:
  movl  $81, %eax
  ret
.L2:
  cmpq  $1, %rax
  je    .L5
  cmpq  $15, %rax
  je    .L4
.L3:
  movl  $1, %eax
  ret
.L4:
  movl  $41, %eax
  ret
.L5:
  movl  $21, %eax
  ret
|}]
