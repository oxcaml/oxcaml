(* TEST
 flags += " -O3";
 flags += " -extension layouts_beta";
 only-default-codegen;
 expect.opt;
*)

(* Matches over dense unboxed-integer constants must compile to the same
   table dispatch as matches over tagged integers, modulo a range test at
   the scrutinee's own width when the scrutinee is wider than a tagged
   integer, and modulo tagging the scrutinee when it is narrower. *)

let f = function
  | 0 -> #0.
  | 1 -> #1.
  | 2 -> #2.
  | 3 -> #3.
  | _ -> #4.

[%%expect_asm X86_64{|
f:
  cmpq  $7, %rax
  jbe   .L0
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L0:
  movq  <hidden PC-relative offset>(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]

let g = function
  | #0n -> #0.
  | #1n -> #1.
  | #2n -> #2.
  | #3n -> #3.
  | _ -> #4.

[%%expect_asm X86_64{|
g:
  cmpq  $0, %rax
  jge   .L0
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L0:
  cmpq  $3, %rax
  jle   .L1
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L1:
  leaq  1(%rax,%rax), %rax
  movq  <hidden PC-relative offset>(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]

let h = function
  | 'a' -> #0.
  | 'b' -> #1.
  | 'c' -> #2.
  | 'd' -> #3.
  | _ -> #4.

[%%expect_asm X86_64{|
h:
  addq  $-194, %rax
  cmpq  $7, %rax
  jbe   .L0
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L0:
  movq  <hidden PC-relative offset>(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]

let k = function
  | #'a' -> #0.
  | #'b' -> #1.
  | #'c' -> #2.
  | #'d' -> #3.
  | _ -> #4.

[%%expect_asm X86_64{|
k:
  leaq  -193(%rax,%rax), %rax
  cmpq  $7, %rax
  jbe   .L0
  vmovsd <hidden PC-relative offset>(%rip), %xmm0
  ret
.L0:
  movq  <hidden PC-relative offset>(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]
