(* TEST
 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 expect.opt;
*)

type 'a r =
  | A of 'a

type 'a s =
  | A of 'a
  | B

(* This demonstrates that a new block is not allocated in the [A] case. *)
let f1 : 'a s -> 'a r or_null = function
  | B -> Null
  | A x -> This (A x)

[%%expect_asm X86_64{|
f1:
  testb $1, %al
  je    .L105
  xorl  %eax, %eax
  ret
.L105:
  ret
|}]

(* We would like [f2] also to not allocate a new block, analogously to
   [f1] above.  The "fully constructed heap block" optimization that allows
   this does not work unless the block in question is known to be heap
   allocated, i.e. definitively not locally allocated.  In [f1] that locality
   information comes from the parameter of [f1], but here it is obtained
   from the return mode of the application of [f]. *)
let f2 : (unit -> 'a s) -> 'a r or_null = fun f ->
  match f () with
  | B -> Null
  | A x -> This (A x)

[%%expect_asm X86_64{|
f2:
  subq  $8, %rsp
  movq  %rax, %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  call  *%rdi
.L111:
  testb $1, %al
  je    .L107
  xorl  %eax, %eax
  addq  $8, %rsp
  ret
.L107:
  addq  $8, %rsp
  ret
|}]
