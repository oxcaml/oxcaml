(* TEST
 flags += " -cfg-block-layout -dcfg-invariants";
 only-default-codegen;
 expect.opt;
*)

(* The bounds-check failure path of [a.(i)] is a cold continuation
   ([Cmm.is_cold]), so [Cfg_block_layout] sinks its block to the end of the
   function, after the hot loop-exit and empty-array return paths. *)

let sum_loop (a : int array) =
  let acc = ref 0 in
  for i = 0 to Array.length a - 1 do
    acc := !acc + a.(i)
  done;
  !acc
[%%expect_asm X86_64{|
sum_loop:
  movq  %rax, %rbx
  movq  -8(%rbx), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  orq   $1, %rdi
  leaq  -2(%rdi), %rsi
  cmpq  $1, %rsi
  jl    .L1
  sarq  $1, %rsi
  movl  $1, %eax
  xorl  %edx, %edx
.L0:
  leaq  1(%rdx,%rdx), %rcx
  cmpq  %rdi, %rcx
  jae   .L2
  movq  -4(%rbx,%rcx,4), %rcx
  leaq  -1(%rax,%rcx), %rax
  incq  %rdx
  cmpq  %rsi, %rdx
  jle   .L0
  ret
.L1:
  movl  $1, %eax
  ret
.L2:
  leaq  <hidden PC-relative offset>(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]

(* Same property for a straight-line function: the cold bounds-check failure
   block is sunk after the hot return path. *)

let get_mid (a : int array) i =
  let v = a.(i) in
  (v * 2) + 1
[%%expect_asm X86_64{|
get_mid:
  movq  -8(%rax), %rdi
  salq  $8, %rdi
  shrq  $17, %rdi
  cmpq  %rdi, %rbx
  jae   .L0
  movq  -4(%rax,%rbx,4), %rax
  leaq  1(%rax,%rax), %rax
  ret
.L0:
  leaq  <hidden PC-relative offset>(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
|}]
