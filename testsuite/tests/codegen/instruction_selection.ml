(* TEST
  flags += " -O3 -extension-universe upstream_compatible";
  include stdlib_upstream_compatible;
  expect.opt;
*)

open Stdlib_upstream_compatible


(* CR ttebbi: The move instruction encoding with immediates is quite big (7-8 bytes).
    It would be better to put the constant into a register.
*)
type t = { mutable a : int ; mutable b : int ; mutable c : int ; mutable d : int }
let initialize_t t : unit =
  t.a <- 0;
  t.b <- 0;
  t.c <- 0;
  t.d <- 0;
;;
[%%expect_asm X86_64{|
initialize_t:
  movq  $1, (%rax)
  movq  $1, 8(%rax)
  movq  $1, 16(%rax)
  movq  $1, 24(%rax)
  movl  $1, %eax
  ret
|}]


(* CR ttebbi: We should use lea instead of add instructions to save moves. *)
let f x =
  let x1 = x + 1 in
  let x2 = x1 + x in
  let x3 = x1 + x2 in
  x + x3
;;
[%%expect_asm X86_64{|
f:
  movq  %rax, %rbx
  addq  $2, %rbx
  movq  %rbx, %rdi
  addq  %rax, %rdi
  addq  %rdi, %rbx
  leaq  -3(%rax,%rbx), %rax
  ret
|}]

(* CR ttebbi: We could merge the and and test instructions *)
let do_intersect t1 t2 = Int64_u.(if equal (logand t1 t2) #0L then #100L else #200L)
[%%expect_asm X86_64{|
do_intersect:
  andq  %rbx, %rax
  testq %rax, %rax
  jne   .L106
  movl  $100, %eax
  ret
.L106:
  movl  $200, %eax
  ret
|}]


(* CR ttebbi: We materialize comparison result bits despite only using them for a single
    branch. Also, the `_ -> 0` case is duplicated for no good reason. *)
let combine_comparisons r f =
  match !r > 5, !r < 20 with
  | true, true -> !r
  | _ -> 0
;;
[%%expect_asm X86_64{|
combine_comparisons:
  movq  (%rax), %rbx
  cmpq  $41, %rbx
  setl  %al
  movzbq %al, %rax
  cmpq  $11, %rbx
  jle   .L114
  testq %rax, %rax
  je    .L111
  movq  %rbx, %rax
  ret
.L111:
  movl  $1, %eax
  ret
.L114:
  movl  $1, %eax
  ret
|}]

(* CR ttebbi: We materialize the boolean needlessly. *)
let branch_and_return o =
  let cmp = o <> 0 in
  if cmp = true then o else 7
;;
[%%expect_asm X86_64{|
branch_and_return:
  movq  %rax, %rbx
  cmpq  $1, %rbx
  setne %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  cmpq  $3, %rax
  jne   .L107
  movq  %rbx, %rax
  ret
.L107:
  movl  $15, %eax
  ret
|}]

(* CR ttebbi: this could be a single lea instruction. *)
let mul_3 x = x * 3
[%%expect_asm X86_64{|
mul_3:
  imulq $3, %rax
  addq  $-2, %rax
  ret
|}]


(* CR ttebbi: If we change the register representation of 32bit values to be
    zero-extended, we could emit 32bit instructions saving 1 byte of instruction
    encoding and remove the sign extensions.
*)
let add32 x y = Int32_u.add x y
[%%expect_asm X86_64{|
add32:
  addq  %rbx, %rax
  movslq %eax, %rax
  ret
|}]

let min32 x y = Int32_u.min x y
[%%expect_asm X86_64{|
min32:
  movq  %rax, %rdi
  movq  %rbx, %rax
  cmpq  %rax, %rdi
  jg    .L105
  movq  %rdi, %rax
  ret
.L105:
  ret
|}]


(* CR ttebbi: `leaq  8(%r15), %rbx` could be merged with the subsequent addition. *)
let two_element_list x = [x; x]
[%%expect_asm X86_64{|
two_element_list:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $48, %r15
  cmpq  (%r14), %r15
  jb    .L105
.L107:
  leaq  8(%r15), %rdi
  addq  $24, %rdi
  movq  $2048, -8(%rdi)
  movq  %rbx, (%rdi)
  movq  $1, 8(%rdi)
  leaq  -24(%rdi), %rax
  movq  $2048, -8(%rax)
  movq  %rbx, (%rax)
  movq  %rdi, 8(%rax)
  addq  $8, %rsp
  ret
|}]


(* CR ttebbi: This could all be folded away. *)
let constant_folding (x: int) = if x < x then 1 + 2 else if x - x = 0 then 3 else 4
[%%expect_asm X86_64{|
constant_folding:
  cmpq  %rax, %rax
  jge   .L105
  movl  $7, %eax
  ret
.L105:
  subq  %rax, %rax
  incq  %rax
  cmpq  $1, %rax
  jne   .L111
  movl  $7, %eax
  ret
.L111:
  movl  $9, %eax
  ret
|}]
