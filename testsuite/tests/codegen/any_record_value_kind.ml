(* TEST
 flags += " -O3 -extension layouts_beta";
 only-default-codegen;
 flat-float-array;
 expect.opt;
*)

type ('a : any) t = { field : 'a }

external box_float : float# -> float = "%box_float"
external is_int : 'a -> bool = "%obj_is_int"

let record_is_int (x : _ t) = is_int x
[%%expect_asm X86_64{|
record_is_int:
  andl  $1, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

type ('a : any) variant = A of 'a | B of int

let variant_is_int (x : _ variant) = is_int x
[%%expect_asm X86_64{|
variant_is_int:
  andl  $1, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

type ('a : any) option = None | Some of 'a

let option_is_int (x : _ option) = is_int x
[%%expect_asm X86_64{|
option_is_int:
  andl  $1, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let rebuild_int (src : int t) : int t = { field = src.field }
[%%expect_asm X86_64{|
rebuild_int:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1024, -8(%rax)
  movq  (%rbx), %rbx
  movq  %rbx, (%rax)
  addq  $8, %rsp
  ret
|}]

let rebuild_float (src : float# t) : float# t = { field = src.field }
[%%expect_asm X86_64{|
rebuild_float:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movabsq $72057594037928960, %rdi
  movq  %rdi, -8(%rax)
  vmovsd (%rbx), %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
|}]

(* Keep the array access polymorphic until inlining. *)
let first (r : int array t) =
  let[@inline always] first (a : _ array) = Array.unsafe_get a 0 in
  first r.field
[%%expect_asm X86_64{|
first:
  movq  (%rax), %rbx
  movzbq -8(%rbx), %rax
  cmpq  $254, %rax
  jne   .L1
  subq  $8, %rsp
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $1277, -8(%rax)
  vmovsd (%rbx), %xmm0
  vmovsd %xmm0, (%rax)
  addq  $8, %rsp
  ret
.L1:
  movq  (%rbx), %rax
  ret
|}]

let () =
  assert (not (record_is_int { field = #42.0 }));
  assert (not (variant_is_int (A #42.0)));
  assert (not (variant_is_int (B 42)));
  assert (option_is_int None);
  assert (not (option_is_int (Some #42.0)));
  assert ((rebuild_int { field = 42 }).field = 42);
  assert (box_float (rebuild_float { field = #42.0 }).field = 42.0);
  assert (first { field = [| 42 |] } = 42)
[%%expect{|
|}]
