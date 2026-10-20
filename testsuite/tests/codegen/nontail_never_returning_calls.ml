(* TEST
 flags += " -O3";
 only-default-codegen;
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

(* Calls that Simplify finds never return, which cannot be tail calls: last in
   a [try], or [@nontail] *)

module Calls = struct
  exception Fail of string

  let[@inline never] fail s = raise (Fail s)

  let[@inline never] fail_product s : #(int * float#) =
    match raise (Fail s) with (n : int) -> #(n, #0.0)

  let[@inline never] in_try s : int = try fail s with Not_found -> 0

  let[@inline never] nontail s : int = fail s [@nontail]

  let[@inline never] product_in_try s : int =
    try
      let #(n, _) = fail_product s in
      n
    with Not_found -> 0

  let[@inline never] product_nontail s : int =
    let #(n, _) = fail_product s [@nontail] in
    n
end
[%%expect_asm X86_64{|
Calls.fail:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2048, -8(%rax)
  movq  <hidden PC-relative offset>(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  call  caml_reraise_exn@PLT
.L1:

Calls.fail_product:
  subq  $8, %rsp
  movq  %rax, %rbx
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movq  $2048, -8(%rax)
  movq  <hidden PC-relative offset>(%rip), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, 8(%rax)
  call  caml_reraise_exn@PLT
.L1:

Calls.in_try:
  subq  $8, %rsp
  movq  64(%r14), %rbx
  movq  %rbx, (%rsp)
.L0:
  leaq  <hidden PC-relative offset>(%rip), %r11
  pushq %r11
  pushq 48(%r14)
  movq  %rsp, 48(%r14)
  call  camlTOP1__fail_0_6_code@PLT
.L1:
.L2:
  movq  (%rsp), %rbx
  movq  %rbx, 64(%r14)
  movq  caml_exn_Not_found@GOTPCREL(%rip), %rbx
  cmpq  %rbx, %rax
  jne   .L3
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L3:
  call  caml_reraise_exn@PLT
.L4:

Calls.nontail:
  subq  $8, %rsp
  call  camlTOP1__fail_0_6_code@PLT
.L0:

Calls.product_in_try:
  subq  $8, %rsp
  movq  64(%r14), %rbx
  movq  %rbx, (%rsp)
.L0:
  leaq  <hidden PC-relative offset>(%rip), %r11
  pushq %r11
  pushq 48(%r14)
  movq  %rsp, 48(%r14)
  call  camlTOP1__fail_product_1_7_code@PLT
.L1:
.L2:
  movq  (%rsp), %rbx
  movq  %rbx, 64(%r14)
  movq  caml_exn_Not_found@GOTPCREL(%rip), %rbx
  cmpq  %rbx, %rax
  jne   .L3
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L3:
  call  caml_reraise_exn@PLT
.L4:

Calls.product_nontail:
  subq  $8, %rsp
  call  camlTOP1__fail_product_1_7_code@PLT
.L0:
|}]
