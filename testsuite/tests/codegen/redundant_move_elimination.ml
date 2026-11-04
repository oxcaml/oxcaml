(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 flags += " -regalloc ls";
 only-default-codegen;
 expect.opt;
*)

(* Codegen test for the deletion of redundant register-to-register moves by
   [Simplify_terminator]: a move is deleted when its source and destination
   registers are both statically known to hold the same integer constant.

   The code is distilled from the stdlib's [Seq.find_mapi] as used by the
   lib-seq tests, where the pattern arises naturally; the linear-scan
   allocator is selected because it leaves such redundant moves behind
   (other allocators coalesce them away).

   In the block advancing to the next list element (the [None] branch of the
   inlined predicate), two registers are set to the constant 1 and then
   swapped through a third register. The last move of the swap sequence, from
   the third register back into a register already known to contain 1, is
   deleted. The two preceding moves are kept: the destination of the first
   holds no known value at that point, and the second is not tracked because
   the machtypes of its source and destination differ. *)
let[@inline never] check =
  let find_mapi f l =
    let rec aux i l = match l with
      | [] -> None
      | x :: tl ->
        match f i x with
        | None -> aux (i + 1) tl
        | Some _ as result -> result in
    aux 0 l
  in
  fun l -> find_mapi (fun i x -> if x + i = 3 then Some i else None) l
[%%expect_asm X86_64{|
check.(fun):
  subq  $8, %rsp
  movl  $1, %ebx
  testb $1, %al
  je    .L1
.L0:
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L1:
  movq  (%rax), %rdi
  leaq  -1(%rdi,%rbx), %rdi
  cmpq  $7, %rdi
  jne   .L3
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L2:
  leaq  8(%r15), %rdi
  movq  $1024, -8(%rdi)
  movq  %rbx, (%rdi)
  xorl  %esi, %esi
  jmp   .L4
.L3:
  movl  $1, %edi
  movl  $1, %esi
  movq  %rdi, %rdx
  movq  %rsi, %rdi
  movq  8(%rax), %rax
  addq  $2, %rbx
  testb $1, %al
  je    .L1
  jmp   .L0
.L4:
  movq  %rdi, %rax
  addq  $8, %rsp
  ret
|}]
