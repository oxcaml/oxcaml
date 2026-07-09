(* TEST
 poll-insertion;
 no-stack-checks;
 no-frame_pointers;
 no-address-sanitizer;
 expect.opt;
*)

(* [opaque] is used (rather than [Sys.opaque_identity]) so that the local
   allocations below cannot be elided by the middle end. *)
external opaque : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

(* Control: a loop with no allocation gets a poll on its back edge (this is
   what proves that poll insertion is active in this configuration). *)
let[@inline never] no_alloc_loop n =
  for i = 0 to n do
    let (_ : int) = opaque i in
    ()
  done
[%%expect_asm_full X86_64{|
no_alloc_loop:
  cmpq  $1, %rax
  jl    .L3
  subq  $8, %rsp
  sarq  $1, %rax
  xorl  %ebx, %ebx
.L0:
  leaq  1(%rbx,%rbx), %rdi
  incq  %rbx
  cmpq  %rax, %rbx
  jg    .L2
  cmpq  (%r14), %r15
  jbe   .L4
.L1:
  jmp   .L0
.L2:
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L3:
  movl  $1, %eax
  ret
.L4:
  call  .Lcaml_call_gc_
.L5:
  jmp   .L1
|}]

(* Control: a loop with a heap allocation gets no poll; the allocation's
   [young_limit] check is the safe point. *)
let[@inline never] heap_alloc_loop n =
  for i = 0 to n do
    let r @ global = ref (opaque i) in
    let (_ : int ref) = Sys.opaque_identity r in
    ()
  done
[%%expect_asm_full X86_64{|
heap_alloc_loop:
  cmpq  $1, %rax
  jl    .L2
  subq  $8, %rsp
  sarq  $1, %rax
  xorl  %ebx, %ebx
.L0:
  leaq  1(%rbx,%rbx), %rdi
  subq  $16, %r15
  cmpq  (%r14), %r15
  jb    .L3
.L1:
  leaq  8(%r15), %rsi
  movq  $1024, -8(%rsi)
  movq  %rdi, (%rsi)
  incq  %rbx
  cmpq  %rax, %rbx
  jle   .L0
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L2:
  movl  $1, %eax
  ret
.L3:
  call  .Lcaml_call_gc_
.L4:
  jmp   .L1
|}]

(* BUG: a loop whose only allocation is local gets no poll at all: there is
   no [young_limit] check anywhere in the code below. *)
let[@inline never] local_alloc_loop n =
  for i = 0 to n do
    let local_ r = ref (opaque i) in
    let (_ : int ref) = opaque r in
    ()
  done
[%%expect_asm_full X86_64{|
local_alloc_loop:
  cmpq  $1, %rax
  jl    .L2
  subq  $8, %rsp
  sarq  $1, %rax
  xorl  %ebx, %ebx
.L0:
  movq  64(%r14), %rdi
  leaq  1(%rbx,%rbx), %rsi
  movq  64(%r14), %rdx
  subq  $16, %rdx
  movq  %rdx, 64(%r14)
  cmpq  80(%r14), %rdx
  jl    .L3
.L1:
  addq  72(%r14), %rdx
  addq  $8, %rdx
  movq  $1792, -8(%rdx)
  movq  %rsi, (%rdx)
  movq  %rdi, 64(%r14)
  incq  %rbx
  cmpq  %rax, %rbx
  jle   .L0
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L2:
  movl  $1, %eax
  ret
.L3:
  call  caml_call_local_realloc@PLT
  jmp   .L1
|}]
