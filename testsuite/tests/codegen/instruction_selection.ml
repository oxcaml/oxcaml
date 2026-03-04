(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 expect.opt;
*)

open Intrinsics


(* CR ttebbi: The move instruction encoding with immediates
   is quite big (7-8 bytes). It would be better to put the
   constant into a register. *)
type t =
  { mutable a : int
  ; mutable b : int
  ; mutable c : int
  ; mutable d : int
  }
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
  leaq  (%rbx,%rax), %rdi
  addq  %rdi, %rbx
  leaq  -3(%rax,%rbx), %rax
  ret
|}]

(* CR ttebbi: We could merge the and and test instructions *)
let do_intersect t1 t2 =
  Int64_u.(if equal (logand t1 t2) #0L then #100L else #200L)
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


(* CR ttebbi: We materialize comparison result bits despite
   only using them for a single branch. Also, the `_ -> 0`
   case is duplicated for no good reason. *)
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


(* CR ttebbi: `leaq  8(%r15), %rbx` could be merged with
   the subsequent addition. *)
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
let constant_folding (x : int) =
  if x < x then 1 + 2
  else if x - x = 0 then 3
  else 4
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



type ptr = nativeint#
external memcmp :
  ptr -> ptr -> len:nativeint# -> int32#
  @@ portable
  = "caml_no_bytecode_impl" "memcmp"
[@@noalloc]

(* CR ttebbi: Double sign extension instructions. *)
let int32_box_unbox_after_call (a : ptr) (b : ptr) =
  Int32_u.of_int (Int32_u.to_int (memcmp a b ~len:#5n))
[%%expect_asm X86_64{|
int32_box_unbox_after_call:
  subq  $8, %rsp
  movq  %rax, %rdi
  movq  %rbx, %rsi
  movl  $5, %edx
  call  memcmp@PLT
  movslq %eax, %rax
  movslq %eax, %rax
  addq  $8, %rsp
  ret
|}]

(* CR ttebbi: "xchg  %ah, %al" is rather slow, a 32bit byte swap followed by a
   shift would be faster. Also, we zero-extend twice. *)
let bswap16 x = Int.bswap16 x
[%%expect_asm X86_64{|
bswap16:
  sarq  $1, %rax
  xchg  %ah, %al
  movzwq %ax, %rax
  andl  $65535, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let int63_to_int64 x = reinterpret_tagged_int63_as_unboxed_int64 x
[%%expect_asm X86_64{|
int63_to_int64:
  ret
|}]

(* CR ttebbi: There should be a way to reinterpret as int without tagging. *)
let int64_to_int63 x = reinterpret_unboxed_int64_as_tagged_int63 x
[%%expect_asm X86_64{|
int64_to_int63:
  orq   $1, %rax
  ret
|}]

let pause () = cpu_relax ()
[%%expect_asm X86_64{|
pause:
  subq  $8, %rsp
  pause
  cmpq  (%r14), %r15
  jbe   .L105
.L106:
  movl  $1, %eax
  addq  $8, %rsp
  ret
|}]

(* Cross-type conversions between unboxed types *)

let int32_to_int64 (x : Int32_u.t) : Int64_u.t =
  Int64_u.of_int32_u x
[%%expect_asm X86_64{|
int32_to_int64:
  ret
|}]

let int64_to_int32 (x : Int64_u.t) : Int32_u.t =
  Int64_u.to_int32_u x
[%%expect_asm X86_64{|
int64_to_int32:
  movslq %eax, %rax
  ret
|}]

let int64_to_nativeint (x : Int64_u.t) : Nativeint_u.t =
  Int64_u.to_nativeint_u x
[%%expect_asm X86_64{|
int64_to_nativeint:
  ret
|}]

let nativeint_to_int64 (x : Nativeint_u.t) : Int64_u.t =
  Int64_u.of_nativeint_u x
[%%expect_asm X86_64{|
nativeint_to_int64:
  ret
|}]


(* Optimization barrier *)

let opaque_int (x : int) = opaque x
[%%expect_asm X86_64{|
opaque_int:
  ret
|}]

(* Tag test for variant discrimination *)

let is_int (x : 'a) = obj_is_int x
[%%expect_asm X86_64{|
is_int:
  andl  $1, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let is_int_branch (x : 'a) f = if obj_is_int x then f()
[%%expect_asm X86_64{|
is_int_branch:
  testb $1, %al
  je    .L107
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L107:
  movl  $1, %eax
  ret
|}]


(* CR ttebbi: https://github.com/oxcaml/oxcaml/issues/2521 *)
let is_block_branch (x : 'a) f = if not(obj_is_int x) then f()
[%%expect_asm X86_64{|
is_block_branch:
  testb $1, %al
  je    .L105
  movl  $1, %eax
  ret
.L105:
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}]


(* CR ttebbi: https://github.com/oxcaml/oxcaml/issues/2929 *)
let branch_or_tailcall x =
  let[@inline never] failure _ = failwith "..." in
  match x with
  | 0 -> 5
  | 1 -> 3
  | 2 -> 7
  | n -> failure n
[%%expect_asm X86_64{|
branch_or_tailcall:
  cmpq  $5, %rax
  jbe   .L105
  movq  camlTOP25__Pmakeblock786@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L105:
  movq  camlTOP25__switch_block787@GOTPCREL(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]


(* CR ttebbi: The final bitwise or is unnecessary. *)
let shift_of_logand (a : int64#) =
  let b = Int64_u.logand a #1L in
  let c = Int64_u.shift_right_logical #3L (Int64_u.to_int b) in
  reinterpret_unboxed_int64_as_tagged_int63 c
;;
[%%expect_asm X86_64{|
shift_of_logand:
  movq  %rax, %rcx
  movl  $1, %eax
  andq  %rax, %rcx
  movl  $3, %eax
  shrq  %cl, %rax
  orq   $1, %rax
  ret
|}]


(* CR ttebbi: We could use lea as a shorter encoding alternative to encode
  small constants. *)
let small_constants () = #(#0L, #5L)
[%%expect_asm X86_64{|
small_constants:
  movl  $5, %ebx
  xorl  %eax, %eax
  ret
|}]
