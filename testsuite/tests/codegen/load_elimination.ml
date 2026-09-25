(* TEST
 only-default-codegen;
 flags = " -O3";
 flags += " -experimental-optimizations";
 flags += " -flambda2-simplify-stubs";
 expect.opt;
*)

let immutable_load l = (List.hd l) + (List.hd l)
[%%expect_asm X86_64{|
immutable_load:
  testb $1, %al
  je    .L0
  movq  camlStdlib__List__Pmakeblock2543_19@GOTPCREL(%rip), %rax
  movq  48(%r14), %rsp
  popq  48(%r14)
  popq  %r11
  jmp   *%r11
.L0:
  movq  (%rax), %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]


let mutable_load r = !r + !r
[%%expect_asm X86_64{|
mutable_load:
  movq  (%rax), %rax
  leaq  -1(%rax,%rax), %rax
  ret
|}]

(* CR ttebbi: There is no need to load the stored value. *)
let write_then_read r = r := 5; !r
[%%expect_asm X86_64{|
write_then_read:
  movq  $11, (%rax)
  movq  (%rax), %rax
  ret
|}]

let mutable_load_branch r b =
  let x = !r in
  x + if b then !r else 7
[%%expect_asm X86_64{|
mutable_load_branch:
  movq  (%rax), %rdi
  cmpq  $1, %rbx
  jne   .L0
  movl  $15, %eax
  jmp   .L1
.L0:
  movq  %rdi, %rax
.L1:
  leaq  -1(%rdi,%rax), %rax
  ret
|}]

type t = {a: int; b: string}
let immutable_load_loop (t: t) =
  let rec foo i acc =
    if i == 0
    then acc
    else foo (i - 1) (acc + t.a)
  in
  foo 10 (t.a)
[%%expect_asm X86_64{|
immutable_load_loop:
  movq  (%rax), %rdi
  movl  $21, %ebx
  movq  %rdi, %rax
  jmp   .L1
.L0:
  ret
.L1:
  leaq  -1(%rax,%rdi), %rax
  addq  $-2, %rbx
  cmpq  $1, %rbx
  jne   .L1
  jmp   .L0
|}]

(* CR ttebbi: Load elimination inside the loop is not working. *)
let mutable_load_loop r =
  let rec foo i acc = if i == 0 then acc else foo (i - 1) (acc + !r) in
  foo 10 !r
[%%expect_asm X86_64{|
mutable_load_loop:
  movq  %rax, %rdi
  movq  (%rdi), %rax
  movl  $21, %ebx
  jmp   .L1
.L0:
  ret
.L1:
  movq  (%rdi), %rsi
  leaq  -1(%rax,%rsi), %rax
  addq  $-2, %rbx
  cmpq  $1, %rbx
  jne   .L1
  jmp   .L0
|}]

(* CR ttebbi: We should figure out that the store and the load cannot alias. *)
let reload_after_nonaliasing_store r out =
  let load r = out := true; !r in
  load r + load r
[%%expect_asm X86_64{|
reload_after_nonaliasing_store:
  movq  $3, (%rbx)
  movq  (%rax), %rdi
  movq  $3, (%rbx)
  movq  (%rax), %rax
  leaq  -1(%rax,%rdi), %rax
  ret
|}]

type cursor = { mutable pos : int; mutable last : int }

(* Instruction selection fuses load-add-store to the same location into a
   read-modify-write instruction; it is not a [Store], so it records no
   forwarding equation, and since it reads memory, dead store elimination
   must leave both of them alone. *)
(* CR xclerc: see whether we could add a peephole rule to merge the addq
   instructions. *)
let bump_twice c =
  c.pos <- c.pos + 1;
  c.pos <- c.pos + 1
[%%expect_asm X86_64{|
bump_twice:
  addq  $2, (%rax)
  addq  $2, (%rax)
  movl  $1, %eax
  ret
|}]

(* Store-to-load forwarding across machtypes: [p + 1] has machtype [Int],
   whereas loading [pos] produces a [Val]. The reload of [pos] right after the
   store to [pos] is replaced by a reinterpret cast from the stored register
   (which, unlike a move, the register allocator does not coalesce), so only
   one load of [pos] should remain. Dead store elimination then removes the
   overwritten intermediate stores of [pos] and [last], so only one store of
   [pos] and one store of [last] should remain. *)
let push_two c =
  let p = c.pos in
  c.last <- p;
  c.pos <- p + 1;
  let q = c.pos in
  c.last <- q;
  c.pos <- q + 1
[%%expect_asm X86_64{|
push_two:
  movq  (%rax), %rbx
  addq  $2, %rbx
  movq  %rbx, 8(%rax)
  addq  $2, %rbx
  movq  %rbx, (%rax)
  movl  $1, %eax
  ret
|}]

(* The stored value and the reload both have machtype [Val], even though
   the store uses [Word_int] and the load uses [Word_val]. *)
let copy_pos c other =
  let p = other.pos in
  c.pos <- p;
  c.last <- c.pos
[%%expect_asm X86_64{|
copy_pos:
  movq  (%rbx), %rbx
  movq  %rbx, (%rax)
  movq  %rbx, 8(%rax)
  movl  $1, %eax
  ret
|}]

external store_bits : int64_u -> int64_u -> unit = "%unsafe_set_ext_ptr"
external load_value : int64_u -> string = "%unsafe_get_ext_ptr"

type snapshot = { root : string; mutable bits : int64_u }

(* [p] is a writable off-heap word and [raw] is the address of a live string.
   The load roots the string before allocating [snapshot]. A moving GC may
   update [rooted], but [raw] must retain the original integer bits: the load
   of [rooted] may be forwarded from [raw], but through a cast into a distinct
   register, and the final store must use the register holding [raw]. *)
let store_load_across_gc p raw =
  store_bits p raw;
  let rooted = load_value p in
  let snapshot = { root = rooted; bits = #0L } in
  snapshot.bits <- raw;
  let result = snapshot.bits in
  #(snapshot, rooted, result)
[%%expect_asm X86_64{|
store_load_across_gc:
  subq  $8, %rsp
  movq  %rbx, (%rax)
  movq  %rbx, %rsi
  subq  $24, %r15
  cmpq  (%r14), %r15
  jb    <hidden GC jump pad>
.L0:
  leaq  8(%r15), %rax
  movabsq $144115188075857920, %rdi
  movq  %rdi, -8(%rax)
  movq  %rsi, (%rax)
  movq  $0, 8(%rax)
  movq  %rbx, 8(%rax)
  movq  %rbx, %rdi
  movq  %rsi, %rbx
  addq  $8, %rsp
  ret
|}]

external store_int : int64_u -> int -> unit = "%unsafe_set_ext_ptr"
external load_bits : int64_u -> int64_u = "%unsafe_get_ext_ptr"

(* Forwarding in the other direction: [c.pos] is loaded with [Word_val] into
   a [Val] register and stored with [Word_int], while the reload produces an
   [Int]; only one load should remain. *)
let store_int_load_bits p (c : cursor) =
  store_int p c.pos;
  let bits = load_bits p in
  #(c, bits)
[%%expect_asm X86_64{|
store_int_load_bits:
  movq  (%rbx), %rdi
  movq  %rdi, (%rax)
  movq  %rbx, %rax
  movq  %rdi, %rbx
  ret
|}]
