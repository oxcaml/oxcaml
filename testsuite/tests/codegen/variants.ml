(* TEST
 flags += " -O3";
 flags += " -experimental-optimizations";
 only-default-codegen;
 flags += " -g -gdwarf-inlined-frames";
 expect.opt;
*)

(* CR ttebbi: The variant index could be used to index into the record. *)

module Variant_as_index = struct
  type index =
  | A | B

  type 'a container =
  { a : 'a
  ; b : 'a
  }

  let get cont idx =
    match idx with
    | A -> cont.a
    | B -> cont.b
end
[%%expect_asm X86_64{|
Variant_as_index.get:
  cmpq  $1, %rbx
  jne   .L0
  movq  (%rax), %rax
  ret
.L0:
  movq  8(%rax), %rax
  ret
|}, Arm64{|
Variant_as_index.get:
  subs  xzr, x1, #1
  b.ne  .L1
.L0:
  ldr   x0, [x0, #0]
  ret
.L1:
  ldr   x0, [x0, #8]
  ret
.L2:
|}]


(* CR ttebbi: The mutable breaks the optimization to access
   the same offset unconditionally. There is an open PR
   https://github.com/oxcaml/oxcaml/pull/579 *)

module Variant_with_uneven_mutability = struct
  type t =
    | A of { x : int; }
    | B of { mutable x : int; }
    | C of { x : int; }

  let get t =
    match t with
    | A r -> r.x
    | B r -> r.x
    | C r -> r.x
end
[%%expect_asm X86_64{|
Variant_with_uneven_mutability.get:
  movzbq -8(%rax), %rbx
  cmpq  $1, %rbx
  jne   .L0
  movq  (%rax), %rax
  ret
.L0:
  movq  (%rax), %rax
  ret
|}, Arm64{|
Variant_with_uneven_mutability.get:
  ldrb  w1, [x0, #-8]
.L0:
  subs  xzr, x1, #1
  b.ne  .L1
  ldr   x0, [x0, #0]
  ret
.L1:
  ldr   x0, [x0, #0]
  ret
.L2:
|}]


module Or_pattern_on_mutable = struct
  type t =
    | A of { mutable x : string }
    | B of { mutable x : string }

  let get (t : t) = match t with A { x } | B { x } -> x
end
[%%expect_asm X86_64{|
Or_pattern_on_mutable.get:
  movq  (%rax), %rax
  ret
|}, Arm64{|
Or_pattern_on_mutable.get:
.L0:
  ldr   x0, [x0, #0]
  ret
.L1:
|}]


type t =
  | A
  | B
  | C
  | D

let variant_id (t : t) : int =
  match t with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
[%%expect_asm X86_64{|
variant_id:
  ret
|}, Arm64{|
variant_id:
  ret
|}]

let last_two (t : t) : bool =
  match t with
  | A -> false
  | B -> false
  | C -> true
  | D -> true
[%%expect_asm X86_64{|
last_two:
  cmpq  $5, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
last_two:
.L0:
  subs  xzr, x0, #5
  csinc x0, xzr, xzr, lt
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]

(* CR ttebbi: This could be done looking at a single bit, and should certainly
    be branchfree.
*)
let even_variant (t : t) : bool =
  match t with
  | A -> true
  | B -> false
  | C -> true
  | D -> false
[%%expect_asm X86_64{|
even_variant:
  cmpq  $3, %rax
  je    .L0
  cmpq  $7, %rax
  setl  %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  ret
.L0:
  movl  $1, %eax
  ret
|}, Arm64{|
even_variant:
.L0:
  subs  xzr, x0, #3
  b.eq  .L1
  subs  xzr, x0, #7
  csinc x0, xzr, xzr, ge
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
  orr   x0, xzr, #1
  ret
.L2:
|}]

let map_to_constants (t : t) : int =
  match t with
  | A -> 5
  | B -> 10
  | C -> 2
  | D -> 7
[%%expect_asm X86_64{|
map_to_constants:
  movq  <hidden PC-relative offset>(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}, Arm64{|
map_to_constants:
  adrp  x1, <hidden PC-relative offset>
  ldr   x1, [x1, <hidden PC-relative offset>]
.L0:
  add   x0, x1, x0, lsl #2
  ldr   x0, [x0, #-4]
  ret
.L1:
|}]

let map_to_float_constants (t : t) : float# =
  match t with
  | A -> #5.0
  | B -> #10.0
  | C -> #2.0
  | D -> #7.0
[%%expect_asm X86_64{|
map_to_float_constants:
  movq  <hidden PC-relative offset>(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}, Arm64{|
map_to_float_constants:
  adrp  x1, <hidden PC-relative offset>
  ldr   x1, [x1, <hidden PC-relative offset>]
.L0:
  add   x0, x1, x0, lsl #2
  ldr   d0, [x0, #-4]
  ret
.L1:
|}]

type t =
  | C of int
  | D of int

let boxed_variant_id (t : t) : int =
  match t with
  | C _ -> 0
  | D _ -> 1
[%%expect_asm X86_64{|
boxed_variant_id:
  movzbq -8(%rax), %rax
  leaq  1(%rax,%rax), %rax
  ret
|}, Arm64{|
boxed_variant_id:
  ldrb  w0, [x0, #-8]
.L0:
  orr   x1, xzr, #1
  add   x0, x1, x0, lsl #1
  ret
.L1:
|}]


(* CR ttebbi: Constant match results could be implemented branch-free. *)
type t =
  | A
  | B

let map_to_constants_two (t : t) : int =
  match t with
  | A -> -1
  | B -> 1
;;
[%%expect_asm X86_64{|
map_to_constants_two:
  cmpq  $1, %rax
  jne   .L0
  movq  $-1, %rax
  ret
.L0:
  movl  $3, %eax
  ret
|}, Arm64{|
map_to_constants_two:
  subs  xzr, x0, #1
  b.ne  .L0
  movn  x0, #0
  ret
.L0:
  orr   x0, xzr, #3
  ret
|}]


(* CR ttebbi: We could eliminate jump tables when all the
   switch cases run the same code. *)
type t =
  | A
  | B
  | C
  | D

let[@inline never] opaque_fun _ = ()

let unnecessary_match = function
  | A -> opaque_fun 0
  | B -> opaque_fun 1
  | C -> opaque_fun 2
  | D -> opaque_fun 3
;;
[%%expect_asm X86_64{|
unnecessary_match:
  sarq  $1, %rax
  leaq  <hidden PC-relative offset>(%rip), %rdx
  movslq (%rdx,%rax,4), %rax
  addq  %rax, %rdx
  jmp   *%rdx
.L0:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L1:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $3, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L2:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $5, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L3:
  movq  <hidden PC-relative offset>(%rip), %rax
  movq  16(%rax), %rbx
  movl  $7, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
|}, Arm64{|
unnecessary_match:
.L0:
  sbfm  x0, x0, #1, #63
  adr   x16, .L1
  add   x16, x16, x0, lsl #2
  br    x16
.L1:
  b     .L2
  b     .L3
  b     .L4
  b     .L5
.L2:
  adrp  x0, <hidden PC-relative offset>
  ldr   x0, [x0, <hidden PC-relative offset>]
  ldr   x1, [x0, #16]
  orr   x0, xzr, #1
  ldr   x2, [x1, #0]
  br    x2
.L3:
  adrp  x0, <hidden PC-relative offset>
  ldr   x0, [x0, <hidden PC-relative offset>]
  ldr   x1, [x0, #16]
  orr   x0, xzr, #3
  ldr   x2, [x1, #0]
  br    x2
.L4:
  adrp  x0, <hidden PC-relative offset>
  ldr   x0, [x0, <hidden PC-relative offset>]
  ldr   x1, [x0, #16]
  movz  x0, #5, lsl #0
  ldr   x2, [x1, #0]
  br    x2
.L5:
  adrp  x0, <hidden PC-relative offset>
  ldr   x0, [x0, <hidden PC-relative offset>]
  ldr   x1, [x0, #16]
  orr   x0, xzr, #7
  ldr   x2, [x1, #0]
  br    x2
.L6:
|}]


type t =
  | C
  | D
  | E

type s =
  | A of int
  | B of int

(* CR ttebbi: We fail to avoid boxing here, see
    https://github.com/oxcaml/oxcaml/pull/2257
*)
let double_match c a b =
  let m =
    match c with
    | C -> A a
    | D -> B b
    | E -> B (b + 1)
  in
  match m with
  | A x -> x
  | B y -> y
[%%expect_asm X86_64{|
double_match:
  subq  $8, %rsp
  movq  64(%r14), %rsi
  sarq  $1, %rax
  cmpq  $1, %rax
  je    .L1
  ja    .L3
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L0:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $1792, -8(%rax)
  movq  %rbx, (%rax)
  jmp   .L5
.L1:
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L2:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $1793, -8(%rax)
  movq  %rdi, (%rax)
  jmp   .L5
.L3:
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    <hidden GC jump pad>
.L4:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $1793, -8(%rax)
  leaq  2(%rdi), %rbx
  movq  %rbx, (%rax)
.L5:
  movq  (%rax), %rax
  movq  %rsi, 64(%r14)
  addq  $8, %rsp
  ret
|}, Arm64{|
double_match:
  sub   sp, sp, #16
  str   lr, [sp, #8]
  ldr   x3, [x28, #64]
.L0:
  sbfm  x0, x0, #1, #63
  subs  xzr, x0, #1
  b.eq  .L2
  b.gt  .L4
  ldr   x16, [x28, #80]
  ldr   x0, [x28, #64]
  sub   x0, x0, #16
  str   x0, [x28, #64]
  subs  xzr, x0, x16
  b.lt  <hidden GC jump pad>
.L1:
  ldr   x16, [x28, #72]
  add   x0, x0, x16
  add   x0, x0, #8
  orr   x2, xzr, #1792
  str   x2, [x0, #-8]
  str   x1, [x0, #0]
  b     .L6
.L2:
  ldr   x16, [x28, #80]
  ldr   x0, [x28, #64]
  sub   x0, x0, #16
  str   x0, [x28, #64]
  subs  xzr, x0, x16
  b.lt  <hidden GC jump pad>
.L3:
  ldr   x16, [x28, #72]
  add   x0, x0, x16
  add   x0, x0, #8
  movz  x1, #1793, lsl #0
  str   x1, [x0, #-8]
  str   x2, [x0, #0]
  b     .L6
.L4:
  ldr   x16, [x28, #80]
  ldr   x0, [x28, #64]
  sub   x0, x0, #16
  str   x0, [x28, #64]
  subs  xzr, x0, x16
  b.lt  <hidden GC jump pad>
.L5:
  ldr   x16, [x28, #72]
  add   x0, x0, x16
  add   x0, x0, #8
  movz  x1, #1793, lsl #0
  str   x1, [x0, #-8]
  add   x1, x2, #2
  str   x1, [x0, #0]
.L6:
  ldr   x0, [x0, #0]
  str   x3, [x28, #64]
  ldr   lr, [sp, #8]
  add   sp, sp, #16
  ret
.L7:
|}]
