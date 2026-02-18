(* TEST
 flags += " -O3";
 only-default-codegen;
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
  jne   .L106
  movq  (%rax), %rax
  ret
.L106:
  movq  8(%rax), %rax
  ret
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
  jne   .L113
  movq  (%rax), %rax
  ret
.L113:
  movq  (%rax), %rax
  ret
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
|}]

(* CR ttebbi: This could be done looking at a single bit, and should certainly
    be branchfree. Also, this xor can be avoided by negating the bit extraction.
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
  je    .L108
  cmpq  $7, %rax
  setge %al
  movzbq %al, %rax
  leaq  1(%rax,%rax), %rax
  xorq  $2, %rax
  ret
.L108:
  movl  $1, %eax
  ret
|}]

let map_to_constants (t : t) : int =
  match t with
  | A -> 5
  | B -> 10
  | C -> 2
  | D -> 7
[%%expect_asm X86_64{|
map_to_constants:
  movq  camlTOP7__switch_block207@GOTPCREL(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
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
  jne   .L105
  movq  $-1, %rax
  ret
.L105:
  movl  $3, %eax
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
  leaq  .L125(%rip), %rdx
  movslq (%rdx,%rax,4), %rax
  addq  %rax, %rdx
  jmp   *%rdx
.L104:
  movq  camlTOP14__unnecessary_match_19@GOTPCREL(%rip), %rax
  movq  16(%rax), %rbx
  movl  $1, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L109:
  movq  camlTOP14__unnecessary_match_19@GOTPCREL(%rip), %rax
  movq  16(%rax), %rbx
  movl  $3, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L114:
  movq  camlTOP14__unnecessary_match_19@GOTPCREL(%rip), %rax
  movq  16(%rax), %rbx
  movl  $5, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
.L119:
  movq  camlTOP14__unnecessary_match_19@GOTPCREL(%rip), %rax
  movq  16(%rax), %rbx
  movl  $7, %eax
  movq  (%rbx), %rdi
  jmp   *%rdi
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
  je    .L111
  ja    .L114
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    .L124
.L125:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $1792, -8(%rax)
  movq  %rbx, (%rax)
  jmp   .L119
.L111:
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    .L126
.L127:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $1793, -8(%rax)
  movq  %rdi, (%rax)
  jmp   .L119
.L114:
  movq  64(%r14), %rax
  subq  $16, %rax
  movq  %rax, 64(%r14)
  cmpq  80(%r14), %rax
  jl    .L128
.L129:
  addq  72(%r14), %rax
  addq  $8, %rax
  movq  $1793, -8(%rax)
  addq  $2, %rdi
  movq  %rdi, (%rax)
.L119:
  movq  (%rax), %rax
  movq  %rsi, 64(%r14)
  addq  $8, %rsp
  ret
|}]
