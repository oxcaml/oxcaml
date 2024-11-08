(* TEST
 flambda2;
 flags += " -flambda2-inline-small-function-size 0";
 flags += " -flambda2-inline-large-function-size 0";
 flags += " -O3";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)

(* Exercise the [simplify_switch_expr] single-arg-to-same-destination
   optimisation for each supported value kind. The expected assembly must
   load the per-arm constant from a static lookup table (of the appropriate
   element kind), so a regression in [simplify_switch_expr.ml] that drops the
   optimisation for a kind will be caught when the [%%expect_asm] blocks are
   regenerated. Empty blocks below are to be filled on an x86_64 machine. *)

type t = A | B | C | D

let match_tagged_immediate (t : t) : int =
  match t with
  | A -> 5
  | B -> 10
  | C -> 2
  | D -> 7
[%%expect_asm X86_64{|
match_tagged_immediate:
  leaq  .LcamlTOP2__switch_block27(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]

external untag_int : int -> int# = "%untag_int"

let match_naked_immediate (t : t) : int# =
  match t with
  | A -> untag_int 5
  | B -> untag_int 10
  | C -> untag_int 2
  | D -> untag_int 7
[%%expect_asm X86_64{|
match_naked_immediate:
  leaq  .LcamlTOP4__switch_block71(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  sarq  $1, %rax
  ret
|}]

let match_naked_float (t : t) : float# =
  match t with
  | A -> #5.0
  | B -> #10.0
  | C -> #2.0
  | D -> #7.0
[%%expect_asm X86_64{|
match_naked_float:
  leaq  .LcamlTOP5__switch_block101(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]

let match_naked_float32 (t : t) : float32# =
  match t with
  | A -> #5.0s
  | B -> #10.0s
  | C -> #2.0s
  | D -> #7.0s
[%%expect_asm X86_64{|
match_naked_float32:
  leaq  .LcamlTOP6__switch_block129(%rip), %rbx
  vmovss -2(%rbx,%rax,2), %xmm0
  ret
|}]

let match_naked_int32 (t : t) : int32# =
  match t with
  | A -> #5l
  | B -> #10l
  | C -> #2l
  | D -> #7l
[%%expect_asm X86_64{|
match_naked_int32:
  leaq  .LcamlTOP7__switch_block157(%rip), %rbx
  movslq -2(%rbx,%rax,2), %rax
  ret
|}]

let match_naked_int64 (t : t) : int64# =
  match t with
  | A -> #5L
  | B -> #10L
  | C -> #2L
  | D -> #7L
[%%expect_asm X86_64{|
match_naked_int64:
  leaq  .LcamlTOP8__switch_block185(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]

let match_naked_nativeint (t : t) : nativeint# =
  match t with
  | A -> #5n
  | B -> #10n
  | C -> #2n
  | D -> #7n
[%%expect_asm X86_64{|
match_naked_nativeint:
  leaq  .LcamlTOP9__switch_block213(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]

let match_naked_int8 (t : t) : int8# =
  match t with
  | A -> #5s
  | B -> #10s
  | C -> #2s
  | D -> #7s
[%%expect_asm X86_64{|
match_naked_int8:
  sarq  $1, %rax
  leaq  .LcamlTOP10__switch_block241(%rip), %rbx
  movsbq (%rbx,%rax), %rax
  ret
|}]

let match_naked_int16 (t : t) : int16# =
  match t with
  | A -> #5S
  | B -> #10S
  | C -> #2S
  | D -> #7S
[%%expect_asm X86_64{|
match_naked_int16:
  leaq  .LcamlTOP11__switch_block269(%rip), %rbx
  movswq -1(%rbx,%rax), %rax
  ret
|}]

let match_symbol (t : t) : string =
  match t with
  | A -> "alpha"
  | B -> "beta"
  | C -> "gamma"
  | D -> "delta"
[%%expect_asm X86_64{|
match_symbol:
  leaq  .LcamlTOP12__switch_block305(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]

(* Mixed symbol and tagged-immediate arms. Both are of kind [value], so the
   simplifier can put them in a single value-kind lookup table (see
   [Symbols_or_tagged_immediates] in [simplify_switch_expr.ml]). *)

type foo = P | Q | R1 of int | R2 of string

let match_symbol_or_tagged_immediate (t : t) : foo =
  match t with
  | A -> R2 "foo"
  | B -> Q
  | C -> R1 42
  | D -> P
[%%expect_asm X86_64{|
match_symbol_or_tagged_immediate:
  leaq  .LcamlTOP14__switch_block347(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
