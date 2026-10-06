(* TEST
 flags += " -O3";
 flags += " -extension layouts_alpha";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 only-default-codegen;
 expect.opt;
*)

(* The match compiler emits per-arm field accesses that disagree at the lambda
   level (e.g. [Pmixedfield]s on constructors with different total field
   counts), even when those accesses resolve to the same [Block_load] / [Cload]
   in Cmm. Without help, the resulting [Cifthenelse (cond, x, x)] is preserved
   all the way to the assembly. [Cmm_helpers.ite] collapses it. *)

type unboxed = #{ foo : int; bar : int }

module Length_of_mixed_variant = struct
  type 'a t =
    | Short of { length : int; tail : unboxed }
    | Long of { length : int; tail : unboxed; shift : int }

  (* Field 0 is at physical offset 0 in both constructors, so the two
     [Pmixedfield]s collapse to identical [Cload]s in Cmm. *)
  let length (Short { length; _ } | Long { length; _ }) = length
end
[%%expect_asm X86_64{|
Length_of_mixed_variant.length:
  movq  (%rax), %rax
  ret
|}]

module Tail_of_mixed_variant = struct
  type 'a t =
    | Short of { length : int; tail : unboxed }
    | Long of { length : int; tail : unboxed }

  (* Both constructors have the same physical layout for the [tail] product, so
     the two Cmm loads at offsets 1 and 2 are identical between arms. *)
  let tail (Short { tail; _ } | Long { tail; _ }) = tail
end
[%%expect_asm X86_64{|
Tail_of_mixed_variant.tail:
  movq  16(%rax), %rbx
  movq  8(%rax), %rax
  ret
|}]

(* Sanity check: when the two branches really do differ (here the unscannable
   [int64#] suffix lives at different offsets), the conditional must survive. *)

type mixed_unboxed = #{ foo : int64#; bar : int }

module Differing_branches = struct
  type 'a t =
    | Short of { length : int; tail : mixed_unboxed }
    | Long of { length : int; tail : mixed_unboxed; shift : int }

  let tail (Short { tail; _ } | Long { tail; _ }) = tail
end
[%%expect_asm X86_64{|
Differing_branches.tail:
  movzbq -8(%rax), %rbx
  testq %rbx, %rbx
  je    .L0
  movq  8(%rax), %rbx
  movq  24(%rax), %rax
  ret
.L0:
  movq  8(%rax), %rbx
  movq  16(%rax), %rax
  ret
|}]
