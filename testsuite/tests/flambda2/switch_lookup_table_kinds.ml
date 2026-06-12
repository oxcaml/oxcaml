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
 expect.opt with dump-simplify;
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
[%%expect_fexpr Simplify{|
let code match_tagged_immediate_0 deleted in
let $camlTOP2__switch_block27 = Value_array [|5; 10; 2; 7|] in
let code loopify(never) size(2) newer_version_of(match_tagged_immediate_0)
      match_tagged_immediate_0_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : imm tagged =
  let arg = %array_load ($camlTOP2__switch_block27, t) in
  cont k (arg)
in
let $camlTOP2__match_tagged_immediate_1 =
  closure match_tagged_immediate_0_1 @match_tagged_immediate
in
let $camlTOP2 = Block 0 ($camlTOP2__match_tagged_immediate_1) in
cont done ($camlTOP2)
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
  leaq  .LcamlTOP4__switch_block79(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_immediate_2 deleted in
let $camlTOP4__switch_block79 = Int_array [|5; 10; 2; 7|] in
let code loopify(never) size(2) newer_version_of(match_naked_immediate_2)
      match_naked_immediate_2_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : imm =
  let arg = %array_load.int ($camlTOP4__switch_block79, t) in
  cont k (arg)
in
let $camlTOP4__match_naked_immediate_3 =
  closure match_naked_immediate_2_1 @match_naked_immediate
in
let $camlTOP4 = Block 0 ($camlTOP4__match_naked_immediate_3) in
cont done ($camlTOP4)
|}]

let match_naked_float (t : t) : float# =
  match t with
  | A -> #5.0
  | B -> #10.0
  | C -> #2.0
  | D -> #7.0
[%%expect_asm X86_64{|
match_naked_float:
  leaq  .LcamlTOP5__switch_block115(%rip), %rbx
  vmovsd -4(%rbx,%rax,4), %xmm0
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_float_4 deleted in
let $camlTOP5__switch_block115 =
  Float_array [|0x1.4p+2;
  0x1.4p+3;
  0x1p+1;
  0x1.cp+2|]
in
let code loopify(never) size(2) newer_version_of(match_naked_float_4)
      match_naked_float_4_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : float =
  let arg = %array_load.`float` ($camlTOP5__switch_block115, t) in
  cont k (arg)
in
let $camlTOP5__match_naked_float_5 =
  closure match_naked_float_4_1 @match_naked_float
in
let $camlTOP5 = Block 0 ($camlTOP5__match_naked_float_5) in
cont done ($camlTOP5)
|}]

let match_naked_float32 (t : t) : float32# =
  match t with
  | A -> #5.0s
  | B -> #10.0s
  | C -> #2.0s
  | D -> #7.0s
[%%expect_asm X86_64{|
match_naked_float32:
  leaq  .LcamlTOP6__switch_block151(%rip), %rbx
  vmovss -2(%rbx,%rax,2), %xmm0
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_float32_6 deleted in
let $camlTOP6__switch_block151 =
  Float32_array [|0x1.4p+2s;
  0x1.4p+3s;
  0x1p+1s;
  0x1.cp+2s|]
in
let code loopify(never) size(3) newer_version_of(match_naked_float32_6)
      match_naked_float32_6_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : float32 =
  let arg = %array_load.`float32` ($camlTOP6__switch_block151, t) in
  cont k (arg)
in
let $camlTOP6__match_naked_float32_7 =
  closure match_naked_float32_6_1 @match_naked_float32
in
let $camlTOP6 = Block 0 ($camlTOP6__match_naked_float32_7) in
cont done ($camlTOP6)
|}]

let match_naked_int32 (t : t) : int32# =
  match t with
  | A -> #5l
  | B -> #10l
  | C -> #2l
  | D -> #7l
[%%expect_asm X86_64{|
match_naked_int32:
  leaq  .LcamlTOP7__switch_block187(%rip), %rbx
  movslq -2(%rbx,%rax,2), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_int32_8 deleted in
let $camlTOP7__switch_block187 = Int32_array [|5l; 10l; 2l; 7l|] in
let code loopify(never) size(3) newer_version_of(match_naked_int32_8)
      match_naked_int32_8_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : int32 =
  let arg = %array_load.`int32` ($camlTOP7__switch_block187, t) in
  cont k (arg)
in
let $camlTOP7__match_naked_int32_9 =
  closure match_naked_int32_8_1 @match_naked_int32
in
let $camlTOP7 = Block 0 ($camlTOP7__match_naked_int32_9) in
cont done ($camlTOP7)
|}]

let match_naked_int64 (t : t) : int64# =
  match t with
  | A -> #5L
  | B -> #10L
  | C -> #2L
  | D -> #7L
[%%expect_asm X86_64{|
match_naked_int64:
  leaq  .LcamlTOP8__switch_block223(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_int64_10 deleted in
let $camlTOP8__switch_block223 = Int64_array [|5L; 10L; 2L; 7L|] in
let code loopify(never) size(2) newer_version_of(match_naked_int64_10)
      match_naked_int64_10_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : int64 =
  let arg = %array_load.`int64` ($camlTOP8__switch_block223, t) in
  cont k (arg)
in
let $camlTOP8__match_naked_int64_11 =
  closure match_naked_int64_10_1 @match_naked_int64
in
let $camlTOP8 = Block 0 ($camlTOP8__match_naked_int64_11) in
cont done ($camlTOP8)
|}]

let match_naked_nativeint (t : t) : nativeint# =
  match t with
  | A -> #5n
  | B -> #10n
  | C -> #2n
  | D -> #7n
[%%expect_asm X86_64{|
match_naked_nativeint:
  leaq  .LcamlTOP9__switch_block259(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_nativeint_12 deleted in
let $camlTOP9__switch_block259 = Nativeint_array [|5n; 10n; 2n; 7n|] in
let code loopify(never) size(2) newer_version_of(match_naked_nativeint_12)
      match_naked_nativeint_12_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : nativeint =
  let arg = %array_load.`nativeint` ($camlTOP9__switch_block259, t) in
  cont k (arg)
in
let $camlTOP9__match_naked_nativeint_13 =
  closure match_naked_nativeint_12_1 @match_naked_nativeint
in
let $camlTOP9 = Block 0 ($camlTOP9__match_naked_nativeint_13) in
cont done ($camlTOP9)
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
  leaq  .LcamlTOP10__switch_block295(%rip), %rbx
  movsbq (%rbx,%rax), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_int8_14 deleted in
let $camlTOP10__switch_block295 = Int8_array [|5s; 10s; 2s; 7s|] in
let code loopify(never) size(3) newer_version_of(match_naked_int8_14)
      match_naked_int8_14_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : int8 =
  let arg = %array_load.int8 ($camlTOP10__switch_block295, t) in
  cont k (arg)
in
let $camlTOP10__match_naked_int8_15 =
  closure match_naked_int8_14_1 @match_naked_int8
in
let $camlTOP10 = Block 0 ($camlTOP10__match_naked_int8_15) in
cont done ($camlTOP10)
|}]

let match_naked_int16 (t : t) : int16# =
  match t with
  | A -> #5S
  | B -> #10S
  | C -> #2S
  | D -> #7S
[%%expect_asm X86_64{|
match_naked_int16:
  leaq  .LcamlTOP11__switch_block331(%rip), %rbx
  movswq -1(%rbx,%rax), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let code match_naked_int16_16 deleted in
let $camlTOP11__switch_block331 = Int16_array [|5S; 10S; 2S; 7S|] in
let code loopify(never) size(3) newer_version_of(match_naked_int16_16)
      match_naked_int16_16_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : int16 =
  let arg = %array_load.int16 ($camlTOP11__switch_block331, t) in
  cont k (arg)
in
let $camlTOP11__match_naked_int16_17 =
  closure match_naked_int16_16_1 @match_naked_int16
in
let $camlTOP11 = Block 0 ($camlTOP11__match_naked_int16_17) in
cont done ($camlTOP11)
|}]

let match_symbol (t : t) : string =
  match t with
  | A -> "alpha"
  | B -> "beta"
  | C -> "gamma"
  | D -> "delta"
[%%expect_asm X86_64{|
match_symbol:
  leaq  .LcamlTOP12__switch_block375(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let $camlTOP12__immstring358 = "alpha" in
let $camlTOP12__immstring360 = "beta" in
let $camlTOP12__immstring362 = "gamma" in
let $camlTOP12__immstring364 = "delta" in
let code match_symbol_18 deleted in
let $camlTOP12__switch_block375 =
  Value_array [|$camlTOP12__immstring358;
  $camlTOP12__immstring360;
  $camlTOP12__immstring362;
  $camlTOP12__immstring364|]
in
let code loopify(never) size(2) newer_version_of(match_symbol_18)
      match_symbol_18_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : val =
  let arg = %array_load ($camlTOP12__switch_block375, t) in
  cont k (arg)
in
let $camlTOP12__match_symbol_19 = closure match_symbol_18_1 @match_symbol in
let $camlTOP12 = Block 0 ($camlTOP12__match_symbol_19) in
cont done ($camlTOP12)
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
  leaq  .LcamlTOP14__switch_block425(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let $camlTOP14__immstring410 = "foo" in
let $camlTOP14__const_block412 = Block 1 ($camlTOP14__immstring410) in
let $camlTOP14__const_block414 = Block 0 (42) in
let code match_symbol_or_tagged_immediate_20 deleted in
let $camlTOP14__switch_block425 =
  Value_array [|$camlTOP14__const_block412;
  1;
  $camlTOP14__const_block414;
  0|]
in
let code loopify(never) size(2) newer_version_of(match_symbol_or_tagged_immediate_20)
      match_symbol_or_tagged_immediate_20_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : [ 0 |1 | 0 of imm tagged |1 of val ] =
  let arg = %array_load ($camlTOP14__switch_block425, t) in
  cont k (arg)
in
let $camlTOP14__match_symbol_or_tagged_immediate_21 =
  closure match_symbol_or_tagged_immediate_20_1
    @match_symbol_or_tagged_immediate
in
let $camlTOP14 = Block 0 ($camlTOP14__match_symbol_or_tagged_immediate_21) in
cont done ($camlTOP14)
|}]

(* As above, but additionally including a [Null] arm. The [Null] constant is
   of kind [value], so it can sit in the same value-kind lookup table as the
   symbol and tagged-immediate arms. *)

let match_symbol_tagged_or_null (t : t) : foo or_null =
  match t with
  | A -> This (R2 "foo")
  | B -> Null
  | C -> This (R1 42)
  | D -> This P
[%%expect_asm X86_64{|
match_symbol_tagged_or_null:
  leaq  .LcamlTOP15__switch_block467(%rip), %rbx
  movq  -4(%rbx,%rax,4), %rax
  ret
|}]
[%%expect_fexpr Simplify{|
let $camlTOP15__immstring452 = "foo" in
let $camlTOP15__const_block454 = Block 1 ($camlTOP15__immstring452) in
let $camlTOP15__const_block456 = Block 0 (42) in
let code match_symbol_tagged_or_null_22 deleted in
let $camlTOP15__switch_block467 =
  Value_array [|$camlTOP15__const_block454;
  null;
  $camlTOP15__const_block456;
  0|]
in
let code loopify(never) size(2) newer_version_of(match_symbol_tagged_or_null_22)
      match_symbol_tagged_or_null_22_1 (t : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : [ 0 |1 | 0 of imm tagged |1 of val ] =
  let arg = %array_load ($camlTOP15__switch_block467, t) in
  cont k (arg)
in
let $camlTOP15__match_symbol_tagged_or_null_23 =
  closure match_symbol_tagged_or_null_22_1 @match_symbol_tagged_or_null
in
let $camlTOP15 = Block 0 ($camlTOP15__match_symbol_tagged_or_null_23) in
cont done ($camlTOP15)
|}]
