(* TEST
   flambda2;
   expect.opt with dump-simplify;
 *)

[@@@flambda_o3]

(* A boolean that has just been computed by a comparison, and is then compared
   against a constant, should not be tagged. *)

let eq_true o =
  let cmp = o <> 0 in
  if cmp = true then o else 7
[%%expect_fexpr Simplify{|
let code eq_true_0 deleted in
let code loopify(never) size(12) newer_version_of(eq_true_0)
      eq_true_0_1 (o : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let prim = %phys_ne (o, 0) in
  switch prim
    | 0 -> k (7)
    | 1 -> k (o)
in
let $camlTOP2__eq_true_2 = closure eq_true_0_1 @eq_true &toplevel in
let $camlTOP2 = Block 0 ($camlTOP2__eq_true_2) in
cont done ($camlTOP2)
|}]

let eq_false o =
  let cmp = o <> 0 in
  if cmp = false then o else 7
[%%expect_fexpr Simplify{|
let code eq_false_1 deleted in
let code loopify(never) size(13) newer_version_of(eq_false_1)
      eq_false_1_1 (o : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let prim = %phys_ne (o, 0) in
  let prim_1 = %boolean_not.`imm` (prim) in
  switch prim_1
    | 0 -> k (7)
    | 1 -> k (o)
in
let $camlTOP3__eq_false_5 = closure eq_false_1_1 @eq_false &toplevel in
let $camlTOP3 = Block 0 ($camlTOP3__eq_false_5) in
cont done ($camlTOP3)
|}]

let neq_true o =
  let cmp = o <> 0 in
  if cmp <> true then o else 7
[%%expect_fexpr Simplify{|
let code neq_true_2 deleted in
let code loopify(never) size(13) newer_version_of(neq_true_2)
      neq_true_2_1 (o : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let prim = %phys_ne (o, 0) in
  let prim_1 = %boolean_not.`imm` (prim) in
  switch prim_1
    | 0 -> k (7)
    | 1 -> k (o)
in
let $camlTOP4__neq_true_8 = closure neq_true_2_1 @neq_true &toplevel in
let $camlTOP4 = Block 0 ($camlTOP4__neq_true_8) in
cont done ($camlTOP4)
|}]

let neq_false o =
  let cmp = o <> 0 in
  if cmp <> false then o else 7
[%%expect_fexpr Simplify{|
let code neq_false_3 deleted in
let code loopify(never) size(12) newer_version_of(neq_false_3)
      neq_false_3_1 (o : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let prim = %phys_ne (o, 0) in
  switch prim
    | 0 -> k (7)
    | 1 -> k (o)
in
let $camlTOP5__neq_false_11 = closure neq_false_3_1 @neq_false &toplevel in
let $camlTOP5 = Block 0 ($camlTOP5__neq_false_11) in
cont done ($camlTOP5)
|}]

(* The constant may be on the left. *)
let true_eq o =
  let cmp = o <> 0 in
  if true = cmp then o else 7
[%%expect_fexpr Simplify{|
let code true_eq_4 deleted in
let code loopify(never) size(12) newer_version_of(true_eq_4)
      true_eq_4_1 (o : imm tagged)
        my_closure &my_alloc_region my_depth
        -> k * k1
        : imm tagged =
  let prim = %phys_ne (o, 0) in
  switch prim
    | 0 -> k (7)
    | 1 -> k (o)
in
let $camlTOP6__true_eq_14 = closure true_eq_4_1 @true_eq &toplevel in
let $camlTOP6 = Block 0 ($camlTOP6__true_eq_14) in
cont done ($camlTOP6)
|}]

(* Not a boolean: the comparison stays on the tagged value. *)
let not_a_bool s x y =
  let c = String.unsafe_get s 0 in
  if c = 'c' then x else y
[%%expect_fexpr Simplify{|
let code not_a_bool_5 deleted in
let code loopify(never) size(17) newer_version_of(not_a_bool_5)
      not_a_bool_5_1 (s : val, x, y)
        my_closure &my_alloc_region my_depth
        -> k * k1 =
  let prim = %string_load.[`8`] (s, 0n) in
  let c = %tag_imm (prim) in
  let prim_1 = %phys_eq (c, 99) in
  switch prim_1
    | 0 -> k (y)
    | 1 -> k (x)
in
let $camlTOP7__not_a_bool_17 = closure not_a_bool_5_1 @not_a_bool &toplevel
in
let $camlTOP7 = Block 0 ($camlTOP7__not_a_bool_17) in
cont done ($camlTOP7)
|}]

(* Likewise when the constant is 0 or 1 but the other value need not be. *)
let not_a_bool_against_one s x y =
  let c = String.unsafe_get s 0 in
  if c = '\001' then x else y
[%%expect_fexpr Simplify{|
let code not_a_bool_against_one_6 deleted in
let code loopify(never) size(17) newer_version_of(not_a_bool_against_one_6)
      not_a_bool_against_one_6_1 (s : val, x, y)
        my_closure &my_alloc_region my_depth
        -> k * k1 =
  let prim = %string_load.[`8`] (s, 0n) in
  let c = %tag_imm (prim) in
  let prim_1 = %phys_eq (c, 1) in
  switch prim_1
    | 0 -> k (y)
    | 1 -> k (x)
in
let $camlTOP8__not_a_bool_against_one_20 =
  closure not_a_bool_against_one_6_1 @not_a_bool_against_one &toplevel
in
let $camlTOP8 = Block 0 ($camlTOP8__not_a_bool_against_one_20) in
cont done ($camlTOP8)
|}]
